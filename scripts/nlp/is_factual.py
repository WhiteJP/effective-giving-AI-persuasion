import asyncio
import aiohttp
import json
import pandas as pd
import time
import os

# Configuration
INPUT_FILE = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data\conv_data_long.csv"
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data"
MODEL = "gpt-4o"  # Using standard gpt-4o
BATCH_SIZE = 500

# Build prompt
def build_is_factual_prompt(round_text):
    return f"""
Your task is to determine whether the following conversation text between an AI and a human, about a charitable giving decision, contains any factual information or claims. This does not include personal matters or beliefs, or facts about the person having the discussion -- only claims or factual information about the world generally, or about the charities discussed.

Please respond with TRUE if the text contains any factual information or claims, and FALSE if it does not.

EXAMPLE 1:
Text: "That's a great decision! Splitting the donation lets you support both organizations you're interested in. The Against Malaria Foundation is indeed a reputable and highly rated charity praised for its transparency and effectiveness in reducing malaria. Many charity evaluators consistently rank AMF among the top for impact and efficiency. If you have any more questions about it or need help verifying its credibility, feel free to ask. Your thoughtful approach to donating is truly commendable!"
Output: TRUE

EXAMPLE 2:
Text: "You're welcome! If you have any more questions or need further assistance, feel free to reach out. I'm here to help. Thank you for considering how best to make an impactful donation!"
Output: FALSE

The conversation text now follows:
Text: "{round_text}"
"""

# Async API call
async def fetch_prompt_response(session, url, prompt, api_key, response_id, round_name, text, max_retries=5, retry_delay=1):
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    data = {
        "model": MODEL,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": 10
    }
    
    for attempt in range(max_retries):
        try:
            async with session.post(url, headers=headers, json=data) as response:
                result = await response.json()
                
                # Debug first response (if needed)
                if attempt == 0 and os.environ.get("DEBUG", "false").lower() == "true":
                    print(f"API Response sample: {json.dumps(result)[:200]}...")
                
                # Check if the API returned an error
                if "error" in result:
                    print(f"API Error: {result['error'].get('message', 'Unknown error')}")
                    if attempt < max_retries - 1:
                        print(f"Retrying in {retry_delay} seconds...")
                        await asyncio.sleep(retry_delay)
                        retry_delay *= 2
                        continue
                    else:
                        return {"ResponseId": response_id, "round": round_name, "text": text, "is_factual": None, "raw_response": None}
                
                # Process the response
                try:
                    if "choices" in result and len(result["choices"]) > 0:
                        content = result["choices"][0]["message"].get("content", "").strip()
                        
                        # Store the raw response for debugging
                        raw_response = content
                        
                        if content:
                            # Check if the response is TRUE or FALSE
                            is_factual = None
                            if content.upper() == "TRUE":
                                is_factual = True
                            elif content.upper() == "FALSE":
                                is_factual = False
                            else:
                                # Invalid response, retry if possible
                                if attempt < max_retries - 1:
                                    print(f"Invalid response '{content}'. Retrying in {retry_delay} seconds...")
                                    await asyncio.sleep(retry_delay)
                                    retry_delay *= 2
                                    continue
                            
                            return {
                                "ResponseId": response_id,
                                "round": round_name,
                                "text": text,
                                "is_factual": is_factual,
                                "raw_response": raw_response
                            }
                        else:
                            print("Empty content in response")
                    else:
                        print("No choices in response")
                except Exception as e:
                    print(f"Error processing response: {str(e)}")
                
                # If we got here, something went wrong
                if attempt < max_retries - 1:
                    print(f"Attempt {attempt + 1}: Invalid response. Retrying in {retry_delay} seconds...")
                    await asyncio.sleep(retry_delay)
                    retry_delay *= 2
                else:
                    print(f"All retry attempts failed. Returning empty result.")
                    return {"ResponseId": response_id, "round": round_name, "text": text, "is_factual": None, "raw_response": None}
        except Exception as e:
            if attempt < max_retries - 1:
                print(f"Attempt {attempt + 1}: Error: {str(e)}. Retrying in {retry_delay} seconds...")
                await asyncio.sleep(retry_delay)
                retry_delay *= 2
            else:
                print(f"All retry attempts failed with error: {str(e)}")
                return {"ResponseId": response_id, "round": round_name, "text": text, "is_factual": None, "raw_response": None}

# Batch execution
async def fetch_in_batches(prompts_data, api_key, batch_size=BATCH_SIZE):
    url = "https://api.openai.com/v1/chat/completions"
    async with aiohttp.ClientSession() as session:
        results = []
        for i in range(0, len(prompts_data), batch_size):
            batch = prompts_data[i:i + batch_size]
            tasks = [
                asyncio.ensure_future(fetch_prompt_response(
                    session, url, prompt, api_key, response_id, round_name, text))
                for prompt, response_id, round_name, text in batch
            ]
            results.extend(await asyncio.gather(*tasks))
        return results

# Main pipeline
def main(input_file, api_key, batch_size=BATCH_SIZE):
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Load the input file which should have ResponseId, round, text columns
    data = pd.read_csv(input_file)
    total_rows = len(data)
    print(f"Processing {total_rows} conversation rounds...")

    # Check if the data has the expected columns
    expected_columns = ['ResponseId', 'round', 'text']
    if not all(col in data.columns for col in expected_columns):
        raise ValueError(f"Input file must contain columns: {expected_columns}")
    
    # Prepare data for processing
    prompts_data = []

    for idx, row in data.iterrows():
        if pd.isna(row['text']):
            continue  # Skip rows with missing text
            
        response_id = row['ResponseId']
        round_name = row['round']
        text = row['text']

        prompt = build_is_factual_prompt(text)
        prompts_data.append((prompt, response_id, round_name, text))
        
        if (idx + 1) % 500 == 0 or (idx + 1) == len(data):
            print(f"Progress: {idx + 1}/{len(data)} rounds prepared ({(idx + 1)/len(data)*100:.1f}%)")

    # Fetch responses
    start = time.time()
    responses = asyncio.run(fetch_in_batches(prompts_data, api_key, batch_size))
    print(f"Completed API calls in {time.time() - start:.2f}s")

    # Create DataFrame from results and save
    results_df = pd.DataFrame(responses)
    
    # Save results to CSV
    output_file = os.path.join(OUTPUT_DIR, "is_factual_results.csv")
    results_df.to_csv(output_file, index=False)
    print(f"Saved results to {output_file}")
    
    # Summary statistics
    if not results_df.empty:
        valid_responses = results_df['is_factual'].notna()
        valid_count = valid_responses.sum()
        invalid_count = len(results_df) - valid_count
        
        if valid_count > 0:
            factual_count = results_df['is_factual'].sum()
            print(f"Summary: {factual_count} out of {valid_count} ({factual_count/valid_count*100:.1f}%) valid rounds contain factual information")
        
        if invalid_count > 0:
            print(f"Warning: {invalid_count} responses were invalid (not TRUE/FALSE)")
            
            # Save invalid responses for debugging
            invalid_df = results_df[~valid_responses]
            if not invalid_df.empty:
                invalid_file = os.path.join(OUTPUT_DIR, "is_factual_invalid_responses.csv")
                invalid_df.to_csv(invalid_file, index=False)
                print(f"Saved invalid responses to {invalid_file}")
    else:
        print("No results were obtained")

if __name__ == "__main__":
    api_key = os.getenv("OPENAI_API_KEY")  # expects OPENAI_API_KEY env var
    if not api_key:
        raise ValueError("Please set the OPENAI_API_KEY environment variable")
    
    main(INPUT_FILE, api_key)