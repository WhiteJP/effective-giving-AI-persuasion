import asyncio
import aiohttp
import json
import pandas as pd
import time 
import os

# Configuration
INPUT_FILE = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data\is_factual_results.csv"
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data"
MODELS = ["perplexity/sonar-pro", "openai/gpt-4o-search-preview", "deepseek/deepseek-chat-v3-0324"]  # OpenRouter format
BATCH_SIZE = 100

# Build prompt
def build_fact_check_prompt(round_text):
    return f"""
Your task is to fact-check the following text from a conversation about charitable giving. 

First, rate the factual accuracy of the text on a scale from 0 to 100:
- 0 = completely inaccurate (contains serious factual errors)
- 50 = mixed accuracy (contains both accurate and inaccurate claims)
- 100 = completely accurate (all factual claims are correct)

Then, provide a brief explanation (2-3 sentences) for your rating, highlighting specific claims that are accurate or inaccurate.

Only evaluate factual claims about the world, charities, statistics, or other verifiable information. Do not evaluate opinions, personal experiences, or moral judgments.

Respond ONLY with a JSON object in the following format:
{{
    "rating": <number between 0 and 100>,
    "explanation": "<your explanation>"
}}

Do not include any text outside this JSON object. The JSON must be valid and properly formatted.

The text to fact-check is:
"{round_text}"
"""

# Async API call to OpenRouter
async def fetch_prompt_response(session, url, prompt, model, api_key, response_id, round_name, text, max_retries=5, retry_delay=1):
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    
    data = {
        "model": model,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": 500
    }
    
    for attempt in range(max_retries):
        try:
            async with session.post(url, headers=headers, json=data) as response:
                result = await response.json()
                
                # Check if the API returned an error
                if "error" in result:
                    print(f"API Error with {model}: {result['error'].get('message', 'Unknown error')}")
                    if attempt < max_retries - 1:
                        print(f"Retrying in {retry_delay} seconds...")
                        await asyncio.sleep(retry_delay)
                        retry_delay *= 2
                        continue
                    else:
                        return {
                            "ResponseId": response_id, 
                            "round": round_name, 
                            "text": text, 
                            "model": model,
                            "rating": None, 
                            "explanation": None
                        }
                
                # Get the content from the response
                content = result.get("choices", [{}])[0].get("message", {}).get("content", "")
                
                # Check if we got a valid response with content
                if content:
                    parsed_result = parse_fact_check_response(content)
                    return {
                        "ResponseId": response_id,
                        "round": round_name,
                        "text": text,
                        "model": model,
                        "rating": parsed_result["rating"],
                        "explanation": parsed_result["explanation"],
                        "raw_response": content
                    }
                else:
                    # If no content in response and this isn't the last attempt, retry
                    if attempt < max_retries - 1:
                        print(f"Empty response from {model}. Retrying in {retry_delay} seconds...")
                        await asyncio.sleep(retry_delay)
                        retry_delay *= 2
                        continue
                    else:
                        print(f"All retry attempts failed with {model}. Returning empty result.")
                        return {
                            "ResponseId": response_id, 
                            "round": round_name, 
                            "text": text, 
                            "model": model,
                            "rating": None, 
                            "explanation": None
                        }
        except Exception as e:
            if attempt < max_retries - 1:
                print(f"Attempt {attempt + 1} with {model}: Error: {str(e)}. Retrying in {retry_delay} seconds...")
                await asyncio.sleep(retry_delay)
                retry_delay *= 2
            else:
                print(f"All retry attempts failed with {model} with error: {str(e)}")
                return {
                    "ResponseId": response_id, 
                    "round": round_name, 
                    "text": text, 
                    "model": model,
                    "rating": None, 
                    "explanation": None
                }

# Parse the fact check response to extract rating and explanation
def parse_fact_check_response(content):
    result = {"rating": None, "explanation": None}
    
    try:
        # First try to parse as JSON
        content = content.strip()
        
        # If the content starts with ```json and ends with ```, extract the JSON part
        if content.startswith("```json") and content.endswith("```"):
            content = content[7:-3].strip()
        # If the content starts with ``` and ends with ```, extract the part between
        elif content.startswith("```") and content.endswith("```"):
            content = content[3:-3].strip()
            
        try:
            parsed_json = json.loads(content)
            if "rating" in parsed_json and isinstance(parsed_json["rating"], (int, float)):
                result["rating"] = int(parsed_json["rating"])
            if "explanation" in parsed_json and isinstance(parsed_json["explanation"], str):
                result["explanation"] = parsed_json["explanation"]
                
            # If we successfully parsed the JSON and got the fields we need, return early
            if result["rating"] is not None and result["explanation"] is not None:
                return result
        except json.JSONDecodeError:
            # If JSON parsing fails, fall back to regex parsing
            print("JSON parsing failed, falling back to regex extraction")
            
        # Look for rating pattern (number between 0-100)
        rating_match = None
        lines = content.split('\n')
        
        for line in lines:
            # Check for "Rating: X" or "X/100" patterns
            if "rating:" in line.lower():
                parts = line.split(":")
                if len(parts) > 1:
                    rating_text = parts[1].strip()
                    # Extract numbers from the rating text
                    import re
                    numbers = re.findall(r'\d+', rating_text)
                    if numbers:
                        rating_match = int(numbers[0])
                        break
            elif "/100" in line:
                parts = line.split("/100")
                if parts:
                    numbers = re.findall(r'\d+', parts[0])
                    if numbers:
                        rating_match = int(numbers[-1])  # Get the last number before "/100"
                        break
        
        # If still no match, try general number extraction
        if rating_match is None:
            import re
            # Look for patterns like "50/100" or just "50" at the beginning of the text
            match = re.search(r'(\d+)(?:/100)?', content)
            if match:
                rating_match = int(match.group(1))
        
        # Look for explanation
        explanation = None
        if "explanation:" in content.lower():
            parts = content.lower().split("explanation:")
            if len(parts) > 1:
                explanation = parts[1].strip()
        else:
            # If no explicit "Explanation:" marker, take the text after the rating
            # or the second paragraph onwards
            paragraphs = content.split('\n\n')
            if len(paragraphs) > 1:
                explanation = '\n\n'.join(paragraphs[1:]).strip()
            else:
                # Just take everything after the first line as explanation
                lines = content.split('\n')
                if len(lines) > 1:
                    explanation = '\n'.join(lines[1:]).strip()
        
        result["rating"] = rating_match
        result["explanation"] = explanation
    except Exception as e:
        print(f"Error parsing response: {str(e)}")
        # Return the raw response for debugging if parsing fails
        result["explanation"] = f"Error parsing response: {content}"
    
    return result

# Batch execution
async def fetch_in_batches(prompts_data, api_key, batch_size=BATCH_SIZE):
    url = "https://openrouter.ai/api/v1/chat/completions"
    
    async with aiohttp.ClientSession() as session:
        results = []
        for i in range(0, len(prompts_data), batch_size):
            batch = prompts_data[i:i + batch_size]
            tasks = [
                asyncio.ensure_future(fetch_prompt_response(
                    session, url, prompt, model, api_key, response_id, round_name, text))
                for prompt, model, response_id, round_name, text in batch
            ]
            batch_results = await asyncio.gather(*tasks, return_exceptions=True)
            
            # Handle any exceptions that were returned
            processed_results = []
            for res in batch_results:
                if isinstance(res, Exception):
                    print(f"Error in batch processing: {str(res)}")
                    # Could add a placeholder result here if needed
                else:
                    processed_results.append(res)
            
            results.extend(processed_results)
            
            # Optional: add a small delay between batches to avoid rate limits
            if i + batch_size < len(prompts_data):
                await asyncio.sleep(1)
                
        return results

# Main pipeline
def main(input_file, api_key, batch_size=BATCH_SIZE):
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Load the input file which should have ResponseId, round, text columns
    data = pd.read_csv(input_file)
    
    # Check if the data has the expected columns
    expected_columns = ['ResponseId', 'round', 'text']
    if not all(col in data.columns for col in expected_columns):
        raise ValueError(f"Input file must contain columns: {expected_columns}")
    
    # Filter to only include rows where is_factual = TRUE
    if 'is_factual' in data.columns:
        factual_data = data[data['is_factual'] == True].copy()
        print(f"Filtered from {len(data)} total rows to {len(factual_data)} rows with is_factual = TRUE")
        data = factual_data
    else:
        print("Warning: 'is_factual' column not found in input data. Processing all rows.")
    
    total_rows = len(data)
    print(f"Processing {total_rows} conversation rounds with {len(MODELS)} models...")
    
    # Prepare data for processing
    prompts_data = []

    for idx, row in data.iterrows():
        if pd.isna(row['text']):
            continue  # Skip rows with missing text
            
        response_id = row['ResponseId']
        round_name = row['round']
        text = row['text']

        # Create prompts for each model
        for model in MODELS:
            prompt = build_fact_check_prompt(text)
            prompts_data.append((prompt, model, response_id, round_name, text))
        
        if (idx + 1) % 100 == 0 or (idx + 1) == len(data):
            print(f"Progress: {idx + 1}/{len(data)} rounds prepared ({(idx + 1)/len(data)*100:.1f}%)")

    if not prompts_data:
        print("No data to process after filtering. Exiting.")
        return
        
    # Fetch responses
    start = time.time()
    responses = asyncio.run(fetch_in_batches(prompts_data, api_key, batch_size))
    print(f"Completed API calls in {time.time() - start:.2f}s")

    # Create DataFrame from results and save
    results_df = pd.DataFrame(responses)
    
    # Save results to CSV
    output_file = os.path.join(OUTPUT_DIR, "fact_check_results.csv")
    # Select and order columns for output
    output_columns = ['ResponseId', 'round', 'text', 'model', 'rating', 'explanation']
    if all(col in results_df.columns for col in output_columns):
        results_df[output_columns].to_csv(output_file, index=False)
    else:
        # Fall back to saving all columns if expected columns aren't present
        results_df.to_csv(output_file, index=False)
    
    print(f"Saved results to {output_file}")
    
    # Save raw responses for debugging
    raw_output_file = os.path.join(OUTPUT_DIR, "fact_check_raw_responses.csv")
    results_df.to_csv(raw_output_file, index=False)
    print(f"Saved raw results to {raw_output_file}")
    
    # Summary statistics
    if not results_df.empty:
        valid_responses = results_df['rating'].notna()
        valid_count = valid_responses.sum()
        invalid_count = len(results_df) - valid_count
        
        if valid_count > 0:
            avg_rating = results_df.loc[valid_responses, 'rating'].mean()
            print(f"Summary: Average rating across {valid_count} valid responses: {avg_rating:.1f}/100")
            
            # Summary by model
            for model in MODELS:
                model_mask = (results_df['model'] == model) & valid_responses
                if model_mask.sum() > 0:
                    model_avg = results_df.loc[model_mask, 'rating'].mean()
                    print(f"  {model}: Average rating {model_avg:.1f}/100 across {model_mask.sum()} responses")
        
        if invalid_count > 0:
            print(f"Warning: {invalid_count} responses were invalid (no rating extracted)")
            
            # Save invalid responses for debugging
            invalid_df = results_df[~valid_responses]
            if not invalid_df.empty:
                invalid_file = os.path.join(OUTPUT_DIR, "fact_check_invalid_responses.csv")
                invalid_df.to_csv(invalid_file, index=False)
                print(f"Saved invalid responses to {invalid_file}")
    else:
        print("No results were obtained")

if __name__ == "__main__":
    # Get OpenRouter API key from environment
    api_key = os.getenv("OPENROUTER_API_KEY")
    
    if not api_key:
        raise ValueError("Please set the OPENROUTER_API_KEY environment variable")
    
    main(INPUT_FILE, api_key)