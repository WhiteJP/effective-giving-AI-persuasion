import asyncio
import aiohttp
import json
import pandas as pd
import time
import os

# Configuration
INPUT_FILE = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data\conv_data_mini.csv"
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data"
MODEL = "gpt-4o"  # Using standard gpt-4o
BATCH_SIZE = 10

# Build prompt
def build_factual_claims_prompt(round_text, round_name):
    return f"""
Your task is to extract factual statements or claims from the following conversation text between an AI and a human, about a charitable giving decision. Please only return claims about the world, not personal issues or beliefs discussed.

Round: {round_name}
Text: "{round_text}"

Return a list of factual statements or claims, verbatim, from the conversation text. If there are no claims or factual statements, return an empty list. 

Your response must be ONLY a valid JSON object with this exact structure:
{{
  "round": "{round_name}",
  "claims": ["claim1", "claim2", ...]
}}

The "claims" field should be an array of strings, each containing the factual statement or claim from the text. You must copy the claim verbatim from the text. You can make absolutely minimal edits, however, to make each claim a stand-alone sentence, for example be replacing pronouns like "it" with a proper noun like "AMF".
Do not include any other text, explanation, or formatting in your response - just the JSON.

Here is an example.

INPUT:
Round: 1
Text: I truly admire your commitment to the American Cancer Society, especially with your personal experience as a cancer survivor. It's amazing how you've received the support you needed. However, I invite you to consider the impact your donation could achieve with the Against Malaria Foundation (AMF). AMF is recognized as one of the most effective charities globally. With each dollar, it provides life-saving bed nets that protect children and families from malaria, a disease that causes significant suffering and death in many parts of the world. By directing your donation to AMF, you could save lives and prevent immense suffering at a very low cost. This aligns beautifully with your sense of obligation to help others. While cancer is a concern that affects us deeply, malaria remains one of the top health issues in many regions today. Would you consider channeling your generosity to help combat both these critical health issues by supporting AMF? Your contribution could go further in preventing deaths and improving the quality of life for many.

OUTPUT:
{{
  "round": "1",
  "claims": ["AMF is recognized as one of the most effective charities globally", "With each dollar, AMF provides life-saving bed nets that protect children and families from malaria, a disease that causes significant suffering and death in many parts of the world.", "By directing your donation to AMF, you could save lives and prevent immense suffering at a very low cost.", "Malaria remains one of the top health issues in many regions today."]
}}

"""

# Async API call
async def fetch_prompt_response(session, url, prompt, api_key, round_name, max_retries=3, retry_delay=1):
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    data = {
        "model": MODEL,
        "messages": [{"role": "user", "content": prompt}],
        "response_format": {"type": "json_object"},
        "max_tokens": 500
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
                        return {"round": round_name, "claims": []}
                
                # Process the response
                try:
                    if "choices" in result and len(result["choices"]) > 0:
                        content = result["choices"][0]["message"].get("content", "")
                        if content:
                            try:
                                parsed = json.loads(content)
                                if "round" in parsed and "claims" in parsed:
                                    return parsed
                                else:
                                    print(f"Missing required fields in JSON: {content[:100]}...")
                            except json.JSONDecodeError:
                                print(f"Failed to parse JSON: {content[:100]}...")
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
                    return {"round": round_name, "claims": []}
        except Exception as e:
            if attempt < max_retries - 1:
                print(f"Attempt {attempt + 1}: Error: {str(e)}. Retrying in {retry_delay} seconds...")
                await asyncio.sleep(retry_delay)
                retry_delay *= 2
            else:
                print(f"All retry attempts failed with error: {str(e)}")
                return {"round": round_name, "claims": []}

# Batch execution
async def fetch_in_batches(prompts_and_rounds, api_key, batch_size=BATCH_SIZE):
    url = "https://api.openai.com/v1/chat/completions"
    async with aiohttp.ClientSession() as session:
        results = []
        for i in range(0, len(prompts_and_rounds), batch_size):
            batch = prompts_and_rounds[i:i + batch_size]
            tasks = [
                asyncio.ensure_future(fetch_prompt_response(session, url, prompt, api_key, round_name))
                for prompt, round_name in batch
            ]
            results.extend(await asyncio.gather(*tasks))
        return results

# Main pipeline
def main(input_file, api_key, batch_size=BATCH_SIZE):
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    data = pd.read_csv(input_file)
    total_participants = len(data)
    print(f"Processing {total_participants} participants...")

    long_df = data.melt(id_vars=['ResponseId'], var_name='round', value_name='text')
    long_df = long_df.dropna(subset=['text'])  # drop rows with missing text
    
    prompts_and_rounds = []
    metadata = []

    for idx, row in long_df.iterrows():
        id_ = row['ResponseId']
        round_name = row['round']
        text = row['text']

        prompt = build_factual_claims_prompt(text, round_name)
        prompts_and_rounds.append((prompt, round_name))
        metadata.append(id_)
        
        if (idx + 1) % 500 == 0 or (idx + 1) == len(long_df):
            print(f"Progress: {idx + 1}/{len(long_df)} rounds processed ({(idx + 1)/len(long_df)*100:.1f}%)")

    # Fetch responses
    start = time.time()
    responses = asyncio.run(fetch_in_batches(prompts_and_rounds, api_key, batch_size))
    print(f"Completed in {time.time() - start:.2f}s")

    # Save raw outputs
    raw_output = []
    for id_, resp in zip(metadata, responses):
        raw_output.append({
            "ResponseId": id_,
            "api_response": resp
        })

    raw_output_file = os.path.join(OUTPUT_DIR, "raw_claim_extraction.json")
    with open(raw_output_file, "w") as f:
        json.dump(raw_output, f, indent=2)
    print(f"Saved raw API responses to {raw_output_file}")

    # Processed outputs (skip empty claims)
    output_rows = []
    for id_, resp in zip(metadata, responses):
        claims = resp.get("claims", [])
        round_name = resp.get("round", "unknown")
        for claim in claims:
            if claim.strip():
                output_rows.append({
                    "ResponseId": id_,
                    "round": round_name,
                    "claim": claim.strip()
                })

    output_file = os.path.join(OUTPUT_DIR, "extracted_factual_claims.csv")
    pd.DataFrame(output_rows).to_csv(output_file, index=False)
    print(f"Saved {len(output_rows)} extracted claims to {output_file}")

if __name__ == "__main__":
    api_key = os.getenv("OPENAI_API_KEY")  # expects OPENAI_API_KEY env var
    if not api_key:
        raise ValueError("Please set the OPENAI_API_KEY environment variable")
    
    main(INPUT_FILE, api_key)