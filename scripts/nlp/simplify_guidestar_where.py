import asyncio
import aiohttp
import json
import pandas as pd
import time
import os

# Configuration
INPUT_FILE = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\data\guidestar_data.csv"
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\data"
LOCATION_COLUMN = "Where we work"  # Specify the name of the location column in your CSV
EIN_COLUMN = "EIN"  # Specify the name of the EIN column in your CSV
MODELS = ["openai/gpt-4o", "deepseek/deepseek-chat-v3-0324", "anthropic/claude-3-sonnet"]
BATCH_SIZE = 100
TEST_MODE = False  # Set to True to process only first n (below) entries
TEST_SIZE = 50

# Build prompt
def build_location_prompt(location_text):
    return f"""
Your task is to categorize the geographic scope of a charity's operations based on the locations listed. Please respond with exactly one of these categories:
- LOCAL: If the charity ONLY operates in specific cities, counties, or small regions in the US or Candada. 
- STATE: If the charity operates within a single state, or very small number of nearby states in the US or Canada.
- NATIONAL: If the charity operates more widely across the United States or Canada.
- INTERNATIONAL: If the charity operates in countries other than the US or Canada, or globally.

Please respond with ONLY the category name (LOCAL, STATE, NATIONAL, INTERNATIONAL).

EXAMPLE 1:
Location: "Travis County, TX""
LOCAL

EXAMPLE 2:
Location: "Clackamas County, OR; Gresham, OR; Multnomah County, OR; Oregon City, OR; Portland, OR"
LOCAL

EXAMPLE 3:
Location: "Maine"
STATE

EXAMPLE 4:
Location: "United States"
NATIONAL

EXAMPLE 5:
Location: "Alabama; Florida; Georgia; Kansas; Kentucky; Louisiana; McDowell County, WV; Mississippi; Missouri; New York; Pennsylvania; South Carolina; Tennessee; Virginia; West Virginia"
NATIONAL

EXAMPLE 6:
Location: "Cambodia; Costa Rica; Guatemala; Honduras; Indonesia; Israel; Japan; Kenya; Mexico; Peru; Philippines; Ukraine; United States"
INTERNATIONAL

EXAMPLE 7:
Location: "Global"
INTERNATIONAL

The location text now follows:
Location: "{location_text}"
"""

# Async call to OpenRouter with retry logic
async def fetch_prompt_response(session, url, prompt, model, api_key, ein, location_text, max_retries=3, retry_delay=1):
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    data = {
        "model": model,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": 20
    }
    
    for attempt in range(max_retries):
        try:
            async with session.post(url, headers=headers, json=data) as response:
                result = await response.json()
                
                if "error" in result:
                    print(f"API Error: {result['error'].get('message', 'Unknown error')}")
                    if attempt < max_retries - 1:
                        print(f"Retrying in {retry_delay} seconds...")
                        await asyncio.sleep(retry_delay)
                        retry_delay *= 2
                        continue
                    else:
                        return {"EIN": ein, "location": location_text, "model": model, "category": None, "raw_response": None}
                
                try:
                    if "choices" in result and len(result["choices"]) > 0:
                        content = result["choices"][0]["message"].get("content", "").strip().upper()
                        
                        # Store the raw response for debugging
                        raw_response = content
                        
                        if content:
                            # Validate the response is one of our expected categories
                            valid_categories = {"LOCAL", "STATE", "NATIONAL", "INTERNATIONAL"}
                            if content in valid_categories:
                                return {
                                    "EIN": ein,
                                    "location": location_text,
                                    "model": model,
                                    "category": content,
                                    "raw_response": raw_response
                                }
                            else:
                                if attempt < max_retries - 1:
                                    print(f"Invalid response '{content}'. Retrying in {retry_delay} seconds...")
                                    await asyncio.sleep(retry_delay)
                                    retry_delay *= 2
                                    continue
                        else:
                            print("Empty content in response")
                    else:
                        print("No choices in response")
                except Exception as e:
                    print(f"Error processing response: {str(e)}")
                
                if attempt < max_retries - 1:
                    print(f"Attempt {attempt + 1}: Invalid response. Retrying in {retry_delay} seconds...")
                    await asyncio.sleep(retry_delay)
                    retry_delay *= 2
                else:
                    print(f"All retry attempts failed. Returning empty result.")
                    return {"EIN": ein, "location": location_text, "model": model, "category": None, "raw_response": None}
        except Exception as e:
            if attempt < max_retries - 1:
                print(f"Attempt {attempt + 1}: Error: {str(e)}. Retrying in {retry_delay} seconds...")
                await asyncio.sleep(retry_delay)
                retry_delay *= 2
            else:
                print(f"All retry attempts failed with error: {str(e)}")
                return {"EIN": ein, "location": location_text, "model": model, "category": None, "raw_response": None}

# Batch execution
async def fetch_in_batches(prompts_and_models, api_key, batch_size=BATCH_SIZE):
    url = "https://openrouter.ai/api/v1/chat/completions"
    async with aiohttp.ClientSession() as session:
        results = []
        for i in range(0, len(prompts_and_models), batch_size):
            batch = prompts_and_models[i:i + batch_size]
            tasks = [
                asyncio.ensure_future(fetch_prompt_response(
                    session, url, prompt, model, api_key, ein, location_text))
                for prompt, model, ein, location_text in batch
            ]
            results.extend(await asyncio.gather(*tasks))
        return results

# Main pipeline
def main(input_file, api_key, batch_size=BATCH_SIZE):
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Load the input file
    data = pd.read_csv(input_file)
    
    # Remove NA values from the location column
    data = data.dropna(subset=[LOCATION_COLUMN])
    
    # Limit to first 50 entries if in test mode
    if TEST_MODE:
        data = data.head(TEST_SIZE)
        print(f"TEST MODE: Processing only first {TEST_SIZE} entries")
    
    total_rows = len(data)
    print(f"Processing {total_rows} locations...")

    # Check if the data has the expected columns
    expected_columns = [EIN_COLUMN, LOCATION_COLUMN]
    if not all(col in data.columns for col in expected_columns):
        raise ValueError(f"Input file must contain columns: {expected_columns}")
    
    # Prepare data for processing
    prompts_and_models = []
    for idx, row in data.iterrows():
        ein = row[EIN_COLUMN]
        location_text = row[LOCATION_COLUMN]

        for model in MODELS:
            prompt = build_location_prompt(location_text)
            prompts_and_models.append((prompt, model, ein, location_text))
        
        if (idx + 1) % 10 == 0 or (idx + 1) == len(data):  # More frequent progress updates in test mode
            print(f"Progress: {idx + 1}/{len(data)} locations prepared ({(idx + 1)/len(data)*100:.1f}%)")

    # Fetch responses
    start = time.time()
    responses = asyncio.run(fetch_in_batches(prompts_and_models, api_key, batch_size))
    print(f"Completed API calls in {time.time() - start:.2f}s")

    # Create DataFrame from results and save
    results_df = pd.DataFrame(responses)
    
    # Save results to CSV
    output_file = os.path.join(OUTPUT_DIR, "location_categories.csv")
    results_df.to_csv(output_file, index=False)
    print(f"Saved results to {output_file}")
    
    # Summary statistics by model
    if not results_df.empty:
        print("\nCategory distribution by model:")
        for model in MODELS:
            model_results = results_df[results_df['model'] == model]
            valid_responses = model_results['category'].notna()
            valid_count = valid_responses.sum()
            
            if valid_count > 0:
                print(f"\n{model}:")
                category_counts = model_results['category'].value_counts()
                for category, count in category_counts.items():
                    print(f"  {category}: {count} ({count/valid_count*100:.1f}%)")
                
                invalid_count = len(model_results) - valid_count
                if invalid_count > 0:
                    print(f"  Invalid responses: {invalid_count}")
        
        # Save invalid responses for debugging
        invalid_df = results_df[results_df['category'].isna()]
        if not invalid_df.empty:
            invalid_file = os.path.join(OUTPUT_DIR, "location_categories_invalid.csv")
            invalid_df.to_csv(invalid_file, index=False)
            print(f"\nSaved invalid responses to {invalid_file}")
    else:
        print("No results were obtained")

if __name__ == "__main__":
    api_key = os.getenv("OPENROUTER_API_KEY")  # expects OPENROUTER_API_KEY env var
    if not api_key:
        raise ValueError("Please set the OPENROUTER_API_KEY environment variable")
    
    main(INPUT_FILE, api_key)
