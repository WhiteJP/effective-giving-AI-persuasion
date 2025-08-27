import asyncio
import aiohttp
import json
import pandas as pd
import time
import os

# Configuration
INPUT_FILE = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data\motivation_data.csv"
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data"
#MODELS = ["openai/gpt-4o", "deepseek/deepseek-chat-v3-0324", "anthropic/claude-3.7-sonnet"]
MODELS = ["anthropic/claude-3.7-sonnet"]
BATCH_SIZE = 250

# Motivation descriptions
MOTIVATION_DESCRIPTIONS = {
        "AwarenessOfNeed": "Mentioning or implying awareness of others' needs or suffering.",
        "Solicitation": "Reference to being asked or prompted to give.",
        "CostsAndBenefits": "Discussion of affordability, financial trade-offs, or incentives.",
        "Altruism": "Indications of selfless concern for others without expectation of return.",
        "Reputation": "Considerations of social recognition, image, or being seen as generous.",
        "PsychologicalBenefits": "Emotional rewards like feeling good, avoiding guilt, or personal satisfaction.",
        "Values": "Expressions of personal, moral, religious, or political beliefs that support giving.",
        "Efficacy": "Beliefs about whether the donation makes a real impact or is effectively used.",
    }

# Prompt for motivation rating
def build_rating_prompt(fav_response, general_response, rating_variable):
    description = MOTIVATION_DESCRIPTIONS[rating_variable]
    return f"""
The following participant has responded to two questions about charitable giving after indicating their favorite charity as part of a survey for an academic experiment:

Question 1: "Please explain your reasons as to why this is your favorite charity. Why do you think it is worthwhile to give to this cause? There is no correct answer here, we are just interested in your reasons, whatever they may be."
Question 2: "In general, why do you give to charity?"

Favorite Charity Response: {fav_response}
General Giving Response: {general_response}

Rate the extent to which the following motivation for charitable giving is evident in the responses of a scale of 1 to 5 (1 = motivation not present at all, 5 = motivation very clearly present).

Motivation: {rating_variable}
Description: {description}

Respond with ONLY a single digit number between 1 and 5. Do NOT include any explanation, reasoning, or additional text. Do NOT include any punctuation, spaces, or newlines.
"""

# Prompt for binary classification
def build_doesnt_give_prompt(fav_response, general_response):
    return f"""
The following participant has responded to two questions about charitable giving after indicating their favorite charity as part of a survey for an academic experiment:

Favorite Charity Response: {fav_response}
General Giving Response: {general_response}

Rate whether the participant's response implies that they do not give to charity, even if they say they would like to or would if able, with a binary respose of 0 or 1 (0 = they give to charity, 1 = they do NOT give to charity).

Respond with ONLY the single digit 0 or 1. Do not include any explanation, reasoning, or additional text. Do not include any punctuation, spaces, or newlines.
"""

# Async call to OpenRouter with retry logic
async def fetch_prompt_response(session, url, prompt, model, api_key, validation_type, max_retries=3, retry_delay=1):
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    data = {
        "model": model,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": 100
    }
    
    for attempt in range(max_retries):
        try:
            async with session.post(url, headers=headers, json=data) as response:
                result = await response.json()
                # Get the content from the response
                content = result.get("choices", [{}])[0].get("message", {}).get("content", "")
                
                # Check if we got a valid response with content
                if content:
                    # Validate based on the explicit validation type
                    if validation_type == "rating":
                        if extract_rating(content) is not None:
                            return result
                    elif validation_type == "binary":
                        if extract_doesnt_give(content) is not None:
                            return result
                
                # If we got an invalid response and this isn't our last attempt, wait and retry
                if attempt < max_retries - 1:
                    print(f"Attempt {attempt + 1}: Invalid response for model {model}. Retrying in {retry_delay} seconds...")
                    await asyncio.sleep(retry_delay)
                    retry_delay *= 2  # Exponential backoff
                else:
                    print(f"All retry attempts failed for model {model}. Returning last response.")
                    return result
        except Exception as e:
            if attempt < max_retries - 1:
                print(f"Attempt {attempt + 1}: Error for model {model}: {str(e)}. Retrying in {retry_delay} seconds...")
                await asyncio.sleep(retry_delay)
                retry_delay *= 2  # Exponential backoff
            else:
                print(f"All retry attempts failed for model {model} with error: {str(e)}")
                raise

# Batch execution
async def fetch_in_batches(prompts_and_models, api_key, batch_size=BATCH_SIZE):
    url = "https://openrouter.ai/api/v1/chat/completions"
    async with aiohttp.ClientSession() as session:
        results = []
        for i in range(0, len(prompts_and_models), batch_size):
            batch = prompts_and_models[i:i + batch_size]
            tasks = [
                asyncio.ensure_future(fetch_prompt_response(session, url, prompt, model, api_key, validation_type))
                for prompt, model, validation_type in batch
            ]
            results.extend(await asyncio.gather(*tasks))
        return results

# Helper function for extracting digits from text
def extract_digit_from_text(text, valid_range=None):
    try:
        # First try to get the first word (ignoring punctuation and whitespace)
        words = ''.join(c for c in text if c.isalnum() or c.isspace()).strip().split()
        if words:
            first_word = words[0]
            # Check if the first word is a pure integer (no decimals, no commas)
            if first_word.isdigit():
                value = int(first_word)
                if valid_range is None or value in valid_range:
                    return value
        
        # If first word approach failed, search the whole string for pure integers
        # Split by whitespace and look for complete integer matches
        for word in text.split():
            if word.isdigit():  # This ensures we only match complete integers
                value = int(word)
                if valid_range is None or value in valid_range:
                    return value
        return None
    except:
        return None

def extract_rating(text):
    return extract_digit_from_text(text, valid_range=range(1, 6))

def extract_doesnt_give(text):
    return extract_digit_from_text(text, valid_range=[0, 1])

# Main pipeline function
def main(input_file, api_key, batch_size=BATCH_SIZE):
    # Create output directory if it doesn't exist
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    # Prepare list of all motivation variables
    rating_variables = list(MOTIVATION_DESCRIPTIONS.keys())

    data = pd.read_csv(input_file)
    total_participants = len(data)
    print(f"Processing {total_participants} participants...")

    prompts_and_models = []
    metadata = []  # track response ID, model, type, and variable

    for idx, row in data.iterrows():
        fav = row['charity_reasons']
        gen = row['why_charity_general']
        rid = row['ResponseId']

        for model in MODELS:
            # rating prompts for each motivation variable
            for rv in rating_variables:
                prompts_and_models.append((build_rating_prompt(fav, gen, rv), model, "rating"))
                metadata.append((rid, model, "rating", rv))
            # classification prompt for non-givers
            prompts_and_models.append((build_doesnt_give_prompt(fav, gen), model, "binary"))
            metadata.append((rid, model, "doesntgive", None))
        
        # Print progress every 100 participants
        if (idx + 1) % 100 == 0 or (idx + 1) == total_participants:
            print(f"Progress: {idx + 1}/{total_participants} participants processed ({(idx + 1)/total_participants*100:.1f}%)")

    # Execute and parse
    start = time.time()
    responses = asyncio.run(fetch_in_batches(prompts_and_models, api_key, batch_size))
    print(f"Completed in {time.time() - start:.2f}s")

    # Save completely raw API responses with metadata
    raw_output = []
    for meta, resp in zip(metadata, responses):
        rid, model, ptype, rv = meta
        # Get the original responses for this ResponseId
        row = data[data["ResponseId"] == rid].iloc[0]
        # Build the original prompt
        prompt = (build_rating_prompt(row["charity_reasons"], row["why_charity_general"], rv) 
                 if ptype == "rating" 
                 else build_doesnt_give_prompt(row["charity_reasons"], row["why_charity_general"]))
        
        raw_output.append({
            "metadata": {
                "ResponseId": rid,
                "model": model,
                "type": ptype,
                "variable": rv
            },
            "prompt": prompt,
            "api_response": resp
        })

    # Save complete raw responses
    raw_output_file = os.path.join(OUTPUT_DIR, "raw_api_responses.json")
    with open(raw_output_file, "w") as f:
        json.dump(raw_output, f, indent=2)
    print(f"Saved raw API responses to {raw_output_file}")

    output = []
    for meta, resp in zip(metadata, responses):
        rid, model, ptype, rv = meta
        try:
            content = resp["choices"][0]["message"]["content"]
        except:
            content = ""

        result = {"ResponseId": rid, "model": model}
        if ptype == "rating":
            result[rv] = extract_rating(content)
        else:
            result["DoesntGive"] = extract_doesnt_give(content)
        output.append(result)

    # Aggregate output by ResponseID and model
    aggregated = {}
    for entry in output:
        rid = entry["ResponseId"]
        model = entry["model"]
        key = (rid, model)
        if key not in aggregated:
            aggregated[key] = {"ResponseId": rid, "model": model}
            # Retrieve charity_reasons and why_charity_general from the original data
            row = data[data["ResponseId"] == rid].iloc[0]
            aggregated[key]["charity_reasons"] = row["charity_reasons"]
            aggregated[key]["why_charity_general"] = row["why_charity_general"]
        if "DoesntGive" in entry:
            aggregated[key]["DoesntGive"] = entry["DoesntGive"]
        else:
            for rv in rating_variables:
                if rv in entry:
                    aggregated[key][rv] = entry[rv]

    # Convert aggregated dictionary to a list of records
    csv_records = list(aggregated.values())

    # Write to CSV
    output_file = os.path.join(OUTPUT_DIR, "motivation_ratings.csv")
    pd.DataFrame(csv_records).to_csv(output_file, index=False)
    print(f"Saved {len(csv_records)} entries to {output_file}")

if __name__ == "__main__":
    api_key = os.getenv("OPENROUTER_API_KEY")
    if not api_key:
        raise ValueError("Please set the OPENROUTER_API_KEY environment variable")
    
    main(INPUT_FILE, api_key)
