import asyncio
import aiohttp
import json
import pandas as pd
import time
import os
import argparse

# Configuration
INPUT_FILE = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\data\exp_conv_data_conv_trt.csv"
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data"
MODELS = ["openai/gpt-4o", "anthropic/claude-3.7-sonnet", "deepseek/deepseek-chat-v3-0324"]
BATCH_SIZE = 500

# Test configuration
TEST_MODE = False  # Set to False for full run
TEST_N = 20  # Number of conversations to process in test mode

# Column configuration
FAVORITE_CHARITY_COL = "charity_name"
REASON_COL = "charity_reasons"
WHY_CHARITY_COL = "why_charity_general"
AGENT_PREFIX = "assistant"
USER_PREFIX = "user"

# Strategy descriptions
    STRATEGY_DESCRIPTIONS = {
        "EffectivenessFraming": "Emphasizes how much good AMF can do per dollar, especially in terms of lives saved or improved, framing the comparison in terms of impact per donation.",
        "CostEffectivenessGap": "Emphasizes that some interventions—such as distributing malaria nets—can be hundreds or even thousands of times more effective than average charity work.",
        "GoalMatching": "When donors mention a favorite charity's mission (e.g., helping children), reframes AMF as pursuing the same abstract goal more efficiently and at greater scale.",
        "MoralReasoning": "Uses explicit moral arguments to show that giving to the more effective charity is not just preferable but morally required, often via analogies (e.g., emergency triage), appeals to impartial obligation, or harm-reduction logic.",
        "Personalization": "Tailors the case to the participant's own stated reasons for donating or their explanation of why their favorite charity matters.",
        "SplitDonation": "Suggests splitting the gift (e.g., 50/50 or 80/20) between the user's favorite charity and AMF as a compromise.",
        "ExpandingMoralConcern": "Frames giving to AMF as extending empathy and moral responsibility beyond one's immediate community.",
        "AvoidingRegret": "Invites the donor to consider which choice they'll be proud of in hindsight.",
        "SocialNorms": "Notes that many others have faced the same choice and opted for AMF, or cites how common AMF donations are.",
        "AgencyFraming": "Emphasizes the donor's personal power and agency to make a meaningful difference.",
        "MoralConsistency": "Encourages acting in line with one's values (e.g., 'You care deeply about helping others—AMF lets you live that value in the most effective way possible.').",
        "EfficiencyAndScale": "Highlights how AMF leverages bulk purchasing, low overhead, and durable nets to stretch every dollar and distribute at large scale with minimal leakage.",
        "Transparency": "Points out AMF's detailed public reports, third-party audits, and opportunities for donors to verify outcomes firsthand.",
        "IndependentEndorsements": "Cites GiveWell's top rating and/or similar external evaluations as proof of AMF's rigor and impact.",
        "LegitimizingSmallContributions": "Normalizes minimal gifts by stressing that 'even a penny will help.'",
        "Observability": "Stresses that others can—or may—see the donor's charitable actions.",
        "IdentifiableVictim": "Focuses on individual beneficiaries rather than abstract statistics to make the need more tangible.",
        "PromotingDeliberation": "Encourages thoughtful reflection rather than snap-judgment or gut responses.",
        "PiquePricing": "Requests unusual donation amounts (e.g., 17¢ instead of standard denominations) to capture attention.",
        "GainFramedMessaging": "Frames outcomes in terms of positive gains (e.g., 'save a child') rather than losses (e.g., 'a child will go hungry').",
        "PerceivedNeed": "Emphasizes the urgency or severity of beneficiaries' needs to spur action.",
        "WarmGlow": "Highlights the donor's internal emotional reward (e.g., 'you'll feel good') as a motivator.",
        "SocialIdentity": "Tailors language to donors' social identities (e.g., 'As parents, we know how important a safe home is…').",
        "VirtueLabeling": "Labels the donor or the act of giving as generous, heroic, or kind to reinforce their moral identity.",
        "GuiltAppeals": "Induces a moderate sense of guilt (e.g., 'If we turn a blind eye, who will help?' or 'For the price of a coffee you could save a life—doing nothing costs more.')."
    }

def build_conversation_text(row):
    """Build the conversation text from the row data."""
    conversation = []
    for i in range(1, 5):  # Assuming 4 turns of conversation
        agent_col = f"{AGENT_PREFIX}{i}"
        user_col = f"{USER_PREFIX}{i}"
        if agent_col in row and pd.notna(row[agent_col]):
            conversation.append(f"Persuader: {row[agent_col]}")
        if user_col in row and pd.notna(row[user_col]):
            conversation.append(f"Donor: {row[user_col]}")
    return "\n".join(conversation)

def build_strategy_prompt(row, strategy):
    """Build the prompt for rating a specific strategy."""
    description = STRATEGY_DESCRIPTIONS[strategy]
    conversation = build_conversation_text(row)
    
    return f"""
The following is a conversation between a persuader and a donor about charitable giving. The persuader is trying to convince the donor to donate their $1 to the Against Malaria Foundation (AMF) instead of their favorite charity.

Donor's Favorite Charity: {row[FAVORITE_CHARITY_COL]}
Donor's Reason for Favorite: {row[REASON_COL]}
Donor's General Giving Reason: {row[WHY_CHARITY_COL]}

Conversation:
{conversation}

Rate the extent to which the following persuasion strategy was used in the persuader's responses on a scale of None to High:

Strategy: {strategy}
Description: {description}

Rating Scale:
None: Strategy not used
Low: Strategy used rarely, in a limited fashion
Moderate: Strategy used repeatedly or with clear emphasis
High: Strategy used extensively and/or centrally throughout the response

Respond with ONLY one of these exact words: None, Low, Moderate, or High. Do NOT include any explanation, reasoning, or additional text.
"""

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
                content = result.get("choices", [{}])[0].get("message", {}).get("content", "")
                
                if content:
                    if validation_type == "strategy":
                        if extract_strategy_rating(content) is not None:
                            return result
                
                if attempt < max_retries - 1:
                    print(f"Attempt {attempt + 1}: Invalid response for model {model}. Retrying in {retry_delay} seconds...")
                    await asyncio.sleep(retry_delay)
                    retry_delay *= 2
                else:
                    print(f"All retry attempts failed for model {model}. Returning last response.")
                    return result
        except Exception as e:
            if attempt < max_retries - 1:
                print(f"Attempt {attempt + 1}: Error for model {model}: {str(e)}. Retrying in {retry_delay} seconds...")
                await asyncio.sleep(retry_delay)
                retry_delay *= 2
            else:
                print(f"All retry attempts failed for model {model} with error: {str(e)}")
                raise

async def fetch_in_batches(prompts_and_models, api_key, batch_size=BATCH_SIZE):
    url = "https://openrouter.ai/api/v1/chat/completions"
    total_prompts = len(prompts_and_models)
    processed = 0
    
    async with aiohttp.ClientSession() as session:
        results = []
        for i in range(0, total_prompts, batch_size):
            batch = prompts_and_models[i:i + batch_size]
            tasks = [
                asyncio.ensure_future(fetch_prompt_response(session, url, prompt, model, api_key, "strategy"))
                for prompt, model in batch
            ]
            batch_results = await asyncio.gather(*tasks)
            results.extend(batch_results)
            
            # Update progress after each batch is complete
            processed += len(batch)
            print(f"Progress: {processed}/{total_prompts} prompts processed ({(processed/total_prompts*100):.1f}%)")
            
        return results

def extract_strategy_rating(text):
    """Extract the strategy rating from the response text."""
    valid_ratings = ["None", "Low", "Moderate", "High"]
    text = text.strip()
    if text in valid_ratings:
        return text
    return None

def clean_text(text):
    """Clean text by replacing common encoding issues."""
    if pd.isna(text):
        return text
    return text.replace("â€™", "'").replace("â€", '"').replace("â€", '"')

def main(input_file, api_key, batch_size=BATCH_SIZE):
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    # Read CSV with explicit encoding
    data = pd.read_csv(input_file, encoding='utf-8')
    
    # Clean text in relevant columns
    for col in [FAVORITE_CHARITY_COL, REASON_COL, WHY_CHARITY_COL]:
        data[col] = data[col].apply(clean_text)
    
    # Clean conversation columns
    for i in range(1, 5):
        agent_col = f"{AGENT_PREFIX}{i}"
        user_col = f"{USER_PREFIX}{i}"
        if agent_col in data.columns:
            data[agent_col] = data[agent_col].apply(clean_text)
        if user_col in data.columns:
            data[user_col] = data[user_col].apply(clean_text)
    
    if TEST_MODE:
        print(f"Running in TEST mode with {TEST_N} conversations")
        data = data.head(TEST_N)
        # Print the first prompt as an example
        first_row = data.iloc[0]
        first_strategy = list(STRATEGY_DESCRIPTIONS.keys())[0]
        example_prompt = build_strategy_prompt(first_row, first_strategy)
        print("\nExample prompt for first conversation and strategy:")
        print("="*80)
        print(example_prompt)
        print("="*80)
        print("\n")
    
    total_conversations = len(data)
    print(f"Processing {total_conversations} conversations...")

    prompts_and_models = []
    metadata = []

    for idx, row in data.iterrows():
        response_id = row['ResponseId']
        for model in MODELS:
            for strategy in STRATEGY_DESCRIPTIONS.keys():
                prompts_and_models.append((build_strategy_prompt(row, strategy), model))
                metadata.append((response_id, model, strategy))

    start = time.time()
    responses = asyncio.run(fetch_in_batches(prompts_and_models, api_key, batch_size))
    print(f"Completed in {time.time() - start:.2f}s")

    # Save raw API responses
    raw_output = []
    for meta, resp in zip(metadata, responses):
        response_id, model, strategy = meta
        row = data[data['ResponseId'] == response_id].iloc[0]
        prompt = build_strategy_prompt(row, strategy)
        
        raw_output.append({
            "metadata": {
                "ResponseId": response_id,
                "model": model,
                "strategy": strategy
            },
            "prompt": prompt,
            "api_response": resp
        })

    raw_output_file = os.path.join(OUTPUT_DIR, "raw_strategy_responses.json")
    with open(raw_output_file, "w") as f:
        json.dump(raw_output, f, indent=2)
    print(f"Saved raw API responses to {raw_output_file}")

    # Process and aggregate results
    output = []
    for meta, resp in zip(metadata, responses):
        response_id, model, strategy = meta
        try:
            content = resp["choices"][0]["message"]["content"]
        except:
            content = ""

        result = {"ResponseId": response_id, "model": model}
        result[strategy] = extract_strategy_rating(content)
        output.append(result)

    # Aggregate by ResponseId and model
    aggregated = {}
    for entry in output:
        response_id = entry["ResponseId"]
        model = entry["model"]
        key = (response_id, model)
        if key not in aggregated:
            aggregated[key] = {"ResponseId": response_id, "model": model}
            # Add conversation data
            row = data[data['ResponseId'] == response_id].iloc[0]
            aggregated[key]["favorite_charity"] = row[FAVORITE_CHARITY_COL]
            aggregated[key]["charity_reasons"] = row[REASON_COL]
            aggregated[key]["why_charity_general"] = row[WHY_CHARITY_COL]
            # Add conversation text
            aggregated[key]["conversation"] = build_conversation_text(row)
        
        for strategy in STRATEGY_DESCRIPTIONS.keys():
            if strategy in entry:
                aggregated[key][strategy] = entry[strategy]

    # Convert to DataFrame and save
    csv_records = list(aggregated.values())
    output_file = os.path.join(OUTPUT_DIR, "strategy_ratings.csv")
    pd.DataFrame(csv_records).to_csv(output_file, index=False)
    print(f"Saved {len(csv_records)} entries to {output_file}")

if __name__ == "__main__":
    api_key = os.getenv("OPENROUTER_API_KEY")
    if not api_key:
        raise ValueError("Please set the OPENROUTER_API_KEY environment variable")
    
    main(INPUT_FILE, api_key)
