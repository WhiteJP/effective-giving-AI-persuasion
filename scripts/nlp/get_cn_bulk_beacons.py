import aiohttp
import asyncio
import json
import os
import csv
import random
import time
from pathlib import Path
from tqdm import tqdm


# --- Configuration ---
API_URL = "https://api.charitynavigator.org/graphql"
API_KEY = os.getenv("CN_API_KEY") 
RESULT_SIZE = 5000
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\parsed_data"
MAX_RETRIES = 5  # Maximum number of retry attempts
INITIAL_BACKOFF = 2  # Initial backoff in seconds


HEADERS = {
    "Authorization": API_KEY,
    "Content-Type": "application/json"
}


def build_query(after_ein=""):
    return {
        "query": f"""
        {{
          bulkNonprofits(
            resultSize: {RESULT_SIZE},
            afterEin: "{after_ein}"
          ) {{
            results {{
              ein
              name
              beacons {{
                name
                shortName
                slug
                weight
                isEligible
                notEligibleReasons
                isPassing
                score
              }}
            }}
          }}
        }}
        """
    }


async def fetch_page_with_retry(session, after_ein):
    retries = 0
    backoff = INITIAL_BACKOFF
    
    while retries <= MAX_RETRIES:
        try:
            async with session.post(API_URL, headers=HEADERS, json=build_query(after_ein)) as resp:
                if resp.status != 200:
                    # Get error message
                    error_text = await resp.text()
                    
                    # If we get a 503 or other status that might benefit from retry
                    if resp.status in [429, 500, 502, 503, 504]:
                        retries += 1
                        if retries > MAX_RETRIES:
                            raise Exception(f"API error {resp.status} after max retries: {error_text}")
                        
                        # Apply jitter to backoff to prevent thundering herd
                        jitter = backoff * (0.5 + random.random())
                        print(f"Received {resp.status} error. Retrying in {jitter:.2f} seconds (attempt {retries}/{MAX_RETRIES})...")
                        await asyncio.sleep(jitter)
                        
                        # Exponential backoff
                        backoff *= 2
                        continue
                    else:
                        # For other errors, fail immediately
                        raise Exception(f"API error {resp.status}: {error_text}")
                
                return await resp.json()
        
        except aiohttp.ClientError as e:
            # Network errors
            retries += 1
            if retries > MAX_RETRIES:
                raise Exception(f"Network error after max retries: {str(e)}")
            
            jitter = backoff * (0.5 + random.random())
            print(f"Network error: {str(e)}. Retrying in {jitter:.2f} seconds (attempt {retries}/{MAX_RETRIES})...")
            await asyncio.sleep(jitter)
            
            # Exponential backoff
            backoff *= 2


async def run():
    async with aiohttp.ClientSession() as session:
        after_ein = ""
        page = 1
        all_results = []
        total = 0

        with tqdm(desc="Fetching", unit=" records") as pbar:
            while True:
                try:
                    data = await fetch_page_with_retry(session, after_ein)
                    results = data["data"]["bulkNonprofits"]["results"]
                except Exception as e:
                    print(f"Error on page {page}: {e}")
                    break

                if not results:
                    print("Finished fetching all data.")
                    break

                all_results.extend(results)
                after_ein = results[-1]["ein"]
                total += len(results)
                page += 1
                pbar.update(len(results))

        # Save all results to a single JSON file
        json_output_file = Path(OUTPUT_DIR) / "bulk_beacons.json"
        with open(json_output_file, "w", encoding="utf-8") as f:
            json.dump(all_results, f, indent=2)
        print(f"Saved all data to {json_output_file}")

        # Also save as CSV
        csv_output_file = Path(OUTPUT_DIR) / "bulk_beacons.csv"
        with open(csv_output_file, "w", newline="", encoding="utf-8") as f:
            # Create CSV writer
            csv_writer = csv.writer(f)
            
            # Write header
            csv_writer.writerow(["ein", "name", "beacon_name", "beacon_shortName", "beacon_slug", 
                               "beacon_weight", "beacon_isEligible", "beacon_notEligibleReasons", 
                               "beacon_isPassing", "beacon_score"])
            
            # Write data
            for org in all_results:
                ein = org["ein"]
                name = org["name"]
                
                if org["beacons"]:
                    for beacon in org["beacons"]:
                        csv_writer.writerow([
                            ein,
                            name,
                            beacon.get("name", ""),
                            beacon.get("shortName", ""),
                            beacon.get("slug", ""),
                            beacon.get("weight", ""),
                            beacon.get("isEligible", ""),
                            ",".join(beacon.get("notEligibleReasons", [])),
                            beacon.get("isPassing", ""),
                            beacon.get("score", "")
                        ])
                else:
                    # Write a row with just the org info if no beacons
                    csv_writer.writerow([ein, name, "", "", "", "", "", "", "", ""])
        
        print(f"Saved all data to {csv_output_file}")


# --- Run when called from command line ---
if __name__ == "__main__":
    asyncio.run(run())
