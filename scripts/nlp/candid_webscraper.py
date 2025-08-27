from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from webdriver_manager.chrome import ChromeDriverManager
import pandas as pd
import time
import os
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Configuration
OUTPUT_DIR = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\data"
INPUT_CSV = r"C:\Users\jpwhite\MIT Dropbox\Josh White\projects\LLM-effective-giving\data\ein_list.csv" # CSV file containing EINs to scrape
EIN_COLUMN = "eins_dashed"  # Name of the column containing EINs in the input CSV
BATCH_SIZE = 25  # Number of EINs to process before saving
HARVARD_GUIDESTAR_URL = "https://www-guidestar-org.ezp-prod1.hul.harvard.edu/"

def load_eins():
    """Load EINs from the input CSV file"""
    try:
        df = pd.read_csv(INPUT_CSV)
        if EIN_COLUMN not in df.columns:
            raise ValueError(f"Column '{EIN_COLUMN}' not found in {INPUT_CSV}")
        
        eins = df[EIN_COLUMN].astype(str)
        
        ein_count = len(eins)
        print(f"Found {ein_count} valid EINs in {INPUT_CSV}")
    
        
        return eins.tolist()
    except Exception as e:
        print(f"Error loading EINs from {INPUT_CSV}: {str(e)}")
        return []

class GuideStarScraper:
    def __init__(self, debug=True):
        self.debug = debug
        self.setup_driver()
        self.data = []

    def debug_print(self, message):
        """Print debug messages if debug mode is enabled"""
        if self.debug:
            print(f"[DEBUG] {message}")

    def setup_driver(self):
        """Setup Chrome driver with appropriate options"""
        chrome_options = Options()
        # Remove headless mode to allow manual login
        # chrome_options.add_argument('--headless')  # Commented out to allow manual login
        chrome_options.add_argument('--no-sandbox')
        chrome_options.add_argument('--disable-dev-shm-usage')
        chrome_options.add_argument('--disable-blink-features=AutomationControlled')
        chrome_options.add_experimental_option("excludeSwitches", ["enable-automation"])
        chrome_options.add_experimental_option('useAutomationExtension', False)
        
        self.driver = webdriver.Chrome(
            service=Service(ChromeDriverManager().install()),
            options=chrome_options
        )
        self.wait = WebDriverWait(self.driver, 20)

    def verify_login(self):
        """Verify if we're actually logged in to GuideStar"""
        self.debug_print("Verifying login status...")
        
        # Wait longer for the page to fully load
        time.sleep(10)
        
        try:
            # Check if we're on GuideStar through Harvard proxy
            current_url = self.driver.current_url
            self.debug_print(f"Current URL: {current_url}")
            
            if "guidestar-org.ezp-prod1.hul.harvard.edu" not in current_url:
                self.debug_print("Not on GuideStar website")
                return False
            
            # Check for sign-in button which should not be present if logged in
            sign_in_elements = self.driver.find_elements(By.CSS_SELECTOR, "a[href*='Account/Login']")
            if sign_in_elements:
                self.debug_print("Not logged in - found sign in link")
                return False
            
            # Check for user menu or profile elements which should be present if logged in
            user_elements = self.driver.find_elements(By.CSS_SELECTOR, "a[href*='Account/Profile']")
            if user_elements:
                self.debug_print("Successfully logged in - found user profile link")
                return True
            
            self.debug_print("Login status unclear - proceeding with caution")
            return True
            
        except Exception as e:
            self.debug_print(f"Error verifying login: {str(e)}")
            return False

    def login(self):
        """Handle Harvard authentication and GuideStar access"""
        self.debug_print("Starting Harvard authentication process...")
        
        # Navigate to Harvard GuideStar URL
        self.driver.get(HARVARD_GUIDESTAR_URL)
        
        # Wait for manual login
        self.debug_print("Please complete the Harvard Key login and 2FA process...")
        self.debug_print("Waiting 60 seconds for manual login...")
        time.sleep(60)  # Increased wait time to 60 seconds
        
        # Check current URL and wait for GuideStar redirect
        current_url = self.driver.current_url
        self.debug_print(f"Current URL: {current_url}")
        
        # If we're not on GuideStar yet, wait a bit longer
        if "guidestar-org.ezp-prod1.hul.harvard.edu" not in current_url:
            self.debug_print("Not yet redirected to GuideStar. Waiting additional 30 seconds...")
            time.sleep(30)
            current_url = self.driver.current_url
            self.debug_print(f"Updated URL: {current_url}")
        
        # Verify we're on GuideStar
        if "guidestar-org.ezp-prod1.hul.harvard.edu" not in current_url:
            self.debug_print("Warning: Not redirected to GuideStar. Please check login status.")
            return False
        
        self.debug_print("Successfully accessed GuideStar through Harvard")
        return True

    def get_profile(self, ein):
        """Directly access the profile page using the EIN"""
        self.debug_print(f"Accessing profile for EIN: {ein}")
        profile_url = f"{HARVARD_GUIDESTAR_URL}profile/{ein}"
        self.driver.get(profile_url)
        time.sleep(5)  # Wait for page to load

    def extract_info(self, ein):
        """Extract the required information from the profile page"""
        self.debug_print("Starting to extract information...")
        info = {'EIN': ein}  # Use the provided EIN
        self.debug_print(f"Using EIN: {ein}")
        
        # Find all report sections
        report_sections = self.driver.find_elements(By.CLASS_NAME, "report-section")
        self.debug_print(f"Found {len(report_sections)} report sections")
        
        for section in report_sections:
            try:
                # Get the section header
                header = section.find_element(By.CLASS_NAME, "report-section-header").text.strip()
                # Remove the info icon text if present
                header = header.replace("info", "").strip()
                self.debug_print(f"Processing section: {header}")
                
                # Get all text elements in the section
                text_elements = section.find_elements(By.CLASS_NAME, "report-section-text")
                # Filter out empty strings and join with semicolon and space
                content = "; ".join([elem.text.strip() for elem in text_elements if elem.text.strip()])
                
                # Special handling for "Where we work" section
                if "Where we work" in header:
                    # Find the list items
                    list_items = section.find_elements(By.CSS_SELECTOR, "#whereWeWorkList li p")
                    content = "; ".join([item.text.strip() for item in list_items if item.text.strip()])
                
                # Store all sections, not just specific ones
                if content:  # Only add if we found some content
                    info[header] = content
                    self.debug_print(f"Extracted {header}: {content[:50]}...")
            except Exception as e:
                self.debug_print(f"Error extracting section: {str(e)}")
                continue
        
        return info

    def scrape_ein(self, ein):
        """Scrape information for a single EIN"""
        try:
            self.get_profile(ein)
            info = self.extract_info(ein)  # Pass the EIN to extract_info
            if info:
                self.data.append(info)
                self.debug_print(f"Successfully scraped data for EIN: {ein}")
            else:
                self.debug_print(f"No data found for EIN: {ein}")
        except Exception as e:
            self.debug_print(f"Error scraping EIN {ein}: {str(e)}")

    def save_to_csv(self, filename="guidestar_data.csv"):
        """Save scraped data to CSV file"""
        if not self.data:
            self.debug_print("No data to save")
            return

        # Create output directory if it doesn't exist
        os.makedirs(OUTPUT_DIR, exist_ok=True)
        
        # Create DataFrame and reorder columns to put EIN first
        df = pd.DataFrame(self.data)
        if 'EIN' in df.columns:
            cols = ['EIN'] + [col for col in df.columns if col != 'EIN']
            df = df[cols]
        
        # Save to CSV
        output_path = os.path.join(OUTPUT_DIR, filename)
        df.to_csv(output_path, index=False)
        self.debug_print(f"Data saved to {output_path}")

    def close(self):
        """Close the browser"""
        if hasattr(self, 'driver'):
            self.driver.quit()

def main():
    # Load EINs from CSV
    eins = load_eins()
    if not eins:
        print("No EINs found in input CSV. Exiting...")
        return

    scraper = GuideStarScraper(debug=True)
    try:
        # Start with manual login
        if not scraper.login():
            print("Failed to access GuideStar. Please check your login.")
            return
            
        # Process EINs in batches
        for i in range(0, len(eins), BATCH_SIZE):
            batch = eins[i:i + BATCH_SIZE]
            print(f"\nProcessing batch {i//BATCH_SIZE + 1} of {(len(eins) + BATCH_SIZE - 1)//BATCH_SIZE}")
            
            for ein in batch:
                scraper.scrape_ein(ein)
            
            # Save progress after each batch
            scraper.save_to_csv()
            print(f"Progress saved after batch {i//BATCH_SIZE + 1}")
            
    finally:
        scraper.close()

if __name__ == "__main__":
    main()
