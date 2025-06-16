import sqlite3
import os
import re
from dataclasses import dataclass, asdict
from typing import List, Optional
import csv

# Parse the apkg file
os.chdir('/home/shane/Desktop/files/current-projects/gaelLearnR/GaelLearnR/')

## Below - uncomment to unzip and extract the SQL database
# apkg_path = 'inst/extdata/Cpla_Mle_Focal.apkg'
# extract_to = 'inst/extdata/Cpla_Mle_Focal_extracted'

# with zipfile.ZipFile(apkg_path, 'r') as zip_ref:
#    zip_ref.extractall(extract_to)


def load_your_anki_data(path):
    """
    Load data from Anki SQL database
        
    Args:
        path: Path to the Anki .anki2 database file
    
    Returns:
        List of parsed Anki .anki2 database file
    """
     
    # Load from SQLite database
    conn = sqlite3.connect(path)
    cursor = conn.cursor()
    cursor.execute("SELECT flds FROM notes")  # Adjust query as needed
    rows = cursor.fetchall()
    conn.close()
    print(f"Loaded {len(rows)} rows from database")
    return parse_anki_database(rows)



def parse_anki_database(rows, clean_html: bool = True):
    """
    Parse Anki database row tuple into structured data
    
    Args:
        rows: Tuple containing the card data as a single string
        
    Returns:
        AnkiCard object with parsed fields
    """
    # Split by the field separator \x1f
    fields = [row[0].split('\x1f') for row in rows]

    # Set labels for each item entry
    field_names = ["ga", "pos", "en", "gender", "genitiveVN", "NTeanglann", "NFocloir", "Frequency"]

    processed_data = []
    for row in fields:
        row_dict = dict(zip(field_names, row))
        #if clean_html ==True:
           # row_dict["en"] = clean_html_content(row_dict["en"])  # uncomment to remove HTML tags
           # row_dict["genitiveVN"] = clean_html_content(row_dict["genitiveVN"]) # uncomment to remove HTML tags
        processed_data.append(row_dict)

    return processed_data


def clean_html_content(text: str) -> str:
    """
    Clean HTML tags and format content for better readability
    """
    if not text:
        return ""
    
    # Replace common HTML tags with readable equivalents
    text = re.sub(r'<br\s*/?>', ' | ', text)  # Use | for line breaks in CSV
    text = re.sub(r'<small>(.*?)</small>', r'(\1)', text)
    text = re.sub(r'<b>(.*?)</b>', r'**\1**', text)
    text = re.sub(r'<aside>(.*?)</aside>', r'[Note: \1]', text)
    text = re.sub(r'<li>', '• ', text)
    text = re.sub(r'</li>', '', text)
    text = re.sub(r'<[^>]+>', '', text)  # Remove any remaining HTML tags
    
    # Clean up whitespace
    text = re.sub(r'\s+', ' ', text)  # Multiple spaces to single space
    text = re.sub(r'\n\s*\n', ' | ', text)  # Multiple newlines to separator
    
    return text.strip()



# Run
data_loadOut=load_your_anki_data('inst/extdata/Cpla_Mle_Focal_extracted/collection.anki21')

# Writing to CSV
with open('inst/extdata/anki.csv', mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames = ["ga", "pos", "en", "gender", "genitiveVN", "NTeanglann", "NFocloir", "Frequency"])
    writer.writeheader()  # Write header row
    writer.writerows(data_loadOut)  # Write data rows
