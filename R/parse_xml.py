import xml.etree.ElementTree as ET
import pandas as pd
import time

# Parse the XML file
os.chdir('/home/shane/Desktop/files/current-projects/gaelLearnR/')

tree = ET.parse('inst/extdata/25.04.01-tearma.ie-concepts.tbx')
root= tree.getroot()


# Get all termEntries
term_entries = root.findall(".//termEntry")
total_entries = len(term_entries)

print(f"Total entries to process: {total_entries}")

# Prepare storage
rows = []
start_time = time.time()

# Process all entries
for i, term_entry in enumerate(term_entries, start=1):
    term_id = term_entry.attrib.get("id")
    subject_el = term_entry.find("descrip[@type='subjectField']")
    subject_text = subject_el.text if subject_el is not None else None

    row = {
        "id": term_id,
        "subjectField": subject_text,
        "en_term": None,
        "ga_term": None,
        "en_pos": None,
        "ga_pos": None,
        "en_explanation": None,
        "ga_explanation": None,
        "en_example": None,
        "ga_example": None,
    }

    for lang_set in term_entry.findall("langSet"):
        lang = lang_set.attrib.get("{http://www.w3.org/XML/1998/namespace}lang")

        if lang not in ["en", "ga"]:
            continue  # Ignore other languages

        term_el = lang_set.find(".//term")
        pos_el = lang_set.find(".//termNote[@type='partOfSpeech']")
        expl_el = lang_set.find("descrip[@type='explanation']")
        ex_el = lang_set.find(".//descrip[@type='example']")

        row[f"{lang}_term"] = term_el.text if term_el is not None else None
        row[f"{lang}_pos"] = pos_el.text if pos_el is not None else None
        row[f"{lang}_explanation"] = expl_el.text if expl_el is not None else None
        row[f"{lang}_example"] = ex_el.text if ex_el is not None else None

    rows.append(row)

    # Progress message every 200 entries
    if i % 200 == 0 or i == total_entries:
        elapsed = time.time() - start_time
        avg_time = elapsed / i
        est_total = avg_time * total_entries
        est_remaining = est_total - elapsed
        print(f"Processed {i}/{total_entries} entries "
              f"({i/total_entries:.1%}). "
              f"Estimated time left: {est_remaining:.1f} seconds.")

# Convert to DataFrame
df = pd.DataFrame(rows)

# Optional: Save
df.to_csv("full_terminology_table.csv", index=False)
print("✅ Done. Output saved to full_terminology_table.csv")
