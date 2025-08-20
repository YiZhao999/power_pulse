import pandas as pd

# Read the file with a specified encoding to handle non-UTF-8 characters
try:
    df = pd.read_csv('nigeria_final_region.csv', encoding='ISO-8859-1')

    # Normalize column names to lowercase
    df.columns = df.columns.str.lower()

    # Ensure the 'party' column values are strings to avoid TypeError
    df['party'] = df['party'].astype(str)

    # Create a new column with the mapping rule
    df['party_category'] = df['party'].apply(
        lambda x: 'APC' if 'APC' in x else ('PDP' if 'PDP' in x else 'N/A')
    )

    # Create two new columns with 0/1 values
    df['is_apc'] = df['party'].apply(lambda x: 1 if 'APC' in x else 0)
    df['is_pdp'] = df['party'].apply(lambda x: 1 if 'PDP' in x else 0)
    df['post_2015'] = df['year'].apply(lambda x: 1 if x > 2015 else 0)

    # Save the updated DataFrame to a new CSV file
    df.to_csv('nigeria_1.csv', index=False)
except UnicodeDecodeError as e:
    print(f"Error reading the file: {e}")