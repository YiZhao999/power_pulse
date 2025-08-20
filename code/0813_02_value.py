# read the 'afro.csv' file
# transform the string values (all case insensitive) in the columns into value following the mapping:
# for 'Q4A' column,
# 1	very good
# 2	fairly good
# 3	neither good nor bad
# 4	fairly bad
# 5	very bad
# other are all considered as 'N/A'
# these string values from the original dataset are case insensitive
# for 'Q98H' and 'Q98I' columns,
# 1	help a lot
# 2	help somewhat
# 3	help a little bit
# 4	do nothing
# other are all considered as 'N/A'
# these string value from the original dataset are case insensitive
# for 'Q86' column, extract all the capital letters in the brackets (). and transform them into a string value, if there is no capital letter in the brackets, then it is considered as 'N/A'
# the extracted string value should only appear in the brackets, if there is no brackets, then it is considered as 'N/A'
# Import necessary libraries
import pandas as pd
import numpy as np
import re

# Read the `afro.csv` file
afro_df = pd.read_csv('afro.csv', low_memory=False)

# Define the mapping for 'Q4A' (case insensitive)
q4a_mapping = {
    'VERY GOOD': '1',
    'FAIRLY GOOD': '2',
    'NEITHER GOOD NOR BAD': '3',
    'FAIRLY BAD': '4',
    'VERY BAD': '5'
}

# Apply the mapping to 'Q4A' column
afro_df['Q4A'] = afro_df['Q4A'].astype(str).str.upper().map(q4a_mapping).fillna('N/A')

# Define the mapping for 'Q98H' and 'Q98I' (case insensitive)
q98_mapping = {
    'HELP A LOT': '1',
    'HELP SOMEWHAT': '2',
    'HELP A LITTLE BIT': '3',
    'DO NOTHING': '4'
}

# Apply the mapping to 'Q98H' and 'Q98I' columns
afro_df['Q98H'] = afro_df['Q98H'].astype(str).str.upper().map(q98_mapping).fillna('N/A')
afro_df['Q98I'] = afro_df['Q98I'].astype(str).str.upper().map(q98_mapping).fillna('N/A')

# Function to extract capital letters from brackets in 'Q86'
def extract_capital_letters(q86_value):
    if pd.isna(q86_value):
        return 'N/A'
    match = re.search(r'\(([^)]+)\)', q86_value)
    if match:
        capital_letters = ''.join([char for char in match.group(1) if char.isupper()])
        return capital_letters if capital_letters else 'N/A'
    return 'N/A'

# Apply the extraction function to 'Q86' column
afro_df['Q86'] = afro_df['Q86'].apply(extract_capital_letters)

# Save the transformed dataframe to a new CSV file
afro_df.to_csv('afro_transformed.csv', index=False)


