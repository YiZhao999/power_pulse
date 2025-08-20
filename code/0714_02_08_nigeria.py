import pandas as pd
import os

# Define folder path and column mapping
folder_path = 'original'
column_mapping = {
    '2007': {'satisfaction': 'Q7', 'fav_us': 'Q16a', 'fav_China': 'Q16c', 'econ': 'Q11', 'weight': 'weight', 'region': 'Q130KEN', 'party': None},
    '2008': {'satisfaction': 'Q2', 'fav_us': 'Q10a', 'fav_China': 'Q10c', 'econ': 'Q4', 'weight': 'weight', 'region': 'q98KEN', 'party': None},
    '2009': {'satisfaction': 'Q4', 'fav_us': 'Q11a', 'fav_China': 'Q11c', 'econ': 'Q5', 'weight': 'weight', 'region': 'Q107KEN', 'party': 'Q103KEN'},
    '2010': {'satisfaction': 'Q5', 'fav_us': 'Q7a', 'fav_China': 'Q7c', 'econ': 'Q12', 'weight': 'weight', 'region': 'Q142KEN', 'party': 'Q138KEN'},
    '2011': {'satisfaction': 'Q2', 'fav_us': 'Q3a', 'fav_China': 'Q3c', 'econ': 'Q4', 'weight': 'weight', 'region': 'Q135KEN', 'party': 'Q129KEN'},
    '2013': {'satisfaction': 'Q1', 'fav_us': 'Q9a', 'fav_China': 'Q9c', 'econ': 'Q4', 'weight': 'weight', 'region': 'Q207KEN', 'party': 'Q190KEN'},
    '2014': {'satisfaction': 'Q5', 'fav_us': 'Q15a', 'fav_China': 'Q15c', 'econ': 'Q9', 'weight': 'weight', 'region': 'Q175KEN', 'party': 'Q158KEN'},
    '2015': {'satisfaction': 'Q2', 'fav_us': 'Q12a', 'fav_China': 'Q12c', 'econ': 'Q3', 'weight': 'weight', 'region': 'Q213KEN', 'party': 'Q182KEN'},
    '2016': {'satisfaction': 'Q2', 'fav_us': 'Q10a', 'fav_China': 'Q10c', 'econ': 'Q3', 'weight': 'weight', 'region': 'QS5KEN', 'party': 'Q131KEN'},
    '2017': {'satisfaction': 'country_satis', 'fav_us': 'fav_us', 'fav_China': 'fav_China', 'econ': 'econ_sit', 'weight': 'weight', 'region': 'QS5KEN', 'party': 'd_ptyid_proximity_kenya'},
    '2018': {'satisfaction': 'country_satis', 'fav_us': 'fav_us', 'fav_China': 'fav_China', 'econ': 'econ_sit', 'weight': 'weight', 'region': 'QS5KEN', 'party': 'd_ptyid_proximity_kenya'},
    '2019': {'satisfaction': 'country_satis', 'fav_us': 'fav_us', 'fav_China': 'fav_China', 'econ': 'econ_sit', 'weight': 'weight', 'region': 'QS5KEN', 'party': 'D_PTYID_PROXIMITY_KENYA'}
}

# Initialize an empty list to store DataFrames
dataframes = []

# Iterate through the files
for year in range(2007, 2020):
    if year in [2012]:  # Skip years without valid data for Nigeria
        print(f"Skipping year {year} due to missing or invalid data.")
        continue

    file_name = f"{year}.csv"
    file_path = os.path.join(folder_path, file_name)

    if not os.path.exists(file_path):
        print(f"File not found: {file_path}, skipping.")
        continue

    # Read the CSV file
    df = pd.read_csv(file_path, low_memory=False)

    # Normalize column names
    df.columns = df.columns.str.strip().str.lower()

    # Remove duplicate columns if any
    if df.columns.duplicated().any():
        print(f"Duplicate columns found in {file_name}: {df.columns[df.columns.duplicated()].tolist()}")
        df = df.loc[:, ~df.columns.duplicated()]

    # Find 'country' column (case-insensitive match)
    country_col = next((col for col in df.columns if col.lower() == 'country'), None)
    if not country_col:
        print(f"'country' column not found in {file_name}, skipping.")
        continue

    # Keep only Nigeria rows
    df = df[df[country_col].str.strip().str.lower() == 'kenya']

    if df.empty:
        print(f"No Nigeria data in {file_name}, skipping.")
        continue

    # Add year column
    df['year'] = year

    # Get mapping for this year
    mapping = column_mapping[str(year)]

    # Check missing columns from mapping
    missing_columns = [
        v.lower() for v in mapping.values() if v and v.lower() not in df.columns
    ]
    if missing_columns:
        print(f"Missing columns in {file_name}: {missing_columns}, skipping.")
        continue

    # Rename columns according to mapping
    rename_dict = {v.lower(): k for k, v in mapping.items() if v}
    df = df.rename(columns=rename_dict)

    # Drop duplicates after renaming (important!)
    if df.columns.duplicated().any():
        print(f"Duplicate columns after renaming in {file_name}: {df.columns[df.columns.duplicated()].tolist()}")
        df = df.loc[:, ~df.columns.duplicated()]

    # Ensure 'party' column exists
    if 'party' not in df.columns:
        df['party'] = pd.NA

    # Select columns safely
    cols_to_select = list(mapping.keys()) + ['year']
    df = df[[col for col in cols_to_select if col in df.columns]]

    # Append processed DataFrame
    dataframes.append(df)

# Merge all dataframes if any
if dataframes:
    merged_df = pd.concat(dataframes, ignore_index=True)
    merged_df.to_csv('kenya_2007-2019.csv', index=False)
    print(f"Saved merged file with {len(merged_df)} rows.")
else:
    print("No valid data processed.")