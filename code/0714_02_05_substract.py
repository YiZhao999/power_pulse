import pandas as pd
import os


def process_csv_files(input_folder, output_folder, column_mapping):
    """
    Process CSV files by selecting specific columns and saving new dataframes.

    :param input_folder: Path to the folder containing original CSV files.
    :param output_folder: Path to the folder to save processed CSV files.
    :param column_mapping: Dictionary mapping years to column selections.
    """
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    for year, columns in column_mapping.items():
        file_name = f"{year}.csv"
        input_path = os.path.join(input_folder, file_name)
        output_path = os.path.join(output_folder, f"{year}_processed.csv")

        if not os.path.exists(input_path):
            print(f"File {file_name} not found in {input_folder}. Skipping...")
            continue

        # Read the CSV file
        df = pd.read_csv(input_path)
        df.columns = [col.lower() for col in df.columns]

        # Normalize column names and select required columns
        columns_lower = [col.lower() for col in columns]
        selected_columns = [col for col in columns_lower if col in df.columns]

        if len(selected_columns) != len(columns_lower):
            print(f"Missing columns in {file_name}. Skipping...")
            continue

        # Create a new dataframe with selected columns
        new_df = df[selected_columns]

        # Save the new dataframe to a CSV file
        new_df.to_csv(output_path, index=False)
        print(f"Processed {file_name} and saved to {output_path}")


# Define column mapping for each year
column_mapping = {
    2004: ['Q1', 'Q2a', 'country', 'weight'],
    2005: ['Q4', 'Q5a', 'Q5c', 'country', 'weight'],
    2006: ['Q1_country_satis', 'Q2a_fav_us', 'Q2c_fav_china', 'country', 'weight'],
    2007: ['Q7', 'Q16a', 'Q16c', 'Q11', 'country', 'weight'],
    2008: ['Q2', 'Q10a', 'Q10c', 'Q4', 'country', 'weight'],
    2009: ['Q4', 'Q11a', 'Q11c', 'Q5', 'country', 'weight'],
    2010: ['Q5', 'Q7a', 'Q7c', 'Q12', 'country', 'weight'],
    2011: ['Q2', 'Q3a', 'Q3c', 'Q4', 'country', 'weight'],
    2012: ['Q2', 'Q8a', 'Q8c', 'Q14', 'country', 'weight'],
    2013: ['Q1', 'Q9a', 'Q9c', 'Q4', 'country', 'weight'],
    2014: ['Q5', 'Q15a', 'Q15c', 'Q9', 'country', 'weight'],
    2015: ['Q2', 'Q12a', 'Q12c', 'Q3', 'country', 'weight'],
    2016: ['Q2', 'Q10a', 'Q10c', 'Q3', 'country', 'weight'],
    2017: ['country_satis', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight'],
    2018: ['country_satis', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight'],
    2019: ['country_satis', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight'],
    2020: ['satisfied_democracy', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight'],
    2021: ['satisfied_democracy', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight'],
    2022: ['satisfied_democracy', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight'],
    2023: ['satisfied_democracy', 'fav_us', 'fav_China', 'econ_sit', 'country', 'weight']
}

# Example usage
input_folder = 'original'
output_folder = 'processed'
process_csv_files(input_folder, output_folder, column_mapping)






