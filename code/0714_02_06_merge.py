# merge all the csv files in the folder 'processed' into one csv file, the new dataframe has 7 columns in total. where 'year' is the csv file name without the extension. the mapping rule for 'satisfaction', 'fav_us', 'fav_China', 'econ' , 'country' and 'weight' is as follows. '/' means the column is not available in that year. then just leave them blank in the new dataframe. make each mapping rule case indifferent
# year 	satisfaction 	fav_us	fav_China	econ	country	weight
# 2004	Q1	Q2a	/	/	country	weight
# 2005	Q4	Q5a	Q5c	/	country	weight
# 2006	Q1	Q2a	Q2c	/	country	weight
# 2007	Q7	Q16a	Q16c	Q11	country	weight
# 2008	Q2	Q10a	Q10c	Q4	country	weight
# 2009	Q4	Q11a	Q11c	Q5	country	weight
# 2010	Q5	Q7a	Q7c	Q12	country	weight
# 2011	Q2	Q3a	Q3c	Q4	country	weight
# 2012	Q2	Q8a	Q8c	Q14	country	weight
# 2013	Q1	Q9a	Q9c	Q4	country	weight
# 2014	Q5	Q15a	Q15c	Q9	country	weight
# 2015	Q2	Q12a	Q12c	Q3	country	weight
# 2016	Q2	Q10a	Q10c	Q3	country	weight
# 2017	country_satis	fav_us	fav_China	econ_sit	country	weight
# 2018	country_satis 	fav_us	fav_China	econ_sit 	country	weight
# 2019	country_satis 	fav_us	fav_China	econ_sit	country	weight
# 2020	satisfied_democracy 	fav_us	fav_China	econ_sit 	country	weight
# 2021	satisfied_democracy 	fav_us	fav_China	econ_sit	country	weight
# 2022	satisfied_democracy 	fav_us	fav_China	econ_sit 	country	weight
# 2023	satisfied_democracy 	fav_us	fav_China	econ_sit	country	weight
import pandas as pd
import os


def merge_csv_files(input_folder, output_file, column_mapping):
    """
    Merge all CSV files in the input folder into one dataframe.

    :param input_folder: Path to the folder containing processed CSV files.
    :param output_file: Path to save the merged CSV file.
    :param column_mapping: Dictionary mapping years to target column names.
    """
    merged_data = []

    for file_name in os.listdir(input_folder):
        if file_name.endswith('.csv'):
            # Extract the year from the file name
            year = file_name.split('_')[0]
            try:
                year = int(year)  # Convert year to integer
            except ValueError:
                print(f"Invalid year format in file name: {file_name}. Skipping...")
                continue

            input_path = os.path.join(input_folder, file_name)

            # Read the CSV file
            df = pd.read_csv(input_path)
            df.columns = [col.lower() for col in df.columns]

            # Map columns to target names
            target_columns = column_mapping.get(year, [])
            new_df = pd.DataFrame()
            for target_col, original_col in zip(['satisfaction', 'fav_us', 'fav_China', 'econ', 'country', 'weight'], target_columns):
                if original_col != '/':
                    new_df[target_col] = df[original_col.lower()] if original_col.lower() in df.columns else None
                else:
                    new_df[target_col] = None

            # Add year column
            new_df['year'] = year

            # Append the dataframe to the merged data list
            merged_data.append(new_df)

    # Concatenate all dataframes
    merged_df = pd.concat(merged_data, ignore_index=True)

    # Save the merged dataframe to a CSV file
    merged_df.to_csv(output_file, index=False)
    print(f"Merged data saved to {output_file}")


# Define column mapping for each year
column_mapping = {
    2004: ['Q1', 'Q2a', '/', '/', 'country', 'weight'],
    2005: ['Q4', 'Q5a', 'Q5c', '/', 'country', 'weight'],
    2006: ['Q1', 'Q2a', 'Q2c', '/', 'country', 'weight'],
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
input_folder = 'processed'
output_file = 'merged.csv'
merge_csv_files(input_folder, output_file, column_mapping)
