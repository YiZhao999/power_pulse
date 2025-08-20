# read all the csv file in the 'value' folder, from 'processed_2011_processed.csv' to 'processed_2023_processed.csv'
# substract columns named 'country', 'weight', 'satisfaction_value', 'fav_us_value', 'fav_China_value', 'econ_value' from the dataframe, merge them into another dataframe,
# add a new column named 'year' to the new dataframe, the value of the column is the year indicated from the csv file
# if the csv file is 'processed_2011_processed.csv', the value of the column is 2011, if the csv file is 'processed_2023_processed.csv', the value of the column is 2023
# append all the rows # to the new dataframe, and save the new dataframe to a csv file named 'processed_2011_to_2023.csv'
# only process file names from 'processed_2011_processed.csv' to 'processed_2023_processed.csv'
import pandas as pd
import os

# Define the folder path
folder_path = 'value'

# Initialize an empty list to store DataFrames
df_list = []

# Process files from 2011 to 2023
for year in range(2015, 2024):
    file_name = f'processed_{year}_processed.csv'
    file_path = os.path.join(folder_path, file_name)

    # Read the CSV file
    df = pd.read_csv(file_path)

    # Subset the necessary columns
    columns_to_keep = ['country', 'weight', 'satisfaction_value', 'fav_us_value', 'fav_China_value', 'econ_value']
    df_subset = df[columns_to_keep].copy()

    # Add the 'year' column
    df_subset['year'] = year

    # Append to the list
    df_list.append(df_subset)

# Concatenate all the dataframes
combined_df = pd.concat(df_list, ignore_index=True)

# Save the combined dataframe to a new CSV file
combined_df.to_csv('processed_2015_to_2023.csv', index=False)
