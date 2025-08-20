# read the csv files ended with '_avg.csv' and merge them into a single dataframe, where country name is still the index
# when merging the dataframes, the columns are renamed to include the suffix of the original file
import pandas as pd
import glob
import os


def merge_dataframes():
    # Get all csv files in the current directory that end with '_avg.csv'
    csv_files = glob.glob('*_avg.csv')

    # Initialize an empty list to hold dataframes
    dataframes = []

    # Loop through each file and read it into a dataframe
    for file in csv_files:
        df = pd.read_csv(file, index_col=0)  # Use the first column as index (country names)
        suffix = os.path.splitext(file)[0].replace('_avg', '')  # Get the suffix from the filename
        df.columns = [f"{col}_{suffix}" for col in df.columns]  # Rename columns to include the suffix
        dataframes.append(df)

    # Merge all dataframes on the index (country names)
    merged_df = pd.concat(dataframes, axis=1)

    return merged_df


if __name__ == "__main__":
    merged_df = merge_dataframes()
    # Save the merged dataframe to a new CSV file
    merged_df.to_csv('merged_data.csv')
    print("Dataframes merged and saved to 'merged_data.csv'.")