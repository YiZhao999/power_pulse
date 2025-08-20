# read the 'aid_nigeria_aggregated_by_region.csv' file and merge it with the 'pew_nigeria.csv' file
# create new columns in the 'pew_nigeria.csv' file for row, the column names should be: CHN_comm,CHN_dummy_comm,WB_comm,WB_dummy_comm,WB_disb,WB_dummy_disb
# the merging should be done by 'region' and 'year'
# for each row in the 'pew_nigeria.csv' file, if the 'region' and 'year' match with the 'aid_nigeria_aggregated_by_region.csv' file, the new columns should be filled with the corresponding values
import pandas as pd


def merge_aid_with_pew(aid_df, pew_df):
    # Merge the two DataFrames on 'region' and 'year'
    merged_df = pew_df.merge(aid_df, on=['region', 'year'], how='left')

    # Fill NaN values with 0 for the new columns
    new_columns = ['CHN_comm', 'CHN_dummy_comm', 'WB_comm', 'WB_dummy_comm', 'WB_disb', 'WB_dummy_disb']
    merged_df[new_columns] = merged_df[new_columns].fillna(0)

    return merged_df
# Load the CSV files


aid_df = pd.read_csv('aid_nigeria_aggregated_2007-2019.csv')
pew_df = pd.read_csv('transformed_nigeria_data_2007-2019.csv')
# Merge the DataFrames
merged_df = merge_aid_with_pew(aid_df, pew_df)
# Save the merged DataFrame to a new CSV file
merged_df.to_csv('pew_nigeria_with_aid_2007-2019.csv', index=False)
