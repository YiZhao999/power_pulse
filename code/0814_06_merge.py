import pandas as pd


def merge_aid_with_pew(aid_df, pew_df):
    # Merge the two DataFrames on 'region' and 'year'
    merged_df = pew_df.merge(aid_df, on=['region', 'year'], how='left')

    # Fill NaN values with 0 for the new columns
    new_columns = ['CHN_comm', 'CHN_dummy_comm', 'CHN_comm_nominal', 'WB_comm', 'WB_comm_nominal', 'WB_dummy_comm', 'WB_disb', 'WB_dummy_disb', 'WB_disb_nominal', 'USA_comm', 'USA_comm_nominal', 'USA_disb', 'USA_disb_nominal', 'USA_projectscount']
    merged_df[new_columns] = merged_df[new_columns].fillna(0)

    return merged_df
# Load the CSV files


aid_df = pd.read_csv('aid_nigeria_aggregated_2007-2019.csv')
pew_df = pd.read_csv('nigeria_1.csv')
# Merge the DataFrames
merged_df = merge_aid_with_pew(aid_df, pew_df)
# Save the merged DataFrame to a new CSV file
merged_df.to_csv('pew_nigeria_with_aid_2007-2019.csv', index=False)