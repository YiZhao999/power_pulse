# read the 'aid_nigeria_sub_with_region.csv' file and aggregate the data by 'region'
# # the aggregation should sum the 'value' column for each region
# the ultimate structure of the aggregated DataFrame should have the following columns:
# 'region', 'year', 'CHN_comm' ,'CHN_dummy_comm' ,'WB_comm', 'WB_dummy_comm', 'WB_disb', 'WB_dummy_disb'
import pandas as pd


def aggregate_by_region(df):
    # Group by 'region' and 'year', then sum the relevant columns
    aggregated_df = df.groupby(['region', 'year']).agg({
        'CHN_comm': 'sum',
        'CHN_dummy_comm': 'sum',
        'WB_comm': 'sum',
        'WB_dummy_comm': 'sum',
        'WB_disb': 'sum',
        'WB_dummy_disb': 'sum'
    }).reset_index()

    return aggregated_df
# Load the CSV file


df = pd.read_csv('aid_nigeria_region_2007-2019.csv')
# Aggregate the data by region
aggregated_df = aggregate_by_region(df)
# Save the aggregated DataFrame to a new CSV file
aggregated_df.to_csv('aid_nigeria_aggregated_2007-2019.csv', index=False)
