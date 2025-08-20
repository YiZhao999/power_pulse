import pandas as pd


def aggregate_by_region(df):
    # Group by 'region' and 'year', then sum the relevant columns
    aggregated_df = df.groupby(['region', 'year']).agg({
        'CHN_comm': 'sum',
        'CHN_dummy_comm': 'sum',
        'CHN_comm_nominal': 'sum',
        'WB_comm': 'sum',
        'WB_comm_nominal': 'sum',
        'WB_dummy_comm': 'sum',
        'WB_disb': 'sum',
        'WB_dummy_disb': 'sum',
        'WB_disb_nominal': 'sum',
        'USA_comm': 'sum',
        'USA_comm_nominal': 'sum',
        'USA_disb': 'sum',
        'USA_disb_nominal': 'sum',
        'USA_projectscount': 'sum'
    }).reset_index()

    return aggregated_df
# Load the CSV file


df = pd.read_csv('aid_nigeria_region_2007-2019.csv')
# Aggregate the data by region
aggregated_df = aggregate_by_region(df)
# Save the aggregated DataFrame to a new CSV file
aggregated_df.to_csv('aid_nigeria_aggregated_2007-2019.csv', index=False)