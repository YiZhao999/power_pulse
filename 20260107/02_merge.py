import pandas as pd

# Read files
aid_df = pd.read_csv('aid_aggregated.csv')
voting_df = pd.read_csv('voting_filtered.csv')

# Filter aid_df for years 2000-2020
aid_df = aid_df[(aid_df['year'] >= 2000) & (aid_df['year'] <= 2020)]

# Filter voting_df for sessions 55-78 (previously 55-75)
voting_df = voting_df[(voting_df['session'] >= 55) & (voting_df['session'] <= 78)]

# Add year column to voting_df
voting_df['year'] = voting_df['session'] + 1945

# Merge on year and Countryname/name_0
merged_df = pd.merge(
    aid_df,
    voting_df,
    left_on=['year', 'name_0'],
    right_on=['year', 'Countryname'],
    how='inner'
)

# Select required columns
merged_df = merged_df[['year', 'Countryname', 'CHN_comm', 'USA_comm', 'ChinaAgree', 'USAgree']]

# Save to CSV
merged_df.to_csv('merged.csv', index=False)
