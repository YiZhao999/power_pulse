import pandas as pd

# # Read and filter aid.csv
# aid_df = pd.read_csv('aid.csv', usecols=['year', 'gid_0', 'name_0', 'name_1', 'CHN_comm', 'USA_comm'])
# aid_df.to_csv('aid_filtered.csv', index=False)

# Read the filtered aid data
aid_df = pd.read_csv('aid_filtered.csv')

# Aggregate by year and name_0 for CHN_comm and USA_comm (using sum as example)
agg_df = aid_df.groupby(['year', 'name_0'])[['CHN_comm', 'USA_comm']].sum().reset_index()

# Save the aggregated data
agg_df.to_csv('aid_aggregated.csv', index=False)

# # Read and filter voting.csv
# voting_df = pd.read_csv('voting.csv', usecols=['session', 'iso3c', 'Countryname', 'USAgree', 'ChinaAgree'])
# voting_df.to_csv('voting_filtered.csv', index=False)