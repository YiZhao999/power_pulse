# read the 'processed_2015_to_2023.csv' file
# firstly, calculate the average value for 'fav_us', 'fav_China' and 'econ' by country (indicated by 'country' column) and year (indicated by 'year' column)
# put the caculated values into a new dataframe following the panel data structure
import pandas as pd

# Read the processed data
df = pd.read_csv('processed_2015_to_2023.csv')

# Calculate the average values by country and year
df_avg = df.groupby(['country', 'year'])[['fav_us_value', 'fav_China_value', 'econ_value']].mean().reset_index()

# Rename the columns to match desired output
df_avg.rename(columns={
    'fav_us_value': 'fav_us',
    'fav_China_value': 'fav_China',
    'econ_value': 'econ'
}, inplace=True)


# Define weighted mean function
def weighted_mean(x):
    return (x['value'] * x['weight']).sum() / x['weight'].sum()


# Melt the dataframe to long format
df_melt = df.melt(id_vars=['country', 'year', 'weight'],
                  value_vars=['fav_us_value', 'fav_China_value', 'econ_value'],
                  var_name='variable', value_name='value')

# Group by and calculate weighted mean
df_weighted = df_melt.groupby(['country', 'year', 'variable']).apply(weighted_mean).reset_index(name='weighted_value')

# Pivot to wide format
df_panel = df_weighted.pivot(index=['country', 'year'], columns='variable', values='weighted_value').reset_index()

# Rename columns
df_panel.rename(columns={
    'fav_us_value': 'fav_us',
    'fav_China_value': 'fav_China',
    'econ_value': 'econ'
}, inplace=True)

# Save
df_panel.to_csv('processed_weighted_descriptive.csv', index=False)
# Save the result to a CSV file
df_avg.to_csv('processed_unweighted_descriptive.csv', index=False)


