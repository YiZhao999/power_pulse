# read 'ken_r4_data.csv' and 'ken_r5_data_july_2015.csv' files
# take the columns named: 'Q98H', 'Q98I', 'Q85', 'Q86', 'REGION', 'Q4A' and all the values under the columns from the r4 file, capital insensitive
# create a column named 'year' and fill it with '2008'
# take the columns named: 'REGION', 'Q4A', 'Q89A', 'Q89B', 'Q3A', 'Q47A', 'Q47B' and all the values under the columns from the r5 file, capital insensitive
# create a column named 'year' and fill it with '2012'
# create a dataframe that combine the two dataframes, and create a new column named 'country' and fill it with 'Kenya'

# read 'nig_r5_data_july_2015.csv' file
# take the columns named: 'REGION', 'Q4A', 'Q89A', 'Q89B', 'Q3A', 'Q47A', 'Q47B' and all the values under the columns from the file, capital insensitive
# create a column named 'year' and fill it with '2012', create a new column named 'country' and fill it with 'Nigeria'

# combine the two dataframes for Kenya and Nigeria
# export the final dataframe to a csv file named 'afro_ken_nig.csv'
import pandas as pd

# Kenya
r4 = pd.read_csv('ken_r4_data.csv', low_memory=False)
r5 = pd.read_csv('ken_r5_data_july_2015.csv', low_memory=False)
r4_cols = ['Q98H', 'Q98I', 'Q85', 'Q86', 'REGION', 'Q4A']
r5_cols = ['REGION', 'Q4A', 'Q89A', 'Q89B', 'Q3A', 'Q47A', 'Q47B']
r4_cols_insensitive = [col for col in r4.columns if col.upper() in [c.upper() for c in r4_cols]]
r5_cols_insensitive = [col for col in r5.columns if col.upper() in [c.upper() for c in r5_cols]]
r4_selected = r4[r4_cols_insensitive].copy()
r5_selected = r5[r5_cols_insensitive].copy()
r4_selected['year'] = 2008
r5_selected['year'] = 2012
r4_selected['country'] = 'Kenya'
r5_selected['country'] = 'Kenya'
kenya_df = pd.concat([r4_selected, r5_selected], ignore_index=True)
# Nigeria
nig = pd.read_csv('nig_r5_data_july_2015.csv', low_memory=False)
nig_cols = ['REGION', 'Q4A', 'Q89A', 'Q89B', 'Q3A', 'Q47A', 'Q47B']
nig_cols_insensitive = [col for col in nig.columns if col.upper() in [c.upper() for c in nig_cols]]
nig_selected = nig[nig_cols_insensitive].copy()
nig_selected['year'] = 2012
nig_selected['country'] = 'Nigeria'
# Combine Kenya and Nigeria dataframes
final_df = pd.concat([kenya_df, nig_selected], ignore_index=True)
# Export to CSV
final_df.to_csv('afro_ken_nig.csv', index=False)



