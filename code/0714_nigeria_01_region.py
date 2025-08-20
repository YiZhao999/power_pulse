# read the 'pew.csv' file and group the data by region and year for Nigeria
# when grouping, average the value for the column named 'satisfaction', 'fav_us', 'fav_China' and 'econ'
# form a new dataframe with the columns 'region', 'year', 'satisfaction', 'fav_us', 'fav_China', 'econ'
import pandas as pd


df = pd.read_csv('pew.csv')
df = df.groupby(['region', 'year']).agg({
        'satisfaction': 'mean',
        'fav_us': 'mean',
        'fav_China': 'mean',
        'econ': 'mean'
    }).reset_index()
df = df.rename(columns={
    'satisfaction': 'avg_satisfaction',
    'fav_us': 'avg_fav_us',
    'fav_China': 'avg_fav_China',
    'econ': 'avg_econ'
})
df.to_csv('nigeria_region.csv', index=False)



