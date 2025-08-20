import pandas as pd
import numpy as np

# Define the mapping function
def map_favorability(value):
    if value == 10:
        return 1
    elif value in [7, 8, 9]:
        return 2
    elif value in [4, 5, 6]:
        return 3
    elif value in [0, 1, 2, 3]:
        return 4
    else:
        return np.nan

# Read the CSV file
df = pd.read_csv('pew_nigeria_with_aid_2007-2019.csv')

# Ensure 'fav_us' and 'fav_china' columns are numeric
df['fav_us'] = pd.to_numeric(df['fav_us'], errors='coerce')
df['fav_china'] = pd.to_numeric(df['fav_china'], errors='coerce')

# Apply the mapping only for rows where 'survey' is 'afrobarometer'
df.loc[df['survey'] == 'afrobarometer', 'fav_us'] = df.loc[df['survey'] == 'afrobarometer', 'fav_us'].apply(map_favorability)
df.loc[df['survey'] == 'afrobarometer', 'fav_china'] = df.loc[df['survey'] == 'afrobarometer', 'fav_china'].apply(map_favorability)

# Save the updated DataFrame to a new CSV file
df.to_csv('updated_nigeria_2007-2019.csv', index=False)