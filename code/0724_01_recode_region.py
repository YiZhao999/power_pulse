# read the '0721.csv' file and check all the variables under region column
import pandas as pd
df = pd.read_csv('0722.csv')
region_map = {
    'South East': 'South East',
    'South South': 'South South',
    'South West': 'South West',
    'North Central': 'North Central',
    'Lagos': 'South West',
    'North West': 'North West',
    'North East': 'North East',
    'South West (Lagos excl)': 'South West',
    ' South South': 'South South',
    ' North East': 'North East',
    ' North Central': 'North Central',
    ' South West': 'South West',
    ' South East': 'South East',
    ' Lagos': 'South West',
    ' North West': 'North West',
    'Middle Belt / North Central': 'North Central',
    'South': 'South South'
}

# Apply to your DataFrame
df['region_mapped'] = df['region'].str.strip().map(region_map)
# save into a new csv file
df.to_csv('0722_mapped_region.csv', index=False)
