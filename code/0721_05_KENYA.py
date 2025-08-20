# read the file 'pew_kenya_with_aid_merged.csv' and print all the names under column 'region'
import pandas as pd

df = pd.read_csv('pew_kenya_with_aid_vote_2007-2019.csv')

# print(df['region'].unique())

def map_region(region):
    region = region.strip().upper()
    if region in ['NAIROBI']:
        return 'Nairobi'
    elif region in ['CENTRAL']:
        return 'Central'
    elif region in ['COAST']:
        return 'Coast'
    elif region in ['EASTERN']:
        return 'Eastern'
    elif region in ['NYANZA']:
        return 'Nyanza'
    elif region in ['RIFT VALLEY']:
        return 'Rift Valley'
    elif region in ['WESTERN']:
        return 'Western'
    elif region in ['NORTH EASTERN']:
        return 'North Eastern'
    else:
        return 'Other'

df['region'] = df['region'].apply(map_region)
# print the unique regions after regrouping
# save the new dataframe to a new csv file
print(df['region'].unique())
df.to_csv('kenya_final.csv', index=False)