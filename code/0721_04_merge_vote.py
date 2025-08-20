# read 'pew_nigeria_with_aid.csv' and 'level-2.csv'
# find the 'name_0' equal 'Nigeria' in 'level-2.csv'
# merge the two dataframes on 'name_0=Nigeria' and 'year'
# add new columns to the 'pew_nigeria_with_aid.csv' dataframe, which comes from 'ChinaAgree', 'USAgree' columns in 'level-2.csv'
import pandas as pd
# read the data
pew_nigeria = pd.read_csv('pew_nigeria_with_aid_2007-2019.csv')
level_2 = pd.read_csv('level-2.csv')
# filter the level_2 dataframe for name_0 == 'Nigeria'
level_2_nigeria = level_2[level_2['name_0'] == 'Nigeria']
# merge the two dataframes on 'year'
merged_df = pd.merge(pew_nigeria, level_2_nigeria[['year', 'ChinaAgree', 'USAgree', 'CHN_comm_added', 'WB_comm_added']], on='year', how='left')
# rename the columns
merged_df.rename(columns={'ChinaAgree': 'China_Agree', 'USAgree': 'US_Agree', 'CHN_comm_added': 'CHN_KEN', 'WB_comm_added': 'WB_KEN'}, inplace=True)
# save the merged dataframe to a new csv file
merged_df.to_csv('pew_nigeria_with_aid_vote_2007-2019.csv', index=False)
# print the first few rows of the merged dataframe
