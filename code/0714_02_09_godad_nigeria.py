# read the csv file in the 'csv' directory, named 'adm1pan_china_india_wb.csv'
# substracting certain rows and columns to a new dataframe according to the following rules:
# select rows where the 'name_0' column is 'Nigeria'
# select the rows where the 'year' column is 2012 to 2020
# select the columns 'gid_1', 'name_1', 'CHN_comm', 'CHN_dummy_comm', 'WB_comm', 'WB_dummy_comm', 'WB_disb', 'WB_dummy_disb'
import pandas as pd

# Step 1: Read the CSV file
file_path = 'adm1pan_china_india_wb.csv'
df = pd.read_csv(file_path)

# Step 2: Filter rows where 'name_0' is 'Nigeria'
df_nigeria = df[df['name_0'] == 'Nigeria']

# Step 3: Filter rows where 'year' is between 2012 and 2020
df_nigeria_filtered = df_nigeria[(df_nigeria['year'] >= 2006) & (df_nigeria['year'] <= 2020)]

# Step 4: Select specific columns
columns_to_select = ['gid_1', 'name_1', 'year', 'CHN_comm', 'CHN_dummy_comm', 'WB_comm', 'WB_dummy_comm', 'WB_disb', 'WB_dummy_disb']
df_result = df_nigeria_filtered[columns_to_select]
# Optionally save to a new CSV file
df_result.to_csv('nigeria_aid_2007-2019.csv', index=False)
# print all the items in the 'name_1' column, that are not repeated
unique_names = df_result['name_1'].unique()
print("Unique names in 'name_1' column:")
for name in unique_names:
    print(name)
