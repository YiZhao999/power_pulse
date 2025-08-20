# read the 'brazil.csv' file
# for column named 'satisfied_democracy', transfer the value to a new column named 'satisfaction_value' following the rules:
# very satisfied 	1
# satisfied 	2
# somewhat satisfied 	3
# not too satisfied 	4
# dissatisfied 	5
# not at all satisfied 	6
# for 'fav_us' and 'fav_china' column, transfer the value to a new column named 'fav_us_value' and 'fav_China_value' following the rules:
# very favorable	1
# somewhat favorable	2
# somewhat unfavorable	3
# very unfavorable 	4
# for the 'econ_sit' column, transfer the value to a new column named 'econ_value' following the rules:
# very good	1
# somewhat good	2
# somewhat bad	3
# very bad	4
# all the text in the columns should be case insensitive, so 'Very Satisfied' and 'very satisfied' are different values
# 'age' and 'weight' columns should be kept as they are
import pandas as pd
df = pd.read_csv('brazil.csv')
# Define the mapping for satisfaction values
satisfaction_mapping = {
    'very satisfied': 1,
    'satisfied': 2,
    'somewhat satisfied': 3,
    'not too satisfied': 4,
    'dissatisfied': 5,
    'not at all satisfied': 6
}
# Define the mapping for favorability values
favorability_mapping = {
    'very favorable': 1,
    'somewhat favorable': 2,
    'somewhat unfavorable': 3,
    'very unfavorable': 4
}
# Define the mapping for economic situation values
economic_mapping = {
    'very good': 1,
    'somewhat good': 2,
    'somewhat bad': 3,
    'very bad': 4
}
# Apply the mappings to create new columns
df['satisfaction_value'] = df['satisfied_democracy'].str.lower().map(satisfaction_mapping)
df['fav_us_value'] = df['fav_us'].str.lower().map(favorability_mapping)
df['fav_china_value'] = df['fav_china'].str.lower().map(favorability_mapping)
df['econ_value'] = df['econ_sit'].str.lower().map(economic_mapping)
# Select the relevant columns to keep
df = df[['age', 'weight', 'satisfaction_value', 'fav_us_value', 'fav_china_value', 'econ_value']]
# Save the modified dataframe to a new CSV file
df.to_csv('brazil_values.csv', index=False)
