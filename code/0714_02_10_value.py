# read the 'merged_nigeria_data.csv' file
# transform the column content into value following the following rules:
# for the 'satisfaction' column,
# very satisfied 	1
# satisfied 	2
# somewhat satisfied 	3
# not too satisfied 	4
# dissatisfied 	5
# not at all satisfied 	6
# for the 'fav_us' and 'fav_China' column,
# very favorable	1
# somewhat favorable	2
# somewhat unfavorable	3
# very unfavorable 	4
# for the 'econ' column
# very good	1
# somewhat good	2
# somewhat bad	3
# very bad	4
import pandas as pd
import numpy as np
# Define the mapping for the columns
s = {
    'satisfaction': {
        'Very satisfied': 1,
        'Satisfied': 2,
        'Somewhat satisfied': 3,
        'Not too satisfied': 4,
        'Dissatisfied': 5,
        'Not at all satisfied': 6
    },
    'fav_us': {
        'Very favorable': 1,
        'Somewhat favorable': 2,
        'Somewhat unfavorable': 3,
        'Very unfavorable': 4
    },
    'fav_China': {
        'Very favorable': 1,
        'Somewhat favorable': 2,
        'Somewhat unfavorable': 3,
        'Very unfavorable': 4
    },
    'econ': {
        'Very good': 1,
        'Somewhat good': 2,
        'Somewhat bad': 3,
        'Very bad': 4
    }
}
# Read the CSV file
file_path = 'nigeria_2007-2019.csv'
df = pd.read_csv(file_path)
# Apply the mapping to the specified columns
for column, mapping in s.items():
    df[column] = df[column].map(mapping)
# Handle NaN values by replacing them with a default value (e.g., 0)
df.fillna(0, inplace=True)
# Save the transformed DataFrame to a new CSV file
output_file_path = 'transformed_nigeria_data_2007-2019.csv'
df.to_csv(output_file_path, index=False)
