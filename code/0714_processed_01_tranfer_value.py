# read all the csv files in the same folder as the python file, these files have the same dataframe structure
# process the csv files following the rules:
# for first column in the dataframe, transfer the value to a new column named 'satisfaction_value' following the rules:
# very satisfied 	1
# satisfied 	2
# somewhat satisfied 	3
# not too satisfied 	4
# dissatisfied 	5
# not at all satisfied 	6
# for second and third column, transfer the value to a new column named 'fav_us_value' and 'fav_China_value' following the rules:
# very favorable	1
# somewhat favorable	2
# somewhat unfavorable	3
# very unfavorable 	4
# for the fourth column, transfer the value to a new column named 'econ_value' following the rules:
# very good	1
# somewhat good	2
# somewhat bad	3
# very bad	4
# all the text in the columns should be case insensitive, so 'Very Satisfied' and 'very satisfied' are different values
import pandas as pd
import os
from pathlib import Path
# Get the current directory
current_dir = Path(__file__).parent
# List all CSV files in the current directory
csv_files = list(current_dir.glob("*.csv"))
# Define the mapping rules
s = {
    'satisfaction_value': {
        'very satisfied': 1,
        'satisfied': 2,
        'somewhat satisfied': 3,
        'not too satisfied': 4,
        'dissatisfied': 5,
        'not at all satisfied': 6
    },
    'fav_us_value': {
        'very favorable': 1,
        'somewhat favorable': 2,
        'somewhat unfavorable': 3,
        'very unfavorable': 4
    },
    'fav_China_value': {
        'very favorable': 1,
        'somewhat favorable': 2,
        'somewhat unfavorable': 3,
        'very unfavorable': 4
    },
    'econ_value': {
        'very good': 1,
        'somewhat good': 2,
        'somewhat bad': 3,
        'very bad': 4
    }
}
# Process each CSV file
for csv_file in csv_files:
    # Read the CSV file
    df = pd.read_csv(csv_file)

    # Transfer values for the first column
    df['satisfaction_value'] = df.iloc[:, 0].str.lower().map(s['satisfaction_value'])

    # Transfer values for the second and third columns
    df['fav_us_value'] = df.iloc[:, 1].str.lower().map(s['fav_us_value'])
    df['fav_China_value'] = df.iloc[:, 2].str.lower().map(s['fav_China_value'])

    # Transfer values for the fourth column
    df['econ_value'] = df.iloc[:, 3].str.lower().map(s['econ_value'])

    # Save the processed DataFrame back to a new CSV file
    output_file = current_dir / f"processed_{csv_file.name}"
    df.to_csv(output_file, index=False)
    print(f"Processed {csv_file.name} and saved to {output_file.name}")
# End of the script



