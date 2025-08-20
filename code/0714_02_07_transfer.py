import pandas as pd
import numpy as np

def reverse_map_values(input_file, output_file):
    """
    Map text in specific columns back to corresponding numbers based on predefined rules.

    :param input_file: Path to the input CSV file.
    :param output_file: Path to save the updated CSV file.
    """
    # Define reversed mapping rules with lowercase keys
    satisfaction_mapping = {
        "very satisfied": 1,
        "satisfied": 2,
        "somewhat satisfied": 3,
        "not too satisfied": 4,
        "dissatisfied": 5,
        "not at all satisfied": 6
    }
    fav_us_mapping = {
        "very favorable": 1,
        "somewhat favorable": 2,
        "somewhat unfavorable": 3,
        "very unfavorable": 4
    }
    fav_China_mapping = {
        "very favorable": 1,
        "somewhat favorable": 2,
        "somewhat unfavorable": 3,
        "very unfavorable": 4
    }
    econ_mapping = {
        "very good": 1,
        "somewhat good": 2,
        "somewhat bad": 3,
        "very bad": 4
    }

    # Read the input CSV file
    df = pd.read_csv(input_file)

    # Apply reversed mappings to the specified columns
    df['satisfaction'] = df['satisfaction'].astype(str).str.lower().map(satisfaction_mapping).fillna(np.nan)
    df['fav_us'] = df['fav_us'].astype(str).str.lower().map(fav_us_mapping).fillna(np.nan)
    df['fav_China'] = df['fav_China'].astype(str).str.lower().map(fav_China_mapping).fillna(np.nan)
    df['econ'] = df['econ'].astype(str).str.lower().map(econ_mapping).fillna(np.nan)

    # Save the updated dataframe to a new CSV file
    df.to_csv(output_file, index=False)
    print(f"Updated data saved to {output_file}")

# Example usage
input_file = 'mapped.csv'
output_file = 'reversed_mapped.csv'
reverse_map_values(input_file, output_file)