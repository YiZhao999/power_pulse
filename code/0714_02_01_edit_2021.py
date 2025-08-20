import pandas as pd
import os


def add_country_column_to_2021():
    # Read the reference mapping file
    reference_df = pd.read_csv('reference.csv')

    # Read the 2021 CSV file
    df_2021 = pd.read_csv('2021.csv')

    # Initialize a new column for country
    df_2021['country'] = None

    # Iterate through each row in the 2021 DataFrame
    for index, row in df_2021.iterrows():
        id_value = str(row['ID'])
        for _, ref_row in reference_df.iterrows():
            if id_value.startswith(str(ref_row['number'])) and len(id_value) == ref_row['digit']:
                df_2021.at[index, 'country'] = ref_row['country']
                break

    # Save the updated DataFrame back to the 2021 CSV file
    df_2021.to_csv('2021.csv', index=False)


if __name__ == "__main__":
    add_country_column_to_2021()
    print("Country column has been added to '2021.csv'.")
