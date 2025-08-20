import pandas as pd
import os


def count_respondents_by_country():
    # Get the list of all CSV files in the current directory
    csv_files = [f for f in os.listdir('.') if f.endswith('.csv')]

    # Initialize an empty DataFrame to store the results
    country_counts = pd.DataFrame(columns=['year', 'country', 'count'])

    for file in csv_files:
        # Extract the year from the filename (assuming format like '2010.csv')
        year = file.split('.')[0]

        # Read the CSV file
        df = pd.read_csv(file)

        # Normalize the country column name
        country_col = next((col for col in df.columns if col.lower() == 'country'), None)

        if country_col is not None:
            # Count the number of respondents per country
            counts = df[country_col].value_counts().reset_index()
            counts.columns = ['country', 'count']
            counts['year'] = year

            # Append to the main DataFrame
            country_counts = pd.concat([country_counts, counts], ignore_index=True)

    # Save the result to a new CSV file
    country_counts.to_csv('country_count.csv', index=False)


if __name__ == "__main__":
    count_respondents_by_country()
    print("Country counts have been saved to 'country_count.csv'.")





