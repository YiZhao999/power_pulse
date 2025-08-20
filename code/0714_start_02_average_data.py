import pandas as pd


def process_and_average_multiple(file_path, variables, country_column, output_file):
    """
    Process multiple variable columns by mapping text to numbers and calculate the average per country.

    :param file_path: Path to the CSV file.
    :param variables: List of variable names to process (case-insensitive).
    :param country_column: Name of the country column (case-insensitive).
    :param output_file: Path to save the resulting CSV file.
    """
    df = pd.read_csv(file_path)
    df.columns = [col.lower() for col in df.columns]
    country_column_lower = country_column.lower()
    variables_lower = [var.lower() for var in variables]

    if country_column_lower not in df.columns or not all(var in df.columns for var in variables_lower):
        raise ValueError(f"Columns '{variables}' or '{country_column}' not found in the file.")

    # Map text values to numbers
    mapping = {
        "very favorable": 1,
        "somewhat favorable": 2,
        "somewhat unfavorable": 3,
        "very unfavorable": 4
    }

    for var in variables_lower:
        df[var] = df[var].str.lower().map(mapping).fillna(0)

    # Group by country and calculate the average for each variable
    result = df.groupby(country_column_lower)[variables_lower].mean().reset_index()
    result.columns = [country_column] + variables

    # Save the result to a CSV file
    result.to_csv(output_file, index=False)
    print(f"Saved averaged data to {output_file}")

# Example usage
file_path = '2016.csv'
variables = ['Q10B', 'Q10A']
country_column = 'COUNTRY'
output_file = '2016_avg.csv'
process_and_average_multiple(file_path, variables, country_column, output_file)










