import pandas as pd


def create_column_mapping():
    # Define the mapping for the columns
    column_mapping = {
        'name_0': 'country',
        'year': 'year',
        'name_1': 'state',
        'CHN_comm': 'CHN_comm',
        'CHN_dummy_comm': 'CHN_dummy_comm',
        'CHN_comm_nominal': 'CHN_comm_nominal',
        'WB_comm': 'WB_comm',
        'WB_comm_nominal': 'WB_comm_nominal',
        'WB_dummy_comm': 'WB_dummy_comm',
        'WB_disb': 'WB_disb',
        'WB_dummy_disb': 'WB_dummy_disb',
        'WB_disb_nominal': 'WB_disb_nominal',
        'USA_comm': 'USA_comm',
        'USA_comm_nominal': 'USA_comm_nominal',
        'USA_disb': 'USA_disb',
        'USA_disb_nominal': 'USA_disb_nominal',
        'USA_projectscount': 'USA_projectscount'
    }
    return column_mapping


def filter_and_rename_columns(df, column_mapping):
    # Filter the DataFrame for 'name_0' as 'Nigeria' and 'year' between 2007 and 2019
    filtered_df = df[(df['name_0'] == 'Nigeria') & (df['year'].between(2007, 2019))]

    # Select and rename the relevant columns
    filtered_df = filtered_df[list(column_mapping.keys())].rename(columns=column_mapping)

    return filtered_df

# Load the CSV file
df = pd.read_csv('GODAD_adm1.csv')

# Create the column mapping
column_mapping = create_column_mapping()

# Filter and rename the columns
filtered_df = filter_and_rename_columns(df, column_mapping)

# Save the filtered DataFrame to a new CSV file
filtered_df.to_csv('aid_nigeria_sub.csv', index=False)