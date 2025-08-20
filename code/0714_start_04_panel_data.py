import pandas as pd

# Step 1: Load the data
data = pd.read_csv('merged_data.csv')

# Step 2: Set the first column as the index (country names)
data.set_index(data.columns[0], inplace=True)

# Step 3: Filter columns for trust in US and China
us_columns = [col for col in data.columns if 'A' in col or 'fav_US' in col]
china_columns = [col for col in data.columns if col not in us_columns]

# Step 4: Extract year from column names and filter for years 2010 to 2017
us_columns_filtered = [col for col in us_columns if any(str(year) in col for year in range(2010, 2018))]
china_columns_filtered = [col for col in china_columns if any(str(year) in col for year in range(2010, 2018))]

# Step 5: Create cleaned panel data
panel_data = []
for country in data.index:
    for year in range(2010, 2018):
        us_col = next((col for col in us_columns_filtered if str(year) in col), None)
        china_col = next((col for col in china_columns_filtered if str(year) in col), None)
        if us_col and china_col:
            panel_data.append({
                'Country': country,
                'Year': year,
                'Trust_US': data.loc[country, us_col],
                'Trust_China': data.loc[country, china_col]
            })

cleaned_data = pd.DataFrame(panel_data)

# Step 6: Save the cleaned panel data to a new CSV file
cleaned_data.to_csv('cleaned_panel_data.csv', index=False)









