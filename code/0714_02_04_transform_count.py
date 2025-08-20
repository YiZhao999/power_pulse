# read the 'country_count.csv' file which indicate the 'year', 'country', and 'count'. transform the dataframe, where the column is 'year', the index is 'country', and the value is 'count'.
# drop the column of 'reference' if it exists.
# add another column 'count' which is the number of years that have values for each country.
# add another column 'total' which is the sum of all the counts for each country.
# add another column 'average' which is the average of all the counts for each country.
import pandas as pd

df = pd.read_csv('country_count.csv')
df = df[pd.to_numeric(df['year'], errors='coerce').notnull()]

    # Pivot the DataFrame to have 'year' as columns and 'country' as index
pivot_df = df.pivot(index='country', columns='year', values='count')

    # Add a 'count' column which is the number of years with values for each country
pivot_df['count'] = pivot_df.count(axis=1)

    # Add a 'total' column which is the sum of all counts for each country
pivot_df['total'] = pivot_df.sum(axis=1)

    # Add an 'average' column which is the average of all counts for each country
pivot_df['average'] = pivot_df.mean(axis=1)

# Save the transformed DataFrame to a new CSV file
pivot_df.to_csv('transformed_country_count.csv')




