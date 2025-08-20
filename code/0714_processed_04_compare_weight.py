# read the 'processed_weighted_descriptive.csv' and 'processed_unweighted_descriptive.csv' files
# compare the values of 'fav_us', 'fav_China' and 'econ' columns in the two files, by country and year
# visualize the comparison one by one
# first is to compare the weighted and unweighted values of 'fav_us' by country and year
# use an interactive plot to visualize the comparison, you can write in html format
import pandas as pd
import plotly.express as px
# Read the processed data
df_weighted = pd.read_csv('processed_weighted_descriptive.csv')
df_unweighted = pd.read_csv('processed_unweighted_descriptive.csv')
# Merge the two dataframes on country and year
df_comparison = pd.merge(df_weighted, df_unweighted, on=['country', 'year'], suffixes=('_weighted', '_unweighted'))
# Create an interactive plot to compare 'fav_us' values
fig_fav_us = px.line(df_comparison, x='year', y=['fav_us_weighted', 'fav_us_unweighted'],
                     color='country', title='Comparison of fav_us (Weighted vs Unweighted)',
                     labels={'value': 'fav_us Value', 'variable': 'Type'})
# Save the plot to an HTML file
fig_fav_us.write_html('comparison_fav_us.html')
# Create an interactive plot to compare 'fav_China' values
fig_fav_china = px.line(df_comparison, x='year', y=['fav_China_weighted', 'fav_China_unweighted'],
                        color='country', title='Comparison of fav_China (Weighted vs Unweighted)',
                        labels={'value': 'fav_China Value', 'variable': 'Type'})
# Save the plot to an HTML file
fig_fav_china.write_html('comparison_fav_china.html')
# Create an interactive plot to compare 'econ' values
fig_econ = px.line(df_comparison, x='year', y=['econ_weighted', 'econ_unweighted'],
                   color='country', title='Comparison of econ (Weighted vs Unweighted)',
                   labels={'value': 'Econ Value', 'variable': 'Type'})
# Save the plot to an HTML file
fig_econ.write_html('comparison_econ.html')
# Save the comparison dataframe to a CSV file