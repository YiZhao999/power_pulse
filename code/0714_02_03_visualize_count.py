import pandas as pd
import plotly.express as px
# Load the CSV file
df = pd.read_csv('country_count.csv')
# Drop rows where 'year' is not a number
df = df[pd.to_numeric(df['year'], errors='coerce').notnull()]
# Convert 'year' to numeric type
df['year'] = pd.to_numeric(df['year'])
# Sort the DataFrame by 'year' in ascending order
df = df.sort_values(by='year')
# Create an interactive world map using Plotly Express
fig = px.choropleth(df,
                    locations='country',
                    locationmode='country names',
                    color='count',
                    animation_frame='year',
                    title='Count of Respondents by Country Over Years',
                    labels={'count': 'Number of Respondents'},
                    color_continuous_scale=px.colors.sequential.Plasma)
# Update layout for better visualization
fig.update_geos(projection_type='natural earth')
fig.update_layout(title_x=0.5, title_y=0.9)
# Show the figure
fig.show()
# Save the figure as an HTML file
fig.write_html('country_count_map.html')
# Note: The generated HTML file can be opened in a web browser to interact with the map.
# The map allows users to click on a country to see its trends over the years.









