import pandas as pd
import plotly.express as px

# Load the cleaned panel data
data = pd.read_csv('cleaned_panel_data.csv')

# Interactive World Map with Year Slider for Trust in US
fig_us = px.choropleth(
    data,
    locations='Country',
    locationmode='country names',
    color='Trust_US',
    hover_name='Country',
    animation_frame='Year',
    title='Trust in US by Country Over Time',
    color_continuous_scale=px.colors.sequential.Plasma
)
fig_us.write_html('trust_us_map.html')  # Save as HTML

# Interactive World Map with Year Slider for Trust in China
fig_china = px.choropleth(
    data,
    locations='Country',
    locationmode='country names',
    color='Trust_China',
    hover_name='Country',
    animation_frame='Year',
    title='Trust in China by Country Over Time',
    color_continuous_scale=px.colors.sequential.Plasma
)
fig_china.write_html('trust_china_map.html')  # Save as HTML