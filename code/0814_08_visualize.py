import pandas as pd
import numpy as np
import plotly.graph_objects as go
import plotly.subplots as sp
import plotly.io as pio  # Import plotly.io to resolve NameError
from pathlib import Path

# Define file paths
data_path = Path("updated_nigeria_2007-2019.csv")
output_path = Path("visualizations")
output_path.mkdir(exist_ok=True)

# Load the dataset
df = pd.read_csv(data_path)

# Replace infinite values with NaN
df.replace([np.inf, -np.inf], np.nan, inplace=True)

# Fill missing values with the mean of the respective columns
df['fav_us'] = df['fav_us'].fillna(df['fav_us'].mean())
df['fav_china'] = df['fav_china'].fillna(df['fav_china'].mean())
df['econ'] = df['econ'].fillna(df['econ'].mean())

# Convert 'year' to datetime for better handling
df['year'] = pd.to_datetime(df['year'], format='%Y')

# Aggregate data by year and region using mean
agg_mean = df.groupby(['year', 'region'])[['fav_us', 'fav_china', 'econ']].mean().reset_index()

# Interactive Visualization using Plotly
fig = sp.make_subplots(rows=3, cols=1, shared_xaxes=True, subplot_titles=("Average Favorability towards US", "Average Favorability towards China", "Average Economic Perception"))

# Favorability towards US
for region in agg_mean['region'].unique():
    region_data = agg_mean[agg_mean['region'] == region]
    fig.add_trace(go.Scatter(x=region_data['year'], y=region_data['fav_us'], mode='lines+markers', name=f'Fav US - {region}'), row=1, col=1)

# Favorability towards China
for region in agg_mean['region'].unique():
    region_data = agg_mean[agg_mean['region'] == region]
    fig.add_trace(go.Scatter(x=region_data['year'], y=region_data['fav_china'], mode='lines+markers', name=f'Fav China - {region}'), row=2, col=1)

# Economic Perception
for region in agg_mean['region'].unique():
    region_data = agg_mean[agg_mean['region'] == region]
    fig.add_trace(go.Scatter(x=region_data['year'], y=region_data['econ'], mode='lines+markers', name=f'Econ - {region}'), row=3, col=1)

fig.update_layout(height=900, width=800, title_text="Public Opinion Trends by Region Over Years")
pio.write_html(fig, file=output_path / "interactive_trends.html", auto_open=False)
fig.show()