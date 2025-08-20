import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Load data from CSV
df = pd.read_csv('0713_pew_NGA.csv')

# Replace 'Lagos' with 'South West'
df['region'] = df['region'].replace('Lagos', 'South West')

# Group by region and year, calculate mean
grouped = df.groupby(['region', 'year']).agg({
    'satisfaction': 'mean',
    'fav_us': 'mean',
    'fav_China': 'mean',
    'econ': 'mean'
}).reset_index()

# Load Nigeria GeoJSON (geopolitical zones)
url = 'gadm41_NGA_1.json'  # Adjust the URL to your GeoJSON file path or URL
gdf = gpd.read_file(url)

# Merge GeoJSON with averaged data
merged_2013 = gdf.merge(
    grouped[grouped['year'] == 2013],
    left_on='name',
    right_on='region',
    how='left'
)
merged_2014 = gdf.merge(
    grouped[grouped['year'] == 2014],
    left_on='name',
    right_on='region',
    how='left'
)

# Create subplots for each metric
metrics = ['satisfaction', 'fav_us', 'fav_China', 'econ']
fig, axes = plt.subplots(4, 2, figsize=(15, 20))
fig.suptitle('Nigeria Regional Averages (2013 vs 2014)', fontsize=16)

for i, metric in enumerate(metrics):
    # Plot 2013 data
    ax1 = axes[i, 0]
    merged_2013.plot(
        column=metric,
        ax=ax1,
        legend=True,
        cmap='viridis',
        missing_kwds={'color': 'lightgrey'}
    )
    ax1.set_title(f'{metric} (2013)')
    ax1.axis('off')

    # Plot 2014 data
    ax2 = axes[i, 1]
    merged_2014.plot(
        column=metric,
        ax=ax2,
        legend=True,
        cmap='viridis',
        missing_kwds={'color': 'lightgrey'}
    )
    ax2.set_title(f'{metric} (2014)')
    ax2.axis('off')

plt.tight_layout(rect=[0, 0, 1, 0.96])
plt.show()
