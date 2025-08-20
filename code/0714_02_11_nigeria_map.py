# read the csv file 'transformed_nigeria_data.csv'
# search the nigeria map online and draw the map of nigeria with the data from the csv file
# mainly divided according to the 'region' column where there are six regions
# average each region's value for a specific column, 'satisfaction', 'econ', 'fav_us', 'fav_China'
# visualize the temporal and spatial distribution of the data
# use an interactive map what written in html
import pandas as pd
import folium
import json
import requests
# Step 1: Read the CSV file
file_path = 'transformed_nigeria_data.csv'
df = pd.read_csv(file_path)
# Step 2: Load Nigeria map data
nigeria_map_url = "https://raw.githubusercontent.com/python-visualization/folium/master/examples/data/nigeria.geojson"
response = requests.get(nigeria_map_url)
if response.status_code == 200:
    nigeria_map_data = response.json()
else:
    raise Exception("Failed to load Nigeria map data from URL")
# Step 3: Prepare data for visualization
# Calculate average values for each region
region_columns = ['satisfaction', 'econ', 'fav_us', 'fav_China']
average_values = df.groupby('region')[region_columns].mean().reset_index()
# Step 4: Create a Folium map
m = folium.Map(location=[9.082, 8.6753], zoom_start=5)
# Step 5: Add GeoJSON layer to the map
folium.GeoJson(
    nigeria_map_data,
    name='Nigeria Map',
    style_function=lambda x: {
        'fillColor': 'blue',
        'color': 'black',
        'weight': 1,
        'fillOpacity': 0.5
    }
).add_to(m)
# Step 6: Add average values to the map
for _, row in average_values.iterrows():
    folium.Marker(
        location=[9.082, 8.6753],  # Placeholder coordinates, adjust as needed
        popup=(
            f"Region: {row['region']}<br>"
            f"Satisfaction: {row['satisfaction']:.2f}<br>"
            f"Econ: {row['econ']:.2f}<br>"
            f"Fav US: {row['fav_us']:.2f}<br>"
            f"Fav China: {row['fav_China']:.2f}"
        ),
        icon=folium.Icon(color='blue')
    ).add_to(m)
# Step 7: Save the map to an HTML file
m.save('nigeria_map.html')
# Step 8: Display the map in a Jupyter Notebook (optional)
# If you're using Jupyter Notebook, you can display the map directly
# from IPython.display import IFrame
# IFrame('nigeria_map.html', width=800, height=600)
# Note: If you're running this script outside of Jupyter Notebook,
# you can open the 'nigeria_map.html' file in a web browser to view the map.
# Step 9: Print unique regions
unique_regions = df['region'].unique()
print("Unique regions in the 'region' column:")
for region in unique_regions:
    print(region)
# Step 10: Print average values for each region
print("\nAverage values for each region:")
for _, row in average_values.iterrows():
    print(f"Region: {row['region']}, Satisfaction: {row['satisfaction']:.2f}, "
          f"Econ: {row['econ']:.2f}, Fav US: {row['fav_us']:.2f}, Fav China: {row['fav_China']:.2f}")
# Exception: Failed to load Nigeria map data from URL
# Note: Ensure you have the necessary libraries installed:
# pip install pandas folium requests
# Ensure you have the necessary libraries installed:


