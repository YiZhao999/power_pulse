# python
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Read merged data
df = pd.read_csv('merged.csv')

# Filter for years 2010-2024
df = df[(df['year'] >= 2010) & (df['year'] <= 2024)]

# Aggregate USA aid by country
aid_by_country = df.groupby('Countryname')[['USA_comm']].sum().reset_index()

# Identify and remove the country with the highest USA_comm (Iraq)
max_country = aid_by_country.loc[aid_by_country['USA_comm'].idxmax(), 'Countryname']
aid_by_country = aid_by_country[aid_by_country['Countryname'] != max_country]

# Load world map
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world = world.rename(columns={'name': 'Countryname'})

# Merge with aid data
merged = world.merge(aid_by_country, on='Countryname', how='left')

# Plot US aid map (without Iraq) and save
fig, ax = plt.subplots(1, 1, figsize=(10, 6))
merged.plot(column='USA_comm', cmap='Blues', linewidth=0.8, ax=ax, edgecolor='0.8', legend=True)
ax.set_title('Total US Aid Commitment (2010-2024, Excluding Iraq)')
ax.axis('off')
plt.tight_layout()
plt.savefig('us_aid_no_iraq_2010_2024.png', dpi=300)
plt.close()