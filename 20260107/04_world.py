import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

# Read merged data
df = pd.read_csv('merged.csv')

# Aggregate aid by country
aid_by_country = df.groupby('Countryname')[['CHN_comm', 'USA_comm']].sum().reset_index()

# Load world map
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world = world.rename(columns={'name': 'Countryname'})

# Merge with aid data
merged = world.merge(aid_by_country, on='Countryname', how='left')

# Plot China aid map and save
fig, ax = plt.subplots(1, 1, figsize=(10, 6))
merged.plot(column='CHN_comm', cmap='Reds', linewidth=0.8, ax=ax, edgecolor='0.8', legend=True)
ax.set_title('Total Aid Commitment from China')
ax.axis('off')
plt.tight_layout()
plt.savefig('china_aid.png', dpi=300)
plt.close()

# Plot US aid map and save
fig, ax = plt.subplots(1, 1, figsize=(10, 6))
merged.plot(column='USA_comm', cmap='Blues', linewidth=0.8, ax=ax, edgecolor='0.8', legend=True)
ax.set_title('Total Aid Commitment from US')
ax.axis('off')
plt.tight_layout()
plt.savefig('us_aid.png', dpi=300)
plt.close()