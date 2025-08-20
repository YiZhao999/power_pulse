import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt

# Load the Nigeria Admin 1 shapefile (state-level). GADM can be downloaded from https://gadm.org
# Alternatively use: geoboundaries or naturalearth_lowres
nigeria_states = gpd.read_file("gadm41_NGA_1.shp")  # Level 1 = States

# Load your aid dataset
aid_df = pd.read_csv("0713_aid_NGA.csv")


# Choose a specific year for mapping
year_to_plot = 2020
aid_2020 = aid_df[['year'] == year_to_plot]

# Merge with spatial data
nigeria_states = nigeria_states.rename(columns={"NAME_1": "name_1"})  # match your data
nigeria_states_aid = nigeria_states.merge(aid_2020, on='name_1', how='left')

# Plotting
fig, ax = plt.subplots(1, 1, figsize=(12, 10))

nigeria_states_aid.plot(column='commitment',
                        cmap='OrRd',
                        linewidth=0.8,
                        edgecolor='black',
                        legend=True,
                        ax=ax)

ax.set_title(f"Aid Commitment by State in Nigeria ({year_to_plot})", fontsize=16)
ax.axis('off')

plt.show()