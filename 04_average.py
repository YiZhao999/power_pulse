import pandas as pd

df = pd.read_csv('merged_opinion_mapped.csv')
countries = sorted(df['country'].dropna().unique())
print(countries)
# Keep only rows where both fav_us and fav_china are >= 0
df_filtered = df[(df['fav_us'] >= 0) & (df['fav_china'] >= 0)]

# Group by year and country, calculate mean
result = df_filtered.groupby(['year', 'country'], as_index=False)[['fav_us', 'fav_china']].mean()

# Save to CSV
result.to_csv('merged_opinion_country_year_avg.csv', index=False)