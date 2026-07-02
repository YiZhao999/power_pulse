import pandas as pd

df = pd.read_csv('merged_opinion_mapped.csv')

def compute_prop_fav(group, col):
    valid = group[(group[col] != -1) & (group[col] != -2)]
    if len(valid) == 0:
        return float('nan')
    fav = valid[(valid[col] == 1) | (valid[col] == 2)]
    return len(fav) / len(valid)

result = (
    df.groupby(['year', 'country'])
    .apply(lambda g: pd.Series({
        'us_prop_fav': compute_prop_fav(g, 'fav_us'),
        'china_prop_fav': compute_prop_fav(g, 'fav_china')
    }))
    .reset_index()
)

result.to_csv('merged_opinion_country_year_prop.csv', index=False)