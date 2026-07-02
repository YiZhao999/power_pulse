import pandas as pd

mapping = {
    'Very good': 1,
    '1': 1, '1.0': 1,
    'Good': 2,
    '2': 2, '2.0': 2,
    'About average': 2,  # adjust if needed
    'Bad': 3,
    '3': 3, '3.0': 3,
    'Very bad': 4,
    '4': 4, '4.0': 4,
    "Don't know": -1,
    'Don´t know': -1,
    '-1': -1,
    'No answer': -2,
    'No answer/Refused': -2,
    '-2': -2
}

def map_value(val):
    if pd.isna(val):
        return val
    val_str = str(val).strip()
    return mapping.get(val_str, val)

df = pd.read_csv('merged_opinion.csv')
df['fav_us'] = df['fav_us'].apply(map_value)
df['fav_china'] = df['fav_china'].apply(map_value)
df.to_csv('merged_opinion_mapped.csv', index=False)