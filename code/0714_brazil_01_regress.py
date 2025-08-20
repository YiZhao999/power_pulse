# read '2023 copy.csv' file
# selection all rows when 'country' is 'Brazil'
# select columns named 'age', 'econ_sit', 'satisfied_democracy', 'fav_us', 'fav_china', 'weight
# form a new dataframe with these columns and rows
import pandas as pd
df = pd.read_csv('2023 copy.csv')
df = df[df['country'] == 'Brazil']
df = df[['age', 'econ_sit', 'satisfied_democracy', 'fav_us', 'fav_china', 'weight']]
# save the new dataframe to a csv file named 'brazil.csv'
df.to_csv('brazil.csv', index=False)
