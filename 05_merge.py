import pandas as pd

sample_countries = [
    'Argentina', 'Bolivia', 'Brasil', 'Chile', 'Colombia', 'Costa Rica', 'Ecuador',
    'El Salvador', 'España', 'Guatemala', 'Honduras', 'México', 'Nicaragua',
    'Panamá', 'Paraguay', 'Perú', 'Rep. Dominicana', 'Uruguay', 'Venezuela'
]

# 2. Read and filter opinion.csv
opinion = pd.read_csv('opinion.csv')
opinion = opinion[opinion['country'].isin(sample_countries)]
opinion = opinion[['year', 'country', 'fav_us', 'fav_china']]
opinion['year'] = opinion['year'].astype(str)

# 3. Read and filter aid_vote.csv
aid_vote = pd.read_csv('aid_vote.csv')
aid_vote = aid_vote[aid_vote['Countryname'].isin(sample_countries)]
aid_vote = aid_vote.rename(columns={
    'Countryname': 'country',
    'CHN_comm': 'aid_china',
    'USA_comm': 'aid_us',
    'ChinaAgree': 'vote_china',
    'USAgree': 'vote_us'
})
aid_vote = aid_vote[['year', 'country', 'aid_china', 'aid_us', 'vote_china', 'vote_us']]
aid_vote['year'] = aid_vote['year'].astype(str)

# 4. Read and filter corruption.csv
corruption = pd.read_csv('corruption.csv', index_col=0)
corruption = corruption.loc[corruption.index.intersection(sample_countries)]
corruption = corruption[[str(y) for y in range(2002, 2019)]]
corruption = corruption.reset_index()
id_var = corruption.columns[0]
corruption = corruption.melt(id_vars=id_var, var_name='year', value_name='corruption')
corruption = corruption.rename(columns={id_var: 'country'})
corruption['year'] = corruption['year'].astype(str)

# 5. Merge all DataFrames
merged = pd.merge(opinion, aid_vote, on=['year', 'country'], how='outer')
merged = pd.merge(merged, corruption, on=['year', 'country'], how='outer')

# 6. Filter for sample countries and years 2002-2018
merged = merged[merged['country'].isin(sample_countries)]
merged = merged[merged['year'].astype(str).between('2002', '2018')]

# 7. Save final dataset
merged = merged[['year', 'country', 'fav_us', 'fav_china', 'corruption', 'aid_china', 'aid_us', 'vote_china', 'vote_us']]
merged.to_csv('final_merged_dataset.csv', index=False)