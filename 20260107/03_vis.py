import pandas as pd
import matplotlib.pyplot as plt

# List of African country names
african_countries = [
    'Algeria', 'Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cabo Verde',
    'Cameroon', 'Central African Republic', 'Chad', 'Comoros', 'Congo', 'Democratic Republic of the Congo',
    'Djibouti', 'Egypt', 'Equatorial Guinea', 'Eritrea', 'Eswatini', 'Ethiopia', 'Gabon',
    'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Ivory Coast', 'Kenya', 'Lesotho', 'Liberia',
    'Libya', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mauritius', 'Morocco', 'Mozambique',
    'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Sao Tome and Principe', 'Senegal', 'Seychelles',
    'Sierra Leone', 'Somalia', 'South Africa', 'South Sudan', 'Sudan', 'Tanzania', 'Togo',
    'Tunisia', 'Uganda', 'Zambia', 'Zimbabwe'
]

# Read merged data
df = pd.read_csv('merged.csv')

# Filter for African countries
df_africa = df[df['Countryname'].isin(african_countries)]

# Group by year and sum aid
aid_by_year = df_africa.groupby('year')[['CHN_comm', 'USA_comm']].sum().reset_index()

# Plot
plt.figure(figsize=(10, 6))
plt.plot(aid_by_year['year'], aid_by_year['CHN_comm'], label='China Aid (CHN_comm)')
plt.plot(aid_by_year['year'], aid_by_year['USA_comm'], label='USA Aid (USA_comm)')
plt.xlabel('Year')
plt.ylabel('Total Aid')
plt.title('Total Aid to African Countries (2000-2020)')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()