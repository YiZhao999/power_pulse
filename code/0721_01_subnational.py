# read the 'aid_nigeria_sub.csv' file and create a new column 'region' for each row
# the region should be determined by the 'name_1' column, following the mapping:
# North Central: Benue, Kogi, Kwara, Nasarawa, Niger, Plateau, and the Federal Capital Territory (FCT).
# North East: Adamawa, Bauchi, Borno, Gombe, Taraba, and Yobe.
# North West: Jigawa, Kaduna, Kano, Katsina, Kebbi, Sokoto, and Zamfara.
# South East: Abia, Anambra, Ebonyi, Enugu, and Imo.
# South South (Niger Delta): Akwa Ibom, Bayelsa, Cross River, Delta, Edo, and Rivers.
# South West: Ekiti, Lagos, Ogun, Ondo, Osun, and Oyo.
# so the new column 'region' should have the following 6 values: North Central, North East, North West, South East, South South, and South West.
# if there is no match for the 'name_1' column, the 'region' column should be set to 'Other'.


import pandas as pd


def assign_region(name_1):
    regions = {
        'North Central': ['Benue', 'Kogi', 'Kwara', 'Nassarawa', 'Niger', 'Plateau', 'Federal Capital Territory'],
        'North East': ['Adamawa', 'Bauchi', 'Borno', 'Gombe', 'Taraba', 'Yobe'],
        'North West': ['Jigawa', 'Kaduna', 'Kano', 'Katsina', 'Kebbi', 'Sokoto', 'Zamfara'],
        'South East': ['Abia', 'Anambra', 'Ebonyi', 'Enugu', 'Imo'],
        'South South': ['Akwa Ibom', 'Bayelsa', 'Cross River', 'Delta', 'Edo', 'Rivers'],
        'South West': ['Ekiti', 'Lagos', 'Ogun', 'Ondo', 'Osun', 'Oyo']
    }
    # regions = {
    #     'Central': ['Kiambu', 'Kirinyaga', 'Murang\’a', 'Nyandarua', 'Nyeri'],
    #     'Coast': ['Kilifi', 'Kwale', 'Lamu', 'Mombasa', 'Taita Taveta', 'Tana River'],
    #     'Eastern': ['Embu', 'Isiolo', 'Kitui', 'Machakos', 'Makueni', 'Marsabit', 'Meru', 'Tharaka-Nithi'],
    #     'Nairobi': ['Abia', 'Anambra', 'Ebonyi', 'Enugu', 'Imo'],
    #     'North Eastern': ['Garissa', 'Mandera', 'Wajir'],
    #     'Nyanza': ['Homa Bay', 'Kisii', 'Kisumu', 'Migori', 'Nyamira', 'Siaya'],
    #     'Nairobi': ['Nairobi'],
    #     'Rift Valley': ['Baringo', 'Bomet', 'Elgeyo-Marakwet', 'Kajiado', 'Kericho', 'Laikipia', 'Nakuru', 'Nandi',
    #                     'Narok', 'Samburu', 'Trans Nzoia', 'Uasin Gishu', 'West Pokot', 'Turkana'],
    #     'Western': ['Bungoma', 'Busia', 'Kakamega', 'Vihiga'],
    # }
    for region, states in regions.items():
        if name_1 in states:
            return region
    return "Other"

# Load the CSV file


df = pd.read_csv('nigeria_aid_2007-2019.csv')
# Apply the function to create the 'region' column
df['region'] = df['name_1'].apply(assign_region)
# Save the updated DataFrame to a new CSV file
df.to_csv('aid_nigeria_region_2007-2019.csv', index=False)


