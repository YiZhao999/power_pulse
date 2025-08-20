import pandas as pd

df = pd.read_csv('nigeria_final.csv')

region_map = {
    'South East': 'South East',
    'South South': 'South South',
    'South West': 'South West',
    'North Central': 'North Central',
    'Lagos': 'South West',
    'North West': 'North West',
    'North East': 'North East',
    'South West (Lagos excl)': 'South West',
    ' South South': 'South South',
    ' North East': 'North East',
    ' North Central': 'North Central',
    ' South West': 'South West',
    ' South East': 'South East',
    ' Lagos': 'South West',
    ' North West': 'North West',
    'Middle Belt / North Central': 'North Central',
    'South': 'South South'
}

def assign_region(row):
    if row['survey'] == 'afrobarometer':
        regions = {
            'North Central': ['Benue', 'Kogi', 'Kwara', 'Nassarawa', 'Niger', 'Plateau', 'Federal Capital Territory'],
            'North East': ['Adamawa', 'Bauchi', 'Borno', 'Gombe', 'Taraba', 'Yobe'],
            'North West': ['Jigawa', 'Kaduna', 'Kano', 'Katsina', 'Kebbi', 'Sokoto', 'Zamfara'],
            'South East': ['Abia', 'Anambra', 'Ebonyi', 'Enugu', 'Imo'],
            'South South': ['Akwa Ibom', 'Bayelsa', 'Cross River', 'Delta', 'Edo', 'Rivers'],
            'South West': ['Ekiti', 'Lagos', 'Ogun', 'Ondo', 'Osun', 'Oyo']
        }
        for region, states in regions.items():
            if row['region'] in states:
                return region
        return "Other"
    elif row['survey'] == 'pew':
        return region_map.get(row['region'].strip(), "Other")
    return "Other"

# Apply the function to create the 'region' column
df['region'] = df.apply(assign_region, axis=1)

# Save the updated DataFrame to a new CSV file
df.to_csv('nigeria_final_region.csv', index=False)