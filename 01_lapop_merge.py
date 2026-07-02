import os
import pandas as pd
import unicodedata

# Standardized country names
standard_names = {
    'Argentina', 'Bolivia', 'Brazil', 'Chile', 'Colombia', 'Costa Rica', 'Rep. Dominicana',
    'Ecuador', 'El Salvador', 'Guatemala', 'Honduras', 'Mexico', 'Nicaragua',
    'Panama', 'Paraguay', 'Peru', 'España', 'Uruguay', 'Venezuela'
}

# Mapping from code to standardized name
code_to_name = {
    32: 'Argentina', 68: 'Bolivia', 76: 'Brazil', 152: 'Chile', 170: 'Colombia',
    188: 'Costa Rica', 214: 'Rep. Dominicana', 218: 'Ecuador', 222: 'El Salvador',
    320: 'Guatemala', 340: 'Honduras', 484: 'Mexico', 558: 'Nicaragua',
    591: 'Panama', 600: 'Paraguay', 604: 'Peru', 724: 'España',
    858: 'Uruguay', 862: 'Venezuela'
}

# All interchangeable names mapped to standardized names
name_variants = {
    'argentina': 'Argentina',
    'bolivia': 'Bolivia',
    'brasil': 'Brazil',
    'brazil': 'Brazil',
    'chile': 'Chile',
    'colombia': 'Colombia',
    'costa rica': 'Costa Rica',
    'rep. dominicana': 'Rep. Dominicana',
    'ecuador': 'Ecuador',
    'el salvador': 'El Salvador',
    'guatemala': 'Guatemala',
    'honduras': 'Honduras',
    'mexico': 'Mexico',
    'méxico': 'Mexico',
    'nicaragua': 'Nicaragua',
    'panama': 'Panama',
    'panamá': 'Panama',
    'paraguay': 'Paraguay',
    'peru': 'Peru',
    'perú': 'Peru',
    'españa': 'España',
    'uruguay': 'Uruguay',
    'venezuela': 'Venezuela'
}

def normalize_country_name(name):
    if not isinstance(name, str):
        return name
    # Remove accents and lowercase
    name = unicodedata.normalize('NFKD', name).encode('ASCII', 'ignore').decode('utf-8').lower().strip()
    return name

def map_country(val):
    # Try code mapping
    try:
        code = int(val)
        return code_to_name.get(code)
    except:
        pass
    # Try name mapping (with normalization)
    name_norm = normalize_country_name(str(val))
    return name_variants.get(name_norm)

# Year to column mapping for US and China
china_col_map = {
    '2001': 'p68std', '2002': 'p56stc', '2003': 'p44std', '2004': 'p70std', '2005': 'p56std',
    '2006': 'p53st_d', '2007': 'p35st_d', '2008': 'p35st_d', '2009': 'p42st_d', '2010': 'P39ST_D',
    '2011': 'P44ST_C', '2013': 'P48ST.C', '2015': 'P35ST.C', '2016': 'P46STC', '2017': 'P45ST.C',
    '2018': 'P40ST.C'
}
us_col_map = {
    '2000': 'P37ST.B', '2001': 'p68stb', '2002': 'p56sta', '2003': 'p44sta', '2004': 'p70sta',
    '2005': 'p56sta', '2006': 'p53st_a', '2007': 'p35st_a', '2008': 'p35st_a', '2009': 'p42st_a',
    '2010': 'P39ST_A', '2011': 'P44ST_A', '2013': 'P48ST.A', '2015': 'P35ST.A', '2016': 'P46STA',
    '2017': 'P45ST.A', '2018': 'P40ST.A'
}

folder = '.'  # Set to your folder path if not current directory
merged_rows = []

for fname in os.listdir(folder):
    if fname.endswith('.csv') and fname[:4].isdigit():
        year = fname[:4]
        path = os.path.join(folder, fname)
        try:
            df = pd.read_csv(path, low_memory=False)
        except UnicodeDecodeError:
            df = pd.read_csv(path, encoding='latin1', low_memory=False)

        # Find country column (e.g., idenpa)
        country_col = None
        for col in df.columns:
            if col.lower() == 'idenpa':
                country_col = col
                break
        if country_col is None:
            continue

        # Map all country values to standardized names
        df['country'] = df[country_col].apply(map_country)
        df = df[df['country'].notnull()]

        # Get opinion columns
        fav_us_col = us_col_map.get(year)
        fav_china_col = china_col_map.get(year)
        if fav_us_col not in df.columns or fav_china_col not in df.columns:
            continue  # Skip if required columns missing

        # Build result DataFrame
        result = pd.DataFrame({
            'year': year,
            'country': df['country'],
            'fav_us': df[fav_us_col],
            'fav_china': df[fav_china_col]
        })
        merged_rows.append(result)

# Concatenate and save
if merged_rows:
    merged_df = pd.concat(merged_rows, ignore_index=True)
    merged_df.to_csv('merged_opinion.csv', index=False)