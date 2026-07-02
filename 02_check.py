import os
import pandas as pd

# Country code to name mapping
country_map = {
    32: 'Argentina', 68: 'Bolivia', 76: 'Brasil', 152: 'Chile', 170: 'Colombia',
    188: 'Costa Rica', 214: 'Rep. Dominicana', 218: 'Ecuador', 222: 'El Salvador',
    320: 'Guatemala', 340: 'Honduras', 484: 'México', 558: 'Nicaragua',
    591: 'Panamá', 600: 'Paraguay', 604: 'Perú', 724: 'España',
    858: 'Uruguay', 862: 'Venezuela'
}

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

folder = '.'  # Adjust if needed
years_to_check = [str(y) for y in range(2006, 2013)]

# Your mapping dictionaries (should be defined above)
# us_col_map, china_col_map

for year in years_to_check:
    fname = f"{year}.csv"
    path = os.path.join(folder, fname)
    print(f"Checking year: {year}")
    if not os.path.exists(path):
        print(f"  File not found: {fname}")
        continue
    try:
        df = pd.read_csv(path, low_memory=False)
    except UnicodeDecodeError:
        df = pd.read_csv(path, encoding='latin1', low_memory=False)
    us_col = us_col_map.get(year)
    china_col = china_col_map.get(year)
    if us_col is None or china_col is None:
        print(f"  Mapping missing for year {year}: us_col={us_col}, china_col={china_col}")
        continue
    print(f"  Columns in file: {list(df.columns)}")
    if us_col not in df.columns:
        print(f"  US column '{us_col}' not found in {fname}")
    if china_col not in df.columns:
        print(f"  China column '{china_col}' not found in {fname}")
    if us_col in df.columns and china_col in df.columns:
        print(f"  Both columns found for {year}")

# This will help you identify if the issue is missing files, missing mappings, or missing columns.