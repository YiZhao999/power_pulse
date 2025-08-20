# read the '2004.sav' file and other year sav files in the data folder 'sav' and transfer that into csv files with years
import pandas as pd
import os


def load_sav_files_to_csv(data_folder='sav', output_folder='csv'):
    """
    Load .sav files from the specified data folder, convert them to CSV format,
    and save them in the specified output folder with the year as part of the filename.

    :param data_folder: Folder containing the .sav files.
    :param output_folder: Folder where the converted .csv files will be saved.
    """
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    for filename in os.listdir(data_folder):
        if filename.endswith('.sav'):
            year = filename.split('.')[0]  # Extract year from filename
            sav_file_path = os.path.join(data_folder, filename)
            df = pd.read_spss(sav_file_path)
            csv_file_path = os.path.join(output_folder, f'{year}.csv')
            df.to_csv(csv_file_path, index=False)
            print(f'Converted {filename} to {csv_file_path}')


if __name__ == '__main__':
    load_sav_files_to_csv()



