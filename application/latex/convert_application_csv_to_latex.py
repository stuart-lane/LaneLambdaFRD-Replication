import pandas as pd
import os
import argparse
from pathlib import Path
from application_latex_utils import *

# Set working directory to current file folder
# current_dir = Path(__file__).parent.absolute()
# os.chdir(current_dir)

# Configuration
# Set up argument parser
parser = argparse.ArgumentParser(description='Convert application CSV to LaTeX tables')
parser.add_argument('--csv-file', type=str, default='./application/anglavy99_output.csv',
                    help='Path to CSV file')
parser.add_argument('--output-folder', type=str, default='./output/tex_tables',
                    help='Path to output folder for LaTeX tables')

args = parser.parse_args()

# Configuration from arguments (normalise paths to handle Windows/Unix differences)
CSV_FILE = os.path.normpath(args.csv_file)
OUTPUT_FOLDER = os.path.normpath(args.output_folder)

# Create output folder if it does not exist
if not os.path.exists(OUTPUT_FOLDER):
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    print(f"Output directory created: {OUTPUT_FOLDER}")

### =====================================================================
### GENERATE TABLES
### =====================================================================

tests = ["verb", "math"]

def main():    

    df = pd.read_csv(CSV_FILE)
    # Produce table

    table = table_raw_text(df)
    output_path = f"./{OUTPUT_FOLDER}/table_anglavy.tex"

    # Path to save the file
    if not os.path.exists(OUTPUT_FOLDER):
        os.mkdir(OUTPUT_FOLDER)

    # Write row to file
    with open(output_path, "w") as f:
        f.write(table + "\n")

    print(f"Table saved to {output_path}")


if __name__ == "__main__":
    main()