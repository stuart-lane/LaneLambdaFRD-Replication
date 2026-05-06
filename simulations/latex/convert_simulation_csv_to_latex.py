import pandas as pd
import os
import argparse
from pathlib import Path
from simulation_latex_utils import *

# Set working directory to current file folder
# current_dir = Path(__file__).parent.absolute()
# os.chdir(current_dir)

# Configuration
# CSV_FOLDER = './simulations/output_final'
# OUTPUT_FOLDER = './output/tex_tables'

parser = argparse.ArgumentParser(description='Convert simulation CSV to LaTeX tables')
parser.add_argument('--csv-folder', type=str, default='./simulations/output',
                    help='Path to folder containing CSV files')
parser.add_argument('--output-folder', type=str, default='./output/tex_tables',
                    help='Path to output folder for LaTeX tables')

args = parser.parse_args()

# Configuration from argumentss (normalise paths to handle Windows/Unix differences)
CSV_FOLDER = os.path.normpath(args.csv_folder)
OUTPUT_FOLDER = os.path.normpath(args.output_folder)

# Create output folder if it does not exist
if not os.path.exists(OUTPUT_FOLDER):
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    print(f"Output directory created: {OUTPUT_FOLDER}")

### =====================================================================
### GENERATE TABLES
### =====================================================================

files = os.listdir(CSV_FOLDER)
main_files = sorted([f for f in os.listdir(CSV_FOLDER) if f.startswith('simulation_results')])

def generate_appendix_simulation_tables():
    types = ["estimation", "inference"]

    for file in main_files:
        for type in types:
            df = pd.read_csv(f"{CSV_FOLDER}/{file}")

            # Extract sample sizes
            small_n = min(df['n'])
            large_n = max(df['n'])

            # Get data generating process info
            file_info = extract_dgp_rv_error(file)
            dgp = file_info['dgp']
            rv = file_info['rv']
            error_distribution = file_info['error_distribution']

            # Total numbers of rows
            no_rows_total = df.shape[0]
            no_rows_subpanel = int(df.shape[0]/2)

            # Generate top and bottom panels and convert to string
            top_panel_string = generate_panel(df, 0, no_rows_subpanel, type)
            bottom_panel_string = generate_panel(df, no_rows_subpanel, no_rows_total, type)

            # Produce table
            table_text = table_raw_text(dgp, rv, error_distribution, small_n, large_n, 
                                        top_panel_string, bottom_panel_string, type)

            # Path to save the file
            if not os.path.exists(OUTPUT_FOLDER):
                os.mkdir(OUTPUT_FOLDER)

            file_path = f"./{OUTPUT_FOLDER}/table_{type}_dgp{dgp}_rv{rv}_error{error_distribution}.tex"

            # Write row to file
            with open(file_path, "w") as f:
                f.write(table_text + "\n")

            print(f"Table saved to {file_path}")

def generate_summary_table():
    summary_df = pd.read_csv(f"{CSV_FOLDER}/estimator_summary.csv", index_col=0)

    total_params = int(summary_df.iloc[1].sum())
    total_metrics = summary_df.shape[0]

    table = table_raw_text_summary(summary_df, total_params, total_metrics)

    # Path to save the file
    if not os.path.exists(OUTPUT_FOLDER):
        os.mkdir(OUTPUT_FOLDER)

    file_path = f"./{OUTPUT_FOLDER}/summary_table.tex"

    # Write row to file
    with open(file_path, "w") as f:
        f.write(table + "\n")

    print(f"Table saved to {file_path}")

def generate_concise_table():
    concise_df = pd.read_csv(f"{CSV_FOLDER}/simulation_results_dgp1_rvnormal_errornormal.csv")

    # Total numbers of rows
    no_rows_total = concise_df.shape[0]
    no_rows_subpanel = int(concise_df.shape[0]/2)

    table = concise_table_text(concise_df, no_rows_subpanel, no_rows_total)

    # Path to save the file
    if not os.path.exists(OUTPUT_FOLDER):
        os.mkdir(OUTPUT_FOLDER)

    file_path = f"./{OUTPUT_FOLDER}/concise_table_main.tex"

    # Write row to file
    with open(file_path, "w") as f:
        f.write(table + "\n")

    print(f"Table saved to {file_path}")

def generate_concise_table_inference():
    concise_df = pd.read_csv(f"{CSV_FOLDER}/simulation_results_dgp1_rvnormal_errornormal.csv")

    # Total number of rows
    no_rows_total = int(concise_df.shape[0]/2)

    table = concise_table_text_inference(concise_df, no_rows_total)

    # Path to save the file
    if not os.path.exists(OUTPUT_FOLDER):
        os.mkdir(OUTPUT_FOLDER)

    file_path = f"./{OUTPUT_FOLDER}/concise_table_inference.tex"

    # Write row to file
    with open(file_path, "w") as f:
        f.write(table + "\n")

    print(f"Table saved to {file_path}")

def main():

    generate_concise_table()

    generate_concise_table_inference()

    generate_appendix_simulation_tables()

    generate_summary_table()
            

if __name__ == "__main__":
    main()