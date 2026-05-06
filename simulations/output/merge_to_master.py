import pandas as pd
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

KEYWORD = "simulation"
MASTER_FILE = "master_results.csv"

def main():

    simulation_files = [
        x for x in os.listdir(SCRIPT_DIR) 
        if x.startswith(KEYWORD) and x.endswith(".csv")
    ]
    master_df = pd.read_csv(os.path.join(SCRIPT_DIR, simulation_files[0]))

    files_to_merge = simulation_files[1:]
    print(f"Files to merge: {len(files_to_merge)}")

    for file in files_to_merge:

        print(f"Loading file '{file}'")
        df_new = pd.read_csv(os.path.join(SCRIPT_DIR,file))

        print("Merging with new ")
        master_df = pd.concat([master_df, df_new])

    master_df.to_csv(os.path.join(SCRIPT_DIR, MASTER_FILE))
    print(f"File saved to '{MASTER_FILE}'")

if __name__ == "__main__":
    main()