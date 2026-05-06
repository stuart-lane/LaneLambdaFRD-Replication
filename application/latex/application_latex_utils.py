import pandas as pd
import math

### =====================================================================
### HELPER FUNCTIONS
### =====================================================================

def format_data_point(
    num: float,
    column_idx: int,
) -> str:
    """Format scalar values for LaTeX tables."""

    if column_idx in (2, 3):
        return f"{num:.0f}"

    if math.isinf(num):
        return r"\infty" if num > 0 else r"-\infty"

    return f"{num:.2f}"


def format_confidence_interval(
    ci_lower: float,
    ci_upper: float,
) -> str:
    """Generate confidence interval string"""

    if math.isinf(ci_lower) and math.isinf(ci_upper):
        return r"$(-\infty, \infty)$"
    if math.isinf(ci_lower):
        return rf"$(-\infty, {ci_upper:.2f}]$"
    if math.isinf(ci_upper):
        return rf"$[{ci_lower:.2f}, \infty)$"
    return rf"[{ci_lower:.2f}, {ci_upper:.2f}]"


def row_string(
        df: pd.DataFrame, 
        row: int
    ) -> str:
    """Generate row of data as string"""

    row_info = df.iloc[row]

    bandwidth = f"{row_info.iloc[2]}"
    sample_size = f"{row_info.iloc[3]}"

    dgp_info_and_estimates =  [format_data_point(row_info.iloc[column_idx], column_idx) for column_idx in range(6, 11)]

    confidence_intervals = [format_confidence_interval(row_info.iloc[column_idx], row_info.iloc[column_idx+1]) for column_idx in range(11, 23, 2)]

    data_points = dgp_info_and_estimates + confidence_intervals

    data_string = " & ".join(data_points)
    full_row = bandwidth + " & " + sample_size + " & " + data_string + r" \\"
    
    return full_row

def generate_panel(
        df: pd.DataFrame, 
        test: str = "verb",
    ) -> str:
    """Create panel of table"""

    rows_per_cutoff = (len(df) // 2)

    start_row = 0 if test == "verb" else rows_per_cutoff
    end_row = start_row + rows_per_cutoff       

    panel_list = [r"\hline"]

    for row_idx in range(start_row, end_row):
        panel_list.append(row_string(df, row_idx))

    panel_list.append(r'\hline')

    return "\n".join(panel_list)

def extract_bandwidth_values(
        df: pd.DataFrame,
        test: str
    ) -> dict:
    """Extract bandwidth values for each cutoff from the dataframe"""
    
    cutoffs = [40]
    bandwidth_values = {}
    
    # Filter dataframe for the specific test
    test_df = df[df['test'] == test]
    
    for cutoff in cutoffs:
        cutoff_df = test_df[test_df['cutoff'] == cutoff]
        if not cutoff_df.empty:
            # Get the first row for this cutoff (all rows have same bandwidth values)
            h_ccf = cutoff_df.iloc[0]['bw_cov']
            h_ik = cutoff_df.iloc[0]['bw_mse']
            bandwidth_values[cutoff] = {
                'h_ik': h_ik,
                'h_ccf': h_ccf
            }
    
    return bandwidth_values

def subheader_text(
        test: str
    ) -> str:

    test_label = "(a) Verb" if test == "verb" else "(b) Mathematics"

    text = rf"""\multicolumn{{13}}{{c}}{{{test_label} test scores}} \\
\hline
\hline  
$h$ & $n_h$ & $\hat{{\tau}}_{{2SLS}}$ & $\hat{{\tau}}_{{1}}$ & $\hat{{\tau}}_{{1}}^{{BC}}$ & $\hat{{\tau}}_{{\Lambda(1)}}$ & $\hat{{\tau}}_{{\Lambda(4)}}$ & $\mcC_{{2SLS}}$ & $STD$ & $RBC$ & $\mcC_{{\Lambda(1)}}$ & $\mcC_{{\Lambda(4)}}$ & $AR_2$ \T \\"""
    
    return text

def footer_text(
        df: pd.DataFrame
    ) -> str:
    
    footer_string = rf"""\hline
\end{{tabular}}
The MSE- and coverage optimal bandwidths are $h_{{IK}} = {df["bw_mse"][0]:.2f}$ and $h_{{CCF}} = {df["bw_cov"][0]:.2f}$ for verb test scores, and $h_{{IK}} = {df["bw_mse"][7]:.2f}$ and $h_{{CCF}} = {df["bw_cov"][7]:.2f}$ for mathematics test scores
\label{{table empirical math}}
\end{{threeparttable}}
\end{{sidewaystable}}
"""
    
    return footer_string

def table_raw_text(
        df: pd.DataFrame, 
    ) -> str:
    """Generate complete latex table"""

    table_text = []

    table_text.append(header_text)
    table_text.append(subheader_text("verb"))
    table_text.append(generate_panel(df, "verb"))
    table_text.append(subheader_text("math"))
    table_text.append(generate_panel(df, "math"))
    table_text.append(footer_text(df))

    return "\n".join(table_text)


### =====================================================================
### STOCK TEXT
### =====================================================================

header_text = rf"""\begin{{sidewaystable}}
\centering
\addtolength{{\tabcolsep}}{{-2pt}} 
\small
\begin{{threeparttable}}
\caption{{Class size effects on test scores}} 
\vspace{{-0.5em}}
\centering 
\begin{{tabular}}{{c c c c c c c c c c c c c}} 
\hline"""