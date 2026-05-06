import numpy as np
import pandas as pd
import builtins

### =====================================================================
### HELPER FUNCTIONS
### =====================================================================

ROWS_PER_GROUP = 4

def extract_dgp_rv_error(
        file: str
    ) -> dict:
    """Get data generating process and running variable info"""

    substring = file.split('_')
    dgp = substring[2][-1]
    rv = substring[3][2:].lower()
    error_distribution = substring[4][5:-4].lower()

    return {
        'dgp': dgp,
        'rv': rv,
        'error_distribution': error_distribution
    }

def panel_title(
        small_n: int, 
        large_n: int, 
        position: str,
        type: str = "estimation"
    ) -> str:
    """Produce panel title"""

    if type == "estimation":
        columns = 20
    else:
        columns = 12

    if position == "top":
        panel = "A"
        n = small_n
    else:
        panel = "B"
        n = large_n
        
    return rf"\multicolumn{{{columns}}}{{c}}{{Panel {panel}: $n = {n}$}} \\"

def format_data_point(
        num: float,
        type: str = "estimation"
    ) -> str: 
    """Round numbers over 100 to integer"""

    if type == "estimation":
        if abs(num) >= 100: 
            return f"{num:.0f}" 
        else: 
            return f"{num:.2f}"
    else:
        return f"{100 * num:.1f}"
    
def first_column(
        row: int
    ) -> str:
    """Inserts multirow into first column"""

    if row % ROWS_PER_GROUP == 0:
        val = int(1 + np.floor((row % 12) / ROWS_PER_GROUP))
        return rf"\multirow{{{ROWS_PER_GROUP}}}{{*}}{{{val}}}" 
    else:
        return ""
    
def row_string(
        df: pd.DataFrame, 
        row: int,
        type: str = "estimation"
    ) -> str:
    """Generate row of data as string"""

    row_info = df.iloc[row]

    if type == "estimation":
        start_idx = 7
        end_idx = 25
    else:
        start_idx = 25
        end_idx = 33

    data_points = [format_data_point(row_info.iloc[column_idx], type) for column_idx in range(start_idx, end_idx)]
    data_points.insert(0, pi_values[str(row % ROWS_PER_GROUP)])
    data_string = " & ".join(data_points)

    first_col = first_column(row)
    if first_col:
        full_row = first_col + " & " + data_string + r" \\"
    else:
        full_row = "& " + data_string + r" \\"

    return full_row

def design_info(
        dgp: int, 
        rv: str,
        errors: str,
        type: str = "estimation"
    ) -> str:
    """Info for table caption and label"""

    if str(dgp) == "1":
        cite_text = rf"\citet{{lee2008randomized}}"
        cite_tag = "lee"
    else:
        cite_text = rf"\citet{{ludwig2007does}}"
        cite_tag = "lm"

    if rv == "normal":
        rv_text = rf"$X_i \sim N(0,1)$"
    else:
        rv_text = rf"$X_i \sim 2Beta(2,4) - 1$"

    if errors == "normal":
        error_text = rf"$U_i \sim N(0,0.09)$"
    else:
        error_text = rf"$U_i \sim t(2.5)$"

    citation_text = cite_text + " design, " + rv_text + ", " + error_text
    table_tag = f"table {type} {cite_tag} {rv}{' t' if errors != "normal" else ""}"
    
    return {
        'citation_text': citation_text,
        'table_tag': table_tag
    }

def generate_panel(
        df: pd.DataFrame, 
        start_row: int, 
        end_row: int, 
        type: str
    ) -> str:
    """Create panel of table"""

    panel_list = []

    for row_idx in range(start_row, end_row):
        panel_list.append(row_string(df, row_idx, type))
        
        if row_idx > start_row and (row_idx + 1) < end_row and (row_idx + 1) % 4 == 0:
            panel_list.append(r'\hline')
    
    return "\n".join(panel_list)

def footer_text(
        dgp: int, 
        rv: str,
        errors: str,
        type: str = "estimation"
    ) -> str:
    """Generate footer text"""

    design_text = design_info(dgp, rv, errors, type)

    if type == "estimation":
        table_type = "tabular"
        final_tag = "sidewaystable"
        centering = ""
        rounding_text = " Any values greater than 100 are rounded to the nearest integer for space considerations."
    else:
        table_type = "tabularx"
        final_tag = "table"
        centering = "\n" + r"\centering"
        rounding_text = ""

    footer_text =  rf"""
\hline
\hline
\end{{{table_type}}}
\caption*{{{design_text['citation_text']}. Each experiment is repeated $10,000$ times.{rounding_text}}}
\label{{{design_text['table_tag']}}}
\end{{threeparttable}}{centering}
\end{{{final_tag}}}"""
    
    return footer_text

def table_raw_text(
        dgp: int, 
        rv: int, 
        errors: int,
        small_n: int, 
        large_n: int, 
        top_panel_string: str, 
        bottom_panel_string: str,
        type: str = "estimation"
    ) -> str:
    """Generate complete latex table"""

    full_table = []
    full_table.append(globals()[f"header_text_{type}"]) 
    full_table.append(f"\n{panel_title(small_n, large_n, position="top", type=type)}")
    full_table.append(globals()[f"{type}_info"]) 
    full_table.append(top_panel_string)
    full_table.append(panel_divider)
    full_table.append(f"\n{panel_title(small_n, large_n, position="bottom", type=type)}")
    full_table.append(globals()[f"{type}_info"])
    full_table.append(bottom_panel_string)
    full_table.append(footer_text(dgp, rv, errors, type))

    table = "".join(full_table)
    return table

### =====================================================================
### STOCK TEXT
### =====================================================================

header_text_estimation = rf"""\begin{{sidewaystable}}
\centering
\addtolength{{\tabcolsep}}{{-1pt}} 
\small
\begin{{threeparttable}}
\caption{{Median bias, absolute median deviation and root mean squared error of estimators}}
\vspace{{-0.5em}}
\centering 
\begin{{tabular}}{{c c|c c c c c c|c c c c c c|c c c c c c}}
\hline"""

header_text_inference = rf"""\begin{{table}}[h]
\small
\centering
\begin{{threeparttable}}
\caption{{Coverage rates of confidence intervals}}
\vspace{{-0.5em}}
\centering
\begin{{tabularx}}{{\textwidth}}{{c c|*{{10}}{{>{{\centering\arraybackslash}}X}}}}
\hline"""

estimation_info = rf"""
\hline
& & \multicolumn{{6}}{{c|}}{{Median bias}} & \multicolumn{{6}}{{c|}}{{Median absolute deviation}} & \multicolumn{{6}}{{c}}{{Root mean squared error}} \T \\\cline{{2-3}}
\hline  
$\pi_i$ & $\pi_0$ & $\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ &
$\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ &
$\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ \T \\ 
\hline
"""

inference_info = rf"""
\hline  
$\pi_i$  \rule{{0pt}}{{2.5ex}} & $\pi_0$ & $RBC_{{CCF}}$ & $RBC_{{IK}}$ & $\mcC_{{\Lambda(1)}}^{{CCF}}$ & $\mcC_{{\Lambda(1)}}^{{IK}}$ & $\mcC_{{\Lambda(4)}}^{{CCF}}$ & $\mcC_{{\Lambda(4)}}^{{IK}}$ &
$AR_1$ & $AR_2$ \\
\hline
"""

panel_divider = rf"""
\hline \\
\hline"""

# Dictionary for values of pi_0
pi_values = {
    '0': '0.2',
    '1': '0.4',
    '2': '0.6',
    '3': '0.8'
}

### =========================================================================
### SUMMARY TABLE
### =========================================================================

def header_text_summary(total_params):
    return rf"""\begin{{table}}[h]
\centering
\caption{{Best performing estimator over {total_params} parameter configurations}}
\begin{{tabularx}}{{\textwidth}}{{c*{{6}}{{>{{\centering\arraybackslash}}X}}}}
\hline
\hline
Metric & $\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(1)}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ \\
\hline
"""

def footer_text_summary(total_params):
    return rf"""
\label{{table estimator summary}}
\end{{tabularx}}
\vspace{{-15pt}}
\caption*{{Summary of estimator performance by metric. The new estimators are columns 3-6. There are {total_params} parameters configurations considered in total.}}
\end{{table}}"""

# Dictionary for summary table row names
row_name_dict = {
    '0': 'Med. Bias',
    '1': 'MAD',
    '2': 'RMSE' 
}

def row_string_summary(
        df: pd.DataFrame, 
        row: int,
    ) -> str:
    """Generate row of data as string"""

    total_estimators = df.shape[1]

    row_info = df.iloc[row]

    data_points = [str(int(row_info.iloc[column_idx])) for column_idx in range(0, total_estimators)]

    data_points.insert(0, row_name_dict[str(row)])
    data_string = " & ".join(data_points)

    full_row = data_string + r" \\"

    return full_row

def generate_panel_summary(
        df: pd.DataFrame, 
        start_row: int, 
        end_row: int, 
    ) -> str:
    """Create panel of table"""

    panel_list = []

    for row_idx in range(start_row, end_row):
        panel_list.append(row_string_summary(df, row_idx))
        
    panel_list.append(r'\hline')
    
    return "\n".join(panel_list)

def table_raw_text_summary(
        df,
        total_params,
        total_metrics
    ) -> str:
    """Generate complete latex table"""

    full_table = []
    full_table.append(header_text_summary(total_params))
    full_table.append(generate_panel_summary(df, 0, total_metrics)) 
    full_table.append(r'\hline')
    full_table.append(footer_text_summary(total_params))
    table = "".join(full_table)

    table = "".join(full_table)
    return table


### =========================================================================
### CONCISE ESTIMATOR TEXT TABLE
### =========================================================================

header_text_concise_table = rf"""\begin{{table}}[h!]
\centering
\addtolength{{\tabcolsep}}{{-2pt}} 
\small
\begin{{threeparttable}}
\caption{{Estimator results}}
\vspace{{-0.5em}}
\begin{{tabular*}}{{\textwidth}}{{@{{\extracolsep{{\fill}}}} c c c c c c c c c c c c c c}}
\toprule"""

def estimation_info_concise_table(n):
    text = rf"""
\multicolumn{{2}}{{c}}{{$n={n}$}} & \multicolumn{{4}}{{c}}{{Median bias}} & \multicolumn{{4}}{{c}}{{MAD}} & \multicolumn{{4}}{{c}}{{RMSE}} \\
\cmidrule(r){{1-2}} \cmidrule(r){{3-6}} \cmidrule(l){{7-10}} \cmidrule(l){{11-14}}
$\pi_j$ & $\pi_0$ & $\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ &
$\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ &
$\hat{{\tau}}^{{CCF}}_{{1}}$ & $\hat{{\tau}}^{{IK}}_{{1}}$ & $\hat{{\tau}}^{{CCF}}_{{\Lambda(4)}}$ & $\hat{{\tau}}^{{IK}}_{{\Lambda(4)}}$ \\
\midrule
"""
    
    return text

# concise_column_indices = [6, 7, 10, 11, 12, 13, 16, 17, 18, 19, 22, 23]
concise_column_indices = [7, 8, 11, 12, 13, 14, 17, 18, 19, 20, 23, 24]

def generate_concise_panel(
        df: pd.DataFrame, 
        start_row: int, 
        end_row: int, 
        type: str = "estimation"
    ) -> str:
    """Create panel of table"""

    panel_list = []

    for row_idx in range(start_row, end_row):
        panel_list.append(row_string_concise(df, row_idx, type))
        
        if row_idx > start_row and (row_idx + 1) < end_row and (row_idx + 1) % 4 == 0:
            panel_list.append(r'\hline')
    
    return "\n".join(panel_list)

concise_footer_text =  rf"""
\bottomrule
\end{{tabular*}}
\caption*{{\citet{{lee2008randomized}} design, $X_i \sim N(0,1)$, $U_i \sim N(0,0.09)$. Each experiment is repeated 10,000 times. Any values greater than 100 are rounded to integers for space.}}
\label{{table estimation lee normal concise}}
\end{{threeparttable}}
\end{{table}}"""

double_midrule = rf"""
\midrule
\midrule"""

def concise_table_text(
        df: pd.DataFrame,
        no_rows_subpanel: int,
        no_rows_total: int
    )-> str:
    """Generate complete latex table"""

    full_table = []
    full_table.append(header_text_concise_table) 
    full_table.append(estimation_info_concise_table(300))
    full_table.append(generate_concise_panel(df, 0, no_rows_subpanel, "estimation"))
    full_table.append(double_midrule)
    full_table.append(estimation_info_concise_table(600))
    full_table.append(generate_concise_panel(df, no_rows_subpanel, no_rows_total, "estimation"))
    full_table.append(concise_footer_text)

    return "".join(full_table)

### =========================================================================
### CONCISE INFERENCE TEXT TABLE
### =========================================================================

header_text_concise_table_inference = rf"""\begin{{table}}[h!]
\small
\centering
\begin{{threeparttable}}
\caption{{Coverage rates of confidence intervals}}
\vspace{{-0.5em}}
\begin{{tabularx}}{{\textwidth}}{{cc *{{5}}{{Y}} @{{\hspace{{1.5em}}}} *{{5}}{{Y}}}}
\toprule"""

inference_info_concise_table = rf"""
& & \multicolumn{{5}}{{c}}{{$n = 300$}} & \multicolumn{{5}}{{c}}{{$n = 600$}} \\
\cmidrule(r){{3-7}} \cmidrule(l){{8-12}}
$\pi_j$ & $\pi_0$ & $BC_1$ & $BC_2$ & $\mcC_{{\Lambda(4)}}^{{CCF}}$ & $\mcC_{{\Lambda(4)}}^{{IK}}$ & $AR_2$ &
$BC_1$ & $BC_2$ & $\mcC_{{\Lambda(4)}}^{{CCF}}$ & $\mcC_{{\Lambda(4)}}^{{IK}}$ & $AR_2$ \\
\hline
"""

def row_string_concise(
        df: pd.DataFrame, 
        row: int,
        type: str = "estimation"
    ) -> str:

    if type == "estimation":
        column_indices = [7, 8, 11, 12, 13, 14, 17, 18, 19, 20, 23, 24]
    else:
        column_indices = [25, 26, 29, 30, 32]

    row_info = df.iloc[row]
    data_points = [format_data_point(row_info.iloc[idx], type) for idx in column_indices]

    if type == "inference":
        row_info_paired = df.iloc[row + 12]
        data_points += [format_data_point(row_info_paired.iloc[idx], type) for idx in column_indices]

    data_points.insert(0, pi_values[str(row % ROWS_PER_GROUP)])

    first_col = first_column(row)
    data_string = " & ".join(data_points)

    if first_col:
        return first_col + " & " + data_string + r" \\"
    else:
        return "& " + data_string + r" \\"

def generate_concise_panel(
        df: pd.DataFrame, 
        start_row: int, 
        end_row: int, 
        type: str = "estimation"
    ) -> str:
    """Create panel of table"""

    panel_list = []

    for row_idx in range(start_row, end_row):
        panel_list.append(row_string_concise(df, row_idx, type))
        
        if row_idx > start_row and (row_idx + 1) < end_row and (row_idx + 1) % 4 == 0:
            panel_list.append(r'\hline')
    
    return "\n".join(panel_list)

concise_footer_text =  rf"""
\bottomrule
\end{{tabular*}}
\caption*{{\citet{{lee2008randomized}} design, $X_i \sim N(0,1)$, $U_i \sim N(0,0.09)$. Each experiment is repeated 10,000 times. Any values greater than 100 are rounded to integers for space.}}
\label{{table estimation lee normal concise}}
\end{{threeparttable}}
\end{{table}}"""

concise_footer_text_inference =  rf"""
\bottomrule
\end{{tabularx}}
\caption*{{\citet{{lee2008randomized}} design, $X_i \sim N(0,1)$, $U_i \sim N(0,0.09)$. Each experiment is repeated 10,000 times.}}
\label{{table inference lee normal concise}}
\end{{threeparttable}}
\end{{table}}"""

double_midrule = rf"""
\midrule
\midrule"""

def concise_table_text_inference(
        df: pd.DataFrame,
        no_rows_subpanel: int
    )-> str:
    """Generate complete latex table"""

    full_table = []
    full_table.append(header_text_concise_table_inference) 
    full_table.append(inference_info_concise_table)
    full_table.append(generate_concise_panel(df, 0, no_rows_subpanel, "inference"))
    full_table.append(concise_footer_text_inference)

    return "".join(full_table)