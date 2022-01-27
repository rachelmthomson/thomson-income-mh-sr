# thomson-income-mh-sr

*Data & analysis files for 2022 Thomson et al systematic review of income change & mental health*
 
Includes .dta file of extracted data and all R analysis files required to replicate findings
 
DATA FILES:

cleaned.dta = data at datapoint level

Creating databases.R = creates R dataframes required for analyses from cleaned.dta (set to auto-run at beginning of files requiring these)

ANALYSIS FILES:

Descriptives.R = runs basic descriptive analysis

SWiM.R = runs all synthesis without meta-analysis and produces effect direction plots

Running MAs.R = runs main meta-analyses (autoruns at beginning of files requiring this)

Running meta-regressions.R = reruns meta-analysis, then runs main meta-regressions

Own SDs.R = runs sensitivity analysis excluding studies where standard deviations for transformations came from external sources

FILES FOR OUTPUTS:

Making forest plots.R = re-runs meta-analysis and produces plots

Making funnel plots.R = re-runs meta-analysis, runs Eggers test and generates funnel plots

Making tables.R = re-runs meta-regression file and generates table of results and table of N/Obs for each analysis
