This folder contains scripts to access and download the raw data used in this project. 
- `01_UCMR5_processing.R`: loads Fifth Unregulated Contaminant Monitoring Rule (UCMR 5) data (final data release) for PFAS, and filters for groundwater data and sites with fewer entry points based on descriptive analyses
- `02a_PFAS_WQP_fetch.R`: uses the `dataRetrieval` R package to download raw PFAS water quality data based on specified parameters from the National Water Quality Portal (WQP)
- `02b_PFAS_WQP_summary.qmd`: calculates various summary statistics, produces descriptive plots, filters the PFAS WQP data for the target analytes and dates, and retains the most recent sample for each site
- `03_combine_UCMR_WQP.R`: combines both datasets into a single dataset for testing/training with common columns and location descriptors
