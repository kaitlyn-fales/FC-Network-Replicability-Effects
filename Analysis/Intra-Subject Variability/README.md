The contents of this folder are as follows:

**Baseline Curves:** This subfolder contains the final baseline curves as .RData files constructed from the bootstrapping procedure to use for plotting results. 

**Bootstrap Data:** Empty subfolder for bootstrapped datasets from running norm_pdiv_baseline_bootstrap.R script. 

**Bootstrap Results:** Empty subfolder for bootstrapped results from running norm_pdiv_baseline_bootstrap.R script. 

**frobenius_norm_bootstrap_comparison.R:** This R script takes the frobenius_norm_result.csv and the bootstrapped results (optional) and draws the pairwise comparisons for pipeline and atlas. If you choose not to run the bootstrap due to computation time, the Baseline Curves folder contains the results to plot with the observed data. The code indicates which part to skip and resume from.

**frobenius_norm_result.csv:** Output file of norm_pdiv_data_calculation.R for Frobenius norm comparison of observed data.

**get_upper_tri_labels.R:** This R script obtains the labels in order to reconstruct the vectorized FC network into the proper matrix, used for portrait divergence calculations. The output is upper_triangle_labels.RData.

To run the following *two* scripts, you need to download the portrait_divergence.py Python script from Bagrow and Bollt (2019) repository and put in the Intra-Subject Variability folder: https://github.com/bagrow/network-portrait-divergence/tree/master 

**norm_pdiv_baseline_bootstrap.R:** The R script to conduct the baseline bootstrap procedure for intra-subject variability. This script takes about 15-16 hours to run, depending on the number of cores available for parallel computing. If you want to skip this script and only plot results, skip to the bootstrap_comparison scripts. 

**norm_pdiv_data_calculation.R:** The R script to get intra-subject variability results on the observed data for the Frobenius norm and portrait divergence metrics. This takes about 10 minutes to run, depending on the number of cores available for parallel computing. The outputs of this script are the two .csv files and can be used directly for plotting.

**pdiv_bootstrap_comparison.R:** This R script takes the pdiv_result.csv and the bootstrapped results (optional) and draws the pairwise comparisons for pipeline and atlas. If you choose not to run the bootstrap due to computation time, the Baseline Curves folder contains the results to plot with the observed data. The code indicates which part to skip and resume from.

**pdiv_result.csv:** Output file of norm_pdiv_data_calculation.R for portrait divergence comparison of observed data.

**upper_triangle_labels.RData:** Output file of get_upper_tri_labels.R, used for portrait divergence calculations. 
