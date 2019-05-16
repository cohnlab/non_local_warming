# non_local_warming
Replication code for the ERL submission: "Forest loss in Brazil increases maximum temperatures within 50km"


The dataset used for this analysis can be found in "https://osf.io/wvr93/?view_only=381e1bde779349e08a74f86c5afc4c07".
The code assumes that the data has been downloaded and saved as "dataset_weights.csv" in the same working directory as the code.

### R scripts to replicate figures:
make\_figure5\_nonlocal.R, make\_figure5\_halo.R  :  replicates Figure 5 in two parts, Figure5\_halo.eps\ and Figure5_nonlocal.eps, which can be found in the 'figures' subfolder. \\
make\_figure6.R  : replicates Figure 6 in two parts, Figure6A.eps and Figure6B.eps

### R scripts to replicate tables:
make\_table1.R : code to replicate Table 2.  This script produced four uncompiled latex files, corresponding to Table 1 sections A-D, which can be found in the 'tables' subfolder as 'table1A','table1B','table1C','table1D'\\

make\_table2.R : code to replicate Table 2.  The uncompiled latex file can be found in the 'tables' subfolder as 'table2'
