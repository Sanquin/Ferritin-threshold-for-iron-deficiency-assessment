# Fer ~ dHb relation

Plotting and fitting the relation between Ferritin measurements and the difference between Hb measurements.

## Usage:
1. to create data files including subsets:  
    `Rscript DataSelection.R -f donations.rds -l "/home/user/data"`

2. To fit and plot relation:  
   `Rscript Analysis_code.R -f data.rds --bootstraps=0`

3. To do the fits on the subsets of the data:  
   `Rscript SubgroupAnalysis.R -f "/home/user/data"`


