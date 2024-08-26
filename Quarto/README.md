# Fer ~ dHb relation

Plotting and fitting the relation between Ferritin measurements and the difference between Hb measurements using Quarto:

In this folder you will find the Quarto scripts, numbered in the order that they are used for the analysis. 

The analysis starts with preparing the data [in file 0](0_LoadData.qmd).
Then, the functions for the analysis are described [in file 1](1_Functions.R).
The main analysis code is contained [in file 2](2_Analysis.qmd). We do not recommend rendering this script until you've tweaked the analysis parameters as described in the file to your liking, as the analysis may take a while to complete.
In [file 4](4_Table1.qmd) and [file 7](7_ForestPlot.qmd) the scripts used for the production of tables and figures in the manuscript can be found.
In [file 5](5_SubgroupAnalysis.qmd) we describe additional analyses, run on subgroups of the data. 
