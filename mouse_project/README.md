# Mouse project report

Data processing and some statistics about the mouses from [article](10.1371/journal.pone.0119491).

Ahmed, M. M., Dhanasekaran, A. R., Block, A., Tong, S., Costa, A. C., Stasko, M., & Gardiner, K. J. (2015). Protein dynamics associated with failed and rescued learning in the Ts65Dn mouse model of Down syndrome. PloS one, 10(3), e0119491.

## Input data format
Data for analysis should be placed in *Data* folder *.xls* format.\

## Output format
The report is generated automatically when compiling the *.rmd* file from *report* folder into *.html*.

You can get acquainted with some data statistics in the *.html* file from *report* folder. 

Some EDA plots are in in *Data/plots* folder.

# R Version and packages
R version 4.1.3 (2022-03-10) is used.\

tidyverse library ver. 1.3.1, car library ver. 3.0.10 and multcomp library ver. 1.4.17 are used for processing data. Also `readxl` used for read .xls files. For data analysis `car`, `caTools`, `caret`, `lmtest`, `multcomp`, `vegan`, `vegan3d`, `plot3D`, `scatterplot3d`, `gridExtra`, and `limma` were used.
