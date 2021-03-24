# Inference-for-Subsamples-in-Complex-Surveys-using-R
This R code computes exact Confidence Intervals for any Complex Survey under any subpopulation regarding the presence of missing data. This script replicates `subpop` command from Stata, based on the research presented in [1]. 

Many practitioners usually oversee this issue, forgetting that the computation of adequate Degrees of Freedom (df) for Confidence Interval's t-distribution must follow special rules. And, using a normal distribution or a gross approximation of *df* could mislead inference. 

As mentioned in [1] this problem is still a matter of research. As far as I studied the literature, there aren't following investigations about this topic.

As far as I know, there isn't an R implementation about this. So, I hope someone finds this useful.

## Bibliography
[1] West, Brady & Berglund, Patricia & Heeringa, Steven. (2008). A Closer Examination of Subpopulation Analysis of Complex-Sample Survey Data. Stata Journal. 8. 520-531.
