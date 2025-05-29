# movielens
Movielens project for HarvardX Data Science Capstone

The project consists of 3x files:
- movielens_report.pdf
- movielens_scripts.R
- movielens_report.Rmd

"movielens_scripts.R" was created in R version 4.5.0 and RStudio/2025.05.0+496. It will run without error and install several packages and download the MovieLens 10M dataset to your working directory. A "data" directory with .rds versions of the train and final holdout set will also be created. One of these files, "edx.rds" is needed to knit "movielens_report.Rmd" if desired. The "movielens_scripts.R" script also performs a randomized PCA step that can be computationally demanding for older CPU with low RAM. 
