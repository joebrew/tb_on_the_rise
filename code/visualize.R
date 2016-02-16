# Directories
root <- getwd() # should be the tb_on_the_rise directory
setwd('data')
data_dir <- getwd()
setwd(root)
setwd('code')
code_dir <- getwd()
setwd(root)

# Libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(readr)

# Read and clean
source(paste0(code_dir, '/read_and_clean.R'))
