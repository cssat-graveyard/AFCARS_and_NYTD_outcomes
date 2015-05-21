# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 5/21/2015
# Date updated: 

###############################################################################
## SCRIPT OVERVIEW

# GOAL: This is a scratchpad for importing, exploring, and modeling the AFCARS
#       foster care and NYTD outcomes data. The objective is exploring a variety
#       of approaches for developing a model for predicting certain outcomes
#       observed in the NYTD outcomes wave 2 respondents. 
#
#       First we will contrast the approaches and the models they produce. Then
#       we will use a selected model (or set of models) to develop into an
#       MOS instance.
#
# SCRIPT OUTLINE:
# - Load Supporting Packages and Scripts
# - Load the Data
#   - from SQL if we want to start from scratch
#   - from an .Rds file if we already have a cleaned dataset available
# - Data Processing
#   - if we started from scratch, here is where the data is processed to be
#     ready for analysis
# - Data Exploration
# - Building Models
#   - logical
#   - stepwise
#   - random forest
#   - Bayesian model average
#   - lasso
#   - Gaussian processes
#       - http://www.gaussianprocess.org/
# - Comparing Models

###############################################################################
## Load Supporting Packages and Scripts

library(RODBC)

###############################################################################
## Load the Data

# define some preliminaries
## load from scratch? if FALSE, an Rds path needs to be provided
from_scratch <- TRUE
## make an Rds? only appropriate if starting from scratch
make_an_Rds <- TRUE
## Rds file pathway? if loading from Rds
Rds_path <- "./AFCARS_NYTD_data.Rds"

# load from scratch or from an .Rds, as appropriate
if(from_scratch) {
    # the from scratch approach gets the data from the POC CA_ODS database via
    # the RODBC package
    
    # connection name may vary depending on where this script is run from and
    # you need to be logged into an account with appropriate network and
    # database permissions
    connection <- odbcConnect("POC")
    
    # define the SQL query to get the joined-but-rough AFCARS and NYTD outcomes
    # tables
    AFCARS_NYTD_query <- 
        "SELECT "
}