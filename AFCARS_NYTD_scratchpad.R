# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 5/21/2015
# Date updated: 5/26/2015

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
#   - from a raw .Rda file if already loaded the data and saved a copy
#     of it before R processing
# - Data Processing
#   - if we started from scratch, here is where the data is processed to be
#     ready for analysis
#   - if we started from a cleaned .Rda file, we skip the processing and
#     simply load the cleaned data
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
## load from scratch? if FALSE, the correct Rda path(s) needs to be provided
from_scratch <- FALSE
## make a raw Rda? only appropriate if starting from scratch AND want to
## make a copy of data prior to R processing
make_a_raw_Rda <- TRUE

## load an Rda of the raw data?
load_raw_Rda <- TRUE

## load an Rda of the processed data?
load_processed_Rda <- FALSE

## raw Rda file pathway? if creating or loading a raw Rda file
raw_Rda_path <- "./AFCARS_NYTD_raw_data.Rda"
## processed Rda file pathway?
processed_Rda_path <- "./AFCARS_NYTD_processed_data.Rda"

# load from scratch or from an .Rda, as appropriate
if(from_scratch) {
    # the from scratch approach gets the data from the POC CA_ODS database via
    # the RODBC package
    
    # define the SQL query to get the joined-but-rough AFCARS and NYTD outcomes
    # tables
    AFCARS_NYTD_query <- 
        "SELECT *
         FROM (
            SELECT RANK() OVER (
                PARTITION BY fc.recnumbr, fc.st, pd.stchid
                ORDER BY fc.datayear DESC
            ) AS datayear_rank,
            pd.stchid,
            fc.recnumbr,
            fc.st,
            fc.\"state\",
            fc.fipscode,
            fc.dob,
            pd.dob AS pd_dob,
            pd.dobyr,
            pd.dobmon,
            fc.sex,
            fc.amiakn,
            fc.asian,
            fc.blkafram,
            fc.hawaiipi,
            fc.hisorgin,
            fc.white,
            fc.untodetm,
            fc.clindis,
            fc.mr,
            fc.vishear,
            fc.phydis,
            fc.dsmiii,
            fc.othermed,
            fc.everadpt,
            fc.ageadopt,
            fc.totalrem,
            fc.numplep,
            fc.manrem,
            fc.phyabuse,
            fc.sexabuse,
            fc.neglect,
            fc.aaparent,
            fc.daparent,
            fc.aachild,
            fc.dachild,
            fc.childis,
            fc.chbehprb,
            fc.prtsdied,
            fc.prtsjail,
            fc.nocope,
            fc.abandmnt,
            fc.relinqsh,
            fc.housing,
            fc.curplset,
            fc.placeout,
            fc.casegoal,
            fc.ctkfamst,
            fc.ctk1yr,
            fc.ctk2yr,
            fc.fosfamst,
            fc.fcctk1yr,
            fc.fcctk2yr,
            fc.disreasn,
            fc.ivefc,
            fc.iveaa,
            fc.ivaafdc,
            fc.ivdchsup,
            fc.xixmedcd,
            fc.ssiother,
            fc.noa,
            fc.fcmntpay,
            fc.rem1dt,
            fc.latremdt,
            fc.dodfcdt,
            fc.latremlos,
            fc.lifelos,
            fc.ageatlatrem,
            fc.agedout
        FROM [CA_ODS].[ndacan].[NYTD_Outcomes_people_dim] AS pd
        INNER JOIN [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc
            ON pd.recnumbr = fc.recnumbr
                AND pd.st = fc.st
    ) AS foster
    INNER JOIN (
        SELECT
            nor.stchid,
            nor.dob AS nor_dob,
            nor.currpte,
            nor.currfte,
            nor.emplysklls,
            nor.socsecrty,
            nor.educaid,
            nor.pubfinas,
            nor.pubfoodas,
            nor.pubhousas,
            nor.othrfinas,
            nor.highedcert,
            nor.currenroll,
            nor.cnctadult,
            nor.homeless,
            nor.subabuse,
            nor.incarc,
            nor.children
        FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS nor
        WHERE nor.cd_wave = 2
    ) AS outcomes
    ON foster.stchid = outcomes.stchid
        AND foster.pd_dob = outcomes.nor_dob
        AND foster.datayear_rank = 1;"
    
    # connection name may vary depending on where this script is run from and
    # you need to be logged into an account with appropriate network and
    # database permissions
    connection <- odbcConnect("POC")
    
    # run the query
    AFCARS_NYTD_raw_data <- sqlQuery(connection, AFCARS_NYTD_query,
                                     stringsAsFactors = FALSE,
                                     as.is = TRUE)
    
    # close the connection
    close(connection)
    
    # if the user requested an Rda, make one in the working directory
    if(make_a_raw_Rda) {
        save(AFCARS_NYTD_raw_data, file = raw_Rda_path)
    }
}

# if a copy of the pre-processed data has been saved as an Rda and we want to 
# start with that, load it (will overwrite the scratch data if both are 
# requested)
if(load_raw_Rda) {
    load(raw_Rda_path)
}