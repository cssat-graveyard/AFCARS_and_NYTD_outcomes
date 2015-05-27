# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 5/21/2015
# Date updated: 5/27/2015

###############################################################################
## SCRIPT OVERVIEW

# TO DO
#   - review the list of predictor candidates against the codebook again
#   - collapse/format the candidates as appropriate
#   - create a variable of candidate outcomes
#   - review candidate outcomes against the codebook
#   - collapse/format the outcomes as appropriate
#   - pick and fit a logical model against the candidate outcomes
#   - identify a resource for the stepwise fitting and implement it
#   - etc.

# GOAL: This is a scratchpad for importing, exploring, and modeling the AFCARS
#       foster care and NYTD outcomes data. The objective is exploring a variety
#       of approaches for developing a model for predicting certain outcomes
#       observed in the NYTD outcomes wave 2 respondents. 
#
#       First we will contrast the approaches and the models they produce. Then
#       we will use a selected model (or set of models) to develop into an
#       MOS instance.
#
#       More about AFCARS:
#       http://www.acf.hhs.gov/programs/cb/research-data-technology/reporting-systems/afcars
#       http://www.ndacan.cornell.edu/datasets/pdfs_user_guides/AFCARS_Guide_2000-Present.pdf
#
#       More about NYTD:
#       http://www.acf.hhs.gov/programs/cb/resource/about-nytd?page=all
#       http://www.ndacan.cornell.edu/datasets/pdfs_user_guides/182user.pdf
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
#   - I usually build this section on an "as-needed" basis - importing the data
#     as simple as possible and then processing just those elements I find
#     myself needing processing
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
    query_AFCARS_NYTD_raw_data <- 
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
    
    # follow-up queries to help provide context for the cases our primary query
    # observes - how does our sample compare to the pools of AFCARS and NYTD
    # datasets?
    # - BOTH: total records v. unique cases? typical records per case?
    # - AFCARS: gender? ethnicity? socioeconomic status? number entries?
    #           outcomes?
    # - later may want to expand this to compare model-retained variables...
    query_AFCARS_record_counts <- 
        "SELECT 
            COUNT(*) AS num_records,
            COUNT(DISTINCT fc.recnumbr) AS num_cases
         FROM [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc;"
    
    query_AFCARS_record_summaries <- 
        "SELECT 
            AVG(fc.record_counts) AS mean_records_per_case,
            MIN(fc.record_counts) AS min_records_per_case,
	        MAX(fc.record_counts) AS max_records_per_case
        FROM (SELECT COUNT(recnumbr) AS record_counts
	          FROM [CA_ODS].[ndacan].[afcars_foster_care_00_13]
	          GROUP BY recnumbr, st) AS fc;"
    
    query_AFCARS_case_summaries <- 
        "SELECT 
            SUM(CASE WHEN foster.sex = 'Male' THEN 1 ELSE 0 END) AS male_cases,
            SUM(CASE WHEN foster.sex = 'Female' THEN 1 ELSE 0 END) AS female_cases,
	        AVG(foster.totalrem) AS mean_num_removals,
	        SUM(CASE WHEN foster.everadpt = 'Yes, child has been legally adopted' THEN 1 ELSE 0 END) 
                AS ever_adopted_count,
	        SUM(CASE WHEN foster.disreasn = 'Emancipation' THEN 1 ELSE 0 END) AS emancipation_count
        FROM (SELECT RANK() OVER (
		        PARTITION BY fc.recnumbr, fc.st
                ORDER BY fc.datayear DESC
              ) AS datayear_rank,
		        fc.sex,
			    fc.totalrem,
			    fc.everadpt,
			    fc.disreasn
	         FROM [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc
        ) AS foster
        WHERE foster.datayear_rank = 1;"
    
    query_NYTD_record_counts <- 
        "SELECT 
            COUNT(*) AS num_records,
            COUNT(DISTINCT ow.recnumbr) AS num_cases,
	        SUM(CASE WHEN ow.cd_wave = 2 THEN 1 ELSE 0 END) AS num_wave2_candidates,
	        SUM(CASE 
                    WHEN ow.cd_wave = 2 
					    AND ow.responded = 'Responded to Survey'
	                THEN 1 
                    ELSE 0 
            END) AS num_wave2_respondents
        FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow;"
    
    query_NYTD_wave1_case_summaries <- 
        "SELECT 
            COUNT(*) AS total_records,
	        COUNT(DISTINCT ow.recnumbr) AS unique_cases,
	        SUM(CASE WHEN ow.sex = 'Male' THEN 1 ELSE 0 END) AS males,
	        SUM(CASE WHEN ow.sex = 'Female' THEN 1 ELSE 0 END) AS females
        FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow
        WHERE ow.cd_wave = 1;"
    
    query_NYTD_wave2_candidate_case_summaries <-
        "SELECT 
            COUNT(*) AS total_records,
	        COUNT(DISTINCT ow.recnumbr) AS unique_cases,
	        SUM(CASE WHEN ow.sex = 'Male' THEN 1 ELSE 0 END) AS males,
	        SUM(CASE WHEN ow.sex = 'Female' THEN 1 ELSE 0 END) AS females
        FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow
        WHERE ow.cd_wave = 2;"
    
    query_NYTD_wave2_respondent_case_summaries <-
        "SELECT 
            COUNT(*) AS total_records,
            COUNT(DISTINCT ow.recnumbr) AS unique_cases,
	        SUM(CASE WHEN ow.sex = 'Male' THEN 1 ELSE 0 END) AS males,
	        SUM(CASE WHEN ow.sex = 'Female' THEN 1 ELSE 0 END) AS females
        FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow
        WHERE ow.cd_wave = 2 AND ow.responded = 'Responded to Survey';"
    
    # connection name may vary depending on where this script is run from and
    # you need to be logged into an account with appropriate network and
    # database permissions
    connection <- odbcConnect("POC")
    
    # run the main query
    AFCARS_NYTD_raw_data <- sqlQuery(connection, query_AFCARS_NYTD_raw_data,
                                     stringsAsFactors = FALSE,
                                     as.is = TRUE)
    
    # run the follow-up queries
    AFCARS_record_summaries1 <- sqlQuery(connection, query_AFCARS_record_counts,
                                         stringsAsFactors = FALSE,
                                         as.is = TRUE)
    AFCARS_record_summaries2 <- sqlQuery(connection, 
                                         query_AFCARS_record_summaries,
                                         stringsAsFactors = FALSE,
                                         as.is = TRUE)
    AFCARS_case_summaries <- sqlQuery(connection, query_AFCARS_case_summaries,
                                      stringsAsFactors = FALSE,
                                      as.is = TRUE)
    
    NYTD_record_summaries <- sqlQuery(connection, query_NYTD_record_counts,
                                      stringsAsFactors = FALSE,
                                      as.is = TRUE)
    NYTD_wave1_case_summaries <- sqlQuery(connection, 
                                          query_NYTD_wave1_case_summaries,
                                          stringsAsFactors = FALSE,
                                          as.is = TRUE)
    NYTD_wave2_candidate_case_summaries <- 
        sqlQuery(connection, 
                 query_NYTD_wave2_candidate_case_summaries,
                 stringsAsFactors = FALSE,
                 as.is = TRUE)
    NYTD_wave2_respondent_case_summaries <- 
        sqlQuery(connection, 
                 query_NYTD_wave2_respondent_case_summaries,
                 stringsAsFactors = FALSE,
                 as.is = TRUE)
    
    # close the connection
    close(connection)
    
    # clean up the queries
    rm(list = grep("^query", ls(), value = TRUE))
    
    # simplify the follow-up query results
    AFCARS_record_summaries <- data.frame(AFCARS_record_summaries1,
                                          AFCARS_record_summaries2,
                                          stringsAsFactors = FALSE)
    rm(AFCARS_record_summaries1,
       AFCARS_record_summaries2)
    
    NYTD_wave_summaries <- rbind(wave1 = NYTD_wave1_case_summaries,
                                 wave2_candidates = 
                                     NYTD_wave2_candidate_case_summaries,
                                 wave2_respondents = 
                                     NYTD_wave2_respondent_case_summaries)
    rm(NYTD_wave1_case_summaries,
       NYTD_wave2_candidate_case_summaries,
       NYTD_wave2_respondent_case_summaries)
    
    # if the user requested an Rda, make one in the working directory
    if(make_a_raw_Rda) {
        save(AFCARS_NYTD_raw_data, 
             AFCARS_record_summaries,
             AFCARS_case_summaries,
             NYTD_record_summaries,
             NYTD_wave_summaries,
             file = raw_Rda_path)
    }
}

# if a copy of the pre-processed data has been saved as an Rda and we want to 
# start with that, load it (will overwrite the scratch data if both are 
# requested)
if(load_raw_Rda) {
    load(raw_Rda_path)
}

###############################################################################
## Data Processing

# create the list of variables that are candidates to be predictors
predictor_candidates <- subset(AFCARS_NYTD_raw_data, 
                               select = -c(70:87))
predictor_candidates <- subset(predictor_candidates,
                               select = -c(datayear_rank,
                                           stchid,
                                           recnumbr,
                                           st,
                                           dob,
                                           pd_dob))
predictor_candidates <- names(predictor_candidates)

###############################################################################
## Data Exploration

###############################################################################
## Building Models
#   - logical

#   - stepwise

#   - random forest

#   - Bayesian model average

#   - lasso

#   - Gaussian processes
#       - http://www.gaussianprocess.org/