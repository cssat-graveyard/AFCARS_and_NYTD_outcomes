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
#   - basic model inferential selection approaches
#       - null
#       - everything
#       - intutive guesses at predictors
#       - inuitive stepwise guesses at predictors (Joe observes this as a common
#           thing)
#           - http://www.statmethods.net/stats/regression.html
#           - use MASS:stepAIC
#   - inferential approaches that emphasize reduction in overfitting
#       - more controlled stepwise
#           - http://www.statmethods.net/stats/regression.html
#           - use MASS::stepAIC, maybe with log(k) as your penalty (so it's really BIC)
#       - Bayesian model average
#           - http://www.research.att.com/~volinsky/papers/paper1.ps
#           - other: http://www2.research.att.com/~volinsky/bma.html
#           - alt package: http://bms.zeugner.eu/
#           - multinomial specific: http://cran.r-project.org/web/packages/mlogitBMA/mlogitBMA.pdf
#       - lasso
#   - emphasize building predictive model
#       - random forest
#           - https://www.google.com/webhp?sourceid=chrome-instant&ion=1&espv=2&es_th=1&ie=UTF-8#es_th=1&q=r%20random%20forest
#           - http://topepo.github.io/caret/Random_Forest.html
#   - not sure where it fits...?
#       - Gaussian processes
#           - http://www.gaussianprocess.org/
# - Comparing Models
#   - overlap in features among the models? (e.g., a yes/no heatmap of var
#     by model)
#   - size of retained feature set by model? (estimate of complexity)
#   - inferential implications by model?
#   - predictive validity by model?

###############################################################################
## Load Supporting Packages and Scripts

library(RODBC)
library(reshape2)
library(tidyr)
library(weights)
library(ggplot2)
library(ggthemes)
library(nnet)
library(randomForest)
library(caret)
library(BMA)
library(Rborist)

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
load_processed_Rda <- TRUE

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
            nor.children,
            nor.weight
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

if(load_processed_Rda) {
    load(processed_Rda_path)
    stop("Processed data loaded.")
}

###############################################################################
## Initial Data Exploration and Processing

# create a "processed" variant of the dataset (AFCARS_NYTD_processed_data or 
# "anpd")
anpd <- AFCARS_NYTD_raw_data

# assess summary by variable (go through the variables one at time after this
# object is defined)
sum_by_var <- apply(anpd, 2, function(x) {table(x, useNA = "always")})

# assess completeness overall
nrow(anpd)
sum(complete.cases(anpd))

# assess completeness by variable
na_by_var <- apply(anpd, 2, function(x) {sum(is.na(x))})
barplot(na_by_var, las = 2)
na_by_var[na_by_var > 0]

na_by_case <- apply(anpd, 2, is.na)
na_by_case <- data.frame(na_by_case)
na_by_case$case_num <- 1:nrow(na_by_case)
na_by_case_melt <- melt(na_by_case, id.vars = "case_num")

ggplot(na_by_case_melt, aes(x = variable, y = case_num, fill = value)) +
    geom_tile() +
    theme(axis.text.x = element_text(size = 12, angle = -90, 
                                     hjust = 0, vjust = 0.5)
    )

# everadpt (ever adopted) is a subsetting variable; checking to see what the
# rate of missing data for ageadopt is when everadpt == TRUE
table(anpd$everadpt == TRUE & 
          (is.null(anpd$ageadopt) | 
               anpd$ageadopt == "Not applicable" | 
               anpd$ageadopt == "Unable to determine"))

# no "TRUE" and the amount of missing values aligns with the number of NAs
# for everadpt - so all is well!

# we have a couple of variables with high rates of missing data and which - at
# face value - don't seem interesting; we drop these
anpd <- subset(anpd, select = -c(ctk2yr, fcctk1yr, fcctk2yr))

# now let's reassess completeness and look for cases that have poor completion 
# rates
na_set <- is.na(anpd)
na_count_by_case <- apply(na_set, 1, sum)
table(na_count_by_case)
sum(na_count_by_case > 6)

# we have 103 cases with 7 or more blank fields, we take a quick check for
# possible covariates with the incompletes
table(anpd$st, na_count_by_case)
table(anpd$fipscode, na_count_by_case)
table(anpd$sex, na_count_by_case)
table(anpd$amiakn, na_count_by_case)
table(anpd$asian, na_count_by_case)
table(anpd$blkafram, na_count_by_case)
table(anpd$hawaiipi, na_count_by_case)
table(anpd$hisorgin, na_count_by_case)
table(anpd$white, na_count_by_case)

# most of these appear to come from LA, California... it seems like minorities
# may be more likely to have incomplete cases, but all race/ethnicities seem
# well enough represented that dropping the cases should not unfairly
# bias the results... still one more peek for patterns
high_na_cases <- anpd[na_count_by_case > 6,]
sum_by_var_nas <- apply(high_na_cases, 2, 
                        function(x) {table(x, useNA = "always")})

# it's possible that the missing value are associated with a variable, but I
# see no obvious candidates... as a result I simply truncate the cases with
# poor completion rates (>6 missing fields)
anpd <- anpd[na_count_by_case < 7,]

# inspect our NAs one last time
na_by_case <- apply(anpd, 2, is.na)
na_by_case <- data.frame(na_by_case)
na_by_case$case_num <- 1:nrow(na_by_case)
na_by_case_melt <- melt(na_by_case, id.vars = "case_num")

ggplot(na_by_case_melt, aes(x = variable, y = case_num, fill = value)) +
    geom_tile() +
    theme(axis.text.x = element_text(size = 12, angle = -90, 
                                     hjust = 0, vjust = 0.5)
    )

# looking better... let's move along for now!

# next, we don't want any implicit NAs... we may want to drop some cases with
# problematic NAs, but otherwise we can identify our NAs more explicitly
# as "Undetermined"
for(index in 1:ncol(anpd)) {
    anpd[, index][is.na(anpd[, index])] <- "Undetermined"
}

# adjust variable types where needed
anpd$st <- factor(anpd$st)
anpd$state <- factor(anpd$state)
anpd$fipscode <- factor(anpd$fipscode, 
                        levels = c(sort(unique(anpd$fipscode))))
anpd$dobyr <- factor(anpd$dobyr)
anpd$dobmon <- factor(anpd$dobmon, levels = c(1:12), 
                      labels = c("January", "February", "March", "April",
                                 "May", "June", "July", "August",
                                 "September", "October", "November", 
                                 "December"))
anpd$sex <- factor(anpd$sex)
anpd$amiakn <- factor(anpd$amiakn)
anpd$asian <- factor(anpd$asian)
anpd$blkafram <- factor(anpd$blkafram)
anpd$hawaiipi <- factor(anpd$hawaiipi)
# hisorigin is the only race factor that has values other than "Yes" and "No"
# however we just want the untodetm factor to capture our unknowns, so we flip
# flip the "Not applicable" and "Unable to determine" values to "No"; in a
# moment we'll update untodetm to capture cases where no race/ethnicity is
# marked at all
anpd$hisorgin[which(anpd$hisorgin == "Not applicable" | 
                        anpd$hisorgin == "Unable to determine")] <- "No"
anpd$hisorgin <- factor(anpd$hisorgin)
anpd$white <- factor(anpd$white)
# untodetm is an odd factor - it is sometimes not marked when it should be
# (i.e., when we have no race/ethnicity categories marked) and sometimes marked
# when it should not be (i.e., when we do have one or more race categories
# marked) - we'll update it to behave in a useful manner
get_yes_counts <- function(df) {
    # get just the race/ethnicity columns
    df <- dplyr::select(df, amiakn:white)
    
    # we've made "No" our first level and there should only be two levels
    # for each, so we can treat "Yes" as 1 and "No" as 0 if we subtract 1
    # from the level numbers
    for(index in 1:ncol(df)) {
        df[, index] <- as.numeric(df[, index]) - 1
    }
    
    # now we can just add by row and we'll get the total number of 
    # races/ethnicities each case is associated with
    race_count <- apply(df, 1, sum)
    
    # return the counts for each case
    return(race_count)
}
yes_counts <- get_yes_counts(anpd)
anpd$untodetm <- ifelse(yes_counts == 0, "Yes", "No")
anpd$untodetm <- factor(anpd$untodetm)
# # now we want to collect our race variables into a single column, including
# # possible multi-racial groups
# get_race_category <- function(df) {
#     # get just the race/ethnicity columns, now including undetermined
#     df <- dplyr::select(df, amiakn:untodetm)
#     
#     # define a bucket to collect row results
#     race_by_row <- c()
#     
#     # we've made "No" our first level and there should only be two levels
#     # for each, so we can treat "Yes" as 1 and "No" as 0 if we subtract 1
#     # from the level numbers; for each column, we collection the column name
#     # IF the value is a 1
#     for(row_index in 1:nrow(df)) {
#         # define a bucket to collect column names associated with "Yes"
#         col_matches <- c()
#         
#         # cycle through the columns and collect relevant column names
#         for(col_index in 1:ncol(df)) {
#             if(df[row_index, col_index] == "Yes") {
#                 col_matches <- c(col_matches, names(df)[col_index])
#             }
#         }
#         
#         race_by_row <- c(race_by_row, paste0(col_matches, collapse = "_"))
#     }
#     
#     # return the race_by_row results
#     return(race_by_row)
# }
# 
# test <- get_race_category(anpd)
anpd$clindis[which(anpd$clindis == "Not yet determined")] <- "Undetermined"
anpd$clindis <- factor(anpd$clindis, 
                       levels = c("Undetermined", "No", "Yes"))
anpd$mr <- factor(anpd$mr,
                  levels = c("Undetermined", "No", "Yes"))
anpd$vishear <- factor(anpd$vishear,
                       levels = c("Undetermined", "No", "Yes"))
anpd$phydis <- factor(anpd$phydis,
                      levels = c("Undetermined", "No", "Yes"))
anpd$dsmiii <- factor(anpd$dsmiii,
                      levels = c("Undetermined", "No", "Yes"))
anpd$othermed <- factor(anpd$othermed,
                        levels = c("Undetermined", "No", "Yes")) 
anpd$everadpt[which(anpd$everadpt == "Unable to determine")] <- "Undetermined"
anpd$everadpt <- factor(anpd$everadpt,
                        levels = c("Undetermined",
                                   "No, has never been legally adopted",
                                   "Yes, child has been legally adopted"),
                        labels = c("Undetermined", "No", "Yes"))
anpd$ageadopt[which(anpd$ageadopt == "Unable to determine")] <- "Undetermined"
anpd$ageadopt <- factor(anpd$ageadopt,
                        levels = c("Not applicable",
                                   "Undetermined",
                                   "Less than 2 years old", 
                                   "2-5 years old",
                                   "6-12 years old",
                                   "13 years or older"),
                        labels = c("NA",
                                   "Undetermined",
                                   "Less than 2 years",
                                   "2-5 years",
                                   "6-12 years",
                                   "13+ years"))
# if a child was never adopted, their ageadopt should be "NA";
# if adotpion was uncertain, their ageadopt should be uncertain
# if a child was adopted, they should have an age or age should be uncertain
anpd$ageadopt[which(anpd$everadpt == "No")] <- "NA"
anpd$ageadopt[which(anpd$everadpt == "Undetermined")] <- "Undetermined"
sum(anpd$ageadopt == "NA" & anpd$everadpt == "Yes") # no kids have this issue
anpd$manrem[which(anpd$manrem == "Not yet determined")] <- "Undetermined"
anpd$manrem <- factor(anpd$manrem,
                      levels = c("Undetermined",
                                 "Court ordered",
                                 "Voluntary"))
anpd$phyabuse <- factor(anpd$phyabuse,
                        levels = c("Undetermined", "No", "Yes"))
anpd$sexabuse <- factor(anpd$sexabuse,
                        levels = c("Undetermined", "No", "Yes"))
anpd$neglect <- factor(anpd$neglect,
                       levels = c("Undetermined", "No", "Yes"))
anpd$aaparent <- factor(anpd$aaparent,
                        levels = c("Undetermined", "No", "Yes"))
anpd$daparent <- factor(anpd$daparent,
                        levels = c("Undetermined", "No", "Yes"))
anpd$aachild <- factor(anpd$aachild,
                       levels = c("Undetermined", "No", "Yes"))
anpd$dachild <- factor(anpd$dachild,
                       levels = c("Undetermined", "No", "Yes"))
anpd$childis <- factor(anpd$childis,
                       levels = c("Undetermined", "No", "Yes"))
anpd$chbehprb <- factor(anpd$chbehprb,
                        levels = c("Undetermined", "No", "Yes"))
anpd$prtsdied <- factor(anpd$prtsdied,
                        levels = c("Undetermined", "No", "Yes"))
anpd$prtsjail <- factor(anpd$prtsjail,
                        levels = c("Undetermined", "No", "Yes"))
anpd$nocope <- factor(anpd$nocope,
                      levels = c("Undetermined", "No", "Yes"))
anpd$abandmnt <- factor(anpd$abandmnt,
                        levels = c("Undetermined", "No", "Yes"))
anpd$relinqsh <- factor(anpd$relinqsh,
                        levels = c("Undetermined", "No", "Yes"))
anpd$housing <- factor(anpd$housing)
anpd$curplset <- factor(anpd$curplset,
                        levels = rev(unique(anpd$curplset)))
anpd$placeout <- factor(anpd$placeout,
                        levels = c("Undetermined", "No", "Yes"))
anpd$casegoal <- factor(anpd$casegoal,
                        levels = rev(unique(anpd$casegoal)))
anpd$ctkfamst[which(anpd$ctkfamst == "Unable to determine")] <- "Undetermined"
anpd$ctkfamst <- factor(anpd$ctkfamst,
                        levels = c("Undetermined",
                                   "Single female",
                                   "Single male",
                                   "Unmarried couple",
                                   "Married couple"))
anpd$fosfamst <- factor(anpd$fosfamst,
                        levels = c("Not applicable",
                                   "Undetermined",
                                   "Single female",
                                   "Single male",
                                   "Unmarried couple",
                                   "Married couple"))
anpd$disreasn <- factor(anpd$disreasn,
                        levels = c("Not applicable",
                                   "Undetermined",
                                   "Adoption",
                                   "Emancipation",
                                   "Guardianship",
                                   "Living with other relative(s)",
                                   "Reunified with parent, primary caretaker",
                                   "Runaway",
                                   "Transfer to another agency"))
anpd$ivefc <- factor(anpd$ivefc, levels = c("Undetermined", "No", "Yes"))
anpd$iveaa <- factor(anpd$iveaa)
anpd$ivaafdc <- factor(anpd$ivaafdc)
anpd$ivdchsup <- factor(anpd$ivdchsup)
anpd$xixmedcd <- factor(anpd$xixmedcd,
                        levels = c("Undetermined", "No", "Yes"))
anpd$ssiother <- factor(anpd$ssiother,
                        levels = c("Undetermined", "No", "Yes"))
anpd$noa <- factor(anpd$noa)
anpd$fcmntpay[which(anpd$fcmntpay == "Undetermined")] <- "0"
anpd$fcmntpay <- as.numeric(anpd$fcmntpay)
anpd$lifelos <- as.numeric(anpd$lifelos)
anpd$ageatlatrem <- as.numeric(anpd$ageatlatrem)
anpd$agedout <- factor(anpd$agedout)
anpd$weight[which(anpd$weight == "Undetermined")] <- "1"
anpd$weight <- as.numeric(anpd$weight)
anpd$totalrem <- as.numeric(anpd$totalrem)
anpd$numplep <- as.numeric(anpd$numplep)

# our key outcome - homelessness - also has low rates for the less interesting
# outcomes (NA and chose not to disclose); we drop cases with these outcomes
anpd <- anpd[-which(anpd$homeless == 2 | anpd$homeless == 77),]
anpd$homeless <- factor(anpd$homeless,
                        levels = c("0", "1"),
                        labels = c("No", "Yes"))

# we allowed our continuous variables to keep their NAs for a moment...
# we double check the NA rates across our dataset
apply(anpd, 2, function(x) {sum(is.na(x))})

# we drop the few cases with NAs for totalrem and numplep - we'll deal with
# the large number of missing cases from lifelos in a moment
anpd <- anpd[-which(is.na(anpd$totalrem)), ]
anpd <- anpd[-which(is.na(anpd$numplep)), ]

# finally, we have been replacing categorical variable NAs with "undetermined" -
# we check to see if any of these have especially low rates of "undetermined"
# (we may want to drop the category in these cases)
sort(apply(anpd, 2, function(x) sum(x == "Undetermined")), decreasing = TRUE)

# we have a number of variables where the values are pretty small... we'll
# drop incomplete cases for the variables where there are less than 20 
# "Undetermined" cases (ssiother, xixmedcd, ivefc) (note: only concerned about
# variables we'll actually be using in our models)
anpd <- anpd[-which(anpd$ssiother == "Undetermined"), ]
anpd <- anpd[-which(anpd$xixmedcd == "Undetermined"), ]
anpd <- anpd[-which(anpd$ivefc == "Undetermined"), ]

# at this point, we have removed the "Undetermined" cases from a number of
# variables; we drop any unused levels from our data frame to clear these
# out
anpd <- drop.levels(anpd, reorder = FALSE)

# we still have an issue with "lifelos" having a large number of NAs and since
# it is continuous we can't just use our "Undetermined" trick; instead, we may
# want to either impute or drop the variable

# we'll do two steps at once here: we'll all our variable pairs for collinearity
# (in quick and dirty manner) while also looking to see if their are
# relationships we can use to impute lifelos values

# first, we identify our predictor candidates explicitly (dropping variables 
# that were problematic or don't make sense for our purposes)
predictor_candidates <- subset(AFCARS_NYTD_raw_data, 
                               select = -c(70:88))
predictor_candidates <- subset(predictor_candidates,
                               select = -c(datayear_rank,
                                           fipscode,
                                           stchid,
                                           recnumbr,
                                           st,
                                           dob,
                                           pd_dob,
                                           ctk1yr,
                                           ctk2yr,
                                           fcctk1yr,
                                           fcctk2yr,
                                           rem1dt,
                                           latremdt,
                                           dodfcdt,
                                           latremlos))
predictor_candidates <- names(predictor_candidates)

# now we create a restricted version of our dataset with just the predictor
# candidates and the outcome and weight variables
homeless_data <- anpd[, c(predictor_candidates, "homeless", "weight")]

# we'll flip all our factors to numeric
flip_to_numbers <- function(df) {
    for(index in 1:ncol(df)) {
        df[, index] <- as.numeric(df[, index])
    }
    
    return(df)
}
test <- flip_to_numbers(homeless_data)

# and then compare all the pairs to look for sizeable correlations (> 0.50)
get_high_corrs <- function(df) {
    high_corrs <- list()
    for(var_index in 1:ncol(df)) {
        current_var <- names(df)[var_index]
        for(col_index in var_index:ncol(df)) {
            pair_var <- names(df)[col_index]
            current_corr <- cor(df[, var_index], df[, col_index],
                                use = "complete.obs")
            if(abs(current_corr) > 0.5 & current_var != pair_var) {
                pair_name <- paste0(current_var, "-", pair_var)
                pair_corr <- list(pair = pair_name, 
                                  corr = current_corr)
                high_corrs <- c(high_corrs, pair_corr)
            }
        }
    }
    
    return(high_corrs)
}

high_corrs <- get_high_corrs(test)

# there are a few strong correlations we might want to keep an eye on
# (clindis-dsmiii, xixmedcd-noa) but the niftiest thing we find is a crazy
# strong correlation (~0.92!) between lifelos and ageatlatrem - likely a
# result of the fact that our sample largely draws on folks who emancipated
# or otherwise left child welfare at a very old age; we'll avoid the tricky
# imputation issue by simply dropping lifelos in favor of of ageatlatrem, both
# because it's more complete and because it's probably more reliable
homeless_data <- dplyr::select(homeless_data, -lifelos)
predictor_candidates <- 
    predictor_candidates[-which(predictor_candidates == "lifelos")]

# we also add our lost weight variable back in...
homeless_data$weight <- anpd$weight

# at this point we have a well-formatted data-set with only complete cases
# and with only the variables we want to work with - we save it and
# head towards analysis!
save(homeless_data, predictor_candidates,
     file = processed_Rda_path)

###############################################################################
## Building Models (Homelessness)

# splitting data for cross-validation
# the dataset is split roughly in half with the sampling being stratified by
# homeless outcomes (roughly equal proportion of each outcome in each
# half) but with the individuals being selected randomly from the strata
strat_sampler <- function(df, group_column, weight_column = NA, 
                          test_size = 0.5) {
    # get the appropriate group proportions
    if(is.na(weight_column)) {
        sample_props <- table(df[[group_column]])/sum(table(df[[group_column]]))
    } else {
        sample_props <- wpct(df[[group_column]], df[[weight_column]])
    }
    
    # get whole number amounts to sample based on desired test size
    amounts_to_sample <- round(sample_props * nrow(df) * test_size)
    
    # snag and sort the unique groups to sample for
    unique_outcomes <- sort(unique(df[[group_column]]))
    
    # build a sample index
    total_sample_index <- c()
    for(index in 1:length(unique_outcomes)) {
        current_outcome <- unique_outcomes[[index]]
        df_subset_index <- which(df[[group_column]] == current_outcome)
        current_sample_index <- sample(df_subset_index, 
                                       amounts_to_sample[[index]])
        total_sample_index <- c(total_sample_index,
                                current_sample_index)
    }
    
    # get the test and training dataframes
    te_data <- df[total_sample_index, ]
    tr_data <- df[-total_sample_index, ]
    
    # get the sample proportions for each
    if(is.na(weight_column)) {
        test_props <- table(te_data[[group_column]])/sum(table(te_data[[group_column]]))
    } else {
        test_props <- wpct(te_data[[group_column]], te_data[[weight_column]])
    }
    
    if(is.na(weight_column)) {
        train_props <- table(tr_data[[group_column]])/sum(table(tr_data[[group_column]]))
    } else {
        train_props <- wpct(tr_data[[group_column]], tr_data[[weight_column]])
    }
    
    # compile key pieces to one object
    strat_result <- list(
        te_data = te_data,
        tr_data = tr_data,
        props = list(
            base_props = round(sample_props, 3),
            test_props = round(test_props, 3),
            train_props = round(train_props, 3)
        )
    )
    
    # return the collection
    return(strat_result)
}

s_data <- strat_sampler(homeless_data, "homeless")
s_data_w <- strat_sampler(homeless_data, "homeless", "weight")

tr_data <- s_data$tr_data
tr_data_w <- s_data_w$tr_data

# logical
# - null (outcome proportions)
props <- s_data$props
props_w <- s_data_w$props

# save all progress to this point
save.image("inprogress.Rdata")

# create objects to capture the models
models <- list()
models_w <- list()

# everything
full_add <- paste(predictor_candidates, collapse = " + ")
full_int <- paste0("(", full_add, ")^2")
full_add <- paste("homeless", full_add, sep = " ~ ")
full_int <- paste("homeless", full_int, sep = " ~ ")

# strict additive
models$homeless_add <- multinom(full_add, tr_data)
models_w$homeless_add <- multinom(full_add, tr_data_w, 
                                  weights = tr_data_w$weight)
# up to two way interactions
models$homeless_int <- multinom(full_int, tr_data, 
                                MaxNWts = 10032)
models_w$homeless_int <- multinom(full_int, tr_data_w,
                                  weights = tr_data_w$weight,
                                  MaxNWts = 10032)

# intuitive guess
ig_formula <- paste0("homeless ~ sex + amiakn + asian + blkafram + hisorgin + ",
                     "white + clindis + mr + vishear + othermed + phyabuse + ",
                     "sexabuse + neglect + totalrem")

models$homeless_ig <- multinom(ig_formula, tr_data)
models_w$homeless_ig <- multinom(ig_formula, tr_data_w, 
                                 weights = tr_data_w$weight)

# intuitive stepwise
# use ig_formula as my starting point...
ig_rev1 <- paste0(ig_formula, 
                  " + ageatlatrem + everadpt")
ig_rev2 <- paste0(ig_formula,
                  " + clindis : mr : othermed")
ig_rev3 <- paste0(ig_formula,
                  " + phyabuse : sexabuse")
ig_rev4 <- paste0(ig_formula,
                  " + amiakn : blkafram : hisorgin : white : asian")

models$rev1 <- multinom(ig_rev1, tr_data)
models$rev2 <- multinom(ig_rev2, tr_data)
models$rev3 <- multinom(ig_rev3, tr_data)
models$rev4 <- multinom(ig_rev4, tr_data)

models_w$rev1 <- multinom(ig_rev1, tr_data_w, weights = tr_data_w$weight)
models_w$rev2 <- multinom(ig_rev2, tr_data_w, weights = tr_data_w$weight)
models_w$rev3 <- multinom(ig_rev3, tr_data_w, weights = tr_data_w$weight)
models_w$rev4 <- multinom(ig_rev4, tr_data_w, weights = tr_data_w$weight)

# then do some comparing...
# rev_list <- list(homeless_add, homeless_ig, 
#                  ig_rev1_fit, ig_rev2_fit, ig_rev3_fit, ig_rev4_fit)
# lapply(rev_list, function(x) x$AIC)

# full formula appears to be the best performer

# controlled stepwise (BIC criterion for interaction models)
models$step_add <- MASS::stepAIC(models$homeless_add, k = log(nrow(tr_data)))
models_w$step_add <- MASS::stepAIC(models_w$homeless_add, k = log(nrow(tr_data_w)))

models$step_int <- MASS::stepAIC(models$homeless_int,
                                 k = log(nrow(tr_data)))
models_w$step_int <- MASS::stepAIC(models_w$homeless_int,
                                   k = log(nrow(tr_data_w)))

t1 = data.frame(actual = s_data$te_data$homeless, predicted = predict(models$step_add, newdata = s_data$te_data, type = "class"))

# random forest
models$rf_add <- randomForest(formula(full_add), data = tr_data)
models$rf_cutoff <- randomForest(formula(full_add), data = tr_data, cutoff = as.numeric(prop.table(table(tr_data$homeless))))
sqrt_cutoff = sqrt(as.numeric(prop.table(table(tr_data$homeless))))
sqrt_cutoff = sqrt_cutoff / sum(sqrt_cutoff)
models$rf_cutoff_sqrt <- randomForest(formula(full_add), data = tr_data, cutoff = sqrt_cutoff)
models$rf_cutoff_sqrt <- randomForest(formula(full_add), data = tr_data, cutoff = sqrt_cutoff, ntree = 800)
models_w$rf_add <- randomForest(formula(full_add), data = tr_data_w)

models$rf_int <- randomForest(formula(full_int), data = tr_data)
models_w$rf_int <- randomForest(formula(full_int), data = tr_data_w)

save.image(file = "current_analysis.Rdata")

# Bayesian model average
models$bma_add <- bic.glm(formula(full_add), data = tr_data, 
                          glm.family = "binomial")
models_w$bma_add <- bic.glm(formula(full_add), data = tr_data_w, 
                            glm.family = "binomial",
                            wt = tr_data_w$weight)

models$bma_int <- bic.glm(formula(full_int), data = tr_data, 
                      glm.family = "binomial")
models_w$bma_int <- bic.glm(formula(full_int), data = tr_data_w, 
                      glm.family = "binomial",
                      wt = tr_data_w$weight)

# lasso


# Gaussian processes
#   - http://www.gaussianprocess.org/

# peeking at ROC
preds <- predict(models$step_add, newdata = s_data$te_data)
preds <- prediction(predictions = as.numeric(preds), labels = as.numeric(s_data$te_data$homeless))
perf <- performance(preds, "sens", "spec")
plot(perf)

preds <- predict(models$rf_add, newdata = s_data$te_data)
preds <- prediction(predictions = as.numeric(preds), labels = as.numeric(s_data$te_data$homeless))
perf <- performance(preds, "sens", "spec")
plot(perf)

preds <- predict(models$rf_cutoff, newdata = s_data$te_data)
preds <- prediction(predictions = as.numeric(preds), labels = as.numeric(s_data$te_data$homeless))
perf <- performance(preds, "sens", "spec")
plot(perf)

preds <- predict(models$rf_cutoff_sqrt, newdata = s_data$te_data)
preds <- prediction(predictions = as.numeric(preds), labels = as.numeric(s_data$te_data$homeless))
perf <- performance(preds, "sens", "spec")
plot(perf)