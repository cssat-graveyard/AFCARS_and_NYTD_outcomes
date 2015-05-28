SELECT COUNT(*) AS num_records,
	   COUNT(DISTINCT fc.recnumbr) AS num_cases	   
FROM [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc;

SELECT AVG(fc.record_counts) AS mean_records_per_case,
	   MIN(fc.record_counts) AS min_records_per_case,
	   MAX(fc.record_counts) AS max_records_per_case
FROM (SELECT COUNT(recnumbr) AS record_counts
	  FROM [CA_ODS].[ndacan].[afcars_foster_care_00_13]
	  GROUP BY recnumbr, st) AS fc;

SELECT SUM(CASE WHEN foster.sex = 'Male' THEN 1 ELSE 0 END) AS male_cases,
	   SUM(CASE WHEN foster.sex = 'Female' THEN 1 ELSE 0 END) AS female_cases,
	   AVG(foster.totalrem) AS average_num_removals,
	   SUM(CASE WHEN foster.everadpt = 'Yes, child has been legally adopted' THEN 1 ELSE 0 END) AS adopted_count,
	   SUM(CASE WHEN foster.agedout = 'Yes' THEN 1 ELSE 0 END) AS aged_out_count
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
WHERE foster.datayear_rank = 1;

SELECT *
FROM [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc;

SELECT COUNT(*) AS num_records,
	   COUNT(DISTINCT ow.recnumbr) AS num_cases,
	   SUM(CASE WHEN ow.cd_wave = 2 THEN 1 ELSE 0 END) AS num_wave2_candidates,
	   SUM(CASE WHEN ow.cd_wave = 2 
					AND ow.responded = 'Responded to Survey'
	       THEN 1 ELSE 0 END) AS num_wave2_respondents
FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow;

SELECT 
	COUNT(*) AS total_records,
	COUNT(DISTINCT ow.recnumbr) AS unique_cases,
	SUM(CASE WHEN ow.sex = 'Male' THEN 1 ELSE 0 END) AS males,
	SUM(CASE WHEN ow.sex = 'Female' THEN 1 ELSE 0 END) AS females
FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow
WHERE ow.cd_wave = 2 AND ow.responded = 'Responded to Survey';

SELECT *
FROM [CA_ODS].[ndacan].[nytd_outcomes_waves_1_2] AS ow;