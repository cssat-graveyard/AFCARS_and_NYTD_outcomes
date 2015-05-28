SELECT TOP 1 * FROM ndacan.afcars_adoption_00_13;
SELECT TOP 1 * FROM ndacan.afcars_foster_care_00_13;
SELECT * FROM ndacan.nytd_outcomes_waves_1_2;
SELECT COUNT(wave) FROM ndacan.nytd_outcomes_waves_1_2
WHERE cd_wave = 2;
SELECT TOP 1 * FROM ndacan.nytd_services_2011_2012_2013;

SELECT COUNT(stfips) FROM ndacan.nytd_outcomes_waves_1_2
WHERE stfips = 'Washington';

SELECT * FROM ndacan.NYTD_Outcomes_people_dim;

SELECT 
	o.recnumbr
	,COUNT(o.recnumbr)
FROM ndacan.NYTD_Outcomes_people_dim AS pd
	JOIN ndacan.nytd_outcomes_waves_1_2 AS o
		ON pd.stchid = o.stchid
		AND o.cd_wave = 1
	LEFT JOIN ndacan.afcars_foster_care_00_13 AS fc
		ON o.recnumbr = fc.recnumbr
		AND o.st = fc.st
GROUP BY
	o.recnumbr
HAVING COUNT(o.recnumbr) > 1


SELECT TOP 100 *
FROM 
WHERE recnumbr = '002D3J0A1CVO'
ORDER BY 
	recnumbr

SELECT 
RANK() OVER(PARTITION BY fc.recnumbr, fc.st, npd.stchid ORDER BY fc.datayear DESC) AS r_order
,fc.recnumbr AS recnumbr_fc
,fc.st
,npd.stchid
,fc.datayear
FROM [CA_ODS].[ndacan].[NYTD_Outcomes_people_dim] AS npd
INNER JOIN [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc
ON npd.recnumbr = fc.RecNumbr
AND npd.st = fc.St

/* TARGET DESCRIPTION:
A table with all of the AFCARS foster care columns and all of the NYTD
outcomes columns for clients that responded to the wave 2 NYTD outcomes
survey. Given that clients may have multiple entries in the AFCARS
foster care table, we will rely on just their most recent record
for the time being. */

-- subquery to get the AFCARS foster care records that have
-- matches to the NYTD wave 2 subset; this query also adds
-- a rank for the record datayear so that we can select the most
-- current record for an individual in the full query

SELECT RANK() OVER (
	PARTITION BY fc.recnumbr, fc.st, pd.stchid
	ORDER BY fc.datayear DESC
	) AS datayear_rank,
	pd.stchid,
	fc.recnumbr,
	fc.st,
	fc."state",
	fc.fipscode,
	fc.dob,
	pd.dob,
	pd.dobyr,
	pd.dobmon,
	pd.sex,
	pd.amiakn,
	pd.asian,
	pd.blkafram,
	pd.hawaiipi,
	pd.hisorgin,
	pd.white,
	pd.raceunkn,
	pd.racedcln,
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
	fc.ctk1yr,
	fc.fosfamst,
	fc.fcctk1yr,
	fc.fcctk1yr,
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
	ON pd.recnumbr = fc.recnumbr;

-- subquery to get the relevant features of the nytd outcome
-- dataset (wave 2 results only)
SELECT
	nor.stchid,
	nor.dob,
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
WHERE nor.cd_wave = 2;

-- putting it all together
SELECT *
FROM (
	SELECT RANK() OVER (
	PARTITION BY fc.recnumbr, fc.st, pd.stchid
	ORDER BY fc.datayear DESC
	) AS datayear_rank,
	pd.stchid,
	fc.recnumbr,
	fc.st,
	fc."state",
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
) AS foster
INNER JOIN (SELECT
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
WHERE nor.cd_wave = 2) AS outcomes
ON foster.stchid = outcomes.stchid
	AND foster.pd_dob = outcomes.nor_dob
	AND foster.datayear_rank = 1;

SELECT *
FROM [ndacan].[NYTD_Outcomes_people_dim];