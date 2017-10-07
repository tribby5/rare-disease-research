Matt Tribby, Gary Koplik
Summer, 2017
SCRIPT README

What follows is the list of scripts in the script folder in alphabetical order and a short description of their use:

afib_PaTH_prevalance.R: Atrial fibrillation subsetting according to criteria in
	../path_afib_inclusion_criteria.docx

findSnomedChildren.R: contains a function which, when given a SNOMED CT concept ID and a number of levels, will
	return the SNOMED CT hierarchial children (is a relationships)

ipf_PaTH.R: Script to replicate prevalence numbers from paper with our data: Dimmock et al (2015) - Evaluation of a
	Computable Phenotype for Identification of Patients with Idiopathic Pulmonary Fibrosis
	../PaTH_Evaluation of a Computable Phenotype for Identification of Patients with IPF.pdf

loadMappingFiles.R: A script that will load into memory the mapping files, that are the foundation of this project
	from the National Library of Medicine. This includes ICD10 mapping to rare diseases, ICD9 mapping to rare 
	diseases, SNOMED CT mapping to rare diseases, and the ORDR list of rare diseases

make_1_to_1.R: Reads in the full icd 9 and 10 mapping files and creates subset files of only the codes that map
	to exactly 1 rare disease:
	../data/icd9_1to1.Rdata
	../data/icd10_1to1.Rdata

mappingToSnomed.R: A script which attempts to make an approximate mapping of ICD to SNOMED CT (data agnostic). The
	script looks into direct matches (codes that match completely) as well as does a technique to approximate a 
	SNOMED match when a rare disease maps to multiple SNOMED CT codes.

num_visits_rare_v_nonrare.R: Script for comparing rare and non-rare cohorts with respect to number of visits.
	Note currently, this document is using an unreliable visits variable, but reliable variable coming...
	This document makes and saves (the 2 is from using new visits column):
	../figures/visits_rare_v_nonrare2.png
	../figures/visits_vs_age_rare_v_nonrare2.png

parse_icd10.R: Script to create a .Rdata file that has a column of ICD-10 codes and the corresponding ICD-10
	chapter and block.
	Parses XML file:
	../data/icd10cm_tabular_2017.xml
	to create:
	../data/icd10_blocks_chapters.Rdata
	Note: in practice ended up needing to use the hardcoded chapter identification dplyr code
	written in this file on the data set directly rather than being able to left join
	icd_blocks_chapters to the patient data. Guessing there are some codes not covered in the XML document...

patientCodePairs.R: The goal of this script is to examine on a patient specific basis, the breakdown of rare codes.
	This could be important for metrics of dividing up the dataset (for example only looking at patients who
	have had at least 10 occurences of a code versus 1) 

patientDataPrep.R: This the initial script used to process our raw, given data into a more usable form. We were
	given data (diagnoses.csv) and this script divides it up into either a rare or non-rare subset and also
	transforms the data into a more tidy and easily understood form.

rare_v_nonrare.R: Script to make multiple figures:
	../figures/chapter_rare_v_nonrare.png
	../figures/age_rare_v_nonrare.png
	../figures/numcode_vs_age_rare_v_nonrare.png

removeICDByFile.R: If you were to add icd codes into either ICD9CodesToRemove.txt or ICD10CodesToRemove.txt in the
	data folder and run this script, then the subset that is suppleid to the script will remove all instances
	where the ICD codes exist. This would be helpful when trying to better refine subsets due to the lack of
	specificity of ICD

uniquenessOfCodes.R: This script looks into the frequency of ICD codes in the data. First, without using patient 
	data it looks at the frequency of codes in the mapping files and then with the data, it looks into their
	frequency in the actual data

visualizeMappingsByStructure.R: This script will first combine the ICD10 to SNOMED CT mappings with ICD10 hierarchial
	concepts (chapters and blocks) for analysis of different parts of the hierarchy. Statistics are made for
	each chapter. Then using ggplot, several plots leveraging these statistics are made.

visualizeSnomedGroup.R: This script utilizes the visNetwork R package to display groupings of SNOMED concepts. The
	original goal of this was to better understand groupings of rare diseases and therefore maybe understand
	the major categories which affect the health care system according to the data
