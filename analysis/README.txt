Gary Koplik, Matt Tribby
Summer, 2017
README.txt


This is a text document discussing what we're doing in
each of our R Markdown files (and resulting .html files).


rare_disease_prevalence:
This file does some initial frequency analysis of our partitioned data
	(i.e. looking for the most common rare diseases by ICD-9 and ICD-10
	codes).
We used this document to first demonstrate the issues with our partition based
	on the mapping files we received from the National Library of Medicine 
	(NLM).
In particular, we also drill down into one particular example of codes mapping
	to both rare and non-rare diseases with Maturity Onset Diabetes in
	the Young (MODY) and Type II Diabetes (both ICD-10 code E11.9)
NOTE: this file also contains the code necessary to make the
	frequency tables colored by ICD-10 chapter, but they are either
	commented out and / or made not to save or print when running this code
	(i.e. requires calling print(figure) to see and running the png(...)
	and dev.off() commands to save the figures).
Also, this code has been written such that you can change the number of top
	diseases discussed in the document simply by changing the "n <- 20"
	near the beginning of the document to your top number of choice.


replicating_paper:
This file attempts to replicate the results from the paper co-authored by
	Rachel Richesson which can be found at:
	../Coverage of Rare Diseases in SNOMED CT.pdf

summary_of_data_so_far:
This is an aggregation of all of our frequency analysis plus some descriptive
	statistics.
Describing this document in the context of other documents from which we
	are aggregating, the structure of this document is:
	* basic data description
	* descriptive statistics
	* rare_disease_prevalence
	* tables_most_common_rare_diseases
	* tables_metabolic_most_common_rare_diseases


tables_metabolic_most_common_rare_diseases:
This file shows the full frequency tables of only metabolic rare diseases as
	defined in ICD-10 (E70 - E88).
This is all done the same way as done in rare_disease prevalence, but here we
	exclude any graphs and show the entire tables rather than the top 20


tables_most_common_rare_diseases:
This file is the same as tables_metabolic_most_common_rare_diseases only it is
	for all diseases in ICD-10

