			*** Descriptive Statistics - Specification Annual ***

*Authors: Ricardo Duque Gabriel and Ana Sofia Pessoa 
*Start Date: 13/12/2018
*Last Update: 14/11/2019

/*
The file Descriptives.do produce two tables which contain descriptive statistics
of relevant macroencomic outcomes by country. The first tables relates to the 
Eurozone countries and the second one to the donor countries in our sample. 
The tables display means, standard deviatons, maximums and minimums for each 
countries' variables of interest. 
*/

clear
set more off
graph drop _all
use "${Data}Analysis_Annual", clear



foreach var in $covariates{
	replace `var' = `var'*100
}

//Table for monetary union countries
estpost tabstat $covariates if mu==1, by (country)  s(mean, sd, max, min) columns(statistics) listwise
esttab using "${Tab}Descriptives_mu_Annual.tex", replace collabels("Mean" "St. Dev." "Max" "Min")  cells("mean(fmt(2)) sd(fmt(2)) max(fmt(2)) min(fmt(2))") nogaps label noeqlines longtable nonumbers title("Descriptive Statiscs per country in the Monetary Union")

//Table for non-eurozone countries
estpost tabstat $covariates if donor==1, by (country) s(mean, sd, max, min) columns(statistics) listwise 
esttab using "${Tab}Descriptives_donor_Annual.tex", replace nomtitles  collabels("Mean" "St. Dev." "Max" "Min")    nonumbers cells("mean(fmt(2)) sd(fmt(2)) max(fmt(2)) min(fmt(2))") nogaps label noeqlines longtable title("Descriptive Statiscs per country from the Donor Pool")
