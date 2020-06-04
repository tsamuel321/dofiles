
clear all
set more off


// This global picks up your UPI.
global UPI=c(username) 
// The lines below pick up your path to the folder in OneDrive.
// Samuel has a slightly different path since he created the OneDrive folder, hence the if statement
if "$UPI"=="WB537472" {
global dir "C:\Users\wb537472\OneDrive - WBG\Documents\Samuel_ETC_Research\paper"
}
else {
global dir "C:\Users\\${UPI}\OneDrive - WBG\Documents\Samuel_ETC_Research\paper"

}


/////Data preparation

*Find what surveys to query
use `"${dir}\data/surveys_query.dta"', clear

drop year
duplicates drop

gen coveragetype = "National"


merge 1:1 countrycode coveragetype using `"${dir}\data/ppp.dta"'

	foreach var of varlist ppp1993 ppp2005 ppp2011_original ppp2011_revised ppp2017{
	replace `var' = . if `var'==-1
	}


rename ppp2011_original ppp2011


tab countryname  if ppp2011_revised==.&ppp2011!=.  //which countries have missing revised 2011 PPPs?
replace ppp2011_revised = ppp2011 if ppp2011_revised==.  //Assign old PPPs to countries with missing PPPs.


*Determine the poverty line that is equivalent to the change in the distribution associated with the PPP revisions.
gen pl2011_new = 5.5*ppp2011_revised/ppp2011

gen pl2011_new_ = 5.7*ppp2011_revised/ppp2011


save `"${dir}\data/survey_query_ppp.dta"', replace


////////Get poverty headcounts with revised 2011 PPPs at $5.5 line

*Load survey query file with PPP data
use `"${dir}\data/survey_query_ppp.dta"', clear
  

*keep if inlist(countrycode,"CHN")

*save `"${dir}\data/survey_query_ppp_missing_new.dta"', replace


count

forvalues row=1/`r(N)' {

	// Finds what surveys to query
	use `"${dir}\data/survey_query_ppp.dta"', clear

	loc ccc = countrycode[`row']
	loc pl = pl2011_new[`row']
	// Loads the data

	
	if "`ccc'"=="ARG" 		local coverage="urban"
	else					local coverage="national"
	
	
	capture povcalnet, country(`ccc') year(all) coverage(`coverage') povline(`pl') fillgaps clear

	save `"${dir}\data/poverty_`ccc'_2011_new.dta"', replace
	}




*Combine (append) poverty data from all countries
use `"${dir}\data/poverty_AGO_2011_new.dta"', clear 

#delimit;
local ccc "ALB ARG ARM AUS AUT AZE BDI BEL BEN BFA BGD BGR BIH BLR BLZ BOL BRA BTN BWA CAF CAN CHE CHL CHN CIV CMR COD COG COL COM CPV CRI CYP CZE DEU DJI DNK DOM DZA ECU EGY ESP EST ETH FIN FJI FRA GAB GBR GHA GIN
GMB GNB GRC GTM HND HRV HTI HUN IDN IND IRL IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KOR LAO LBR LCA LKA LSO LTU LUX LVA MAR MDA MDG MDV MEX MKD MLI MLT MMR MNE MNG MOZ MRT MUS MWI MYS NAM NER NGA NIC NLD
NOR NPL PAK PAN PER PHL POL PRT PRY PSE ROU RUS RWA SDN SEN SLE SLV SRB STP SUR SVK SVN SWE SWZ SYC TCD TGO THA TJK TTO TUN TUR TZA UGA URY USA VEN VNM YEM ZAF ZMB ZWE FSM GEO GUY IRN KIR LBN PNG SLB SSD SYR TKM TLS TON TUV UKR UZB VUT WSM XKX LBY OMN ARE TWN
";

#delimit cr;  

 foreach x of local ccc {
	append using `"${dir}\data/poverty_`x'_2011_new.dta"', force
	
	}
	
rename headcount headcount_2011_new
label var headcount_2011_new "Poverty rate (with new 2011 PPPs) at $5.5 line"

duplicates drop
drop if year==.

drop if headcount_2011_new==.
	
save `"${dir}\data/poverty_2011_new_all.dta"', replace




////////Get poverty headcounts with revised 2011 PPPs

*Load survey query file with PPP data
use `"${dir}\data/survey_query_ppp.dta"', clear


*keep if inlist(countrycode,"BWA","COD","GEO")

*save `"${dir}\data/survey_query_ppp_missing_new.dta"', replace


count

forvalues row=1/`r(N)' {

	// Finds what surveys to query
	use `"${dir}\data/survey_query_ppp.dta"', clear

	loc ccc = countrycode[`row']
	loc pl = pl2011_new_[`row']
	// Loads the data

	
	if "`ccc'"=="ARG" 		local coverage="urban"
	else					local coverage="national"
	
	
	capture povcalnet, country(`ccc') year(all) coverage(`coverage') povline(`pl') fillgaps clear

	save `"${dir}\data/poverty_`ccc'_2011_new_.dta"', replace
	}




*Combine (append) poverty data from all countries
use `"${dir}\data/poverty_AGO_2011_new_.dta"', clear 

#delimit;
local ccc "ALB ARG ARM AUS AUT AZE BDI BEL BEN BFA BGD BGR BIH BLR BLZ BOL BRA BTN BWA CAF CAN CHE CHL CHN CIV CMR COD COG COL COM CPV CRI CYP CZE DEU DJI DNK DOM DZA ECU EGY ESP EST ETH FIN FJI FRA GAB GBR GHA GIN
GMB GNB GRC GTM HND HRV HTI HUN IDN IND IRL IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KOR LAO LBR LCA LKA LSO LTU LUX LVA MAR MDA MDG MDV MEX MKD MLI MLT MMR MNE MNG MOZ MRT MUS MWI MYS NAM NER NGA NIC NLD
NOR NPL PAK PAN PER PHL POL PRT PRY PSE ROU RUS RWA SDN SEN SLE SLV SRB STP SUR SVK SVN SWE SWZ SYC TCD TGO THA TJK TTO TUN TUR TZA UGA URY USA VEN VNM YEM ZAF ZMB ZWE FSM GEO GUY IRN KIR LBN PNG SLB SSD SYR TKM TLS TON TUV UKR UZB VUT WSM XKX LBY OMN ARE TWN
";

#delimit cr;  

 foreach x of local ccc {
	append using `"${dir}\data/poverty_`x'_2011_new_.dta"', force
	
	}
	
rename headcount headcount_2011_new_
label var headcount_2011_new_ "Poverty rate (with new 2011 PPPs) at $5.7 line"

duplicates drop
drop if year==.
drop if headcount_2011_new_==.
	
save `"${dir}\data/poverty_2011_new_all_.dta"', replace







////////Get poverty headcounts with old 2011 PPPs

*Load survey query file with PPP data
use `"${dir}\data/survey_query_ppp.dta"', clear


*keep if inlist(countrycode,"BWA","COD","GEO")

*save `"${dir}\data/survey_query_ppp_missing.dta"', replace



count

forvalues row=1/`r(N)' {

	// Finds what surveys to query
	use `"${dir}\data/survey_query_ppp.dta"', clear

	loc ccc = countrycode[`row']
	*loc pl = pl2011_new[`row']
	// Loads the data

	
	
	if "`ccc'"=="ARG" 		local coverage="urban"
	else					local coverage="national"
	
	
	
	capture povcalnet, country(`ccc') year(all) povline(5.5) coverage(`coverage') fillgaps clear   

	save `"${dir}\data/poverty_`ccc'_2011.dta"', replace
}


*Combine (append) poverty data from all countries
use `"${dir}\data/poverty_AGO_2011.dta"', clear 

#delimit;
local ccc "ALB ARG ARM AUS AUT AZE BDI BEL BEN BFA BGD BGR BIH BLR BLZ BOL BRA BTN BWA CAF CAN CHE CHL CHN CIV CMR COD COG COL COM CPV CRI CYP CZE DEU DJI DNK DOM DZA ECU EGY ESP EST ETH FIN FJI FRA GAB GBR GHA GIN
GMB GNB GRC GTM HND HRV HTI HUN IDN IND IRL IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KOR LAO LBR LCA LKA LSO LTU LUX LVA MAR MDA MDG MDV MEX MKD MLI MLT MMR MNE MNG MOZ MRT MUS MWI MYS NAM NER NGA NIC NLD
NOR NPL PAK PAN PER PHL POL PRT PRY PSE ROU RUS RWA SDN SEN SLE SLV SRB STP SUR SVK SVN SWE SWZ SYC TCD TGO THA TJK TTO TUN TUR TZA UGA URY USA VEN VNM YEM ZAF ZMB ZWE FSM GEO GUY IRN KIR LBN PNG SLB SSD SYR TKM TLS TON TUV UKR UZB VUT WSM XKX LBY OMN TWN ARE
";

#delimit cr;  

 foreach x of local ccc {
	append using `"${dir}\data/poverty_`x'_2011.dta"', force
	}
	
rename headcount headcount_2011

label var headcount_2011 "Poverty rate (with old 2011 PPPs)"

duplicates drop
drop if headcount_2011==.
save `"${dir}\data/poverty_2011_all.dta"', replace




//////Now poverty analysis/////////

*Combine (merge) poverty headcount data (old 2011 PPP) with poverty headcount data (new 2011 PPP)
use `"${dir}\data/poverty_2011_all.dta"', clear

merge 1:1 countrycode year using `"${dir}\data/poverty_2011_new_all.dta"', keep(3) nogen

merge 1:1 countrycode year using `"${dir}\data/poverty_2011_new_all_.dta"', keep(3) nogen


*coverage of China, India, Indonesia
replace coveragetype = 3 if coveragetype==4  //changed from "National (Aggregate)" to "National", in order to coincide with other countries in the data set and facilitate poverty calculations.


*Set Argentina's urban population to national, since headcount is for only urban Argentina...in oder to coincide with other countries in the data set and facilitate poverty calculations.
replace coveragetype = 3 if countrycode=="ARG"

generate str coverage = "."

replace coverage = "National" if coveragetype ==3

keep if coveragetype==3 

drop regioncode



preserve 

*Scatter plot (slide #4)
	keep if year==2015

	gen diff = headcount_2011_new - headcount_2011

	replace countryname="" if  diff> -.012 & diff < .018

	gen pos = .
	
	replace pos = 3 if diff<0
	replace pos = 9 if diff>0

	replace pos = 11 if countryname=="Nigeria" 
	replace pos = 9 if countryname=="Liberia" 
	replace pos = 4 if countryname=="Armenia"
	replace pos = 4 if countryname=="Fiji"
	replace pos = 4 if countryname=="Indonesia"
	replace pos = 2 if countryname=="Kyrgyz Republic" 
	replace pos = 2 if countryname=="Maldives" 
	replace pos = 4 if countryname=="Kazakhstan" 
	replace pos = 4 if countryname=="West Bank and Gaza" 
	replace pos = 12 if countryname=="Argentina" 
	
	*replace countryname = "" if countryname=="Albania"
	
	replace headcount_2011_new = 100*headcount_2011_new
	replace headcount_2011 = 100*headcount_2011

	scatter headcount_2011_new headcount_2011, mlabel(countryname) ytitle("Poverty rate w/ revised 2011 PPPs (%)") xtitle("Poverty rate w/ original 2011 PPPs (%)") msymbol(D)  ///
	mcolor(black) mlabcolor(black) mlabvposition(pos) legend(off) graphregion(color(white))    ///
		|| function y = x, ra(headcount_2011) clpat	(dash) lcolor(black) lcolor(black)

restore

merge m:1 countrycode using `"${dir}\data/economies.dta"', keep(3) nogen


//////Regional and global poverty counts///////

*
//////Regional and global poverty counts///////

*Now include the population of countries which are not in povcalnet and prepare the data for poverty calculations

*Load population data
preserve
	*Load population data from povcalnet master file
	pcn master, load(pop)
	
	rename coveragetype coverage

	duplicates drop
	save `"${dir}\data/pop.dta"', replace
	
	*Argentina's national population
	keep if countrycode=="ARG"
	keep if coverage=="National"                     
	keep countrycode pop year
	sort year
	*rename pop population
	save `"${dir}\data/argentina_national_pop.dta"', replace
	
restore

merge 1:m countrycode year coverage using `"${dir}\data/pop.dta"'

preserve
*Get a file for countries not available in povcalnet

	keep if _merge==2                         //these are countries not available in povcalnet
	keep countryname countrycode coverage pop year
	save `"${dir}\data/pop_povcalnet_na.dta"', replace
restore

keep if _merge==3
drop _merge


keep countrycode countryname headcount_2011 headcount_2011_new headcount_2011_new_ population coverage year

append using `"${dir}\data/pop_povcalnet_na.dta"'   //Add the population of countries not included in povcalnet

keep if coverage=="National"

keep if year>1990
rename coverage coveragetype
rename population pop

*Get country-level poverty numbers  (see slide 7), sort by number of poor
preserve
	keep if year==2015
	drop year
	merge 1:1 countrycode coveragetype  using  `"${dir}\data/ppp.dta"'
	
	
		foreach var of varlist ppp2011_original ppp2011_revised ppp2017{
		replace `var' = . if `var'==-1
		}


	rename ppp2011_original ppp2011


	tab countryname  if ppp2011_revised==.&ppp2011!=.  //which countries have missing revised 2011 PPPs?
	replace ppp2011_revised = ppp2011 if ppp2011_revised==.  //Assign old PPPs to countries with missing PPPs.

	gen pov_diff = headcount_2011_new - headcount_2011
	gen poor_diff = pov_diff*pop
	gsort -poor_diff
	replace headcount_2011_new = 100*headcount_2011_new
	replace headcount_2011 = 100*headcount_2011
	keep countryname ppp2011 ppp2011_revised headcount_2011 headcount_2011_new poor_diff
	order countryname headcount_2011 headcount_2011_new ppp2011 ppp2011_revised poor_diff
	format headcount_2011 headcount_2011_new ppp2011 ppp2011_revised poor_diff %12.2f
	export excel using `"${dir}\results\Country_poverty_numbers_5_50.xlsx"', sheet("poor") firstrow(var) replace       
restore

*Get country-level poverty numbers  (see slide 8), sort by poverty rate
preserve
	keep if year==2015
	drop year
	merge 1:1 countrycode coveragetype using  `"${dir}\data/ppp.dta"'
	
	foreach var of varlist ppp2011_original ppp2011_revised ppp2017{
		replace `var' = . if `var'==-1
		}


	rename ppp2011_original ppp2011


	tab countryname  if ppp2011_revised==.&ppp2011!=.  //which countries have missing revised 2011 PPPs?
	replace ppp2011_revised = ppp2011 if ppp2011_revised==.  //Assign old PPPs to countries with missing PPPs.
	

	gen pov_diff = headcount_2011_new - headcount_2011
	gen poor_diff = pov_diff*pop
	gsort -pov_diff
	replace pov_diff = 100*pov_diff
	replace headcount_2011_new = 100*headcount_2011_new
	replace headcount_2011 = 100*headcount_2011
	keep countryname ppp2011 ppp2011_revised headcount_2011 headcount_2011_new pov_diff
	order countryname headcount_2011 headcount_2011_new ppp2011 ppp2011_revised pov_diff
	format headcount_2011 headcount_2011_new ppp2011 ppp2011_revised pov_diff %12.2f
	export excel using `"${dir}\results\Country_poverty_numbers_5_50.xlsx"', sheet("povrate") firstrow(var)
restore

merge m:1 countrycode using `"${dir}\data/economies"', keep(3) nogen

duplicates drop

keep if coverage=="National"


*Regional aggregation  (w/ old 2011 PPPs)
bysort region year: egen headcount_regavg_2011 = wtmean(headcount_2011), weight(pop)
replace headcount_2011 = headcount_regavg_2011 if missing(headcount_2011)     //Assign to countries without povcalnet poverty numbers the poverty rate of their region

*Regional aggregation  (w/ new 2011 PPPs) at $5.5
bysort region year: egen headcount_regavg_2011_new = wtmean(headcount_2011_new), weight(pop)
replace headcount_2011_new = headcount_regavg_2011_new if missing(headcount_2011_new)     //Assign to countries without povcalnet poverty numbers the poverty rate of their region

*Regional aggregation  (w/ new 2011 PPPs) at $5.7
bysort region year: egen headcount_regavg_2011_new_ = wtmean(headcount_2011_new_), weight(pop)
replace headcount_2011_new = headcount_regavg_2011_new_ if missing(headcount_2011_new_)     //Assign to countries without povcalnet poverty numbers the poverty rate of their region



*Regional change in poverty (see slide #4)

bysort region year: egen region_pop = total(pop) if year==2015   //regional population in 2015


*Rename regions for the purposes of presentation slides
replace region= "East Asia and Pacific" if region=="EAP"
replace region= "Europe and Central Asia" if region=="ECA"
replace region= "Other High Income" if region=="OHI"
replace region= "Latin America and the Caribbean" if region=="LAC"
replace region= "Middle East and North Africa" if region=="MNA"
replace region= "South Asia" if region=="SAS"
replace region= "Sub-Saharan Africa" if region=="SSA"

preserve

	replace headcount_regavg_2011 = 100*headcount_regavg_2011
	replace headcount_regavg_2011_new = 100*headcount_regavg_2011_new

	collapse headcount_regavg_2011 headcount_regavg_2011_new region_pop if year==2015, by(region)
	
	gen pov_abs_diff = headcount_regavg_2011_new - headcount_regavg_2011
	gen pov_rel_diff = 100*(headcount_regavg_2011_new - headcount_regavg_2011)/headcount_regavg_2011
	gen poor_reg_2011 = region_pop*headcount_regavg_2011/100
	gen poor_reg_2011_new = region_pop*headcount_regavg_2011_new/100
	gen poor_reg_diff = region_pop*pov_abs_diff/100
	keep region headcount_regavg_2011 headcount_regavg_2011_new pov_abs_diff poor_reg_2011 poor_reg_2011_new poor_reg_diff
	order region headcount_regavg_2011 headcount_regavg_2011_new pov_abs_diff poor_reg_2011 poor_reg_2011_new poor_reg_diff
	format headcount_regavg_2011 headcount_regavg_2011_new pov_abs_diff poor_reg_2011 poor_reg_2011_new poor_reg_diff %12.2f
	
	lab var headcount_regavg_2011 "Poverty rate, % (old)"
	lab var headcount_regavg_2011_new "Poverty rate, % (new)"
	lab var pov_abs_diff "Change in poverty rate, pp"
	lab var poor_reg_2011 "Millions of poor (old)"
	lab var poor_reg_2011_new "Millions of poor (new)"
	lab var poor_reg_diff "Change in millions of poor"
	
	export excel using `"${dir}\results\Global_poverty_5_50.xlsx"', sheet("regions") firstrow(varl) replace
restore

merge m:1 countrycode year using `"${dir}\data/argentina_national_pop.dta"'    //deal with Argentina's population
keep if _merge==1 | _merge==3
replace pop = population if countrycode=="ARG"


*Global aggregation (w/ old & new 2011 PPPs)
preserve
	collapse headcount_2011 headcount_2011_new pop if year==2015, by(countryname year)
	bysort year: egen headcount_gloavg_2011 = wtmean(headcount_2011), weight(pop)
	bysort year: egen headcount_gloavg_2011_new = wtmean(headcount_2011_new), weight(pop)
	egen global_pop = total(pop)
	replace headcount_gloavg_2011 = 100*headcount_gloavg_2011
	replace headcount_gloavg_2011_new = 100*headcount_gloavg_2011_new
	replace headcount_2011 = 100*headcount_2011
	replace headcount_2011_new = 100*headcount_2011_new
	format headcount_gloavg_2011 headcount_gloavg_2011_new headcount_2011 headcount_2011_new %12.2f 
	export excel using `"${dir}\results\Global_poverty_5_50.xlsx"', sheet("countries", modify) firstrow(var) 
	
	
	
	collapse headcount_gloavg_2011 headcount_gloavg_2011_new global_pop,by(year)
	gen pov_abs_diff_glo = headcount_gloavg_2011_new - headcount_gloavg_2011
	gen poor_glo_2011 = (headcount_gloavg_2011*global_pop)/100
	gen poor_glo_2011_new = (headcount_gloavg_2011_new*global_pop)/100
	gen poor_glo_diff = poor_glo_2011_new - poor_glo_2011 
	
	gen countryname="World"
	
	keep countryname headcount_gloavg_2011 headcount_gloavg_2011_new pov_abs_diff_glo poor_glo_2011 poor_glo_2011_new poor_glo_diff
	order countryname headcount_gloavg_2011 headcount_gloavg_2011_new pov_abs_diff_glo poor_glo_2011 poor_glo_2011_new poor_glo_diff
	format headcount_gloavg_2011 headcount_gloavg_2011_new pov_abs_diff_glo poor_glo_2011 poor_glo_2011_new poor_glo_diff %12.2f
	
	lab var headcount_gloavg_2011 "Poverty rate, % (old)"
	lab var headcount_gloavg_2011_new "Poverty rate, % (new)"
	lab var pov_abs_diff_glo "Change in poverty rate, pp"
	lab var poor_glo_2011 "Millions of poor (old)"
	lab var poor_glo_2011_new "Millions of poor (new)"
	lab var poor_glo_diff "Change in millions of poor"
	
	
	export excel using `"${dir}\results\Global_poverty_5_50.xlsx"', sheet("world", modify) firstrow(varl)
restore




preserve
*Regional poverty trends (see slide #3)
replace headcount_regavg_2011 = 100*headcount_regavg_2011
replace headcount_regavg_2011_new = 100*headcount_regavg_2011_new
replace headcount_regavg_2011_new_ = 100*headcount_regavg_2011_new_

keep if year>1990

collapse headcount_regavg_2011 headcount_regavg_2011_new headcount_regavg_2011_new_,by(region year) 

tempfile headcount_regions
save `headcount_regions'

*twoway line headcount_regavg_2011 headcount_regavg_2011_new year if region!="Other High Income" &year<=2015,  ///
	ytitle("Poverty rate, %") xtitle("") graphregion(lcolor(white)) ylabel(0(20)80) xlabel(1995(5)2015)     ///
	by(region)   


*twoway line headcount_regavg_2011 headcount_regavg_2011_new year if region=="Middle East and North Africa", ytitle("Poverty rate, %")  ylabel(0(1)10) xlabel(1995(5)2015) by(region) sort   //only for MENA

/*
twoway connected coverage_noNGA year, mlab(coverage_noNGA) mlabpos(6)|| ///
connected coverage year if inlist(region,"World","SSA"), mlab(coverage) mlabpos(12) || ///
line forty year, lcolor(gs6) lpattern(dash) ///
by(region, title("Coverage") ///
graphregion(color(white)) note("") rows(2)) ///
xsize(15) ysize(10) ylab(0(20)100,angle(horinzontal) format(%2.0f)) ///
ytitle("%") legend(order(1 "Baseline" 2 "With Nigeria" 3 "Cut-off value") region(lcolor(white)) rows(1))
graph export "LineUperror\Figures\Coverage_share.png", as(png) replace 
*/
restore

*Global poverty trends (w/ old & new 2011 PPPs)
preserve

collapse headcount_2011 headcount_2011_new headcount_2011_new_ pop, by(countryname year)
bysort year: egen headcount_gloavg_2011 = wtmean(headcount_2011), weight(pop)
bysort year: egen headcount_gloavg_2011_new = wtmean(headcount_2011_new), weight(pop)
bysort year: egen headcount_gloavg_2011_new_ = wtmean(headcount_2011_new_), weight(pop)

replace headcount_gloavg_2011 = 100*headcount_gloavg_2011
replace headcount_gloavg_2011_new = 100*headcount_gloavg_2011_new
replace headcount_gloavg_2011_new_ = 100*headcount_gloavg_2011_new_

collapse headcount_gloavg_2011 headcount_gloavg_2011_new headcount_gloavg_2011_new_,by(year) 

rename headcount_gloavg_2011 headcount_regavg_2011
rename headcount_gloavg_2011_new headcount_regavg_2011_new
rename headcount_gloavg_2011_new_ headcount_regavg_2011_new_

gen region = "World"

tempfile headcount_world
save `headcount_world'
	
*twoway line headcount_gloavg_2011 headcount_gloavg_2011_new year if year<=2015 , ytitle("Poverty rate, %")  ylabel(0(10)40) xlabel(1995(5)2015) sort
restore
	

use `headcount_regions', clear
append using `headcount_world'


replace region= "Latin A. & Caribbean" if region=="Latin America and the Caribbean"
replace region= "Middle East & N. Africa" if region=="Middle East and North Africa"

twoway line headcount_regavg_2011 headcount_regavg_2011_new headcount_regavg_2011_new_ year if  year<=2015,  ///
	ytitle("Poverty rate, %") xtitle("") graphregion(color(white))  ylabel(0(20)80) xlabel(1995(10)2015)  ///
	legend(col(1) lab(1 "w/ original 2011 PPPs at $5.5 line") lab(2 "w/ revised 2011 PPPs at $5.5 line") lab(3 "w/ revised 2011 PPPs at $5.7 line")) by(region, row(2) style(compact) note("") iscale(*.9) ) 

