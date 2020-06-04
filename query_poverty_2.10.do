

*-------------------------------------------------------------------------------------------------------------------------
*Compute global, regional, country-level extreme poverty estimates: compare poverty at $1.9 in 2011 PPPs to $2.1 2017 PPPs
*-------------------------------------------------------------------------------------------------------------------------

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

*Get CPI data
use `"${dir}\data/CPI_final_yearly_series.dta"', clear

*Syria and Iraq do not have CPI for 2017

*1) Iraq: Estimate CPI with WEO series.

preserve	
	datalibweb, country(Support) year(2005) type(GMDRAW) surveyid(Support_2005_CPI_v04_M) filename(WEO_Yearly_CPI.dta)
	keep if code=="IRQ"

	gen cpi_growth = value[_n]/value[_n-1]
	gen cpi_growth_2017_ = cpi_growth if year==2017
	egen cpi_growth_2017 = mean(cpi_growth_2017_)
	sum cpi_growth_2017
	return list  //Use this cpi growth rate (= 1.000988960266113) to estimate CPI for 2017 from 2016 CPI (=118.3852920532227)
	
	keep if year==2017
	gen cpi2010 = cpi_growth_2017*118.3852920532227
	
	collapse cpi2010 year, by(code)
	rename code countrycode
	
	tempfile cpi_iraq_2017
	save `cpi_iraq_2017', replace
restore 

*2) Syria: Link CPI series from different sources, including datalibweb and two references (see Excel Sheet below for details)

preserve 
	import excel "${dir}\data\Syria CPI series v2.xlsx", sheet("syria_cpi_") firstrow clear 
	
	keep countrycode year linkedcpi2010
	rename linkedcpi2010 cpi2010
	keep if year>2012   //CPI series in datalibweb ends in 2012. 

	tempfile cpi_syria_2013_2017
	save `cpi_syria_2013_2017', replace
restore
	
append using `cpi_iraq_2017'
append using `cpi_syria_2013_2017'
	
sort countrycode year

save `"${dir}\data/CPI_final_yearly_series_updated.dta"', replace

keep if year==2011 | year==2017
rename cpi2010 cpi

reshape wide cpi, i(countrycode) j(year)

gen cpi_2011_2017 = cpi2011/cpi2017

reshape long cpi, i(countrycode) j(year)
keep countrycode year cpi_2011_2017
rename cpi_2011_2017 cpi_

collapse cpi_, by(countrycode)



*Get relevant PPP ratio using 2011 original PPPs and 2017 PPPs
preserve 
	use `"${dir}\data/ppp.dta"', clear
	keep countrycode coveragetype ppp2011_original ppp2017
	keep if coveragetype=="National"
	drop coveragetype
	
	*Clean and organize data
	foreach var of varlist ppp2011_original ppp2017{
	replace `var' = . if `var'==-1
	}

	rename ppp2011_original ppp2011
	gen ppp_ = ppp2017/ppp2011
	
	keep countrycode ppp_
	
	tempfile ppp_2011orig_2017
	save `ppp_2011orig_2017', replace
restore
	
merge 1:1 countrycode using `ppp_2011orig_2017'

drop _merge

tempfile cpi_ppp_ratios
save `cpi_ppp_ratios', replace


*Get separate 2017 urban, rural PPPs for China, India, and Indonesia 
import excel "${dir}\data\IND_IDN_CHN\urban_rural_ppp_summary_calculations.xlsx", sheet("calculations") firstrow clear 

	keep Country China2017 India2017 Indon2017
	rename Country coverage

	keep if coverage=="Rural PPP" | coverage=="Urban PPP" | coverage=="National PPP" 

	replace coverage="Rural" if coverage=="Rural PPP"
	replace coverage="Urban" if coverage=="Urban PPP"
	replace coverage="National" if coverage=="National PPP"

	rename China2017 China1
	rename India2017 India2
	rename Indon2017 Indonesia3

	reshape long China India Indonesia, i(coverage) j(country)
	egen ppp2017 = rowmean(China India Indonesia)

	gen countryname = "China" if country==1
	replace countryname = "India" if country==2
	replace countryname = "Indonesia" if country==3

	gen countrycode = "CHN" if countryname=="China"
	replace countrycode = "IND" if countryname=="India"
	replace countrycode = "IDN" if countryname=="Indonesia"

	drop China India Indonesia country
	sort countryname coverage
	order countryname countrycode coverage ppp2017
	
	
	*Get original 2011 PPPs from povcalnet master data
	preserve
		pcn master, load(ppp)
		keep if inlist(countrycode, "CHN", "IND", "IDN")
		keep countrycode countryname coveragetype ppp2011
		rename coveragetype coverage
		
		tempfile urb_rur_ppp2011
		save `urb_rur_ppp2011', replace
	restore
	
	merge 1:1 countrycode countryname coverage using `urb_rur_ppp2011', keep(3) nogen

	save `"${dir}\data/IND_IDN_CHN/urb_rur_ppp.dta"', replace



*Get separate urban, rural CPI series for China
import excel "${dir}\data\IND_IDN_CHN\urban_rural_ppp_summary_calculations.xlsx", sheet("china sources") firstrow clear 

	keep year lreplicpirur lreplicpiurb
	rename lreplicpirur cpi_rur
	rename lreplicpiurb cpi_urb
	keep if year==2011 | year==2017

	rename cpi_rur cpi_rur1
	rename cpi_urb cpi_urb2

	gen id = _n
	reshape long cpi_rur cpi_urb, i(id) j(coveragetype)
	egen cpi = rowmean(cpi_rur cpi_urb)

	gen coverage = "Rural" if coveragetype==1
	replace coverage = "Urban" if coveragetype==2

	drop cpi_rur cpi_urb id coveragetype

	reshape wide cpi, i(coverage) j(year)

	*Urban-rural price ratio is 1.285 in 2011
	replace cpi2011 = cpi2011 if coverage=="Rural"
	replace cpi2011 = 1.285*cpi2011 if coverage=="Urban"

	replace cpi2017 = cpi2011*cpi2017 if coverage=="Rural" 
	replace cpi2017 = cpi2011*cpi2017 if coverage=="Urban"

	gen countryname = "China"
	gen countrycode = "CHN"

	tempfile cpi_china
	save `cpi_china', replace

 

*Get separate urban, rural CPI series for India
import excel "${dir}\data\IND_IDN_CHN\urban_rural_ppp_summary_calculations.xlsx", sheet("india sources") firstrow clear 

	keep if Year=="2011" | Year=="2017"
	keep Year AVG_urb AVG_rur
	gen year = 2011 if Year=="2011"
	replace year = 2017 if Year=="2017"

	rename AVG_urb cpi_urb
	rename AVG_rur cpi_rur
	drop Year 

	rename cpi_rur cpi_rur1
	rename cpi_urb cpi_urb2

	gen id = _n
	reshape long cpi_rur cpi_urb, i(id) j(coveragetype)

	egen cpi = rowmean(cpi_rur cpi_urb)

	gen coverage = "Rural" if coveragetype==1
	replace coverage = "Urban" if coveragetype==2
	drop cpi_rur cpi_urb id coveragetype

	reshape wide cpi, i(coverage) j(year)

	gen countryname = "India"
	gen countrycode = "IND"

	tempfile cpi_india
	save `cpi_india', replace


*For Indonesia, CPI series is available only at the national level, so rural and urban CPIs are the same.
pcn master, load(cpi)	
	
	keep if countrycode=="IDN"
	keep if year==2011 | year==2017
	
	replace coveragetype = "Rural" if coveragetype=="rural"
	replace coveragetype = "Urban" if coveragetype=="urban"
	rename coveragetype coverage
	
	keep countrycode countryname coverage cpi year
	
	reshape wide cpi, i(countrycode countryname coverage) j(year)
	
	tempfile cpi_indonesia
	save `cpi_indonesia', replace
	

*Combine CPIs for China, India, and Indonesia.	
use `cpi_china', clear
append using `cpi_india'
append using `cpi_indonesia'
	
save `"${dir}\data/IND_IDN_CHN/urb_rur_cpi.dta"', replace	
	
*Combine both CPIs and PPPs for China, India, and Indonesia.
use `"${dir}\data/IND_IDN_CHN/urb_rur_cpi.dta"', clear
merge 1:1 countrycode countryname coverage using `"${dir}\data/IND_IDN_CHN/urb_rur_ppp.dta"', nogen keep(3)

order countryname countrycode coverage 

gen ppp_ = ppp2017/ppp2011

gen cpi_ = cpi2011/cpi2017
	
	
*Query urban poverty lines and poverty rates for China, India, and Indonesia.
	
preserve
	keep if coverage=="Urban"
	keep countrycode countryname ppp_ cpi_ 
	gen pl = 2.1*cpi_*ppp_
	
	save `"${dir}\data/chi_ind_indo_urb.dta"', replace

	count
	
	forvalues row=1/`r(N)' {

		// Finds what surveys to query
		use `"${dir}\data/chi_ind_indo_urb.dta"', clear
		
		loc ccc = countrycode[`row']
		loc pl = pl[`row']
		
		capture povcalnet, country(`ccc') year(all) coverage(urban) povline(`pl') fillgaps clear
		save `"${dir}\data/poverty_query/`ccc'_urb.dta"', replace
		}

restore	

*Query rural poverty lines and poverty rates for China, India, and Indonesia.
		keep if coverage=="Rural"
		keep countrycode countryname ppp_ cpi_ 
		gen pl = 2.1*cpi_*ppp_
		
		save `"${dir}\data/chi_ind_indo_rur.dta"', replace

		count
		
		forvalues row=1/`r(N)' {

			// Finds what surveys to query
			use `"${dir}\data/chi_ind_indo_rur.dta"', clear
			
			loc ccc = countrycode[`row']
			loc pl = pl[`row']
			
			capture povcalnet, country(`ccc') year(all) coverage(rural) povline(`pl') fillgaps clear
			save `"${dir}\data/poverty_query/`ccc'_rur.dta"', replace
			
			}
	
	
*Query national poverty lines and poverty rates for the remaining countries, except Argentina.
use `"${dir}\data/surveys_query.dta"', clear

drop year
duplicates drop


merge 1:1 countrycode using `cpi_ppp_ratios', keep(3) nogen
drop if inlist(countrycode,"CHN","IND","IDN","ARG")

gen pl = 2.1*cpi_*ppp_

replace pl = 1.9 if inlist(countrycode,"SYR","TUV","VEN","XKX","YEM")  //These are the countries with survey data that do not have 2017 PPPs.

*keep if inlist(countrycode,"ARE","CYP","DEU","GIN","KIR") | inlist(countrycode,"LAO","LCA","MDG","TWN","UGA")

save `"${dir}\data/survey_query_cpi_ppp_2017.dta"', replace


use `"${dir}\data/survey_query_cpi_ppp_2017.dta"', clear

count

forvalues row=1/`r(N)' {

	// Finds what surveys to query
	use `"${dir}\data/survey_query_cpi_ppp_2017.dta"', clear

	loc ccc = countrycode[`row']
	loc pl = pl[`row']
	
	capture povcalnet, country(`ccc') year(all) coverage(national) povline(`pl') fillgaps clear
	save `"${dir}\data/poverty_query/`ccc'.dta"', replace
	}

	
*Query national poverty lines and poverty rates for Argentina.
use `"${dir}\data/surveys_query.dta"', clear

drop year
duplicates drop

merge 1:1 countrycode using `cpi_ppp_ratios', keep(3) nogen
keep if countrycode=="ARG"

gen pl = 2.1*cpi_*ppp_
save `"${dir}\data/survey_query_cpi_ppp_arg.dta"', replace

count

forvalues row=1/`r(N)' {

	// Finds what surveys to query
	use `"${dir}\data/survey_query_cpi_ppp_arg.dta"', clear

	loc ccc = countrycode[`row']
	loc pl = pl[`row']
	
	capture povcalnet, country(`ccc') year(all) coverage(urban) povline(`pl') fillgaps clear
	save `"${dir}\data/poverty_query/`ccc'.dta"', replace
	}

	
*Combine (append) poverty data from all countries
use `"${dir}\data/poverty_query/AGO.dta"', clear 

*LBY OMN are missing countries in povcalnet

#delimit;
local ccc "ALB ARG ARM AUS AUT AZE BDI BEL BEN BFA BGD BGR BIH BLR BLZ BOL BRA BTN BWA CAF CAN CHE CHL CHN_rur CHN_urb CIV CMR COD COG COL COM CPV CRI CYP CZE DEU DJI DNK DOM DZA ECU EGY ESP EST ETH FIN FJI FRA GAB GBR GHA GIN
GMB GNB GRC GTM HND HRV HTI HUN IDN_rur IDN_urb IND_rur IND_urb IRL IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KOR LAO LBR LCA LKA LSO LTU LUX LVA MAR MDA MDG MDV MEX MKD MLI MLT MMR MNE MNG MOZ MRT MUS MWI MYS NAM NER NGA NIC NLD
NOR NPL PAK PAN PER PHL POL PRT PRY PSE ROU RUS RWA SDN SEN SLE SLV SRB STP SUR SVK SVN SWE SWZ SYC TCD TGO THA TJK TTO TUN TUR TZA UGA URY USA VEN VNM YEM ZAF ZMB ZWE FSM GEO GUY IRN KIR LBN PNG SLB SSD SYR TKM TLS TON TUV UKR UZB VUT WSM XKX
TWN ARE 
";

#delimit cr;  

 foreach x of local ccc {
	append using `"${dir}\data/poverty_query/`x'.dta"', force
	}
	
	drop if headcount==.
save `"${dir}\data/ipl2017_poverty_query.dta"', replace	
	
	
	
	

//////Now poverty analysis/////////
use `"${dir}\data/ipl2017_poverty_query.dta"', clear

*keep if year==2015

*First off, determine the national poverty rates for China, India, and Indonesia.
gen poor_urb_rur = headcount*population if inlist(countrycode, "CHN","IND","IDN")
egen poor_total = sum(poor_urb_rur) if inlist(countrycode, "CHN","IND","IDN"), by(countrycode year)
egen pop_total = sum(population) if inlist(countrycode, "CHN","IND","IDN"), by(countrycode year)
gen headcount_nat = poor_total/pop_total

list countrycode coveragetype povertyline headcount population poor_urb_rur poor_total pop_total headcount_nat if inlist(countrycode,"CHN","IND","IDN")

drop if coveragetype == 1    //Drop all Rural coverage, which holds for only these three countries.
replace coveragetype = 3 if inlist(countrycode, "CHN", "IND", "IDN")  //Set Urban coverage to National for these three countries.
replace headcount = headcount_nat if inlist(countrycode, "CHN", "IND", "IDN")    //replace headcount with national headcount
replace population = pop_total if inlist(countrycode, "CHN", "IND", "IDN")  //replace Urban population with National population


list countrycode coveragetype povertyline headcount population poor_urb_rur poor_total pop_total headcount_nat if inlist(countrycode,"CHN","IND","IDN")
drop poor_urb_rur poor_total pop_total headcount_nat



save `"${dir}\data/ipl2017_poverty_query_new.dta"', replace	







////////Get poverty headcounts with old 2011 PPPs

*Load survey query file with PPP data
use `"${dir}\data/survey_query_ppp.dta"', clear

*keep if countrycode=="ZAF"

*save `"${dir}\data/survey_query_ppp_2017.dta"', replace

count

forvalues row=1/`r(N)' {

	// Finds what surveys to query
	use `"${dir}\data/survey_query_ppp.dta"', clear

	loc ccc = countrycode[`row']
	*loc pl =  pl2017[`row']
	// Loads the data

	capture povcalnet, country(`ccc') year(all) povline(1.9) coverage(national) fillgaps clear   

	save `"${dir}\data/poverty_`ccc'_2011.dta"', replace
}

*Argentina
capture povcalnet, country(ARG) year(all) coverage(urban) povline(1.9) fillgaps clear     //1. Argentina has urban coverage, so I get its data separately.
save `"${dir}\data/poverty_ARG_2011.dta"', replace



*Combine (append) poverty data from all countries
use `"${dir}\data/poverty_AGO_2011.dta"', clear 

#delimit;
local ccc "ALB ARG ARM AUS AUT AZE BDI BEL BEN BFA BGD BGR BIH BLR BLZ BOL BRA BTN BWA CAF CAN CHE CHL CHN CIV CMR COD COG COL COM CPV CRI CYP CZE DEU DJI DNK DOM DZA ECU EGY ESP EST ETH FIN FJI FRA GAB GBR GHA GIN
GMB GNB GRC GTM HND HRV HTI HUN IDN IND IRL IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KOR LAO LBR LCA LKA LSO LTU LUX LVA MAR MDA MDG MDV MEX MKD MLI MLT MMR MNE MNG MOZ MRT MUS MWI MYS NAM NER NGA NIC NLD
NOR NPL PAK PAN PER PHL POL PRT PRY PSE ROU RUS RWA SDN SEN SLE SLV SRB STP SUR SVK SVN SWE SWZ SYC TCD TGO THA TJK TTO TUN TUR TZA UGA URY USA VEN VNM YEM ZAF ZMB ZWE FSM GEO GUY IRN KIR LBN PNG SLB SSD SYR TKM TLS TON TUV UKR UZB VUT WSM XKX 
ARE TWN";

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


merge 1:1 countrycode year using `"${dir}\data/ipl2017_poverty_query_new.dta"', keep(3) nogen
drop if headcount_2011==.    //This is missing headcount for India (2018)

rename headcount headcount_2017

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

	

	gen pos = 3

	replace pos = 7 if countrycode=="COD" 
	replace pos = 6 if countrycode=="PNG"
	replace pos = 12 if countrycode=="SWZ"
	replace pos = 9 if countrycode=="NER"
	replace pos = 9 if countrycode=="KIR"
	replace pos = 12 if countrycode=="GEO"
	replace pos = 9 if countrycode=="STP"
	replace pos = 11 if countrycode=="MDG"
	
	
	replace headcount_2017 = 100*headcount_2017
	replace headcount_2011 = 100*headcount_2011
	
	gen diff = abs(headcount_2017 - headcount_2011)

	replace countrycode="" if  diff< 5 
	

	scatter headcount_2017 headcount_2011, mlabel(countrycode) ytitle("Poverty rate w/ 2017 PPPs (%)") xtitle("Poverty rate w/ original 2011 PPPs (%)") msymbol(D)  ///
	mcolor(black) mlabcolor(black) mlabvposition(pos) legend(off) graphregion(color(white))    ///
		|| function y = x, ra(headcount_2011) clpat	(dash) lcolor(black) lcolor(black)

restore

merge m:1 countrycode using `"${dir}\data/economies.dta"', keep(3) nogen

//////Regional and global poverty counts///////

*Now include the population of countries which are not in povcalnet and prepare the data for poverty calculations

*Load population data
preserve
	*Load population data from povcalnet master file
	pcn master, load(pop)
	
	rename coveragetype coverage

	duplicates drop
	save `"${dir}\data/pop.dta"', replace
restore

merge 1:m countrycode year coverage using `"${dir}\data/pop.dta"'

preserve
*Get a file for countries not available in povcalnet

	keep if _merge==2                         //these are countries not available in povcalnet
	keep countryname countrycode coverage pop year
	save `"${dir}\data/pop_povcalnet_na.dta"', replace
restore


preserve
*Argentina national population
	keep if countrycode=="ARG"
	keep if coverage=="National"                     
	keep countrycode pop year
	sort year
	*rename pop population
	save `"${dir}\data/argentina_national_pop.dta"', replace
restore


/*
preserve
*Get full set of regions
	sort countryname year
	collapse year, by(countrycode region)
	egen year_ = mean(year), by(countrycode region)
	replace year = year_ if year==.
	*keep if region !=""
	drop year year_

	save `"${dir}\data/regions_full"', replace
restore
*/

keep if _merge==3
drop _merge

keep countrycode countryname headcount_2011 headcount_2017 population coverage year

append using `"${dir}\data/pop_povcalnet_na.dta"'   //Add the population of countries not included in povcalnet

keep if coverage=="National"

*replace population = pop if population==.   //population is the variable in povcalnet. pop is the population figure obtained from povcalnet.
*drop pop


*merge m:1 countrycode using `"${dir}\data/regions_full"', keep(3) nogen


*rename region_povcalnet region
*rename population pop

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

	gen pov_diff = headcount_2017 - headcount_2011
	gen poor_diff = pov_diff*pop
	gsort -poor_diff
	replace headcount_2017 = 100*headcount_2017
	replace headcount_2011 = 100*headcount_2011
	keep countryname ppp2011 ppp2017 headcount_2011 headcount_2017 poor_diff
	order countryname headcount_2011 headcount_2017 ppp2011 ppp2017 poor_diff
	format headcount_2011 headcount_2017 ppp2011 ppp2017 poor_diff %12.2f
	export excel using `"${dir}\results\Country_poverty_numbers_2_1.xlsx"', sheet("poor") firstrow(var) replace       
restore

*Get country-level poverty numbers  (see slide 8), sort by poverty rate
preserve
	keep if year==2015
	drop year
	merge 1:1 countrycode coveragetype  using  `"${dir}\data/ppp.dta"'
	
	
		foreach var of varlist ppp2011_original ppp2011_revised ppp2017{
		replace `var' = . if `var'==-1
		}

	rename ppp2011_original ppp2011


	gen pov_diff = headcount_2017 - headcount_2011
	gen poor_diff = pov_diff*pop
	gsort -pov_diff
	replace pov_diff = 100*pov_diff
	replace headcount_2017 = 100*headcount_2017
	replace headcount_2011 = 100*headcount_2011
	keep countryname ppp2011 ppp2017 headcount_2011 headcount_2017 pov_diff
	order countryname headcount_2011 headcount_2017 ppp2011 ppp2017 pov_diff
	format headcount_2011 headcount_2017 ppp2011 ppp2017 pov_diff %12.2f
	export excel using `"${dir}\results\Country_poverty_numbers_2_1.xlsx"', sheet("povrate") firstrow(var)
restore



*append using `"${dir}\data/pop_povcalnet_na.dta"'

merge m:1 countrycode using `"${dir}\data/economies"', keep(3) nogen

duplicates drop

keep if coverage=="National"
*duplicates drop _all, force


*Regional aggregation  (w/ old 2011 PPPs)
bysort region year: egen headcount_regavg_2011 = wtmean(headcount_2011), weight(pop)
replace headcount_2011 = headcount_regavg_2011 if missing(headcount_2011)     //Assign to countries without povcalnet poverty numbers the poverty rate of their region

*Regional aggregation  (w/ new 2011 PPPs)
bysort region year: egen headcount_regavg_2017 = wtmean(headcount_2017), weight(pop)
replace headcount_2017 = headcount_regavg_2017 if missing(headcount_2017)     //Assign to countries without povcalnet poverty numbers the poverty rate of their region







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
	replace headcount_regavg_2017 = 100*headcount_regavg_2017

	collapse headcount_regavg_2011 headcount_regavg_2017 region_pop if year==2015, by(region)
	
	gen pov_abs_diff = headcount_regavg_2017 - headcount_regavg_2011
	gen pov_rel_diff = 100*(headcount_regavg_2017 - headcount_regavg_2011)/headcount_regavg_2011
	gen poor_reg_2011 = region_pop*headcount_regavg_2011/100
	gen poor_reg_2017 = region_pop*headcount_regavg_2017/100
	gen poor_reg_diff = region_pop*pov_abs_diff/100
	keep region headcount_regavg_2011 headcount_regavg_2017 pov_abs_diff poor_reg_2011 poor_reg_2017 poor_reg_diff
	order region headcount_regavg_2011 headcount_regavg_2017 pov_abs_diff poor_reg_2011 poor_reg_2017 poor_reg_diff
	format headcount_regavg_2011 headcount_regavg_2017 pov_abs_diff poor_reg_2011 poor_reg_2017 poor_reg_diff %12.2f
	
	lab var headcount_regavg_2011 "Poverty rate, % (old)"
	lab var headcount_regavg_2017 "Poverty rate, % (new)"
	lab var pov_abs_diff "Change in poverty rate, pp"
	lab var poor_reg_2011 "Millions of poor (old)"
	lab var poor_reg_2017 "Millions of poor (new)"
	lab var poor_reg_diff "Change in millions of poor"
	
	export excel using `"${dir}\results\Global_poverty_2_1.xlsx"', sheet("regions") firstrow(varl) replace
restore



 merge m:1 countrycode year using `"${dir}\data/argentina_national_pop.dta"'    //deal with Argentina's population
 keep if _merge==1 | _merge==3
 replace pop = population if countrycode=="ARG"


*Global aggregation (w/ old & new 2011 PPPs)
preserve
	collapse headcount_2011 headcount_2017 pop if year==2015, by(countryname countrycode year)
	bysort year: egen headcount_gloavg_2011 = wtmean(headcount_2011), weight(pop)
	bysort year: egen headcount_gloavg_2017 = wtmean(headcount_2017), weight(pop)
	egen global_pop = total(pop)
	replace headcount_gloavg_2011 = 100*headcount_gloavg_2011
	replace headcount_gloavg_2017 = 100*headcount_gloavg_2017
	replace headcount_2011 = 100*headcount_2011
	replace headcount_2017 = 100*headcount_2017
	format headcount_gloavg_2011 headcount_gloavg_2017 headcount_2011 headcount_2017 %12.2f 
	export excel using `"${dir}\results\Global_poverty_2_1.xlsx"', sheet("countries", modify) firstrow(var) 
	
	
	
	collapse headcount_gloavg_2011 headcount_gloavg_2017 global_pop,by(year)
	gen pov_abs_diff_glo = headcount_gloavg_2017 - headcount_gloavg_2011
	gen poor_glo_2011 = (headcount_gloavg_2011*global_pop)/100
	gen poor_glo_2017 = (headcount_gloavg_2017*global_pop)/100
	gen poor_glo_diff = poor_glo_2017 - poor_glo_2011 
	
	gen countryname="World"
	
	keep countryname headcount_gloavg_2011 headcount_gloavg_2017 pov_abs_diff_glo poor_glo_2011 poor_glo_2017 poor_glo_diff
	order countryname headcount_gloavg_2011 headcount_gloavg_2017 pov_abs_diff_glo poor_glo_2011 poor_glo_2017 poor_glo_diff
	format headcount_gloavg_2011 headcount_gloavg_2017 pov_abs_diff_glo poor_glo_2011 poor_glo_2017 poor_glo_diff %12.2f
	
	lab var headcount_gloavg_2011 "Poverty rate, % (old)"
	lab var headcount_gloavg_2017 "Poverty rate, % (new)"
	lab var pov_abs_diff_glo "Change in poverty rate, pp"
	lab var poor_glo_2011 "Millions of poor (old)"
	lab var poor_glo_2017 "Millions of poor (new)"
	lab var poor_glo_diff "Change in millions of poor"
	
	
	export excel using `"${dir}\results\Global_poverty_2_1.xlsx"', sheet("world", modify) firstrow(varl)
restore


preserve
*Regional poverty trends (see slide #3)
replace headcount_regavg_2011 = 100*headcount_regavg_2011
replace headcount_regavg_2017 = 100*headcount_regavg_2017

keep if year>1990

collapse headcount_regavg_2011 headcount_regavg_2017,by(region year) 

tempfile headcount_regions
save `headcount_regions'

*twoway line headcount_regavg_2011 headcount_regavg_2017 year if region!="Other High Income" &year<=2015,  ///
	ytitle("Poverty rate, %") xtitle("") graphregion(lcolor(white)) ylabel(0(10)60) xlabel(1995(5)2015)     ///
	by(region)   


*twoway line headcount_regavg_2011 headcount_regavg_2017 year if region=="Middle East and North Africa", ytitle("Poverty rate, %")  ylabel(0(1)10) xlabel(1995(5)2015) by(region) sort   //only for MENA

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

collapse headcount_2011 headcount_2017 pop, by(countryname year)
bysort year: egen headcount_gloavg_2011 = wtmean(headcount_2011), weight(pop)
bysort year: egen headcount_gloavg_2017 = wtmean(headcount_2017), weight(pop)

replace headcount_gloavg_2011 = 100*headcount_gloavg_2011
replace headcount_gloavg_2017 = 100*headcount_gloavg_2017

collapse headcount_gloavg_2011 headcount_gloavg_2017,by(year) 

rename headcount_gloavg_2011 headcount_regavg_2011
rename headcount_gloavg_2017 headcount_regavg_2017
gen region = "World"

tempfile headcount_world
save `headcount_world'
	
*twoway line headcount_gloavg_2011 headcount_gloavg_2017 year if year<=2015 , ytitle("Poverty rate, %")  ylabel(0(10)40) xlabel(1995(5)2015) sort
restore
	

use `headcount_regions', clear
append using `headcount_world'


replace region= "Latin A. & Caribbean" if region=="Latin America and the Caribbean"
replace region= "Middle East & N. Africa" if region=="Middle East and North Africa"

twoway line headcount_regavg_2011 headcount_regavg_2017 year if  year<=2015,  ///
	ytitle("Poverty rate, %") xtitle("") graphregion(color(white))  ylabel(0(10)60) xlabel(1995(10)2015)  ///
	legend(lab(1 "w/ original 2011 PPPs") lab(2 "w/ 2017 PPPs")) by(region, row(2) style(compact) note("") iscale(*.9) ) 



