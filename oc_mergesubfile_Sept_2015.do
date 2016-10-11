

//Alex Reda
* Prelim notes
* Check data avaialbility for each round and country
* The data for each round is stored in a separete folder in the DS main folder. 
* This is done to provide a simpler file directory, and easy access to the data 
* of each round without having to parse through multiple layers of sub-folders - the case 
* if one had followed the file organization used by UK Data Service.

* Setting up pre-anlaysis. 
* Clearing matrices, opened files, and creating the log file
clear
if regexm(c(os),"Mac") == 1 {
	global ds `"/Users/Alex/Google Drive/DS"' 
	}
	else if regexm(c(os),"Windows") == 1 global ds `"C:/Users/areda/Google Drive/DS"' 

//global ds `"C:/Users/areda/Google Drive/DS"' //Cross directory file path defined
cd "$ds"
capture log close
log using oc_mergesubfile_sept_11.log, replace
global user "Alex Reda"
display "Analysis run by $user at `c(current_time)' `c(current_date)'
version 11.0
clear mata
clear matrix
set maxvar 27000
set more off 

****************
* Round 1
* Checking contents in general through the describe command
* I created a global file path to make analysis and retrieval easier


* Ethiopia
global etor1 $ds/Round 1/ethiopia_r1/ // Data of Ethiopia
use "$etor1/etchildlevel8yrold.dta", clear
save etor1, replace
d, short

* India
global inor1 $ds/Round 1/india_r1/ // Data of India 
use "$inor1/inchildlevel8yrold.dta", clear
save inor1, replace
d, short

* Vietnam
global peor1 $ds/Round 1/peru_r1/ // Data of Peru 
use "$peor1/pechildlevel8yrold.dta", clear
save peor1, replace
d, short

* Peru
global vnor1 $ds/Round 1/vietnam_r1/ // Data of Vietnam
use "$vnor1/vnchildlevel8yrold.dta", clear
save vnor1, replace
d, short

***************
* Appending the first round (r1) data for all countries
***************

* Capital letter variables created a problem due to naming inconsistency, so I am going to convert all variables
// into lower case before comining them for uniformity and ease of coding.
* We install renvars ado file manually first***
* To save the new files I use country odes, and o for older and r1 for round 1.

* Ethiopia
use etor1, clear
renvars *, lower
save etor1, replace

* India
use inor1, clear
renvars *, lower
save inor1, replace

* Peru
use peor1, clear
renvars *, lower
save peor1, replace

* Vietnam
use vnor1, clear
renvars *, lower
save vnor1, replace

* Merging round 1 data with identifiers. 
*********************************************************
* Next_I create a round, and country indicators to faciliate identification of 
* different rounds after mergin them.
use etor1, clear
qui append using inor1 peor1 vnor1, generate(filenum)
gen round=1

tab filenum
gen cntry = filenum //creating a country variable
label var cntry "country"
label define cntry 0 "Ethiopia" 1 "India" 2 "Peru" 3 "Vietnam" 
label values cntry cntry
drop filenum
tab cntry
save allor1, replace

*** Working on cleaning and merging household roster data ***

* Ethiopia
use "$etor1/etsubsec2householdroster8", clear
gen cntry = 0 
save etros1, replace

* India
use "$inor1/insubsec2householdroster8", clear
gen cntry = 1
save inros1, replace

* Peru
use "$peor1/pesubsec2householdroster8", clear
gen cntry = 2
save peros1, replace

* Vietnam
use "$vnor1/vnsubsec2householdroster8", clear
gen cntry = 3
save vnros1, replace

* Merging household roster data from round 1
use vnros1, clear
append using etros1 inros1 peros1, generate(filenum) force
save allros1, replace

* Calculating household composition
egen ntotal1 = count(ID), by(CHILDID) //total number of people excluding the index child
label var ntotal1 "Total number in the house excluding index child"
egen nchild1 = total(AGE < 5), missing by(CHILDID) //children younger than 5
label var nchild1 "Children younger than 5"
egen nschage1 = total(AGE >= 5 & AGE <= 18), missing by(CHILDID) //School age members
label var nschage1 "No of school age children" 
egen nworkingage1 = total(AGE > 18 & AGE <= 64), missing by(CHILDID)
replace nworkingage1 = nworkingage1 - 1 if DISABLED == 1 & AGE > 18 & AGE <= 64 & nworkingage1 != .
//In the line above I excluded disabled members since they are likely to be dependent on the family
//rather than contributing to it financially.
label var nworkingage1 "Working age members"
egen noldage1 = total(AGE>=65), by(CHILDID)
egen nmales1 = total(SEX==1), by(CHILDID) //number of males
egen nfemales1 = total(SEX==2), by(CHILDID) //number of females
recode SUPPORT 88 = 2 99 = 2 //I recode n/a and not known into no
egen nsupport1 = total(SUPPORT==1), by(CHILDID)
recode DISABLED 99 = 2 //recoding unknown disability status into no disability
* Make sure to subtract out the disabled when calculating the working age (19-65)
egen ndisable1 = total(DISABLED==1), by(CHILDID)

* Working on age order
recode AGE -9999 = .
egen anysibling1 = max(RELATE==5 | RELATE==10 | RELATE==11 | RELATE == 12), by(CHILDID)
egen totsibling1 = total(RELATE==5 | RELATE==10 | RELATE==11 | RELATE == 12), by(CHILDID)
egen maxsibage1 = max(AGE) if (RELATE==5 | RELATE==10 | RELATE==11 | RELATE == 12), by(CHILDID)
egen minsibage1 = min(AGE) if (RELATE==5 | RELATE==10 | RELATE==11 | RELATE == 12), by(CHILDID)

*egen medianage1 = median(AGE), by(CHILDID)

* Proportion of school age children in school
recode STILL 88=. 99=. 2 = 0 //I recoded 2 to 0, not in school
* I generate proportion in school to check school going rates for ages 5 to 12
egen ninschooli = count(STILL) if (AGE>=5 & AGE <= 12 & STILL==1), by(CHILDID)
*tab ninschooli cntry
*bysort cntry: sum ninschooli
egen nschoolagei = count(STILL) if (AGE>=5 & AGE <= 12 & STILL < 3), by(CHILDID)
* I next copy the values of ninschool1 and nschoolage1 to all observations
sort CHILDID ninschooli
by CHILDID: gen ninschool1 = ninschooli[1] 
by CHILDID: gen nschoolage1 = nschoolagei[1]
drop ninschooli nschoolagei
replace ninschool1 = 0 if ninschool1 == . & nschoolage1 < . //Here I am replacing zeros to indicate not being in school.
keep CHILDID ntotal1 - nschoolage1
*dropping duplicate observations
sort CHILDID
quietly by CHILDID: gen dup1 = cond(_N==1,0,_n)
drop if dup1>1 //keeping the first in the sorted cases and dropping duplicates
drop dup1
rename CHILDID childid //renaming childid to facilitate the merge process
save allros1, replace

* Merging household roster data for round 1 wiht individual data
use allor1, clear
merge 1:1 childid using allros1
save allor1, replace
drop _merge
d, short
save allor1, replace //append process complete "all countries older round 1" data saved

* =====================================================
******************
* Round 2

* Checking contents in general through the describe command
global etor2 $ds/Round 2/ethiopia_r2/
use "$etor2/etchildlevel12yrold", clear
d, short

global inor2 $ds/Round 2/india_r2/
use "$inor2/inchildlevel12yrold.dta", clear
d, short

global peor2 $ds/Round 2/peru_r2/
use "$peor2/pechildlevel12yrold.dta", clear
d, short

global vnor2 $ds/Round 2/vietnam_r2/
use "$vnor2/vnchildlevel12yrold.dta", clear
d, short

* Merging childquestions data of r2
// This is done because "childlevel12yrold" data holds only household level data, 
// and child level data is stored in "childquest12yrold".

use "$etor2/etchildlevel12yrold", clear
merge 1:1 CHILDID using "$etor2/etchildquest12yrold"
save etor2, replace
drop _merge
save etor2, replace
d, short

use "$inor2/inchildlevel12yrold.dta", clear
merge 1:1 CHILDID using "$inor2/inchildquest12yrold"
drop _merge
save inor2, replace
d, short

use "$peor2/pechildlevel12yrold.dta", clear
merge 1:1 CHILDID using "$peor2/pechildquest12yrold"
drop _merge
save peor2, replace
d, short

use "$vnor2/vnchildlevel12yrold.dta", clear
merge 1:1 CHILDID using "$vnor2/vnchildquest12yrold"
drop _merge
save vnor2, replace
d, short

* Merging round 2 individual data

* Setting caps to small caps and dropping variables with complete missingness
use etor2, clear
renvars *, lower
save, replace
dropmiss, force

use inor2, clear
renvars *, lower
save, replace
dropmiss, force

use peor2, clear
renvars *, lower
save, replace
dropmiss, force

use vnor2, clear
renvars *, lower
save vnor2, replace
dropmiss, force

***************
* Merging round 2 data from each country
********************
use vnor2, clear
dropmiss, force
qui append using inor2 peor2 etor2, generate(filenum) 
gen round = 2
** Error * detected here **---------purged now
tab filenum
gen cntry = filenum //creating a country variable
recode cntry 0=3 3=0
label var cntry "country"
label define cntry 0 "Ethiopia" 1 "India" 2 "Peru" 3 "Vietnam" 
label values cntry cntry
drop filenum
tab cntry 
save allor2, replace //append process complete "all countries older round 1" data saved

* Wrokin on merging the household roster data
**************

* Note that in the second round the rosters indclude the data about yl child
// Here I am excluding the YL child data

* Ethiopia
use "$etor2/etsubhouseholdmember12", clear
d, short
save etros2, replace

* India
use "$inor2/insubhouseholdmember12", clear
d, short
save inros2, replace

* Peru
use "$peor2/pesubhouseholdmember12", clear
d, short
save peros2, replace

* Vietnam
use "$vnor2/vnsubhouseholdmember12", clear
d, short
save vnros2, replace

* Merging household roster data from round 2
use vnros2, clear
append using etros2 inros2 peros2, generate(filenum) force
save allros2, replace


drop if ID == 0 //dropping the index child from the roster
egen ntotal2 = count(ID), by(CHILDID) //total number of people excluding the index child
label var ntotal2 "Total number in the house excluding index child"
egen nchild2 = total(AGE < 5), missing by(CHILDID) //children younger than 5
label var nchild2 "Children younger than 5"
egen nschage2 = total(AGE >= 5 & AGE <= 18), missing by(CHILDID) //School age members
label var nschage2 "No of school age children" 
egen nworkingage2 = total(AGE > 18 & AGE <= 64), missing by(CHILDID)
recode DISAB 99 = . 1 2 3 4 5 = 1 //recoding unknown disability status into no disability
replace nworkingage2 = nworkingage2 - 1 if DISAB == 1 & AGE > 18 & AGE <= 64 & nworkingage2 != .
//In the line above I excluded disabled members since they are likely to be dependent on the family financially
//rather than contributing to it financially.
label var nworkingage2 "Working age members"
egen noldage2 = total(AGE>=65), by(CHILDID)
egen nmales2 = total(MEMSEX==1), by(CHILDID) //number of males
egen nfemales2 = total(MEMSEX==2), by(CHILDID) //number of females
*recode SUPPORT 88 = 2 99 = 2 //I recode n/a and not known into no
* egen nsupport1 = total(SUPPORT==1), by(CHILDID) // not measured in round 2
*recode DISAB 99 = . 1 2 3 4 5 = 1 //recoding unknown disability status into no disability
egen ndisable2 = total(DISAB==1), by(CHILDID)

* Working on age order
recode AGE -9999 = .
egen anysibling2 = max(RELATE >=7 & RELATE <=12), by(CHILDID)
egen totsibling2 = total(RELATE >=7 & RELATE <=12), by(CHILDID)
egen maxsibage2 = max(AGE) if (RELATE==5 | RELATE==10 | RELATE==11 | RELATE == 12), by(CHILDID)
egen minsibage2 = min(AGE) if (RELATE==5 | RELATE==10 | RELATE==11 | RELATE == 12), by(CHILDID)

* Proportion of school age children in school
recode STILL 88=. 77=. 2 = 1 //I recoded 2 to 1, indicating inschool status
* I generate proportion in school to check school going rates for ages 5 to 12 among household members in that age range
egen ninschoolii = count(STILL) if (AGE>=12 & AGE <= 18 & STILL==1), by(CHILDID)
egen nschoolageii = count(STILL) if (AGE>=12 & AGE <= 18 & STILL < 2), by(CHILDID)
* I next copy the values of ninschoolii and nschoolageii to all observations
sort CHILDID ninschoolii
by CHILDID: gen ninschool2 = ninschoolii[1] 
sort CHILDID nschoolageii
by CHILDID: gen nschoolage2 = nschoolageii[1]
drop ninschoolii nschoolageii
replace ninschool2 = 0 if ninschool2 == . & nschoolage2 < . //Here I am replacing zeros to indicate not being in school.
keep CHILDID ntotal2 - nschoolage2

*dropping duplicate observations
sort CHILDID
quietly by CHILDID: gen dup2 = cond(_N==1,0,_n)
drop if dup2>1 //keeping the first in the sorted cases and dropping duplicates
drop dup2
rename CHILDID childid
save allros2, replace

* Merging household roster data for round 2
use allor2, clear
merge 1:1 childid using allros2
drop _merge
save allor2, replace
d, short


********************
* Round 3
* Checking contents in general through the describe command, and 
//	merging important data.

* Household level data are not included

global etor3 $ds/Round 3/ethiopia_r3/olderchild/
use "$etor3/et_oc_childlevel.dta", clear
d, short

global inor3 $ds/Round 3/india_r3/olderchild/
use "$inor3/in_oc_childlevel.dta", clear
d, short

global peor3 $ds/Round 3/peru_r3/olderchild/
use "$peor3/pe_oc_childlevel.dta", clear
d, short

global vnor3 $ds/Round 3/vietnam_r3/olderchild/
use "$vnor3/vn_oc_childlevel.dta", clear
d, short

* Note:
* r3 Merging household, household members and child level data for each country
* Data about household members not yet included, it will be a 1:m merge

* Ethiopia
use "$etor3/et_oc_childlevel.dta", clear
merge 1:1 CHILDID using "$etor3/et_oc_householdlevel.dta"
drop _merge
save etor3, replace
d, short

* India
use "$inor3/in_oc_childlevel.dta", clear
merge 1:1 CHILDID using "$inor3/in_oc_householdlevel.dta"
drop _merge
save inor3, replace
d, short

* Peru
use "$peor3/pe_oc_childlevel.dta", clear
merge 1:1 CHILDID using "$peor3/pe_oc_householdlevel.dta"
drop _merge
save peor3, replace
d, short

* Vietnam
use "$vnor3/vn_oc_childlevel.dta", clear
merge 1:1 CHILDID using "$vnor3/vn_oc_householdlevel.dta"
drop _merge
save vnor3, replace
d, short

************************************
* Preliminatry check up and tabulation of important variables
***************************************


*******************************************************************************
* *** Round 2 ***

* *** Round 3 ****

* Round 3 merging household and child level data for each country
* And then dropping variables with complete missingness

* Ethiopia
use etor3, clear
renvars *, lower
save, replace
dropmiss, force

* India
use inor3, clear
renvars *, lower
save, replace
dropmiss, force

* Peru
use peor3, clear
renvars *, lower
save, replace
dropmiss, force

* Vietnam
use vnor3, clear
renvars *, lower
save, replace
dropmiss, force

******************
* Merging round 3 data 
***********************

use vnor3, clear
dropmiss, force

qui append using inor3 peor3 etor3, generate(filenum) force ////Note I use 
// force here because of a type (string/numeric) mismatch in the variable 
// date of child's birth. The variable is not avaialbe for India, so it will 
// not have much of a problem, since it wont be useful anyway.
gen round = 3

tab filenum
gen cntry = filenum //creating a country variable
recode cntry 0=3 3=0
label var cntry "country"
label define cntry 0 "Ethiopia" 1 "India" 2 "Peru" 3 "Vietnam" 
label values cntry cntry
drop filenum
tab cntry 
save allor3, replace //append process complete "all countries older round 1" 
// data saved
