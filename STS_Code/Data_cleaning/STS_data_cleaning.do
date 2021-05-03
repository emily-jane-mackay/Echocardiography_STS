// setup  
// Change working directory

// set markdoc to OFF
//OFF
cd "<data file>"

set more off
set linesize 255

// set file name
// set file path
local file_name = "Program_1_working_02_24_2021"
local path = "<data file>"

// create markdoc log file
qui log using "`path'`file_name'", replace name("markdoc_log")

//OFF
// create typical file name and path
log using "`path'`file_name'.txt", text replace 


// load in the original data in .dta format
// note: converted from .xlsx to .dta in the ["0_STS_data_load_09_25_2020.do"] file
use "<data file>", clear

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************

*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// look at TEE variable
// InOpTEE: categorical: 1 = yes | 2 = No
tabulate InOpTEE, miss

// code out InOpTEE variable to string with "yes" vs "no" vs "-" 
generate InOpTEE_str = " "
replace InOpTEE_str = "yes" if InOpTEE == 1
replace InOpTEE_str = "no" if InOpTEE == 2
replace InOpTEE_str = "-" if InOpTEE != 1 & InOpTEE != 2
// sanity check
tabulate InOpTEE_str, miss
// check variable covers 2011 - 2019
tabulate InOpTEE_str surgyear, miss

// code out surgeries
// STS Data dictionary "OpValve" -> Indicate whether a surgical procedure was done on the aortic, mitral, tricuspid, or pulmonic valves
// OpValve: categorical: 1 = yes | 2 = no 
tabulate OpValve, miss

// code out OpValve variable to string variable: "yes" vs "no" 
generate OpValve_str = " " 
replace OpValve_str = "yes" if OpValve == 1
replace OpValve_str = "no" if OpValve == 2
replace OpValve_str = "-" if OpValve != 1 & OpValve != 2
// sanity check
tabulate OpValve_str, miss
// check variable covers 2011 - 2019
tabulate OpValve_str surgyear, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Issue: surgical subcategories have different indicator variables 
// v2.9  2017-2019
// v2.81 2014-2017
// v2.73 2011-2014
// Will need data three distinct data dictionaries to join
// note: aortic procedures have separate labels starting in v2.9
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Aortic Valve
// v2.9  2017-2019
// v2.81 2014-2017
// v2.73 2011-2014
// Will need data three distinct data dictionaries to join
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// aortic valve

// VSAV
tabulate VSAV, miss
tabulate VSAV surgyear, miss
//


// VSAV 2011 - 2013 
// [STS v2.73]
// tabulate VSAV surgyear, miss
//
generate VSAV_2011_2013 = " "
replace VSAV_2011_2013 = "+ Aortic Valve" if VSAV == "1" & (surgyear == 2011 | surgyear == 2012 | surgyear == 2013)
// replace VSAV_2011_2013 = "- Aortic Valve" if VSAV == "2" & (surgyear == 2011 | surgyear == 2012 | surgyear == 2013)
// sanity check
tabulate VSAV_2011_2013, miss 
tabulate VSAV VSAV_2011_2013 if surgyear <2014, miss 

//////

// VSAV 2014 (STS v2.73 & v2.81)
// {note: including indicators "4" & "5" is including "unplanned"}
// look at the years
tabulate VSAV surgyear, miss
// 
generate VSAV_2014 = " " 
replace VSAV_2014 = "+ Aortic Valve" if (VSAV == "1" | VSAV == "3" | VSAV == "4" | VSAV == "5") & (surgyear == 2014) 
// replace VSAV_2014 = "- Aortic Valve" if VSAV == "2" & (surgyear == 2014) 
// sanity check 
tabulate VSAV_2014, miss
tabulate VSAV VSAV_2014 if surgyear == 2014, miss 

//////

// VSAV 2015 - 2019 [STS v2.9]
// {note: including indicators "4" & "5" is including "unplanned"}
// look at the years 
tabulate VSAV surgyear, miss
//
generate VSAV_2015_2019 = " " 
replace VSAV_2015_2019 = "+ Aortic Valve" if (VSAV == "1" | VSAV == "3" | VSAV == "4" | VSAV == "5") & (surgyear > 2014) 
// replace VSAV_2015_2019 = "- Aortic Valve" if VSAV == "2" & (surgyear > 2014) 
// sanity check
tabulate VSAV_2015_2019, miss
tabulate VSAV VSAV_2015_2019 if surgyear > 2014, miss

//////

// check for overlap
tabulate VSAV_2011_2013 VSAV_2014, miss
tabulate VSAV_2014 VSAV_2015_2019, miss
tabulate VSAV_2011_2013 VSAV_2015_2019, miss

//////

// combine 
// {next step: deal with VSAVPr}
generate VSAV_2011_2019 = " " 
replace VSAV_2011_2019 = "+ Aortic Valve" if VSAV_2011_2013 == "+ Aortic Valve" | VSAV_2014 == "+ Aortic Valve" | VSAV_2015_2019 == "+ Aortic Valve"
// sanity check
tab VSAV_2011_2019 surgyear, miss

////////////

// VSAVPr
tabulate VSAVPr, miss
tabulate VSAVPr surgyear, miss

// VSAVPr 2011 - 2019
// [STS v2.73; v2.81; v2.9;]
// create aortic valve replacement variable
generate aortic_valve_replacement = " " 
replace aortic_valve_replacement = "+AVR" if VSAVPr == 1 & VSAV_2011_2019 == "+ Aortic Valve"
// sanity check
tab aortic_valve_replacement, miss
tab aortic_valve_replacement surgyear, miss
// generate binary categorical 1 vs 0 for logistic
generate aortic_valve_replacement_b = .
replace aortic_valve_replacement_b = 1 if aortic_valve_replacement == "+AVR" 
replace aortic_valve_replacement_b = 0 if aortic_valve_replacement != "+AVR" 
// sanity check
tabulate aortic_valve_replacement_b aortic_valve_replacement, miss

//////

// aortic valve repair
generate aortic_valve_repair = " " 
replace aortic_valve_repair = "+AV Repair" if VSAVPr == 2 & VSAV_2011_2019 == "+ Aortic Valve" 
// sanity check
tab aortic_valve_repair, miss
tab aortic_valve_repair surgyear, miss
tab aortic_valve_repair aortic_valve_replacement, miss

// generate binary categorical 1 vs 0 for logistic
generate aortic_valve_repair_b = .
replace aortic_valve_repair_b = 1 if aortic_valve_repair == "+AV Repair" 
replace aortic_valve_repair_b = 0 if aortic_valve_repair != "+AV Repair" 
// sanity check
tabulate aortic_valve_repair_b aortic_valve_repair, miss

//////

// combine
generate AV_repair_replace = " " 
replace AV_repair_replace = "+AVR/R" if aortic_valve_replacement == "+AVR" | aortic_valve_repair == "+AV Repair"

// generate combined binary categorical for logistic
generate AV_repair_replace_b = . 
replace AV_repair_replace_b = 1 if AV_repair_replace == "+AVR/R"
replace AV_repair_replace_b = 0 if AV_repair_replace != "+AVR/R"
// sanity check
tabulate AV_repair_replace_b AV_repair_replace, miss

//////

// string variables
tabulate aortic_valve_replacement, miss
tabulate aortic_valve_repair, miss
tabulate AV_repair_replace, miss

// binary for logistic variables
tabulate aortic_valve_replacement_b
tabulate aortic_valve_repair_b
tabulate AV_repair_replace_b

//////

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Proximal Aortic Surgeries
// Issue: Proximal Aortic surgical subcategories have different indicator variables 
// v2.9  2017-2019* new proximal aortic surgical variables added [previously under VSAVPr variable]
// v2.81 2014-2017 [aortic root variables under VSAVPr]
// v2.73 2011-2014 [aortic root variables under VSAVPr]
// Will need data three distinct data dictionaries to join
// note: aortic procedures have separate labels starting in v2.9
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// Look for proximal aortic surgeries in VSAVPr variable
tabulate VSAVPr, miss
tabulate VSAVPr surgyear, miss
// Look for proximal aortic surgeries categorized seperately in V2.9
tabulate VSAVRoot, miss
tabulate VSAVRoot surgyear, miss
// Look for proximal root surgeries
tabulate VSAVRootOReimp, miss
tabulate VSAVRootOReimp surgyear, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Proximal Aortic Surgeries
// Bentall: bioprosthetic or mechanical
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// Look for Bentall breakdown
tabulate VSAVRootOReimpTy, miss
tabulate VSAVRootOReimpTy surgyear, miss
// (1) generate bentall variable
generate bentall_ao_root_valve_conduit = " " 
replace bentall_ao_root_valve_conduit = "Bentall(Mech/Bio)" if VSAVPr == 3 | VSAVPr == 14 | (VSAVRoot == 1 & (VSAVRootOReimpTy == 1 | VSAVRootOReimpTy == 2)) 
// tabulate 
tabulate bentall_ao_root_valve_conduit, miss
tabulate bentall_ao_root_valve_conduit surgyear, miss
// generate binary categorical 1 vs 0 for logistic
generate bentall_ao_root_valve_conduit_b = .
replace bentall_ao_root_valve_conduit_b = 1 if bentall_ao_root_valve_conduit == "Bentall(Mech/Bio)"
replace bentall_ao_root_valve_conduit_b = 0 if bentall_ao_root_valve_conduit != "Bentall(Mech/Bio)"
// sanity check
tabulate bentall_ao_root_valve_conduit_b bentall_ao_root_valve_conduit, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Proximal Aortic Surgeries
// Aortic Valve Sparing Root Surgeris
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// Look for aortic valve sparing breakdown
tabulate VSAVSparRtOp, miss
tabulate VSAVSparRtOp surgyear, miss
// (2) generate aortic valve sparing root variable
generate av_sparing_root = " " 
replace av_sparing_root = "AVS Root" if VSAVPr == 5 | VSAVPr == 6 | VSAVPr == 10 | VSAVPr == 11 | VSAVPr == 15 | VSAVRoot == 1 
// tabulate 
tabulate av_sparing_root, miss
tabulate av_sparing_root surgyear, miss
// generate binary categorical 1 vs 0 for logistic
generate av_sparing_root_b = .
replace av_sparing_root_b = 1 if av_sparing_root == "AVS Root" 
replace av_sparing_root_b = 0 if av_sparing_root != "AVS Root" 
// sanity check
tabulate av_sparing_root_b av_sparing_root, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Proximal Aortic Surgeries
// Aortic non-valve conduit / Ross / homograft
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// Look for non-valved conduit / Ross / homograft procedures
tabulate AorticImplantTy, miss
tabulate AorticImplantTy surgyear, miss
// Look for non-valved conduit / Ross / homograft procedures
tabulate VSAVRootOReimpTy surgyear, miss
// (3) generate non-valved conduit / Ross / homograft variable
generate non_v_conduit_Ross_homograft = " " 
replace non_v_conduit_Ross_homograft = "non-v conduit/Ross/homograft" if VSAVPr == 4 | VSAVPr == 7 | VSAVPr == 8 | VSAVPr == 9 | VSAVPr == 13 | AorticImplantTy == 5 | AorticImplantTy == 6 | AorticImplantTy == 7 | VSAVRootOReimpTy == 3 | VSAVRootOReimpTy == 4
// tabulate
tabulate non_v_conduit_Ross_homograft, miss
tabulate non_v_conduit_Ross_homograft surgyear, miss
// generate binary categorical 1 vs 0 for logistic
generate non_v_conduit_Ross_homograft_b = .
replace non_v_conduit_Ross_homograft_b = 1 if non_v_conduit_Ross_homograft == "non-v conduit/Ross/homograft"
replace non_v_conduit_Ross_homograft_b = 0 if non_v_conduit_Ross_homograft != "non-v conduit/Ross/homograft"
// sanity check
tabulate non_v_conduit_Ross_homograft_b non_v_conduit_Ross_homograft, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Proximal Aortic Surgeries
// Three buckets
// (a) Bentall procedures (bioprosthetic or mechanical)
// (b) Aortic valve sparing root proceudres 
// (c) Aortic non-valved conduit, Ross, or homograft procedures 
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

//////

// combine
generate aortic_proximal = " " 
replace aortic_proximal = "Proximal Aortic" if bentall_ao_root_valve_conduit == "Bentall(Mech/Bio)" | av_sparing_root == "AVS Root" | non_v_conduit_Ross_homograft == "non-v conduit/Ross/homograft"

// generate binary categorical 1 vs 0 for logistic
generate aortic_proximal_b = . 
replace aortic_proximal_b = 1 if aortic_proximal == "Proximal Aortic"
replace aortic_proximal_b = 0 if aortic_proximal != "Proximal Aortic" 
// sanity check
tabulate aortic_proximal_b aortic_proximal, miss

// string variables
tabulate bentall_ao_root_valve_conduit, miss
tabulate av_sparing_root, miss
tabulate non_v_conduit_Ross_homograft, miss
tabulate aortic_proximal, miss

// binary for logistic
tabulate bentall_ao_root_valve_conduit_b, miss
tabulate av_sparing_root_b, miss
tabulate non_v_conduit_Ross_homograft_b, miss
tabulate aortic_proximal_b, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Mitral Valve
// v2.9  2017-2019
// v2.81 2014-2017 
// v2.73 2011-2014 
// Will need data three distinct data dictionaries to join
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// VSMV
tabulate VSMV, miss
tabulate VSMV surgyear, miss
//

// VSMV 2011 - 2013 
// [STS v2.73]
tabulate VSMV surgyear, miss
//
generate VSMV_2011_2013 = " "
replace VSMV_2011_2013 = "+ Mitral Valve" if VSMV == "1" & (surgyear == 2011 | surgyear == 2012 | surgyear == 2013)
// sanity check
tabulate VSMV_2011_2013, miss 
tabulate VSMV VSMV_2011_2013 if surgyear <2014, miss 

// VSMV 2014 
// [STS v2.73 & v2.81]
// {note: including indicators "4" & "5" is including "unplanned"}
// look at the years
tabulate VSMV surgyear, miss
// 
generate VSMV_2014 = " " 
replace VSMV_2014 = "+ Mitral Valve" if (VSMV == "1" | VSMV == "3" | VSMV == "4" | VSMV == "5") & (surgyear == 2014) 
// sanity check 
tabulate VSMV_2014, miss
tabulate VSMV VSMV_2014 if surgyear == 2014, miss 

// VSMV 2015 - 2019 
// [STS v2.9]
// {note: including indicators "4" & "5" is including "unplanned"}
// look at the years 
tabulate VSMV surgyear, miss
//
generate VSMV_2015_2019 = " " 
replace VSMV_2015_2019 = "+ Mitral Valve" if (VSMV == "1" | VSMV == "3" | VSMV == "4" | VSMV == "5") & (surgyear > 2014) 
// sanity check
tabulate VSMV_2015_2019, miss
tabulate VSMV VSMV_2015_2019 if surgyear > 2014, miss

//////

// check for overlap
tabulate VSMV_2011_2013 VSMV_2014, miss
tabulate VSMV_2014 VSMV_2015_2019, miss
tabulate VSMV_2011_2013 VSMV_2015_2019, miss

// combine 
generate VSMV_2011_2019 = " " 
replace VSMV_2011_2019 = "+ Mitral Valve" if (VSMV_2011_2013 == "+ Mitral Valve" | VSMV_2014 == "+ Mitral Valve" | VSMV_2015_2019 == "+ Mitral Valve" | VSMVPr == 3 | VSMVPr == 5)
// sanity check
tab VSMV_2011_2019 surgyear, miss

////////////

// {next step: deal with VSMVPr}
// VSMVPr
tabulate VSMVPr, miss
tabulate VSMVPr surgyear, miss

// VSMVPr 2011 - 2019
// [STS v2.73; v2.81; v2.9;]
// create mitral valve replacement variable
tabulate VSMVPr surgyear, miss
generate mitral_valve_replacement = " " 
replace mitral_valve_replacement = "+MVR" if VSMVPr == 2 & VSMV_2011_2019 == "+ Mitral Valve"
// sanity check
tab mitral_valve_replacement, miss
tab mitral_valve_replacement surgyear, miss

// generate binary categorical 1 vs 0 for logistic
generate mitral_valve_replacement_b = . 
replace mitral_valve_replacement_b = 1 if mitral_valve_replacement == "+MVR"
replace mitral_valve_replacement_b = 0 if mitral_valve_replacement != "+MVR"
// sanity check
tabulate mitral_valve_replacement mitral_valve_replacement

//////

// mitral valve repair
generate mitral_valve_repair = " " 
replace mitral_valve_repair = "+MV Repair" if VSMVPr == 1 & VSMV_2011_2019 == "+ Mitral Valve" 
// sanity check
tab mitral_valve_repair, miss
tab mitral_valve_repair surgyear, miss
tab mitral_valve_repair mitral_valve_replacement, miss

// generate binary categorical 1 vs 0 for logistic
generate mitral_valve_repair_b = . 
replace mitral_valve_repair_b = 1 if mitral_valve_repair == "+MV Repair" 
replace mitral_valve_repair_b = 0 if mitral_valve_repair != "+MV Repair" 
// sanity check
tabulate mitral_valve_repair_b mitral_valve_repair, miss

//////

// combine
generate MV_repair_replace = " " 
replace MV_repair_replace = "+MVR/R" if mitral_valve_replacement == "+MVR" | mitral_valve_repair == "+MV Repair"

// generate binary categorical 1 vs 0 for logistic
generate MV_repair_replace_b = . 
replace MV_repair_replace_b = 1 if MV_repair_replace == "+MVR/R"
replace MV_repair_replace_b = 0 if MV_repair_replace != "+MVR/R" 
// sanity check
tabulate MV_repair_replace_b MV_repair_replace, miss

// string variables
tabulate mitral_valve_replacement, miss
tabulate mitral_valve_repair, miss
tabulate MV_repair_replace, miss

// binary for logistic
tabulate mitral_valve_replacement_b
tabulate mitral_valve_repair_b
tabulate MV_repair_replace_b 

//////


///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Tricuspid Valve
// v2.9  2017-2019
// v2.81 2014-2017
// v2.73 2011-2014 
// Will need data three distinct data dictionaries to join
// Note: 2018 & 2019 missing tricuspid granularity from "OpTricus"
// Note: Will only have VSTV yes vs no for 2018 & 2019
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////


// OpTricus vs VSTV 
tabulate OpTricus, miss
tabulate OpTricus surgyear, miss
tabulate VSTV, miss
tabulate VSTV surgyear, miss

// OpTricus vs VSTV 
tabulate OpTricus surgyear, miss
tabulate VSTV surgyear, miss

// generate a combined repair/replacement variable
generate tricuspid_repair_replace = " "
replace tricuspid_repair_replace = "+TVR" if (OpTricus == 2 | OpTricus == 3 | OpTricus == 4 | OpTricus == 5 | OpTricus == 6) | (VSTV == "3" | VSTV == "4" | VSTV == "5")

// generate binary categorical 1 vs 0 for logistic
generate tricuspid_repair_replace_b = . 
replace tricuspid_repair_replace_b = 1 if tricuspid_repair_replace == "+TVR"
replace tricuspid_repair_replace_b = 0 if tricuspid_repair_replace != "+TVR"
// sanity check
tabulate tricuspid_repair_replace_b tricuspid_repair_replace, miss

//////

// string variable
tabulate tricuspid_repair_replace 

// binary for logistic
tabulate tricuspid_repair_replace_b

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Pulmonic Valve
// v2.9  2017-2019
// v2.81 2014-2017
// v2.73 2011-2014 
// Will need data three distinct data dictionaries to join
// Note: pulmonic valve OpPulm covers all years (unlike tricuspid)
// Note: create variable to match tricuspid (e.g. repair & replacement variable)
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// OpPulm vs VSPV
tabulate OpPulm, miss
tabulate OpPulm surgyear, miss
tabulate VSPV, miss
tabulate VSPV surgyear, miss 

// generate a combined repair/replacement variable
generate pulmonic_repair_replace = " " 
replace pulmonic_repair_replace = "+PVR" if (OpPulm == 2 | OpPulm == 3 | OpPulm == 4) | (VSPV == "3" | VSPV == "4" | VSPV == "5")

// generate binary categorical 1 vs 0 for logistic
generate pulmonic_repair_replace_b = . 
replace pulmonic_repair_replace_b = 1 if pulmonic_repair_replace == "+PVR"
replace pulmonic_repair_replace_b = 0 if pulmonic_repair_replace != "+PVR"
// sanity check
tabulate pulmonic_repair_replace_b pulmonic_repair_replace, miss

//////

// string variable 
tabulate pulmonic_repair_replace

// binary for logistic
tabulate pulmonic_repair_replace_b


///////////////////////////////////////////////////////////////////////////////

// list valve string variables
tabulate aortic_valve_replacement, miss
tabulate aortic_valve_repair, miss
tabulate AV_repair_replace, miss
tabulate mitral_valve_replacement, miss
tabulate mitral_valve_repair, miss
tabulate MV_repair_replace, miss
tabulate tricuspid_repair_replace, miss
tabulate pulmonic_repair_replace, miss
tabulate bentall_ao_root_valve_conduit, miss
tabulate av_sparing_root, miss
tabulate non_v_conduit_Ross_homograft, miss
tabulate aortic_proximal, miss

// list of valve binary (1 vs 0) for logistic regression variables
tabulate aortic_valve_replacement_b, miss
tabulate aortic_valve_repair_b, miss
tabulate AV_repair_replace_b, miss
tabulate mitral_valve_replacement_b, miss
tabulate mitral_valve_repair_b, miss
tabulate MV_repair_replace_b, miss
tabulate tricuspid_repair_replace_b, miss
tabulate pulmonic_repair_replace_b, miss
tabulate bentall_ao_root_valve_conduit_b, miss
tabulate av_sparing_root_b, miss
tabulate non_v_conduit_Ross_homograft_b, miss
tabulate aortic_proximal_b, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// +CABG Surgery
// v2.9  2017-2019
// v2.81 2014-2017 
// v2.73 2011-2014 
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// OpCAB
tabulate OpCAB, miss
tabulate OpCAB surgyear, miss

// generate a +CABG variable
generate CABG = " " 
replace CABG = "+CABG" if (OpCAB == 1 | OpCAB == 3 | OpCAB == 4 | OpCAB == 5)
replace CABG = "-CABG" if (OpCAB != 1 & OpCAB != 3 & OpCAB != 4 & OpCAB != 5)
// encode for logistic
encode CABG, generate(CABG_e)

// binary
generate CABG_b = cond(CABG == "+CABG", 1, 0)

// sanity check
tabulate CABG, miss
tabulate OpCAB CABG, miss
tabulate CABG surgyear, miss
tabulate CABG_e, miss

///////////////////////////////////////////////////////////////////////////////
*******************************************************************************
// Create multiple valve variable
*******************************************************************************
///////////////////////////////////////////////////////////////////////////////

// generate multiple valve variables
// four valve
generate four_valve = .
replace four_valve = 1 if AV_repair_replace == "+AVR/R" & MV_repair_replace == "+MVR/R" & tricuspid_repair_replace == "+TVR" & pulmonic_repair_replace == "+PVR"

// three valve
generate three_valve = . 
replace three_valve = 1 if AV_repair_replace == "+AVR/R" & MV_repair_replace == "+MVR/R" & (tricuspid_repair_replace == "+TVR" | pulmonic_repair_replace == "+PVR") & four_valve != 1

// two valve
generate two_valve = . 
replace two_valve = 1 if AV_repair_replace == "+AVR/R" & (MV_repair_replace == "+MVR/R" | tricuspid_repair_replace == "+TVR" | pulmonic_repair_replace == "+PVR") & three_valve != 1 & two_valve != 1

// multiple valve single variable
generate multiple_valve = " " 
replace multiple_valve = "2 V" if two_valve == 1
replace multiple_valve = "3 V" if three_valve == 1
replace multiple_valve = "4 V" if four_valve == 1
tabulate multiple_valve, miss
// encode for logistic
encode multiple_valve, generate(multiple_valve_e)
tabulate multiple_valve_e, miss

// sanity check 
tabulate multiple_valve AV_repair_replace, miss
tabulate multiple_valve MV_repair_replace, miss

// robotic surgery
generate robotic_surg = " " 
replace robotic_surg = "yes" if Robotic == 1
replace robotic_surg = "no" if Robotic == 2

///////////////////////////////////////////////////////////////////////////////

// Find missing surgeries

///////////////////////////////////////////////////////////////////////////////

// look at instances where observations don't have a valve or aortic surgical case 
count if aortic_valve_replacement_b == 1 | aortic_valve_repair_b == 1 | AV_repair_replace_b == 1 | mitral_valve_replacement_b == 1 | mitral_valve_repair_b == 1 | MV_repair_replace_b == 1 | tricuspid_repair_replace_b == 1 | pulmonic_repair_replace_b == 1 | bentall_ao_root_valve_conduit_b == 1 | av_sparing_root_b == 1 | non_v_conduit_Ross_homograft_b == 1 | aortic_proximal_b == 1 

// count without AV_repair_replace_b | MV_repair_replace_b | aortic_proximal_b
count if aortic_valve_replacement_b == 1 | aortic_valve_repair_b == 1 | mitral_valve_replacement_b == 1 | mitral_valve_repair_b == 1 | tricuspid_repair_replace_b == 1 | pulmonic_repair_replace_b == 1 | bentall_ao_root_valve_conduit_b == 1 | av_sparing_root_b == 1 | non_v_conduit_Ross_homograft_b == 1 
// count is consistent

// count with negative condition
count if aortic_valve_replacement_b != 1 & aortic_valve_repair_b != 1 & AV_repair_replace_b != 1 & mitral_valve_replacement_b != 1 & mitral_valve_repair_b != 1 & MV_repair_replace_b != 1 & tricuspid_repair_replace_b != 1 & pulmonic_repair_replace_b != 1 & bentall_ao_root_valve_conduit_b != 1 & av_sparing_root_b != 1 & non_v_conduit_Ross_homograft_b != 1 & aortic_proximal_b != 1

// count negative condition without AV_repair_replace_b | MV_repair_replace_b | aortic_proximal_b
count if aortic_valve_replacement_b != 1 & aortic_valve_repair_b != 1 & mitral_valve_replacement_b != 1 & mitral_valve_repair_b != 1 & tricuspid_repair_replace_b != 1 & pulmonic_repair_replace_b != 1 & bentall_ao_root_valve_conduit_b != 1 & av_sparing_root_b != 1 & non_v_conduit_Ross_homograft_b != 1 
// count is consistent

// create an include variable to find missing cases
// note: this include variable indicates valve or aortic surgery categorized

///////////////////////////////////////////////////////////////////////////////

// Create an include variable

///////////////////////////////////////////////////////////////////////////////

generate include = cond((aortic_valve_replacement_b == 1 | aortic_valve_repair_b == 1 | AV_repair_replace_b == 1 | mitral_valve_replacement_b == 1 | mitral_valve_repair_b == 1 | MV_repair_replace_b == 1 | tricuspid_repair_replace_b == 1 | pulmonic_repair_replace_b == 1 | bentall_ao_root_valve_conduit_b == 1 | av_sparing_root_b == 1 | non_v_conduit_Ross_homograft_b == 1 | aortic_proximal_b == 1), 1, 0)

tabulate include, miss

///////////////////////////////////////////////////////////////////////////////

// observation count of include == 0 is 32,597 observations
// 32,597 observations need to be accounted for

///////////////////////////////////////////////////////////////////////////////

// Concretely defining the Cohort 

///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////

// 1st issue: Start by dealing with the CABG issue: i.e. get rid of CABG surgeries outside the "include" condition

///////////////////////////////////////////////////////////////////////////////

tabulate CABG include, miss
tabulate CABG_b include, miss

// generate plus_CABG
generate plus_CABG = " " 
replace plus_CABG = "+CABG" if (CABG_b == 1 & include == 1)
// sanity check
tabulate plus_CABG include, miss
// create binary
generate plus_CABG_b = cond(plus_CABG == "+CABG", 1, 0) & include == 1
// sanity check
tabulate plus_CABG include, miss
tabulate plus_CABG_b include, miss

// generate CABG_without_aortic_or_valve
generate CABG_without_aortic_or_valve = " " 
replace CABG_without_aortic_or_valve = "CABG" if (CABG_b == 1 & include != 1)
// create binary
generate CABG_without_aortic_or_valve_b = cond(CABG_without_aortic_or_valve == "CABG", 1, 0) & include != 1
// sanity check
tabulate CABG_without_aortic_or_valve include, miss
tabulate CABG_without_aortic_or_valve_b include, miss

///////////////////////////////////////////////////////////////////////////////

// 2nd issue: OpOCard {"other cardiac surgery" defined two ways: (1) w/aortic or valve; (2) w/out aortic or valve}

///////////////////////////////////////////////////////////////////////////////

// generate plus_other_cardiac_surg (i.e. surgery other than valve or aortic) derived from variable: "OpOCard"
// plus_other_cardiac_surg + one of above categories (condition include == 1)
// include plus_other_cardiac_surg as surgical category
generate plus_other_cardiac_surg = " " 
replace plus_other_cardiac_surg = "+surgery other" if (OpOCard == 1 | OpOCard == 3 | OpOCard == 4 | OpOCard == 5) & include == 1
// sanity check
tabulate plus_other_cardiac_surg include, miss
// create binary
generate plus_other_cardiac_surg_b = cond(plus_other_cardiac_surg == "+surgery other", 1, 0) & include == 1
// sanity check
tabulate plus_other_cardiac_surg include, miss
tabulate plus_other_cardiac_surg_b include, miss

///////////////////////////////////////////////////////////////////////////////

// generate other_cardiac_surg_isolated (i.e. surgery other than aortic or valve) derived from variable: "OpOCard"
// other_cardiac_surg_isolated is defined as being *without* one of above categories (condition include != 1)
// this variable will be excluded from cohort
generate other_cardiac_surg_isolated = " " 
replace other_cardiac_surg_isolated = "surgery other" if (OpOCard == 1 | OpOCard == 3 | OpOCard == 4 | OpOCard == 5) & include != 1
// sanity check
tabulate other_cardiac_surg_isolated include, miss
// create binary
generate other_cardiac_surg_isolated_b = cond(other_cardiac_surg_isolated == "surgery other", 1, 0) & include != 1
// sanity check
tabulate other_cardiac_surg_isolated include, miss
tabulate other_cardiac_surg_isolated_b include, miss
tabulate other_cardiac_surg_isolated_b if include != 1 

///////////////////////////////////////////////////////////////////////////////

// Start a cohort development

///////////////////////////////////////////////////////////////////////////////

generate CT_surgery = 1

// overall cohort development
tabulate CT_surgery, miss
// inclusion count
count if CT_surgery == 1

// exclude those undergoing CABG surgery without identified aortic surgery or valve surgery
tabulate CABG_without_aortic_or_valve_b if CT_surgery == 1
// exclusion count
count if CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1

// exclude those undergoing a surgery defined as "other" *without* an additional aortic or valve surgery
tabulate other_cardiac_surg_isolated_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1
// exclusion count
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1

///////////////////////////////////////////////////////////////////////////////

// 3rd issue: Key missing variable: OpValve {"valve unspecified" (i.e. valve not otherwise categorized)} 

///////////////////////////////////////////////////////////////////////////////

// generate valve_unspecified (i.e. valve surgery not categorized) 
// conditional logic include != 1
generate valve_unspecified = " " 
replace valve_unspecified = "+valve" if OpValve == 1 & include != 1

// binary
generate valve_unspecified_b = cond(valve_unspecified == "+valve", 1, 0) & include != 1

// sanity check
tabulate valve_unspecified include, miss
tabulate valve_unspecified_b include, miss

///////////////////////////////////////////////////////////////////////////////

// Continue cohort development

///////////////////////////////////////////////////////////////////////////////

// overall cohort development
tabulate CT_surgery, miss
// inclusion count
count if CT_surgery == 1

// exclude those undergoing CABG surgery without identified aortic surgery or valve surgery
tabulate CABG_without_aortic_or_valve_b if CT_surgery == 1
// exclusion count
count if CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1

// exclude those undergoing a surgery defined as "other" *without* an additional aortic or valve surgery
tabulate other_cardiac_surg_isolated_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1
// exclusion count
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1

// exclude those undergoing a valve surgery not otherwise specified
tabulate valve_unspecified_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1
// exclusion count 
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1 | valve_unspecified_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1

// sanity check: make sure CABG surgeries (with aortic or valve surgery) still present
tabulate CABG if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1

///////////////////////////////////////////////////////////////////////////////

// 4th issue: Key missing variable: AortProc {"aortic procedure performed" (i.e. unspecified aortic procedure} } 

///////////////////////////////////////////////////////////////////////////////

// generate aortic_uncategorized (i.e. aortic procedure not categorized)
// conditional logic: include != 1
generate aortic_uncategorized = " " 
replace aortic_uncategorized = "+aortic" if (AortProc == 3 | AortProc == 4 | AortProc == 5) & include != 1

// binary
generate aortic_uncategorized_b = cond(aortic_uncategorized == "+aortic", 1, 0)
// sanity check
tabulate aortic_uncategorized include, miss
tabulate aortic_uncategorized_b include, miss

///////////////////////////////////////////////////////////////////////////////

// Continue cohort development

///////////////////////////////////////////////////////////////////////////////

// overall cohort development
tabulate CT_surgery, miss
// inclusion count
count if CT_surgery == 1

// exclude those undergoing CABG surgery without identified aortic surgery or valve surgery
tabulate CABG_without_aortic_or_valve_b if CT_surgery == 1
// exclusion count
count if CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1

// exclude those undergoing a surgery defined as "other" *without* an additional aortic or valve surgery
tabulate other_cardiac_surg_isolated_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1
// exclusion count
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1

// exclude those undergoing a valve surgery not otherwise specified
tabulate valve_unspecified_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1
// exclusion count 
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1 | valve_unspecified_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1

// exclude those underoing an aortic surgery not otherwise categorized
tabulate aortic_uncategorized_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1
// exclusion count 
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1 | valve_unspecified_b == 1 | aortic_uncategorized_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1


///////////////////////////////////////////////////////////////////////////////

// missing data categorized and accounted for in cohort development 

///////////////////////////////////////////////////////////////////////////////

//ON
/***
Defining the Cohort
=====================
Exclusion and Inclusion Counts
--------------------------------------
***/ 
//OFF

///////////////////////////////////////////////////////////////////////////////

// Finalized cohort 

///////////////////////////////////////////////////////////////////////////////

//ON
// overall cohort 
tabulate CT_surgery, miss
// inclusion count
count if CT_surgery == 1
//OFF

//ON
/***
### Starting observation count: 905,533
***/ 
//OFF

//ON
// exclude those undergoing CABG surgery without identified aortic surgery or valve surgery
tabulate CABG_without_aortic_or_valve_b if CT_surgery == 1
// exclusion count
count if CABG_without_aortic_or_valve_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1
// sanity check count
di 905533 - 897241
//OFF

//ON
/***
### Observations excluded: 8,292 undergoing CABG without an identified aortic or valve surgery
### Running inclusion count: 879,241 
***/ 
//OFF

//ON
// exclude those undergoing a surgery defined as "other" *without* an additional aortic or valve surgery
tabulate other_cardiac_surg_isolated_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1
// total running exclusion count
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1
// running inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1
// sanity check count  
di 897241 - 889238
//OFF 

//ON
/***
### Observations excluded: 8,003 undergoing an "other cardiac surgery" without an identified aortic or valve surgery
### Running inclusion count: 889,238
***/ 
//OFF

//ON
// exclude those undergoing a valve surgery not otherwise specified
tabulate valve_unspecified_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1
// total running exclusion count 
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1 | valve_unspecified_b == 1
// running inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1
// sanity check count  
di 889238 - 887514
//OFF

//ON
/***
### Observations excluded: 1,724 undergoing an "unspecified valve surgery" (i.e. not previously categorized) 
### Running inclusion count: 887,514
***/ 
//OFF

//ON
// exclude those underoing an aortic surgery not otherwise categorized
tabulate aortic_uncategorized_b if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1
// exclusion count 
count if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1 | valve_unspecified_b == 1 | aortic_uncategorized_b == 1
// inclusion count
count if CT_surgery == 1 & CABG_without_aortic_or_valve_b != 1 & other_cardiac_surg_isolated_b != 1 & valve_unspecified_b != 1
// sanity check count
di 887514 - 14578
//OFF 

//ON
/***
### Observations excluded: 14,578 undergoing an "uncategorized aortic surgery" (i.e. not previously categorized into one of aortic surgery categories) 
### Final cohort inclusion count: 872,936
***/ 
//OFF

///////////////////////////////////////////////////////////////////////////////

// Create exclude variable using conditions created in cohort development 

///////////////////////////////////////////////////////////////////////////////

generate exclude = . 
replace exclude = 1 if other_cardiac_surg_isolated_b == 1 | CABG_without_aortic_or_valve_b == 1 | valve_unspecified_b == 1 | aortic_uncategorized_b == 1

///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////

//ON
// sanity check: include and exclude conditions
tabulate include exclude, miss
tabulate CT_surgery if include == 1, miss
tabulate CT_surgery if exclude != 1, miss
//OFF

//ON
/***
Final Cohort Observation Count Confirmed: 872,936
==============================================
***/ 
//OFF

// Procedure categorization sanity check
// list valve string variables
tabulate aortic_valve_replacement, miss
tabulate aortic_valve_replacement if exclude != 1, miss
tabulate aortic_valve_repair, miss
tabulate aortic_valve_repair if exclude != 1, miss
tabulate AV_repair_replace, miss
tabulate AV_repair_replace if exclude != 1, miss
tabulate mitral_valve_replacement, miss
tabulate mitral_valve_replacement if exclude != 1, miss
tabulate mitral_valve_repair, miss
tabulate mitral_valve_repair if exclude != 1, miss
tabulate MV_repair_replace, miss
tabulate MV_repair_replace if exclude != 1, miss
tabulate tricuspid_repair_replace, miss
tabulate tricuspid_repair_replace if exclude != 1, miss
tabulate pulmonic_repair_replace, miss
tabulate pulmonic_repair_replace if exclude != 1, miss
tabulate bentall_ao_root_valve_conduit, miss
tabulate bentall_ao_root_valve_conduit if exclude != 1, miss
tabulate av_sparing_root, miss
tabulate av_sparing_root if exclude != 1, miss
tabulate non_v_conduit_Ross_homograft, miss
tabulate non_v_conduit_Ross_homograft if exclude != 1, miss
tabulate aortic_proximal, miss
tabulate aortic_proximal if exclude != 1, miss
tabulate plus_CABG, miss
tabulate plus_CABG if exclude != 1, miss
tabulate plus_other_cardiac_surg, miss
tabulate plus_other_cardiac_surg if exclude != 1, miss

// list of valve binary (1 vs 0)
tabulate aortic_valve_replacement_b, miss
tabulate aortic_valve_replacement_b if exclude != 1, miss
tabulate aortic_valve_repair_b, miss
tabulate aortic_valve_repair_b if exclude != 1, miss
tabulate AV_repair_replace_b, miss
tabulate AV_repair_replace_b if exclude != 1, miss
tabulate mitral_valve_replacement_b, miss
tabulate mitral_valve_replacement_b if exclude != 1, miss
tabulate mitral_valve_repair_b, miss
tabulate mitral_valve_repair_b if exclude != 1, miss
tabulate MV_repair_replace_b, miss
tabulate MV_repair_replace_b if exclude != 1, miss
tabulate tricuspid_repair_replace_b, miss
tabulate tricuspid_repair_replace_b if exclude != 1, miss
tabulate pulmonic_repair_replace_b, miss
tabulate pulmonic_repair_replace_b if exclude != 1, miss
tabulate bentall_ao_root_valve_conduit_b, miss
tabulate bentall_ao_root_valve_conduit_b if exclude != 1, miss
tabulate av_sparing_root_b, miss
tabulate av_sparing_root_b if exclude != 1, miss
tabulate non_v_conduit_Ross_homograft_b, miss
tabulate non_v_conduit_Ross_homograft_b if exclude != 1, miss
tabulate aortic_proximal_b, miss
tabulate aortic_proximal_b if exclude != 1, miss
tabulate plus_CABG_b, miss
tabulate plus_CABG_b if exclude != 1, miss
tabulate plus_other_cardiac_surg_b, miss
tabulate plus_other_cardiac_surg_b if exclude != 1, miss


//ON
/***
Surgical variables to be included in the match
==============================================
***/ 
//OFF

//ON 
tabulate aortic_valve_replacement_b if exclude != 1, miss
tabulate aortic_valve_repair_b if exclude != 1, miss
tabulate mitral_valve_replacement_b if exclude != 1, miss
tabulate mitral_valve_repair_b if exclude != 1, miss
tabulate tricuspid_repair_replace_b if exclude != 1, miss
tabulate pulmonic_repair_replace_b if exclude != 1, miss
tabulate bentall_ao_root_valve_conduit_b if exclude != 1, miss
tabulate av_sparing_root_b if exclude != 1, miss
tabulate non_v_conduit_Ross_homograft_b if exclude != 1, miss
tabulate plus_CABG_b if exclude != 1, miss
tabulate plus_other_cardiac_surg_b if exclude != 1, miss
//OFF 

///////////////////////////////////////////////////////////////////////////////

// Demographics

///////////////////////////////////////////////////////////////////////////////

// Age
summarize Age, detail

// BSA
summarize BSA, detail 

// HeightCm
summarize HeightCm, detail

// WeightKg
summarize WeightKg, detail

// calculate BMI
// (1) convert cm to m
generate HeightM = HeightCm/100 
// check variable
summarize HeightM, detail
// (2) BMI formula
generate BMI = WeightKg / (HeightM)^2
// check
summarize BMI, detail

// sex
generate sex = " " 
replace sex = "male" if Gender == 1
replace sex = "female" if Gender == 2 
tabulate sex, miss
// encode for logistic
encode sex, generate(sex_e)

// race 
generate race = " " 
replace race = "white" if RaceCaucasian == 1
replace race = "black" if RaceBlack == 1
replace race = "native american" if RaceNativeAm == 1
replace race = "native pacific" if RacNativePacific == 1
replace race = "other" if RaceOther == 1
replace race = "unknown" if race == " " 
tabulate race
// encode for logistic
encode race, generate(race_e)

// ethnicity
generate ethnic = " " 
replace ethnic = "+Hispanic/Latino" if Ethnicity == 1
replace ethnic = "-Hispanic/Latino" if Ethnicity == 2
replace ethnic = "unknown" if Ethnicity == 3 | ethnic == " " 
tabulate ethnic
// encode for logistic
encode ethnic, generate(ethnic_e)

// geographic region
tabulate region, miss
// encode for logistic
encode region, generate(region_e)

// AdmitSrc
tabulate AdmitSrc, miss
generate admit_source = " " 
replace admit_source = "elective" if AdmitSrc == 1
replace admit_source = "ER" if AdmitSrc == 2
replace admit_source = "transfer" if AdmitSrc == 3
replace admit_source = "other" if AdmitSrc == 4
replace admit_source = "unknown" if admit_source == " " 
tabulate admit_source, miss
// encode for logistic
encode admit_source, generate(admit_source_e)

// year
summarize surgyear, detail

// Demographic variables


///////////////////////////////////////////////////////////////////////////////

// Preexisting Disease & Preoperative Covariates

///////////////////////////////////////////////////////////////////////////////

// coded using [cond( )] command

// Diabetes
generate Diabetes_preexist = cond(Diabetes == 1, 1, 0)

// Dyslipidemia
generate Dyslip_preexist = cond(Dyslip == 1, 1, 0)

// Dialysis
generate Dialysis_preexist = cond(Dialysis == 1, 1, 0)

// Heart Failure
generate HeartFail_preexist = cond(HeartFail == 1, 1, 0)

// NYHA
generate ClassNYH_preexist = ///
	cond(ClassNYH == 1, "1", ///
	cond(ClassNYH == 2, "2", ///
	cond(ClassNYH == 3, "3", ///
	cond(ClassNYH == 4, "4", "none" ///
		))))
// encode string variable for logistic
encode ClassNYH_preexist, generate(ClassNYH_preexist_e)

// Hypertension
generate Hypertn_preexist = cond(Hypertn == 1, 1, 0)

// Infectious Endocarditis
generate InfEndo_preexist = cond(InfEndo == 1, 1, 0)

// Chronic Lung Disease
generate ChrLungD_preexist = ///
	cond(ChrLungD == 2, "mild", /// 
	cond(ChrLungD == 3, "moderate", ///
	cond(ChrLungD == 4, "severe", "none" ///
		)))
// encode string variable for logistic
encode ChrLungD_preexist, generate (ChrLungD_preexist_e)

// Home O2
generate HmO2_preexist = cond((HmO2 == 1 | HmO2 == 3 | HmO2 == 4), "+O2", "-O2")
// encode
encode HmO2_preexist, generate(HmO2_preexist_e)

// Sleep Apnea
generate SlpApn_preexist = cond(SlpApn == 1, 1, 0)

// Liver Disease
generate LiverDis_preexist = cond(LiverDis == 1, 1, 0)

// Cancer 
generate Cancer_preexist = cond(Cancer == 1, 1, 0)

// PVD
generate PVD_preexist = cond(PVD == 1, 1, 0)

// CVD
generate CVD_preexist = cond(CVD == 1, 1, 0)

// Previous Stroke
generate CVA_preexist = cond(CVA == 1, 1, 0)

// Ejection Fraction (preoperative)
summarize HDEF, detail 

// Creatinine (preoperative)
summarize CreatLst, detail

// Albumin (preoperative)
summarize TotAlbumin, detail

// MELD
summarize MELDScr, detail

///////////////////////////////////////////////////////////////////////////////

// surgical covariates

// previous CABG
generate PrCAB_prior = cond(PrCAB == 1, 1, 0)

// previous valve
generate PrValve_prior = cond(PrValve == 1, 1, 0)

// IABP
generate IABP_placed = ///
	cond(IABPWhen == 1, "Preop IABP", ///
	cond(IABPWhen == 2, "Intraop IABP", "-Pre/Intra IABP" ///
		))
// encode IABP
encode IABP_placed, generate(IABP_placed_e)

// perfusion time
summarize PerfusTm, detail

// ORDuration
summarize ORDuration, detail

// SIDuration
summarize SIDuration, detail

///////////////////////////////////////////////////////////////////////////////

// Clean outcome data 

///////////////////////////////////////////////////////////////////////////////

// code out 30-day mortality outcome
// Mt30Stat
generate Mt30Stat_outcome = cond(Mt30Stat == 2, 1, 0)
// sanity check
tabulate Mt30Stat_outcome, miss

//////

// code out CNStrokeP outcome
// CNStrokP
generate CNStrokP_outcome = cond((CNStrokP == 1 | CNStrokP == 3 | CNStrokP == 4 | CNStrokP == 5), 1, 0)
// sanity check
tabulate CNStrokP_outcome

//////

// stroke or mortality variable
generate CNStrokP_Mt30Stat_outcome = cond((Mt30Stat_outcome == 1 | CNStrokP_outcome == 1), 1, 0)
// sanity check
tabulate CNStrokP_Mt30Stat_outcome

//////

///////////////////////////////////////////////////////////////////////////////

// look at complication variable 
tabulate Complics surgyear, miss 
// look at COpReBld by year
tabulate COpReBld surgyear, miss
// look at COpReBldTim surgyear
tabulate COpReBldTim surgyear, miss
// look at COpReVlv surgyear
tabulate COpReVlv surgyear, miss
// look at COpReGft surgyear
tabulate COpReGft surgyear, miss

//////

// code out valve reoperation outcome 
// COpReVlv
generate COpReVlv_outcome = cond((COpReVlv == 1 | COpReVlv == 3 | COpReVlv == 4), 1, 0)
// sanity check
tabulate COpReVlv_outcome, miss
// sanity check 
tabulate COpReVlv_outcome surgyear, miss

//////

// code out CABG reoperation outcome
generate COpReGft_outcome = cond(((COpReGft == 1 | COpReGft == 3) | (CReintMIIntTy == 1 | CReintMIIntTy == 3)), 1, 0)
// sanity check
tabulate COpReGft_outcome, miss
tabulate COpReGft_outcome surgyear, miss

//////

// code out reoperation for bleed outcome
// COpReBld
generate COpReBld_outcome = cond(COpReBld == 1, 1, 0)
// sanity check
tabulate COpReBld_outcome, miss
tabulate COpReBld_outcome surgyear, miss

//////

// generate reoperation composite outcome
generate reoperation_complication = cond((Complics == 1 & (COpReBld_outcome == 1 | COpReGft_outcome == 1 | COpReVlv_outcome == 1)), 1, 0)
// sanity check
tabulate reoperation_complication, miss
tabulate reoperation_complication surgyear, miss

// generate reoperation / 30-day mortality outcome
generate reoperation_Mt30Stat_outcome = cond((Mt30Stat_outcome == 1 | reoperation_complication == 1), 1, 0)
// sanity check 
tabulate reoperation_Mt30Stat_outcome, miss


///////////////////////////////////////////////////////////////////////////////


// SurSInf
tabulate SurSInf surgyear, miss
// CSternal
tabulate CSternal surgyear, miss
// CSepsis
tabulate CSepsis surgyear, miss

//////

// code out surgical site infection
// SurSInf
generate SurSInf_outcome = cond(SurSInf == 1, 1, 0)
// sanity check
tabulate SurSInf_outcome surgyear, miss

// code out sternal complication 
// CSternal
generate CSternal_outcome = cond(CSternal == 1, 1, 0)
// sanity check
tabulate CSternal_outcome surgyear, miss

// code out sepsis complication 
generate CSepsis_outcome = cond(CSepsis == 1, 1, 0)
// sanity check
tabulate CSepsis_outcome surgyear, miss

//////

// code out infection composite outcome
generate infectious_outcome = cond((Complics == 1 & (SurSInf_outcome == 1 | CSternal_outcome == 1 | CSepsis == 1)), 1, 0)
// sanity check
tabulate infectious_outcome, miss
tabulate infectious_outcome surgyear, miss

// binary TEE exposure variable
generate TEE = " " 
replace TEE = "TEE" if InOpTEE_str == "yes"
replace TEE = "No TEE" if InOpTEE_str == "no" | InOpTEE_str == "-" | InOpTEE_str == " " 
// sanity check
tabulate TEE, miss
// code out for logistic
generate TEE_b = . 
replace TEE_b = 1 if InOpTEE_str == "yes"
replace TEE_b = 0 if InOpTEE_str != "yes" 
// sanity check 
tabulate TEE_b, miss 


///////////////////////////////////////////////////////////////////////////////

// Look at site | hospital | surgeon distributions

///////////////////////////////////////////////////////////////////////////////

// generate CT_n indicator variable
generate CT_n = _n

egen surgery_vol_Site_ID_yr = count(CT_n), by(SiteID surgyear)
summarize surgery_vol_Site_ID_yr, detail
histogram surgery_vol_Site_ID_yr

egen surgery_vol_Surg_ID_yr = count(CT_n), by(SurgID surgyear)
summarize surgery_vol_Surg_ID_yr, detail
histogram surgery_vol_Surg_ID_yr

egen surgery_vol_Hosp_ID_yr = count(CT_n), by(HospID surgyear)
summarize surgery_vol_Hosp_ID_yr, detail
histogram surgery_vol_Hosp_ID_yr

///////////////////////////////////////////////////////////////////////////////

egen TEE_probability_Site_ID_yr = mean(TEE_b), by(SiteID surgyear) 
summarize TEE_probability_Site_ID_yr, detail
histogram TEE_probability_Site_ID_yr

egen TEE_probability_Surg_ID_yr = mean(TEE_b), by(SurgID surgyear) 
summarize TEE_probability_Surg_ID_yr, detail
histogram TEE_probability_Surg_ID_yr

egen TEE_probability_Hosp_ID_yr = mean(TEE_b), by(HospID surgyear)
summarize TEE_probability_Hosp_ID_yr, detail
histogram TEE_probability_Hosp_ID_yr

///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////

log close _all

markdoc "`path'`file_name'", export(docx) replace install title("Echocardiography: Cohort Development,  Surgical Categorization, and Outcomes Finalization") 

///////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////

// export command lines commented out with /* /*

///////////////////////////////////////////////////////////////////////////////



// export & save as .csv file
// change directory
cd "<data file>"
// export .csv to the changed directory
export delimited Program_1_data_export_defined_cohort_01_24_2021.csv, replace 
// export & save as .dta file
save "<data file>", replace

/*


///////////////////////////////////////////////////////////////////////////////

