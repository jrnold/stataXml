/* Generate debugging stata dataset */
clear
set obs 5
gen byte vbyte = _n
gen int vint = _n
gen long vlong = _n
gen float vfloat = _n
gen double vdouble = _n

gen str1 vstr = ""
local letters a b c d e
forvalues i = 1/5 {
    replace vstr = "`: word `i' of `letters''" in `i'
}

gen byte vlabel = _n
label define alpha 1 "a" 2 "b" 3 "c" 4 "d" 5 "e"
label value vlabel alpha

sort vbyte vint

label data "test data label"

label var vbyte "byte variable"
label var vdouble "double variable"
label var vlabel "variable with labels"
label var vstr "string variable"
label var vfloat "float variable"
label var vint "int variable"
label var vlong "long variable"

note _dta : 1st data note
note vbyte : 1st vbyte note

// compact xml
xmlsave "foo.xml", doctype(dta) dtd replace
// verbose xml
xmlsave "bar.xml", doctype(dta) dtd legible replace
// binary dta 114
save "foo.dta", replace
// binary dta 113
saveold "bar.dta", replace
