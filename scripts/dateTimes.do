/*
* Generate a dataset of test data-time variables
*/

clear
set obs 3

foreach x in c C d w m q h g {
    gen int t`x' = _n - 2
    format t`x' %t`x'
}

save dateTimes.dta, replace
saveold dateTimes8.dta, replace
xmlsave dateTimes.xml, doctype(dta) dtd replace
