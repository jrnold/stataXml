* dataset to test missing values
clear
set obs 28

local letters a b c d e f g h i j k l m n o p q r s t u v w x y z

foreach x in byte int long float double {
     gen `x' mi_`x' = .
     replace mi_`x' = 0 in 1
     forvalues i = 1/26 {
          local alpha : word `i' of `letters'
          di "`alpha'"
          replace mi_`x' = .`alpha' in `=`i'+2'
     }
}

save missings.dta, replace
saveold missings8.dta
xmlsave missings.xml, doctype(dta) dtd replace
