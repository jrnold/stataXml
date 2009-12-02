* dataset to test missing values handling
clear
set obs 27

local letters a b c d e f g h i j k l m n o p q r s t u v w x y z

foreach x in byte int long float double {
     gen `x' mi_`x' = . 
     forvalues i = 1/26 {
          local alpha : word `i' of `letters'
          di "`alpha'"
          replace mi_`x' = .`alpha' in `=`i'+1'	
     } 
}
