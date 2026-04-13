***Import Table and Define Time Variable which contains irregular gaps***
import excel "LOCATION OF THE EXCEL FILE IN WHICH THE EUCLIDEAN DATA FILE IS SAVED.XLS", sheet("Euclidean Data") firstrow
tsset Year
sort Year

***Finding Pairwise/Yearwise Euclidean Distance for Sectoral Shares*** 
tsset year
ds year, not               
local allvars `r(varlist)'

***Taking difference variables with unqual gaps*** 

foreach v of local allvars {
    gen d_`v' = `v' - `v'[_n-1]
    label variable d_`v' 
    format d_`v' %9.2f      
}
ds year, not
local allvars `r(varlist)'
foreach v of local allvars {
    gen d_`v'_sq = (d_`v')^2
    label variable d_`v'_sq "`v'
} 
***Prepaing Year-wise sum of Distance Squared***
bysort year: gen sqe= d_agriallied_sq+d_mining_sq+d_manuf_sq+d_utilities_sq+ d_construction_sq+d_wrtcwholesaleretailtrade_sq+d_tstctransportcommunication_sq+ d_banking_sq+d_realestatefinancenonbank_sq+d_publicadministration_sq+d_otherservices_sq

***Euclidean Distance: Square Root of SQE***
bysort year: gen ECD_period=sqrt(sqe)

***Finding Maximum Euclidean Distance// Results Table 1***
table year, c(max ECD_period)

***Robustness Check 1: Structural Change with Similar (Different) Direction/ Structural Change with/without Transformation***
ds Year, not
local allvars `r(varlist)'

****Generating dot products and norm_current and norm previous (squared***
gen double dot_product   = 0
gen double norm_curr_sq  = 0
gen double norm_prev_sq  = 0

foreach v of local allvars {
    replace dot_product  = dot_product  + `v' * `v'[_n-1]
    replace norm_curr_sq = norm_curr_sq + `v'^2
    replace norm_prev_sq = norm_prev_sq + `v'[_n-1]^2
}
***generating denominator of Cosine Similarity***
gen double norm_current   = sqrt(norm_curr_sq)
gen double norm_previous  = sqrt(norm_prev_sq)
***generating cosine similarity***
gen double cosine_similarity = dot_product / (norm_current * norm_previous)
***generating cosine distance***
gen double cosine_distance   = 1 - cosine_similarity

***Result***
table Year, c( max cosine_distance max cosine_similarity)
