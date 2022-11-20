                                  ******************************************
								  ************** Problem Set 5 *************
								  ******************************************

								  
clear all
set more off


// path or address of the file in the system
global path "/Users/sudikshajoshi/Desktop/Fall 2022/ECON587 Microeconometrics/Problem Sets/Problem Set 5" 
global path_graph "$path/Graphs" // stores all graphs

use "$path/Ozier_JHR_Econ587.dta", clear


log using PS5Codes, text replace // Create a log file and replace the older one (if exists)



******************** Q1 *************************

/* (a) A treatment was assigned for all students with test score at least 0. Generate an indicator
variable for this treatment. Estimate a regression of secondary on test score, treatment,
and an interaction term between those two variables. Report the results.   */

gen treat = test >=0


// add c before a continuous variable to interact a continuous with a binary variable
reg secondary test treat c.test##treat 



/* (c) Re-do part (a) but restricting to the absolute value of the test score being less than each
of 0.8, 0.4, 0.2, and 0.1. Report the estimated treatment effects and standard errors for
each “bandwidth” and discuss how they change as the window gets smaller. */

gen abs_val = abs(test)

reg secondary test treat c.test##treat if abs_val < 0.8
est store ols8, title("OLS 0.8")

reg secondary test treat c.test##treat if abs_val < 0.4
est store ols4, title("OLS 0.4")

reg secondary test treat c.test##treat if abs_val < 0.2
est store ols2, title("OLS 0.2")

reg secondary test treat c.test##treat if abs_val < 0.1
est store ols1, title("OLS 0.1")

estout ols8 ols4 ols2 ols1, cells(b(star fmt(5)) se(par fmt(4)))   /// add standard errors and stars to indicate that the coefficients are statistically significant
legend label varlabels(_cons constant)               ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) // add R-squared, residual degrees of freedom and BIC, and adjust the number of decimal places to display for each item


/* (d) Use command rd with only arguments secondary and test (in that order). Report the
estimated treatment effect and contrast to part (c).     */

rd secondary test
est store rd1, title("RD 1")

/* (e) Use command rdplot with arguments secondary and test (in that order). Interpret the
resulting graph.     */

rdplot secondary test
graph save Graph "$path_graph/1e)rdplot.gph", replace

hist test, bin(100) addplot(pci 0 0 0.8 0)
graph save Graph "$path_graph/1e)hist.gph", replace

rddensity test
rdbwselect secondary test, bwselect(mserd) 
local halfwidth = s(h_mserd)/2

/* rdplot secondary test, graph_options(xline(-`halfwidth') xline(`halfwidth'))
graph save Graph "$path_graph/1e)rdplot_mserd.gph", replace */

/*(f) Use the command rd with arguments secondary and test (in that order). To override
the default kernel and bandwidth, use options “k(rect)” and “bw(0.8).” Compare your
estimated treatment effect to the one in part (c) that restricts estimation to those with
test score between -0.8 and 0.8. Are they the same? */

rd secondary test, k(rect) bw(0.8)
est store rd2, title("RD 2")

/* (g) Next, estimate by 2SLS the effect of secondary schooling on the combined reading and
vocabulary score. That is, your first stage should predict secondary as a function of
treatment, while the second stage estimates the effect of secondary on rv. Include test,
female, and an interaction between test and treatment on the RHS of both stages.
Compare your results to those presented in the published paper’s Table 3, Column 3.
(Note that they should not be numerically identical since we are not using covariates.)   */

global xlist test female c.test##treat


reg secondary treat $xlist //1st stage
est store first, title("1st stage")

reg rv secondary $xlist // 2nd stage
est store sec, title("2nd stage")

ivreg2 rv (secondary = treat $xlist), first
est store ivreg2, title("2SLS")

estout first sec ivreg2, cells(b(star fmt(5)) se(par fmt(4)))   /// add standard errors and stars to indicate that the coefficients are statistically significant
legend label varlabels(_cons constant)               ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) // add R-squared, residual degrees of freedom and BIC, and adjust the number of decimal places to display for each item

/* (h) Use the command rd, with default options, with arguments rv, secondary, and test (in
that order). Compare to your results from (g). */

rd rv secondary test
est store rd3, title("RD3")

estout rd1 rd2 rd3, cells(b(star fmt(5)) se(par fmt(4)))   /// add standard errors and stars to indicate that the coefficients are statistically significant
legend label varlabels(_cons constant)               ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) // add R-squared, residual degrees of freedom and BIC, and adjust the number of decimal places to display for each item

/* (i) Use the rd command to generate the same results that you found in part (g). What
options do you need to use? (Hint: you’ll need to override the defaults as in part (f).)    */

******************** Q2 *************************



use "$path/RD_Manip_Econ587.dta", clear


/* (a) First, look at the densities of reported wealth under all four scenarios. To do this,
estimate kernel densities (using the kdensity command) of the four measures of reported
wealth, using bandwidths of 0.1, 0.05, and 0.01. Describe what you see and relate to the
DGP for each of the four measures. (Hint: for a given bandwidth, you should graph the
four kernel densities simultaneously using the twoway language in Stata.)     */

twoway kdensity reportwealth1 ||kdensity reportwealth2 ||kdensity reportwealth3 ||kdensity reportwealth4, bwidth(0.1)
graph save Graph "$path_graph/2a)kdensity0.1.gph", replace

twoway kdensity reportwealth1 ||kdensity reportwealth2 ||kdensity reportwealth3 ||kdensity reportwealth4, bwidth(0.05)
graph save Graph "$path_graph/2a)kdensity0.05.gph", replace

twoway kdensity reportwealth1 ||kdensity reportwealth2 ||kdensity reportwealth3 ||kdensity reportwealth4, bwidth(0.01)
graph save Graph "$path_graph/2a)kdensity0.01.gph", replace


/* (b) Next, do a crude test of the “smoothness” of the density around the cutoff for each of the
four running variables. For each running variable, perform a series of tests of whether
the proportion of observations within v of 0 on either side of the cutoff is the same. Do
this for v = 0.1, 0.05, and 0.01. Report and discuss your results.   */


hist reportwealth1, bin(100) addplot(pci 0 0 0.8 0)
graph save Graph "$path_graph/2b)hist_rw1.gph", replace

hist reportwealth2, bin(100) addplot(pci 0 0 0.8 0)
graph save Graph "$path_graph/2b)hist_rw2.gph", replace

hist reportwealth3, bin(100) addplot(pci 0 0 0.8 0)
graph save Graph "$path_graph/2b)hist_rw3.gph", replace

hist reportwealth4, bin(100) addplot(pci 0 0 0.8 0)
graph save Graph "$path_graph/2b)hist_rw4.gph", replace

// more formal hypothesis test

rddensity reportwealth1, h(0.1)
rddensity reportwealth2, h(0.1)
rddensity reportwealth3, h(0.1)
rddensity reportwealth4, h(0.1)

rddensity reportwealth1, h(0.05)
rddensity reportwealth2, h(0.05)
rddensity reportwealth3, h(0.05)
rddensity reportwealth4, h(0.05)

rddensity reportwealth1, h(0.01)
rddensity reportwealth2, h(0.01)
rddensity reportwealth3, h(0.01)
rddensity reportwealth4, h(0.01)


/* (c) Now, let’s do a more sophisticated test. McCrary and Kovak have developed code to test
a density change at the cutoff. First, place the file DCdensity.ado in your .ado directory.
The code will generate an estimate of the (log) difference in density at the discontinuity
as well as a standard error, from which you can calculate a T-stat and p-value. Report
your p-values for each of the four measures,and briefly comment on these results. */ 

 
DCdensity reportwealth1, breakpoint(0) generate(Xj Yj r0 fhat se)
graph save Graph "$path_graph/2c)DCdense_rw1.gph", replace

DCdensity reportwealth2, breakpoint(0) generate(Xj2 Yj2 r02 fhat2 se2)
graph save Graph "$path_graph/2c)DCdense_rw2.gph", replace

DCdensity reportwealth3, breakpoint(0) generate(Xj3 Yj3 r03 fhat3 se3)
graph save Graph "$path_graph/2c)DCdense_rw3.gph", replace

DCdensity reportwealth4, breakpoint(0) generate(Xj4 Yj4 r04 fhat4 se4)
graph save Graph "$path_graph/2c)DCdense_rw4.gph", replace



/* (d) For each of the four scenarios, generate a treatment indicator that is equal to 1 if reported
wealth is below 0. For each scenario, simulate a variable (e.g., newwalth1 ) as the sum of
truewealth, 0.2 × treatment (e.g., the treatment effect = 0.2), and a random error with
mean 0, variance 0.01. Summarize new wealth for each scenario and comment on the
differences.     */

gen treat1 = reportwealth1 < 0 
gen treat2 = reportwealth2 < 0 
gen treat3 = reportwealth3 < 0 
gen treat4 = reportwealth4 < 0 

set obs 10000 // number of observations
gen error = rnormal(0,0.01) // draw 10000 obs randomly from the normal dist with mean and var

gen newwealth1 = truewealth + 0.2*treat1 + error
gen newwealth2 = truewealth + 0.2*treat2 + error
gen newwealth3 = truewealth + 0.2*treat3 + error
gen newwealth4 = truewealth + 0.2*treat4 + error

sum newwealth1-newwealth4



/*    (e) Use the regress command to conduct a regression discontinuity analysis of the effect of the
program in the 4 scenarios. Control for the running variable (reported wealth) separately
(and linearly) on each side of the discontinuity. Try using different “bandwidths” by
restricting your estimation to observations within a fixed distance from 0. */


reg newwealth1 treat1 reportwealth1
est store ols_rd1, title("OLS RD1")

reg newwealth2 treat2 reportwealth2
est store ols_rd2, title("OLS RD2")

reg newwealth3 treat3 reportwealth3
est store ols_rd3, title("OLS RD3")

reg newwealth4 treat4 reportwealth4
est store ols_rd4, title("OLS RD4")


estout ols_rd1 ols_rd2 ols_rd3 ols_rd4, cells(b(star fmt(5)) se(par fmt(4)))   /// add standard errors and stars to indicate that the coefficients are statistically significant
legend label varlabels(_cons constant)               ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) // add R-squared, residual degrees of freedom and BIC, and adjust the number of decimal places to display for each item




/*   (f) Re-do the analysis using the rdrobust command. What do you find? (Note: rdrobust
assumes that treatment is 1 above 0, so your estimates will have opposite signs.) */

rdrobust newwealth1 reportwealth1 treat1
est store rd_rob1, title("RD Robust 1")

rdrobust newwealth2 reportwealth2 treat2 
est store rd_rob2, title("RD Robust 2")

rdrobust newwealth3 reportwealth3 treat3 
est store rd_rob3, title("RD Robust 3")

rdrobust newwealth4 reportwealth4 treat4 
est store rd_rob4, title("RD Robust 4")


estout rd_rob1 rd_rob2 rd_rob3 rd_rob4, cells(b(star fmt(5)) se(par fmt(4)))   /// add standard errors and stars to indicate that the coefficients are statistically significant
legend label varlabels(_cons constant)               ///
stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC)) // add R-squared, residual degrees of freedom and BIC, and adjust the number of decimal places to display for each item



log close
