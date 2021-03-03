## Do File RDD Replication STATA 
# Set up directory, data and necessary packages 
cd "C:\Users\megan"
use "C:\Users\megan\Documents\UT\Spring 2021\Casual Inference\Replications\RDD Replication Stata\hansen_dwi.dta"
ssc install rd
ssc install cmogram
net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace

# Question 3: Creating the Dui dummy variable 
gen dui = 0
replace dui = 1 if bac1>=0.8 
sum dui

# Question 4: Replication of Figure 1 and McCrary Density Test 
hist bac1, frequency bin(4000) xline(0.08) xline(0.15)

rddensity bac1, c(0.08) 
# this produces a p-value of 0.8897 which is roughly what was produced in the paper and because of the high p-value we can confidently conclude that there is not self-selection into the treatement group
rddensity bac1, c(0.08) plot

# Question 5: Replication of Table 2 Panel A: Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predermined Charactersistics
rdrobust male bac1, c(0.08) h(0.05)
rdrobust white bac1, c(0.08) h(0.05)
rdrobust aged bac1, c(0.08) h(0.05)
rdrobust acc bac1, c(0.08) h(0.05)


# Question 6: Replication of Figure 2 Panel A-D: BAC and Charactersistics (linear and quadratic) 

#This is the disjointed panel for characteristics correct for the quadratic model
cmogram acc bac1, cut(0.08) scatter line(0.08) qfitci
cmogram male bac1, cut(0.08) scatter line (0.08) qfitci
cmogram age bac1, cut(0.08) scatter line(0.08) qfitci
cmogram white bac1, cut(0.08) scatter line(0.08) qfitci

rdplot male bac1, c(0.08) p(2)
rdplot white bac1, c(0.08) p(2)
rdplot aged bac1, c(0.08) p(2)
rdplot acc bac1, c(0.08) p(2)

#This is the disjointed panel for charactersistics correct for linear model
cmogram acc bac1, cut(0.08) scatter line(0.08) lfitci
cmogram male bac1, cut(0.08) scatter line (0.08) lfitci
cmogram age bac1, cut(0.08) scatter line(0.08) lfitci
cmogram white bac1, cut(0.08) scatter line(0.08) lfitci

rdplot male bac1, c(0.08) p(1)
rdplot white bac1, c(0.08) p(1)
rdplot aged bac1, c(0.08) p(1)
rdplot acc bac1, c(0.08) p(1)

#Question 7: Replicate Equation 1, and Replicate Table 3 (under specified conditions)
# Equation 1
regress recidivism bac1 male white acc aged dui lininteract 

gen bac1sq = bac1^2
gen lininteract = bac1*dui
gen quadinteract = bac1sq*dui

# Column 1 Panel A
reg recidivism bac1 if bac1 >= 0.03 & bac1 <= 0.13, robust 

rdrobust recidivism bac1, c(0.08) covs(male white acc lininteract aged dui) h(0.05)

# Column 1 Panel B
reg recidivism bac1 if bac1 >= 0.055 & bac1 <= 0.105, robust

rdrobust recidivism bac1, c(0.08) covs(male white acc lininteract aged dui) h(0.025)

# Column 2 Panel A
xi: reg recidivism i.bac1*cutoff if bac1 >= 0.03 & bac1 <= 0.13, robust
 
rdrobust recidivism interaction, c(0.08) covs(male white acc lininteract aged dui bac1) h(0.05)

# Column 2 Panel B
xi: reg recidivism i.bac1*cutoff if bac1 >= 0.055 & bac1 <= 0.105, robust

rdrobust recidivism bac1 lininteract, c(0.08) covs(male white acc interaction aged dui bac1) h(0.025)

# Column 3 Panel A
xi: reg score cutoff##c.(bac1 bac1sq) if bac1 >= 0.03 & bac1 <= 0.13, robust

rdrobust recidivism bac1 lininteract quadinteract, c(0.08) covs(male white acc lininteract aged dui) h(0.05)

# Column 3 Panel B
xi: reg score cutoff##c.(bac1 bac1sq) if bac1 >= 0.055 & bac1 <= 0.13, robust 

rdrobust recidivism bac1 lininteract quadinteract, c(0.08) covs(male white acc interaction aged dui) h(0.025)

# Question 8: Replicate Figure 3 Panel A with linear and quadratic fits 
cmogram recidivism bac1, cut(0.08) scatter line(0.08) qfitci
cmogram recidivism bac1, cut(0.08) scatter line(0.08) lfit

rdplot recidivism bac1 if bac1 < 0.15, c(0.08) p(1)
rdplot recidivism bac1 if bac1 < 0.15, c(0.08) p(2)

