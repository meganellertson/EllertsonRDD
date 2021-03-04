## Do File RDD Replication STATA 
# Set up directory, data and necessary packages 
cd "C:\Users\megan"
use "C:\Users\megan\Documents\UT\Spring 2021\Casual Inference\Replications\RDD Replication Stata\hansen_dwi.dta"
ssc install rd
ssc install cmogram
net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
ssc install estout, replace

**Question 3: Creating the Dui dummy variable 
gen dui = 0
replace dui = 1 if bac1>=0.8 
sum dui

**Question 4: Replication of Figure 1 and McCrary Density Test  name will be Figure1
hist bac1, frequency bin(4000) xline(0.08) xline(0.15) title("Freqency Distribution")


rddensity bac1, c(0.08) 
**this produces a p-value of 0.8897 which is roughly what was produced in the paper and because of the high p-value we can confidently conclude that there is not self-selection into the treatement group
**This is called Figrue2
rddensity bac1, c(0.08) plot

**Question 5: Replication of Table 2 Panel A: Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predermined Charactersistics 
rdrobust male bac1, c(0.08) h(0.05) kernel("uniform")
estimates store Male, title(Male)
rdrobust white bac1, c(0.08) h(0.05) kernel("uniform")
estimates store White, title(White)
rdrobust aged bac1, c(0.08) h(0.05) kernel("uniform")
estimates store Age, title(Age)
rdrobust acc bac1, c(0.08) h(0.05) kernel("uniform")
estimates store Accident, title(Accident)

estout Male White Age Accident, cells(b(star fmt(4)) se(par fmt(4))) stats(N, labels(N)) label legend varlabels(RD_Estimate "Panel A DUI Threshold")


**Question 6: Replication of Figure 2 Panel A-D: BAC and Charactersistics (linear and quadratic) 

cmogram acc bac1, cut(0.08) scatter line(0.08) lfitci title("Panel A Accident Linear") 
**Figure3.gph
cmogram male bac1, cut(0.08) scatter line (0.08) lfitci title("Panel B Male Linear")
**Figure4.gph
cmogram aged bac1, cut(0.08) scatter line(0.08) lfitci title("Panel C Age Linear")
**Figure5.gph
cmogram white bac1, cut(0.08) scatter line(0.08) lfitci title("Panel D White Linear")
**Figure6.gph

graph combine Figure3 Figure4 Figure5 Figure6, replace 
**linear.gph

rdplot male bac1, c(0.08) p(1) graph_options(title("Panel B Male Linear"))
**Figure7.gph
rdplot white bac1, c(0.08) p(1) graph_options(title("Panel D White Linear"))
**Figure8.gph
rdplot aged bac1, c(0.08) p(1) graph_options(title("Panel C Age Linear"))
**Figure9.gph
rdplot acc bac1, c(0.08) p(1) graph_options(title("Panel A Accident Linear"))
**Figure10.gph

graph combine Figure7 Figure8 Figure9 Figure10, replace 
**linear2.gph

cmogram acc bac1, cut(0.08) scatter line(0.08) qfitci title("Panel A Accident Quadratic")
**Figure11.gph
cmogram male bac1, cut(0.08) scatter line (0.08) qfitci title("Panel B Male Quadratic")
**Figure12.gph
cmogram aged bac1, cut(0.08) scatter line(0.08) qfitci title("Panel C Age Quadratic")
**Figure13.gph
cmogram white bac1, cut(0.08) scatter line(0.08) qfitci title("Panel D White Quadratic")
**Figure14.gph

graph combine Figure11 Figure12 Figure13 Figure14 
**quadratic.gph

rdplot male bac1, c(0.08) p(2) graph_options(title("Panel B Male Quadratic"))
**Figure15.gph
rdplot white bac1, c(0.08) p(2) graph_options(title("Panel D White Quadratic"))
**Figure16.gph
rdplot aged bac1, c(0.08) p(2) graph_options(title("Panel C Age Quadratic"))
**Figure17.gph
rdplot acc bac1, c(0.08) p(2)  graph_options(title("Panel A Accident Quadratic"))
**Figure18.gph

graph combine Figure15 Figure16 Figure17 Figure18 
**quadratic2.gph 


**Question 7: Replicate Equation 1, and Replicate Table 3 (under specified conditions)
**Equation 1
regress recidivism bac1 male white acc aged dui lininteract 

gen bac1sq = bac1^2
gen lininteract = bac1*dui
gen quadinteract = bac1sq*dui

# Column 1 Panel A
rdrobust recidivism bac1, c(0.08) covs(male white acc lininteract aged dui) h(0.05) kernel("uniform")

# Column 1 Panel B
rdrobust recidivism bac1, c(0.08) covs(male white acc lininteract aged dui) h(0.025) kernel("uniform")

# Column 2 Panel A
rdrobust recidivism bac1 lininteract, c(0.08) covs(male white acc aged dui) h(0.05) kernel("uniform")

# Column 2 Panel B
rdrobust recidivism bac1 lininteract, c(0.08) covs(male white acc interaction aged dui bac1) h(0.025) kernel("uniform")

# Column 3 Panel A
rdrobust recidivism bac1 lininteract quadinteract, c(0.08) covs(male white acc lininteract aged dui) h(0.05) kernel("uniform")

# Column 3 Panel B
rdrobust recidivism bac1 lininteract quadinteract, c(0.08) covs(male white acc interaction aged dui) h(0.025) kernel("uniform")

**Question 8: Replicate Figure 3 Panel A with linear and quadratic fits 
cmogram recidivism bac1, cut(0.08) scatter line(0.08) qfitci title("BAC and Recidivism Quadratic")
**Figure19.gph
cmogram recidivism bac1, cut(0.08) scatter line(0.08) lfitci title("BAC and Recidivism Linear")
**Figure20.gph

graph combine Figure19 Figure20 
**Basic.gph

rdplot recidivism bac1 if bac1 < 0.15, c(0.08) p(1) ci(95) graph_options(title("BAC and Recidivism Linear"))
**Figure21.gph
rdplot recidivism bac1 if bac1 < 0.15, c(0.08) p(2) ci(95) graph_option(title("BAC and Recidivism Quadratic"))
**Figure22.gph

graph combine Figure21 Figure22
**Basic2.gph

