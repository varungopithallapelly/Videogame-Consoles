//--(2) Descriptive Analysis

//Corelation Matrix of variables
pwcorr period mhz ram price age active_n month year log_unit_sale log_games

//Summary Statistics
tabstat log_unit_sale period mhz ram price age active_n month year log_games , by(tech) s(n median variance mean) longstub column(s)

//Significance level across years
oneway log_unit_sale year


//--(3)Exploratory Analysis

// Summary Statistic graphs over years and technology generation
//bar graph for tech
graph hbar (mean) log_unit_sale (median) log_unit_sale (sd) log_unit_sale, over(tech) name(z)
graph hbar (mean) age (median) age (sd) age, over(tech) name(x)
graph hbar (mean) active_n (median) active_n (sd) active_n, over(tech) name(c)
graph hbar (mean) log_games (median) log_games (sd) log_games, over(tech) name(v)
graph combine z x c v


//Correlation graph matrix of the variables
graph matrix log_unit_sale  mhz ram price age active_n log_games, half title(Correlation Graph Matrix)


//Distribution of main variables
histogram log_unit_sale, normal name(q)
histogram log_games, normal name(w)
histogram age, normal name(e)
histogram active_n, normal name(r)
graph combine q w e r 

//Outliers
graph hbox period mhz ram age active_n log_unit_sale log_games price
graph hbox log_unit_sale log_games active_n
graph hbox age  price ram period
graph hbox mhz

//Relation b/w dependent & independent variables
twoway (scatter log_unit_sale log_games, mcolor(navy) msymbol(circle))  (lfit log_unit_sale log_games), name(t)
twoway (scatter log_unit_sale age, mcolor(gray) msymbol(triangle)) (lfit log_unit_sale age), name(y)
twoway (scatter log_unit_sale active_n, mcolor(eltblue) msymbol(circle))(lfit log_unit_sale active_n), name(u)
graph combine t y u

//Longitudinal trend of the main variables
twoway (lfit log_games period), ytitle(log_games) name(h)
twoway (lfit age period), ytitle(age) name(j)
twoway (lfit active_n period), ytitle(active_n) name(k)
twoway (lfit log_unit_sale period), ytitle(log_unit_sale) name(l)
graph combine h j k l

//Console's market share across period 
//Console market share (neu)
bysort period console: egen consoles_unit_sale= sum(log_unit_sale)

//console market share(den)
bysort period: egen sum_consoles_sale= sum(log_unit_sale)

//generate market share variable
generate Market_share = consoles_unit_sale /sum_consoles_sale

//Console market share trend -- Market share vs period 
twoway (line Market_share period if console == "A") (line Market_share period if console == "B") (line Market_share period if console == "C") (line Market_share period if console == "D") (line Market_share period if console =="E") (line Market_share period if console =="F") (line Market_share period if console =="G") (line Market_share period if console =="H") (line Market_share period if console =="I") (line Market_share period if console =="J") (line Market_share period if console =="K") (line Market_share period if console =="L") (line Market_share period if console =="M") (line Market_share period if console =="N"), ytitle(Market share) xtitle(years) title(Console's market share trend)


//--(4)Main Regression

// Generate log of variables
gen log_price = ln(price) 
gen log_ram = ln(ram)
gen log_mhz = ln(mhz)

//As we convert above variables to log form, the outliers are automatically removed in those variables

//OLS regression baseline model 
//Create dummy variable for "tech" as it's a categorical variable
encode tech, generate(tech_dum)

eststo clear
reg log_unit_sale log_games age active_n log_price log_ram log_mhz period i.tech_dum
eststo BL1


//model2-model3

reg log_unit_sale log_games age active_n log_price log_ram log_mhz period i.tech_dum if leader=="NO"
eststo NL2

reg log_unit_sale log_games age active_n log_price log_ram log_mhz period i.tech_dum if leader=="YES"
eststo L3

esttab BL1 NL2 L3 using Main4_reg.rtf, replace ar2(3) b(3) se(3) r2(3) label compress title (Main Regression) mtitles("Baseline model" "Model2" "Model3")

//Create dummy variable for "tech" as it's a categorical variable
encode tech, generate(tech_dum)

//Modified baseline model with different technology generation
reg log_unit_sale log_games age active_n log_price log_ram log_mhz year period i.tech_dum

//tech dummy main effect on log_unit_sales with margin plot graph
reg log_unit_sale log_games age active_n log_price log_ram log_mhz year period i.tech_dum
margins tech_dum 
marginsplot

//Modified basline model for Leader vs Non Leader consoles
reg log_unit_sale log_games age active_n log_price log_ram log_mhz year period i.tech_dum if leader=="NO"
reg log_unit_sale log_games age active_n log_price log_ram log_mhz year period i.tech_dum if leader=="YES"

//Create dummy variable for "leader" as it's a categorical variable
encode leader, generate(leader_dum)

//leader dummy main effect on log_unit_sales with margin plot graph
reg log_unit_sale log_games age active_n log_price log_ram log_mhz year i.tech_dum i.leader_dum
margins leader_dum 
marginsplot

//leader effect on log_games with margin plot graph
encode leader, generate(leader_dum) //Create dummy variable for "leader" 
reg log_unit_sale log_games age active_n log_price log_ram log_mhz period i.tech_dum i.leader_dum //model with leader_dum variable
margins leader_dum, at(log_games=( 0  (2) 16))
marginsplot


//--(5)Diagnostics and Robustness Analysis

//diagnostic analysis on baseline model to check potential of heteroskedasticity
estat hettest  
estat imtest, white
//residuals vs fitted value
rvfplot, yline(0)

//baseline ols regression model 
eststo clear
reg log_unit_sale log_games age active_n log_price log_ram log_mhz i.tech_dum period
eststo md1
//quadtraic effect on number of games 
generate games_sq=games*games

reg log_unit_sale log_games games_sq age active_n log_price log_ram log_mhz i.tech_dum period
eststo md2

twoway (scatter log_unit_sale games) (lowess log_unit_sale log_games)
twoway (qfit log_unit_sale log_games)

//robust regression model
reg log_unit_sale log_games age active_n log_price log_ram log_mhz i.tech_dum period,vce(robust)
eststo md3

//fixed effect
egen console_id=group(console)
xtset console_id period
xtline y
xtreg log_unit_sale log_games age active_n log_price log_ram log_mhz i.tech_dum period,fe
eststo md4

//table comparision
esttab md1 md2 md3 md4 using Main_reg.rtf, replace ar2(3) b(3) se(3) r2(3) label compress title(Model comparision)mtitles("Baseline model" "quadratic" "robust" "fixed")

//Exporting tables to Excel
ssc install estout, replace
cd "C:\Users\vt00196\OneDrive - University of Surrey\Desktop\stata"
set more off
eststo clear
estpost corr log_unit_sale mhz ram price age active_n log_games, matrix listwise
eststo corrl
esttab corrl using correl_table.csv, replace label unstack compress

ssc install estout, replace
cd "C:\Users\vt00196\OneDrive - University of Surrey\Desktop\stata"
set more off
eststo clear
estpost oneway log_unit_sale year
eststo onew
esttab onew using one_way_table.csv, replace label unstack compress



