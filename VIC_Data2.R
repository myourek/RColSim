# week_counter= time steps of model does NOT set to zero each year
# The Unit is AF/week
# reading the input file

VIC_Data <- function() {

####### supply, demands, curtailment ########
terms = c("PriVIC", "DemVIC", "Iflow", "CurtVIC", "Vol")
for (j in terms) {
	cols = grep(j, names(input_file))
	for (i in cols) {
		var = names(input_file)[i]
		assign(var, pos=1, input_file[week_counter,names(input_file)==var])
	}
}

####### other inputs ##########
BRRefillCurve <<- input_file$BRRefillCurve[week_counter]
BRRunoffAprJuly <<- input_file$BRRunoffAprJuly[year_counter]
DallesJanJul <<- input_file$DallesJanJul[year_counter]
DallesRunoffAprAug <<- input_file$DallesRunoffAprAug[year_counter]
DURunoffAprAug <<- input_file$DURunoffAprAug[year_counter]
DWRunoffAprJuly <<- input_file$DWRunoffAprJuly[year_counter]
GCRunoffJanApr <<- input_file$GCRunoffJanApr[year_counter]
HHInQ_AprAug <<- input_file$HHInQAprAug[year_counter]
HHSumMaySept <<- input_file$HHSumMaySept[year_counter]
LBSumAprAug <<- input_file$LBSumAprAug[year_counter]
MISumRunoffAprilAug <<- input_file$MISumRunoffAprilAug[year_counter]
MISumRunoffMayAug <<- input_file$MISumRunoffMayAug[year_counter]
}
