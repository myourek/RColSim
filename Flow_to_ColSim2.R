
get_iflow_mainstem = function(station) {
	ifile = read.table(paste('~/Bias_correction/flow_weekly_bc/', station, '_iflow_rules', sep=""))
	Months = timeseries$Month
	Days = timeseries$Day
	instream = as.numeric(matrix(nrow=length(Days), ncol=1, 0))
	for (line in 1:length(ifile[,1])) {
		mo = ifile[line,1]
		first_day = ifile[line,2]
		last_day = ifile[line,3]
		instream[Months==mo & Days>=first_day & Days<=last_day] = ifile[line,4] * 13.8838 # converts from cfs to AF/wk
	}
	return(instream)
}

options(warn=-1)
simulate_demand = 1


gcm_list = read.table("~/Bias_correction/bc_Columbia/bias_correct_0.125_Natflow/scenario_list", stringsAsFactors=F)[-1,1]
scr_list = c("hist", "rcp45", "rcp85")
list_scenarios = vector(length=3*length(gcm_list)+1)
for (i in 1:length(scr_list)) {
	j = (i - 1) * length(gcm_list) + 2
	list_scenarios[j:(i*length(gcm_list)+1)] = sapply(gcm_list, function(x) {paste(x, scr_list[i], sep="/")})
}
list_scenarios[1] = "Historical_baseline/baseline"
list_scenarios = c("Historical_baseline/baseline", "bcc-csm1-1/rcp45")
# month start from August, therefore month 1 is August, 2 September, and so on
for (scr_no in 1:length(list_scenarios)) {
	gcm = strsplit(list_scenarios[scr_no], "/")[[1]][1]
	scr = strsplit(list_scenarios[scr_no], "/")[[1]][2]
	WD_Step_4 = "~/Step_4/"
	ListOfDams = c("ARROW", "BROWN", "DALLE", "DUNCA", "DWORS", "GCOUL", "FLASF", "LIBBY", "MICAA")
	Supply_Input = read.table(paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", gcm, "/supply_", scr, ".txt", sep=""), header=T)
	N = length(Supply_Input[,1])
	if (simulate_demand == 1) {
		Demand_Input = read.table(paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", gcm, "/demand_", scr, ".txt", sep=""), header=T)
	} else {
		Demand_Input = data.frame(matrix(nrow=N, ncol=26, 0))
	}
	if (simulate_demand == 1) {
		supply_for_return_flow = data.frame(matrix(nrow=N, ncol=7, 0))
		supply_for_return_flow[,1:4] = Supply_Input[,1:4]
		supply_for_return_flow_locs = c("WANAP", "PRIRA", "MCNAR")
		for (i in 1:3) {
			col = which(names(Supply_Input) == supply_for_return_flow_locs[i])
			supply_for_return_flow[,i+4] = Supply_Input[,col]
		}
		names(supply_for_return_flow) = c("Week", "Month", "Day", "Year", supply_for_return_flow_locs)
		return_fractions = read.csv("~/Step_4/return_flow_fractions.csv", header=F) # these fractions were derived from the document, "Calculation of 2020 Irrigation Depletions for
		# 2020 Level Modified Streamsflows, Hills et al., prepared for BPA, 2020.
	} else {
		return_fractions = data.frame(matrix(nrow=N, ncol=3, 0))
		supply_for_return_flow = data.frame(matrix(nrow=N, ncol=7, 0))
	}
	if (simulate_demand == 1) {
		mainstem_interruptible = read.table(paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", gcm, "/interruptible_", scr, ".txt", sep=""), header=T)
	} else {
		mainstem_interruptible = data.frame(matrix(nrow=N, ncol=13, 0))
	}
	timeseries = Supply_Input[,2:4]
	Number_of_ts = length(Supply_Input[,1])
	Number_of_Years = length(unique(Supply_Input[,4])) - 1
	Forecast_coef = 0.2
	
	########################## data frames
	names_Output = c("Week_Number", "month", "day", "year", "ARRefillVol1", "ARRefillVol2", "BRRunoffAprJuly", "BRRefillCurve", "DallesJanJul", "DallesRunoffAprAug", "DallesRunoffAprSep", "DARunoffJanApr",
	"DURefillVol",	"DURefillVol1", "DURunoffAprAug", "DWRefillVol1", "DWRefillVol2", "DWRunoffAprJuly", "DWSumRunoffMarJun", "GCRefillVol1", "GCRefillVol2", "GCRunoffJanApr", "HHInQAprAug", "HHRefillVol", "HHRefillVol1", "HHSumAprAug", 	
	"HHSumAprJul", "HHSumMaySept", "LBRefillVol2", "LBRefillVol1", "LBSumAprAug", "LBSumMayAug", "MIRefillVol1", "MIRefillVol2", "MISumRunoffAprilAug",	"MISumRunoffMayAug", "PriVICMI", "PriVICRev", "PriVICAR", "PriVICDU", "PriVICLB",
	"PriVICBONF", "PriVICCL", "PriVICCOL", "PriVICHH", "PriVICKE", "PriVICNOX", "PriVICCB", "PriVICAF", "PriVICBD", "PriVICGC", "PriVICCJ", "PriVICWE", "PriVICRR", "PriVICRI", "PriVICWA", "PriVICPR", "PriVICPAL", "PriVICMIL", "PriVICBR", "PriVICOX",
	"PriVICHC", "PriVICDW", "PriVICLG", "PriVICLiG", "PriVICLM", "PriVICIH", "PriVICMCN", "PriVICJD", "PriVICDA", "PriVICBON", "DemVICBON", "DemVICLG", "DemVICDA", "DemVICJD", "DemVICMCN", "DemVICPR", "DemVICWA", "DemVICRI", "DemVICRR",
	"DemVICWE", "DemVICCJ", "DemVICGC", "DemVICMIL", "DemVICLB", "DemVICIH", "DemVICKE", "DemVICDW", "DemVICCL", "DemVICCOL", "DemVICBR", "DemVICBONF", "DemVICAF", "RetVICWA", "RetVICPR", "RetVICMCN", "IflowCJ", "IflowDA", "IflowJD", "IflowMCN", "IflowPR", 
	"IflowRR", "IflowRI", "IflowWA", "IflowWE", "CurtVICCJ", "CurtVICDA", "CurtVICJD", "CurtVICMCN", "CurtVICPR", "CurtVICRR", "CurtVICRI", "CurtVICWA", "CurtVICWE")	
	
	Output_to_ColSim = data.frame(matrix(ncol=length(names_Output), nrow=Number_of_ts, 0))
	names(Output_to_ColSim) = names_Output
	Output_to_ColSim[,1:4] = Supply_Input[,1:4]
	
	########## cfs to acre-feet Per week
	cfsTOafw_coef = 13.8838
	Supply_Input[,5:length(Supply_Input[1,])] = Supply_Input[,5:length(Supply_Input[1,])] * cfsTOafw_coef
	Demand_Input[,5:length(Demand_Input[1,])] = Demand_Input[,5:length(Demand_Input[1,])] * cfsTOafw_coef
	supply_for_return_flow[,5:length(supply_for_return_flow[1,])] = supply_for_return_flow[,5:length(supply_for_return_flow[1,])] * cfsTOafw_coef
	mainstem_interruptible[,5:length(mainstem_interruptible[1,])] = mainstem_interruptible[,5:length(mainstem_interruptible[1,])] * cfsTOafw_coef
	
	dam_column = match(ListOfDams, names(Supply_Input))
	
	########## Assumed Release
	AssumedRelease = read.table(paste(WD_Step_4, "AssumedRelease.txt", sep=""), header=T) # monthly
	weekly_assumedRelease = data.frame(matrix(ncol=length(dam_column)+3, nrow=Number_of_ts, 0))
	names(weekly_assumedRelease) = c("Week_Number", "month", "year", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
	weekly_assumedRelease[1:Number_of_ts,1:3] = Output_to_ColSim[1:Number_of_ts,1:3]
	for (i_dam in 1:length(ListOfDams)){
		for (i_m in 1:12){
			weekly_assumedRelease[which(i_m==weekly_assumedRelease[,2]),3+i_dam] = AssumedRelease[which(AssumedRelease[,1]==i_m),1+i_dam]
		}
	}
	weekly_assumedRelease[,4:12] = weekly_assumedRelease[,4:12] / 0.50417 * 7
############################# Assured Refill
	# 1- monthly to weekly (52 weeks in a year)
	# this part converts the weekly timeseries to monthly time series

	AssuredRefill = read.table("~/Step_4/AssuredRefill.txt", header=T)
	ncols = length(AssuredRefill[1,]) + 2
	weekly_AssuredRefill = data.frame(matrix(ncol=ncols, nrow=52, 0))
	names(weekly_AssuredRefill) = c("Month", "CalendarMonth", "WeekInMonth", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
	MonthToWeek = read.table("~/Step_4/MonthToWeek.txt", header=T)
	weekly_AssuredRefill[,1:3] = MonthToWeek[,2:4]
	for (i_ts_week in 1:52){
		N_month = MonthToWeek[i_ts_week,2] # month start from August, therefore month 1 is August, 2 September, and so on
		if(N_month<2){
			weekly_AssuredRefill[i_ts_week,4:ncols] = AssuredRefill[12,2:10] - (AssuredRefill[12,2:10] - AssuredRefill[1,2:10]) / MonthToWeek[i_ts_week,5] * MonthToWeek[i_ts_week,4]
		} else {
			weekly_AssuredRefill[i_ts_week,4:ncols] = AssuredRefill[(N_month-1),2:10] + ((AssuredRefill[N_month,2:10] - AssuredRefill[(N_month-1),2:10]) / MonthToWeek[i_ts_week,5])*MonthToWeek[i_ts_week,4]
		}
	} #check
	write.table(weekly_AssuredRefill, "~/Step_4/test_weekly_assured.txt")
	
	# 2- Weekly to each time step
	# this part applies the weekly assured refill curves (52 week/year) to all of the years
	
	AssuredRefill_EntireTime = data.frame(matrix(ncol=ncols, nrow=Number_of_ts, 0))
	names(AssuredRefill_EntireTime) = c("WeekInYear", "Month","Number_of_Weeks", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
	AssuredRefill_EntireTime[,1:2] = Output_to_ColSim[1:Number_of_ts,1:2]
	august_firsts = which(AssuredRefill_EntireTime[,2]==8 & AssuredRefill_EntireTime[,1]==1)
	for(i_year in 1:(Number_of_Years)){ 
		ind1 = august_firsts[i_year]
		if(i_year==Number_of_Years) {
			ind2 = Number_of_ts
		} else {
			ind2 = august_firsts[i_year + 1] - 1
		}
		L = length(AssuredRefill_EntireTime[ind1:ind2,1])
		if (L==52) {
			AssuredRefill_EntireTime[ind1:ind2,3:ncols] = weekly_AssuredRefill[,3:ncols]
		} else {
			AssuredRefill_EntireTime[ind1:(ind2-1),3:ncols] = weekly_AssuredRefill[,3:ncols]
			AssuredRefill_EntireTime[ind2-1,3:ncols] = weekly_AssuredRefill[52,3:ncols]
		}
	}
	
	# assured refil code works fine
	####################################### Refill Alternative
	# For some of the dams this is RefillVol0 for some of them this is RefillVol2
	DamMaxMin = read.table("~/Step_4/DamMaxMin.txt", header=T)
	Refill_Alternative = data.frame(matrix(ncol=ncols , nrow=Number_of_ts, 0))
	names(Refill_Alternative) = c("WeekInYear", "Month", "Number_of_Weeks","Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
	Refill_Alternative[,1:3] = AssuredRefill_EntireTime[,1:3]
	July31s = which(AssuredRefill_EntireTime[,2]==7 & AssuredRefill_EntireTime[,1]==52)
	Refill_Alternative[July31s,4:ncols] = DamMaxMin[1,1:9]
	for(i_week1 in 1:51){
		for (i_dam in 1:9){
			refill_alt = Refill_Alternative[July31s-i_week1+1,3+i_dam] - (Supply_Input[July31s-i_week1+1,dam_column[i_dam]] - weekly_assumedRelease[July31s-i_week1+1,3+i_dam])   
			refill_alt[refill_alt<DamMaxMin[2,i_dam]] = DamMaxMin[2,i_dam]
			refill_alt[refill_alt>DamMaxMin[1,i_dam]] = DamMaxMin[1,i_dam]
			Refill_Alternative[July31s-i_week1,3+i_dam] = refill_alt
		} 
    }
	#write.table(Refill_Alternative, "~/Step_4/test_weekly_assured.txt")
	#######################################  
	# this is the RefillVol1 parameter in the excel sheet

	RefillStatusQuo = data.frame(matrix(ncol=ncols, nrow=Number_of_ts, 0))
	names(RefillStatusQuo) = c("WeekInYear", "Month", "Number_of_Weeks", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
	RefillStatusQuo[,1:3] = AssuredRefill_EntireTime[,1:3]
	RefillStatusQuo[RefillStatusQuo[,2]>=8,4:ncols] = AssuredRefill_EntireTime[RefillStatusQuo[,2]>=8,4:ncols] ### August to December
	RefillStatusQuo[RefillStatusQuo[,2]<8,4:ncols] = Refill_Alternative[RefillStatusQuo[,2]<8,4:ncols] ### January to July
	write.table(RefillStatusQuo, "~/Step_4/test_weekly_assured.txt")

	##################################################### 
	###
	###
	###               Generating the RColSim input file
	###
	###
	##################################################### 

	########################### ARRefillVol1  Column 1
	Output_to_ColSim$ARRefillVol1 = RefillStatusQuo[,4]
	
	########################### ARRefillVol2  Column 2
	Output_to_ColSim$ARRefillVol2 = Refill_Alternative[,4]
	
	########################### BRRunoffAprJuly # Column 3
	tmp = aggregate(Supply_Input$BROWN, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<8), sum)
	BRRunoffAprJuly = tmp[apply(tmp[,2:3],1,sum)==2,4]	# The condition, apply(tmp[,2:3],1,sum)==2, selects only the sums for which 3 < month < 8. 
	Output_to_ColSim$BRRunoffAprJuly[1:length(BRRunoffAprJuly)] = BRRunoffAprJuly 
	
	##########  BRRefillCurve   # Column 4
	Output_to_ColSim$BRRefillCurve = RefillStatusQuo[,5]
	
	######### DallesJanJul  # Column 5
	tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month<8), sum)
	DallesJanJul = tmp[tmp[,2]==1,3]
	Output_to_ColSim$DallesJanJul[1:length(DallesJanJul)] = DallesJanJul 
	
	#########  DallesRunoffAprAug  # Column 6
	tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum) 
	DallesRunoffAprAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$DallesRunoffAprAug[1:length(DallesRunoffAprAug)] = DallesRunoffAprAug 
	
	######### DallesRunoffAprSep  # Column 7
	tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<10), sum)
	DallesRunoffAprSep = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$DallesRunoffAprSep[1:length(DallesRunoffAprSep)] = DallesRunoffAprSep 
	
	######### DARunoffJanApr  # Column 8
	tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month<5), sum)
	DARunoffJanApr = tmp[tmp[,2]==1,3]
	Output_to_ColSim$DARunoffJanApr[1:length(DARunoffJanApr)] = DARunoffJanApr 
	########################### DURefillVol # Column 9
	Output_to_ColSim$DURefillVol = Refill_Alternative[,7]
	
	########################### DURefillVol1  # Column 10
	Output_to_ColSim$DURefillVol1 = RefillStatusQuo[,7]
	
	########################### DURunoffAprAug   # Column 11
	tmp = aggregate(Supply_Input$DUNCA, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum)
	DURunoffAprAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$DURunoffAprAug[1:length(DURunoffAprAug)] = DURunoffAprAug

	########################### DWRefillVol1  # Column 12
	Output_to_ColSim$DWRefillVol1 = RefillStatusQuo[,8]
	
	########################### DWRefillVol2  # Column 13
	Output_to_ColSim$DWRefillVol2 = Refill_Alternative[,8]

	########################### DWRunoffAprJuly  # Column 14
	tmp = aggregate(Supply_Input$DWORS, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=7), sum)
	DWRunoffAprJuly = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$DWRunoffAprJuly[1:length(DWRunoffAprJuly)] = DWRunoffAprJuly
	
	########################### DWSumRunoffMarJun  # Column 15
	tmp = aggregate(Supply_Input$DWORS, list(Supply_Input$Year, Supply_Input$Month>2, Supply_Input$Month<=6), sum)
	DWSumRunoffMarJun = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$DWSumRunoffMarJun[1:length(DWSumRunoffMarJun)] = DWSumRunoffMarJun
	
	########################### GCRefillVol1  # Column 16
	Output_to_ColSim$GCRefillVol1 = RefillStatusQuo[,9]
	
	########################### GCRefillVol2  # Column 17
	Output_to_ColSim$GCRefillVol2 = Refill_Alternative[,9]  # check
	
	########################### GCRunoffJanApr  # Column 18
	tmp = aggregate(Supply_Input$GCOUL, list(Supply_Input$Year, Supply_Input$Month<5), sum)
	GCRunoffJanApr = tmp[tmp[,2]==1,3]
	Output_to_ColSim$GCRunoffJanApr[1:length(GCRunoffJanApr)] = GCRunoffJanApr 
	
	########################### HHInQAprAug  # Column 19 ****
	tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum)
	HHInQAprAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$HHInQAprAug[1:length(HHInQAprAug)]=HHInQAprAug
	
	########################### HHRefillVol  # Column 20
	Output_to_ColSim$HHRefillVol = Refill_Alternative[,10] 
	
	########################### HHRefillVol1  # Column 21
	Output_to_ColSim$HHRefillVol1 = RefillStatusQuo[,10] # check
	
	########################### HHSumAprAug  # Column 22
	tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum) 
	HHSumAprAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$HHSumAprAug[1:length(HHSumAprAug)] = HHSumAprAug
	
	########################### HHSumAprJul  # Column 23
	HHSumAprJul = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=7), sum)
	HHSumAprJul = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$HHSumAprJul[1:length(HHSumAprJul)] = HHSumAprJul
	
	########################### HHSumMaySept  # Column 24
	tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>4, Supply_Input$Month<=9), sum)
	HHSumMaySept = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$HHSumMaySept[1:length(HHSumMaySept)] = HHSumMaySept
	
	########################### LBRefillVol2  # Column 25
	Output_to_ColSim$LBRefillVol2 = Refill_Alternative[,11] 
	
	########################### LBRefillVol1  # Column 26
	Output_to_ColSim$LBRefillVol1 = RefillStatusQuo[,11] 
	
	########################### LBSumAprAug  # Column 27
	tmp = aggregate(Supply_Input$LIBBY, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum)
	LBSumAprAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$LBSumAprAug[1:length(LBSumAprAug)] = LBSumAprAug
	
	########################### LBSumMayAug  # Column 28
	tmp = aggregate(Supply_Input$LIBBY, list(Supply_Input$Year, Supply_Input$Month>4, Supply_Input$Month<=8), sum)
	LBSumMayAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$LBSumMayAug[1:length(LBSumMayAug)] = LBSumMayAug
	
	########################### MIRefillVol1  # Column 29
	Output_to_ColSim$MIRefillVol1 = RefillStatusQuo[,12]
	
	########################### MIRefillVol2  # Column 30
	Output_to_ColSim$MIRefillVol2 = Refill_Alternative[,12] 
	
	########################### MISumRunoffAprilAug  # Column 28
	tmp = aggregate(Supply_Input$MICAA, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum) 
	MISumRunoffAprilAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$MISumRunoffAprilAug[1:length(MISumRunoffAprilAug)] = MISumRunoffAprilAug
	
	########################### MISumRunoffMayAug  # Column 28
	tmp = aggregate(Supply_Input$MICAA, list(Supply_Input$Year, Supply_Input$Month>4, Supply_Input$Month<=8), sum)
	MISumRunoffMayAug = tmp[apply(tmp[,2:3],1,sum)==2,4]
	Output_to_ColSim$MISumRunoffMayAug[1:length(MISumRunoffMayAug)] = MISumRunoffMayAug
	#------------------------------------------------- FLOW INPUTS ----------------------------------------------
	pod_stns = as.character(read.csv("~/Step_3/Aggregate_demand_ColSim/Stn_code_name_original.txt", sep="\t", header=T)[,2])
	stn_colsim = as.character(read.csv("~/Step_3/Aggregate_demand_ColSim/RColSim_stations.txt", sep="\t", header=FALSE)[,1])
	new_pod_names = pod_stns[pod_stns %in% stn_colsim]
	mainstem_names = c("CHIEF", "DALLE", "JDAYY", "MCNAR", "PRIRA", "ROCKY", "RISLA", "WANAP", "WELLS")

	cols = grep("PriVIC", names_Output)
	for (i in 1:length(cols)) {
		#print(paste(names(Output_to_ColSim)[cols[i]], " = ", stn_colsim[i], sep=""))
		Output_to_ColSim[,cols[i]] = Supply_Input[,names(Supply_Input)==stn_colsim[i]]
	}
	
	cols = grep("DemVIC", names_Output)
	for(i in 1:length(cols)) {
		#print(paste(names(Output_to_ColSim)[cols[i]], " = ", new_pod_names[i], sep=""))
		if (simulate_demand == 1) {
			Output_to_ColSim[,cols[i]] = Demand_Input[,names(Demand_Input)==new_pod_names[i]]
		} else {
			Output_to_ColSim[,cols[i]] = rep(0, length(Output_to_ColSim[,1])) 
		}
	}
	
	cols = grep("Iflow", names_Output)
	for (i in 1:length(cols)) {
		#print(paste(names(Output_to_ColSim)[cols[i]], " = ", mainstem_names[i], sep=""))
		Output_to_ColSim[,cols[i]] = get_iflow_mainstem(mainstem_names[i]) 
	}
	
	cols = grep("CurtVIC", names_Output)
	for (i in 1:length(cols)) {
		if (simulate_demand == 1) {
			Output_to_ColSim[,cols[i]] = mainstem_interruptible[,names(mainstem_interruptible)==mainstem_names[i]]
		} else {
			Output_to_ColSim[,cols[i]] = rep(0, length(Output_to_ColSim[,1])) 
		}
	}
	
	annual_return = aggregate(supply_for_return_flow[,5], list(supply_for_return_flow$Year), sum)
	if (simulate_demand == 1) {
		Output_to_ColSim$RetVICWA = return_fractions[match(supply_for_return_flow$Week, return_fractions[,1]),1] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
		Output_to_ColSim$RetVICPR = return_fractions[match(supply_for_return_flow$Week, return_fractions[,1]),2] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
		Output_to_ColSim$RetVICMCN = return_fractions[match(supply_for_return_flow$Week, return_fractions[,1]),3] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
	} else {
		Output_to_ColSim$RetVICWA = rep(0, length(Output_to_ColSim[,1])) 
		Output_to_ColSim$RetVICPR = rep(0, length(Output_to_ColSim[,1])) 
		Output_to_ColSim$RetVICMCN = rep(0, length(Output_to_ColSim[,1])) 
	}
	
	run_type = ifelse(simulate_demand==1, "Supply_and_demand", "Supply_only")
	
	if (!dir.exists(paste("~/Step_4/output", run_type, gcm, sep="/"))) {
		dir.create(paste("~/Step_4/output", run_type, gcm, sep="/"))
	}
	
	write.table(Output_to_ColSim, file=paste("~/Step_4/output/", run_type, "/", gcm, "/ToRColSim_scenario_", scr, ".txt", sep=""), row.names=FALSE)
} 

global_input_files = sub("/", "_", list_scenarios)
global_input_files[1] = "Historical_baseline"
for (scr_no in 1:length(global_input_files)) {
	print(global_input_files[scr_no])
	gcm = strsplit(list_scenarios[scr_no], "/")[[1]][1]
	scr = strsplit(list_scenarios[scr_no], "/")[[1]][2]
	GIF = data.frame(matrix(nrow=4, ncol=2))
	GIF[,1] = c("RColSim_WD", "Flow_Input_File", "N_of_TimeSteps", "Output_Folder")
	GIF[1,2] = "~/Step_5/RColSim"
	GIF[2,2] = paste("~/Step_4/output/", run_type, "/", gcm, "/ToRColSim_scenario_", scr, ".txt", sep="")
	GIF[3,2] = length(read.table(GIF[2,2], header=T)[,1])
	GIF[4,2] = paste("~/Step_5/output", run_type, gcm, scr, sep="/")
	if (!dir.exists(paste("~/Step_5/output", run_type, sep="/"))) {
		dir.create(paste("~/Step_5/output", run_type, sep="/"))
	}
	if (!dir.exists(paste("~/Step_5/output", run_type, gcm, sep="/"))) {
		dir.create(paste("~/Step_5/output", run_type, gcm, sep="/"))
	}
	if (!dir.exists(paste("~/Step_5/output", run_type, gcm, scr, sep="/"))) {
		dir.create(paste("~/Step_5/output", run_type, gcm, scr, sep="/"))
	}
	if (!dir.exists(paste("~/Step_5/output", run_type, gcm, scr, sep="/"))) {
		dir.create(paste("~/Step_5/output", run_type, gcm, scr, sep="/"))
	}
	if (!dir.exists(paste("~/Step_5/RColSim/inputs", run_type, sep="/"))) {
		dir.create(paste("~/Step_5/RColSim/inputs", run_type, sep="/"))
	}
	write.table(GIF, paste("~/Step_5/RColSim/inputs/", run_type, "/", "GIF_", global_input_files[scr_no], sep=""), col.names=FALSE, row.names=FALSE, quote=FALSE)
}





