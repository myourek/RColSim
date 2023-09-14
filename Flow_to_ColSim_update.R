#project_name <- commandArgs()[6]
#scr_name <- sub("_", "/", commandArgs()[7])
#run_type <- commandArgs()[8]

project_name <- "update_RColSim"
scr_name <- "Historical/baseline"
run_type <- "supply_and_demand"
source("~/RColSim/LoadFunctions_input_file.R")
source("~/RColSim/LoadFunctions_update_cleaned.R")
source("~/RColSim/Read_Rule_Curves_new.R")
Read_Rule_Curves()
if(scr_name == "Historical/baseline") {
	scr_name <- "Historical_baseline/baseline"
}
if (run_type == "supply_and_demand") {
	simulate_demand <- 1
} else {
	simulate_demand <- 0
}
gcm <- strsplit(scr_name, "/")[[1]][1]
scr <- strsplit(scr_name, "/")[[1]][2]

stn_colsim <- read.table("~/Step_3/Aggregate_demand_ColSim/RColSim_stations_new.txt", header=T, stringsAsFactors=F)
DamMaxMin <- read.table("~/Step_4/DamMaxMin_new.txt", header=T)
mainstem_names <- c("CHIEF", "DALLE", "JDAYY", "MCNAR", "PRIRA", "ROCKY", "RISLA", "WANAP", "WELLS")
indir <- paste0("~/Step_3/Aggregate_demand_ColSim/to_step_4/", project_name, "/", run_type, "/", gcm, "/")
ListOfDams <- c("MICAA", "ARROW", "DUNCA", "DWORS", "GCOUL", "FLASF", "LIBBY", "BROWN", "ALBEN", "FLAPO", "CORRA")
dam_lookup <- data.frame(name1=ListOfDams, name2=c("Mica", "Arrow", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Brownlee", "AF", "Kerr", "CL"), stringsAsFactors=F)

Supply_Input <- read.table(paste0(indir, "supply_", scr, ".txt"), header=T)
names(Supply_Input)[1:4] <- c("Week", "Month", "Day", "Year")
timeseries <- Supply_Input[1:4]
begin_year <- min(timeseries$Year)
N <- nrow(Supply_Input)
sim_start_year <- unique(Supply_Input$Year)[1]
sim_end_date <- as.Date(paste(Supply_Input$Month, Supply_Input$Day, Supply_Input$Year, sep="/"), "%m/%d/%Y")[nrow(Supply_Input)]
cfsTOafw <- 1.9834 * 7
if (simulate_demand == 1) {
	Demand_Input <- read.table(paste0(indir, "demand_", scr, ".txt"), header=T)
	interruptible_demand <- read.table(paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/", project_name, "/", run_type, "/", gcm, "/interruptible_", scr, ".txt", sep=""), header=T)
} else {
	Demand_Input <- data.frame(matrix(nrow=N, ncol=nrow(stn_colsim)+9, 0))
	interruptible_demand <- data.frame(matrix(nrow=N, ncol=length(mainstem_names) + 4, 0))
}

########### Calculate modified flow for rule curves #############################

flow_map <- read.table("~/Step_4/modified_flow_map.txt", header=T, stringsAsFactors=F)
mod_flow_list <- names(Supply_Input)[-c(1:4)]
modified_flow <- data.frame(matrix(nrow=nrow(Supply_Input), ncol=length(mod_flow_list) + 4))
modified_flow[1:4] <- timeseries
names(modified_flow) <- c(names(timeseries), mod_flow_list)
for (dam in mod_flow_list) {
	col_names <- get_columns(dam, flow_map)
	up_demand <- apply(Demand_Input[col_names], 1, sum)
	modified_flow[dam] <- Supply_Input[,dam] - up_demand
}
if (simulate_demand == 1) {
	supply_for_return_flow <- data.frame(matrix(nrow=N, ncol=7, 0))
	supply_for_return_flow[,1:4] <- timeseries
	supply_for_return_flow_locs <- c("WANAP", "PRIRA", "MCNAR")
	for (i in 1:3) {
		col <- which(names(modified_flow) == supply_for_return_flow_locs[i])
		supply_for_return_flow[,i+4] <- modified_flow[,col]
	}
	names(supply_for_return_flow) <- c("Week", "Month", "Day", "Year", supply_for_return_flow_locs)
	return_fractions <- read.csv("~/Step_4/return_flow_fractions.csv", header=F) # these fractions were derived from the document, "Calculation of 2020 Irrigation Depletions for
	# 2020 Level Modified Streamsflows, Hills et al., prepared for BPA, 2020.
} else {
	return_fractions <- data.frame(matrix(nrow=N, ncol=3, 0))
	supply_for_return_flow <- data.frame(matrix(nrow=N, ncol=7, 0))
}

########################## data frames
refill_names <- c("AR", "DU", "DW", "GC", "HH", "LB", "MI")
min_refill_names <- c("AF", "AR", "BR", "CL", "DU", "DW", "GC", "HH", "LB", "MI", "KE")
Dem_list <- c("BoiseSys", "Minidoka", "Owyhee", "Payette", "UpSnake", stn_colsim[match(mainstem_names, stn_colsim[,1]),2])
names_Output <- c("Week", "Month", "Day", "Year", "BRRunoffAprJul", "DARunoffAprAug", "DARunoffAprSep", "DARunoffJanJul", "DURunoffAprAug", "DWRunoffAprJul", "HHRunoffAprAug", "HHRunoffMaySep",
	"LBRunoffAprAug", "LGRunoffAprJul", "MIRunoffAprAug", "MIRunoffMayAug", "PayetteResidualInflowJanJun", "OwyheeResidualInflowJanMay", "BoiseResidualInflowJanJul", "HeiseResidualInflowJanJul", 
	"HenryResidualInflowJanJun", "RirieResidualInflowJanJun", "PRResidualInflowJanMar", "GCResidualInflowJanMar", "RetVICWA", "RetVICPR", "RetVICMCN", paste0(refill_names, "VariableRefillCurve"),
	paste0(min_refill_names, "MinRefillCurve"), paste0(min_refill_names, "OperatingRuleCurve"), paste0(min_refill_names, "ECCurve"), paste0("Flow", c("LimePoint", stn_colsim[,2])), paste0("Dem", Dem_list), 
	paste0(min_refill_names, "FloodCurve"), paste0("Curt", stn_colsim[match(mainstem_names, stn_colsim[,1]),2]), paste0("Iflow", stn_colsim[match(mainstem_names, stn_colsim[,1]),2]), "CorrectedDARunoffAprAug", 
	"InitialControlledFlow", "start_refill_wk", "DACorrectedResidualInflowAprAug")
Output_to_ColSim <- data.frame(matrix(ncol=length(names_Output), nrow=N))
Output_to_ColSim[1:4] <- timeseries
names(Output_to_ColSim) <- names_Output

####################################### Variable Refill Curves ###########################################

### Power Discharge Requirements. These are parameters used by the Columbia River Treaty Operating Committee for computing variable refill.
## Described in "Columbia River Treaty Principles and Procedures" (2003). 

DARunoffJanJul <- subset(aggregate(modified_flow$DALLE, list(modified_flow$Year, modified_flow$Month<=7), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
DA_forecast <- data.frame(unique(timeseries$Year)[-1], DARunoffJanJul)
names(DA_forecast) <- c("Year", "Q")
n_years <- nrow(DA_forecast)
years <- DA_forecast$Year

refill_option <- 1
if (refill_option < 3) {
	PDR_80 <- read.table("~/Step_4/AssumedRelease_80MAF_med.txt", header=T)
	PDR_95 <- read.table("~/Step_4/AssumedRelease_95MAF_med.txt", header=T)
	PDR_110 <- read.table("~/Step_4/AssumedRelease_110MAF_med.txt", header=T)
	forecast_error <- read.table("~/Step_4/Forecast_errors.txt", header=T)
} else if (refill_option == 3) {
	PDR_80 <- read.table("~/Step_4/AssumedRelease_80MAF_PF.txt", header=T)
	PDR_95 <- read.table("~/Step_4/AssumedRelease_95MAF_PF.txt", header=T)
	PDR_110 <- read.table("~/Step_4/AssumedRelease_110MAF_PF.txt", header=T)
	forecast_error <- read.table("~/Step_4/Forecast_errors_PF.txt", header=T)
}
PDR_lower <- read.table("~/Step_4/AssumedRelease_min.txt", header=T)

#### Set the target refill Week

target_refill_week <- data.frame(Dam=ListOfDams, Week=c(52, 52, 52, 48, 48, 48, 52, 48, 52, 52, 5))
July31s <- data.frame(matrix(nrow=n_years, ncol=length(ListOfDams)))
nws <- data.frame(matrix(nrow=n_years, ncol=length(ListOfDams)))
names(July31s) <- ListOfDams
names(nws) <- ListOfDams
for (d in ListOfDams) {
	wk <- target_refill_week$Week[target_refill_week$Dam==d]
	if (wk == 52) {
		July31s[d] <- (which(timeseries$Month == 8 & timeseries$Week == 1) - 1)[-1]
	} else if (wk > 40) {
		July31s[d] <- which(timeseries$Week == wk)
	} else {
		July31s[d] <- which(timeseries$Week == wk)[-1]
	}
	nws[d] <- c(52, July31s[,d][2:length(July31s[,d])] - July31s[,d][1:(length(July31s[,d])-1)])
}

############ Runoff volumes for inflow forecasts

Output_to_ColSim$DARunoffJanJul[1:n_years] <- DARunoffJanJul
Output_to_ColSim$DARunoffAprAug[1:n_years] <- subset(aggregate(modified_flow$DALLE, list(modified_flow$Year, modified_flow$Month %in% 4:8), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$DARunoffAprSep[1:n_years] <- subset(aggregate(modified_flow$DALLE, list(modified_flow$Year, modified_flow$Month %in% 4:9), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$DURunoffAprAug[1:n_years] <- subset(aggregate(modified_flow$DUNCA, list(modified_flow$Year, modified_flow$Month %in% 4:8), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$DWRunoffAprJul[1:n_years] <- subset(aggregate(modified_flow$DWORS, list(modified_flow$Year, modified_flow$Month %in% 4:7), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$BRRunoffAprJul[1:n_years] <- subset(aggregate(modified_flow$BROWN, list(modified_flow$Year, modified_flow$Month %in% 4:7), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$LGRunoffAprJul[1:n_years] <- subset(aggregate(modified_flow$LGRAN, list(modified_flow$Year, modified_flow$Month %in% 4:7), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$HHRunoffAprAug[1:n_years] <- subset(aggregate(modified_flow$FLASF, list(modified_flow$Year, modified_flow$Month %in% 4:7), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$HHRunoffMaySep[1:n_years] <- subset(aggregate(modified_flow$FLASF, list(modified_flow$Year, modified_flow$Month %in% 5:9), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$LBRunoffAprAug[1:n_years] <- subset(aggregate(modified_flow$LIBBY, list(modified_flow$Year, modified_flow$Month %in% 4:8), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$MIRunoffAprAug[1:n_years] <- subset(aggregate(modified_flow$MICAA, list(modified_flow$Year, modified_flow$Month %in% 4:8), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]
Output_to_ColSim$MIRunoffMayAug[1:n_years] <- subset(aggregate(modified_flow$MICAA, list(modified_flow$Year, modified_flow$Month %in% 5:8), sum), Group.2 == TRUE & Group.1 > begin_year)[,3]

### Calculate refill curve for reservoirs without upstream storage 

VariableRefillCurve <- calc_refill_curve("regular")
VariableRefillCurve_min <- calc_refill_curve("minimum")

MIRefill_min <- VariableRefillCurve_min$MICAA[2:N] - VariableRefillCurve_min$MICAA[1:(N-1)]
DURefill_min <- VariableRefillCurve_min$DUNCA[2:N] - VariableRefillCurve_min$DUNCA[1:(N-1)]
LBRefill_min <- VariableRefillCurve_min$LIBBY[2:N] - VariableRefillCurve_min$LIBBY[1:(N-1)]
HHRefill_min <- VariableRefillCurve_min$FLASF[2:N] - VariableRefillCurve_min$FLASF[1:(N-1)]
KERefill_min <- VariableRefillCurve_min$FLAPO[2:N] - VariableRefillCurve_min$FLAPO[1:(N-1)]
AFRefill_min <- VariableRefillCurve_min$ALBEN[2:N] - VariableRefillCurve_min$ALBEN[1:(N-1)]
CLRefill_min <- VariableRefillCurve_min$CORRA[2:N] - VariableRefillCurve_min$CORRA[1:(N-1)]
BRRefill_min <- VariableRefillCurve_min$BROWN[2:N] - VariableRefillCurve_min$BROWN[1:(N-1)]
DWRefill_min <- VariableRefillCurve_min$DWORS[2:N] - VariableRefillCurve_min$DWORS[1:(N-1)]

MIRuleCurves.df <- RuleCurve_df("MICAA")
ARRuleCurves.df <- RuleCurve_df("ARROW")
DURuleCurves.df <- RuleCurve_df("DUNCA")
LBRuleCurves.df <- RuleCurve_df("LIBBY")
HHRuleCurves.df <- RuleCurve_df("FLASF")
KERuleCurves.df <- RuleCurve_df("FLAPO")
AFRuleCurves.df <- RuleCurve_df("ALBEN")
CLRuleCurves.df <- RuleCurve_df("CORRA")
BRRuleCurves.df <- RuleCurve_df("BROWN")
DWRuleCurves.df <- RuleCurve_df("DWORS")

###### Rule curves for reservoirs with upstream storage

VariableRefillCurve$ARROW <- calc_refill_with_upstream_storage("regular", "ARROW")
VariableRefillCurve_min$ARROW <- calc_refill_with_upstream_storage("minimum", "ARROW")
ARRuleCurves.df <- RuleCurve_df("ARROW")
ARRefill_min <- VariableRefillCurve_min$ARROW[2:N] - VariableRefillCurve_min$ARROW[1:(N-1)]


GCUpstreamRefill <- MIRuleCurves.df$RefillReq + ARRuleCurves.df$RefillReq + DURuleCurves.df$RefillReq + 
	LBRuleCurves.df$RefillReq + HHRuleCurves.df$RefillReq + KERuleCurves.df$RefillReq + 
	AFRuleCurves.df$RefillReq + CLRuleCurves.df$RefillReq + DWRuleCurves.df$RefillReq
GCUpstreamRefill[is.na(GCUpstreamRefill)] <- 0

GCUpstreamRefill_min <- MIRefill_min + ARRefill_min + DURefill_min + LBRefill_min + HHRefill_min + KERefill_min +
	AFRefill_min + CLRefill_min + DWRefill_min
GCUpstreamRefill_min[is.na(GCUpstreamRefill_min)] <- 0

VariableRefillCurve$GCOUL <- calc_refill_with_upstream_storage("regular", "GCOUL")
VariableRefillCurve_min$GCOUL <- calc_refill_with_upstream_storage("minimum", "GCOUL")

write.table(VariableRefillCurve, "~/Step_4/test_weekly_assured_after_refill.txt", col.names=T, row.names=F, quote=F)
write.table(VariableRefillCurve_min, "~/Step_4/test_weekly_assured_min_after_refill.txt", col.names=T, row.names=F, quote=F)

##### Corrected April to Aug inflow at The Dalles for Grand Coulee flood curve

OperatingRuleCurves.df <- data.frame(matrix(nrow=N, ncol=ncol(VariableRefillCurve)))
OperatingRuleCurves.df[1:3] <- VariableRefillCurve[1:3]
names(OperatingRuleCurves.df) <- c("Week", "Month", "Year", "MICAA", "ARROW", "DUNCA", "DWORS", "FLASF",
	"LIBBY", "ALBEN", "FLAPO", "CORRA", "BROWN", "GCOUL")
OperatingRuleCurves.df$MICAA <- MIRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$ARROW <- ARRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$DUNCA <- DURuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$DWORS <- DWRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$FLASF <- HHRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$LIBBY <- LBRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$ALBEN <- AFRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$FLAPO <- KERuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$CORRA <- CLRuleCurves.df$OperatingRuleCurve
OperatingRuleCurves.df$BROWN <- BRRuleCurves.df$OperatingRuleCurve

FloodCurves.df <- data.frame(MIRuleCurves.df[1:5], MICAA=MIRuleCurves.df$Flood, ARROW=ARRuleCurves.df$Flood, LIBBY=LBRuleCurves.df$Flood, FLASF=HHRuleCurves.df$Flood,
	DUNCA=DURuleCurves.df$Flood, DWORS=DWRuleCurves.df$Flood, BROWN=BRRuleCurves.df$Flood, CORRA=CLRuleCurves.df$Flood, FLAPO=KERuleCurves.df$Flood, ALBEN=AFRuleCurves.df$Flood)
ECC.df <- data.frame(MIRuleCurves.df[1:5], MICAA=pmin(pmax(MIRuleCurves.df$Critical, MIRuleCurves.df$AssuredRefill), MIRuleCurves.df$Flood), 
	ARROW=pmin(pmax(ARRuleCurves.df$Critical, ARRuleCurves.df$AssuredRefill), ARRuleCurves.df$Flood), LIBBY=pmin(pmax(LBRuleCurves.df$Critical, LBRuleCurves.df$AssuredRefill), LBRuleCurves.df$Flood),
	FLASF=pmin(pmax(HHRuleCurves.df$Critical, HHRuleCurves.df$AssuredRefill), HHRuleCurves.df$Flood), DUNCA=pmin(pmax(DURuleCurves.df$Critical, DURuleCurves.df$AssuredRefill), DURuleCurves.df$Flood),
	DWORS=pmin(pmax(DWRuleCurves.df$Critical, DWRuleCurves.df$AssuredRefill), DWRuleCurves.df$Flood), BROWN=pmin(pmax(BRRuleCurves.df$Critical, BRRuleCurves.df$AssuredRefill), BRRuleCurves.df$Flood),
	CORRA=pmin(CLRuleCurves.df$Critical, CLRuleCurves.df$Flood), FLAPO=pmin(pmax(KERuleCurves.df$Critical, KERuleCurves.df$AssuredRefill), KERuleCurves.df$Flood), 
	ALBEN=pmin(pmax(AFRuleCurves.df$Critical, AFRuleCurves.df$AssuredRefill), AFRuleCurves.df$Flood))
Mar_31s <- which(Output_to_ColSim$Week == 35)
AprilDAUpstreamStorageGC <- pmin(4.08e6, (MIFullPoolVol - MIRuleCurves.df$Flood[Mar_31s])) + 
	pmin(3.6e6, (ARFullPoolVol - ARRuleCurves.df$Flood[Mar_31s])) + (LBFullPoolVol - LBRuleCurves.df$Flood[Mar_31s]) + 
	(HHFullPoolVol - HHRuleCurves.df$Flood[Mar_31s]) + (DUFullPoolVol - DURuleCurves.df$Flood[Mar_31s]) + 
	(DWFullPoolVol - DWRuleCurves.df$Flood[Mar_31s]) + (BRFullPoolVol - BRRuleCurves.df$Flood[Mar_31s])
Output_to_ColSim$CorrectedDARunoffAprAug <- Output_to_ColSim$DARunoffAprAug - c(AprilDAUpstreamStorageGC, rep(NA, N - n_years))


###### Corrected residual inflow at The Dalles for initial controlled flow computation

GCRuleCurves.df <- RuleCurve_df("GCOUL")
FloodCurves.df$GCOUL <- GCRuleCurves.df$Flood
ECC.df$GCOUL <- min(max(GCRuleCurves.df$Critical, GCRuleCurves.df$AssuredRefill), GCRuleCurves.df$Flood)
OperatingRuleCurves.df$GCOUL <- GCRuleCurves.df$OperatingRuleCurve

AprilDAUpstreamStorage <- c(AprilDAUpstreamStorageGC + (GCFullPoolVol - GCRuleCurves.df$Flood[Mar_31s]), rep(NA, N - n_years))

evacuation_end <- c(MICAA=35, ARROW=35, LIBBY=39, FLASF=39, DUNCA=35, DWORS=37, BROWN=39, GCOUL=39)
full_pool <- c(MICAA=MIFullPoolVol, ARROW=ARFullPoolVol, LIBBY=LBFullPoolVol, FLASF=HHFullPoolVol, DUNCA=DUFullPoolVol, DWORS=DWFullPoolVol, BROWN=BRFullPoolVol, GCOUL=GCFullPoolVol)
min_storage <- c(MICAA=4.08e6, ARROW=3.6e6, LIBBY=4.98e6, FLASF=3.07e6, DUNCA=1.27e6, DWORS=2015200, BROWN=975000, GCOUL=5.19e6)

DAUpStreamStorage <- data.frame(matrix(nrow=N, ncol=length(evacuation_end) + 4, 0))
DAUpStreamStorage[1:4] <- modified_flow[1:4]
names(DAUpStreamStorage) = c("Week", "Month", "Day", "Year", names(evacuation_end))
for (res in names(evacuation_end)) {
	flood_evac_period <- which(FloodCurves.df$Week %in% 35:evacuation_end[res])
	refill_period <- (evacuation_end[res] + 1):51
	row_num_refill <- which(OperatingRuleCurves.df$Week %in% refill_period)
	DAUpStreamStorage[flood_evac_period,res] <- pmin(min_storage[res], full_pool[res] - FloodCurves.df[flood_evac_period,res])
	DAUpStreamStorage[row_num_refill,res] <- pmin(min_storage[res], full_pool[res] - pmax(OperatingRuleCurves.df[row_num_refill,res], rep(FloodCurves.df[FloodCurves.df$Week==evacuation_end[res],res], each=length(refill_period))))
}
DAUpStreamStorage$Sum <- apply(DAUpStreamStorage[-c(1:4)], 1, sum)

DACorrectedResidualInflow <- function() {
	mod_flow <- subset(modified_flow, Month %in% 4:8)[c("Week", "Month", "Day", "Year", "DALLE")]
	storage <- subset(DAUpStreamStorage, Month %in% 4:8)[c("Week", "Month", "Day", "Year", "Sum")]
	mod_flow.df <- data.frame(matrix(nrow=nrow(Output_to_ColSim), ncol=3))
	mod_flow.df[1:2] <- Output_to_ColSim[c("Month", "Year")]
	mod_flow.df[3] <- 0
	names(mod_flow.df) <- c("Month", "Year", "Flow")
	for (y in years) {
		year_flow <- subset(mod_flow, Year==y)
		year_storage <- subset(storage, Year==y)
		cumflow <- cumsum(year_flow$DALLE)
		total_inflow <- sum(year_flow$DALLE)
		mod_flow.df[which(mod_flow.df$Year==y & mod_flow.df$Month %in% 4:8),3] <- head(c(total_inflow - year_storage$Sum[1], pmax(0, total_inflow - cumflow - year_storage$Sum)), -1)
	}
	return(mod_flow.df[,3])
}
Output_to_ColSim$DACorrectedResidualInflowAprAug <- DACorrectedResidualInflow()

##### Initial controlled flow 

ICF_table <- read.table("~/RColSim/default_rule_curves/Dalles_ICF.txt", header=T)
flow_inc <- seq(from=30e6, to=140e6, by=5e6)
get_ICF <- function(res_inflow, wk) {
	if (res_inflow <= 30e6) {
		ICF <- 9999
	} else {
		upper_inflow <- flow_inc[which(flow_inc >= res_inflow)[1]]
		lower_inflow <- flow_inc[tail(which(flow_inc <= res_inflow), 1)]
		upper_CF <- ICF_table[wk, which(flow_inc >= res_inflow)[1]+1]
		lower_CF <- ICF_table[wk, tail(which(flow_inc <= res_inflow), 1)+1]
		ICF <- lower_CF + (upper_CF - lower_CF) / (upper_inflow - lower_inflow) * (res_inflow - lower_inflow)
	}
	ICF_o <- 1000 * ICF
	return(ICF_o)
}

Output_to_ColSim$InitialControlledFlow <- sapply(1:N, function(x) get_ICF(Output_to_ColSim$DACorrectedResidualInflowAprAug[x], timeseries$Week[x]))
Output_to_ColSim$start_refill_wk[1:n_years] <- aggregate(modified_flow$DALLE, list(modified_flow$Year), function(x) which(x > 450000 * cfsTOafw)[1] - 3 + 22)[-1,2]


start_refill_wk_GC = vector()
for (y in 1:length(years)) {
	ICF <- subset(Output_to_ColSim, Year == years[y] & Week == Output_to_ColSim$start_refill_wk[y])$InitialControlledFlow
	wk_GC <- which(subset(modified_flow, Year == years[y])$DALLE >= ICF * cfsTOafw)[1] + 22
	start_refill_wk_GC <- c(start_refill_wk_GC, wk_GC)
}


########## Residual runoff

runoff_remaining <- function(stn, start_wk, end_wk, flow) {
	mod_flow <- subset(flow, Week %in% start_wk:end_wk)[c("Week", "Month", "Day", "Year", stn)]
	mod_flow.df <- data.frame(matrix(nrow=nrow(Output_to_ColSim), ncol=3))
	mod_flow.df[1:2] <- Output_to_ColSim[c(1,4)]
	mod_flow.df[3] <- 0
	for (y in unique(mod_flow$Year)) {
		year_flow <- subset(mod_flow, Year==y)
		cumflow <- cumsum(year_flow[stn])[,1]
		total_inflow <- sum(year_flow[stn])
		mod_flow.df[which(mod_flow.df[,2]==y & mod_flow.df[,1] %in% start_wk:end_wk),3] <- pmax(0, total_inflow - cumflow)
	}
	return(mod_flow.df[,3])
}
Output_to_ColSim$HeiseResidualInflowJanJul <- runoff_remaining("SNKHE", 23, 50, modified_flow) # Residual inflow for Palisades and Jackson Lake rule curves
Output_to_ColSim$HenryResidualInflowJanJun <- runoff_remaining("IPARK", 23, 47, modified_flow) # Residual inflow for Island Park rule curve
Output_to_ColSim$RirieResidualInflowJanJun <- runoff_remaining("RIRDM", 23, 47, modified_flow) # Residual inflow for Ririe rule curve
Output_to_ColSim$BoiseResidualInflowJanJul <- runoff_remaining("LUCKY", 23, 49, modified_flow) # Residual inflow for Boise system rule curve
Output_to_ColSim$OwyheeResidualInflowJanMay <- runoff_remaining("OWYHE", 23, 45, modified_flow) # Residual inflow for Owyhee rule curve
Output_to_ColSim$PayetteResidualInflowJanJun <- runoff_remaining("PAYHS", 23, 47, modified_flow) # Residual inflow for Payette system rule curve
Output_to_ColSim$PRResidualInflowJanMar <- runoff_remaining("PRIRA", 23, 36, modified_flow) # Residual inflow to Priest Rapids dam for computation of Grand Coulee variable draft limit
Output_to_ColSim$GCResidualInflowJanMar <- runoff_remaining("GCOUL", 23, 36, modified_flow) # Residual inflow to Priest Rapids dam for computation of Grand Coulee variable draft limit

#------------------------------------------------- FLOW INPUTS ----------------------------------------------

############# Refill curves
variable_refill_list <- c("MICAA", "ARROW", "DUNCA", "LIBBY", "FLASF", "DWORS", "GCOUL")
for (res in variable_refill_list) {
	abbrev <- paste0(stn_colsim[which(stn_colsim[,1]==res),2])
	var <- paste0(abbrev, "VariableRefillCurve")
	Output_to_ColSim[,var] <- VariableRefillCurve[,res]
}
for (res in ListOfDams) {
	abbrev <- paste0(stn_colsim[which(stn_colsim[,1]==res),2])
	var1 <- paste0(abbrev, "MinRefillCurve")
	var2 <- paste0(abbrev, "FloodCurve")
	var3 <- paste0(abbrev, "ECCurve")
	Output_to_ColSim[,var1] <- VariableRefillCurve_min[,res]
	Output_to_ColSim[,var2] <- FloodCurves.df[,res]
	Output_to_ColSim[,var3] <- ECC.df[,res]
}
for (res in names(OperatingRuleCurves.df)[-c(1:3)]) {
	abbrev <- paste0(stn_colsim[which(stn_colsim[,1]==res),2])
	var <- paste0(abbrev, "OperatingRuleCurve")
	Output_to_ColSim[,var] <- OperatingRuleCurves.df[,res]
}
flow_list <- c(stn_colsim[,1], "LimePoint")
for (loc in flow_list) {
	abbrev <- ifelse(loc=="LimePoint", "LimePoint", stn_colsim[which(stn_colsim[,1]==loc),2])
	loc <- ifelse(loc=="LimePoint", "LIMEP", loc)
	var_supply <- paste0("Flow", abbrev)
	Output_to_ColSim[,var_supply] <- modified_flow[,loc]
}
demand_list = c("BoiseSys", "Minidoka", "Owyhee", "Payette", "UpSnake", mainstem_names)
for (dem in demand_list) {
	abbrev <- ifelse(dem %in% mainstem_names, stn_colsim[which(stn_colsim[,1]==dem),2], dem)
	var_demand <- paste0("Dem", abbrev)
	if (simulate_demand == 1) {
		Output_to_ColSim[,var_demand] <- Demand_Input[,dem]
	} else {
		Output_to_ColSim[,var_demand] <- rep(0, nrow(Output_to_ColSim)) 
	}
}
for (m in mainstem_names) {
	abbrev <- stn_colsim[which(stn_colsim[,1]==m),2]
	var_interruptible <- paste0("Curt", abbrev)
	var_iflow <- paste0("Iflow", abbrev)
	if (simulate_demand == 1) {
		Output_to_ColSim[,var_interruptible] <- interruptible_demand[,m]
		Output_to_ColSim[,var_iflow] <- get_iflow_mainstem(m) 
	} else {
		Output_to_ColSim[,var_interruptible] <- rep(0, nrow(Output_to_ColSim))
		Output_to_ColSim[,var_iflow] <- rep(0, nrow(Output_to_ColSim))
	}
}

if (simulate_demand == 1) {
	annual_return <- aggregate(supply_for_return_flow[5], list(supply_for_return_flow$Year), sum)
	Output_to_ColSim$RetVICWA <- return_fractions[match(supply_for_return_flow$Week, 1:52),1] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
	Output_to_ColSim$RetVICPR <- return_fractions[match(supply_for_return_flow$Week, 1:52),2] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
	Output_to_ColSim$RetVICMCN <- return_fractions[match(supply_for_return_flow$Week, 1:52),3] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
} else {
	Output_to_ColSim$RetVICWA <- rep(0, nrow(Output_to_ColSim)) 
	Output_to_ColSim$RetVICPR <- rep(0, nrow(Output_to_ColSim)) 
	Output_to_ColSim$RetVICMCN <- rep(0, nrow(Output_to_ColSim)) 
}
if (!dir.exists(paste0("~/Step_4/output/", project_name, "/", run_type, "/", gcm))) {
	dir.create(paste0("~/Step_4/output/", project_name, "/", run_type, "/", gcm), recursive=TRUE)
}
step4_dir <- paste0("~/Step_4/output/", project_name, "/", run_type, "/", gcm, "/")
write.table(Output_to_ColSim, file=paste0(step4_dir, "ToRColSim_scenario_", scr, ".txt"), row.names=FALSE)

##################### Make Global Input File #############################################

outdir <- paste0("~/Step_5/output/", project_name, "/", run_type, "/", gcm, "/")
if (scr_name == "Historical_baseline/baseline") {
	global_input_file <- "Historical_baseline"
	scr <- "baseline"
	gcm <- "Historical_baseline"
} else {
	global_input_file <- sub("/", "_", scr_name)
	scr <- strsplit(global_input_file, "_")[[1]][2]
	gcm <- strsplit(global_input_file, "_")[[1]][1]
}
GIF <- data.frame(matrix(nrow=5, ncol=2))
GIF[,1] <- c("RColSim_WD", "Flow_Input_File", "Output_Folder", "simulation_start_year", "simulation_end_date")
GIF[1,2] <- "~/Step_5/RColSim"
GIF[2,2] <- paste0(step4_dir, "ToRColSim_scenario_", scr, ".txt")
if (global_input_file == "Historical_baseline") {
	GIF[3,2] <- outdir
} else {
	GIF[3,2] <- paste0(outdir, scr, "/")
}
GIF[4,2] <- sim_start_year
GIF[5,2] <- as.character(sim_end_date)

if (!dir.exists(outdir)) {
	dir.create(outdir, recursive=TRUE)
}
if (gcm!="Historical_baseline") {
	if (!dir.exists(paste0(outdir, scr))) {
		dir.create(paste0(outdir, scr))
	}
}
if (!dir.exists(paste0("~/Step_5/RColSim/inputs/", project_name, "/", run_type))) {
	dir.create(paste0("~/Step_5/RColSim/inputs/", project_name, "/", run_type), recursive=TRUE)
}
write.table(GIF, paste0("~/Step_5/RColSim/inputs/", project_name, "/", run_type, "/GIF_", global_input_file), col.names=F, row.names=F, quote=F)


	