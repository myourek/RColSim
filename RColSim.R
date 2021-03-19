################################################
#
#
#                    R-ColSim
#
#
################################################
#######################################
#                                     #
#                                     #
#          Initialization             #
#                                     #
#                                     #
#######################################

# 5 files need to be loaded in the following order: 1- LoadFunctions.R 2- ReadFiles.R 3- Switches.R  4- dataframes.R  5- VIC_Data.R
#
#
#
#
tempfile(tmpdir='/fastscratch/myourek')
project_name = commandArgs()[6]
landuse_scenario = commandArgs()[7]
scr = commandArgs()[8]
run_type = commandArgs()[9]

#project_name="Forecast"
#landuse_scenario="Historical_baseline"
#scr="Historical_baseline"
#run_type="supply_and_demand"

if (project_name == "GCAM") {
	landuse_scenario = paste(landuse_scenario, "/", sep="")
} else {
	landuse_scenario = ""
}

print(paste("Now doing scenario:", scr))

GlobalFile = read.table(paste("~/Step_5/RColSim/inputs/", project_name, "/", run_type, "/", landuse_scenario, "GIF_", scr, sep=""), stringsAsFactors=FALSE)	# reads the global input file
N_of_TimeSteps = as.numeric(as.character(GlobalFile[3,2]))	
input_file = read.table(GlobalFile[2,2], header=TRUE)
setwd(GlobalFile[1,2])
# 1- LOAD ALL FUNCTIONS
source("LoadFunctions3.R")
# 2- READ ALL INPUT FILES
source("ReadFiles2.R")
ReadFiles()
# 3- DEFINE SWITCHES AND DEFAULTS
source("Switches2.R")
ReadSwitches()
# 4- CREATE DATAFRAMES
source("dataframes.R")
# 5- LOAD PMFs
source("PMFs2.R")
# 6- OUTPUT FILE
OutputFolder=GlobalFile[4,2]

#######################################
#                                     #
#                                     #
#          WEEKLY TIME-STEP           #
#                                     #
#                                     #
#######################################

print(paste("initialization"))
I_Week = 1
week_counter = 1
no_week_in_year = week_counter_in_year()
WeekToMonth = read.table("inputs/WeekToMonth.txt", header = T)
month_in_year =  WeekToMonth[no_week_in_year,2]
year_counter = year_from_weekly()
###### READ INPUT DATA FOR EACH WEEK
source("VIC_Data2.R")
VIC_Data()
############### Common weekly variables
BRPrelim_c = -9999
TotalFloodSpace_c = -9999
ARFirmEngSupReq_c = -9999
ARFirmEngSup_c = -9999
TotalEnergyContent_c = -9999
TotalECCEnergyContent_c = -9999
FirmEnergyDeficit_c = -9999
TotalCoordPreEnergy_c = -9999
TotalNFEnergyContent_c = -9999
NonFirmEnergyDeficit_c = -9999
TotalMcNarySharedWater_c = -9999		
if (I_Week==1) {
############################ initialize reservoirs
	reservoir_vol_df[1,1] = MicaReservoir()
	reservoir_vol_df[1,2] = ArrowReservoir()
	reservoir_vol_df[1,3] = Duncan()
	reservoir_vol_df[1,4] = CorraLinnReservoir()
	reservoir_vol_df[1,5] = Libby()
	reservoir_vol_df[1,6] = HungryHorse()
	reservoir_vol_df[1,7] = GrandCoulee()
	reservoir_vol_df[1,8] = Dworshak()
	reservoir_vol_df[1,9] = Brownlee()
	reservoir_vol_df[1,10] = UpSnakeComb()
	reservoir_vol_df[1,11] = MdlSnakeComb()
}

MicaRelease_c = MIRelease()
dams_in[I_Week,1] = MIInflow()
dams_out[I_Week,1] = MIOutflow()
other_curtailments[I_Week,1] = MICurtail()

dams_in[I_Week,2] = REVIn()
dams_out[I_Week,2] = REVOut()
other_curtailments[I_Week,2] = REVCurtail()

ARRelease_c = ARRelease()
dams_in[I_Week,3] = ARInflow()
dams_out[I_Week,3] = AROutflow()
other_curtailments[I_Week,3] = ARCurtail()

DUOutflow_c = DURelease() 
dams_in[I_Week,4] = DUInflow()
dams_out[I_Week,4] = DUOutflow_c
other_curtailments[I_Week,4] = DUCurtail()

dams_in[I_Week,5] = LBInflow()
dams_out[I_Week,5] = LBOutflow()
other_curtailments[I_Week,5] = LBCurtail()

BonnersFerry_c = BonnersFerry() 
dams_in[I_Week,6] = BonnersFerry_c
dams_out[I_Week,6] = BonnersFerry_c
other_curtailments[I_Week,6] = BONFCurtail()

CLRelease_c = CLRelease() 
dams_in[I_Week,7] = CLInflow()
dams_out[I_Week,7] = CLOutflow()
other_curtailments[I_Week,7] = CLCurtail()

HHRelease_c = HHRelease()
dams_in[I_Week,8] = HHInflow()
dams_out[I_Week,8] = HHOutflow()
other_curtailments[I_Week,8] = HHCurtail()

KERelease_c = KERelease()
dams_in[I_Week,9] = KEInflow()
dams_out[I_Week,9] = KEOutflow()
other_curtailments[I_Week,9] = KECurtail()

dams_in[I_Week,10] = NOXIn()
dams_out[I_Week,10] = NOXOut()
other_curtailments[I_Week,10] = NOXCurtail()

dams_in[I_Week,11] = CBIn()
dams_out[I_Week,11] = CBOut()
other_curtailments[I_Week,11] = CBCurtail()

AFRelease_c = AFRelease()
dams_in[I_Week,12] = AFInflow()
dams_out[I_Week,12] = AFOutflow()
other_curtailments[I_Week,12] = AFCurtail()

dams_in[I_Week,13] = BDIn()
dams_out[I_Week,13] = BDOut()
other_curtailments[I_Week,13] = BDCurtail()

GCRelease_c = GCRelease()
dams_in[I_Week,14] = GCInflow()
dams_out[I_Week,14] = GCOutflow()
other_curtailments[I_Week,14] = GCCurtail()

dams_in[I_Week,15] = CJIn()
dams_out[I_Week,15] = CJOut()
mainstem_curtailments[I_Week,1] = CJCurtail()
mainstem_shortfall[I_Week,1] = CJInstreamShortfall()

dams_in[I_Week,16] = WEIn()
dams_out[I_Week,16] = WEOut()
mainstem_curtailments[I_Week,2] = WECurtail()
mainstem_shortfall[I_Week,2] = WEInstreamShortfall()

dams_in[I_Week,17] = RRIn()
dams_out[I_Week,17] = RROut()
mainstem_curtailments[I_Week,3] = RRCurtail()
mainstem_shortfall[I_Week,3] = RRInstreamShortfall()

dams_in[I_Week,18] = RIIn()
dams_out[I_Week,18] = RIOut()
mainstem_curtailments[I_Week,4] = RICurtail()
mainstem_shortfall[I_Week,4] = RIInstreamShortfall()

dams_in[I_Week,19] = WAIn()
dams_out[I_Week,19] = WAOut()
mainstem_curtailments[I_Week,5] = WACurtail()
mainstem_shortfall[I_Week,5] = WAInstreamShortfall()

dams_in[I_Week,20] = PRIn()
dams_out[I_Week,20] = PROut()
mainstem_curtailments[I_Week,6] = PRCurtail()
mainstem_shortfall[I_Week,6] = PRInstreamShortfall()
	
dams_in[I_Week,21] = USInflow()
dams_out[I_Week,21] = USOutflow()
other_curtailments[I_Week,15] = PALCurtail()

dams_in[I_Week,22] = MSInflow()
dams_out[I_Week,22] = MSOutflow()
other_curtailments[I_Week,16] = MILCurtail()

BRRelease_c = BRRelease()
dams_in[I_Week,23] = BRInflow()
dams_out[I_Week,23] = BROutflow()
other_curtailments[I_Week,17] = BRCurtail()

dams_in[I_Week,24] = OXIn()
dams_out[I_Week,24] = OXOut()
other_curtailments[I_Week,18] = OXCurtail()

dams_in[I_Week,25] = HCIn()
dams_out[I_Week,25] = HCOut()
other_curtailments[I_Week,19] = HCCurtail()

DWRelease_c = DWRelease()
dams_in[I_Week,26] = DWInflow()
dams_out[I_Week,26] = DWOutflow()
other_curtailments[I_Week,20] = DWCurtail()

dams_in[I_Week,27] = LGIn()
dams_out[I_Week,27] = LGOut()	
other_curtailments[I_Week,21] = LGCurtail()

dams_in[I_Week,28] = LIGIn()
dams_out[I_Week,28] = LIGOut()
other_curtailments[I_Week,22] = LIGCurtail()

dams_in[I_Week,29] = LMIn()
dams_out[I_Week,29] = LMOut()
other_curtailments[I_Week,23] = LMCurtail()

dams_in[I_Week,30] = IHIn()
dams_out[I_Week,30] = IHOut()
other_curtailments[I_Week,24] = IHCurtail()

dams_in[I_Week,31] = MCNIn()
dams_out[I_Week,31] = MCNOut()
mainstem_curtailments[I_Week,7] = MCNCurtail()
mainstem_shortfall[I_Week,7] = MCNInstreamShortfall()
Biop[I_Week,1] = McNaryFlowTarget()

dams_in[I_Week,32] = JDIn()
dams_out[I_Week,32] = JDOut()
mainstem_curtailments[I_Week,8] = JDCurtail()
mainstem_shortfall[I_Week,8] = JDInstreamShortfall()

dams_in[I_Week,33] = DAIn()
dams_out[I_Week,33] = DAOut()
mainstem_curtailments[I_Week,9] = DACurtail()
mainstem_shortfall[I_Week,9] = DAInstreamShortfall()

dams_in[I_Week,34] = BONIn()
dams_out[I_Week,34] = BONOut()
other_curtailments[I_Week,25] = BONCurtail()
Biop[I_Week,2] = BONTarget_AcFt()

######### STORAGE FOR THE SECOND TIME STEP
reservoir_vol_df[1,1] = reservoir_vol_df[1,1] + (dams_in[1,1] - dams_out[1,1]) # MICAA RESERVOIR
reservoir_vol_df[1,2] = reservoir_vol_df[1,2] + (dams_in[1,3] - dams_out[1,3]) # ARROW RESERVOIR
reservoir_vol_df[1,3] = reservoir_vol_df[1,3] + (dams_in[1,4] - dams_out[1,4]) # DUNCAN RESERVOIR
reservoir_vol_df[1,4] = reservoir_vol_df[1,4] + (dams_in[1,7] - dams_out[1,7]) # CORRA LINN RESERVOIR 
reservoir_vol_df[1,5] = reservoir_vol_df[1,5] + (dams_in[1,5] - dams_out[1,5]) # LIBBY RESERVOIR
reservoir_vol_df[1,6] = reservoir_vol_df[1,6] + (dams_in[1,8] - dams_out[1,8]) # HUNGRY HORSE RESERVOIR
reservoir_vol_df[1,7] = reservoir_vol_df[1,7] + (dams_in[1,14] - dams_out[1,14]) # GRAND COULEE RESERVOIR
reservoir_vol_df[1,8] = reservoir_vol_df[1,8] + (dams_in[1,26] - dams_out[1,26]) # DWORSHAK RESERVOIR
reservoir_vol_df[1,9] = reservoir_vol_df[1,9] + (dams_in[1,23] - dams_out[1,23]) # BROWNLEE RESERVOIR
reservoir_vol_df[1,10] = reservoir_vol_df[1,10] + (dams_in[1,21] - dams_out[1,21]) # UPPER SNAKE COMPOSITE RESERVOIR
reservoir_vol_df[1,11] = reservoir_vol_df[1,11] + (dams_in[1,22] - dams_out[1,22]) # MIDDLE SNAKE COMPOSITE RESERVOIR

###### MOPs Measures Of Performance
MOP_df[I_Week,1] = shortfall() # Firm Energy Performance Metrics ==> if(shortfall()>0.001) --> does not meet target
MOP_df[I_Week,2] = shortfall_2() # Non-Firm Energy Performance Metric ==> if(shortfall_2()>0.001) --> does not meet target
MOP_df[I_Week,3] = shortfall_5() # Columbia Falls Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
MOP_df[I_Week,4] = shortfall_6() # Lower Granite Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
MOP_df[I_Week,5] = shortfall_7() # Vernita Bar Flow Target Metrics ==> if(shortfall_7()>0.001) --> does not meet target
MOP_df[I_Week,6] = shortfall_8() # McNary Flow Target Metrics ==> if(shortfall_8()>0.001) --> does not meet target
MOP_df[I_Week,7] = shortfall_9() # Grand Coulee Recreation Metrics ==> if greater than zero "0" does not meet target flow
MOP_df[I_Week,8] = shortfall_10() # Dalles Flood Protection Metrics ==> if greater than zero "0" does not meet target flow
MOP_df[I_Week,9] = shortfall_11() # Ice Harbor Flow Metrics ==> if(shortfall_11()>0.001) --> does not meet target
MOP_df[I_Week,10] = ChumSF() # Bonneville Winter Chum Flow Metrics ==> if greater than zero "0" does not meet target flow
MOP_df[I_Week,11] = BelowFCC() # Flood Pool below  curve
MOP_df[I_Week,12] = FirmEnergySales()
MOP_df[I_Week,13] = NonFirmSpotSales()
MOP_df[I_Week,14] = MaxSystemEnergy()

write.table(data.frame(matrix(names(dams_out), nrow=1)), paste(OutputFolder, "dams_out.txt", sep=""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(dams_in), nrow=1)), paste(OutputFolder, "dams_in.txt", sep=""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(reservoir_vol_df), nrow=1)), paste(OutputFolder, "reservoir_volume.txt", sep=""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(water_df), nrow=1)), paste(OutputFolder, "water.txt", sep = ""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(energy_df), nrow=1)), paste(OutputFolder, "energy.txt", sep = ""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(mainstem_curtailments), nrow=1)), paste(OutputFolder, "mainstem_curtailment.txt", sep=""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(MOP_df), nrow=1)), paste(OutputFolder, "MOP_df.txt", sep=""), row.names=F, col.names=F, append=F)
write.table(data.frame(matrix(names(other_curtailments), nrow=1)), paste(OutputFolder, "other_curtailment.txt", sep=""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(mainstem_shortfall), nrow=1)), paste(OutputFolder, "mainstem_shortfall.txt", sep=""), row.names=F, col.names=F, quote=F)
write.table(data.frame(matrix(names(Biop), nrow=1)), paste(OutputFolder, "Biop_flow.txt", sep=""), row.names=F, col.names=F, quote=F)

write.table(dams_out[1,], paste(OutputFolder, "dams_out.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(dams_in[1,], paste(OutputFolder, "dams_in.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(reservoir_vol_df[1,], paste(OutputFolder, "reservoir_volume.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(MOP_df[1,], paste(OutputFolder, "MOP_df.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(water_df[1,], paste(OutputFolder, "water.txt", sep = ""), row.names=F, col.names=F, append=T)
write.table(energy_df[1,], paste(OutputFolder, "energy.txt", sep = ""), row.names=F, col.names=F, append=T)
write.table(mainstem_curtailments[1,], paste(OutputFolder, "mainstem_curtailment.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(other_curtailments[1,], paste(OutputFolder, "other_curtailment.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(mainstem_shortfall[1,], paste(OutputFolder, "mainstem_shortfall.txt", sep=""), row.names=F, col.names=F, append=T)
write.table(Biop[1,], paste(OutputFolder, "Biop_flow.txt", sep=""), row.names=F, col.names=F, append=T)

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

for (I_Week in 2:N_of_TimeSteps){
	initial_ts=1
	print(paste("time step=", I_Week))
	# FIND THE RIGHT TIME STEP
	week_counter = I_Week
	no_week_in_year = week_counter_in_year()
	WeekToMonth = read.table("inputs/WeekToMonth.txt", header = T)
	month_in_year =  WeekToMonth[no_week_in_year,2]
	year_counter = year_from_weekly()
  
	###### READ INPUT DATA FOR EACH WEEK
	VIC_Data()
	############### COMMON WEEKLY VARIABLES
	BRPrelim_c = -9999
	TotalFloodSpace_c = -9999
	ARFirmEngSupReq_c = -9999
	ARFirmEngSup_c = -9999
	TotalEnergyContent_c = -9999
	TotalECCEnergyContent_c = -9999
	FirmEnergyDeficit_c = -9999
	TotalCoordPreEnergy_c = -9999
	TotalNFEnergyContent_c = -9999
	NonFirmEnergyDeficit_c = -9999
	TotalMcNarySharedWater_c = -9999
	
	MicaRelease_c = MIRelease()
	dams_in[I_Week,1] = MIInflow()
	dams_out[I_Week,1] = MIOutflow()
	other_curtailments[I_Week,1] = MICurtail()

	dams_in[I_Week,2] = REVIn()
	dams_out[I_Week,2] = REVOut()
	other_curtailments[I_Week,2] = REVCurtail()

	ARRelease_c = ARRelease()
	dams_in[I_Week,3] = ARInflow()
	dams_out[I_Week,3] = AROutflow()
	other_curtailments[I_Week,3] = ARCurtail()

	DUOutflow_c = DURelease() 
	dams_in[I_Week,4] = DUInflow()
	dams_out[I_Week,4] = DUOutflow_c
	other_curtailments[I_Week,4] = DUCurtail()

	dams_in[I_Week,5] = LBInflow()
	dams_out[I_Week,5] = LBOutflow()
	other_curtailments[I_Week,5] = LBCurtail()

	BonnersFerry_c = BonnersFerry() 
	dams_in[I_Week,6] = BonnersFerry_c
	dams_out[I_Week,6] = BonnersFerry_c
	other_curtailments[I_Week,6] = BONFCurtail()

	CLRelease_c = CLRelease() 
	dams_in[I_Week,7] = CLInflow()
	dams_out[I_Week,7] = CLOutflow()
	other_curtailments[I_Week,7] = CLCurtail()

	HHRelease_c = HHRelease()
	dams_in[I_Week,8] = HHInflow()
	dams_out[I_Week,8] = HHOutflow()
	other_curtailments[I_Week,8] = HHCurtail()

	KERelease_c = KERelease()
	dams_in[I_Week,9] = KEInflow()
	dams_out[I_Week,9] = KEOutflow()
	other_curtailments[I_Week,9] = KECurtail()

	dams_in[I_Week,10] = NOXIn()
	dams_out[I_Week,10] = NOXOut()
	other_curtailments[I_Week,10] = NOXCurtail()

	dams_in[I_Week,11] = CBIn()
	dams_out[I_Week,11] = CBOut()
	other_curtailments[I_Week,11] = CBCurtail()

	AFRelease_c = AFRelease()
	dams_in[I_Week,12] = AFInflow()
	dams_out[I_Week,12] = AFOutflow()
	other_curtailments[I_Week,12] = AFCurtail()

	dams_in[I_Week,13] = BDIn()
	dams_out[I_Week,13] = BDOut()
	other_curtailments[I_Week,13] = BDCurtail()

	GCRelease_c = GCRelease()
	dams_in[I_Week,14] = GCInflow()
	dams_out[I_Week,14] = GCOutflow()
	other_curtailments[I_Week,14] = GCCurtail()

	dams_in[I_Week,15] = CJIn()
	dams_out[I_Week,15] = CJOut()
	mainstem_curtailments[I_Week,1] = CJCurtail()
	mainstem_shortfall[I_Week,1] = CJInstreamShortfall()

	dams_in[I_Week,16] = WEIn()
	dams_out[I_Week,16] = WEOut()
	mainstem_curtailments[I_Week,2] = WECurtail()
	mainstem_shortfall[I_Week,2] = WEInstreamShortfall()

	dams_in[I_Week,17] = RRIn()
	dams_out[I_Week,17] = RROut()
	mainstem_curtailments[I_Week,3] = RRCurtail()
	mainstem_shortfall[I_Week,3] = RRInstreamShortfall()

	dams_in[I_Week,18] = RIIn()
	dams_out[I_Week,18] = RIOut()
	mainstem_curtailments[I_Week,4] = RICurtail()
	mainstem_shortfall[I_Week,4] = RIInstreamShortfall()

	dams_in[I_Week,19] = WAIn()
	dams_out[I_Week,19] = WAOut()
	mainstem_curtailments[I_Week,5] = WACurtail()
	mainstem_shortfall[I_Week,5] = WAInstreamShortfall()

	dams_in[I_Week,20] = PRIn()
	dams_out[I_Week,20] = PROut()
	mainstem_curtailments[I_Week,6] = PRCurtail()
	mainstem_shortfall[I_Week,6] = PRInstreamShortfall()
		
	dams_in[I_Week,21] = USInflow()
	dams_out[I_Week,21] = USOutflow()
	other_curtailments[I_Week,15] = PALCurtail()

	dams_in[I_Week,22] = MSInflow()
	dams_out[I_Week,22] = MSOutflow()
	other_curtailments[I_Week,16] = MILCurtail()

	BRRelease_c = BRRelease()
	dams_in[I_Week,23] = BRInflow()
	dams_out[I_Week,23] = BROutflow()
	other_curtailments[I_Week,17] = BRCurtail()

	dams_in[I_Week,24] = OXIn()
	dams_out[I_Week,24] = OXOut()
	other_curtailments[I_Week,18] = OXCurtail()

	dams_in[I_Week,25] = HCIn()
	dams_out[I_Week,25] = HCOut()
	other_curtailments[I_Week,19] = HCCurtail()

	DWRelease_c = DWRelease()
	dams_in[I_Week,26] = DWInflow()
	dams_out[I_Week,26] = DWOutflow()
	other_curtailments[I_Week,20] = DWCurtail()

	dams_in[I_Week,27] = LGIn()
	dams_out[I_Week,27] = LGOut()	
	other_curtailments[I_Week,21] = LGCurtail()

	dams_in[I_Week,28] = LIGIn()
	dams_out[I_Week,28] = LIGOut()
	other_curtailments[I_Week,22] = LIGCurtail()

	dams_in[I_Week,29] = LMIn()
	dams_out[I_Week,29] = LMOut()
	other_curtailments[I_Week,23] = LMCurtail()

	dams_in[I_Week,30] = IHIn()
	dams_out[I_Week,30] = IHOut()
	other_curtailments[I_Week,24] = IHCurtail()

	dams_in[I_Week,31] = MCNIn()
	dams_out[I_Week,31] = MCNOut()
	mainstem_curtailments[I_Week,7] = MCNCurtail()
	mainstem_shortfall[I_Week,7] = MCNInstreamShortfall()
	Biop[I_Week,1] = McNaryFlowTarget()

	dams_in[I_Week,32] = JDIn()
	dams_out[I_Week,32] = JDOut()
	mainstem_curtailments[I_Week,8] = JDCurtail()
	mainstem_shortfall[I_Week,8] = JDInstreamShortfall()

	dams_in[I_Week,33] = DAIn()
	dams_out[I_Week,33] = DAOut()
	mainstem_curtailments[I_Week,9] = DACurtail()
	mainstem_shortfall[I_Week,9] = DAInstreamShortfall()

	dams_in[I_Week,34] = BONIn()
	dams_out[I_Week,34] = BONOut()
	other_curtailments[I_Week,25] = BONCurtail()
	Biop[I_Week,2] = BONTarget_AcFt()

	reservoir_vol_df[I_Week,1] = reservoir_vol_df[I_Week-1,1] + (dams_in[I_Week,1] - dams_out[I_Week,1]) # MICAA RESERVOIR
	reservoir_vol_df[I_Week,2] = reservoir_vol_df[I_Week-1,2] + (dams_in[I_Week,3] - dams_out[I_Week,3]) # ARROW RESERVOIR
	reservoir_vol_df[I_Week,3] = reservoir_vol_df[I_Week-1,3] + (dams_in[I_Week,4] - dams_out[I_Week,4]) # DUNCAN RESERVOIR
	reservoir_vol_df[I_Week,4] = reservoir_vol_df[I_Week-1,4] + (dams_in[I_Week,7] - dams_out[I_Week,7]) # CORRA LINN RESERVOIR 
	reservoir_vol_df[I_Week,5] = reservoir_vol_df[I_Week-1,5] + (dams_in[I_Week,5] - dams_out[I_Week,5]) # LIBBY RESERVOIR
	reservoir_vol_df[I_Week,6] = reservoir_vol_df[I_Week-1,6] + (dams_in[I_Week,8] - dams_out[I_Week,8]) # HUNGRY HORSE RESERVOIR
	reservoir_vol_df[I_Week,7] = reservoir_vol_df[I_Week-1,7] + (dams_in[I_Week,14] - dams_out[I_Week,14]) # GRAND COULEE RESERVOIR
	reservoir_vol_df[I_Week,8] = reservoir_vol_df[I_Week-1,8] + (dams_in[I_Week,26] - dams_out[I_Week,26]) # DWORSHAK RESERVOIR
	reservoir_vol_df[I_Week,9] = reservoir_vol_df[I_Week-1,9] + (dams_in[I_Week,23] - dams_out[I_Week,23]) # BROWNLEE RESERVOIR
	reservoir_vol_df[I_Week,10] = reservoir_vol_df[I_Week-1,10] + (dams_in[I_Week,21] - dams_out[I_Week,21]) # UPPER SNAKE COMPOSITE RESERVOIR
	reservoir_vol_df[I_Week,11] = reservoir_vol_df[I_Week-1,11] + (dams_in[I_Week,22] - dams_out[I_Week,22]) # MIDDLE SNAKE COMPOSITE RESERVOIR
	if (dams_out[I_Week,5]==0) {
		stop
	}
	###### MOPs Measures Of Performance
	MOP_df[I_Week,1] = shortfall() # Firm Energy Performance Metrics ==> if(shortfall()>0.001) --> does not meet target
	MOP_df[I_Week,2] = shortfall_2() # Non-Firm Energy Performance Metric ==> if(shortfall_2()>0.001) --> does not meet target
	MOP_df[I_Week,3] = shortfall_5() # Columbia Falls Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
	MOP_df[I_Week,4] = shortfall_6() # Lower Granite Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
	MOP_df[I_Week,5] = shortfall_7() # Vernita Bar Flow Target Metrics ==> if(shortfall_7()>0.001) --> does not meet target
	MOP_df[I_Week,6] = shortfall_8() # McNary Flow Target Metrics ==> if(shortfall_8()>0.001) --> does not meet target
	MOP_df[I_Week,7] = shortfall_9() # Grand Coulee Recreation Metrics ==> if greater than zero "0" does not meet target flow
	MOP_df[I_Week,8] = shortfall_10() # Dalles Flood Protection Metrics ==> if greater than zero "0" does not meet target flow
	MOP_df[I_Week,9] = shortfall_11() # Ice Harbor Flow Metrics ==> if(shortfall_11()>0.001) --> does not meet target
	MOP_df[I_Week,10] = ChumSF() # Bonneville Winter Chum Flow Metrics ==> if greater than zero "0" does not meet target flow
	MOP_df[I_Week,11] = BelowFCC() # Flood Pool below  curve
	MOP_df[I_Week,12] = FirmEnergySales()
	MOP_df[I_Week,13] = NonFirmSpotSales()
	MOP_df[I_Week,14] = MaxSystemEnergy()
  
	print(paste("reached here to the Bonnevile"))
	#print(paste("BR IN=", water_df[week_counter,4]))
	#print(paste("Scenario is:",scr))
	print(Sys.time())
	print(paste("---------------------------------------------------------"))
	
	write.table(dams_out[I_Week,], paste(OutputFolder, "dams_out.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(dams_in[I_Week,], paste(OutputFolder, "dams_in.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(reservoir_vol_df[I_Week,], paste(OutputFolder, "reservoir_volume.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(MOP_df[I_Week,], paste(OutputFolder, "MOP_df.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(water_df[I_Week,], paste(OutputFolder, "water.txt", sep = ""), row.names=F, col.names=F, append=T)
	write.table(energy_df[I_Week,], paste(OutputFolder, "energy.txt", sep = ""), row.names=F, col.names=F, append=T)
	write.table(mainstem_curtailments[I_Week,], paste(OutputFolder, "mainstem_curtailment.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(other_curtailments[I_Week,], paste(OutputFolder, "other_curtailment.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(mainstem_shortfall[I_Week,], paste(OutputFolder, "mainstem_shortfall.txt", sep=""), row.names=F, col.names=F, append=T)
	write.table(Biop[I_Week,], paste(OutputFolder, "Biop_flow.txt", sep=""), row.names=F, col.names=F, append=T)
}

