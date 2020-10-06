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
scr = commandArgs(trailingOnly=TRUE)
#scr="Historical_baseline"
print(paste("Now doing scenario:", scr))

run_type = "Supply_and_demand"

GlobalFile = read.table(paste("~/Step_5/RColSim/inputs/", run_type, "/", "GIF_", scr, sep=""), stringsAsFactors=FALSE)	# reads the global input file
N_of_TimeSteps = as.numeric(as.character(GlobalFile[3,2]))	
input_file = read.table(GlobalFile[2,2], header=TRUE)
setwd(GlobalFile[1,2])
# 1- LOAD ALL FUNCTIONS
source("LoadFunctions2.R")
# 2- READ ALL INPUT FILES
source("ReadFiles2.R")
ReadFiles()
# 3- DEFINE SWITCHES AND DEFAULTS
source("Switches.R")
ReadSwitches()
# 4- CREATE DATAFRAMES
source("dataframes.R")
# 5- LOAD PMFs
source("PMFs.R")
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
	
if (I_Week==1){
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
dams_in[I_Week,1] = MicaInflow()
dams_out[I_Week,1] = MicaOutflow()

dams_in[I_Week,2] = RevIn()
dams_out[I_Week,2] = RevOut()

ArrowRelease_c = ArrowRelease()
dams_out[I_Week,3] = ArrowOutflow()
dams_in[I_Week,3] = ArrowInflow()

DuncanOutflow_c = DuncanRelease() #check
dams_in[I_Week,4] = DuncanInflow()
dams_out[I_Week,4] = DuncanOutflow_c

dams_out[I_Week,5] = LibbyOutflow()
dams_in[I_Week,5] = LibbyInflow()

BonnersFerry_c = BonnersFerry() # check

CLRelease_c = CLRelease() #check
dams_in[I_Week,6] = CLInflow()
dams_out[I_Week,6] = CLOutflow()

dams_out[I_Week,7] = Canada_Outflows()

HHRelease_c = HHRelease()
dams_out[I_Week,8] = HHOutflow()
dams_in[I_Week,7] = HHInflow()

KerrRelease_c = KerrRelease()
dams_out[I_Week,9] = KerrOutflow()

dams_out[I_Week,10] = NoxOut()

dams_out[I_Week,11] = CBOut()

AFRelease_c = AFRelease()
dams_out[I_Week,12] = AFOutflow()

dams_out[I_Week,13] = BoundOut()

GCRelease_c = GCRelease()
dams_out[I_Week,14] = GCOutflow()
dams_in[I_Week,13] = GCInflow()

dams_out[I_Week,15] = CJOut()
mainstem_curtailments[I_Week,1] = CJCurtail()
dams_out[I_Week,16] = WeOut()
mainstem_curtailments[I_Week,2] = WECurtail()
dams_out[I_Week,17] = RROut()
mainstem_curtailments[I_Week,3] = RRCurtail()
dams_out[I_Week,18] = RIOut()
mainstem_curtailments[I_Week,4] = RICurtail()
dams_out[I_Week,19] = WaOut()
mainstem_curtailments[I_Week,5] = WACurtail()
dams_out[I_Week,20] = PROut()
mainstem_curtailments[I_Week,6] = PRCurtail()
	
dams_in[I_Week,33] = USInflow()
dams_out[I_Week,21] = UpSnOutflow()

dams_in[I_Week,34] = MSInflow()
dams_out[I_Week,34] = MSOutflow()

BRRelease_c = BRRelease()
dams_out[I_Week,22] = BROutflow()
dams_in[I_Week,24] = BRInflow()

dams_out[I_Week,23] = OXOut()
	
dams_out[I_Week,24] = HCOut()

DWRelease_c = DWRelease()
dams_out[I_Week,25] = DWOutflow()
dams_in[I_Week,23] = DWInflow()

dams_out[I_Week,26] = LGOut()	
dams_out[I_Week,27] = LiGOut()
dams_out[I_Week,28] = LMOut()
dams_out[I_Week,29] = IHOut()
dams_out[I_Week,30] = McNOut()
mainstem_curtailments[I_Week,7] = MCNCurtail()
dams_out[I_Week,31] = JDOut()
mainstem_curtailments[I_Week,8] = JDCurtail()
dams_out[I_Week,32] = DaOut()
mainstem_curtailments[I_Week,9] = DACurtail()
dams_out[I_Week,33] = BONOut()

######### STORAGE FOR THE SECOND TIME STEP
reservoir_vol_df[1,1] = reservoir_vol_df[1,1] + (dams_in[1,1] - dams_out[1,1]) - MicaNetWith()
reservoir_vol_df[1,2] = reservoir_vol_df[1,2] + (dams_in[1,3] - dams_out[1,3]) - ArrowNetWith()
reservoir_vol_df[1,3] = reservoir_vol_df[1,3] + (dams_in[1,4] - dams_out[1,4]) - DuncanNetWith()
reservoir_vol_df[1,5] = reservoir_vol_df[1,5] + (dams_in[1,5] - dams_out[1,5]) - LibbyNetWith()
reservoir_vol_df[1,4] = reservoir_vol_df[1,4] + (dams_in[1,6] - dams_out[1,6]) - CLNetWith()
reservoir_vol_df[1,6] = reservoir_vol_df[1,6] + (dams_in[1,7] - dams_out[1,8]) - HHNetWith()
reservoir_vol_df[1,7] = reservoir_vol_df[1,7] + (dams_in[1,13] - dams_out[1,14]) - GCNetWith()
reservoir_vol_df[1,10] = reservoir_vol_df[1,10] + (dams_in[1,33] - dams_out[1,21])
reservoir_vol_df[1,11] = reservoir_vol_df[1,11] + (dams_in[1,34]-dams_out[1,34])
reservoir_vol_df[1,9] = reservoir_vol_df[1,9] + (dams_in[1,24] - dams_out[1,22]) - BRNetWith()
reservoir_vol_df[1,8] = reservoir_vol_df[1,8] + (dams_in[1,23] - dams_out[1,25]) - DWNetWith()
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
	
	
	MicaRelease_c=MIRelease()
	dams_in[I_Week,1]=MicaInflow()
	dams_out[I_Week,1]=MicaOutflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,1] = reservoir_vol_df[I_Week-1,1] + (dams_in[I_Week,1] - dams_out[I_Week,1]) - MicaNetWith()
	}
	
	dams_in[I_Week,2] = RevIn()
	dams_out[I_Week,2] = RevOut()
  
	ArrowRelease_c = ArrowRelease()
	dams_out[I_Week,3] = ArrowOutflow()
	dams_in[I_Week,3] = ArrowInflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,2] = reservoir_vol_df[I_Week-1,2] + (dams_in[I_Week,3] - dams_out[I_Week,3]) - ArrowNetWith()
	}
  
	DuncanOutflow_c = DuncanRelease() #check
	dams_in[I_Week,4] = DuncanInflow()
    dams_out[I_Week,4] = DuncanOutflow_c
	if(I_Week>1){
		reservoir_vol_df[I_Week,3] = reservoir_vol_df[I_Week-1,3] + (dams_in[I_Week,4] - dams_out[I_Week,4]) - DuncanNetWith()
	}
		
	dams_out[I_Week,5] = LibbyOutflow()
	dams_in[I_Week,5] = LibbyInflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,5] = reservoir_vol_df[I_Week-1,5] + (dams_in[I_Week,5] - dams_out[I_Week,5]) - LibbyNetWith()
	}
		
	#### BonnersFerryFlowData
	BonnersFerry_c = BonnersFerry() 
		
	CLRelease_c = CLRelease() 
	dams_in[I_Week,6] = CLInflow()
	dams_out[I_Week,6] = CLOutflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,4] = reservoir_vol_df[I_Week-1,4] + (dams_in[I_Week,6] - dams_out[I_Week,6]) - CLNetWith()
	}

	dams_out[I_Week,7] = Canada_Outflows()
  
	# Hungry Horse -----------------------------------------------------------
	HHRelease_c = HHRelease()
	dams_out[I_Week,8] = HHOutflow()
	dams_in[I_Week,7] = HHInflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,6] = reservoir_vol_df[I_Week-1,6] + (dams_in[I_Week,7] - dams_out[I_Week,8]) - HHNetWith()
	}
  
	KerrRelease_c = KerrRelease()
	dams_out[I_Week,9] = KerrOutflow()
  
	dams_out[I_Week,10] = NoxOut()
	dams_out[I_Week,11] = CBOut()
  
	AFRelease_c = AFRelease()
	dams_out[I_Week,12]=AFOutflow()
	dams_out[I_Week,13] = BoundOut()
  
	####### GCInflow
	GCRelease_c = GCRelease()
	dams_out[I_Week,14] = GCOutflow()
	dams_in[I_Week,13] = GCInflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,7] = reservoir_vol_df[I_Week-1,7] + (dams_in[I_Week,13] - dams_out[I_Week,14]) - GCNetWith()
	}
	
	dams_out[I_Week,15] = CJOut()
	mainstem_curtailments[I_Week,1] = CJCurtail()
	dams_out[I_Week,16] = WeOut()
	mainstem_curtailments[I_Week,2] = WECurtail()
	dams_out[I_Week,17] = RROut()
	mainstem_curtailments[I_Week,3] = RRCurtail()
	dams_out[I_Week,18] = RIOut()
	mainstem_curtailments[I_Week,4] = RICurtail()
	dams_out[I_Week,19] = WaOut()
	mainstem_curtailments[I_Week,5] = WACurtail()
	dams_out[I_Week,20] = PROut()
	mainstem_curtailments[I_Week,6] = PRCurtail()
	
	# Snake River -------------------------------------------------------------
	###### UpperSnakeRelease
	dams_in[I_Week,33] = USInflow()
	dams_out[I_Week,21] = UpSnOutflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,10] = reservoir_vol_df[I_Week-1,10] + (dams_in[I_Week,33] - dams_out[I_Week,21])
	}
		
	########### Middle Snake
	dams_in[I_Week,34] = MSInflow()
	dams_out[I_Week,34] = MSOutflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,11] = reservoir_vol_df[I_Week-1,11] + (dams_in[I_Week,34] - dams_out[I_Week,34])
	}
		
	BRRelease_c = BRRelease()
	dams_out[I_Week,22] = BROutflow()
	dams_in[I_Week,24] = BRInflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,9] = reservoir_vol_df[I_Week-1,9] + (dams_in[I_Week,24] - dams_out[I_Week,22]) - BRNetWith()
	}
  
	dams_out[I_Week,23] = OXOut()
	dams_out[I_Week,24] = HCOut()
	
	DWRelease_c = DWRelease()
	dams_out[I_Week,25] = DWOutflow()
	dams_in[I_Week,23] = DWInflow()
	if(I_Week>1){
		reservoir_vol_df[I_Week,8] = reservoir_vol_df[I_Week-1,8] + (dams_in[I_Week,23] - dams_out[I_Week,25]) - DWNetWith()
	}
	
	dams_out[I_Week,26] = LGOut()	
	dams_out[I_Week,27] = LiGOut()
	dams_out[I_Week,28] = LMOut()
	dams_out[I_Week,29] = IHOut()
	dams_out[I_Week,30] = McNOut()
	mainstem_curtailments[I_Week,7] = MCNCurtail()
	dams_out[I_Week,31] = JDOut()
	mainstem_curtailments[I_Week,8] = JDCurtail()
	dams_out[I_Week,32] = DaOut()
	mainstem_curtailments[I_Week,9] = DACurtail()
	dams_out[I_Week,33] = BONOut()	
  
	###### MOPs Measures Of Performance
	MOP_df[I_Week,1] = shortfall() # Firm Energy Performance Metrics ==> if(shortfall()>0.001) --> does not meet target
	MOP_df[I_Week,2] = shortfall_2() # Non-Firm Energy Performance Metric ==> if(shortfall_2()>0.001) --> does not meet target
	MOP_df[I_Week,3] = shortfall_3() # Shortfall_3 # Upper Snake Agriculture Metrics ==>if greater than zero "0" does not meet target flow
	MOP_df[I_Week,4] = shortfall_4() # Shortfall_4 # Middle Snake Agriculture Metrics ==>if greater than zero "0" does not meet target flow
	MOP_df[I_Week,5] = shortfall_5() # Columbia Falls Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
	MOP_df[I_Week,6] = shortfall_6() # Lower Granite Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
	MOP_df[I_Week,7] = shortfall_7() # Vernita Bar Flow Target Metrics ==> if(shortfall_7()>0.001) --> does not meet target
	MOP_df[I_Week,8] = shortfall_8() # McNary Flow Target Metrics ==> if(shortfall_8()>0.001) --> does not meet target
	MOP_df[I_Week,9] = shortfall_9() # Grand Coulee Recreation Metrics ==> if greater than zero "0" does not meet target flow
	MOP_df[I_Week,10] = shortfall_10() # Dalles Flood Protection Metrics ==> if greater than zero "0" does not meet target flow
	MOP_df[I_Week,11] = shortfall_11() # Ice Harbor Flow Metrics ==> if(shortfall_11()>0.001) --> does not meet target
	MOP_df[I_Week,12] = ChumSF() # Bonneville Winter Chum Flow Metrics ==> if greater than zero "0" does not meet target flow
	MOP_df[I_Week,13] = BelowFCC() # Flood Pool below  curve
	MOP_df[I_Week,14] = FirmEnergySales()
	MOP_df[I_Week,15] = NonFirmSpotSales()
  
	print(paste("reached here to the Bonnevile"))
	#print(paste("BR IN=", water_df[week_counter,4]))
	#print(paste("Scenario is:",scr))
	print(Sys.time())
	print(paste("---------------------------------------------------------"))
}

write.table(dams_out, paste(OutputFolder, "dams_out.txt", sep="/"), row.names=F)
write.table(dams_in, paste(OutputFolder, "dams_in.txt", sep="/"), row.names=F)
write.table(reservoir_vol_df, paste(OutputFolder, "reservoir_volume.txt", sep="/"), row.names=F)
write.table(MOP_df, paste(OutputFolder, "MOP_df.txt", sep="/"), row.names=F)
write.table(water_df, paste(OutputFolder, "water.txt", sep = "/"), row.names=F)
write.table(energy_df, paste(OutputFolder, "energy.txt", sep = "/"), row.names=F)
write.table(mainstem_curtailments, paste(OutputFolder, "mainstem_curtailment.txt", sep="/"), row.names=F)

