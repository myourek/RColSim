                          ##################################################################################################################
                          ##################################################################################################################
                                                            ################################################
                                                            #                                              #
                                                            #                                              #
                                                            #                    R-ColSim                  #
                                                            #                                              #
                                                            #                                              #
                                                            ################################################
                          ##################################################################################################################
                          ##################################################################################################################                                                            
                                                            
                                                                #######################################
                                                                #                                     #
                                                                #         Principle Developers:       #
                                                                #                                     #
                                                                #           Keyvan Malek              #
                                                                #           Matthew Yourek            #
                                                                #                                     #
                                                                #######################################
                                                            
                                                              


# -------------------------------------------------------------------------------------------------------------------------------------------
#
#                                                    Read the global input file
#                                                            
# -------------------------------------------------------------------------------------------------------------------------------------------
                                                            



scr <- "Historical_baseline"

# -------- Read the global input file
print(paste0("Now doing scenario: ", scr))
GlobalFile <- read.table("~/RColSim/inputs/Global_Input_File_Historical_baseline.txt", stringsAsFactors<-F)


# -------------------------------------------------------------------------------------------------------------------------------------------
#
#                                                    Prepare input files and date time series for simulation
#                                                            
# -------------------------------------------------------------------------------------------------------------------------------------------

input_file <- read.table(GlobalFile[2,2], header<-T)
print(paste0("Reading: ", GlobalFile[2,2]))
setwd(GlobalFile[1,2])

datetxt_sim <- as.Date(paste0(input_file$month, "-", input_file$day, "-", input_file$year), format="%m-%d-%Y")
input_start_date <- datetxt_sim[1] # Current input file starts on "1979-08-05"
input_end_date <- datetxt_sim[length(datetxt_sim)] # Current input file ends on "2015-09-27"

# Input data time frame
date_hist_sim_0 <- data.frame(date=datetxt_sim,
	month=as.numeric(format(datetxt_sim, format="%m")),
    week=input_file$Week_Number, day=as.numeric(format(datetxt_sim, format="%d")),
    year=as.numeric(format(datetxt_sim, format="%Y")))

# Simulation start and end date
start_year <- GlobalFile[4,2]
simulation_start_date <- datetxt_sim[which(datetxt_sim > paste0(start_year, "-07-31"))[1]]
simulation_end_date <- GlobalFile[5,2] #"2015-09-27"

lines_to_keep <- which(datetxt_sim >= simulation_start_date & datetxt_sim <= simulation_end_date) 
num_lines_to_skip <<- which(datetxt_sim >= simulation_start_date)[1]
num_years_to_skip <<- as.numeric(start_year) - as.numeric(format(as.Date(input_start_date), format="%Y"))

date_hist_sim <- date_hist_sim_0[lines_to_keep,]
N_of_TimeSteps <- length(date_hist_sim[,1])
input_file <- input_file[lines_to_keep,]

# ------- Load functions and rule curves
# 5 files need to be loaded in the following order: 1- LoadFunctions.R 2- Read_Rule_Curves.R 3- Switches.R  4- dataframes.R 5- Measures of performance

# 1- LOAD ALL FUNCTIONS
source("LoadFunctions_new.R") 
# 2- READ ALL INPUT FILES
source("Read_Rule_Curves_new.R")
Read_Rule_Curves()
# 3- DEFINE SWITCHES AND DEFAULTS
source("Switches.R")
ReadSwitches()
# 4- CREATE DATAFRAMES
source("dataframes.R")
# 5- LOAD PMFs
source("PMFs_new.R")
# OUTPUT FILE
OutputFolder<-GlobalFile[3,2]

###### READ INPUT DATA FOR EACH WEEK
source("VIC_Data_new.R")


# ----------------- New simulation?
#------------------- Attention!!!!!!!!!!!!!!
# If new simulation is TRUE the program removes all file in the output directory

NEW_SIMULATION <- TRUE

##################################################################################################################
##################################################################################################################
                                  #######################################
                                  #                                     #
                                  #                                     #
                                  #          WEEKLY TIME-STEP           #
                                  #                                     #
                                  #                                     #
                                  #######################################
##################################################################################################################
##################################################################################################################
I_Week <- 1
for (I_Week in 1:N_of_TimeSteps){
	if(I_Week == 1) { # Model initialization
		print(paste0("initialization"))
		week_counter <- 1
		week_in_year <- input_file$Week_Number[I_Week]
		month_in_year <-  ifelse(input_file$month[I_Week] >= 8, input_file$month[I_Week] - 7, input_file$month[I_Week] + 5) # Convert from calendar month to reservoir operation month
		year_counter <- year_from_weekly()
		###### READ INPUT DATA FOR WEEK 1
		VIC_Data()
		############### Common weekly variables
		BRPrelim_c <- -9999
		TotalFloodSpace_c <- -9999
		ARFirmEngSupReq_c <- -9999
		ARFirmEngSup_c <- -9999
		TotalEnergyContent_c <- -9999
		TotalECCEnergyContent_c <- -9999
		FirmEnergyDeficit_c <- -9999
		TotalCoordPreEnergy_c <- -9999
		TotalNFEnergyContent_c <- -9999
		NonFirmEnergyDeficit_c <- -9999
		TotalMcNarySharedWater_c <- -9999		
		# ---------------- Initialize the model
		source("initialize_model.R")		
	} else {
		print(paste0("time step = ", I_Week))
		# FIND THE RIGHT TIME STEP
		week_counter <- I_Week
		week_in_year <- input_file$Week_Number[I_Week]
		month_in_year <-  ifelse(input_file$month[I_Week] >= 8, input_file$month[I_Week] - 7, input_file$month[I_Week] + 5) ## Month of the current time step
		year_counter <- year_from_weekly() ## Year of the current time step
		###### READ INPUT DATA FOR EACH WEEK
		VIC_Data()
		############### COMMON WEEKLY VARIABLES
		BRPrelim_c <- -9999
		TotalFloodSpace_c <- -9999
		ARFirmEngSupReq_c <- -9999
		ARFirmEngSup_c <- -9999
		TotalEnergyContent_c <- -9999
		TotalECCEnergyContent_c <- -9999
		FirmEnergyDeficit_c <- -9999
		TotalCoordPreEnergy_c <- -9999
		TotalNFEnergyContent_c <- -9999
		NonFirmEnergyDeficit_c <- -9999
		TotalMcNarySharedWater_c <- -9999
  
		MicaRelease_c <- MIRelease()
		dams_in$MICAA[I_Week] <- MIInflow()
		dams_out$MICAA[I_Week] <- MIOutflow()

		dams_in$REVEL[I_Week] <- REVIn()
		dams_out$REVEL[I_Week] <- REVOut()

		ARRelease_c <- ARRelease()
		dams_in$ARROW[I_Week] <- ARInflow()
		dams_out$ARROW[I_Week] <- AROutflow()

		DUOutflow_c <- DURelease() 
		dams_in$DUNCA[I_Week] <- DUInflow()
		dams_out$DUNCA[I_Week] <- DUOutflow()

		dams_in$LIBBY[I_Week] <- LBInflow()
		dams_out$LIBBY[I_Week] <- LBOutflow()

		dams_in$BONFE[I_Weeek] <- BONFIn()
		dams_out$BONFE[I_Week] <- BONFOut()

		CLRelease_c <- CLRelease() 
		dams_in$CORRA[I_Week] <- CLInflow()
		dams_out$CORRAI_Week] <- CLOutflow()

		HHRelease_c <- HHRelease()
		dams_in$FLASF[I_Week] <- HHInflow()
		dams_out$FLASF[I_Week] <- HHOutflow()

		KERelease_c <- KERelease()
		dams_in$FLAPO[I_Week] <- KEInflow()
		dams_out$FLAPO[I_Week] <- KEOutflow()

		dams_in$NOXON[I_Week] <- NOXIn()
		dams_out$NOXON[I_Week] <- NOXOut()

		dams_in$CABIN[I_Week] <- CBIn()
		dams_out$CABIN[I_Week] <- CBOut()

		AFRelease_c <- AFRelease()
		dams_in$ALBEN[I_Week] <- AFInflow()
		dams_out$ALBEN[I_Week] <- AFOutflow()

		dams_in$BOUND[I_Week] <- BDIn()
		dams_out$BOUND[I_Week] <- BDOut()

		GCRelease_c <- GCRelease()
		dams_in$GCOUL[I_Week] <- GCInflow()
		dams_out$GCOUL[I_Week] <- GCOutflow()

		dams_in$JDAYY[I_Week] <- CJIn()
		dams_out$JDAYY[I_Week] <- CJOut()
		if (track_curtailment==1) {
			mainstem_curtailments$CHIEF[I_Week] <- CJCurtail()
			mainstem_shortfall$CHIEF[I_Week] <- CJInstreamShortfall()
		}

		dams_in$WELLS[I_Week] <- WEIn()
		dams_out$WELLS[I_Week] <- WEOut()
		if (track_curtailment==1) {
			mainstem_curtailments$WELLS[I_Week] <- WECurtail()
			mainstem_shortfall$WELLS[I_Week] <- WEInstreamShortfall()
		}

		dams_in$ROCKY[I_Week] <- RRIn()
		dams_out$ROCKY[I_Week] <- RROut()
		if (track_curtailment == 1) {
			mainstem_curtailments$ROCKY[I_Week] <- RRCurtail()
			mainstem_shortfall$ROCKY[I_Week] <- RRInstreamShortfall()
		}

		dams_in$RISLA[I_Week] <- RIIn()
		dams_out$RISLA[I_Week] <- RIOut()
		if (track_curtailment == 1) {
			mainstem_curtailments$RISLA[I_Week] <- RICurtail()
			mainstem_shortfall$RISLA[I_Week] <- RIInstreamShortfall()
		}

		dams_in$WANAP[I_Week] <- WAIn()
		dams_out$WANAP[I_Week] <- WAOut()
		if (track_curtailment == 1) {
			mainstem_curtailments$WANAP[I_Week] <- WACurtail()
			mainstem_shortfall$WANAP[I_Week] <- WAInstreamShortfall()
		}

		dams_in$PRIRA[I_Week] <- PRIn()
		dams_out$PRIRA[I_Week] <- PROut()
		if (track_curtailment == 1) {
			mainstem_curtailments$PRIRA[I_Week] <- PRCurtail()
			mainstem_shortfall$PRIRA[I_Week] <- PRInstreamShortfall()
		}

		dams_in$UpSnake[I_Week] <- USInflow()
		dams_outUpSnake[I_Week] <- USOutflow()

		BRRelease_c <- BRRelease()
		dams_in$BROWN[I_Week] <- BRInflow()
		dams_out$BROWN[I_Week] <- BROutflow()

		dams_in$OXBOW[I_Week] <- OXIn()
		dams_out$OXBOW[I_Week] <- OXOut()

		dams_in$HCANY[I_Week] <- HCIn()
		dams_out$HCANY[I_Week] <- HCOut()

		DWRelease_c <- DWRelease()
		dams_in$DWORS[I_Week] <- DWInflow()
		dams_out$DWORS[I_Week] <- DWOutflow()

		dams_in$LGRAN[I_Week] <- LGIn()
		dams_out$LGRAN[I_Week] <- LGOut()	

		dams_in$LGOOS[I_Week] <- LIGIn()
		dams_out$LGOOS[I_Week] <- LIGOut()

		dams_in$LMONU[I_Week] <- LMIn()
		dams_out$LMONU[I_Week] <- LMOut()

		dams_in$ICEHA[I_Week] <- IHIn()
		dams_out$ICEHA[I_Week] <- IHOut()

		dams_in$MCNAR[I_Week] <- MCNIn()
		dams_out$MCNAR[I_Week] <- MCNOut()
		if (track_curtailment == 1) {
			mainstem_curtailments$MCNAR[I_Week] <- MCNCurtail()
			mainstem_shortfall$MCNAR[I_Week] <- MCNInstreamShortfall()
		}
		Biop[I_Week,1] <- McNaryFlowTarget()

		dams_in$JDAYY[I_Week] <- JDIn()
		dams_out$JDAYY[I_Week] <- JDOut()
		if (track_curtailment == 1) {
			mainstem_curtailments$JDAYY[I_Week] <- JDCurtail()
			mainstem_shortfall$JDAYY[I_Week] <- JDInstreamShortfall()
		}

		dams_in$DALLE[I_Week] <- DAIn()
		dams_out$DALLE[I_Week] <- DAOut()
		if (track_curtailment == 1) {
			mainstem_curtailments$DALLE[I_Week] <- DACurtail()
			mainstem_shortfall$DALLE[I_Week] <- DAInstreamShortfall()
		}

		dams_in$BONNE[I_Week] <- BONIn()
		dams_out$BONNE[I_Week] <- BONOut()
		Biop[I_Week,2] <- BonnevilleFlowTarget()
    
		##################################################################################################################
		##################################################################################################################
									  #######################################
									  #                                     #
									  #                                     #
									  #          Write Model Output         #
									  #                                     #
									  #                                     #
									  #######################################
		##################################################################################################################
		##################################################################################################################
		
		reservoir_vol_df$MI[I_Week] <- reservoir_vol_df$MI[I_Week] + (dams_in$MICAA[I_Week] - dams_out$MICAA[I_Week])
		reservoir_vol_df$AR[I_Week] <- reservoir_vol_df$AR[I_Week] + (dams_in$ARROW[I_Week] - dams_out$ARROW[I_Week])
		reservoir_vol_df$DU[I_Week] <- reservoir_vol_df$DU[I_Week] + (dams_in$DUNCA[I_Week] - dams_out$DUNCA[I_Week]) 
		reservoir_vol_df$CL[I_Week] <- reservoir_vol_df$CL[I_Week] + (dams_in$CORRA[I_Week] - dams_out$CORRA[I_Week])  
		reservoir_vol_df$LB[I_Week] <- reservoir_vol_df$LB[I_Week] + (dams_in$LIBBY[I_Week] - dams_out$LIBBY[I_Week]) 
		reservoir_vol_df$HH[I_Week] <- reservoir_vol_df$HH[I_Week] + (dams_in$FLASF[I_Week] - dams_out$FLASF[I_Week]) 
		reservoir_vol_df$GC[I_Week] <- reservoir_vol_df$GC[I_Week] + (dams_in$GCOUL[I_Week] - dams_out$GCOUL[I_Week]) 
		reservoir_vol_df$DW[I_Week] <- reservoir_vol_df$DW[I_Week] + (dams_in$DWORS[I_Week] - dams_out$DWORS[I_Week])
		reservoir_vol_df$BR[I_Week] <- reservoir_vol_df$BR[I_Week] + (dams_in$BROWN[I_Week] - dams_out$BROWN[I_Week])
		reservoir_vol_df$US[I_Week] <- reservoir_vol_df$US[I_Week] + (dams_in$UpSnake[I_Week] - dams_out$UpSnake[I_Week]) 
		reservoir_vol_df$KE[I_Week] <- reservoir_vol_df$KE[I_Week] + (dams_in$FLAPO[I_Week] - dams_out$FLAPO[I_Week]) 
		reservoir_vol_df$AF[I_Week] <- reservoir_vol_df$AF[I_Week] + (dams_in$ALBEN[I_Week] - dams_out$ALBEN[I_Week]) 

		###### MOPs Measures Of Performance
		MOP_df$FirmEnergy[I_Week] <- FirmEnergyMOP() # Firm energy shortfall
		MOP_df$NonFirmEnergy[I_Week] <- NonFirmEnergyMOP() # Non-Firm energy shortfall
		MOP_df$ColFallsFlow[I_Week] <- ColFallsFlowMOP() # Columbia Falls flow shortfall
		MOP_df$LowerGraniteFlow[I_Week] <- LowerGraniteFlowMOP() # Lower Granite flow shortfall 
		MOP_df$VernitaBarFlow[I_Week] <- VernitaBarFlowMOP() # Vernita Bar flow shortfall
		MOP_df$McNaryFlow[I_Week] <- McNaryFlowMOP() # McNary flow shortfall
		MOP_df$GCRec[I_Week] <- GCRecMOP() # Grand Coulee recreation metric
		MOP_df$DallesFlood[I_Week] <- DallesFloodMOP() # The Dalles flood protection metric
		MOP_df$IHNav[I_Week] <- IHNavMOP() # Ice Harbor navigation metric
		MOP_df$BonnevillFlow[I_Week] <- BonnevillFlowMOP() # Bonneville flow shortfall
		MOP_df$BelowFCC[I_Week] <- BelowFCC() # Excess flood storage space
		MOP_df$FirmEnergySales[I_Week] <- FirmEnergySales()
		MOP_df$NonFirmSpotSales[I_Week] <- NonFirmSpotSales()
		MOP_df$TotalSysEnergy[I_Week] <- TotalSysEnergy()
		
		print(paste0("Simulation date = ", as.Date(date_hist_sim[I_Week,1])))
		print(Sys.time())
		print(paste0("---------------------------------------------------------"))
		
		write.table(cbind(date_hist_sim[I_Week,], dams_out[I_Week,]), paste0(OutputFolder, "/dams_out.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[I_Week,], dams_in[I_Week,]), paste0(OutputFolder, "/dams_in.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[I_Week,], reservoir_vol_df[I_Week,]), paste0(OutputFolder, "/reservoir_volume.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[I_Week,], MOP_df[I_Week,]), paste0(OutputFolder, "/MOP_df.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[I_Week,], water_df[I_Week,]), paste0(OutputFolder, "/water.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[I_Week,], energy_df[I_Week,]), paste0(OutputFolder, "/energy.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[I_Week,], Biop[I_Week,]), paste0(OutputFolder, "/Biop_flow.txt"), row.names=F, col.names=F, append=T)

		if (track_curtailment == 1) {
			write.table(cbind(date_hist_sim[I_Week,], mainstem_shortfall[I_Week,]), paste0(OutputFolder, "/mainstem_shortfall.txt"), row.names=F, col.names=F, append=T)
			write.table(cbind(date_hist_sim[I_Week,], mainstem_curtailments[I_Week,]), paste0(OutputFolder, "/mainstem_curtailment.txt"), row.names=F, col.names=F, append=T)
		}
	}
}



