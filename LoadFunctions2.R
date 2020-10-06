

get_iflow = function(time_series, station) {
	ifile = read.table(paste('~/Bias_correction/flow_weekly_bc/', station, '_iflow_rules', sep=""))
	Months = as.numeric(strftime(time_series,"%m"))
	Days = as.numeric(strftime(time_series,"%d"))
	instream = as.numeric(matrix(nrow=length(Days), ncol=1, 0))
	for (line in 1:length(ifile[,1])) {
		mo = ifile[line,1]
		first_day = ifile[line,2]
		last_day = ifile[line,3]
		instream[Months==mo & Days>=first_day & Days<=last_day] = ifile[line,4]
	}
	return(instream)
}


######## MidColMult

MidColMult <- function() { # check to see if the values are changing during a year
	MidColMult_o <- 1
	return(MidColMult_o)
}

######## upperColMult

UpperColMult <- function() {
	UpperColMult_o <- UseInflowMult #* week_counter_in_year()
	return(UpperColMult_o)
}

######### SnakeMult
# This forces expected changes in the hydrology to occur.  The four areas are the Upper Columbia, the Snake, and the Lower Columbia.
# Below is a list of the VIC produced inflows associated with each category.
# Snake: Dworshak, Oxbow, Ice Harbor
# 7.17.01

SnakeMult <- function() { # check # weekly time-series of one
	SnakeMult_o <- 1
	return(SnakeMult_o)
}

##### week_counter_in_year

week_counter_in_year <- function() {
	if (week_counter %% 52 == 0) {
		week_counter_in_year_o <- 52 }
	else {
		week_counter_in_year_o <- week_counter %% 52 }
	return(week_counter_in_year_o)
}

######## find which operation year we are in, from weekly data

year_from_weekly <- function() {
	year_from_weekly_o <- week_counter %/% 52 + 1
	return(year_from_weekly_o)
}

##### MarketHydrgy
MarketHydrgy <- 0

###### RefillMinSw
# Switch to select alternate minimum release for generating inflow estimates at Grand Coulee and Arrow.
# These flow estimates are used to compute refill curves at Grand Coulee and Arrow.  Options:
# 0--Normal minimum requirements
# 1--Special minimum requirements for estimating inflows for refill curves.

RefillMinSw <- function() {
	if (Normal_minQ == 1) {
		RefillMinSw_o <- 0
	} else if (Developed_minQ == 1) {
		RefillMinSw_o <- 1
	}
	return(RefillMinSw_o)
}

###### ResetStorage

ResetStorage <- function() {
	if (ResetStorageSwitch() == 1 && month_in_year == ResetStorageMonth()) {
		ResetStorage_o <- 1
	} else if (ResetStorageSwitch() == 2 && TimeserStorReset() == 1) {
		ResetStorage_o <- 1
	} else {
		ResetStorage_o <- 0
	}
	return(ResetStorage_o)
}

####### ResetStorageSwitch
# This switch controls a routine that resets the storage at particular dams to preset storage values stored in the INIT<dam name> icons.
# The month that the storage is reset is specified in the icon ResetStorageMonth.
# Options:
# 0--do not reset storage
# 1--reset the storage to the initial value at the end of the month specified
# 2- reset the storage according to a timeseries

ResetStorageSwitch <- function() {
	ResetStorageSwitch_o <- 0
	return(ResetStorageSwitch_o)
}

####### ResetStorageMonth
# When the ResetStorageSwitch is set to 1, this icon determines the month for which the storage is reset to a preset initial value.
# If this icon is set to 6, for example, the storage at the end of January  will be the value set in the INIT<dam name> icon.

ResetStorageMonth <- function() {
	ResetStorageMonth_o <- 12
	return(ResetStorageMonth_o)
}

####### TimeserStorReset   #check
# this is a time-series of values (mostly zero), which has not been implemented in this version

TimeserStorReset <- function() {
	TimeserStorReset_o <- 0
	return(TimeserStorReset_o)
}

###### UseRegFlowAtBR
# Options:
# 0--Inflow to Brownlee is modeled explicitly from natural flow input at Milner.  Use this setting when using VIC data to drive the model.
# 1--Inflow to Brownlee is regulated flow at the 1989 level of development from Id Dept of Water Resources model.
# Use this setting when making runs using observed flow data and explicit effects in the Snake do not need to be simulated.

UseRegFlowAtBR <- function() {
	UseRegFlowAtBR_o <- 0
	return(UseRegFlowAtBR_o)
}

#########################################################################################################################################################
##########################################################################################################################################################

MIFullPoolVol <- 20075520
InitMILink <- 20075520
ResInitFractionFull <- 0
OptimizedRelSw <- 0 # This switch forces the model to use time series releases.  The capacity of the dam should be set to the top of pool using the Top Rule Switch.
# The model releases either the time series release or the rule release, whichever is larger.  Unitless.  Options:
# 0--Don't use time series releases.
# 1--Use time series releases.
Normal_FRC <- 1
FC_Option <- 4
days_per_week <- 7
cfsTOafw <- 1.9834 * days_per_week
Developed_minQ <- 1
Normal_minQ <- 1
MIAvgMin <- 3000
MIBotVol <- 10580000 # Approximate volume cooresponding to 2364.8 ft of elevation (1.058e7 acre ft).  The lower limit of operating range in the Columbia River Treaty.
# Minimum pool value is 2,320 ft of elevation, or 0.80035e7 acre ft
Fish_Pool_Alternative <- 1
UseAllStorForMcNLG <- 0 # Options: #0--Use current draft limits for McNary and Lower Granite #1-Use all major system storage for McNary and Lower Granite

######################### temp
week_counter <- 1

######################### MIRelease
# The total release from Jim Woodruff.  Units cfsd/m.

MIRelease <- function() {
	if (OptimizedRelSw == 1) {
		MIRelease_o <- max(MIDamProtectRel(), min(MIPrelim(), MIRelLimit()))
	} else {
		MIRelease_o <- max(MIDamProtectRel(), min(MIPrelim() + MICombSup() - MIRelReducReq(), MIRelLimit()))
	}
	return(MIRelease_o)
}

# MIDamProtectRel ---------------------------------------------------------
# The release required to keep the dam from filling above the full pool volume.  Units acre-ft.

MIDamProtectRel <- function() {
	MIDamProt <- max(0, MicaReservoir() + MicaInflow() - MicaNetWith() - MIFullPoolVol())
	return(MIDamProt)
}

######## MicaReservoir

MicaReservoir <- function() {
  #  InitMI=MicaRes
	if (week_counter == 1) {
		if (InitialConditionSwitch == 0) {
			MicaRes <- InitMILink
		} else if (InitialConditionSwitch == 1) {
			MicaRes <- ResInitFractionFull * MIFullPoolVol()
		} else if (InitialConditionSwitch == 2) {
			MIHistStor <- HistStor[week_counter, 2]
			MicaRes <- MIHistStor
		} else {
			MicaRes <- MIFullPoolVol()
		}
		# reservoir_vol_df[week_counter,1]=MicaRes
	} else {
		MicaRes <- reservoir_vol_df[week_counter - 1, 1]
	}
	return(MicaRes)
}

######## MicaInflow

MicaInflow <- function() {
  #   IF (ResetStorage =1){
  #   InitMI-MicaReservoir
  #   }else { MIIn}

  # MIIn= MicaFlowData-MIEvap
  # MicaFlowData=IF InflowDataSwitch= 1 THEN VICMI ELSE ModMicaFlowData
  # MIEvap=(MISufaceArea*MIEvapData)/12
  # VICMI=(PriVICMica-MIEstEvap-MIWith)*UpperColMult #
  # UpperColMult=Fow multipliers:This forces expected changes in the hydrology to occur. The fourareas are the Upper Columbia, the Snake, and the Lower Columbia.
  # Below is a list of the VIC produced inflows associated with each category. #Upper Columbia Inflows:  Mica, Revelstoke, Arrow, Duncan, Libby, Corra Linn (a.k.a. BC Hydro)
  # MIWith=IF MarketHydrgy=0 THEN MIEstWith ELSE MIAgWith+MIAgRech
  # MIEstEvap=0
  #PriVICMica=input_file[week_counter,50]
  return(PriVICMI)
}

###### MicaFlowData

MicaFlowData <- function() {
	return(MicaInflow())
}

###### MicaNetWith

MicaNetWith <- function() { # set to zero in ColSim
	MINetWith <- 0
	return(MINetWith)
}

##### MIFullPoolVol
# Approximate volume associated with 2475 ft of elevation.  This is the normal full pool elevation.

MIFullPoolVol <- function() {
	MIFullPool <- 20075520
	return(MIFullPool)
}

# MIPrelim ----------------------------------------------------------------
# Preliminary release based upon rule requirements and minimum releases.
# The release cannot be larger than the available water after withdrawals.  Units cfs-days/m

MIPrelim <- function() {
	if (OptimizedRelSw == 1) {
		MIpre <- max(MIOpRel(), MIRuleReq())
	} else {
		MIpre <- min(MIAvailAfter(), max(MIRuleReq(), MIReq()))
	}
	return(MIpre)
}

######### MIOpRel
# Time series reservoir  release to compare with optimization models.  Units acre-ft/month.

MIOpRel <- function() { # Time series reservoir  release to compare with optimization models.  Units acre-ft/month.
	MIOpRel_1 <- 0
	return(MIOpRel_1)
}

######## MIRuleReq

MIRuleReq <- function() {
	MIRuleReq_1 <- max(MicaReservoir() + MIIn() - MicaNetWith() - MITopVol(), 0)
	return(MIRuleReq_1)
}

########## MITopVol

MITopVol <- function() {
	if (TopRuleSw() == 0) {
		MITopVol_1 <- MIFloodCurve()
	} else if (TopRuleSw() == 1) {
		MITopVol_1 <- MIFullPoolVol()
	} else {
		MIFlood1 <- MIFlood_712[week_counter_in_year(), 2]
		MITopVol_1 <- MIFlood1
	}
	return(MITopVol_1)
}

##########  TopRuleSw #Top of conservation pool.  Units acre-ft.

TopRuleSw <- function() {
  # Options:
  # 0-Use flood storage prescribed by forecasts
  # 1-Don't evacuate any flood storage
  # 2-Use the highest flood rule curve at all times
	Ignore_FRC <- 0
	Dry_Year_FRC <- 0
	if (Normal_FRC == 1) {
		TopRuleSw_1 <- 0
	} else if (Ignore_FRC == 1) {
		TopRuleSw_1 <- 1
	} else if (Dry_Year_FRC == 1) {
		TopRuleSw_1 <- 2
	}
	return(TopRuleSw_1)
}

######### MIIn

MIIn <- function() { # right now it only includes the flow inputs and evap and withdrawals are zero
  #   IF (ResetStorage =1){
  #   InitMI-MicaReservoir
  #   }else { MIIn}

  # MIIn= MicaFlowData-MIEvap
  # MicaFlowData=IF InflowDataSwitch= 1 THEN VICMI ELSE ModMicaFlowData
  # MIEvap=(MISufaceArea*MIEvapData)/12
  # VICMI=(PriVICMica-MIEstEvap-MIWith)*UpperColMult #
  # UpperColMult=Fow multipliers:This forces expected changes in the hydrology to occur. The fourareas are the Upper Columbia, the Snake, and the Lower Columbia.
  # Below is a list of the VIC produced inflows associated with each category. #Upper Columbia Inflows:  Mica, Revelstoke, Arrow, Duncan, Libby, Corra Linn (a.k.a. BC Hydro)
  # MIWith=IF MarketHydrgy=0 THEN MIEstWith ELSE MIAgWith+MIAgRech
  # MIEstEvap=0
  # PriVICMica=input_file[week_counter,50]
  return(PriVICMI)
}

######## MIReq
# Minimum release requirement converted to cfsd.

MIReq <- function() {
	if (RefillMinSw() == 1) {
		MIReq_o <- MIAssuredReleasedMin() * cfsTOafw
	} else {
		MIReq_o <- max(MIMinRelForDust(), MIAvgMin * cfsTOafw)
	}
	return(MIReq_o)
}

######## MIAssuredReleasedMin

MIAssuredReleasedMin <- function() {
	MIAssuredReleasedMin_o <- MIAssuredReleasedMin_i[week_counter_in_year(), 2]
	return(MIAssuredReleasedMin_o)
}

######## MIMinRelForDust       
#Minimum release from Mica for Arrow.  Units acre-ft/month.

MIMinRelForDust <- function() {
	if (week_counter_in_year() >= 1 && week_counter_in_year() <= 18) {
		MIMinRelForDust_o <- MIAugNovMin() * cfsTOafw
	} else if (week_counter_in_year() >= 19 && week_counter_in_year() <= 22) {
		MIMinRelForDust_o <- MIDecMin() * cfsTOafw
	} else if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
		MIMinRelForDust_o <- MIJanMin() * cfsTOafw
	} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
		MIMinRelForDust_o <- MIFebMin() * cfsTOafw
	} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
		MIMinRelForDust_o <- MIMarchMin() * cfsTOafw
	} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
		MIMinRelForDust_o <- MIApriMin() * cfsTOafw
	} else if (week_counter_in_year() >= 40 && week_counter_in_year() <= 44) {
		MIMinRelForDust_o <- MIMayMin() * cfsTOafw
	} else if (week_counter_in_year() >= 45 && week_counter_in_year() <= 48) {
		MIMinRelForDust_o <- MIJuneMin() * cfsTOafw
	} else if (week_counter_in_year() >= 49 && week_counter_in_year() <= 52) {
		MIMinRelForDust_o <- MIJulyMin() * cfsTOafw
	} else {
		MIMinRelForDust_o <- 0
	}
	MIMinRelForDust_o <- 0 # it is set to zero in the current version of ColSim
	return(MIMinRelForDust_o)
}

# MI dust rule functions --------------------------------------------------

MIAugNovMin <- function() {
	if (ARFloodCurve() < 2.64E5 || ARFloodCurve() < 3.53E5) {
		dust_o <- 0
	} else if (ArrowReservoir() < 2.64E5) {
		dust_o <- 32000
	} else if (ArrowReservoir() < 3.53E5) {
		dust_o <- 27000
	} else {
		dust_o <- 0
	}
	return(dust_o)
}

####### MIDecMin

MIDecMin <- function() {
	if (ARFloodCurve() < 1.34E6) {
		dust_o <- 23000
	} else if (ArrowReservoir() < 1.34E6) {
		dust_o <- 32000
	} else {
		dust_o <- 23000
	}
	return(dust_o)
}

###### MIJanMin

MIJanMin <- function() {
	if (ARFloodCurve() < 8.93E5) {
		dust_o <- 23000
	} else if (ArrowReservoir() < 8.93E5) {
		dust_o <- 32000
	} else {
		dust_o <- 23000
	}
	return(dust_o)
}

##### MIFebMin

MIFebMin <- function() {
	if (ARFloodCurve() < 3.48E5) {
		dust_o <- 23000
	} else if (ArrowReservoir() < 3.48E5) {
		dust_o <- 27000
	} else {
		dust_o <- 23000
	}
	return(dust_o)
}

##### MIMarchMin

MIMarchMin <- function() {
	if (ARFloodCurve() < 3.48E5) {
		dust_o <- 18000
	} else if (ArrowReservoir() < 1.39E6) {
		dust_o <- 23000
	} else {
		dust_o <- 18000
	}
	return(dust_o)
}

###### MIApriMin

MIApriMin <- function() {
	dust_o <- 18000
	return(dust_o)
}

###### MIMayMin

MIMayMin <- function() {
	dust_o <- 10000
	return(dust_o)
}

###### MIJuneMin

MIJuneMin <- function() {
	if (ArrowReservoir() < 248000) {
		dust_o <- 25000
	} else {
		dust_o <- 10000
	}
	return(dust_o)
}

####### MIJulyMin

MIJulyMin <- function() {
	if (ArrowReservoir() < 7.45E5) {
		dust_o <- 32000
	} else {
		dust_o <- 0
	}
	return(dust_o)
}

##### MIAvailAfter
# Total water available for release after withdrawals and returns  including inflow for the month.  Units cfs-days.

MIAvailAfter <- function() {
	MIAvailAfter_o <- max(0, MicaReservoir() + MIIn() - MicaNetWith() - MIBotVol)
	return(MIAvailAfter_o)
}

# MICombSup ---------------------------------------------------------------

MICombSup <- function() {
# Units Mwhr
	if (TotalEnergyContent_c == -9999) {
		#print(paste("energy generation", week_counter))
		print(paste("initial___total energy content", TotalEnergyContent_c))
		TotalEnergyContent_c <<- TotalEnergyContent()
		energy_df[week_counter,2]<<-TotalEnergyContent_c
		#print(paste("final___total energy content", TotalEnergyContent_c))
	}
	if (TotalMcNarySharedWater_c == -9999) {
		TotalMcNarySharedWater_c <<- TotalMcNarySharedWater()
		#print(paste("TotalMcNarySharedWater=", TotalMcNarySharedWater_c))
		water_df[week_counter,3]<<-TotalMcNarySharedWater_c
	}
	if (TotalECCEnergyContent_c == -9999) {
		TotalECCEnergyContent_c <<- TotalECCEnergyContent()
		energy_df[week_counter,3]<<-TotalECCEnergyContent_c
	}
	if (TotalCoordPreEnergy_c == -9999) {
		TotalCoordPreEnergy_c <<- TotalCoordPreEnergy()
		energy_df[week_counter,5]<<-TotalCoordPreEnergy_c
	}
	if (FirmEnergyDeficit_c == -9999) {
		FirmEnergyDeficit_c <<- FirmEnergyDeficit()
		energy_df[week_counter,4] <<- FirmEnergyDeficit_c
	}
	if (NonFirmEnergyDeficit_c == -9999) {
		NonFirmEnergyDeficit_c <- NonFirmEnergyDeficit()
		energy_df[week_counter,7]<<-NonFirmEnergyDeficit_c
	}
	if (TotalNFEnergyContent_c == -9999) {
		TotalNFEnergyContent_c <<- TotalNFEnergyContent()
		energy_df[week_counter,6]<<-TotalNFEnergyContent_c
	}
  # if(TotalFloodSpace_c==-9999){
  #   TotalFloodSpace_c<<-TotalFloodSpace()
  # }
  #
	MICombSup_o <- MIEnergySup() + MIMcNarySup()
	return(MICombSup_o)
}

####### MIEnergySup

MIEnergySup <- function() {
  # if(TotalNFEnergyContent_c==-9999) {
  #   TotalNFEnergyContent_c<<-TotalNFEnergyContent()
  # }
  #
  # print(paste("MIEnergySup"))
	if (UseTotalEnergyContentForFirm() == 1) {
		MIEnergySup_o <- max(min(MIFirmEngSupReq(), MISharedWater()), min(MINonFirmSupReq(), MIECCSharedWater()))
	} else {
		MIEnergySup_o <- max(min(MIFirmEngSupReq(), MIECCSharedWater()), min(MINonFirmSupReq(), MIECCSharedWate()))
	}
  #  if(TotalMcNarySharedWater_c==0){MIEnergySup_o= 0
  #  } else{MIEnergySup_o= min(MIMcNarySharedWater(),McNaryFlowDeficit()*MIMcNarySharedWater()/TotalMcNarySharedWater_c)}
	return(MIEnergySup_o)
}

########## MIFirmEngSupReq

MIFirmEngSupReq <- function() {
	MIFirmEngSupReq_o <- min(MIPenLimit(), 4.260306e7 * (MIFirmEngSup()) / (43560 * (MINetHead() + RevNetHead() + TotalGCHead()) * MICombEfficiency))
	return(MIFirmEngSupReq_o)
}


######### MINonFirmSupReq

MINonFirmSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		MINonFirmSupReq_o <- min(MIPenLimit(), 4.260306e7 * (MIFirmEngSup() + MINonFirmEngSup()) / (43560 * (MINetHead() + RevNetHead() + TotalGCHead()) * MICombEfficiency))
	} else {
		MINonFirmSupReq_o <- 0
	}
	return(MINonFirmSupReq_o)
}

####### MINonFirmEngSup

MINonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		MINonFirmEngSup_o <- 0
	} else {
		MINonFirmEngSup_o <- MINFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(MINonFirmEngSup_o)
}

####### MIMcNarySharedWater

MIMcNarySharedWater <- function() {
	MIMcNarySharedWater_o <- max(0, MicaReservoir() + MIIn() - MicaNetWith() - MIPrelim() - MIMcNaryDraftLimit())
	return(MIMcNarySharedWater_o)
}

# McNary flow target
# The Corps designates Libby, and the USBR designates Grand Coulee and Hungry Horse to contribute to the Federal Columbia River Power System (FCRPS)
# goal of meeting the following BiOp flow objectives:
# Spring (4/10 - 6/30)      # Priest Rapids (135 kcfs)
# Spring (4/20 - 6/30)**    # McNary Dam (220-260 kcfs)
# Summer (7/1 - 8/31)       # McNary Dam (200 kcfs)
# **Interpolation between 220 and 260 kcfs is based upon Corps of Engineers data submittal, which bases targets on forecasts of Jan-July flow at The Dalles.

####### MIMcNaryDraftLimit

MIMcNaryDraftLimit <- function() {
	MIMcNaryDraftLimit_o <- if (UseAllStorForMcNLG == 1) {
    MIMcNaryDraftLimit_o <- MIBotVol
	} else if (Fish_Pool_Alternative == 1) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol()
	} else if (Fish_Pool_Alternative == 2) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 0
	} else if (Fish_Pool_Alternative == 3) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 0.402E6
	} else if (Fish_Pool_Alternative == 4) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 0.804E6
	} else if (Fish_Pool_Alternative == 5) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 1.205E6
	} else {
		MIMcNaryDraftLimit_o <- MIFullPoolVol
	}
	return(MIMcNaryDraftLimit_o)
}

###### MIMcNarySup

MIMcNarySup <- function() {

  # if(TotalMcNarySharedWater_c==-9999){
  #   TotalMcNarySharedWater_c<<-TotalMcNarySharedWater()
  #   print(paste("TotalMcNarySharedWater=", TotalMcNarySharedWater_c))
  # }

  # print(paste("MIMcNarySup"))
	if (TotalMcNarySharedWater_c == 0) {
		MIMcNarySup_o <- 0
	} else {
		MIMcNarySup_o <- min(MIMcNarySharedWater(), McNaryFlowDeficit() * MIMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(MIMcNarySup_o)
}

###### MIRelReducReq

MIRelReducReq <- function() {
	MIRelReducReq_o <- TotalRelReducReq() * MIFloodFrac()
	return(MIRelReducReq_o)
}

###### MIFloodFrac

MIFloodFrac <- function() {
	if (TotalFloodSpace_c == -9999) {
		TotalFloodSpace_c <<- TotalFloodSpace()
		water_df[week_counter,2]<<-TotalFloodSpace_c
	}
	if (TotalFloodSpace_c == 0) {
		MIFloodFrac_o <- 0
	} else {
		MIFloodFrac_o <- MIFloodMult * MIFloodSpace() / TotalFloodSpace_c
	}
	return(MIFloodFrac_o)
}

###### MIFloodSpace

MIFloodSpace <- function() {
	MIFloodSpace_o <- min(MIPrelim() + MIEnergySup(), max(0, MIFullPoolVol() - MicaReservoir() + MIIn() - (MIPrelim() + MIEnergySup())))
	return(MIFloodSpace_o)
}

######## MIEnergyContent

MIEnergyContent <- function() {
	MIEnergyContent_o <- MISharedWater() * (MINetHead() + RevNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIEnergyContent_o)
}

###### MISharedWater

MISharedWater <- function() {
	MISharedWater_o <- max(0, MicaReservoir() + MIIn() - MicaNetWith() - MIPrelim() - MIMcNarySup() - MIBotVol)
	return(MISharedWater_o)
}

######### MINetHead

MINetHead <- function() {
	MITailElev <- 1867
	MILoss <- 0
	MINetHead_o <- MIElev_ft() - MITailElev - MILoss
	return(MINetHead_o)
}

###### MIElev_ft

MIElev_ft <- function() {
	MIElev_ft_o <- 2.86317977E-33 * MicaReservoir()^5 - 1.60110881E-25 * MicaReservoir()^4 + 3.33032270E-18 * MicaReservoir()^3 -
    3.24050996E-11 * MicaReservoir()^2 + 1.67124652E-04 * MicaReservoir() + 1.90570627E+03
	return(MIElev_ft_o)
}

####### RevNetHead

RevNetHead <- function() {
	RevNetHead_o <- 420
	return(RevNetHead_o)
}

##### MIRelLimit

MIRelLimit <- function() {
	MIRelLimit_o <- min(MIMaxFloodOutflow(), max(MicaReservoir() + MicaInflow() - MicaNetWith() - MIBotVol, 0))
	return(MIRelLimit_o)
}

######## MIMaxFloodOutflow

MIMaxFloodOutflow <- function() {
	MIMaxFloodOutflow_o <- MIMaxFloodOutflow_input[week_counter_in_year(), 2]
	return(MIMaxFloodOutflow_o)
}

# week_counter_in_year
week_counter <- 1

########### MIFloodCurve

# Mica Flood Control Curves are modified according to January 1995 SRDs which are showed in USCOE Website
# (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/MCDBChart8.pdf) (by Se-yeun Lee, Dec.05)
# Flood storage curve selected is based on the total run off April-August  for the variable period && is fixed during the rest of the year.
# For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles and/or to the dam itself.  Units acre-ft.

MIFloodCurve <- function() {
	GlobalFloodEvacMult <- GlobalFloodEvacMult_i[week_counter_in_year(), 2]
	if (FC_Option == 1) {
		MIFloodCurve_o <- MIFullPoolVol() - GlobalFloodEvacMult * (MIFullPoolVol() - MI_CurFC())
	} else if (FC_Option == 4) {
		MIFloodCurve_o <- MIFullPoolVol() - GlobalFloodEvacMult * (MIFullPoolVol() - MICurFC_Cont())
	} else if (FC_Option == 2) {
		MIFloodCurve_o <- GlobalFloodEvacMult * (MIFullPoolVol() - MI_HecFC())
	} else {
		MIFloodCurve_o <- GlobalFloodEvacMult * (MIFullPoolVol() - MI_ENSO_FC)
	}
	return(MIFloodCurve_o)
}

############ MICurFC_Cont() ##### CHECK TO make sure it makes sense ==== check mean(MIFlood_DecToApr[,1]) ...

MICurFC_Cont <- function() {
	MIFlood_Range <- c(10000000, 12000000, 14000000, 16000000, 18000000, 20000000)
	if (MIFlood_Range[length(MIFlood_Range)] < MISumRunoffAprilAug) {
		row_no <- length(MIFlood_Range)
	} else {
		row_no <- max(1.0, which(MIFlood_Range > MISumRunoffAprilAug)[1] - 1)
	}
	if (DallesRunoffAprAug >= 111.0E06) {
		if (MISumRunoffAprilAug <= 20.0E06) {
			if (week_counter_in_year() >= 19 && week_counter_in_year() <= 22) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 1] # DEC_MI
			} else if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 2] # JAN_MI
			} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 3] # FEB_MI
			} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 4] # MAR_MI
			} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 48) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 5] # APR_MI
			} else if (week_counter_in_year() >= 10 && week_counter_in_year() <= 18) {
				MI_CurFC_o <- 19875520
			} else {
				MI_CurFC_o <- 20075520
			}
		} else {
			MIFlood12 <- MIFlood_712[week_counter_in_year(), 13]
			MI_CurFC_o <- MIFlood12
		}
	} else {
		MIFlood7 <- MIFlood_712[week_counter_in_year(), 8]
		MI_CurFC_o <- MIFlood7
	}
	return(MI_CurFC_o)
}


############ MI_CurFC ###########

MI_CurFC <- function() {
	if (DallesRunoffAprAug >= 111E6 && MISumRunoffAprilAug > 19.3E6) {
		MI_CurFC_o <- MIFlood12
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 18E6 || MISumRunoffMayAug > 17.5E6)) {
		MIFlood11 <- MIFlood_712[week_counter_in_year(), 12]
		MI_CurFC_o <- MIFlood11
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 16E6 || MISumRunoffMayAug > 15.50E6)) {
		MIFlood10 <- MIFlood_712[week_counter_in_year(), 11]
		MI_CurFC_o <- MIFlood10
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 14E6 || MISumRunoffMayAug > 13.50E6)) {
		MIFlood9 <- MIFlood_712[week_counter_in_year(), 10]
		MI_CurFC_o <- MIFlood9
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 12E6 || MISumRunoffMayAug > 11.50E6)) {
		MIFlood8 <- MIFlood_712[week_counter_in_year(), 9]
		MI_CurFC_o <- MIFlood8
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 10E6 || MISumRunoffMayAug > 9.5E6)) {
		MIFlood7 <- MIFlood_712[week_counter_in_year(), 8]
		MI_CurFC_o <- MIFlood7
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 8E6 || MISumRunoffMayAug > 7.5E6)) {
		MIFlood_6 <- MIFlood_712[week_counter_in_year(), 7]
		MI_CurFC_o <- MIFlood6
	} else if (DallesRunoffAprAug >= 80.0E6) {
		MIFlood_5 <- MIFlood_712[week_counter_in_year(), 6]
		MI_CurFC_o <- MIFlood5
	} else if (DallesRunoffAprAug >= 75.0E6) {
		MIFlood_4 <- MIFlood_712[week_counter_in_year(), 5]
		MI_CurFC_o <- MIFlood4
	} else if (DallesRunoffAprAug >= 70.0E6) {
		MIFlood_3 <- MIFlood_712[week_counter_in_year(), 4]
		MI_CurFC_o <- MIFlood3
	} else if (DallesRunoffAprAug >= 65.0E6) {
		MIFlood_2 <- MIFlood_712[week_counter_in_year(), 3]
		MI_CurFC_o <- MIFlood2
	} else {
		MIFlood_1 <- MIFlood_712[week_counter_in_year(), 2]
		MI_CurFC_o <- MIFlood1
	}
	return(MI_CurFC_o)
}

############# MI_HecFC ##### called when FC_Option=4

MI_HecFC <- function() {
	print(paste("warning: this function is empty right now and it needs to be implemented from ColSim"))
	return(0)
}

############# MI_ENSO_FC ###### called when FC_Option=3

MI_ENSO_FC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

####### MI_April_Evac_Target

MI_April_Evac_Target <- function() { ## CHECK # Flood_2_1 and etc have not been included yet
	if (FC_Option == 1) {
		if (DallesRunoffAprAug >= 111E6 && MISumRunoffAprilAug > 19.3E6) {
			MIF_o <- MIFlood12_2
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 18E6 || MISumRunoffMayAug > 17.5E6)) {
			MIF_o <- MIFlood11_2
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 16E6 || MISumRunoffMayAug > 15.50E6)) {
			MIF_o <- MIFlood10_2
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 14E6 || MISumRunoffMayAug > 13.50E6)) {
			MIF_o <- MIFlood9_2
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 12E6 || MISumRunoffMayAug > 11.50E6)) {
			MIF_o <- MIFlood8_2
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 10E6 || MISumRunoffMayAug > 9.5E6)) {
			MIF_o <- MIFlood7_2
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 8E6 || MISumRunoffMayAug > 7.5E6)) {
			MIF_o <- MIFlood7_2
		} else if (DallesRunoffAprAug >= 80.0E6) {
			MIF_o <- MIFlood7_2
		} else if (DallesRunoffAprAug >= 75.0E6) {
			MIF_o <- MIFlood7_2
		} else if (DallesRunoffAprAug >= 70.0E6) {
			MIF_o <- MIFlood7_2
		} else if (DallesRunoffAprAug >= 65.0E6) {
			MIF_o <- MIFlood7_2
		} else {
			MIF_o <- (MIFlood1_2 + MIFlood2_2 + MIFlood3_2 + MIFlood4_2 + MIFlood5_2 + MIFlood6_2) * 0 + MIFlood7_2
		}
		MI_April_Evac_Target_o <- GlobalFloodEvacMult * (MIFullPoolVol() - MIF_o)
  } else {
		MI_April_Evac_Target_o <- GlobalFloodEvacMult * (MIFullPoolVol() - MIAprEvaq_Cont())
  }
  return(MI_April_Evac_Target_o)
}

######## MIAprEvaq_Cont

MIAprEvaq_Cont <- function() {
	if (DallesRunoffAprAug >= 111E06) {
		if (MISumRunoffAprilAug <= 20.0E06) {
			MIAprEvaq_Cont_o <- APR_MI()
		} else {
			MIAprEvaq_Cont_o <- 7537058
		}
	} else {
		MIAprEvaq_Cont_o <- 15075520
	}
	return(MIAprEvaq_Cont_o)
}


######## APR_MI

APR_MI <- function() {
	if(which(APR_MI_input > MISumRunoffAprilAug)[1]>1){
		APR_MI_o <- APR_MI_input[which(APR_MI_input > MISumRunoffAprilAug)[1] - 1, 2]
	} else {
		APR_MI_o <- APR_MI_input[1,2]
	}
	return(APR_MI_o)
}

###### MIECCEnergyContent

MIECCEnergyContent <- function() {
	MIECCEnergyContent_o <- MIECCSharedWater() * (MINetHead() + RevNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIECCEnergyContent_o)
}

##### MIECCSharedWater

MIECCSharedWater <- function() {
	MIECCSharedWater_o <- max(0, MicaReservoir() + MIIn() - MicaNetWith() - MIMcNarySup() - MIPrelim() - MIECC())
	return(MIECCSharedWater_o)
}

##### MIECC

MIECC <- function() {
  # During the fixed period from August-December before the forecast, the ECC is the greater of the critical curve or the refill curve based on 1931 inflows.
  # In the variable period from January-July the VEEC can be lower than the ECC due to revised refill curve (based on forecast).
  # The VEEC curve, however, cannot be higher than the original ECC curve.  The value in this icon is compared to the flood storage curve and the minimum value is selected.
  # The model will always release water to evacuate flood storage, and will release to the ECC and VECC if the user selects maximum allowed non-firm releases.
	if (UseUpdatedOpSystem == 0) {
		if (month_in_year <= 5 || month_in_year == 12) {
			out1 <- max(MICriticalCurve(), MIRefillCurve())
		} else {
			out1 <- min(max(MIRefillCurve(), MICriticalCurve()), max(MICriticalCurve(), MI1931Refill()))
		}
		MIECC_o <- min(MIFloodCurve(), out1)
	} else if (UseUpdatedOpSystem == 1 && UseForecastRLFCurve == 1) { # not included
		if (month_in_year <= 5 || month_in_year == 12) {
			out2 <- MIForecastRLFCurve
		} else {
			out2 <- MIActualRFCurve
		}
		min(MIFloodCurve(), out2)
	} else { # not included
		if (month_in_year <= 5 || month_in_year == 12) {
			out3 <- MI1931RFCurve
		} else {
			out3 <- MIActualRFCurve
		}
		min(MIFloodCurve(), out3)
	}
  return(MIECC_o)
}


####### MICriticalCurve

MICriticalCurve <- function() {
	MICriticalCurve_o <- MICriticalCurve_input[week_counter_in_year(), 1]
	return(MICriticalCurve_o)
}

###### MIRefillCurve

MIRefillCurve <- function() {
	MIRefillVol3 <- 0
	if (RefillSwitch() == 1) {
		MIRefillCurve_o <- MIRefillVol1
	} else if (RefillSwitch() == 2) {
		MIRefillCurve_o <- MIRefillVol2
	} else {
		MIRefillCurve_o <- MIRefillVol3
	}
	return(MIRefillCurve_o)
}

####### MI1931Refill

MI1931Refill <- function() {
	MI1931Refill_o <- MI1931Refill_input[week_counter_in_year(), 2]
  # print(paste("last function loaded"))
	return(MI1931Refill_o)
}

# } ##### end of of MICA LOAD function

# Arrow
InitARLink <- 4886277.48
ARBotVol <- 227340

###### ARFullPoolVol

ARFullPoolVol <- function() {
  ARFullPoolVol_o <- 7327300 # Volume cooresponding to 1444 ft of elevation.  This is normal full pool.  Units acre-ft.
  return(ARFullPoolVol_o)
}

########## ArrowReservoir
ArrowReservoir <- function() {
	if (week_counter == 1) {
		if (InitialConditionSwitch == 0) {
			ArrowReservoir_o <- InitARLink
		} else if (InitialConditionSwitch == 1) {
			ArrowReservoir_o <- ResInitFractionFull * ARFullPoolVol()
		} else if (InitialConditionSwitch == 2) {
			ARHistStor <- ARHistStor_i[week_counter, 2]
			ArrowReservoir_o <- ARHistStor
		} else {
			ArrowReservoir_o <- ARFullPoolVol
		}
    #  reservoir_vol_df[week_counter,2]=ArrowReservoir_o
	} else {
		ArrowReservoir_o <- reservoir_vol_df[week_counter - 1, 2]
	}
	return(ArrowReservoir_o)
}

######## ARMcNarySharedWater

ARMcNarySharedWater <- function() {
	ARMcNarySharedWater_o <- max(0, ArrowReservoir() + ARIn() - ArrowNetWith() - ARPrelim() - ARMcNaryDraftLimit())
	return(ARMcNarySharedWater_o)
}

# ARIn --------------------------------------------------------------------

ARIn <- function() {
	ARIn_o <- MIPrelim() + (ArrowFlowData() - MicaFlowData()) - MicaToARNetWith() - AREvap()
	return(ARIn_o)
}

############# ArrowFlowData

ArrowFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		ArrowFlowData_o <- VICAR()
	} else {
		ArrowFlowData_o <- ModArrowFlowData()
	}
	return(ArrowFlowData_o)
}

######## ModArrowFlowData

ModArrowFlowData <- function() {
	###### check "ModArrowFlowData()" has not been implemented yet
	return(0)
}

######### VICAR

VICAR <- function() { # check
	VICAR_o1 <- PriVICAR
	VICAR_o <- VICAR_o1 * UpperColMult() # check
	return(VICAR_o)
}

######## AREstEvap

AREstEvap <- function() {
	AREstEvap_o <- 0
	return(AREstEvap_o)
}

####### ARWith

ARWith <- function() {
	ARAgRech <- ARWith_input[week_counter_in_year(), 2]
	ARAgWith <- ARWith_input[week_counter_in_year(), 3]
	if (MarketHydrgy == 0) {
		ARWith_o <- AREstWith()
	} else {
		ARWith_o <- ARAgRech + ARAgWith
	}
	return(ARWith_o)
}

#####  AREstWith

AREstWith <- function() {
	AREstWith_o <- 0
	return(AREstWith_o)
}

####### MicaToARNetWith

MicaToARNetWith <- function() {
	MicaToARNetWith_o <- 0
	return(MicaToARNetWith_o)
}

###### AREvap

AREvap <- function() {
	AREvap_o <- (ARSufaceArea() * AREvapData()) * 0.5042 / 12
	return(AREvap_o)
}

###### ARSufaceArea

ARSufaceArea <- function() {
	ARSufaceArea_o <- (-1.21281443E-13 * (ArrowReservoir() / 1000)^4 + 1.53692112E-09 * (ArrowReservoir() / 1000)^3 -
    6.75961255E-06 * (ArrowReservoir() / 1000)^2 + 1.87278268E-02 * (ArrowReservoir() / 1000) + 2.30403996) * 1000
	return(ARSufaceArea_o)
}

#### AREvapData

AREvapData <- function() {
	AREvapData_o <- 0
	return(AREvapData_o)
}

#### ArrowNetWith

ArrowNetWith <- function() {
	ArrowNetWith_o <- 0
	return(ArrowNetWith_o)
}

#### ARPrelim

ARPrelim <- function() {
	if (OptimizedRelSw == 1) {
		ARPrelim_o <- max(AROpRel(), ARRuleReq())
	} else {
		ARPrelim_o <- min(ARAvailAfter(), max(ARRuleReq(), ARMinReq()))
	}
	return(ARPrelim_o)
}

#### AROpRel

AROpRel <- function() {
	AROpRel_o <- 0
	return(AROpRel_o)
}

###### ARRuleReq

ARRuleReq <- function() {
	ARRuleReq_o <- max(ArrowReservoir() + ARIn() - ArrowNetWith() - ARTopVol(), 0)
	return(ARRuleReq_o)
}

####### ARTopVol

ARTopVol <- function() {
	ARFlood1 <- ARFlood[week_counter_in_year(), 2]
	if (TopRuleSw() == 0) {
		ARTopVol_o <- ARFloodCurve()
	} else if (TopRuleSw() == 1) {
		ARTopVol_o <- ARFullPoolVol()
	} else {
		ARTopVol_o <- ARFlood1()
	}
	return(ARTopVol_o)
}

###### ARAvailAfter

ARAvailAfter <- function() {
	ARAvailAfter_o <- max(0, ArrowReservoir() + ARIn() - ArrowNetWith() - ARBotVol)
	return(ARAvailAfter_o)
}

######## ARMinReq

ARMinReq <- function() {
	if (RefillMinSw() == 1) {
		ARMinReq_o <- ARAssuredRelease() * cfsTOafw
	} else {
		ARMinReq_o <- ARAvgMin() * cfsTOafw
	}
	return(ARMinReq_o)
}

######## ARAssuredRelease

ARAssuredRelease <- function() {
	ARAssuredRelease_o <- ARAssuredRelease_input[week_counter_in_year(), 2]
	return(ARAssuredRelease_o)
}

###### ARAvgMin

ARAvgMin <- function() {
	ARAvgMin_o <- 5000
	return(ARAvgMin_o)
}

# ARMcNaryDraftLimit ------------------------------------------------------

ARMcNaryDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		ARMcNaryDraftLimit_o <- ARBotVol
	} else if (Fish_Pool_Alternative == 1) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol()
	} else if (Fish_Pool_Alternative == 2) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol()
	} else if (Fish_Pool_Alternative == 3) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol() - 0.386E6
	} else if (Fish_Pool_Alternative == 4) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol() - 0.773E6
	} else if (Fish_Pool_Alternative == 5) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol() - 1.159E6
	} else {
		ARMcNaryDraftLimit_o <- ARFullPoolVol()
	}
	return(ARMcNaryDraftLimit_o)
}

####### ARFloodSpace

ARFloodSpace <- function() {
	ARFloodSpace_o <- min((ARPrelim() + AREnergySup() + ARCombUpSup()), max(0, ARFullPoolVol() - ArrowReservoir() + ARIn() - (ARPrelim() + AREnergySup() + ARCombUpSup())))
	return(ARFloodSpace_o)
}


###### AREnergySup

AREnergySup <- function() { # initializes the energy parameters
	if (ARFirmEngSup_c == -9999) {
		ARFirmEngSup_c <<- ARFirmEngSup()
	}
	if (ARFirmEngSupReq_c == -9999) {
		ARFirmEngSupReq_c <<- ARFirmEngSupReq()
		energy_df[week_counter,1]<<-ARFirmEngSupReq_c
    }
	if (UseTotalEnergyContentForFirm() == 1) {
		AREnergySup_o <- max(min(ARFirmEngSupReq_c, ARSharedWater()), min(ARNonFirmSupReq(), ARECCSharedWater()))
	} else {
		AREnergySup_o <- max(min(ARFirmEngSupReq_c, ARECCSharedWater()), min(ARNonFirmSupReq(), ARECCSharedWater()))
	}
	return(AREnergySup_o)
}

###### ARFirmEngSupReq

ARFirmEngSupReq <- function() {
  # Release required to produce the firm energy target.  Units af.

  # ARFirmEngSup_c<<-ARFirmEngSup()
	ARFirmEngSupReq_o <- 4.260306e7 * (ARFirmEngSup_c) / (43560 * (TotalGCHead()) * ARCombEfficiency)
	return(ARFirmEngSupReq_o)
}


######## ARFirmEngSup

ARFirmEngSup <- function() {
  #   TotalEnergyContent_c<<-TotalEnergyContent()
  #   TotalECCEnergyContent_c<<-TotalECCEnergyContent()
  #   FirmEnergyDeficit_c<<-FirmEnergyDeficit()
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			ARFirmEngSup_o <- 0
		} else {
			ARFirmEngSup_o <- (AREnergyContent() + ARECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		ARFirmEngSup_o <- 0
	} else {
		ARFirmEngSup_o <- ARECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(ARFirmEngSup_o)
}

######### AREnergyContent

AREnergyContent <- function() {
	AREnergyContent_o <- ARSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AREnergyContent_o)
}

########### ARSharedWater

ARSharedWater <- function() {
	ARSharedWater_o <- max(0, ArrowReservoir() + ARIn() - ArrowNetWith() - ARPrelim() - ARMcNarySup() - ARBotVol)
	return(ARSharedWater_o)
}


##### ARMcNarySup

ARMcNarySup <- function() {
  # print(paste("ARMcNarySup"))
	if (TotalMcNarySharedWater_c == 0) {
		ARMcNarySup_o <- 0
	} else {
		ARMcNarySup_o <- min(ARMcNarySharedWater(), McNaryFlowDeficit() * ARMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(ARMcNarySup_o)
}

####### TotalGCHead

TotalGCHead <- function() {
	TotalGCHead_o <- GCNetHead() + GCDownStreamHead()
	return(TotalGCHead_o)
}

########### ARECCEnergyContent

ARECCEnergyContent <- function() {
	ARECCEnergyContent_o <- ARECCSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(ARECCEnergyContent_o)
}

############ ARECCSharedWater

ARECCSharedWater <- function() {
	ARECCSharedWater_o <- max(0, ArrowReservoir() + ARIn() - ArrowNetWith() - ARMcNarySup() - ARPrelim() - ARECC())
	return(ARECCSharedWater_o)
}

####### ARECC

ARECC <- function() {
	if (UseUpdatedOpSystem == 0) {
		if (month_in_year <= 5 || month_in_year == 12) {
			out1 <- max(ARCriticalCurve(), ARRefillCurve())
		} else {
			out1 <- min(ARRefillCurve(), max(ARCriticalCurve(), AR1931Refill()))
		}
		ARECC_o <- min(ARFloodCurve(), out1)
	} else if (UseUpdatedOpSystem == 1 && UseForecastRLFCurve == 1) {
		if (month_in_year <= 5 || month_in_year == 12) {
			out2 <- ARForecastRLFCurve()
		} else {
			out2 <- ARActualRFCurve()
		}
		ARECC_o <- min(ARFloodCurve(), out2)
	} else {
		if (month_in_year <= 5 || month_in_year == 12) {
			out3 <- AR1931RFCurve()
		} else {
			out3 <- ARActualRFCurve()
		}
		ARECC_o <- min(ARFloodCurve(), out3)
	}
	return(ARECC_o)
}

####### ARCriticalCurve

ARCriticalCurve <- function() {
	ARCriticalCurve_o <- ARCriticalCurve_input[week_counter_in_year(), 2]
	return(ARCriticalCurve_o)
}

####### ARRefillCurve

ARRefillCurve <- function() {
	if (RefillSwitch() == 1) {
		ARRefillCurve_o <- ARRefillVol1
	} else if (RefillSwitch() == 2) {
		ARRefillCurve_o <- ARRefillVol2
	} else {
		ARRefillCurve_o <- LBRefillVol3()
	}
	return(ARRefillCurve_o)
}

###### LBRefillVol3

LBRefillVol3 <- function() {
	LBRefillVol3_o <- 0
	return(LBRefillVol3_o)
}

##### AR1931Refill

AR1931Refill <- function() {
	AR1931Refill_o <- AR1931Refill_input[week_counter_in_year(), 2]
	return(AR1931Refill_o)
}

###### ARForecastRLFCurve

ARForecastRLFCurve <- function() { # check out the timing of time series
	print(paste("make sure the streamflow timing is correctly implemented"))
	ARForecastRLFCurve_o <- ARForecastRLFCurve_input[week_counter_in_year(), 2]
	return(ARForecastRLFCurve_o)
}

####### ARActualRFCurve

ARActualRFCurve <- function() {
	ARActualRFCurve_o <- ARActualRFCurve_input[week_counter_in_year(), 2]
	return(ARActualRFCurve_o)
}

###### AR1931RFCurve

AR1931RFCurve <- function() {
	AR1931RFCurve_o <- AR1931RFCurve_input[week_counter_in_year(), 2]
	return(AR1931RFCurve_o)
}

######### AREnergyContent

AREnergyContent <- function() {
	AREnergyContent_o <- ARSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AREnergyContent_o)
}

### ARNonFirmSupReq

ARNonFirmSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		ARNonFirmSupReq_o <- 4.260306e7 * (ARFirmEngSup_c + ARNonFirmEngSup()) / (43560 * (TotalGCHead()) * ARCombEfficiency)
	} else {
		ARNonFirmSupReq_o <- 0
	}
	return(ARNonFirmSupReq_o)
}

######### ARNonFirmEngSup

ARNonFirmEngSup <- function() {
	# print(paste("energy, here 1"))
	if (NonFirmEnergyDeficit_c == -9999) {
		NonFirmEnergyDeficit_c <<- NonFirmEnergyDeficit()
		energy_df[week_counter,7]<<-NonFirmEnergyDeficit_c
	}
	if (TotalNFEnergyContent_c == 0) {
		ARNonFirmEngSup_o <- 0
	} else {
		ARNonFirmEngSup_o <- ARNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(ARNonFirmEngSup_o)
}

###### ARCombUpSup

ARCombUpSup <- function() {
	ARCombUpSup_o <- MICombSup()
	return(ARCombUpSup_o)
}

######## Arrow ECC and FC
check <- 1

######### Arrow flood curve
# Arrow Flood Control Curves are modified according to January 1995 SRDs which are showed in 2003 Columbia River Treaty Flood Control Operating Plan (by Se-yeun Lee, Dec.05)
# Flood storage curve selected is based on the total run off April-August  for the variable period and is fixed during the rest of the year.
# For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.
ARFloodCurve <- function() {
	if (FC_Option == 1) {
		ARFloodCurve_o <- ARFullPoolVol() - GlobalFloodEvacMult * (ARFullPoolVol() - (AR_CurFC()))
	} else if (FC_Option == 4) {
		ARFloodCurve_o <- ARFullPoolVol() - GlobalFloodEvacMult * (ARFullPoolVol() - (ARCurFC_Cont()))
	} else if (FC_Option == 2) {
		ARFloodCurve_o <- GlobalFloodEvacMult * (ARFullPoolVol() - (AR_HecFC()))
	} else {
		ARFloodCurve_o <- GlobalFloodEvacMult * (ARFullPoolVol() - (AR_ENSO_FC()))
	}
	return(ARFloodCurve_o)
}

#######  AR_CurFC

AR_CurFC <- function() {
	ARFlood1 <- ARFlood[week_counter_in_year(), 2]
	ARFlood2 <- ARFlood[week_counter_in_year(), 3]
	ARFlood3 <- ARFlood[week_counter_in_year(), 4]
	ARFlood4 <- ARFlood[week_counter_in_year(), 5]
	ARFlood5 <- ARFlood[week_counter_in_year(), 6]
	ARFlood6 <- ARFlood[week_counter_in_year(), 7]
	if (DallesRunoffAprAug < 64.0E6) {
		AR_CurFC_o <- ARFlood1
	} else if (DallesRunoffAprAug < 65.0E6) {
		AR_CurFC_o <- ARFlood2
	} else if (DallesRunoffAprAug < 70.0E6) {
		AR_CurFC_o <- ARFlood3
	} else if (DallesRunoffAprAug < 75E6) {
		AR_CurFC_o <- ARFlood4
	} else {
		AR_CurFC_o <- ARFlood5
	}
	return(AR_CurFC_o)
}

######## ARCurFC_Cont ##### CHECK TO make sure it makes sense ==== check mean(MIFlood_DecToApr[,1]) ...
# Refill timing is changed from Jun to Apr (10/24/2007)-Se-yeun Lee
# i.e. Use "ELSE IF (month=9) THEN APR_AR" instead of "ELSE IF (month=9 or month=10 or month=11) THEN APR_AR"

ARCurFC_Cont <- function() {
	DAFlood_Range <- c(60000000, 65000000, 70000000, 75000000, 80000000)
	if (DAFlood_Range[length(DAFlood_Range)] < DallesRunoffAprAug) {
		row_no <- length(DAFlood_Range)
	} else {
		row_no <- max(1.0, which(DAFlood_Range > DallesRunoffAprAug)[1] - 1)
	}
	if (DallesRunoffAprAug <= 64.0E06) {
		ARCurFC_Cont_o <- ARFlood_1May_input[week_counter_in_year(), 2]
	} else if (DallesRunoffAprAug <= 80.0E06) {
		if (month_in_year == 3 || month_in_year == 4) {
			ARCurFC_Cont_o <- 7077300
		} else if (month_in_year == 5) {
			ARCurFC_Cont_o <- 6617300
		} else if (month_in_year == 6) {
			ARCurFC_Cont_o <- ARFloodMonth[row_no, 2] # JAN_AR
		} else if (month_in_year == 7) {
			ARCurFC_Cont_o <- ARFloodMonth[row_no, 3] # FEB_AR
		} else if (month_in_year == 8) {
			ARCurFC_Cont_o <- ARFloodMonth[row_no, 4] # MAR_AR
		} else if (month_in_year == 9 || month_in_year == 10) {
			ARCurFC_Cont_o <- ARFloodMonth[row_no, 5] # APR_AR
		} else {
			ARCurFC_Cont_o <- 7327300
		}
	} else {
		ARCurFC_Cont_o <- ARFlood_May5_i[week_counter_in_year(), 2]
	} # ARFlood_5May
	return(ARCurFC_Cont_o)
}

############# AR_HecFC ##### called when FC_Option=4

AR_HecFC <- function() {
	print(paste("warning: this function is empty right now and it needs to be implemented from ColSim"))
	return(0)
}

############# AR_ENSO_FC ###### called when FC_Option=3
AR_ENSO_FC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

###### AR_April_Evac_Target
# Expected Arrow flood evacuation at the end of April.
# Arrow Flood Control Curves are modified according to January 1995 SRDs which are showed in 2003 Columbia River Treaty Flood Control Operating Plan (by Se-yeun Lee, Dec.05)
# Flood storage curve selected is based on the total run off April-August
# for the variable period and is fixed during the rest of the year.  For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.

AR_April_Evac_Target <- function() {
	if (DallesRunoffAprAug < 64.0E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 2] # ARFlood1_2
	} else if (DallesRunoffAprAug < 65.0E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 3] # ARFlood2_2
	} else if (DallesRunoffAprAug < 70.0E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 4] # ARFlood3_2
	} else if (DallesRunoffAprAug < 75E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 5] # ARFlood4_2
	} else {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 6]
	} # ARFlood5_2
	if (FC_Option == 4) {
		AR_April_Evac_Target_o <- (GlobalFloodEvacMult * (ARFullPoolVol() - ARAprEvaq_Cont()))
	} else {
		AR_April_Evac_Target_o <- (GlobalFloodEvacMult * (ARFullPoolVol() - ARF_o))
	}
	return(AR_April_Evac_Target_o)
}

##### ARAprEvaq_Cont

ARAprEvaq_Cont <- function() {
	if (DallesRunoffAprAug <= 64.0E06) {
		ARAprEvaq_Cont_o <- 6617300
	} else if (DallesRunoffAprAug <= 80.0E06) {
		DAFlood_Range <- c(60000000, 65000000, 70000000, 75000000, 80000000)
		if (DAFlood_Range[length(DAFlood_Range)] < DallesRunoffAprAug) {
			row_no <- length(DAFlood_Range)
		} else {
			row_no <- max(1.0, which(DAFlood_Range > DallesRunoffAprAug)[1] - 1)
		}
	ARAprEvaq_Cont_o <- ARFloodMonth[row_no, 5] # APR_AR
	} else {
		ARAprEvaq_Cont_o <- 3727300
	}
	return(ARAprEvaq_Cont_o)
}

#### InitAR

InitAR <- function() {
	if (InitialConditionSwitch == 0) {
		InitAR_o <- InitARLink
	} else if (InitialConditionSwitch == 1) {
		InitAR_o <- ResInitFractionFull * ARFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitAR_o <- ARHistStor
	} else {
		ARFullPoolVol
	}
	return(InitAR_o)
}

### ARInEvap

ARInEvap <- function() {
	ARInEvap_o <- (ArrowFlowData() - RevFlowData()) - AREvap()
	return(ARInEvap_o)
}

###### ArrowRelease

ArrowRelease <- function() {
	ArrowRelease_o <- max(ARDamProtectRel(), if (OptimizedRelSw == 1) {
    min(ARPrelim(), ARRelLimit())
	} else {
	min(ARPrelim() + ARCombSup() - ARRelReducReq(), ARRelLimit())
	})
	return(ArrowRelease_o)
}

###### ARRelLimit

ARRelLimit <- function() {
	ARRelLimit_o <- max(ArrowReservoir() + ArrowInflow() - ArrowNetWith() - ARBotVol, 0)
	return(ARRelLimit_o)
}

######### ARRelReducReq

ARRelReducReq <- function() {
	ARRelReducReq_o <- TotalRelReducReq() * ARFloodFrac()
	return(ARRelReducReq_o)
}

###### ARFloodFrac

ARFloodFrac <- function() {
	#  TotalFloodSpace_c<<-TotalFloodSpace()
	if (TotalFloodSpace_c == 0) {
		ARFloodFrac_o <- 0
	} else {
		ARFloodFrac_o <- (MIFloodSpace() + ARFloodSpace()) / TotalFloodSpace_c
	}
	return(ARFloodFrac_o)
}

######## ARDamProtectRel

ARDamProtectRel <- function() {
	ARDamProtectRel_o <- max(0, ArrowReservoir() + ArrowInflow() - ArrowNetWith() - ARFullPoolVol())
	return(ARDamProtectRel_o)
}

##################################################
##################################################
##############################################################################################

###### TotalEnergyFromMcNarySups

TotalEnergyFromMcNarySups <- function() {
	TotalEnergyFromMcNarySups_o <- MIMcNarySupEnergy() + ARMcNarySupEnergy() + DUMcNarySupEnergy() + GCMcNaryBONSupEnergy() + HHMcNarySupEnergy() + LBMcNarySupEnergy()
	return(TotalEnergyFromMcNarySups_o)
}

######### MIMcNarySupEnergy

MIMcNarySupEnergy <- function() {
	MIMcNarySupEnergy_o <- MIMcNarySup() * (MINetHead() + TotalGCHead() + RevNetHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIMcNarySupEnergy_o)
}

####### ARMcNarySupEnergy

ARMcNarySupEnergy <- function() {
	ARMcNarySupEnergy_o <- ARMcNarySup() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(ARMcNarySupEnergy_o)
}

######### DUMcNarySupEnergy

DUMcNarySupEnergy <- function() {
	DUMcNarySupEnergy_o <- DUMcNarySup() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt	
	return(DUMcNarySupEnergy_o)
}

#### GCMcNaryBONSupEnergy

GCMcNaryBONSupEnergy <- function() {
	GCMcNaryBONSupEnergy_o <- max(GCCombFlowSup(), BONFlowDeficit()) * (GCNetHead() + GCDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCMcNaryBONSupEnergy_o)
}

####### GCCombFlowSup

GCCombFlowSup <- function() {
	GCCombFlowSup_o <- max(GCMcNarySup(), GCSupForVernitaBar())
	return(GCCombFlowSup_o)
}

##### GCSupForVernitaBar

GCSupForVernitaBar <- function() {
	GCSupForVernitaBar_o <- max(0, VernitaBarFlowTarget() * cfsTOafw - (GCPrelim() + (PriestRapidsFlowData() - GrandCouleeFlowData())))
	return(GCSupForVernitaBar_o)
}

#### VernitaBarFlowTarget

VernitaBarFlowTarget <- function() {
	VernitaBarFlowTarget_o <- VernitaBarFlowTarget_input[month_in_year, 2]
	return(VernitaBarFlowTarget_o)
}

##### PriestRapidsFlowData

PriestRapidsFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		PriestRapidsFlowData_o <- VICPR()
	} else {
		PriestRapidsFlowData_o <- ModPriestRapidsFlowData()
	}
	return(PriestRapidsFlowData_o)
}

######## VICPR

VICPR <- function() {
	VICPR_o <- PriVICPR
	return(VICPR_o)
}

###### Demand between Priest Rapids and Wanapum

PRDem <- function() {
	if (simulate_curtailment == 1) {
		PRDem_o <- DemVICPR - PRCurtail()
	} else {
		PRDem_o <- DemVICPR
	}
	return(PRDem_o)
}

PRCurtail <- function()	{
	if (simulate_curtailment == 1) {
		PRCurtail_0 <- min(DemVICPR, max(IflowPR + DemVICPR - (WaOut() + PRInc()), 0))
		PRCurtail_o <- ifelse(PRCurtail_0 > 0, CurtVICPR, 0)
	} else {
		PRCurtail_o <- 0
	}
	return(PRCurtail_o)
}

###### ModPriestRapidsFlowData

ModPriestRapidsFlowData <- function() {
	ModPriestRapidsFlowData_o <- ModPriestRapidsFlowData_input[week_counter_in_year(), 2]
	return(ModPriestRapidsFlowData_o)
}

####### HHMcNarySupEnergy

HHMcNarySupEnergy <- function() {
	HHMcNarySupEnergy_o <- HHCombFlowSup() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(HHMcNarySupEnergy_o)
}

###### HHCombFlowSup

HHCombFlowSup <- function() {
	HHCombFlowSup_o <- max(HHMcNarySup(), HHSupForVernitaBar())
	return(HHCombFlowSup_o)
}

######### HHSupForVernitaBar

HHSupForVernitaBar <- function() {
  # Minimum Flows at the Vernita Bar are required to be at least 36,000 cfs at all times and approximately 70,000 cfs Dec-May.
  # In actual operations, the Dec-May flow value is determined in part by the previous years flows, but this aspect of the system is not modeled at this time.
  # While in actual practice some storage is available for support of these flows from run-of-river projects,
  # Grand Coulee must generally make sufficient average releases to support these flows on a monthly time frame.
  # The model assumes that Grand Coulee supplies all supplements to natural inflow required to meet the Vernita Bar Target.
	if (SumGCLBHHSharedWater() > 0) {
		HHSupForVernitaBar_o <- GCSupForVernitaBar() * HHVBFrat() * HHSharedWater() / SumGCLBHHSharedWater()
	} else {
		HHSupForVernitaBar_o <- 0
	}
	return(HHSupForVernitaBar_o)
}

######### HHVBFrat

HHVBFrat <- function() {
	HHVBFrat <- 0
	return(HHVBFrat)
}

####### SumGCLBHHSharedWater

SumGCLBHHSharedWater <- function() {
	SumGCLBHHSharedWater_o <- HHVBFrat() * HHSharedWater() + LBVBFact() * LBSharedWater()
	return(SumGCLBHHSharedWater_o)
}

##### LBVBFact

LBVBFact <- function() {
	LBVBFact_o <- 0
	return(LBVBFact_o)
}

####### LBMcNarySupEnergy

LBMcNarySupEnergy <- function() {
	LBMcNarySupEnergy_o <- LBCombFlowSup() * (LBNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBMcNarySupEnergy_o)
}

#### LBCombFlowSup

LBCombFlowSup <- function() {
	LBCombFlowSup_o <- max(LBMcNarySup(), LBSupForVernitaBar())
	return(LBCombFlowSup_o)
}

##### LBSupForVernitaBar

LBSupForVernitaBar <- function() {
	if (SumGCLBHHSharedWater() > 0) {
		LBSupForVernitaBar_o <- GCSupForVernitaBar() * LBVBFact() * LBSharedWater() / SumGCLBHHSharedWater()
	} else {
		LBSupForVernitaBar_o <- 0
	}
	return(LBSupForVernitaBar_o)
}

####### HungryHorsePreEnergy

HungryHorsePreEnergy <- function() {
	HungryHorsePreEnergy_o <- 43560 * (998 * min(HHPrelim(), HHPenLimit()) * 0.028317 * 9.81 * HHNetHead() * 0.3048 * HHCombEfficiency) / 3.6E9
	return(HungryHorsePreEnergy_o)
}

####### HHPenLimit

HHPenLimit <- function() {
	HHpen <- 12048
	HHPenLimit_o <- HHpen * cfsTOafw
	return(HHPenLimit_o)
}

###### LibbyPreEnergy

LibbyPreEnergy <- function() {
	LibbyPreEnergy_o <- 43560 * (998 * min(LBPrelim(), LBPenLimit()) * 0.028317 * 9.81 * LBNetHead() * 0.3048 * LBCombEfficiency) / 3.6E9
	return(LibbyPreEnergy_o)
}

####### LBPenLimit

LBPenLimit <- function() {
	LBGenPenCap <- 24100
	LBPenLimit_o <- LBGenPenCap * cfsTOafw
	return(LBPenLimit_o)
}

###### MicaGrPreEnergy

MicaGrPreEnergy <- function() {
	MicaGrPreEnergy_o <- MIPreEnergy() + RevPreEnergy()
	return(MicaGrPreEnergy_o)
}

######## MIPreEnergy

MIPreEnergy <- function() {
	MIPreEnergy_o <- 43560 * (998 * min(MIPrelim(), MIPenLimit()) * 0.028317 * 9.81 * MINetHead() * 0.3048 * MICombEfficiency) / 3.6E9
	return(MIPreEnergy_o)
}

######## MIPenLimit

MIPenLimit <- function() {
	MIPenCap <- 41600
	MIPenLimit_o <- MIPenCap * cfsTOafw
	return(MIPenLimit_o)
}

##### RevPreEnergy

RevPreEnergy <- function() {
	RevPreEnergy_o <- 43560 * (998 * min(RevPrelim(), RevPenLimit()) * 0.028317 * 9.81 * RevNetHead() * 0.3048 * RevCombEfficiency) / 3.6E9
	return(RevPreEnergy_o)
}

###### KerrGrPreEnergy

KerrGrPreEnergy <- function() {
	KerrGrPreEnergy_o <- CBPreEnergy() + KEPreEnergy() + NOXPreEnergy()
	return(KerrGrPreEnergy_o)
}

###### CBPreEnergy

CBPreEnergy <- function() {
	CBPreEnergy_o <- 43560 * (998 * min(CBPrelim(), CBPenLimit()) * 0.028317 * 9.81 * CBNetHead() * 0.3048 * CBCombEfficiency) / 3.6E9
	return(CBPreEnergy_o)
}

###### CBPrelim

CBPrelim <- function() {
	CBPrelim_o <- KEPrelim() + (CabinetFlowData() - KerrFlowData())
	return(CBPrelim_o)
}

####### CabinetFlowData

CabinetFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		CabinetFlowData_o <- VICCB()
	} else {
		CabinetFlowData_o <- ModCabinetFlowData()
	}
	return(CabinetFlowData_o)
}

####### VICCB

VICCB <- function() {
	VICCB_o <- PriVICCB
	return(VICCB_o)
}

####### AFToCAB

AFToCAB <- function() {
	AFToCAB_o <- 0.8395
	return(AFToCAB_o)
}

##### ModCabinetFlowData

ModCabinetFlowData <- function() {
	ModCabinetFlowData_o <- ModCabinetFlowData_input[week_counter_in_year(), 2]
	return(ModCabinetFlowData_o)
}

######### CBPenLimit

CBPenLimit <- function() {
	CBPenCap <- 35700
	CBPenLimit_o <- CBPenCap * cfsTOafw
	return(CBPenLimit_o)
}

######## KEPreEnergy

KEPreEnergy <- function() {
	KEPreEnergy_o <- 43560 * (998 * min(KEPrelim(), KEPenLimit()) * 0.028317 * 9.81 * KENetHead() * 0.3048 * KECombEfficiency) / 3.6E9
	return(KEPreEnergy_o)
}

##### NOXPreEnergy

NOXPreEnergy <- function() {
	NOXPreEnergy_o <- 43560 * (998 * min(NoxPrelim(), NoxPenLimit()) * 0.028317 * 9.81 * NoxNetHead() * 0.3048 * NoxCombEfficiency) / 3.6E9
	return(NOXPreEnergy_o)
}

######### NoxPrelim

NoxPrelim <- function() {
	NoxPrelim_o <- KEPrelim() + (NoxonFlowData() - KerrFlowData())
	return(NoxPrelim_o)
}

######### NoxonFlowData

NoxonFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		NoxonFlowData_o <- VICNOX()
	} else {
		NoxonFlowData_o <- ModNoxonFlowData
	}
	return(NoxonFlowData_o)
}

###### VICNOX

VICNOX <- function() {
	VICNOX_o <- PriVICNOX
	return(VICNOX_o)
}

######## ModNoxonFlowData

ModNoxonFlowData <- function() {
	ModNoxonFlowData_o <- ModNoxonFlowData_input[week_counter_in_year(), 2]
	return(ModNoxonFlowData_o)
}

############# NoxPenLimit

NoxPenLimit <- function() {
	NoxPenCap <- 50000
	NoxPenLimit_o <- NoxPenCap * cfsTOafw
	return(NoxPenLimit_o)
}

###### AlbeniFallsGroupPreEnergy

AlbeniFallsGroupPreEnergy <- function() {
	AlbeniFallsGroupPreEnergy_o <- AFPreEnergy() + BCPreEnergy() + BDPreEnergy()
	return(AlbeniFallsGroupPreEnergy_o)
}

####### AFPreEnergy

AFPreEnergy <- function() {
	AFPreEnergy_o <- 43560 * (998 * min(AFPrelim(), AFPenLimit()) * 0.028317 * 9.81 * AFNetHead() * 0.3048 * AFCombEfficiency()) / 3.6E9
	return(AFPreEnergy_o)
}

###### BCPreEnergy

BCPreEnergy <- function() {
	BCPreEnergy_o <- 43560 * (998 * min(BCPrelim(), BCPenLimit()) * 0.028317 * 9.81 * BCNetHead() * 0.3048 * BCCombEfficiency) / 3.6E9
	return(BCPreEnergy_o)
}

######## BCPrelim

BCPrelim <- function() {
	BCPrelim_o <- AFPrelim()
	return(BCPrelim_o)
}

######### BCPenLimit

BCPenLimit <- function() {
	BoxCanyonPenCap <- 29000	
	BCPenLimit_o <- BoxCanyonPenCap * cfsTOafw
	return(BCPenLimit_o)
}

####### BDPreEnergy

BDPreEnergy <- function() {
	BDPreEnergy_o <- 43560 * (998 * min(BDPrelim(), BDPenLimit()) * 0.028317 * 9.81 * BDNetHead() * 0.3048 * BDCombEfficiency) / 3.6E9
	return(BDPreEnergy_o)
}

####### BDPrelim

BDPrelim <- function() {
	BDPrelim_o <- AFPrelim() + (BoundaryFlowData() - AlbeniFallFlowData())
	return(BDPrelim_o)
}

####### BoundaryFlowData

BoundaryFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		BoundaryFlowData_o <- VICBD()
	} else {
		BoundaryFlowData_o <- ModBoundaryFlowData()
	}
	return(BoundaryFlowData_o)
}

###### VICBD

VICBD <- function() {
	VICBD_o <- PriVICBD
	return(VICBD_o)
}

####### WANToBD

WANToBD <- function() {
	WANToBD_o <- 0.958
	return(WANToBD_o)
}

###### ModBoundaryFlowData

ModBoundaryFlowData <- function() {
	ModBoundaryFlowData_o <- ModBoundaryFlowData_input[week_counter_in_year(), 2]
	return(ModBoundaryFlowData_o)
}

######## BDPenLimit

BDPenLimit <- function() {
	BDPenCap <- 53000
	BDPenLimit_o <- BDPenCap * cfsTOafw
	return(BDPenLimit_o)
}

####### GrandCouleePreEnergy

GrandCouleePreEnergy <- function() {
	GrandCouleePreEnergy_o <- CJPreEnergy() + GCPreEnergy() + PRPreEnergy() + RIPreEnergy() + RRPreEnergy() + WAPreEnergy() + WEPreEnergy()
	return(GrandCouleePreEnergy_o)
}

###### CJPreEnergy

CJPreEnergy <- function() {
	CJPreEnergy_o <- 43560 * (998 * min(CJPrelim(), CJPenLimit()) * 0.028317 * 9.81 * CJNetHead() * 0.3048 * CJCombEfficiency) / 3.6E9
	return(CJPreEnergy_o)
}

###### CJPrelim

CJPrelim <- function() {
	CJPrelim_o <- GCPrelim() + (ChiefJosephFlowData() - GrandCouleeFlowData())
	return(CJPrelim_o)
}
	
####### ChiefJosephFlowData

ChiefJosephFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		ChiefJosephFlowData_o <- VICCJ()
	} else {
		ChiefJosephFlowData_o <- ModChiefJosephFlowData()
	}
	return(ChiefJosephFlowData_o)
}

##### VICCJ

VICCJ <- function() {
	VICCJ_o <- PriVICCJ
	return(VICCJ_o)
}

#### Demand between Chief Joseph and Grand Coulee

CJDem <- function() {
	if (simulate_curtailment == 1) {
		CJDem_o <- DemVICCJ - CJCurtail()
	} else {
		CJDem_o <- DemVICCJ
	}
	return(CJDem_o)
}

CJCurtail <- function() {
	if (simulate_curtailment == 1) {
		CJCurtail_0 <-  min(DemVICCJ, max(IflowCJ + DemVICCJ - (GCOutflow() + CJInc()), 0))
		CJCurtail_o <- ifelse(CJCurtail_0 > 0, CurtVICCJ, 0)
	} else {	
		CJCurtail_o <- 0
	}
	return(CJCurtail_o)
}

######### ModChiefJosephFlowData

ModChiefJosephFlowData <- function() {
	ModChiefJosephFlowData_o <- ModChiefJosephFlowData_input[week_counter_in_year(), 2]
	return(ModChiefJosephFlowData_o)
}

##### CJPenLimit

CJPenLimit <- function() {
	CJPenCap <- 219000
	CJPenLimit_o <- CJPenCap * cfsTOafw
	return(CJPenLimit_o)
}

###### GCPreEnergy

GCPreEnergy <- function() {
	GCPreEnergy_o <- 43560 * (998 * min(GCPrelim(), GCPenLimit()) * 0.028317 * 9.81 * GCNetHead() * 0.3048 * GCCombEfficiency) / 3.6E9
	return(GCPreEnergy_o)
}

###### GCPenLimit

GCPenLimit <- function() {
	GCPenCap <- 280000
	GCPenLimit_o <- GCPenCap * cfsTOafw
	return(GCPenLimit_o)
}

######## PRPreEnergy

PRPreEnergy <- function() {
	PRPreEnergy_o <- 43560 * (998 * min(PRPrelim(), PRPenLimit()) * 0.028317 * 9.81 * PRNetHead() * 0.3048 * PRCombEfficiency) / 3.6E9
	return(PRPreEnergy_o)
}

##### PRPrelim

PRPrelim <- function() {
	PRPrelim_o <- GCPrelim() + (PriestRapidsFlowData() - GrandCouleeFlowData())
	return(PRPrelim_o)
}

##### PRPenLimit

PRPenLimit <- function() {
	PRPenCap <- 187000
	PRPenLimit_o <- PRPenCap * cfsTOafw
	return(PRPenLimit_o)
}

###### RIPreEnergy

RIPreEnergy <- function() {
	RIPreEnergy_o <- 43560 * (998 * min(RIPrelim(), RIPenLimit()) * 0.028317 * 9.81 * RINetHead() * 0.3048 * RICombEfficiency) / 3.6E9
	return(RIPreEnergy_o)
}

###### RIPrelim

RIPrelim <- function() {
	RIPrelim_o <- GCPrelim() + (RockIslandFlowData() - GrandCouleeFlowData())
	return(RIPrelim_o)
}

#### RockIslandFlowData

RockIslandFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		RockIslandFlowData_o <- VICRI()
	} else {
		RockIslandFlowData_o <- ModRockIslandFlowData()
	}
	return(RockIslandFlowData_o)
}

##### VICRI

VICRI <- function() {
	VICRI_o <- PriVICRI
	return(VICRI_o)
}

##### Demand between Rock Island and Rocky Reach

RIDem <- function() {
	if (simulate_curtailment == 1) {
		RIDem_o <- DemVICRI - RICurtail()
	} else {
		RIDem_o <- DemVICRI
	}
	return(RIDem_o)
}

RICurtail <- function() {
	if (simulate_curtailment == 1) {
		RICurtail_0 <- min(DemVICRI, max(IflowRI + DemVICRI - (RROut() + RIInc()), 0))
		RICurtail_o <- ifelse(RICurtail_0 > 0, CurtVICRI, 0)
	} else {
		RICurtail_o <- 0
	}
	return(RICurtail_o)
}


### PRToRI

PRToRI <- function() {
	PRToRI_o <- 0.9911
	return(PRToRI_o)
}

##### ModRockIslandFlowData

ModRockIslandFlowData <- function() {	
	ModRockIslandFlowData_o <- ModRockIslandFlowData_input[week_counter_in_year(), 2]
	return(ModRockIslandFlowData_o)
}

##### RIPenLimit

RIPenLimit <- function() {
	RIPenCap <- 220000
	RIPenLimit_o <- RIPenCap * cfsTOafw
	return(RIPenLimit_o)
}

####### RRPreEnergy

RRPreEnergy <- function() {
	RRPreEnergy_o <- 43560 * (998 * min(RRPrelim(), RRPenLimit()) * 0.028317 * 9.81 * RRNetHead() * 0.3048 * RRCombEfficiency) / 3.6E9
	return(RRPreEnergy_o)
}

###### RRPrelim

RRPrelim <- function() {
	RRPrelim_o <- GCPrelim() + (RockyReachFlowData() - GrandCouleeFlowData())
	return(RRPrelim_o)
}

####### RockyReachFlowData

RockyReachFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		RockyReachFlowData_o <- VICRR()
	} else {
		RockyReachFlowData_o <- ModRockyReachFlowData()
	}
	return(RockyReachFlowData_o)
}

######## VICRR

VICRR <- function() {
	VICRR_o <- PriVICRR
	return(VICRR_o)
}

##### Demand between Rocky Reach and Wells

RRDem <- function() {
	if (simulate_curtailment == 1) {
		RRDem_o <- DemVICRR - RRCurtail()
	} else {
		RRDem_o <- DemVICRR
	}
	return(RRDem_o)
}

RRCurtail <- function() {
	if (simulate_curtailment == 1) {
		RRCurtail_0 <- min(DemVICRR, max(IflowRR + DemVICRR - (WeOut() + RRInc()), 0))
		RRCurtail_o <- ifelse(RRCurtail_0 > 0, CurtVICRR, 0)
	} else {
		RRCurtail_o <- 0
	}
	return(RRCurtail_o)
}

##### PRToRR

PRToRR <- function() {
	PRToRR_o <- 0.965
	return(PRToRR_o)
}

######## ModRockyReachFlowData

ModRockyReachFlowData <- function() {
	ModRockyReachFlowData_o <- ModRockyReachFlowData_input[week_counter_in_year(), 2]
	return(ModRockyReachFlowData_o)
}

##### RRPenLimit

RRPenLimit <- function() {
	RRPenCap <- 220000
	RRPenLimit_o <- RRPenCap * cfsTOafw
	return(RRPenLimit_o)
}

##### WAPreEnergy

WAPreEnergy <- function() {
	WAPreEnergy_o <- 43560 * (998 * min(WAPrelim(), WAPenLimit()) * 0.028317 * 9.81 * WANetHead() * 0.3048 * WACombEfficiency) / 3.6E9
	return(WAPreEnergy_o)
}

####### WAPrelim

WAPrelim <- function() {
	WAPrelim_o <- GCPrelim() + (WanapumFlowData() - GrandCouleeFlowData())
	return(WAPrelim_o)
}

#### WanapumFlowData

WanapumFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		WanapumFlowData_o <- VICWA()
	} else {
		WanapumFlowData_o <- ModWanapumFlowData()
	}
	return(WanapumFlowData_o)
}

######## VICWA

VICWA <- function() {
	VICWA_o <- PriVICWA * LowerColMult()
	return(VICWA_o)
}

######## Demand between Wanapum and Rock Island

WADem <- function() {
	if (simulate_curtailment == 1) {
		WADem_o <- DemVICWA - WACurtail()
	} else {
		WADem_o <- DemVICWA
	}
	return(WADem_o)
}

WACurtail <- function() {
	if (simulate_curtailment == 1) {
		WACurtail_0 <- min(DemVICWA, max(IflowWA + DemVICWA - (RIOut() + WAInc()), 0))
		WACurtail_o <- ifelse(WACurtail_0 > 0, CurtVICWA, 0)
	} else {
		WACurtail_o <- 0
	}
	return(WACurtail_o)
}

#### PRToWA

PRToWA <- function() {
	PRToWA_o <- 0.9914
	return(PRToWA_o)
}

###### ModWanapumFlowData

ModWanapumFlowData <- function() {
	ModWanapumFlowData_o <- ModWanapumFlowData_input[week_counter_in_year(), 2]
	return(ModWanapumFlowData_o)
}

####### WAPenLimit

WAPenLimit <- function() {
	WAPenCap <- 178000
	WAPenLimit_o <- WAPenCap * cfsTOafw
	return(WAPenLimit_o)
}

######### WEPreEnergy

WEPreEnergy <- function() {
	WEPreEnergy_o <- 43560 * (998 * min(WEPrelim(), WEPenLimit()) * 0.028317 * 9.81 * WENetHead() * 0.3048 * WECombEfficiency) / 3.6E9
	return(WEPreEnergy_o)	
}

###### WEPrelim

WEPrelim <- function() {
	WEPrelim_o <- GCPrelim() + (WellsFlowData() - GrandCouleeFlowData())
	return(WEPrelim_o)
}

######### WellsFlowData

WellsFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		WellsFlowData_o <- VICWE()
	} else {
		WellsFlowData_o <- ModWellsFlowData()
	}
	return(WellsFlowData_o)
}

###### VICWE

VICWE <- function() {
	VICWE_o <- PriVICWE * LowerColMult()
	return(VICWE_o)
}

###### Demand between Wells and Chief Joseph

WEDem <- function() {
	if (simulate_curtailment == 1) {
		WEDem_o <- DemVICWE - WECurtail()
	} else {
		WEDem_o <- DemVICWE
	}
	return(WEDem_o)
}

WECurtail <- function() {
	if (simulate_curtailment == 1) {
		WECurtail_0 <- min(DemVICWE, max(IflowWE + DemVICWE - (CJOut() + WEInc()), 0))
		WECurtail_o <- ifelse(WECurtail_0 > 0, CurtVICWE, 0)
	} else {
		WECurtail_o <- 0
	}
	return(WECurtail_o)
}

######### CJToWE

CJToWE <- function() {
	CJToWE_o <- 1.042
	return(CJToWE_o)
}

###### ModWellsFlowData

ModWellsFlowData <- function() {
	ModWellsFlowData_o <- ModWellsFlowData_input[week_counter_in_year(), 2]
	return(ModWellsFlowData_o)
}

###### WEPenLimit

WEPenLimit <- function() {
	WEPenCap <- 220000
	WEPenLimit_o <- WEPenCap * cfsTOafw
	return(WEPenLimit_o)
}

###### DworshakGroupPreEnergy

DworshakGroupPreEnergy <- function() {
	DworshakGroupPreEnergy_o <- DWPreEnergy() + IHPreEnergy() + LGPreEnergy() + LiGPreEnergy() + LMPreEnergy()
	return(DworshakGroupPreEnergy_o)
}

##### DWPreEnergy

DWPreEnergy <- function() {
	DWPreEnergy_o <- 43560 * (998 * min(DWPrelim(), DWPenLimit()) * 0.028317 * 9.81 * DWNetHead() * 0.3048 * DWCombEfficiency) / 3.6E9
	return(DWPreEnergy_o)
}

#### DWPenLimit

DWPenLimit <- function() {
	DWGenPenCap <- 24000
	DWPenLimit_o <- DWGenPenCap * cfsTOafw
	return(DWPenLimit_o)
}

##### IHPreEnergy

IHPreEnergy <- function() {
	IHPreEnergy_o <- 43560 * (998 * min(IHPrelim(), IHPenLimit()) * 0.028317 * 9.81 * IHNetHead() * 0.3048 * IHCombEfficiency) / 3.6E9
	return(IHPreEnergy_o)
}

###### IHPrelim

IHPrelim <- function() {
	IHPrelim_o <- (BRPrelim_c + DWPrelim()) + (IceHarborFlowData() - DworshakFlowData() - BrownleeFlowData())
	return(IHPrelim_o)
}

###### IceHarborFlowData

IceHarborFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		IceHarborFlowData_o <- VICIH()
	} else {
		IceHarborFlowData_o <- ModIceHarborFlowData()
	}
	return(IceHarborFlowData_o)
}

####### VICIH

VICIH <- function() {
	VICIH_o <- PriVICIH * SnakeMult()
	return(VICIH_o)
}

####### Demand between Ice Harbor and Lower Monument

IHDem <- function() {
	IHDem_o <- DemVICIH
	return(IHDem_o)
}

####### ModIceHarborFlowData

ModIceHarborFlowData <- function() {
	ModIceHarborFlowData_o <- ModIceHarborFlowData_input[week_counter_in_year(), 2]
	return(ModIceHarborFlowData_o)
}

######## IHPenLimit

IHPenLimit <- function() {
	IHPenCap <- 106000
	IHPenLimit_o <- IHPenCap * cfsTOafw
	return(IHPenLimit_o)
}

###### LGPreEnergy

LGPreEnergy <- function() {
	LGPreEnergy_o <- 43560 * (998 * min(LGPrelim(), LGPenLimit()) * 0.028317 * 9.81 * LGNetHead() * 0.3048 * LGCombEfficiency) / 3.6E9
	return(LGPreEnergy_o)
}

###### LGPrelim

LGPrelim <- function() {
    LGPrelim_o <- (BRPrelim_c + DWPrelim()) + (LowerGraniteFlowData() - DworshakFlowData() - BrownleeFlowData())
	return(LGPrelim_o)
}

###### LGPenLimit

LGPenLimit <- function() {
	LGPenCap <- 130000
	LGPenLimit_o <- LGPenCap * cfsTOafw
	return(LGPenLimit_o)
}

####### LiGPreEnergy

LiGPreEnergy <- function() {
	LiGPreEnergy_o <- 43560 * (998 * min(LiGPrelim(), LiGPenLimit()) * 0.028317 * 9.81 * LiGNetHead() * 0.3048 * LiGCombEfficiency) / 3.6E9
	return(LiGPreEnergy_o)
}

####### LiGPrelim

LiGPrelim <- function() {
	LiGPrelim_o <- (BRPrelim_c + DWPrelim()) + (LittleGooseFlowData() - DworshakFlowData() - BrownleeFlowData())
	return(LiGPrelim_o)
}

###### LittleGooseFlowData

LittleGooseFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		LittleGooseFlowData_o <- VICLiG()
	} else {
		LittleGooseFlowData_o <- ModLittleGooseFlowData()
	}
	return(LittleGooseFlowData_o)
}

######## VICLIG

VICLiG <- function() {
	VICLiG_o <- PriVICLiG * SnakeMult()
	return(VICLiG_o)
}

#### IHToLIG

IHToLIG <- function() {
	IHToLIG_o <- 0.992
	return(IHToLIG_o)
}

### ModLittleGooseFlowData

ModLittleGooseFlowData <- function() {
	ModLittleGooseFlowData_o <- ModLittleGooseFlowData_input[week_counter_in_year(), 2]
	return(ModLittleGooseFlowData_o)
}

##### LiGPenLimit

LiGPenLimit <- function() {
	LiGPenCap <- 130000
	LiGPenLimit_o <- LiGPenCap * cfsTOafw
	return(LiGPenLimit_o)
}

######## LMPreEnergy

LMPreEnergy <- function() {
	LMPreEnergy_o <- 43560 * (998 * min(LMPrelim(), LMPenLimit()) * 0.028317 * 9.81 * LMNetHead() * 0.3048 * LMCombEfficiency) / 3.6E9
	return(LMPreEnergy_o)
}

###### LMPrelim

LMPrelim <- function() {
	LMPrelim_o <- (BRPrelim_c + DWPrelim()) + (LowerMonuFlowData() - DworshakFlowData() - BrownleeFlowData())
	return(LMPrelim_o)
}

###### LowerMonuFlowData

LowerMonuFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		LowerMonuFlowData_o <- VICLM()
	} else {
		LowerMonuFlowData_o <- ModLowerMonuFlowData()
	}
	return(LowerMonuFlowData_o)
}

###### VICLM

VICLM <- function() {
	VICLM_o <- PriVICLM * SnakeMult()
	return(VICLM_o)
}

######## IHToLM

IHToLM <- function() {
	IHToLM_o <- 1
	return(IHToLM_o)
}

######## ModLowerMonuFlowData

ModLowerMonuFlowData <- function() {
	ModLowerMonuFlowData_o <- ModLowerMonuFlowData_input[week_counter_in_year(), 2]
	return(ModLowerMonuFlowData_o)
}

###### LMPenLimit

LMPenLimit <- function() {
	LMPenCap <- 130000
	LMPenLimit_o <- LMPenCap * cfsTOafw
	return(LMPenLimit_o)
}

#### LowerColPreEnergy

LowerColPreEnergy <- function() {
	LowerColPreEnergy_o <- BONPreEnergy() + DAPreEnergy() + JDPreEnergy() + McPreEnergy()
	return(LowerColPreEnergy_o)
}

####### BONPreEnergy

BONPreEnergy <- function() {
	BONPreEnergy_o <- 43560 * (998 * min(BONPrelim(), BONPenLimit()) * 0.028317 * 9.81 * BONNetHead() * 0.3048 * BONCombEfficiency) / 3.6E9
	return(BONPreEnergy_o)
}

##### BONPenLimit

BONPenLimit <- function() {
	BONPenCap <- 288000
	BONPenLimit_o <- BONPenCap * cfsTOafw
	return(BONPenLimit_o)
}

####### DAPreEnergy

DAPreEnergy <- function() {
	DAPreEnergy_o <- 43560 * (998 * min(DaPrelim(), DAPenLimit()) * 0.028317 * 9.81 * DANetHead() * 0.3048 * DACombEfficiency) / 3.6E9
	return(DAPreEnergy_o)
}

###### DaPrelim

DaPrelim <- function() {
	#print(paste("now here 1"))
	DaPrelim_o <- LowerPrelimUpFlow_c + DallesFlowData()
	return(DaPrelim_o)
}

###### DallesFlowData

DallesFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		DallesFlowData_o <- VICDA()
	} else {
		DallesFlowData_o <- ModDallesFlowData()
	}
	return(DallesFlowData_o)
}

##### VICDA

VICDA <- function() {
	VICDA_o <- PriVICDA * SnakeMult()
	return(VICDA_o)
}

####### Demand between Dalles and John Day

DADem <- function() {
	if (simulate_curtailment == 1) {
		DADem_o <- DemVICDA - DACurtail()
	} else {
		DADem_o <- DemVICDA
	}
	return(DADem_o)
}

DACurtail <- function() {
	if (simulate_curtailment == 1) {
		DACurtail_0 <- min(DemVICDA, max(IflowDA + DemVICDA - (JDOut() + DAInc()), 0))
		DACurtail_o <- ifelse(DACurtail_0 > 0, CurtVICDA, 0)
	} else {
		DACurtail_o <- 0
	}
	return(DACurtail_o)
}

######## ModDallesFlowData

ModDallesFlowData <- function() {
	ModDallesFlowData_o <- ModDallesFlowData_input[week_counter_in_year(), 2]
	return(ModDallesFlowData_o)
}

###### DAPenLimit

DAPenLimit <- function() {
	DAPenCap <- 375000
	DAPenLimit_o <- DAPenCap * cfsTOafw
	return(DAPenLimit_o)
}

####### JDPreEnergy

JDPreEnergy <- function() {
	JDPreEnergy_o <- 43560 * (998 * min(JDPrelim(), JDPenLimit()) * 0.028317 * 9.81 * JDNetHead() * 0.3048 * JDCombEfficiency) / 3.6E9
	return(JDPreEnergy_o)
}

####### JDPrelim

JDPrelim <- function() {
	# print(paste("now here 2"))
	JDPrelim_o <- LowerPrelimUpFlow_c + JohnDayFlowData()
	return(JDPrelim_o)
}

###### JohnDayFlowData

JohnDayFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		JohnDayFlowData_o <- VICJD()
	} else {
		JohnDayFlowData_o <- ModJohnDayFlowData()
	}
	return(JohnDayFlowData_o)
}

########## VICJD

VICJD <- function() {
	VICJD_o <- PriVICJD * SnakeMult()
	return(VICJD_o)
}

#### Demand between John Day and McNary

JDDem <- function() {
	if (simulate_curtailment == 1) {
		JDDem_o <- DemVICJD - JDCurtail()
	} else {
		JDDem_o <- DemVICJD
	}
	return(JDDem_o)
}

JDCurtail <- function() {
	if (simulate_curtailment == 1) {
		JDCurtail_0 <- min(DemVICJD, max(IflowJD + DemVICJD - (McNOut() + JDInc()), 0))
		JDCurtail_o <- ifelse(JDCurtail_0 > 0, CurtVICJD, 0)
	} else {
		JDCurtail_o <- 0
	}
	return(JDCurtail_o)
}

#### DAToJD

DAToJD <- function() {
	DAToJD_o <- 0.968
	return(DAToJD_o)
}

###### ModJohnDayFlowData

ModJohnDayFlowData <- function() {
	ModJohnDayFlowData_o <- ModJohnDayFlowData_input[week_counter_in_year(), 2]
	return(ModJohnDayFlowData_o)
}

######## JDPenLimit

JDPenLimit <- function() {
	JDPenCap <- 322000
	JDPenLimit_o <- JDPenCap * cfsTOafw
	return(JDPenLimit_o)
}

##### McPreEnergy

McPreEnergy <- function() {
	# print(paste("pre energy"))
	McPreEnergy_o <- 43560 * (998 * min(McPrelim(), MCPenLimit()) * 0.028317 * 9.81 * MCNetHead() * 0.3048 * MCCombEfficiency) / 3.6E9
	return(McPreEnergy_o)
}

###### MCPenLimit

MCPenLimit <- function() {
	MCPenCap <- 232000
	MCPenLimit_o <- MCPenCap * cfsTOafw
	return(MCPenLimit_o)
}

# GCFloodSpace ------------------------------------------------------------

##### GCFloodSpace

GCFloodSpace <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCFloodSpace_o <- min(
    (GCPrelim() + GCCombUpSup() + GCUpMcNarySup() + GCMcNarySup() + GCEnergySup()),
    max(0, GCFullPoolVol - GrandCoulee() + GCIn_c - (GCPrelim() + GCEnergySup() + GCMcNarySup() + GCCombUpSup() + GCUpMcNarySup()))
	)
	return(GCFloodSpace_o)
}

##### GCCombUpSup

GCCombUpSup <- function() {
	GCCombUpSup_o <- AFCombSup() + ARCombSup() + CLCombSup()
	return(GCCombUpSup_o)
}

####### AFCombSup

AFCombSup <- function() {
	AFCombSup_o <- KECombSup()
	return(AFCombSup_o)
}

####### KECombSup

KECombSup <- function() {
	KECombSup_o <- HHCombSup()
	return(KECombSup_o)
}

###### HHCombSup

HHCombSup <- function() {
	HHCombSup_o <- min(HHEngSup(), HHEnergySupAllow()) + HHCombFlowSup()
	return(HHCombSup_o)
}

###### HHEngSup

HHEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		HHEngSup_o <- max(min(HHFirmEngSupReq(), HHSharedWater()), min(HHNonFirmEngSupReq(), HHECCSharedWater()))
	} else {
		HHEngSup_o <- max(MIN(HHFirmEngSupReq(), HHECCSharedWater()), min(HHNonFirmEngSupReq(), HHECCSharedWater()))
	}
	return(HHEngSup_o)
}

####### HHFirmEngSupReq

HHFirmEngSupReq <- function() {
	HHFirmEngSupReq_o <- min(HHPenLimit(), 4.260306e7 * (HHFirmEngSup()) / (43560 * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
	return(HHFirmEngSupReq_o)
}

#### HHNonFirmEngSupReq

HHNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		HHNonFirmEngSupReq_o <- min(
		HHPenLimit(),
		4.260306e7 * (HHFirmEngSup() + HHNonFirmEngSup()) / (43560 * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency)
		)
	} else {
		HHNonFirmEngSupReq_o <- 0
	}
	return(HHNonFirmEngSupReq_o)
}

###### HHNonFirmEngSup

HHNonFirmEngSup <- function() {

  #  print(paste("energy, here 2"))

  if (NonFirmEnergyDeficit_c == -9999) {
    NonFirmEnergyDeficit_c <- NonFirmEnergyDeficit()
    energy_df[week_counter,7]<<-NonFirmEnergyDeficit_c
  }

  if (TotalNFEnergyContent_c == 0) {
    HHNonFirmEngSup_o <- 0
  } else {
    HHNonFirmEngSup_o <- HHNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
  }

  return(HHNonFirmEngSup_o)
}


#### HHEnergySupAllow

HHEnergySupAllow <- function() {
	HHEnergySupAllow_o <- max(0, min(HHColFallsmax() - HHPrelim(), HH_USBRmax() * cfsTOafw - HHPrelim()))
	return(HHEnergySupAllow_o)
}

##### HHColFallsmax

HHColFallsmax <- function() {
	HHColFallsmax_o <- max(0, (ColFallsMaxFlow() * cfsTOafw) - (ColumbiaFallsFlowData() - HungryHFlowData()))
	return(HHColFallsmax_o)
}

######## ColFallsMaxFlow

ColFallsMaxFlow <- function() {
# Maximum flow at Columbia Falls.  This flow is maintained unless it conflicts with flood storage evacuation requirements.  Units cfs.
	ColFallsMaxFlow_o <- ColFallsMaxFlow_input[month_in_year, 2]
	return(ColFallsMaxFlow_o)
}

##### HH_USBRmax
# Maximum outflow limited to turbine capacity 12,048 cfs
# Limited June-Labor Day by 6,800, unless this interferes with ESA reqs.

HH_USBRmax <- function() {
	HH_USBRmax_o <- HH_USBRmax_input[month_in_year, 2]
	return(HH_USBRmax_o)
}

##### CLCombSup

CLCombSup <- function() {
	CLCombSup_o <- DUCombSup() + LBCombSup()
	return(CLCombSup_o)
}

###### DUCombSup

DUCombSup <- function() {
	DUCombSup_o <- DUEnergySup() + DUMcNarySup()
	return(DUCombSup_o)
}

##### DUEnergySup

DUEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		DUEnergySup_o <- max(min(DUFirmEngSupReq(), DUSharedWater()), min(DUNonFirmSupReq(), DUECCSharedWater()))
	} else {
		DUEnergySup_o <- max(min(DUFirmEngSupReq(), DUECCSharedWater()), min(DUNonFirmSupReq(), DUECCSharedWater()))
	}
	return(DUEnergySup_o)
}

####### DUFirmEngSupReq

DUFirmEngSupReq <- function() {
	DUFirmEngSupReq_o <- 4.260306e7 * (DUFirmEngSup()) / (43560 * (TotalGCHead()) * DUCombEfficiency)
	return(DUFirmEngSupReq_o)
}

##### DUNonFirmSupReq

DUNonFirmSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		DUNonFirmSupReq_o <- 4.260306e7 * (DUFirmEngSup() + DUNonFirmEngSup()) / (43560 * (TotalGCHead()) * DUCombEfficiency)
	} else {
		DUNonFirmSupReq_o <- 0
	}
	return(DUNonFirmSupReq_o)
}

###### DUNonFirmEngSup

DUNonFirmEngSup <- function() {
	# print(paste("energy, here 3"))
	if (TotalNFEnergyContent_c == 0) {
		DUNonFirmEngSup_o <- 0
	} else {
		DUNonFirmEngSup_o <- DUNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(DUNonFirmEngSup_o)
}

###### LBCombSup

LBCombSup <- function() {
	LBCombSup_o <- LBCombFlowSup() + LBEnergySup()
	return(LBCombSup_o)
}

###### LBEnergySup

LBEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		LBEnergySup_o <- max(min(LBFirmEngSupReq(), LBSharedWater()), min(LBNonFirmEngSupReq(), LBECCSharedWater()))
	} else {
		LBEnergySup_o <- max(min(LBFirmEngSupReq(), LBECCSharedWater()), min(LBNonFirmEngSupReq(), LBECCSharedWater()))
	}
	return(LBEnergySup_o)
}

####### LBFirmEngSupReq

LBFirmEngSupReq <- function() {
	LBFirmEngSupReq_o <- min(LBPenLimit(), 4.260306e7 * (LBFirmEngSup()) / (43560 * (LBNetHead() + TotalGCHead()) * LBCombEfficiency))
	return(LBFirmEngSupReq_o)
}

####### LBNonFirmEngSupReq

LBNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		LBNonFirmEngSupReq_o <- min(LBPenLimit(), 4.260306e7 * (LBFirmEngSup() + LBNonFirmEngSup()) / (43560 * (LBNetHead() + TotalGCHead()) * LBCombEfficiency))
	} else {
		LBNonFirmEngSupReq_o <- 0
	}
	return(LBNonFirmEngSupReq_o)
}

###### LBNonFirmEngSup

LBNonFirmEngSup <- function() {
	# print(paste("energy, here 4"))
	if (TotalNFEnergyContent_c == 0) {
		LBNonFirmEngSup_o <- 0
	} else {
		LBNonFirmEngSup_o <- LBNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(LBNonFirmEngSup_o)
}

####### ARCombSup

ARCombSup <- function() {
	ARCombSup_o <- ARCombUpSup() + AREnergySup() + ARMcNarySup()
	return(ARCombSup_o)
}

##### GCUpMcNarySup

GCUpMcNarySup <- function() {
	GCUpMcNarySup_o <- MIMcNarySup() + ARMcNarySup() + DUMcNarySup() + HHMcNarySup() + LBMcNarySup()
	return(GCUpMcNarySup_o)
}

####### GCEnergySup

GCEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		GCEnergySup_o <- max(min(GCFirmEngSupReq(), GCSharedWater()), min(GCNonFirmEngSupReq(), GCECCSharedWater()))
	} else {
		GCEnergySup_o <- max(min(GCFirmEngSupReq(), GCECCSharedWater()), min(GCNonFirmEngSupReq(), GCECCSharedWater()))
	}
	return(GCEnergySup_o)
}

##### GCFirmEngSupReq

GCFirmEngSupReq <- function() {
	GCFirmEngSupReq_o <- min(GCPenLimit(), 4.260306e7 * (GCFirmEngSup()) / (43560 * (GCNetHead() + GCDownStreamHead()) * GCCombEfficiency))
	return(GCFirmEngSupReq_o)
}

##### GCNonFirmEngSupReq

GCNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		GCNonFirmEngSupReq_o <- min(
		GCPenLimit(),
		4.260306e7 * (GCFirmEngSup() + GCNonFirmEngSup()) / (43560 * (GCNetHead() + GCDownStreamHead()) * GCCombEfficiency)
		)
	} else {
		GCNonFirmEngSupReq_o <- 0
	}
	return(GCNonFirmEngSupReq_o)
}

####### GCNonFirmEngSup

GCNonFirmEngSup <- function() {
	# print(paste("energy, here 6"))
	if (TotalNFEnergyContent_c == 0) {
		GCNonFirmEngSup_o <- 0
	} else {
		GCNonFirmEngSup_o <- GCEngContMult * GCNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(GCNonFirmEngSup_o)
}

############################################
#########################################################
################################################################
###########

MIFloodMult <- 1.25
LBFloodMult <- 1.25

# McNary Flow Target ------------------------------------------------------
# The Corps designates Libby, and the USBR designates Grand Coulee and Hungry Horse to contribute to the Federal Columbia River Power System (FCRPS)
# goal of meeting the following BiOp flow objectives:
# Spring (4/10 - 6/30)     #Priest Rapids (135 kcfs)
# Spring (4/20 - 6/30)**   #McNary Dam (220-260 kcfs)
# Summer (7/1 - 8/31)      #McNary Dam (200 kcfs)
# Interpolation between 220 and 260 kcfs is based upon Corps of Engineers data submittal, which bases targets on forecasts of Jan-July flow at The Dalles.

TotalMcNarySharedWater <- function() {
	TotalMcNarySharedWater_o <- MIMcNarySharedWater() + DuMcNarySharedWater() + ARMcNarySharedWater() + GCMcNarySharedWater() + HHMcNarySharedWater() + LBMcNarySharedWater()
	return(TotalMcNarySharedWater_o)
}

##### TotalFloodSpace

TotalFloodSpace <- function() {
	TotalFloodSpace_o <- MIFloodMult * MIFloodSpace() + ARFloodSpace() + GCFloodSpace() + LBFloodMult * LBFloodSpace() + HHFloodSpace() + KRFloodSpace() + DWFloodSpace()
	return(TotalFloodSpace_o)
}

###### TotalRelReducReq

TotalRelReducReq <- function() {
	TotalRelReducReq_o <- max(0, (DaPrelim() - (DAFloodTarget() * cfsTOafw)))
	return(TotalRelReducReq_o)
}

######## DAFloodTarget

DAFloodTarget <- function() {
	DAFloodTarget_o <- max(400000, if (DallesRunoffAprAug > 120E6) {
		DAHighFloodTarget()
	} else {
		DALowFloodTarget()
	})
	return(DAFloodTarget_o)
}

##### DALowFloodTarget

DALowFloodTarget <- function() {
	DALowFloodTarget_o <- DALowFloodTarget_input[month_in_year, 2]
	return(DALowFloodTarget_o)
}

###### DAHighFloodTarget

DAHighFloodTarget <- function() {
	DAHighFloodTarget_o <- DAHighFloodTarget_input[month_in_year, 2]
	return(DAHighFloodTarget_o)
}

###### GCDownStreamHead

GCDownStreamHead <- function() {
	GCDownStreamHead_o <- BONNetHead() + CJNetHead() + DANetHead() + JDNetHead() + MCNetHead() + PRNetHead() + RINetHead() + RRNetHead() + WANetHead() + WENetHead()
	return(GCDownStreamHead_o)
}

###### BONNetHead

BONNetHead <- function() {
	BONNetHead_o <- BONNetHead_input[month_in_year, 2]
	return(BONNetHead_o)
}

###### CJNetHead

CJNetHead <- function() {
	CJNetHead_o <- 167
	return(CJNetHead_o)
}

####### DANetHead

DANetHead <- function() {
	DANetHead_o <- 80.14
	return(DANetHead_o)
}

####### JDNetHead

JDNetHead <- function() {
	JDNetHead_o <- 100
	return(JDNetHead_o)
}

##### MCNetHead

MCNetHead <- function() {
	MCNetHead_o <- 74
	return(MCNetHead_o)
}

###### PRNetHead

PRNetHead <- function() {
	PRNetHead_o <- 76.5
	return(PRNetHead_o)
}

####### RINetHead

RINetHead <- function() {
	RINetHead_o <- 34.4
	return(RINetHead_o)
}

#### RRNetHead

RRNetHead <- function() {
	RRNetHead_o <- 86.5
	return(RRNetHead_o)
}

##### WANetHead

WANetHead <- function() {
	WANetHead_o <- 77.8
	return(WANetHead_o)
}

###### WENetHead

WENetHead <- function() {
	WENetHead_o <- 66.9
	return(WENetHead_o)
}

###### IHNetHead

IHNetHead <- function() {
	IHNetHead_o <- 98
	return(IHNetHead_o)
}

######### LGNetHead

LGNetHead <- function() {
	LGNetHead_o <- 100
	return(LGNetHead_o)
}

###### LiGNetHead

LiGNetHead <- function() {
	LiGNetHead_o <- 98
	return(LiGNetHead_o)
}

###### LMNetHead

LMNetHead <- function() {
	LMNetHead_o <- 100
	return(LMNetHead_o)
}

######### BCNetHead

BCNetHead <- function() {  
	BCNetHead_o <- 152
	return(BCNetHead_o)
}

######### CBNetHead

CBNetHead <- function() {
	CBNetHead_o <- 97.2
	return(CBNetHead_o)
}

######## NoxNetHead

NoxNetHead <- function() {
	NoxNetHead_o <- 152
	return(NoxNetHead_o)
}

####### HHDownStreamHead

BDNetHead <- function() {
	BDNetHead_o <- 97.2
	return(BDNetHead_o)
}

########## TotalEnergyContent (MWhr)

TotalEnergyContent <- function() {
	TotalEnergyContent_o <- DWEnergyContent() + GCEngContMult * GCEnergyContent() + HHEnergyContent() + LBEnergyContent() + MIEnergyContent() + AREnergyContent() + DUEnergyContent()
	return(TotalEnergyContent_o)
}

####### TotalECCEnergyContent

TotalECCEnergyContent <- function() {
	TotalECCEnergyContent_o <-
    HHECCEnergyContent() + LBECCEnergyContent() + MIECCEnergyContent() + GCEngContMult * GCECCEnergyContent() + DWECCEnergyContent() + ARECCEnergyContent() + DUECCEnergyContent()
	return(TotalECCEnergyContent_o)
}

######### FirmEnergyDeficit

FirmEnergyDeficit <- function() {
	# TotalCoordPreEnergy_c<<-TotalCoordPreEnergy()
	FirmEnergyDeficit_o <- max(0, EnergyAllocSafeFactor * FirmEnergyTarget() - TotalCoordPreEnergy_c)
	return(FirmEnergyDeficit_o)
}

######### FirmEnergyTarget

FirmEnergyTarget <- function() {
	FirmEnergyTarget_o <- AvgFirmLoad() * (Deviation__From_Normal_Curve * FirmFraction() + 1)
	return(FirmEnergyTarget_o)
}

####### AvgFirmLoad
# Average firm energy target.  This value is multiplied by the seasonal fraction to yield the firm energy target for each month.  Units MW-hr/month.

AvgFirmLoad <- function() {
	AvgFirmLoad_o <- 1.20E+06
	return(AvgFirmLoad_o)
}

###### FirmFraction

FirmFraction <- function() {
	FirmFraction_o <- FirmFraction_input[week_counter_in_year(), 2]
	return(FirmFraction_o)
}

###### TotalCoordPreEnergy

TotalCoordPreEnergy <- function() {
	TotalCoordPreEnergy_o <- TotalEnergyFromMcNarySups() + HungryHorsePreEnergy() + LibbyPreEnergy() + MicaGrPreEnergy() + KerrGrPreEnergy() +
    AlbeniFallsGroupPreEnergy() + GrandCouleePreEnergy() + DworshakGroupPreEnergy() + LowerColPreEnergy()
	return(TotalCoordPreEnergy_o)
}

###### TotalNFEnergyContent

TotalNFEnergyContent <- function() {
	TotalNFEnergyContent_o <- ARNFEnergyContent() + DUNFEnergyContent() + DWNFEnergyContent() +
    GCEngContMult * GCNFEnergyContent() + HHNFEnergyContent() + LBNFEnergyContent() + MINFEnergyContent()
	return(TotalNFEnergyContent_o)
}

####### ARNFEnergyContent

ARNFEnergyContent <- function() {
	ARNFEnergyContent_o <- max(0, ARECCEnergyContent() - ARFirmEngSup_c)
	return(ARNFEnergyContent_o)
}

######## DUNFEnergyContent

DUNFEnergyContent <- function() {
	DUNFEnergyContent_o <- max(0, DUECCEnergyContent() - DUFirmEngSup())
	return(DUNFEnergyContent_o)
}

##### DUFirmEngSup

DUFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			DUFirmEngSup_o <- 0
		} else {
			DUFirmEngSup_o <- (DUEnergyContent() + DUECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		DUFirmEngSup_o <- 0
	} else {
		DUFirmEngSup_o <- DUECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(DUFirmEngSup_o)
}

##### DWNFEnergyContent

DWNFEnergyContent <- function() {
	DWNFEnergyContent_o <- max(0, DWECCEnergyContent() - DWFirmEngSup())
	return(DWNFEnergyContent_o)
}

###### DWFirmEngSup

DWFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			DWFirmEngSup_o <- 0
		} else {
			DWFirmEngSup_o <- (DWEnergyContent() + DWECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		DWFirmEngSup_o <- 0
	} else {
		DWFirmEngSup_o <- DWECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(DWFirmEngSup_o)
}

###### GCNFEnergyContent

GCNFEnergyContent <- function() {
	GCNFEnergyContent_o <- max(0, GCECCEnergyContent() - GCFirmEngSup())
	return(GCNFEnergyContent_o)
}

##### GCFirmEngSup

GCFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			GCFirmEngSup_o <- 0
		} else {
			GCFirmEngSup_o <- GCEngContMult * (GCEnergyContent() + GCECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		GCFirmEngSup_o <- 0
	} else {
		GCFirmEngSup_o <- GCEngContMult * GCECCEnergyContent / TotalECCEnergyContent * FirmEnergyDeficit
	}
	return(GCFirmEngSup_o)
}

######## HHNFEnergyContent

HHNFEnergyContent <- function() {
	HHNFEnergyContent_o <- max(0, HHECCEnergyContent() - HHFirmEngSup())
	return(HHNFEnergyContent_o)
}

#### HHFirmEngSup

HHFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			HHFirmEngSup_o <- 0
		} else {
			HHFirmEngSup_o <- (HHEnergyContent() + HHECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		HHFirmEngSup_o <- 0
	} else {
		HHFirmEngSup_o <- HHECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(HHFirmEngSup_o)
}

####### LBNFEnergyContent

LBNFEnergyContent <- function() {
	LBNFEnergyContent_o <- max(0, LBECCEnergyContent() - LBFirmEngSup())
	return(LBNFEnergyContent_o)
}

####### LBFirmEngSup

LBFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			LBFirmEngSup_o <- 0
		} else {
			LBFirmEngSup_o <- (LBEnergyContent() + LBECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		LBFirmEngSup_o <- 0
	} else {
		LBFirmEngSup_o <- LBECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(LBFirmEngSup_o)
}

######## MINFEnergyContent

MINFEnergyContent <- function() {
	MINFEnergyContent_o <- max(0, MIECCEnergyContent() - MIFirmEngSup())
	return(MINFEnergyContent_o)
}

########## MIFirmEngSup

MIFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			MIFirmEngSup_o <- 0
		} else {
			MIFirmEngSup_o <- (MIEnergyContent() + MIECCEnergyContent()) / (TotalEnergyContent_c + MIECCEnergyContent()) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		MIFirmEngSup_o <- 0
	} else {
		MIFirmEngSup_o <- MIECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(MIFirmEngSup_o)
}

###### NonFirmEnergyDeficit

NonFirmEnergyDeficit <- function() {
	NonFirmEnergyDeficit_c <- max(0, EnergyAllocSafeFactor * NonFirmEnergyTarget() - max(0, (TotalCoordPreEnergy_c - FirmEnergyTarget())))
	return(NonFirmEnergyDeficit_c)
}

############## NonFirmEnergyTarget

NonFirmEnergyTarget <- function() {
	# Non-firm energy target by month.  Units MW-hr/month.
	# IF UseAlternateNonFirmTarget = 0 THEN NonFirmFraction*ClimateForecastEnergyMult
	# ELSE
	# IF month<=6 THEN ClimateForecastEnergyMult*AltAvgNonFirmLoad*AltNonFirmFraction
	# ELSE NonFirmFraction*AvgNonFirmLoad
	if (UseAlternateNonFirmTarget == 0) {
		NonFirmEnergyTarget_o <- NonFirmFraction() * AvgNonFirmLoad()
	} else {
		NonFirmEnergyTarget_o <- AltNonFirmLoad()
	}
	return(NonFirmEnergyTarget_o)
}

###### NonFirmFraction

NonFirmFraction <- function() {
	NonFirmFraction_o <- NonFirmFraction_input[week_counter_in_year(), 2]
	return(NonFirmFraction_o)
}

###### AvgNonFirmLoad

AvgNonFirmLoad <- function() {
	# Yearly average non-firm energy load.  Units MWhr.
	# Status quo average non-firm load.
	# Use this control when UseAlternateNonFirmTarget is set to a value of 0.
	AvgNonFirmLoad_o <- 0.2617 * AvgFirmLoad()
	# for some reason the output of this function is different in the original ColSim the value is always
	# 2.9 * 10^6
	AvgNonFirmLoad_o <- 2.9 * 10^6
	return(AvgNonFirmLoad_o)
}

######## AltNonFirmLoad

AltNonFirmLoad <- function() {
	AltNonFirmLoad_o <- AltNonFirmLoad_input[week_counter_in_year(), 2]
	return(AltNonFirmLoad_o)
}

######## LBFloodSpace

LBFloodSpace <- function() {
	LBFloodSpace_o <- min((LBPrelim() + LBMcNarySup() + LBEnergySup()), max(0, LBFullPoolVol - Libby() + LBIn() - LBPrelim() - LBEnergySup() - LBMcNarySup()))
	return(LBFloodSpace_o)
}

####### HHFloodSpace

HHFloodSpace <- function() {
	HHFloodSpace_o <- 0 # min((HHPrelim()+HHEnergySupAllow()),max(0,HHFullPoolVol-HungryHorse()+HHIn()-(HHPrelim()+HHEnergySupAllow())))*0
	return(HHFloodSpace_o)
}

######## KRFloodSpace

KRFloodSpace <- function() {
	KRFloodSpace_o <- 0 # min((KEPrelim()+KECombSup()), max(0,KEFullPoolVol-Kerr_Reservoir()+KEIn()-(KECombSup())))*0
	return(KRFloodSpace_o)
}

######## DWFloodSpace

DWFloodSpace <- function() {
	DWFloodSpace_o <- 0 # min((DWPrelim() + DWEnergySup), max(0, DWFullPoolVol - Dworshak + DWIn() - (DWPrelim + DWEnergySup))) * 0
	return(DWFloodSpace_o)
}

#################################
#######################################################
###################################################################

InitialConditionSwitch <- 2 # Options: #0:  Use values imported from spreadsheet. #1:  Use fixed multiplier for whole basin (fraction of full pool)
# 2:  Use historic storage value at update time step
InitDULink <- 1423400
ResInitFractionFull <- 0
DUFullPoolVol <- 1.4234000E+06
Import_VIC_Flows <- 1
Use_Observed_Flows <- 0
DUBotVol <- 2.568e4

##### DuMcNarySharedWater

DuMcNarySharedWater <- function() {
	DuMcNarySharedWater_o <- max(0, Duncan() + DuncanIn() - DuncanNetWith() - DUPrelim() - DUMcNaryDraftLimit())	
	return(DuMcNarySharedWater_o)
}

######### Duncan

Duncan <- function() {
	if (week_counter == 1) {
		Duncan_o <- InitDU()
		# reservoir_vol_df[week_counter,3]=Duncan_o
	} else {
		Duncan_o <- reservoir_vol_df[week_counter - 1, 3]
	}
	return(Duncan_o)
}

####### InitDU

InitDU <- function() {
	if (InitialConditionSwitch == 0) {
		InitDU_o <- InitDULink
	} else if (InitialConditionSwitch == 1) {
		InitDU_o <- ResInitFractionFull * DUFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitDU_o <- DUHistStor(week_counter)
	} else {
		InitDU_o <- DUFullPoolVol
	}
	return(InitDU_o)
}

###### DUHistStor

DUHistStor <- function(week_counter) {
	DUHistStor_o <- HistStor[week_counter, 4]
	return(DUHistStor_o)
}

##### DuncanIn

DuncanIn <- function() {
	DuncanIn_o <- DuncanFlowData() - DUEvap()
	return(DuncanIn_o)
}

##### DuncanFlowData

DuncanFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		DuncanFlowData_o <- VICDU()
	} else {
		DuncanFlowData_o <- ModDuncanFlowData()
	}
	###### check "ModDuncanFlowData()" has not been implemented yet
	return(DuncanFlowData_o)
}

######## ModDuncanFlowData

ModDuncanFlowData <- function() {
	print(paste("check ModDuncanFlowData() has not been implemented yet"))
	###### check "ModDuncanFlowData()" has not been implemented yet
	return(0)
}

##### InflowDataSwitch
InflowDataSwitch <- function() {
	if (Use_Observed_Flows == 1) {
		InflowDataSwitch_o <- 0
	} else if (Import_VIC_Flows == 1) {
		InflowDataSwitch_o <- 1
	}
	return(InflowDataSwitch_o)
}

###### VICDU ####### check "UpperColMult", it seems to be wrong

VICDU <- function() {
	if (Use4trDegreeInQAppx == 1) {
		VICDU_o1 <- PriVICDU
	} else {
		VICDU_o1 <- PriVICDU - DUEstEvap() - DUWith()
	}
	VICDU_o <- VICDU_o1 * UpperColMult() # check
	return(VICDU_o)
}

############ DUEstEvap

DUEstEvap <- function() { # in the original ColSim this is an annual time series with 52 rows, but the values of all of them are zero
	DUEstEvap_O <- 0
	return(DUEstEvap_O)
}

###### DUWith
DUWith <- function() {
	if (MarketHydrgy == 0) {
		DUWith_o <- DUEstWith()
	} else {
		DUWith_o <- DUAgWith() + DUAgRech()
	}
	return(DUWith_o)
}

######## DUEstWith
DUEstWith <- function() { # in the original ColSim this is an annual time series with 52 rows, but the values of all of them are zero
	DUEstWith_o <- 0
	return(DUEstWith_o)
}

######## DUAgWith
DUAgWith <- function() { # in the original ColSim this is an annual time series with 52 rows, but the values of all of them are zero
	DUAgWith_o <- 0
	return(DUAgWith_o)
}

####### DUAgRech
DUAgRech <- function() { # in the original ColSim this is an annual time series with 52 rows, but the values of all of them are zero
	DUAgRech_o <- 0
	return(DUAgRech_o)
}

##### DUEvap

DUEvap <- function() {
	DUEvapData <- 0
	DUEvap_o <- DUSufaceArea() * DUEvapData * 0.5042 / 12
	return(DUEvap_o)
}

##### DUSufaceArea

DUSufaceArea <- function() {
	DUSufaceArea_o <- (-1.21281443E-13 * (Duncan() / 1000)^4 + 1.53692112E-09 * (Duncan() / 1000)^3 - 6.75961255E-06 * (Duncan() / 1000)^2 + 1.87278268E-02 * (Duncan() / 1000) + 2.30403996) * 1000
	return(DUSufaceArea_o)
}

############ DuncanNetWith

DuncanNetWith <- function() {
	DuncanNetWith_o <- 0
	return(DuncanNetWith_o)
}

# DUPrelim ----------------------------------------------------------------

DUPrelim <- function() {
	if (OptimizedRelSw == 1) {
		DUPrelim_o <- max(DUOpRel_4(), DURuleReq())
	} else {
		DUPrelim_o <- min(DUAvailAfter(), max(DURuleReq(), DUMinReq()))
	}
	return(DUPrelim_o)
}

##### DUOpRel_4

DUOpRel_4 <- function() {
	DUOpRel_4_o <- 0
	return(DUOpRel_4_o)
}

######### DURuleReq

DURuleReq <- function() {
	DURuleReq_o <- max(Duncan() + DuncanIn() - DuncanNetWith() - DUTopVol(), 0)
	return(DURuleReq_o)
}

####### DUTopVol

DUTopVol <- function() {
	if (TopRuleSw() == 0) {
		DUTopVol_o <- DUFloodCurve()
	} else if (TopRuleSw() == 1) {
		DUTopVol_o <- DUFullPoolVol
	} else {
		DUTopVol_o <- DUFlood_input[week_counter_in_year(), 2]
	} # DUFlood1
	return(DUTopVol_o)
}

##### DUFloodCurve

DUFloodCurve <- function() {
	if (FC_Option == 1) {
		DUFloodCurve_o <- DUFullPoolVol - GlobalFloodEvacMult * (DUFullPoolVol - DU_CurFC())
	} else if (FC_Option == 4) {
		DUFloodCurve_o <- DUFullPoolVol - GlobalFloodEvacMult * (DUFullPoolVol - DUCurFC_Cont())
	} else if (FC_Option == 2) {
		DUFloodCurve_o <- GlobalFloodEvacMult * (DUFullPoolVol - DU_HecFC())
	} else {
		DUFloodCurve_o <- GlobalFloodEvacMult * (DUFullPoolVol - DU_ENSOFC())
	}
	return(DUFloodCurve_o)
}

######### DU_CurFC

DU_CurFC <- function() {
	if (DURunoffAprAug < 1.4E6) {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 2] # DUFlood1
	} else if (DURunoffAprAug < 1.6E6) {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 3] # DUFlood2
	} else if (DURunoffAprAug < 1.8E6) {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 4] # DUFlood3
	} else {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 5]
	} # DUFlood4}
	return(DU_CurFC_o)
}

########  DUCurFC_Cont()

DUCurFC_Cont <- function() {
	DURunof_Range <- c(1400000, 1600000, 1800000, 2000000)
	if (DURunof_Range[length(DURunof_Range)] < DURunoffAprAug) {
		row_no <- length(DURunof_Range)
	} else {
		row_no <- max(1.0, which(DURunof_Range > DURunoffAprAug)[1] - 1)
	}
	if (DURunoffAprAug <= 2.0E06) {
		if (month_in_year == 5) {
			DUCurFC_Cont_o <- 1023400
		} else if (month_in_year == 6) {
			DUCurFC_Cont_o <- DU_JAN_FAMAJ[row_no, 2] # JAN_DU # check
		} else if (month_in_year == 7 || month_in_year == 8 || month_in_year == 9 || month_in_year == 10 || month_in_year == 11) {
			DUCurFC_Cont_o <- DU_JAN_FAMAJ[row_no, 3] # FMAMJ_DU #check
		} else {
			DUCurFC_Cont_o <- 1423400
		}
	} else {
		DUCurFC_Cont_o <- DUFlood_input[week_counter_in_year(), 5]
	} # DUFlood4
	return(DUCurFC_Cont_o)
}

############# DU_HecFC ##### called when FC_Option=4

DU_HecFC <- function() {
	print(paste("warning: this function is empty right now and it needs to be implemented from ColSim"))
	return(0)
}

############# DU_ENSOFC ###### called when FC_Option=3
DU_ENSOFC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

######### DUMinReq
# Minimum release requirement converted to cfsd.

DUMinReq <- function() {
  DUAvgMin <- 100 # it was a time-series in ColSim but all values were 100 #check
	if (RefillMinSw() == 1) {
		DURefillMin_o <- DURefillMin_input[week_counter_in_year(), 2]
	} else {
		DURefillMin_o <- DUAvgMin * cfsTOafw
	}
	return(DURefillMin_o)
}

########  DUAvailAfter

DUAvailAfter <- function() {
	DUAvailAfter_o <- max(0, Duncan() + DuncanIn() - DuncanNetWith() - DUBotVol)
	return(DUAvailAfter_o)
}

########### DUMcNaryDraftLimit

DUMcNaryDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		DUMcNaryDraftLimit_o <- DUBotVol
	} else if (Fish_Pool_Alternative == 1 || Fish_Pool_Alternative == 2) {
		DUMcNaryDraftLimit_o <- DUFullPoolVol
	} else if (Fish_Pool_Alternative == 3) {
		DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.232E6
	} else if (Fish_Pool_Alternative == 4) {
		DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.464E6
	} else if (Fish_Pool_Alternative == 5) {
		DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.695E6
	} else {
		DUMcNaryDraftLimit_o <- DUFullPoolVol
	}	
	return(DUMcNaryDraftLimit_o)
}

######## Duncun_April_Evac_Target
# Expected Duncan flood evacuation at the end of April.
# Flood storage curve selected is based on the total run off April-August  for the variable period and is fixed during the rest of the year.
# For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.

Duncun_April_Evac_Target <- function() {
	if (DURunoffAprAug < 1.4E6) {
		DUF_o <- DUF_2_input[month_in_year, 2] # DUFlood1_2
	} else if (DURunoffAprAug < 1.6E6) {
		DUF_o <- DUF_2_input[month_in_year, 3] # DUFlood2_2
	} else if (DURunoffAprAug < 1.8E6) {
		DUF_o <- DUF_2_input[month_in_year, 4] # DUFlood3_2
	} else {
		DUF_o <- DUF_2_input[month_in_year, 5]
	} # DUFlood4_2
	if (FC_Option == 4) { # If FC option = 4, then use "Current FC_Continuous_calculated by interpolation"
		Duncun_April_Evac_Target_o <- GlobalFloodEvacMult * (DUFullPoolVol - (DUAprEvaq_Cont()))
	} else {
		Duncun_April_Evac_Target_o <- GlobalFloodEvacMult * (DUFullPoolVol - (DUF_o))
	}
	return(Duncun_April_Evac_Target_o)
}

####### DUAprEvaq_Cont

DUAprEvaq_Cont <- function() {
	if (DURunoffAprAug <= 2.0E06) {
		DUAprEvaq_Cont_o <- FMAMJ_DU()
	} else {
		DUAprEvaq_Cont_o <- 153400
	}
	return(DUAprEvaq_Cont_o)
}

###### FMAMJ_DU

FMAMJ_DU <- function() {
	if (DURunoffAprAug < 1600000) {
		FMAMJ_DU_o <- FMAMJ_DU_input[1, 2]
	} else if (DURunoffAprAug > 1600000 && DURunoffAprAug < 1800000) {
		FMAMJ_DU_o <- FMAMJ_DU_input[2, 2]
	} else if (DURunoffAprAug > 1800000 && DURunoffAprAug < 2000000) {
		FMAMJ_DU_o <- FMAMJ_DU_input[3, 2]
	} else {
		FMAMJ_DU_o <- FMAMJ_DU_input[4, 2]
	}
	return(FMAMJ_DU_o)
}

######## DUEnergyContent

DUEnergyContent <- function() {
	DUEnergyContent_o <- DUSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DUEnergyContent_o)
}

######### DUSharedWater

DUSharedWater <- function() {
	DUSharedWater_o <- max(0, Duncan() + DuncanIn() - DuncanNetWith() - DUPrelim() - DUMcNarySup() - DUBotVol)
	return(DUSharedWater_o)
}

####### DUMcNarySup

DUMcNarySup <- function() {
	# print(paste("DUMcNarySup"))
	if (TotalMcNarySharedWater_c == 0) {
		DUMcNarySup_o <- 0
	} else {
		DUMcNarySup_o <- min(DuMcNarySharedWater(), McNaryFlowDeficit() * DuMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(DUMcNarySup_o)
}

###### DUECCEnergyContent

DUECCEnergyContent <- function() {
	DUECCEnergyContent_o <- DUECCSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DUECCEnergyContent_o)
}

###### DUECCSharedWater

DUECCSharedWater <- function() {
	DUECCSharedWater_o <- max(0, Duncan() + DuncanIn() - DuncanNetWith() - DUPrelim() - DUMcNarySup() - DUECC())
	return(DUECCSharedWater_o)
}

######## DUECC

DUECC <- function() {
	if (UseUpdatedOpSystem == 0) {
		if (month_in_year <= 5 || month_in_year == 12) {
			out1 <- max(DUCriticalCurve(), DURefillCurve())
		} else {
			out1 <- min(max(DURefillCurve(), DUCriticalCurve()), max(DUCriticalCurve(), DU1931Refill()))
		}
		DUECC_o <- min(DUFloodCurve(), out1)
	} else if (UseUpdatedOpSystem == 1 && UseForecastRLFCurve == 1) { # not included
		if (month_in_year <= 5 || month_in_year == 12) {
			out2 <- DUForecastRLFCurve
		} else {
			out2 <- DUActualRFCurve
		}
		DUECC_o <- min(DUFloodCurve(), out2)
	} else { # not included
		if (month_in_year <= 5 || month_in_year == 12) {
			out3 <- DU1931RFCurve
		} else {
			out3 <- DUActualRFCurve
		}
		DUECC_o <- min(DUFloodCurve(), out3)
	}
	return(DUECC_o)
}

######## DUCriticalCurve

DUCriticalCurve <- function() {
	DUCriticalCurve_o <- DUCriticalCurve_input[week_counter_in_year(), 2]
	return(DUCriticalCurve_o)
}

######## DURefillCurve

DURefillCurve <- function() {
	LBRefillVol_4 <- 0
	if (RefillSwitch() == 1) {
		DURefillCurve_o <- DURefillVol1
	} else if (RefillSwitch() == 2) {
		DURefillCurve_o <- DURefillVol
	} else {
		DURefillCurve_o <- LBRefillVol_4
	}
	return(DURefillCurve_o)
}

############ DU1931Refill

DU1931Refill <- function() {
	DU1931Refill_o <- DU1931Refill_input[week_counter_in_year(), 2]
	return(DU1931Refill_o)
}

####### DURelLimit

DURelLimit <- function() {
	DURelLimit_o <- min(DUMaxFCRel(), max(Duncan() + DuncanInflow() - DuncanNetWith() - DUBotVol, 0))
	return(DURelLimit_o)
}

######## DUMaxFCRel

DUMaxFCRel <- function() {
	DUMaxFCRel_o <- DUMaxFCRel_input[week_counter_in_year(), 2]
	return(DUMaxFCRel_o)
}

######## DUDamProtectRel

DUDamProtectRel <- function() {
	DUDamProtectRel_o <- max(0, Duncan() + DuncanInflow() - DuncanNetWith() - DUFullPoolVol)
	return(DUDamProtectRel_o)
}

###############################
########################################
####################################################
######### grand coulee
GCFullPoolVol <- 9.1073000E+06
InitGCLink <- 8332155.071
GCBotVol <- 3.9219E+06
GCTailElev <- 947 # Tailwater elevation.  Units ft.

#######  LowerColMult

LowerColMult <- function() { # check
	LowerColMult_o <- 1
	return(LowerColMult_o)
}

# GCMcNarySharedWater -----------------------------------------------------

####### GCMcNarySharedWater

GCMcNarySharedWater <- function() {
	if(is.na(water_df[week_counter,5])){
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCMcNarySharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCNetWith() - GCPrelim() - GCMcNaryDraftLimit())
	return(GCMcNarySharedWater_o)
}

############ GCMcNaryDraftLimit

GCMcNaryDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		GCMcNaryDraftLimit_o <- GCBotVol
	} else if (Fish_Pool_Alternative == 1) {
		GCMcNaryDraftLimit_o <- 8.316e6
	} else if (Fish_Pool_Alternative == 2) {
		GCMcNaryDraftLimit_o <- GCFullPoolVol - 1.203e6
	} else if (Fish_Pool_Alternative == 3) {
		GCMcNaryDraftLimit_o <- GCFullPoolVol - 1.615e6
	} else if (Fish_Pool_Alternative == 4) {
		GCMcNaryDraftLimit_o <- GCFullPoolVol - 2.292e6
	} else if (Fish_Pool_Alternative == 5) {
		GCMcNaryDraftLimit_o <- GCFullPoolVol - 3.235e6	
	} else {
		GCMcNaryDraftLimit_o <- 8.316e6
	}
	return(GCMcNaryDraftLimit_o)
}

####### GrandCoulee

GrandCoulee <- function() {
	if (week_counter == 1) {
		GrandCoulee_o <- InitGC()
		# reservoir_vol_df[week_counter,7]=GrandCoulee_o
	} else {
		GrandCoulee_o <- reservoir_vol_df[week_counter - 1, 7]
	}
	return(GrandCoulee_o)
}

####### InitGC

InitGC <- function() {
	if (InitialConditionSwitch == 0) {
		InitGC_o <- InitGCLink
	} else if (InitialConditionSwitch == 1) {
		InitGC_o <- ResInitFractionFull * GCFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitGC_o <- GCHistStor()
	} else {
		InitGC_o <- GCFullPoolVol
	}
	return(InitGC_o)
}

######### GCHistStor # check to see if week_counter should be replaced with week_counter_in_year

GCHistStor <- function() {
	GCHistStor_o <- HistStor[week_counter, 8]
	return(GCHistStor_o)
}

###### GCIn

GCIn <- function() {
	if(week_counter == 1) {
		GCIn_o <- AFPrelim() + ARPrelim() + CLPrelim() + (GrandCouleeFlowData() - (AlbeniFallFlowData() + ArrowFlowData() + CorraLinnFlowData())) - GCEvap() - GCUpstreamNetWith() - GCDem()
	} else {
		GCIn_o <- BoundOut() + ARPrelim() + CLPrelim() + (GrandCouleeFlowData() - BoundaryFlowData() - ArrowFlowData() - CorraLinnFlowData()) - GCEvap() - GCUpstreamNetWith() - GCDem()
	}
	return(GCIn_o)
}

###### GrandCouleeFlowData

GrandCouleeFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		GrandCouleeFlowData_o <- VICGC()
	} else {
		GrandCouleeFlowData_o <- ModGrandCouleeFlowData
	}
	return(GrandCouleeFlowData_o)
}

###### VICGC

VICGC <- function() {
	VICGC_o <- PriVICGC * LowerColMult()
	return(VICGC_o)
}

GCDem <- function() {
	GCDem_o <- DemVICGC
	return(GCDem_o)
}

####### CJToGC

CJToGC <- function() {
	CJToGC_o <- 1
	return(CJToGC_o)
}

#### ModGrandCouleeFlowData # has not been implemented yet

ModGrandCouleeFlowData <- function() {
	print(paste("has not been implemented yet"))
	return(0)
}

###### GCEvap

GCEvap <- function() {
	GCEvapData <- 0
	GCEvap_o <- GCSufaceArea() * GCEvapData * 0.5042 / 12
	return(GCEvap_o)
}

###### GCSufaceArea

GCSufaceArea <- function() {
	GCSufaceArea_o <- (-1.21281443E-13 * (GrandCoulee() / 1000)^4 + 1.53692112E-09 * (GrandCoulee() / 1000)^3 -
    6.75961255E-06 * (GrandCoulee() / 1000)^2 + 1.87278268E-02 * (GrandCoulee() / 1000) + 2.30403996) * 1000
	return(GCSufaceArea_o)
}

###### GCUpstreamNetWith

GCUpstreamNetWith <- function() {
	GCUpstreamNetWith_o <- 0
	return(GCUpstreamNetWith_o)
}

#### GCNetWith

GCNetWith <- function() {
	GCNetWith_o <- 0
	return(GCNetWith_o)
}

###### GCPrelim
# Preliminary release based upon rule requirements and minimum releases.
# The release cannot be larger than the available water after withdrawals.  Units cfs-days/m

GCPrelim <- function() {
	GCPrelim_o <- min(GCAvailAfter(), max(GCRuleReq(), GCMinReq()))
	return(GCPrelim_o)
}

##### GCAvailAfter

GCAvailAfter <- function() {
	if(is.na(water_df[week_counter,5])){
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c=water_df[week_counter,5]
	GCAvailAfter_o <- max(0, GrandCoulee() + GCIn_c - GCNetWith() - GCBotVol)
	return(GCAvailAfter_o)
}

####### GCRuleReq

GCRuleReq <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c=water_df[week_counter,5]
	GCRuleReq_o <- max(GrandCoulee() + GCIn_c - GCNetWith() - GCTopVol(), 0)
	return(GCRuleReq_o)
}

###### GCTopVol
# Volume corresponding the rule curve in the units of cfs-days.  Enter conversion formula.  Units cfs-d.

GCTopVol <- function() {
	if (TopRuleSw() == 0) {
		GCTopVol_o <- GCFloodCurve()
	} else if (TopRuleSw() == 1) {
		GCTopVol_o <- GCFullPoolVol
	} else {
		GCTopVol_o <- GCFlood1_input[week_counter_in_year(), 2]
	} # GCFlood1
	return(GCTopVol_o)
}

######## GCFloodCurve
# Grand Coulee Flood Control Curves are modified according to March 1997 SRDs which are showed in USCOE Website
# (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/GCL1997.pdf) (by Se-yeun Lee, Dec.05)
# Draft valuses at end of each month are given Carolyn in COE on Dec. 05
# Flood storage curve selected is based on the total run off April-August
# for the variable period and is fixed during the rest of the year.  For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.

GCFloodCurve <- function() {
	if (FC_Option == 1) {
		GCFloodCurve_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - (GC_CurFC()))
	} else if (FC_Option == 4) {
		GCFloodCurve_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - (GCCurFC_Cont()))
	} else if (FC_Option == 2) {
		GCFloodCurve_o <- GlobalFloodEvacMult * (GCFullPoolVol - (GC_HecFC)) # not implemented
	} else {
		GCFloodCurve_o <- GlobalFloodEvacMult * (GCFullPoolVol - (GC_ENSOFC))
	}  # not implemented
	return(GCFloodCurve_o)
}

###### GCFloodEvacMult

GCFloodEvacMult <- function() { # check # this function seems to have different values for different flood level but I only see one value (1)
	# for all the values of runoff
	# check this function
	GCFloodEvacMult_o <- 1 # this is actually a time series
	return(GCFloodEvacMult_o)
}

# ####### GCRunoffCond
# GCRunoffCond=function(){
#
# if(CorrectedDallesRunoff<57E6) {GCRunoffCond_o=1
# }else if (CorrectedDallesRunoff<60E6) {GCRunoffCond_o=2
# }else if (CorrectedDallesRunoff<63.25E6) {GCRunoffCond_o=3
# }else if (CorrectedDallesRunoff<65E6) {GCRunoffCond_o=4
# }else if (CorrectedDallesRunoff<67.66E6) {GCRunoffCond_o=5
# }else if ( CorrectedDallesRunoff<80.0E6) {GCRunoffCond_o=6
# }else if ( CorrectedDallesRunoff<95.0E6) {GCRunoffCond_o=7
# }else if (CorrectedDallesRunoff<100E6) {GCRunoffCond_o=8
# }else {GCRunoffCond_o=9}
#
#  return(GCRunoffCond_o)
# }


####### GC_CurFC

GC_CurFC <- function() {
	if (CorrectedDallesRunoff() < 57E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood1
	} else if (CorrectedDallesRunoff() < 60E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood2
	} else if (CorrectedDallesRunoff() < 63.25E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood3
	} else if (CorrectedDallesRunoff() < 65E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood4
	} else if (CorrectedDallesRunoff() < 67.66E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood5
	} else if (CorrectedDallesRunoff() < 71.0E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood6
	} else if (CorrectedDallesRunoff() < 80.0E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood7
	} else if (CorrectedDallesRunoff() < 90.0E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood8
	} else if (CorrectedDallesRunoff() < 100E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood9
	} else {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2]
	}
	return(GC_CurFC_o)
}

####### CorrectedDallesRunoff

CorrectedDallesRunoff <- function() {
	CorrectedDallesRunoff_o <- DallesRunoffAprAug - AprilUpstreamFloodEvacGC()
	return(CorrectedDallesRunoff_o)
}

########## AprilUpstreamFloodEvacGC

AprilUpstreamFloodEvacGC <- function() {
	AprilUpstreamFloodEvacGC_o <- (AFFullPoolVol - AFalls_April_Target) + AR_April_Evac_Target() + (CLFullPool - CL_April_Target) + Duncun_April_Evac_Target() + HHorse_April_Evac_Target() +
    (KEFullPoolVol - Kerr_April_Target()) + Libby_April_Evac_Target() + MI_April_Evac_Target()
	return(AprilUpstreamFloodEvacGC_o)
}

############## GCCurFC_Cont

GCCurFC_Cont <- function() {
	CorrectedDallesRunoff_Range <<- c(60000000, 65000000, 70000000, 75000000, 80000000, 85000000, 90000000, 95000000, 100000000, 105000000, 110000000, 115000000)
	if (CorrectedDallesRunoff_Range[length(CorrectedDallesRunoff_Range)] < CorrectedDallesRunoff()) {
		row_no <- length(CorrectedDallesRunoff_Range)
	} else {
		row_no <- max(1.0, which(CorrectedDallesRunoff_Range > CorrectedDallesRunoff())[1] - 1)
	}
	if (CorrectedDallesRunoff() <= 115.0E06) {
		if (month_in_year == 6) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 2] # JAN_GC
		} else if (month_in_year == 7) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 3] # FEB_GC
		} else if (month_in_year == 8) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 4] # MAR_GC
		} else if (month_in_year == 9) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 5] # APR_GC
		} else {
			GCCurFC_Cont_o <- 9107300
		}
	} else {
		GCCurFC_Cont_o <- GCFlood1_input[week_counter_in_year(), 2]
	}
	return(GCCurFC_Cont_o)
}

############# GC_HecFC ###### called when FC_Option=2
GC_HecFC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

############# GC_ENSOFC ###### called when FC_Option=3

GC_ENSOFC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

######### GCMinReq

GCMinReq <- function() {
	if (RefillMinSw() == 1) {
		GCMinReq_o <- GCRefillMin()
	} else {
		GCMinReq_o <- max(GCAvgMin() * cfsTOafw)
	}
	return(GCMinReq_o)
}

###### GCRefillMin

GCRefillMin <- function() {
	GCRefillMin_o <- GCRefillMin_input[week_counter_in_year(), 2]
	return(GCRefillMin_o)
}

###### GCAvgMin

GCAvgMin <- function() {
	GCAvgMin_o <- GCAvgMin_input[week_counter_in_year(), 2]
	return(GCAvgMin_o)
}

####### TotalGCHead

TotalGCHead = function() {
	TotalGCHead_o <- GCNetHead() + GCDownStreamHead()
	return(TotalGCHead_o)
}

######## GCNetHead

GCNetHead <- function() {
	GCLoss <- 0
	GCNetHead_o <- GCElev_ft() - GCTailElev - GCLoss
	return(GCNetHead_o)
}

######## GCElev_ft

GCElev_ft <- function() {
	GCElev_ft_o <- -8.86464496E-13 * GrandCoulee()^2 + 2.72969963E-05 * GrandCoulee() + 1.11465576E+03
	return(GCElev_ft_o)
}

####### GCEnergyContent

GCEnergyContent <- function() {
	GCEnergyContent_o <- GCSharedWater() * (GCNetHead() + GCDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCEnergyContent_o)
}

####### GCSharedWater

GCSharedWater <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCNetWith() - GCPrelim() - GCMcNarySup() - GCBotVol)
	return(GCSharedWater_o)
}

##### GCMcNarySup

GCMcNarySup <- function() {
	# print(paste("GCMcNarySup"))
	if (TotalMcNarySharedWater_c == 0) {
		GCMcNarySup_o <- 0
	} else {
		GCMcNarySup_o <- min(GCMcNarySharedWater(), McNaryFlowDeficit() * GCMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(GCMcNarySup_o)
}

####### GCECCEnergyContent

GCECCEnergyContent <- function() {
	GCECCEnergyContent_o <- GCECCSharedWater() * (GCNetHead() + GCDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCECCEnergyContent_o)
}

########## GCECCSharedWater

GCECCSharedWater <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCECCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCNetWith() - GCPrelim() - GCMcNarySup() - GCECC())
	return(GCECCSharedWater_o)
}

####### GCECC

GCECC <- function() {
	GCECC_o <- min(GCFloodCurve(), max(GCRefillCurve(), max(GCCriticalCurve(), max(GCRecLimit(), GC_VDL()))))
	return(GCECC_o)
}

#### GCRefillCurve

GCRefillCurve <- function() {
	GCRefillVol3 <- 0
	if (RefillSwitch() == 1) {
		GCRefillCurve_o <- GCRefillVol1
	} else if (RefillSwitch() == 2) {
		GCRefillCurve_o <- GCRefillVol2
	} else {
		GCRefillCurve_o <- GCRefillVol3
	}
	return(GCRefillCurve_o)
}

###### GCCriticalCurve

GCCriticalCurve <- function() {
	GCCriticalCurve_o <- GCCriticalCurve_input[week_counter_in_year(), 2]
	return(GCCriticalCurve_o)
}

#### GCRecLimit

GCRecLimit <- function() {
	# Recreation requirements at Grand Coulee require that the ECC and VECC are limited to 1285 ft of elevation (storage=8.712e6 acre ft) from June 30 to Labor Day.
	# This model uses end-of-month storage targets to drive the model
	# calculations so the draft limit is in essence a lower bound for the end of month ECC and VECC for June, July, and August.    Units acre-ft.
	GCRecLimit_o <- GCRecLimit_input[month_in_year, 2]
	return(GCRecLimit_o)
}

GC_VDL <- function() {
	GC_VDL_o <- min(GCTopVol(), max(GCStaticLimit(), max(GCVariableLimit(), max(GCBotVol, GCFerryLimit()))))
	return(GC_VDL_o)
}

GCStaticLimit <- function() {
	if (month_in_year == 8) {
		GCStaticLimit_o <- GCFloodCurve()
	} else if (month_in_year == 9 | month_in_year == 10) {
		GCStaticLimit_o <- min(GCFloodCurve(), PreGCDraftLimit())
	} else if (month_in_year == 1 & DallesFlowDELAY() >= 92.0E6) {
		GCStaticLimit_o <- 8166925
	} else if (month_in_year == 1 & DallesFlowDELAY() < 92.0E6) {
		GCStaticLimit_o <- 8319471
	} else {
		GCStaticLimit_o <- PreGCDraftLimit()
	}
	return(GCStaticLimit_o)
}

###### PreGCDraftLimit

PreGCDraftLimit <- function() {
	PreGCDraftLimit_o <- PreGCDraftLimit_input[week_counter_in_year(), 2]
	return(PreGCDraftLimit_o)
}

##### DallesFlowDELAY

DallesFlowDELAY <- function() { # check   # not sure if it needs the data
	DallesFlowDELAY_o <- input_file[week_counter, 7] # DallesRunoffAprAug
	return(DallesFlowDELAY_o)
}

###### GCVariableLimit

GCVariableLimit <- function() {
	#   Pre Variable Draft Limit
	#
	#   April Refill Target
	#   - GC Inflow
	#   + BiOP Flow Requirements
	#   + expected upstream refill requirements
	#   - expected upstream flood evacuation
	GCVariableLimit_o <- min(GCFloodCurve(), (GC_April_Target() - GCRunoffJanApr + GCEnvirQBdgt() - AprilUpstreamFloodEvacGC()))
	return(GCVariableLimit_o)
}

###### GC_April_Target

GC_April_Target <- function() {
	#   Grand Coulee Flood Control Curves are modified according to March 1997 SRDs which are showed in USCOE Website
	#   (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/GCL1997.pdf) (by Se-yeun Lee, Dec.05)
	#   Draft valuses at end of each month are given Carolyn in COE on Dec. 05
	#   Flood storage curve selected is based on the total run off April-August  for the variable period and is fixed during the rest of the year.
	#   For some dams the decision is based on the forecast of  total inflow to dam itself.
	#   For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.
	if (FC_Option == 1) {
		GC_April_Target_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - (GCCur_Apr())) #
	} else if (FC_Option == 4) {
		GC_April_Target_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - (GCAprEvaq_Cont()))
	} else if (FC_Option == 2) {
		GC_April_Target_o <- GlobalFloodEvacMult * (GCFullPoolVol - (GC_HecFS)) # not implement
	} else {
		GC_April_Target_o <- GlobalFloodEvacMult * (GCFullPoolVol - (GCENSO_Apr)) # not implement
	}
	return(GC_April_Target_o)
}

######### GCCur_Apr

GCCur_Apr <- function() {
	if (CorrectedDallesRunoff() < 57E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood1_2
	} else if (CorrectedDallesRunoff() < 60E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood2_2
	} else if (CorrectedDallesRunoff() < 63.25E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood3_2
	} else if (CorrectedDallesRunoff() < 65E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood4_2
	} else if (CorrectedDallesRunoff() < 67.66E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood5_2
	} else if (CorrectedDallesRunoff() < 71.0E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood6_2
	} else if (CorrectedDallesRunoff() < 80.0E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood7_2
	} else if (CorrectedDallesRunoff() < 90.0E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood8_2
	} else if (CorrectedDallesRunoff() < 100E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood9_2
	} else {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2]
	} # GCFlood10_2
	return(GCCur_Apr_o)
}

####### GCAprEvaq_Cont

GCAprEvaq_Cont <- function() {
	if (CorrectedDallesRunoff() <= 115.0E06) {
		APR_GC <- GCF_month[which(GCF_month[, 1] > CorrectedDallesRunoff())[1], 5]
		GCAprEvaq_Cont_o <- APR_GC
	} else {
		GCAprEvaq_Cont_o <- 3921300
	}	
	return(GCAprEvaq_Cont_o)
}

###### GCEnvirQBdgt

GCEnvirQBdgt <- function() {
	GCEnvirQBdgt_o <- max(GCAbsMinQ(), max(GCBdgtForVB(), 0))
	return(GCEnvirQBdgt_o)
}

###### GCAbsMinQ

GCAbsMinQ <- function() {
	GCAbsMinQ_o <- GCAbsMinQ_input[week_counter_in_year(), 2]
	return(GCAbsMinQ_o)
}

####### GCBdgtForVB

GCBdgtForVB <- function() {
	GCBdgtForVB_o <- GCBdgtForVB_input[week_counter_in_year(), 2]
	return(GCBdgtForVB_o)
}

###### GCFerryLimit

GCFerryLimit <- function() {
	# The Gifford-Inchelium ferry cannot operate if the reservoir is below 1225 ft of elevation (storage=4,775,542 acre ft).
	# This storage is used as a lower bound for the ECC and VECC curve.  Units acre-ft.
	GCFerryLimit_o <- 4.775542e6
	return(GCFerryLimit_o)
}

###### GCInEvap

GCInEvap <- function() {
	GCInEvap_o <- GrandCouleeFlowData() - (BoundaryFlowData() + ArrowFlowData() + CorraLinnFlowData()) - GCEvap()
	return(GCInEvap_o)
}

###### GCDamProtectRel

GCDamProtectRel <- function() {
	GCDamProtectRel_o <- max(0, GrandCoulee() + GCInflow() - GCNetWith() - GCFullPoolVol)
	return(GCDamProtectRel_o)
}

####### GCRelLimit

GCRelLimit <- function() {
	GCRelLimit_o <- max(GrandCoulee() + GCInflow() - GCNetWith() - GCBotVol, 0)
	return(GCRelLimit_o)
}

###### GCLimitedStorage

GCLimitedStorage <- function() {
	GCLimitedStorage_o <- max(0, GrandCoulee() - GC_VDL())
	return(GCLimitedStorage_o)
}

##### GCCombSup

GCCombSup <- function() {
	GCCombSup_o <- GCCombFlowSup() + GCCombUpSup() + GCEnergySup()
	return(GCCombSup_o)
}

##### CJInc

CJInc <- function() {
	CJInc_o <- ChiefJosephFlowData() - GrandCouleeFlowData()
	return(CJInc_o)
}

#### WEInc

WEInc <- function() {
	WEInc_o <- WellsFlowData() - ChiefJosephFlowData()
	return(WEInc_o)
}

###### RRInc

RRInc <- function() {
	RRInc_o <- RockyReachFlowData() - WellsFlowData()
	return(RRInc_o)
}

### RIInc

RIInc <- function() {
	RIInc_o <- RockIslandFlowData() - RockyReachFlowData()
	return(RIInc_o)
}

#### WAInc

WAInc <- function() {
	WAInc_o <- WanapumFlowData() - RockIslandFlowData()
	return(WAInc_o)
}

###### PRInc

PRInc <- function() {
	PRInc_o <- PriestRapidsFlowData() - WanapumFlowData()
	return(PRInc_o)
}

#######################################
##################################################
#############################################################
###### RevPrelim

RevPrelim <- function() {
	RevPrelim_o <- MIPrelim() + (RevFlowData() - MicaFlowData())
	return(RevPrelim_o)
}

##### RevFlowData

RevFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		RevFlowData_o <- VICREV()
	} else {
		RevFlowData_o <- ModRevFlowData()
	}
	return(RevFlowData_o)
}

####### VICREV

VICREV <- function() {
	VICREV_o <- (PriVICRev - REVEstEvap() - REVWith()) * UpperColMult()
	return(VICREV_o)
}

####### REVEstEvap

REVEstEvap <- function() {
	REVEstEvap_o <- 0
	return(REVEstEvap_o)
}

######## REVWith

REVWith <- function() {
	if (MarketHydrgy == 0) {
		REVWith_o <- REVEstWith()
	} else {
		REVWith_o <- REVAgWith() + REVAgRech()
	}
	return(REVWith_o)
}

####### REVEstWith

REVEstWith <- function() {
	REVEstWith_o <- 0
	return(REVEstWith_o)
}

###### REVAgWith

REVAgWith <- function() {
	REVAgWith_o <- REVAgWith_input[week_counter_in_year(), 2]
	return(REVAgWith_o)
}

######## REVAgRech

REVAgRech <- function() {
	REVAgRech_o <- REVAgRech_input[week_counter_in_year(), 2]
	return(REVAgRech_o)
}

###### RevPenLimit

RevPenLimit <- function() {
	RevPenCap <- 56000
	RevPenLimit_o <- RevPenCap * cfsTOafw
	return(RevPenLimit_o)
}

###########################
#############################################
#################################################################

# Albeni Falls

AFBotVol <- 479.4e3
AFFullPoolVol <- 1519e3 # Volume corresponding to 2062.5 ft of elevation.  Normal full pool.  Units acre-ft.
AFalls_April_Target <- 829390
# AFPrelim ----------------------------------------------------------------
# Preliminary release based upon rule requirements and minimum releases.
# The release cannot be larger than the available water after withdrawals.  Units cfs-days/m

AFPrelim <- function() {
	if (OptimizedRelSw == 1) {
		AFPrelim_o <- max(AFOpRel(), AFRuleReq())
	} else {
		AFPrelim_o <- min(AFAvailAfter(), max(AFRuleReq(), AFMinReq()))
	}
	return(AFPrelim_o)
}

###### AFOpRel

AFOpRel <- function() {
	AFOpRel_o <- 0
	return(AFOpRel_o)
}

#### AFRuleReq

AFRuleReq <- function() {
	AFRuleReq_o <- max(AlbeniFalls() + AFIn() - AFNetWith() - AFTopVol(), 0)
	return(AFRuleReq_o)
}

#####  AlbeniFalls

AlbeniFalls <- function() {
	AlbeniFalls_o <- AFTopVol()
	return(AlbeniFalls_o)
}

##### AFTopVol

AFTopVol <- function() {
	AFTopVol_o <- AFFloodCurve()
	return(AFTopVol_o)
}

##### AFFloodCurve

AFFloodCurve <- function() {
	AFFloodCurve_o <- AFFlood[week_counter_in_year(), 2]
	return(AFFloodCurve_o)
}

###### AFIn

AFIn <- function() {
	AFIn_o <- (AlbeniFallFlowData() - KerrFlowData()) + KEPrelim() - AFEvap() - KerrToAFNetWith() - AFDem()
	return(AFIn_o)
}

########## AlbeniFallFlowData

AlbeniFallFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		AlbeniFallFlowData_o <- VICAF()
	} else {
		AlbeniFallFlowData_o <- ModAlbeniFallFlowData()
	}
	return(AlbeniFallFlowData_o)
}

####### VICAF

VICAF <- function() {
	VICAF_o <- MidColMult() * PriVICAF
	return(VICAF_o)
}

#### ModAlbeniFallFlowData # has not been implemented yet

ModAlbeniFallFlowData <- function() {
	print(paste("has not been implemented yet"))
	return(0)
}

#### AFEvap

AFEvap <- function() {
	AFEvapData <- 0
	AFEvap_o <- AFSufacearea() * AFEvapData * 0.5042 / 12
	return(AFEvap_o)
}

####### AFSufacearea

AFSufacearea <- function() {
	AFSufacearea_o <- (-1.21281443E-13 * (AlbeniFalls() / 1000)^4 + 1.53692112E-09 * (AlbeniFalls() / 1000)^3 -
    6.75961255E-06 * (AlbeniFalls() / 1000)^2 + 1.87278268E-02 * (AlbeniFalls() / 1000) + 2.30403996) * 1000
	return(AFSufacearea_o)
}

###### KerrToAFNetWith

KerrToAFNetWith <- function() {
	KerrToAFNetWith_o <- 0
	return(KerrToAFNetWith_o)
}

###### AFNetWith

AFNetWith <- function() {
	AFNetWith_o <- 0
	return(AFNetWith_o)
}

####### AFAvailAfter

AFAvailAfter <- function() {
	AFAvailAfter_o <- max(0, AlbeniFalls() + AFIn() - AFNetWith() - AFBotVol)
	return(AFAvailAfter_o)
}

###### AFMinReq

AFMinReq <- function() {
	AFMinReq_o <- max(min(AFAvailAfter(), AFCombEngRelReq()), AFAvgMin() * cfsTOafw)
	return(AFMinReq_o)
}

##### AFCombEngRelReq
# Release required to produce the firm energy target.  Units af.

AFCombEngRelReq <- function() {
	AFCombEngRelReq_o <- min(AFPenLimit(), 4.2603e7 * (AFFirmEngTarget() + AFNonFirmTarget()) / (43560 * AFNetHead() * AFCombEfficiency()))
	return(AFCombEngRelReq_o)
}

##### AFPenLimit

AFPenLimit <- function() {
	AFPenCap <- 33000
	AFPenLimit_o <- AFPenCap * cfsTOafw
	return(AFPenLimit_o)
}

######## AFFirmEngTarget

AFFirmEngTarget <- function() { # check
	AFFirmEngTarget_o <- 0 # actual object in ColSim is a weekly time series but all the values are zero #why?
	return(AFFirmEngTarget_o)
}

####### AFNonFirmTarget

AFNonFirmTarget <- function() {
	AFNonFirmTarget_o <- 0 # check
	return(AFNonFirmTarget_o)
}

###### AFNetHead
# Net head for power production.  Units ft.

AFNetHead <- function() {
	AFTailElev <- 2042.2 # Tailwater elevation.  Units ft.
	AFLoss <- 0
	AFNetHead_o <- AFElev_ft() - AFTailElev - AFLoss
	return(AFNetHead_o)
}

######## AFElev_ft
# Elevation of water in reservoir from curve fit storage to elevation relationship.  Units ft.

AFElev_ft <- function() {
	AFElev_ft_o <- -4.55570216E-13 * AlbeniFalls()^2 + 1.20161922E-05 * AlbeniFalls() + 2.04508013E+03
	return(AFElev_ft_o)
}

###### AFCombEfficiency

AFCombEfficiency <- function() {
	AFCombEfficiency_o <- 0.8
	return(AFCombEfficiency_o)
}

######### AFAvgMin
# Minimum release from Jim Woodruff Dam.  Units cfs.

AFAvgMin <- function() {
	AFAvgMin_o <- AFAvgMin_input[week_counter_in_year(), 2]
	return(AFAvgMin_o)
}

###### AFInEvap

AFInEvap <- function() {
	AFInEvap_o <- (AlbeniFallFlowData() - CabinetFlowData()) - AFEvap() - AFDem()
	return(AFInEvap_o)
}

#### AF Demands

AFDem <- function() {
	AFDem_o <- DemVICAF
	return(AFDem_o)
}

##### AFRelLimit

AFRelLimit <- function() {
	AFRelLimit_o <- max(AlbeniFalls() + AFInflow() - AFNetWith() - AFBotVol, 0)	
	return(AFRelLimit_o)
}

##### BoundInc

BoundInc <- function() {
	BoundInc_o <- BoundaryFlowData() - AlbeniFallFlowData()
	return(BoundInc_o)
}

############################################
##########################################################
#########################################################################

###### BONFlowDeficit

BONFlowDeficit <- function() {
	BONFlowDeficit_o <- max(0, BONTarget_AcFt() - BONPrelim()) * Chum_Q_Switch()
	return(BONFlowDeficit_o)
}

######## BONTarget_AcFt

BONTarget_AcFt <- function() {
	BONTarget_AcFt_o <- BonnevilleTarget() * cfsTOafw
	return(BONTarget_AcFt_o)
}

###### BonnevilleTarget

BonnevilleTarget <- function() {
	# Grand Coulee is designated to meet the BiOP chum flow requirement at Bonneville Dam
	# 125-160 kcfs
	# November to April 9.
	# The USBR did not specify a method for designating the variable flow target, other than that the critical flow periods will assume the minimum 125 kcfs as a target.
	# The algorithm below uses the same method of ranking critical years as that of McNary flow targets and linearly
	# interpolates the flow targets between forecasted inflows at The Dalles of 85 and 105 MAF between January and July.
	BonnevilleTarget_o <- (Chum_variable_2() + Chum_variable_1())
	return(BonnevilleTarget_o)
}

######### Chum_variable_2

Chum_variable_2 <- function() {
	Chum_variable_2_o <- Chum_variable_2_input[month_in_year, 2]
	return(Chum_variable_2_o)
}

###### Chum_variable_1

Chum_variable_1 <- function() {
	# Variable minflow for Bonneville chum based upon forecasted inflow to the Dalles, for the period of November to April 9.
	# units cfs
	# Flow varies linearly between 125 and 160 kcfs based on inflows of 85 to 105 MAF at the Dalles
	if (month_in_year >= 4 && month_in_year <= 8) {
		if (DallesJanJul <= 85e6) {
			Chum_variable_1_o <- 0
		} else if (DallesJanJul >= 105E6) {
			Chum_variable_1_o <- 35000
		} else {
			Chum_variable_1_o <- (DallesJanJul - 85E6) / 20E6 * 35000
		}	
	} else if (month_in_year == 9) {
		if (DallesJanJul <= 85e6) {
			out2 <- 125000
		} else if (DallesJanJul >= 105E6) {
			out2 <- 160000
		} else {
			out2 <- (DallesJanJul - 85E6) / 20E6 * 35000
		}
		Chum_variable_1_o <- 9 / 30 * out2 + 125000
	} else {
		Chum_variable_1_o <- 0
	}
	return(Chum_variable_1_o)
}

####### BONPrelim

BONPrelim <- function() {
	# print(paste("now here 3"))
	BONPrelim_o <- LowerPrelimUpFlow_c + BonnevilleFlowData()
	return(BONPrelim_o)
}

####### BonnevilleFlowData

BonnevilleFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		BonnevilleFlowData_o <- VICBON()
	} else {
		BonnevilleFlowData_o <- ModBonnevilleFlowData()
	}
	return(BonnevilleFlowData_o)
}

####### VICBON

VICBON <- function() {
	VICBON_o <- PriVICBON * SnakeMult()
	return(VICBON_o)
}

###### Demand between Bonneville and Dalles

BONDem <- function() {
	BONDem_o <- DemVICBON
	return(BONDem_o)
}

###### DAToBON

DAToBON <- function() {
	DAToBON_o <- 1.024
	return(DAToBON_o)
}

###### ModBonnevilleFlowData

ModBonnevilleFlowData <- function() {
	ModBonnevilleFlowData_o <- ModBonnevilleFlowData_input[week_counter_in_year(), 2]
	return(ModBonnevilleFlowData_o)
}

######### BONFlowDeficit

BONFlowDeficit <- function() {
	BONFlowDeficit_o <- max(0, BONTarget_AcFt() - BONPrelim()) * Chum_Q_Switch()
	return(BONFlowDeficit_o)
}

###### Chum_Q_Switch

Chum_Q_Switch <- function() {
	# Toggles the  Bonneville chum target, met by Grand Coulee
	Chum_Q_Switch_o <- 1
	return(Chum_Q_Switch_o)
}

###### BOInc

BOInc <- function() {
	BOInc_o <- BonnevilleFlowData() - DallesFlowData()
	return(BOInc_o)
}

###############################
###########################################
#################################################################

# Kerr

KerrTopVolSw <- 1
KEFullPoolVol <- 1792e3
KEBotVol <- 572.8e3

# Kerr --------------------------------------------------------------------

# Cummulative flow data with agricultural withdrawals (1990 estimates)
# and estimated seasonal evaporation with existing dams included for the entire period of record at a constant level . Units acre-ft/month.

KerrFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		KerrFlowData_o <- VICKE()
	} else {
		KerrFlowData_o <- ModKerrFlowData
	}
	return(KerrFlowData_o)
}

KEDem <- function() {
	KEDem_o <- DemVICKE
	return(KEDem_o)
}

####### VICKE

VICKE <- function() {
	VICKE_o <- MidColMult() * PriVICKE
	return(VICKE_o)
}

##### ModKerrFlowData

ModKerrFlowData <- function() {
	print(paste("this  has not been implemented yet"))
	ModKerrFlowData_o <- 0
	return(ModKerrFlowData_o)
}

# KEPrelim ----------------------------------------------------------------

KEPrelim <- function() {
	if (OptimizedRelSw == 1) {
		KEPrelim_o <- max(KEOpRel(), KERuleReq())
	} else {
		KEPrelim_o <- min(KEAvailAfter(), max(KERuleReq(), KEMinReq()))
	}
	return(KEPrelim_o)
}

###### KEOpRel

KEOpRel <- function() {
	KEOpRel_o <- 0
	return(KEOpRel_o)
}

####### KERuleReq

KERuleReq <- function() {
	KERuleReq_o <- max(Kerr_Reservoir() + KEIn() - KerrNetWith() - KETopVol(), 0)
	return(KERuleReq_o)
}

##### Kerr_Reservoir

Kerr_Reservoir <- function() {
	Kerr_Reservoir_o <- KETopVol()
	return(Kerr_Reservoir_o)
}

#### KETopVol

KETopVol <- function() {
	# Kerr is currently undergoing experimental changes in operation  that essentially hold the pool level at a constant value.
	# This switch allows the user to select the previous rule curves for flood storage, or the fixed full pool volume as the top of the conservation pool.  Options:
	# 0-Fixed full pool
	# 1-Previous flood storage rule curves
	KerrFloodCurve <- KerrFloodC[week_counter_in_year(), 2]
	if (KerrTopVolSw == 0) {
		KETopVol_o <- KEFullPoolVol
	} else {
		KETopVol_o <- KerrFloodCurve
	}
	return(KETopVol_o)
}

####### KEIn

KEIn <- function() {
	KEIn_o <- HHPrelim() + (KerrFlowData() - HungryHFlowData()) - KEEvap() - HHToKENetWith() - KEDem()
	return(KEIn_o)
}

##### KEEvap

KEEvap <- function() {
	KEEvapData <- 0
	KEEvap_o <- KESufaceArea() * KEEvapData * 0.5042 / 12
	return(KEEvap_o)
}

######  KESufaceArea

KESufaceArea <- function() {
	KESufaceArea_o <- Kerr_Reservoir() * 0
	return(KESufaceArea_o)
}

############  HHToKENetWith
# Net withdrawals  between upstream reservoir and generic reservoir.  Units cfs-days.

HHToKENetWith <- function() {
	HHToKENetWith_o <- 0
	return(HHToKENetWith_o)
}

######  KerrNetWith
# Net withdrawal from generic reservoir.  Connect to data icon.  Units cfs-days.

KerrNetWith <- function() {
	KerrNetWith_o <- 0
	return(KerrNetWith_o)
}

######### KEAvailAfter
# Total water available for release after withdrawals and returns  including inflow for the month.  Units cfs-days.

KEAvailAfter <- function() {
	KEAvailAfter_o <- max(0, Kerr_Reservoir() + KEIn() - KerrNetWith() - KEBotVol)
	return(KEAvailAfter_o)
}

######## KEMinReq

KEMinReq <- function() {
	KEMinReq_o <- max(min(KEAvailAfter(), KECombEngRelReq()), Article_56() * cfsTOafw)
	return(KEMinReq_o)
}

######  Article_56
# License Requirement for Kerr - Article 56
# Min Flows for fish (cfs)

Article_56 <- function() {
	Article_56_o <- Article_56_input[week_counter_in_year(), 2]
	return(Article_56_o)
}

######  KECombEngRelReq
# Release required to produce the firm energy target.  Units af.

KECombEngRelReq <- function() {
	KECombEfficiency <- 0.8
	KECombEngRelReq_o <- min(KEPenLimit(), 4.2603e7 * (KEFirmEngTarget() + KENonFirmTarget()) / (43560 * KENetHead() * KECombEfficiency))
	return(KECombEngRelReq_o)
}

###### KEFirmEngTarget # check
# Units MW-hr per month.

KEFirmEngTarget <- function() {
	KEFirmEngTarget_o <- 0 # actual object in ColSim is a weekly time series but all the values are zero #why?
	return(KEFirmEngTarget_o)
}

##### KENonFirmTarget #check

KENonFirmTarget <- function() {
	KENonFirmTarget_o <- 0 # why
	return(KENonFirmTarget_o)
}

##### KEPenLimit

KEPenLimit <- function() {
	KEPenCap <- 14350 # cfs
	KEPenLimit_o <- KEPenCap * cfsTOafw
	return(KEPenLimit_o)
}

###### KENetHead
# Net head for power production.  Units ft.

KENetHead <- function() {
	KETailElev <- 2706 # Tailwater elevation.  Units ft.
	KELoss <- 0 # Piping head losses.  Units ft.
	KENetHead_o <- KEElev_ft() - KETailElev - KELoss
	return(KENetHead_o)
}

####### KEElev_ft

KEElev_ft <- function() {
	KEElev_ft_o <- -3.41281327E-13 * Kerr_Reservoir()^2 + 9.04645500E-06 * Kerr_Reservoir() + 2.87787155E+03
	return(KEElev_ft_o)
}

########## Kerr_April_Target

Kerr_April_Target <- function() {
	Kerr_April_Target_o <- 579583
	return(Kerr_April_Target_o)
}

######## KEInEvap

KEInEvap <- function() {
	KEInEvap_o <- (KerrFlowData() - ColumbiaFallsFlowData()) - KEEvap() - KEDem()
	return(KEInEvap_o)
}

###### KERelLimit

KERelLimit <- function() {
	KERelLimit_o <- max(Kerr_Reservoir() + KerrInflow() - KerrNetWith() - KEBotVol, 0)
	return(KERelLimit_o)
}

##################################
######################################################
#########################################################################

# Hungry Hourse

InitHHLink <- 3286250
HHFullPoolVol <- 3647e3
HHBotVol <- 4.86e5

# HHPrelim ----------------------------------------------------------------

HHPrelim <- function() {
	if (OptimizedRelSw == 1) {
		HHPrelim_o <- max(HHOpRel(), HHRuleReq())
	} else {
		HHPrelim_o <- min(HHAvailAfter(), max(HHRuleReq(), HHMinReq()))
	}
	return(HHPrelim_o)
}

####### HHOpRel

HHOpRel <- function() {
	HHOpRel_o <- 0
	return(HHOpRel_o)
}

##### HHRuleReq

HHRuleReq <- function() {
	HHRuleReq_o <- max(HungryHorse() + HHIn() - HHNetWith() - HHTopVol(), 0)
	return(HHRuleReq_o)
}

##### HungryHorse

HungryHorse <- function() {
	if (week_counter == 1) {
		HungryHorse_o <- InitHH()
		#  reservoir_vol_df[week_counter,6]=HungryHorse_o
	} else {
		HungryHorse_o <- reservoir_vol_df[week_counter - 1, 6]
	}	
	return(HungryHorse_o)
}

##### InitHH

InitHH <- function() {
	HHHistStor <- HHStor[week_counter, 2] # check
	if (InitialConditionSwitch == 0) {
		InitHH_o <- InitHHLink
	} else if (InitialConditionSwitch == 1) {
		InitHH_o <- ResInitFractionFull * HHFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitHH_o <- HHHistStor
	} else {
		HHFullPoolVol
	}
	return(InitHH_o)
}

####### HHIn

HHIn <- function() {
	HHIn_o <- HungryHFlowData() - HHEvap()
	return(HHIn_o)
}

	
####### HungryHFlowData

HungryHFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		HungryHFlowData_o <- VICHH()
	} else {
		HungryHFlowData_o <- ModHungryHFlowData()
	}
	return(HungryHFlowData_o)
}

####### VICHH

VICHH <- function() {
	VICHH_o <- MidColMult() * PriVICHH
	return(VICHH_o)
}

####### ModHungryHFlowData

ModHungryHFlowData <- function() {
	print(paste("this has not been used yet"))
	return(0)
}

###### HHEvap

HHEvap <- function() {
	HHEvapData <- 0
	HHEvap_o <- HHSufaceArea() * HHEvapData * 0.5042 / 12
	return(HHEvap_o)
}

######## HHSufaceArea

HHSufaceArea <- function() {
	HHSufaceArea_o <- (-1.21281443E-13 * (HungryHorse() / 1000)^4 + 1.53692112E-09 * (HungryHorse() / 1000)^3 -
    6.75961255E-06 * (HungryHorse() / 1000)^2 + 1.87278268E-02 * (HungryHorse() / 1000) + 2.30403996) * 1000
	return(HHSufaceArea_o)
}

###### HHNetWith

HHNetWith <- function() {
	HHNetWith_o <- 0
	return(HHNetWith_o)
}

###### HHTopVol

HHTopVol <- function() {
	if (TopRuleSw() == 0) {
		HHTopVol_o <- HHFloodCurve()
	} else if (TopRuleSw() == 1) {
		HHTopVol_o <- HHFullPoolVol
	} else {
		HHTopVol_o <- HHFlood[week_counter_in_year(), 2]
	} # HHFlood1
	return(HHTopVol_o)
}

###### HHFloodCurve

HHFloodCurve <- function() {
	if (FC_Option == 1) {
		HHFloodCurve_o <- HHFullPoolVol - GlobalFloodEvacMult * (HHFullPoolVol - (HH_CurFC()))
	} else if (FC_Option == 4) {
		HHFloodCurve_o <- HHFullPoolVol - GlobalFloodEvacMult * (HHFullPoolVol - (HHCurFC_Cont()))
	} else if (FC_Option == 2) {
		HHFloodCurve_o <- GlobalFloodEvacMult * (HHFullPoolVol - HH_HecFC())
	} else {
		HHFloodCurve_o <- GlobalFloodEvacMult * (HHFullPoolVol - HH_ENSOFC())
	}
	return(HHFloodCurve_o)
}

######  HH_CurFC

HH_CurFC <- function() {
	if (HHSumMaySept < 1.0E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 2] # HHFlood1
	} else if (HHSumMaySept < 1.4E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 3] # HHFlood2
	} else if (HHSumMaySept < 1.6E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 4] # HHFlood3
	} else if (HHSumMaySept < 2.0E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 5] # HHFlood4
	} else if (HHSumMaySept < 2.2E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 6] # HHFlood5
	} else if (HHSumMaySept < 2.5E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 7] # HHFlood6
	} else if (HHSumMaySept < 2.8E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 8] # HHFlood7
	} else {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 9]
	} # HHFlood
	return(HH_CurFC_o)
}

######## HHCurFC_Cont

HHCurFC_Cont <- function() {
	JanToAprFlood_Range <- c(1000000, 1500000, 2000000, 2500000, 3000000, 3500000)
	if (JanToAprFlood_Range[length(JanToAprFlood_Range)] < HHSumMaySept) {
		row_no <- length(JanToAprFlood_Range)
	} else {
		row_no <- max(1.0, which(JanToAprFlood_Range > HHSumMaySept)[1] - 1)
	}
	if (HHSumMaySept <= 3.68E06) {
		if (week_counter_in_year() == 3 || week_counter_in_year() == 4) {
			HHCurFC_Cont_o <- 3547000
		} else if (week_counter_in_year() == 5) {
			HHCurFC_Cont_o <- 3397000
		} else if (week_counter_in_year() == 6) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 2] # JanToAprFlood[1,2] #JAN_HH
		} else if (week_counter_in_year() == 7) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 3] # JanToAprFlood[1,3] #FEB_HH
		} else if (week_counter_in_year() == 8) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 4] # JanToAprFlood[1,4] #MAR_HH
		} else if (week_counter_in_year() == 9) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 5] # JanToAprFlood[1,5] #APR_HH
		} else {
			HHCurFC_Cont_o <- 3647000
		}
	} else {
		HHCurFC_Cont_o <- HHFlood[week_counter_in_year(), 9]
	}
	return(HHCurFC_Cont_o)
}

############# HH_HecFC ##### called when FC_Option=2

HH_HecFC <- function() {
	print(paste("warning: this function is empty right now and it needs to be implemented from ColSim"))
	return(0)
}

############# HH_ENSOFC ###### called when FC_Option=3
HH_ENSOFC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

######### HHAvailAfter
# Volume cooresponding to the bottom of conservation pool elevation of 3336 ft.  Units acre-ft.

HHAvailAfter <- function() {
	HHAvailAfter_o <- max(0, HungryHorse() + HHIn() - HHNetWith() - HHBotVol)
	return(HHAvailAfter_o)
}

###### HHMinReq
# Minimum release requirement converted to cfsd.

HHMinReq <- function() {
	if (RefillMinSw() == 1) {
    HHMinReq_o <- HHRefillMin
	} else {
		HHMinReq_o <- max(HHMin() * cfsTOafw, HHRelForColFalls())
	}
	return(HHMinReq_o)
}

####### HHMin
# Minimum release from Hungry Horse for Columbia Falls.  Units cfs.
# Jan to Aug 400 to 900 kcfs
# variable based upon predicted inflows between april and august

HHMin <- function() {
	HHMin_o <- 400 # prescribed to be 400 cfs in the ColSim
	return(HHMin_o)
}

###### HHRelForColFalls # this code is a part of "Col Falls Target" calculations
# Minimum Flows at the Columbia Falls are required to be at least 3,500 cfs, with maximum values  of 4500 cfs Oct-Dec.
# Hungry Horse must make sufficient average releases to support these flows on a monthly time frame.
# The model assumes that Hungry Horse supplies all supplements to natural inflow required to meet the Columbia Falls Target.

HHRelForColFalls <- function() {
	HHRelForColFalls_o <- max(0, (ColFallsTarget() * cfsTOafw) - (ColumbiaFallsFlowData() - HungryHFlowData()))
	return(HHRelForColFalls_o)
}

######### HHorse_April_Evac_Target

HHorse_April_Evac_Target <- function() {
	if (HHSumMaySept < 1.0E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 2] # HHFlood1_2
	} else if (HHSumMaySept < 1.4E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 3] # HHFlood2_2
	} else if (HHSumMaySept < 1.6E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 4] # HHFlood3_2
	} else if (HHSumMaySept < 2.0E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 5] # HHFlood4_2
	} else if (HHSumMaySept < 2.2E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 6] # HHFlood5_2
	} else if (HHSumMaySept < 2.5E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 7] # HHFlood6_2
	} else if (HHSumMaySept < 2.8E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 8] # HHFlood7_2
	} else {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 9] # HHFlood8_2
	} 
	if (FC_Option == 4) {
		HHorse_April_Evac_Target_o <- (GlobalFloodEvacMult * (HHFullPoolVol - HHAprEvaq_Cont()))
	} else {
		HHorse_April_Evac_Target_o <- GlobalFloodEvacMult * (HHFullPoolVol - HHF_o)
	}
	return(HHorse_April_Evac_Target_o)
}

###### HHAprEvaq_Cont

HHAprEvaq_Cont <- function() {
	if (HHSumMaySept <= 3.68E06) {
		HHAprEvaq_Cont_o <- APR_HH()
	} else {
		HHAprEvaq_Cont_o <- 665000
	}
	return(HHAprEvaq_Cont_o)
}

########## APR_HH

APR_HH <- function() {
	if (HHSumMaySept < APR_HH_input[1, 1]) {
		APR_HH_o <- APR_HH_input[1, 2]
	} else if (HHSumMaySept < APR_HH_input[1, 1]) {
		APR_HH_o <- APR_HH_input[1, 2]
	} else if (HHSumMaySept < APR_HH_input[2, 1]) {
		APR_HH_o <- APR_HH_input[2, 2]
	} else if (HHSumMaySept < APR_HH_input[3, 1]) {
		APR_HH_o <- APR_HH_input[3, 2]
	} else if (HHSumMaySept < APR_HH_input[4, 1]) {
		APR_HH_o <- APR_HH_input[4, 2]
	} else if (HHSumMaySept < APR_HH_input[5, 1]) {
		APR_HH_o <- APR_HH_input[5, 2]
	} else {
		APR_HH_o <- APR_HH_input[6, 2]
	}
	return(APR_HH_o)
}

###### HHMcNarySharedWater
# Options:
# 0--Use current draft limits for McNary and Lower Granite
# 1-Use all major system storage for McNary and Lower Granite

HHMcNarySharedWater <- function() {
	#   if(Observe_Draft_Limits==1) {HHMcNarySharedWater_o=0
	#   }else if(Use_Storage_for_Targets==1) {HHMcNarySharedWater_o=1
	#   }else{print(paste("Not sure what (stoptime) means, check out original ColSim"))}
	HHMcNarySharedWater_o <- max(0, HungryHorse() + HHIn() - HHNetWith() - HHPrelim() - HHMcNaryDraftLimit())
	return(HHMcNarySharedWater_o)
}

###### HHMcNaryDraftLimit

HHMcNaryDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		HHMcNaryDraftLimit_o <- HHBotVol
	} else if (Fish_Pool_Alternative == 1) {
		HHMcNaryDraftLimit_o <- 3.166e6
	} else if (Fish_Pool_Alternative == 2) {
		HHMcNaryDraftLimit_o <- HHMaxPoolVol - 0.827E6
	} else if (Fish_Pool_Alternative == 3) {
		HHMcNaryDraftLimit_o <- HHMaxPoolVol - 1.051E6
	} else if (Fish_Pool_Alternative == 4) {
		HHMcNaryDraftLimit_o <- HHMaxPoolVol - 1.419E6
	} else if (Fish_Pool_Alternative == 5) {
		HHMcNaryDraftLimit_o <- HHMaxPoolVol - 1.932E6
	} else {
		HHMcNaryDraftLimit_o <- 3.166e6
	}
	return(HHMcNaryDraftLimit_o)
}

######### HHEnergyContent

HHEnergyContent <- function() {
	if (month_in_year >= 3 && month_in_year <= 5) {
		HHEnergyContent_o <- 0
	} else {
		HHEnergyContent_o <- HHSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	}
	return(HHEnergyContent_o)
}

#### HHSharedWater

HHSharedWater <- function() {
	HHSharedWater_o <- max(0, HungryHorse() + HHIn() - HHNetWith() - HHPrelim() - HHMcNarySup() - HHBotVol)
	return(HHSharedWater_o)
}

######### HHMcNarySup

HHMcNarySup <- function() {
	# print(paste("HHMcNarySup"))
	if (TotalMcNarySharedWater_c == 0) {
		HHMcNarySup_o <- 0
	} else {
		HHMcNarySup_o <- min(HHMcNarySharedWater(), McNaryFlowDeficit() * HHMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(HHMcNarySup_o)
}

####### HHNetHead

HHNetHead <- function() {
	HHTailElev <- 3082.6
	HHLoss <- 0
	HHNetHead_o <- HHElev_ft() - HHTailElev - HHLoss
	return(HHNetHead_o)
}

###### HHElev_ft

HHElev_ft <- function() {
	HHElev_ft_o <- 4.84974275E-18 * HungryHorse()^3 - 4.48065506E-11 * HungryHorse()^2 + 1.81102983E-04 * HungryHorse() + 3.26045812E+03
	return(HHElev_ft_o)
}

######### HHDownStreamHead

HHDownStreamHead <- function() {
	HHDownStreamHead_o <- AFNetHead() + BCNetHead() + BDNetHead() + CBNetHead() + KENetHead() + NoxNetHead() + TotalGCHead()
	return(HHDownStreamHead_o)
}

###### HHECCEnergyContent

HHECCEnergyContent <- function() {
	if (month_in_year >= 3 && month_in_year <= 5) {
		HHECCEnergyContent_o <- 0
	} else {
		HHECCEnergyContent_o <- (HHECCSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt)
	}
	return(HHECCEnergyContent_o)
}

######## HHECCSharedWater

HHECCSharedWater <- function() {
	HHECCSharedWater_o <- max(0, HungryHorse() + HHIn() - HHNetWith() - HHPrelim() - HHMcNarySup() - HHECC())
	return(HHECCSharedWater_o)
}

####### HHECC

HHECC <- function() {
  # During the fixed period from August-December before the forecast, the ECC is the greater of the critical curve or the refill curve based on 1931 inflows.
  # In the variable period from January-July the VEEC can be lower than the ECC due to revised refill curve (based on forecast).
  # The VEEC curve, however, cannot be higher than the original ECC curve.
  # The value in this icon is compared to the flood storage curve and the minimum value is selected.
  # The model will always release water to evacuate flood storage, and will release to the ECC and VECC if the user selects maximum allowed non-firm releases.
	if (UseUpdatedOpSystem == 0) {
		if (week_counter_in_year() <= 5 || week_counter_in_year() == 12) {
			out1 <- max(HHCriticalCurve(), max(HHRefillCurve(), HHBiOpDraftLimit()))
		} else {
			out1 <- min(max(HHRefillCurve(), max(HHCriticalCurve(), HHBiOpDraftLimit())), max(HHCriticalCurve(), max(HHAssuredRefill(), HHBiOpDraftLimit())))
		}
		HHECC_o <- min(HHFloodCurve(), out1)
	} else if (UseUpdatedOpSystem == 1 && UseForecastRLFCurve == 1) { # some of the components is not included
		if (week_counter_in_year() <= 5 || week_counter_in_year() == 12) {
			out2 <- max(HHForecastRLFCurve, HHBiOpDraftLimit())
		} else {
			out2 <- max(HHActualRFCurve, HHBiOpDraftLimit())
		}
		HHECC_o <- min(HHFloodCurve(), out2)
	} else { # some these is not included
		if (week_counter_in_year() <= 5 || week_counter_in_year() == 12) {
			out3 <- max(HH1931RFCurve, HHBiOpDraftLimit())
		} else {
			out3 <- max(HHActualRFCurve, HHBiOpDraftLimit())
		}	
		HHECC_o <- min(HHFloodCurve(), out3)
	}	
	return(HHECC_o)
}

######## HHCriticalCurve

HHCriticalCurve <- function() {
	HHCriticalCurve_o <- HHCriticalCurve_input[week_counter_in_year(), 2]
	return(HHCriticalCurve_o)
}

###### HHRefillCurve
# Refill rule curve based on 1931 inflows and forecasts.  If the refill switch is set to the status quo rules,
# the model selects the 1931 based refill curve August-December and then selects the forecast refill curve for the remainder of the year.
# If the refill switch is set to alternate rules, the forecast refill curve is used for the entire year.
# The second option may allow more non-firm energy releases Aug-December than the status quo rules. Units af.

HHRefillCurve <- function() {
	if (RefillSwitch() == 1) {
		HHRefillCurve_o <- HHRefillVol1
	} else if (RefillSwitch() == 2) {
		HHRefillCurve_o <- HHRefillVol
	} else {
		HHRefillCurve_o <- LBRefillVol_6
	}
	return(HHRefillCurve_o)
}

####### LBRefillVol_6

LBRefillVol_6 <- function() {
	LBRefillVol_6_o <- 0
	return(LBRefillVol_6_o)
}

########## HHBiOpDraftLimit

HHBiOpDraftLimit <- function() {
	if (month_in_year >= 6 && month_in_year <= 8) {
		HHBiOpDraftLimit_o <- HHRefillCurve()
	} else {
		HHBiOpDraftLimit_o <- HHBaseDraftLimit()
	}
	return(HHBiOpDraftLimit_o)
}

######## HHBaseDraftLimit

HHBaseDraftLimit <- function() {
	HHBaseDraftLimit_o <- HHBaseDraftLimit_input[week_counter_in_year(), 2]
	return(HHBaseDraftLimit_o)
}

######## HHAssuredRefill

HHAssuredRefill <- function() {
	HHAssuredRefill_o <- HHAssuredRefill_input[month_in_year, 2]
	return(HHAssuredRefill_o)
}

#########################
########################################
######################################################

#### ColFallsTarget # check
# Columbia falls obligation to Columbia Falls Flow Target is based upon forecasted inflows from April through August.  The target is as follows (units cfs)
# Forecast - target
# Above 1790 taf - 3500 cfs
# Below 1190 taf - 3200 cfs
# between two values is a linear interpolation.

ColFallsTarget <- function() {
	# not sure how these values are calculated and what the actual time step is
	if (ColFall_Target[length(ColFall_Target[, 3]), 2] < HHInQ_AprAug / 1000) {
		row_no <- length(ColFall_Target[, 3])
	} else {
		row_no <- which(ColFall_Target[, 2] > HHInQ_AprAug / 1000)[1]
	}
	ColFallsTarget_o <- ColFall_Target[row_no, 3]
	return(ColFallsTarget_o)
}

#### ColumbiaFallsFlowData

ColumbiaFallsFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		ColumbiaFallsFlowData_o <- VICCOL()
	} else {
		ColumbiaFallsFlowData_o <- ModColumbiaFallsFlowData()
	}	
	return(ColumbiaFallsFlowData_o)
}

COLDem <- function() {
	COLDem_o <- DemVICCOL
	return(COLDem_o)
}

################ VICCOL

VICCOL <- function() {
	VICCOL_o <- MidColMult() * PriVICCOL
	return(VICCOL_o)
}

########### ModColumbiaFallsFlowData

ModColumbiaFallsFlowData <- function() {
	print(paste("Columbia Falls has not been defined in the RColSim yet"))
	ModColumbiaFallsFlowData_o <- 0
	retuen(ModColumbiaFallsFlowData_o)
}

######################
#####################################
########################################################

##### Dworshak Dam

InitDWLink <- 2776338.073
DWFullPoolVol <- 3.4679500E+06
DWBotVol <- 1.4522E+06 # Volume cooresponding to the bottom of conservation pool elevation of 1445 ft.  Units acre-ft.

##### Dworshak

Dworshak <- function() {
	if (week_counter == 1) {
		Dworshak_o <- InitDW()
		# reservoir_vol_df[week_counter,8]=Dworshak_o
	} else {
		Dworshak_o <- reservoir_vol_df[week_counter - 1, 8]
	}
	return(Dworshak_o)
}

######## InitDW

InitDW <- function() {
	if (InitialConditionSwitch == 0) {
		InitDW_o <- InitDWLink
	} else if (InitialConditionSwitch == 1) {
		InitDW_o <- ResInitFractionFull * DWFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitDW_o <- DWHistStor()
	} else {
		InitDW_o <- DWFullPoolVol
	}
	return(InitDW_o)
}

###### DWHistStor

DWHistStor <- function() {
	DWHistStor_o <- DWHistStor_input[week_counter_in_year(), 2]
	return(DWHistStor_o)
}

##### DWIn

DWIn <- function() {
	DWIn_o <- DworshakFlowData() - DWEvap() - DWDem()
	return(DWIn_o)
}

####### DworshakFlowData

DworshakFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		DworshakFlowData_o <- VICDW()
	} else {
		DworshakFlowData_o <- ModDworshakFlowData()
	}
	return(DworshakFlowData_o)
}

DWDem <- function() {
	DWDem_o <- DemVICDW - DWCurtail()
	return(DWDem_o)
}

DWCurtail <- function() {
	DWCurtail_o <- max(0, DemVICDW - DworshakFlowData())
	return(DWCurtail_o)
}

###### VICDW

VICDW <- function() {
	VICDW_o <- PriVICDW * SnakeMult()
	return(VICDW_o)
}

#### ModDworshakFlowData

ModDworshakFlowData <- function() {
	ModDworshakFlowData_o <- ModDworshakFlowData_input[week_counter_in_year(), 2] # check # what is the actual time-step weekly or daily?
	return(ModDworshakFlowData_o)
}

#### DWEvap

DWEvap <- function() {
	DWEvapData <- 0
	DWEvap_o <- DWSufaceArea() * DWEvapData * 0.5042 / 12
	return(DWEvap_o)
}

##### DWSufaceArea

DWSufaceArea <- function() {
	DWSufaceArea_o <- (-1.21281443E-13 * (Dworshak() / 1000)^4 + 1.53692112E-09 * (Dworshak() / 1000)^3 -
    6.75961255E-06 * (Dworshak() / 1000)^2 + 1.87278268E-02 * (Dworshak() / 1000) + 2.30403996) * 1000
	return(DWSufaceArea_o)
}

####### DWDraftLimit

DWDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		DWDraftLimit_o <- DWBotVol
	} else {
		DWDraftLimit_o <- 2.238e6
	}
	return(DWDraftLimit_o)
}

###### DWPrelim

DWPrelim <- function() {
	if (OptimizedRelSw == 1) {
		DWPrelim_o <- max(DWOpRel(), DWRuleReq())
	} else {
		DWPrelim_o <- min(DWAvailAfter(), max(DWRuleReq(), DWMinReq()))
	}
	return(DWPrelim_o)
}

####### DWOpRel

DWOpRel <- function() {
	DWOpRel_o <- 0
	return(DWOpRel_o)
}

###### DWRuleReq

DWRuleReq <- function() {
	DWRuleReq_o <- max(Dworshak() + DWIn() - DWNetWith() - DWTopVol(), 0)
	return(DWRuleReq_o)
}

##### DWNetWith

DWNetWith <- function() {
	DWNetWith_o <- 0
	return(DWNetWith_o)
}

##### DWTopVol

DWTopVol <- function() {
	if (TopRuleSw() == 0) {
		DWTopVol_o <- DWFloodCurve()
	} else if (TopRuleSw() == 1) {
		DWTopVol_o <- DWFullPoolVol
	} else {
		DWTopVol_o <- DWFlood_input[week_counter_in_year(), 2]
	}
	return(DWTopVol_o)
}

##### DWFloodCurve

DWFloodCurve <- function() {
	if (FC_Option == 1) {
		DWFloodCurve_o <- DWFullPoolVol - DWFloodEvacMult() * GlobalFloodEvacMult * (DWFullPoolVol - DW_CurFC())
	} else if (FC_Option == 4) {
		DWFloodCurve_o <- DWFullPoolVol - DWFloodEvacMult() * GlobalFloodEvacMult * (DWFullPoolVol - DWCurFC_Cont())
	} else if (FC_Option == 2) {
		DWFloodCurve_o <- GlobalFloodEvacMult * (DWFullPoolVol - DW_HecFC())
	} else {
		DWFloodCurve_o <- GlobalFloodEvacMult * (DWFullPoolVol - DW_ENSOFC())
	}			
	return(DWFloodCurve_o)
}

###### DWFloodEvacMult

DWFloodEvacMult <- function() {
	DWFloodEvacMult_o <- 1
	return(DWFloodEvacMult_o)
}

###### DW_CurFC

DW_CurFC <- function() {
	if (DWRunoffAprJuly < 1.2E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 2] # DWFlood1
	} else if (DWRunoffAprJuly < 1.4E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 3] # DWFlood2
	} else if (DWRunoffAprJuly < 1.8E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 4] # DWFlood3
	} else if (DWRunoffAprJuly < 2.2E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 5] # DWFlood4
	} else if (DWRunoffAprJuly < 2.6E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 6] # DWFlood5
	} else if (DWRunoffAprJuly < 3.0E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 7] # DWFlood6
	} else if (DWRunoffAprJuly < 3.2E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 8] # DWFlood7
	} else if (DWRunoffAprJuly < 3.4E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 9] # DWFlood8
	} else if (DWRunoffAprJuly < 3.6E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 10] # DWFlood9
	} else {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 11] # DWFlood10
	} 
	return(DW_CurFC_o)
}

##########  DWCurFC_Cont

DWCurFC_Cont <- function() {
	DW_FloodM_Range <- c(1200000, 1400000, 1600000, 1800000, 2000000, 2200000, 2400000, 2600000, 2800000, 3000000, 3200000, 3400000, 3600000, 3800000)
	if (DW_FloodM_Range[length(DW_FloodM_Range)] < DWRunoffAprJuly) {
		row_no <- length(DW_FloodM_Range)
	} else {
		row_no <- max(1.0, which(DW_FloodM_Range > DWRunoffAprJuly)[1] - 1)
	}
	if (DWRunoffAprJuly <= 3.8E06) {
		if (month_in_year == 3 || month_in_year == 4 || month_in_year == 5) {
			DWCurFC_Cont_o <- 2767950
		} else if (month_in_year == 6) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 2] # JAN_DW
		} else if (month_in_year == 7) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 3] # FEB_DW
		} else if (month_in_year == 8) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 4] # MAR_DW
		} else if (month_in_year == 9) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 5] # APR_DW
		} else {
			DWCurFC_Cont_o <- 3467950
		}
	} else {
		DWCurFC_Cont_o <- DWFlood_input[week_counter_in_year(), 10] # DWFlood10
	} 
	return(DWCurFC_Cont_o)
}

############# DW_HecFC ##### called when FC_Option=2

DW_HecFC <- function() {
	print(paste("warning: this function is empty right now and it needs to be implemented from ColSim"))
	return(0)
}

############# DW_ENSOFC ###### called when FC_Option=3
DW_ENSOFC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

##### DWAvailAfter

DWAvailAfter <- function() {
	DWAvailAfter_o <- max(0, Dworshak() + DWIn() - DWNetWith() - DWBotVol)
	return(DWAvailAfter_o)
}

####### DWMinReq
# Minimum release requirement converted to cfsd.

DWMinReq <- function() {
	if (RefillMinSw() == 1) {
		DWMinReq_o <- DWRefillMin
	} else {
		DWMinReq_o <- max(DWAvgMin() * cfsTOafw, min(DWLGAvailWater(), DWRelForLG()))
	}
	return(DWMinReq_o)
}

####### DWRefillMin

DWRefillMin <- function() {
	DWRefillMin_o <- DWRefillMin_input[week_counter_in_year(), 2]
	return(DWRefillMin_o)
}

####### DWAvgMin

DWAvgMin <- function() {
	DWAvgMin_o <- DWAvgMin_input[week_counter_in_year(), 2]
	return(DWAvgMin_o)
}

###### DWLGAvailWater

DWLGAvailWater <- function() {
	DWLGAvailWater_o <- max(0, Dworshak() + DWIn() - DWNetWith() - DWDraftLimit())
	return(DWLGAvailWater_o)
}

####### DWRelForLG

DWRelForLG <- function() {
    if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	if ((StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()) == 0) {
		DWRelForLG_o <- 0
	} else {
		DWRelForLG_o <- TotalRelForLowerGranite() *
		((StorFrac * Dworshak() + InflowFrac * DWIn() - StorFrac * DWDraftLimit()) /
        (StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()))
	}
	return(DWRelForLG_o)
}

###############################################################################################################
# ENERGY
####### DWEnergyContent

DWEnergyContent <- function() {
	DWEnergyContent_o <- DWSharedWater() * (DWNetHead() + DWDownstreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DWEnergyContent_o)
}

####### DWSharedWater

DWSharedWater <- function() {
	# Generic shared water calculation.  Connect to supplemental release calculations.  Units cfs-days.
	DWSharedWater_o <- max(0, Dworshak() + DWIn() - DWNetWith() - DWPrelim() - DWBotVol)
	return(DWSharedWater_o)
}

###### DWNetHead

DWNetHead <- function() {
	DWTailElev <- 980
	DWLoss <- 0
	DWNetHead_o <- DWElev_ft() - DWTailElev - DWLoss
	return(DWNetHead_o)
}

###### DWElev_ft

DWElev_ft <- function() {
	DWElev_ft_o <- -1.47637889E-11 * Dworshak()^2 + 1.49196994E-04 * Dworshak() + 1.25967539E+03
	return(DWElev_ft_o)
}

######## DWDownstreamHead

DWDownstreamHead <- function() {
	DWDownstreamHead_o <- BONNetHead() + DANetHead() + IHNetHead() + JDNetHead() + LGNetHead() + LiGNetHead() + LMNetHead() + MCNetHead()
	return(DWDownstreamHead_o)
}

######## DWECCEnergyContent

DWECCEnergyContent <- function() {
	DWECCEnergyContent_o <- DWECCSharedWater() * (DWNetHead() + DWDownstreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DWECCEnergyContent_o)
}

##### DWECCSharedWater

DWECCSharedWater <- function() {
	DWECCSharedWater_o <- max(0, Dworshak() + DWIn() - DWNetWith() - DWPrelim() - DWECC())
	return(DWECCSharedWater_o)
}

###### DWECC

DWECC <- function() {
	if (UseUpdatedOpSystem == 0) {
		if (week_counter_in_year() <= 5 || week_counter_in_year() == 12) {
			out1 <- max(DWCriticalCurve(), DWRefillCurve())
		} else {
			out1 <- min(max(DWRefillCurve(), DWCriticalCurve()), max(DWCriticalCurve(), DW1931Refill()))
		}
		DWECC_o <- min(DWFloodCurve(), out1)
	} else if (UseUpdatedOpSystem == 1 && UseForecastRLFCurve == 1) { # not included
		if (week_counter_in_year() <= 5 || week_counter_in_year() == 12) {
			out2 <- DWForecastRLFCurve
		} else {
			out2 <- DWActualRFCurve
		}
		DWECC_o <- min(DWFloodCurve(), out2)
	} else { # not included
		if (week_counter_in_year() <= 5 || week_counter_in_year() == 12) {
			out3 <- DW1931RFCurve
		} else {
			out3 <- DWActualRFCurve
		}
		DWECC_o <- min(DWFloodCurve(), out3)
	}
	return(DWECC_o)
}

###### DWCriticalCurve

DWCriticalCurve <- function() {
	DWCriticalCurve_o <- DWCriticalCurve_input[week_counter_in_year(), 2]
	return(DWCriticalCurve_o)
}

####### DWRefillCurve

DWRefillCurve <- function() {
	DWRefillVol3 <- 0
	if (RefillSwitch() == 1) {
		DWRefillCurve_o <- DWRefillVol1
	} else if (RefillSwitch() == 2) {
		DWRefillCurve_o <- DWRefillVol2
	} else {
		DWRefillCurve_o <- DWRefillVol3
	}
	return(DWRefillCurve_o)
}

##### DW1931Refill

DW1931Refill <- function() {
	DW1931Refill_o <- DW1931Refill_input[week_counter_in_year(), 2]
	return(DW1931Refill_o)
}

###### DWRelLimit

DWRelLimit <- function() {
	DWRelLimit_o <- max(Dworshak() + DWInflow() - DWNetWith() - DWBotVol, 0)
	return(DWRelLimit_o)
}

###### DWCombSup

DWCombSup <- function() {
	DWCombSup_o <- DWEnergySup()
	return(DWCombSup_o)
}

##### DWEnergySup

DWEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DWSharedWater()), min(DWNonFirmEngSupReq(), DWECCSharedWater())))
	} else {
		DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DWECCSharedWater()), min(DWNonFirmEngSupReq(), DWECCSharedWater())))
	}
	return(DWEnergySup_o)
}

###### DWFirmEngSupReq

DWFirmEngSupReq <- function() {
	DWFirmEngSupReq_o <- min(DWPenLimit(), 4.260306e7 * (DWFirmEngSup()) / (43560 * (DWNetHead() + DWDownstreamHead()) * DWCombEfficiency))
	return(DWFirmEngSupReq_o)
}

###### DWNonFirmEngSupReq

DWNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		DWNonFirmEngSupReq_o <- min(DWPenLimit(), 4.260306e7 * (DWFirmEngSup() + DWNonFirmEngSup()) / (43560 * (DWNetHead() + DWDownstreamHead()) * DWCombEfficiency))
	} else {
		DWNonFirmEngSupReq_o <- 0
	}
	return(DWNonFirmEngSupReq_o)
}

###### DWNonFirmEngSup

DWNonFirmEngSup <- function() {
	# print(paste("energy, here 5"))
	if (TotalNFEnergyContent_c == 0) {
		DWNonFirmEngSup_o <- 0
	} else {
		DWNonFirmEngSup_o <- DWNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}	
	return(DWNonFirmEngSup_o)
}
##########################
#################################################
##########################################################################
###### Lower Granite dam

StorFrac <- 0.2
InflowFrac <- 1

##### BRRelForLG

BRRelForLG <- function() {
	if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	if ((StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()) == 0) {
		BRRelForLG_o <- 0
	} else {
		BRRelForLG_o <- TotalRelForLowerGranite() * ((StorFrac * Brownlee() + InflowFrac * BRIn_c - StorFrac * BRDraftLimit())
		/ (StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()))
	}
	return(BRRelForLG_o)
}

######## TotalRelForLowerGranite

TotalRelForLowerGranite <- function() {
	TotalRelForLowerGranite_o <- max(0, (LowerGraniteTarget() * cfsTOafw) - (LowerGraniteFlowData() - DworshakFlowData() - BrownleeFlowData()))
	return(TotalRelForLowerGranite_o)
}

#### LowerGraniteTarget

LowerGraniteTarget <- function() {
	LowerGraniteTarget_o <- LowerGraniteTarget_input[month_in_year, 2]
	return(LowerGraniteTarget_o)
}

##### LowerGraniteFlowData

LowerGraniteFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		LowerGraniteFlowData_o <- VICLG()
	} else {
		LowerGraniteFlowData_o <- ModLowerGraniteFlowData()
	}
	return(LowerGraniteFlowData_o)
}

###### VICLG

VICLG <- function() {
	VICLG_o <- PriVICLG * SnakeMult()
	return(VICLG_o)
}

##### Demand between Lower Granite and Hells Canyon

LGDem <- function() {
	LGDem_o <- DemVICLG
	return(LGDem_o)
}

##### IHToLG

IHToLG <- function() {
	IHToLG_o <- .992
	return(IHToLG_o)
}

####### ModLowerGraniteFlowData

ModLowerGraniteFlowData <- function() {
	ModLowerGraniteFlowData_o <- ModLowerGraniteFlowData_input[week_counter_in_year(), 2] # check # what is the actual time step?
	return(ModLowerGraniteFlowData_o)
}

################# LGInc

LGInc <- function() {
	LGInc_o <- LowerGraniteFlowData() - HellsCanyonFlowData() - DworshakFlowData()
	return(LGInc_o)
}

####### LiGInc

LiGInc <- function() {
	LiGInc_o <- LittleGooseFlowData() - LowerGraniteFlowData()
	return(LiGInc_o)
}

######## LMInc

LMInc <- function() {
	LMInc_o <- LowerMonuFlowData() - LittleGooseFlowData()
	return(LMInc_o)
}

##### IHInc

IHInc <- function() {
	IHInc_o <- IceHarborFlowData() - LowerMonuFlowData()
	return(IHInc_o)
}

#################################
########################################################
############################################################################
##### Corra Linn

CLBotVol <- 1.44e5
CLFullPool <- 816730
CL_April_Target <- 245000

##### CLPrelim

CLPrelim <- function() {
	CLPrelim_o <- 
	if (OptimizedRelSw == 1) {
		max(CLOpRel(), CLRuleReq())
	} else {
		min(CLAvailAfter(), max(CLRuleReq(), CLMinReq()))
	}
	return(CLPrelim_o)
}

####### CLOpRel

CLOpRel <- function() {
	CLOpRel_o <- 0	
	return(CLOpRel_o)
}

###### CLRuleReq

CLRuleReq <- function() {
	CLRuleReq_o <- max(CorraLinnReservoir() + CLIn() - CLNetWith() - CLRuleVol(), 0)
	return(CLRuleReq_o)
}

######### CorraLinnReservoir

CorraLinnReservoir <- function() {
	if (week_counter == 1) {
		CorraLinnReservoir_o <- InitCL()
		# reservoir_vol_df[week_counter,4]=CorraLinnReservoir_o
	} else {
		CorraLinnReservoir_o <- reservoir_vol_df[week_counter - 1, 4]
	}
	return(CorraLinnReservoir_o)
}

###### InitCL

InitCL <- function() {
	if (InitialConditionSwitch == 0) {
		InitCL_o <- InitCLLink()
	} else if (InitialConditionSwitch == 1) {
		InitCL_o <- ResInitFractionFull * CLRuleVol()
	} else if (InitialConditionSwitch == 2) {
		InitCL_o <- CLHistStor()
	} else {
		InitCL_o <- CLRuleVol()
	}
	return(InitCL_o)
}

###### InitCLLink

InitCLLink <- function() {
	InitCLLink_o <- 310978.75
	return(InitCLLink_o)
}

#### CLRuleVol
# Corra Linn operates to Internation Joint Commission guidelines.  This agreement specifies reservoir elevations at five dates throughout the year.
# To include this curve in the model,  intermediate data points have been added for other months based on a linear interpolation between points.
# The January 7 storage value was assumed to occur on Dec 31 for simplicity.  Aug 31 corresponds to month 1.

CLRuleVol <- function() {
	CLRuleVol_o <- CLIJCRuleCurve()
	return(CLRuleVol_o)
}

####### CLIJCRuleCurve

CLIJCRuleCurve <- function() {
	CLIJCRuleCurve_o <- CLIJCRuleCurve_input[week_counter_in_year(), 2]
	return(CLIJCRuleCurve_o)
}

####### CLHistStor   # check #week_counter or week_counter_in_year()?

CLHistStor <- function() {
	CLHistStor_o <- CLHistStor_input[week_counter, 2]
	return(CLHistStor_o)
}

#### CLIn()

CLIn <- function() {
	if (week_counter == 1) {
		CLIn_o <- DUPrelim() + LBPrelim() + (CorraLinnFlowData() - DuncanFlowData() - LibbyFlowData()) - DuncanToCLNetWith() - CLEvap() - CLDem()
	} else {
	CLIn_o <- DUPrelim() + BonnersFerry() + (CorraLinnFlowData() - DuncanFlowData() - BonnersFerryFlowData()) - DuncanToCLNetWith() - CLEvap() - CLDem()
	}
	return(CLIn_o)
}

######### CorraLinnFlowData

CorraLinnFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		CorraLinnFlowData_o <- VICCL()
	} else {
		CorraLinnFlowData_o <- ModCorraLinnFlowData()
	}
	return(CorraLinnFlowData_o)
}

####### ModCorraLinnFlowData

ModCorraLinnFlowData <- function() {
	print(paste("this has not been used yet"))
	return(0)
}

CLDem <- function() {
	CLDem_o <- DemVICCL
	return(CLDem_o)
}

######## VICCL

VICCL <- function() {
	VICCL_o <- (PriVICCL - CLEstEvap() - CLWith()) * UpperColMult()
	return(VICCL_o)
}

###### CLEstEvap

CLEstEvap <- function() {
	CLEstEvap_o <- 0 # there was time-series of zeros in the ColSim
	return(CLEstEvap_o)
}

###### CLWith

CLWith <- function() {
	CLEstWith <- 0
	CLAgWith <- CLAgWith_input[week_counter_in_year(), 2]
	CLAgRech <- CLAgWith_input[week_counter_in_year(), 3]
	if (MarketHydrgy == 0) {
		CLWith_o <- CLEstWith
	} else {
		CLWith_o <- CLAgWith + CLAgRech
	}
	return(CLWith_o)
}

####### DuncanToCLNetWith

DuncanToCLNetWith <- function() {
	DuncanToCLNetWith_o <- 0
	return(DuncanToCLNetWith_o)
}

####### CLEvap

CLEvap <- function() {
	CLEvap_o <- (CLSufaceArea() * CLEvapData()) * 0.5042 / 12
	return(CLEvap_o)
}

##### CLSufaceArea

CLSufaceArea <- function() {
	CLSufaceArea_o <- (-1.21281443E-13 * (CorraLinnReservoir() / 1000)^4 + 1.53692112E-09 * (CorraLinnReservoir() / 1000)^3 -
    6.75961255E-06 * (CorraLinnReservoir() / 1000)^2 + 1.87278268E-02 * (CorraLinnReservoir() / 1000) + 2.30403996) * 1000
	return(CLSufaceArea_o)
}

####### CLEvapData

CLEvapData <- function() {
	CLEvapData_o <- 0
	return(CLEvapData_o)
}

###### CLNetWith

CLNetWith <- function() {
	CLNetWith_o <- 0
	return(CLNetWith_o)
}

######### CLAvailAfter

CLAvailAfter <- function() {
	CLAvailAfter_o <- max(0, CorraLinnReservoir() + CLIn() - CLNetWith() - CLBotVol)
	return(CLAvailAfter_o)
}

########## CLMinReq

CLMinReq <- function() {
	CLMinReq_o <- max(min(CLAvailAfter(), CLCombEngRelReq()), CLMin() * cfsTOafw)
	return(CLMinReq_o)
}

######## CLCombEngRelReq

CLCombEngRelReq <- function() {
	CLCombEngRelReq_o <- min(CLPenLimit(), 4.2603e7 * (CLFirmEngTarget() + CLNonFirmTarget()) / (43560 * CLNetHead() * CLCombEfficiency()))
	return(CLCombEngRelReq_o)
}

####### CLPenLimit

CLPenLimit <- function() {
	CLPenCap <- 12600
	CLPenLimit_o <- CLPenCap * cfsTOafw
	return(CLPenLimit_o)
}

######## CLFirmEngTarget

CLFirmEngTarget <- function() {
	CLFirmEngTarget_o <- 0 # the actual data is a time-series of zeros for each week
	return(CLFirmEngTarget_o)
}

###### CLNonFirmTarget

CLNonFirmTarget <- function() {
	CLNonFirmTarget_o <- 0
	return(CLNonFirmTarget_o)
}

##### CLNetHead

CLNetHead <- function() {
	CLTailElev <- 1686
	CLLoss <- 0
	CLNetHead_o <- CLElev_ft() - CLTailElev - CLLoss
	return(CLNetHead_o)
}

#### CLElev_ft

CLElev_ft <- function() {
	CLElev_ft_o <- -4.15160146E-13 * CorraLinnReservoir()^2 + 9.39564626E-06 * CorraLinnReservoir() + 1.73792103E+03
	return(CLElev_ft_o)
}

######### CLCombEfficiency

CLCombEfficiency <- function() {
	CLCombEfficiency_o <- 0.8
	return(CLCombEfficiency_o)
}

###### CLMin

CLMin <- function() {
	CLMin_o <- 5000 # the actual is a time series that includes 52, 5000 one for each week
	return(CLMin_o)
}

####### CL_April_Target

# CL_April_Target=function(){
#
#   CL_April_Target_o=
#
#   return(CL_April_Target_o)
# }

####### CLInEvap

CLInEvap <- function() {
	CLInEvap_o <- CorraLinnFlowData() - (DuncanFlowData() + BonnersFerryFlowData()) - CLEvap() - CLDem()
	return(CLInEvap_o)
}

####### CLRelLimit

CLRelLimit <- function() {
	CLRelLimit_o <- max(CorraLinnReservoir() + CLInflow() - CLNetWith() - CLBotVol, 0)
	return(CLRelLimit_o)
}

#############################
################################################
################################################################
###### Libby

LBFullPoolVol <- 5855100
InitLBLink <- 5654926.179
CORToLB <- 0.409
LBBotVol <- 900300

####### LBPrelim
LBPrelim <- function() {
	if (OptimizedRelSw == 1) {
		LBPrelim_o <- max(LBOpRel(), LBRuleReq())
	} else {
		LBPrelim_o <- min(LBAvailAfter(), max(LBRuleReq(), LBMinReq()))
	}
	return(LBPrelim_o)
}

##### LBOpRel

LBOpRel <- function() {
	LBOpRel_o <- 0
	return(LBOpRel_o)
}

##### LBRuleReq

LBRuleReq <- function() {
	LBRuleReq_o <- max(Libby() + LBIn() - LibbyNetWith() - LBTopVol(), 0)
	return(LBRuleReq_o)
}

##### Libby

Libby <- function() {
	if (week_counter == 1) {
		Libby_o <- InitLB()
		# reservoir_vol_df[week_counter,5]=Libby_o
	} else {
		Libby_o <- reservoir_vol_df[week_counter - 1, 5]
	}
	return(Libby_o)
}

####### InitLB

InitLB <- function() {
	if (InitialConditionSwitch == 0) {
		InitLB_o <- InitLBLink
	} else if (InitialConditionSwitch == 1) {
		InitLB_o <- ResInitFractionFull * LBFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitLB_o <- LBHistStor()
	} else {
		InitLB_o <- LBFullPoolVol
	}
	return(InitLB_o)
}

######## LBHistStor

LBHistStor <- function() {
	LBHistStor_o <- LBHistStor_input[week_counter, 2]
	return(LBHistStor_o)
}

##### LBIn

LBIn <- function() {
	LBIn_o <- LibbyFlowData() - LBEvap() - LBDem()
	return(LBIn_o)
}

LBDem <- function() {
	LBDem_o <- DemVICLB - LBCurtail()
	return(LBDem_o)
}

LBCurtail <- function() {
	LBCurtail_o <- max(0, DemVICLB - LibbyFlowData())
	return(LBCurtail_o)
}
#### LibbyFlowData

LibbyFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		LibbyFlowData_o <- VICLB()
	} else {
		LibbyFlowData_o <- ModLibbyFlowData()
	}
	return(LibbyFlowData_o)
}

####### ModLibbyFlowData

ModLibbyFlowData <- function() {
	print(paste("this has not been used yet"))
	return(0)
}

###### 
VICDem <- function() {
	VICDem_o <- DemVICLB
	return(VICDem_o)
}

######## VICLB

VICLB <- function() {
	if (Use4trDegreeInQAppx == 1) {
		VICLB_o1 <- PriVICLB
	} else {
		VICLB_o1 <- PriVICLB - LBEstEvap() - LBWith()
	}
	VICLB_o <- VICLB_o1 * UpperColMult()
	return(VICLB_o)
}

##### LBEstEvap

LBEstEvap <- function() {
	LBEstEvap_o <- 0	
	return(LBEstEvap_o)
}

####### LBWith

LBWith <- function() {
	if (MarketHydrgy == 0) {
		LBWith_o <- LBEstWith()
	} else {
		LBWith_o <- LBAgWith() + LBAgRech()
	}
	return(LBWith_o)
}

###### LBEstWith

LBEstWith <- function() {
	LBEstWith_o <- 0
	return(LBEstWith_o)
}

#### LBAgWith

LBAgWith <- function() {
	LBAgWith_o <- LBAgWith_input[week_counter_in_year(), 2]
	return(LBAgWith_o)
}

######### LBAgRech

LBAgRech <- function() {
	LBAgRech_o <- LBAgRech_input[week_counter_in_year(), 2]
	return(LBAgRech_o)
}

#### LBEvap

LBEvap <- function() {
	LBEvap_o <- (LBSufaceArea() * LibbyEvapData()) * 0.5042 / 12
	return(LBEvap_o)
}

###### LBSufaceArea

LBSufaceArea <- function() {
	LBSufaceArea_o <- (-1.21281443E-13 * (Libby() / 1000)^4 + 1.53692112E-09 * (Libby() / 1000)^3 - 6.75961255E-06 * (Libby() / 1000)^2 + 1.87278268E-02 * (Libby() / 1000) + 2.30403996) * 1000
	return(LBSufaceArea_o)
}

####### LibbyEvapData

LibbyEvapData <- function() {
	LibbyEvapData_o <- 0
	return(LibbyEvapData_o)
}

###### LibbyNetWith

LibbyNetWith <- function() {
	LibbyNetWith_o <- 0
	return(LibbyNetWith_o)
}

###### LBTopVol

LBTopVol <- function() {
	if (TopRuleSw() == 0) {
		LBTopVol_o <- LibbyFloodCurve()
	} else if (TopRuleSw() == 1) {
		LBTopVol_o <- LBFullPoolVol()
	} else {
		LBTopVol_o <- LBFlood_input[week_counter_in_year(), 2]
	}
	return(LBTopVol_o)
}

#### LibbyFloodCurve

LibbyFloodCurve <- function() {
	if (FC_Option == 1) {
		LibbyFloodCurve_o <- LBFullPoolVol - GlobalFloodEvacMult * (LBFullPoolVol - LB_CurFC())
	} else if (FC_Option == 4) {
		LibbyFloodCurve_o <- LBFullPoolVol - GlobalFloodEvacMult * (LBFullPoolVol - LBCurFC_Cont())
	} else if (FC_Option == 2) {
		LibbyFloodCurve_o <- GlobalFloodEvacMult * (LBFullPoolVol - LB_Hec_FC())
	} else {
		LibbyFloodCurve_o <- GlobalFloodEvacMult * (LBFullPoolVol - LB_ENSOFC())
	}
	return(LibbyFloodCurve_o)
}

###### LB_CurFC

LB_CurFC <- function() {
	if (LBSumAprAug < 4.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 2] # LBFlood1
	} else if (LBSumAprAug < 5.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 3] # LBFlood2
	} else if (LBSumAprAug < 6.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 4] # LBFlood3
	} else if (LBSumAprAug < 7.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 5] # LBFlood4
	} else {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 6] # LBFlood5}
	} 
	return(LB_CurFC_o)
}

##### LBCurFC_Cont

LBCurFC_Cont <- function() { # check
	LB_FloodM_Range <- c(4500000, 5000000, 5500000, 6000000, 6500000, 7000000, 7500000, 8000000)
	if (LB_FloodM_Range[length(LB_FloodM_Range)] < LBSumAprAug) {
		row_no <- length(LB_FloodM_Range)
	} else {
		row_no <- max(1.0, which(LB_FloodM_Range > LBSumAprAug)[1] - 1)
	}
	if (LBSumAprAug <= 8.0E06) {
		if (week_counter_in_year() >= 15 && week_counter_in_year() <= 18) {
			LBCurFC_Cont_o <- 5355100
		} else if (week_counter_in_year() >= 19 && week_counter_in_year() <= 22) {
			LBCurFC_Cont_o <- 3855100
		} else if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 2] # JAN_LB
		} else if ((week_counter_in_year() >= 28 && week_counter_in_year() <= 31)) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 3] # FEB_LB
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 4] #  MAR_LB
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 5] #  APR_LB
		} else {
			LBCurFC_Cont_o <- 5855100
		}
	} else {
		LBCurFC_Cont_o <- LBFlood_input[week_counter_in_year(), 5]
	}
	return(LBCurFC_Cont_o)
}

############# LB_Hec_FC ##### called when FC_Option=2

LB_Hec_FC <- function() {
	print(paste("warning: this function is empty right now and it needs to be implemented from ColSim"))
	return(0)
}

############# LB_ENSOFC ###### called when FC_Option=3
LB_ENSOFC <- function() {
	print(paste("warning: this function is empty right now and needs to be implemented from ColSim"))
	return(0)
}

###### LBAvailAfter

LBAvailAfter <- function() {
	LBAvailAfter_o <- max(0, Libby() + LBIn() - LibbyNetWith() - LBBotVol)
	return(LBAvailAfter_o)
}

###### LBMinReq

LBMinReq <- function() {
	if (unit_convert_switch == 1) {
		LBMinReq_o <- cfsTOafw * LBAvgMin()
	} else {
		LBMinReq_o <- LBAvgMin()
	}
	return(LBMinReq_o)
}

###### LBAvgMin

LBAvgMin <- function() {
	LBAvgMin_o <- 2000 # the original is a time series for each week of year
	return(LBAvgMin_o)
}

######### Libby_April_Evac_Target

Libby_April_Evac_Target <- function() {
	if (LBSumAprAug < 4.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 2] # LBFlood1_2
	} else if (LBSumAprAug < 5.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 2] # LBFlood2_2
	} else if (LBSumAprAug < 6.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 2] # LBFlood3_2
	} else if (LBSumAprAug < 7.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 2] # LBFlood4_2
	} else {
		LBF_o <- LBF_input[week_counter_in_year(), 2]
	} 
	if (FC_Option == 4) {
		Libby_April_Evac_Target_o <- GlobalFloodEvacMult * (LBFullPoolVol - LBAprEvaq_Cont())
	} else {
		Libby_April_Evac_Target_o <- GlobalFloodEvacMult * (LBFullPoolVol - LBF_o)
	}
	return(Libby_April_Evac_Target_o)
}

######### LBAprEvaq_Cont

LBAprEvaq_Cont <- function() {
	if (LBSumAprAug <= 8.0E06) {
		LBAprEvaq_Cont_o <- APR_LB()
	} else {
		LBAprEvaq_Cont_o <- 875100
	}
	return(LBAprEvaq_Cont_o)
}

##### APR_LB

APR_LB <- function() {
	if (which(APR_LB_input > LBSumAprAug)[1] > 1) {
		APR_LB_o <- APR_LB_input[which(APR_LB_input > LBSumAprAug)[1] - 1, 2]
	} else {
		APR_LB_o <- APR_LB_input[which(APR_LB_input > LBSumAprAug)[1], 2]
	}
	return(APR_LB_o)
}

######### LBMcNarySharedWater

LBMcNarySharedWater <- function() {
	LBMcNarySharedWater_o <- max(0, Libby() + LBIn() - LibbyNetWith() - LBPrelim() - LBMcNaryDraftLimit())
	return(LBMcNarySharedWater_o)
}

####### LBMcNaryDraftLimit

LBMcNaryDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		LBMcNaryDraftLimit_o <- LBBotVol
	} else if (Fish_Pool_Alternative == 1) {
		LBMcNaryDraftLimit_o <- 4.975e6
	} else if (Fish_Pool_Alternative == 2) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 1.258E6)
	} else if (Fish_Pool_Alternative == 3) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 1.643E6)
	} else if (Fish_Pool_Alternative == 4) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 2.277E6)
	} else if (Fish_Pool_Alternative == 5) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 3.161E6)
	} else {
		LBMcNaryDraftLimit_o <- 4.975E6
	}
	return(LBMcNaryDraftLimit_o)
}

########## LBEnergyContent

LBEnergyContent <- function() {
	LBEnergyContent_o <- LBSharedWater() * (LBNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBEnergyContent_o)
}

######## LBSharedWater

LBSharedWater <- function() {
	LBSharedWater_o <- max(0, Libby() + LBIn() - LibbyNetWith() - LBPrelim() - LBMcNarySup() - LBBotVol)
	return(LBSharedWater_o)
}

####### LBMcNarySup

LBMcNarySup <- function() {
	# print(paste("LBMcNarySup"))
	if (TotalMcNarySharedWater_c == 0) {
		LBMcNarySup_o <- 0
	} else {
		LBMcNarySup_o <- min(LBMcNarySharedWater(), McNaryFlowDeficit() * LBMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(LBMcNarySup_o)
}

##### LBNetHead

LBNetHead <- function() {
	LBTailElev <- 2118
	LBLoss <- 0
	LBNetHead_o <- LBElev_ft() - LBTailElev - LBLoss
	return(LBNetHead_o)
}

####### LBElev_ft

LBElev_ft <- function() {
	LBElev_ft_o <- 2.50182927E-31 * Libby()^5 - 4.25316189E-24 * Libby()^4 + 2.73998760E-17 * Libby()^3 - 8.64412557E-11 * Libby()^2 + 1.73222635E-04 * Libby() + 2.18520663E+03
	return(LBElev_ft_o)
}

####### LBECCEnergyContent

LBECCEnergyContent <- function() {
	LBECCEnergyContent_o <- LBECCSharedWater() * (LBNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBECCEnergyContent_o)
}

###### LBECCSharedWater

LBECCSharedWater <- function() {
	LBECCSharedWater_o <- max(0, Libby() + LBIn() - LibbyNetWith() - LBPrelim() - LBMcNarySup() - LBECC())
	return(LBECCSharedWater_o)
}

####### LBECC

LBECC <- function() {
	if (UseUpdatedOpSystem == 0) {
		if (month_in_year <= 5 || month_in_year == 12) {
			out1 <- max(LBCriticalCurve(), RefillCurve())
		} else {
			out1 <- min(max(RefillCurve(), LBCriticalCurve()), max(LBCriticalCurve(), LB1931Refill()))
		}	
		LBECC_o <- min(LibbyFloodCurve(), out1)
	} else if (UseUpdatedOpSystem == 1 && UseForecastRLFCurve == 1) { # components are not defined
		if (month_in_year <= 5 || month_in_year == 12) {
			out2 <- LBForecastRLFCurve
		} else {
			out2 <- LBActualRFCurve
		}
		LBECC_o <- min(LibbyFloodCurve(), out2)
	} else { # components are not defined
		if (month_in_year <= 5 || month_in_year == 12) {
			out3 <- LB1931RFCurve
		} else {
			out3 <- LBActualRFCurve
		}
		LBECC_o <- min(LibbyFloodCurve(), out3)
	}
	return(LBECC_o)
}

####### LBCriticalCurve

LBCriticalCurve <- function() {
	LBCriticalCurve_o <- LBCriticalCurve_input[week_counter_in_year(), 2]
	return(LBCriticalCurve_o)
}

#### LB1931Refill

LB1931Refill <- function() {
	LB1931Refill_o <- LB1931Refill_input[week_counter_in_year(), 2]
	return(LB1931Refill_o)
}

######### RefillCurve

RefillCurve <- function() {
	# Refill rule curve based on 1931 inflows and forecasts.  If the refill switch is set to the status quo rules,
	# the model selects the 1931 based refill curve August-December and then selects the forecast refill curve for the remainder of the year.
	# If the refill switch is set to alternate rules, the forecast refill curve is used for the entire year.
	# The second option may allow more non-firm energy releases Aug-December than the status quo rules. Units af.
	LBRefillVol_3 <- 0
	if (RefillSwitch() == 1) {
		RefillCurve_o <- LBRefillVol1
	} else if (RefillSwitch() == 2) {
		RefillCurve_o <- LBRefillVol_2
	} else {
		RefillCurve_o <- LBRefillVol_3
	}
	return(RefillCurve_o)
}

########## LBDamProtectRel

LBDamProtectRel <- function() {
	LBDamProtectRel_o <- max(0, Libby() + LibbyInflow() - LibbyNetWith() - LBFullPoolVol)
	return(LBDamProtectRel_o)
}

#####  LBRelLimit

LBRelLimit <- function() {
	LBRelLimit_o <- min(LBMaxFCRel(), max(Libby() + LibbyInflow() - LibbyNetWith() - LBBotVol, 0))
	return(LBRelLimit_o)
}

##### LBMaxFCRel

LBMaxFCRel <- function() {
	LBMaxFCRel_o <- LBMaxFCRel_input[week_counter_in_year(), 2]
	return(LBMaxFCRel_o)
}

###### LBRelReducReq

LBRelReducReq <- function() {
	LBRelReducReq_o <- TotalRelReducReq() * LBFloodFrac()
	return(LBRelReducReq_o)
}

######## LBFloodFrac

LBFloodFrac <- function() {
	if (TotalFloodSpace_c == 0) {
		LBFloodFrac_o <- 0
	} else {
		LBFloodFrac_o <- (LBFloodMult * LBFloodSpace()) / TotalFloodSpace_c
	}	
	return(LBFloodFrac_o)
}

####### ModBonnersFerryFlowData

ModBonnersFerryFlowData <- function() {
	ModBonnersFerryFlowData_o <- ModBonnersFerryFlowData_input[week_counter_in_year(), 2]
	return(ModBonnersFerryFlowData_o)
}

############### Brownlee

BRFullPoolVol <- 1.420e6 # Volume corresponding to 2077 ft of elevation.  Normal full pool.  Units acre-ft.
BRBotVol <- 443000 # Volume cooresponding to the bottom of conservation pool elevation of 1976 ft.  Units acre-ft.
BRTailElev <- 1805 # Tailwater elevation.  Units ft.
BRCombEfficiency <- 0.8

####  BRPrelim

BRPrelim <- function() {
	if (OptimizedRelSw == 1) {
		BRPrelim_o <- max(BROpRel(), BRRuleReq())
	} else {
		BRPrelim_o <- min(BRAvailAfter(), max(BRRuleReq(), BRMinReq()))
	}
	return(BRPrelim_o)
}

######  BROpRel
# Time series reservoir  release to compare with optimization models.  Units acre-ft/month.

BROpRel <- function() {
	BROpRel_o <- 0
	return(BROpRel_o)
}

###### BRRuleReq

BRRuleReq <- function() {
	BRRuleReq_o <- max(Brownlee() + BRInflow() - BRNetWith() - BRTopVol(), 0)
	return(BRRuleReq_o)
}

###### BRNetWith

BRNetWith <- function() {
	BRNetWith_o <- 0
	return(BRNetWith_o)
}

######### Brownlee

Brownlee <- function() {
	if (week_counter == 1) {
		Brownlee_o <- InitBR()
		#  reservoir_vol_df[week_counter,9]=Brownlee_o
	} else {
		Brownlee_o <- reservoir_vol_df[week_counter - 1, 9]
	}
	return(Brownlee_o)
}

##### InitBR

InitBR <- function() {
	if (InitialConditionSwitch == 0) {
		InitBR_o <- InitBRLink()
	} else if (InitialConditionSwitch == 1) {
		InitBR_o <- ResInitFractionFull * BRFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitBR_o <- BRHistStor()
	} else {
		InitBR_o <- BRFullPoolVol
	}
	return(InitBR_o)
}

####### InitBRLink

InitBRLink <- function() {
	InitBRLink_o <- 1295476.748
	return(InitBRLink_o)
}

#### BRHistStor

BRHistStor <- function() {
	BRHistStor_o <- BRHistStor_input[week_counter, 2]
	return(BRHistStor_o)
}

######## BRInflow

BRInflow <- function() {
	if (ResetStorage() == 1) {
		BRInflow_o <- InitBR() - Brownlee()
	} else if (UseRegFlowAtBR() == 1) {
		BRInflow_o <- BRIncInflow()
	} else {
		#BRInflow_o <- MSOutflow() + BRIncInflow() + MSAgReturns() - MSAgDiversion() - MSToMarket()
		BRInflow_o <- MSOutflow() + BRIncInflow() - BRDem()
	}
	return(BRInflow_o)
}

#####  BRIncInflow

BRIncInflow <- function() {
	if (UseRegFlowAtBR() == 1) {
		BRIncInflow_o <- (BrownleeObsRegFlowData() - BREvap())
	} else {
		BRIncInflow_o <- (BrownleeNatFlowData() - MilnerNatFlowData()) - BREvap()
	}
	return(BRIncInflow_o)
}

####### BREvap

BREvap <- function() {
	BREvapData <- 0
	BREvap_o <- BRSufaceArea() * BREvapData * 0.5042 / 12
	return(BREvap_o)
}

#### BRSufaceArea

BRSufaceArea <- function() {
	BRSufaceArea_o <- Brownlee() * 0
	return(BRSufaceArea_o)
}


####### BrownleeNatFlowData

BrownleeNatFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		BrownleeNatFlowData_o <- VICBR()
	} else {
		BrownleeNatFlowData_o <- BrownleeObsNatFlowData()
	}
	return(BrownleeNatFlowData_o)
}

###### BrownleeObsNatFlowData

BrownleeObsNatFlowData <- function() {
	BrownleeObsNatFlowData_o <- BrownleeObsNatFlowData_input[week_counter_in_year(), 2]
	return(BrownleeObsNatFlowData_o)
}

###### VICNatBR

VICNatBR <- function() {
	VICNatBR_o <- SnakeMult() * PriVICBR
	return(VICNatBR_o)
}

####### MilnerNatFlowData

MilnerNatFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		MilnerNatFlowData_o <- VICNatMIL()
	} else {
		MilnerNatFlowData_o <- MilnerObsNatFlowData()
	}
	return(MilnerNatFlowData_o)
}

######## VICNatMIL

VICNatMIL <- function() {
	VICNatMIL_o <- PriVICMIL * SnakeMult()
	return(VICNatMIL_o)
}

###### Demand upstream of Milner

MILDem <- function() {
	MILDem_o <- DemVICMIL
	return(MILDem_o)
}


######## MilnerObsNatFlowData

MilnerObsNatFlowData <- function() {
	MilnerObsNatFlowData_o <- MilnerObsNatFlowData_input[week_counter_in_year(), 2]
	return(MilnerObsNatFlowData_o)
}

###### BRTopVol

BRTopVol <- function() {
	BRTopVol_o <- BRFloodVolume()
	return(BRTopVol_o)
}

###### BRFloodVolume

BRFloodVolume <- function() {
	if (FC_Option == 1) {
		if (month_in_year == 8 || month_in_year == 9) {
			BRFV <- max(0, BRBaseFloodCurve() - Add_Space())
		} else {
			BRFV <- BRBaseFloodCurve()
		}
		BRFloodVolume_o <- (BRFullPoolVol - GlobalFloodEvacMult * (BRFullPoolVol - (min(BRFullPoolVol, BRFV))))
	} else {
		BRFloodVolume_o <- (BRFullPoolVol - GlobalFloodEvacMult * (BRFullPoolVol - BRFC_Cont()))
	}
	return(BRFloodVolume_o)
}

######### BRBaseFloodCurve

BRBaseFloodCurve <- function() {
	if (BRFloodCond() == 1) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 2] # BRFlood1
	} else if (BRFloodCond() == 2) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 3] # BRFlood2
	} else if (BRFloodCond() == 3) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 4] # BRFlood3
	} else if (BRFloodCond() == 4) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 5] # BRFlood4
	} else if (BRFloodCond() == 5) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 6] # BRFlood5
	} else {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 7]
	} # BRFlood6
	return(BRBaseFloodCurve_o)
}

####### BRFloodCond

BRFloodCond <- function() {
	if (BRRunoffAprJuly < 2.5e6) {
		BRFloodCond_o <- 1
	} else if (BRRunoffAprJuly < 3.0e6) {
		BRFloodCond_o <- 2
	} else if (BRRunoffAprJuly < 4.0e6) {
		BRFloodCond_o <- 3
	} else if (BRRunoffAprJuly < 5.0e6) {
		BRFloodCond_o <- 4
	} else if (BRRunoffAprJuly < 6.0e6) {
		BRFloodCond_o <- 5
	} else {
		BRFloodCond_o <- 6
	}
	return(BRFloodCond_o)
}

####### Add_Space

Add_Space <- function() {
	row_num <- DallesFloodCond()
	Add_Space_o <- AddSp_input[row_num, 2]
	return(Add_Space_o)
}

# Flood Storage Selection Based on Forecast Inflows -----------------------
######## BRFC_Cont

BRFC_Cont <- function() {
	if (DallesRunoffAprAug <= 75e06) {
		BRFC_Cont_o <- BRCurFC_Cont75()
	} else if (DallesRunoffAprAug <= 80e06) {
		BRFC_Cont_o <- BRCurFC_Cont80()
	} else if (DallesRunoffAprAug <= 85e06) {
		BRFC_Cont_o <- BRCurFC_Cont85()
	} else if (DallesRunoffAprAug <= 90e06) {
		BRFC_Cont_o <- BRCurFC_Cont90()
	} else if (DallesRunoffAprAug <= 95e06) {
		BRFC_Cont_o <- BRCurFC_Cont95()
	} else if (DallesRunoffAprAug <= 100e06) {
		BRFC_Cont_o <- BRCurFC_Cont100()
	} else if (DallesRunoffAprAug <= 105e06) {
		BRFC_Cont_o <- BRCurFC_Cont105()
	} else if (DallesRunoffAprAug <= 110e06) {
		BRFC_Cont_o <- BRCurFC_Cont110()
	} else {
		BRFC_Cont_o <- BRCurFC_Cont115()
	}
	return(BRFC_Cont_o)
}

##### BRCurFC_Cont75

BRCurFC_Cont75 <- function() {
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont75_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont75_o <- BRForecastFloodStorage[row_no, 2] # FEB_BR75
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont75_o <- BRForecastFloodStorage[row_no, 3] # MAR_BR75
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont75_o <- BRForecastFloodStorage[row_no, 4] # APR_BR75
		} else {
			BRCurFC_Cont75_o <- 1420000
		}
	} else {
		BRCurFC_Cont75_o <- BRCurFC_Cont[week_counter_in_year(), 2]
	} # BRFlood6_75
	return(BRCurFC_Cont75_o)
}

##### BRCurFC_Cont80

BRCurFC_Cont80 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont80_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont80_o <- BRForecastFloodStorage[row_no, 5] # FEB_BR80
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont80_o <- BRForecastFloodStorage[row_no, 6] # MAR_BR80
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont80_o <- BRForecastFloodStorage[row_no, 7] # APR_BR80
		} else {
			BRCurFC_Cont80_o <- 1420000
		}
	} else {
		BRCurFC_Cont80_o <- BRCurFC_Cont[week_counter_in_year(), 3]
	} # BRFlood6_80
	return(BRCurFC_Cont80_o)
}

##### BRCurFC_Cont85

BRCurFC_Cont85 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont85_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont85_o <- BRForecastFloodStorage[row_no, 8] # FEB_BR85
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont85_o <- BRForecastFloodStorage[row_no, 9] # MAR_BR85
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont85_o <- BRForecastFloodStorage[row_no, 10] # APR_BR85
		} else {
		BRCurFC_Cont85_o <- 1420000
		}
	} else {
		BRCurFC_Cont85_o <- BRCurFC_Cont[week_counter_in_year(), 4]
	} # BRFlood6_85
	return(BRCurFC_Cont85_o)
}

##### BRCurFC_Cont90

BRCurFC_Cont90 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont90_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont90_o <- BRForecastFloodStorage[row_no, 11] # FEB_BR90
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont90_o <- BRForecastFloodStorage[row_no, 12] # MAR_BR90
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont90_o <- BRForecastFloodStorage[row_no, 13] # APR_BR90
		} else {
			BRCurFC_Cont90_o <- 1420000
		}
	} else {
		BRCurFC_Cont90_o <- BRCurFC_Cont[week_counter_in_year(), 5]
	}	 # BRFlood6_90
	return(BRCurFC_Cont90_o)
}

##### BRCurFC_Cont95

BRCurFC_Cont95 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont95_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont95_o <- BRForecastFloodStorage[row_no, 14] # FEB_BR95
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont95_o <- BRForecastFloodStorage[row_no, 15] # MAR_BR95
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont95_o <- BRForecastFloodStorage[row_no, 16] # APR_BR95
		} else {
			BRCurFC_Cont95_o <- 1420000
		}
	} else {
		BRCurFC_Cont95_o <- BRCurFC_Cont[week_counter_in_year(), 6]
	} # BRFlood6_95
	return(BRCurFC_Cont95_o)
}

##### BRCurFC_Cont100

BRCurFC_Cont100 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont100_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont100_o <- BRForecastFloodStorage[row_no, 17] # FEB_BR100
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont100_o <- BRForecastFloodStorage[row_no, 18] # MAR_BR100
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont100_o <- BRForecastFloodStorage[row_no, 19] # APR_BR100
		} else {
			BRCurFC_Cont100_o <- 1420000
		}
	} else {
		BRCurFC_Cont100_o <- BRCurFC_Cont[week_counter_in_year(), 7]
	} # BRFlood6_100
	return(BRCurFC_Cont100_o)
}

##### BRCurFC_Cont105

BRCurFC_Cont105 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont105_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont105_o <- BRForecastFloodStorage[row_no, 20] # FEB_BR105
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont105_o <- BRForecastFloodStorage[row_no, 21] # MAR_BR105
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont105_o <- BRForecastFloodStorage[row_no, 22] # APR_BR105
		} else {
			BRCurFC_Cont105_o <- 1420000
		}
	} else {
		BRCurFC_Cont105_o <- BRCurFC_Cont[week_counter_in_year(), 8]
	} # BRFlood6_105
	return(BRCurFC_Cont105_o)
}

##### BRCurFC_Cont110

BRCurFC_Cont110 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont110_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont110_o <- BRForecastFloodStorage[row_no, 23] # FEB_BR110
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont110_o <- BRForecastFloodStorage[row_no, 24] # MAR_BR110
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont110_o <- BRForecastFloodStorage[row_no, 25] # APR_BR110
		} else {
			BRCurFC_Cont110_o <- 1420000
		}
	} else {
		BRCurFC_Cont110_o <- BRCurFC_Cont[week_counter_in_year(), 9]
	}	 # BRFlood6_110
	return(BRCurFC_Cont110_o)
}

##### BRCurFC_Cont115

BRCurFC_Cont115 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (week_counter_in_year() >= 23 && week_counter_in_year() <= 27) {
			BRCurFC_Cont115_o <- 1170000
		} else if (week_counter_in_year() >= 28 && week_counter_in_year() <= 31) {
			BRCurFC_Cont115_o <- BRForecastFloodStorage[row_no, 26] # FEB_BR115
		} else if (week_counter_in_year() >= 32 && week_counter_in_year() <= 35) {
			BRCurFC_Cont115_o <- BRForecastFloodStorage[row_no, 27] # MAR_BR115
		} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
			BRCurFC_Cont115_o <- BRForecastFloodStorage[row_no, 28] # APR_BR115
		} else {
			BRCurFC_Cont115_o <- 1420000
		}
	} else {
		BRCurFC_Cont115_o <- BRCurFC_Cont[week_counter_in_year(), 10]
	} # BRFlood6_115
	return(BRCurFC_Cont115_o)
}

# BRAvailAfter ------------------------------------------------------------

BRAvailAfter <- function() {
	if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	BRAvailAfter_o <- max(0, Brownlee() + BRIn_c - BRNetWith() - BRBotVol)
	return(BRAvailAfter_o)
}

######## BRIn

BRIn <- function() {
	if (UseRegFlowAtBR() == 1) {
		BRIn_o <- BrownleeObsRegFlowData() - BREvap()
	} else {
		#BRIn_o <- MSRelease() + (BrownleeNatFlowData() - MilnerNatFlowData()) - MSPreAgSupply() - BREvap() - BRDem()
		BRIn_o <- MSRelease() + (BrownleeNatFlowData() - MilnerNatFlowData()) - BREvap() - BRDem()
	}
	return(BRIn_o)
}

####### BrownleeObsRegFlowData

BrownleeObsRegFlowData <- function() {
	BrownleeObsRegFlowData_o <- BrownleeObsRegFlowData_input[week_counter_in_year(), 2]
	return(BrownleeObsRegFlowData_o)
}

####### BRMinReq
# Minimum release requirement converted to cfsd.

BRMinReq <- function() {
	BRMinReq_o <- max(min(BRAvailAfter(), BRFirmEngRelReq()), max(
	min(BRMaxNonFirmRel(), BRNonFirmEngRelReq()),
    max(min(BRLGAvailAfter(), BRRelForLG()), max(BRRelForJBandLP(), BRAvgMin() * cfsTOafw))
	))
	return(BRMinReq_o)
}

##### BRMaxNonFirmRel

BRMaxNonFirmRel <- function() {
	if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4]<<-BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	BRMaxNonFirmRel_o <- max(0, Brownlee() + BRIn_c - BRNetWith() - BRRefillCurve)
	return(BRMaxNonFirmRel_o)
}

####### BRNonFirmEngRelReq

BRNonFirmEngRelReq <- function() {
	if (NonFirmEnergySw == 1) {
		BRNonFirmEngRelReq_o <- min(BRPenLimit(), 4.2603e7 * (BRFirmEngTarget() + BRNonFirmTarget()) / (43560 * BRNetHead() * BRCombEfficiency))
	} else {
		BRNonFirmEngRelReq_o <- 0
	}
	return(BRNonFirmEngRelReq_o)
}

####### BRPenLimit

BRPenLimit <- function() {
	BRPenCap <- 34500
	BRPenLimit_o <- BRPenCap * cfsTOafw
	return(BRPenLimit_o)
}

####### BRFirmEngTarget

BRFirmEngTarget <- function() {
	BRFirmEngTarget_o <- 0 # weekly time-series of zeros
	return(BRFirmEngTarget_o)
}

######### BRNonFirmTarget

BRNonFirmTarget <- function() {
	BRNonFirmTarget_o <- 0 # weekly time-series of zeros
	return(BRNonFirmTarget_o)
}

###### BRNetHead

BRNetHead <- function() {
	BRLoss <- 0 # Piping head losses.  Units ft.
	BRNetHead_o <- BRElev_ft() - BRTailElev - BRLoss
	return(BRNetHead_o)
}


####### BRElev_ft
# Elevation of water in reservoir from curve fit storage to elevation relationship.  Units ft.

BRElev_ft <- function() {
	BRElev_ft_o <- 4.98417182E-28 * Brownlee()^5 - 2.02560727E-21 * Brownlee()^4 + 3.05422588E-15 * Brownlee()^3 - 0.00000000215214417 * Brownlee()^2
	+ 0.000843642399 * Brownlee() + 1827.36033
	return(BRElev_ft_o)
}

####### BRLGAvailAfter

BRLGAvailAfter <- function() {
	if (is.na(water_df[week_counter,4])) { 
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c=water_df[week_counter,4]
	BRLGAvailAfter_o <- max(0, Brownlee() + BRIn_c - BRNetWith() - BRDraftLimit())
	return(BRLGAvailAfter_o)
}

###### BRDraftLimit

BRDraftLimit <- function() {
	if (UseAllStorForMcNLG == 1) {
		BRDraftLimit_o <- BRBotVol
	} else {
		BRDraftLimit_o <- 1.183e6
	}
	return(BRDraftLimit_o)
}

####### BrownleeFlowData

BrownleeFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		BrownleeFlowData_o <- VICBR()
	} else {
		BrownleeFlowData_o <- ModBrownleeFlowData()
	}
	return(BrownleeFlowData_o)
}

##### VICBR

VICBR <- function() {
	VICBR_o <- SnakeMult() * PriVICBR
	return(VICBR_o)
}

##### Demand between Brownlee and Milner

BRDem <- function() {
	BRDem_o <- DemVICBR
	return(BRDem_o)
}

##### ModBrownleeFlowData

ModBrownleeFlowData <- function() {
	ModBrownleeFlowData_o <- ModBrownleeFlowData_input[week_counter_in_year(), 2] # check
	return(ModBrownleeFlowData_o)
}

####### BRFirmEngRelReq
# Release required to produce the firm energy target.  Units af.

BRFirmEngRelReq <- function() {
	BRFirmEngRelReq_o <- min(BRPenLimit(), 4.2603e7 * (BRFirmEngTarget()) / (43560 * BRNetHead() * BRCombEfficiency))
	return(BRFirmEngRelReq_o)
}

####### BRRelForJBandLP

BRRelForJBandLP <- function() {
	BRRelForJBandLP_o <- max(BRRelForJohnsonsBar(), BRRelForLimePoint())
	return(BRRelForJBandLP_o)
}

######### BRRelForJohnsonsBar

BRRelForJohnsonsBar <- function() {
	BRRelForJohnsonsBar_o <- max(0, (JohnsonBarFlowTarget() * cfsTOafw) - (HCInc()))
	return(BRRelForJohnsonsBar_o)
}

########## JohnsonBarFlowTarget

JohnsonBarFlowTarget <- function() {
	JohnsonBarFlowTarget_o <- 5000 # monthly time-series
	return(JohnsonBarFlowTarget_o)
}

######## HCInc

HCInc <- function() {
	HCInc_o <- HellsCanyonFlowData() - OxbowFlowData()
	return(HCInc_o)
}

######### HellsCanyonFlowData

HellsCanyonFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		HellsCanyonFlowData_o <- VICHC()
	} else {
		HellsCanyonFlowData_o <- ModHellsCanyonFlowData()
	}
	return(HellsCanyonFlowData_o)
}

###### VICHC

VICHC <- function() {
	VICHC_o <- PriVICHC
	return(VICHC_o)
}

#### BRToHC

BRToHC <- function() {
	BRToHC_o <- 1.0389
	return(BRToHC_o)
}

######## ModHellsCanyonFlowData

ModHellsCanyonFlowData <- function() { # check # what is the right time step?
	ModHellsCanyonFlowData_o <- ModHellsCanyonFlowData_input[week_counter_in_year(), 2]
	return(ModHellsCanyonFlowData_o)
}

######### OxbowFlowData

OxbowFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		OxbowFlowData_o <- VICOX()
	} else {
		OxbowFlowData_o <- ModOxbowFlowData()
	}
	return(OxbowFlowData_o)
}

##### VICOX

VICOX <- function() {
	VICOX_o <- PriVICOX
	return(VICOX_o)
}

###### BRToOx

BRToOx <- function() {
	BRToOx_o <- 1.0
	return(BRToOx_o)
}


####### ModOxbowFlowData

ModOxbowFlowData <- function() {
	ModOxbowFlowData_o <- ModOxbowFlowData_input[week_counter_in_year(), 2]
	return(ModOxbowFlowData_o)
}

##### BRRelForLimePoint

BRRelForLimePoint <- function() {
	BRRelForLimePoint_o <- max(0, (LimePointFlowTarget() * cfsTOafw) - (HCInc() + LPInc()))
	return(BRRelForLimePoint_o)
}

##### LimePointFlowTarget
# Minimum Flows at the Lime Point (75 miles below Hells Canyon) are required to be at least 13000 cfs 95% of the time,
# with any deviations from these flows occuring July-Sept.  The model assumes these flows must be maintained 100% of the time.
# In addition a minimum flow of 5000 cfs at Johnson's Bar (17 miles downstream of Hells Canyon) must be maintained.
# While in actual practice some storage is available for support of these flows from run-of-river projects,
# Brownlee must generally make sufficient average releases to support these flows on a monthly time frame.
# The model assumes that Brownlee supplies all supplements to natural inflow required to meet the targets.

LimePointFlowTarget <- function() {
	LimePointFlowTarget_o <- 13000 # monthly time-series
	return(LimePointFlowTarget_o)
}

####### LPInc

LPInc <- function() {
	LPInc_o <- LimePointFlowData() - HellsCanyonFlowData()
	return(LPInc_o)
}

###### LimePointFlowData

LimePointFlowData <- function() {
	LimePointFlowData_o <- HellsCanyonFlowData() * 1.3
	return(LimePointFlowData_o)
}

######### BRAvgMin

BRAvgMin <- function() {
	BRAvgMin_o <- 0
	return(BRAvgMin_o)
}

####### BRRelLimit

BRRelLimit <- function() {
	BRRelLimit_o <- max(Brownlee() + BRInflow() - BRNetWith() - BRBotVol, 0)
	return(BRRelLimit_o)
}

####### BRCombSup

BRCombSup <- function() {
	BRCombSup_o <- 0
	return(BRCombSup_o)
}

##################################
#######################################################
###################################################################
# McNary
######### McNaryFlowDeficit

McNaryFlowDeficit <- function() {
	# print(paste("flow deficit"))
	if (BRPrelim_c == -9999) {
		BRPrelim_c <<- BRPrelim()
		#BRPrelim_df[week_counter, 1] <- BRPrelim_c
		water_df[week_counter,1] <<- BRPrelim_c
		LowerPrelimUpFlow_c <<- LowerPrelimUpFlow()
		LowerPrelimUpFlow_df[week_counter, 1] <- LowerPrelimUpFlow_c
	}
	McNaryFlowDeficit_o <- max(0, ((McNaryBaseTarget() + McN_variable_1()) * cfsTOafw) - McPrelim())
	return(McNaryFlowDeficit_o)
}

######## McNaryBaseTarget

McNaryBaseTarget <- function() {
	McNaryBaseTarget_o <- MNB_input[month_in_year, 2]
	return(McNaryBaseTarget_o)
}

##### McN_variable_1
# Variable minflow for McNary based upon forecasted inflow to the Dalles, for the period of April 20 to June 30.
# units cfs
# Flow varies linearly between 220 and 260 kcfs based on inflows of 85 to 105 MAF at the Dalles

McN_variable_1 <- function() {
	if (DallesJanJul <= 85e6) {
		McNV9 <- 0
	} else if (DallesJanJul >= 105E6) {
		McNV9 <- 40000
	} else {
		McNV9 <- (DallesJanJul - 85E6) / 20E6 * 40000
	}
	if (month_in_year == 10 || month_in_year == 11) {
		if (DallesJanJul <= 85e6) {
			McN_variable_1_o <- 0
		} else if (DallesJanJul >= 105E6) {
			McN_variable_1_o <- 40000
		} else {
			McN_variable_1_o <- (DallesJanJul - 85E6) / 20E6 * 40000
		}
	} else if (month_in_year == 9) {
		McN_variable_1_o <- 10 / 30 * (McNV9)
	} else {
		McN_variable_1_o <- 0
	}
	return(McN_variable_1_o)
}

########## McPrelim

McPrelim <- function() {
	# print(paste("now here 4"))
	McPrelim_o <- LowerPrelimUpFlow_c + McNaryFlowData()
	return(McPrelim_o)
}

######### LowerPrelimUpFlow

LowerPrelimUpFlow <- function() {
	LowerPrelimUpFlow_o <- (BRPrelim_c + DWPrelim() + GCPrelim()) - (BrownleeFlowData() + DworshakFlowData() + GrandCouleeFlowData())
	return(LowerPrelimUpFlow_o)
}

###### McNaryFlowData

McNaryFlowData <- function() {
	if (InflowDataSwitch() == 1) {
		McNaryFlowData_o <- VICMCN()
	} else {
		McNaryFlowData_o <- ModMcNaryFlowData()
	}	
	return(McNaryFlowData_o)
}

####### VICMCN

VICMCN <- function() {
	VICMCN_o <- PriVICMCN * SnakeMult()
	return(VICMCN_o)
}

####### Demand between McNary and Ice Harbor

MCNDem <- function() {
	if (simulate_curtailment == 1) {
		MCNDem_o <- DemVICMCN - MCNCurtail()
	} else {
		MCNDem_o <- DemVICMCN
	}
	return(MCNDem_o)
}

MCNCurtail <- function() {
	if (simulate_curtailment == 1) {
		MCNCurtail_0 <- min(DemVICMCN, max(IflowMCN + DemVICMCN - (IHOut() + PROut() + McNInc()), 0))
		MCNCurtail_o <- ifelse(MCNCurtail_0 > 0, CurtVICMCN, 0)
	} else {
		MCNCurtail_o <- 0
	}
	return(MCNCurtail_o)
}

######## ModMcNaryFlowData

ModMcNaryFlowData <- function() { # check
	ModMcNaryFlowData_o <- ModMcNaryFlowData_input[week_counter, 2]
	return(ModMcNaryFlowData_o)
}

###### McNInc

McNInc <- function() {
	McNInc_o <- McNaryFlowData() - IceHarborFlowData() - PriestRapidsFlowData()
	return(McNInc_o)
}

##### JDInc

JDInc <- function() {
	JDInc_o <- JohnDayFlowData() - McNaryFlowData()
	return(JDInc_o)
}

###################################
###########################################################
##################################################################################
######## MSAgReturns

MSAgReturns <- function() {
	if (MSAgDiversion() > 0) {
		MSAD <- 0.9 * BaseMSAgReturn() * max(0, min(1, (MSAgDiversion() / BaseMSAgWith())))
	} else {
		MSAD <- 0
	}
	MSAgReturns_o <- AgON * MSAD
	return(MSAgReturns_o)
}

###### BaseMSAgReturn

BaseMSAgReturn <- function() {
	BaseMSAgReturn_o <- BaseMSAgReturn_input[month_in_year, 2]
	return(BaseMSAgReturn_o)
}

####### MSAgDiversion

## Agricultural Diversions

MSAgDiversion <- function() {
	MSAgDiversion_o <- MSAgFinal() * AgON
	return(MSAgDiversion_o)
}

####### MSAgFinal

MSAgFinal <- function() {
	MSAgFinal_o <- MSPreAgSupply() - MSAgSale()
	return(MSAgFinal_o)
}

######## MSPreAgSupply

MSPreAgSupply <- function() {
	MSPreAgSupply_o <- min(MSNetAgRight(), MSAgAvailWater())
	return(MSPreAgSupply_o)
}

######## MSNetAgRight

MSNetAgRight <- function() {
	MSNetAgRight_o <- (BaseMSAgWith()) * SnakeAgMultiplier
	return(MSNetAgRight_o)
}

######### BaseMSAgWith

BaseMSAgWith <- function() {
	BaseMSAgWith_o <- BaseMSAgWith_input[week_counter_in_year(), 2]
	return(BaseMSAgWith_o)
}

###### MSAgAvailWater

MSAgAvailWater <- function() {
	MSAgAvailWater_o <- max(0, MSPrelim() + (BrownleeNatFlowData() - MilnerNatFlowData()) - (MSMIFReq() * cfsTOafw))
	return(MSAgAvailWater_o)
}

######## MSAgSale

MSAgSale <- function() {
	MSAgSale_o <- SnakeTransfer() * MSQ()
	return(MSAgSale_o)
}

####### MSQ

MSQ <- function() {
	if (SnakeMarketQ() > 0) {
		MSQ_o <- AvailMSMarketQ() / SnakeMarketQ()
	} else {
		MSQ_o <- 0
	}
	return(MSQ_o)
}

########  SnakeMarketQ

SnakeMarketQ <- function() {
	SnakeMarketQ_o <- AvailMSMarketQ() + AvailUSMarketQ()
	return(SnakeMarketQ_o)
}

#### AvailMSMarketQ

AvailMSMarketQ <- function() {
	if (MSPreAgSupply() > BaseMSAgWith()) {
		AvailMSMarketQ_o <- BaseMSAgWith() * (1 - QReserved())
	} else {
		AvailMSMarketQ_o <- MSPreAgSupply() - BaseMSAgWith() * (QReserved())
	}
	return(AvailMSMarketQ_o)
}

####### QReserved

QReserved <- function() {
	QReserved_o <- 0 # check # is it 0 or 0.5?
	return(QReserved_o)
}

###### AvailUSMarketQ

AvailUSMarketQ <- function() {
	if (USPreAgSupply() > BaseUSAgWith()) {
		AvailUSMarketQ_o <- BaseUSAgWith() * (1 - QReserved())
	} else {
		AvailUSMarketQ_o <- USPreAgSupply() - BaseUSAgWith() * (QReserved())
	}
	return(AvailUSMarketQ_o)
}

#### SnakeTransfer

SnakeTransfer <- function() {
	if (ScarcitySwtchSnake() == 1) {
		SnakeTransfer_o <- min(SnakeMarketQ(), LGPremarketSF())
	} else {
		SnakeTransfer_o <- 0
	}
	return(SnakeTransfer_o)
}

###### LGPremarketSF

LGPremarketSF <- function() { # check # not sure if the time series of data is weekly or daily or something else
	if (PreMarketSF_preprocessed == 4) {
		LGPremarketSF_o <- B064X_input[week_counter_in_year(), 2] # B0644
	} else if (PreMarketSF_preprocessed == 6) {
		LGPremarketSF_o <- B064X_input[week_counter_in_year(), 4] # B0646
	} else if (PreMarketSF_preprocessed == 7) {
		LGPremarketSF_o <- B064X_input[week_counter_in_year(), 5] # B0647
	} else {
		LGPremarketSF_o <- B064X_input[week_counter_in_year(), 3]
	} # B0645
	return(LGPremarketSF_o)
}

####### ScarcitySwtchSnake

ScarcitySwtchSnake <- function() {
	ScarcitySwtchSnake_o <- Market_Switch() * VCSnke()
	return(ScarcitySwtchSnake_o)
}

####### Market_Switch

Market_Switch <- function() {
	if (Market_OPEN == 1) {
		Market_Switch_o <- 1
	} else if (Market_CLOSED == 1) {
		Market_Switch_o <- 0
	} else {
		Market_Switch_o <- 0
	}
	return(Market_Switch_o)
}

####### VCSnke
# Volume contingency

VCSnke <- function() {
	VCSnke_o <- 
	if (FCIrQSnke() <= QSnke()) {
		VCSnke_o <- 1
	} else {
		VCSnke_o <- 0
	}
	return(VCSnke_o)
}

###### FCIrQSnke # check # the time series doesn't look right, make sure this uses a correct time step

FCIrQSnke <- function() {
	FCIrQSnke_o <- FCIrQSnke_input[week_counter_in_year(), 1]
	return(FCIrQSnke_o)
}

##### QSnke # Q%Snke

QSnke <- function() {
	row_noQS <- which(QSnke_input[, 1] > CDFSnke)[1]
	QSnke_o <- QSnke_input[row_noQS, 2]
	return(QSnke_o)
}

########## MSToMarket

MSToMarket <- function() {
	MSToMarket_o <- MSAgSale()
	return(MSToMarket_o)
}

#################################
#########################################################
###################################################################################
# Middle Snake Composite Reservoir

MSBotVol <- 119800 # Volume cooresponding to sum of bottom pool volume at all reservoirs.  Units acre-ft.
MSFullPoolVol <- 4242400 # Volume cooresponding to sum of full pool volume at all reservoirs.  Units acre-ft.

######### MSPrelim
# Preliminary release based upon rule requirements and minimum releases. The release cannot be larger than the available water after withdrawals.  Units cfs-days/m

MSPrelim <- function() {
	#MSPrelim_o <- min(MSAvailAfter(), max(MSRuleReq(), MSMinReq()))
	MSPrelim_o <- min(MSAvailAfter(), MSRuleReq())
	return(MSPrelim_o)
}

####### MSAvailAfter
# Total water available for release after withdrawals and returns  including inflow for the month.  Units cfs-days.

MSAvailAfter <- function() {
	MSAvailAfter_o <- max(0, MdlSnakeComb() + MSIn() - MSBotVol)
	return(MSAvailAfter_o)
}

####### MdlSnakeComb

MdlSnakeComb <- function() {
	# MdlSnakeComb_o=MSTopVol()
	if (week_counter == 1) {
		MdlSnakeComb_o <- MSTopVol()
		# reservoir_vol_df[week_counter,1]=MicaRes
	} else {
		MdlSnakeComb_o <- reservoir_vol_df[week_counter - 1, 11]
	}
	return(MdlSnakeComb_o)
}

##### MSTopVol
# Top of conservation pool.  Units acre-ft.

MSTopVol <- function() {
	MSTopVol_o <- MSFloodRuleCurve()
	return(MSTopVol_o)
}

######### MSFloodRuleCurve

MSFloodRuleCurve <- function() {
	MSFloodRuleCurve_o <- MSFloodRuleCurve_input[week_counter_in_year(), 2]
	return(MSFloodRuleCurve_o)
}

##### MSIn

MSIn <- function() {
	#MSIn_o <- USPrelim() + USRchIncInflow() - min(USPrelim() + USRchIncInflow(), USNetAgRight() + MSEvap())
	MSIn_o <- USPrelim() + USRchIncInflow() - min(USPrelim() + USRchIncInflow(), MILDem() + MSEvap())
	#MSIn_o <- max(0, MilnerNatFlowData() - MSEvap() - MILDem())
	return(MSIn_o)
}

######## MSEvap

MSEvap <- function() {
	MSEvapData <- 0
	MSEvap_o <- (MSSufaceArea() * MSEvapData) * 0.5042 / 12
	return(MSEvap_o)
}

###### MSSufaceArea

MSSufaceArea <- function() {
	MSSufaceArea_o <- MdlSnakeComb() * 0
	return(MSSufaceArea_o)
}

###### MSRuleReq

MSRuleReq <- function() {
	MSRuleReq_o <- max(MdlSnakeComb() + MSIn() - MSTopVol(), 0)
	return(MSRuleReq_o)
}

##### MSMinReq

MSMinReq <- function() {
	MSMinReq_o <- max((MSAvgMin() * cfsTOafw), MSAgReleaseReq())	
	return(MSMinReq_o)
}

###### MSAvgMin

MSAvgMin <- function() {
	MSAvgMin_o <- 1000 # it was originally a time-series
	return(MSAvgMin_o)
}

#### MSAgReleaseReq

MSAgReleaseReq <- function() {
	MSAgReleaseReq_o <- max(0, (MSNetAgRight() - (BrownleeNatFlowData() - MilnerNatFlowData()) + (MSMIFReq() * cfsTOafw)))
	return(MSAgReleaseReq_o)
}

###### MSMIFReq

MSMIFReq <- function() {
	MSMIFReq_o <- MSMIFReq_input[week_counter_in_year(), 2]
	return(MSMIFReq_o)
}

##### MSOutflow

MSOutflow <- function() {
	MSOutflow_o <- MSRelease()
	return(MSOutflow_o)
}

##### MSRelease

MSRelease <- function() {
	MSRelease_o <- min(MSPrelim() + MSSup(), MSRelLimit())
	return(MSRelease_o)
}

###### MSSup

MSSup <- function() {
	MSSup_o <- 0
	return(MSSup_o)
}

######### MSRelLimit

MSRelLimit <- function() {
	MSRelLimit_o <- MdlSnakeComb() + MSInflow() - MSBotVol
	return(MSRelLimit_o)
}

######## MSInflow

MSInflow <- function() {
	#MSInflow_o <- UpSnOutflow() + USAgReturns() - USAgDiversion() - USToMarket() + USRchIncInflow() - MSEvap()
	MSInflow_o <- MSIn()
	return(MSInflow_o)
}

#####################################
############################################################
############################################################################
### Upper Snake Composite Reservoir

USBotVol <- 201000

#### USPrelim
# Preliminary release based upon rule requirements and minimum releases.
# The release cannot be larger than the available water after withdrawals.  Units cfs-days/m

USPrelim <- function() {
	USPrelim_o <- min(USAvailAfter(), max(USRuleReq(), USMinReq()))
	return(USPrelim_o)
}

## USAvailAfter

USAvailAfter <- function() {
	USAvailAfter_o <- max(0, UpSnakeComb() + USIn() - USBotVol)
	return(USAvailAfter_o)
}

###### UpSnakeComb

UpSnakeComb <- function() {
	if (week_counter == 1) {
		UpSnakeComb_o <- USTopVol()
		# reservoir_vol_df[week_counter,1]=MicaRes
	} else {
		UpSnakeComb_o <- reservoir_vol_df[week_counter - 1, 10]
	}
	return(UpSnakeComb_o)
}

##### USTopVol

USTopVol <- function() {
	USTopVol_o <- USFloodRuleCurve()
	return(USTopVol_o)
}

###### USFloodRuleCurve

USFloodRuleCurve <- function() {
	USFloodRuleCurve_o <- USFloodRuleCurve_input[week_counter_in_year(), 2]
	return(USFloodRuleCurve_o)
}

###### USIn

USIn <- function() {
	USIn_o <- PalisadesFlowData() - USEvap()
	return(USIn_o)
}

##### USInflowEst

USInflowEst <- function() {
	M <- MB_input[week_counter_in_year(), 2]
	B <- MB_input[week_counter_in_year(), 3]
	USInflowEst_o <- MilnerNatFlowData() * M + B
	return(USInflowEst_o)
}

#### USEvap

USEvap <- function() {
	USEvapData <- 0
	USEvap_o <- (USSufaceArea() * USEvapData) * 0.5042 / 12
	return(USEvap_o)
}

###### USSufaceArea

USSufaceArea <- function() {
	USSufaceArea_o <- UpSnakeComb() * 0
	return(USSufaceArea_o)
}

###### USRuleReq

USRuleReq <- function() {
	USRuleReq_o <- max(UpSnakeComb() + USIn() - USTopVol(), 0)
	return(USRuleReq_o)
}

########## USMinReq

USMinReq <- function() {
	USMinReq_o <- max(USMin(), USAgReleaseReq())
	return(USMinReq_o)
}

##### USMin

USMin <- function() {
	USMin_o <- USAvgMin() * cfsTOafw
	return(USMin_o)
}

#### USAvgMin

USAvgMin <- function() {
	USAvgMin_o <- 1000
	return(USAvgMin_o)
}

########## USAgReleaseReq

USAgReleaseReq <- function() {
	USAgReleaseReq_o <- max(0, ((USPreAgCalc()) - USRchIncInflow() + (USMIFReq() * cfsTOafw)))
	return(USAgReleaseReq_o)
}

##### USPreAgCalc

USPreAgCalc <- function() {
	# BaseUSAgWith=BaseUSAg_input[week_counter_in_year(),2]
	# BaseUSAgReturn=BaseUSAg_input[week_counter_in_year(),3]
	USPreAgCalc_o <- 0.9 * (BaseUSAgWith() - BaseUSAgReturn())
	return(USPreAgCalc_o)
}

###### USRchIncInflow

USRchIncInflow <- function() {
	USRchIncInflow_o <- MilnerNatFlowData() - PalisadesFlowData()
	#USRchIncInflow_o <- MilnerNatFlowData()
	return(USRchIncInflow_o)
}

PalisadesFlowData <- function() {
	PalisadesFlowData_o <- VICPAL()
	return(PalisadesFlowData_o)
}

VICPAL <- function() {
	VICPAL_o <- PriVICPAL
	return(VICPAL_o)
}
####### USMIFReq

USMIFReq <- function() {
	USMIFReq_o <- 0 # in the ColSim this variable is a weekly time-series
	return(USMIFReq_o)
}

###### USNetAgRight
USNetAgRight <- function() {
	USNetAgRight_o <- SnakeAgMultiplier * BaseUSAgWith()
	return(USNetAgRight_o)
}

######## BaseUSAgWith

BaseUSAgWith <- function() {
	BaseUSAgWith_o <- BaseUSAg_input[week_counter_in_year(), 2]
	return(BaseUSAgWith_o)
}

####### USPreAgSupply

USPreAgSupply <- function() {
	USPreAgSupply_o <- min(USNetAgRight(), USAgAvailWater())
	return(USPreAgSupply_o)
}

###### USAgAvailWater

USAgAvailWater <- function() {
	USAgAvailWater_o <- max(0, UpSnOutflow() + USRchIncInflow() - USMIFReq() * cfsTOafw)
	return(USAgAvailWater_o)
}

###### UpSnOutflow

UpSnOutflow <- function() {
	UpSnOutflow_o <- UpperSnakeRelease()
	return(UpSnOutflow_o)
}

###### UpperSnakeRelease
# The total release from Jim Woodruff.  Units cfsd/m.

UpperSnakeRelease <- function() {
	UpperSnakeRelease_o <- min(USPrelim() + USSup(), USRelLimit())
	return(UpperSnakeRelease_o)
}

##### USSup

USSup <- function() {
	USSup_o <- MSNetAgRight() * MSAllocationToUs()
	return(USSup_o)
}

####### MSAllocationToUs

MSAllocationToUs <- function() {
	if (MdlSnakeComb() > 0.5 * MSFullPoolVol) {
		MSAllocationToUs_o <- 0
	} else if ((USSharedWater() + MSAvailAfter()) == 0) {
		MSAllocationToUs_o <- 0
	} else {
		MSAllocationToUs_o <- USSharedWater() / (USSharedWater() + MSAvailAfter())
	}
	return(MSAllocationToUs_o)
}

###### USSharedWater
# Generic shared water calculation.  Connect to supplemental release calculations.  Units cfs-days.

USSharedWater <- function() {
	USSharedWater_o <- max(0, UpSnakeComb() + USIn() - USPrelim() - USBotVol)
	return(USSharedWater_o)
}

####### USRelLimit

USRelLimit <- function() {
	USRelLimit_o <- max(UpSnakeComb() + USInflow() - USBotVol, 0)
	return(USRelLimit_o)
}

###### USInflow

USInflow <- function() {
	USInflow_o <- USIn()
	return(USInflow_o)
}

####### USAgReturns

USAgReturns <- function() {
	if (USAgDiversion() > 0) {
		USAD <- 0.9 * BaseUSAgReturn() * max(0, min(1, (USAgDiversion() / BaseUSAgWith())))
	} else {
		USAD <- 0
	}
	USAgReturns_o <- USAD * AgON
	return(USAgReturns_o)
}

########## BaseUSAgReturn

BaseUSAgReturn <- function() {
	BaseUSAgReturn_o <- BaseUSAg_input[week_counter_in_year(), 3]
	return(BaseUSAgReturn_o)
}

####### USAgDiversion

USAgDiversion <- function() {
	USAgDiversion_o <- USAgFinal() * AgON
	return(USAgDiversion_o)
}

#### USAgFinal

USAgFinal <- function() {
	USAgFinal_o <- USPreAgSupply() - USAgSale()
	return(USAgFinal_o)
}

######## USAgSale

USAgSale <- function() {
	USAgSale_o <- USQ() * SnakeTransfer()
	return(USAgSale_o)
}

######## USQ # US%Q

USQ <- function() {
	if (SnakeMarketQ() > 0) {
		USQ_o <- AvailUSMarketQ() / SnakeMarketQ()
	} else {
		USQ_o <- 0
	}
	return(USQ_o)
}

##### USToMarket

USToMarket <- function() {
	USToMarket_o <- USAgSale()
	return(USToMarket_o)
}

##################################
######################################################
##############################################################################
####### Dalles
###### DallesFloodCond

DallesFloodCond <- function() {
	if (DallesRunoffAprAug < 60e6) {
		DallesFloodCond_o <- 1
	} else if (DallesRunoffAprAug < 70e6) {
		DallesFloodCond_o <- 2
	} else if (DallesRunoffAprAug < 80e6) {
		DallesFloodCond_o <- 3
	} else if (DallesRunoffAprAug < 90e6) {
		DallesFloodCond_o <- 4
	} else if (DallesRunoffAprAug < 100e6) {
		DallesFloodCond_o <- 5
	} else if (DallesRunoffAprAug < 110e6) {
		DallesFloodCond_o <- 6
	} else if (DallesRunoffAprAug < 120e6) {
		DallesFloodCond_o <- 7
	} else if (DallesRunoffAprAug < 130e6) {
		DallesFloodCond_o <- 8
	} else if (DallesRunoffAprAug < 140e6) {
		DallesFloodCond_o <- 9
	} else if (DallesRunoffAprAug < 160e6) {
		DallesFloodCond_o <- 10
	} else {
		DallesFloodCond_o <- 11
	}
	return(DallesFloodCond_o)
}

##### DAInc

DAInc <- function() {
	DAInc_o <- DallesFlowData() - JohnDayFlowData()
	return(DAInc_o)
}
####################
##############################################
#########################################################################

### RColSim Main fiunctions

########### MicaOutflow

MicaOutflow <- function() {
	if (ResetStorage() == 1) {
		MicaOutflow_o <- 0
	} else {
		MicaOutflow_o <- MicaRelease_c
	}
	return(MicaOutflow_o)
}

###### RevIn

RevIn <- function() {
	RevIn_o <- MicaOutflow() + (RevFlowData() - MicaFlowData())
	return(RevIn_o)
}

####### RevOut

RevOut <- function() {
	RevOut_o <- RevIn()
	return(RevOut_o)
}

###### ArrowInflow

ArrowInflow <- function() {
	if (ResetStorage() == 1) {
		ArrowInflow_o <- InitAR() - ArrowReservoir()
	} else {
		ArrowInflow_o <- RevOut() + ARInEvap()
	}
	return(ArrowInflow_o)
}

###### ArrowOutflow

ArrowOutflow <- function() {
	if (ResetStorage() == 1) {
		ArrowOutflow_o <- 0
	} else {
		ArrowOutflow_o <- ArrowRelease_c
	}
	return(ArrowOutflow_o)
}

######## DuncanInflow

DuncanInflow <- function() {
	if (ResetStorage() == 1) {
		DuncanInflow_o <- InitDU() - Duncan()
	} else {
		DuncanInflow_o <- DuncanIn()
	}
	return(DuncanInflow_o)
}

######## DuncanOutflow

DuncanOutflow <- function() {
	if (ResetStorage() == 1) {
		DuncanOutflow_o <- 0
	} else {
		DuncanOutflow_o <- DuncanRelease()
	}
	return(DuncanOutflow_o)
}

########## DuncanRelease

DuncanRelease <- function() {
	DuncanRelease_o <- max(DUDamProtectRel(),
	if (OptimizedRelSw == 1) {
		min(DUPrelim(), DURelLimit())
	} else {
		min(DUPrelim() + DUCombSup(), DURelLimit())
	})
	return(DuncanRelease_o)
}

########## LibbyInflow

LibbyInflow <- function() {
	if (ResetStorage() == 1) {
		LibbyInflow_o <- InitLB() - Libby()
	} else {
		LibbyInflow_o <- LBIn()
	}
	return(LibbyInflow_o)
}

######### LibbyOutflow

LibbyOutflow <- function() {
	if (ResetStorage() == 1) {
		LibbyOutflow_o <- 0
	} else {
		LibbyOutflow_o <- LBRelease()
	}
	return(LibbyOutflow_o)
}

###### LBRelease

LBRelease <- function() {
	LBRelease_o <- max(LBDamProtectRel(), 
	if (OptimizedRelSw == 1) {		
		LBRelease_o <- min(LBPrelim(), LBRelLimit())
	} else {
		LBRelease_o <- min(LBPrelim() + LBCombSup() - LBRelReducReq(), LBRelLimit())
	})
	return(LBRelease_o)
}

###### BonnersFerry

BonnersFerry <- function() {
	BonnersFerry_o <- LibbyOutflow() + (BonnersFerryFlowData() - LibbyFlowData()) - BONFDem()
	return(BonnersFerry_o)
}

BONFDem <- function() {
	BONFDem_o <- DemVICBONF
	return(BONFDem_o)
}
#### BonnersFerryFlowData

BonnersFerryFlowData <- function() {
	LBToBF <- 1.423
	if (InflowDataSwitch() == 1) {
		BonnersFerryFlowData_o <- PriVICBONF
	} else {
		BonnersFerryFlowData_o <- ModBonnersFerryFlowData()
	}
	return(BonnersFerryFlowData_o)
}

###### CLInflow

CLInflow <- function() {
	if (ResetStorage() == 1) {
		CLInflow_o <- InitCL() - CorraLinnReservoir()
	} else {
		CLInflow_o <- DuncanOutflow_c + BonnersFerry() + CLInEvap()
	}
	return(CLInflow_o)
}

####### CLRelease

CLRelease <- function() {
	if (OptimizedRelSw == 1) {
		CLRelease_o <- min(CLPrelim(), CLRelLimit())
	} else {
		CLRelease_o <- min(CLPrelim() + CLCombSup(), CLRelLimit())
	}
	return(CLRelease_o)
}

####### CLOutflow

CLOutflow <- function() {
	if (ResetStorage() == 1) {
		CLOutflow <- 0
	} else {
		CLOutflow <- CLRelease_c
	}
	return(CLOutflow)
}

######### CanadaOutflows

Canada_Outflows <- function() {
	CanadaOutflows_o <- ArrowOutflow() + CLOutflow()
	return(CanadaOutflows_o)
}

# Hungry Horse ------------------------------------------------------------
######## HHInflow

HHInflow <- function() {
	if (ResetStorage() == 1) {
		HHInflow_o <- InitHH() - HungryHorse()
	} else {
		HHInflow_o <- HHIn()
	}
	return(HHInflow_o)
}

######### HHRelease

HHRelease <- function() {
	if (OptimizedRelSw == 1) {
		HHRelease_o <- HHPrelim()
	} else {
		HHRelease_o <- min(HHAvailAfter(), HHPrelim() + HHCombSup())
	}
	return(HHRelease_o)
}

####### HHOutflow

HHOutflow <- function() {
	if (ResetStorage() == 1) {
		HHOutflow_o <- 0
	} else {
		HHOutflow_o <- HHRelease_c
	}
	return(HHOutflow_o)
}

######## ColumbiaFalls

ColumbiaFalls <- function() {
	ColumbiaFalls_o <- HHOutflow() + (ColumbiaFallsFlowData() - HungryHFlowData()) - COLDem()
	return(ColumbiaFalls_o)
}

###### KerrInflow

KerrInflow <- function() {
	KerrInflow_o <- ColumbiaFalls() + KEInEvap()
	return(KerrInflow_o)
}

####### KerrRelease

KerrRelease <- function() {
	if (OptimizedRelSw == 1) {
		KerrRelease_o <- min(KEPrelim(), KERelLimit())
	} else {
		KerrRelease_o <- min(KERelLimit(), KEPrelim() + KECombSup())
	}
	return(KerrRelease_o)
}

###### KerrOutflow

KerrOutflow <- function() {
	KerrOutflow_o <- KerrRelease_c
	return(KerrOutflow_o)
}

####### NoxIn

NoxIn <- function() {
	NoxIn_o <- KerrOutflow() + NoxInc()
	return(NoxIn_o)
}

##### NoxInc

NoxInc <- function() {
	NoxInc_o <- NoxonFlowData() - KerrFlowData()
	return(NoxInc_o)
}

##### NoxOut

NoxOut <- function() {
	NoxOut_o <- NoxIn()
	return(NoxOut_o)
}

######## CBIn

CBIn <- function() {
	CBIn_o <- NoxOut() + CBInc()
	return(CBIn_o)
}

######## CBInc

CBInc <- function() {
	CBInc_o <- CabinetFlowData() - NoxonFlowData()
	return(CBInc_o)
}

####### CBOut

CBOut <- function() {
	CBOut_o <- CBIn()
	return(CBOut_o)
}

###### AFInflow

AFInflow <- function() {
	AFInflow_o <- CBOut() + AFInEvap()
	return(AFInflow_o)
}

##### AFRelease

AFRelease <- function() {
	if (OptimizedRelSw == 1) {
		AFRelease_o <- min(AFPrelim(), AFRelLimit())
	} else {
		AFRelease_o <- min(AFPrelim() + AFCombSup(), AFRelLimit())
	}
	return(AFRelease_o)
}

####### AFOutflow

AFOutflow <- function() {
	AFOutflow_o <- AFRelease_c
	return(AFOutflow_o)
}

##### BoundIn

BoundIn <- function() {
	BoundIn_o <- AFOutflow() + BoundInc()
	return(BoundIn_o)
}

###### BoundOut

BoundOut <- function() {
	BoundOut_o <- BoundIn()
	return(BoundOut_o)
}

####### GCInflow

GCInflow <- function() {
	if (ResetStorage() == 1) {
		GCInflow_o <- InitGC() - GrandCoulee()
	} else {
		GCInflow_o <- GCInEvap() + Canada_Outflows() + BoundOut() - GCDem()
	}
	return(GCInflow_o)
}

##### GCRelease

GCRelease <- function() {
	GCRelease_o <- 
	max(GCDamProtectRel(),
    min(max(min(BONFlowDeficit(), GCLimitedStorage()), GCPrelim() + GCCombSup() - TotalRelReducReq()), GCRelLimit())
	)
	return(GCRelease_o)
}

########## GCOutflow

GCOutflow <- function() {
	if (ResetStorage() == 1) {
		GCOutflow_o <- 0
	} else {
		GCOutflow_o <- GCRelease_c
	}
	return(GCOutflow_o)
}

##### CJIn

CJIn <- function() {
	CJIn_o <- GCOutflow() + CJInc() - CJDem()
	return(CJIn_o)
}

##### CJOut

CJOut <- function() {
	CJOut_o <- CJIn()
	return(CJOut_o)
}

### WeIn

WeIn <- function() {
	WeIn_o <- CJOut() + WEInc() - WEDem()
	return(WeIn_o)
}

##### WeOut

WeOut <- function() {
	WeOut_o <- WeIn()
	return(WeOut_o)
}

##### RRIn

RRIn <- function() {
	RRIn_o <- WeOut() + RRInc() - RRDem()
	return(RRIn_o)
}

##### RROut

RROut <- function() {
	RROut_o <- RRIn()
	return(RROut_o)
}

### RIIn # RockIsland

RIIn <- function() {
	RIIn_o <- RROut() + RIInc() - RIDem()
	return(RIIn_o)
}

#### RIOut

RIOut <- function() {
	RIOut_o <- RIIn()
	return(RIOut_o)
}

#### WaIn

WaIn <- function() {
	WaIn_o <- RIOut() + WAInc() - WADem()
	return(WaIn_o)
}

###### WaOut

WaOut <- function() {
	WaOut_o <- WaIn()
	return(WaOut_o)
}

#### PRIn

PRIn <- function() {
	PRIn_o <- WaOut() + PRInc() - PRDem()
	return(PRIn_o)
}

######## PROut

PROut <- function() {
	PROut_o <- PRIn()
	return(PROut_o)
}

# Snake River -------------------------------------------------------------

###### USInflow

USInflow <- function() {
	USInflow_o <- USIn()
	return(USInflow_o)
}

###### UpperSnakeRelease

UpSnOutflow <- function() {
	UpSnOutflow_o <- UpperSnakeRelease()
	return(UpSnOutflow_o)
}

######## BRRelease

BRRelease <- function() {
	if (OptimizedRelSw == 1) {
		BRRelease_o <- min(BRPrelim(), BRRelLimit())
	} else {
		BRRelease_o <- min(BRPrelim() + BRCombSup(), BRRelLimit())
	}
	return(BRRelease_o)
}

###### BROutflow

BROutflow <- function() {
	if (ResetStorage() == 1) {
		BROutflow_o <- 0
	} else {
		BROutflow_o <- BRRelease_c
	}
	return(BROutflow_o)
}

########## OXIn

OXIn <- function() {
	OXIn_o <- BROutflow()
	return(OXIn_o)
}

####### OXOut

OXOut <- function() {
	OXOut_o <- OXIn()
	return(OXOut_o)
}


##### HCIn

HCIn <- function() {
	HCIn_o <- OXOut() + HCInc()
	return(HCIn_o)
}

######## HCOut

HCOut <- function() {
	HCOut_o <- HCIn()
	return(HCOut_o)
}

#### DWInflow

DWInflow <- function() {
	if (ResetStorage() == 1) {
		DWInflow_o <- InitDW() - Dworshak()
	} else {
		DWInflow_o <- DWIn()
	}
	return(DWInflow_o)
}

##### DWRelease

DWRelease <- function() {
	if (OptimizedRelSw == 1) {
		DWRelease_o <- min(DWPrelim(), DWRelLimit())
	} else {
		DWRelease_o <- min(DWPrelim() + DWCombSup(), DWRelLimit())
	}
	return(DWRelease_o)
}

#### DWOutflow

DWOutflow <- function() {
	if (ResetStorage() == 1) {
		DWOutflow_o <- 0
	} else {
		DWOutflow_o <- DWRelease_c
	}
	return(DWOutflow_o)
}

##### LGIn

LGIn <- function() {
	LGIn_o <- DWOutflow() + HCOut() + LGInc() - LGDem()
	return(LGIn_o)
}

###### LGOut

LGOut <- function() {
	#LGOut_o <- LGIn() + MSAgSale() + USAgSale()
	LGOut_o <- LGIn()
	return(LGOut_o)
}

###### LiGIn

LiGIn <- function() {
	LiGIn_o <- LGOut() + LiGInc()
	return(LiGIn_o)
}

###### LiGOut

LiGOut <- function() {
	LiGOut_o <- LiGIn()
	return(LiGOut_o)
}

#### LMIn

LMIn <- function() {
	LMIn_o <- LiGOut() + LMInc()
	return(LMIn_o)
}

##### LMOut

LMOut <- function() {
	LMOut_o <- LMIn()
	return(LMOut_o)
}

##### IHIn

IHIn <- function() {
	IHIn_o <- LMOut() + IHInc() - IHDem()
	return(IHIn_o)
}

####### IHOut

IHOut <- function() {
	IHOut_o <- IHIn()
	return(IHOut_o)
}

# McNarry -----------------------------------------------------------------

##### McNIn

McNIn <- function() {
	McNIn_o <- IHOut() + PROut() + McNInc() - MCNDem()
	return(McNIn_o)
}

###### McNOut

McNOut <- function() {
	McNOut_o <- McNIn()
	return(McNOut_o)
}

##### JDIn

JDIn <- function() {
	JDIn_o <- McNOut() + JDInc() - JDDem()
	return(JDIn_o)
}

##### JDOut

JDOut <- function() {
	JDOut_o <- JDIn()
	return(JDOut_o)
}

####### DaIn

DaIn <- function() {
	DaIn_o <- JDOut() + DAInc() - DADem()
	return(DaIn_o)
}

##### DaOut

DaOut <- function() {
	DaOut_o <- DaIn()
	return(DaOut_o)
}

#### BoIn

BoIn <- function() {
	BoIn_o <- DaOut() + BOInc() - BONDem()
	return(BoIn_o)
}

####### BONOut

BONOut <- function() {
	BONOut_o <- BoIn()
	return(BONOut_o)
}

