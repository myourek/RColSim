
ReadSwitches<<-function(){
######## Simulate curtailment. Options: 1: curtailment on. 0: curtailment off.
simulate_curtailment <<- 1
# MIMinRelForDust is set to zero in the current version of ColSim
##### InitialConditionSwitch
#Options: 0:  Use values imported from spreadsheet. 1:  Use fixed multiplier for whole basin (fraction of full pool)
#2:  Use historic storage value at update time step
InitialConditionSwitch <<- 2
unit_convert_switch <<- 1
Observe_Draft_Limits <<- 1
Use_Storage_for_Targets <<- 0
UseInflowMult <<- 1

##############  UpperColMult
#Fow multipliers:
# This forces expected changes in the hydrology to occur.  The fourareas are the Upper Columbia, the Snake, and the Lower Columbia.
# Below is a list of the VIC produced inflows associated with each category.
# Upper Columbia Inflows:  Mica, Revelstoke, Arrow, Duncan, Libby, Corra Linn (a.k.a. BC Hydro)

####### Approximated flows
Use4trDegreeInQAppx <<- 0 #This switch activates approximations of inflow for VIC areas which are too small to simulate in 1/4 degree spatial cells.
#The "off" position" infers that there are 1/8th degree spatial resolutions available from the VIC data.
#The basins which are not recognized in 1/4th degree resoultions are approximated as follows.
#Revelstoke to Arrow          1.560
#Corra Linn to Duncan        0.134
#Corra Linn to Libby            0.409
#Waneta to Hungry Horse  0.103
#Waneta to Kerr                 0.298
#Waneta to Columbia Falls 0.360


########## MOPControl
#This control variable is used to track measures of performance only for specific climate conditions.
#For example one could track measures of performance only for ENSODefinition<<-1 (El Nino).
#If this variable <<-1 in the timestep, then measures of performance are recorded, otherwise they are ignored.
#The definitions for the climate parameters remain constant for each water year.

#This variable can be overridden using the slider, and care must be exercised to ensure that the slider is set to "equation on" when intending to use
#the logical expressions in MOPControl.

MOPControl <<- 1

######### SensitivityFraction
#Energy shortfall based on SEPA weekly contracts.  Units MWhr.

#IF MOPControl<<-1 THEN MAX(0,ColFallsFlowTarget*(1-SensitivityFraction)-ColFallsFlow_cfs)
#ELSE 0

SensitivityFraction <<- 0.001

######### CORToDU
#
CORToDU <<- 0.134
######## MidColMult
MidColMult <<- function() { # check to see if the values are changing during a year
	return(1)
}
UpperColMult <<- function() {
	UpperColMult_o = 1 #week_counter_in_year()*UseInflowMult
	return(UpperColMult_o)
}
######### SnakeMult
#This forces expected changes in the hydrology to occur.  The four areas are the Upper Columbia, the Snake, and the Lower Columbia.
#Below is a list of the VIC produced inflows associated with each category.
#Snake: Dworshak, Oxbow, Ice Harbor
#7.17.01
SnakeMult <<- function() { # check # weekly time-series of one
	SnakeMult_o = 1
	return(SnakeMult_o)
}
week_counter_in_year <<- function() {
	if (week_counter %% 52==0){week_counter_in_year_o= 52} else {week_counter_in_year_o=week_counter %% 52}
	return(week_counter_in_year_o)
}

######## find which operation year we are in, from weekly data

year_from_weekly <<- function() {
	year_from_weekly_o = week_counter %/% 52 + 1  
	return(year_from_weekly_o)
}

##### MarketHydrgy
MarketHydrgy <<- 0

###### RefillMinSw
# Switch to select alternate minimum release for generating inflow estimates at Grand Coulee and Arrow.
#These flow estimates are used to compute refill curves at Grand Coulee and Arrow.  Options:
# 0--Normal minimum requirements
# 1--Special minimum requirements for estimating inflows for refill curves.

RefillMinSw <<- function(){
	if(Normal_minQ==1) {RefillMinSw_o=0} else if (Developed_minQ==1) {RefillMinSw_o=1}
	return (RefillMinSw_o)
}

###### ResetStorage

ResetStorage <<- function() {
	if(ResetStorageSwitch()==1 && month==ResetStorageMonth()) {
		ResetStorage_o = 1
	} else if (ResetStorageSwitch() == 2 && TimeserStorReset() ==1) {
		ResetStorage_o = 1
	} else {ResetStorage_o=0}
		return(ResetStorage_o)
}

####### ResetStorageSwitch
#This switch controls a routine that resets the storage at particular dams to preset storage values stored in the INIT<dam name> icons.
#The month that the storage is reset is specified in the icon ResetStorageMonth.
#Options:
#  0--do not reset storage
#1--reset the storage to the initial value at the end of the month specified
#2- reset the storage according to a timeseries

ResetStorageSwitch <<- function() {
	ResetStorageSwitch_o = 0
	return(ResetStorageSwitch_o)
}

####### ResetStorageMonth
#When the ResetStorageSwitch is set to 1, this icon determines the month for which the storage is reset to a preset initial value.
#If this icon is set to 6, for example, the storage at the end of January  will be the value set in the INIT<dam name> icon.

ResetStorageMonth <<- function() {
	ResetStorageMonth_o = 12
	return(ResetStorageMonth_o)
}

####### TimeserStorReset   #check
#this is a time-series of values (mostly zero), which has not been implemented in this version

TimeserStorReset = function() {
	TimeserStorReset_o = 0
	return(TimeserStorReset_o)
}

###### UseRegFlowAtBR
#Options:
#0--Inflow to Brownlee is modeled explicitly from natural flow input at Milner.  Use this setting when using VIC data to drive the model.
#1--Inflow to Brownlee is regulated flow at the 1989 level of development from Id Dept of Water Resources model.
#Use this setting when making runs using observed flow data and explicit effects in the Snake do not need to be simulated.

UseRegFlowAtBR <<- function() {
	UseRegFlowAtBR_o = 0
	return(UseRegFlowAtBR_o)
}

GlobalFloodEvacMult <<- 1
AgON <<- 1
SnakeAgMultiplier <<- 1
PreMarketSF_preprocessed <<- 5
Market_OPEN <<- 0
Market_CLOSED <<- 1
CDFSnke <<- 0
NonFirmEnergySw <<- 1

UseTotalEnergyContentForFirm <<- function() {
	#Options:
	#0--Allocate based on  ECC for Firm Energy.  Reservoir may release only to ECC after allocation.
	#1-Allocate based on volume mid point between ECC and bottom of pool for Firm Energy.  Reservoir may release to bottom of pool after allocation.
	if (Observe_ECC_Rules==1) {
		UseTotalEnergyContentForFirm_o = 0
	} else if (Use_Storage_for_Firm==1) {
		UseTotalEnergyContentForFirm_o = 1
	} else {
		print(paste("these values are wrong make sure the default values make sense"))
	}
	return(UseTotalEnergyContentForFirm_o)
}

Observe_ECC_Rules <<- 0
Use_Storage_for_Firm <<- 1
Estimated_Efficiency <<- 0.8 #Estimated combined efficiency for all plants.  This efficiency is used for estimating energy content only.
MWhr_per_ftAcFt <<- 1.02246e-3 #Conversion factor to convert head times volume (ft-acre-ft) to energy (MW-hr).

########### UseUpdatedOpSystem
#Options:
#0--Use critical curve, assured refill curve, and flood curve to construct ECC
#1--Use refill to least flood (with or without forecast) and flood curve to construct ECC
UseUpdatedOpSystem <<- 0
###### RefillSwitch

SQuo_Refill_Targ <<- 1
AltForecast_Refill_Targ <<- 0
PfctForecast_Refill_Targ <<- 0

RefillSwitch<<-function(){
	#This switch selects refill rule curves that are based on status quo operations (forecasts only affect Jan-July)
	# or refill rule curves based on higher quality forecasts that specify refill lower values Aug-July.  Settings are:
	#  1-Status Quo Operations
	#  2-All year perfect  refill forecast
	#  3-All year alternate refill forecast
	if (SQuo_Refill_Targ==1) {
		RefillSwitch_o = 1
	} else if (PfctForecast_Refill_Targ==1) {
		RefillSwitch_o = 2
	} else if (AltForecast_Refill_Targ==1) {
		RefillSwitch_o = 3
	} else {
		print(paste("ERROR: the default values might be incorrect"))
	}
	return(RefillSwitch_o)
}

UseForecastRLFCurve <<- 1
GCEngContMult <<- 0

########## EnergyAllocSafeFactor
#Due to penstock constraints, the energy allocation algorithm may occasionally request water from an upstream dam that is already spilling water.
#In this case the system wide energy target may be missed by a small amount since no extra energy will be produced at the upstream dam itself.
#This control increases the energy target by a small amount to ensure that the system wide targets are met despite this allocation decision.
#Note that supplemental energy releases are modeled  to pass through the entire system, so spill from upstream dams still has substantial energy content.

EnergyAllocSafeFactor <<- 1.01

######## UseAlternateNonFirmTarget
#Options: 0:  Use status quo energy target and fraction
#1:  Use alternative forecast for non-firm energy that uses climate forecast multipliers for the period from August-January.

UseAlternateNonFirmTarget <<- 0
Deviation__From_Normal_Curve <<- 1
HHCombEfficiency <<- 0.8
LBCombEfficiency <<- 0.8
MICombEfficiency <<- 0.8
RevCombEfficiency <<- 0.8
CBCombEfficiency <<- 0.8
KECombEfficiency <<- 0.8
NOXCombEfficiency <<- 0.8
BCCombEfficiency <<- 0.8
BDCombEfficiency <<- 0.8
CJCombEfficiency <<- 0.8
GCCombEfficiency <<- 0.8
PRCombEfficiency <<- 0.8
RICombEfficiency <<- 0.8
RRCombEfficiency <<- 0.8
WACombEfficiency <<- 0.8
WECombEfficiency <<- 0.8
DWCombEfficiency <<- 0.8
IHCombEfficiency <<- 0.8
LGCombEfficiency <<- 0.8
LIGCombEfficiency <<- 0.8
LMCombEfficiency <<- 0.8
BONCombEfficiency <<- 0.8
DACombEfficiency <<- 0.8
JDCombEfficiency <<- 0.8
MCCombEfficiency <<- 0.8
ARCombEfficiency <<- 0.8
DUCombEfficiency <<- 0.8
# UseAlternateNonFirmTarget ------------------------------------------------------------------------
DUCombEfficiency_00 <<- -999
} ### end of all switches
