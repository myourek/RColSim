
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

                                                              #######################################
                                                              #                                     #
                                                              #                                     #
                                                              #        Performance Measures         #
                                                              #                                     #
                                                              #                                     #
                                                              #######################################
                                                              #######################################
                                                              #                                     #
                                                              #                                     #
                                                              #        Performance Measures         #
                                                              #                                     #
                                                              #                                     #
                                                              #######################################


############################### Instream Flow


############################# Bonneville

Rel_Perc_Bonn_Target<-function(){
	Count_Timestep=N_of_TimeSteps
	if (Count_Timestep==0) {
		Rel_Perc_Bonn_Target_o = 0
	} else {
		Rel_Perc_Bonn_Target_o = (1 - (NumOfLowFlowBonn / Count_Timestep)) * 100
	} #(1-(Months_Bonn_SF/Count_Timestep))*100
	return(Rel_Perc_Bonn_Target_o)
}

############################# Columbia Falls

shortfall_2<-function(){
	if (MOPControl==1) {
		Shortfall_2_o <- max(0, NonFirmEnergyTarget() * (1 - SensitivityFraction) - NonFirmEnergyMarketed())
	} else {
		Shortfall_2_o <- 0
	}
	return(Shortfall_2_o)
}
shortfall_5 <- function() {
	if(MOPControl==1){
		Shortfall_5_o <- max(0, ColFallsTarget() * (1 - SensitivityFraction) - ColFallsFlow_cfs())
	} else { 
		Shortfall_5_o <- 0
	}
	return(Shortfall_5_o)
}
ColFallsFlow_cfs <- function() {
	ColFallsFlow_cfs_o <- ColumbiaFalls() / cfsTOafw
	return(ColFallsFlow_cfs_o)
}

########## Reliability%ColFallsFlow

ReliabilityPercColFallsFlow <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) {
		ReliabilityPercColFallsFlow_o <- 0
	} else {
		ReliabilityPercColFallsFlow_o <- (1- count_Shortfall_5 / Count_Timestep) * 100
	}
	return(ReliabilityPercColFallsFlow_o)
}
VulnerabilityColFallsFlow <- function() {
	if (count_Shortfall_5==0) {
		VulnerabilityColFallsFlow_o <- 0
	} else {
		VulnerabilityColFallsFlow_o <- sum_ShortfallAdd_5 / count_Shortfall_5
	}
	return(VulnerabilityColFallsFlow_o)
}
LowerGraniteFlow_cfs <- function() {
	LowerGraniteFlow_cfs_o <- LGOut() / cfsTOafw
	return(LowerGraniteFlow_cfs_o)
}
shortfall_6 <- function() {
	if (MOPControl==1) {
		Shortfall_6_o <- max(0, LowerGraniteTarget() * (1 - SensitivityFraction) - LowerGraniteFlow_cfs())
	} else { 
		Shortfall_6_o <- 0
	}
	return(Shortfall_6_o)
}
ReliabilityPercLGFlow <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) {
		ReliabilityPercLGFlow_o <- 0
	} else {
		ReliabilityPercLGFlow_o <- (1 - (count_Shortfall_6 / Count_Timestep)) * 100
	}
	return(ReliabilityPercLGFlow_o)
}
VulnerabilityLGFlow <- function() {
	if (count_Shortfall_6==0) {
		VulnerabilityLGFlow_o <- 0
	} else {
		VulnerabilityLGFlow_o <- sum_ShortfallAdd_6 / count_Shortfall_6
	}
	return(VulnerabilityLGFlow_o)
}

################# Ice Harbor 

IHFlowcfs <- function() {
	IHFlowcfs_o <- IHOut() / cfsTOafw
	return(IHFlowcfs_o)
}

######### Shortfall 11

shortfall_11 <- function() {
	IHNavMaxTarget <- 100000
	if (MOPControl==1) {
		Shortfall_11_o <- max(0, IHNavMaxTarget * (1 - SensitivityFraction) - IHFlowcfs())
	} else { 
		Shortfall_11_o <- 0
	}
	return(Shortfall_11_o)
}
ReliabilityPercASnNav <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) {
		ReliabilityPercASnNav_o=0
	} else {
		ReliabilityPercASnNav_o <- ( 1 - (count_Shortfall_11 / Count_Timestep)) * 100
	} 
	return(ReliabilityPercASnNav_o)
}
VulnerabilityASnNav <- function() {
	if(count_Shortfall_11==0) {
		VulnerabilityASnNav_o <- 0
	} else {
		VulnerabilityASnNav_o <- sum_ShortfallAdd_11 / count_Shortfall_11
	}
	return(VulnerabilityASnNav_o)
}
VernitaBarFlow_cfs <- function() {
	VernitaBarFlow_cfs_o <- PROut() / cfsTOafw
	return(VernitaBarFlow_cfs_o)
}
shortfall_7 <- function() {
	if(MOPControl==1) {
		Shortfall_7_o <- max(0, VernitaBarFlowTarget() * (1 - SensitivityFraction) - VernitaBarFlow_cfs())
	} else { 
		Shortfall_7_o <- 0
	}
	return(Shortfall_7_o)
}
ReliabilityPercVernBar <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) { 
		ReliabilityPercVernBar_o <- 0
	} else {
		ReliabilityPercVernBar_o <- (1 - (count_Shortfall_7 / Count_Timestep)) * 100
	}
	return(ReliabilityPercVernBar_o)
}
VulnerabilityVernBar <- function() {
	if(count_Shortfall_11==0) {
		VulnerabilityVernBar_o <- 0
	} else {
		VulnerabilityVernBar_o <- sum_ShortfallAdd_7 / count_Shortfall_7
	}
	return(VulnerabilityVernBar_o)
}

####### McNary

McNaryFlow_cfs <- function() {
	McNaryFlow_cfs_o <- MCNOut() / cfsTOafw
	return(McNaryFlow_cfs_o)
}
shortfall_8<-function(){
	if (MOPControl==1) {
		Shortfall_8_o=max(0, McNaryBaseTarget() * (1 - SensitivityFraction) - McNaryFlow_cfs())
	} else { 
		Shortfall_8_o <- 0
	}
	return(Shortfall_8_o)
}
ReliabilityPercMcNary <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) {
		ReliabilityPercMcNary_o <- 0
	} else {
		ReliabilityPercMcNary_o <- (1 - (count_Shortfall_8 / Count_Timestep)) * 100
	}
	return(ReliabilityPercMcNary_o)
}
VulnerabilityMcNary <- function() {
	if (count_Shortfall_11==0) {
		VulnerabilityMcNary_o <- 0
	} else {
		VulnerabilityMcNary_o <- sum_ShortfallAdd_8 / count_Shortfall_8
	}
	return(VulnerabilityMcNary_o)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############                          Hydropower Measures of Performance
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######### Firm Energy Performance Metrics

MaxSystemEnergy <- function() {
	MaxSystemEnergy_o <- AlbeniFallsGroupEnergy() + DworshakGroupEnergy() + GrandCouleeGroupEnergy()
	+ HungryHorseEnergy() + KerrGroupEnergy() + LibbyEnergy() + LowerColumbiaEnergy() + MicaGroupEnergy()
	return(MaxSystemEnergy_o)
}
AlbeniFallsGroupEnergy <- function() {
	AlbeniFallsGroupEnergy_o <- BDEnergyProduction() + AFEnergyProduction() + BCEnergyProduction()
	return(AlbeniFallsGroupEnergy_o)
}
BDEnergyProduction <- function() {
	BDEnergyProduction_o <- 43560 * (998 * min(BDOut(), BDPenLimit()) * 0.028317 * 9.81 * BDNetHead() * 0.3048 * BDCombEfficiency) / 3.6E9
	return(BDEnergyProduction_o)
}
AFEnergyProduction <- function() { 
	AFEnergyProduction_o <- 43560 * (998 * min(AFRelease(), AFPenLimit()) * 0.028317 * 9.81 * AFNetHead() * 0.3048 * AFCombEfficiency()) / 3.6E9
	return(AFEnergyProduction_o)
}
BCEnergyProduction <- function() {
	BCEnergyProduction_o <- 43560 * (998 * min(BCRelease(), BCPenLimit()) * 0.028317 * 9.81 * BCNetHead() * 0.3048 * BCCombEfficiency) / 3.6E9
	return(BCEnergyProduction_o)
}
BCRelease <- function(){
	BCRelease_o <- AFRelease()
	return(BCRelease_o)
}

##################################################################################

DworshakGroupEnergy <- function() {
	DworshakGroupEnergy_o <- LIGEnergyProduction() + DWEnergyProduction() + LMEnergyProduction() + LGEnergyProduction() + IHEnergyProduction()
	return(DworshakGroupEnergy_o)
}
LIGEnergyProduction <- function() {
	LIGEnergyProduction_o <- 43560 * (998 * min(LIGOut(), LIGPenLimit()) * 0.028317 * 9.81 * LIGNetHead() * 0.3048 * LIGCombEfficiency) / 3.6E9
	return(LIGEnergyProduction_o)
}
DWEnergyProduction <- function() {
	DWEnergyProduction_o <- 43560 * (998 * min(DWRelease(), DWPenLimit()) * 0.028317 * 9.81 * DWNetHead() * 0.3048 * DWCombEfficiency) / 3.6E9
	return(DWEnergyProduction_o)
}
LMEnergyProduction <- function() {
	LMEnergyProduction_o <- 43560 * (998 * min(LMOut(), LMPenLimit()) * 0.028317 * 9.81 * LMNetHead() * 0.3048 * LMCombEfficiency) / 3.6E9
	return(LMEnergyProduction_o)
}
LGEnergyProduction <- function() {
	LGEnergyProduction_o <- 43560 * (998 * min(LGOut(), LGPenLimit()) * 0.028317 * 9.81 * LGNetHead() * 0.3048 * LGCombEfficiency) / 3.6E9
	return(LGEnergyProduction_o)
}
IHEnergyProduction <- function() {
	IHEnergyProduction_o <- 43560 * (998 * min(IHOut(), IHPenLimit()) * 0.028317 * 9.81 * IHNetHead() * 0.3048 * IHCombEfficiency) / 3.6E9
	return(IHEnergyProduction_o)
}
GrandCouleeGroupEnergy <- function() {
	GrandCouleeGroupEnergy_o <- CJEnergyProduction() + GCEnergyProduction() + PREnergyProduction() + RIEnergyProduction()
	+ RREnergyProduction() + WAEnergyProduction() + WEEnergyProduction()
	return(GrandCouleeGroupEnergy_o)
}
CJEnergyProduction <- function() {
	CJEnergyProduction_o <- 43560 * (998 * min(CJOut(), CJPenLimit()) * 0.028317 * 9.81 * CJNetHead() * 0.3048 * CJCombEfficiency) / 3.6E9
	return(CJEnergyProduction_o)
}
GCEnergyProduction <- function() {
	GCEnergyProduction_o <- 43560 * (998 * min(GCRelease(), GCPenLimit()) * 0.028317 * 9.81 * GCNetHead() * 0.3048 * GCCombEfficiency) / 3.6E9
	return(GCEnergyProduction_o)
}
PREnergyProduction <- function() {
	PREnergyProduction_o <- 43560 * (998 * min(PROut(), PRPenLimit()) * 0.028317 * 9.81 * PRNetHead() * 0.3048 * PRCombEfficiency) / 3.6E9
	return(PREnergyProduction_o)
}
RIEnergyProduction <- function() {
	RIEnergyProduction_o <- 43560 * (998 * min(RIOut(), RIPenLimit()) * 0.028317 * 9.81 * RINetHead() * 0.3048 * RICombEfficiency) / 3.6E9
	return(RIEnergyProduction_o)
}
RREnergyProduction <- function() {
	RREnergyProduction_o <- 43560 * (998 * min(RROut(), RRPenLimit()) * 0.028317 * 9.81 * RRNetHead() * 0.3048 * RRCombEfficiency) / 3.6E9
	return(RREnergyProduction_o)
}
WAEnergyProduction <- function() {
	WAEnergyProduction_o <- 43560 * (998 * min(WAOut(), WAPenLimit()) * 0.028317 * 9.81 * WANetHead() * 0.3048 * WACombEfficiency) / 3.6E9
	return(WAEnergyProduction_o)
}
WEEnergyProduction <- function() {
	WEEnergyProduction_o <- 43560 * (998 * min(WEOut(), WEPenLimit()) * 0.028317 * 9.81 * WENetHead() * 0.3048 * WECombEfficiency) / 3.6E9
	return(WEEnergyProduction_o)
}
HungryHorseEnergy <- function() {
	HungryHorseEnergy_o <- 43560 * (998 * min(HHRelease(), HHPenLimit()) * 0.028317 * 9.81 * HHNetHead() * 0.3048 * HHCombEfficiency) / 3.6E9
	return(HungryHorseEnergy_o)
}
KerrGroupEnergy <- function() {
	KerrGroupEnergy_o <- CBEnergyProduction() + KEEnergyProduction() + NoxEnergyProduction()
	return(KerrGroupEnergy_o)
}
CBEnergyProduction <- function() {
	CBEnergyProduction_o <- 43560 * (998 * min(CBOut(), CBPenLimit()) * 0.028317 * 9.81 * CBNetHead() * 0.3048 * CBCombEfficiency) / 3.6E9
	return(CBEnergyProduction_o)
}
KEEnergyProduction <- function() {
	KEEnergyProduction_o <- 43560 * (998 * min(KERelease(), KEPenLimit()) * 0.028317 * 9.81 * KENetHead() * 0.3048 * KECombEfficiency) / 3.6E9
	return(KEEnergyProduction_o)
}
NoxEnergyProduction <- function() {
	NoxEnergyProduction_o <- 43560 * (998*min(NOXOut(), NOXPenLimit()) * 0.028317 * 9.81 * NOXNetHead() * 0.3048 * NOXCombEfficiency) / 3.6E9
	return(NoxEnergyProduction_o)
}
LibbyEnergy <- function() {
	LibbyEnergy_o <- 43560 * (998 * min(LBRelease(), LBPenLimit()) * 0.028317 * 9.81 * LBNetHead() * 0.3048 * LBCombEfficiency) / 3.6E9
	return(LibbyEnergy_o)
}
LowerColumbiaEnergy <- function() {
	LowerColumbiaEnergy_o <- BONEnergyProduction() + DAEnergyProduction() + JDEnergyProduction() + McNaryEnergyProduction()
	return(LowerColumbiaEnergy_o)
}
BONEnergyProduction <- function() {
	BONEnergyProduction_o <- 43560 * (998 * min(BONOut(), BONPenLimit()) * 0.028317 * 9.81 * BONNetHead() * 0.3048 * BONCombEfficiency) / 3.6E9
	return(BONEnergyProduction_o)
}
DAEnergyProduction <- function() {
	DAEnergyProduction_o <- 43560 * (998 * min(DAOut(), DAPenLimit()) * 0.028317 * 9.81 * DANetHead() * 0.3048 * DACombEfficiency) / 3.6E9
	return(DAEnergyProduction_o)
}
JDEnergyProduction <- function() {
	JDEnergyProduction_o <- 43560 * (998 * min(JDOut(), JDPenLimit()) * 0.028317 * 9.81 * JDNetHead() * 0.3048 * JDCombEfficiency) / 3.6E9
	return(JDEnergyProduction_o)
}
McNaryEnergyProduction <- function() {
	McNaryEnergyProduction_o <- 43560 * (998 * min(MCNOut(), MCNPenLimit()) * 0.028317 * 9.81 * MCNetHead() * 0.3048 * MCCombEfficiency) / 3.6E9
	return(McNaryEnergyProduction_o)
}
MicaGroupEnergy <- function() {
	MicaGroupEnergy_o <- MIEnergyProduction() + RevEnergyProduction()
	return(MicaGroupEnergy_o)
}
MIEnergyProduction <- function() {
	MIEnergyProduction_o <- 43560 * (998 * min(MIRelease(), MIPenLimit()) * 0.028317 * 9.81 * MINetHead() * 0.3048 * MICombEfficiency) / 3.6E9
	return(MIEnergyProduction_o)
}
RevEnergyProduction <- function() {
	RevEnergyProduction_o <- 43560 * (998 * min(REVOut(), REVPenLimit()) * 0.028317 * 9.81 * REVNetHead() * 0.3048 * RevCombEfficiency) / 3.6E9
	return(RevEnergyProduction_o)
}

#######################################################################################################
########################################################################################################
################### shortfalls

########### shortfall #Firm Energy

shortfall <- function() {
	if (MOPControl==1) {
		shortfall_o <- max(0, FirmEnergyTarget() * (1 - SensitivityFraction) - MaxSystemEnergy())
	} else {
		shortfall_o <- 0
	}
	return(shortfall_o)
}
ReliabilityPercFirmEng <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) {
		ReliabilityPercFirmEng_o <- 0
	} else {
		ReliabilityPercFirmEng_o <- ((count_Shortfall / Count_Timestep) * 100)
	} #ShortCount
	return(ReliabilityPercFirmEng_o)
}
VulnerabilityFirmEng <- function() {
	if (count_Shortfall==0) {
		VulnerabilityFirmEng_o <- 0
	} else {
		VulnerabilityFirmEng_o <- sum_ShortfallAdd/count_Shortfall
	}#ShortfallSum
	return(VulnerabilityFirmEng_o)
}
NonFirmEnergyMarketed <- function() {
	NonFirmEnergyMarketed_o <- min(NonFirmEnergyTarget(), (MaxSystemEnergy() - FirmEnergyMarketed()))
	return(NonFirmEnergyMarketed_o)
}
FirmEnergyMarketed <- function() {
	FirmEnergyMarketed_o <- min(FirmEnergyTarget(), MaxSystemEnergy())
	return(FirmEnergyMarketed_o)
}
ReliabilityPercNFEng <- function() {
	Count_Timestep <- N_of_TimeSteps
	if(Count_Timestep==0) {
		ReliabilityPercNFEng_o <- 0
	} else {
		ReliabilityPercNFEng_o <- ((count_shortfall_2 / Count_Timestep) * 100)
	} 
	return(ReliabilityPercNFEng_o)
}
VulnerabilityNFEng <- function() {
	if(count_shortfall_2==0) {
		VulnerabilityNFEng_o <- 0
	} else {
		VulnerabilityNFEng_o <- sum_ShortfallAdd_2 / count_shortfall_2
	} #ShortfallSum
	return(VulnerabilityNFEng_o)
}

########### Energy Production Metrics #########################

FirmEnergySales <- function() {
	FirmEnergyPrice <- 35
	FirmEnergySales_o <- FirmEnergyMarketed() * FirmEnergyPrice
	return(FirmEnergySales_o)
}
NonFirmSpotSales <- function() {
	SpotNonFirmPrice <- c(39.7,32.5 ,26.2, 32.6, 33.2, 28.3, 27.2, 25.8, 18.3, 16.8, 21.0, 29.0)
    NonFirmSpotSales_o <- NonFirmEnergyMarketed() * SpotNonFirmPrice[month_in_year]
	return(NonFirmSpotSales_o)
}

###############################################################################################
#######################################################################################################################
################################### flood and storage metrics

########## DallesFlow_cfs #Dalles Flood Protection Metric

DallesFlow_cfs <- function() {
	DallesFlow_cfs_o <- DAOut() / cfsTOafw
	return(DallesFlow_cfs_o)
}
shortfall_10 <- function() {
	if(MOPControl==1)  {
		Shortfall_10_o <- max(0, DallesFlow_cfs() - DAFloodTarget() * (1 + SensitivityFraction)) 
	} else {
		Shortfall_10_o <- 0
	}
	return(Shortfall_10_o)
}

########## Grand Coulee Recreation Metric

shortfall_9 <- function() {
	GCRecStdElev <- 1280 #ft
	if(MOPControl==1) {
		if (month_in_year<12 && month_in_year>3) {
			Shortfall_9_o <- 0
		} else {
			Shortfall_9_o <- max(0, GCRecStdElev - GCElev_ft()) 
		}
    } else {
		Shortfall_9_o <- 0
	}
	return(Shortfall_9_o)
}
TotalSysStorage <- function() {
	TotalSysStorage_o <- ArrowReservoir() + Brownlee() + CorraLinnReservoir() + Duncan() + Dworshak() + GrandCoulee() + HungryHorse() + Libby() + MicaReservoir()
	return(TotalSysStorage_o)
}
SumFloodTarget <- function() {
	SumFloodTarget_o <- ARFloodCurve() + DUFloodCurve() + DWFloodCurve() + GCFloodCurve() + HHFloodCurve()
	+ LibbyFloodCurve() + MIFloodCurve() + BRFloodVolume() + CLRuleVol()
	return(SumFloodTarget_o)
}
BelowFCC <- function() {
	if (TotalSysStorage() < SumFloodTarget() * (1 - SensitivityFraction)) {
		BelowFCC_o <- SumFloodTarget() - TotalSysStorage()
	} else {
		BelowFCC_o <- 0
	}
	return(BelowFCC_o)
}

##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

                                                        #######################################
                                                        #                                     #
                                                        #                                     #
                                                        #        Performance Measures         #
                                                        #                                     #
                                                        #                                     #
                                                        #######################################


############################### Instream Flow

Rel_Perc_Bonn_Target <- function() {
	Count_Timestep <- N_of_TimeSteps
	if (Count_Timestep==0) {
		Rel_Perc_Bonn_Target_o <- 0
	} else {
		Rel_Perc_Bonn_Target_o <- (1 - (NumOfLowFlowBonn / Count_Timestep)) * 100} #(1-(Months_Bonn_SF/Count_Timestep))*100
	return(Rel_Perc_Bonn_Target_o)
}

######################## count number of timesteps with low streamflow

ChumSF <- function() {
	if(BONTarget_AcFt() == 0) {
		ChumSF_o <- 0
	} else if((BONTarget_AcFt() - BONOut()) / BONTarget_AcFt() > 0.05) {
		ChumSF_o <- max(BONTarget_AcFt() - BONOut(), 0)
	} else {
		ChumSF_o <- 0
	}
	return(ChumSF_o)
}
shortfall_5 <- function() {
	if(MOPControl==1) {
		Shortfall_5_o=max(0, ColFallsTarget()*(1-SensitivityFraction)-ColFallsFlow_cfs())
	} else { 
		Shortfall_5_o <- 0
	}
	return(Shortfall_5_o)
}
ColFallsFlow_cfs <- function() {
	ColFallsFlow_cfs_o <- ColumbiaFalls() / cfsTOafw
	return(ColFallsFlow_cfs_o)
}
ReliabilityPercColFallsFlow <- function() {
	Count_Timestep <- N_of_TimeSteps
	if(Count_Timestep==0) { 
		ReliabilityPercColFallsFlow_o <- 0
	} else {
		ReliabilityPercColFallsFlow_o <- (1 - (count_Shortfall_5 / Count_Timestep)) * 100} #ShortCount_5=
	return(ReliabilityPercColFallsFlow_o)
}
VulnerabilityColFallsFlow<-function(){
	if (count_Shortfall_5==0) {
		VulnerabilityColFallsFlow_o <- 0
	} else {
		VulnerabilityColFallsFlow_o <- sum_ShortfallAdd_5 / count_Shortfall_5
	}
	return(VulnerabilityColFallsFlow_o)
}

# NumOfLowFlowBonn<<-0
# count_Shortfall_5<<-0
# sum_ShortfallAdd_5<<-0
# count_Shortfall_6<<-0
# sum_ShortfallAdd_6<<-0
# count_Shortfall_7<<-0
# sum_ShortfallAdd_7<<-0
# count_Shortfall_8<<-0
# sum_ShortfallAdd_8<<-0
# count_Shortfall_11<<-0
# sum_ShortfallAdd_11<<-0



