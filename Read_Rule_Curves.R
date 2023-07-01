input_dir = "/data/hydro/users/Forecast_2026/ForecastProject/Forecast_data_files/RColSim/default_rule_curves/"
Read_Rule_Curves <- function() {
	###### initial ##########
	HistStor <<- read.table(paste(input_dir, "HistStor.txt", sep=""), header=T, nrows=367)
	DUFlood_input <<- read.table(paste(input_dir, "DUECCandFC/DUFlood_new.txt", sep=""), header=T)
	DU_JAN_FAMAJ <<- read.table(paste(input_dir, "DUECCandFC/DU-JAN-FAMAJ.txt", sep=""), header=T)
	ARFlood <<- read.table(paste(input_dir, "ARECCandFC/ARFlood_new.txt", sep=""), header=T)
	BRCurFC_Cont <<- read.table(paste(input_dir, "/BRL/BRCurFC_Cont.txt", sep=""), header=T)
	BRForecastFloodStorage <<- read.table(paste(input_dir, "/BRL/BRForecastFloodStorage.txt", sep=""), header=T)
  
	###### Dworshak Dam ######
	DWHistStor_input <<- read.table(paste(input_dir, "/DW/DWHistStor.txt", sep=""), header=T)
	DWFlood_input <<- read.table(paste(input_dir, "/DW/DWFlood_new.txt", sep=""), header=T)
	DW_FloodM <<- read.table(paste(input_dir, "/DW/DWFlood_Month.txt", sep=""), header=T)
	DWRefillMin_input <<- read.table(paste(input_dir, "/DW/DWRefillMin.txt", sep=""), header=T)
	DWAvgMin_input <<- read.table(paste(input_dir, "/DW/DWAvgMin.txt", sep=""), header=T)
	DWCriticalCurve_input <<- read.table(paste(input_dir, "/DW/DWCriticalCurve_new.txt", sep=""), header=T)
	DW1931Refill_input <<- read.table(paste(input_dir, "/DW/DW1931Refill.txt", sep=""), header=T)
	DWBiOpDraftLimit_input <<- read.table(paste(input_dir, "/DW/DWBiOpDraftLimit.txt", sep=""), header=T)
  
	##### Low Granite Dam ######
	LowerGraniteTarget_input <<- read.table(paste(input_dir, "/LG/LowerGraniteTarget.txt", sep=""), header=T)
  
	###### US-CR ############
	USFloodRuleCurve_input <<- read.table(paste(input_dir, "/USCR/USFloodRuleCurve.txt", sep=""), header=T)
  
	#### MS-CR ###########
	MSFloodRuleCurve_input <<- read.table(paste(input_dir, "/MSCR/MSFloodRuleCurve.txt", sep=""), header=T)

	#### MSAg #############
	B064X_input <<- read.table(paste(input_dir, "/MSAg/B065-4567.txt", sep=""), header=T)
	FCIrQSnke_input <<- read.table(paste(input_dir, "/MSAg/FCIrQSnke.txt", sep=""), header=T)
	QSnke_input <<- read.table(paste(input_dir, "/MSAg/QSnke.txt", sep=""), header=T)
	BaseMSAgReturn_input <<- read.table(paste(input_dir, "/BRL/BaseMSAgReturn.txt", sep=""), header=T)
	BaseMSAgWith_input <<- read.table(paste(input_dir, "/MSAg/MSNetAgRight.txt", sep=""), header=T)
  
	#### McNary Dam #############
	MNB_input <<- read.table(paste(input_dir, "/McNary/MNBaseTarget.txt", sep=""), header=T)
	ModMcNaryFlowData_input <<- read.table(paste(input_dir, "/McNary/ModMcNaryFlowData.txt", sep=""), header=T)
  
	#### Brownlee #######
	BRHistStor_input <<- read.table(paste(input_dir, "/BRL/BRHistStor.txt", sep=""), header=T)
	BRFlood <<- read.table(paste(input_dir, "/BRL/BRFlood_1998.txt", sep=""), header=T)
	AddSp_input <<- read.table(paste(input_dir, "/BRL/Add_Space.txt", sep=""), header=T)

	####### Libby ########
	LBHistStor_input <<- read.table(paste(input_dir, "/Libby/LBHistStor.txt", sep=""), header=T)
	LBFlood_input <<- read.table(paste(input_dir, "/Libby/LBFlood_1998.txt", sep=""), header=T)
	MFlood_input <<- read.table(paste(input_dir, "/Libby/MFlood.txt", sep=""), header=T)
	#LBF_input <<- read.table(paste(input_dir, "/Libby/LBFlood_2.txt", sep=""), header=T)
	#APR_LB_input <<- read.table(paste(input_dir, "/Libby/APRLB.txt", sep=""), header=T)
	LBCriticalCurve_input <<- read.table(paste(input_dir, "/Libby/LBCriticalCurve_new.txt", sep=""), header=T)
	LB1931Refill_input <<- read.table(paste(input_dir, "/Libby/LB1931Refill.txt", sep=""), header=T)
	LBMaxFCRel_input <<- read.table(paste(input_dir, "/Libby/LBMaxFCRel.txt", sep=""), header=T)

	##### Corra Linn ###########
	CLHistStor_input <<- read.table(paste(input_dir, "/CL/CLHistStor.txt", sep=""), header=T)
	CLIJCRuleCurve_input <<- read.table(paste(input_dir, "/CL/CLIJCRuleCurve_new.txt", sep=""), header=T)
  
	##### Columbia Falls ######
	ColFall_Target <<- read.table(paste(input_dir, "/HH/ColFalls_Target.txt", sep=""), header=T) 
  
	##### Hungary Horse ########
	HHCriticalCurve_input <<- read.table(paste(input_dir, "/HH/HHCriticalCurve_new.txt", sep=""), header=T)
	HHBaseDraftLimit_input <<- read.table(paste(input_dir, "/HH/HHBaseDraftLimit_new.txt", sep=""), header=T)
	HHAssuredRefill_input <<- read.table(paste(input_dir, "/HH/HHAssuredRefill_new.txt", sep=""), header=T)
	HHStor <<- read.table(paste(input_dir, "/HH/HHHistStor.txt", sep=""), header=T)
	HHFlood <<- read.table(paste(input_dir, "/HH/HHFlood_1998.txt", sep=""), header=T)
	#JanToAprFlood <<- read.table(paste(input_dir, "/HH/HHFlood_JanApr.txt", sep=""), header=T)
	#APR_HH_input <<- read.table(paste(input_dir, "/HH/AprHH.txt", sep=""), header=T)
  
	### Kerr #####
	KerrFloodC <<- read.table(paste(input_dir, "/Kerr/FloodCurve_new.txt", sep=""), header=T)
	Article_56_input <<- read.table(paste(input_dir, "/Kerr/article_56_new.txt", sep=""), header=T)
  
	#### Bonneville ######
	Chum_variable_2_input <<- read.table(paste(input_dir, "/BONN/Chum_variable_2.txt", sep=""), header=T)

	##### PreEnergy ########3
	VernitaBarFlowTarget_input <<- read.table(paste(input_dir, "/GC/VernitaBarFlowTarget.txt", sep=""), header=T)
  
	### AlbeniFalls ######
	AFAvgMin_input <<- read.table(paste(input_dir, "/AFECCandFC/AFAvgMin.txt", sep=""), header=T)
	AFFlood <<- read.table(paste(input_dir, "/AFECCandFC/AFFC_new.txt", sep=""), header=T)
	AF_elev_input <<- read.table(paste(input_dir, "/AFECCandFC/AF_storage_elevation.txt", sep=""), header=T)
  
	### Grand Coulee #######
	GCFlood1_input <<- read.table(paste(input_dir, "GC/GCFlood_2015.txt", sep=""), header=T)
	#GCF_Month_input <<- read.table(paste(input_dir, "GC/GCFlood_Month.txt", sep=""), header=T)
	GCRefillMin_input <<- read.table(paste(input_dir, "GC/GCRefillMin.txt", sep=""), header=T)
	GCAvgMin_input <<- read.table(paste(input_dir, "GC/GCAvgMin.txt", sep=""), header=T)
	GCCriticalCurve_input <<- read.table(paste(input_dir, "GC/GCCriticalCurve_new.txt", sep=""), header=T)
	GCRecLimit_input <<- read.table(paste(input_dir, "GC/GCRecLimit.txt", sep=""), header=T)
	PreGCDraftLimit_input <<- read.table(paste(input_dir, "GC/PreGCDraftLimit.txt", sep=""), header=T)
	#GCFlood2 <<- read.table(paste(input_dir, "GC/GCFlood2.txt", sep=""), header=T)
	#GCF_month <<- read.table(paste(input_dir, "GC/GCFlood_Month.txt", sep=""), header=T)
	GCAbsMinQ_input <<- read.table(paste(input_dir, "GC/GCAbsMinQ.txt", sep=""), header=T)
	GCBdgtForVB_input <<- read.table(paste(input_dir, "GC/GCBdgtForVB.txt", sep=""), header=T)
  
	##### Duncan ##########
	DUCriticalCurve_input <<- read.table(paste(input_dir, "DUECCandFC/DUCriticalCurve_new.txt", sep=""), header=T)
	DU1931Refill_input <<- read.table(paste(input_dir, "DUECCandFC/DU1931Refill.txt", sep=""), header=T)
	DURefillMin_input <<- read.table(paste(input_dir, "DUECCandFC/DURefillMin.txt", sep=""), header=T)
	DUFlood_input <<- read.table(paste(input_dir, "DUECCandFC/DUFlood_new.txt", sep=""), header=T)
	#FMAMJ_DU_input <<- read.table(paste(input_dir, "DUECCandFC/FMAMJ_DU.txt", sep=""), header=T)

	###### Arrow ##########
	#ARFloodMonth <<- read.table(paste(input_dir, "ARECCandFC/ARFloodMonth.txt", sep=""), header=T)
	#ARFlood_May5_i <<- read.table(paste(input_dir, "ARECCandFC/ARFlood_May5.txt", sep=""), header=T)
	#ARFlood_1May_input <<- read.table(paste(input_dir, "ARECCandFC/ARFlood_1May.txt", sep=""), header=T)
	ARFlood_input <<- read.table(paste(input_dir, "ARECCandFC/ARFlood_new.txt", sep=""), header=T)
	ARCriticalCurve_input <<- read.table(paste(input_dir, "ARECCandFC/ARCriticalCurve_new.txt", sep=""), header=T)
	AR1931Refill_input <<- read.table(paste(input_dir, "ARECCandFC/AR1931Refill.txt", sep=""), header=T)
	ARHistStor_i <<- read.table(paste(input_dir, "ARECCandFC/ARHistStor.txt", sep=""), header=T)
	ARAssuredRelease_input <<- read.table(paste(input_dir, "ARECCandFC/AssuredRelease.txt", sep=""), header=T)
  
	#### Mica ###########
	MI1931Refill_input <<- read.table(paste(input_dir, "MicaECCandFC/MI1931Refill.txt", sep=""), header=T)
	GlobalFloodEvacMult_i <<- read.table(paste(input_dir, "GlobalFloodEvacMult.txt", sep=""), header=T)
	MIFlood_712 <<- read.table(paste(input_dir, "MicaECCandFC/MIFlood7-12_new.txt", sep=""), header=T)
	#MIFlood_DecToApr <<- read.table(paste(input_dir, "MicaECCandFC/MIFlood-DecToApr.txt", sep=""), header=T)
	#APR_MI_input <<- read.table(paste(input_dir, "MicaECCandFC/APR_MI.txt", sep=""), header=T)
	MICriticalCurve_input <<- read.table(paste(input_dir, "MicaECCandFC/MICriticalCurve_input.txt", sep=""), header=T)
	MIAssuredReleasedMin_i <<- read.table(paste(input_dir, "MIAssuredReleasedMin.txt", sep=""), header=T)
	MIMaxFloodOutflow_input <<- read.table(paste(input_dir, "MicaECCandFC/MIMaxFloodOutflow.txt", sep=""), header=T)
  
	##### Energy #######
	NonFirmFraction_input <<- read.table(paste(input_dir, "Energy/NonFirmFraction.txt", sep=""), header=T)
	AltNonFirmLoad_input <<- read.table(paste(input_dir, "Energy/AltNonFirmLoad.txt", sep=""), header=T)
	ColFallsMaxFlow_input <<- read.table(paste(input_dir, "Energy/ColFallsMaxFlow.txt", sep=""), header=T)
	HH_USBRmax_input <<- read.table(paste(input_dir, "Energy/HH_USBRmax.txt", sep=""), header=T)
	FirmFraction_input <<- read.table(paste(input_dir, "FirmFraction.txt", sep=""), header=T)
  
	######### Dalles #####
	DALowFloodTarget_input <<- read.table(paste(input_dir, "Dall/DALowFloodTarget.txt", sep=""), header=T)
	DAHighFloodTarget_input <<- read.table(paste(input_dir, "Dall/DAHighFloodTarget.txt", sep=""), header=T)

	######### Bonneville ########
	BONNetHead_input <<- read.table(paste(input_dir, "BONN/BONNetHead.txt", sep=""), header=T)
  
	######### CRT Scenarios ###########
	ARCRT1_storage <<- read.table(paste(input_dir, "CRT/ARCRT1_storage_weekly.txt", sep=""), header=T)
	ARCRT2_storage <<- read.table(paste(input_dir, "CRT/ARCRT2_storage_weekly.txt", sep=""), header=T)
	ARCRT1_release <<- read.table(paste(input_dir, "CRT/ARCRT1_release_weekly.txt", sep=""), header=T)
	ARCRT2_release <<- read.table(paste(input_dir, "CRT/ARCRT2_release_weekly.txt", sep=""), header=T)
	MICRT2_storage <<- read.table(paste(input_dir, "CRT/MICRT2_storage_weekly.txt", sep=""), header=T)
}
