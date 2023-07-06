input_dir = "~/RColSim/default_rule_curves/"
Read_Rule_Curves <- function() {
	###### initial ##########
	HistStor <<- read.table(paste0(input_dir, "HistStor.txt"), header=T, nrows=367)
	lower_limit_input <<- read.table(paste0(input_dir, "rule_curve_lower_limits.txt"), header=T)
	
	###### Dworshak Dam ######
	DWFlood_input <<- read.table(paste0(input_dir, "DW/DWFlood_new.txt"), header=T)
	DWCriticalCurve_input <<- read.table(paste0(input_dir, "DW/DWCriticalCurve_new.txt"), header=T)
	DWAssuredRefill_input <<- read.table(paste0(input_dir, "DW/DW1931Refill_new.txt"), header=T)
	DWBiOpDraftLimit_input <<- read.table(paste0(input_dir, "DW/DWBiOpDraftLimit.txt"), header=T)
	DW_elev_input <<- read.table(paste0(input_dir, "DW/DW_storage_elevation.txt"), header=T)

	###### US-CR ############
	USFlood_input <<- read.table(paste0(input_dir, "USCR/USFloodRuleCurve_new.txt"), header=T)

	#### Brownlee #######
	BRFlood_input <<- read.table(paste0(input_dir, "BRL/BRFlood_1998.txt"), header=T)
	BRCriticalCurve_input <<- read.table(paste0(input_dir, "BRL/BRCriticalCurve_new.txt"), header=T)
	BRAssuredRefill_input <<- read.table(paste0(input_dir, "BRL/BR1931Refill.txt"), header=T)	
	BR_elev_input <<- read.table(paste0(input_dir, "BRL/BR_storage_elevation.txt"), header=T)

	####### Libby ########
	LBFlood_input <<- read.table(paste0(input_dir, "Libby/LBFlood_1998.txt"), header=T)
	LBCriticalCurve_input <<- read.table(paste0(input_dir, "Libby/LBCriticalCurve_new.txt"), header=T)
	LBAssuredRefill_input <<- read.table(paste0(input_dir, "Libby/LB1931Refill.txt"), header=T)
	LibbyBiOpDraftLimit_input <<- read.table(paste0(input_dir, "Libby/LibbyBiOpDraftLimit.txt"), header=T)
	LB_elev_input <<- read.table(paste0(input_dir, "Libby/LB_storage_elevation.txt"), header=T)

	##### Corra Linn ###########
	CLIJCRuleCurve_input <<- read.table(paste0(input_dir, "CL/CLIJCRuleCurve_new.txt"), header=T)
	CLCriticalCurve_input <<- read.table(paste0(input_dir, "CL/CLCriticalCurve_new.txt"), header=T)
	CLAssuredRefill_input <<- read.table(paste0(input_dir, "CL/CL1931Refill.txt"), header=T)
  	CL_elev_input <<- read.table(paste0(input_dir, "CL/CL_storage_elevation.txt"), header=T)

	##### Hungary Horse ########
	HHFlood_input <<- read.table(paste0(input_dir, "HH/HHFlood_1998.txt"), header=T)
	HHCriticalCurve_input <<- read.table(paste0(input_dir, "HH/HHCriticalCurve_new.txt"), header=T)
	HHAssuredRefill_input <<- read.table(paste0(input_dir, "HH/HH1931Refill_new.txt"), header=T)
	HHBiOpDraftLimit_input <<- read.table(paste0(input_dir, "HH/HHBiOpDraftLimit_new.txt"), header=T)
	HH_elev_input <<- read.table(paste0(input_dir, "HH/HH_storage_elevation.txt"), header=T)
	
	### Kerr #####
	KEFlood_input <<- read.table(paste0(input_dir, "Kerr/FloodCurve_new.txt"), header=T)
	KECriticalCurve_input <<- read.table(paste0(input_dir, "Kerr/KECriticalCurve.txt"), header=T)
	KEAssuredRefill_input <<- read.table(paste0(input_dir, "Kerr/KE1931Refill.txt"), header=T)
	KE_elev_input <<- read.table(paste0(input_dir, "Kerr/KE_storage_elevation.txt"), header=T)
  
	### AlbeniFalls ######
	AFFlood_input <<- read.table(paste0(input_dir, "AFECCandFC/AFFC_new.txt"), header=T)
	AFCriticalCurve_input <<- read.table(paste0(input_dir, "AFECCandFC/AF_critical_curve.txt"), header=T)
	AFAssuredRefill_input <<- read.table(paste0(input_dir, "AFECCandFC/AF1931Refill.txt"), header=T)	
	AF_elev_input <<- read.table(paste0(input_dir, "AFECCandFC/AF_storage_elevation.txt"), header=T)
  
	### Grand Coulee #######
	GCFlood_input <<- read.table(paste0(input_dir, "GC/GCFlood_2015.txt"), header=T)
	GCCriticalCurve_input <<- read.table(paste0(input_dir, "GC/GCCriticalCurve_new.txt"), header=T)
	GCAssuredRefill_input <<- read.table(paste0(input_dir, "GC/GC1931Refill.txt"), header=T)
	GC_VDLL_input <<- read.table(paste0(input_dir, "GC/GC_VDLL.txt"), header=T)
	GC_elev_input <<- read.table(paste0(input_dir, "GC/GC_storage_elevation.txt"), header=T)

	##### Duncan ##########
	DUFlood_input <<- read.table(paste0(input_dir, "DUECCandFC/DUFlood_new.txt"), header=T)
	DUCriticalCurve_input <<- read.table(paste0(input_dir, "DUECCandFC/DUCriticalCurve_new.txt"), header=T)
	DUAssuredRefill_input <<- read.table(paste0(input_dir, "DUECCandFC/DU1931Refill_new.txt"), header=T)

	###### Arrow ##########
	ARFlood_input <<- read.table(paste0(input_dir, "ARECCandFC/ARFlood_new.txt"), header=T)
	ARCriticalCurve_input <<- read.table(paste0(input_dir, "ARECCandFC/ARCriticalCurve_new.txt"), header=T)
	ARAssuredRefill_input <<- read.table(paste0(input_dir, "ARECCandFC/AR1931Refill_new.txt"), header=T)
	AR_elev_input <<- read.table(paste0(input_dir, "ARECCandFC/AR_storage_elevation.txt"), header=T)

	#### Mica ###########
	MIFlood_input <<- read.table(paste0(input_dir, "MicaECCandFC/MIFlood_new.txt"), header=T)
	MICriticalCurve_input <<- read.table(paste0(input_dir, "MicaECCandFC/MICriticalCurve_new.txt"), header=T)
	MIAssuredRefill_input <<- read.table(paste0(input_dir, "MicaECCandFC/MI1931Refill_new.txt"), header=T)
 	MI_elev_input <<- read.table(paste0(input_dir, "MicaECCandFC/MI_storage_elevation.txt"), header=T)
 
	##### Energy #######
	NonFirmFraction_input <<- read.table(paste0(input_dir, "Energy/NonFirmFraction.txt"), header=T)
	AltNonFirmLoad_input <<- read.table(paste0(input_dir, "Energy/AltNonFirmLoad.txt"), header=T)
	HH_USBRmax_input <<- read.table(paste0(input_dir, "Energy/HH_USBRmax.txt"), header=T)
	FirmFraction_input <<- read.table(paste0(input_dir, "FirmFraction.txt"), header=T)
  
  	#### Flow targets ######
	BonnevilleFlowTarget_input <<- read.table(paste0(input_dir, "BONN/BON_flow_target.txt"), header=T)
	LowerGraniteFlowTarget_input <<- read.table(paste0(input_dir, "LG/LowerGraniteTarget_new.txt"), header=T)
	VernitaBarFlowTarget_input <<- read.table(paste0(input_dir, "GC/VernitaBarFlowTarget_new.txt"), header=T)
	GCBdgtForVB_input <<- read.table(paste0(input_dir, "GC/GCBdgtForVB_new.txt"), header=T)
  	Article_56_input <<- read.table(paste0(input_dir, "Kerr/article_56_new.txt"), header=T)

	######### Bonneville ########
	BONNetHead_input <<- read.table(paste0(input_dir, "BONN/BONNetHead.txt"), header=T)
  
	######### CRT Scenarios ###########
	ARCRT1_storage <<- read.table(paste0(input_dir, "CRT/ARCRT1_storage_weekly.txt"), header=T)
	ARCRT2_storage <<- read.table(paste0(input_dir, "CRT/ARCRT2_storage_weekly.txt"), header=T)
	ARCRT1_release <<- read.table(paste0(input_dir, "CRT/ARCRT1_release_weekly.txt"), header=T)
	ARCRT2_release <<- read.table(paste0(input_dir, "CRT/ARCRT2_release_weekly.txt"), header=T)
	MICRT2_storage <<- read.table(paste0(input_dir, "CRT/MICRT2_storage_weekly.txt"), header=T)
}
