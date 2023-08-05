#######################
# OUTPUT DATAFRAMES ###
#######################
dams_out = data.frame(matrix(nrow=N_of_TimeSteps, ncol=46))
names(dams_out) = c("ALBEN", "AMERI", "ARROW", "BONFE", "BONNE", "BOUND", "BOXCA", "BROWN", "CABIN", "CHELA",
	"CHIEF", "CORRA", "DALLE", "DUNCA", "DWORS", "FLAPO", "FLASF", "GCOUL", "HCANY", "ICEHA", "IPARK", 
	"JDAYY", "JLAKE", "LIBBY", "LGOOS", "LGRAN", "LMONU", "LUCKY", "MCNAR", "MICAA", "MILNE", "MINAD", "NOXON",
	"OWYHE", "OXBOW", "PALIS", "PAYHS", "PELTO", "PRIRA", "REVEL", "RIRDM", "RISLA", "ROCKY", "THOMF", "WANAP",
	"WELLS")

dams_in = data.frame(matrix(nrow=N_of_TimeSteps, ncol=46))
names(dams_in) = c("ALBEN", "AMERI", "ARROW", "BONFE", "BONNE", "BOUND", "BOXCA", "BROWN", "CABIN", "CHELA",
	"CHIEF", "CORRA", "DALLE", "DUNCA", "DWORS", "FLAPO", "FLASF", "GCOUL", "HCANY", "ICEHA", "IPARK", 
	"JDAYY", "JLAKE", "LIBBY", "LGOOS", "LGRAN", "LMONU", "LUCKY", "MCNAR", "MICAA", "MILNE", "MINAD", "NOXON",
	"OWYHE", "OXBOW", "PALIS", "PAYHS", "PELTO", "PRIRA", "REVEL", "RIRDM", "RISLA", "ROCKY", "THOMF", "WANAP",
	"WELLS")
	
mainstem_curtailments = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_curtailments) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE")

mainstem_shortfall = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_shortfall) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE") 

MOP_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=14))
names(MOP_df) = c("FirmEnergy", "NonFirmEnergy", "ColFallsFlow", "LowerGraniteFlow", "VernitaBarFlow", "McNaryFlow",
	"GCRec", "DallesFlood", "IHNav", "BonnevillFlow", "BelowFCC", "FirmEnergySales", "NonFirmSpotSales", "TotalSysEnergy")

reservoir_vol_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=22))
names(reservoir_vol_df) = c("MICAA", "ARROW", "DUNCA", "CORRA", "LIBBY", "FLASF", "GCOUL", "DWORS", "BROWN", "FLAPO", 
	"ALBEN", "CHELA", "JLAKE", "PALIS", "IPARK", "RIRDM", "AMERI", "MINAD", "BOISE", "PAYHS", "OWYHE", "PELTO")

energy_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=7))
names(energy_df) = c("ARFirmEngSupReq", "TotalEnergyContent", "TotalECCEnergyContent", "FirmEnergyDeficit",
"TotalCoordPreEnergy", "TotalNFEnergyContent", "NonFirmEnergyDeficit")

water_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=21))
names(water_df) = c("BRPrelim_c", "TotalFloodSpace_c", "TotalRelReducReq", "ARPrelim", "ARCombSup", "AREnergySup", "ARMcNarySup", "ARSharedWater", "ARECCSharedWater", "AR_CRT_SOA_storage", "AR_CRT_storage", "AR_CRT_release", "MI_CRT_storage", "MI_CRT_release", "MIMcNarySup", "MI_CRT_SOA_storage", "MIRelReducReq", "ARRelReducReq", "TotalMcNarySharedWater_c", "BRIn", "GCIn")

Biop = data.frame(matrix(nrow=N_of_TimeSteps, ncol=2))
names(Biop) = c("MCNAR", "BONNE")