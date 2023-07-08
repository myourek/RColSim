#######################
# OUTPUT DATAFRAMES ###
#######################
dams_out = data.frame(matrix(nrow=N_of_TimeSteps, ncol=33))
names(dams_out) = c("MICAA", "REVEL", "ARROW", "DUNCA", "LIBBY", "BONFE", "CORRA", "FLASF", "FLAPO", "NOXON", "CABIN", "ALBEN", "BOUND",
"GCOUL", "CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "UpSnake", "BROWN", "OXBOW", "HCANY", "DWORS", "LGRAN", "LGOOS", 
"LMONU", "ICEHA", "MCNAR", "JDAYY", "DALLE", "BONNE")

dams_in = data.frame(matrix(nrow=N_of_TimeSteps, ncol=33))
names(dams_in) = c("MICAA", "REVEL", "ARROW", "DUNCA", "LIBBY", "BONFE", "CORRA", "FLASF", "FLAPO", "NOXON", "CABIN", "ALBEN", "BOUND",
"GCOUL", "CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "UpSnake", "BROWN", "OXBOW", "HCANY", "DWORS", "LGRAN", "LGOOS", 
"LMONU", "ICEHA", "MCNAR", "JDAYY", "DALLES", "BONNE")

mainstem_curtailments = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_curtailments) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE")

other_curtailments = data.frame(matrix(nrow=N_of_TimeSteps, ncol=25))
names(other_curtailments) = c("MICAA", "REVEL", "ARROW", "DUNCA", "LIBBY", "BONFE", "CORRA", "FLASF", "FLAPO", "NOXON", "CABIN", "ALBEN", "BOUND",
"GCOUL", "UpSnake", "MdlSnake", "BROWN", "OXBOW", "HCANY", "DWORS", "LGRAN", "LGOOS", "LMONU", "ICEHA", "BONNE")

mainstem_shortfall = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_shortfall) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE") 

MOP_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=14))
names(MOP_df) = c("FirmEnergy", "NonFirmEnergy", "ColFallsFlow", "LowerGraniteFlow", "VernitaBarFlow", "McNaryFlow",
"GCRec", "DallesFlood", "IHNav", "BonnevillFlow", "BelowFCC", "FirmEnergySales", "NonFirmSpotSales", "TotalSysEnergy")

reservoir_vol_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=12))
names(reservoir_vol_df) = c("MI", "AR", "DU", "CL", "LB", "HH", "GC", "DW", "BR", "US", "KE", "AF")

energy_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=7))
names(energy_df) = c("ARFirmEngSupReq", "TotalEnergyContent", "TotalECCEnergyContent", "FirmEnergyDeficit",
"TotalCoordPreEnergy", "TotalNFEnergyContent", "NonFirmEnergyDeficit")

water_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=21))
names(water_df) = c("BRPrelim_c", "TotalFloodSpace_c", "TotalRelReducReq", "ARPrelim", "ARCombSup", "AREnergySup", "ARMcNarySup", "ARSharedWater", "ARECCSharedWater", "AR_CRT_SOA_storage", "AR_CRT_storage", "AR_CRT_release", "MI_CRT_storage", "MI_CRT_release", "MIMcNarySup", "MI_CRT_SOA_storage", "MIRelReducReq", "ARRelReducReq", "TotalMcNarySharedWater_c", "BRIn", "GCIn")

Biop = data.frame(matrix(nrow=N_of_TimeSteps, ncol=2))
names(Biop) = c("MCNAR", "BONNE")