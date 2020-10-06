#######################
# OUTPUT DATAFRAMES ###
#######################
LowerPrelimUpFlow_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=1))

dams_out = data.frame(matrix(nrow=N_of_TimeSteps, ncol=34))
names(dams_out) = c("MicaOutflow","RevOut","ArrowOutflow","DuncanOutflow","LibbyOutflow",
"CLOutflow","Canada_Outflows","HHOutflow","KerrOutflow","NoxOut","CBOut","AFOutflow","BoundOut","GCOutflow", "CJOut","WeOut", "RROut", "RIOut",
"WaOut","PROut", "UpSnOutflow","BROutflow", "OXOut","HCOut","DWOutflow","LGOut","LiGOut","LMOut","IHOut","McNOut","JDOut","DaOut","BONOut", "MSOUT")

dams_in = data.frame(matrix(nrow=N_of_TimeSteps, ncol=34))
names(dams_in) = c("MicaInflow","RevIn","ArrowInflow","DuncanInflow","LibbyInflow","CLInflow","HHInflow","KerrInflow","NoxIn","CBIn","AFInflow","BoundIn",
"GCInflow","CJIn","WeIn","RRIn","RIIn","WaIn","PRIn","USInflow","OXIn","HCIn","DWInflow", "BRInflow","LGIn","LiGIn","LMIn","IHIn","McIn",
            "JDIn","DaIn","BoIn", "USIn", "MSin" )

mainstem_curtailments = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_curtailments) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE")

MOP_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=15))
names(MOP_df)<-c("shortfall_1","shortfall_2", "shortfall_3", "shortfall_4", "shortfall_5", "shortfall_6", "shortfall_7","shortfall_8",
                 "shortfall_9", "shortfall_10", "shortfall_11", "NumOfLowFlowBonn", "BelowFCC", "FirmEnergySales", "NonFirmSpotSales")


reservoir_vol_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=11))
names(reservoir_vol_df) = c("MicaReservoir","ArrowReservoir","Duncan","CorraLinnReservoir","Libby","HungryHorse",
                           "GrandCoulee","Dworshak","Brownlee","UpperSnake", "MiddleSnake")

energy_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=7))
names(energy_df) = c("ARFirmEngSupReq", "TotalEnergyContent", "TotalECCEnergyContent", "FirmEnergyDeficit",
                            "TotalCoordPreEnergy", "TotalNFEnergyContent", "NonFirmEnergyDeficit")#, "MaxSystemEnergy")

water_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=5))
names(water_df) = c("BRPrelim_c", "TotalFloodSpace_c", "TotalMcNarySharedWater_c", "BRIn", "GCIn")
