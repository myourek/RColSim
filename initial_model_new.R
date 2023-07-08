
# This script initializes the model and create necessary output files


if(NEW_SIMULATION == TRUE){
	do.call(file.remove, list(list.files(paste(OutputFolder, "/"), full.names=TRUE)))
}

reservoir_vol_df$MI[1] <- Mica()
reservoir_vol_df$AR[1] <- Arrow()
reservoir_vol_df$DU[1] <- Duncan()
reservoir_vol_df$CL[1] <- CorraLinn()
reservoir_vol_df$LB[1] <- Libby()
reservoir_vol_df$HH[1] <- HungryHorse()
reservoir_vol_df$GC[1] <- GrandCoulee()
reservoir_vol_df$DW[1] <- Dworshak()
reservoir_vol_df$BR[1] <- Brownlee()
reservoir_vol_df$US[1] <- UpSnakeComb()
reservoir_vol_df$KE[1] <- Kerr()
reservoir_vol_df$AF[1] <- AlbeniFalls()

MicaRelease_c <- MIRelease()
dams_in$MICAA[1] <- MIInflow()
dams_out$MICAA[1] <- MIOutflow()

dams_in$REVEL[1] <- REVIn()
dams_out$REVEL[1] <- REVOut()

ARRelease_c <- ARRelease()
dams_in$ARROW[1] <- ARInflow()
dams_out$ARROW[1] <- AROutflow()

DUOutflow_c <- DURelease() 
dams_in$DUNCA[1] <- DUInflow()
dams_out$DUNCA[1] <- DUOutflow()

dams_in$LIBBY[1] <- LBInflow()
dams_out$LIBBY[1] <- LBOutflow()

dams_in$BONFE[I_Weeek] <- BONFIn()
dams_out$BONFE[1] <- BONFOut()

CLRelease_c <- CLRelease() 
dams_in$CORRA[1] <- CLInflow()
dams_out$CORRA1] <- CLOutflow()

HHRelease_c <- HHRelease()
dams_in$FLASF[1] <- HHInflow()
dams_out$FLASF[1] <- HHOutflow()

KERelease_c <- KERelease()
dams_in$FLAPO[1] <- KEInflow()
dams_out$FLAPO[1] <- KEOutflow()

dams_in$NOXON[1] <- NOXIn()
dams_out$NOXON[1] <- NOXOut()

dams_in$CABIN[1] <- CBIn()
dams_out$CABIN[1] <- CBOut()

AFRelease_c <- AFRelease()
dams_in$ALBEN[1] <- AFInflow()
dams_out$ALBEN[1] <- AFOutflow()

dams_in$BOUND[1] <- BDIn()
dams_out$BOUND[1] <- BDOut()

GCRelease_c <- GCRelease()
dams_in$GCOUL[1] <- GCInflow()
dams_out$GCOUL[1] <- GCOutflow()

dams_in$JDAYY[1] <- CJIn()
dams_out$JDAYY[1] <- CJOut()
if (track_curtailment == 1) {
	mainstem_curtailments$CHIEF[1] <- CJCurtail()
	mainstem_shortfall$CHIEF[1] <- CJInstreamShortfall()
}

dams_in$WELLS[1] <- WEIn()
dams_out$WELLS[1] <- WEOut()
if (track_curtailment == 1) {
	mainstem_curtailments$WELLS[1] <- WECurtail()
	mainstem_shortfall$WELLS[1] <- WEInstreamShortfall()
}

dams_in$ROCKY[1] <- RRIn()
dams_out$ROCKY[1] <- RROut()
if (track_curtailment == 1) {
	mainstem_curtailments$ROCKY[1] <- RRCurtail()
	mainstem_shortfall$ROCKY[1] <- RRInstreamShortfall()
}

dams_in$RISLA[1] <- RIIn()
dams_out$RISLA[1] <- RIOut()
if (track_curtailment == 1) {
	mainstem_curtailments$RISLA[1] <- RICurtail()
	mainstem_shortfall$RISLA[1] <- RIInstreamShortfall()
}

dams_in$WANAP[1] <- WAIn()
dams_out$WANAP[1] <- WAOut()
if (track_curtailment == 1) {
	mainstem_curtailments$WANAP[1] <- WACurtail()
	mainstem_shortfall$WANAP[1] <- WAInstreamShortfall()
}

dams_in$PRIRA[1] <- PRIn()
dams_out$PRIRA[1] <- PROut()
if (track_curtailment == 1) {
	mainstem_curtailments$PRIRA[1] <- PRCurtail()
	mainstem_shortfall$PRIRA[1] <- PRInstreamShortfall()
}

dams_in$UpSnake[1] <- USInflow()
dams_outUpSnake[1] <- USOutflow()

BRRelease_c <- BRRelease()
dams_in$BROWN[1] <- BRInflow()
dams_out$BROWN[1] <- BROutflow()

dams_in$OXBOW[1] <- OXIn()
dams_out$OXBOW[1] <- OXOut()

dams_in$HCANY[1] <- HCIn()
dams_out$HCANY[1] <- HCOut()

DWRelease_c <- DWRelease()
dams_in$DWORS[1] <- DWInflow()
dams_out$DWORS[1] <- DWOutflow()

dams_in$LGRAN[1] <- LGIn()
dams_out$LGRAN[1] <- LGOut()	

dams_in$LGOOS[1] <- LIGIn()
dams_out$LGOOS[1] <- LIGOut()

dams_in$LMONU[1] <- LMIn()
dams_out$LMONU[1] <- LMOut()

dams_in$ICEHA[1] <- IHIn()
dams_out$ICEHA[1] <- IHOut()

dams_in$MCNAR[1] <- MCNIn()
dams_out$MCNAR[1] <- MCNOut()
if (track_curtailment == 1) {
	mainstem_curtailments$MCNAR[1] <- MCNCurtail()
	mainstem_shortfall$MCNAR[1] <- MCNInstreamShortfall()
}
Biop[1,1] <- McNaryFlowTarget()

dams_in$JDAYY[1] <- JDIn()
dams_out$JDAYY[1] <- JDOut()
if (track_curtailment == 1) {
	mainstem_curtailments$JDAYY[1] <- JDCurtail()
	mainstem_shortfall$JDAYY[1] <- JDInstreamShortfall()
}

dams_in$DALLE[1] <- DAIn()
dams_out$DALLE[1] <- DAOut()
if (track_curtailment == 1) {
	mainstem_curtailments$DALLE[1] <- DACurtail()
	mainstem_shortfall$DALLE[1] <- DAInstreamShortfall()
}

dams_in$BONNE[1] <- BONIn()
dams_out$BONNE[1] <- BONOut()
Biop[1,2] <- BonnevilleFlowTarget()

######### STORAGE FOR THE FIRST TIME STEP
reservoir_vol_df$MI[1] <- reservoir_vol_df$MI[1] + (dams_in$MICAA[1] - dams_out$MICAA[1])
reservoir_vol_df$AR[1] <- reservoir_vol_df$AR[1] + (dams_in$ARROW[1] - dams_out$ARROW[1]) 
reservoir_vol_df$DU[1] <- reservoir_vol_df$DU[1] + (dams_in$DUNCA[1] - dams_out$DUNCA[1]) 
reservoir_vol_df$CL[1] <- reservoir_vol_df$CL[1] + (dams_in$CORRA[1] - dams_out$CORRA[1]) 
reservoir_vol_df$LB[1] <- reservoir_vol_df$LB[1] + (dams_in$LIBBY[1] - dams_out$LIBBY[1]) 
reservoir_vol_df$HH[1] <- reservoir_vol_df$HH[1] + (dams_in$FLASF[1] - dams_out$FLASF[1]) 
reservoir_vol_df$GC[1] <- reservoir_vol_df$GC[1] + (dams_in$GCOUL[1] - dams_out$GCOUL[1])
reservoir_vol_df$DW[1] <- reservoir_vol_df$DW[1] + (dams_in$DWORS[1] - dams_out$DWORS[1]) 
reservoir_vol_df$BR[1] <- reservoir_vol_df$BR[1] + (dams_in$BROWN[1] - dams_out$BROWN[1]) 
reservoir_vol_df$US[1] <- reservoir_vol_df$US[1] + (dams_in$UpSnake[1] - dams_out$UpSnake[1]) 
reservoir_vol_df$KE[1] <- reservoir_vol_df$KE[1] + (dams_in$FLAPO[1] - dams_out$FLAPO[1]) 
reservoir_vol_df$AF[1] <- reservoir_vol_df$AF[1] + (dams_in$ALBEN[1] - dams_out$ALBEN[1]) 

###### MOPs Measures Of Performance
MOP_df$FirmEnergy[1] <- FirmEnergyMOP() # Firm energy shortfall
MOP_df$NonFirmEnergy[1] <- NonFirmEnergyMOP() # Non-Firm energy shortfall
MOP_df$ColFallsFlow[1] <- ColFallsFlowMOP() # Columbia Falls flow shortfall
MOP_df$LowerGraniteFlow[1] <- LowerGraniteFlowMOP() # Lower Granite flow shortfall 
MOP_df$VernitaBarFlow[1] <- VernitaBarFlowMOP() # Vernita Bar flow shortfall
MOP_df$McNaryFlow[1] <- McNaryFlowMOP() # McNary flow shortfall
MOP_df$GCRec[1] <- GCRecMOP() # Grand Coulee recreation metric
MOP_df$DallesFlood[1] <- DallesFloodMOP() # The Dalles flood protection metric
MOP_df$IHNav[1] <- IHNavMOP() # Ice Harbor navigation metric
MOP_df$BonnevillFlow[1] <- BonnevillFlowMOP() # Bonneville flow shortfall
MOP_df$BelowFCC[1] <- BelowFCC() # Excess flood storage space
MOP_df$FirmEnergySales[1] <- FirmEnergySales()
MOP_df$NonFirmSpotSales[1] <- NonFirmSpotSales()
MOP_df$TotalSysEnergy[1] <- TotalSysEnergy()

write.table(cbind(date_hist_sim[1,], dams_out[1,]), paste0(OutputFolder, "/dams_out.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], dams_in[1,]), paste0(OutputFolder, "/dams_in.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], reservoir_vol_df[1,]), paste0(OutputFolder, "/reservoir_volume.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], MOP_df[1,]), paste0(OutputFolder, "/MOP_df.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], water_df[1,]), paste0(OutputFolder, "/water.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], energy_df[1,]), paste0(OutputFolder, "/energy.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], Biop[1,]), paste0(OutputFolder, "/Biop_flow.txt"), row.names=F, col.names=T, append=F)

if (track_curtailment == 1) {
	write.table(cbind(date_hist_sim[1,], mainstem_shortfall[1,]), paste0(OutputFolder, "/mainstem_shortfall.txt"), row.names=F, col.names=T, append=F)
	write.table(cbind(date_hist_sim[1,], mainstem_curtailments[1,]), paste0(OutputFolder, "/mainstem_curtailment.txt"), row.names=F, col.names=T, append=F)
}
