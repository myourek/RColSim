ReadFiles <- function() {
###### initial ##########
HistStor <<- read.table("inputs/HistStor.txt", header=T, nrows=367)
DUFlood_input <<- read.table("inputs/DUECCandFC/DUFlood.txt", header=T)
DU_JAN_FAMAJ <<- read.table("inputs/DUECCandFC/DU-JAN-FAMAJ.txt", header=T)
ARFlood <<- read.table("inputs/ARECCandFC/ARFlood.txt", header=T)
BRCurFC_Cont <<- read.table("inputs/BRL/BRCurFC_Cont.txt", header=T)
BRForecastFloodStorage <<- read.table("inputs/BRL/BRForecastFloodStorage.txt", header=T) # check last row APR_BR115 needs to be fixed

###### Dworshak Dam ######
DWHistStor_input <<- read.table("inputs/DW/DWHistStor.txt", header=T)
ModDworshakFlowData_input <<- read.table("inputs/DW/ModDworshakFlowData.txt", header=T)
DWFlood_input <<- read.table("inputs/DW/DWFlood.txt", header=T)
DW_FloodM <<-read.table("inputs/DW/DWFlood_Month.txt", header=T)
DWRefillMin_input <<- read.table("inputs/DW/DWRefillMin.txt", header=T)
DWAvgMin_input <<- read.table("inputs/DW/DWAvgMin.txt", header=T)
DWCriticalCurve_input <<- read.table("inputs/DW/DWCriticalCurve.txt", header=T)
DW1931Refill_input <<- read.table("inputs/DW/DW1931Refill.txt", header=T)

##### Low Granite Dam ######
ModLowerGraniteFlowData_input <<- read.table("inputs/LG/ModLowerGraniteFlowData.txt", header=T)
LowerGraniteTarget_input <<- read.table("inputs/LG/LowerGraniteTarget.txt", header=T)

###### US-CR ############
BaseUSAg_input <<- read.table("inputs/USCR/BaseUSAg.txt", header=T)
USFloodRuleCurve_input <<- read.table("inputs/USCR/USFloodRuleCurve.txt", header=T)
MB_input <<- read.table("inputs/USCR/MB.txt", header=T)
BaseUSAg_input <<- read.table("inputs/USCR/BaseUSAg.txt", header=T)

#### MS-CR ###########
MSFloodRuleCurve_input <<- read.table("inputs/MSCR/MSFloodRuleCurve.txt", header=T)
MSMIFReq_input <<- read.table("inputs/MSCR/MSMIFReq.txt", header=T)

#### MSAg #############3
B064X_input <<- read.table("inputs/MSAg/B065-4567.txt", header=T)
FCIrQSnke_input <<- read.table("inputs/MSAg/FCIrQSnke.txt", header=T)
QSnke_input <<- read.table("inputs/MSAg/QSnke.txt", header=T)
BaseMSAgReturn_input <<- read.table("inputs/BRL/BaseMSAgReturn.txt", header=T)
BaseMSAgWith_input <<- read.table("inputs/MSAg/MSNetAgRight.txt", header=T)

#### McNary Dam #############
MNB_input <<- read.table("inputs/McNary/MNBaseTarget.txt", header=T)
ModMcNaryFlowData_input <<- read.table("inputs/McNary/ModMcNaryFlowData.txt", header=T)

#### Brownlee #######
ModHellsCanyonFlowData_input <<- read.table("inputs/BRL/ModHellsCanyonFlowData.txt", header=T)
ModOxbowFlowData_input <<- read.table("inputs/BRL/ModOxbowFlowData.txt", header=T)
BRHistStor_input <<- read.table("inputs/BRL/BRHistStor.txt", header=T)
BrownleeObsNatFlowData_input <<- read.table("inputs/BRL/BrownleeObsNatFlowData.txt", header=T)
MilnerObsNatFlowData_input <<- read.table("inputs/BRL/MilnerObsNatFlowData.txt", header=T)
BRFlood <<- read.table("inputs/BRL/BRFlood.txt", header=T)
AddSp_input <<- read.table("inputs/BRL/Add_Space.txt", header=T)
BrownleeObsRegFlowData_input <<- read.table("inputs/BRL/BrownleeObsRegFlowData.txt", header=T)
ModBrownleeFlowData_input <<- read.table("inputs/BRL/ModBrownleeFlowData.txt", header=T)

####### Libby ########
LBHistStor_input <<- read.table("inputs/Libby/LBHistStor.txt", header=T)
LBAgWith_input <<- read.table("inputs/Libby/LBAgWith.txt", header=T)
LBAgRech_input <<- read.table("inputs/Libby/LBAgRech.txt", header=T)
LBFlood_input <<- read.table("inputs/Libby/LBFlood.txt", header=T)
MFlood_input <<- read.table("inputs/Libby/MFlood.txt", header=T)
LBF_input <<- read.table("inputs/Libby/LBFlood_2.txt", header=T)
APR_LB_input <<- read.table("inputs/Libby/APRLB.txt", header=T)
LBCriticalCurve_input <<- read.table("inputs/Libby/LBCriticalCurve.txt", header=T)
LB1931Refill_input <<- read.table("inputs/Libby/LB1931Refill.txt", header=T)
LBMaxFCRel_input <<- read.table("inputs/Libby/LBMaxFCRel.txt", header=T)
ModBonnersFerryFlowData_input <<- read.table("inputs/Libby/ModBonnersFerryFlowData.txt", header=T)

##### Corra Linn ###########
CLHistStor_input <<- read.table("inputs/CL/CLHistStor.txt", header=T)
CLAgWith_input <<- read.table("inputs/CL/AgWith.txt", header=T)
CLIJCRuleCurve_input <<- read.table("inputs/CL/CLIJCRuleCurve.txt", header=T)

##### Columbia Falls ######
ColFall_Target <<- read.table("inputs/HH/ColFalls_Target.txt",header=T) # check ,

##### Hungary Horse ########
HHCriticalCurve_input <<- read.table("inputs/HH/HHCriticalCurve.txt", header=T)
HHBaseDraftLimit_input <<- read.table("inputs/HH/HHBaseDraftLimit.txt", header=T)
HHAssuredRefill_input <<- read.table("inputs/HH/HHAssuredRefill.txt", header=T)
HHStor <<- read.table("inputs/HH/HHHistStor.txt", header=T)
HHFlood <<- read.table("inputs/HH/HHFlood.txt", header=T)
JanToAprFlood <<- read.table("inputs/HH/HHFlood_JanApr.txt", header=T) # check , why are ther more than one line?
HHFlood_2_input <<- read.table("inputs/HH/HHFlood_2.txt", header=T)
APR_HH_input <<- read.table("inputs/HH/AprHH.txt", header=T)

### Kerr #####
KerrFloodC <<- read.table("inputs/Kerr/FloodCurve.txt", header=T)
Article_56_input <<- read.table("inputs/Kerr/article_56.txt", header=T)

#### Bonneville ######
Chum_variable_2_input <<- read.table("inputs/BONN/Chum_variable_2.txt", header=T)
ModBonnevilleFlowData_input <<- read.table("inputs/BONN/ModBonnevilleFlowData.txt", header=T)

##### PreEnergy ########3
VernitaBarFlowTarget_input <<- read.table("inputs/GC/VernitaBarFlowTarget.txt", header=T)


### AlbeniFalls ######
AFAvgMin_input <<- read.table("inputs/AFECCandFC/AFAvgMin.txt", header=T)
AFFlood <<- read.table("inputs/AFECCandFC/AFFC.txt", header=T)

#### Revelstoke #######
REVAgWith_input <<- read.table("inputs/REV/REVAgWith.txt", header=T)
REVAgRech_input <<- read.table("inputs/REV/REVAgRech.txt", header=T)

### Grand Coulee #######
GCFlood1_input <<- read.table("inputs/GC/GCFlood1.txt", header=T)
GCF_Month_input <<- read.table("inputs/GC/GCFlood_Month.txt", header=T)
GCRefillMin_input <<- read.table("inputs/GC/GCRefillMin.txt", header=T)
GCAvgMin_input <<- read.table("inputs/GC/GCAvgMin.txt", header=T)
GCCriticalCurve_input <<- read.table("inputs/GC/GCCriticalCurve.txt", header=T)
GCRecLimit_input <<- read.table("inputs/GC/GCRecLimit.txt", header=T)
PreGCDraftLimit_input <<- read.table("inputs/GC/PreGCDraftLimit.txt", header=T)
GCFlood2 <<- read.table("inputs/GC/GCFlood2.txt", header=T)
GCF_month <<- read.table("inputs/GC/GCFlood_Month.txt", header=T)
GCAbsMinQ_input <<- read.table("inputs/GC/GCAbsMinQ.txt", header=T)
GCBdgtForVB_input <<- read.table("inputs/GC/GCBdgtForVB.txt", header=T)
GCFlood1_input <<- read.table("inputs/GC/GCFlood1.txt", header=T)

##### Duncan ##########
DUCriticalCurve_input <<- read.table("inputs/DUECCandFC/DUCriticalCurve.txt", header=T)
DU1931Refill_input <<- read.table("inputs/DUECCandFC/DU1931Refill.txt", header=T)
DURefillMin_input <<- read.table("inputs/DUECCandFC/DURefillMin.txt", header=T)
DUF_2_input <<- read.table("inputs/DUECCandFC/DUFlood_2.txt", header=T)
FMAMJ_DU_input <<- read.table("inputs/DUECCandFC/FMAMJ_DU.txt", header=T)
DUMaxFCRel_input <<- read.table("inputs/DUECCandFC/DUMaxFCRel.txt", header=T)

######## Flow Targets #########
FirmFraction_input <<- read.table("inputs/FirmFraction.txt", header=T)
BONNetHead_input <<- read.table("inputs/BONN/BONNetHead.txt", header=T)

###### Arrow ##########
ARFloodMonth <<- read.table("inputs/ARECCandFC/ARFloodMonth.txt", header=T)
ARFlood_May5_i <<- read.table("inputs/ARECCandFC/ARFlood_May5.txt", header=T)
ARFlood_2_input <<- read.table("inputs/ARECCandFC/ARFlood_2.txt", header=T)
ARCriticalCurve_input <<- read.table("inputs/ARECCandFC/ARCriticalCurve.txt", header=T)
AR1931Refill_input <<- read.table("inputs/ARECCandFC/AR1931Refill.txt", header=T)
ARForecastRLFCurve_input <<- read.table("inputs/ARECCandFC/ARForecastRLFCurve.txt", header=T)
ARActualRFCurve_input <<- read.table("inputs/ARECCandFC/ARActualRFCurve.txt", header=T)
AR1931RFCurve_input <<- read.table("inputs/ARECCandFC/ARActualRFCurve.txt", header=T)
ARHistStor_i <<- read.table("inputs/ARECCandFC/ARHistStor.txt", header=T)
ARWith_input <<- read.table("inputs/ARECCandFC/ARWith.txt", header=T)
ARAssuredRelease_input <<- read.table("inputs/ARECCandFC/AssuredRelease.txt", header=T)

#### Mica ###########
MI1931Refill_input <<- read.table("inputs/MicaECCandFC/MI1931Refill.txt", header=T)
GlobalFloodEvacMult_i <<- read.table("inputs/GlobalFloodEvacMult.txt", header=T)
MIFlood_712 <<- read.table("inputs/MicaECCandFC/MIFlood7-12.txt",header=T)
MIFlood_DecToApr <<- read.table("inputs/MicaECCandFC/MIFlood-DecToApr.txt", header=T)
APR_MI_input <<- read.table("inputs/MicaECCandFC/APR_MI.txt", header=T)
MICriticalCurve_input <<- read.table("inputs/MicaECCandFC/MICriticalCurve.txt", header=T)
MIAssuredReleasedMin_i <<- read.table("inputs/MIAssuredReleasedMin.txt", header=T)
MIMaxFloodOutflow_input <<- read.table("inputs/MicaECCandFC/MIMaxFloodOutflow.txt", header=T)
ARFlood_1May_input <<- read.table("inputs/ARECCandFC/ARFlood_1May.txt", header=T)

##### Energy #######
NonFirmFraction_input <<- read.table("inputs/Energy/NonFirmFraction.txt", header=T)
AltNonFirmLoad_input <<- read.table("inputs/Energy/AltNonFirmLoad.txt", header=T)
ColFallsMaxFlow_input <<- read.table("inputs/Energy/ColFallsMaxFlow.txt", header=T)
HH_USBRmax_input <<- read.table("inputs/Energy/HH_USBRmax.txt", header=T)

######### Dalles #####
DALowFloodTarget_input <<- read.table("inputs/Dall/DALowFloodTarget.txt", header=T)
DAHighFloodTarget_input <<- read.table("inputs/Dall/DAHighFloodTarget.txt", header=T)
}


















































































