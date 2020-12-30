#obsolete

###########

MIFloodMult=1.25
LBFloodMult=1.25


# McNary Flow Target ------------------------------------------------------


#The Corps designates Libby, and the USBR designates Grand Coulee and Hungry Horse to contribute to the Federal Columbia River Power System (FCRPS) 
#goal of meeting the following BiOp flow objectives:
#Spring (4/10 - 6/30)     #Priest Rapids (135 kcfs)
#Spring (4/20 - 6/30)**   #McNary Dam (220-260 kcfs)
#Summer (7/1 - 8/31)      #McNary Dam (200 kcfs)
#Interpolation between 220 and 260 kcfs is based upon Corps of Engineers data submittal, which bases targets on forecasts of Jan-July flow at The Dalles.

TotalMcNarySharedWater= function(){
  TotalMcNarySharedWater_o=MIMcNarySharedWater()+DuMcNarySharedWater()+ARMcNarySharedWater()+GCMcNarySharedWater()+HHMcNarySharedWater()+LBMcNarySharedWater()
  return(TotalMcNarySharedWater_o)
}


##### TotalFloodSpace

TotalFloodSpace=function(){
  
  
  TotalFloodSpace_o=MIFloodMult*MIFloodSpace()+ARFloodSpace()+GCFloodSpace()+LBFloodMult*LBFloodSpace()+HHFloodSpace()+KRFloodSpace()+DWFloodSpace()
  
 return(TotalFloodSpace_o) 
}


###### TotalRelReducReq

TotalRelReducReq=function(){
  
  TotalRelReducReq_o=max(0,(DaPrelim()-(DAFloodTarget()*cfsTOafw)))
  
  return(TotalRelReducReq_o)
}


######## DAFloodTarget

DAFloodTarget=function(){
  
  DAFloodTarget_o=max(400000, if (DallesRunoffAprAug>120E6)  {DAHighFloodTarget()} else {DALowFloodTarget()})
  
  return(DAFloodTarget_o)
}



##### DALowFloodTarget

DALowFloodTarget=function(){
  
  
  DALowFloodTarget_o=DALowFloodTarget_input[month_in_year,2]
  
  return(DALowFloodTarget_o)
}



###### DAHighFloodTarget

DAHighFloodTarget=function(){
  
  DAHighFloodTarget_o=DAHighFloodTarget_input[month_in_year,2]
  
  return(DAHighFloodTarget_o)
}


###### GCDownStreamHead

GCDownStreamHead=function(){

  GCDownStreamHead_o=BONNetHead()+CJNetHead()+DANetHead()+JDNetHead()+MCNetHead()+PRNetHead()+RINetHead()+RRNetHead()+WANetHead()+WENetHead()
 
  return(GCDownStreamHead_o) 
}


###### BONNetHead

BONNetHead=function(){
  
  BONNetHead_o=BONNetHead_input[month_in_year,2]
  
  return(BONNetHead_o)
}


###### CJNetHead

CJNetHead=function(){
  
  CJNetHead_o=167
  
  return(CJNetHead_o)
}


####### DANetHead

DANetHead=function(){

  DANetHead_o=80.14

  return(DANetHead_o)
}


####### JDNetHead

JDNetHead=function(){
  
  JDNetHead_o=100
  
  return(JDNetHead_o)
}

##### MCNetHead

MCNetHead=function(){
  
  MCNetHead_o=74
  
  return(MCNetHead_o)
}

###### PRNetHead

PRNetHead=function(){
  
  PRNetHead_o=76.5
  
  return(PRNetHead_o)
}

####### RINetHead

RINetHead=function(){
  
  RINetHead_o=34.4
  
  return(RINetHead_o)
}

#### RRNetHead

RRNetHead=function(){
  
  RRNetHead_o=86.5
  
  return(RRNetHead_o)
}

##### WANetHead

WANetHead=function(){
  
  WANetHead_o=77.8
  
  return(WANetHead_o)
}

###### WENetHead

WENetHead=function(){
  
  WENetHead_o=66.9
  
  return(WENetHead_o)
}


###### IHNetHead

IHNetHead=function(){
  
  IHNetHead_o=98
  
  return(IHNetHead_o)
}


######### LGNetHead

LGNetHead=function(){
  
  LGNetHead_o=100
  
return(LGNetHead_o)  
}


###### LiGNetHead

LiGNetHead=function(){
  
  LiGNetHead_o=98

  return(LiGNetHead_o)  
}

###### LMNetHead

LMNetHead=function(){
  
  LMNetHead_o=100
  
  return(LMNetHead_o)
}


######### BCNetHead

BCNetHead=function(){
  
  BCNetHead_o=152
  
  return(BCNetHead_o)
}


######### CBNetHead

CBNetHead=function(){
  
  CBNetHead_o=97.2
  
  return(CBNetHead_o)
}


######## NoxNetHead

NoxNetHead=function(){
  
  NoxNetHead_o=152
  
  return(NoxNetHead_o)
}

####### HHDownStreamHead

BDNetHead=function(){
  
  BDNetHead_o=97.2
  
  return(BDNetHead_o)
}


########## TotalEnergyContent 

TotalEnergyContent=function(){
  
  TotalEnergyContent_o=DWEnergyContent()+GCEngContMult*GCEnergyContent()+HHEnergyContent()+LBEnergyContent()+MIEnergyContent()+AREnergyContent()+DUEnergyContent()
    
    return(TotalEnergyContent_o)
}


####### TotalECCEnergyContent

TotalECCEnergyContent=function(){
  
  TotalECCEnergyContent_o=
  HHECCEnergyContent()+LBECCEnergyContent()+MIECCEnergyContent()+GCEngContMult*GCECCEnergyContent()+DWECCEnergyContent()+ARECCEnergyContent()+DUECCEnergyContent()
  
  return(TotalECCEnergyContent_o)
}


######### FirmEnergyDeficit

FirmEnergyDeficit=function(){
  
  
  TotalCoordPreEnergy_c<<-TotalCoordPreEnergy()
  
  FirmEnergyDeficit_o=max(0,EnergyAllocSafeFactor*FirmEnergyTarget()-TotalCoordPreEnergy_c)
 
  return(FirmEnergyDeficit_o) 
}


######### FirmEnergyTarget

FirmEnergyTarget=function(){
  
  FirmEnergyTarget_o=AvgFirmLoad()*(Deviation__From_Normal_Curve*FirmFraction()+1)
  
  return(FirmEnergyTarget_o)
}


####### AvgFirmLoad
# Average firm energy target.  This value is multiplied by the seasonal fraction to yield the firm energy target for each month.  Units MW-hr/month.

AvgFirmLoad=function(){
  
  AvgFirmLoad_o=1.20E+06
  
  return(AvgFirmLoad_o)
}


###### FirmFraction

FirmFraction=function(){
  
  FirmFraction_o=FirmFraction_input[week_counter_in_year(),2]
  
  return(FirmFraction_o)
}


###### TotalCoordPreEnergy

TotalCoordPreEnergy=function(){
  
  TotalCoordPreEnergy_o=TotalEnergyFromMcNarySups()+HungryHorsePreEnergy()+LibbyPreEnergy()+MicaGrPreEnergy()+KerrGrPreEnergy()
  +AlbeniFallsGroupPreEnergy()+GrandCouleePreEnergy()+DworshakGroupPreEnergy()+LowerColPreEnergy()
  
  return(TotalCoordPreEnergy_o)
}


###### TotalNFEnergyContent

TotalNFEnergyContent=function(){
  
  TotalNFEnergyContent_o=ARNFEnergyContent()+DUNFEnergyContent()+DWNFEnergyContent()
  +GCEngContMult*GCNFEnergyContent()+HHNFEnergyContent()+LBNFEnergyContent()+MINFEnergyContent()
  
  return(TotalNFEnergyContent_o)
}



####### ARNFEnergyContent

ARNFEnergyContent=function(){
  
  
  ARNFEnergyContent_o=max(0,ARECCEnergyContent()-ARFirmEngSup())
  
  return(ARNFEnergyContent_o)
}


######## DUNFEnergyContent

DUNFEnergyContent=function(){
  
  DUNFEnergyContent_o=max(0,DUECCEnergyContent()-DUFirmEngSup())
  
  return(DUNFEnergyContent_o)
}


##### DUFirmEngSup

DUFirmEngSup=function(){
  
  if(UseTotalEnergyContentForFirm()==1) { if(TotalEnergyContent_c==0) {DUFirmEngSup_o=0 
  }else {DUFirmEngSup_o=(DUEnergyContent()+DUECCEnergyContent())/(TotalEnergyContent_c+TotalECCEnergyContent_c)*FirmEnergyDeficit_c }
  }else if (TotalECCEnergyContent_c==0) {DUFirmEngSup_o=0} else {DUFirmEngSup_o=DUECCEnergyContent()/TotalECCEnergyContent_c*FirmEnergyDeficit_c}
  
  return(DUFirmEngSup_o)
}


##### DWNFEnergyContent

DWNFEnergyContent=function(){
  
  DWNFEnergyContent_o=max(0,DWECCEnergyContent()-DWFirmEngSup())
  
  return(DWNFEnergyContent_o)
}


###### DWFirmEngSup

DWFirmEngSup=function(){
  
  if(UseTotalEnergyContentForFirm()==1) {if(TotalEnergyContent_c==0)  {DWFirmEngSup_o=0 
  }else {DWFirmEngSup_o=(DWEnergyContent()+DWECCEnergyContent())/(TotalEnergyContent_c+TotalECCEnergyContent_c)*FirmEnergyDeficit_c }
  }else if(TotalECCEnergyContent_c==0) {DWFirmEngSup_o=0} else {DWFirmEngSup_o=DWECCEnergyContent()/TotalECCEnergyContent_c*FirmEnergyDeficit_c}
  
  return(DWFirmEngSup_o)
}


###### GCNFEnergyContent

GCNFEnergyContent=function(){
  
  GCNFEnergyContent_o=max(0,GCECCEnergyContent()-GCFirmEngSup())
  
  return(GCNFEnergyContent_o)
}


##### GCFirmEngSup

GCFirmEngSup=function(){
  
  
  if(UseTotalEnergyContentForFirm()==1) {if (TotalEnergyContent_c==0) {GCFirmEngSup_o= 0 
  } else {GCFirmEngSup_o=GCEngContMult*(GCEnergyContent()+GCECCEnergyContent())/(TotalEnergyContent_c+TotalECCEnergyContent_c)*FirmEnergyDeficit_c }
  }else if(TotalECCEnergyContent_c==0)  {GCFirmEngSup_o=0} else {GCFirmEngSup_o=GCEngContMult*GCECCEnergyContent/TotalECCEnergyContent*FirmEnergyDeficit}
  
  return(GCFirmEngSup_o)
}


######## HHNFEnergyContent

HHNFEnergyContent=function(){
  
  HHNFEnergyContent_o=max(0,HHECCEnergyContent()-HHFirmEngSup())
  
  return(HHNFEnergyContent_o)
}


#### HHFirmEngSup

HHFirmEngSup=function(){
  
  if(UseTotalEnergyContentForFirm()==1) { if(TotalEnergyContent_c==0)  {HHFirmEngSup_o=0 
  }else {HHFirmEngSup_o=(HHEnergyContent()+HHECCEnergyContent())/(TotalEnergyContent_c+TotalECCEnergyContent_c)*FirmEnergyDeficit_c}
  }else if(TotalECCEnergyContent_c==0) {HHFirmEngSup_o=0} else {HHFirmEngSup_o=HHECCEnergyContent()/TotalECCEnergyContent_c*FirmEnergyDeficit_c}
  
  return(HHFirmEngSup_o)
}



####### LBNFEnergyContent

LBNFEnergyContent=function(){
  
  LBNFEnergyContent_o=max(0,LBECCEnergyContent()-LBFirmEngSup())
  
  return(LBNFEnergyContent_o)
}


####### LBFirmEngSup

LBFirmEngSup=function(){
  
  if(UseTotalEnergyContentForFirm()==1) {if (TotalEnergyContent_c==0) {LBFirmEngSup_o=0 
  }else {LBFirmEngSup_o=(LBEnergyContent()+LBECCEnergyContent())/(TotalEnergyContent_c+TotalECCEnergyContent_c)*FirmEnergyDeficit_c}
  } else if(TotalECCEnergyContent_c==0) {LBFirmEngSup_o=0} else {LBFirmEngSup_o=LBECCEnergyContent()/TotalECCEnergyContent_c*FirmEnergyDeficit_c}
  
  return(LBFirmEngSup_o)
}



######## MINFEnergyContent

MINFEnergyContent=function(){
  
  MINFEnergyContent_o=max(0,MIECCEnergyContent()-MIFirmEngSup())
  
  return(MINFEnergyContent_o)
}


########## MIFirmEngSup

MIFirmEngSup=function(){
  
  if(UseTotalEnergyContentForFirm()==1) { if(TotalEnergyContent_c==0) {MIFirmEngSup_o=0 
  } else {MIFirmEngSup_o=(MIEnergyContent()+MIECCEnergyContent())/(TotalEnergyContent_c+MIECCEnergyContent())*FirmEnergyDeficit_c }
  }else if (TotalECCEnergyContent_c==0)  {MIFirmEngSup_o=0} else{MIFirmEngSup_o= MIECCEnergyContent()/TotalECCEnergyContent_c*FirmEnergyDeficit_c}

  return(MIFirmEngSup_o)
}


###### NonFirmEnergyDeficit

NonFirmEnergyDeficit=function(){
  
  NonFirmEnergyDeficit_c=max(0,EnergyAllocSafeFactor*NonFirmEnergyTarget()-max(0,(TotalCoordPreEnergy_c-FirmEnergyTarget())))
  
  return(NonFirmEnergyDeficit_c)
}


############## NonFirmEnergyTarget

NonFirmEnergyTarget=function(){
  
  if(UseAlternateNonFirmTarget==0) {NonFirmEnergyTarget_o=NonFirmFraction()*AvgNonFirmLoad()
  }else {NonFirmEnergyTarget_o=AltNonFirmLoad()}
  
  return(NonFirmEnergyTarget_o)
}


###### NonFirmFraction

NonFirmFraction=function(){
  
  NonFirmFraction_o=NonFirmFraction_input[week_counter_in_year(),2]
  
  return(NonFirmFraction_o)
}


###### AvgNonFirmLoad

AvgNonFirmLoad=function(){
  
  AvgNonFirmLoad_o=0.2617*AvgFirmLoad()
  
  return(AvgNonFirmLoad_o)
}


######## AltNonFirmLoad

AltNonFirmLoad=function(){
  
  AltNonFirmLoad_o=AltNonFirmLoad_input[week_counter_in_year(),2]
  
  return(AltNonFirmLoad_o)
}


######## LBFloodSpace

LBFloodSpace=function(){
  
  LBFloodSpace_o= min((LBPrelim()+LBMcNarySup()+LBEnergySup()),max(0, LBFullPoolVol-Libby()+LBIn()-LBPrelim()-LBEnergySup()-LBMcNarySup()))
  
  return(LBFloodSpace_o)
}


####### HHFloodSpace

HHFloodSpace=function(){
  
  HHFloodSpace_o=0 #min((HHPrelim()+HHEnergySupAllow()),max(0,HHFullPoolVol-HungryHorse()+HHIn()-(HHPrelim()+HHEnergySupAllow())))*0
  
  return(HHFloodSpace_o)
}


######## KRFloodSpace

KRFloodSpace=function(){
  
  KRFloodSpace_o=0 #min((KEPrelim()+KECombSup()), max(0,KEFullPoolVol-Kerr_Reservoir()+KEIn()-(KECombSup())))*0
  
  return(KRFloodSpace_o)
}


######## DWFloodSpace

DWFloodSpace=function(){
  
  DWFloodSpace_o=0 # min((DWPrelim()+DWEnergySup),MAX(0,DWFullPoolVol-Dworshak+DWIn-(DWPrelim+DWEnergySup)))*0
  
  return(DWFloodSpace_o)
}