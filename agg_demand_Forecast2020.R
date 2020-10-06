# this code aggregates the demands to different ColSim points and 
#2- then makes the flows weekly consistent with the flow data

POD_names = read.table("~/Step_3/Aggregate_demand_ColSim/Stn_code_name.txt", stringsAsFactors=F, header=T)	
unit_conversion = 0.4087 # converts from mm * km^2/day to cfs
POD_file = read.csv("~/Step_3/Aggregate_demand_ColSim/pod_file_Forecast2021.csv", stringsAsFactors=F)
names(POD_file) = c("CellID", "GWFraction", "ReturnFlowFraction", "WaterCallID", "CurtailID", "NoOfCrops", "InstreamProvision", "Lon", 
"Lat", "Area", "OldPODStnNM", "OldPODStn", "OldStnNo", "NewStnNo", "NewStnNo2", "NewStnNo3", "NewStnForCurtailment")
POD_file$GWFraction[POD_file$NewStnNo3==4 || POD_file$NewStnNo3==13] = 0.35
crop_param = read.csv("~/Step_3/Crop_params/crop_params_Forecast2021.csv", stringsAsFactors=F)


gcm_list = read.table("~/Bias_correction/bc_Columbia/bias_correct_0.125_Natflow/scenario_list", stringsAsFactors=F)[-1,1]
scr_list = c("hist", "rcp45", "rcp85")
list_scenarios = vector(length=3*length(gcm_list)+1)
for (i in 1:length(scr_list)) {
	j = (i - 1) * length(gcm_list) + 2
	list_scenarios[j:(i*length(gcm_list)+1)] = sapply(gcm_list, function(x) {paste(x, scr_list[i], sep="/")})
}
list_scenarios[1] = "Historical_baseline"
list_scenarios = c("Historical_baseline", "bcc-csm1-1/rcp45")

for (i_scr in 1:length(list_scenarios)) {
	scr_name = list_scenarios[i_scr]
	if(scr_name == "Historical_baseline") {
		begin_date = as.Date("1979-01-01", "%Y-%m-%d")
		end_date = as.Date("2015-12-31", "%Y-%m-%d")
	} else if (length(grep("hist", scr_name)) == 1) {
		begin_date = as.Date("1950-01-01", "%Y-%m-%d")
		end_date = as.Date("2005-12-31", "%Y-%m-%d")
	} else {
		begin_date = as.Date("2006-01-01", "%Y-%m-%d") 
		end_date = as.Date("2094-12-31", "%Y-%m-%d")
	}
	nrows = as.numeric(end_date - begin_date + 1)
	irrig_demand = data.frame(matrix(ncol=length(POD_names[,1]), nrow=nrows, 0))
	interruptible_demand = data.frame(matrix(ncol=length(POD_names[,1]), nrow=nrows, 0))	
	missing_cells = vector(length=0)
	for (i_pod in 1:length(POD_names[,1])){
		irrig_sum = rep(0, nrows)
		gridcell_lines = which(POD_file$NewStnNo3 == POD_names[i_pod,1])
		interruptible = rep(0, nrows)
		print(paste("now doing:", POD_names[i_pod,2]))
		if (length(gridcell_lines) == 0) {
			next
		} else {
			print(paste("name=", POD_names[i_pod,2], "---------", "number of cells=",length(gridcell_lines)))
			for (i_cell in 1:length(gridcell_lines)){
				line_no = gridcell_lines[i_cell]
				cell = which(crop_param[,1]==POD_file[line_no,1])[1]
				grid_area = POD_file$Area[line_no]
				surfaceFraction = 1 - POD_file$GWFraction[line_no]
				returnFlowFraction = POD_file$Return[line_no]
				#paperInterruptible = paperInterruptible + POD_file$InterruptibleQuantity[line_no]
				number_crops = crop_param[cell,2] 
				if(number_crops>0){ 
					f_name = paste("~/crop_files/", scr_name, "/crop_",  POD_file$Lat[line_no] , "_", POD_file$Lon[line_no], ".part.csv", sep="")
					if (!file.exists(f_name)) {
						next
					}
					ts_irrig = read.csv(f_name, header=T)[,1]
					#baseflow_runoff = baseflow_runoff + (flux_file[,6] + flux_file[,7]) * grid_area
					for(i_crop in 1:number_crops){ 
						total_rows = length(ts_irrig)
						select_rows = seq(from=i_crop, to=(total_rows - number_crops + i_crop), by=number_crops)
						fractional_area = crop_param[cell+i_crop,3] # fraction of grid that is irrigated for the given crop
						avg_irrig = ts_irrig[select_rows] * fractional_area # irrigation requirement mm/day      
						if (length(avg_irrig) != length(irrig_sum)) {
							missing_cells = c(missing_cells,POD_file$CellID[line_no])	
						}
						irrig_sum = irrig_sum + avg_irrig * grid_area * surfaceFraction * (1 + returnFlowFraction) # irrigation in mm*km^2/day for each day  
						if (crop_param$CurtailID[cell+i_crop] == 1) {
							interruptible_fraction = crop_param$IntFraction[cell+i_crop]
							interruptible = interruptible + interruptible_fraction * avg_irrig * grid_area * (1 + returnFlowFraction)
						}
					}				
				} else {
					print(paste(cell, "------" , "0  -no irrig"))
				}
			}	
		}
		irrig_demand[1:length(irrig_sum),i_pod] = irrig_sum * unit_conversion
		interruptible_demand[1:length(irrig_sum),i_pod] = interruptible * unit_conversion
	}
	if (!dir.exists(paste("~/Step_3/Aggregate_demand_ColSim/output/", strsplit(scr_name, "/")[[1]][1], sep=""))) {
		dir.create(paste("~/Step_3/Aggregate_demand_ColSim/output/", strsplit(scr_name, "/")[[1]][1], sep=""))
	}
	if (scr_name == "Historical_baseline") {
		write.table(irrig_demand, paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "/baseline_demands.txt", sep=""), row.names=F, col.names=F)
		write.table(interruptible_demand, paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "/baseline_interruptible_demands.txt", sep=""), row.names=F, col.names=F) 
	} else {
		write.table(irrig_demand, paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "_demands.txt", sep=""), row.names=F, col.names=F)
		write.table(interruptible_demand, paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "_interruptible_demands.txt", sep=""), row.names=F, col.names=F)
	}
}

  
