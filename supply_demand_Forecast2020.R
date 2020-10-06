###################################################################
# make it weekly and make the timing consistent with streamflow 
###########################################################
#
# check daily bias corrected flow-turned off
##########################################################

get_iflow = function(time_series, station) {
	ifile = read.table(paste('~/Bias_correction/flow_weekly_bc/', station, '_iflow_rules', sep=""))
	Months = as.numeric(strftime(time_series,"%m"))
	Days = as.numeric(strftime(time_series,"%d"))
	instream = as.numeric(matrix(nrow=length(Days), ncol=1, 0))
	for (line in 1:length(ifile[,1])) {
		mo = ifile[line,1]
		first_day = ifile[line,2]
		last_day = ifile[line,3]
		instream[Months==mo & Days>=first_day & Days<=last_day] = ifile[line,4]
	}
	return(instream)
}

write_time_series = function(start_index,end_index) {
	Months = as.numeric(strftime(dates[start_index:end_index],"%m"))
	Days = as.numeric(strftime(dates[start_index:end_index],"%d"))
	Years = as.numeric(strftime(dates[start_index:end_index],"%Y"))
	L = length(dates[start_index:end_index])
	Weeks = 1:L %% 52
	Weeks[which(Weeks==0)] = 52
	# since the number of weeks in a year is not exactly 52, we need to adjust the weeks to ensure that week 1 is always the first week in August
	# we do this by introducing an extra long 52nd "week" whenever the week 1 would fall in July 
	LeapYear_offset = seq(from=1948, to=2096, by=4) - 1 # sequence of leap years - 1
	first_mismatch = which(Weeks==1 & Months!=8)[1]
	Weeks_final = Weeks
	mismatch = first_mismatch
	while(mismatch < length(Months)) {
		if((Years[mismatch] %in% LeapYear_offset) | Days[mismatch+1]==6) {
			times_repeat = 5
		} else {
			times_repeat = 6
		}
		Weeks_final[mismatch:(mismatch+52*times_repeat)] = c(52,rep(1:52,times_repeat))
		mismatch = mismatch + 52 * times_repeat + 1
	}
	Weeks_final = Weeks_final[1:L]
	Weeks_final[L] = 52
	return(data.frame(Weeks_final, Months, Days, Years))
}

library(xts)
setwd('~/Bias_correction/')
stn_list = c("ALBEN", "ARROW", "BONFE", "BONNE", "BOUND", "BROWN", "CABIN", "CHIEF", "COLFA", "COLKE", "CORRA", "DALLE", 
	"DUNCA", "DWORS", "FLAPO", "FLASF", "GCOUL", "HCANY", "ICEHA", "JDAYY", "LIBBY", "LISPO", "LGOOS", "LGRAN", "LMONU", "MCNAR", "MICAA",
	"METPA", "MILNE", "NOXON", "OKANA", "OKANO", "OXBOW", "PALIS", "PRIRA", "REVEL", "RISLA", "ROCKY", "SIMNI", "SPALD", "WALST", "WANAP", 
	"WANET", "WELLS", "WENMO", "WENPE", "WILFA", "YAPAR")
pod_stns = read.table("~/Step_3/Aggregate_demand_ColSim/Stn_code_name.txt", header=T, stringsAsFactors=F)[,2]
stn_colsim = read.csv("~/Step_3/Aggregate_demand_ColSim/RColSim_stations.txt", sep="\t", stringsAsFactors=F, header=F)[,1]
stn_iflow = c("SIMNI","METPA","WENMO","WENPE","OKANA","OKANO", "COLKE", "LISPO")
mainstem_names = c("CHIEF", "DALLE", "JDAYY", "MCNAR", "PRIRA", "ROCKY", "RISLA", "WANAP", "WELLS")
gcm_list = read.table("bc_Columbia/bias_correct_0.125_Natflow/scenario_list", stringsAsFactors=F)[-1,1]
scr_list = c("hist", "rcp45", "rcp85")
list_scenarios = vector(length=3*length(gcm_list)+1)
for (i in 1:length(scr_list)) {
	j = (i - 1) * length(gcm_list) + 2
	list_scenarios[j:(i*length(gcm_list)+1)] = sapply(gcm_list, function(x) {paste(x, scr_list[i], sep="/")})
}
list_scenarios[1] = "Historical_baseline"
list_scenarios = c("Historical_baseline", "bcc-csm1-1/rcp45")

for(i_scr in 1:length(list_scenarios)) {
	scr_name = list_scenarios[i_scr]
	print(paste("now doing:", scr_name))
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
	daily_supply = data.frame(matrix(ncol=(length(stn_list)+3), nrow=nrows, -9999))
	for (ii_stn in 1:length(stn_list)){
		flow_raw_month = read.table(paste("/home/fscarpare/Routing/Output/", scr_name, "/", stn_list[ii_stn], ".month", sep=""), header=F)
		flow_bc_month = read.table(paste("~/Bias_correction/bc_Columbia/bias_correct_0.125_Natflow/final_to_RColSim/", scr_name, "/" , stn_list[ii_stn], ".month.bc.ff", sep=""), header=F)
		flow_raw_day = read.table(paste("/home/fscarpare/Routing/Output/", scr_name, "/", stn_list[ii_stn], ".day", sep=""), header=F)
		flow_bc_day = flow_raw_day
		SIZE_DF = length(flow_raw_day[,1])		
		number_of_month = (length(unique(flow_raw_month[,1]))) * 12
		for (ii_mo in 1:number_of_month) {
			month = flow_raw_month[ii_mo,2]
			year = flow_raw_month[ii_mo,1]
			diff_month = flow_raw_month[ii_mo,3] / flow_bc_month[ii_mo,]
			corresponding_month = which(flow_raw_day[,2]==month & flow_raw_day[,1]==year)			
			#print(paste(month, "#########",year))
			#print(paste("diff===", diff_month))
			flow_bc_day[corresponding_month,4] = flow_raw_day[corresponding_month,4] / diff_month
		}
		daily_supply[,(ii_stn+3)] = flow_bc_day[,4]
	}
	daily_supply[,1:3] = flow_raw_day[,1:3]
	write.table(daily_supply, file=paste("flow_weekly_bc/bias_corrected_Forecast/daily/", sub("/", "_", scr_name), ".txt" , sep=""), row.names=F, col.names=F) 
	
	if (scr_name == "Historical_baseline") {
		daily_demand = read.table(paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "/baseline_demands.txt", sep=""))
		daily_interruptible_demand = read.table(paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "/baseline_interruptible_demands.txt", sep=""))
	} else {
		daily_demand = read.table(paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "_demands.txt", sep=""))
		daily_interruptible_demand = read.table(paste("~/Step_3/Aggregate_demand_ColSim/output/", scr_name, "_interruptible_demands.txt", sep=""))
	}
	L_col1 = length(stn_list) + 3
	L_col2 = length(pod_stns)
	xts_supply = xts(daily_supply[1:nrows,4:L_col1], seq(from=begin_date, to=end_date, by=1))
	xts_demand = xts(daily_demand[1:nrows,1:L_col2], seq(from=begin_date, to=end_date, by=1))
	xts_interruptible_demand = xts(daily_interruptible_demand[1:nrows,1:L_col2], seq(from=begin_date, to=end_date, by=1))
	weekly_supply = apply.weekly(xts_supply, mean)
	weekly_demand = apply.weekly(xts_demand, mean)
	interruptible_weekly = apply.weekly(xts_interruptible_demand, mean)

	dates = as.Date(row.names(as.data.frame(weekly_supply)),"%Y-%m-%d")	
	iflows = matrix(nrow=length(dates), ncol=length(pod_stns), 0)
	names(interruptible_weekly) = pod_stns
	write.table(daily_supply, file=paste("flow_weekly_bc/bias_corrected_Forecast/daily/", sub("/", "_", scr_name), ".txt" , sep=""), row.names=F, col.names=F) 
	for(i in 1:length(stn_iflow)) {
		col = which(pod_stns==stn_iflow[i])
		if(length(col) == 0) {
			next
		}
		iflows[,col] = get_iflow(dates, stn_iflow[i])	
	}	

	curtailment = matrix(nrow=length(dates), ncol=length(pod_stns), 0)
	instream_shortfall = matrix(nrow=length(dates), ncol=length(pod_stns), 0)
	for (i in 1:length(pod_stns)) {
		col = which(stn_list==pod_stns[i])
		if (length(col) > 0) {
			arg = as.numeric(apply(cbind(iflows[,i] + weekly_demand[,i] - weekly_supply[,col], rep(0, length(dates))), 1, max))
			#curtailment[,i] = apply(cbind(weekly_demand[,i], arg), 1, min)	
			curtailment[,i] = ifelse(arg>0, interruptible_weekly[,i], 0)
			remaining_instream = weekly_supply[,col] - (weekly_demand[,i] - curtailment[,i])
			instream_shortfall[,i] = iflows[,i] - apply(cbind(remaining_instream, iflows[,i]), 1, min)
		}	
	}
	curtailment[,!(pod_stns %in% stn_iflow)] = 0
	
	adj_demand = weekly_demand - curtailment
	map_stations = read.csv("~/Step_3/Aggregate_demand_ColSim/station_mapping.txt", sep="\t", header=F, stringsAsFactors=F)
	weekly_demand_agg = adj_demand[,which(pod_stns %in% stn_colsim)]
	new_pod_names = pod_stns[pod_stns %in% stn_colsim]
	for(i in 1:length(new_pod_names)) {
		stations_to_sum = map_stations[map_stations[,1] == new_pod_names[i],2]
		weekly_demand_agg[,i] = apply(adj_demand[,match(stations_to_sum, pod_stns)], 1, sum)
	}
		
	if (scr_name == "Historical_baseline") {
		start_index = which(dates==as.Date("1979-08-05", "%Y-%m-%d"))
		end_index = which(dates==as.Date("2015-07-26", "%Y-%m-%d"))
	} else if (length(grep("hist", scr_name)) == 1) {
		start_index = which(dates==as.Date("1950-08-06", "%Y-%m-%d"))
		end_index = which(dates==as.Date("2005-07-31", "%Y-%m-%d"))
	} else {
		start_index = which(dates==as.Date("2006-08-06", "%Y-%m-%d"))
		end_index = which(dates==as.Date("2094-07-25", "%Y-%m-%d"))
	}
	
	#time_table = write_time_series(start_index, end_index)
	#if (scr_name == "Historical_baseline") {
#		write.table(time_table, "flow_weekly_bc/ts_historical.txt")
	#} else if (length(grep("hist", scr_name)) == 1) {
	#	write.table(time_table, "flow_weekly_bc/ts_hist.txt")
	#} else {
	#	write.table(time_table, "flow_weekly_bc/ts_GCM.txt")
	#}
	#time_table = write_time_series(start_index, end_index)
	#write.table(time_table, "flow_weekly_bc/ts_hist.txt")

	final_weekly_supply = matrix(nrow=length(weekly_supply[,1]), ncol=length(stn_colsim), 0)
	for (ii_s in 1:length(stn_colsim)) { # this part changes the order of columns and make it consistent with ColSim's input files
		col_nu = which(stn_list==stn_colsim[ii_s])
		#print(paste(stn_list[col_nu], " = ", stn_colsim[ii_s], sep=""))
		final_weekly_supply[,ii_s] = weekly_supply[,col_nu]		
	}	
	write.table(final_weekly_supply[start_index:end_index,], file=paste("flow_weekly_bc/bias_corrected_Forecast/weekly/", sub("/", "_", scr_name), ".txt", sep=""), row.names=F, col.names=F) #2nd paper-future

	if (scr_name == "Historical_baseline") {
		ts_step4 = read.table("~/Bias_correction/flow_weekly_bc/ts_historical.txt") 
	} else if (length(grep("hist", scr_name)) == 1) {
		ts_step4 = read.table("~/Bias_correction/flow_weekly_bc/ts_hist.txt")
	} else {
		ts_step4 = read.table("~/Bias_correction/flow_weekly_bc/ts_GCM.txt")
	}
	
	supply = cbind(ts_step4, final_weekly_supply[start_index:end_index,])
	demand = cbind(ts_step4, data.frame(weekly_demand_agg[start_index:end_index,]))
	shortfall = cbind(ts_step4, instream_shortfall[start_index:end_index,match(stn_iflow,pod_stns)])
	iflow = cbind(ts_step4, iflows[start_index:end_index,match(stn_iflow,pod_stns)])
	curtailment = cbind(ts_step4, curtailment[start_index:end_index,match(stn_iflow,pod_stns)])
	mainstem_interruptible = cbind(ts_step4, data.frame(interruptible_weekly[start_index:end_index, match(mainstem_names,pod_stns)]))
	names(supply) = c("Week", "Month", "Day", "Year", stn_colsim)
	names(demand) = c("Week", "Month", "Day", "Year", new_pod_names)
	names(shortfall) = c("Week", "Month", "Day", "Year", pod_stns[match(stn_iflow,pod_stns)])
	names(iflow) = c("Week", "Month", "Day", "Year", pod_stns[match(stn_iflow,pod_stns)])
	names(curtailment) = c("Week", "Month", "Day", "Year", pod_stns[match(stn_iflow,pod_stns)])
	names(mainstem_interruptible) = c("Week", "Month", "Day", "Year", pod_stns[match(mainstem_names,pod_stns)])
	
	if (!dir.exists(paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], sep=""))) {
		dir.create(paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], sep=""))
	}
	
	if (scr_name == "Historical_baseline") {
		write.table(demand, paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/Historical_baseline/demand_baseline.txt", sep=""), row.names=F)
		write.table(supply, paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/Historical_baseline/supply_baseline.txt", sep=""), row.names=F)
		write.table(shortfall, paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/Historical_baseline/shortfall_baseline.txt", sep=""), row.names=F)
		write.table(iflow, paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/Historical_baseline/iflow_baseline.txt", sep=""), row.names=F)
		write.table(curtailment, paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/Historical_baseline/curtailment_baseline.txt",  sep=""), row.names=F)
		write.table(mainstem_interruptible, paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/Historical_baseline/interruptible_baseline.txt",  sep=""), row.names=F)
	} else {
		write.table(demand, file=paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], "/", "demand_", strsplit(scr_name, "/")[[1]][2],  ".txt" , sep=""), row.names=F)
		write.table(supply, file=paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], "/", "supply_", strsplit(scr_name, "/")[[1]][2],  ".txt" , sep=""), row.names=F)
		write.table(shortfall, file=paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], "/", "shortfall_", strsplit(scr_name, "/")[[1]][2],  ".txt" , sep=""), row.names=F)
		write.table(iflow, file=paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], "/", "iflow_", strsplit(scr_name, "/")[[1]][2],  ".txt" , sep=""), row.names=F)
		write.table(curtailment, file=paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], "/", "curtailment_", strsplit(scr_name, "/")[[1]][2],  ".txt" , sep=""), row.names=F)
		write.table(interruptible, file=paste("~/Step_3/Aggregate_demand_ColSim/to_step_4/Supply_and_demand/", strsplit(scr_name, "/")[[1]][1], "/", "interruptible_", strsplit(scr_name, "/")[[1]][2],  ".txt" , sep=""), row.names=F)
	}
}












