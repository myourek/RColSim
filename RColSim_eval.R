setwd("/data/hydro/users/Forecast_2026/ForecastProject/Forecast_data_files/RColSim/")
plot_dir = paste(getwd(), "/plots/", sep="")
library(xts)

obs_reg = read.csv("reg_streamflow.csv", header=T)
obs_nat = read.csv("NRNI_daily.csv", header=T) 
sim_reg = read.table(paste("out_RColSim/Forecast_with_CO2/supply_and_demand/Historical_baseline/dams_out.txt", sep=""), header=T)[,-1]
sim_reg = sim_reg / 13.8843
sim_nat = read.table("supply_baseline.txt", header=T) 
sim_nat[,-c(1:4)] = sim_nat[,-c(1:4)] / 13.8843


begin_date = as.Date("1979-01-01", "%Y-%m-%d")
end_date = as.Date("2007-12-31", "%Y-%m-%d")
dates = seq(from=begin_date, to=end_date, by=1)
n_rows= length(dates)
start_index = which(as.Date(obs_reg$DATE, "%m/%d/%Y") == begin_date)
end_index = which(as.Date(obs_reg$DATE, "%m/%d/%Y") == end_date)

xts_obs_reg = xts(obs_reg[start_index:end_index,-1], as.Date(1:n_rows, origin=begin_date-1))
xts_obs_nat = xts(obs_nat[start_index:end_index,-1], as.Date(1:n_rows, origin=begin_date-1))


weekly_obs_reg = apply.weekly(xts_obs_reg, mean)
weekly_obs_nat = apply.weekly(xts_obs_nat, mean)

stns = names(weekly_obs_nat)
stns_out = names(sim_reg)
stns_nat = names(sim_nat)[-c(1,2,3,4)]
start_wk = which(row.names(as.data.frame(weekly_obs_nat))=="1979-08-05")
end_wk = which(row.names(as.data.frame(weekly_obs_nat))=="2007-07-29")

obs_reg_weekly = weekly_obs_reg[start_wk:end_wk,]
obs_nat_weekly = weekly_obs_nat[start_wk:end_wk,]

start_wk = which(sim_nat$Month==8 & sim_nat$Day==5 & sim_nat$Year==1979)
end_wk = which(sim_nat$Month==7 & sim_nat$Day==29 & sim_nat$Year==2007)

sim_nat_weekly = data.frame(matrix(nrow=(end_wk - start_wk + 1), ncol=length(names(weekly_obs_nat))))
sim_reg_weekly = data.frame(matrix(nrow=(end_wk - start_wk + 1), ncol=length(names(weekly_obs_nat))))
for (i in 1:length(stns)) {
	#print(i)
	colnu_1 = which(stns_out == stns[i])
	colnu_2 = which(stns_nat == stns[i])
	sim_reg_weekly[1:(end_wk - start_wk + 1),i] = sim_reg[start_wk:end_wk,colnu_1]
	sim_nat_weekly[1:(end_wk - start_wk + 1),i] = sim_nat[start_wk:end_wk,colnu_2+4]
}
names(sim_reg_weekly) = stns
names(sim_nat_weekly) = stns

#for (var in c("sim_nat_weekly", "sim_reg_weekly", "obs_nat_weekly", "obs_reg_weekly")) {
#	assign(var, get(var) * 13.8843 / 1000)
#}

var_names = c("sim_nat", "sim_reg", "obs_reg", "obs_nat")
vars = list(sim_nat_weekly, sim_reg_weekly, obs_reg_weekly, obs_nat_weekly)
timeseries = sim_nat[start_wk:end_wk,1:4]
for(i in 1:length(vars)) {
	assign(paste(var_names[i], "weekly", sep="_"), data.frame(timeseries, vars[[i]]))
	vars = list(sim_nat_weekly, sim_reg_weekly, obs_reg_weekly, obs_nat_weekly)
	assign(paste(var_names[i], "monthly", sep="_"), aggregate(vars[[i]][-(1:4)], list(vars[[i]]$Month, vars[[i]]$Year), mean))
}
ncols_wk = length(obs_reg_weekly[1,])
ncols_mo = length(obs_reg_monthly[1,])
obs_sig_weekly = obs_nat_weekly
sim_sig_weekly = sim_nat_weekly
obs_sig_monthly = obs_nat_monthly
sim_sig_monthly = sim_nat_monthly

names(obs_reg_monthly) = c("Month", "Year", stns)
names(obs_nat_monthly) = c("Month", "Year", stns)
names(sim_nat_monthly) = c("Month", "Year", stns)
names(sim_reg_monthly) = c("Month", "Year", stns)
names(obs_sig_monthly) = c("Month", "Year", stns)
names(sim_sig_monthly) = c("Month", "Year", stns)

obs_sig_weekly[,5:ncols_wk] = obs_reg_weekly[,5:ncols_wk] - obs_nat_weekly[,5:ncols_wk]
sim_sig_weekly[,5:ncols_wk] = sim_reg_weekly[,5:ncols_wk] - sim_nat_weekly[,5:ncols_wk]
obs_sig_monthly[,3:ncols_mo] = obs_reg_monthly[,3:ncols_mo] - obs_nat_monthly[,3:ncols_mo]
sim_sig_monthly[,3:ncols_mo] = sim_reg_monthly[,3:ncols_mo] - sim_nat_monthly[,3:ncols_mo]

xmin_wk = which(timeseries$Year==2006 & timeseries$Month==8)[1]
xmax_wk = which(timeseries$Year==2006 & timeseries$Month==8)[1]
xmin_mo = which(sim_reg_monthly$Year==1981 & sim_reg_monthly$Month==8)
xmax_mo = which(sim_reg_monthly$Year==2006 & sim_reg_monthly$Month==8)

obs_sig_mean_monthly = aggregate(obs_sig_monthly[,3:ncols_mo], list(obs_sig_monthly$Month), function(x) {mean(x,na.rm=T)})
sim_sig_mean_monthly = aggregate(sim_sig_monthly[,3:ncols_mo], list(sim_sig_monthly$Month), mean)

par(mar=c(0,0,0,0))
eval_plot <- function(flow, timestep) {
  if(timestep == "monthly") {
    col_start = 3
    col_end = ncols_mo
    xmin = xmin_mo
    xmax = xmax_mo
  } else if (timestep == "weekly") {
    col_start = 5
    col_end = ncols_wk
    xmin = xmin_wk
    xmax = xmax_wk
  } else if (timestep == "mean_monthly") {
    col_start = 2
    col_end = length(stns) + 1
    xmin = 1
    xmax = 12
  } else {
    print("out of range, select monthly, weekly, or mean_monthly")
  }
  assign("obs", get(paste("obs_", flow, "_", timestep, sep="")))
  assign("sim", get(paste("sim_", flow, "_", timestep, sep="")))
  for (i in col_start:col_end) {
    if (flow == "sig") {
      ymin = 1.4 * min(min(sim[xmin:xmax,i]), min(obs[xmin:xmax,i]))
    } else {
      ymin = 0 
    }
    ymax = 1.4 * max(max(obs[xmin:xmax,i], na.rm=T), max(obs[xmin:xmax,i], na.rm=T))		
    png(paste(plot_dir, flow, "_", timestep, "_", stns[i-(col_start-1)], ".png", sep=""), width=1500, height=800, units="px", res=200)
    plot(obs[xmin:xmax,i], ylim=c(ymin, ymax), type='l', ylab="cubic feet per second")
    lines(sim[xmin:xmax,i], lty=2, col="red")
    dev.off()
  }
}


eval_plot(flow="reg", timestep="monthly")
eval_plot(flow="reg", timestep="weekly")
eval_plot(flow="sig", timestep="mean_monthly")


