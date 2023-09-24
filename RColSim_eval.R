setwd("~/Step_5/output/")
plot_dir <- paste(getwd(), "/plots/", sep="")
library(xts)

obs_reg <- read.csv("daily_historical_flow_2018.csv", header=T)
obs_nat <- read.csv("NRNI_2020_daily.csv", header=T) 
sim_reg <- read.table(paste("update_RColSim/supply_and_demand/Historical_baseline/dams_out.txt", sep=""), header=T, stringsAsFactors=F)
sim_reg[,-c(1:5)] <- sim_reg[,-c(1:5)] / 13.8843
sim_nat <- read.table("~/Step_3/Aggregate_demand_ColSim/to_step_4/update_RColSim/supply_and_demand/Historical_baseline/supply_baseline.txt", header=T) 
sim_nat[,-c(1:4)] <- sim_nat[,-c(1:4)] / 13.8843
sim_nat$DATE <- as.Date(paste(sim_nat$Years, sim_nat$Months, sim_nat$Days, sep="-"))
stns <- names(sim_nat)[-c(1:4)][which(names(sim_nat)[-c(1:4)] %in% names(obs_reg) & names(sim_nat)[-c(1:4)] %in% names(sim_reg))]


begin_date <- as.Date("1979-01-01", "%Y-%m-%d")
end_date <- as.Date("2007-12-31", "%Y-%m-%d")
dates <- seq(from=begin_date, to=end_date, by=1)
n_rows <- length(dates)
start_index <- which(as.Date(obs_reg$Date) == begin_date)
end_index <- which(as.Date(obs_reg$Date) == end_date)
weekly_obs_reg <- apply.weekly(xts(obs_reg[start_index:end_index,-1], as.Date(1:n_rows, origin=begin_date-1)), mean)
weekly_obs_nat <- apply.weekly(xts(obs_nat[start_index:end_index,-1], as.Date(1:n_rows, origin=begin_date-1)), mean)
weekly_dates <- row.names(as.data.frame(weekly_obs_reg))

start_wk <- as.Date("1979-08-05")
end_wk <- as.Date("2007-07-29")

obs_reg_weekly <- weekly_obs_reg[weekly_dates >= start_wk & weekly_dates <= end_wk,stns]
obs_nat_weekly <- weekly_obs_nat[weekly_dates >= start_wk & weekly_dates <= end_wk,stns]
sim_reg_weekly <- subset(sim_reg, as.Date(sim_reg$date) >= start_wk & as.Date(sim_reg$date) <= end_wk)[,stns]
sim_nat_weekly <- subset(sim_nat, DATE >= start_wk & DATE <= end_wk)[,stns]
var_names <- c("sim_nat", "sim_reg", "obs_reg", "obs_nat")
vars <- list(sim_nat_weekly, sim_reg_weekly, obs_reg_weekly, obs_nat_weekly)
ts_wk <- subset(sim_nat, DATE >= start_wk & DATE <= end_wk)[,c("Months", "Years", "DATE")]
ts_mo <- unique(subset(sim_nat, DATE >= start_wk & DATE <= end_wk)[,c("Months", "Years")])
ts_mo <- cbind(ts_mo, DATE=as.Date(paste(ts_mo$Years, ts_mo$Months, "01", sep="-")))
for(i in 1:length(vars)) {
	weekly_data <- data.frame(ts_wk, vars[[i]])
	assign(paste(var_names[i], "weekly", sep="_"), weekly_data[,-c(1:2)])
	assign(paste(var_names[i], "monthly", sep="_"), data.frame(DATE=ts_mo$DATE, aggregate(weekly_data[-c(1:3)], list(weekly_data$Months, weekly_data$Years), mean)[,-c(1:2)]))
}




obs_sig_weekly <- obs_nat_weekly
sim_sig_weekly <- sim_nat_weekly
obs_sig_monthly <- obs_nat_monthly
sim_sig_monthly <- sim_nat_monthly

names(obs_reg_monthly) <- c("Month", "Year", stns)
names(obs_nat_monthly) <- c("Month", "Year", stns)
names(sim_nat_monthly) <- c("Month", "Year", stns)
names(sim_reg_monthly) <- c("Month", "Year", stns)
names(obs_sig_monthly) <- c("Month", "Year", stns)
names(sim_sig_monthly) <- c("Month", "Year", stns)

obs_sig_weekly[,5:ncols_wk] <- obs_reg_weekly[,5:ncols_wk] - obs_nat_weekly[,5:ncols_wk]
sim_sig_weekly[,5:ncols_wk] <- sim_reg_weekly[,5:ncols_wk] - sim_nat_weekly[,5:ncols_wk]
obs_sig_monthly[,3:ncols_mo] <- obs_reg_monthly[,3:ncols_mo] - obs_nat_monthly[,3:ncols_mo]
sim_sig_monthly[,3:ncols_mo] <- sim_reg_monthly[,3:ncols_mo] - sim_nat_monthly[,3:ncols_mo]




xmin_wk <- which(timeseries$Year==1981 & timeseries$Month==8)[1]
xmax_wk <- which(timeseries$Year==2006 & timeseries$Month==8)[1]
xmin_mo <- which(sim_reg_monthly$Year==1981 & sim_reg_monthly$Month==8)
xmax_mo <- which(sim_reg_monthly$Year==2006 & sim_reg_monthly$Month==8)

obs_sig_mean_monthly <- aggregate(obs_sig_monthly[,3:ncols_mo], list(obs_sig_monthly$Month), function(x) {mean(x,na.rm=T)})
sim_sig_mean_monthly <- aggregate(sim_sig_monthly[,3:ncols_mo], list(sim_sig_monthly$Month), mean)


monthly

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


