# week_counter= time steps of model does NOT set to zero each year
# The Unit is AF/week
# reading the input file

VIC_Data <- function() {
	####### supply, demands, curtailment ########
	terms = c("Flow", "Dem", "Iflow", "Curt", "Refill", "Ret", "Residual")
	for (j in terms) {
		cols = grep(j, names(input_file))
		for (i in cols) {
			var = names(input_file)[i]
			assign(var, pos=1, input_file[week_counter + num_lines_to_skip,names(input_file)==var])
		}
	}
	cols = grep("Runoff", names(input_file))
	for (i in cols) {
		var = names(input_file)[i]
		assign(var, pos=1, input_file[year_counter + num_years_to_skip,names(input_file)==var])
	}
}


