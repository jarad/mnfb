
# Quit function
myquit = function() q(ifelse(interactive(),"ask","no"))

# Functions to return proper location for tables and figures
tab_dir = function(filename) {
  paste("tables/", filename, sep="")
}

fig_dir = function(filename) {
  paste("figs/", filename, sep="")
}

# Function to use forest name rather than code
# This should probably be updated to ensure the codes match the 
# forest names. 
refactor_forests = function(forest) {
  factor(forest, labels=c("Chequamegon","Chippewa","Superior"))
}



