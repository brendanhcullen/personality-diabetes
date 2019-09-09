
library(tidyverse)
library(here)

scripts_dir = here("output/machine_learning/training/scripts")
logs_dir = here("output/machine_learning/training/logs")

# use to indicate which set of scripts you want to run. 
pattern = "spi_135*"
#pattern = "spi_5"
#pattern = "*"

scripts = list.files(scripts_dir, full.names = TRUE, pattern = pattern)
script_names = script_names = list.files(scripts_dir, full.names = FALSE, pattern = pattern)

# create 'safe' version of 'source' function that outputs error messages
safe_source = safely(source)

# execute all selected .R scripts and create list of error messages

warning_file = file("RScriptErrors.log", open = "wt")
sink(warning_file, type = "message")
try(map(scripts, source))

tryCatch(source("/Users/brendancullen/Dropbox/SAP/personality-diabetes/output/machine_learning/training/scripts/knn_spi_135.R"))

safe_source_list = map(scripts, safe_source)

error_list = transpose(set_names(safe_source_list, script_names))$error

error_list = transpose(safe_source_list)$error


safe_source_list = set_names(safe_source_list_transposed, script_names)

log_name = paste0(names(error_list)[1], "_error")
write_file(err_ex, path = paste0(logs_dir, "/", log_name, ".txt"))


write_error_logs = function(x) {
  logs_dir = here("output/machine_learning/training/logs")
  log_name = names(x)
  
  
  if(!is_null(x[1])) {
    write(toString(x[[1]]$message), file = paste0(logs_dir, "/", log_name, "_error.txt"), append= TRUE)
  } else if (is_null(x[[1]])) {
    write(toString("No error to report!"), file = paste0(logs_dir, "/", log_name, "_error.txt"), append = TRUE)
  }

}

map(error_list, write_error_logs)
