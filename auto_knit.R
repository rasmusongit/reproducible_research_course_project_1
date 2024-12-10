library(fs)
library(knitr)

watch_file <- function(file) {
  last_mtime <- file_info(file)$modification_time
  
  repeat {
    Sys.sleep(1) # Check every second
    current_mtime <- file_info(file)$modification_time
    
    if (!identical(last_mtime, current_mtime)) {
      last_mtime <- current_mtime
      cat("File updated. Knitting...\n")
      try(knit2html(file))
    }
  }
}

watch_file("analysis.Rmd")
