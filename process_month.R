#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript process_month.R <YEAR> <MONTH>")
}

year_in <- as.integer(args[1])
month_in <- as.integer(args[2])

library(data.table)

base_url <- "http://data.gdeltproject.org/events/"


select_cols <- c(2, 30, 31, 35)
col_names <- c("SQLDATE", "QuadClass", "GoldsteinScale", "AvgTone")

start_date <- as.Date(paste(year_in, month_in, "01", sep="-"))


end_date <- seq(start_date, by = "month", length.out = 2)[2] - 1
date_seq <- seq(start_date, end_date, by = "day")
file_dates <- format(date_seq, "%Y%m%d")


print(paste("Processing", length(file_dates), "days for:", format(start_date, "%Y-%m")))


monthly_data <- list()

for (d_str in file_dates) {
  file_name <- paste0(d_str, ".export.CSV.zip")
  url <- paste0(base_url, file_name)
  
  zip_file <- paste0("temp_", d_str, ".zip")
  
  tryCatch({
    download.file(url, zip_file, mode = "wb", quiet = TRUE)
    
    dt <- fread(cmd = paste("unzip -p", zip_file), 
                select = select_cols,
                header = FALSE,
                showProgress = FALSE)
    
    setnames(dt, col_names)
    
    if (!is.numeric(dt$AvgTone)) {
        warning(paste("Date", d_str, ": AvgTone column seems non-numeric. Check column indices."))
    }

    monthly_data[[d_str]] <- dt
    
    unlink(zip_file)
    
  }, error = function(e) {

    message(paste("Failed or missing data for:", d_str, "-", e$message))
    if (file.exists(zip_file)) unlink(zip_file)
  })
}


full_month_dt <- rbindlist(monthly_data)


out_file <- paste0("gdelt_", year_in, "_", sprintf("%02d", month_in), ".csv")

print(paste("Writing", nrow(full_month_dt), "rows to", out_file))

fwrite(full_month_dt, out_file)

print("Done.")