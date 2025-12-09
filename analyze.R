
library(data.table)
library(lubridate)


files <- list.files(pattern = "gdelt_\\d{4}_\\d{2}\\.csv")
message(paste("Found", length(files), "monthly files to process."))

if(length(files) == 0) stop("No CSV files found!")

process_file <- function(f) {
  target_year <- as.numeric(substr(f, 7, 10))
  
  dt <- fread(f, header = TRUE)
  dt[, AvgTone := as.numeric(AvgTone)]
  dt[, Date := ymd(SQLDATE)]
  
  dt <- dt[year(Date) == target_year]
  
  dt[, `:=`(
    is_VC = (QuadClass == 1),
    is_PC = (QuadClass == 2),
    is_VF = (QuadClass == 3),
    is_PF = (QuadClass == 4)
  )]
  
  monthly_stats <- dt[, .(
    AvgTone = mean(AvgTone, na.rm = TRUE),
    GoldsteinScale = mean(GoldsteinScale, na.rm = TRUE),
    N_Total = .N,
    N_VC = sum(is_VC, na.rm=TRUE),
    N_PC = sum(is_PC, na.rm=TRUE),
    N_VF = sum(is_VF, na.rm=TRUE),
    N_PF = sum(is_PF, na.rm=TRUE)
  ), by = .(Month = floor_date(Date, "month"))]
  
  return(monthly_stats)
}

all_data <- rbindlist(lapply(files, process_file))

final_df <- all_data[, .(
  AvgTone = weighted.mean(AvgTone, N_Total),
  GoldsteinScale = weighted.mean(GoldsteinScale, N_Total),
  S_VC = sum(N_VC) / sum(N_Total),
  S_PC = sum(N_PC) / sum(N_Total),
  S_VF = sum(N_VF) / sum(N_Total),
  S_PF = sum(N_PF) / sum(N_Total)
), by = Month]

final_df[, `:=`(
  Ct = S_VF + S_PF,
  Pt = S_PC + S_PF  
)]

setorder(final_df, Month)
print(final_df)


fwrite(final_df, "gdelt_final_2018_2022.csv")
message("Success! Full dataset saved to gdelt_final_2018_2022.csv")