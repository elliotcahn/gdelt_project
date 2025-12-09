#!/usr/bin/env Rscript
setwd("/Users/xy/Desktop/605hw/projecet/")
suppressMessages({
  library(data.table)   
  library(lmtest)       
  library(sandwich)     
})

csv_file <- "gdelt_final_2018_2022.csv"
month_data <- fread(csv_file)

month_data[, Month := as.Date(Month)]
setorder(month_data, Month)

needed_cols <- c("AvgTone", "GoldsteinScale",
                 "S_PC", "S_VF", "S_PF",
                 "Ct", "Pt")
missing <- setdiff(needed_cols, names(month_data))
if (length(missing) > 0L) {
  stop("Missing columns in data: ", paste(missing, collapse = ", "))
}

cat("==== Model 1: AvgTone ~ GoldsteinScale (OLS) ====\n\n")

mod1 <- lm(AvgTone ~ GoldsteinScale, data = month_data)
print(summary(mod1))
cat("\n")

pearson  <- cor(month_data$AvgTone, month_data$GoldsteinScale,
                use = "complete.obs", method = "pearson")
spearman <- cor(month_data$AvgTone, month_data$GoldsteinScale,
                use = "complete.obs", method = "spearman")
cat(sprintf("Pearson  corr = %.4f\n", pearson))
cat(sprintf("Spearman corr = %.4f\n\n", spearman))


cat("==== Model 2: Dynamic model with lag (Newey-West SE) ====\n\n")

month_data[, AvgTone_lag := shift(AvgTone, 1)]
dat2 <- month_data[!is.na(AvgTone_lag)]

mod2 <- lm(AvgTone ~ AvgTone_lag + S_PC + S_VF + S_PF, data = dat2)

nw_se <- NeweyWest(mod2, lag = 1)
print(coeftest(mod2, vcov = nw_se))
cat("\n")


cat("==== Model 3: Difference model ΔAvgTone ~ ΔPt + ΔCt (OLS) ====\n\n")

month_data[, d_AvgTone := AvgTone - shift(AvgTone, 1)]
month_data[, dPt      := Pt      - shift(Pt, 1)]
month_data[, dCt      := Ct      - shift(Ct, 1)]

dat3 <- month_data[!is.na(d_AvgTone) & !is.na(dPt) & !is.na(dCt)]

mod3 <- lm(d_AvgTone ~ dPt + dCt, data = dat3)
print(summary(mod3))
cat("\n")

cat("All three models have been estimated.\n")
