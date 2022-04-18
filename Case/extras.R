## miscellaneous

## reading the MS Excel file
library(xlsx)
cashflows <- read.xlsx("./Cashflow.xlsx", sheetIndex = 1, 
                       colIndex = 1:2, rowIndex = 4:67)
names(cashflows) <- c("Year", "Estimate")

plot(cashflows$Year, cashflows$Estimate, type = "l", xlab = "Horizon", 
     ylab = "Cashflow" , main = "Estimated Cashflows", 
     ylim = c(0, max(cashflows$Estimate)))

