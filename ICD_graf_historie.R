rm(list = ls())

# ICD - historie graf

library(ggplot2)

hodnoty <- c(7,9,10,10,9,10,7,10,10,14,33,NA)
roky <- c(1893,1900,1909,1919,1929,1938,1948,1955,1965,1975,1989,2022)
popis_ICD <- c("ICD0",
               "ICD1",
               "ICD2",
               "ICD3",
               "ICD4",
               "ICD5",
               "ICD6",
               "ICD7",
               "ICD8",
               "ICD9",
               "ICD10",
               "ICD11")
data <- data.frame(roky,hodnoty, popis_ICD)

par(family = "Georgia")
bp <- barplot(data$hodnoty, names.arg = data$roky, xlab = "Rok revize", ylab = "Počet let platnosti klasifikace", ylim = c(0,40))
abline(v = 7.3)
text(x = bp, y = data$hodnoty, label = data$hodnoty, pos = 3, cex = 0.8, col = "black")
text(x = bp, y = 3, label = data$popis_ICD, cex = 0.9, col = "black", srt = 90, font = 2)
text(8,18, "WHO ve vedení MKN", pos = 2, srt = -90)
text(13.9,10,"?", font = 2)
arrows(x0 = 8.5,
       y0 = 20,
       x1 = 9.5,
       y1 = 20)


