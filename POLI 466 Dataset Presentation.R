setwd("~/Desktop")

# Import Data and Load Libraries
qog <- read.csv("qog_std_ts_jan22.csv")
library(stargazer)
library(ggplot2)

#######
# BCI Plots
# Histogram of BCI for all countries and years
hist(qog$bci_bci, xlim = c(0,100), xlab = "BCI (0-100)",
     main = "Bayesian Corruption Index 
(All Countries, All Years)", col = "lightseagreen")

# Subset US
us_dta <- qog[qog$cname == "United States of America (the)",]

# Plot US Data
plot(us_dta$year, us_dta$bci_bci, xlim = c(1946, 2021), pch = 16, 
     main = "US Bayesian Corruption Index (1946-2017)", 
     xlab = "Year", ylab = "BCI", col = "royalblue")
# add average BCI
abline(h = mean(us_dta$bci_bci, na.rm = TRUE))
# Add relevant years and labels
abline(v = 2008)
text(x = 2008, y = 32, "2008, Great Recession", cex = 0.9)
abline(v=2016)
text(x = 2016, y = 29, "2016, 
Presidential 
  Election", cex = 0.9)

# Subset Russia
russia_dta <- qog[qog$cname == "Russian Federation (the)",]
# Subset China
china_dta <- qog[qog$cname == "China",]
# Subset Denmark
denmark_dta <- qog[qog$cname == "Denmark",]

# Plot Comparative Plots
plot(russia_dta$year, russia_dta$bci_bci, xlim = c(1946, 2021), ylim = c(0,100),
     pch = 16, main = "Comparative Bayesian Corruption Index (1946-2017)", 
     xlab = "Year", ylab = "BCI", col = "tomato3")
points(us_dta$year, us_dta$bci_bci, pch = 16, col = "royalblue")
points(china_dta$year, china_dta$bci_bci, pch = 16, col = "tan1")
points(denmark_dta$year, denmark_dta$bci_bci, pch = 16, col = "mediumpurple1")
legend("topleft", c("Russia", "China", "United States", "Denmark"), 
       col = c("tomato3", "tan1", "royalblue", "mediumpurple1"), pch = 16)

# Regression of BCI and HDI relationship in 2017
# Subset 2017 data
recent <- qog[qog$year == "2017",]
# Simple regression
reg <- lm(undp_hdi ~ bci_bci , data = recent)
summary(reg)
# Plot regression with regression line and R squared, n label
plot(recent$bci_bci, recent$undp_hdi, type = "p", pch = 20,
     xlab = "BCI", 
     ylab = "HDI",
     main = "2017 Bayesian Corruption Index vs. 
     Human Development Index")
abline(reg, col = "red")
legend("topright", c("Adjusted R-squared:", 
                     as.character(round(summary(reg)$r.squared, digits = 4)),
                     "Number of Observations: 198"))
legend("bottomleft", c("Russia", "China", "United States", "Denmark"), 
       col = c("tomato3", "tan1", "royalblue", "mediumpurple1"), pch = 16)

# Find US point
US_x <- recent$bci_bci[recent$cname == "United States of America (the)"]
US_y <- recent$undp_hdi[recent$cname == "United States of America (the)"]

# Find Denmark point
Denmark_x <- recent$bci_bci[recent$cname == "Denmark"]
Denmark_y <- recent$undp_hdi[recent$cname == "Denmark"]

# Find China point
China_x <- recent$bci_bci[recent$cname == "China"]
China_y <- recent$undp_hdi[recent$cname == "China"]

# Find Russia point
Russia_x <- recent$bci_bci[recent$cname == "Russian Federation (the)"]
Russia_y <- recent$undp_hdi[recent$cname == "Russian Federation (the)"]

# Label points
points(US_x, US_y, col = "royalblue", pch = 19, cex = 2)
points(Denmark_x, Denmark_y, col = "mediumpurple1", pch = 19, cex = 2)
points(China_x, China_y, col = "tan1", pch = 19, cex = 2)
points(Russia_x, Russia_y, col = "tomato3", pch = 19, cex = 2)

# Print Regression table
stargazer(reg, type = "html", 
          out="bci.html",
          align=TRUE, dep.var.labels = c("Human Development Index"),
          covariate.labels= c("Bayesian Corruption Index"))

#######
# Political Parties Plots
# Histogram of all Party Data
hist(qog$cpds_enps, main = "Number of Parties that Hold Seats", col = "pink2", 
     xlab = "Number of Seats", xlim = c(0,10), ylim = c(0,600))

# Subset Certain Countries
qog1 <- qog[qog$cname == c("United States of America (the)", "Germany", "France",
                           "Australia"), ]
# Plot Trends over time
ggplot(data = qog1, aes(x = year, y = cpds_enps, color=cname)) +
  geom_line() +
  scale_fill_discrete(name="Country") + 
  scale_color_discrete(name="Country") +
  ggtitle("Number of Parties Over Time") +
  ylab("Number of Parties (Seats)") +
  theme(plot.title = element_text(hjust=0.5))

