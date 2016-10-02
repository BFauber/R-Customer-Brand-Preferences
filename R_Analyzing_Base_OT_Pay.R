###############################################################
###############################################################
# Title: Analysis of City of San Francisco (California, USA) Employee 
# Payroll Data, 2011-2015
# Version: 0.1
# Date: 01Oct2016
# Authors@R: person("Ben", "Fauber",
#                  role = c("aut", "cre"))
# Description: Analyzes the City of San Francisco (SF) employee base pay, 
# overtime (OT) pay, and potential OT abuse from 2011-2015.
###############################################################
###############################################################

# install packages, load libraries

install.packages("ggplot2")
library(ggplot2)


###############################################################
# import the cleaned dataset (cleaned with Python)
###############################################################

sal <- read.csv("~/Documents/DataSci/RawData/SF_CitySalaries/output/Salaries_cleaned.csv")


###############################################################
# subset of all data for individual years, 2011-2015
###############################################################

sub2015 <- subset(sal, with(sal, (year==2015)))

sub2014 <- subset(sal, with(sal, (year==2014)))

sub2013 <- subset(sal, with(sal, (year==2013)))

sub2012 <- subset(sal, with(sal, (year==2012)))

sub2011 <- subset(sal, with(sal, (year==2011)))


###############################################################
# general functions
###############################################################

# pay statistics (min, Q1, median, mean, Q3, max) on:
# basePay, overtimePay, and overtimePercent (as a percent of basePay)


payStats <- function(dfName, id) { 
	store <- data.frame(
	id,
	nrow(dfName), 
	sum(as.numeric(dfName$basePay)),
	min(as.numeric(dfName$basePay)),
	quantile(as.numeric(dfName$basePay), 0.25),
	median(as.numeric(dfName$basePay)), 
	mean(as.numeric(dfName$basePay)),
	quantile(as.numeric(dfName$basePay), 0.75),
	max(as.numeric(dfName$basePay)), 
	sum(as.numeric(dfName$overtimePay)),
	min(as.numeric(dfName$overtimePay)),
	quantile(as.numeric(dfName$overtimePay), 0.25),
	median(as.numeric(dfName$overtimePay)),
	mean(as.numeric(dfName$overtimePay)),
	quantile(as.numeric(dfName$overtimePay), 0.75),
	max(as.numeric(dfName$overtimePay)),
	min(as.numeric(dfName$overtimePercent)),
	quantile(as.numeric(dfName$overtimePercent), 0.25),
	median(as.numeric(dfName$overtimePercent)),
	mean(as.numeric(dfName$overtimePercent)),
	quantile(as.numeric(dfName$overtimePercent), 0.75),
	max(as.numeric(dfName$overtimePercent)))
}


###############################################################
# pay stats for 2015-2011
###############################################################

# Year = 2015 stats
payStats2015 <- payStats(sub2015, 2015)

# Year = 2014 stats
payStats2014 <- payStats(sub2014, 2014)

# Year = 2013 stats
payStats2013 <- payStats(sub2013, 2013)

# Year = 2012 stats
payStats2012 <- payStats(sub2012, 2012)

# Year = 2011 stats
payStats2011 <- payStats(sub2011, 2011)

# Build the data table with the annual pay stats from 2015-2011
annualPayStats <- rbind(payStats2015, payStats2014, payStats2013, payStats2012, payStats2011)

colnames(annualPayStats) <- c("Year", "Number of Employees", "Sum of All
BasePay", "Min BasePay", "1st Quartile BasePay", "Median BasePay", "Mean
BasePay", "3rd Quartile BasePay", "Max BasePay", "Sum of All OvertimePay",
"Min OvertimePay", "1st Quartile OvertimePay", "Median OvertimePay", "Mean OvertimePay",
 "3rd Quartile OvertimePay", "Max OvertimePay", "Min OvertimePercent", "1st Quartile
OvertimePercent", "Median OvertimePercent", "Mean OvertimePercent", "3rd Quartile
 OvertimePercent", "Max OvertimePercent")

rownames(annualPayStats) <- NULL

# export annualPayStats to a CSV file
write.csv(annualPayStats, file = "Documents/DataSci/RawData/SF_CitySalaries/output/annualPayStats.csv")


###############################################################
# subset of all 2015 data for 10 most prevalent job title keywords in 2015
###############################################################

# 10 most prevalent job title keywords in 2015 as determined with a Python script

subTransit <- sub2015[grep("transit", sub2015$jobTitle), ]

subNurse <- sub2015[grep("nurse", sub2015$jobTitle), ]

subOperator <- sub2015[grep("operator", sub2015$jobTitle), ]

subOfficer <- sub2015[grep("officer", sub2015$jobTitle), ]

subClerk <- sub2015[grep("clerk", sub2015$jobTitle), ]

subPolice <- sub2015[grep("police", sub2015$jobTitle), ]

subEng1 <- sub2015[grep("engineer", sub2015$jobTitle), ]
subEng2 <- sub2015[grep("eng", sub2015$jobTitle), ]
subEng <- rbind(subEng1, subEng2)

subAnalyst <- sub2015[grep("analyst", sub2015$jobTitle), ]

subAide <- sub2015[grep(" aide", sub2015$jobTitle), ]

subFire <- sub2015[grep("fire", sub2015$jobTitle), ]


###############################################################
# pay stats for 10 most prevalent job title keywords in 2015
###############################################################

# Keyword = Transit stats
payStatsTransit <- payStats(subTransit, "Transit")

# Keyword = Nurse stats
payStatsNurse <- payStats(subNurse, "Nurse")

# Keyword = Operator stats
payStatsOperator <- payStats(subOperator, "Operator")

# Keyword = Officer stats
payStatsOfficer <- payStats(subOfficer, "Officer")

# Keyword = Clerk stats
payStatsClerk <- payStats(subClerk, "Clerk")

# Keyword = Police stats
payStatsPolice <- payStats(subPolice, "Police")

# Keyword = Eng stats
payStatsEng <- payStats(subEng, "Engineer")

# Keyword = Analyst stats
payStatsAnalyst <- payStats(subAnalyst, "Analyst")

# Keyword = Aide stats
payStatsAide <- payStats(subAide, "Aide")

# Keyword = Fire stats
payStatsFire <- payStats(subFire, "Fire")

# Build the data table with the annual pay stats of ten most prevalent job 
# title keywords in 2015
jobTitlePayStats <- rbind(payStatsTransit, payStatsNurse, payStatsOperator, payStatsOfficer, payStatsClerk, payStatsPolice, payStatsEng, payStatsAnalyst, payStatsAide, payStatsFire)

colnames(jobTitlePayStats) <- c("Job Title Keyword", "Number of Employees", "Sum of All
BasePay", "Min BasePay", "1st Quartile BasePay", "Median BasePay", "Mean
BasePay", "3rd Quartile BasePay", "Max BasePay", "Sum of All OvertimePay",
"Min OvertimePay", "1st Quartile OvertimePay", "Median OvertimePay", "Mean OvertimePay",
 "3rd Quartile OvertimePay", "Max OvertimePay", "Min OvertimePercent", "1st Quartile
OvertimePercent", "Median OvertimePercent", "Mean OvertimePercent", "3rd Quartile
 OvertimePercent", "Max OvertimePercent")

rownames(jobTitlePayStats) <- NULL

# export jobTitlePayStats to a CSV file
write.csv(jobTitlePayStats, file = "Documents/DataSci/RawData/SF_CitySalaries/output/jobTitlePayStats.csv")


###############################################################
# focus on the top percent overtime pay in top job title keywords in 2015
###############################################################

# Keyword = Fire export CSV

subFireS <- subFire[order(-subFire$overtimePercent),]

write.csv(subFireS, file = "Documents/DataSci/RawData/SF_CitySalaries/output/subFire2015.csv")


# Keyword = Transit export CSV

subTransitS <- subTransit[order(-subTransit$overtimePercent),]

write.csv(subTransitS, file = "Documents/DataSci/RawData/SF_CitySalaries/output/subTransit2015.csv")


# Keyword = Police export CSV

subPoliceS <- subPolice[order(-subPolice$overtimePercent),] 

write.csv(subPoliceS, file = "Documents/DataSci/RawData/SF_CitySalaries/output/subPolice2015.csv")


###############################################################
# focus on the top percent overtime pay employees in 2015, if base pay >33.4k 
# (1st quartile pay in 2015), this logic avoids capturing employees that received
# backpay from prior year (i.e. 2014)
###############################################################

# subset of year = 2015, with basePay >= 1st quartile of 2015 basePay
sub2015aboveQ1 <- subset(sub2015, with(sub2015, (basePay>=quantile(as.numeric(sub2015$basePay), 0.25))))

# export CSV

sub2015aboveQ1S <- sub2015aboveQ1[order(-sub2015aboveQ1$overtimePercent),] 

write.csv(sub2015aboveQ1S, file = "Documents/DataSci/RawData/SF_CitySalaries/output/sub2015aboveQ1.csv")


###############################################################
# investigate sheriff's employees as they had very high % overtime pay in 2015
###############################################################

subSheriff <- sub2015[grep("sheriff", sub2015$jobTitle), ]

subSheriffS <- subSheriff[order(-subSheriff$overtimePercent),] 

write.csv(subSheriffS, file = "Documents/DataSci/RawData/SF_CitySalaries/output/subSheriff2015.csv")

# Keyword = Sheriff stats
payStatsSheriff <- payStats(subSheriff, "Sheriff")

# Compare Sheriff stats vs. other highly-compensated job title keywords

jobTitlePayStats2 <- rbind(payStatsTransit, payStatsNurse, payStatsOperator, payStatsOfficer, payStatsPolice, payStatsEng, payStatsAnalyst, payStatsSheriff, payStatsFire, payStats2015)

colnames(jobTitlePayStats2) <- c("Job Title Keyword", "Number of Employees", "Sum of All
BasePay", "Min BasePay", "1st Quartile BasePay", "Median BasePay", "Mean
BasePay", "3rd Quartile BasePay", "Max BasePay", "Sum of All OvertimePay",
"Min OvertimePay", "1st Quartile OvertimePay", "Median OvertimePay", "Mean OvertimePay",
 "3rd Quartile OvertimePay", "Max OvertimePay", "Min OvertimePercent", "1st Quartile
OvertimePercent", "Median OvertimePercent", "Mean OvertimePercent", "3rd Quartile
 OvertimePercent", "Max OvertimePercent")

rownames(jobTitlePayStats2) <- NULL

# export jobTitlePayStats2 to a CSV file
write.csv(jobTitlePayStats, file = "Documents/DataSci/RawData/SF_CitySalaries/output/jobTitlePayStatsSheriff.csv")


###############################################################
# focus on highest % overtime pay in 2015 employees, explore 2015-2011 % overtime
###############################################################

subABS1 <- sal[grep("antonio santiago", sal$employeeName), ]
subABS2 <- sal[grep("antonio b santiago", sal$employeeName), ]
subABS <- rbind(subABS1, subABS2)

subWPY1 <- sal[grep("whitney yee", sal$employeeName), ]
subWPY2 <- sal[grep("whitney p yee", sal$employeeName), ]
subWPY <- rbind(subWPY1, subWPY2)

subBJB1 <- sal[grep("barry j bloom", sal$employeeName), ]
subBJB2 <- sal[grep("barry bloom", sal$employeeName), ]
subBJB <- rbind(subBJB1, subBJB2)

subKCD1 <- sal[grep("kristian c dejesus", sal$employeeName), ]
subKCD2 <- sal[grep("kristian dejesus", sal$employeeName), ]
subKCD3 <- sal[grep("kristian c de jesus", sal$employeeName), ]
subKCD4 <- sal[grep("kristian de jesus", sal$employeeName), ]
subKCD <- rbind(subKCD1, subKCD2, subKCD3, subKCD4)

subCSL1 <- sal[grep("chui s lee", sal$employeeName), ]
subCSL2 <- sal[grep("chui lee", sal$employeeName), ]
subCSL <- rbind(subCSL1, subCSL2)

subDXB <- sal[grep("donald bryant", sal$employeeName), ]

subIJC1 <- sal[grep("ivan j cordoba", sal$employeeName), ]
subIJC2 <- sal[grep("ivan cordoba", sal$employeeName), ]
subIJC <- rbind(subIJC1, subIJC2)

subLXL <- sal[grep("lawrence lee$", sal$employeeName), ]

subKCS1 <- sal[grep("khae c saephan", sal$employeeName), ]
subKCS2 <- sal[grep("khae saephan", sal$employeeName), ]
subKCS <- rbind(subKCS1, subKCS2)

subMXS <- sal[grep("marcus santiago", sal$employeeName), ]


###############################################################
# INDEX
###############################################################
