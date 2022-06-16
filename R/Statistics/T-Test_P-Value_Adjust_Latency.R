# T-test and P-value adjustments
# Katie Ford
# 6/7/2022

# Load readxl package
# Version: 1.4.0
library(readxl)

#### Latency to NREM ####

# Read File
All_Ages_Latency <- read_xlsx("Latency_All_Ages.xlsx")

# Change from Tibble to Data Frame
All_Ages_Latency <- data.frame(All_Ages_Latency)

# P24 WT
P24_WT <- All_Ages_Latency[c(1:7), 4]

# P24 MT
P24_MT <- All_Ages_Latency[c(8:15), 4]

# P30 WT
P30_WT <- All_Ages_Latency[c(16:21), 4]

# P30 MT
P30_MT <- All_Ages_Latency[c(22:29), 4]

# P45 WT
P45_WT <- All_Ages_Latency[c(30:35), 4]

# P45 MT
P45_MT <- All_Ages_Latency[c(36:42), 4]

# P60 WT
P60_WT <- All_Ages_Latency[c(43:49), 4]

# P60 MT
P60_MT <- All_Ages_Latency[c(50:55), 4]

# P90 3 HR WT
P90_3hr_WT <- All_Ages_Latency[c(56:65), 4]

# P90 3 HR MT
P90_3hr_MT <- All_Ages_Latency[c(66:76), 4]

# P90 5 HR WT
P90_5hr_WT <- All_Ages_Latency[c(77:85), 4]

# P90 5 HR MT
P90_5hr_MT <- All_Ages_Latency[c(86:95), 4]

# Between Genotype T Tests (One tailed, equal variance)
# P24
P24 <- t.test(P24_WT, P24_MT, var.equal = TRUE, alternative = "less")
P24_P_value <- P24$p.value
# [1] 0.3673158

# P30
P30 <- t.test(P30_WT, P30_MT, var.equal = TRUE, alternative = "less")
P30_P_value <- P30$p.value
# [1] 0.01674131

# P45
P45 <- t.test(P45_WT, P45_MT, var.equal = TRUE, alternative = "less")
P45_P_value <- P45$p.value
# [1] 0.3252768

# P60
P60 <- t.test(P60_WT, P60_MT, var.equal = TRUE, alternative = "less")
P60_P_value <- P60$p.value
# 0.181439

# P90 3 hr
P90_3hr <- t.test(P90_3hr_WT, P90_3hr_MT, var.equal = TRUE, alternative = "less")
P90_3_hr_P_value <- P90_3hr$p.value
# [1] 0.0008425768

# P90 5 hr
P90_5hr <- t.test(P90_5hr_WT, P90_5hr_MT, var.equal = TRUE, alternative = "less")
P90_5_hr_P_value <- P90_5hr$p.value
# [1] 0.003539975





