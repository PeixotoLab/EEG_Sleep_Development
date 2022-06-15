# T-test and P-value adjustments
# Kaitlyn Ford
# 6/1/2022

# Load readxl package
# Version: 1.4.0
library(readxl)

#### Bout Number ####

# Read File
Bout_Number <- read_xlsx("BL_Data_Bout_Number_SPSS.xlsx")

# Change from Tibble to Data Frame
Bout_Number <- data.frame(Bout_Number)

# Significance from two-way ANOVA, see SPSS output
# T Test for Genotype (4) and Age (3)
# Wake LP: Age, Genotype
# Wake DP: Age
# NREM LP: Age, Genotype
# NREM DP: Age
# REM LP: Age, Genotype ***Interaction
# REM DP: Age

#### Wake Light Period, Genotype ####
# P23 WT vs MT
P23_WT_Wake_LP <- Bout_Number[c(1:7),4]
P23_WT_Wake_LP

P23_MT_Wake_LP <- Bout_Number[c(8:15),4]
P23_MT_Wake_LP

P23_Genotype_W_LP <- t.test(P23_WT_Wake_LP, P23_MT_Wake_LP, var.equal = TRUE)
P23_Genotype_W_LP_P_Value <- P23_Genotype_W_LP$p.value
# [1] 0.26239

# P29 WT vs MT
P29_WT_Wake_LP <- Bout_Number[c(16:22),4]
P29_WT_Wake_LP

P29_MT_Wake_LP <- Bout_Number[c(23:31),4]
P29_MT_Wake_LP

P29_Genotype_W_LP <- t.test(P29_WT_Wake_LP, P29_MT_Wake_LP, var.equal = TRUE)
P29_Genotype_W_LP_P_Value <- P29_Genotype_W_LP$p.value
# [1] 0.02268665

# P44 WT vs MT
P44_WT_Wake_LP <- Bout_Number[c(32:37),4]
P44_WT_Wake_LP

P44_MT_Wake_LP <- Bout_Number[c(38:44),4]
P44_MT_Wake_LP

P44_Genotype_W_LP <- t.test(P44_WT_Wake_LP, P44_MT_Wake_LP, var.equal = TRUE)
P44_Genotype_W_LP_P_Value <- P44_Genotype_W_LP$p.value
# [1] 0.6339481

# P59 WT vs MT
P59_WT_Wake_LP <- Bout_Number[c(45:52),4]
P59_WT_Wake_LP

P59_MT_Wake_LP <- Bout_Number[c(53:59),4]
P59_MT_Wake_LP

P59_Genotype_W_LP <- t.test(P59_WT_Wake_LP, P59_MT_Wake_LP, var.equal = TRUE)
P59_Genotype_W_LP_P_Value <- P59_Genotype_W_LP$p.value
# [1] 0.549628

Wake_LP_Genotype <-c(P23_Genotype_W_LP_P_Value, P29_Genotype_W_LP_P_Value, P44_Genotype_W_LP_P_Value, P59_Genotype_W_LP_P_Value)
Wake_LP_Genotype_Adjust <- p.adjust(Wake_LP_Genotype,method="hochberg")
# [1] 0.63394814 0.09074659 0.63394814 0.63394814

#### Wake Light Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_Wake_LP <- t.test(P23_WT_Wake_LP, P29_WT_Wake_LP, var.equal = TRUE)
WT_P23_P29_Wake_LP_P_Value <- WT_P23_P29_Wake_LP$p.value
# [1] 0.009107538

# WT P29 vs P44
WT_P29_P44_Wake_LP <- t.test(P29_WT_Wake_LP, P44_WT_Wake_LP, var.equal = TRUE)
WT_P29_P44_Wake_LP_P_Value <- WT_P29_P44_Wake_LP$p.value
# [1] 0.1150682

# WT P44 vs P59
WT_P44_P59_Wake_LP <- t.test(P44_WT_Wake_LP, P59_WT_Wake_LP, var.equal = TRUE)
WT_P44_P59_Wake_LP_P_Value <- WT_P44_P59_Wake_LP$p.value
# [1] 0.8701951

WT_Wake_LP_Age <-c(WT_P23_P29_Wake_LP_P_Value, WT_P29_P44_Wake_LP_P_Value, WT_P44_P59_Wake_LP_P_Value)
WT_Wake_LP_Age_Adjust <- p.adjust(WT_Wake_LP_Age,method="hochberg")
# [1] 0.02732261 0.23013634 0.87019507

# Mutant
# MT P23 vs P29
MT_P23_P29_Wake_LP <- t.test(P23_MT_Wake_LP, P29_MT_Wake_LP, var.equal = TRUE)
MT_P23_P29_Wake_LP_P_Value <- MT_P23_P29_Wake_LP$p.value
# [1] 0.004683218

# MT P29 vs P44
MT_P29_P44_Wake_LP <- t.test(P29_MT_Wake_LP, P44_MT_Wake_LP, var.equal = TRUE)
MT_P29_P44_Wake_LP_P_Value <- MT_P29_P44_Wake_LP$p.value
# [1] 0.8641329

# MT P44 vs P59
MT_P44_P59_Wake_LP <- t.test(P44_MT_Wake_LP, P59_MT_Wake_LP, var.equal = TRUE)
MT_P44_P59_Wake_LP_P_Value <- MT_P44_P59_Wake_LP$p.value
# [1] 0.9345903

MT_Wake_LP_Age <-c(MT_P23_P29_Wake_LP_P_Value, MT_P29_P44_Wake_LP_P_Value, MT_P44_P59_Wake_LP_P_Value)
MT_Wake_LP_Age_Adjust <- p.adjust(MT_Wake_LP_Age,method="hochberg")
# [1] 0.01404965 0.93459026 0.93459026

#### Wake Dark Period, Age ####

# P23 WT
P23_WT_Wake_DP <- Bout_Number[c(1:7), 5]
P23_WT_Wake_DP

# P23 MT
P23_MT_Wake_DP <- Bout_Number[c(8:15), 5]
P23_MT_Wake_DP

# P29 WT
P29_WT_Wake_DP <- Bout_Number[c(16:22), 5]
P29_WT_Wake_DP

# P29 MT
P29_MT_Wake_DP <- Bout_Number[c(23:31), 5]
P29_MT_Wake_DP

# P44 WT
P44_WT_Wake_DP <- Bout_Number[c(32:37), 5]
P44_WT_Wake_DP

# P44 MT
P44_MT_Wake_DP <- Bout_Number[c(38:44), 5]
P44_MT_Wake_DP

# P59 WT
P59_WT_Wake_DP <- Bout_Number[c(45:52), 5]
P59_WT_Wake_DP

# P59 MT
P59_MT_Wake_DP <- Bout_Number[c(53:59), 5]
P59_MT_Wake_DP

# Wild Type
# WT P23 vs P29
WT_P23_P29_Wake_DP <- t.test(P23_WT_Wake_DP, P29_WT_Wake_DP, var.equal = TRUE)
WT_P23_P29_Wake_DP_P_Value <- WT_P23_P29_Wake_DP$p.value
# [1] 0.6481316

# WT P29 vs P44
WT_P29_P44_Wake_DP <- t.test(P29_WT_Wake_DP, P44_WT_Wake_DP, var.equal = TRUE)
WT_P29_P44_Wake_DP_P_Value <- WT_P29_P44_Wake_DP$p.value
# [1] 0.1382213

# WT P44 vs P59
WT_P44_P59_Wake_DP <- t.test(P44_WT_Wake_DP, P59_WT_Wake_DP, var.equal = TRUE)
WT_P44_P59_Wake_DP_P_Value <- WT_P44_P59_Wake_DP$p.value
# [1] 0.1661665

WT_Wake_DP_Age <-c(WT_P23_P29_Wake_DP_P_Value, WT_P29_P44_Wake_DP_P_Value, WT_P44_P59_Wake_DP_P_Value)
WT_Wake_DP_Age_Adjust <- p.adjust(WT_Wake_DP_Age,method="hochberg")
# [1] 0.6481316 0.3323330 0.3323330

# Mutant
# MT P23 vs P29
MT_P23_P29_Wake_DP <- t.test(P23_MT_Wake_DP, P29_MT_Wake_DP, var.equal = TRUE)
MT_P23_P29_Wake_DP_P_Value <- MT_P23_P29_Wake_DP$p.value
# [1] 0.5285995

# MT P29 vs P44
MT_P29_P44_Wake_DP <- t.test(P29_MT_Wake_DP, P44_MT_Wake_DP, var.equal = TRUE)
MT_P29_P44_Wake_DP_P_Value <- MT_P29_P44_Wake_DP$p.value
# [1] 0.1397674

# MT P44 vs P59
MT_P44_P59_Wake_DP <- t.test(P44_MT_Wake_DP, P59_MT_Wake_DP, var.equal = TRUE)
MT_P44_P59_Wake_DP_P_Value <- MT_P44_P59_Wake_DP$p.value
# [1] 0.8353809

MT_Wake_DP_Age <-c(MT_P23_P29_Wake_DP_P_Value, MT_P29_P44_Wake_DP_P_Value, MT_P44_P59_Wake_DP_P_Value)
MT_Wake_DP_Age_Adjust <- p.adjust(MT_Wake_DP_Age,method="hochberg")
# [1] 0.8353809 0.4193023 0.8353809

#### NREM Light Period, Genotype ####
# P23 WT vs MT
P23_WT_NREM_LP <- Bout_Number[c(1:7),6]
P23_WT_NREM_LP

P23_MT_NREM_LP <- Bout_Number[c(8:15),6]
P23_MT_NREM_LP

P23_Genotype_NREM_LP <- t.test(P23_WT_NREM_LP, P23_MT_NREM_LP, var.equal = TRUE)
P23_Genotype_NREM_LP_P_Value <- P23_Genotype_NREM_LP$p.value
# [1] 0.01996681

# P29 WT vs MT
P29_WT_NREM_LP <- Bout_Number[c(16:22),6]
P29_WT_NREM_LP

P29_MT_NREM_LP <- Bout_Number[c(23:31),6]
P29_MT_NREM_LP

P29_Genotype_NREM_LP <- t.test(P29_WT_NREM_LP, P29_MT_NREM_LP, var.equal = TRUE)
P29_Genotype_NREM_LP_P_Value <- P29_Genotype_NREM_LP$p.value
# [1] 0.5141383

# P44 WT vs MT
P44_WT_NREM_LP <- Bout_Number[c(32:37),6]
P44_WT_NREM_LP

P44_MT_NREM_LP <- Bout_Number[c(38:44),6]
P44_MT_NREM_LP

P44_Genotype_NREM_LP <- t.test(P44_WT_NREM_LP, P44_MT_NREM_LP, var.equal = TRUE)
P44_Genotype_NREM_LP_P_Value <- P44_Genotype_NREM_LP$p.value
# [1] 0.6678747

# P59 WT vs MT
P59_WT_NREM_LP <- Bout_Number[c(45:52),6]
P59_WT_NREM_LP

P59_MT_NREM_LP <- Bout_Number[c(53:59),6]
P59_MT_NREM_LP

P59_Genotype_NREM_LP <- t.test(P59_WT_NREM_LP, P59_MT_NREM_LP, var.equal = TRUE)
P59_Genotype_NREM_LP_P_Value <- P59_Genotype_NREM_LP$p.value
# [1] 0.03110017

NREM_LP_Genotype <-c(P23_Genotype_NREM_LP_P_Value, P29_Genotype_NREM_LP_P_Value, P44_Genotype_NREM_LP_P_Value, P59_Genotype_NREM_LP_P_Value)
NREM_LP_Genotype_Adjust <- p.adjust(NREM_LP_Genotype,method="hochberg")
# [1] 0.07986724 0.66787465 0.66787465 0.09330050

#### NREM Light Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_NREM_LP <- t.test(P23_WT_NREM_LP, P29_WT_NREM_LP, var.equal = TRUE)
WT_P23_P29_NREM_LP_P_Value <- WT_P23_P29_NREM_LP$p.value
# [1] 0.1703177

# WT P29 vs P44
WT_P29_P44_NREM_LP <- t.test(P29_WT_NREM_LP, P44_WT_NREM_LP, var.equal = TRUE)
WT_P29_P44_NREM_LP_P_Value <- WT_P29_P44_NREM_LP$p.value
# [1] 0.1351547

# WT P44 vs P59
WT_P44_P59_NREM_LP <- t.test(P44_WT_NREM_LP, P59_WT_NREM_LP, var.equal = TRUE)
WT_P44_P59_NREM_LP_P_Value <- WT_P44_P59_NREM_LP$p.value
# [1] 0.00817215

WT_NREM_LP_Age <-c(WT_P23_P29_NREM_LP_P_Value, WT_P29_P44_NREM_LP_P_Value, WT_P44_P59_NREM_LP_P_Value)
WT_NREM_LP_Age_Adjust <- p.adjust(WT_NREM_LP_Age,method="hochberg")
# [1] 0.17031767 0.17031767 0.02451645

# Mutant
# MT P23 vs P29
MT_P23_P29_NREM_LP <- t.test(P23_MT_NREM_LP, P29_MT_NREM_LP, var.equal = TRUE)
MT_P23_P29_NREM_LP_P_Value <- MT_P23_P29_NREM_LP$p.value
# [1] 0.03118572

# MT P29 vs P44
MT_P29_P44_NREM_LP <- t.test(P29_MT_NREM_LP, P44_MT_NREM_LP, var.equal = TRUE)
MT_P29_P44_NREM_LP_P_Value <- MT_P29_P44_NREM_LP$p.value
# [1] 0.5156285

# MT P44 vs P59
MT_P44_P59_NREM_LP <- t.test(P44_MT_NREM_LP, P59_MT_NREM_LP, var.equal = TRUE)
MT_P44_P59_NREM_LP_P_Value <- MT_P44_P59_NREM_LP$p.value
# [1] 0.373631

MT_NREM_LP_Age <-c(MT_P23_P29_NREM_LP_P_Value, MT_P29_P44_NREM_LP_P_Value, MT_P44_P59_NREM_LP_P_Value)
MT_NREM_LP_Age_Adjust <- p.adjust(MT_NREM_LP_Age,method="hochberg")
# [1] 0.09355715 0.51562846 0.51562846

#### NREM Dark Period, Age ####

# P23 WT
P23_WT_NREM_DP <- Bout_Number[c(1:7), 7]
P23_WT_NREM_DP

# P23 MT
P23_MT_NREM_DP <- Bout_Number[c(8:15), 7]
P23_MT_NREM_DP

# P29 WT
P29_WT_NREM_DP <- Bout_Number[c(16:22), 7]
P29_WT_NREM_DP

# P29 MT
P29_MT_NREM_DP <- Bout_Number[c(23:31), 7]
P29_MT_NREM_DP

# P44 WT
P44_WT_NREM_DP <- Bout_Number[c(32:37), 7]
P44_WT_NREM_DP

# P44 MT
P44_MT_NREM_DP <- Bout_Number[c(38:44), 7]
P44_MT_NREM_DP

# P59 WT
P59_WT_NREM_DP <- Bout_Number[c(45:52), 7]
P59_WT_NREM_DP

# P59 MT
P59_MT_NREM_DP <- Bout_Number[c(53:59), 7]
P59_MT_NREM_DP

# Wild Type
# WT P23 vs P29
WT_P23_P29_NREM_DP <- t.test(P23_WT_NREM_DP, P29_WT_NREM_DP, var.equal = TRUE)
WT_P23_P29_NREM_DP_P_Value <- WT_P23_P29_NREM_DP$p.value
# [1] 0.00132274

# WT P29 vs P44
WT_P29_P44_NREM_DP <- t.test(P29_WT_NREM_DP, P44_WT_NREM_DP, var.equal = TRUE)
WT_P29_P44_NREM_DP_P_Value <- WT_P29_P44_NREM_DP$p.value
# [1] 1

# WT P44 vs P59
WT_P44_P59_NREM_DP <- t.test(P44_WT_NREM_DP, P59_WT_NREM_DP, var.equal = TRUE)
WT_P44_P59_NREM_DP_P_Value <- WT_P44_P59_NREM_DP$p.value
# [1] 0.6153295

WT_NREM_DP_Age <-c(WT_P23_P29_NREM_DP_P_Value, WT_P29_P44_NREM_DP_P_Value, WT_P44_P59_NREM_DP_P_Value)
WT_NREM_DP_Age_Adjust <- p.adjust(WT_NREM_DP_Age,method="hochberg")
# [1] 0.003968221 1.000000000 1.000000000

# Mutant
# MT P23 vs P29
MT_P23_P29_NREM_DP <- t.test(P23_MT_NREM_DP, P29_MT_NREM_DP, var.equal = TRUE)
MT_P23_P29_NREM_DP_P_Value <- MT_P23_P29_NREM_DP$p.value
# [1] 0.09330112

# MT P29 vs P44
MT_P29_P44_NREM_DP <- t.test(P29_MT_NREM_DP, P44_MT_NREM_DP, var.equal = TRUE)
MT_P29_P44_NREM_DP_P_Value <- MT_P29_P44_NREM_DP$p.value
# [1] 0.3785703

# MT P44 vs P59
MT_P44_P59_NREM_DP <- t.test(P44_MT_NREM_DP, P59_MT_NREM_DP, var.equal = TRUE)
MT_P44_P59_NREM_DP_P_Value <- MT_P44_P59_NREM_DP$p.value
# [1] 0.6666651

MT_NREM_DP_Age <-c(MT_P23_P29_NREM_DP_P_Value, MT_P29_P44_NREM_DP_P_Value, MT_P44_P59_NREM_DP_P_Value)
MT_NREM_DP_Age_Adjust <- p.adjust(MT_NREM_DP_Age,method="hochberg")
# [1] 0.2799034 0.6666651 0.6666651

#### REM Light Period, Genotype ####

# P23 WT vs MT
P23_WT_REM_LP <- Bout_Number[c(1:7),8]
P23_WT_REM_LP

P23_MT_REM_LP <- Bout_Number[c(8:15),8]
P23_MT_REM_LP

P23_Genotype_REM_LP <- t.test(P23_WT_REM_LP, P23_MT_REM_LP, var.equal = TRUE)
P23_Genotype_REM_LP_P_Value <- P23_Genotype_REM_LP$p.value
# [1] 0.0004053605

# P29 WT vs MT
P29_WT_REM_LP <- Bout_Number[c(16:22),8]
P29_WT_REM_LP

P29_MT_REM_LP <- Bout_Number[c(23:31),8]
P29_MT_REM_LP

P29_Genotype_REM_LP <- t.test(P29_WT_REM_LP, P29_MT_REM_LP, var.equal = TRUE)
P29_Genotype_REM_LP_P_Value <- P29_Genotype_REM_LP$p.value
# [1] 0.01886433

# P44 WT vs MT
P44_WT_REM_LP <- Bout_Number[c(32:37),8]
P44_WT_REM_LP

P44_MT_REM_LP <- Bout_Number[c(38:44),8]
P44_MT_REM_LP

P44_Genotype_REM_LP <- t.test(P44_WT_REM_LP, P44_MT_REM_LP, var.equal = TRUE)
P44_Genotype_REM_LP_P_Value <- P44_Genotype_REM_LP$p.value
# [1] 0.4079345

# P59 WT vs MT
P59_WT_REM_LP <- Bout_Number[c(45:52),8]
P59_WT_REM_LP

P59_MT_REM_LP <- Bout_Number[c(53:59),8]
P59_MT_REM_LP

P59_Genotype_REM_LP <- t.test(P59_WT_REM_LP, P59_MT_REM_LP, var.equal = TRUE)
P59_Genotype_REM_LP_P_Value <- P59_Genotype_REM_LP$p.value
# [1] 0.02646182

REM_LP_Genotype <-c(P23_Genotype_REM_LP_P_Value, P29_Genotype_REM_LP_P_Value, P44_Genotype_REM_LP_P_Value, P59_Genotype_REM_LP_P_Value)
REM_LP_Genotype_Adjust <- p.adjust(REM_LP_Genotype,method="hochberg")
# [1] 0.001621442 0.052923645 0.407934486 0.052923645

#### REM Light Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_REM_LP <- t.test(P23_WT_REM_LP, P29_WT_REM_LP, var.equal = TRUE)
WT_P23_P29_REM_LP_P_Value <- WT_P23_P29_REM_LP$p.value
# [1] 0.9253508

# WT P29 vs P44
WT_P29_P44_REM_LP <- t.test(P29_WT_REM_LP, P44_WT_REM_LP, var.equal = TRUE)
WT_P29_P44_REM_LP_P_Value <- WT_P29_P44_REM_LP$p.value
# [1] 0.8273452

# WT P44 vs P59
WT_P44_P59_REM_LP <- t.test(P44_WT_REM_LP, P59_WT_REM_LP, var.equal = TRUE)
WT_P44_P59_REM_LP_P_Value <- WT_P44_P59_REM_LP$p.value
# [1] 0.03162617

WT_REM_LP_Age <-c(WT_P23_P29_REM_LP_P_Value, WT_P29_P44_REM_LP_P_Value, WT_P44_P59_REM_LP_P_Value)
WT_REM_LP_Age_Adjust <- p.adjust(WT_REM_LP_Age,method="hochberg")
# [1] 0.92535077 0.92535077 0.09487852

# Mutant
# MT P23 vs P29
MT_P23_P29_REM_LP <- t.test(P23_MT_REM_LP, P29_MT_REM_LP, var.equal = TRUE)
MT_P23_P29_REM_LP_P_Value <- MT_P23_P29_REM_LP$p.value
# [1] 0.001932809

# MT P29 vs P44
MT_P29_P44_REM_LP <- t.test(P29_MT_REM_LP, P44_MT_REM_LP, var.equal = TRUE)
MT_P29_P44_REM_LP_P_Value <- MT_P29_P44_REM_LP$p.value
# [1] 0.862075

# MT P44 vs P59
MT_P44_P59_REM_LP <- t.test(P44_MT_REM_LP, P59_MT_REM_LP, var.equal = TRUE)
MT_P44_P59_REM_LP_P_Value <- MT_P44_P59_REM_LP$p.value
# [1] 0.3276689

MT_REM_LP_Age <-c(MT_P23_P29_REM_LP_P_Value, MT_P29_P44_REM_LP_P_Value, MT_P44_P59_REM_LP_P_Value)
MT_REM_LP_Age_Adjust <- p.adjust(MT_REM_LP_Age,method="hochberg")
# [1] 0.005798428 0.862075032 0.655337895

#### REM Dark Period, Age ####

# P23 WT
P23_WT_REM_DP <- Bout_Number[c(1:7), 9]
P23_WT_REM_DP

# P23 MT
P23_MT_REM_DP <- Bout_Number[c(8:15), 9]
P23_MT_REM_DP

# P29 WT
P29_WT_REM_DP <- Bout_Number[c(16:22), 9]
P29_WT_REM_DP

# P29 MT
P29_MT_REM_DP <- Bout_Number[c(23:31), 9]
P29_MT_REM_DP

# P44 WT
P44_WT_REM_DP <- Bout_Number[c(32:37), 9]
P44_WT_REM_DP

# P44 MT
P44_MT_REM_DP <- Bout_Number[c(38:44), 9]
P44_MT_REM_DP

# P59 WT
P59_WT_REM_DP <- Bout_Number[c(45:52), 9]
P59_WT_REM_DP

# P59 MT
P59_MT_REM_DP <- Bout_Number[c(53:59), 9]
P59_MT_REM_DP

# Wild Type
# WT P23 vs P29
WT_P23_P29_REM_DP <- t.test(P23_WT_REM_DP, P29_WT_REM_DP, var.equal = TRUE)
WT_P23_P29_REM_DP_P_Value <- WT_P23_P29_REM_DP$p.value
# [1] 0.0001170254

# WT P29 vs P44
WT_P29_P44_REM_DP <- t.test(P29_WT_REM_DP, P44_WT_REM_DP, var.equal = TRUE)
WT_P29_P44_REM_DP_P_Value <- WT_P29_P44_REM_DP$p.value
# [1] 0.01517945

# WT P44 vs P59
WT_P44_P59_REM_DP <- t.test(P44_WT_REM_DP, P59_WT_REM_DP, var.equal = TRUE)
WT_P44_P59_REM_DP_P_Value <- WT_P44_P59_REM_DP$p.value
# [1] 0.2982057

WT_REM_DP_Age <-c(WT_P23_P29_REM_DP_P_Value, WT_P29_P44_REM_DP_P_Value, WT_P44_P59_REM_DP_P_Value)
WT_REM_DP_Age_Adjust <- p.adjust(WT_REM_DP_Age,method="hochberg")
# [1] 0.0003510761 0.0303588929 0.2982056907

# Mutant
# MT P23 vs P29
MT_P23_P29_REM_DP <- t.test(P23_MT_REM_DP, P29_MT_REM_DP, var.equal = TRUE)
MT_P23_P29_REM_DP_P_Value <- MT_P23_P29_REM_DP$p.value
# [1] 0.007661163

# MT P29 vs P44
MT_P29_P44_REM_DP <- t.test(P29_MT_REM_DP, P44_MT_REM_DP, var.equal = TRUE)
MT_P29_P44_REM_DP_P_Value <- MT_P29_P44_REM_DP$p.value
# [1] 0.02626741

# MT P44 vs P59
MT_P44_P59_REM_DP <- t.test(P44_MT_REM_DP, P59_MT_REM_DP, var.equal = TRUE)
MT_P44_P59_REM_DP_P_Value <- MT_P44_P59_REM_DP$p.value
# [1] 0.193405

MT_REM_DP_Age <-c(MT_P23_P29_REM_DP_P_Value, MT_P29_P44_REM_DP_P_Value, MT_P44_P59_REM_DP_P_Value)
MT_REM_DP_Age_Adjust <- p.adjust(MT_REM_DP_Age,method="hochberg")
# [1] 0.02298349 0.05253481 0.19340504
