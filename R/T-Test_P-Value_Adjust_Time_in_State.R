# T-test and P-value adjustments
# Katie Ford
# 6/2/2022

# Load readxl package
# Version: 1.4.0
library(readxl)

# Set working directory
setwd("~/Dropbox/Shank3Baby_Data/Time in State (all ages)")

#### Time in State ####

# Read File
Time_in_State <- read_xlsx("BaselineTimeInState_LM_finalizingfigures.xlsx", sheet = "SPSS data-mainfig2 and ratio")

# Change from Tibble to Data Frame
Time_in_State <- data.frame(Time_in_State)

# Significance from two-way ANOVA, see SPSS output
# T Test for Genotype (4) and Age (3)
# Wake LP: Age
# Wake DP: Age, Genotype
# NREM LP: Age, Genotype
# NREM DP: Genotype
# REM LP: Age, Genotype
# REM DP: Age, Genotype

#### Wake Light Period, Age ####

# P23 WT
P23_WT_Wake_LP <- Time_in_State[c(1:7), 4]
P23_WT_Wake_LP

# P23 MT
P23_MT_Wake_LP <- Time_in_State[c(8:15), 4]
P23_MT_Wake_LP

# P29 WT
P29_WT_Wake_LP <- Time_in_State[c(16:22), 4]
P29_WT_Wake_LP

# P29 MT
P29_MT_Wake_LP <- Time_in_State[c(23:31), 4]
P29_MT_Wake_LP

# P44 WT
P44_WT_Wake_LP <- Time_in_State[c(32:37), 4]
P44_WT_Wake_LP

# P44 MT
P44_MT_Wake_LP <- Time_in_State[c(38:44), 4]
P44_MT_Wake_LP

# P59 WT
P59_WT_Wake_LP <- Time_in_State[c(45:52), 4]
P59_WT_Wake_LP

# P59 MT
P59_MT_Wake_LP <- Time_in_State[c(53:59), 4]
P59_MT_Wake_LP

# Wild Type
# WT P23 vs P29
WT_P23_P29_Wake_LP <- t.test(P23_WT_Wake_LP, P29_WT_Wake_LP, var.equal = TRUE)
WT_P23_P29_Wake_LP_P_Value <- WT_P23_P29_Wake_LP$p.value
# [1] 0.003035978

# WT P29 vs P44
WT_P29_P44_Wake_LP <- t.test(P29_WT_Wake_LP, P44_WT_Wake_LP, var.equal = TRUE)
WT_P29_P44_Wake_LP_P_Value <- WT_P29_P44_Wake_LP$p.value
# [1] 0.8618653

# WT P44 vs P59
WT_P44_P59_Wake_LP <- t.test(P44_WT_Wake_LP, P59_WT_Wake_LP, var.equal = TRUE)
WT_P44_P59_Wake_LP_P_Value <- WT_P44_P59_Wake_LP$p.value
# [1] 0.6306454

WT_Wake_LP_Age <-c(WT_P23_P29_Wake_LP_P_Value, WT_P29_P44_Wake_LP_P_Value, WT_P44_P59_Wake_LP_P_Value)
WT_Wake_LP_Age_Adjust <- p.adjust(WT_Wake_LP_Age,method="hochberg")
# [1] 0.009107933 0.861865316 0.861865316

# Mutant
# MT P23 vs P29
MT_P23_P29_Wake_LP <- t.test(P23_MT_Wake_LP, P29_MT_Wake_LP, var.equal = TRUE)
MT_P23_P29_Wake_LP_P_Value <- MT_P23_P29_Wake_LP$p.value
# [1] 0.5836918

# MT P29 vs P44
MT_P29_P44_Wake_LP <- t.test(P29_MT_Wake_LP, P44_MT_Wake_LP, var.equal = TRUE)
MT_P29_P44_Wake_LP_P_Value <- MT_P29_P44_Wake_LP$p.value
# [1] 0.1694646

# MT P44 vs P59
MT_P44_P59_Wake_LP <- t.test(P44_MT_Wake_LP, P59_MT_Wake_LP, var.equal = TRUE)
MT_P44_P59_Wake_LP_P_Value <- MT_P44_P59_Wake_LP$p.value
# [1] 0.5880432

MT_Wake_LP_Age <-c(MT_P23_P29_Wake_LP_P_Value, MT_P29_P44_Wake_LP_P_Value, MT_P44_P59_Wake_LP_P_Value)
MT_Wake_LP_Age_Adjust <- p.adjust(MT_Wake_LP_Age,method="hochberg")
# [1] 0.5880432 0.5083939 0.5880432

#### Wake Dark Period, Genotype ####
# P23 WT vs MT
P23_WT_Wake_DP <- Time_in_State[c(1:7),5]
P23_WT_Wake_DP

P23_MT_Wake_DP <- Time_in_State[c(8:15),5]
P23_MT_Wake_DP

P23_Genotype_W_DP <- t.test(P23_WT_Wake_DP, P23_MT_Wake_DP, var.equal = TRUE)
P23_Genotype_W_DP_P_Value <- P23_Genotype_W_DP$p.value
# [1] 0.0001422824

# P29 WT vs MT
P29_WT_Wake_DP <- Time_in_State[c(16:22),5]
P29_WT_Wake_DP

P29_MT_Wake_DP <- Time_in_State[c(23:31),5]
P29_MT_Wake_DP

P29_Genotype_W_DP <- t.test(P29_WT_Wake_DP, P29_MT_Wake_DP, var.equal = TRUE)
P29_Genotype_W_DP_P_Value <- P29_Genotype_W_DP$p.value
# [1] 0.2461733

# P44 WT vs MT
P44_WT_Wake_DP <- Time_in_State[c(32:37),5]
P44_WT_Wake_DP

P44_MT_Wake_DP <- Time_in_State[c(38:44),5]
P44_MT_Wake_DP

P44_Genotype_W_DP <- t.test(P44_WT_Wake_DP, P44_MT_Wake_DP, var.equal = TRUE)
P44_Genotype_W_DP_P_Value <- P44_Genotype_W_DP$p.value
# [1] 0.03289308

# P59 WT vs MT
P59_WT_Wake_DP <- Time_in_State[c(45:52),5]
P59_WT_Wake_DP

P59_MT_Wake_DP <- Time_in_State[c(53:59),5]
P59_MT_Wake_DP

P59_Genotype_W_DP <- t.test(P59_WT_Wake_DP, P59_MT_Wake_DP, var.equal = TRUE)
P59_Genotype_W_DP_P_Value <- P59_Genotype_W_DP$p.value
# [1] 0.00582331

Wake_DP_Genotype <-c(P23_Genotype_W_DP_P_Value, P29_Genotype_W_DP_P_Value, P44_Genotype_W_DP_P_Value, P59_Genotype_W_DP_P_Value)
Wake_DP_Genotype_Adjust <- p.adjust(Wake_DP_Genotype,method="hochberg")
# [1] 0.0005691296 0.2461732918 0.0657861550 0.0174699299

#### Wake Dark Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_Wake_DP <- t.test(P23_WT_Wake_DP, P29_WT_Wake_DP, var.equal = TRUE)
WT_P23_P29_Wake_DP_P_Value <- WT_P23_P29_Wake_DP$p.value
# [1] 0.005113999

# WT P29 vs P44
WT_P29_P44_Wake_DP <- t.test(P29_WT_Wake_DP, P44_WT_Wake_DP, var.equal = TRUE)
WT_P29_P44_Wake_DP_P_Value <- WT_P29_P44_Wake_DP$p.value
# [1] 0.7897374

# WT P44 vs P59
WT_P44_P59_Wake_DP <- t.test(P44_WT_Wake_DP, P59_WT_Wake_DP, var.equal = TRUE)
WT_P44_P59_Wake_DP_P_Value <- WT_P44_P59_Wake_DP$p.value
# [1] 0.3285657

WT_Wake_DP_Age <-c(WT_P23_P29_Wake_DP_P_Value, WT_P29_P44_Wake_DP_P_Value, WT_P44_P59_Wake_DP_P_Value)
WT_Wake_DP_Age_Adjust <- p.adjust(WT_Wake_DP_Age,method="hochberg")
# [1] 0.0153420 0.7897374 0.6571315

# Mutant
# MT P23 vs P29
MT_P23_P29_Wake_DP <- t.test(P23_MT_Wake_DP, P29_MT_Wake_DP, var.equal = TRUE)
MT_P23_P29_Wake_DP_P_Value <- MT_P23_P29_Wake_DP$p.value
# [1] 0.6052736

# MT P29 vs P44
MT_P29_P44_Wake_DP <- t.test(P29_MT_Wake_DP, P44_MT_Wake_DP, var.equal = TRUE)
MT_P29_P44_Wake_DP_P_Value <- MT_P29_P44_Wake_DP$p.value
# [1] 0.07053341

# MT P44 vs P59
MT_P44_P59_Wake_DP <- t.test(P44_MT_Wake_DP, P59_MT_Wake_DP, var.equal = TRUE)
MT_P44_P59_Wake_DP_P_Value <- MT_P44_P59_Wake_DP$p.value
# [1] 0.1751653

MT_Wake_DP_Age <-c(MT_P23_P29_Wake_DP_P_Value, MT_P29_P44_Wake_DP_P_Value, MT_P44_P59_Wake_DP_P_Value)
MT_Wake_DP_Age_Adjust <- p.adjust(MT_Wake_DP_Age,method="hochberg")
# [1] 0.6052736 0.2116002 0.3503307

#### NREM Light Period, Genotype ####

# P23 WT vs MT
P23_WT_NREM_LP <- Time_in_State[c(1:7),6]
P23_WT_NREM_LP

P23_MT_NREM_LP <- Time_in_State[c(8:15),6]
P23_MT_NREM_LP

P23_Genotype_NREM_LP <- t.test(P23_WT_NREM_LP, P23_MT_NREM_LP, var.equal = TRUE)
P23_Genotype_NREM_LP_P_Value <- P23_Genotype_NREM_LP$p.value
# [1] 0.01962016

# P29 WT vs MT
P29_WT_NREM_LP <- Time_in_State[c(16:22),6]
P29_WT_NREM_LP

P29_MT_NREM_LP <- Time_in_State[c(23:31),6]
P29_MT_NREM_LP

P29_Genotype_NREM_LP <- t.test(P29_WT_NREM_LP, P29_MT_NREM_LP, var.equal = TRUE)
P29_Genotype_NREM_LP_P_Value <- P29_Genotype_NREM_LP$p.value
# [1] 0.003973006

# P44 WT vs MT
P44_WT_NREM_LP <- Time_in_State[c(32:37),6]
P44_WT_NREM_LP

P44_MT_NREM_LP <- Time_in_State[c(38:44),6]
P44_MT_NREM_LP

P44_Genotype_NREM_LP <- t.test(P44_WT_NREM_LP, P44_MT_NREM_LP, var.equal = TRUE)
P44_Genotype_NREM_LP_P_Value <- P44_Genotype_NREM_LP$p.value
# [1] 0.0309948

# P59 WT vs MT
P59_WT_NREM_LP <- Time_in_State[c(45:52),6]
P59_WT_NREM_LP

P59_MT_NREM_LP <- Time_in_State[c(53:59),6]
P59_MT_NREM_LP

P59_Genotype_NREM_LP <- t.test(P59_WT_NREM_LP, P59_MT_NREM_LP, var.equal = TRUE)
P59_Genotype_NREM_LP_P_Value <- P59_Genotype_NREM_LP$p.value
# [1] 0.05910823

NREM_LP_Genotype <-c(P23_Genotype_NREM_LP_P_Value, P29_Genotype_NREM_LP_P_Value, P44_Genotype_NREM_LP_P_Value, P59_Genotype_NREM_LP_P_Value)
NREM_LP_Genotype_Adjust <- p.adjust(NREM_LP_Genotype,method="hochberg")
# [1] 0.05886047 0.01589202 0.05910823 0.05910823

#### NREM Light Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_NREM_LP <- t.test(P23_WT_NREM_LP, P29_WT_NREM_LP, var.equal = TRUE)
WT_P23_P29_NREM_LP_P_Value <- WT_P23_P29_NREM_LP$p.value
# [1] 0.002282076

# WT P29 vs P44
WT_P29_P44_NREM_LP <- t.test(P29_WT_NREM_LP, P44_WT_NREM_LP, var.equal = TRUE)
WT_P29_P44_NREM_LP_P_Value <- WT_P29_P44_NREM_LP$p.value
# [1] 0.8323475

# WT P44 vs P59
WT_P44_P59_NREM_LP <- t.test(P44_WT_NREM_LP, P59_WT_NREM_LP, var.equal = TRUE)
WT_P44_P59_NREM_LP_P_Value <- WT_P44_P59_NREM_LP$p.value
# [1] 0.2151335

WT_NREM_LP_Age <-c(WT_P23_P29_NREM_LP_P_Value, WT_P29_P44_NREM_LP_P_Value, WT_P44_P59_NREM_LP_P_Value)
WT_NREM_LP_Age_Adjust <- p.adjust(WT_NREM_LP_Age,method="hochberg")
# [1] 0.006846228 0.832347450 0.430267034

# Mutant
# MT P23 vs P29
MT_P23_P29_NREM_LP <- t.test(P23_MT_NREM_LP, P29_MT_NREM_LP, var.equal = TRUE)
MT_P23_P29_NREM_LP_P_Value <- MT_P23_P29_NREM_LP$p.value
# [1] 0.01211252

# MT P29 vs P44
MT_P29_P44_NREM_LP <- t.test(P29_MT_NREM_LP, P44_MT_NREM_LP, var.equal = TRUE)
MT_P29_P44_NREM_LP_P_Value <- MT_P29_P44_NREM_LP$p.value
# [1] 0.1663964

# MT P44 vs P59
MT_P44_P59_NREM_LP <- t.test(P44_MT_NREM_LP, P59_MT_NREM_LP, var.equal = TRUE)
MT_P44_P59_NREM_LP_P_Value <- MT_P44_P59_NREM_LP$p.value
# [1] 0.04994861

MT_NREM_LP_Age <-c(MT_P23_P29_NREM_LP_P_Value, MT_P29_P44_NREM_LP_P_Value, MT_P44_P59_NREM_LP_P_Value)
MT_NREM_LP_Age_Adjust <- p.adjust(MT_NREM_LP_Age,method="hochberg")
# [1] 0.03633757 0.16639639 0.09989721

#### NREM Dark Period, Genotype ####

# P23 WT vs MT
P23_WT_NREM_DP <- Time_in_State[c(1:7),7]
P23_WT_NREM_DP

P23_MT_NREM_DP <- Time_in_State[c(8:15),7]
P23_MT_NREM_DP

P23_Genotype_NREM_DP <- t.test(P23_WT_NREM_DP, P23_MT_NREM_DP, var.equal = TRUE)
P23_Genotype_NREM_DP_P_Value <- P23_Genotype_NREM_DP$p.value
# [1] 2.013939e-05

# P29 WT vs MT
P29_WT_NREM_DP <- Time_in_State[c(16:22),7]
P29_WT_NREM_DP

P29_MT_NREM_DP <- Time_in_State[c(23:31),7]
P29_MT_NREM_DP

P29_Genotype_NREM_DP <- t.test(P29_WT_NREM_DP, P29_MT_NREM_DP, var.equal = TRUE)
P29_Genotype_NREM_DP_P_Value <- P29_Genotype_NREM_DP$p.value
# [1] 0.07651169

# P44 WT vs MT
P44_WT_NREM_DP <- Time_in_State[c(32:37),7]
P44_WT_NREM_DP

P44_MT_NREM_DP <- Time_in_State[c(38:44),7]
P44_MT_NREM_DP

P44_Genotype_NREM_DP <- t.test(P44_WT_NREM_DP, P44_MT_NREM_DP, var.equal = TRUE)
P44_Genotype_NREM_DP_P_Value <- P44_Genotype_NREM_DP$p.value
# [1] 0.02147906

# P59 WT vs MT
P59_WT_NREM_DP <- Time_in_State[c(45:52),7]
P59_WT_NREM_DP

P59_MT_NREM_DP <- Time_in_State[c(53:59),7]
P59_MT_NREM_DP

P59_Genotype_NREM_DP <- t.test(P59_WT_NREM_DP, P59_MT_NREM_DP, var.equal = TRUE)
P59_Genotype_NREM_DP_P_Value <- P59_Genotype_NREM_DP$p.value
# [1] 0.002755658

NREM_DP_Genotype <-c(P23_Genotype_NREM_DP_P_Value, P29_Genotype_NREM_DP_P_Value, P44_Genotype_NREM_DP_P_Value, P59_Genotype_NREM_DP_P_Value)
NREM_DP_Genotype_Adjust <- p.adjust(NREM_DP_Genotype,method="hochberg")
# [1] 8.055754e-05 7.651169e-02 4.295811e-02 8.266975e-03

#### REM Light Period, Genotype ####

# P23 WT vs MT
P23_WT_REM_LP <- Time_in_State[c(1:7),8]
P23_WT_REM_LP

P23_MT_REM_LP <- Time_in_State[c(8:15),8]
P23_MT_REM_LP

P23_Genotype_REM_LP <- t.test(P23_WT_REM_LP, P23_MT_REM_LP, var.equal = TRUE)
P23_Genotype_REM_LP_P_Value <- P23_Genotype_REM_LP$p.value
# [1] 0.0004003655

# P29 WT vs MT
P29_WT_REM_LP <- Time_in_State[c(16:22),8]
P29_WT_REM_LP

P29_MT_REM_LP <- Time_in_State[c(23:31),8]
P29_MT_REM_LP

P29_Genotype_REM_LP <- t.test(P29_WT_REM_LP, P29_MT_REM_LP, var.equal = TRUE)
P29_Genotype_REM_LP_P_Value <- P29_Genotype_REM_LP$p.value
# [1] 0.02568868

# P44 WT vs MT
P44_WT_REM_LP <- Time_in_State[c(32:37),8]
P44_WT_REM_LP

P44_MT_REM_LP <- Time_in_State[c(38:44),8]
P44_MT_REM_LP

P44_Genotype_REM_LP <- t.test(P44_WT_REM_LP, P44_MT_REM_LP, var.equal = TRUE)
P44_Genotype_REM_LP_P_Value <- P44_Genotype_REM_LP$p.value
# [1] 0.04786035

# P59 WT vs MT
P59_WT_REM_LP <- Time_in_State[c(45:52),8]
P59_WT_REM_LP

P59_MT_REM_LP <- Time_in_State[c(53:59),8]
P59_MT_REM_LP

P59_Genotype_REM_LP <- t.test(P59_WT_REM_LP, P59_MT_REM_LP, var.equal = TRUE)
P59_Genotype_REM_LP_P_Value <- P59_Genotype_REM_LP$p.value
# [1] 0.01138334

REM_LP_Genotype <-c(P23_Genotype_REM_LP_P_Value, P29_Genotype_REM_LP_P_Value, P44_Genotype_REM_LP_P_Value, P59_Genotype_REM_LP_P_Value)
REM_LP_Genotype_Adjust <- p.adjust(REM_LP_Genotype,method="hochberg")
# [1] 0.001601462 0.047860351 0.047860351 0.034150010

#### REM Light Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_REM_LP <- t.test(P23_WT_REM_LP, P29_WT_REM_LP, var.equal = TRUE)
WT_P23_P29_REM_LP_P_Value <- WT_P23_P29_REM_LP$p.value
# [1] 0.7938401

# WT P29 vs P44
WT_P29_P44_REM_LP <- t.test(P29_WT_REM_LP, P44_WT_REM_LP, var.equal = TRUE)
WT_P29_P44_REM_LP_P_Value <- WT_P29_P44_REM_LP$p.value
# [1] 0.8639358

# WT P44 vs P59
WT_P44_P59_REM_LP <- t.test(P44_WT_REM_LP, P59_WT_REM_LP, var.equal = TRUE)
WT_P44_P59_REM_LP_P_Value <- WT_P44_P59_REM_LP$p.value
# [1] 0.1485655

WT_REM_LP_Age <-c(WT_P23_P29_REM_LP_P_Value, WT_P29_P44_REM_LP_P_Value, WT_P44_P59_REM_LP_P_Value)
WT_REM_LP_Age_Adjust <- p.adjust(WT_REM_LP_Age,method="hochberg")
# [1] 0.8639358 0.8639358 0.4456964

# Mutant
# MT P23 vs P29
MT_P23_P29_REM_LP <- t.test(P23_MT_REM_LP, P29_MT_REM_LP, var.equal = TRUE)
MT_P23_P29_REM_LP_P_Value <- MT_P23_P29_REM_LP$p.value
# [1] 0.004109556

# MT P29 vs P44
MT_P29_P44_REM_LP <- t.test(P29_MT_REM_LP, P44_MT_REM_LP, var.equal = TRUE)
MT_P29_P44_REM_LP_P_Value <- MT_P29_P44_REM_LP$p.value
# [1] 0.5673768

# MT P44 vs P59
MT_P44_P59_REM_LP <- t.test(P44_MT_REM_LP, P59_MT_REM_LP, var.equal = TRUE)
MT_P44_P59_REM_LP_P_Value <- MT_P44_P59_REM_LP$p.value
# [1] 0.1446626

MT_REM_LP_Age <-c(MT_P23_P29_REM_LP_P_Value, MT_P29_P44_REM_LP_P_Value, MT_P44_P59_REM_LP_P_Value)
MT_REM_LP_Age_Adjust <- p.adjust(MT_REM_LP_Age,method="hochberg")
# [1] 0.01232867 0.56737678 0.28932516

#### REM Dark Period, Genotype ####

# P23 WT vs MT
P23_WT_REM_DP <- Time_in_State[c(1:7),9]
P23_WT_REM_DP

P23_MT_REM_DP <- Time_in_State[c(8:15),9]
P23_MT_REM_DP

P23_Genotype_REM_DP <- t.test(P23_WT_REM_DP, P23_MT_REM_DP, var.equal = TRUE)
P23_Genotype_REM_DP_P_Value <- P23_Genotype_REM_DP$p.value
# [1] 0.180355

# P29 WT vs MT
P29_WT_REM_DP <- Time_in_State[c(16:22),9]
P29_WT_REM_DP

P29_MT_REM_DP <- Time_in_State[c(23:31),9]
P29_MT_REM_DP

P29_Genotype_REM_DP <- t.test(P29_WT_REM_DP, P29_MT_REM_DP, var.equal = TRUE)
P29_Genotype_REM_DP_P_Value <- P29_Genotype_REM_DP$p.value
# [1] 0.002538658

# P44 WT vs MT
P44_WT_REM_DP <- Time_in_State[c(32:37),9]
P44_WT_REM_DP

P44_MT_REM_DP <- Time_in_State[c(38:44),9]
P44_MT_REM_DP

P44_Genotype_REM_DP <- t.test(P44_WT_REM_DP, P44_MT_REM_DP, var.equal = TRUE)
P44_Genotype_REM_DP_P_Value <- P44_Genotype_REM_DP$p.value
# [1] 0.143106

# P59 WT vs MT
P59_WT_REM_DP <- Time_in_State[c(45:52),9]
P59_WT_REM_DP

P59_MT_REM_DP <- Time_in_State[c(53:59),9]
P59_MT_REM_DP

P59_Genotype_REM_DP <- t.test(P59_WT_REM_DP, P59_MT_REM_DP, var.equal = TRUE)
P59_Genotype_REM_DP_P_Value <- P59_Genotype_REM_DP$p.value
# [1] 0.5027122

REM_DP_Genotype <-c(P23_Genotype_REM_DP_P_Value, P29_Genotype_REM_DP_P_Value, P44_Genotype_REM_DP_P_Value, P59_Genotype_REM_DP_P_Value)
REM_DP_Genotype_Adjust <- p.adjust(REM_DP_Genotype,method="hochberg")
# [1] 0.36071007 0.01015463 0.36071007 0.50271217

#### REM Dark Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_REM_DP <- t.test(P23_WT_REM_DP, P29_WT_REM_DP, var.equal = TRUE)
WT_P23_P29_REM_DP_P_Value <- WT_P23_P29_REM_DP$p.value
# [1] 1.56567e-05

# WT P29 vs P44
WT_P29_P44_REM_DP <- t.test(P29_WT_REM_DP, P44_WT_REM_DP, var.equal = TRUE)
WT_P29_P44_REM_DP_P_Value <- WT_P29_P44_REM_DP$p.value
# [1] 0.05258838

# WT P44 vs P59
WT_P44_P59_REM_DP <- t.test(P44_WT_REM_DP, P59_WT_REM_DP, var.equal = TRUE)
WT_P44_P59_REM_DP_P_Value <- WT_P44_P59_REM_DP$p.value
# [1] 0.5240205

WT_REM_DP_Age <-c(WT_P23_P29_REM_DP_P_Value, WT_P29_P44_REM_DP_P_Value, WT_P44_P59_REM_DP_P_Value)
WT_REM_DP_Age_Adjust <- p.adjust(WT_REM_DP_Age,method="hochberg")
# [1] 4.697009e-05 1.051768e-01 5.240205e-01

# Mutant
# MT P23 vs P29
MT_P23_P29_REM_DP <- t.test(P23_MT_REM_DP, P29_MT_REM_DP, var.equal = TRUE)
MT_P23_P29_REM_DP_P_Value <- MT_P23_P29_REM_DP$p.value
# [1] 0.000634944

# MT P29 vs P44
MT_P29_P44_REM_DP <- t.test(P29_MT_REM_DP, P44_MT_REM_DP, var.equal = TRUE)
MT_P29_P44_REM_DP_P_Value <- MT_P29_P44_REM_DP$p.value
# [1] 0.002753923

# MT P44 vs P59
MT_P44_P59_REM_DP <- t.test(P44_MT_REM_DP, P59_MT_REM_DP, var.equal = TRUE)
MT_P44_P59_REM_DP_P_Value <- MT_P44_P59_REM_DP$p.value
# [1] 0.6191626

MT_REM_DP_Age <-c(MT_P23_P29_REM_DP_P_Value, MT_P29_P44_REM_DP_P_Value, MT_P44_P59_REM_DP_P_Value)
MT_REM_DP_Age_Adjust <- p.adjust(MT_REM_DP_Age,method="hochberg")
# [1] 0.001904832 0.005507847 0.619162604


