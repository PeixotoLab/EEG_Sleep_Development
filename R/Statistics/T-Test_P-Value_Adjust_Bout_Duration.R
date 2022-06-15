# T-test and P-value adjustments
# Katie Ford
# 6/1/2022

# Load readxl package
# Version: 1.4.0
library(readxl)

# Set working directory
setwd("~/Dropbox/Shank3Baby_Data/Bout_Analysis")

#### Bout Duration ####

# Read File
Bout_Duration <- read_xlsx("BL_Data_Bout_Duration_SPSS.xlsx")

# Change from Tibble to Data Frame
Bout_Duration <- data.frame(Bout_Duration)

# Significance from two-way ANOVA, see SPSS output
# T Test for Genotype (4) and Age (3)
# NREM LP: Age, Genotype
# NREM DP: Age, Genotype ***Interaction
# REM LP: Genotype 
# REM DP: Age ***Interaction

#### NREM Light Period, Genotype ####
# P23 WT vs MT
P23_WT_NREM_LP <- Bout_Duration[c(1:7),6]
P23_WT_NREM_LP

P23_MT_NREM_LP <- Bout_Duration[c(8:15),6]
P23_MT_NREM_LP

P23_Genotype_NREM_LP <- t.test(P23_WT_NREM_LP, P23_MT_NREM_LP, var.equal = TRUE)
P23_Genotype_NREM_LP_P_Value <- P23_Genotype_NREM_LP$p.value
# [1] 0.004456813

# P29 WT vs MT
P29_WT_NREM_LP <- Bout_Duration[c(16:22),6]
P29_WT_NREM_LP

P29_MT_NREM_LP <- Bout_Duration[c(23:31),6]
P29_MT_NREM_LP

P29_Genotype_NREM_LP <- t.test(P29_WT_NREM_LP, P29_MT_NREM_LP, var.equal = TRUE)
P29_Genotype_NREM_LP_P_Value <- P29_Genotype_NREM_LP$p.value
# [1] 0.0108422

# P44 WT vs MT
P44_WT_NREM_LP <- Bout_Duration[c(32:37),6]
P44_WT_NREM_LP

P44_MT_NREM_LP <- Bout_Duration[c(38:44),6]
P44_MT_NREM_LP

P44_Genotype_NREM_LP <- t.test(P44_WT_NREM_LP, P44_MT_NREM_LP, var.equal = TRUE)
P44_Genotype_NREM_LP_P_Value <- P44_Genotype_NREM_LP$p.value
# [1] 0.1842012

# P59 WT vs MT
P59_WT_NREM_LP <- Bout_Duration[c(45:52),6]
P59_WT_NREM_LP

P59_MT_NREM_LP <- Bout_Duration[c(53:59),6]
P59_MT_NREM_LP

P59_Genotype_NREM_LP <- t.test(P59_WT_NREM_LP, P59_MT_NREM_LP, var.equal = TRUE)
P59_Genotype_NREM_LP_P_Value <- P59_Genotype_NREM_LP$p.value
# [1] 0.0306607

NREM_LP_Genotype <-c(P23_Genotype_NREM_LP_P_Value, P29_Genotype_NREM_LP_P_Value, P44_Genotype_NREM_LP_P_Value, P59_Genotype_NREM_LP_P_Value)
NREM_LP_Genotype_Adjust <- p.adjust(NREM_LP_Genotype,method="hochberg")
# [1] 0.01782725 0.03252659 0.18420123 0.06132140

#### NREM Light Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_NREM_LP <- t.test(P23_WT_NREM_LP, P29_WT_NREM_LP, var.equal = TRUE)
WT_P23_P29_NREM_LP_P_Value <- WT_P23_P29_NREM_LP$p.value
# [1] 0.001988383

# WT P29 vs P44
WT_P29_P44_NREM_LP <- t.test(P29_WT_NREM_LP, P44_WT_NREM_LP, var.equal = TRUE)
WT_P29_P44_NREM_LP_P_Value <- WT_P29_P44_NREM_LP$p.value
# [1] 0.2067559

# WT P44 vs P59
WT_P44_P59_NREM_LP <- t.test(P44_WT_NREM_LP, P59_WT_NREM_LP, var.equal = TRUE)
WT_P44_P59_NREM_LP_P_Value <- WT_P44_P59_NREM_LP$p.value
# [1] 0.01194818

WT_NREM_LP_Age <-c(WT_P23_P29_NREM_LP_P_Value, WT_P29_P44_NREM_LP_P_Value, WT_P44_P59_NREM_LP_P_Value)
WT_NREM_LP_Age_Adjust <- p.adjust(WT_NREM_LP_Age,method="hochberg")
# [1] 0.005965148 0.206755875 0.023896362

# Mutant
# MT P23 vs P29
MT_P23_P29_NREM_LP <- t.test(P23_MT_NREM_LP, P29_MT_NREM_LP, var.equal = TRUE)
MT_P23_P29_NREM_LP_P_Value <- MT_P23_P29_NREM_LP$p.value
# [1] 0.004057948

# MT P29 vs P44
MT_P29_P44_NREM_LP <- t.test(P29_MT_NREM_LP, P44_MT_NREM_LP, var.equal = TRUE)
MT_P29_P44_NREM_LP_P_Value <- MT_P29_P44_NREM_LP$p.value
# [1] 0.9371104

# MT P44 vs P59
MT_P44_P59_NREM_LP <- t.test(P44_MT_NREM_LP, P59_MT_NREM_LP, var.equal = TRUE)
MT_P44_P59_NREM_LP_P_Value <- MT_P44_P59_NREM_LP$p.value
# [1] 0.1716826

MT_NREM_LP_Age <-c(MT_P23_P29_NREM_LP_P_Value, MT_P29_P44_NREM_LP_P_Value, MT_P44_P59_NREM_LP_P_Value)
MT_NREM_LP_Age_Adjust <- p.adjust(MT_NREM_LP_Age,method="hochberg")
# [1] 0.01217385 0.93711043 0.34336510

#### NREM Dark Period, Genotype ####

# P23 WT vs MT
P23_WT_NREM_DP <- Bout_Duration[c(1:7), 7]
P23_WT_NREM_DP

P23_MT_NREM_DP <- Bout_Duration[c(8:15), 7]
P23_MT_NREM_DP

P23_Genotype_NREM_DP <- t.test(P23_WT_NREM_DP, P23_MT_NREM_DP, var.equal = TRUE)
P23_Genotype_NREM_DP_P_Value <- P23_Genotype_NREM_DP$p.value
# [1] 1.928749e-05

# P29 WT vs MT
P29_WT_NREM_DP <- Bout_Duration[c(16:22), 7]
P29_WT_NREM_DP

P29_MT_NREM_DP <- Bout_Duration[c(23:31), 7]
P29_MT_NREM_DP

P29_Genotype_NREM_DP <- t.test(P29_WT_NREM_DP, P29_MT_NREM_DP, var.equal = TRUE)
P29_Genotype_NREM_DP_P_Value <- P29_Genotype_NREM_DP$p.value
# [1] 0.6254715

# P44 WT vs MT
P44_WT_NREM_DP <- Bout_Duration[c(32:37), 7]
P44_WT_NREM_DP

P44_MT_NREM_DP <- Bout_Duration[c(38:44), 7]
P44_MT_NREM_DP

P44_Genotype_NREM_DP <- t.test(P44_WT_NREM_DP, P44_MT_NREM_DP, var.equal = TRUE)
P44_Genotype_NREM_DP_P_Value <- P44_Genotype_NREM_DP$p.value
# [1] 0.0168142

# P59 WT vs MT
P59_WT_NREM_DP <- Bout_Duration[c(45:52), 7]
P59_WT_NREM_DP

P59_MT_NREM_DP <- Bout_Duration[c(53:59), 7]
P59_MT_NREM_DP

P59_Genotype_NREM_DP <- t.test(P59_WT_NREM_DP, P59_MT_NREM_DP, var.equal = TRUE)
P59_Genotype_NREM_DP_P_Value <- P59_Genotype_NREM_DP$p.value
# [1] 0.001332034

NREM_DP_Genotype <-c(P23_Genotype_NREM_DP_P_Value, P29_Genotype_NREM_DP_P_Value, P44_Genotype_NREM_DP_P_Value, P59_Genotype_NREM_DP_P_Value)
NREM_DP_Genotype_Adjust <- p.adjust(NREM_DP_Genotype,method="hochberg")
# [1] 7.714995e-05 6.254715e-01 3.362839e-02 3.996102e-03

#### NREM Dark Period, Age ####

# Wild Type
# WT P23 vs P29
WT_P23_P29_NREM_DP <- t.test(P23_WT_NREM_DP, P29_WT_NREM_DP, var.equal = TRUE)
WT_P23_P29_NREM_DP_P_Value <- WT_P23_P29_NREM_DP$p.value
# [1] 0.1235381

# WT P29 vs P44
WT_P29_P44_NREM_DP <- t.test(P29_WT_NREM_DP, P44_WT_NREM_DP, var.equal = TRUE)
WT_P29_P44_NREM_DP_P_Value <- WT_P29_P44_NREM_DP$p.value
# [1] 0.2352799

# WT P44 vs P59
WT_P44_P59_NREM_DP <- t.test(P44_WT_NREM_DP, P59_WT_NREM_DP, var.equal = TRUE)
WT_P44_P59_NREM_DP_P_Value <- WT_P44_P59_NREM_DP$p.value
# [1] 0.02184518

WT_NREM_DP_Age <-c(WT_P23_P29_NREM_DP_P_Value, WT_P29_P44_NREM_DP_P_Value, WT_P44_P59_NREM_DP_P_Value)
WT_NREM_DP_Age_Adjust <- p.adjust(WT_NREM_DP_Age,method="hochberg")
# [1] 0.23527993 0.23527993 0.06553555

# Mutant
# MT P23 vs P29
MT_P23_P29_NREM_DP <- t.test(P23_MT_NREM_DP, P29_MT_NREM_DP, var.equal = TRUE)
MT_P23_P29_NREM_DP_P_Value <- MT_P23_P29_NREM_DP$p.value
# [1] 0.03155452

# MT P29 vs P44
MT_P29_P44_NREM_DP <- t.test(P29_MT_NREM_DP, P44_MT_NREM_DP, var.equal = TRUE)
MT_P29_P44_NREM_DP_P_Value <- MT_P29_P44_NREM_DP$p.value
# [1] 0.4100614

# MT P44 vs P59
MT_P44_P59_NREM_DP <- t.test(P44_MT_NREM_DP, P59_MT_NREM_DP, var.equal = TRUE)
MT_P44_P59_NREM_DP_P_Value <- MT_P44_P59_NREM_DP$p.value
# [1] 0.02734656

MT_NREM_DP_Age <-c(MT_P23_P29_NREM_DP_P_Value, MT_P29_P44_NREM_DP_P_Value, MT_P44_P59_NREM_DP_P_Value)
MT_NREM_DP_Age_Adjust <- p.adjust(MT_NREM_DP_Age,method="hochberg")
# [1] 0.06310903 0.41006140 0.06310903

#### REM Light Period, Genotype ####

# P23 WT vs MT
P23_WT_REM_LP <- Bout_Duration[c(1:7),8]
P23_WT_REM_LP

P23_MT_REM_LP <- Bout_Duration[c(8:15),8]
P23_MT_REM_LP

P23_Genotype_REM_LP <- t.test(P23_WT_REM_LP, P23_MT_REM_LP, var.equal = TRUE)
P23_Genotype_REM_LP_P_Value <- P23_Genotype_REM_LP$p.value
# [1] 0.7338524

# P29 WT vs MT
P29_WT_REM_LP <- Bout_Duration[c(16:22),8]
P29_WT_REM_LP

P29_MT_REM_LP <- Bout_Duration[c(23:31),8]
P29_MT_REM_LP

P29_Genotype_REM_LP <- t.test(P29_WT_REM_LP, P29_MT_REM_LP, var.equal = TRUE)
P29_Genotype_REM_LP_P_Value <- P29_Genotype_REM_LP$p.value
# [1] 0.2280493

# P44 WT vs MT
P44_WT_REM_LP <- Bout_Duration[c(32:37),8]
P44_WT_REM_LP

P44_MT_REM_LP <- Bout_Duration[c(38:44),8]
P44_MT_REM_LP

P44_Genotype_REM_LP <- t.test(P44_WT_REM_LP, P44_MT_REM_LP, var.equal = TRUE)
P44_Genotype_REM_LP_P_Value <- P44_Genotype_REM_LP$p.value
# [1] 0.1493518

# P59 WT vs MT
P59_WT_REM_LP <- Bout_Duration[c(45:52),8]
P59_WT_REM_LP

P59_MT_REM_LP <- Bout_Duration[c(53:59),8]
P59_MT_REM_LP

P59_Genotype_REM_LP <- t.test(P59_WT_REM_LP, P59_MT_REM_LP, var.equal = TRUE)
P59_Genotype_REM_LP_P_Value <- P59_Genotype_REM_LP$p.value
# [1] 0.01400972

REM_LP_Genotype <-c(P23_Genotype_REM_LP_P_Value, P29_Genotype_REM_LP_P_Value, P44_Genotype_REM_LP_P_Value, P59_Genotype_REM_LP_P_Value)
REM_LP_Genotype_Adjust <- p.adjust(REM_LP_Genotype,method="hochberg")
# [1] 0.73385241 0.45609857 0.44805536 0.05603889

#### REM Dark Period, Age ####

# P23 WT
P23_WT_REM_DP <- Bout_Duration[c(1:7), 9]
P23_WT_REM_DP

# P23 MT
P23_MT_REM_DP <- Bout_Duration[c(8:15), 9]
P23_MT_REM_DP

# P29 WT
P29_WT_REM_DP <- Bout_Duration[c(16:22), 9]
P29_WT_REM_DP

# P29 MT
P29_MT_REM_DP <- Bout_Duration[c(23:31), 9]
P29_MT_REM_DP

# P44 WT
P44_WT_REM_DP <- Bout_Duration[c(32:37), 9]
P44_WT_REM_DP

# P44 MT
P44_MT_REM_DP <- Bout_Duration[c(38:44), 9]
P44_MT_REM_DP

# P59 WT
P59_WT_REM_DP <- Bout_Duration[c(45:52), 9]
P59_WT_REM_DP

# P59 MT
P59_MT_REM_DP <- Bout_Duration[c(53:59), 9]
P59_MT_REM_DP

# Wild Type
# WT P23 vs P29
WT_P23_P29_REM_DP <- t.test(P23_WT_REM_DP, P29_WT_REM_DP, var.equal = TRUE)
WT_P23_P29_REM_DP_P_Value <- WT_P23_P29_REM_DP$p.value
# [1] 0.0002443185

# WT P29 vs P44
WT_P29_P44_REM_DP <- t.test(P29_WT_REM_DP, P44_WT_REM_DP, var.equal = TRUE)
WT_P29_P44_REM_DP_P_Value <- WT_P29_P44_REM_DP$p.value
# [1] 0.7151063

# WT P44 vs P59
WT_P44_P59_REM_DP <- t.test(P44_WT_REM_DP, P59_WT_REM_DP, var.equal = TRUE)
WT_P44_P59_REM_DP_P_Value <- WT_P44_P59_REM_DP$p.value
# [1] 0.4008784

WT_REM_DP_Age <-c(WT_P23_P29_REM_DP_P_Value, WT_P29_P44_REM_DP_P_Value, WT_P44_P59_REM_DP_P_Value)
WT_REM_DP_Age_Adjust <- p.adjust(WT_REM_DP_Age,method="hochberg")
# [1] 0.0007329554 0.7151063309 0.7151063309

# Mutant
# MT P23 vs P29
MT_P23_P29_REM_DP <- t.test(P23_MT_REM_DP, P29_MT_REM_DP, var.equal = TRUE)
MT_P23_P29_REM_DP_P_Value <- MT_P23_P29_REM_DP$p.value
# [1] 0.7490235

# MT P29 vs P44
MT_P29_P44_REM_DP <- t.test(P29_MT_REM_DP, P44_MT_REM_DP, var.equal = TRUE)
MT_P29_P44_REM_DP_P_Value <- MT_P29_P44_REM_DP$p.value
# [1] 0.1030271

# MT P44 vs P59
MT_P44_P59_REM_DP <- t.test(P44_MT_REM_DP, P59_MT_REM_DP, var.equal = TRUE)
MT_P44_P59_REM_DP_P_Value <- MT_P44_P59_REM_DP$p.value
# [1] 0.1119802

MT_REM_DP_Age <-c(MT_P23_P29_REM_DP_P_Value, MT_P29_P44_REM_DP_P_Value, MT_P44_P59_REM_DP_P_Value)
MT_REM_DP_Age_Adjust <- p.adjust(MT_REM_DP_Age,method="hochberg")
# [1] 0.7490235 0.2239604 0.2239604



