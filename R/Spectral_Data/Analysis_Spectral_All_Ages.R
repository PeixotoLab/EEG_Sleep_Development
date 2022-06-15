# Analysis for Spectral Data: ALL AGES
# Katie Ford, Date: 5/26/22
# Adapted from the Peixoto eLife paper: https://github.com/elifesciences-publications/peixoto

# GAM plots

library(nlme) # Version 3.1-157

library(mgcv) # Version 1.8-40

library(readxl) # Version 1.4.0

library(dplyr) # Version 1.0.9

library(tidyr) # Version 1.2.0

# Light Period: 
data <- read_xlsx("All_Ages_BL_LP_Spectral_Organized_by_State.xlsx")
# Dark Period:
# data <- read_xlsx("All_Ages_BL_DP_Spectral_Organized_by_State.xlsx")

data <- data %>% gather(Hertz, value, -c(1:3))
data <- data %>%
  mutate(GT=replace(GT,GT == 1, "WT")) %>%
  mutate(GT=replace(GT,GT == 2, "MT")) %>%
  mutate(AGE=replace(AGE,AGE == 1, "P23")) %>%
  mutate(AGE=replace(AGE,AGE == 2, "P29")) %>%
  mutate(AGE=replace(AGE,AGE == 3, "P44")) %>%
  mutate(AGE=replace(AGE,AGE == 4, "P59")) %>%
  mutate(hz= as.numeric(Hertz)) %>%
  mutate(STATE = factor(STATE, levels = unique(STATE))) %>%
  mutate(GT = factor(GT, levels = unique(GT))) %>%
  mutate(AGE = factor(AGE, levels = unique(AGE))) %>%
  mutate(value = replace(value,value == -99, NA))

index <-paste(data$STATE, data$AGE, sep = "")
index_lev <- unique(index)
length(index_lev)
# [1] 12
head(index_lev)
# [1] "WAKEFULNESSP23" "WAKEFULNESSP29" "WAKEFULNESSP44" "WAKEFULNESSP59" "NREMP23"       
# [6] "NREMP29" 

layout(matrix(seq_len(12), nrow = 3, ncol = 4, byrow = TRUE)) 
par(mar = c(1.5, 1.5, 1.5, 1.5))
shadow_col <- c(rgb(109, 109, 109, max = 255, alpha = 80), #black
                rgb(244, 66, 66, max = 255, alpha = 80)) #red

for(this_index in index_lev) {
  state <- unique(data[index == this_index, ][, 1]) #STATE
  state <- as.character(unlist(state))
  age <- unique(data[index == this_index, ][, 3]) #AGE
  age <- as.character(unlist(age)) 
  
  #temp2 <- data[index == this_index, ]
  #plot(x = temp2$hz, y = temp2$value, type = "n",
     #ylab = "% of Total Power", ylim = c(0,8),
     #xlab = 'Hertz', lwd = 3, cex = 1.2,
     #main = paste0(state, "-", age), axes = FALSE)
  #axis(1, at = seq(0, 20, by = 5), las = 1, pos = 0, lwd = 3) #x-axis
  #axis(2, at = seq(0, 8, by = 2), las = 2, pos = 0, lwd = 3) # y-axis
  
  temp2 <- data[index == this_index, ]
  plot(x = temp2$hz, y = temp2$value, type = "n",
       ylab = "", ylim = c(0,8),
       xlab = "", lwd = 3, cex = 1.2,
       main = paste0(state, "-", age), axes = FALSE)
  axis(1, at = seq(0, 20, by = 5), las = 1, pos = 0, lwd = 3) #x-axis
  axis(2, at = seq(0, 8, by = 2), las = 2, pos = 0, lwd = 3) # y-axis
  
  mod <- list(wt = gam(value~s(hz), data = temp2[temp2$GT == "WT",]),
              mt = gam(value~s(hz), data = temp2[temp2$GT == "MT",]))
  
  for(i in seq_along(mod)) {
    ss <- seq(min(temp2$hz) + 0.1, max(temp2$hz) - 0.1, 0.1)
    pred <- predict(mod[[i]], data.frame(hz = ss), se = TRUE)
    fit <- pred$fit
    se <- pred$se.fit
    lower <- fit - 1.96 * se
    upper <- fit + 1.96 * se
    to_plot <- data.frame(hz = ss, fit, lower, upper)
    
    polygon(c(to_plot$hz, rev(to_plot$hz)),
            c(to_plot$lower, rev(to_plot$upper)),
            col = shadow_col[i],
            border = NA)
    lines(to_plot$hz, fit, lwd=2, col = c("black", "red")[i]) 
  }
  
 
}

sink('sessionInfo.txt')
sessionInfo()
sink() 

