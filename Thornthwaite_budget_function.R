
#create Thornthwaite water budget function

#P is 1 year of monthly Precip (12 values)
#Temp is 1 year of monthly Temperature (12 values)
#lat is decimal degree latitude (1 value)
#lon is decimal degree longitude (1 value)
#SOILmax is soil AWC (1 value)

thorn_budget <- function(P, Temp, lat, lon, SOILmax){
  
  library(tidyverse)
  
  # Variable allocation
  Month <- seq(1,12)
  PET <- rep(0,12)
  F1 <- rep(0,12)
  RAIN <- rep(0,12)
  SNOW <- rep(0,12)
  PACK <- rep(0,12)
  MELT <- rep(0,12)
  W <- rep(0,12)
  W_PET <- rep(0,12)
  SOIL <- rep(0,12)
  dSOIL <- rep(0,12)
  ET <- rep(0,12)
  SURPLUS <- rep(0,12)
  MonthName <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  #Thornthwaite ET, mm/month
  
  # latitude correction
  din <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) #days in month
  doy <- c(16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350)
  gamma <- 2 * pi * (doy - 1) / 365
  del <- (180 / pi) * (0.006918 - 0.399912 * cos(gamma) + 0.070257 * sin(gamma) - 0.006758 * cos(2 * gamma) + 0.000907 * sin(2 * gamma))
  Ts <- abs(acos(-tan(del * pi / 180) * tan(lat * pi / 180)) / 0.2618)
  daylen <- Ts * 2
  LCF <- daylen / 12 * din / 30
  
  # heat index
  i <- which(Temp > 0)
  HI <- rep(0,12)
  HI[i] <- (Temp[i] / 5) ^ 1.514
  I <- sum(HI) # heat index
  
  # Thornthwaite ET
  a <- 6.57e-7 * I ^ 3 - 7.71e-5 * I ^ 2 + 0.0179 * I + 0.49
  PET <- LCF * 16 * (10 * Temp / I) ^ a
  
  i <- which(PET < 0)
  if (length(i) > 0) PET[i] <- 0
  
  z <- which(is.na(PET) == TRUE)
  if(length(z) > 0) PET[z] <- 0
  
  
  
  # ------------ SOIL WATER BALANCE ------------------
  
  # LOOP
  for (j in 1:100){ #iterate to resolve Dec-Jan dependence
    for (i in 2:12){
      # melt factor
      if (Temp[i] <= 0)
        F1[i] <- 0
      else if (Temp[i] > 0 & Temp[i] < 6)
        F1[i] <- (1 / 6) * Temp[i]
      else
        F1[i] <- 1
      
      # RAIN & SNOW
      RAIN[i] <- F1[i]*P[i]
      SNOW[i] <- (1 - F1[i]) * P[i]
      
      # PACK & MELT
      PACK[i] <- (1 - F1[i]) ^ 2 * P[i] + (1 - F1[i]) * PACK[i-1]
      MELT[i] <- F1[i] * (PACK[i-1] + SNOW[i])
      
      # W input
      W[i] <- RAIN[i] + MELT[i]
      
      # W - PET, available moisture check
      W_PET[i] <- W[i] - PET[i]
      
      # SOIL & ET
      if (W[i] >= PET[i]){
        SOIL[i] <- min(c(W_PET[i] + SOIL[i-1], SOILmax))
        ET[i] <- PET[i]
      }else if (W[i] < PET[i]){
        SOIL[i] <- SOIL[i-1] * (exp(-(PET[i] - W[i]) / SOILmax))
        ET[i] <- W[i] + SOIL[i-1] - SOIL[i]
      }
      
      dSOIL[i] <- SOIL[i]-SOIL[i-1]
      
      # SURPLUS
      SURPLUS[i] <- W[i] - dSOIL[i] - ET[i]
    }
    
    
    # Jan calcs
    if (Temp[1] <= 0)
      F1[1] <- 0
    else if (Temp[1] > 0 & Temp[1] < 6)
      F1[1] <- (1 / 6) * Temp[1]
    else
      F1[1] <- 1
    
    RAIN[1] <- F1[1] * P[1]
    SNOW[1] <- (1 - F1[1]) * P[1]
    PACK[1] <- (1 - F1[1]) ^ 2 * P[1] + (1 - F1[1]) * PACK[12]
    MELT[1] <- F1[1] * (PACK[12] + SNOW[1])
    W[1] <- RAIN[1] + MELT[1]
    W_PET[1] <- W[1] - PET[1]
    
    if (W[1] >= PET[1]){
      SOIL[1] <- min(c(W_PET[1] + SOIL[12], SOILmax))
      ET[1] <- PET[1]
    }else if (W[1] < PET[1]){
      SOIL[1] <- SOIL[12] * (exp(-(PET[1] - W[1]) / SOILmax))
      ET[1] <- W[1] + SOIL[12]-SOIL[1]
    }
    
    dSOIL[1] <- SOIL[1] - SOIL[12]
    SURPLUS[1] <- W[1] - dSOIL[1] - ET[1]
  }
  
  SURPLUS[SURPLUS < 0] <- 0
  
  Budget <- cbind.data.frame(Month, MonthName, P, Temp, PET, F1, RAIN, SNOW, 
                            PACK, MELT, W, W_PET, SOIL, dSOIL, ET, SURPLUS,
                            stringsAsFactors = FALSE) 
  
  Budget <- Budget %>% mutate(across(3:16, round, 1))
  
  return(Budget)
}