#Swift radiation parameter
#Swift 1976
#Courtney Giebink
#clgiebink@gmail.com
#February 2020

#calculates daily solar radiation
#daily total in gram calories per square centimeter
#by integration equations
#inputs
#lat, slope, aspect, julian dates
swift_rad <- function(TRE_CN, LAT, SLOPE, ASPECT){
  #create dataframe with empty rows for julian date solar radiation
  sol_rad_df <- data.frame(TRE_CN, LAT, SLOPE, ASPECT, 
                           Julian = 1:365, L1 = NA, L2 = NA, R4 = NA)
  LAT <- sol_rad_df$LAT[1]
  SLOPE <- sol_rad_df$SLOPE[1]
  ASPECT <- sol_rad_df$ASPECT[1]
  #calculate constants for tree
  #L1 & L2 for given slope
  sol_rad_df$L1 <- asin(cos(SLOPE) * sin(LAT) + sin(SLOPE) * cos(LAT) * cos(ASPECT))
  #alternative route for poleward facing slope
  #when sum of the slope inclination + latitude of the slope exceeds 66
  #north poleward is azimuth/aspect <= 45 or >= 315
  
  sol_rad_df$L2 <- atan((sin(SLOPE) * sin(ASPECT))/
                 (cos(SLOPE) * cos(LAT) - sin(SLOPE) * sin(LAT) * cos(ASPECT)))
  #julian date in row
  #func1
  #Z = W - X * cos((J+Y) * 0.986)
  #D = solar declination
  #D = func1
  for(i in 1:nrow(sol_rad_df)){
    J <- sol_rad_df$Julian[i]
    D <- asin(0.39785 * sin(278.9709 + 0.9856*J
                                  + 1.9163 * sin(356.6153+0.9856*J))) #complex
    #E is radius vector of sun
    #E = func1
    E <- 1 - 0.0167 * cos((J - 3) * 0.986)
    #R1 is solar constant for 60 min period adjusted for eccentriciy of the orbit of earth
    R1 <- 60 * 1.95 / (E*E)
    #RO solar constant
    #frank and lee (1966) use 2
    #drummond et al. (1986) use 1.95
    
    #func2 calculation of sunrise and sunset times
    #Z = arcos(-tan(Y) * tan(D))
    #T = func2
    S <- acos(-tan(sol_rad_df$L1[i]) * tan(D))
    #TODO check undefined
    if(is.nan(S)){
      cat("L1[", i, "]=",sol_rad_df$L1[i],"\n")
      cat("D=",D, ", tan(D)=",tan(D), "\n")
      cat("TRE_CN[",i,"]=",TRE_CN[i], "\n\n")
    }
    
    T7 <- S - sol_rad_df$L2[i]
    T6 <- -S - sol_rad_df$L2[i]
    
    S2 <- acos(-tan(sol_rad_df$LAT[i]) * tan(D))
    
    T1 <- S2
    T0 <- -S2
    
    T3 <- NA
    T2 <- NA
    
    ifelse(T7 < T1, T3 <- T7, T3 <- T1)
    ifelse(T6 > T0, T2 <- T6, T2 <- T0)
    
    #R4 is potential solar radiation for mountain slope
    R4 <- R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T3-T2) / 15 + 
                 cos(D) * cos(sol_rad_df$L1[i]) * (sin(T3+sol_rad_df$L2[i]) - 
                                                     sin(T2+sol_rad_df$L2[i])) * 12/pi)
    #TODO check for negatives
    
    sol_rad_df$R4[i] <- R4
  }
  return(sol_rad_df$R4)
}

sol_rad_all <- data_all %>%
  ungroup()%>%
  filter(SPCD %in% c(93, 122, 202)) %>%
  select(TRE_CN,LAT,SLOPE,tASPECT) %>%
  distinct()%>% #only unique values
  slice(rep(1:n(), each = 365)) %>% #repeat each row 365 times
  group_by(TRE_CN) %>%
  mutate(Day = seq(1,365,1),
         R4 = swift_rad(TRE_CN = TRE_CN,LAT = LAT,SLOPE = SLOPE,ASPECT = tASPECT))

summary(sol_rad_all$R4) #NAs
#TODO check NAs
solrad_check <- data_all %>%
  ungroup()%>%
  filter(SPCD %in% c(93, 122, 202)) %>%
  select(TRE_CN,LAT,SLOPE,tASPECT) %>%
  distinct()%>%
  mutate(limit = LAT + SLOPE)

#calculate seasonal and annual solar radiation
seas_solrad <- sol_rad_all %>%
  group_by(TRE_CN) %>%
  summarise(mean_annual = mean(R4),
            mean_seas_JanApr = mean(R4[Day %in% c(1:120)]),
            mean_seas_MayAug = mean(R4[Day %in% c(121:243)]),
            mean_seas_SepDec = mean(R4[Day %in% c(244:365)]),
            ttl_annual = sum(R4),
            ttl_seas_JanApr = sum(R4[Day %in% c(1:120)]),
            ttl_seas_MayAug = sum(R4[Day %in% c(121:243)]),
            ttl_seas_SepDec = sum(R4[Day %in% c(244:365)]))
# 70 trees with NAs

# Notes ----

#func1
#Z = W - X * cos((J+Y) * 0.986)
# D = solar declination
#D = func1
D = 0.4 - 23.3 * cos((J + 10) * 0.986)
D = arcsin(0.39785 * sin(278.9709 + 0.9856*J
                         + 1.9163 * sin(356.6153+0.9856*J)))
#alternative for radians


#func2 calculation of sunrise and sunset times
#Z = arcos(-tan(Y) * tan(D))
#T = func2
T = arcos(-tan(L1) * tan(D))
#TODO check undefined ----

T7 = T - L2
T6 = -T - L2

T = arcos(-tan(lat) * tan(D))

T1 = T
T0 = -T

ifelse(T7 < T1, T3 = T7, T3 = T1)
ifelse(T6 > T0, T2 = T6, T2 = T0)

#func3(V, W, X, Y) integrated equation
#Z = R1 + (sin(D) * sin(W) * (X-Y) / 15 + 
# cos(D) * cos(W) * (sin(X+V) - sin(Y+V)) * 12/pi)
#R4 is potential solar radiation for mountain slope
R4 = R1 + (sin(D) * sin(L1) * (T3-T2) / 15 + 
             cos(D) * cos(L1) * (sin(T3+L2) - sin(T2+L2)) * 12/pi)
#TODO check for negatives ----
#may need to use other subroutine

T4 = T2 / 15
T5 = T3 / 15

#print T4, T5, R4
#R3 is potential solar radiation for horizontal surface
R3 = R1 + (sin(D) * sin(L0) * (T1-T0) / 15 + 
             cos(D) * cos(L0) * (sin(T1) - sin(T0)) * 12/pi)
#F ratio of potential solar radiation for a slope surface to horizontal surface
#could be used to estimate actual solar radiation
#if R2 (radiation measured on nearby horizontal surface) not available
#F can be used as index of relative energy
F = R4 / R3
#continue if R2 input

#loop to other julian dates
  
#if poleward-facing slope
#when the sum of the slope inclination plus the absolute value of the latitude of the slope exceeds 66