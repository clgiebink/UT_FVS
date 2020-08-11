#Swift radiation parameter----
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
  if((LAT+SLOPE > 66) & (LAT <= 45 | LAT >= 315)){
    D1 <- cos(SLOPE) * cos(LAT) - sin(SLOPE) * sin(LAT) * cos(ASPECT)
    ifelse(D1 == 0, D1 <- 0.0000000001, D1 <- D1)
    L2 = atan(sin(SLOPE)*sin(ASPECT)/D1)
    ifelse(D1 < 0, L2 <- L2+180, L2 <- L2)
  }
  else{
    L2 <- atan((sin(SLOPE) * sin(ASPECT))/
                            (cos(SLOPE) * cos(LAT) - sin(SLOPE) * sin(LAT) * cos(ASPECT)))
  }
  sol_rad_df$L2 <- L2
  
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
    #prints undefined S values
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
    if((LAT+SLOPE > 66) & (LAT <= 45 | LAT >= 315)){
      ifelse(T3 < T2, (T2 <- 0) & (T3 <- 0), (T2 <- T2) & (T3 <- T3))
      T6 <- T6 + 360
      if(T6 < T1){
        T8 <- T6
        T9 <- T1
        R4 <- R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T3-T2) / 15 + 
                      cos(D) * cos(sol_rad_df$L1[i]) * (sin(T3+sol_rad_df$L2[i]) - sin(T2+sol_rad_df$L2[i])) * 12/pi) +
          R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T9-T8) / 15 + 
                  cos(D) * cos(sol_rad_df$L1[i]) * (sin(T9+sol_rad_df$L2[i]) - sin(T8+sol_rad_df$L2[i])) * 12/pi)
      } else {
        T7 <- T7 - 360
        if(T7 > T0){
          T8 <- T0
          T9 <- T7
          R4 <- R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T3-T2) / 15 + 
                        cos(D) * cos(sol_rad_df$L1[i]) * (sin(T3+sol_rad_df$L2[i]) - sin(T2+sol_rad_df$L2[i])) * 12/pi) +
            R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T9-T8) / 15 + 
                    cos(D) * cos(sol_rad_df$L1[i]) * (sin(T9+sol_rad_df$L2[i]) - sin(T8+sol_rad_df$L2[i])) * 12/pi)
        } else {
          R4 <- R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T3-T2) / 15 + 
                        cos(D) * cos(sol_rad_df$L1[i]) * (sin(T3+sol_rad_df$L2[i]) - sin(T2+sol_rad_df$L2[i])) * 12/pi)
        }
      }

    } else {
      R4 <- R1 + (sin(D) * sin(sol_rad_df$L1[i]) * (T3-T2) / 15 + 
                    cos(D) * cos(sol_rad_df$L1[i]) * (sin(T3+sol_rad_df$L2[i]) - 
                                                        sin(T2+sol_rad_df$L2[i])) * 12/pi)
      #TODO check for negatives
    }
    sol_rad_df$R4[i] <- R4
  }
  return(sol_rad_df$R4)
}

#load data
load("./data/formatted/data_all.Rdata")

#test function
sol_rad_test <- data_all %>%
  ungroup()%>%
  filter(SPCD %in% c(93, 122, 202)) %>%
  select(TRE_CN,LAT,SLOPE,tASPECT) %>%
  distinct()%>% #only unique values
  slice(rep(1:n(), each = 365)) %>% #repeat each row 365 times
  group_by(TRE_CN) %>% #loop function over each tree
  mutate(Day = seq(1,365,1),
         R4 = swift_rad(TRE_CN = TRE_CN,LAT = LAT,SLOPE = SLOPE,ASPECT = tASPECT))

summary(sol_rad_all$R4) #NAs
#TODO check NAs
solrad_check <- data_all %>%
  ungroup()%>%
  filter(SPCD %in% c(93, 122, 202)) %>%
  select(TRE_CN,LAT,SLOPE,tASPECT) %>%
  distinct()%>%
  mutate(limit = LAT + SLOPE) #check paper - this needs to be a certain number or what???

#once the above works...
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

# Notes

#R3 is potential solar radiation for horizontal surface
R3 = R1 + (sin(D) * sin(L0) * (T1-T0) / 15 + 
             cos(D) * cos(L0) * (sin(T1) - sin(T0)) * 12/pi)
#F ratio of potential solar radiation for a slope surface to horizontal surface
#could be used to estimate actual solar radiation
#if R2 (radiation measured on nearby horizontal surface) not available
#F can be used as index of relative energy
F = R4 / R3
#continue if R2 input
  
#if poleward-facing slope
#when the sum of the slope inclination plus the absolute value of the latitude of the slope exceeds 66

#solrad ----
library(solrad)
load("./data/formatted/incr_calcov.Rdata")

incr_calcov <- incr_calcov %>%
  mutate(tASPECT = ifelse(is.na(ASPECT) & SLOPE <= 5, 0, ASPECT))
miss_asp <- unique(incr_calcov$TRE_CN[is.na(incr_calcov$tASPECT)]) 
asp_check_df <- incr_calcov %>% 
  filter(TRE_CN %in% miss_asp) %>% 
  select(TRE_CN,SPCD,ASPECT,SLOPE) %>% #most are 106 - pinyon
  distinct()
save(incr_calcov, file = "./data/formatted/incr_calcov.Rdata")

un_tre_cov <- incr_calcov %>%
  select(-c(Year,RW,DIA_C)) %>%
  ungroup() %>%
  distinct() #same as per_cov
length(unique(un_tre_cov$TRE_CN)) #568
save(un_tre_cov, file = "./data/formatted/un_tre_cov.Rdata")


#function applied to all trees.
seas_dirad <- function(begin, end, Lat, Lon, Elevation, Slope, Aspect) {
  DOY <- seq(1,365,1)
  aspect_s <- ifelse(Aspect[1] <= 180, Aspect[1] + 180, Aspect[1] - 180)
  yr_dirad <- DirectRadiation(DOY = DOY, Lat = abs(Lat[1]), Lon = abs(Lon[1]), #lat & lon in degrees
                              SLon = -105, DS = 60, #Slon and DS for UT; SLon = -7*15, DS = 60 minutes
                              Elevation = Elevation[1]/3.281, #from ft to meters
                              Slope[1], Aspect = aspect_s) #Aspect
  sum_rad = sum(yr_dirad[begin:end])
  return(sum_rad) #W/m2
}

#JanApr = (1:120)
#MayAug = (121:243)
#SepDec = (244:365)

incr_calcov <- incr_calcov %>%
  group_by(TRE_CN) %>%
  mutate(solrad_an = seas_dirad(begin = 1, end = 365, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = tASPECT),
         solrad_JanApr = seas_dirad(begin = 1, end = 120, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = tASPECT),
         solrad_MayAug = seas_dirad(begin = 121, end = 243, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = tASPECT),
         solrad_SepDec = seas_dirad(begin = 244, end = 365, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = tASPECT))
save(incr_calcov,file = "./data/formatted/incr_calcov.Rdata")

#Validation data ----

val_dset <- val_dset %>%
  mutate(sin = sin((ASPECT * (pi/180)) - 0.7854) * SLOPE,
         cos = cos((ASPECT * (pi/180)) - 0.7854) * SLOPE)

#function applied to all trees.
#seas_dirad
#JanApr = (1:120)
#MayAug = (121:243)
#SepDec = (244:365)

val_dset <- val_dset %>%
  group_by(TRE_CN) %>%
  mutate(solrad_an = seas_dirad(begin = 1, end = 365, Lat = LAT, Lon = LON, 
                                Elevation = ELEV, Slope = SLOPE, Aspect = ASPECT),
         solrad_JanApr = seas_dirad(begin = 1, end = 120, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = ASPECT),
         solrad_MayAug = seas_dirad(begin = 121, end = 243, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = ASPECT),
         solrad_SepDec = seas_dirad(begin = 244, end = 365, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = ASPECT))

save(val_dset,file = "./data/formatted/val_dset.Rdata")
