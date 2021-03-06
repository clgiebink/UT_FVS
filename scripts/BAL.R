#Calculate BAL from annualized DBH
#BAL = total basal area in trees larger than the subject tree
#Courtney Giebink
#clgiebink@email.arizona.edu
#4-26-19

#load data
load(file = "./data/formatted/density_data")

#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa

density_data <- density_data %>%
  mutate(BA_pa = ((DIA_C^2) * 0.005454) * TPA_C)


#rank trees per year per plot
#BAL
#sum BApa of trees larger on the same plot in same year

density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  mutate(rank_pltyr = rank(DIA_C, na.last = TRUE, ties.method = "min")) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA_C,~sum(BA_pa[DIA_C>.x],na.rm = TRUE)))

#check to make sure dense rank is okay
max(density_data$rank_pltyr)
#[1] 66
bal_check <- density_data %>%
  group_by(PLT_CN)%>%
  summarise(max = max(BAL))
bal_check$BALIVE <- per_cov$BALIVE[match(bal_check$PLT_CN, per_cov$PLT_CN)]
#between 0 and 10 off

save(density_data,file = "./data/formatted/density_data.Rdata")

# TPA constant ----

load("./data/formatted/dens_data_con.Rdata")

#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa

dens_data_con <- dens_data_con %>%
  mutate(BA_pa = ((DIA_C^2) * 0.005454) * TPA_UNADJ)

#BAL
#sum BApa of trees larger on the same plot in same year

density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  #mutate(rank_pltyr = rank(DIA_C, na.last = TRUE, ties.method = "min")) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA_C,~sum(BA_pa[DIA_C>.x],na.rm = TRUE)))

save(dens_data_con,file = "./data/formatted/dens_data_con.Rdata")

