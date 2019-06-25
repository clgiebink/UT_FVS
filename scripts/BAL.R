#Calculate BAL from annualized DBH
#BAL = total basal area in trees larger than the subject tree
#Courtney Giebink
#clgiebink@email.arizona.edu
#4-26-19

#load data
load(file = "./data/formatted/glmm.data.imputed")

#basal area
# = dbh^2 * 0.0005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa

glmm.data.imputed <- glmm.data.imputed %>%
  mutate(BA_pa = (DIA_C^2 * 0.0005454) * TPA_UNADJ)


#rank trees per year per plot
#BAL
#sum BApa of trees larger on the same plot in same year

glmm.data.imputed <- glmm.data.imputed %>%
  group_by(PLT_CN.y,Year) %>%
  mutate(rank_pltyr = rank(DIA_C, na.last = TRUE, ties.method = "min")) %>%
  mutate(BAL = map_dbl(BA_pa,~sum(BA_pa[BA_pa>.x],na.rm = TRUE)))

#check to make sure dense rank is okay

save(glmm.data.imputed,file = "./data/formatted/glmm.data.imputed")
