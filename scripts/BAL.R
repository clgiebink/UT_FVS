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

glmm.data.imputed %>%
  mutate(BA_pa = (DIA_C^2 * 0.0005454) * TPA_UNADJ)

#or

for(i in 1:nrow(glmm.data.imputed)){
  glmm.data.imputed$BA_pa[i] <- 
    ((glmm.data.imputed$DIA_C[i]^2)*0.0005454) * glmm.data.imputed$TPA_UNADJ[i]
}

#rank trees per year per plot
#BAL
#sum BApa of trees larger on the same plot in same year

rank_pltyr <- vector(mode="numeric", length=nrow(glmm.data.imputed))
BAL <- vector(mode="numeric", length=nrow(glmm.data.imputed))
for(i in 1:nrow(glmm.data.imputed)){
  plot_cn <- glmm.data.imputed$PLT_CN.y[i]
  year <- glmm.data.imputed$Year[i]
  rank_df <- glmm.data.imputed[glmm.data.imputed$Year == year 
                               & glmm.data.imputed$PLT_CN.y == plot_cn,]
  rank_df %>%
    mutate(rank = dense_rank(DIA_C))
  glmm.data.imputed$rank_pltyr[i] <- rank_df$rank[i]
  rank_t <- rank_df$rank[i]
  glmm.data.imputed$BAL[i] <- sum(rank_df$BA_pa[rank_df$rank > rank_t])
}


