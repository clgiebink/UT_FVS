#visualize model coefficients for Ponderosa pine

load("./glmm_pp_z.Rdata")

#lmm
#reduced based on vif
#slope^2
lmm1_pp <- lmer(log(dds)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR_weib+I(z.CR_weib^2)+
                  #climate
                  z.wateryr+z.tmax_JunAug+
                  #competition/density
                  z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+
                  z.sin+z.cos+
                  #random effects
                  (1+z.DIA_C|TRE_CN)+(1|Year),
                data = glmm_pp_z)

#glmm
#reduced based on vif
#slope^2
glmm1_pp <- glmer(dds~
                   #tree variables
                   z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                   z.CR_weib+I(z.CR_weib^2)+
                   #climate
                   z.wateryr+z.tmax_JunAug+
                   #competition/density
                   z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                   #site variables
                   z.SICOND+z.SLOPE+
                   z.sin+z.cos+
                   #random effects
                   (1+z.DIA_C|TRE_CN)+(1|Year),
                 data = glmm_pp_z,
                 family = Gamma(link = "log"),
                 glmerControl(optimizer = "bobyqa", 
                              optCtrl = list(maxfun = 100000)))


lmm_pp <- tidy(lmm1_pp) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       "I(z.CR_weib^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

glmm_pp <- tidy(glmm1_pp) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "GLMM")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" ="DBH^2",
                       z.CR_weib = "Crown Ratio",
                       "I(z.CR_weib^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models1_pp <- full_join(lmm_pp,glmm_pp)

dwplot(models1_pp, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Ponderosa Pine") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

three_brackets <- list(c("Tree", "DBH", "Crown Ratio^2"), 
                       c("Climate", "Precipitation", "Temperature"),
                       c("Density", "PCCF", "CCF"),
                       c("Site", "Site Index", "cos(Aspect-0.7854)*Slope"))

{dwplot(models1_pp, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Ponderosa Pine") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.05,0.93),
        legend.justification=c(0, 1),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank())} %>%
  add_brackets(three_brackets)

#CR & Random Effects influence
lmm_re <- lmer(log(dds)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR_weib+I(z.CR_weib^2)+
                  #climate
                  z.wateryr+z.tmax_JunAug+
                  #competition/density
                  z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+
                  z.sin+z.cos+
                  #random effects
                  (1|TRE_CN)+(1|Year),
                data = glmm_pp_z)

lmm_CR <- lmer(log(dds)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR+I(z.CR^2)+
                  #climate
                  z.wateryr+z.tmax_JunAug+
                  #competition/density
                  z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+
                  z.sin+z.cos+
                  #random effects
                  (1+z.DIA_C|TRE_CN)+(1|Year),
                data = glmm_pp_z)

lmm_CR_re <- lmer(log(dds)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR+I(z.CR^2)+
                  #climate
                  z.wateryr+z.tmax_JunAug+
                  #competition/density
                  z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+
                  z.sin+z.cos+
                  #random effects
                  (1|TRE_CN)+(1|Year),
                data = glmm_pp_z)

lmm_rane <- tidy(lmm_re) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_rmre") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       "I(z.CR_weib^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

lmm_CR_pp <- tidy(lmm_CR) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_CR") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       "I(z.CR^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
lmm_CRre_pp <- tidy(lmm_CR_re) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_rm_CR") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       "I(z.CR^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models_pp <- full_join(lmm_pp,lmm_rane) %>%
  full_join(.,lmm_CR_pp) %>%
  full_join(.,lmm_CRre_pp)

dwplot(models_pp, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Ponderosa Pine") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)
