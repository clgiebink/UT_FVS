#visualize model coefficients for Engelmann spruce

load("./glmm_es_z.Rdata")

#lmm
#reduce based on vif score
#ccf
lmm1_es <- lmer(log(dds+0.001)~
                     #tree variables
                     z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                     z.CR_weib+I(z.CR_weib^2)+
                     #climate
                     z.wateryr+z.tmax_pAug+
                     #competition/density
                     z.BAL+z.PCCF+ #remove /100 due to standardization
                     #site variables
                     z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                     z.sin+z.cos+
                     #random effects
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)

#glmm
#reduce based on vif
#ccf
glmm1_es <- glmer((dds+0.001)~
                    #tree variables
                    z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                    z.CR_weib+I(z.CR_weib^2)+
                    #climate
                    z.wateryr+z.tmax_pAug+
                    #competition/density
                    z.BAL+z.PCCF+ #remove /100 due to standardization
                    #site variables
                    z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                    z.sin+z.cos+
                    #random effects
                    (1+z.DIA_C|TRE_CN)+(1|Year),
                  data = glmm_es_z,
                  family = Gamma(link = "log"),
                  glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 100000)))


lmm_es <- tidy(lmm1_es) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       "I(z.CR_weib^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
glmm_es <- tidy(glmm1_es) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "GLMM")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       "I(z.CR_weib^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models1_es <- full_join(lmm_es,glmm_es)

dwplot(models1_es, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Engelmann Spruce") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

three_brackets <- list(c("Tree", "DBH", "Crown Ratio^2"), 
                       c("Climate", "Precipitation", "Temperature"),
                       c("Density", "BAL", "CCF"),
                       c("Site", "Site Index", "cos(Aspect-0.7854)*Slope"))

{dwplot(models1_es, 
        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    ggtitle("Growth for Englemann Spruce") +
    theme(plot.title = element_text(face="bold"),
          legend.position = c(0.05,0.93),
          legend.justification=c(0, 1),
          legend.background = element_rect(colour="grey80"),
          legend.title = element_blank())} %>%
  add_brackets(three_brackets)

#CR & Random Effects
lmm1_ranef_es <- lmer(log(dds+0.001)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR_weib+I(z.CR_weib^2)+
                  #climate
                  z.wateryr+z.tmax_pAug+
                  #competition/density
                  z.BAL+z.PCCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                  z.sin+z.cos+
                  #random effects
                  (1|TRE_CN)+(1|Year),
                data = glmm_es_z)
lmm1_CR_es <- lmer(log(dds+0.001)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR+I(z.CR^2)+
                  #climate
                  z.wateryr+z.tmax_pAug+
                  #competition/density
                  z.BAL+z.PCCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                  z.sin+z.cos+
                  #random effects
                  (1+z.DIA_C|TRE_CN)+(1|Year),
                data = glmm_es_z)
lmm1_CRre_es <- lmer(log(dds+0.001)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR+I(z.CR^2)+
                  #climate
                  z.wateryr+z.tmax_pAug+
                  #competition/density
                  z.BAL+z.PCCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                  z.sin+z.cos+
                  #random effects
                  (1|TRE_CN)+(1|Year),
                data = glmm_es_z)

lmm_ranef <- tidy(lmm1_ranef_es) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_rmre") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       "I(z.CR_weib^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
lmm_CR <- tidy(lmm1_CR_es) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_CR") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       "I(z.CR^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
lmm_CRre <- tidy(lmm1_CRre_es) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_rm_CR") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       "I(z.CR^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
models_cre <- full_join(lmm_es,lmm_ranef)%>%
  full_join(.,lmm_CR)%>%
  full_join(.,lmm_CRre)

dwplot(models_cre, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Englemann spruce") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)
