#visualize model coefficients for Douglas fir
library(dotwhisker)
library(broom.mixed)

load("./glmm_df_z.Rdata")

#lmm
#reduced based on vif
lmm1_red_df <- lmer(log(dds)~
                    #tree variables
                    z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                    z.CR_weib+
                    #climate
                    z.wateryr+z.tmax_FebJul+
                    #competition/density
                    z.BAL+z.CCF+ #remove /100 due to standardization
                    #site variables
                    z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                    z.sin+z.cos+
                    #random effects
                    (1+z.DIA_C|TRE_CN)+(1|Year),
                  data = glmm_df_z)

#glmm
glmm1_red_df <- glmer(dds~
                    #tree variables
                    z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                    z.CR_weib+
                    #climate
                    z.wateryr+z.tmax_FebJul+
                    #competition/density
                    z.BAL+z.CCF+ #remove /100 due to standardization
                    #site variables
                    z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                    z.cos+
                    #random effects
                    (1+z.DIA_C|TRE_CN)+(1|Year),
                  data = glmm_df_z,
                  family = Gamma(link = "log"),
                  glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 100000)))

lmm1_df <- tidy(lmm1_red_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
glmm1_df <- tidy(glmm1_red_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "GLMM")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models1_df <- full_join(lmm1_df,glmm1_df)

dwplot(models1_df, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Douglas Fir") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

three_brackets <- list(c("Tree", "DBH", "Crown Ratio^2"), 
                       c("Climate", "Precipitation", "Temperature"),
                       c("Density", "BAL", "CCF"),
                       c("Site", "Site Index", "cos(Aspect-0.7854)*Slope"))

{dwplot(models1_df, 
        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    ggtitle("Growth for Douglas Fir") +
    theme(plot.title = element_text(face="bold"),
          legend.position = c(0.05,0.93),
          legend.justification=c(0, 1),
          legend.background = element_rect(colour="grey80"),
          legend.title = element_blank())} %>%
  add_brackets(three_brackets)

#check CR ....
CR_red <- lmer(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 z.CR+I(z.CR^2)+
                 #climate
                 z.wateryr+z.tmax_FebJul+
                 #competition/density
                 z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+I(z.SLOPE^2)+
                 I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                 I(cos(z.ASPECT-0.7854)*z.SLOPE)+
                 #random effects
                 (1+z.DIA_C|TRE_CN)+(1|Year),
               data = glmm_df_z)
lmmCR_df <- tidy(CR_red) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM") %>%
  relabel_predictors(c(z.DIA_C = "DBH", # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       "I(z.CR^2)" = "Crown Ratio^2",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.PCCF = "PCCF", 
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       "I(z.SLOPE^2)" = "Slope^2",
                       "I(sin(z.ASPECT - 0.7854) * z.SLOPE)" = "sin(Aspect-0.7854)*Slope",
                       "I(cos(z.ASPECT - 0.7854) * z.SLOPE)" = "cos(Aspect-0.7854)*Slope"))

modelsCR_df <- full_join(lmm1_df,lmmCR_df)

dwplot(modelsCR_df, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Douglas Fir") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

par(mfrow=c(1,2))

#how does new variables affect vif?
library(car)
lmm_df <- lmer(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 z.CR_weib+I(z.CR_weib^2)+
                 #climate
                 z.wateryr+z.tmax_FebJul+
                 #competition/density
                 z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                 I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                 I(cos(z.ASPECT-0.7854)*z.SLOPE)+
                 #random effects
                 (1+z.DIA_C|TRE_CN)+(1|Year),
               data = glmm_df_z)
vif(lmm_df)
#slope

lmm_asp_df <- lmer(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 z.CR_weib+I(z.CR_weib^2)+
                 #climate
                 z.wateryr+z.tmax_FebJul+
                 #competition/density
                 z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                 z.sin+z.cos+
                 #random effects
                 (1+z.DIA_C|TRE_CN)+(1|Year),
               data = glmm_df_z)
vif(lmm_asp_df)
#none

lmm_CR_df <- lmer(log(dds)~
                    #tree variables
                    z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                    z.CR+I(z.CR^2)+
                    #climate
                    z.wateryr+z.tmax_FebJul+
                    #competition/density
                    z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                    #site variables
                    z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                    I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                    I(cos(z.ASPECT-0.7854)*z.SLOPE)+
                    #random effects
                    (1+z.DIA_C|TRE_CN)+(1|Year),
                  data = glmm_df_z)
vif(lmm_CR_df)


lmm_aspCR_df <- lmer(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 z.CR+I(z.CR^2)+
                 #climate
                 z.wateryr+z.tmax_FebJul+
                 #competition/density
                 z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                 z.sin+z.cos+
                 #random effects
                 (1+z.DIA_C|TRE_CN)+(1|Year),
               data = glmm_df_z)
vif(lmm_aspCR_df)


#standardize sin and cos after transforming
lmm_red_b <- lmer(log(dds)~
                    #tree variables
                    z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                    z.CR_weib+
                    #climate
                    z.wateryr+z.tmax_FebJul+
                    #competition/density
                    z.BAL+z.CCF+ #remove /100 due to standardization
                    #site variables
                    z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                    z.sin+z.cos+
                    #random effects
                    (1+z.DIA_C|TRE_CN)+(1|Year),
                  data = glmm_df_z)
lmm_rane_red <- lmer(log(dds)~
                   #tree variables
                   z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                   z.CR_weib+
                   #climate
                   z.wateryr+z.tmax_FebJul+
                   #competition/density
                   z.BAL+z.CCF+ #remove /100 due to standardization
                   #site variables
                   z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                   z.sin+z.cos+
                   #random effects
                   (1|TRE_CN)+(1|Year),
                 data = glmm_df_z)

lmm_CR_b <- lmer(log(dds)~
                   #tree variables
                   z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                   z.CR+
                   #climate
                   z.wateryr+z.tmax_FebJul+
                   #competition/density
                   z.BAL+z.CCF+ #remove /100 due to standardization
                   #site variables
                   z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                   z.sin+z.cos+
                   #random effects
                   (1+z.DIA_C|TRE_CN)+(1|Year),
                 data = glmm_df_z)

lmm_re <- tidy(lmm_rane_red) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_rmre") %>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
lmm_b_df <- tidy(lmm_red_b) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

lmmCR_b_df <- tidy(lmm_CR_b) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_CR")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models_asp <- full_join(lmm_b_df,lmm_re) %>%
  full_join(.,lmmCR_b_df)

dwplot(models_asp, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Douglas Fir") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

#remove random effect 
#and add CR instead of CR_weib
lmm_rmre_CR <- lmer(log(dds)~
                       #tree variables
                       z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                       z.CR+
                       #climate
                       z.wateryr+z.tmax_FebJul+
                       #competition/density
                       z.BAL+z.CCF+ #remove /100 due to standardization
                       #site variables
                       z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                       z.sin+z.cos+
                       #random effects
                       (1|TRE_CN)+(1|Year),
                     data = glmm_df_z)
lmm_rmCR_df <- tidy(lmm_rmre_CR) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_rm_CR")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
models_asp <- full_join(lmm_b_df,lmm_re) %>%
  full_join(.,lmmCR_b_df) %>%
  full_join(.,lmm_rmCR_df)

dwplot(models_asp, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Douglas Fir") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

#compare models with different competition variables
lmm1_sin_2 <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      z.CR_weib+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.BAL+z.CCF+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+z.SLOPE+z.cos+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)

lmm_asp_df <-  tidy(lmm1_sin_2) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_check")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
lmm1_bal <- lmer(log(dds)~
                     #tree variables
                     z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                     z.CR_weib+
                     #climate
                     z.wateryr+z.tmax_FebJul+
                     #competition/density
                     z.CCF+ #remove /100 due to standardization
                     #site variables
                     z.SICOND+z.SLOPE+
                     #random effects
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)

lmm_bal_df <-  tidy(lmm1_bal) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_check")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models_check2 <- full_join(lmm1_df,lmm_bal_df)

dwplot(models_check2, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Douglas Fir") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)
