### Code to make figure 6 ####

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("lfe", "ggplot2", "foreign","stargazer", "knitr", "readstata13", "coefplot","latex2exp",
       "reshape2",  "xtable", "tidyverse", "clusterSEs", "plm" ,  "dotwhisker", "broom", "plyr","Jmisc",
       "regclass", "sda" , "care" , "mlr", "magrittr", "data.table" , "expss")

setwd("~/non_local_warming/")  #   set working directory to code repository
data_path <-  "~/Dropbox/to_octavia/"
#data_path <-  getwd()


if(exists("dataset")== FALSE)   {
  dataset <- read.csv(paste0(data_path, "data_weights.csv"), header=TRUE, stringsAsFactors = FALSE)
}
# limit to forest frontier dataset
forest <- dataset[dataset$set == "forest", ]

source("model_strings.R")


#### H_eqs: model sensitivity to varying halo groupings ####

H_eqs <- c(fy_1_10, fy_1_50,fy_1_2_50,fy_1_2_4_50,
           fy_1_2_4_10_50, fy_1_10_50 ) 
H_names <- c("fy_1_10", "fy_1_50","fy_1_2_50","fy_1_2_4_50", "fy_1_2_4_10_50", "fy_1_10_50" )
# (model specifications can be found in model_strings.R)


# H_models is a dataframe of coefficients estimates for the models  in H_eqs:
H_models <- tidy()

halos <- c("f_1to2", "f_1to4","f_1to10","f_1to50",  "f_2to4", "f_4to10" , "f_10to50",
           "f_2to50","f_4to50"  )

for (i in 1:6){  
  model_eq <- H_eqs[[i]]
  
  temp_tibble <- forest %>% 
    do(tidy(felm(model_eq, weights=.$forest_wght, data = .), conf.int = .95))  
  H_tibble <- temp_tibble %>% filter(term %in% halos)
  H_tibble$submodel <- H_names[i]
  
  H_models <- rbind(H_models,H_tibble)
  
}

H_models$model <- H_models$term
H_models

H_models <- H_models %>% relabel_predictors(c(f_1to2 = "1-2 km",
                                              f_2to4 = "2-4 km",
                                              f_4to10 = "4-10 km",
                                              f_10to50 = "10-50 km",
                                              f_1to4 =  "1-4 km",
                                              f_1to10 = "1-10 km",
                                              f_1to50 = "1-50 km",
                                              f_2to50 = "2-50 km",
                                              f_4to50 = "4-50 km")
)

rename_models <- c("fy_1_10"="1-10 km",
                   "fy_1_50"="1-50 km",
                   "fy_1_10_50"="1-10,10-50 km",
                   "fy_1_2_50"="1-2,2-50 km",
                   "fy_1_2_4_50"="1-2,2-4,4-50 km",
                   "fy_1_2_4_10_50"="1-2,2-4,4-10,10-50 km"
)

H_models$submodel <- revalue(H_models$submodel, rename_models)



a_1to2 <- pi*(2**2 - 1**2)*0.25
a_2to4 <- pi*(4**2 - 2**2)*0.25
a_4to10 <- pi*(10**2 - 4**2)*0.25
a_10to50 <- pi*(50**2 - 10**2)*0.25
a_1to10 <- pi*(10**2 - 1**2)*0.25
a_1to50 <- pi*(50**2 - 1**2)*0.25
a_2to50 <- pi*(50**2 - 2**2)*0.25
a_4to50 <- pi*(50**2 - 4**2)*0.25
a_10to50 <- pi*(50**2 - 10**2)*0.25


H_scenario <- H_models

scale_cols <- c("estimate", "std.error", "conf.low", "conf.high")

H_scenario[which(H_scenario$term == "1-2 km"),scale_cols] <-
  H_models[which(H_models$term == "1-2 km"),scale_cols]*a_1to2

H_scenario[which(H_scenario$term == "2-4 km"),scale_cols] <-
  H_models[which(H_models$term == "2-4 km"),scale_cols]*a_2to4

H_scenario[which(H_scenario$term == "4-10 km"),scale_cols] <-
  H_models[which(H_models$term == "4-10 km"),scale_cols]*a_4to10

H_scenario[which(H_scenario$term == "10-50 km"),scale_cols] <-
  H_models[which(H_models$term == "10-50 km"),scale_cols]*a_10to50

H_scenario[which(H_scenario$term == "1-10 km"),scale_cols] <-
  H_models[which(H_models$term == "1-10 km"),scale_cols]*a_1to10

H_scenario[which(H_scenario$term == "1-50 km"),scale_cols] <-
  H_models[which(H_models$term == "1-50 km"),scale_cols]*a_1to50

H_scenario[which(H_scenario$term == "2-50 km"),scale_cols] <-
  H_models[which(H_models$term == "2-50 km"),scale_cols]*a_2to50

H_scenario[which(H_scenario$term == "4-50 km"),scale_cols] <-
  H_models[which(H_models$term == "4-50 km"),scale_cols]*a_4to50




H_scenario_merge <- H_scenario %>% group_by(submodel) %>% dplyr::summarise(estimate = sum(estimate),
                                                                           std.error = sum(std.error),
                                                                           conf.low = sum(conf.low),
                                                                           conf.high = sum(conf.high))

H_scenario_merge$term <- ""
H_scenario_merge$model <- "forest frontier \n (halo models)"
H_scenario_merge$estimate <- -H_scenario_merge$estimate
H_scenario_merge$conf.high <- - H_scenario_merge$conf.high
H_scenario_merge$conf.low <- - H_scenario_merge$conf.low
# dot_args = list(aes(shape = submodel)
ystr <- TeX("$\\Delta AT$ for a 25 p.p. loss in forest cover ($\\degree$C)")


p <- small_multiple(H_scenario_merge) +
  scale_x_discrete(limits =  "forest frontier \n (halo models)") + # order the models
  theme_bw() + ylab(ystr) + ylim(-1.1, 2.5)+
  xlab("Dataset") + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(size =10 ,angle = 45, hjust = 1),
        legend.title = element_text(size=10),
        axis.title.x = element_text(size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_rect(color="gray90"),
        legend.key.size = unit(10, "pt"))  +
  labs(color='Model lengthscale') + scale_color_brewer(palette="Blues") 

p
ggsave(filename=paste0("figures/Figure5_halo.eps"), plot=p, width = 4.5, height = 4.5)


# 
# H_scenario <- H_models
# 
# scale_cols <- c("estimate", "std.error", "conf.low", "conf.high")
# 
# H_scenario[which(H_scenario$term == "1-2 km"),scale_cols] <-
#   H_models[which(H_models$term == "1-2 km"),scale_cols]*a_1to2
# 
# H_scenario[which(H_scenario$term == "2-4 km"),scale_cols] <-
#   H_models[which(H_models$term == "2-4 km"),scale_cols]*a_2to4
# 
# H_scenario[which(H_scenario$term == "4-10 km"),scale_cols] <-
#   H_models[which(H_models$term == "4-10 km"),scale_cols]*a_4to10
# 
# H_scenario[which(H_scenario$term == "10-50 km"),scale_cols] <-
#   H_models[which(H_models$term == "10-50 km"),scale_cols]*a_10to50
# 
# H_scenario[which(H_scenario$term == "1-10 km"),scale_cols] <-
#   H_models[which(H_models$term == "1-10 km"),scale_cols]*a_1to10
# 
# H_scenario[which(H_scenario$term == "1-50 km"),scale_cols] <-
#   H_models[which(H_models$term == "1-50 km"),scale_cols]*a_1to50
# 
# H_scenario[which(H_scenario$term == "2-50 km"),scale_cols] <-
#   H_models[which(H_models$term == "2-50 km"),scale_cols]*a_2to50
# 
# H_scenario[which(H_scenario$term == "4-50 km"),scale_cols] <-
#   H_models[which(H_models$term == "4-50 km"),scale_cols]*a_4to50
# 
# H_scenario
# H_scenario_grouped <- H_scenario %>% group_by(submodel) %>% dplyr::summarise(estimate = sum(estimate),
#                                                                              std.error = sum(std.error))
# 
# saveRDS(H_models, paste0(data_path,"results/H_models.rds"))
# write.csv(H_models, paste0(data_path,"results/H_models.csv"))
# 
# 
# 
# saveRDS(H_scenario, paste0(data_path,"results/H_scenario.rds"))
# write.csv(H_scenario, paste0(data_path,"results/H_scenario.csv"))
# head(H_scenario)
# 
# saveRDS(H_scenario_grouped, paste0(data_path,"results/H_scenario_grouped.rds"))
# write.csv(H_scenario, paste0(data_path,"results/H_scenario_grouped.csv"))
# 
