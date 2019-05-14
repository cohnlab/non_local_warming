### Code to make SI Figure 3 ####

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
stations <- dataset[dataset$set == "station", ]

source("model_strings.R")

# see model_strings.R
L1_eqs <- c( L1_no, L1_iyear, L1_year, L1_year2)
L2_eqs <- c( L2_no, L2_iyear, L2_year, L2_year2)
L3_eqs <- c( L3_no, L3_iyear, L3_year, L3_year2)
L4_eqs <- c( L4_no, L4_iyear, L4_year, L4_year2)
L_eqs_nc <- c( L1_iyear_nc, L2_iyear_nc, L3_iyear_nc, L4_iyear_nc)

L1_models <- tidy()
L1_models_verb <- tidy()

for (model_eq in L1_eqs){
  
  temp_tibble <- dataset %>% 
    group_by(set) %>%
    do(tidy(felm(model_eq, weights=.$forest_wght, data = .), conf.int = .95)) %>%
    filter(term == "forest_2000" ) %>%
    ungroup  # %>% rename(model = set)
  
  L1_models <- rbind(L1_models,temp_tibble)
  
}

L2_models <- tidy()

for (model_eq in L2_eqs){
  
  temp_tibble <- dataset %>% 
    group_by(set) %>%
    do(tidy(felm(model_eq, weights=.$forest_wght, data = .), conf.int = 0.95)) %>%
    filter(term == "forest_5000" ) %>%
    ungroup# %>% rename(model = set)
  
  L2_models <- rbind(L2_models,temp_tibble)
}


L3_models <- tidy()

for (model_eq in L3_eqs){
  
  temp_tibble <- dataset %>% 
    group_by(set) %>%
    do(tidy(felm(model_eq,  weights=.$forest_wght, data = .), conf.int = 0.95)) %>%
    filter(term == "forest_10000" ) %>%
    ungroup # %>% rename(model = set)
  
  L3_models <- rbind(L3_models,temp_tibble)
}

L4_models <- tidy()

for (model_eq in L4_eqs){
  
  temp_tibble <- dataset %>% 
    group_by(set) %>%
    do(tidy(felm(model_eq,  weights=.$forest_wght, data = .), conf.int = 0.95)) %>%
    filter(term == "forest_20000" ) %>%
    ungroup # %>% rename(model = set)
  L4_models <- rbind(L4_models,temp_tibble)
}

L_models_nc <- tidy()

for (model_eq in L_eqs_nc){
  
  temp_tibble <- dataset %>% 
    group_by(set) %>%
    do(tidy(felm(model_eq, data = .), conf.int = 0.95)) %>%
    filter(term %in% c("forest_2000", "forest_5000", "forest_10000", "forest_20000"  )) %>%
    ungroup # %>% rename(model = set)
  L_models_nc <- rbind(L_models_nc,temp_tibble)
}

L1_models$model <- L1_models$set
L2_models$model <- L2_models$set
L3_models$model <- L3_models$set
L4_models$model <- L4_models$set
L_models_nc$model <- paste0(L_models_nc$set, "_nc")

L_models <-  rbind(L1_models,L2_models,L3_models,L4_models)

L_models <- L_models %>% relabel_predictors(c(forest_2000 = "2 km",                       
                                              forest_5000 = "5 km",                                                                         
                                              forest_10000 = "10 km",
                                              forest_20000 = "20 km") )

submodel_names <- c("None", "Fixed effects", "Linear year", "Quadratic year")
L_models$submodel <- L_models$term
L_models$term<- rep(rep(submodel_names, each = 1), times = 20) 


L_models_add_nc <- L_models_nc %>% relabel_predictors(c(forest_2000 = "2 km",                       
                                                        forest_5000 = "5 km",                                                                         
                                                        forest_10000 = "10 km",
                                                        forest_20000 = "20 km") )

L_models_add_nc$submodel <- L_models_add_nc$term
L_models_add_nc$term<-"Fixed effects"


L_models <- rbind(L_models, L_models_add_nc)

forest_str <-"forest frontier \n (clear sky days)"
clear_str <- "station \n (clear sky days)"

station_nc_str <-"station \n (unweighted, unclustered)"
forest_nc_str <-"forest frontier \n (clear sky days,\n unweighted, unclustered)"
clear_nc_str <- "station \n (clear sky days,\n unweighted, unclustered)"

clear_aq_str <- "station \n (Aqua clear sky)"
clear_ter_str <- "station \n (Terra clear sky)"


L_models[L_models$model == "forest",]$model <- forest_str
L_models[L_models$model == "clear",]$model <- clear_str

L_models[L_models$model == "forest_nc",]$model <- forest_nc_str
L_models[L_models$model == "clear_nc",]$model <- clear_nc_str
L_models[L_models$model == "station_nc",]$model <- station_nc_str

L_models[L_models$model == "aqua clear",]$model <- clear_aq_str
L_models[L_models$model == "terra clear",]$model <- clear_ter_str



forest_str <-"forest frontier \n (clear sky days)"
clear_str <- "station \n (clear sky days)"

station_nc_str <-"station \n (unweighted, unclustered)"
forest_nc_str <-"forest frontier \n (clear sky days,\n unweighted, unclustered)"
clear_nc_str <- "station \n (clear sky days,\n unweighted, unclustered)"

clear_aq_str <- "station \n (Aqua clear sky)"
clear_ter_str <- "station \n (Terra clear sky)"


#### SI Lenghtscale models  #####
df <- L_models


df[,c("estimate", "conf.low", "conf.high")] <- -df[,c("estimate", "conf.low", "conf.high")]

ystr <- TeX("$\\Delta AT$ per unit change $NL$ ($\\degree$C)")
model_names <- c( "station", clear_str,clear_aq_str, clear_ter_str,  forest_str)

p_L_SI <- small_multiple(df) +
  scale_x_discrete(limits = model_names) + # order the models 
  theme_bw() + ylab(ystr) +
  xlab("Dataset") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(size =10 ,angle = 45, hjust = 1),
        legend.title = element_text(size=10),
        axis.title.x = element_text(size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_rect(color="gray90"),
        legend.key.size = unit(10, "pt"))  +
  labs(color='Model version ') 

p_L_SI 

ggsave(file=paste0( "figures/Figure_S3.eps"), plot=p_L_SI, width = 6, height = 5.5)
