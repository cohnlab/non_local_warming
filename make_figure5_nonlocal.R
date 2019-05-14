### Code to make figure 5 ####

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
L1_eqs <- c(  L1_iyear )
L2_eqs <- c(  L2_iyear )
L3_eqs <- c( L3_iyear )
L4_eqs <- c( L4_iyear )
L_eqs_nc <- c( L1_iyear_nc, L2_iyear_nc, L3_iyear_nc, L4_iyear_nc)

L1_models <- tidy()


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


L_models$submodel <- L_models$term
L_models$term<-  "Fixed effects"


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

# Lengthscale sensitivity to year specification  
df <- L_models
df[,c("estimate", "conf.low", "conf.high")] <- -df[,c("estimate", "conf.low", "conf.high")]
df <- df[df$model %in% c(forest_str, "station", clear_str,  station_nc_str, clear_nc_str, forest_nc_str),]

df <- df %>% filter(term %in% c( "Fixed effects"))
df$estimate <- df$estimate/4
df$conf.low <- df$conf.low/4
df$conf.high <- df$conf.high/4

df$term <- ""
df

model_names <- c( station_nc_str, "station", clear_nc_str, clear_str, forest_str)

ystr <- TeX("$\\Delta AT$ for a 25 p.p. loss in forest cover ($\\degree$C)")

p <- small_multiple(df) +
  scale_x_discrete(limits = model_names) + # order the models
  theme_bw() + ylab(ystr) + ylim(-1.1, 2.5)+
  xlab("Dataset") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(size =10 ,angle = 45, hjust = 1),
        legend.title = element_text(size=10),
        axis.title.x = element_text(size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_rect(color="gray90"),
        legend.key.size = unit(10, "pt"))  +
  labs(color='Model lengthscale') +  scale_color_brewer(palette="PuRd") 

p

ggsave(filename=paste0("figures/Figure5_nonlocal.eps"), plot=p, width = 7, height = 4.5)

