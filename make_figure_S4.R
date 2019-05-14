### Code to make supporting info figure S4 ####

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

halo_year_eqs <- c( halo_no_year, halo_iyear, halo_year, halo_year2)

models <- tidy()


for (model_eq in halo_year_eqs){
  temp_tibble <- forest %>% 
    do(tidy(felm(model_eq, data = .), conf.int = .99))  %>%
    filter(term %in% c("f_1to2", "f_2to4", "f_4to10" , "f_10to50" ))
  models <- rbind(models,temp_tibble)
}

models <- models %>% relabel_predictors(c(f_1to2 = "1-2 km",                       
                                          f_2to4 = "2-4 km",                       
                                          f_4to10 = "4-10 km",
                                          f_10to50 = "10-50 km"))
H_yr_models <- models
H_yr_models$model <- models$term
H_yr_models$term <-"halos"
submodel_names <- c("None","Fixed effects", "Linear year", "Quadratic year")
H_yr_models$submodel <- rep(rep(submodel_names, each = 1), times = 4)


halo_names <- c("1-2 km", "2-4 km", "4-10 km","10-50 km")

df <- H_yr_models
df[,c("estimate", "conf.low", "conf.high")] <- - df[,c("estimate", "conf.low", "conf.high")]

ystr <- TeX("$\\Delta$AT per km$^2$ forest loss ($\\degree$C/km$^2$)")

p <- small_multiple(df)   +
  scale_x_discrete(limits = halo_names) +# order the models 
  theme_bw() + ylab(ystr) +
  xlab("Halo Range") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(size =10 ,angle = 45, hjust = 1),
        legend.title = element_text(size=10),
        axis.title.x = element_text(size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_rect(color="gray90"),
        legend.key.size = unit(10, "pt"))  +
  labs(color='Model version ') 

p
ggsave(filename= "figures/figure_S4.eps", plot=p, width = 6, height = 3.3)


