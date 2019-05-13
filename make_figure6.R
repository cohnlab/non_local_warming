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


#### Figure 3A  #####
df <- expand.grid(unique(H_models$model), unique(H_models$submodel))
df$model <-  df$Var1 
df$submodel <-  df$Var2 
df <- df[,  c("model", "submodel")]
H_models[, c("model", "submodel")]

df <- df[!do.call(paste0, df) %in% do.call(paste0, H_models[,c("model", "submodel")]),]
df[,c("term", "estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high")  ] <- NaN
df <- rbind(df, H_models)
df$term <- ""
df[,c("estimate", "conf.low", "conf.high")] <- -df[,c("estimate", "conf.low", "conf.high")]

model_names <-
  na.omit(df) %>% group_by(model) %>% 
  dplyr::summarise(estimate = mean(estimate)) %>% 
  arrange(desc(estimate)) %>% .$model

ystr <- TeX("$\\Delta AT$ per km$^2$ forest loss ($\\degree$C/km$^2$)")


p <- small_multiple(df) +
  scale_x_discrete(limits = model_names) + # order the models 
  theme_bw() + ylab(ystr) +
  xlab("Halo range") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(size =10 ,angle = 45, hjust = 1),
        legend.title = element_text(size=10),
        axis.title.x = element_text(size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_rect(color="gray90"),
        legend.key.size = unit(10, "pt"))  +
  labs(color='Model specification') 
p

ggsave(filename=paste0( "figures/Figure6A.eps"), plot=p, width = 7, height = 4)

# ##### H_scenario: prediction sensitibity to halo selection for 25% forest loss #####
# H_scenario weights by area for a 25% forest loss case
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



#### Figure 3B  #####

H_scenario$model <- H_scenario$term

df <- expand.grid(unique(H_scenario$model), unique(H_scenario$submodel))
df$model <-  df$Var1 
df$submodel <-  df$Var2 
df <- df[,  c("model", "submodel")]

df <- df[!do.call(paste0, df) %in% do.call(paste0, H_scenario[,c("model", "submodel")]),]
df[,c("term", "estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high")  ] <- NaN
df <- rbind(df, H_scenario)
df$term <- ""
df[,c("estimate", "conf.low", "conf.high")] <- -df[,c("estimate", "conf.low", "conf.high")]

model_names <-
  na.omit(df) %>% group_by(model) %>% 
  dplyr::summarise(estimate = mean(estimate)) %>% 
  arrange(desc(estimate)) %>% .$model
model_names

# ystr <- TeX("$\\Delta AT$ per 25% forest loss ($\\degree$C/km$^2$)")
ystr <- TeX("$\\Delta AT$ for a 25 p.p. loss in forest cover ($\\degree$C)")

p <- small_multiple(df) +
  scale_x_discrete(limits = model_names) + # order the models 
  theme_bw() + ylab(ystr) +
  xlab("Halo range") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(size =10 ,angle = 45, hjust = 1),
        legend.title = element_text(size=10),
        axis.title.x = element_text(size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_rect(color="gray90"),
        legend.key.size = unit(10, "pt"))  +
  labs(color='Model specification') 
p

ggsave(filename=paste0( "figures/Figure6B.eps"), plot=p, width = 7, height = 4)


