### Code to make Table 2 ####

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


source("lincom_functions.R")
# source("read_biomes.R")

####  specifcy halo model with fixed effects year #### 
grid.rings <- felm(ATmax ~ f_1to2 + f_2to4 + f_4to10 +f_10to50 +
                    soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
                    rad+ rad_xav +  nino4 + tna  |id + year | 0 | id,
                    data=forest)


halo_vars <- c("f_1to2","f_2to4", "f_4to10", "f_10to50")
agg_vars <- c("f_1to2","f_2to4", "f_4to10", "f_10to50", "id", "lat", "lon")

model_eq <- ATmax ~ f_1to2 + f_2to4 + f_4to10 +f_10to50 +
  soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year | 0 | id

temp_tibble <- forest %>% 
  do(tidy(felm(model_eq, data = .), conf.int = .95))
temp_tibble

head(temp_tibble)

#### scenario:  warming at sample from 25% forest loss ####
forest.2015 <- forest[forest$year == 2015,agg_vars]
head(forest.2015)
forest.2015 <- aggregate(forest.2015,list(forest.2015$id), first)
forest.2015[halo_vars] <- forest.2015[halo_vars]*0.25
forest.2015 <- apply_lincom(forest.2015, temp_tibble ) 

# estimated contribution from each halo
forest.2015$b_1to2 <- - forest.2015$f_1to2*grid.rings$coefficients[c("f_1to2"),]
forest.2015$b_2to4 <-  - forest.2015$f_2to4*grid.rings$coefficients[c("f_2to4"),]
forest.2015$b_4to10 <-  - forest.2015$f_4to10*grid.rings$coefficients[c("f_4to10"),]
forest.2015$b_10to50 <-  - forest.2015$f_10to50*grid.rings$coefficients[c("f_10to50"),]

head(forest.2015)

#### scenario: historical warming at sample ####

# forests year 2000 
forest.fin <- forest[forest$year == 2015,agg_vars]
forest.fin <- aggregate(forest.fin,list(forest.fin$id), first)

forest.2000 <- forest[forest$year == 2000,agg_vars]
forest.2000 <- aggregate(forest.2000,list(forest.2000$id), first)

ids <- intersect(forest.fin$id , forest.2000$id)
forest.diff <- forest.2000
diff_vars <- c("f_1to2","f_2to4", "f_4to10", "f_10to50")
forest.diff[,diff_vars] <- forest.2000[,diff_vars] - forest.fin[match(forest.2000$id, forest.fin$id),diff_vars]
forest.diff <- forest.diff %>% drop_na() 
forest.diff <- apply_lincom(forest.diff, temp_tibble )
# forest.diff[halo_vars]*grid.rings$coefficients[halo_vars,]
forest.diff$b_1to2 <- - forest.diff$f_1to2*grid.rings$coefficients[c("f_1to2"),]
forest.diff$b_2to4 <- - forest.diff$f_2to4*grid.rings$coefficients[c("f_2to4"),]
forest.diff$b_4to10 <- - forest.diff$f_4to10*grid.rings$coefficients[c("f_4to10"),]
forest.diff$b_10to50 <- - forest.diff$f_10to50*grid.rings$coefficients[c("f_10to50"),]

head(forest.diff)

#####  scenario: warming from 25% forest loss ##### 
forest.m <- forest.2015[1,agg_vars]
forest.m[1, halo_vars] <- c(pi*(2**2 - 1**2),pi*(4**2 - 2**2), pi*(10**2 - 4**2),
                            pi*(50**2 - 10**2))*0.25
forest.m <- apply_lincom(forest.m, temp_tibble )
# forest.m[halo_vars] <- forest.m[halo_vars]*grid.rings$coefficients[halo_vars,]
forest.m$b_1to2 <-  -forest.m$f_1to2*grid.rings$coefficients[c("f_1to2"),]
forest.m$b_2to4 <-  -forest.m$f_2to4*grid.rings$coefficients[c("f_2to4"),]
forest.m$b_4to10 <-  -forest.m$f_4to10*grid.rings$coefficients[c("f_4to10"),]
forest.m$b_10to50 <-  -forest.m$f_10to50*grid.rings$coefficients[c("f_10to50"),]
head(forest.m)

##### scenario: historical (10.8%) forest loss in amazonia and cerrado, 1985-  2018  ##### 
forest.h <- forest.2015[1,agg_vars]

forest.h[1, halo_vars] <- 0.107605*c(pi*(2**2 - 1**2),pi*(4**2 - 2**2), pi*(10**2 - 4**2),pi*(50**2 - 10**2))
forest.h <- apply_lincom(forest.h,temp_tibble ) 
forest.h$b_1to2 <-  -forest.h$f_1to2*grid.rings$coefficients[c("f_1to2"),]
forest.h$b_2to4 <-  -forest.h$f_2to4*grid.rings$coefficients[c("f_2to4"),]
forest.h$b_4to10 <- -forest.h$f_4to10*grid.rings$coefficients[c("f_4to10"),]
forest.h$b_10to50 <-  -forest.h$f_10to50*grid.rings$coefficients[c("f_10to50"),]
head(forest.h)


stack_vars <- c("f_1to2",  "f_2to4",  "f_4to10", "f_10to50", 
                "b_1to2",  "b_2to4",  "b_4to10", "b_10to50", 
                "estimate", "conf.low", "conf.high")

scenarios.pred <- rbind(colMeans(forest.diff[,stack_vars]), colMeans(forest.2015[,stack_vars]), 
                        forest.m[,stack_vars], forest.h[,stack_vars] )
scenarios.pred$names <- c("Historial (sample)", "Loss of 25% of forest remaining in 2015 (sample)",
                          "Uniform loss of 25% forest cover",
                          "Historical (1985-2017) for Amazonia + Cerrado ")
scenarios.pred <- scenarios.pred[, c("names", "estimate", "conf.low", "conf.high")]
scenarios.pred$estimate = abs(scenarios.pred$estimate)


df <- scenarios.pred

df["Forest Loss Scenario"] <-  c("Historical (2000-2015)", 
                                  "Loss of 25% of forest remaining in 2015",
                                  "Uniform loss of 25% forest cover",
                                  "Historical (1985-2017)")


df$Dataset <- c("Spatially representative", "Spatially representative",  "Theoretical", 
"Amazonia + Cerrado")

df <- df[c("Forest Loss Scenario", "Dataset", "estimate", "conf.low", "conf.high")]
df[, 3:5]  <- round(df[, 3:5] ,4)

options(xtable.sanitize.text.function=identity)
   
dfL <- df[,1:4]
dfL[,4] <- df$estimate - df$conf.low
colnames(dfL)[3] <-  " $\\Delta AT$ ($^\\circ$C)"
colnames(dfL)[4] <-  "CI 95\\% ($^\\circ$C)"
dfL[,4]<-paste0("$\\pm$",dfL[,4])
print(xtable(dfL[,1:4], type = "latex"),file = paste0( "tables/table2"))

