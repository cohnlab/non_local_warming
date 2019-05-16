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


source("model_strings.R")

stations <- dataset[dataset$set == "station", ]
head(stations)


#### STATIONS ######

#stations$p_lag_1_5 <- stations$p_lag_1 +stations$p_lag_2 + stations$p_lag_3 + stations$p_lag_4 + stations$p_lag_5


flds <- c("ATmax", "f_1to50", "rad", "rad_xav", 
          "p_lag_1", "p_lag_2","p_lag_3","p_lag_4","p_lag_5", "soilm0")

station_table <- tidy()

for (i in 1:10){
  fld <- flds[i] 
  t <- stations[complete.cases(stations[ ,fld]),]
  stats <- cbind( mean(t[,fld]), sd(t[,fld]),  min(t[,fld]), max(t[,fld]) )
  stats <- signif(stats,4)
  station_table <- rbind(station_table,stats)
}

rownames(station_table) <- flds
colnames(station_table) <- c("Mean", "SD", "Min", "Max")
rownames(station_table) <- c("AT ($^{\\circ}$C)", "F(1-50 km) (km$^2$)", 
                             "TOA radiation (W/m$^2$)",
                          "Net surface radiation (W/m$^2$)",
                          "P(lag=1 day) (mm)", "P(lag=2 day) (mm)" , "P(lag=3 day) (mm)" ,
                          "P(lag=4 day) (mm)", "P(lag=5 day) (mm)" , 
                          "Soil moisture (kg/m$^2$)"  )


fld <- "ATmax"
t <- stations[complete.cases(stations[ ,fld]),]
t<- t[,fld] - mean(t[,fld])
stats <- cbind( 0, sd(t),  min(t), max(t) )
stats <- signif(stats, 5)
colnames(stats) <- colnames(station_table)

station_table <- rbind(station_table,stats)

rownames(station_table)[nrow(station_table)] <- "AT anomaly ($^{\\circ}$C)"


### STATIONS : FOREST COVER CHANGE   ####

station.2000 <- stations %>% filter(stations$year == 2000)
station.2015 <- stations %>% filter(stations$year == 2015)
ids <- intersect(station.2015$id , station.2000$id)
station.diff <- station.2000
diff_vars <- c("f_1to50", "f_1to2","f_2to4", "f_4to10",  "f_10to50")
station.diff[,diff_vars] <- station.2000[,diff_vars] - station.2015[match(station.2000$id,station.2015$id),diff_vars]

station.diff <- station.diff %>% drop_na() 
colMeans(station.diff[, diff_vars])

t <- station.diff
fld <- "f_1to50"
stats <- cbind( mean(t[,fld]), sd(t[,fld]),  min(t[,fld]), max(t[,fld]) )

stats <- signif(stats, 4)
colnames(stats) <- colnames(station_table)


#### STATIONS: COMBINE ######

station_table2 <- rbind(station_table,stats)
rownames(station_table2)[nrow(station_table2)] <- "F(1-50 km) change (km$^2$)"

rownames(station_table2)

row_order <- c( "AT ($^{\\circ}$C)", "AT anomaly ($^{\\circ}$C)",
                "F(1-50 km) (km$^2$)" ,  "F(1-50 km) change (km$^2$)" ,
                "P(lag=1 day) (mm)", "P(lag=2 day) (mm)" , "P(lag=3 day) (mm)" , "P(lag=4 day) (mm)", 
                "P(lag=5 day) (mm)", 
                "Soil moisture (kg/m$^2$)" ,  
                "TOA radiation (W/m$^2$)",
                "Net surface radiation (W/m$^2$)")

station_table2 <- station_table2[row_order,]


options(xtable.sanitize.text.function=identity)
station_smry  <- xtable(station_table2, type = "latex")
station_smry
print(xtable(station_smry, type = "latex"), math.style.exponents = TRUE,file = paste0( "tables/table1B"))

#### SST INDICES ######

sst_table <- tidy()
flds <- c("nino4", "tna")

subset  <- stations[stations$id == 241,]
for (i in 1:2){
  fld <- flds[i] 
  t <- subset[complete.cases(subset[ ,fld]),]
  stats <- cbind( mean(t[,fld]), sd(t[,fld]),  min(t[,fld]), max(t[,fld]) )
  stats <- signif(stats,4)
  sst_table <- rbind(sst_table,stats)
}

rownames(sst_table) <- flds
colnames(sst_table) <- c("Mean", "SD", "Min", "Max")

rownames(sst_table) <- c("Ni\\~no 4 SST", "TNA"  )


sst_smry  <- xtable(sst_table, type = "latex")


print(xtable(station_smry, type = "latex"), math.style.exponents = TRUE,file = paste0( "tables/table1A"))