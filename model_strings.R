######  HALO MODELS  #####

#  "fy", "ny", 'ly' and 'l2y' denote year fixed effects, no year,
# linear year and quadratic year, respectively
# "_nc" denotes unclustered errors

fy_1_2 <- ATmax ~ f_1to2  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_2 <- ATmax ~ f_1to2  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id
ly_1_2 <- ATmax ~ f_1to2  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna  + year |id  | 0 | id
l2y_1_2 <- ATmax ~ f_1to2  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna + year2  |id  | 0 | id


fy_1_4 <- ATmax ~ f_1to4  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_4 <- ATmax ~ f_1to4  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id
ly_1_4 <- ATmax ~ f_1to4  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna  + year |id  | 0 | id
l2y_1_4 <- ATmax ~ f_1to4  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna + year2  |id  | 0 | id

fy_1_2_4 <- ATmax ~ f_1to2  + f_2to4  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_2_4 <- ATmax ~ f_1to2  + f_2to4  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id

fy_1_10 <- ATmax ~ f_1to10  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_10 <- ATmax ~ f_1to10  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id
ly_1_10 <- ATmax ~ f_1to10  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna  + year |id  | 0 | id
l2y_1_10 <- ATmax ~ f_1to10  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna + year2  |id  | 0 | id

fy_1_10_50 <- ATmax ~ f_1to10 + f_10to50  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_10_50 <- ATmax ~ f_1to10  + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id
ly_1_10_50 <- ATmax ~ f_1to10  + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna  + year |id  | 0 | id
l2y_1_10_50 <- ATmax ~ f_1to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna + year2  |id  | 0 | id


fy_1_2_4_10 <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_2_4_10 <- ATmax ~  f_1to2 + f_2to4 + f_4to10  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id

fy_1_50 <- ATmax ~ f_1to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_50 <- ATmax ~  f_1to50  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id
ly_1_50 <- ATmax ~ f_1to50  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna  + year |id  | 0 | id
l2y_1_50 <- ATmax ~ f_1to50  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna + year2  |id  | 0 | id


fy_1_2_50 <- ATmax ~ f_1to2 + f_2to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_2_50 <- ATmax ~  f_1to2 + f_2to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id

fy_1_2_4_50 <- ATmax ~ f_1to2 + f_2to4 + f_4to50  + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_2_4_50 <- ATmax ~  f_1to2 + f_2to4 + f_4to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id

fy_1_2_4_10_50 <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id
ny_1_2_4_10_50 <- ATmax ~  f_1to2 + f_2to4 + f_4to10  + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id  | 0 | id

sens_eqs <- c(fy_1_2, ny_1_2, fy_1_4, ny_1_4,fy_1_2_4, ny_1_2_4,fy_1_10, 
           ny_1_10,fy_1_2_4_10, ny_1_2_4_10, fy_1_50, ny_1_50, 
           fy_1_2_50, ny_1_2_50, fy_1_2_4_50, ny_1_2_4_50,
           fy_1_2_4_10_50, ny_1_2_4_10_50)

### model strings for senstivity analysis
sens_names <- c("fy_1_2", "ny_1_2", "fy_1_4", "ny_1_4","fy_1_2_4", "ny_1_2_4","fy_1_10", 
           "ny_1_10","fy_1_2_4_10", "ny_1_2_4_10", "fy_1_50", "ny_1_50", 
           "fy_1_2_50", "ny_1_2_50", "fy_1_2_4_50", "ny_1_2_4_50",
           "fy_1_2_4_10_50", "ny_1_2_4_10_50")


##### DROP VARIABLE HALO MODELS  ####### 

fy_all <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id

fy_soil <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id

fy_rain <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id

fy_rad <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  nino4 + tna  |id + year | 0 | id

fy_sst <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad + rad_xav   |id + year | 0 | id


fy_sst_rain <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  rad + rad_xav   |id + year | 0 | id
fy_sst_rad <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5    |id + year | 0 | id
fy_sst_soil <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 +
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad + rad_xav   |id + year | 0 | id

 
fy_soil_rad <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
    nino4 + tna   |id + year | 0 | id
fy_soil_rain <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + 
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id

fy_rain_rad <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
    nino4 + tna   |id + year | 0 | id


fy_sst_rad_rain <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0   |id + year | 0 | id

fy_sst_rad_soil <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5    |id + year | 0 | id

fy_rad_soil_rain <- ATmax ~ f_1to2 + f_2to4 + f_4to10 + f_10to50 + 
  nino4 + tna  |id + year | 0 | id


drop_eqs <- c(fy_all, fy_soil, fy_rain, fy_rad, fy_sst, 
              fy_sst_rain, fy_sst_rad, fy_sst_soil,
              fy_soil_rad,fy_soil_rain, fy_rain_rad,
              fy_sst_rad_rain, fy_sst_rad_soil,fy_rad_soil_rain)
### model strings for senstivity analysis
drop_names <-c("fy_all", "fy_soil", "fy_rain", "fy_rad", "fy_sst", 
               "fy_sst_rain", "fy_sst_rad", "fy_sst_soil",
               "fy_soil_rad","fy_soil_rain", "fy_rain_rad",
               "fy_sst_rad_rain", "fy_sst_rad_soil","fy_rad_soil_rain")

######  HALO MODELS WITH DIFFERENT YEAR REPRESENTATIONS #####

halo_no_year <- ATmax ~  f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id | 0 | id

halo_iyear <- ATmax ~  f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna   |id + year | 0 | id

halo_year <- ATmax ~  f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad+ rad_xav +  nino4 + tna + year  |id  | 0 | id

halo_year2 <- ATmax ~  f_1to2 + f_2to4 + f_4to10 + f_10to50 + soilm0 + 
  p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5 +
  rad + rad_xav +  nino4 + tna + year2  |id  | 0 | id

######  LENGTHSCALE MODELS #####

L1_no <- ATmax ~ forest_2000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id | 0 | id

L1_iyear <- ATmax ~ forest_2000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 | id

L1_iyear_nc <- ATmax ~ forest_2000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 

L1_year <- ATmax ~ forest_2000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna + year |id | 0 | id

L1_year2 <- ATmax ~ forest_2000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna + year2 |id | 0 | id


L2_no <- ATmax ~ forest_5000  + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id | 0 | id

L2_iyear <- ATmax ~ forest_5000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 | id

L2_iyear_nc <- ATmax ~ forest_5000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 

L2_year <- ATmax ~ forest_5000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna + year |id | 0 | id

L2_year2 <- ATmax ~ forest_5000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna + year2 |id | 0 | id


L3_no <- ATmax ~ forest_10000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna |id | 0 | id

L3_iyear <- ATmax ~ forest_10000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 | id

L3_iyear_nc <- ATmax ~ forest_10000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 

L3_year <- ATmax ~ forest_10000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna + year |id | 0 | id

L3_year2 <- ATmax ~ forest_10000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna +  year2 |id | 0 | id


L4_no <- ATmax ~ forest_20000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna |id | 0 | id

L4_iyear <- ATmax ~ forest_20000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 | id

L4_iyear_nc <- ATmax ~ forest_20000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna  |id + year| 0 

L4_year <- ATmax ~ forest_20000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna + year |id | 0 | id

L4_year2 <- ATmax ~ forest_20000   + soilm0 + p_lag_1 + p_lag_2 + p_lag_3 + p_lag_4 + p_lag_5+
  rad+ rad_xav +  nino4 + tna +  year2 |id | 0 | id

