setwd("E:\\06博士论文\\002出国交流\\02cliamte_economy\\output_ld_new")
library(terra)
library(dplyr)
library(readr)
library(tidyr)
global_map<-vect("01data\\polyg_adm1_gdp_perCapita_1990_2022.gpkg")
global_map <- global_map[, 1:3]

grid<-rast(nrows = 360, ncols = 720,
           xmin = -180, xmax = 180, ymin = -90, ymax = 90,
           crs="+proj=longlat +datum=WGS84")
weights = cellSize(grid)


######GDPpc data#######
gdp<-read_csv("01data\\tabulated_adm1_gdp_perCapita.csv")
gdp<-gdp[,-5]
gdp<- tidyr:: gather(gdp,year,value,-c("GID_nmbr","iso3","Country","Subnat"))
colnames(gdp)[6]<-"gdppc"
gdp$year<-as.numeric(gdp$year)
gdp<- gdp %>%
  complete(year = 1970:2015, nesting(GID_nmbr, iso3, Country, Subnat))

#######pop and urbanization data######
rural<-rast("01data\\02pop\\rural_population.nc")
urban<-rast("01data\\02pop\\urban_population.nc")
pop_join<-data.frame()
total_pop <- rast()
for (i in c(73:126)) {
  rural_y<-rural[[i]]
  urban_y<-urban[[i]]
  year_layer <- rural_y + urban_y
  year_layer <- aggregate(year_layer , fact=4, fun=sum, na.rm=TRUE)
  total_pop <- c(total_pop, year_layer)
  rural_y <- aggregate(rural_y, fact=4, fun=sum, na.rm=TRUE)
  urban_y <- aggregate(urban_y, fact=4, fun=sum, na.rm=TRUE)
  region_rural <-terra::extract(rural_y,fun=sum,global_map,bind=T,na.rm=T)
  region_urban <-terra::extract(urban_y,fun=sum,global_map,bind=T,na.rm=T)
  region_rural_df<-terra::as.data.frame(region_rural)
  region_urban_df<-terra::as.data.frame(region_urban)
  colnames(region_rural_df)[4]<-"rural"
  colnames(region_urban_df)[4]<-"urban"
  region_pop_df<-left_join(region_rural_df,region_urban_df,by=c("GID_nmbr","iso3","Country"))
  region_pop_df$pop<-region_pop_df$rural+region_pop_df$urban
  region_pop_df$urb<-region_pop_df$urban/region_pop_df$pop
  year<-1897+i
  region_pop_df$year<-year
  pop_join<-bind_rows(pop_join,region_pop_df)
  print(year)
}
pop_join<-pop_join[,-c(4,5)]
gdp<-left_join(gdp, pop_join,by=c("GID_nmbr","iso3","Country","year"))
gdp$pop[gdp$pop==0]<-NA
gdp$pop<-round(gdp$pop)
gdp$urb[is.nan(gdp$urb)]<-NA

spop<-read_csv("01data//02pop//spopulation_world bank.csv")
spop<-spop[,-c(1,5)]
colnames(spop)[4]<-"urb"
gdp_na<-filter(gdp,is.na(pop))
gdp_na<-gdp_na[,-c(7,8)]
gdp_na<-left_join(gdp_na, spop, by=c("iso3"="Country Code","year"))
gdp<-filter(gdp,!is.na(pop))
gdp<-bind_rows(gdp,gdp_na)

gdp <- gdp %>%
  group_by(Country) %>%
  add_count(name = "weight") %>%
  ungroup()
gdp$weight<-1/(gdp$weight/53)

area<-expanse(global_map, unit ="km")
global_map$area<-round(area)
global_map_df<-as.data.frame(global_map)
gdp<-left_join(gdp, global_map_df,by=c("GID_nmbr","iso3","Country"))
global_map <- global_map[, 1:3]

names(total_pop) <- paste0("population_", 1970:2022)

#####temperature data######
cru_tem_list<- list.files(path = "01data\\01meteor\\01temp",pattern="*.nc$",full.names=T )
tem_list<-rast()
for (i in c(1:6)) {
tem_year <- rast(cru_tem_list[i])
tmp_layers <- grep("^tmp", names(tem_year), value = TRUE)
tem_year <- tem_year[[tmp_layers]]
tem_list<-c(tem_list,tem_year)
}

template <- total_pop[[1]]
tem_list<- resample(tem_list, template, method = "bilinear")

tem_join<-data.frame()
for (i in c(0:51)) {
  j<-1+i*12
  cru_y<-tem_list[[j]]
  for (k in c(1:11)){
    cru_m<-tem_list[[j+k]]
    cru_y<-cru_y+cru_m
  }
  cru_y<-cru_y/12
  
  weights<-total_pop[[(i+2)]] #####
  
  tem<-cru_y*weights
  region_tem <-terra::extract(tem,fun=sum,global_map,bind=T,na.rm=T)
  region_tem_df<-terra::as.data.frame(region_tem)
  colnames(region_tem_df)[4]<-"tem"
  region_weights <-terra::extract(weights,fun=sum,global_map,bind=T,na.rm=T)
  region_weights<-terra::as.data.frame(region_weights)
  colnames(region_weights)[4]<-"weights"
  region_tem_df<-left_join(region_tem_df,region_weights,by=c("GID_nmbr","iso3","Country"))
  region_tem_df$tem<-region_tem_df$tem/region_tem_df$weights
  region_tem_df<-region_tem_df[,c(1:4)]
  year<-1971+i
  region_tem_df$year<-year
  tem_join<-bind_rows(tem_join,region_tem_df)
  print(year)
}
tem_join$year<-as.numeric(tem_join$year)

tem_join <- tem_join %>%
  group_by(GID_nmbr) %>%
  mutate(zero_count = sum(tem == 0)) %>%
  ungroup()
tem_join$tem[tem_join$zero_count==52]<-NA
tem_join<-tem_join[,-6]

gdp<-left_join(gdp, tem_join,by=c("GID_nmbr","iso3","Country","year"))


#####precipitation data######
cru_pre_list<- list.files(path = "01data\\01meteor\\02pre",pattern="*.nc$",full.names=T )
pre_list<-rast()
for (i in c(1:6)) {
  pre_year <- rast(cru_pre_list[i])
  pre_layers <- grep("^pre", names(pre_year), value = TRUE)
  pre_year <- pre_year[[pre_layers]]
  pre_list<-c(pre_list,pre_year)
}

pre_list<- resample(pre_list, template, method = "bilinear")

pre_join<-data.frame()
for (i in c(0:51)) {
  j<-1+i*12
  cru_y<-pre_list[[j]]
  for (k in c(1:11)){
    cru_m<-pre_list[[j+k]]
    cru_y<-cru_y+cru_m
  }
  cru_y<-cru_y/1000
  
  weights<-total_pop[[(i+2)]] #####
  
  pre<-cru_y*weights
  region_pre <-terra::extract(pre,fun=sum,global_map,bind=T,na.rm=T)
  region_pre_df<-terra::as.data.frame(region_pre)
  colnames(region_pre_df)[4]<-"pre"
  region_weights <-terra::extract(weights,fun=sum,global_map,bind=T,na.rm=T)
  region_weights<-terra::as.data.frame(region_weights)
  colnames(region_weights)[4]<-"weights"
  region_pre_df<-left_join(region_pre_df,region_weights,by=c("GID_nmbr","iso3","Country"))
  region_pre_df$pre<-region_pre_df$pre/region_pre_df$weights
  region_pre_df<-region_pre_df[,c(1:4)]
  year<-1971+i
  region_pre_df$year<-year
  pre_join<-bind_rows(pre_join,region_pre_df)
  print(year)
}
pre_join$year<-as.numeric(pre_join$year)

pre_join <- pre_join %>%
  group_by(GID_nmbr) %>%
  mutate(zero_count = sum(pre == 0)) %>%
  ungroup()
pre_join$pre[pre_join$zero_count==52]<-NA
pre_join<-pre_join[,-6]

gdp<-left_join(gdp, pre_join,by=c("GID_nmbr","iso3","Country","year"))


#######CO2######
carbon_list<- list.files(path = "01data\\03co2",pattern="*.nc$",full.names=T )

carbon_join<-data.frame()
for (i in c(1:35)) {
  carbon_y<-rast(carbon_list[i])
  region_carbon <-terra::extract(carbon_y,fun=sum,global_map,bind=T,na.rm=T)
  region_carbon_df<-terra::as.data.frame(region_carbon)
  year<-1989+i
  region_carbon_df$year<-year
  carbon_join<-bind_rows(carbon_join,region_carbon_df)
  print(year)
}

gdp<-left_join(gdp, carbon_join,by=c("GID_nmbr","iso3","Country","year"))

write_excel_csv(gdp,file = "GDPpc_s.csv")

#############temperature  ERA5##########
library(rgdal)
tem_era <-readGDAL("F:\\01数据库\\中国气象数据\\ERA5\\ERA_month\\ERA_tem_1980_2025.grib")

grid<-tem_era[1]
grid<-rast(grid)
weights = cellSize(grid)
region_weights <-terra::extract(weights,fun=sum,global_map,bind=T,na.rm=T)
region_weights<-terra::as.data.frame(region_weights)
colnames(region_weights)[4]<-"weights"

tem_join<-data.frame()
for (i in c(0:42)) {
  s<-i*12+1
  e<-i*12+12
  period <- c(s:e)
  tem <-tem_era[period]
  tem<-rast(tem)
  tem <- app(tem, fun = mean, na.rm = TRUE)-273.15
  tem<-tem*weights
  region_tem <-terra::extract(tem,fun=sum,global_map,bind=T,na.rm=T)
  region_tem_df<-terra::as.data.frame(region_tem)
  colnames(region_tem_df)[4]<-c("tem_era")
  region_tem_df<-left_join(region_tem_df,region_weights,by=c("GID_nmbr","iso3","Country"))
  region_tem_df$tem_era<-region_tem_df$tem_era/region_tem_df$weights
  region_tem_df<-region_tem_df[,c(1:4)]
  year<-1980+i
  region_tem_df$year<-year
  tem_join<-bind_rows(tem_join,region_tem_df)
  print(year)
}
gdppc<-read_csv("GDPpc.csv")
gdppc<-left_join(gdppc,tem_join, by=c("GID_nmbr","iso3","Country","year"))



pre_era <-readGDAL("F:\\01数据库\\中国气象数据\\ERA5\\ERA_month\\ERA_pre_1980_2025.grib")

grid<-pre_era[1]
grid<-rast(grid)
weights = cellSize(grid)
region_weights <-terra::extract(weights,fun=sum,global_map,bind=T,na.rm=T)
region_weights<-terra::as.data.frame(region_weights)
colnames(region_weights)[4]<-"weights"

pre_join<-data.frame()
for (i in c(0:42)) {
  s<-i*12+1
  e<-i*12+12
  period <- c(s:e)
  pre <-pre_era[period]
  pre<-rast(pre)
  pre <- app(pre, fun = sum, na.rm = TRUE)
  pre<-pre*weights
  region_pre <-terra::extract(pre,fun=sum,global_map,bind=T,na.rm=T)
  region_pre_df<-terra::as.data.frame(region_pre)
  colnames(region_pre_df)[4]<-c("pre_era")
  region_pre_df<-left_join(region_pre_df,region_weights,by=c("GID_nmbr","iso3","Country"))
  region_pre_df$pre_era<-region_pre_df$pre_era/region_pre_df$weights
  region_pre_df<-region_pre_df[,c(1:4)]
  year<-1980+i
  region_pre_df$year<-year
  pre_join<-bind_rows(pre_join,region_pre_df)
  print(year)
}

gdppc<-left_join(gdppc,pre_join, by=c("GID_nmbr","iso3","Country","year"))

write_excel_csv(pre_join,file = "01data\\gdppc1.csv")

#########country temperature#######
library(sp)
library(sf)
global_map<-st_read("01data\\05countryboundary\\WB_countries_Admin0_10m.shp", 
                    fid_column_name = 'FID')
global_map <- vect(global_map[, 17])

grid<-rast(nrows = 360, ncols = 720,
           xmin = -180, xmax = 180, ymin = -90, ymax = 90,
           crs="+proj=longlat +datum=WGS84")
weights = cellSize(grid)

gdppc<-read_csv("gdppc.csv")
gdppc$gdp<-gdppc$gdppc*gdppc$pop
gdppc<-gdppc%>%
  group_by(iso3,subcontinent,year) %>%
  summarise(gdp=sum(gdp,na.rm=T),pop=sum(pop,na.rm=T))
gdppc$gdppc<-gdppc$gdp/gdppc$pop
gdppc<-gdppc[,-c(4)]
gdppc$gdppc[gdppc$gdppc==0]<-NA

gdp<-read_csv("01data\\gdppc_world_bank.csv")
colnames(gdp)[4]<-"gdppc_wb"

gdppc<-left_join(gdppc, gdp, by=c("iso3"="code","year"))

cru_tem_list<- list.files(path = "01data\\01meteor\\01temp",pattern="*.nc$",full.names=T )
tem_list<-rast()
for (i in c(1:6)) {
  tem_year <- rast(cru_tem_list[i])
  tmp_layers <- grep("^tmp", names(tem_year), value = TRUE)
  tem_year <- tem_year[[tmp_layers]]
  tem_list<-c(tem_list,tem_year)
}

tem_join<-data.frame()
for (i in c(0:51)) {
  j<-1+i*12
  cru_y<-tem_list[[j]]
  for (k in c(1:11)){
    cru_m<-tem_list[[j+k]]
    cru_y<-cru_y+cru_m
  }
  cru_y<-cru_y/12
  tem<-cru_y*weights
  region_tem <-terra::extract(tem,fun=sum,global_map,bind=T,na.rm=T)
  region_tem_df<-terra::as.data.frame(region_tem)
  colnames(region_tem_df)[2]<-"tem"
  region_weights <-terra::extract(weights,fun=sum,global_map,bind=T,na.rm=T)
  region_weights<-terra::as.data.frame(region_weights)
  colnames(region_weights)[2]<-"weights"
  region_tem_df<-left_join(region_tem_df,region_weights,by=c("ISO_A3"))
  region_tem_df$tem<-region_tem_df$tem/region_tem_df$weights
  region_tem_df<-region_tem_df[,c(1:2)]
  year<-1971+i
  region_tem_df$year<-year
  tem_join<-bind_rows(tem_join,region_tem_df)
  print(year)
}
tem_join$year<-as.numeric(tem_join$year)

tem_join <- tem_join %>%
  group_by(ISO_A3) %>%
  mutate(zero_count = sum(tem == 0)) %>%
  ungroup()
tem_join$tem[tem_join$zero_count==52]<-NA
tem_join<-tem_join[,c(1:3)]

gdppc<-left_join(gdppc, tem_join,by=c("iso3"="ISO_A3","year"))

cru_pre_list<- list.files(path = "01data\\01meteor\\02pre",pattern="*.nc$",full.names=T )
pre_list<-rast()
for (i in c(1:6)) {
  pre_year <- rast(cru_pre_list[i])
  pre_layers <- grep("^pre", names(pre_year), value = TRUE)
  pre_year <- pre_year[[pre_layers]]
  pre_list<-c(pre_list,pre_year)
}

pre_join<-data.frame()
for (i in c(0:51)) {
  j<-1+i*12
  cru_y<-pre_list[[j]]
  for (k in c(1:11)){
    cru_m<-pre_list[[j+k]]
    cru_y<-cru_y+cru_m
  }
  cru_y<-cru_y/1000
  pre<-cru_y*weights
  region_pre <-terra::extract(pre,fun=sum,global_map,bind=T,na.rm=T)
  region_pre_df<-terra::as.data.frame(region_pre)
  colnames(region_pre_df)[2]<-"pre"
  region_weights <-terra::extract(weights,fun=sum,global_map,bind=T,na.rm=T)
  region_weights<-terra::as.data.frame(region_weights)
  colnames(region_weights)[2]<-"weights"
  region_pre_df<-left_join(region_pre_df,region_weights,by=c("ISO_A3"))
  region_pre_df$pre<-region_pre_df$pre/region_pre_df$weights
  region_pre_df<-region_pre_df[,c(1:2)]
  year<-1971+i
  region_pre_df$year<-year
  pre_join<-bind_rows(pre_join,region_pre_df)
  print(year)
}
pre_join$year<-as.numeric(pre_join$year)

pre_join <- pre_join %>%
  group_by(ISO_A3) %>%
  mutate(zero_count = sum(pre == 0)) %>%
  ungroup()
pre_join$pre[pre_join$zero_count==52]<-NA
pre_join<-pre_join[,1:3]

gdppc<-left_join(gdppc, pre_join,by=c("iso3"="ISO_A3","year"))

write_excel_csv(gdppc,file = "01data\\gdppc_world_bank_merge1.csv")
