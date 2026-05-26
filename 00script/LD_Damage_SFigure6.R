setwd("~")
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(data.table)
tem<-read_csv("..\\01data\\04scenarios\\gdppc_k_tems_ssp126_2015-2100_new.csv")
tem<-tem[,-c(2,3)]
tem<-tidyr::gather(tem,model,tem, -c(region,year))
tem <- tem %>%
  group_by(region) %>%
  mutate(
    tem = ifelse(year == 2100 & tem == 0, 
                 tem[year == 2099], 
                 tem))
tem<-tem%>%
  group_by(region,year) %>%
  summarise(tem=mean(tem,na.rm=T))


#######get GDP per capita#########
gdppc_o<-read_csv("..\\01data\\tabulated_adm1_gdp_perCapita.csv")
gdppc_o<-gdppc_o[,-c(2:5)]
gdppc_o<- tidyr:: gather(gdppc_o,year,value,-c("GID_nmbr"))
gdppc_o$year<-as.numeric(gdppc_o$year)
colnames(gdppc_o)[c(1,3)]<-c("region","gdppc")
gdppc<-filter(gdppc_o, year==2019)
colnames(gdppc)[c(1,3)]<-c("region","gdppc0")
gdppc<-gdppc[,-2]

pop<-read_csv("..\\01data\\04scenarios\\scenario_ssp1_pop_2015-2100_new.csv")
gdp<-read_csv("..\\01data\\04scenarios\\scenario_ssp1_gdp_2005-2100_new.csv")
gdp$growth[is.na(gdp$growth)]<-0
pop$growth[is.na(pop$growth)]<-0
pop<-pop[pop$year %in% c(2020:2100),]
gdp<-gdp[gdp$year %in% c(2020:2100),]
colnames(gdp)[4]<-"gdp_growth"
colnames(pop)[4]<-"pop_growth"
gdp<-left_join(gdp,pop,by=c("region","year"))
gdp<-arrange(gdp, region, year)
gdp$growthpc<-gdp$gdp_growth-gdp$pop_growth
gdp$growthpc1<-1+gdp$growthpc
gdp <- gdp %>%
  group_by(region) %>%
  mutate(growthpcsum = cumprod(growthpc1))
gdp<-gdp[,c(1,2,7,9)]
gdppc<-left_join(gdp,gdppc,by=c("region"))
gdppc$gdppc<-gdppc$gdppc0*gdppc$growthpcsum
gdppc<-gdppc[,-c(4,5)]

#########GSE model#######
df_GSE <- tem
alpha = -0.0004
beta = -0.0095  

df_GSE <-df_GSE %>%
  group_by(region) %>%
  mutate(dtem=tem-lag(tem))
df_GSE<-df_GSE %>%
  mutate(gchange=alpha*dtem^2 + beta*dtem) 
df_GSE<-filter(df_GSE,year>=2020)
df_GSE<-df_GSE %>%
  group_by(region) %>%
  mutate(damage= cumsum(gchange))
result_GSE<-df_GSE[,c(1,2,6)]
result_GSE$model<-"GSE"

#########ML model#######
df_ML <- tem
alpha1 = 0.00683 
alpha2 = 0.00302 
beta1 = -0.00110  
beta2 = -0.000726  

df_ML <-df_ML %>%
  group_by(region) %>%
  mutate(ltem=lag(tem)) %>%
  mutate(dtem=tem-ltem) %>%
  mutate(ldtem = lag(dtem))
df_ML<-df_ML %>%
  mutate(gchange=alpha1*dtem + alpha2*ldtem + ltem*(beta1*dtem+beta2*ldtem)) 
df_ML<-filter(df_ML,year>=2020)
df_ML<-df_ML %>%
  group_by(region) %>%
  mutate(damage= cumsum(gchange))
result_ML<-df_ML[,c(1,2,8)]
result_ML$model<-"ML"

#########JRJ_LD model#######
gdppc_sub<-filter(gdppc_o, year %in% c(2016:2019))
gdppc_ld<-bind_rows(gdppc,gdppc_sub)
gdppc_ld <- gdppc_ld %>%
  mutate(
    period = ((year - 2016) %/% 4) + 1
  )
gdppc_ld<-gdppc_ld %>%
  filter(.,period<=21) %>%
  group_by(region,period) %>%
  summarise(gdppc=mean(gdppc,na.rm=T)) %>%
  mutate(growthpc = (gdppc-lag(gdppc))/lag(gdppc))

gdppc_ld <- gdppc_ld %>% 
  arrange(region, period) %>%
  mutate(
    gdppc_new = ifelse(period == 1, gdppc, NA),
    growth_new = NA
  )

tem_ld <- tem %>%
  filter(year>2015) %>%
  mutate(
    period = ((year - 2016) %/% 4) + 1
  )
tem_ld<-tem_ld %>%
  group_by(region,period) %>%
  summarise(tem=mean(tem,na.rm=T)) %>%
  filter(.,period<=21)

tem_ld <-tem_ld %>%
  group_by(region) %>%
  mutate(T0 = tem[period==1]) %>%
  ungroup()

df_ld <- as.data.table(gdppc_ld)[as.data.table(tem_ld), on = c("region", "period")]
thresholds <- 25000
lalphas <- c(-0.00562, -0.00154)
lbetas <- c(0.188, 0.0882)
alphas <- c(-0.00644, 0, 0)
betas <- c(0.233, 0, 0)
gammas <- c(0.00615, 0)
deltas <- c(-0.216, 0)

result_ld <- df_ld[, {
  gdppc_new <- gdppc
  damage <- numeric(.N)
  
  for(j in 1:.N) {
    if(j == 1) {
      base_gdp <- gdppc[j]
    } else {
      base_gdp <- gdppc_new[j-1]
    }
    
    # 选择参数组
    group <- findInterval(base_gdp, thresholds) + 1
    lalpha <- lalphas[group]
    lbeta <- lbetas[group]
    alpha <- alphas[group]
    beta <- betas[group]
    gamma <- gammas[group]
    delta <- deltas[group]
    
    if(j == 2) {
      if(base_gdp>25000){
        growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - 
          (alpha * tem[j-1]^2 + beta * tem[j-1])
        growth_level <- 0
      } else {
        growth_new <- lalpha * (tem[j]^2-tem[j-1]^2) + lbeta * (tem[j]-tem[j-1])
        growth_level <-(gamma * tem[j]^2 + delta * tem[j]) - 
          (gamma * tem[j-1]^2 + delta * tem[j-1])
        
      }
      gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new+ growth_level)
    }
    
    if(j > 2) {
      if(base_gdp>25000){
        growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - 
          (alpha * tem[j-1]^2 + beta * tem[j-1])
        growth_level <- 0
      } else {
        growth_new <- lalpha * (tem[j]^2-tem[j-1]^2) + lbeta * (tem[j]-tem[j-1]) +
          alpha * (tem[j-1]^2-tem[j-2]^2) + beta * (tem[j-1]-tem[j-2])
        growth_level <-(gamma * tem[j]^2 + delta * tem[j]) - 
          (gamma * tem[j-1]^2 + delta * tem[j-1])
        
      }
      gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new+ growth_level)
    }
    
    damage[j] <- (gdppc_new[j] - gdppc[j]) / gdppc[j]
    if(damage[j] > 1) {
      damage[j] <- 1
    } else if(damage[j] < -1) {
      damage[j] <- -1
    }
    
  }
  
  list(year = period, damage = damage)
}, by = region]
result_ld$model<-"JRJ_LD"

pop_ld <- pop %>%
  mutate(
    period = ((year - 2016) %/% 4) + 1
  )
pop_ld<-pop_ld %>%
  filter(.,period<=21) %>%
  group_by(region,period) %>%
  summarise(pop=mean(pop,na.rm=T)) 

gdppc_ld<-gdppc_ld[,c(1:4)]
gdppc_ld<-left_join(gdppc_ld,pop_ld,by=c("region","period"))
gdppc_ld<-gdppc_ld[,c(1,2,3,5)]
gdppc_ld$gdp<-gdppc_ld$gdppc*gdppc_ld$pop
gdppc1_ld<-left_join(result_ld,gdppc_ld,by=c("region","year"="period"))
gdppc1_ld$loss<-gdppc1_ld$gdppc*gdppc1_ld$damage*gdppc1_ld$pop
gdppc1_ld$year<-2020+4*(gdppc1_ld$year-1)

#############
gdppc <- gdppc %>% 
  arrange(region, year) %>%
  mutate(
    gdppc_new = ifelse(year == 2020, gdppc, NA),
    growth_new = NA
  )

tem <- tem %>%
  group_by(region) %>%
  mutate(T0 = mean(tem[year >= 2015 & year <= 2019], na.rm = TRUE)) %>%
  ungroup()
tem<-filter(tem, year>=2020)
library(data.table)
gdppc <- as.data.table(gdppc)
tem <- as.data.table(tem)
#########JRJ_panel model#######
df_panel <- gdppc[tem, on = c("region", "year")]
thresholds <- c(25200)
alphas <- c(-0.000486,0)
betas <- c(0.0132, 0)

  # 按地区计算
result_panel <- df_panel[, {
    gdppc_new <- gdppc
    damage <- numeric(.N)
    
    for(j in 1:.N) {
      if(j == 1) {
        base_gdp <- gdppc[j]
      } else {
        base_gdp <- gdppc_new[j-1]
      }
      
      # 选择参数组
      group <- findInterval(base_gdp, thresholds) + 1
      alpha <- alphas[group]
      beta <- betas[group]
      gamma <- gammas[group]
      delta <- deltas[group]
      
      growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - (alpha * T0[j]^2 + beta * T0[j])
      
      if(j > 1) {
        gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new)
      }
      
      damage[j] <- (gdppc_new[j] - gdppc[j]) / gdppc[j]
      if(damage[j] > 1) {
        damage[j] <- 1
      } else if(damage[j] < -1) {
        damage[j] <- -1
      }
      
    }
    
    list(year = year, damage = damage)
  }, by = region]
result_panel$model<-"JRJ_Panel"

#########BHM model#######
df_BHM <- gdppc[tem, on = c("region", "year")]
alpha <- -0.000487  # 统一的alpha值
beta <- 0.0127      # 统一的beta值
# 直接计算所有地区的损失
result_BHM <- df_BHM[, {
  gdppc_new <- gdppc
  damage <- numeric(.N)
  
  for(j in 1:.N) {
    # 计算温升带来的增长率变化
    growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - (alpha * T0[j]^2 + beta * T0[j])
    if(j > 1) {
      gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new)
    }
    damage[j] <- (gdppc_new[j] - gdppc[j]) / gdppc[j]
    # 限制损失率在[-1, 1]范围内
    damage[j] <- pmin(pmax(damage[j], -1), 1)
  }
  
  list(year = year, damage = damage)
}, by = region]
result_BHM$model<-"BHM"

#df_BHM <-df_BHM[,-c(5,6)]
#df_BHM<-df_BHM %>%
#  group_by(region) %>%
#  mutate(gchange=alpha*(tem^2-T0^2) + beta*(tem-T0)) 
#df_BHM<-df_BHM %>%
#  group_by(region) %>%
#  mutate(damage= cumsum(gchange))
#df_BHM<-df_BHM[,c(1,2,8)]
#result_BHM<-left_join(result_BHM,df_BHM, by=c("region","year"))

#########MBB model#######
df_MBB <- gdppc[tem, on = c("region", "year")]
alpha <- -0.0104  # 统一的alpha值
# 直接计算所有地区的损失
result_MBB <- df_MBB[, {
  gdppc_new <- gdppc
  damage <- numeric(.N)
  
  for(j in 1:.N) {
    if(j == 1) {
      base_gdp <- gdppc[j]
    } else {
      base_gdp <- gdppc_new[j-1]
    }
    
    if(base_gdp<= 25000){
      growth_new <- (alpha * tem[j]) - (alpha * T0[j])
    } else {
      growth_new<-0
    }
    
    if(j > 1) {
      gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new)
    }
    
    damage[j] <- (gdppc_new[j] - gdppc[j]) / gdppc[j]
    if(damage[j] > 1) {
      damage[j] <- 1
    } else if(damage[j] < -1) {
      damage[j] <- -1
    }
    
  }
  
  list(year = year, damage = damage)
}, by = region]
result_MBB$model<-"MBB"

result<-bind_rows(result_BHM,result_ML,result_GSE,result_panel,result_MBB)

###GLOBAL AVERAGE LOSS####
gdppc<-gdppc[,c(1:4)]
gdppc<-left_join(gdppc,pop,by=c("region","year"))
gdppc<-gdppc[,c(1,2,4,5)]
gdppc$gdp<-gdppc$gdppc*gdppc$pop
gdppc1<-left_join(result,gdppc,by=c("region","year"))
gdppc1$loss<-gdppc1$gdppc*gdppc1$damage*gdppc1$pop
gdppc1<-bind_rows(gdppc1,gdppc1_ld)
global<-gdppc1%>%
    group_by(year,model) %>%
    summarise(gdp=sum(gdp,na.rm = T),loss=sum(loss,na.rm = T))
global$damage<-global$loss/global$gdp
global$damage[is.nan(global$damage)]<-0
ggplot(data = global) +
  geom_line(aes(x = year, y =damage, color = model), 
           linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_cartesian(ylim = c(-0.4, 0.4)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.2)) +
  theme_classic() +
  labs(x = "", y = "Percentage change in GDP per capita") +
  theme(axis.title.y = element_text(size = 8))
ggsave(
  filename = "..\\03figure\\FigureS1.pdf")
write_excel_csv(global,file = "..\\02table\\figureS1_ssp126.csv")
