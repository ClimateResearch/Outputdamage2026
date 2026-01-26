setwd("E:\\06博士论文\\002出国交流\\02cliamte_economy\\output_ld_new\\00script")
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
tem<-read_csv("..\\01data\\04scenarios\\gdppc_k_tems_ssp245_2015-2100_new.csv")
tem<-tem[,-c(2,3)]
tem<-tidyr::gather(tem,model,tem, -c(region,year))
tem_models <- unique(tem$model)
tem <- tem %>%
  group_by(region, model) %>%
  mutate(
    tem = ifelse(year == 2100 & tem == 0, 
                 tem[year == 2099], 
                 tem))

boots<-read_csv("..\\01data\\04scenarios\\Bootstrap3.csv")

region<-read_csv("..\\01data\\04scenarios\\region.csv")

#######get GDP per capita#########
gdppc_o<-read_csv("..\\01data\\tabulated_adm1_gdp_perCapita.csv")
gdppc_o<-gdppc_o[,-c(2:5)]
gdppc_o<- tidyr:: gather(gdppc_o,year,value,-c("GID_nmbr"))
gdppc_o$year<-as.numeric(gdppc_o$year)
colnames(gdppc_o)[c(1,3)]<-c("region","gdppc")
gdppc<-filter(gdppc_o, year==2019)
colnames(gdppc)[c(1,3)]<-c("region","gdppc0")
gdppc<-gdppc[,-2]

pop<-read_csv("..\\01data\\04scenarios\\scenario_ssp2_pop_2015-2100_new.csv")
gdp<-read_csv("..\\01data\\04scenarios\\scenario_ssp2_gdp_2005-2100_new.csv")
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

##################
gdppc_panel <- gdppc %>% 
  arrange(region, year) %>%
  mutate(
    gdppc_new = ifelse(year == 2020, gdppc, NA),
    growth_new = NA
  )

tem_panel <- tem %>%
  group_by(model, region) %>%
  mutate(T0 = mean(tem[year >= 2015 & year <= 2019], na.rm = TRUE)) %>%
  ungroup()
tem_panel<-filter(tem_panel, year>=2020)

library(data.table)
# 转换为data.table
gdppc_panel <- as.data.table(gdppc_panel)
tem_panel <- as.data.table(tem_panel)
boots <- as.data.table(boots)

# 使用data.table进行高效计算
loss_list <- lapply(1:1000, function(i) {
  
  selc_tem <- sample(tem_models, 1)
  selc_boot <- boots[sample(.N, 1)]
  
  # 合并数据
  df <- gdppc_panel[tem_panel[model == selc_tem], on = c("region", "year")]
  
  # 设置参数
  thresholds <- c(11600, 35200)
  alphas <- c(selc_boot$T2_panel0, selc_boot$T2_panel1, selc_boot$T2_panel2)
  betas <- c(selc_boot$T_panel0, selc_boot$T_panel1, selc_boot$T_panel2)
  
  gammas <- c(selc_boot$dT2_panel0, selc_boot$dT2_panel1, selc_boot$dT2_panel2)
  deltas <- c(selc_boot$dT_panel0, selc_boot$dT_panel1, selc_boot$dT_panel2)
  
  # 按地区计算
  result <- df[, {
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
        growth_level <- (gamma * tem[j]^2 + delta * tem[j]) - (gamma * tem[j-1]^2 + delta * tem[j-1])
        gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new+ growth_level) #
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
  
  cat(i,"\n")
  
  setnames(result, "damage", paste0("damage", i))
  result
})

# 合并结果
loss <- Reduce(function(x, y) merge(x, y, by = c("region", "year")), loss_list)
save(loss, file = "..\\01data\\04scenarios\\bootstrap_panel_ssp245_loss_1000.RData")
load("..\\01data\\04scenarios\\bootstrap_panel_ssp245_loss_1000.RData")

###GDPpc weighted results####
gdppc_panel<-left_join(gdppc,pop,by=c("region","year"))
gdppc_panel<-gdppc_panel[,c(1,2,4,5)]
gdppc_panel$gdp<-gdppc_panel$gdppc*gdppc_panel$pop

loss_summary <- gdppc_panel %>%
  left_join(loss,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, damage) %>%
  summarise(value_gdp = sum(value * gdp, na.rm = TRUE) / sum(gdp, na.rm = TRUE),
            value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 

loss_summary <-loss_summary %>%
  group_by(year) %>%
  summarise(
    mean_gdp = median(value_gdp , na.rm = TRUE),
    sd_gdp = sd(value_gdp , na.rm = TRUE),
    q10_gdp = quantile(value_gdp , 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp , 0.90, na.rm = TRUE),
    mean_pop = median(value_pop , na.rm = TRUE),
    sd_pop = sd(value_pop , na.rm = TRUE),
    q10_pop = quantile(value_pop , 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop , 0.90, na.rm = TRUE),
  )
write_excel_csv(loss_summary,file = "..\\02table\\loss_panel_ssp245_summary.csv")

b<-ggplot(data = loss_summary) +
  # 绘制95%置信区间
  geom_ribbon(aes(x = year, ymin =q10_gdp, ymax =q90_gdp), 
              fill = rgb(252/255, 141/255, 98/255), alpha = 0.3) +
  # 绘制平均值的红色折线
  geom_line(aes(x = year, y = mean_gdp), 
            color = rgb(252/255, 141/255, 98/255), linewidth = 1) +
  geom_ribbon(aes(x = year, ymin =q10_pop, ymax =q90_pop), 
              fill = rgb(60/255, 180/255, 160/255), alpha = 0.3) +
  # 绘制平均值的红色折线
  geom_line(aes(x = year, y = mean_pop), 
            color = rgb(60/255, 180/255, 160/255), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_cartesian(ylim = c(-0.8, 0.8)) +
  scale_y_continuous(breaks = seq(-0.8, 0.8, 0.4)) +
  theme_classic() +
  labs(x = "", y = "Percentage change in GDP per capita") +
  theme(axis.title.y = element_text(size = 8))
plot(b)
#######
gdppc_panel<-left_join(gdppc_panel,region, by=c("region"="GID_nmbr"))

loss_summary <- gdppc_panel %>%
  left_join(loss,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, damage,continent) %>%
  summarise(value_gdp = sum(value * gdp, na.rm = TRUE) / sum(gdp, na.rm = TRUE),
            value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 

loss_summary <-loss_summary %>%
  group_by(year,continent) %>%
  summarise(
    mean_gdp = median(value_gdp , na.rm = TRUE),
    sd_gdp = sd(value_gdp , na.rm = TRUE),
    q10_gdp = quantile(value_gdp , 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp , 0.90, na.rm = TRUE),
    mean_pop = median(value_pop , na.rm = TRUE),
    sd_pop = sd(value_pop , na.rm = TRUE),
    q10_pop = quantile(value_pop , 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop , 0.90, na.rm = TRUE),
  )
write_excel_csv(loss_summary,file = "..\\02table\\loss_panel_ssp245_continent_summary.csv")
######
loss_region<-filter(loss,year==2100)
loss_region<-loss_region[,-2]
loss_region<-tidyr::gather(loss_region,damage,value, -region)
loss_region<-loss_region %>%
  group_by(region) %>%
  summarise(mean=mean(value,na.rm=T))

library(sp)
library(sf)
global_map<-st_read("..\\01data\\polyg_adm1_gdp_perCapita_1990_2022.gpkg", 
                fid_column_name = 'FID')
global_map <- global_map[, 1:3]
global_map<-sf::st_transform(global_map,crs="+proj=longlat +datum=WGS84")

map<-left_join(global_map,loss_region, by=c("GID_nmbr"="region"))

library(ggplot2)
library(RColorBrewer)
a<-ggplot(data = map) +
  geom_sf(aes(fill = mean)) +  # 使用 fill 映射 loss_avg 列
  scale_fill_distiller(palette = "RdYlBu",
                       direction=1,
                       limits = c(-1, 1),                # 设置填充颜色的范围
                       oob = scales::squish,
                       name = "Percentage Change in GDP per capita" )+
  theme_minimal() +
  labs(fill="")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),# 移除主要网格线
        axis.text = element_blank(),
        legend.title = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = unit(20, "lines"),  # 设置图例的宽度
                               barheight = unit(0.5, "lines"),
                               title.position = "top",             # 标题在图例的上部
                               title.hjust = 0.5))+ 
  coord_sf(crs = "+proj=robin") # 调整图例的高度)
print(a)


################
gdppc_sub<-filter(gdppc_o, year %in% c(2016:2019))
gdppc_ld<-bind_rows(gdppc,gdppc_sub)
gdppc_ld <- gdppc_ld %>%
  mutate(
    period = ((year - 2016) %/% 4) + 1
  )
gdppc_ld<-gdppc_ld %>%
  filter(.,period<=21) %>%
  group_by(region,period) %>%
  summarise(gdppc=mean(gdppc,na.rm=T))

gdppc_ld<-gdppc_ld %>%
  group_by(region) %>%
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
  group_by(region,period,model) %>%
  summarise(tem=mean(tem,na.rm=T)) %>%
  filter(.,period<=21)

tem_ld <- tem_ld %>%
  group_by(model, region) %>%
  mutate(T0 = tem[period= 1], na.rm = TRUE) %>%
  ungroup()

library(data.table)
gdppc_ld <- as.data.table(gdppc_ld)
tem_ld <- as.data.table(tem_ld)
boots <- as.data.table(boots)

# 使用data.table进行高效计算
loss_list <- lapply(1:1000, function(i) {
  
  selc_tem <- sample(tem_models, 1)
  selc_boot <- boots[sample(.N, 1)]
  
  # 合并数据
  df <- gdppc_ld[tem_ld[model == selc_tem], on = c("region", "period")]
  
  # 设置参数
  thresholds <- c(12000, 35500)
  alphas <- c(selc_boot$T2_ld0, selc_boot$T2_ld1, selc_boot$T2_ld2)
  betas <- c(selc_boot$T_ld0, selc_boot$T_ld1, selc_boot$T_ld2)
  
  # 按地区计算
  result <- df[, {
    gdppc_new <- gdppc
    damage <- numeric(.N)
    
    for(j in 1:.N) {
      if(j == 1) {
        base_gdp <- gdppc_new[j]
      } else {
        base_gdp <- gdppc_new[j-1]
      }
      # 选择参数组
      group <- findInterval(base_gdp, thresholds) + 1
      alpha <- alphas[group]
      beta <- betas[group]
    
      if(j > 1) {
        if(base_gdp < 35500) {
          # 对于小于35500的分组：使用现有的损失函数
          # 比较当前期和上一期的温度影响
          growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - 
            (alpha * tem[j-1]^2 + beta * tem[j-1])
        } else {
          # 对于大于等于35500的分组：使用新的损失函数
          # 比较当前期和基期（T0）的温度影响
          growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - 
            (alpha * tem[j-1]^2 + beta * tem[j-1])
          #growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - 
          #  (alpha * T0[j]^2 + beta * T0[j])
          
        }
        
        #growth_new <- (alpha * tem[j]^2 + beta * tem[j]) - (alpha * tem[j-1]^2 + beta * tem[j-1])
        gdppc_new[j] <- gdppc_new[j-1] * (1 + growthpc[j] + growth_new)
      }
      damage[j] <- (gdppc_new[j] - gdppc[j]) / gdppc[j]
      if(damage[j] > 1) {
        damage[j] <- 1
      } else if(damage[j] < -1) {
        damage[j] <- -1
      }
    }
    
    list(period = period, damage = damage)
  }, by = region]
  
  cat(i,"\n")
  
  setnames(result, "damage", paste0("damage", i))
  result
})

# 合并结果
loss <- Reduce(function(x, y) merge(x, y, by = c("region", "period")), loss_list)
save(loss, file = "..\\01data\\04scenarios\\bootstrap_ld_ssp245_loss_1000.RData")
load("..\\01data\\04scenarios\\bootstrap_ld_ssp245_loss_1000.RData")

gdppc_ld<-gdppc_ld[,c(1:4)]
pop <- pop %>%
  mutate(
    period = ((year - 2016) %/% 4) + 1
  )
pop<-pop %>%
  filter(.,period<=21) %>%
  group_by(region,period) %>%
  summarise(pop=mean(pop,na.rm=T))
gdppc_ld<-left_join(gdppc_ld,pop,by=c("region","period"))
gdppc_ld<-gdppc_ld[,c(1,2,3,5)]
gdppc_ld$gdp<-gdppc_ld$gdppc*gdppc_ld$pop

loss_summary <- gdppc_ld %>%
  left_join(loss,by=c("region","period")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(period, damage) %>%
  summarise(value_gdp = sum(value * gdp, na.rm = TRUE) / sum(gdp, na.rm = TRUE),
            value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 

loss_summary <-loss_summary %>%
  group_by(period) %>%
  summarise(
    mean_gdp = median(value_gdp , na.rm = TRUE),
    sd_gdp = sd(value_gdp , na.rm = TRUE),
    q10_gdp = quantile(value_gdp , 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp , 0.90, na.rm = TRUE),
    mean_pop = median(value_pop , na.rm = TRUE),
    sd_pop = sd(value_pop , na.rm = TRUE),
    q10_pop = quantile(value_pop , 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop , 0.90, na.rm = TRUE),
  )

loss_summary$period<-2020+4*(loss_summary$period-1)
loss_summary[is.na(loss_summary)]<-0
d<-ggplot(data = loss_summary) +
  # 绘制95%置信区间
  geom_ribbon(aes(x =period, ymin =q10_gdp, ymax =q90_gdp), 
              fill = rgb(252/255, 141/255, 98/255), alpha = 0.3) +
  # 绘制平均值的红色折线
  geom_line(aes(x = period, y = mean_gdp), 
            color = rgb(252/255, 141/255, 98/255), linewidth = 1) +
  geom_ribbon(aes(x = period, ymin =q10_pop, ymax =q90_pop), 
              fill = rgb(60/255, 180/255, 160/255), alpha = 0.3) +
  # 绘制平均值的红色折线
  geom_line(aes(x = period, y = mean_pop), 
            color = rgb(60/255, 180/255, 160/255), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.1)) +
  theme_classic() +
  labs(x = "", y = "Percentage change in GDP per capita") +
  theme(axis.title.y = element_text(size = 8))
plot(d)

write_excel_csv(loss_summary,file = "..\\02table\\loss_ld_ssp245_summary.csv")

#######
gdppc_ld<-left_join(gdppc_ld,region, by=c("region"="GID_nmbr"))

loss_summary <- gdppc_ld %>%
  left_join(loss,by=c("region","period")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(period, damage,continent) %>%
  summarise(value_gdp = sum(value * gdp, na.rm = TRUE) / sum(gdp, na.rm = TRUE),
            value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 

loss_summary <-loss_summary %>%
  group_by(period,continent) %>%
  summarise(
    mean_gdp = median(value_gdp , na.rm = TRUE),
    sd_gdp = sd(value_gdp , na.rm = TRUE),
    q10_gdp = quantile(value_gdp , 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp , 0.90, na.rm = TRUE),
    mean_pop = median(value_pop , na.rm = TRUE),
    sd_pop = sd(value_pop , na.rm = TRUE),
    q10_pop = quantile(value_pop , 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop , 0.90, na.rm = TRUE),
  )
write_excel_csv(loss_summary,file = "..\\02table\\loss_ld_ssp245_continent_summary.csv")

######
loss_region<-filter(loss,period==21)
loss_region<-loss_region[,-2]
loss_region<-tidyr::gather(loss_region,damage,value, -region)
loss_region<-loss_region %>%
  group_by(region) %>%
  summarise(mean=mean(value,na.rm=T))

library(sp)
library(sf)
global_map<-st_read("..\\01data\\polyg_adm1_gdp_perCapita_1990_2022.gpkg", 
                    fid_column_name = 'FID')
global_map <- global_map[, 1:3]
global_map<-sf::st_transform(global_map,crs="+proj=longlat +datum=WGS84")

map<-left_join(global_map,loss_region, by=c("GID_nmbr"="region"))

library(ggplot2)
library(RColorBrewer)
c<-ggplot(data = map) +
  geom_sf(aes(fill = mean)) +  # 使用 fill 映射 loss_avg 列
  scale_fill_distiller(palette = "RdYlBu",
                       direction=1,
                       limits = c(-0.201, 0.201),                # 设置填充颜色的范围
                       oob = scales::squish,
                       name = "Percentage Change in GDP per capita" )+
  theme_minimal() +
  labs(fill="")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),# 移除主要网格线
        axis.text = element_blank(),
        legend.title = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = unit(20, "lines"),  # 设置图例的宽度
                               barheight = unit(0.5, "lines"),
                               title.position = "top",             # 标题在图例的上部
                               title.hjust = 0.5))+ 
  coord_sf(crs = "+proj=robin") # 调整图例的高度)
print(c)


library(patchwork)
library(cowplot)
combined_plot <- (a | b) / (c | d)
combined_plot <- plot_grid(
  a, b, c, d,
  nrow = 2, ncol = 2,
  labels = c("a","b","c","d")
)
print(combined_plot )

ggsave(
  filename = "..\\03figure\\Figure6_585.pdf")

####unweighted results####
loss_year<-tem_loss %>%
  left_join(region,by=c("region")) %>%
  group_by(year,metacontinent) %>%
  summarise_all(mean,na.rm=T)
loss_year<-loss_year[,-3]
loss_year<-tidyr::gather(loss_year,damage,value, -c(metacontinent,year))
####population weighted results####
loss_year_pop <- tem_loss %>%
  left_join(region,by=c("region")) %>%
  left_join(pop,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, metacontinent,damage) %>%
  summarise(value_pop = sum(value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)) 
###GDPpc weighted results####
loss_year_gdp <- tem_loss %>%
  left_join(region,by=c("region")) %>%
  left_join(gdppc,by=c("region","year")) %>%
  pivot_longer(cols = starts_with("damage"), 
               names_to = "damage", 
               values_to = "value") %>%
  group_by(year, metacontinent,damage) %>%
  summarise(value_gdp = sum(value * gdppc, na.rm = TRUE) / sum(gdppc, na.rm = TRUE)) 

loss_year<- loss_year %>%
  left_join(loss_year_pop,by=c("year","metacontinent","damage")) %>%
  left_join(loss_year_gdp,by=c("year","metacontinent","damage"))

loss_summary <- loss_year %>%
  group_by(year,metacontinent) %>%
  summarise(
    mean = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q10 = quantile(value, 0.10, na.rm = TRUE),
    q90 = quantile(value, 0.90, na.rm = TRUE),
    
    mean_pop = median(value_pop, na.rm = TRUE),
    sd_pop = sd(value_pop, na.rm = TRUE),
    q10_pop = quantile(value_pop, 0.10, na.rm = TRUE),
    q90_pop = quantile(value_pop, 0.90, na.rm = TRUE),
    
    mean_gdp = median(value_gdp, na.rm = TRUE),
    sd_gdp = sd(value_gdp, na.rm = TRUE),
    q10_gdp = quantile(value_gdp, 0.10, na.rm = TRUE),
    q90_gdp = quantile(value_gdp, 0.90, na.rm = TRUE)
  )
write_excel_csv(loss_summary,file = "..\\01data\\loss_ld_summary_subcont.csv")

loss_summary<-read_csv("..\\01data\\loss_panel_summary_subcont.csv")
continent<-unique(loss_summary$metacontinent)
plot<-list()
library(scales)
for (i in continent) {
  loss_sub<-filter(loss_summary, metacontinent ==i)
  p<-ggplot(data = loss_sub) +
    # 绘制95%置信区间
    geom_ribbon(aes(x = year, ymin =q10_gdp, ymax =q90_gdp), 
                fill = rgb(252/255, 141/255, 98/255), alpha = 0.3) +
    # 绘制平均值的红色折线
    geom_line(aes(x = year, y = mean_gdp), 
              color = rgb(252/255, 141/255, 98/255), linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    coord_cartesian(ylim = c(-0.90, 0.60)) +
    scale_y_continuous(breaks = seq(-0.90, 0.60, 0.30),labels = label_number(accuracy = 0.1)) +
    theme_classic() +
    labs(title = i,x = "", y = "Percentage change in GDP per capita") +
    theme(plot.title = element_text(hjust = 0.5, size = 12),axis.title.y = element_text(size = 10))
  print(p)
  plot[[i]] <- p
}
combined_plot <- (plot[[1]] | plot[[2]] | plot[[3]]) /
  (plot[[4]] | plot[[5]] | plot[[6]]) /
  (plot[[7]] | plot[[8]] | plot[[9]])
final_plot <- combined_plot + 
  plot_annotation(tag_levels = 'a')  # 自动标注 a, b, c
print(final_plot)

ggsave(
  filename = "..\\03figure\\FigureS6_2.jpg",
  dpi = 500,                         # 设置分辨率为300像素
  width = 10,                        # 设置图像宽度（单位：英寸）
  height = 10                        # 设置图像高度（单位：英寸）
)
##############
tem<-filter(tem,year %in% c(2015:2019,2100))

tem<-tem %>%
  group_by(region,model) %>%
  mutate(T0=mean(tem[year >= 2015 & year <= 2019],na.rm=T))

tem<-filter(tem, year==2100)
tem$dT <- tem$tem-tem$T0

t=2100-2019

alpha = 0.0218
beta = -0.000760

tem2 <- tem %>%
  mutate(damage = t*((alpha+2*beta*T0)*dT+beta*dT^2))
