library(tidyverse)
library(cansim)
library(lubridate)
library(cowplot)

#### Download 2017 CPI Basket Weights (link month is Dec 2018)
cpi_weights<-get_cansim("18-10-0007-01") %>%
  normalize_cansim_values

#components we will be using
cpi_components<-c("Food","Transportation","Household operations, furnishings and equipment",
                  "Clothing and footwear","Health and personal care","Recreation, education and reading",
                  "Alcoholic beverages, tobacco products and recreational cannabis",
                  "Rented accommodation","Water, fuel and electricity","Mortgage interest cost",
                  "Homeowners' replacement cost","Property taxes and other special charges",
                  "Homeowners' home and mortgage insurance","Homeowners' maintenance and repairs",
                  "Other owned accommodation expenses")

cpi_weights<-cpi_weights %>%
  filter(REF_DATE==2017) %>%
  filter(GEO=="Canada") %>%
  rename(type='Price period of weight',
         where='Geographic distribution of weight',
         category='Products and product groups',
         weight=VALUE) %>%
  filter(type=="Weight at basket link month prices") %>%
  filter(where=="Distribution to selected geographies") %>%
  select(category, weight) %>%
  filter(category %in% cpi_components) 


#### Download seasonally unadjusted CPI data for the components

cpi_data<-get_cansim("18-10-0004-01") %>%
  normalize_cansim_values %>%
  rename(category='Products and product groups',
         cpi=VALUE) %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  filter(GEO=="Canada")%>%
  select(date,category,cpi) %>%
  mutate(date=ymd(date)) %>%
  filter(date>"2018-11-01")

cpi_base<-cpi_data %>%
  filter(date=="2018-12-01") %>%
  rename(base=cpi) %>%
  select(category,base)

cpi_data<-cpi_data %>%
  left_join(cpi_base) %>%
  group_by(category) %>%
  mutate(cpi=100*cpi/base) %>%
  select(date,category, cpi) %>%
  filter(category %in% cpi_components) %>%
  ungroup()


official<-cpi_data %>%
  left_join(cpi_weights) %>%
  group_by(date) %>%
  summarize(official=sum(cpi*weight))

money_outlays<-cpi_data %>% 
  left_join(cpi_weights) %>%
  filter(category!="Homeowners' replacement cost") %>%
  group_by(date) %>%
  summarize(money_outlays=sum(cpi*(weight/sum(weight))))

###Load in Teranet House Price Index

house_prices<-read_csv("House_Price_Index.csv") %>%
  rename(date='Transaction Date') %>%
  select(date,c11) %>%
  mutate(date=paste("01",date,sep="-")) %>%
  mutate(date=lubridate::parse_date_time(date, orders = "d-b-Y", locale = "us")) %>%
  mutate(date=ymd(date)) %>%
  filter(date>"2018-11-01") %>%
  mutate(price=as.numeric(c11)) %>%
  select(date, price)

hp_base<-house_prices %>%
  filter(date=="2018-12-01")

house_prices<-house_prices %>%
  mutate(price=100*price/hp_base$price)

net_purchases<-cpi_data %>%
  left_join(cpi_weights) %>%
  left_join(house_prices) %>%
  mutate(cpi_np=ifelse(category %in% c("Homeowners' replacement cost","Mortgage interest cost"),price,cpi)) %>%
  group_by(date) %>%
  summarize(net_purchases=sum(cpi_np*(weight/sum(weight))))


df<-official %>%
  left_join(money_outlays) %>%
  left_join(net_purchases) %>%
  pivot_longer(official:net_purchases,names_to="Method",values_to="CPI") %>%
  group_by(Method) %>%
  mutate(lag12=lag(CPI,n=12)) %>%
  mutate(inf_rate=100*(CPI-lag12)/lag12) %>%
  ungroup()


df %>%
  filter(date>"2019-11-01")%>%
  ggplot(aes(date,inf_rate))+
  geom_line(aes(color=Method),size=1.3)+
  theme_minimal_hgrid(14)+
  labs(title="Inflation using different methods for Owned Accommodation:\nCanada 2019-2021",
       caption="Data: Statistics Canada Tables 18-10-0004-01, 18-10-0007-01\n     Teranet & National Bank House Price Index" ,
       x=NULL,
       y="Inflation (year over year, %)")+
  theme(legend.position="bottom")

