#big mac data from https://github.com/TheEconomist/big-mac-data 

bigmac <- rio::import(here::here("data","raw","big-mac-2022-07-01.xls")) %>% janitor::clean_names()

#region data from https://ourworldindata.org/world-region-map-definitions

regions <- rio::import(here::here("data","raw","world-regions-sdg-united-nations.csv")) %>% 
  janitor::clean_names() %>% 
  select(code,world_regions_according_to_the_united_nations) %>% 
  rename("iso_a3"="code","region"="world_regions_according_to_the_united_nations")
bigmac2 <- left_join(bigmac,regions)

bigmac <- bigmac %>% mutate(above_us = case_when(
  bigmac$dollar_price >= bigmac$dollar_price[bigmac$Country == "United States"] ~ 1,
  TRUE ~ 0
))

#create world region var
#table with local_price, dollar_price, dollar_ppp, GDP_bigmac, dollar_valuation

tbl_summary(bigmac2,by=region,include=c(local_price, dollar_price, dollar_ppp, gdp_bigmac, dollar_valuation))
