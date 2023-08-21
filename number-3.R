pacman::p_load(tidyverse,
               gtsummary,
               patchwork)

#big mac data from https://github.com/TheEconomist/big-mac-data 

bigmac <- rio::import(here::here("data","raw","big-mac-2022-07-01.xls")) %>% janitor::clean_names()

#region data from https://ourworldindata.org/world-region-map-definitions

regions <- rio::import(here::here("data","raw","world-regions-sdg-united-nations.csv")) %>% 
  janitor::clean_names() %>% 
  select(code,world_regions_according_to_the_united_nations) %>% 
  rename("iso_a3"="code","region"="world_regions_according_to_the_united_nations")
bigmac2 <- left_join(bigmac,regions) %>% drop_na()

#summary statistics
tbl_summary(bigmac2,
            by=region,
            include=c(local_price, dollar_price, dollar_ppp, gdp_bigmac, dollar_valuation),
            label=list(local_price ~ "Local Price",
                       dollar_price ~ "Dollar Price",
                       dollar_ppp ~ "USD PPP",
                       gdp_bigmac ~ "Big Mac GDP",
                       dollar_valuation ~ "Dollar Valuation")) %>% 
  add_overall(col_label = "**Total**")

#regression statistics
model <- lm(gdp_bigmac ~ local_price + dollar_ex, data=bigmac2)
tbl_regression(model,
               intercept = TRUE,
               label = list(
                 local_price ~ "Local Price",
                 dollar_ex ~ "Dollar Exchange Rate"
               ))

#create a figure
price_graph <- bigmac2 %>% ggplot(aes(x=log(local_price),y=log(gdp_bigmac))) + 
  geom_point(color="red") +
  labs(x="log(Local Price)",y="log(Dollar Echange Rate)")
dollar_graph <- bigmac2 %>% ggplot(aes(x=log(dollar_ex),y=log(gdp_bigmac))) + 
  geom_point(color="blue") +
  labs(x="log(Local Price)",y="log(Dollar Echange Rate)")
price_graph + dollar_graph

#create a function
    #input iso_a3 for geometric mean of local prices

geo_mean <- function(x) {
  df <- data.frame(matrix(nrow=length(x),ncol=2))
  colnames(df) <- c("position","local_price")
  for (i in 1:length(x)) {
    df[i,1] <- which(bigmac2$iso_a3 == x[i])
    df[i,2] <- bigmac2$local_price[df[i,1]]
  }
  n <- length(x)
  geo_mean <- (prod(df$local_price))^(1/n)
  return(geo_mean)
}

geo_mean(x=c("VNM","URY","AUS","THA","KWT"))
