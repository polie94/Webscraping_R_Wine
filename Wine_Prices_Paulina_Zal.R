library(rvest)
library(httr)
library(XML)
library(xml2)
library(RSelenium)
library(stringr)
library(ggplot2)
library(dplyr)


## Scrape every wine
Wine <- function(link, remDr) {
  remDr$open()
  remDr$navigate(link) 
  #Productbeschreibung find and click
  details<-remDr$findElement(using ="xpath", 
                             './/*[@class="sc-r3s8uh-0 sc-r3s8uh-1 sc-c68xzx-0 ccNYOj esYDbS fbHzAX"]')
  details$clickElement()
  #convert to html
  page_html1 <- read_html(remDr$getPageSource()[[1]])
  #find descriptions, details, title and values
  descriptions_html<-html_nodes(page_html1,xpath='//*[@class="sc-r3s8uh-0 sc-r3s8uh-2 sc-el6y42-0 czOuMp eLRtUm kOtXGd"]')
  wine_details_title<-html_nodes(descriptions_html,xpath='.//*[@class="sc-1nmlmz4-4 kkKmUX"]') %>%
    html_text()
  wine_details_title<-wine_details_title[wine_details_title != ""]
  wine_details_values<-html_nodes(descriptions_html,xpath='.//*[@class="sc-1nmlmz4-3 fSoTwx"]') %>%
    html_text()
  #combine it together
  df <- data.frame(matrix(ncol = length(wine_details_title), nrow = 0))
  df[nrow(df) + 1,] <- wine_details_values
  colnames(df) <- wine_details_title
  Sys.sleep(3)
  remDr$close()
  Sys.sleep(1)
  return(df) #information about the bottle from the page
}

#open browser
rD <- rsDriver(port = 4458L, browser = c("firefox"))
remDr <- rD[["client"]]
remDr$open()
remDr$navigate("https://www.manor.ch/de/shop/wein/wein-sortiment/c/all-wines?country=20000743-30006425&category=whitewine&category=redwine&category=rosewine")

test=TRUE
time =10
#infinity loop for dealing with every item 
while(TRUE){
  tryCatch(
    {
      acc<-remDr$findElement(using ="xpath", './/*[@class="sc-18pdz1z-0 sc-18pdz1z-2 sc-1p0xck9-5 biwDss kyVLGJ kFWlEL"]')
      acc$clickElement()
      Sys.sleep(time)
    },
    error=function(e) {
      message('An Error Occurred')
      print(e)
      break  #stop the loop
    },
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
    
  )
  
}
page_html <- read_html(remDr$getPageSource()[[1]])
product_html<-html_nodes(page_html,xpath='//*[@class="sc-r3s8uh-0 sc-r3s8uh-2 sc-1p0xck9-0 eFZERw dlJheV jVwxnB"]')
remDr$close()
products_html<-html_nodes(page_html,xpath='//*[@data-track-id="product"]')

df_wine <- data.frame(matrix(ncol =12, nrow = 0))
x <- c("Wine", "Info", "Price Reduced", "Price Before", "Price Now", "Price_Regular", "Region AOC", "Land", "Region", "Grapes", "Volumen", "Preis per 75cl")
colnames(df_wine) <- x


i=1

#Split the data into items
products_html<-html_nodes(page_html,xpath='//*[@data-track-id="product"]')
#iterate over products
for (product_html in products_html){
  wine_name<-html_node(product_html,xpath='.//*[@class="sc-1nmlmz4-4 sc-7uj4q3-6 sc-7uj4q3-7 kkKmUX iDXTAH cMAPUM"]') %>% 
    html_text()
  wine_info<-html_node(product_html,
                       xpath='.//*[@class="sc-1nmlmz4-4 sc-7uj4q3-6 kkKmUX iDXTAH"]') %>% 
    html_text()
  price_reduced<-html_node(product_html,xpath='.//*[@class="sc-16vhs2x-4 cQdTJc"]') %>%
    html_text()
  price_before<-html_node(product_html,xpath='.//*[@class="sc-16vhs2x-2 gcVPIE"]') %>%
    html_text()
  price_now<-html_nodes(product_html,xpath='.//*[@class="sc-16vhs2x-4 cQdTJc"]') %>%
    html_text()
  link <- html_attr(product_html, "href")
  link<-paste0("https://www.manor.ch/",link)
  price_reduced<-as.numeric(str_extract(price_reduced, "[+-]?([0-9]*[.])?[0-9]+"))
  price_before<-as.numeric(str_extract(price_before, "[+-]?([0-9]*[.])?[0-9]+"))
  price_now<-as.numeric(str_extract(price_now, "[+-]?([0-9]*[.])?[0-9]+"))
  region<-as.list(str_split(wine_info,",")[[1]])
  region<-tail(region,n=1)
  #Find regular price
  if(is.na(price_before)==T){
    price_regular<-price_now
  }else {
    price_regular<-price_before
  }


  df_wine[nrow(df_wine)+1,"Wine"]<-wine_name
  df_wine[nrow(df_wine),"Info"]<-wine_info
  df_wine[nrow(df_wine),"Price Reduced"]<-price_reduced
  df_wine[nrow(df_wine),"Price Before"]<-price_before
  df_wine[nrow(df_wine),"Price Now"]<-price_now
  df_wine[nrow(df_wine),"Price_Regular"]<-price_regular
  df_wine[nrow(df_wine),"Region AOC"]<-region
 # cat(toString(product_html))
 # print(paste0("https://www.manor.ch/",link))
  #Open the item's web page
  df<-Wine(link,remDr)
  #assign scraped data
  if ("Region" %in% names(df)){
    df_wine[nrow(df_wine),"Region"]<-df$Region}
  if ("Herkunftsland " %in% names(df)){
    df_wine[nrow(df_wine),"Land"]<-df$`Herkunftsland `}
  if ("Traubensorte" %in% names(df)){
    df_wine[nrow(df_wine),"Grapes"]<-df$Traubensorte}
  if ("Inhalt" %in% names(df)){
    df_wine[nrow(df_wine),"Volumen"]<-as.numeric(str_extract(df$Inhalt, "[+-]?([0-9]*[.])?[0-9]+"))
    df_wine[nrow(df_wine),"Preis per 75cl"]<-df_wine[nrow(df_wine),"Price_Regular"]/df_wine[nrow(df_wine),"Volumen"]*75}
  print("*******")

}
df_wine
df_wine$Grapes<-tolower(str_trim(df_wine$Grapes,side = c("right")))
write.csv(df_wine,"wine_results_2.csv")


#plot Price average per region
p<-df_wine%>% 
  group_by(Region)  %>%
  summarise(regula_price = mean(`Preis per 75cl`,na.rm=T)) %>% 
  ggplot( aes(x=reorder(Region,desc(regula_price)), y=regula_price)) +
  geom_bar(stat="identity", fill="#741b47")+
  ylab("Regular price in CHF per 75cl bottle")+
  xlab("Region")+
  theme_minimal()+
  labs("  ")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()) 
p

#Grape variety analysis
df_weinsorte <- data.frame(matrix(ncol =2, nrow = 0))
x <- c("Sorte", "Preis")
colnames(df_weinsorte) <- x

df_temp<-df_wine %>% 
  filter(!is.na(Grapes)) %>% 
  filter(Region!="Sizilien")
#Data frame with assignment wine  gravety - price
for (i in 1:nrow(df_temp)){
  lista<-tolower(str_replace(str_trim(as.list(str_split(df_temp[i,"Grapes" ],",")[[1]]),
                                      side = c("both")),"’", "'"))
  df_loop<-data.frame(Sort=lista)
  df_loop$Price<-df_temp[i,"Preis per 75cl" ]
  print(df_loop)
  df_weinsorte<-rbind(df_weinsorte,df_loop)
} 

df_weinsorte
#plot price per grape variety
p<-df_weinsorte%>% 
  group_by(Sort)  %>%
  summarise(regular_price = mean(Price)) %>% 
  ggplot( aes(x=reorder(Sort,desc(regular_price)), y=regular_price)) +
  geom_bar(stat="identity", fill="#741b47")+ylab("Regular price in CHF per 75cl bottle")+
  xlab("Wine variety")+
  theme_minimal()+
  labs("  ")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                             panel.background = element_blank()) 
p

w<-df_weinsorte%>% 
  group_by(Sort)  %>%
  summarise(regular_price = mean(Price))


#test

for (i in 1:nrow(df_temp)){
  lista<-tolower(str_replace(str_trim(as.list(str_split(df_temp[i,"Grapes" ],",")[[1]]),
                                      side = c("both")),"’", "'"))
  df_loop<-data.frame(Sort=lista)
  new_bottle_cl <-df_temp[i,"Volumen" ]/length(lista)
  price_temp<-df_temp[i,"Preis per 75cl" ]/length(lista)
  df_loop$Price<-price_temp/new_bottle_cl*75
  print(df_loop)
  df_weinsorte<-rbind(df_weinsorte,df_loop)
} 

df_wine%>% 
  count(Region, sort = T)  

df_weinsorte%>% 
  count(Sort, sort = T) 
