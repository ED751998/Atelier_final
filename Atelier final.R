##### POKEMON
library(tidyverse)
library(rvest)
# Partie 1 ----

url <- "https://scrapeme.live/product-category/pokemon/"

table_poke <- data.frame(nom=c(),prix=c(),href=c(),poids=c())

noms <- c()
prix <- c()
href<-c()
for (i in seq(1,10,1)) {
  
  lien <- paste0(url,"page/",i,"/")
  
  n <- read_html(lien) %>% 
    html_nodes("a.woocommerce-LoopProduct-link.woocommerce-loop-product__link > h2") %>% html_text()
  
  p <- read_html(lien) %>% 
    html_nodes("span.woocommerce-Price-amount.amount") %>% 
    html_text() %>% 
    substr(2,10000000) %>% 
    as.numeric()
  
  h <- read_html(lien) %>% 
    html_nodes("a.woocommerce-LoopProduct-link") %>% html_attr("href")
  
  noms <- c(noms,n)
  prix <- c(prix,p)
  href <- c(href,h)
}



# poids
table_poke <- data.frame(noms,prix,href)

table_poke_2 <- table_poke %>% 
  rowwise() %>% 
  mutate(poids = ((read_html(href) %>% 
                    html_nodes("td.product_weight") %>% 
                    html_text() %>% 
                    str_split_fixed(pattern = "kg",n=2))[1]) %>% as.numeric())

## Partie 2 ----
library(jsonlite)

url <- "https://pokeapi.co/api/v2/pokemon/"

hp <- c()
attack <- c()
defense <- c()
speed <- c()

for (i in seq(1,nrow(table_poke_2),1)) {
  
  # stats
  stats <- fromJSON(paste0(url,table_poke_2$noms[i] %>% str_to_lower()))[["stats"]]
  
  h <- stats$base_stat[1]
  a <- stats$base_stat[2]
  d <- stats$base_stat[3]
  s <- stats$base_stat[6]
  
  hp <- c(hp,h)
  attack <- c(attack,a)
  defense <- c(defense,d)
  speed <- c(speed,s)
  
}

table_poke_2 <- table_poke_2 %>% 
  cbind(hp=hp,
    attack=attack,
    defense=defense,
    speed=speed) %>% 
  rowwise() %>% 
  mutate(abilities= list((fromJSON(paste0(url,noms %>% str_to_lower())))[["abilities"]][[1]][,1]),
         types= list((fromJSON(paste0(url,noms %>% str_to_lower())))[["types"]][[2]][,1])) 

stream_out(table_poke_2,file("table_poke.json"))


## Partie 3 ----
library(mongolite)
m <- mongo("pokemon")
if(m$count() > 0) m$drop()
m$count()

poke_path <- file.path("table_poke.json")
m$import(file(poke_path))
m$count()


m$aggregate('[
{
"$group": {
"_id": "$types",
"prix": {"$avg": "$prix"},
"poids": {"$avg": "$poids"},
"hp": {"$avg": "$hp"},
"attack": {"$avg": "$attack"},
"speed": {"$avg": "$speed"},
"defense": {"$avg": "$defense"}
}
}
]') 

library(ggplot2)

# Prix des cartes en fonction de leur type
T <- m$aggregate('[
{
"$unwind": "$types"
},
{
"$group": {
"_id": "$types",
"prix": {"$avg": "$prix"},
"poids": {"$avg": "$poids"},
"hp": {"$avg": "$hp"},
"attack": {"$avg": "$attack"},
"speed": {"$avg": "$speed"},
"defense": {"$avg": "$defense"}
}
}
]')  %>% as_tibble 


T %>% mutate(Type=`_id` %>% as.character()) %>% 
  ggplot(aes(x=poids,y=prix,color=Type)) + 
  geom_point() +
  theme_bw() +
  theme(legend.position = "bottom")

T %>% 
  pivot_longer(cols = 4:7,names_to = "type",values_to = "stat") %>% 
  ggplot(aes(x=reorder(`_id`,stat),y=stat,fill=reorder(`_id`,stat)))+
  geom_col(position = "dodge")+
  facet_wrap(facets = "type")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  xlab("") + ylab("") +
  scale_fill_viridis_d(option = "H")


T <- m$aggregate('[
{
"$group": {
"_id": "$types",
"prix": {"$avg": "$prix"},
"poids": {"$avg": "$poids"},
"hp": {"$avg": "$hp"},
"attack": {"$avg": "$attack"},
"speed": {"$avg": "$speed"},
"defense": {"$avg": "$defense"}
}
}
]')  %>% as_tibble  %>% rowwise() %>% 
  mutate(nb_type=length(`_id`))


T %>% 
  pivot_longer(cols = 4:7,names_to = "type",values_to = "stat") %>% 
  ggplot(aes(x=reorder(nb_type,stat),y=stat,fill=reorder(type,stat)))+
  geom_col(position = "dodge",width = 0.5)+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  xlab("Nombre de type") + ylab("") +
  scale_fill_viridis_d(option = "H")

T <- m$aggregate('[
{
"$group": {
"_id": "$abilities",
"prix": {"$avg": "$prix"},
"poids": {"$avg": "$poids"},
"hp": {"$avg": "$hp"},
"attack": {"$avg": "$attack"},
"speed": {"$avg": "$speed"},
"defense": {"$avg": "$defense"}
}
}
]')  %>% as_tibble  %>% rowwise() %>% 
  mutate(nb_abilities=length(`_id`))


T %>% 
  pivot_longer(cols = 4:7,names_to = "type",values_to = "stat") %>% 
  ggplot(aes(x=nb_abilities %>% as.character(),y=stat,fill=reorder(type,stat)))+
  geom_col(position = "dodge",width = 0.5)+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank())+
  xlab("Nombre de facultés spéciales") + ylab("") +
  scale_fill_viridis_d(option = "H")


T <- m$aggregate('[
{
"$unwind": "$abilities"
},
{
"$group": {
"_id": "$abilities",
"prix": {"$avg": "$prix"},
"poids": {"$avg": "$poids"},
"hp": {"$avg": "$hp"},
"attack": {"$avg": "$attack"},
"speed": {"$avg": "$speed"},
"defense": {"$avg": "$defense"}
}
}
]')  %>% as_tibble 



