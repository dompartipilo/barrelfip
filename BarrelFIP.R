library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

bref = "https://www.baseball-reference.com/leagues/MLB/2019-standard-pitching.shtml#players_standard_pitching::none"
bref = read_html(bref)
table = bref %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
  html_text() %>%    # extract comment text
  paste(collapse = '') %>%    # collapse to a single string
  read_html() %>%    # reparse to HTML
  html_node('table#players_standard_pitching') %>%    # select the desired table
  html_table() %>%    # parse table
  .[colSums(is.na(.)) < nrow(.)]  

table$IP = gsub("\\.1", ".33", table$IP)
table$IP = gsub("\\.2", ".66", table$IP)

breftable = table %>%
  group_by(Name) %>%
  summarise(bf = sum(as.numeric(BF)),
            kpct = sum(as.numeric(SO))*100/sum(as.numeric(BF)),
            bbpct = sum(as.numeric(BB))*100/sum(as.numeric(BF)),
            hbppct = sum(as.numeric(HBP))*100/sum(as.numeric(BF)),
            era = round(sum(as.numeric(ER))*9/sum(as.numeric(IP)), 2)) %>%
  filter(bf > 100)

breftable$firstname = sapply(strsplit(breftable$Name, "\\s+"), function(x) x[1])
breftable$lastname = sapply(strsplit(breftable$Name, "\\s+"), function(x) x[length(x)])
breftable$lastname = str_replace(breftable$lastname, '\\*', '')

breftable = breftable %>%
  select(-c(Name))


url <- "https://baseballsavant.mlb.com/statcast_leaderboard?player_type=pitcher"
brl = read_html(url)
brl <- brl %>%
  html_nodes("script")

info = as.character(brl[10])
obs = as.vector(strsplit(info, "name"))





namecands = c()

for (i in 2:length(obs[[1]])){
  namecands = c(namecands, substr(obs[[1]][i], 4, 30))
}
names = c()
for (j in 1:length(namecands)){
  names = c(names, sub('",.*', '', namecands[j]))
}

lastname = sub(',.*', '', names)
firstname = sub('.*, ', '', names)






brlpa = c()

for (k in 1:length(obs[[1]])){
  if (grepl("brl_pa", obs[[1]][k]) == TRUE){
    brlpos = gregexpr("brl_pa", obs[[1]][k])[[1]][1]
    brlpastring = substr(obs[[1]][k], brlpos + 9, brlpos + 19)
    brlpa = c(brlpa, sub('",.*', '', brlpastring))
  }
  }
  
  
barrels = data.frame(
  lastname = lastname,
  firstname = firstname,
  brl_pa = as.numeric(brlpa)
)  
  


data = merge(breftable, barrels)
data$bbhbppct = data$bbpct + data$hbppct 


model = lm(data = data, formula = era ~ kpct + bbhbppct + brl_pa)

data$BarrelFIP = round(predict(model), 2)
View(data %>%
       filter(bf > 400) %>% 
       select(firstname, lastname, era, BarrelFIP) %>%
       mutate(diff = era - BarrelFIP))
