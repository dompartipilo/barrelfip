library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

#read table of pitchers' statistics from baseball-reference
bref = "https://www.baseball-reference.com/leagues/MLB/2019-standard-pitching.shtml#players_standard_pitching::none"
bref = read_html(bref)
table = bref %>% html_nodes(xpath = '//comment()') %>%
  html_text() %>%
  paste(collapse = '') %>%
  read_html() %>%
  html_node('table#players_standard_pitching') %>%
  html_table() %>%
  .[colSums(is.na(.)) < nrow(.)]  

table <- data.frame(table[table$Name != "Name",])
table <- as.data.frame(table)

table[-c(2, 4, 5)] <- sapply(table[-c(2, 4, 5)], as.numeric)

#convert partial innings to correct decimals
table$IP = gsub("\\.1", ".33", table$IP)
table$IP = gsub("\\.2", ".66", table$IP)

#summarize using relevant metrics
breftable = table %>%
  group_by(Name) %>%
  summarise(bf = sum(as.numeric(BF)),
            kpct = sum(as.numeric(SO))*100/sum(as.numeric(BF)),
            bbpct = sum(as.numeric(BB))*100/sum(as.numeric(BF)),
            hbppct = sum(as.numeric(HBP))*100/sum(as.numeric(BF)),
            era = round(sum(as.numeric(ER))*9/sum(as.numeric(IP)), 2)) %>%
  filter(bf > 100)

#separate lastname and firstname for merging/joining later
breftable$firstname = sapply(strsplit(breftable$Name, "\\s+"), function(x) x[1])
breftable$lastname = sapply(strsplit(breftable$Name, "\\s+"), function(x) x[length(x)])
breftable$lastname = str_replace(breftable$lastname, '\\*', '')

breftable = breftable %>%
  select(-c(Name))

#collect statcast data
url <- "https://baseballsavant.mlb.com/leaderboard/statcast?type=pitcher&year=2019&position=&team=&min=100"
brl = read_html(url)
brl <- brl %>%
  html_nodes("script")

info = as.character(brl[10])
obs = as.vector(strsplit(info, "name"))

#select strings that contain names
namecands = c()

for (i in 2:length(obs[[1]])){
  namecands = c(namecands, substr(obs[[1]][i], 4, 30))
}
names = c()
for (j in 1:length(namecands)){
  names = c(names, sub('",.*', '', namecands[j]))
}

names <- names[(1:(length(names)/3))*3-2]
lastname = sub(',.*', '', names)
firstname = sub('.*, ', '', names)


#collect barrel rate info
brlpa = c()

for (k in 1:length(obs[[1]])){
  if (grepl("barrels_per_pa", obs[[1]][k]) == TRUE){
    brlpos = gregexpr("barrels_per_pa", obs[[1]][k])[[1]][1]
    brlpastring = substr(obs[[1]][k], brlpos + 17, brlpos + 21)
    brlpastring <- gsub("\"", "", brlpastring, fixed = TRUE)
    brlpastring <- gsub(",", "", brlpastring)
    brlpa <- c(brlpa, brlpastring)
  }
  }
  
  
barrels = data.frame(
  lastname = lastname,
  firstname = firstname,
  brl_pa = as.numeric(brlpa)
)  
  

#merge data
data = merge(breftable, barrels)
data$bbhbppct = data$bbpct + data$hbppct 

#build linear regression model
model = lm(data = data, formula = era ~ kpct + bbhbppct + brl_pa)

#predict using model (adjust batters faced in filter function if needed)
data$BarrelFIP = round(predict(model), 2)
brlDF <- data %>%
  filter(bf > 150) %>% 
  select(firstname, lastname, bf, era, BarrelFIP) %>%
  mutate(diff = era - BarrelFIP)

View(brlDF)
                            
