library(dplyr)
library(rvest)
library(stringr)
library(lubridate)
setwd("C:/devl/phantomjs")
options(stringsAsFactors = F)

# ref table
yrs <- 2009:2018
dts <- c("6/21/2009", "06/20/2010", "06/19/2011", "06/17/2012", "06/16/2013", "06/15/2014",
          "06/21/2015", "06/19/2016", "06/18/2017", "06/17/2018")
dt.ref <- data.frame(yrs = yrs, dts = mdy(dts))

# build trim function to use later
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# function to capture player name
clean.up <- function(x){
  n.pos <- gregexpr(pattern = '\n', x)
  pl.nm <- substr(x, n.pos[[1]][1]+2, n.pos[[1]][2]-1)
  pl.nm <- gsub("a\\)", "", pl.nm)
  pl.nm <- gsub("\\(", "", pl.nm)
  pl.nm <- trim(pl.nm)
  print(pl.nm)
  return(pl.nm)
}

# function to pull and clean up all of the html
get_data <- function(yr){
    pl <- read_html(paste0("USOpen_stats_", as.character(yr), ".html")) %>% 
              html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 's-cell--player', ' ' ))]") %>%
              html_text()
    
    players <- unlist(lapply(X = as.list(pl[2:length(pl)]), FUN = clean.up))
    
    r4 <- read_html(paste0("USOpen_stats_", as.character(yr), ".html")) %>% 
      html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 's-cell--small-hidden', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]") %>%
      html_text()
    
    round4 <- unlist(lapply(X = as.list(r4[2:length(r4)]), FUN = clean.up))
    
    round4 <- as.numeric(round4)
    
    out <- data.frame(year = yr, player = players, Round4 = round4)
    return(out)
}

# sample years
years <- 2009:2018

# put the data together
ds <- lapply(X = as.list(years), FUN = get_data)
ds <- do.call("rbind", ds)
ds$player2 <- tolower(ds$player)


# dude.  Scrape the PGA player profiles for wife and kids
pga <- read.csv("PGA_Players.csv")
pga.lst <- as.list(pga$link)

get.info <- function(pl){
  pl <- gsub("\\\\", "/", pl)
  temp <- read_html(pl) %>%
            html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 's-top-text', ' ' ))]") %>%
            html_text()
  temp <- paste(temp, collapse = "")
  n.start <- regexpr(pattern = '\\(.*?\\/.*?\\/.*?\\)', text = temp)
  n.start2 <- gregexpr(pattern = '\\(.*?\\/.*?\\/.*?\\)', text = temp)
  oldest.bd <- mdy("01/01/1901")
  parent <- 0
  kids <- 0
  dot.loc <- gregexpr(pattern = '\\.', pl)
  dot.cnt <- length(dot.loc[[1]])
  player <- substr(pl, dot.loc[[1]][dot.cnt - 1]+1, dot.loc[[1]][dot.cnt]-1)
  player <- gsub("-", " ", player)
  if(n.start[[1]][1] != -1){
    oldest.bd <- mdy(substr(temp, n.start[[1]][1]+1, n.start[[1]][1] + attr(n.start,"match.length")))
    parent <- 1
    kids <- length(n.start2[[1]])
  }
  df <- data.frame(player = player, parent = parent, oldest.bd = oldest.bd, kids = kids)
  print(player)
  pl.kids <<- rbind(pl.kids, df)
}

pl.kids <- data.frame()
pl <- lapply(X = pga.lst, FUN = get.info)


# fix some names
pl.kids$player2 <- pl.kids$player
pl.kids[pl.kids$player == "alex noren", "player2"] <- "alexander noren"
pl.kids[pl.kids$player == "c t pan", "player2"] <- "c.t. pan"
pl.kids[pl.kids$player == "charles howell iii", "player2"] <- "charles howell"
pl.kids[pl.kids$player == "j b holmes", "player2"] <- "j.b. holmes"
pl.kids[pl.kids$player == "matthew fitzpatrick", "player2"] <- "matt fitzpatrick"


#merge
ds <- merge(ds, dt.ref, by.x = "year", by.y = "yrs", all.x = T)
ds2 <- merge(ds, pl.kids, by.x = "player2", "player2", all.x = T)
#unique(ds2[is.na(ds2$parent),"player2"])

# filter it down
ds2 <- filter(ds2, !is.na(parent))
ds2 <- filter(ds2, !is.na(Round4))

# clean up
pl.kids[pl.kids$player == "branden grace", "oldest.bd"] <- mdy("04/23/2018")
pl.kids[pl.kids$player == "charl schwartzel", "oldest.bd"] <- mdy("03/16/2014")
pl.kids[pl.kids$player == "kevin na", "oldest.bd"] <- mdy("08/29/2016")
pl.kids[pl.kids$player == "sergio garcia", "oldest.bd"] <- mdy("03/14/2018")
pl.kids[pl.kids$player == "tommy fleetwood", "oldest.bd"] <- mdy("09/28/2017")
pl.kids[pl.kids$player == "vijay singh", "oldest.bd"] <- mdy("06/19/1990")

ds2$parent2 <- 0
ds2[ds2$oldest.bd != mdy("01/01/1901") & ds2$dts > ds2$oldest.bd, "parent2"] <- 1

save(pl.kids, file = "PGA_Kids.Rdata")
save(ds, file = "ds.Rdata")
save(ds2, file = "ds2.Rdata")


