library(tidyverse)
library(rvest) # read HTML
library(purrr) # mapping?

tokyo <- read_html('https://en.wikipedia.org/wiki/Tokyo_Marathon') %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()
berlin <- read_html('https://en.wikipedia.org/wiki/Berlin_Marathon') %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill=T)
london <- read_html('https://en.wikipedia.org/wiki/List_of_winners_of_the_London_Marathon') %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()
boston <- read_html('https://en.wikipedia.org/wiki/List_of_winners_of_the_Boston_Marathon') %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()
nyc <- read_html('https://en.wikipedia.org/wiki/List_of_winners_of_the_New_York_City_Marathon') %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()
chicago <- read_html('https://en.wikipedia.org/wiki/List_of_winners_of_the_Chicago_Marathon') %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill=T)

#**********************************************************************************************/

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 0, n)
}

#**********************************************************************************************/
  
tokyo_men <- tokyo[[2]][,1:4] %>%
  rename(winner="Men's winner",country=Country,"time"="Time (m:s)",year=Year) %>%
  mutate(year = substr(year,0,4),gender="Male")
tokyo_women <- tokyo[[2]][,c(1,5:7)] %>%
  rename(winner="Women's winner",country=Country,"time"="Time (m:s)",year=Year) %>%
  mutate(year = substr(year,0,4),gender="Female")
tokyo_full <- bind_rows(tokyo_men,tokyo_women) %>%
  mutate(marathon="Tokyo")

berlin_men <- berlin[[4]][,2:5] %>%
  rename(winner="Male winner",country=Country,"time"="Time (h:m:s)") %>%
  mutate(year = substrRight(Date,4),gender="Male",winner=str_replace(winner,",","")) %>%
  select(-Date,year,winner,gender,country,time)
berlin_women <- berlin[[4]][,c(2,6:8)] %>%
  rename(winner="Female winner",country=Country,"time"="Time (h:m:s)") %>%
  mutate(year = substrRight(Date,4),gender="Female",winner=str_replace(winner,",","")) %>%
  select(-Date,year,winner,gender,country,time)
berlin_full <- bind_rows(berlin_men,berlin_men) %>%
  mutate(marathon="Berlin")

nyc_men <- nyc[[1]] %>%
  rename(winner="Winner",country=Country,"time"="Time",year=Year) %>%
  mutate(gender="Male") %>%
  select(year,winner,gender,country,time,-Notes)
nyc_women <- nyc[[2]] %>%
  rename(winner="Winner",country=Country,"time"="Time",year=Year) %>%
  mutate(gender="Female") %>%
  select(year,winner,gender,country,time,-Notes)
nyc_full <- bind_rows(nyc_men,nyc_women) %>%
  mutate(marathon="NYC")

london_men <- london[[1]] %>%
  rename(winner=Athlete,country=Nationality,"time"="Time\n(h:m:s)",year=Year) %>%
  mutate(gender="Male",winner=str_replace(winner,",","")) %>%
  select(year,winner,gender,country,time,-Notes)
london_women <- london[[2]] %>%
  rename(winner=Athlete,country=Nationality,"time"="Time\n(h:m:s)",year=Year) %>%
  mutate(gender="Female",winner=str_replace(winner,",","")) %>%
  select(year,winner,gender,country,time,-Notes)
london_full <- bind_rows(london_men,london_women) %>%
  mutate(marathon="London")

boston_men <- boston[[2]] %>%
  rename(winner=Athlete,country="Country/State","time"="Time",year=Year) %>%
  mutate(gender="Male",winner=str_replace(winner,",","")) %>%
  select(year,winner,gender,country,time,-Notes)
boston_women <- boston[[3]] %>%
  rename(winner=Athlete,country="Country/State","time"="Time",year=Year) %>%
  mutate(gender="Female",winner=str_replace(winner,",","")) %>%
  select(year,winner,gender,country,time,-Notes)
boston_full <- bind_rows(boston_men,boston_women) %>%
  mutate(marathon="Boston")

chicago_men <- chicago[[2]][,1:4] %>%
  rename(winner="Male athlete",country=Country,"time"="Time") %>%
  mutate(year = substrRight(Date,4),gender="Male",winner=str_replace(winner,",","")) %>%
  select(-Date,year,winner,gender,country,time)
chicago_women <- chicago[[2]][,c(1,5:7)] %>%
  rename(winner="Female athlete",country=Country,"time"="Time") %>%
  mutate(year = substrRight(Date,4),gender="Female",winner=str_replace(winner,",","")) %>%
  select(-Date,year,winner,gender,country,time)
chicago_full <- bind_rows(chicago_men,chicago_women) %>%
  mutate(marathon="Chicago")

# south korea and 17 18 not doubled?
boston_full$winner[74] <- "Hill, Ron !Ron Hill"
for (i in 1:(nrow(boston_full))) {
  boston_full$winner[i] <- if_else(boston_full$country[i] == "South Korea",boston_full$winner[i],
         if_else(boston_full$year[i] %in% c(2017,2018) & boston_full$gender[i] == "Male",boston_full$winner[i],
                substrLeft(boston_full$winner[i],
                           # even
                           if_else(substrRight(boston_full$winner[i],5)=="(Tie)",
                                   round(nchar(substrLeft(boston_full$winner[i],
                                                          nchar(boston_full$winner[i])-6))/2),
                                   round(nchar(boston_full$winner[i])/2)))))
}

head(london_full)
for (i in 1:(nrow(london_full))) {
  london_full$winner[i] <- substrLeft(london_full$winner[i],
                                      if_else(substrRight(london_full$winner[i],5)=="(Tie)",
                                              round(nchar(substrLeft(london_full$winner[i],
                                                                     nchar(london_full$winner[i])-6))/2),
                                              round(nchar(london_full$winner[i])/2)))
}
    
for (i in 1:(nrow(berlin_full))) {
  berlin_full$winner[i] <- substrLeft(berlin_full$winner[i], round(nchar(berlin_full$winner[i])/2))
}

head(berlin_full)
tail(berlin_full)


for (i in 1:(nrow(chicago_full))) {
  chicago_full$winner[i] <- substrLeft(chicago_full$winner[i], round(nchar(chicago_full$winner[i])/2))
}
head(chicago_full)
tail(chicago_full)


tokyo_full$winner[20] <- "Noriko Higuchi" # footnote added to name?
nyc_full[c(43,89),] <- NULL # hurrican sandy
chicago_full$winner[69] <- "Diță, Constantina"
chicago_full[c(11,52),] <- NULL # Due to sponsorship complications, the event was contested as a half marathon
boston_full[22,] <- NULL # realy team?