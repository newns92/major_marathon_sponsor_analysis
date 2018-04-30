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
  mutate(year = substr(year,0,4),gender="Male",time=substrLeft(time,7))
tokyo_women <- tokyo[[2]][,c(1,5:7)] %>%
  rename(winner="Women's winner",country=Country,"time"="Time (m:s)",year=Year) %>%
  mutate(year = substr(year,0,4),gender="Female",time=substrLeft(time,7))
tokyo_full <- bind_rows(tokyo_men,tokyo_women) %>%
  mutate(marathon="Tokyo", year=as.character(year),time=as.POSIXct(time, format = '%H:%M:%S')) %>%
  select(year,winner,gender,country,time,marathon) %>%
  arrange(desc(year))

berlin_men <- berlin[[4]][,2:5] %>%
  rename(winner="Male winner",country=Country,"time"="Time (h:m:s)") %>%
  mutate(year = substrRight(Date,4),gender="Male",winner=str_replace(winner,",","")
         ,time=substrLeft(time,7)) %>%
  select(-Date,year,winner,gender,country,time)
berlin_women <- berlin[[4]][,c(2,6:8)] %>%
  rename(winner="Female winner",country=Country,"time"="Time (h:m:s)") %>%
  mutate(year = substrRight(Date,4),gender="Female",winner=str_replace(winner,",","")
         ,time=substrLeft(time,7)) %>%
  select(-Date,year,winner,gender,country,time)
berlin_full <- bind_rows(berlin_men,berlin_women) %>%
  mutate(marathon="Berlin", year=as.character(year),time=as.POSIXct(time, format = '%H:%M:%S')) %>%
  select(year,winner,gender,country,time,marathon) %>%
  arrange(desc(year))

nyc_men <- nyc[[1]] %>%
  rename(winner="Winner",country=Country,"time"="Time",year=Year) %>%
  mutate(gender="Male",time=substrLeft(time,7)) %>%
  select(year,winner,gender,country,time,-Notes)
nyc_women <- nyc[[2]] %>%
  rename(winner="Winner",country=Country,"time"="Time",year=Year) %>%
  mutate(gender="Female",time=substrLeft(time,7)) %>%
  select(year,winner,gender,country,time,-Notes)
nyc_full <- bind_rows(nyc_men,nyc_women) %>%
  mutate(marathon="NYC", year=as.character(year),time=as.POSIXct(time, format = '%H:%M:%S')) %>%
  select(year,winner,gender,country,time,marathon) %>%
  arrange(desc(year))

london_men <- london[[1]] %>%
  rename(winner=Athlete,country=Nationality,"time"="Time\n(h:m:s)",year=Year) %>%
  mutate(gender="Male",winner=str_replace(winner,",",""),time=substrLeft(time,7)) %>%
  select(year,winner,gender,country,time,-Notes)
london_women <- london[[2]] %>%
  rename(winner=Athlete,country=Nationality,"time"="Time\n(h:m:s)",year=Year) %>%
  mutate(gender="Female",winner=str_replace(winner,",",""),time=substrLeft(time,7)) %>%
  select(year,winner,gender,country,time,-Notes)
london_full <- bind_rows(london_men,london_women) %>%
  mutate(marathon="London", year=as.character(year),time=as.POSIXct(time, format = '%H:%M:%S')) %>%
  select(year,winner,gender,country,time,marathon) %>%
  arrange(desc(year))

boston_men <- boston[[2]] %>%
  rename(winner=Athlete,country="Country/State","time"="Time",year=Year) %>%
  mutate(gender="Male",winner=str_replace(winner,",",""),time=substrLeft(time,7)) %>%
  select(year,winner,gender,country,time,-Notes)
boston_women <- boston[[3]] %>%
  rename(winner=Athlete,country="Country/State","time"="Time",year=Year) %>%
  mutate(gender="Female",winner=str_replace(winner,",",""),time=substrLeft(time,7)) %>%
  select(year,winner,gender,country,time,-Notes)
boston_full <- bind_rows(boston_men,boston_women) %>%
  mutate(marathon="Boston", year=as.character(year),time=as.POSIXct(time, format = '%H:%M:%S')) %>%
  select(year,winner,gender,country,time,marathon) %>%
  arrange(desc(year))

chicago_men <- chicago[[2]][,1:4] %>%
  rename(winner="Male athlete",country=Country,"time"="Time") %>%
  mutate(year = substrRight(Date,4),gender="Male",winner=str_replace(winner,",","")
         ,time=substrLeft(time,7)) %>%
  select(-Date,year,winner,gender,country,time)
chicago_women <- chicago[[2]][,c(1,5:7)] %>%
  rename(winner="Female athlete",country=Country,"time"="Time") %>%
  mutate(year = substrRight(Date,4),gender="Female",winner=str_replace(winner,",","")
         ,time=substrLeft(time,7)) %>%
  select(-Date,year,winner,gender,country,time)
chicago_full <- bind_rows(chicago_men,chicago_women) %>%
  mutate(marathon="Chicago", year=as.character(year),time=as.POSIXct(time, format = '%H:%M:%S')) %>%
  select(year,winner,gender,country,time,marathon) %>%
  arrange(desc(year))

#**********************************************************************************************/

rm(tokyo,tokyo_men,tokyo_women,chicago,chicago_men,chicago_women,boston,boston_men,boston_women,
   berlin,berlin_men,berlin_women,london,london_men,london_women,nyc,nyc_men,nyc_women)

#**********************************************************************************************/

# south korea and 2017 2018 males not doubled?
for (i in 1:(nrow(boston_full))) {
  # south korea athletes + 2017/2018 mens champs not doubled
  boston_full$winner[i] <- if_else(boston_full$country[i] == "South Korea",boston_full$winner[i],
         if_else(boston_full$year[i] %in% c(2017,2018) & boston_full$gender[i] == "Male",boston_full$winner[i],
                substrRight(boston_full$winner[i],
                           # check for ties, then cut in half correctly based on even/odd # of characters
                           if_else(substrRight(boston_full$winner[i],5)=="(Tie)",
                                   round(nchar(substrLeft(boston_full$winner[i],
                                                          nchar(boston_full$winner[i])-6))/2),
                                   round(nchar(boston_full$winner[i])/2)))))
}
#head(boston_full)

for (i in 1:(nrow(london_full))) {
  london_full$winner[i] <- substrRight(
                                # check for ties, then cut in half correctly based on even/odd # of characters
                                if_else(substrRight(london_full$winner[i],5)=="(Tie)",
                                        substrLeft(london_full$winner[i],nchar(london_full$winner[i])-6),
                                        london_full$winner[i]),
                                              nchar(if_else(substrRight(london_full$winner[i],5)=="(Tie)",
                                                  substrLeft(london_full$winner[i],nchar(london_full$winner[i])-6),
                                              london_full$winner[i]))/2)
}
#head(london_full)

### rest are even b/c doubled (any int * even int = even int)
for (i in 1:(nrow(berlin_full))) {
  berlin_full$winner[i] <- substrRight(berlin_full$winner[i], round(nchar(berlin_full$winner[i])/2))
}
#head(berlin_full)

for (i in 1:(nrow(chicago_full))) {
  chicago_full$winner[i] <- substrRight(chicago_full$winner[i], round(nchar(chicago_full$winner[i])/2))
}
#head(chicago_full)

#**********************************************************************************************/
# special ad-hoc final cleaning

tokyo_full$winner[16] <- "Noriko Higuchi" # footnote added to name?

# bad characters
chicago_full$winner[28] <- "Dita, Constantina" 
# Due to sponsorship complications, the event was contested as a half marathon
chicago_full[81:82,] <- mutate(chicago_full[81:82,], gender=NA, year=1987, winner=NA,country=NA,time=NA)
chicago_full <- chicago_full %>% arrange(desc(year))

# hurricane sandy
nyc_full[11:12,] <- mutate(nyc_full[11:12,], gender=NA,winner=NA,country=NA,time=NA) 

# random edits = nickname for Caffery, non-consistent middle initials (Ryan, Barron, Gibb = ???, split/not split last name (deBruyn)
# inconsistent names (Gosta), exclamation point (Hill)?
boston_full$winner[c(172,173,161,141,124,103,105,107,98,83)] <- c("Jack Caffery","Jack Caffery","Michael J. Ryan","Paul de Bruyn",
                                                             "Gösta Leandersson","Bobbi Gibb","Bobbi Gibb","Bobbi Gibb","Ron Hill",
                                                             "Gayle Barron")
# relay team?
boston_full[155,] <- mutate(boston_full[155,], gender=NA, winner=NA,country=NA,time=NA)
# fix doubled country's + the US, which includes state for some reason
boston_full <- boston_full %>%
  mutate(country = if_else(str_detect(country, "United States"),"United States",
                           if_else(str_detect(country, "Canada"),"Canada",
                                   if_else(str_detect(country, "Germany"),"Germany",
                                           if_else(str_detect(country, "Greece"),"Greece",country)))))

#**********************************************************************************************/
#final binding
majorMarathons <- bind_rows(tokyo_full,berlin_full,boston_full,nyc_full,london_full,chicago_full) %>%
  mutate(year=as.integer(year))
glimpse(majorMarathons) 

londonFemale18 <- c(as.integer(2018),"Vivian Cheruiyot","Female","Kenya",(paste(Sys.Date()-1,"02:18:31")),"London")

majorMarathons <- majorMarathons %>% 
                    rbind(londonFemale18) %>%
                    mutate(year=as.integer(year))



saveRDS(majorMarathons, file="majorMarathons.Rda")
write.csv(majorMarathons, file = "majorMarathons.csv",row.names=F)

# "Gösta Leandersson" # why cutoff?
#16 = Michael J. Ryan # middle initial
#36 = Paul de Bruyn # split last name
#4,5, <- Jack Caffery # nicname
#74 <- Ron Hill # ! = ???
#124,125 <- Bobbi Gibb # where did middle a come from?
#135 <- S.Gayle Barron # middle initial?