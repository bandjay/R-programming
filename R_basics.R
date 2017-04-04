# R - programming Environmnet
# There is really only one rule about vectors in R, which is A vector can only contain objects of the same class.
# But of course, like any good rule, there is an exception, which is a list,
library(tidyr)
library(dplyr)
data(VADeaths)
VADeaths=VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) %>%
  gather(key, death_rate, -age) %>%
  separate(key, c("urban", "gender"), sep = " ") %>%
  mutate(age = factor(age), urban = factor(urban), gender = factor(gender)) 

VADeaths %>% group_by(gender) %>% summarize(cnt=n(),avg=sum(death_rate)/n(),max_rate=max(death_rate))

VADeaths %>% filter (death_rate >=10) %>% select (age,death_rate)

## Select if for columns and filter is for rows

library(knitr)
library(faraway)
data("worldcup")
worldcup %>% group_by(Team,Position) %>% summarize(n_t=n(),sum(Passes))%>% kable()
#worldcup %>% group_by(Position) %>% spread(Position,Passes) %>% kable()

library(readr)
library(lubridate)
library(ggplot2)
full_data=read_csv("C:/Users/jaycb/Desktop/NYtimes/full_data.csv")
full_data$date<-ymd(full_data$date)
head(year(full_data$date))
full_data<-full_data %>% mutate(year=as.numeric(year(date)),month=as.numeric(month(date)))
full_data %>% group_by(year) %>% summarize(count=n())%>% as.data.frame %>% kable()


### regular expressions
# Metacharacter Meaning
# . Any Character
# \w A Word
# \W Not a Word
# \d A Digit
# \D Not a Digit
# \s Whitespace
# \S Not Whitespace
# [xyz] A Set of Characters
# [^xyz] Negation of Set
# [a-z] A Range of Characters
# ^ Beginning of String
# $ End of String
# \n Newline
# + One or More of Previous
# * Zero or More of Previous
# ? Zero or One of Previous
# Either the Previous or the Following
# {5} Exactly 5 of Previous
# {2, 5} Between 2 and 5 or Previous
# {2, } More than 2 of Previous

country=names(table(worldcup$Team))

#country names with space
gi=grep('.( ).',country)
gi=grep('[ ]',country)
for (i in 1:length(gi)){
  print(country[gi[i]])
}

# country with Z begining
gi=grep('^[Zz]',country)
for (i in 1:length(gi)){
  print(country[gi[i]])
}

#substitution of USA 
gsub("USA","United States",country)

# replace all special characters with $
str="stfgglg$l32$kkG&5%%43(8329&_+9941&(*)(^^^8)"
gsub("[A-Za-z0-9]","",str)

library(stringr)
str2="I opended 2 account on 21-05-2015 with $500 each"
list_str=unlist(str_split(str2," "))


# getting the date out of a string
for (i in 1:length(list_str)){
if (grepl('[0-9]',list_str[i]) && grepl('[//-]',list_str[i]) ){
  print (list_str[i])
}
}

## R physical memory

library(pryr)
mem_used()
ls()
sapply(ls(),function(x) object_size(x)) %>% sort() %>% tail()

# apply functions
# sapply for apply for function on a list and return a vector use sapply rather than lapply
#tapply - For when you want to apply a function to subsets of a vector and the subsets are defined by some other vector, usually a factor
x <- 1:20
y <- factor(rep(letters[1:5], each = 4))
tapply(x, y, sum) 


### Faster R packages
library(data.table)
dt<-fread("C:/Users/jaycb/Desktop/NYtimes/full_data.csv",select = c("date"),nrows = 5)

