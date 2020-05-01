# -------------------------------------------------- 
# Script Name : Web scrapping and data cleaning.R
# Purpose: Scrapping of websites for data;then clean and output csv files
# Potential bugs on this script:
#    1. If the source websites makes changes on html layout, then script will fail											
# --------------------------------------------------


#' Load the libraries
#'
library("rvest")
library(tabulizer)
library(dplyr)
library(miniUI)
library(tidyverse)
library(pdftools)

library(xlsx)
library(condformat)
library("readxl")
library(tidyr)
library(tidyverse)
library(DT)
library(reshape2)
library("ggplot2")

#' Change directory to point to data folder
#' 
mypath<- "C:/Users/user/Documents/UN volunteer work/data/" 

#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases for Nepal
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

site <- "https://www.edcd.gov.np/ewars"
#wrap read_html into try function and suppress error. This ensures that if website fails #to respond on first request, the second  and third request is send

#set webpage to null to help in the loop
webpage <- NULL
trial <- 1
while( is.null(webpage) && trial <= 3 ) {
  trial <- trial + 1
  try(
    webpage <- read_html(site),silent = TRUE
  )
}

webpage <- webpage %>% html_nodes("span") %>% html_nodes("a") %>% html_attr("href")

url <- pdf_text(webpage[1])
date_slipt <- url[1] %>% str_split("\r", simplify = TRUE) %>% str_split("\n", simplify = TRUE)
date <- trimws(date_slipt [4,2])
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][2]
data_slipt <- url[4] %>% str_split("\r", simplify = TRUE) %>% grep('Dengue|dengue', ., value = T) 
data <- gsub("[\n]", "", data_slipt)
nepal_cases <- gsub( " .*$", "", data[2] )
word_to_number <- function(x){
  # Remove punctuation and 'and'
  x <- tolower(gsub("([[:punct:]]| and )", " ", x))
  # separate into distinct words
  x <- trimws(unlist(strsplit(x, "\\s+")))
  
  # verify that all words are found in the reference vectors.
  if (!(all(x %in% names(c(word_to_number_reference, magnitude_reference)))))
    stop("Text found that is not compatible with conversion. Check your spelling?")
  
  # translate words to the numeric reference
  num <- c(word_to_number_reference, magnitude_reference)[x]
  
  # Identify positions with a magnitude indicator
  magnitude_at <- 
    which(names(num) %in% 
            c("quadrillion", "trillion", "billion",
              "million", "thousand"))
  
  # Create an indexing vector for each magnitude class of the number
  magnitude_index <- 
    cut(seq_along(num), 
        breaks = unique(c(0, magnitude_at, length(num))))
  
  # Make a list with each magnitude
  num_component <- 
    lapply(unique(magnitude_index),
           FUN = function(i) num[magnitude_index == i])
  
  # Transate each component
  num_component <- 
    vapply(num_component,
           FUN = word_to_number_translate_hundred,
           FUN.VALUE = numeric(1))
  
  # Add the components together
  num <- sum(num_component)
  
  if (is.na(num))
    warning(sprintf("Unable to translate %s", x))
  
  num
}

word_to_number_translate_hundred <- function(n){
  # set a magnitude multiplier for thousands and greater
  if (tail(names(n), 1) %in% names(magnitude_reference)){
    magnitude <- tail(n, 1)
    n <- head(n, -1)
  } else {
    magnitude <- 1
  }
  
  # if hundred appears anywhere but the second position or of the
  # value preceding hundred is greater than 9, handle with care
  # (for instance, 1200)
  if ( ("hundred" %in% names(n) && which(names(n) == "hundred") != 2) ||
       ("hundred" %in% names(n) && n[1] > 1) )
  {
    which_hundred <- which(names(n) == "hundred")
    (sum(n[seq_along(n) < which_hundred]) * 100 + 
        sum(n[seq_along(n) > which_hundred])) * magnitude
  } else {
    op <- rep("+", length(n) - 1)
    op[names(n)[-1] == "hundred"] <- "*"
    op <- c(op, "")
    eval(parse(text = paste(paste(n, op), collapse = " "))) * magnitude
  }
}



word_to_number_reference <- 
  c("zero" = 0,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fourteen" = 14,
    "fifteen" = 15,
    "sixteen" = 16,
    "seventeen" = 17,
    "eighteen" = 18,
    "nineteen" = 19,
    "twenty" = 20,
    "thirty" = 30,
    "forty" = 40,
    "fifty" = 50,
    "sixty" = 60,
    "seventy" = 70,
    "eighty" = 80,
    "ninety" = 90,
    "hundred" = 100)

magnitude_reference <- 
  c("thousand" = 1000,
    "million" =  1e6,
    "billion" =  1e9,
    "trillion" = 1e12,
    "quadrillion" = 1e15)

total <- ifelse(nepal_cases == "No", 0, word_to_number(nepal_cases))

#'Read in previous dataset to be updated

my_data <- read_excel(paste0(mypath,"Nepal_dengue.xlsx"))
my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,5], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename("2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

#save data to be used in rmardown
saveRDS(my_data, file =paste0(mypath,"save_data_nepal_dengue.Rdata"))

#store date of last update into the dataset
my_data[13, '2020'] <- date
data_date_nepal <- my_data
#write external file after adding new cases to it
write.xlsx(data_date_nepal, "C:/Users/user/Documents/UN volunteer work/data/Nepal_dengue.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)


#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases for Sirlanka
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 


site <- "http://www.epid.gov.lk/web/index.php?option=com_casesanddeaths&Itemid=448&lang=en"
Date <- read_html(site) %>% html_nodes("form") %>% html_nodes("div")  %>% html_nodes("span")  %>% html_text() %>% grep('Date', ., value = T)
paste("Last update: ", Date)
data <- site %>% read_html() %>% html_nodes(xpath='//*[@id="rt-mainbody"]/form/table[2]') %>% html_table()

my_data <- read_excel(paste0(mypath,"sri_lanka_dengue.xlsx"))

my_data <- my_data[-c(13:19), ]
my_data_before <- sum(my_data$`2020`)
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- data[[1]]$X2[1]
my_data[2, '2020'] <- data[[1]]$X2[2]
my_data[3, '2020'] <- data[[1]]$X2[3]
my_data[4, '2020'] <- data[[1]]$X2[4]
my_data[5, '2020'] <- data[[1]]$X2[5]
my_data[6, '2020'] <- data[[1]]$X2[6]
my_data[7, '2020'] <- data[[1]]$X2[7]
my_data[8, '2020'] <- data[[1]]$X2[8]
my_data[9, '2020'] <- data[[1]]$X2[9]
my_data[10, '2020'] <- data[[1]]$X2[10]
my_data[11, '2020'] <- data[[1]]$X2[11]
my_data[12, '2020'] <- data[[1]]$X2[12]

save_data_srilanka <- as.data.frame(my_data)
#' Save data to be used in ggplot
saveRDS(save_data_srilanka, file =paste0(mypath,"sri_lanka_dengue_ggplot.Rdata"))

#write data to excel for the subsequent update when the site updates data
write.xlsx(save_data_srilanka, paste0(mypath,"sri_lanka_dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data_now <- sum(save_data_srilanka$`2020`)
new_cases <- my_data_now - my_data_before
paste("New Cases: ", new_cases)


my_data$ten_mean <- ifelse(my_data$`2010` > means$Above, 2,
                           ifelse(my_data$`2010` < means$Below, 0,
                                  1))

my_data$eleven_mean <- ifelse(my_data$`2011` > means$Above, 2,
                              ifelse(my_data$`2011` < means$Below, 0,
                                     1))

my_data$twelve_mean <- ifelse(my_data$`2012` > means$Above, 2,
                              ifelse(my_data$`2012` < means$Below, 0,
                                     1))

my_data$thirdten_mean <- ifelse(my_data$`2013` > means$Above, 2,
                                ifelse(my_data$`2013` < means$Below, 0,
                                       1))
my_data$fourthten_mean <- ifelse(my_data$`2014` > means$Above, 2,
                                 ifelse(my_data$`2014` < means$Below, 0,
                                        1))
my_data$fiveten_mean <- ifelse(my_data$`2015` > means$Above, 2,
                               ifelse(my_data$`2015` < means$Below, 0,
                                      1))
my_data$sixten_mean <- ifelse(my_data$`2016` > means$Above, 2,
                              ifelse(my_data$`2016` < means$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > means$Above, 2,
                                ifelse(my_data$`2017` < means$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))

saveRDS(my_data, file =paste0(mypath,"sri_lanka_dengue.Rdata"))

#' ----------------------------------------------------------------------------------------------
#' scrapping dengue data from fiji
#' 
#' 
#' ----------------------------------------------------------------------------------------------
site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
date1 <- pdf_text(web[11])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[,5])
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][3]
df_results <- extract_tables(web[11], pages = 3)
data <- as.data.frame(df_results)
data <- data[-c(1,2), ]
data <- data[!sapply(data, function(x) all(x == ""))]
final <- data[ , c(1, length( names( data ) ) ) ] %>% 
  rename(
    "Countries" = X1,
    "Dengue Cases" = colnames(data)[ncol(data)]
  )
final1 <- final %>% filter(
  Countries == "French Polynesia" | Countries == "Fiji" | Countries == "Marshall Islands"
)

finalFiji1<- as.character(final1[1,2])
total<- as.numeric(finalFiji1)

my_data <- read_excel(paste0(mypath,"Fiji_dengue.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

#save externally for use in ggplot later
save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"save_data_fiji_dengue_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_fiji <- my_data

#write to excel for subsequent update when data is updated by the site
write.xlsx(data_date_fiji, paste0(mypath,"Fiji_dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))
#save the processed data 
saveRDS(my_data, file = paste0(mypath,"save_data_fiji_dengue.Rdata"))





#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases in French Polynesia:
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
date1 <- pdf_text(web[11])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[,5])
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][3]
df_results <- extract_tables(web[11], pages = 3)
data <- as.data.frame(df_results)
data <- data[-c(1,2), ]
data <- data[!sapply(data, function(x) all(x == ""))]
final <- data[ , c(1, length( names( data ) ) ) ] %>% 
  rename(
    "Countries" = X1,
    "Dengue Cases" = colnames(data)[ncol(data)]
  )
final1 <- final %>% filter(
  Countries == "French Polynesia" | Countries == "Fiji" | Countries == "Marshall Islands"
)

finalFP<- as.character(final1[2,2])
total<- as.numeric(finalFP)

my_data <- read_excel(paste0(mypath,"French_Polynesya_dengue.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

#save data to used for ggplot later
save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"french_polynesia_dengue_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_french_polynesia <- my_data
#write data to excel for future update when the site updates the data.

write.xlsx(data_date_french_polynesia, paste0(mypath,"French_Polynesya_dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))
#save the processed data
saveRDS(my_data, file = paste0(mypath,"french_polynesia_dengue.Rdata"))


#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases in Marshall Islands:
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
date1 <- pdf_text(web[11])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[,5])
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][3]
df_results <- extract_tables(web[11], pages = 3)
data <- as.data.frame(df_results)
data <- data[-c(1,2), ]
data <- data[!sapply(data, function(x) all(x == ""))]
final <- data[ , c(1, length( names( data ) ) ) ] %>% 
  rename(
    "Countries" = X1,
    "Dengue Cases" = colnames(data)[ncol(data)]
  )
final1 <- final %>% filter(
  Countries == "French Polynesia" | Countries == "Fiji" | Countries == "Marshall Islands"
)

finalMI<- as.character(final1[3,2])
total<- as.numeric(finalMI)

my_data <- read_excel(paste0(mypath,"Marshall_Islands_dengue.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)


#save data to used for ggplot later
save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"marshall_dengue_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_marshall <- my_data

#write data to excel for future update when the site updates the data.
write.xlsx(data_date_marshall,paste0(mypath,"Marshall_Islands_dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))

#save the processed data
saveRDS(my_data, file = paste0(mypath,"marshall_dengue.Rdata"))


#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases in Malaysia
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

site <- "http://idengue.arsm.gov.my/"
date <- site %>% read_html() %>% html_nodes(xpath='//*[@id="txtsya2"]/center[1]')
Date <- date %>% html_nodes("tr")  %>% html_nodes("th") %>% html_nodes("span")
date <- Date[3] %>% html_text %>% trimws()
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][2]


my_data <- read_excel(paste0(mypath,"Malaysia_Dengue.xlsx"))

my_data_date <- my_data
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
data <- site %>% read_html() %>% html_nodes(xpath='//*[@id="txtsya2"]/center[2]/table') %>% html_table()
total1 <- sum(my_data$`2020`)
total2 <- data[[1]]$X3
total2 <- gsub(",","",total2)
total2 <- as.numeric(total2)
total <- total2 - total1
new_cases <- ifelse(date != my_data_date[13,6], total, 0)
paste("New Cases: ", new_cases)


means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)



my_data[1, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

#save data to used for ggplot later
save_data <- my_data
saveRDS(save_data, file =paste0(mypath,"Malaysia_dengue_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_malaysia <- my_data
write.xlsx(data_date_malaysia,paste0(mypath,"Malaysia_Dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")


my_data$sixten_mean <- ifelse(my_data$`2016` > means$Above, 2,
                              ifelse(my_data$`2016` < means$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > means$Above, 2,
                                ifelse(my_data$`2017` < means$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))

#save the processed data
saveRDS(my_data, file = paste0(mypath,"Malaysia_dengue.Rdata"))


#' ----------------------------------------------------------------------------------------------
#' scrapping Polio cases in Afghanistan
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 
url <- 'http://polioeradication.org/polio-today/polio-now/this-week/'
webpage <- read_html(url)
title_html <- html_nodes(webpage,'h2')
date <- html_text(title_html[1])
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][7]
info_html <- html_nodes(webpage,'.panel-collapse')
Afghanistan <-  html_text(info_html[1])
Afghanistan <-toString(Afghanistan)
afghanistan_cases <- gsub( " .*$", "", Afghanistan )
word_to_number <- function(x){
  # Remove punctuation and 'and'
  x <- tolower(gsub("([[:punct:]]| and )", " ", x))
  # separate into distinct words
  x <- trimws(unlist(strsplit(x, "\\s+")))
  
  # verify that all words are found in the reference vectors.
  if (!(all(x %in% names(c(word_to_number_reference, magnitude_reference)))))
    stop("Text found that is not compatible with conversion. Check your spelling?")
  
  # translate words to the numeric reference
  num <- c(word_to_number_reference, magnitude_reference)[x]
  
  # Identify positions with a magnitude indicator
  magnitude_at <- 
    which(names(num) %in% 
            c("quadrillion", "trillion", "billion",
              "million", "thousand"))
  
  # Create an indexing vector for each magnitude class of the number
  magnitude_index <- 
    cut(seq_along(num), 
        breaks = unique(c(0, magnitude_at, length(num))))
  
  # Make a list with each magnitude
  num_component <- 
    lapply(unique(magnitude_index),
           FUN = function(i) num[magnitude_index == i])
  
  # Transate each component
  num_component <- 
    vapply(num_component,
           FUN = word_to_number_translate_hundred,
           FUN.VALUE = numeric(1))
  
  # Add the components together
  num <- sum(num_component)
  
  if (is.na(num))
    warning(sprintf("Unable to translate %s", x))
  
  num
}

word_to_number_translate_hundred <- function(n){
  # set a magnitude multiplier for thousands and greater
  if (tail(names(n), 1) %in% names(magnitude_reference)){
    magnitude <- tail(n, 1)
    n <- head(n, -1)
  } else {
    magnitude <- 1
  }
  
  # if hundred appears anywhere but the second position or of the
  # value preceding hundred is greater than 9, handle with care
  # (for instance, 1200)
  if ( ("hundred" %in% names(n) && which(names(n) == "hundred") != 2) ||
       ("hundred" %in% names(n) && n[1] > 1) )
  {
    which_hundred <- which(names(n) == "hundred")
    (sum(n[seq_along(n) < which_hundred]) * 100 + 
        sum(n[seq_along(n) > which_hundred])) * magnitude
  } else {
    op <- rep("+", length(n) - 1)
    op[names(n)[-1] == "hundred"] <- "*"
    op <- c(op, "")
    eval(parse(text = paste(paste(n, op), collapse = " "))) * magnitude
  }
}



word_to_number_reference <- 
  c("zero" = 0,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fourteen" = 14,
    "fifteen" = 15,
    "sixteen" = 16,
    "seventeen" = 17,
    "eighteen" = 18,
    "nineteen" = 19,
    "twenty" = 20,
    "thirty" = 30,
    "forty" = 40,
    "fifty" = 50,
    "sixty" = 60,
    "seventy" = 70,
    "eighty" = 80,
    "ninety" = 90,
    "hundred" = 100)

magnitude_reference <- 
  c("thousand" = 1000,
    "million" =  1e6,
    "billion" =  1e9,
    "trillion" = 1e12,
    "quadrillion" = 1e15)

total <- ifelse(afghanistan_cases == "No", 0, word_to_number(afghanistan_cases))

my_data <- read_excel(paste0(mypath,"afghanistan_polio.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,7], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

save_data <- my_data
saveRDS(save_data, file =paste0(mypath,"afghanistan_polio_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_afghanistan <- my_data

write.xlsx(data_date_afghanistan,paste0(mypath,"afghanistan_polio.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

my_data$fiveten_mean <- ifelse(my_data$`2015` > means$Above, 2,
                               ifelse(my_data$`2015` < means$Below, 0,
                                      1))
my_data$sixten_mean <- ifelse(my_data$`2016` > means$Above, 2,
                              ifelse(my_data$`2016` < means$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > means$Above, 2,
                                ifelse(my_data$`2017` < means$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))
#save the processed data
saveRDS(my_data, file =paste0(mypath,"afghanistan_polio.Rdata"))

#' ----------------------------------------------------------------------------------------------
#' scrapping Polio cases in Pakistan
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

url <- 'http://polioeradication.org/polio-today/polio-now/this-week/'
webpage <- read_html(url)
title_html <- html_nodes(webpage,'h2')
date <- html_text(title_html[1])
paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][7]
info_html <- html_nodes(webpage,'.panel-collapse')
Pakistan <-  html_text(info_html[2])
pakistan_cases <- gsub( " .*$", "", Pakistan )
word_to_number <- function(x){
  # Remove punctuation and 'and'
  x <- tolower(gsub("([[:punct:]]| and )", " ", x))
  # separate into distinct words
  x <- trimws(unlist(strsplit(x, "\\s+")))
  
  # verify that all words are found in the reference vectors.
  if (!(all(x %in% names(c(word_to_number_reference, magnitude_reference)))))
    stop("Text found that is not compatible with conversion. Check your spelling?")
  
  # translate words to the numeric reference
  num <- c(word_to_number_reference, magnitude_reference)[x]
  
  # Identify positions with a magnitude indicator
  magnitude_at <- 
    which(names(num) %in% 
            c("quadrillion", "trillion", "billion",
              "million", "thousand"))
  
  # Create an indexing vector for each magnitude class of the number
  magnitude_index <- 
    cut(seq_along(num), 
        breaks = unique(c(0, magnitude_at, length(num))))
  
  # Make a list with each magnitude
  num_component <- 
    lapply(unique(magnitude_index),
           FUN = function(i) num[magnitude_index == i])
  
  # Transate each component
  num_component <- 
    vapply(num_component,
           FUN = word_to_number_translate_hundred,
           FUN.VALUE = numeric(1))
  
  # Add the components together
  num <- sum(num_component)
  
  if (is.na(num))
    warning(sprintf("Unable to translate %s", x))
  
  num
}

word_to_number_translate_hundred <- function(n){
  # set a magnitude multiplier for thousands and greater
  if (tail(names(n), 1) %in% names(magnitude_reference)){
    magnitude <- tail(n, 1)
    n <- head(n, -1)
  } else {
    magnitude <- 1
  }
  
  # if hundred appears anywhere but the second position or of the
  # value preceding hundred is greater than 9, handle with care
  # (for instance, 1200)
  if ( ("hundred" %in% names(n) && which(names(n) == "hundred") != 2) ||
       ("hundred" %in% names(n) && n[1] > 1) )
  {
    which_hundred <- which(names(n) == "hundred")
    (sum(n[seq_along(n) < which_hundred]) * 100 + 
        sum(n[seq_along(n) > which_hundred])) * magnitude
  } else {
    op <- rep("+", length(n) - 1)
    op[names(n)[-1] == "hundred"] <- "*"
    op <- c(op, "")
    eval(parse(text = paste(paste(n, op), collapse = " "))) * magnitude
  }
}



word_to_number_reference <- 
  c("zero" = 0,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fourteen" = 14,
    "fifteen" = 15,
    "sixteen" = 16,
    "seventeen" = 17,
    "eighteen" = 18,
    "nineteen" = 19,
    "twenty" = 20,
    "thirty" = 30,
    "forty" = 40,
    "fifty" = 50,
    "sixty" = 60,
    "seventy" = 70,
    "eighty" = 80,
    "ninety" = 90,
    "hundred" = 100)

magnitude_reference <- 
  c("thousand" = 1000,
    "million" =  1e6,
    "billion" =  1e9,
    "trillion" = 1e12,
    "quadrillion" = 1e15)

total <- ifelse(pakistan_cases == "No", 0, word_to_number(pakistan_cases))

my_data <- read_excel(paste0(mypath,"pakistan_polio.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,7], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"pakistan_polio_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_pakistan <- my_data

write.xlsx(data_date_pakistan,paste0(mypath,"pakistan_polio.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

my_data$fiveten_mean <- ifelse(my_data$`2015` > means$Above, 2,
                               ifelse(my_data$`2015` < means$Below, 0,
                                      1))
my_data$sixten_mean <- ifelse(my_data$`2016` > means$Above, 2,
                              ifelse(my_data$`2016` < means$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > means$Above, 2,
                                ifelse(my_data$`2017` < means$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))
#save the processed data
saveRDS(my_data, file = paste0(mypath,"pakistan_polio.Rdata"))

#' ----------------------------------------------------------------------------------------------
#' scrapping Measles cases in Bangladesh
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

site <- "http://www.searo.who.int/bangladesh/mmwb/en/"
webpage <- read_html(site) %>% html_nodes("ul") %>% html_nodes("li") %>% html_nodes("a") %>% html_attr("href")
web <- paste("http://www.searo.who.int/", webpage[16],sep="")
date1 <- pdf_text(web)[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[2])
paste("Last update: ", date)
month1 <- gsub('[[:digit:]]+', '', date)
month1.1 <- strsplit(month1, " ")
month2 <- month2 <- ifelse(length(month1.1[[1]]) >= 8, month1.1[[1]][7],
                           ifelse(length(month1.1[[1]]) == 5, month1.1[[1]][4],
                                  month1.1[[1]][6]))
data1 <- pdf_text(web)[6]
data2 <- data2 <- str_extract(data1, "(?i)(?<=Total\\D)\\d+")
total <- data2 %>% as.numeric()

my_data <- read_excel(paste0(mypath,"Bangladesh_Measles.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)



my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)


save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"Bangladesh_Measles_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_bangladesh_measles <- my_data

write.xlsx(data_date_bangladesh_measles,paste0(mypath,"Bangladesh_Measles.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")


my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))
#save the processed data
saveRDS(my_data, file = paste0(mypath,"Bangladesh_Measles.Rdata"))

#' ----------------------------------------------------------------------------------------------
#' scrapping Diarrhea cases in Bangladesh
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#'
site <- "http://www.searo.who.int/bangladesh/mmwb/en/"
webpage <- read_html(site) %>% html_nodes("ul") %>% html_nodes("li") %>% html_nodes("a") %>% html_attr("href")
web <- paste("http://www.searo.who.int/", webpage[16],sep="")
date1 <- pdf_text(web)[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[2])
paste("Last update: ", date)
month1 <- gsub('[[:digit:]]+', '', date)
month1.1 <- strsplit(month1, " ")
month2 <- month2 <- ifelse(length(month1.1[[1]]) >= 8, month1.1[[1]][7],
                           ifelse(length(month1.1[[1]]) == 5, month1.1[[1]][4],
                                  month1.1[[1]][6]))
data1 <- pdf_text(web)[8]
data2 <- str_match(data1, "A total (.*?) cases")
total <- gsub(" ", "", data2[,2], fixed = TRUE) %>% as.numeric()


my_data <- read_excel(paste0(mypath,"Bangladesh_Diarrhea.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)


save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"Bangladesh_Diarrhea_ggplot.Rdata"))

my_data[13, '2020'] <- date

data_date_bangladesh_diarrhea <- my_data

write.xlsx(data_date_bangladesh_diarrhea,paste0(mypath,"Bangladesh_Diarrhea.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")


my_data$eigthten_mean <- ifelse(my_data$`2018` > means$Above, 2,
                                ifelse(my_data$`2018` < means$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > means$Above, 2,
                               ifelse(my_data$`2019` < means$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > means$Above, 2,
                              ifelse(my_data$`2020` < means$Below, 0,
                                     1))
#save the processed data
saveRDS(my_data, file = paste0(mypath,"Bangladesh_Diarrhea.Rdata"))

#' ----------------------------------------------------------------------------------------------
#' scrapping Drought Situation Report- Sri Lanka
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#'

site <- "http://www.dmc.gov.lk/index.php?option=com_dmcreports&view=reports&Itemid=273&report_type_id=1&lang=en"
date <- read_html(site) %>% html_nodes("td")
paste("Last update: ", gsub("\\D+\\D+", "\\1",  date[10]))



my_data <- read_excel(paste0(mypath,"sri_lanka_drought.xlsx"))

my_data <- as.data.frame(my_data)


#my_data[1, '2020'] <- my_data$`2020`[1]+
#my_data[2, '2020'] <- my_data$`2020`[2]+
#my_data[3, '2020'] <- my_data$`2020`[3]+
#my_data[4, '2020'] <- my_data$`2020`[4]+
#my_data[5, '2020'] <- my_data$`2020`[5]+
#my_data[6, '2020'] <- my_data$`2020`[6]+
#my_data[7, '2020'] <- my_data$`2020`[7]+
#my_data[8, '2020'] <- 7707
#my_data[9, '2020'] <- my_data$`2020`[9]+
#my_data[10, '2020'] <- 82906
#my_data[11, '2020'] <- 103445
#my_data[12, '2020'] <- 124348
#my_data[13, '2020'] <- 178426
#my_data[14, '2020'] <- 262032
#my_data[15, '2020'] <- 248340
my_data[16, '2020'] <- 297039
#my_data[17, '2020'] <- my_data$`2020`[17]+
#my_data[18, '2020'] <- my_data$`2020`[18]+
#my_data[19, '2020'] <- my_data$`2020`[19]+
#my_data[20, '2020'] <- my_data$`2020`[20]+
#my_data[21, '2020'] <- my_data$`2020`[21]+
#my_data[22, '2020'] <- my_data$`2020`[22]+
#my_data[23, '2020'] <- my_data$`2020`[23]+
#my_data[24, '2020'] <- my_data$`2020`[24]+
#my_data[25, '2020'] <- my_data$`2020`[25]+
#my_data[26, '2020'] <- my_data$`2020`[26]+
#my_data[27, '2020'] <- my_data$`2020`[27]+
#my_data[28, '2020'] <- my_data$`2020`[28]+
#my_data[29, '2020'] <- my_data$`2020`[29]+
#my_data[30, '2020'] <- my_data$`2020`[30]+
#my_data[31, '2020'] <- my_data$`2020`[31]+
#my_data[32, '2020'] <- my_data$`2020`[32]+
#my_data[33, '2020'] <- my_data$`2020`[33]+
#my_data[34, '2020'] <- my_data$`2020`[34]+
#my_data[35, '2020'] <- my_data$`2020`[35]+
#my_data[36, '2020'] <- my_data$`2020`[36]+
#my_data[37, '2020'] <- my_data$`2020`[37]+
#my_data[38, '2020'] <- my_data$`2020`[38]+
#my_data[39, '2020'] <- my_data$`2020`[39]+
#my_data[40, '2020'] <- my_data$`2020`[40]+
#my_data[41, '2020'] <- my_data$`2020`[41]+
#my_data[42, '2020'] <- my_data$`2020`[42]+
#my_data[43, '2020'] <- my_data$`2020`[43]+
#my_data[44, '2020'] <- my_data$`2020`[44]+
#my_data[45, '2020'] <- my_data$`2020`[45]+
#my_data[46, '2020'] <- my_data$`2020`[46]+
#my_data[47, '2020'] <- my_data$`2020`[47]+
#my_data[48, '2020'] <- my_data$`2020`[48]+
#my_data[49, '2020'] <- my_data$`2020`[49]+
#my_data[50, '2020'] <- my_data$`2020`[50]+
#my_data[51, '2020'] <- my_data$`2020`[51]+
#my_data[52, '2020'] <- my_data$`2020`[52]+

save_data_srilanka_drought <- my_data

write.xlsx(save_data_srilanka_drought,paste0(mypath,"sri_lanka_drought.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data$twente_mean <- ifelse(my_data$`2020` > 50000, 2,
                              ifelse(my_data$`2020` < 30000, 0,
                                     1))
#save the processed data
saveRDS(my_data, file = paste0(mypath,"save_data_srilanka_drought.Rdata"))

