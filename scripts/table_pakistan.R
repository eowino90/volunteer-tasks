library(xlsx)
library(condformat)
library("readxl")
library(tidyr)
library(tidyverse)
library(DT)
library("rvest")
library(tabulizer)
library(dplyr)
library(miniUI)
library(pdftools)
library(condformat)
library("readxl")
library(data.table)

url <- 'http://polioeradication.org/polio-today/polio-now/this-week/'
webpage <- read_html(url)
#Obtain and clean the date
title_html <- html_nodes(webpage,'h2')
date <- html_text(title_html[1])
paste("Last update: ", date)
#Obtain the month name (we will use it to introduce the data in the correct month cell)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][7]
#Obtain the number of Dengue affected people
info_html <- html_nodes(webpage,'.panel-collapse')
Pakistan <-  html_text(info_html[2])
pakistan_cases <- gsub( " .*$", "", Pakistan )
#We obtain a writen number and we will use the following code to convert it into a numerical number
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


#Number of cases in a numerical number
total <- ifelse(pakistan_cases == "No", 0, word_to_number(pakistan_cases))

#We upload the data for the previous months/years
my_data <- read_excel("pakistan_polio.xlsx")
my_data[1] <- NULL 
#We save the data with the date from the previous week. We will use it to confirm that the data from this week is new. In case they don't update the data we will found the same date and then the number of cases will be 0
my_data_date <- my_data
#Number fo cases, if the date of this week is the same as the date of last week as we said it will show 0 cases
new_cases <- ifelse(date != my_data_date[13,7], total, 0)
paste("New Cases: ", new_cases)
#We delete the date from the data table 
my_data <- my_data[-c(13), ] 
#Transform all the columns into numeric data
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

#Obtain the means for every month we will use it to indicate the color of each cell
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
#Obtain the Above and Below 25% of the mean. We will use red color if a cell is 25% Above the medium, Green color if it's 25% Bellow the mean and Yellow color if it's in the mean
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


#We introduce the new number of cases to the correspondent month. If the date for this week is different from the date from previous week and the month name is equal to the correspondent month name we will introduce the data in that month otherwise we will add 0 cases.
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

#We save the table with the new data
save_data <- my_data
#We incorporate the date of this week to the table
my_data[13, '2020'] <- date
data_date <- my_data
#We delete the date and convert every column in numeric to continue operating
my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")


#We create new columns in which we associate every month with the number 0, 1 or 2 depending if the number of this month is Above (2), Below(0) or in the Mean(1)
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

#We create the table using the previous numbers to indicate from which color will be each cell
DT::datatable(
  my_data,
  rownames = FALSE,
  options = list(pageLength = 15,
                 columnDefs = list(list(targets = c(7:12), visible = FALSE))
  )
) %>% 
  formatStyle(columns = "2015",
              valueColumns = "fiveten_mean",
              backgroundColor = styleEqual(levels = c(0,1,2), values = c("#008000","#FFA500","#F00"))) %>%
  formatStyle(columns = "2016",
              valueColumns = "sixten_mean",
              backgroundColor = styleEqual(levels = c(0,1,2), values = c("#008000","#FFA500","#F00"))) %>%
  formatStyle(columns = "2017",
              valueColumns = "seventen_mean",
              backgroundColor = styleEqual(levels = c(0,1,2), values = c("#008000","#FFA500","#F00"))) %>%
  formatStyle(columns = "2018",
              valueColumns = "eigthten_mean",
              backgroundColor = styleEqual(levels = c(0,1,2), values = c("#008000","#FFA500","#F00"))) %>%
  formatStyle(columns = "2019",
              valueColumns = "nineten_mean",
              backgroundColor = styleEqual(levels = c(0,1,2), values = c("#008000","#FFA500","#F00"))) %>%
  formatStyle(columns = "2020",
              valueColumns = "twente_mean",
              backgroundColor = styleEqual(levels = c(0,1,2), values = c("#008000","#FFA500","#F00")))


#We create the Graph
save_data$Month <-factor(save_data$Month, 
                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

meltdf <- melt(save_data,id="Month")
ggplot(meltdf,aes(x=Month,y=value,colour=variable,group=variable)) + geom_line()


#We rewrite the Excel file to include the new number of cases and the date
write.xlsx(data_date, "pakistan_polio.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)





#Potential problems:
  
#- If the source change the way they present de date then we will not obtain the correspondent month and the code will fail.

#- To obtain the number of cases we take the first word of the sentence that contains the word Dengue in the page 4. If the they present the number of cases in another page, or more than one sentence contain the word Dengue or the number of cases is not the first word the code will fail.

