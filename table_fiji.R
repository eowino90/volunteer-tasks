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

site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
#Obtain the list of links to the reports
webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
#Take the 1st link (will be the page that contains the last report)
web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
#Obtain and clean the date
date1 <- pdf_text(web[11])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[,5])
paste("Last update: ", date)
#Obtain the month name (we will use it to introduce the data in the correct month cell)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][3]
#We obtain the number of cases
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
#Number of cases
total<- as.numeric(finalFiji1)

#We upload the data for the previous months/years
my_data <- read_excel("Fiji_dengue.xlsx")
my_data[1] <- NULL 
#We save the data with the date from the previous week. We will use it to confirm that the data from this week is new. In case they don't update the data we will found the same date and then the number of cases will be 0
my_data_date <- my_data
#Number of cases, if the date of this week is the same as the date of last week as we said it will show 0 cases
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
paste("New Cases: ", new_cases)
#We delete the date from the data table
my_data <- my_data[-c(13), ] 
#Transform all the columns into numeric data
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

#Obtain the means for every month we will use it to indicate the color of each cell
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
#Obtain the Above and Below 25% of the mean. We will use red color if a cell is 25% Above the medium, Green color if it's 25% Bellow the mean and Yellow color if it's in the mean
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


#We introduce the new number of cases to the correspondent month. If the date for this week is different from the date from previous week and the month name is equal to the correspondent month name we will introduce the data in that month otherwise we will add 0 cases.
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


#We save the table with the new data
save_data <- my_data
#We incorporate the date of this week to the table
my_data[13, '2020'] <- date
data_date <- my_data
#We delete the date and convert every column in numeric to continue operating
my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")


#We create new columns in which we associate every month with the number 0, 1 or 2 depending if the number of this month is Above (2), Below(0) or in the Mean(1)
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
                 columnDefs = list(list(targets = c(4:6), visible = FALSE))
  )
) %>% 
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
write.xlsx(data_date, "Fiji_dengue.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)



#Potential problems:
#- The link we take to navigate to the page with the last report is the 1st and the we take the 11th link from that page, but if the source page change the way it present it, we will obtain the wrong report

#- If the source change the way they present de date then we will not obtain the correspondent month and the code will fail.

#- If they change the way or the page number where they present the tables the code will fail