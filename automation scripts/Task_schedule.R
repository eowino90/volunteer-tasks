# -------------------------------------------------- 
# Script Name : Task_schedule.R
# Purpose: Automate running of Render_the_Diseases_monitoring_tables_pdf.R, which in turn runs Diseases_monitoring_tables_pdf.Rmd
# 
# Note: This uses windows taskscheduler so it only works in windows platform
# --------------------------------------------------

#set path for the Render_the_Diseases_monitoring_tables_pdf.R (this is the file that calls Diseases_monitoring_tables_pdf.Rmd markdown)

#change the path to the folder that contains Render_the_Diseases_monitoring_tables_pdf.R script
path= "C:/Users/user/Documents/volunteer/"
#install this library in case it is missing.
if(!require(taskscheduleR)) install.packages("taskscheduleR"); library(taskscheduleR)

## Change the schedule time as it suites you
taskscheduler_create(taskname = "webscrapping", 
                     rscript =paste0(path,"Render_the_Diseases_monitoring_tables_pdf.R"), 
                     schedule = "ONCE",
                     starttime = "16:25",
                     Rexe = file.path(Sys.getenv("R_HOME"),"bin","Rscript.exe")
                     )

#' stop the task before deleting it in case you need to it stopped.
#' uncomment the below if your want to stop then delete the task
#taskcheduler_stop("webscrapping")
#taskscheduler_delete("webscrapping")
