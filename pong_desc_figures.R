require(mlogit)
library(dplyr)
library(data.table)
library(ggplot2)
library(jsonlite)

# Background ----
source("Figure_theme.R")

source(file = "functions_pong_soft_launch.R")

input_specs <- read.csv("input/input_specs.csv")
setDT(input_specs)

if (input_specs[,anyDuplicated(.SD),.SDcols = c("survey_name","data_path")]){
  stop("Sample-data file pairs should be unique")
}

survey.names =c("panelclix_hoorn","full_launch_panelclix") # names(files.path)#c("soft_launch_pooled")

invest.list.lvl <- list("insu" = 5000,"heat_networks" = 2000, "heat_pumps" = 7000)

for (survey.name in survey.names){
  
  dir.out <- paste0("output/",survey.name,"/")
  dir.create(dir.out)
  
  dt.insu = read.csv2(paste0(dir.out,"data_mlogit_insu.csv"))


}

