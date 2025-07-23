library(data.table)
library(ggplot2)
library(jsonlite)
library(scales) 

# Background ----
source("Figure_theme.R")

source(file = "functions_pong_soft_launch.R")

plot_percent_bar <- function(data, var_name_str) {
  var_sym <- sym(var_name_str)
  
  ggplot(data, aes(x = !!var_sym)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = ..count.., y = (..count..) / sum(..count..)), 
              stat = "count", hjust = -0.1) +
    coord_flip() +
    theme.graphs +
    theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
    scale_y_continuous(labels = percent_format(), name = "Share of respondents")
}

clean_str <- \(x) stringr::str_to_sentence(gsub("_"," ",x))

grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")

# Surveys
survey.names =c("panelclix_hoorn","full_launch_panelclix")

for (survey.name in survey.names){
  
  dir.out <- paste0("output/",survey.name,"/")
  dir.create(dir.out)
  
  dt = read.csv2(paste0(dir.out,"data_descriptives.csv"))
  setDT(dt)
  for (grp.ht in names(grp.ht.list)){
    
  for (desc in grp.ht.list[[grp.ht]]){
    
    dt.plt <- dt[, .(col = clean_str(get(gsub("desc_","",desc))))]
    
    new.nm <- names(grp.ht.list[[grp.ht]][which(grp.ht.list[[grp.ht]] == desc)])
    setnames(dt.plt, "col", new.nm)
    
    p <- plot_percent_bar(dt.plt, new.nm)
    show(p)
  }
  }
  
  
  
}

