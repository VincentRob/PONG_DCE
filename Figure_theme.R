library(ggthemes) 

# Figures parameters ----

scl <- 1
height <- 17*scl
width <- height*1.5*scl # was 1.3
sz.lines <- 2*0.75
sz.txt <- 23 # was 25
binary.continous <- "avg" # use binary or continuous treatment
z = 1.96
scl.double.graph <- 0.75

width.double <- width*scl.double.graph*0.9
height.double <- height*scl.double.graph
aspect_ratio_base_fig = 1.5
  
theme.graphs <- theme_stata() +
  theme(text = element_text(size = sz.txt,family = "serif"), axis.text = element_text(size = sz.txt*0.75)) + 
  theme(axis.title.y = element_text(vjust=2)) +  # Space between y title and y labels 
  theme(axis.title.x = element_text(vjust=-1)) +  # Space between x title and x labels 
  theme(axis.text.y = element_text(hjust=0.5)) +
  theme(plot.background = element_rect(fill = "transparent")) +
  theme(legend.title = element_text(color = "black",size=sz.txt))


grey1 <- "#1e1e1e"
grey2 <- "#808080"
grey3 <- "#c5c5c5"
