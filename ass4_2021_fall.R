###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment4.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

library("tidyverse") #we use functions from this library.
library("viridis")
library("ggridges")
library("plotly")
library("ggpubr")
library("ggrepel")
library("assertthat")
library("here")
####################which section?
section <- 1
directory <- paste0("publicdata",section)
file <- paste0("lvcm",section,".csv")
############
mydf <- read_csv(here(directory,file))%>%
  mutate(round=factor(round))
############functions
calc_relative <- function(adf){
  adf <- adf%>%  
    mutate(choice_relative_to_first_round=choice-head(choice,n=1))
}

fix_gg_labs <- function(gg){
  gg+
    labs(x=str_to_title(str_replace_all(gg$labels$x, "_", " ")),
         y=str_to_title(str_replace_all(gg$labels$y, "_", " ")),
         colour=str_to_title(str_replace_all(gg$labels$colour, "_", " ")),
         edge_colour=str_to_title(str_replace_all(gg$labels$edge_colour, "_", " ")))  
}

fifth_plot <-ggplot(mydf, aes(x = choice, y = round, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)+
  facet_wrap(vars(rematching))+
  labs(title="Density plot of effort levels by round and treatment")

