###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment3.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") #we use functions from this library.
library("viridis")
library("ggridges")
slash <- ifelse(.Platform$OS.type=="unix", "/", "\\") #The eternal directory battle: Windows vs. *nix
####################which section?
section <- 1
#########################
mydf <- read_csv(paste("publicdata",section,slash,"lvcm",section,".csv",sep="")) # reads in the data
####################################
within_group_variability <- mydf%>%
  group_by(whichgroup, round, rematching)%>%
  summarise(variability_contributions=sd(choice))
####################################
# you should be able to recycle code from assignment3.R for plots 1 and 2.)
# https://github.com/rpmartin/381assignment3/
##########################################################
(third_plot <- mydf %>% 
    ggplot(aes(x=choice, y =factor(round), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.75, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_fill_viridis(name = "effort", option = "C") +
    xlim(0,10)+
    labs(x="effort",y="round number")+
    facet_wrap(vars(rematching)))

