###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment3.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") #we use functions from this library.

####################which section are you in?
section <- 1 #either put a 1 or a 2 here.
#########################
mydf <- read_csv(paste("publicdata",section,"/lvcm",section,".csv",sep="")) # reads in the data

(first_plot <- mydf %>% # assign the name first_plot (referenced in your assignment3.Rmd file), use dataframe mydf THEN
    ggplot(aes(x=rematching,y=choice))+ 
    geom_boxplot(outlier.shape = NA,fill="red",alpha=.1)+ # do not plot outliers because we add data below.
    geom_jitter(alpha=0.1,height=0.25) + # the data with some random noise and transparency to avoid overplotting.
    labs(y="contributions to public good", x="rematching",title = "Contributions by treatment")+
    scale_y_continuous(breaks=0:10)) 

(third_plot <- mydf %>% 
    ggplot(aes(x = round,y = choice,colour=rematching))+ 
    geom_line(stat="smooth",method = "lm",se=FALSE,alpha=.5)+
    geom_jitter(width=.25,height=.25,alpha=.1)+
  labs(y="contributions to public good", x="round",title="Contributions by treatment and round number")+
    scale_y_continuous(breaks=0:10)) 


