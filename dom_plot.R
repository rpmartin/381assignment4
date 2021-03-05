dom_plot <- function(data, group_var, measure_var){
#plots pdf, cdf, integral of cdf for visualizing 2nd order stochastic dominance. 
#make sure inputs are valid  
  assert_that(is.data.frame(data), msg="1st arg must be dataframe.")
  group_var_levels <- data%>%
    select({{ group_var }})%>%
    unique()%>%
    nrow()
  assert_that(group_var_levels==2, msg="2nd arg must be binary variable.")
  measure_var_numeric <- data%>%
    select({{ measure_var }})%>%
    pull()%>%
    is.numeric()
  assert_that(measure_var_numeric, msg="3rd arg must be a numeric variable.")

####do the thing
  mean_measure <- data%>%
    group_by({{ group_var }})%>%
    summarise(mean_measure=mean({{ measure_var }}, na.rm=TRUE))
  pdf <- ggplot(data,mapping=aes(x={{ measure_var }}, fill=factor({{ group_var }}), colour=factor({{ group_var }}))) +
    geom_density(alpha=.15,adjust=.15)+
    geom_vline(data=mean_measure,aes(xintercept=mean_measure,colour=factor({{ group_var }})),lty=2)+
    labs(title=paste0("Empirical PDF and mean ",substitute(measure_var), " (dashed line) by ",substitute(group_var)),
         x= substitute(measure_var),
         y="density",
         fill=substitute(group_var),
         colour=substitute(group_var)) 
  cdf <-   ggplot(data, mapping=aes(x={{ measure_var }}, colour=factor({{ group_var }}), fill=factor({{ group_var }}))) +
    stat_ecdf(geom = "area", alpha=.15)+
    geom_vline(data=mean_measure,aes(xintercept=mean_measure,colour=factor({{ group_var }})),lty=2)+
    labs(title=paste0("Empirical CDF and mean ",substitute(measure_var), " (dashed line) by ",substitute(group_var)),
         x= substitute(measure_var),
         y="cumulative density",
         fill=substitute(group_var),
         colour=substitute(group_var))
  cdf_df <- data%>%
    group_by({{ group_var }})%>%
    mutate(cdf=cume_dist({{ measure_var }}))%>%
    group_by({{ measure_var }}, .add=TRUE)%>%
    summarize(m_var=mean({{ measure_var }}),height=mean(cdf))%>%
    mutate(base=c(0,diff(m_var)),
           area=base*height,
           cum_sum_area=cumsum(area))
  integral <- ggplot(cdf_df,aes({{ measure_var }}, cum_sum_area, colour=factor({{ group_var }})))+
    geom_line()+
    labs(title=paste0("Area under empirical CDF by ",substitute(group_var)),
         x= substitute(measure_var),
         y="area under CDF",
         colour=substitute(group_var))
  figure <- ggarrange(pdf,cdf,integral, ncol = 1, nrow = 3)
}


