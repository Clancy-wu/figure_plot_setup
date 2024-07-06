library(ggplot2)
# correlation plot
## note: use title bold, 36 size text and 24 size axis for a good view.
data.table(
  indicator=indicator_sub_all,
  beta=colMeans(active_sub_all[,-1])) %>%
  ggplot(., aes(x=indicator, y=beta)) +
    geom_point(size=4) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth=4) +
    theme_test() + 
    labs(x='E.nodal.wt (thres 0.25)', y='Beta', 
         title='Spearman r(274)=-0.452') +
    theme(axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.text = element_text(size=24),
        plot.title = element_text(size = 32, hjust = 0.5, face='bold'))  
