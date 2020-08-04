# Fake "dashboard" picture

library(ggplot2)

(fakebar <- ggplot(data.frame(x=1:3, y=3:1), aes(x,y,fill=factor(y))) + 
    geom_col() +
    theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_brewer(palette='Dark2') +
    theme(legend.position = 'none', axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
          axis.line = element_line(size = 2)))
(fakepie <- ggplot(data.frame(y=c(10,40,60),x=4:6),aes(x="",y=y,fill=factor(x))) +
    geom_bar(width=1, stat='identity') +
    coord_polar(theta='y') +
    scale_fill_brewer(palette='Dark2') +
    theme_void() +
    theme(legend.position = 'none'))

ggsave('~/Dropbox/Q/projects/foodwaste/Interventions_MS/abstract_imgs/usmapgg.png', usmap::plot_usmap(regions='states'), dpi = 400, height = 3, width = 4)
