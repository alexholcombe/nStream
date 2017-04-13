
#Set it up as an actual theme with parameters. Charlie previously had it as something that wasn't a function.
theme_apa <- function(base_size = 12, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
		panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line.x=element_line(size = .25),
        axis.line.y=element_line(size = .25),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        #text=element_text(family='Arial', size=16),
        legend.key = element_rect(colour = NA),
        strip.background = element_rect(colour=NA, fill=NA)
      )
}