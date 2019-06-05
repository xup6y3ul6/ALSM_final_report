###########################################################
## reference: https://github.com/ggobi/ggally/issues/139 ##
###########################################################

library(ggplot2)
library(RColorBrewer)

my_custom_cor <- function(data, mapping, color = I("grey50"), sizeRange = c(1, 5), ...) {
  
  # get the x and y data to use the other code
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  ct <- cor.test(x,y)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  
  # since we can't print it to get the strsize, just use the max size range
  cex <- max(sizeRange)
  
  # helper function to calculate a useable size
  percent_of_range <- function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)
  }
  
  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...
  ) + 
    # add the sig stars
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8
      ),
      label = sig, 
      size = I(cex),
      color = color,
      ...
    ) + 
    # remove all the background stuff and wrap it with a dashed line
    theme_classic() + 
    theme(
      panel.background = element_rect(
        color = color, 
        linetype = "longdash"
      ), 
      axis.line = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank()
    )
}

my_custom_cor_color <- function(data, mapping, color = I("black"), sizeRange = c(1, 5), ...) {
  
  # get the x and y data to use the other code
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  ct <- cor.test(x,y)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  tt <- as.character(rt)
  
  
  # plot the cor value
  p <- ggally_text(
    label = tt, 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = 6,
    color=color,
    ...
  ) +
  # add the sig stars
  geom_text(
    aes_string(
      x = 0.8,
      y = 0.8
    ),
    label = sig, 
    color = color,
    ...
  ) + 
  theme(
    panel.background=element_rect(fill="white", color = "black", linetype = "dashed"),
    #panel.background = element_rect(color = "black", linetype = "dashed"),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank()
  ) 
  
  corColors <- RColorBrewer::brewer.pal(n = 7, name = "RdYlGn")[2:6]
  
  if (r <= -0.8) {
    corCol <- corColors[1]
  } else if (r <= -0.6) {
    corCol <- corColors[2]
  } else if (r < 0.6) {
    corCol <- corColors[3]
  } else if (r < 0.8) {
    corCol <- corColors[4]
  } else {
    corCol <- corColors[5]
  }
  p <- p + theme(
    panel.background = element_rect(fill= corCol)
  )
  
  p
}

my_custom_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "loess", color = I("blue"), size = 0.8, ...) +
    theme_bw()
}

# my_custom_smooth(iris, aes(Sepal.Length, Sepal.Width))
# my_custom_cor(iris, aes(Sepal.Length, Sepal.Width))
# my_custom_cor_color(iris, aes(Sepal.Length, Sepal.Width))


##
library(gridExtra)

scatter_and_histogram_plot <- function(.data, .x, .y, .group){
  g <- ggplot(.data, aes_string(.x, .y, color = .group)) +
    geom_point(size = 1.2) +
    theme_bw() +
    theme(legend.justification=c(0,1), legend.position=c(0.02,0.98),
          legend.text = element_text(size = 18),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(1.5, "cm")
          legend.title = element_blank())
  
  theme0 <- function(...) theme( legend.position = "none",
                                 panel.background = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.margin = unit(0,"null"),
                                 axis.ticks = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.ticks.length = unit(0,"null"),
                                 axis.ticks.margin = unit(0,"null"),
                                 panel.border=element_rect(color=NA),...)
  
  g_up <- ggplot(.data, aes_string(.x, colour = .group, fill = .group)) + 
    geom_histogram(alpha=0.5) + 
    scale_x_continuous(breaks=NULL,expand=c(0.02,0)) +
    scale_y_continuous(breaks=NULL,expand=c(0.02,0)) +
    theme_bw() +
    theme0(plot.margin = unit(c(1,0,0,2.2),"lines")) 
  
  g_right <- ggplot(.data, aes_string(.y, colour = .group, fill = .group)) + 
    geom_histogram(alpha=0.5) + 
    coord_flip()  + 
    scale_x_continuous(labels = NULL,breaks=NULL,expand=c(0.02,0)) +
    scale_y_continuous(labels = NULL,breaks=NULL,expand=c(0.02,0)) +
    theme_bw() +
    theme0(plot.margin = unit(c(0,1,1.2,0),"lines"))
  
  grid.arrange(arrangeGrob(g_up, ncol = 2, widths = c(4,1)),
               arrangeGrob(g, g_right, ncol = 2, widths = c(4,1)),
               heights = c(1,4))
}

print_summary <- function(data){
  .s <- summary(data) %>% t() %>% t() %>% round(digits = 3)
  rownames(.s) <- c("Min.", "Qu_1", "Med.", "Mean", "Qu_3", "Max.")
  write.table(format(.s, justify = "right"), 
              quote =FALSE, col.names = FALSE, sep = "\t")
}

