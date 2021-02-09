#' @example
#' str(dat) 
#' palette(RColorBrewer::brewer.pal(12, "Dark2"))
#'ggplot(dat) +
#'    ggproto_ans_plot(dat) +
#'    facet_wrap(vars(sim_nm)) + theme_minimal()

## ggproto for the ans_plot
ggproto_ans_plot <- function(resp_ans_longer){
  ## List of ggproto objects
  lab_fill <- "Varaiable cluster seperation"
  ret <- list(
    ## Boxplot, signal
    geom_bar(aes(x = var_num, y = signal, fill = lab_fill),
             resp_ans_longer, position = "dodge", stat = "identity",
             width = .5),
    
    ## Titles and colors
    labs(x = "Variable number", y = "Value"),
    theme(legend.position = "bottom",
          legend.direction = "vertical"),
    scale_fill_manual(
      values = c(palette()[1], "grey80", "lightblue"), name = "",
      labels = c("Varaiable cluster seperation", "selected", "not selected")),
    scale_colour_manual(values = c("green", "red"),
                        name = "", labels = c("marks (+)", "marks (-)"))
  )
  
  ## Add in the bar and weight
  p <- 6#<- length(unique(resp_ans_longer$var_num))
  mark_col <- dplyr::if_else(sign(resp_ans_longer$diff) == 1, "green", "red")
  ret <- c(ret, 
           list(
             ## Uniform bar, and text
             geom_hline(aes(yintercept = bar), resp_ans_longer, size = 1), 
             geom_text(aes(x = p + 1, y = bar + .1, 
                           label = paste0("1/p = ", round(bar, 2))),
                       size = 4, hjust = 1),
             ## Marks segment
             geom_segment(aes(x = var_num, xend = var_num,
                              y = bar, yend = weight + bar),
                          resp_ans_longer, colour = mark_col, size = 2)
           )
  )
  
  return(ret)
}
