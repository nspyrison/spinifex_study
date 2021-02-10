#' @example
#' str(dat) 
#' ggplot(dat) +
#'   ggproto_ans_plot(dat) +
#'   facet_wrap(vars(sim_nm)) + theme_minimal()

## ggproto for the ans_plot
ggproto_ans_plot <- function(
  resp_ans_longer, 
  brewer_pal = RColorBrewer::brewer.pal(8L, "Dark2")){
  ## List of ggproto objects
  lab_fill <- "Varaiable cluster seperation"
  ret <- list(
    ## Boxplot, signal
    ggplot2::geom_bar(ggplot2::aes(x = var_num, y = signal, fill = lab_fill),
                      resp_ans_longer, position = "dodge", stat = "identity",
                      width = .5),
    
    ## Titles and colors
    ggplot2::labs(x = "Variable number", y = "Value"),
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = "horizontal"),
    ggplot2::scale_fill_manual(
      values = c(brewer_pal[1L], "grey80", "lightblue"), name = "",
      labels = c("Varaiable cluster seperation", "selected", "not selected")),
    ggplot2::scale_colour_manual(values = c("green", "red"),
                                 name = "", labels = c("marks (+)", "marks (-)"))
  )
  
  ## Add in the bar and weight
  p <- 6L #<- length(unique(resp_ans_longer$var_num))
  mark_col <- dplyr::if_else(sign(resp_ans_longer$diff) == 1L, "green", "red")
  ret <- c(ret,
           list(
             ## 0 line
             ggplot2::geom_hline(yintercept = 0L, size = 1L, linetype = 2L),
             ## Uniform bar, and text
             ggplot2::geom_hline(ggplot2::aes(yintercept = bar), 
                                 resp_ans_longer, 
                                 size = 1L, linetype = 3L),
             ggplot2::geom_text(ggplot2::aes(x = p + 1L, y = bar + .1,
                                             label = paste0("1/p = ", round(bar, 2))),
                                resp_ans_longer,
                                size = 4L, hjust = 1L),
             ## Marks segment
             ggplot2::geom_segment(ggplot2::aes(x = var_num, xend = var_num,
                                                y = 0, yend = weight),
                                   resp_ans_longer, colour = mark_col, size = 2L)
           )
  )
  
  return(ret)
}
