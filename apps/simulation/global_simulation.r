### Global init for each factor variant.


##### App simulation global.r -----
##### Initialization -----
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("mvtnorm") # for simulations.
library("plotly")

axes_position <- "center"
options(max.print = 500)

load_list <- list.files("./", pattern = "^simulation_data")
for (i in 1:length(load_list)){
  load_name <- load_list[i]
  assign(load_name, readRDS(load_name))
}
loaded_sim_names <- ls()[grepl("simulation_data", ls())]
load_choices <- if (length(loaded_sim_names) > 0) {loaded_sim_names
} else {"<no 'simulation_dataNNN.rds' loaded>"} 


### Generate simulation panel -----
panel_generate <- tabPanel(
  "generate simulation", 
  sidebarPanel(
    numericInput("sim_p", label = "Number of variables, p", value = 10)
    , numericInput("sim_cl", label = "Number of clusters, cl", value = 4)
    , numericInput("sim_pnoise", label = "Number of noise variables, pnoise", value = 4)
    , fluidRow(column(6, radioButtons(inputId = "task_x_axis", 
                                      label = "x axis", choices = "PC1")),
               column(6, radioButtons(inputId = "task_y_axis", 
                                      label = "y axis", choices = "PC2")))
    , hr() # horizontal line
    , actionButton("save_sim", "save simulation")
    , verbatimTextOutput("save_msg")
  ),
  mainPanel(plotOutput("task_pca")
            , plotlyOutput("task_gtour", height = 600)
  )
)

### Review (static+gtour) panel -----
panel_review <- tabPanel(
  "review, static and grand tour",
  sidebarPanel(
    selectInput("load_sim_name", "Simulation to review",
                choices = load_choices)
    , radioButtons("load_color_pts", "Color points by cluster", 
                 choices = c("yes", "no"), selected = "no", inline = T)
    , fluidRow(column(6, radioButtons(inputId = "load_x_axis", 
                                      label = "x axis", choices = "PC1")),
               column(6, radioButtons(inputId = "load_y_axis", 
                                      label = "y axis", choices = "PC2"))
    )
    , hr() # horizontal line
    , tableOutput("load_curr_basis")
    , verbatimTextOutput("load_dat_attr")
    , verbatimTextOutput("str_load_dat")
  ),
  mainPanel(plotOutput("load_pca")
            , plotlyOutput("load_gtour") #, height = 600)
            , h4("Cluter means, display order")
            , verbatimTextOutput("load_mncl_reord")
            , h4("variance-covariance, display order")
            , verbatimTextOutput("load_vc_reord")
            , h4("Task 2 ans a/b")
            , verbatimTextOutput("load_task2_ptile")
  )
)


### Review (manual) panel -----
panel_review_manual <- tabPanel(
  "review, manual",
  sidebarPanel(
    selectInput("load2_sim_name", "Simulation to review",
                choices = load_choices)
    , radioButtons("load2_color_pts", "Color points by cluster", 
                   choices = c("yes", "no"), selected = "no", inline = T)
    , fluidRow(column(6, radioButtons(inputId = "load2_x_axis", label = "x axis", choices = "PC1")),
               column(6, radioButtons(inputId = "load2_y_axis", label = "y axis", choices = "PC2")))
    , selectInput('manip_var', 'Manip var', "<none>")
    , sliderInput("manip_slider", "Contribution",
                  min = 0, max = 1, value = 0, step = .1)
    , hr() # horizontal line
    , tableOutput("load2_curr_basis")
    , verbatimTextOutput("load2_dat_attr")
    , verbatimTextOutput("str_load2_dat")
  ),
  mainPanel(plotOutput("load2_manual")
            , h4("Cluter means, display order")
            , verbatimTextOutput("load2_mncl_reord")
            , h4("variance-covariance, display order")
            , verbatimTextOutput("load2_vc_reord")
            , h4("Task 2 ans a/b")
            , verbatimTextOutput("load2_task2_ptile")
  )
)


##### UI, combine panels -----
ui <- fluidPage(
  navbarPage("Multivariate simulation generation"
             , panel_generate
             , panel_review
             , panel_review_manual
  )
  , actionButton("browser", "browser()")
  , verbatimTextOutput("dev_msg")
)


###### Simulate clusters
APP_simulate_clusters <- function(p = 10, pnoise = 4, cl = 4){
  #default: p = 10; pnoise = 4; cl = 4
  x <- NULL
  ncl <- NULL
  mncl <- NULL
  vc <- NULL
  for (i in 1:cl) {
    n <- sample(30:150, 1)
    vc <- matrix(sample(seq(-.1, 0.6, by = 0.1), p * p, replace = T), nrow = p) 
    ind <- lower.tri(vc) 
    vc[ind] <- t(vc)[ind] 
    vc <- lqmm::make.positive.definite(vc) # Variance-covariance matrix
    diag(vc) <- 1
    mn <- c(sample(seq(-9, 9, 1), p - pnoise, replace = T), rep(0, pnoise))
    x <- rbind(x, rmvnorm(n = n, mean = mn, vc))
    ncl <- c(ncl, n)
    mncl <- rbind(mncl, mn)
  }
  x <- scale(x)
  x <- as.data.frame(x)
  
  # Reorder rows and columns
  x.indx <- sample(1:nrow(x))
  y.indx <- sample(1:ncol(x))
  x <- x[x.indx, y.indx]
  
  # Mask output after reorder
  rownames(x) <- 1:nrow(x)
  colnames(x) <- paste0("V", 1:ncol(x))
  mncl <- mncl[, y.indx]
  rownames(mncl) <- paste0("cl ", letters[1:nrow(mncl)])
  colnames(mncl) <- paste0("V", 1:ncol(mncl))
  vc <- vc[y.indx, y.indx]
  cluster <- factor(rep(letters[1:cl], ncl))
  cluster <- cluster[x.indx]
  
  # Record attributes
  attr(x, "ncl") <- ncl            # number of obs in each cluster
  attr(x, "mncl") <- mncl          # mean of each cluster*variable
  attr(x, "vc") <- vc              # variance-covariance matrix, after reorder
  attr(x, "cluster") <- cluster    # cluster factor, after reorder
  attr(x, "col_reorder") <- y.indx # order variables were scrambled in
  attr(x, "row_reorder") <- x.indx # order rows were scrambled in 
  return(x)
}

##### App PCA plot
APP_pca_plot <- function(dat, class, in_x, in_y){
  col <- col_of(class)
  pch <- pch_of(class)
  axes <- "bottomleft"
  
  pca <- prcomp(dat)
  pca_x <- data.frame(pca$x)
  pca_rotation <- data.frame(pca$rotation)
  x_axis_num <- as.integer(substr(in_x, 3, 3))
  y_axis_num <- as.integer(substr(in_y, 3, 3))
  basis <- set_axes_position(pca_rotation[, c(x_axis_num, y_axis_num)], axes)
  
  angle <- seq(0, 2 * pi, length = 360)
  circ <- set_axes_position(data.frame(x = cos(angle), y = sin(angle)), axes)
  zero  <- set_axes_position(0, axes)
 
  
  ggplot() + 
    # data points
    geom_point(pca_x, mapping = aes(x = get(in_x), 
                                    y = get(in_y),
                                    color = class,
                                    fill = class,
                                    shape = class)) +
    # axis segments
    geom_segment(basis, 
                 mapping = aes(x = get(in_x), xend = zero[, 1],
                               y = get(in_y), yend = zero[, 2]),
                 size = .3, colour = "grey80") +
    # axis label text
    geom_text(basis, 
              mapping = aes(x = get(in_x), 
                            y = get(in_y), 
                            label = rownames(pca_rotation)), 
              size = 4, colour = "grey50", 
              vjust = "outward", hjust = "outward") +
    # Cirle path
    geom_path(circ, 
              mapping = aes(x = x, y = y),
              color = "grey80", size = .3, inherit.aes = F) +
    # options
    theme_minimal() + 
    theme(aspect.ratio = 1) + # a_ratio) +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.box.background = element_rect(),
          legend.title = element_text(size = 18, face = "bold"),
          legend.text  = element_text(size = 18, face = "bold")
          ) +
    labs(x = in_x, y = in_y)
}


##### Pursuit guided tour path funtion
APP_guided_tour <- function(index, cluster_id = NA){ # returns a tour function
  if(index == "holes"){ return(guided_tour(holes()))}
  if(index == "cmass"){ return(guided_tour(cmass()))}
  if(index == "lda_pp"){return(guided_tour(lda_pp(cluster_id)))}
  if(index == "pda_pp"){return(guided_tour(pda_pp(cluster_id)))}
  else return(error("index not found"))
} #ex # animate_xy(flea[, 1:6], guided_tour(pda_pp(flea$species)), sphere = TRUE)
### END OF PROJECTION PURSUIT

