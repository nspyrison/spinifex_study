### Global init for each factor variant.


##### App simulation global.r -----
##### Initialization -----
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("mvtnorm")
library("plotly")


load_num <- 1
load_name <- sprintf("simulation_data%03d", load_num)
load_file <- paste0("./", load_name, ".rds") # "./apps/simulation/"
#local_file <- paste0("./apps/simulation/", load_name, ".rds"); assign(load_name, readRDS(local_file))
while (file.exists(load_file)){
  assign(load_name, readRDS(load_file))
  load_num <- load_num + 1
  load_name <- sprintf("simulation_data%03d", load_num)
  load_file <- paste0("./", load_name, ".rds")
}
loaded_sim_names <- ls()[grepl("simulation_data", ls())]

load_choices <- if (length(loaded_sim_names) > 0) {loaded_sim_names
} else {"<no 'simulation_dataNNN.rds' loaded>"} 

###### Simulate clusters
simulate_clusters <- function(p = 10, pnoise = 4, cl = 4){
  #p = 10; pnoise = 4; cl = 4
  x <- NULL
  ncl <- NULL
  mncl <- NULL
  vc <- NULL
  for (i in 1:cl) {
    n <- sample(30:150, 1)
    ### TODO: double check that vc is doing what we want.
    # vc_original <- matrix(sample(seq(-0.1, 0.7, by = 0.1), 1), nrow = p, ncol = p) # Add some association
    # print(vc_original) # not as expected
    ### making vc random, symetric, definite matrix:
    vc <- matrix(sample(seq(-.1, 0.7, by = 0.1), p * p, replace = T), nrow = p) 
    ind <- lower.tri(vc) 
    vc[ind] <- t(vc)[ind] 
    vc <- lqmm::make.positive.definite(vc) # Variance-covariance matrix
    diag(vc) <- 1
    mn <- c(sample(seq(-3, 3, 1), p - pnoise, replace = T), rep(0, pnoise))
    x <- rbind(x, rmvnorm(n = n, mean = mn, vc))
    ncl <- c(ncl, n)
    mncl <- rbind(mncl, mn)
  }
  x <- scale(x)
  x <- as.data.frame(x)
  
  # Show color on plots to check clustering
  cluster <- factor(rep(letters[1:cl], ncl))
  cluster_col <- col_of(cluster)
  
  # Scramble rows and columns
  x.indx <- sample(1:nrow(x))
  y.indx <- sample(1:ncol(x))
  x <- x[x.indx, y.indx]
  cluster <- cluster[x.indx]
  
  attr(x, "ncl") <- ncl            # number of obs in each cluster
  attr(x, "mncl") <- mncl          # mean of each cluster*variable
  attr(x, "vc") <- vc              # variance-covariance matrix
  attr(x, "cluster") <- cluster    # cluster factor
  attr(x, "col_reorder") <- y.indx # order variables were scrambled in
  return(x)
}

##### App PCA plot -----
app_PCA_plot <- function(dat, class, in_x, in_y){
  col <- col_of(class)
  pch <- pch_of(class)
  
  pca <- prcomp(dat)
  pca_x <- data.frame(pca$x)
  pca_rotation <- set_axes_position(data.frame(t(pca$rotation)), 
                                    "bottomleft")
  rot_x_axis <- paste0("V", substr(in_x, 3, 3))
  rot_y_axis <- paste0("V", substr(in_y, 3, 3))
  
  angle <- seq(0, 2 * pi, length = 360)
  circ <- set_axes_position(data.frame(x = cos(angle), y = sin(angle)), 
                            "bottomleft")
  zero  <- set_axes_position(0, "bottomleft")
  #x_range <- max(x) - min(x)
  #y_range <- max(y) - min(y)
  #a_ratio <- x_range / y_range
  
  ggplot() + 
    # data points
    geom_point(pca_x, mapping = aes(x = get(in_x), 
                                    y = get(in_y)),
               color = col, fill = col, shape = pch) +
    # axis segments
    geom_segment(pca_rotation, 
                 mapping = aes(x = get(rot_x_axis), xend = zero,
                               y = get(rot_y_axis), yend = zero),
                 size = .3, colour = "grey80") +
    # axis label text
    geom_text(pca_rotation, 
              mapping = aes(x = get(rot_x_axis), 
                            y = get(rot_y_axis), 
                            label = colnames(pca_rotation)), 
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
          legend.position = 'none') +
    labs(x = in_x, y = in_y)
}


##### tabPanels (UI objs) -----
### Task/generate panels
panel_generate <- tabPanel(
  "generate simulation", 
  sidebarPanel(
    numericInput("sim_p", label = "Number of variables, p", value = 10)
    , numericInput("sim_pnoise", label = "Number of noise variables, pnoise", value = 4)
    , numericInput("sim_cl", label = "Number of clusters, cl", value = 4)
    , fluidRow(column(6, radioButtons(inputId = "load_x_axis", 
                                      label = "x axis", choices = "PC1")),
               column(6, radioButtons(inputId = "load_y_axis", 
                                      label = "y axis", choices = "PC2")))
    , hr() # horizontal line
    , actionButton("save_sim", "save simulation")
    , verbatimTextOutput("save_msg")
  ),
  mainPanel(plotOutput("task_pca")
            , plotlyOutput("task_gtour", height = 600)
  )
)

### Review panel
panel_review <- tabPanel(
  "review simulation",
  sidebarPanel(
    selectInput("load_sim_name", "Simulation to review",
                choices = load_choices)
    , fluidRow(column(6, radioButtons(inputId = "task_x_axis", 
                                      label = "x axis", choices = "PC1")),
               column(6, radioButtons(inputId = "task_y_axis", 
                                      label = "y axis", choices = "PC2"))
    )
    , hr() # horizontal line
    , verbatimTextOutput("load_dat_attr")
    , verbatimTextOutput("load_sim_name")
    , verbatimTextOutput("str_load_dat")
  ),
  mainPanel(plotOutput("load_pca")
            , plotlyOutput("load_gtour", height = 600)
            , h4("Cluter means, display order")
            , verbatimTextOutput("load_mncl_reord")
            , verbatimTextOutput("load_sum_squares")
            , h4("variance-covariance, display order")
            , verbatimTextOutput("load_vc_reord")
  )
)


##### UI, combine panels -----
ui <- fluidPage(
  navbarPage("Multivariate data visualization study"
             , panel_generate
             , panel_review
  )
  , verbatimTextOutput("dev_msg")
)
