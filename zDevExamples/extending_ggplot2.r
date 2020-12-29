### Example of extending ggplot2
## Following along with
if(F){
  browseURL("https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html")
  browseURL("https://ggplot2-book.org/programming.html")
  browseURL("https://ggplot2-book.org/spring1.html")
}

require("ggplot2")

## 1) make a CammelCase stat
## 2) make a geom wrapper
## ==---==
## remember: only the mapping aes() uses NSE, will more likely want to use
## aes_(substitute(arg)).
## may also need some sorts of %*% opperators.

geom_mean <- function() {
  list(
    stat_summary(fun = "mean", geom = "bar", fill = "grey70"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4)
  )
}
ggplot(mpg, aes(class, cty)) + geom_mean()
ggplot(mpg, aes(drv, cty)) + geom_mean()
