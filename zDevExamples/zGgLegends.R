library(spinifex)
library(ggplot2)

d <- mtcars
this_col <- col_of(d$gear)
this_pch <- pch_of(d$gear)


# Vlaues in aes
ggplot(data=d, aes(x=cyl, y=mpg, col=this_col)) + 
  geom_point() + theme_minimal()

# Vlaues out of aes
ggplot(data=d, aes(x=cyl, y=mpg), col=this_col) + 
  geom_point() + theme_minimal()


# Col in aes
ggplot(data=d, aes(x=cyl, y=mpg, col=gear)) + 
  geom_point() + theme_minimal()

# Col out of aes
ggplot(data=d, aes(x=cyl, y=mpg), col=gear) + 
  geom_point() + theme_minimal()


# Playing with interaction with pch., 
ggplot(data=d, aes(x=cyl, y=mpg, col=as.factor(gear))) + 
  geom_point() + theme_minimal() + 
  theme(#legend.key.size = unit(.2, "cm"),
        legend.box.background = element_rect(),
        legend.title = element_text(colour="blue", size=10, face="bold"),
        legend.text = element_text(colour="red", size=10, face="bold")
  ) +
  if (T) {labs(col="ANOther" )}

### CONDITIONAL USE OF ARGS IN A FUNCTION CALL
# https://stackoverflow.com/questions/13992367/conditional-inclusion-of-arguments-in-a-function-call
do.call(FUN, c(list(arg1 = x1, arg2 = x2),   # unconditional args
               list(arg3 = x3)[use.arg3]))   # conditional arg


# default
ggplot(data=d, 
       aes(x=cyl, y=mpg, col=as.factor(gear), pch =as.factor(gear))
) + 
  geom_point() + theme_minimal() 

# conditional use of args. Stange things happen with the legend
## need to go to a piecemeal approach?
USE_COL <- T
USE_PCH <- T
ggplot(data=d,
       do.call(aes, c(list(x=d$cyl, y=d$mpg),
                      list(col=as.factor(d$gear))[USE_COL],
                      list(pch=as.factor(d$gear))[USE_PCH])
       )
) + 
  geom_point() + theme_minimal() 

(a <- ggplot(data=d) + 
    geom_point(aes(x=cyl, y=mpg)) + theme_minimal() 
)
(a <- a + geom_point(aes(x=cyl, y=mpg, col=as.factor(gear), pch =as.factor(gear))) )
