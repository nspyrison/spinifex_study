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
