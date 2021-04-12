### Learn how tro control the text abnd position of significance test better.
if(F)
  browseURL("http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/")

library("ggpubr")
data("ToothGrowth")
head(ToothGrowth)

## tibble of signif tests
compare_means(len ~ supp, data = ToothGrowth)

## ggplot with signif tests:
p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")

## Add global p-value
p + stat_compare_means()


## setting position and text
p + stat_compare_means( aes(label = paste0( "blah", ..p.signif..)), 
                        label.x = 1.5, label.y = 40)

## custom label:
p + stat_compare_means( aes(label = paste0("my lab: ", ..p.signif..)), 
                        label.x = 1.5, label.y = 40)



# Load myeloma data from GitHub
myeloma <- read.delim("https://raw.githubusercontent.com/kassambara/data/master/myeloma.txt")
# Perform the test
compare_means(DEPDC1 ~ molecular_group,  data = myeloma,
              ref.group = ".all.", method = "t.test")
## # A tibble: 7 x 8
##      .y. group1           group2        p   p.adj p.format p.signif method
##                                   
## 1 DEPDC1  .all.       Cyclin D-1 0.149690 0.44907  0.14969       ns T-test
## 2 DEPDC1  .all.       Cyclin D-2 0.523143 1.00000  0.52314       ns T-test
## 3 DEPDC1  .all.     Hyperdiploid 0.000282 0.00169  0.00028      *** T-test
## 4 DEPDC1  .all. Low bone disease 0.005084 0.02542  0.00508       ** T-test
## 5 DEPDC1  .all.              MAF 0.086107 0.34443  0.08611       ns T-test
## 6 DEPDC1  .all.            MMSET 0.576291 1.00000  0.57629       ns T-test
## # ... with 1 more rows
# Visualize the expression profile
ggboxplot(myeloma, x = "molecular_group", y = "DEPDC1", color = "molecular_group", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(myeloma$DEPDC1), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 1600)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "wilcox.test",
                     ref.group = ".all.")                      # Pairwise comparison against all
