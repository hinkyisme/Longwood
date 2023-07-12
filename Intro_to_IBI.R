require(tidyverse)
require(ggpubr)
require(vegan)
#dependencies

data(BCI)

data(BCI.env)

simpson <- diversity(BCI, "simpson")
# get simpson diversity index measures of trees in BCI dataset.

trees <- bind_cols(BCI.env, simpson)
#combine environmental variables with plant/tree data.

cor(trees$EnvHet, trees$...10)
#correlate Simpon indexed Environmnetal Heterogenity with Simpson index for trees.

p <- ggboxplot(trees, x = "Habitat", y = "`...10`")
#look for differences in simpson index based off habitat.

p + stat_compare_means(method = "anova")

p1 <- ggboxplot(trees, x = "Stream", y = "`...10`")
#look for differences in simpson index based off stream side habitat.

p1 + stat_compare_means(method = "t.test")