require(tidyverse)
require(ggpubr)
library("FactoMineR")
library("factoextra")

test <- read_csv("DEQ_2018_Probmondata.csv")

Phab_var <- read_csv("Phab_variables.csv")

deq_filter <- test %>% select(Phab_var$Variable)

deq_pca <- deq_filter %>% select(-StationID, -Basin, -SubBasin, -BayShed, -EcoRegion, -reference)
# filter out character class variables

res.pca <- PCA(deq_pca, scale.unit = TRUE, graph = FALSE)
#consider missing values, maybe

fviz_pca_ind(res.pca, geom.ind = "point")
# this plot is probably currently not necessary
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
# scree plot
var <- get_pca_var(res.pca)
var <- var$contrib

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# look at contributing variables to PC1

cor(deq_filter$N_INDEX, deq_filter$VSCIVCPMI)

cor(deq_filter$PFOR, deq_filter$VSCIVCPMI)

cor(deq_filter$PAGT, deq_filter$VSCIVCPMI)

cor(deq_filter$U_INDEX, deq_filter$VSCIVCPMI)

cor(deq_filter$SLPMEAN, deq_filter$VSCIVCPMI)
# correlate highest contributing variables to see how they drive scores with VSCI/CPMI

ggscatter(deq_filter, x = "N_INDEX", y = "VSCIVCPMI")
ggscatter(deq_filter, x = "N_INDEX", y = "VSCIVCPMI", color = "Basin")

ggboxplot(deq_filter, x = "Basin", y = "N_INDEX")

deq_filter <- deq_filter %>% mutate(reference = if_else(VSCIVCPMI > 60, "Reference", "Impaired"))
# create reference column for boxplotting of stream condition. 
