# ---
# title: "Predictions"
# author: "Matthew J Cashman"
# date: "October 11, 2018"
#  
# ---
  
#Import Data 
root.dir = getwd()
source(paste0(root.dir,"/import_merge_gis_field.R"))

##Load packages
p_load(e1071,caret,corrplot)

#Choose Bed
data <- XS_ws_site %>%
  filter(Class=="Bed")
data_num <- data[,-1:-2]

apply(data_num, 2, skewness)

data_num <- data_num[,-nearZeroVar(data_num)]

data_num <- data_num %>%
   select(-p_dev_high, -p_dev_med, -p_dev_lo, -p_dev_os,
          -S25RANG, -area_imper, -area_canopy, -G25RANG, -S100RANG,
          -range_slopes, -p_herbshrub, -grad_range_100m, -p_ag_sum,
          -G25STD, -S25STD, -p_forest_sum)

trans <- preProcess(as.data.frame(data_num), method = c("BoxCox"))
data_trans <- predict(trans, as.data.frame(data_num))

##Check Collinearity
correlations <- cor(data_trans)
corrplot(correlations, order="hclust")
highCorr <- findCorrelation(correlations, cutoff = 0.75)
length(highCorr)
head(highCorr)
colnames(data[,highCorr])

##Do PCA on transformeddata
ir.pca <- prcomp(data_trans,
                center = TRUE,
                scale. = TRUE) 
library(devtools)
#install_github("ggbiplot", "vqv")
print(ir.pca)
plot(ir.pca, type = "l")
summary(ir.pca)

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1)

print(g)

#Predictions

ggplot(data = data, aes(x=contr_area, y=log(net.rate+1), color=strahler,group=strahler, fill=strahler))+
  geom_point()

p_load(pls)
fit <- plsr(data = data_trans[,-1:-2], ncomp=10,net.rate~., validation="LOO")
summary(fit)
gas2.cv <- crossval(fit, segments = 10) 
plot(MSEP(gas2.cv), legendpos="topright")
summary(gas2.cv, what = "validation")

ncomp.onesigma <- selectNcomp(fit, method = "onesigma", plot = TRUE,  ylim = c(0.1, .3)) 
ncomp.permut <- selectNcomp(fit, method = "randomization", plot = TRUE, ylim = c(0.1, .3)) 
