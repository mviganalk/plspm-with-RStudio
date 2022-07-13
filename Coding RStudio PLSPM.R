require(plspm)
library(plspm)
attach(DATA_PLSM_EVAL)


DATA_PLSPM_EVAL = read.csv(file.choose(DATA_PLSPM_EVAL),header = T)
View(DATA_PLSPM_EVAL)
str(DATA_PLSPM_EVAL)
dim(DATA_PLSPM_EVAL)
summary(DATA_PLSPM_EVAL[,1:14])

#Variabel laten
#PEOU = Perceived Ease of Use
#PU   = Perceived Usefulness
#ATU  = Attitude Toward Using
#ITU  = Intention To Use
#AU   = Actual Usage

#Questionare Indicators
X1.1 = "Perceived Usefulness1"
X1.2 = "Perceived Usefulness2"
X1.3 = "Perceived Usefulness3"
X1.5 = "Perceived Usefulness5"

X2.1 = "Perceived Ease of Use1"
X2.2 = "Perceived Ease of Use2"
X2.3 = "Perceived Ease of Use3"

X3.1  = "Attitude Toward Using1"
X3.2  = "Attitude Toward Using2"

X4.1  = "Intention To Use1"
X4.2  = "Itention To Use2"
X4.3  = "Intention To Use3"

X5.1   = "Actual Usage1"
X5.2   = "Actual Usage2"

#Put Questions

Perceived Ease of Use = c(X2.1, X2.2, X2.3)
Perceived Usefulness  = c(X1.1, X1.2, X1.3, X1.5)
Attitude Toward Using = c(X3.1, X3.2)
Intention To Use      = c(X4.1, X4.2, X4.3)
Actual Usage          = c(X5.1, X5.2)


#building the inner model

PEOU = c(0,0,0,0,0)
PU   = c(1,0,0,0,0)
ATU  = c(1,1,0,0,0)
ITU  = c(0,1,1,0,0)
AU   = c(0,0,0,1,0)

plsmodel = rbind(PEOU, PU, ATU, ITU, AU)
colnames(plsmodel) = rownames(plsmodel)
innerplot(plsmodel, colpos = "#6890c4BB", colneg = "#f9675dBB",
          box.prop = 0.55, box.size = 0.08, box.cex = 1,
          box.col = "gray95", lcol = "gray95", box.lwd = 2,
          txt.col = "gray50", shadow.size = 0, curve = 0,
          lwd = 3, arr.pos = 0.5, arr.width = 0.2, arr.lwd = 3,
          cex.txt = 0.9, show.values = FALSE)

#building the outer model
Plsmodel_blocks = list (5:7,1:4,8:9,10:12,13:14)
Plsmodel_blocks

Plsmodel_modes = c("A","A","A","A","A")
Plsmodel_modes
mydata_pls =plspm(DATA_PLSPM_EVAL, plsmodel, Plsmodel_blocks,
                  modes = Plsmodel_modes)
mydata_pls

#check unidimensionality
mydata_pls$unidim
summary(mydata_pls)

#check outer_model
mydata_pls$outer_model

#check cross_loading
mydata_pls$crossloadings

#check inner_model
mydata_pls$inner_model

#check inner_Summary
mydata_pls$inner_summary

#check goodness of fit
mydata_pls$gof

#check Bootstrapping
mydata_pls$boot

#check effects paths
mydata_pls$path_coefs
