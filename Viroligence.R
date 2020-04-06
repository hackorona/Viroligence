# Cases plot ####

require(devtools)
devtools::install_github("covid19r/coronavirus")
require(coronavirus) 

#data("coronavirus")

# https://github.com/RamiKrispin/coronavirus-csv/blob/master/coronavirus_dataset.csv
coroni <- read.csv("Downloads/coronavirus_dataset.csv")

require(ggplot2)
ggplot(coroni[(coroni$Country.Region=="Israel" & coroni$type=="confirmed"),], 
       aes(x=date,y=cumsum(cases), group=factor(type)#, colour=factor(type)
           )) +
  #stat_summary(fun = sum, geom="line",size=1) #+
  geom_line()


# Simulate data ####
require(PredictABEL)
data("ExampleData")

# Israel population stats

# reproducible random numbers
set.seed(2205)

# source: https://en.wikipedia.org/wiki/Demographics_of_Israel
Background <- c(rep("Jew",0.7424*100000),rep("Arab",0.2095*100000),rep("Other",0.0481*100000))
Gender <- c(rep("Male",1.01/2*1e5),rep("Female",(1-1.01/2)*1e5))

# model age distribution
(dat <- data.frame(min=seq(0,100,5), max=seq(4,104,5), prop=c(0.103,0.093,0.0855,0.0776,0.0746,
                                                              0.0733,0.0702,0.0682,0.0593,
                                                              0.0516,0.0494,0.0473,0.0441,0.0311,
                                                              0.0238,0.0198,0.0144,0.0091,
                                                              0.0034,0.0009,0.0003)))
# min max  prop
# 1   0  14 0.273
# 2  15  64 0.622
# 3  65 103 0.105

rows <- sample(nrow(dat), 1e4, replace=TRUE, prob=dat$prop)
Age <- round(dat$min[rows] + runif(1e4) * (dat$max[rows] - dat$min[rows]))
ExampleData$Age <- Age #sample(Age, 1e4, replace = F)
ExampleData$Background <- sample(Background,10000,replace = F)
ExampleData$Sex <- sample(Gender, 1e4, replace = F)



# Define plotting theme ####
require(extrafont)

theme_corla <- function(base_size = 12, base_family = "Palatino", legend_position = "right",
                        title_size= base_size){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(legend.position = legend_position, legend.background = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "#000000"), 
          axis.ticks = element_line(colour = "#000000"),
          legend.key = element_blank(),
          axis.text = element_text(size = base_size, face="bold"), 
          plot.title=element_text(face="bold", size = title_size),
          axis.title = element_text(face="bold", size = base_size), 
          legend.text=element_text(size = base_size, face="bold"),
          legend.title=element_text(face="bold", size = base_size), 
          strip.text=element_text(face="bold", size = base_size)
    )
}

# Define color scheme ####
hackorcolors <- c("#FF2372","#FFA116","#FFCD33")

# Age histogram ####
p1 <- ggplot(ExampleData, aes(Age)) +
  geom_histogram(bins=22, fill=hackorcolors[2],color="#000000") +
  labs(title  = "Israeli age distribution", # plot title
       x      = "Age (years)", # x-axis label
       y      = "Count") + # y-axis label
  theme_corla()

p1

ggsave(filename = paste0("AgeHistogram.pdf"), 
       width=100, height=100*9/16, units="mm", dpi=150, useDingbats=FALSE)



# model BMI distribution
# https://ijhpr.biomedcentral.com/articles/10.1186/2045-4015-1-17/tables/2
(dat <- data.frame(min=seq(18,43,1), max=seq(19,44,1), 
                   prop=c(0.024,0.025,0.046,0.053,0.072,0.087,0.098,0.098,0.089,
                          0.082,0.072,0.06,0.049,0.031,0.024,0.025,0.014,0.015,0.011,
                          0.0078,0.0039,0.0035,0.0023,0.0012,0.0019,0.0055)))
# min max  prop
# 1   0  14 0.273
# 2  15  64 0.622
# 3  65 103 0.105

rows <- sample(nrow(dat), 1e4, replace=TRUE, prob=dat$prop)
BMI <- round(dat$min[rows] + runif(1e4) * (dat$max[rows] - dat$min[rows]))
ExampleData$BMI <- BMI #sample(Age, 1e4, replace = F)

# BMI histogram ####
p2 <- ggplot(ExampleData, aes(BMI)) +
  geom_histogram(bins=27, fill=hackorcolors[2],color="#000000") +
  labs(title  = "Israeli BMI distribution", # plot title
       x      = "BMI (kg/m^2)", # x-axis label
       y      = "Count") + # y-axis label
  theme_corla()

p2

ggsave(filename = paste0("BMI_Histogram.pdf"), 
       width=100, height=100*9/16, units="mm", dpi=150, useDingbats=FALSE)


# model fratality distribution
set.seed(2205)
Death <- ifelse(ExampleData$Age < 10, 0,
                   ifelse(ExampleData$Age < 40, rbinom(n=1, size=1, prob=0.002),
                          ifelse(ExampleData$Age < 50, rbinom(n=1, size=1, prob=0.004),
                                 ifelse(ExampleData$Age < 60, rbinom(n=1, size=1, prob=0.013),
                                        ifelse(ExampleData$Age < 70, rbinom(n=1, size=1, prob=0.036),
                                               ifelse(ExampleData$Age < 80, rbinom(n=1, size=1, prob=0.08),
                                                      rbinom(n=1, size=1, prob=0.148))
                                        )
                                  )
                          )
                   ))

probs <- ifelse(ExampleData$Age < 10, 0,
                ifelse(ExampleData$Age < 40, 0.002,
                       ifelse(ExampleData$Age < 50, 0.004,
                              ifelse(ExampleData$Age < 60, 0.013,
                                     ifelse(ExampleData$Age < 70, 0.036,
                                            ifelse(ExampleData$Age < 80, 0.08,
                                                   0.148)
                                     )
                              )
                       )
                ))

set.seed(1710)
Death <- sapply(probs, function(x) rbinom(n=1, size=1, prob=x)) 

ExampleData$Death <- Death

colnames(ExampleData)[2] <- "Death"

ExampleData$Cat <- ifelse(ExampleData$Death==1, "Dead", "Alive")

ExampleData$DiscAge <- cut(ExampleData$Age, 8, include.lowest=TRUE)
ExampleData$DiscAge <- factor(ExampleData$DiscAge, 
                              levels=c("[-0.104,13]", "(13,26]", "(26,39]", "(39,52]",
                                       "(52,65]", "(65,78]", "(78,91]", "(91,104]"),
                              labels=c("[0,13]", "(13,26]", "(26,39]", "(39,52]",
                                       "(52,65]", "(65,78]", "(78,91]", "(91,104]"))

# Fratality balloon plot ####
balloon <- as.data.frame(table(ExampleData$DiscAge,ExampleData$Death))


require(ggpubr)
p3 <- ggballoonplot(balloon, fill=hackorcolors[2], size.range = c(1, 10)) + 
 theme_corla() +
  labs(title  = "Corona mortality distribution", # plot title
       x      = "Years of age range", # x-axis label
       y      = NULL) + # y-axis label
  scale_y_discrete(labels=rev(c("Alive","Dead"))) +
  theme(axis.text.x = element_text(angle=30,hjust=1)) 

p3

ggsave(filename = paste0("Death_Histogram.pdf"), 
       width=120, height=120*9/16, units="mm", dpi=150, useDingbats=FALSE)



# Readjust data ####
# adjust to factor
col_names <- c("BaselineAMD","Smoking")
ExampleData[col_names] <- lapply(ExampleData[col_names] , factor)
colnames(ExampleData)[c(6,9)] <- c("LungDiseaseHistory","ACE2")
ExampleData <- ExampleData[,c(1:17)]

# Train model ####

# specify column number of the outcome variabl
cOutcome <- 2
# specify column numbers of non-genetic predictors
cNonGenPred1 <- c(3:10,17)
cNonGenPred2 <- c(4:10,17)
# specify column numbers of non-genetic predictors that are categorical
cNonGenPredCat1 <- c(6:7,17)
cNonGenPredCat2 <- c(6:7,17)
# specify column numbers of genetic predictors
cGenPred1 <- c(11:16)
cGenPred2 <- c(11:16)
# specify column numbers of genetic predictors that are categorical
cGenPredsCat1 <- c(0)
cGenPredsCat2 <- c(0)

# fit logistic regression models
riskmodel1 <- fitLogRegModel(data=ExampleData, cOutcome=cOutcome,cNonGenPreds=cNonGenPred1, 
                             cNonGenPredsCat=cNonGenPredCat1,cGenPreds=cGenPred1, 
                             cGenPredsCat=cGenPredsCat1)
riskmodel2 <- fitLogRegModel(data=ExampleData, cOutcome=cOutcome,cNonGenPreds=cNonGenPred2, 
                             cNonGenPredsCat=cNonGenPredCat2,cGenPreds=cGenPred2, 
                             cGenPredsCat=cGenPredsCat2)

# show summary details for the fitted risk model
summary(riskmodel1)
summary(riskmodel2)

# obtain multivariate OR(95% CI) for all predictors of the fitted model
ORmultivariate(riskModel=riskmodel1)


# obtain predicted risks
predRisk1 <- predRisk(riskmodel1)
predRisk2 <- predRisk(riskmodel2)

# Hosmer-Lemeshow goodness of fit test ####
HLtest <- as.data.frame(out$Table_HLtest)

pearson <- cor.test(HLtest$meanpred,HLtest$meanobs,method = "pearson")

p4 <- ggplot(HLtest, aes(meanpred, meanobs),colour="#000000") +
  geom_smooth(method = lm, se = T ,fill=hackorcolors[3], colour="#000000") +
  geom_point(shape=21,fill=hackorcolors[2],size=2) +
  annotate("text", x = 0.0125, y = 0.085, label = paste0("r=",round(pearson$estimate,3)),
          family="Palatino")+
  labs(title  = "Hosmer-Lemeshow goodness of fit", # plot title
       x      = "Predicted risk", # x-axis label
       y      = "Observed risk") + # y-axis label
  theme_corla()
p4

ggsave(filename = paste0("GoodnessOfFit.pdf"), 
       width=100, height=100*3/4, units="mm", dpi=150, useDingbats=FALSE)


# specify label of the ROC curve
labels <- c("w/ age", "w/o age")
# produce ROC curve
plotROC(data=ExampleData, cOutcome=cOutcome,
        predrisk=cbind(predRisk1,predRisk2), labels=labels)
# AUC [95% CI] for the model 1 :  0.901 [ 0.876  -  0.926 ] 
# AUC [95% CI] for the model 2 :  0.59 [ 0.544  -  0.636 ] 



# prediction of risk for new patient ####
# input from Viroligence App
newPatient <- ExampleData[5,]
newPatient$ID <- 10001
newPatient$ACE2 <- 2
newPatient$Smoking <- "2"

# export predicted risk
predRisk(riskModel=riskmodel1, data=newPatient, cID=1, filename="out.txt")


