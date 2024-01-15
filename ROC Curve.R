################################################
# ROC Curve in R with ggplot2
################################################

Dt <- read.csv('wiedat2b.csv')
head(Dt)

require(plotROC)
require(ggplot2)
# Version 1
p_ROC1 <- 
  ggplot(Dt, aes(d = Pancreatic_Cancer,            # true disease label
                 m = CA_19.9)) +                   # continuous biomarker
  geom_roc(labelsize = 3) +                      # draw ROC curve
  geom_abline(slope = 1, intercept = 0) +        # add a unit slope line
  style_roc(xlab = 'False Positive Fraction',    # modify x/y-axis labels
            ylab = 'True Positive Fraction') + 
  theme(axis.text.x = element_text(color = 'black', size = 11),    
        axis.text.y = element_text(color = 'black', size = 11),
        axis.title.x = element_text(color = 'black', size = 11, face = 'bold'),                              
        axis.title.y = element_text(color = 'black', size = 11, face = 'bold'))
p_ROC1

# Version 2
p_ROC2 <- 
  ggplot(Dt, aes(d = Pancreatic_Cancer,            # true disease label
                 m = CA_19.9)) +                   # continuous biomarker
  geom_roc(n.cuts = 5) +                         # draw ROC curve
  geom_rocci(ci.at = 21.8) +                     # draw 95% CI for a selected point
  geom_abline(slope = 1, intercept = 0) +        # add a unit slope line
  style_roc(xlab = 'False Positive Fraction',    # modify x/y-axis labels
            ylab = 'True Positive Fraction') + 
  theme(axis.text.x = element_text(color = 'black', size = 11),    
        axis.text.y = element_text(color = 'black', size = 11),
        axis.title.x = element_text(color = 'black', size = 11, face = 'bold'),                              
        axis.title.y = element_text(color = 'black', size = 11, face = 'bold'))
p_ROC2

require(ggthemes)
# Obtain the long version of dataset and modify columns/labels 
Dt_long <- melt_roc(Dt, d = 'Pancreatic_Cancer', c('CA_19.9', 'CA_125'))
colnames(Dt_long) <- c('Pancreatic Cancer', 'Level', 'Biomarker')
Dt_long$Biomarker <- ifelse(Dt_long$Biomarker == 'CA_19.9', 'CA 19-9', 'CA 125')

# Version 3
p_ROC3 <- 
  ggplot(Dt_long, aes(d = `Pancreatic Cancer`,     # true disease label
                      m = Level,                   # continuous biomarker level
                      col = Biomarker)) +          # color for different biomarkers
  geom_roc(pointsize = 0, labels = F) +          # draw ROC curve and remove labels/points
  geom_rocci(ci.at = 21.8) + 
  geom_abline(slope = 1, intercept = 0) +        # add a unit slope line
  style_roc(xlab = 'False Positive Fraction',    # modify x/y-axis labels
            ylab = 'True Positive Fraction') + 
  scale_color_tableau() + 
  theme(axis.text.x = element_text(color = 'black', size = 11),    
        axis.text.y = element_text(color = 'black', size = 11),
        axis.title.x = element_text(color = 'black', size = 11, face = 'bold'),                              
        axis.title.y = element_text(color = 'black', size = 11, face = 'bold'))
p_ROC3

# Using pROC package to calculate AUC
require(pROC)
# Classification model: logistic regression
model <- glm(Pancreatic_Cancer ~ CA_19.9, family = binomial(), data = Dt)
# Predicted probability
predProb <- predict(model, newdata = Dt, type = 'response')
# Combined prob with real labels
Result <- data.frame('Predicted_Probability' = predProb, 
                     'Pancreatic_Cancer' = Dt$Pancreatic_Cancer)
# Calculate AUC
AUC <- round(auc(response = Result$Pancreatic_Cancer, 
                 predictor = Result$Predicted_Probability), 2)

# Version 4
p_ROC4 <- 
  ggplot(Result, aes(d = Pancreatic_Cancer,            # true disease label
                     m = Predicted_Probability)) +     # continuous biomarker
  geom_roc(n.cuts = 0) +                             # draw ROC curve
  geom_abline(slope = 1, intercept = 0) +            # add a unit slope line
  annotate("text", x = .75, y = .25,                 # add AUC score
           label = paste("AUC =", AUC)) + 
  style_roc(xlab = 'False Positive Fraction',        # modify x/y-axis labels
            ylab = 'True Positive Fraction') + 
  theme(axis.text.x = element_text(color = 'black', size = 11),    
        axis.text.y = element_text(color = 'black', size = 11),
        axis.title.x = element_text(color = 'black', size = 11, face = 'bold'),                              
        axis.title.y = element_text(color = 'black', size = 11, face = 'bold'))
p_ROC4

















