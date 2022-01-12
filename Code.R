### Overview
# Read in data
NBA <- read.csv("Lei_3483.csv")
# Examine outliers
boxplot(NBA$TOV., col = "purple", border = T, main = "Turnover Rate", ylab = "percentage")
summary(NBA$TOV.)
boxplot(NBA$TS., col = "darkred", border = T, main = "True Shooting Percentage", ylab = "percentage")
summary(NBA$TS.)
# Removing outliers
NBA <- NBA[NBA$TS. != 0, ]
NBA <- NBA[NBA$TRB. != 0, ]
NBA <- NBA[NBA$AST. != 0, ]
NBA <- NBA[NBA$USG. != 0, ]
NBA <- NBA[NBA$TOV. != 0, ]
NBA <- NBA[NBA$Height != 0, ]
NBA <- NBA[NBA$G > 15, ]
# Duplicating the dataset for future usage
NBA_Dupe <- NBA
# Select some attributes to do cluster analysis
NBA <- NBA[,c("TS.", "TRB.", "AST.", "USG.", "TOV.", "Height", "position_factor", "Dcontri_factor")]

### Nonprobabilistic K-means
# Initializing the centers for each cluster
c1=c(0.51,6,20,20.4,14,76) # Guard
c2=c(0.53,10,9,17.7,12,81)  # Forward
c3=c(0.55,16,6,18.3,14.5,83) # Center
indicator <- c(1:477)
pastIndicator <- c(477:1)
X <- as.matrix(NBA[,c(1:6)])
while(sum(pastIndicator!=indicator)!=0) {
  pastIndicator=indicator;
  #distance to current cluster centers
  dc1 =colSums((t(X)-c1)^2)
  dc2=colSums((t(X)-c2)^2)
  dc3=colSums((t(X)-c3)^2)
  dMat=matrix(c(dc1,dc2,dc3),ncol=3)
  #decide which cluster each point belongs to 
  indicator = max.col(-dMat)
  # update the cluster centers
  c1=colMeans(X[indicator==1,])
  c2=colMeans(X[indicator==2,])
  c3=colMeans(X[indicator==3,])
}
c1
c2
c3
indicator
# Summary for nonprobabilistic K-means
X_Kmeans <- cbind(X, indicator)
X_Guard <- X_Kmeans[X_Kmeans[,"indicator"] == 1,]
X_Forward <- X_Kmeans[X_Kmeans[,"indicator"] == 2,]
X_Center <- X_Kmeans[X_Kmeans[,"indicator"] == 3,]
# Guard
sum_guard <- summary(X_Guard)
summary_guard <- c()
for (i in 1:nrow(sum_guard[c(3,4),])) {
  split_temp <- strsplit(sum_guard[c(3,4),][i,], ":")
  vec_temp <- c()
  for (i in 1:7) {
    vec_temp <- c(vec_temp, as.numeric(split_temp[[i]][2]))
  }
  summary_guard <- rbind(summary_guard, vec_temp)
}
summary_guard <- rbind(summary_guard, round(apply(X_Guard, 2, sd), 3))
rownames(summary_guard) <- c("Median", "Mean", "SD")
summary_guard <- data.frame(summary_guard[,-7])
# Forward
sum_forward <- summary(X_Forward)
summary_forward <- c()
for (i in 1:nrow(sum_forward[c(3,4),])) {
  split_temp <- strsplit(sum_forward[c(3,4),][i,], ":")
  vec_temp <- c()
  for (i in 1:7) {
    vec_temp <- c(vec_temp, as.numeric(split_temp[[i]][2]))
  }
  summary_forward <- rbind(summary_forward, vec_temp)
}
summary_forward <- rbind(summary_forward, round(apply(X_Forward, 2, sd), 3))
rownames(summary_forward) <- c("Median", "Mean", "SD")
summary_forward <- data.frame(summary_forward[,-7])
# Center
sum_center <- summary(X_Center)
summary_center <- c()
for (i in 1:nrow(sum_center[c(3,4),])) {
  split_temp <- strsplit(sum_center[c(3,4),][i,], ":")
  vec_temp <- c()
  for (i in 1:7) {
    vec_temp <- c(vec_temp, as.numeric(split_temp[[i]][2]))
  }
  summary_center <- rbind(summary_center, vec_temp)
}
summary_center <- rbind(summary_center, round(apply(X_Center, 2, sd), 3))
rownames(summary_center) <- c("Median", "Mean", "SD")
summary_center <- data.frame(summary_center[,-7])
summary_guard
summary_forward
summary_center

# Plot the clusters
# True shooting % vs. assist %
plot(X[,1], X[,3], col=c("red","purple","darkgreen")[indicator],
     pch=c(16,16,16)[unclass(indicator)],
     main="Non-probabilistic K-means classification",xlab="True Shooting %", ylab="Assist %")
legend("topleft",c("Guards","Forwards","Centers"),
       col=c("red","purple","darkgreen"),pch=c(16,16,16)[unclass(indicator)])

# Total rebound % vs. assist %
plot(X[,2], X[,3], col=c("red","purple","darkgreen")[indicator],
     pch=c(16,16,16)[unclass(indicator)],
     main="Non-probabilistic K-means classification",xlab="Total rebound %", ylab="Assist %")

legend("topleft",c("Guards","Forwards","Centers"),
       col=c("red","purple","darkgreen"),pch=c(16,16,16)[unclass(indicator)])

# Assist % vs. Turnover %
plot(X[,3], X[,5], col=c("red","purple","darkgreen")[indicator],
     pch=c(16,16,16)[unclass(indicator)],
     main="Non-probabilistic K-means classification",xlab="Assist %", ylab="Turnover %")
legend("topleft",c("Guards","Forwards","Centers"),
       col=c("red","purple","darkgreen"),pch=c(16,16,16)[unclass(indicator)])

# Distribution of players' actual positions in our clusters
NBA_New <- NBA
NBA_New$Prediction <- indicator
Pred_pos <- c()
for (i in 1:length(indicator)) {
  if (indicator[i] == 1) {
    Pred_pos <- c(Pred_pos, "Guard")
  } else if (indicator[i] == 2) {
    Pred_pos <- c(Pred_pos, "Forward")
  } else if (indicator[i] == 3) {
    Pred_pos <- c(Pred_pos, "Center")
  }
}
NBA_New$Pos_Pred <- Pred_pos

Actual_Pos <- c()
for (i in 1:nrow(NBA_New)) {
  if (NBA_New$position_factor[i] == "PG" | NBA_New$position_factor[i] == "SG"){
    Actual_Pos <- c(Actual_Pos, "Guard")
  } else if (NBA_New$position_factor[i] == "SF" | NBA_New$position_factor[i] == "PF"){
    Actual_Pos <- c(Actual_Pos, "Forward")
  } else {
    Actual_Pos <- c(Actual_Pos, "Center")
  }
}
NBA_New$Actual_Pos <- Actual_Pos

boxplot(NBA_New$USG. ~ NBA_New$Pos_Pred, border = "darkblue", col = "white", frame = FALSE, xlab = NA, ylab = "Usage Percentage", axis = F)
boxplot(NBA_New$Height ~ NBA_New$Pos_Pred, border = "darkred", col = "white", frame = FALSE, xlab = NA, ylab = "Height", axis = F)

Pred_Guard <- NBA_New[NBA_New$Prediction == 1,]
Table_1 <- prop.table(table(Pred_Guard$Actual_Pos))
Pred_Forward <- NBA_New[NBA_New$Prediction == 2,]
Table_2 <- prop.table(table(Pred_Forward$Actual_Pos))
Pred_Center <- NBA_New[NBA_New$Prediction == 3,]
Table_3 <- prop.table(table(Pred_Center$Actual_Pos))
Table_sum <- rbind(Table_1, Table_2, Table_3)
Table_sum[3,3] <- 0
rownames(Table_sum) <- c("Cluster_Guard", "Cluster_Forward", "Cluster_Center")
Table_sum


### Principle Components Analysis
# Select around a dozen attributes, and we decided to use Production as dependent variable
NBA_PC <- NBA_Dupe[, c("Production", "MinPG", "Age", "PER", "TRB.", "AST.", "STL.", "BLK.", "USG.",
                       "TOV.", "WS", "Shot.", "Defense", "Offense")]
# Brief cleaning
NBA_PC$Production <- gsub(",","",NBA_PC$Production)
NBA_PC$Production <- gsub("\\$","",NBA_PC$Production)
NBA_PC$Production <- gsub("\\(","",NBA_PC$Production)
NBA_PC$Production <- gsub("\\)","",NBA_PC$Production)
NBA_PC$Production <- as.numeric(NBA_PC$Production)
NBA_PC <- NBA_PC[NBA_PC$Production != 0,]
# First find variance covariance matrix of centered and scaled data
NBA_PC_CS <- scale(NBA_PC, center = TRUE, scale = TRUE)
Var_NBA <- var(NBA_PC_CS[,c(2:14)])
head(Var_NBA)
# Then find the eigenvalues and the eigenvectors
Eigen_NBA <- eigen(Var_NBA) 
Eigenvalues <- Eigen_NBA$values
Eigenvalues
Eigenvectors <- Eigen_NBA$vectors
Eigenvectors

Prop_pc <- 100*(Eigenvalues/sum(Eigenvalues))
Prop_pc
round(Prop_pc, 3)
cumsum(Prop_pc)
# Create a pie plot that visualizes the proportion of variance explained by each PC
pie(Prop_pc, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", NA,NA,NA,NA,NA,NA), 
    main = "Percentage explained", col = c(rainbow(7), 0,0,0,0,0,0), border = F)
# Create a matrix that represents the proportion of variance explained by each PC
First_7 <- paste(as.character(round(Prop_pc[c(1:7)], 3)), "%", sep = "")
names(First_7) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
cumsum_7 <- paste(round(cumsum(Prop_pc)[1:7], 3), "%", sep = "")
Cumsun_mat <- rbind(First_7, cumsum_7)
rownames(Cumsun_mat) <- c("Individual Prop", "Cumulative Prop")

#Compute the matrix of principal components scores
PC <- NBA_PC_CS[,c(2:14)] %*% Eigenvectors
colnames(PC) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11",
                  "PC12", "PC13")
Cormat <- cor(PC, NBA_PC_CS[,c(2:14)])
rownames(Cormat) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11",
                      "PC12", "PC13")
Cormat <- round(Cormat, 3)
# Use corrplot to visualize the correlation
library(corrplot)
corrplot(Cormat, "square")
# Visualize the correlation between dependent variable and PCs
PC_norm <- c()
# A function that calculates the norm of a variable
norm_2 <- function(x) {
  sqrt(sum((abs(x))^2))
}
for (i in 1:ncol(PC)) {
  length_temp <- norm_2(PC[,i])
  column_norm <- PC[,i]/length_temp
  PC_norm <- cbind(PC_norm, column_norm)
}
colnames(PC_norm) <- colnames(PC)[1:13]
Regression_dat <- cbind(NBA_PC_CS[,1], PC_norm)
colnames(Regression_dat)[1] <- "Salary.cs"
Regression_dat <- as.data.frame(Regression_dat)
cor(Regression_dat)
corrplot(cor(Regression_dat[,c(1:8)]), method = "square", type = "upper")


### Principle Components Regression
# Split the data into training and testing
set.seed(12345)
part_index <- sample(472, 10, replace = FALSE)
Training <- Regression_dat[-part_index,]
Testing <- Regression_dat[part_index, ]
# Fit a model on it. We decided to use principle components 1,2,4,5,6
lm1 <- lm(Salary.cs ~ PC1+PC2+PC4+PC5+PC6, data = Regression_dat)
summary(lm1)
# Evaluate the model
sqrt(sum((predict(lm1, Testing) - Testing$Salary.cs)^2)/10)

### MLE estimation
xt <- c(100,0)
eps <- 0.00000000001 # Tolerance
xtp1 <- c(14,1) # Initial value
xHist <- matrix(xtp1,2,1) # save history of xt
fHist <- c() # history of log likelihood 
xHist <- c() # history of parameter vectors
# objective function=log likelihood 
y <- NBA_PC$Production
n <- length(NBA_PC$Production)
f <- -sum(y) - n*log(xtp1[2]) - n/2*log(2*pi) - sum((log(y)-xtp1[1])^2/(2*(xtp1[2])^2))
# History of the objective function
fHist=f
# Apply Newton's Method
while(sum((xtp1-xt)^2)>eps){
  xt=xtp1
  xt
  gradient <- as.vector(c(sum((log(y)-xt[1])/(xt[2])^2),
                          -n/xt[2]+sum((log(y)-xt[1])^2/(xt[2])^3)))
  gradient
  hessian=matrix(c(-n/(xt[2])^2,
                   -2*sum((log(y)-xt[1])/(xt[2])^3),
                   -2*sum((log(y)-xt[1])/(xt[2])^3),
                   n/(xt[2])^2-3*sum((log(y)-xt[1])^2/(xt[2])^4)),ncol=2,nrow=2)
  hessian
  ### compute xtp1
  xtp1=xt-solve(hessian)%*%gradient  # Newton iteration
  xtp1
  ### save history
  xHist=matrix(c(xHist,xtp1),2)
  xHist
  f <- -sum(y) - n*log(xtp1[2]) - n/2*log(2*pi) - sum((log(y)-xtp1[1])^2/(2*(xtp1[2])^2))
  fHist=c(fHist,f)
}
xHist
fHist
mu <- xHist[, ncol(xHist)][1] + c(-1,1) * sqrt(diag(solve(-hessian)))[1]
sigma <- xHist[, ncol(xHist)][2] + c(-1,1) * sqrt(diag(solve(-hessian)))[2]
mu
sigma
# Fir the curve of our model onto the original histogram
library(MASS)
x_vals <- c(seq(from = 0, 4e+7, by = 10^5))
hist(NBA_PC$Production, 100, freq = F, xlab = "Production", col = "white", main = "Density of Players' Production")
points(x_vals, dlnorm(x_vals, meanlog = 14.583989, sdlog = 1.291851, log = FALSE), type = "l", col = "darkred", lwd = 2)
# Finding the probability that a player's production is between the median and mean of all.
f <- function(x) {
  1/(1.291851*sqrt(2*pi)*x) * exp(-(log(x)-14.583989)^2/(2*1.291851^2))
}
integrate(f, lower = 2200000, upper = 4442094)
