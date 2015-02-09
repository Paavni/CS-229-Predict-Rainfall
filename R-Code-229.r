# install libraries
install.packages("nnet");
install.package("neuralnet");
install.packages("multicore");
install.packages("caret");
install.packages("nlme");
install.packages("R.utils");
library(multicore)
library(nnet)
library(neuralnet)
library(caret)
library(nlme)
library(R.utils)

# read data
setwd("C:/Users/Paavni/My Academics/Quarter IV/CS 229/Project/Final_Data/")
# setwd("~/CS229/Project/Final_Data/")
sourceDirectory("Functions/")
kg = read.csv("kishanganj_features.csv",header = FALSE, sep = ",")

kg = kg[,-1]
# mean for each variable
meancity = colMeans(kg);

# normalization
kgn = normalization(kg)

# dividing into test and train
train =  kg[c(1:1068),]
test =  kg[c(1069:1224),]
trainn =  kgn[c(1:1068),]
testn =  kgn[c(1069:1224),]

################################################################################################################################################
# OUR METHOD
################################################################################################################################################
# Window Size
k = 3;
s = 20;
decay = 0.00001;

n_f = nnet_sgd(trainn, maxit = 1000, s = 20, decay = 0.00001, future = FALSE);
predtrain = de_normalization(as.matrix(n_f$p), as.matrix(kg[,11]))
actualtrain = de_normalization(as.matrix(trainn[,11]), as.matrix(kg[,11]))
we = weighted_error(predtrain, actualtrain)
mae = mean(abs(predtrain-actualtrain))
rmse = sqrt(mean((predtrain-actualtrain)^2))
hist(abs(predtrain-actualtrain), xlab = "Absolute Error", col = "red")
par(mfrow=c(2,1))
plot(actualtrain, type='l', col = "blue",xlab = "Time", ylab = "Actual Rainfall(mm)"); plot(predtrain, type='l', col = "red", xlab = "Time", ylab = "Predicted Rainfall(mm)")

n_f_test = nnet_sgd(testn,maxit = 1000, s = 20, decay = 0.00001, future = FALSE);
pred = de_normalization(as.matrix(n_f_test$p), as.matrix(kg[,11]))
actual = de_normalization(as.matrix(testn[,11]), as.matrix(kg[,11]))
wet = weighted_error(pred, actual)
maet = mean(abs(pred-actual))
rmset = sqrt(mean((pred-actual)^2))
################################################################################################################################################
# TUNING
################################################################################################################################################
nnet.wrapper.k=function(k){
	        nnet_sgd(trainn,k=k, window = TRUE)
	}	
res.k=mclapply(c(3,5,7,9),nnet.wrapper.k)
we = rep(0, length(res.k)); mae = rep(0, length(res.k))
for(i in 1:length(res.k))
{
pred = de_normalization(as.matrix(res.k[[i]]$p), as.matrix(kg[,11]))
actual = de_normalization(as.matrix(trainn[,11]), as.matrix(kg[,11]))
we[i] = weighted_error(pred, actual)
mae[i] = mean(abs(pred-actual))
}

nnet.wrapper.s=function(s){
	        n_f = nnet_sgd(trainn,s=s, future = FALSE)
}
res.s=mclapply(c(5,10,15,20,25),nnet.wrapper.s)	
we = rep(0, length(res.s)); mae = rep(0, length(res.s))
for(i in 1:length(res.s))
{
pred = de_normalization(as.matrix(res.s[[i]]$p), as.matrix(kg[,11]))
actual = de_normalization(as.matrix(trainn[,11]), as.matrix(kg[,11]))
we[i] = weighted_error(pred, actual)
mae[i] = mean(abs(pred-actual))
}

nnet.wrapper.d=function(decay){
	        n_f = nnet_sgd(trainn,decay = decay, s= 20, future = FALSE)
	}
res.d=mclapply(c(1,0.1,0.01,0.001,0.0001, 0.00001),nnet.wrapper.d)	
we = rep(0, length(res.d)); mae = rep(0, length(res.d))
for(i in 1:length(res.d))
{
pred = de_normalization(as.matrix(res.d[[i]]$p), as.matrix(kg[,11]))
actual = de_normalization(as.matrix(trainn[,11]), as.matrix(kg[,11]))
we[i] = weighted_error(pred, actual)
mae[i] = mean(abs(pred-actual))
}	

nnet.wrapper.i=function(maxit){
	        n_f = nnet_sgd(trainn,maxit = maxit, decay = 0.00001, s= 20, future = FALSE)
	}
res.i=mclapply(c(100,500,1000,1500),nnet.wrapper.i)	
we = rep(0, length(res.i)); mae = rep(0, length(res.i))
for(i in 1:length(res.i))
{
pred = de_normalization(as.matrix(res.i[[i]]$p), as.matrix(kg[,11]))
actual = de_normalization(as.matrix(trainn[,11]), as.matrix(kg[,11]))
we[i] = weighted_error(pred, actual)
mae[i] = mean(abs(pred-actual))
}



################################################################################################################################################
# REGRESSION
################################################################################################################################################
kg_hist = read.csv("kishanganj_longfeatures.csv",header = FALSE, sep = ",")
plot(kg_hist[c(1:60),1], kg_hist[c(1:60),24], type='o',ylab = "Rainfall (mm)", col= "blue", xlab = "Time", xaxt='n', pch = 18)
axis(side=1, at=c(191206,191306,191406, 191506, 191606), labels=c("1912","1913","1914", "1915", "1916"))
plot(kg_hist[c(1033:1092),1], kg_hist[c(1033:1092),24], type='o',ylab = "Rainfall (mm)", col= "blue", xlab = "Time", xaxt='n', pch = 18)
axis(side=1, at=c(199806,199906,200006, 200106, 200206), labels=c("1998","1999","2000", "2001", "2002"))
kg_hist = kg_hist[,-1]
feat.train.hist =  kg_hist[c(1:937),]
feat.test.hist =  kg_hist[c(938:1092),]
lm.hist <- lm(V24 ~ ., data=feat.train.hist)
# lm.hist <- gls(V24 ~ ., data=feat.train.hist, method='ML', correlation=corARMA(p,q))

p.hist.train.s = predict(lm.hist, feat.train.hist[,c(1:22)]);
error.hist.train.s = mean(abs(p.hist.train.s-feat.train.hist[,23]))
we.hist.train.s = weighted_error(as.matrix(p.hist.train.s), as.matrix(feat.train.hist[,23]))

p.hist.test.s = predict(lm.hist, feat.test.hist[,c(1:22)]);
error.hist.test.s = mean(abs(p.hist.test.s-feat.test.hist[,23]))
we.hist.test.s = weighted_error(as.matrix(p.hist.test.s), as.matrix(feat.test.hist[,23]))

################################################################################################################################################
kg_feat = read.csv("kishanganj_features.csv",header = FALSE, sep = ",")
kg_feat = kg_feat[,-1]
feat.train =  kg_feat[c(1:1068),]
feat.test =  kg_feat[c(1069:1224),]
lm.feat <- lm(V12 ~ ., data=feat.train)
p.feat.train = predict(lm.feat, feat.train[,c(1:10)]);
error.feat.train = mean(abs(p.feat.train-feat.train[,11]))
we.feat.train = weighted_error(as.matrix(p.feat.train), as.matrix(feat.train[,11]))
p.feat.test = predict(lm.feat, feat.test[,c(1:10)]);
error.feat.test = mean(abs(p.feat.test-feat.test[,11]))
we.feat.test = weighted_error(as.matrix(p.feat.test), as.matrix(feat.test[,11]))
rmse.feat.test = sqrt(mean((p.feat.train-feat.test[,11])^2))
################################################################################################################################################
s.feat = svd(kg_feat[,-11])
s.feat$d
s.feat$d[c((length(s.feat$d)-2):length(s.feat$d))] = 0
x.feat = s.feat$u %*% diag(s.feat$d) %*% t(s.feat$v)
x.feat = data.frame(cbind(x.feat, kg_feat[,11]))
x.feat = x.feat[,c(-8,-9,-10)]
x.feat.train =  x.feat[c(1:1068),]
x.feat.test =  x.feat[c(1069:1224),]
lm.feat.s <- lm(X11 ~ ., data=x.feat.train)
p.feat.train.s = predict(lm.feat.s, x.feat.train[,c(1:7)]);
error.feat.train.s = mean(abs(p.feat.train.s-x.feat.train[,8]))
we.feat.train.s = weighted_error(as.matrix(p.feat.train.s), as.matrix(x.feat.train[,8]))

p.feat.test.s = predict(lm.feat.s, x.feat.test[,c(1:7)]);
error.feat.test.s = mean(abs(p.feat.test.s-x.feat.test[,8]))
we.feat.test.s = weighted_error(as.matrix(p.feat.test.s), as.matrix(x.feat.test[,8]))
################################################################################################################################################
# NEURAL NETWORK
################################################################################################################################################
train =  kg[c(1:1068),]
test =  kg[c(1069:1224),]
trainn =  kgn[c(1:1068),]
testn =  kgn[c(1069:1224),]
h = 15
t = 0.1
sm = 1e+5
r = 1
lr=0.001
af = "tanh"

nn_f = neuralnet(V12~(V2+V3+V4+V5+V6+V7+V8+V9+V10+V11), trainn, hidden = h, threshold = t, stepmax = sm, rep = r, learningrate=lr,err.fct = "sse", act.fct = af, linear.output = TRUE)
pred.train = compute(nn_f, trainn[,c(1:10)])$net.result
pred.train = de_normalization(as.matrix(pred.train), as.matrix(kg[,11]))
actual.train = de_normalization(as.matrix(trainn[,11]), as.matrix(kg[,11]))
error.train = mean(abs(pred.train-actual.train))
we.train= weighted_error(as.matrix(pred.train), as.matrix(actual.train))

pred.test = compute(nn_f, testn[,c(1:10)])$net.result
pred.test = de_normalization(as.matrix(pred.test), as.matrix(kg[,11]))
actual.test = de_normalization(as.matrix(testn[,11]), as.matrix(kg[,11]))
error.test = mean(abs(pred.test-actual.test))
we.test= weighted_error(as.matrix(pred.test), as.matrix(actual.test))

error.train
we.train
error.test
we.test
################################################################################################################################################
# LOGISTIC REGRESSION
################################################################################################################################################
kg = read.csv("kishanganj_features.csv",header = FALSE, sep = ",")
# kg = normalization(kg)
flood = read.csv("floods.csv",header = FALSE, sep = ",")
flood_class = cbind(kg[c(301:1224),-1],"flood" = flood[c(301:1224),-1])
# predprec = rbind(predtrain, pred)
# flood_class = cbind(kg[c(301:1224),c(-1,-12)],"predicted" = predprec[c(301:1224),], "flood" = flood[c(301:1224),-1])
flood_class$flood <- factor(flood_class$flood, labels = c(0,1))
row.names(flood_class) = NULL
train =  flood_class[c(1:720),]
test =  flood_class[c(720:924),]
row.names(test) = NULL
row.names(train) = NULL
logit <- glm(flood ~ ., data = train, family = "binomial")
p = predict(logit, train[,c(1:11)], type="response")
ptest = predict(logit, test[,c(1:11)], type="response")

prob = sum(train$flood==1)/(dim(train)[1])
p = ifelse(p>prob, 1, 0)
prec = sum(p==1 & train$flood==1)/((sum(p==1 & train$flood==1))+(sum(p==0 & train$flood==1)))
rec = sum(p==1 & train$flood==1)/((sum(p==1 & train$flood==1))+(sum(p==1 & train$flood==0)))

ptest = ifelse(ptest>prob, 1, 0)
prect = sum(ptest==1 & test$flood==1)/((sum(ptest==1 & test$flood==1))+(sum(ptest==0 & test$flood==1)))
rect = sum(ptest==1 & test$flood==1)/((sum(ptest==1 & test$flood==1))+(sum(ptest==1 & test$flood==0)))

