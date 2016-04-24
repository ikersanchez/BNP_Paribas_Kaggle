#Ensembling Script (weights)

        #Reproducible sets
        setwd("C:/Users/Iker/Desktop/bnp")
        train <- read.csv("train.csv" ,na.strings = c("NA",""),stringsAsFactors = F)
        #dist <- read.csv("distTrainknn2.csv")
        #train <- cbind(train,dist) ## For ET python
        library(caTools)
        set.seed(1234)
        spl <- sample.split(train$target, SplitRatio = 0.8)
        trainset <-  subset(train, spl==TRUE)
        validation <- subset(train, spl==FALSE)
        setwd("C:/Users/Iker/Desktop/bnp/Ensembling")
       
        
        
        #
        y_true <- validation$target
        validation$target <- NULL
        test <- validation
        rm(validation)
        train <- trainset
        rm(trainset)
        
        #Save new predictions
        
                #Xgb2 preds : write.csv(sub,"XGB2ens.csv",row.names = F)
                #Xgb4 preds : write.csv(sub,"XGB4ens.csv",row.names = F)
                #Xgb3 preds :  write.csv(sub,"XGB3ens.csv",row.names = F)
                #glmnet preds : write.csv(sub,"GLMNETens.csv",row.names = F)
                #nn h2o preds: write.csv(submission,"NNens.csv",row.names=F)
                #random forest preds: write.csv(submission,"RF2ens.csv",row.names=F)
                #Extratree1 = Python
                #Extratree2 = Python
                #ExtratreeKnn= Python
				#XGB+features
				
        #Load predictions
        Xgb2 <- read.csv("XGB2ens.csv")$PredictedProb
        Xgb3 <- read.csv("XGB3ens.csv")$PredictedProb
        Xgb4 <- read.csv("XGB4ens.csv")$PredictedProb
        Glmnet <- read.csv("GLMNETens.csv")$PredictedProb
        NN <- read.csv("NNens.csv")$PredictedProb
        Rf2 <- read.csv("RF2ens.csv")$PredictedProb
        ET1 <- read.csv("extra_trees1ens.csv")$PredictedProb
        ET2 <- read.csv("extra_trees2ens.csv")$PredictedProb
        ETknn <- read.csv("extra_trees_knnens.csv")$PredictedProb
        #... other preds
        #
        
        library(MLmetrics)
        LogLoss(y_true,preds)
        
        #Best weights
        weights <- c(weight1 = 0, weight2 = 0, weight3 = 0,weight4 = 0,weight5 = 0,weight6 = 0,logloss = 1111)
        
        for(i in seq(0,1,0.05)) {
                for(j in seq(0,1,0.05)) {
                        for(k in seq(0,1,0.05)){
                                        for(m in seq(0,1,0.05)){
                                                for(n in seq(0,1,0.05)){
                                                        for(o in seq(0,1,0.05)){
                                        if((i + j + k+m+n+o) == 1) {
                                        ensemble_pred <- i * Xgb2 + j  * Xgb4 + k * ET1 + m * Rf2 + n * Xgb3 + o * ETknn
                                        logloss <- LogLoss(y_true, ensemble_pred)
                                        weights <- rbind.data.frame(weights, data.frame(weight1 = i, weight2 = j, weight3 = k,weight4 = m,weight5 = n ,weight6 = o ,logloss = logloss))}
                                       
                                    
                             }           
                        }
                }
        }}}
        
        
        weights[which.min(weights$logloss),]
        


#Final submission
        
        
	model1 <- read.csv("xgbFullfeat.csv")
	model2 <- read.csv("extra_trees_knn.csv")
	avg <- 0.6*model1$PredictedProb + 0.4* model2$PredictedProb 
	sub <- data.frame(ID = model1$ID, PredictedProb = avg)
	write.csv(sub,"final.csv",row.names = FALSE)
	#0.44699 leaderboard
