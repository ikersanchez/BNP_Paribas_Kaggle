#Exploratory analysis

        setwd("C:/Users/Iker/Desktop/bnp")
        train <- read.csv("train.csv" ,na.strings = c("NA",""),stringsAsFactors = T)

#Get only numeric variables
        
        nums <- sapply(train, is.numeric)
        numtrain <- train[,nums == TRUE]
       
        #Check correlations
        
        library(caret)
        library(psych)
        
        cor <- corr.test(numtrain)
        cormatrix <- cor$r
        library(corrplot)
        cor.plot(cormatrix,cex = 0.01,numbers = F,cex.lab =0.01)
        findCorrelation(cormatrix,cutoff = 0.95)
        
        library(reshape2)
        subset(melt(cormatrix), value > .80 &  value != 1)
        
#Locate NAs
        
        apply(train,2,function(x) sum(is.na(x)))
        
        #Na patterns plot(Kaggle script by JohnM)
        library(VIM)
        
        png(filename="NAsPatternEq.png",
            type="cairo",
            units="in",
            width=12,
            height=6.5,
            pointsize=10,
            res=300)
        
        miceplot1 <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                          numbers=TRUE, combined=TRUE, varheight=FALSE, border="gray50",
                          sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern"),
                          labels=names(train[-c(1:2)]), cex.axis=.7)
        dev.off()
        
        png(filename="NAsPatternAdj.png",
            type="cairo",
            units="in",
            width=12,
            height=6.5,
            pointsize=10,
            res=300)
        
        miceplot2 <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                          numbers=TRUE, combined=TRUE, varheight=TRUE, border=NA,
                          sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern w/ Height Adjustment"),
                          labels=names(train[-c(1:2)]), cex.axis=.7)
        dev.off()
        
        
        
#Locate columns with no Missing values
        
        Nona <- sapply(train,function(x){sum(is.na(x)) < 10})
        colnames(train)[Nona == TRUE]
        
#Density plots for each numeric variable
        
        vector <- colnames(numtrain)
        for(i in 1:length(vector)){
                
                plot(density(numtrain[,vector[i]],na.rm = T),main = paste0("Density of variable"," ",vector[i]))
        }
        
#Descriptive analytics grouped by target (#claims for which 
#approval could be accelerated leading to faster payments = 1)
        
        describeBy(numtrain,group = train$target)
        
#Superimposed density plots by target
        
        vector <- colnames(numtrain)
        
        train0 <- train[train$target == 0,]
        train1 <- train[train$target == 1,]
        
        for(i in 1:length(vector)){
                
                dens0 <- density(train0[,vector[i]],na.rm = T)
                dens1 <- density(train1[,vector[i]],na.rm = T)
                plot(dens0,col = "red",main = paste0("Density of variable ",vector[i]),lwd=1)
                lines(dens1,col = "blue",lwd=1)
                legend(quantile(dens0$x,0.75),max(dens0$y),legend = c("Target=0","Target=1"),lty= c(1,1),lwd=c(2.5,2.5),col=c("red","blue"))
        }
        
        #Compare quantiles
        
        num0 <- train0[,sapply(train0,is.numeric)]
        x <- apply(num0,2,function(x) round(quantile(x,na.rm =T),3))
        
        num1 <- train1[,sapply(train1,is.numeric)]
        y <- apply(num1,2,function(x) round(quantile(x,na.rm =T),3))
        
        z <- apply(numtrain,2,function(x) round(quantile(x,na.rm =T),3))
        
        
        #Negative counts
        
        NegativeValues <- rep(0,dim(train)[1])
        for (i in 1:dim(train)[1]){
              NegativeValues[i] <- length(which(train[i,]< 0))
        }
        
#Factor variables
        
        #Too many factor in v22.
        
        train$v22 <- NULL #?
        
        #First letter for each factor in v22
        train$firstletter <- sapply(train$v22,function(x) substr(x,1,1))
        
        factors <- sapply(train, is.factor)
        trainfac <- train[,factors == TRUE]
        
        vector2 <- colnames(trainfac)
        for(i in 1:length(vector2)){
                
                barplot(table(train[,vector2[i]]),las=2,main = paste0("Factor counts in ",vector2[i]))
        }
        
        #Filtered by target
        
        for(i in 1:length(vector)){
                par(mfrow= c(1,2))
                barplot(table(train0[,vector2[i]]),las=2,main = paste0("Factor counts in ",vector2[i]," when target = 0"))
                barplot(table(train1[,vector2[i]]),las=2,main = paste0("Factor counts in ",vector2[i]," when target = 1"))
        }
        #Relations:
                library(DescTools)
                library(ca)
        
                #For loop over all categorical variables to calculate lambda
                trainfac$v22 <- NULL
                VectorA <- names(trainfac)
                VectorB <- names(trainfac)
                
                for ( a in 1:length(VectorA)){
                        for ( b in 1:length(VectorB)){
                                
                                matrix <- as.matrix(table(trainfac[,VectorA[a]],trainfac[,VectorB[b]]))
                                Lambda <- Lambda(matrix,direction = "column")
                                print(cbind(Variable1 = VectorA[a],Variable2 = VectorB[b],round(Lambda,2)))
                        }
                }
        
                #v107 and v91
        
                table(train$v107,train$v91)
                X <- as.matrix(table(train$v107,train$v91))
                Lambda(X, direction="column")
                chisq.test(X)
                (ca <- ca(X))
                plot(ca) # Total overlapping
                
                #v71 and v75
                
                X <- as.matrix(table(train$v71,train$v75))
                Lambda(X, direction="column")
                chisq.test(X)
                (ca <- ca(X))
                plot(ca)
                
                #v79 and v71
                
                X <- as.matrix(table(train$v79,train$v71))
                Lambda(X, direction="column")
                chisq.test(X)
                (ca <- ca(X))
                plot(ca)
                
                #v79 and v47
                
                table(train$v79,train$v47)
                X <- as.matrix(table(train$v79,train$v47))
                Lambda(X, direction="column")
                (ca <- ca(X))
                plot(ca) # Total overlapping
                
                
                #v47 and v110
                table(train$v47,train$v110)
                X <- as.matrix(table(train$v47,train$v110))
                Lambda(X, direction="column")
                (ca <- ca(X))
                plot(ca) #Total overlapping
                
                #Desc(X,verbose = "high")
                
                
#PCA

       pca <- prcomp(numtrain,scale = T,center =T) 
       plot(cumsum((pca$sdev)^2)) #19

       
#Check multicolinearity

       #Some numeric variables are linear combinations:
       x <- lm(v100 ~ v58,data = train) #R2 = 1
       summary(x)
       
       xy <- lm(v72 ~ v129+v38+v62,data = train) #R2 = 1
       summary(xy)
       
       library(usdm)
       vif(numtrain) 
       multic <- vifstep(numtrain,th=10) #vifstep vifcor
       excluded <- attr(multic,"excluded") #Pca with removed variables?               
      
       
#Hierarchical clustering of variables
       
       library(Hmisc)
       clust <- varclus(as.matrix(data))
       plot(clust)
       cut <- cutree(clust$hclust, k=6)
       cut[order(cut)]
       
#Structure of data using rpart
       
       library(rpart)
       library(rpart.plot)
       library(caret)
       train$Id <- NULL
       train$v22 <- NULL
       tr <- train(factor(target) ~ .,data = train,method = "rpart")
       rpart.plot(tr$finalModel)
       
#check Boruta to perform feature selection
       
      library(Boruta)
      Bo <- Boruta(train,factor(train$target),doTrace = 1,maxRuns = 100)

	#Useful post: https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/forums/t/19240/analysis-of-duplicate-variables-correlated-variables-large-post/110016#post110016

#Fun patterns :) https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/forums/t/19634/strange-happenings

library(data.table)
train <- data.table(train)
a <- frankv(train$v16)
b <- frankv(train$v115)
plot(a,b)


#K-means

        #Choose best number of K:
        #http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
        numtrain <- numtrain[complete.cases(numtrain),]
        
        wss <- (nrow(numtrain)-1)*sum(apply(numtrain,2,var))
        for (i in 2:15) wss[i] <- sum(kmeans(scale(numtrain),
                                             centers=i)$withinss)
        plot(1:15, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares")
        
		library(cluster)
        library(fpc)
        km <- kmeans(numtrain,centers = 2)
        plotcluster(numtrain, km$cluster)
		
        #calinsky criterion
        require(vegan)
        fit <- cascadeKM(scale(numtrain, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
        plot(fit, sortg = TRUE, grpmts.plot = TRUE)
        calinski.best <- as.numeric(which.max(fit$results[2,]))
        cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
		
        #Affinity prop
        library(apcluster)
        d.apclus <- apcluster(negDistMat(r=1), numtrain)
        cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
        heatmap(d.apclus)
        plot(d.apclus, d)
		
        #Bayesian clustering method, good for high-dimension data, more details:
        #http://vahid.probstat.ca/paper/2012-bclust.pdf
        install.packages("bclust")
        library(bclust)
        x <- as.matrix(numtrain)
        d.bclus <- bclust(x, transformed.par = c(0, -50, log(16), 0, 0, 0))
        viplot(imp(d.bclus)$var); plot(d.bclus); ditplot(d.bclus)
        dptplot(d.bclus, scale = 20, horizbar.plot = TRUE,varimp = imp(d.bclus)$var, horizbar.distance = 0, dendrogram.lwd = 2)


        
#Mutual information - target variable
        
        library(infotheo)
        MI <- c(Var = "var",MI = 0)
        names <- setdiff(names(numtrain),c("target","ID"))
        for ( i in 1:length(names)){
                Mut <- round(mutinformation(train$target,discretize(numtrain[,names[i]])),4)
                MI <- rbind.data.frame(MI,data.frame(Var = names[i],MI = Mut))
        }
        MI[order(MI$MI,decreasing = T),] #Check weight of evidence and IV with WOE library.
		
		
        
#Linear combinations

        #After xgb1 full
        train <- full[-Idx,]
        library(caret)
        Linear <- findLinearCombos(train[,2:ncol(train)])
        