#Feature engineering

#Number of zeros

        zeros <- ncol(fullmat) - apply(full,1,nnzero)

#Substract mean 
 
        idx <- which(is.na(train))
        names <- colnames(train)
        
        for( i in 1:length(names)){
                if (class(train[,names[i]]) == "numeric"){
                        
                namecol <- paste0("DistanceMean",names[i])
                dist <-train[,names[i]]-mean(train[,names[i]],trim=0.10,na.rm = T)
                train[,namecol] <- dist
                }
        }
        
        train2 <- cbind(train[,74:ncol(train)],train[,which(sapply(train,is.character))])
        

#Add min,max,mean,var,sd

        featnum <- train[,sapply(train, is.numeric)]
        featcar <- train[,sapply(train, is.character)]
        featcar$ID <- NULL
        train$Means <- apply(featnum,1,mean)
        train$Var <- apply(featnum,1,var)
        train$Sd <- apply(featnum,1,sd)
        train$Sum <- apply(featnum,1,sum)
        train$Min <- apply(featnum,1,min)
        train$Max <- apply(featnum,1,max)
        train$Median <- apply(featnum,1,median)

#Sum NAs 

        NAcount <- apply(train,1,function(x) sum(is.na(x)))

#0-1 flags to NA counts

        barplot(table(full$NAcount)) #Two clear separations.
        full$Na100<- ifelse(full$NAcount >= 100,1,0)
        full$Na10 <- ifelse(full$NAcount <= 10,1,0)
        full$Na10 <- factor(full$Na10)
        full$Na100 <- factor(full$Na100)



#Number of "below 0" and number of "above 0" per row 

        NegativeCount <- rowSums(train < 0)
        PositiveCount <- rowSums(train > 0)
        SD <- apply(featnum[,-which(names(featnum) %in% c("v22"))],1,sd)
        

#Naive bayes transformation of categorical data 
#https://www.kaggle.com/scirpus/bnp-paribas-cardif-claims-management/benouilli-naive-bayes/files

        library(e1071)
        fac <- train[,sapply(train,is.character)]
        fac$v22 <- NULL
        
        features <- names(fac[3:ncol(fac)])
        preds <- matrix(1,ncol=length(features), nrow=dim(train)[1])
        colnames(preds) <- features
        
        for(i in 1:length(features)){
                print(i)
                subset <- train[,c(features[i],"target")]
                target <- subset$target
                subset <- model.matrix(target ~ .-1,data = subset)
                nb <- naiveBayes(x = subset,y = target)
                preds[,i] <- predict(nb,subset,type = "raw")[,1]
        }


#Differences between correlated variables

        # https://github.com/mpearmain/homesite/blob/3dd541efe6c48bfd1cbda9ca5fc81156906d9ca4/R/data_preparation.R

        library(psych)
        library(caret)
        
        data <- train[,excluded]
        cor <- corr.test(data)
        cormatrix <- cor$r
        flc <- findCorrelation(cormatrix, 0.95)
        corr_pairs <- which(cormatrix > 0.95, arr.ind = T)
        corr_pairs <- corr_pairs[corr_pairs[,1] > corr_pairs[,2],]
		
        # v96   18   1
        # v92   37   2
        # v96   18   4
        # v60   27   6
        # v11   10   8
        # v64   30  13
        # v76   80  13
        # v33   16  15
        # v121  31  15
        # v97   70  17
        # v116  58  22
        # v76   80  30
        # v46   40  35
        # v63   50  35
        # v105  64  35
        # v54   69  35
        # v63   50  40
        # v108  61  46
        
        store <- array(0, c(nrow(train), nrow(corr_pairs)))
        
        
        for (i in 1:nrow(corr_pairs))
        {
                store[,i] <- apply(data[,corr_pairs[i,]],1,diff)
                #store[,i] <- apply(test[,corr_pairs[i,]],1,diff)
                
        }
        colnames(store) <- paste("diff", 1:ncol(store), sep = "")
        
        
        vector2 <- colnames(store)
        for (i in 1:length(vector2)){
                if (class(store[,vector2[i]]) == "numeric") {
                        store[which(is.na(store[,vector2[i]])),vector2[i]] <- -1}
                else {store[which(is.na(store[,vector2[i]])),vector2[i]] <- "Missing"}
        }
        write.csv(store,"dist.csv") 

#Euclidean distances to k nearest neighbor (up to 4) #Very useful
        
        fullmat <- model.matrix(ID ~ .-1,data = full) 
        trainmat <- fullmat[-Idx,]
        #rm(fullmat)
        library(FNN)
        kn <- get.knn(fullmat,k = 2)
        dist <-  kn$nn.dist
        colnames(dist) <- c("distKNN1","distKNN2")
        
#Distances to kmeans centers
        
        #xgb2 script trainmat sin v22
        numtrain <- trainmat
        
        wss <- (nrow(numtrain)-1)*sum(apply(numtrain,2,var))
        for (i in 2:50){
         print(i)     
         wss[i] <- sum(kmeans(numtrain,
                                             centers=i)$withinss)}
        plot(1:50, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares")
        
        
        nof_centers <- 2
        km <- kmeans(traimat, centers = nof_centers)
        dist1 <- array(0, c(nrow(trainmat), nof_centers))
        for (i in 1:nof_centers)
        {
                dist1[,i] <- apply(trainmat,1,function(s) sd(s - km$centers[ii,]))
               
        }

        
#Some interactions (excluded)

        way2int <- t(apply(data[,1:3],1,combn,2,sum)) #Every possible comb
        #length(combn(1:83,2,sum)) = 3403 
       
        set.seed(123)
        way2int <- t(apply(data[,sample(1:83,10)],1,combn,2,sum))
        #Expand this approach using IV as criterion
        
#Interactions between excluded variables,by clusters

        # full xgb2
        library(stats)
        library(caret)
        library(psych)
        
        data <- full[,excluded]
        library(Hmisc)
        clust <- varclus(as.matrix(data))
        cut <- cutree(clust$hclust, k=10)
        plot(clust)
        rect.hclust(clust$hclust, k = 10, cluster = cut)
        cut[order(cut)]
        
        #Cluster1
        varsCluster1 <- names(cut[cut==1])
        subset <- data[,varsCluster1]
        #length(combn(1:11,3,diff)) = 330 cols
        way2int <- data.frame(t(apply(data[,varsCluster1],1,combn,2,prod)),row.names = NULL)
        cor <- corr.test(as.matrix(way2int))
        cormatrix <- cor$r
        flc <- findCorrelation(cormatrix, 0.95)
        way2int <- way2int[,-flc]
        
#Tsne 

        library(Rtsne)
        set.seed(1234)
        tsne_out_train <- Rtsne(trainmat, check_duplicates = FALSE, pca = TRUE, 
                                max_iter = 1000, perplexity=60, theta=0.5, dims=2, verbose=TRUE)
        plot(tsne_out_train$Y, main="BarnesHutSNE")
        text(tsne_out_train$Y, labels=y, col=y)
        tsneFeat <- tsne_out_train$Y
        colnames(tsneFeat) <- c("tsnefeat1","tsnefeat2")
        write.csv(tsneFeat,"tsne2d.csv",row.names = F)
        
        set.seed(1234)
        tsne_out_test <- Rtsne(testmat, check_duplicates = FALSE, pca = TRUE, 
                                max_iter = 1000, perplexity=60, theta=0.5, dims=2, verbose=TRUE)
        plot(tsne_out_test$Y, main="BarnesHutSNE")
        text(tsne_out_test$Y, labels=y, col=y)
        tsneFeat <- tsne_out_test$Y
        colnames(tsneFeat) <- c("tsnefeat1","tsnefeat2")
        write.csv(tsneFeat,"tsne2dtest.csv",row.names = F)
        
        #write.csv(tsneFeat,"tsneXGBfull.csv",row.names = F) 
#Svd

        svd_out <- svd(trainmat) #xgb2
        u <- svd_out$u
        #library irlba for truncated svd
        
#Autoencoder

        #full xgb2
        full[, sapply(full, is.character)] <- lapply(full[, sapply(full, is.character)],function(x)  as.numeric(factor(x)))
        
        train <- full[-Idx,]
        test <- full[Idx,]
        test$target <- NULL
        
        library(h2o)
        h2o.init(max_mem_size = "2g")
        
        train <- as.h2o(train, destination_frame="train.hex")
        test <- as.h2o(test, destination_frame="test.hex")
        
        autoencoder <-  h2o.deeplearning(x=2:50,
                 training_frame = train,
                 autoencoder = T,
                 activation= "Tanh",
                 hidden=c(100,50,2,50,100),
#                  hidden_dropout_ratio=c(0.5,0.5,0.5,0.5),
#                  input_dropout_ratio=0.05,
                 epochs=80,
                 rho=0.99,
                 epsilon=1e-8,
                 # train_samples_per_iteration=2000,
                 max_w2=10,
                 seed=111)
                
        train_supervised_features2 = h2o.deepfeatures(autoencoder, train, layer=3)
        
        plotdata2 = as.data.frame(train_supervised_features2)
        plotdata2$label = as.character(as.vector(y))
        
        library(ggplot2)
        qplot(DF.L3.C1, DF.L3.C2, data = plotdata2, color = label, main = "autoencoder ")
        write.csv(plotdata2,"autoencoderoutput.csv",row.names = F)
        
        
#Brute force feature expansion using dcast.data.table

        train <- full[-Idx,]
        library(data.table)
        data <- train[,c("v72","v129","v62","v38","NAcount")] #target
        data2 <- train[,sapply(train,is.character)]
        data2 <- cbind(data,data2)
        data2 <- cbind(ID = train$ID,data2)
        data2 <- data.table(data2)
        vars <- names(data2)
        
        #v3
        feat1v3 <- dcast.data.table(data2,ID ~ v3,fun.aggregate = sum,value.var = "v72")
        feat2v3 <- dcast.data.table(data2,ID ~ v3,fun.aggregate = sum,value.var = "v129")
        feat3v3 <- dcast.data.table(data2,ID ~ v3,fun.aggregate = sum,value.var = "v62")
        feat4v3 <- dcast.data.table(data2,ID ~ v3,fun.aggregate = sum,value.var = "v38")
        
        feat1v24 <- dcast.data.table(data2,ID ~ v24,fun.aggregate = sum,value.var = "v72")
        feat2v24 <- dcast.data.table(data2,ID ~ v24,fun.aggregate = sum,value.var = "v129")
        feat3v24 <- dcast.data.table(data2,ID ~ v24,fun.aggregate = sum,value.var = "v62")
        feat4v24 <- dcast.data.table(data2,ID ~ v24,fun.aggregate = sum,value.var = "v38")
        
        
        feat1v30 <- dcast.data.table(data2,ID ~ v30,fun.aggregate = sum,value.var = "v72")
        feat2v30 <- dcast.data.table(data2,ID ~ v30,fun.aggregate = sum,value.var = "v129")
        feat3v30 <- dcast.data.table(data2,ID ~ v30,fun.aggregate = sum,value.var = "v62")
        feat4v30 <- dcast.data.table(data2,ID ~ v30,fun.aggregate = sum,value.var = "v38")
        
        
        feat1v31 <- dcast.data.table(data2,ID ~ v31,fun.aggregate = sum,value.var = "v72")
        feat2v31 <- dcast.data.table(data2,ID ~ v31,fun.aggregate = sum,value.var = "v129")
        feat3v31 <- dcast.data.table(data2,ID ~ v31,fun.aggregate = sum,value.var = "v62")
        feat4v31 <- dcast.data.table(data2,ID ~ v31,fun.aggregate = sum,value.var = "v38")
        
        
        feat1v47 <- dcast.data.table(data2,ID ~ v47,fun.aggregate = sum,value.var = "v72")
        feat2v47 <- dcast.data.table(data2,ID ~ v47,fun.aggregate = sum,value.var = "v129")
        feat3v47 <- dcast.data.table(data2,ID ~ v47,fun.aggregate = sum,value.var = "v62")
        feat4v47 <- dcast.data.table(data2,ID ~ v47,fun.aggregate = sum,value.var = "v38")
        
        
        feat1v52 <- dcast.data.table(data2,ID ~ v52,fun.aggregate = sum,value.var = "v72")
        feat2v52 <- dcast.data.table(data2,ID ~ v52,fun.aggregate = sum,value.var = "v129")
        feat3v52 <- dcast.data.table(data2,ID ~ v52,fun.aggregate = sum,value.var = "v62")
        feat4v52 <- dcast.data.table(data2,ID ~ v52,fun.aggregate = sum,value.var = "v38")
        
        feat1v56 <- dcast.data.table(data2,ID ~ v56,fun.aggregate = sum,value.var = "v72")
        feat2v56 <- dcast.data.table(data2,ID ~ v56,fun.aggregate = sum,value.var = "v129")
        feat3v56 <- dcast.data.table(data2,ID ~ v56,fun.aggregate = sum,value.var = "v62")
        feat4v56 <- dcast.data.table(data2,ID ~ v56,fun.aggregate = sum,value.var = "v38")
        
        
        # using loops
        vector <- names(train[,sapply(train,is.character)])
        vector <- setdiff(vector,"v22")
        
        allnames <- c()
        for( i in 1: length(vector) ){
                print(vector[i])
                
                name1 <- paste0("feat1",vector[i])
                name2 <- paste0("feat2",vector[i])
                name3 <- paste0("feat3",vector[i])
                name4 <- paste0("feat4",vector[i])
                name5 <- paste0("feat5",vector[i])
                
                names <- rbind(name1,name2,name3,name4,name5)
                
                v <- Matrix(as.matrix(dcast.data.table(data2,ID ~ get(vector[i]),fun.aggregate = sum,value.var = "NAcount")),sparse = T)
                v <- v[,2:ncol(v)]
                w <- Matrix(as.matrix(dcast.data.table(data2,ID ~ get(vector[i]),fun.aggregate = sum,value.var = "v72")),sparse = T)
                w <- w[,2:ncol(w)]
                x <- Matrix(as.matrix(dcast.data.table(data2,ID ~ get(vector[i]),fun.aggregate = sum,value.var = "v129")),sparse = T)
                x <- x[,2:ncol(x)]
                y <- Matrix(as.matrix(dcast.data.table(data2,ID ~ get(vector[i]),fun.aggregate = sum,value.var = "v62")),sparse = T)
                y <- y[,2:ncol(y)]
                z <- Matrix(as.matrix(dcast.data.table(data2,ID ~ get(vector[i]),fun.aggregate = sum,value.var = "v38")),sparse = T)
                z <- z[,2:ncol(z)]
                
                assign(name1,w)
                assign(name2,x)
                assign(name3,y)
                assign(name4,z)
                assign(name5,v)
                
                allnames <- rbind.data.frame(allnames,names)
                
        
        }
        
        newVars <- as.vector(as.character(allnames$V1))
        train <- train[,setdiff(names(train),vector)]
  
        trainmat <- sparse.model.matrix(ID ~ .-1,data = train)
        x <- do.call(cbind,mget(newVars))
        trainmat <- cbind(trainmat,x)
        knn <- read.csv("DistKnn4.csv")
        trainmat <- cbind(trainmat,as.matrix(knn[1:nrow(trainmat),]))
        
       
        #Train = test
                train$v47[train$v47 == "H" ]<- "Missing"
                test$v56[test$v56 == "AD"] <- "AE"
                test$v56[test$v56 == "AQ"] <- "AP"
                train$v56[train$v56 == "AT"] <- "Missing"
                train$v56[train$v56 = "AX"] <- "Missing"
                train$v56[train$v56 == "BO"] <- "Missing"
                train$v56[train$v56 == "CE"] <- "Missing"
                train$v56[train$v56 == "CG"] <- "Missing"
                train$v56[train$v56 == "CK"] <- "Missing"
                test$v56[test$v56 == "CR"] <- "Missing"
                test$v56[test$v56  == "CU"] <- "Missing"
                train$v56[train$v56 == "CZ"] <- "Missing"
                train$v56[train$v56 == "DB"] <- "Missing"
                train$v56[train$v56 == "DC"] <- "Missing"
                test$v56[test$v56 == "J"] <- "Missing"
                test$v56[test$v56 == "K"] <- "Missing"
                test$v56[test$v56 == "S"] <- "Missing"
                train$v56[train$v56 == "X"] <- "Missing"
                
                train$v71[train$v71 == "A"] <- "Missing"
                train$v71[train$v71 == "D"] <- "Missing"
                train$v71[train$v71 == "K"] <- "Missing"
                test$v71[test$v71 == "E"] <- "Missing"
                test$v71[test$v71 == "H"] <- "Missing"
                test$v71[test$v71 == "J"] <- "Missing"
                
                train$v79[train$v79 == "L"] <- "Missing"
                
                
                test$v113[test$v113 == "K"] <- "AK" 
                
#Other approach

	data <- full[,excluded]
	vector <- names(cut)
	names <- list()
	for ( i in 1:length(vector)){
			b = i + 1
			nam <- paste("Diff",i, "-",b,sep = "")
			x <- data[,vector[i]] - data[,vector[b]]
			assign(nam,x)
			names[[i]] <- nam
			
	}
	names <- do.call(rbind,names)

	full2 <- data.frame(full,mget(names))

	##assign to assign names to a object
	##get to retrieve objects. mget = multiple get
	#Lots of features,try combs only with most important features (xgboost)

	excluded <- c("v41", "v95", "v67", "v29", "v20", "v26", "v42", "v53", "v32", "v11", "v68", "v49", "v17", "v94", "v83", "v33", "v118", "v96", "v9", "v65", "v106", "v43", "v78", "v77", "v73", "v19", "v60", "v86", "v100", "v64", "v121", "v13", "v7", "v115", "v25", "v61", "v92", "v48", "v90", "v46", "v70", "v104", "v37", "v59", "v128", "v55", "v93", "v84", "v63", "v123", "v35", "v5", "v4", "v44", "v111", "v116", "v36", "v57", "v108", "v87", "v15", "v105", "v27", "v101", "v51", "v6", "v54", "v97", "v45", "v103", "v1", "v99", "v98", "v80", "v18", "v69", "v76", "v85", "v117", "v126")


	data <- full[,excluded]
	data$ID <- full$ID

	fullmat <- sparse.model.matrix(ID ~ .-1,data = data)
	trainmat <- fullmat[-Idx,]
	testmat <- fullmat[Idx,]
	trainmat <- xgb.DMatrix(trainmat,label = y)
	testmat <- xgb.DMatrix(testmat)

	param <- list(
			
			"objective"  = "binary:logistic"
			, "eval_metric" = "logloss"
			, "eta" = 0.01
			, "subsample" = 0.8
			, "colsample_bytree" = 0.45
			, "min_child_weight" = 1
			, "max_depth" = 10
	)

	set.seed(111)
	nround.cv = 1600
	cv <- xgb.cv(param = param,data=trainmat,label=y,nfold=5,nrounds= nround.cv,predictions = T,verbose = T)
	min.error <- which.min(cv[,test.logloss.mean])
	set.seed(111)
	model1 <- xgboost(param = param,data = trainmat,label = y,nrounds = 316)


	##Variable importance plot
	model <- xgb.dump(model1,with.stats = TRUE)
	names <- dimnames(data)[[2]]
	importance <- xgb.importance(names,model = model1)
	p <- xgb.plot.importance(importance[1:15])
	print(p)

	# Output:
	# Feature        Gain       Cover   Frequence
	# 1:    v123 0.020024985 0.040574765 0.013918662
	# 2:     v68 0.019948980 0.025145750 0.021516097
	# 3:     v85 0.018355869 0.028878210 0.015523894
	# 4:     v99 0.017836848 0.014554271 0.016560814
	# 5:      v6 0.017665696 0.031152345 0.016351436
	# 6:     v57 0.016541090 0.014432078 0.016092206
	# 7:     v18 0.016423961 0.019918379 0.015593687
	# 8:      v4 0.016361353 0.032304842 0.014357359
	# 9:     v70 0.015125280 0.013395153 0.014596648
	# 10:     v48 0.015039276 0.022839775 0.009601484
	# 11:      v1 0.015023774 0.016669496 0.013649461
	# 12:     v45 0.014957279 0.013309085 0.013380261
	# 13:      v9 0.014700547 0.013776249 0.015404249
	# 14:     v78 0.014673831 0.007118975 0.015384309
	# 15:    v126 0.014573769 0.013984367 0.013759135
	# 16:     v51 0.014356859 0.021066932 0.012742156
	# 17:    v101 0.014354259 0.016186337 0.012752126
	# 18:     v37 0.014320739 0.007368421 0.014357359
	# 19:    v100 0.014288648 0.013147209 0.014556766
	# 20:     v55 0.014218349 0.023756084 0.012233666
	# 21:     v44 0.014215159 0.012241470 0.013868810
	# 22:    v118 0.014211429 0.018008671 0.014975523
	# 23:     v93 0.014195628 0.019390659 0.012772067
	# 24:     v90 0.014190322 0.013117059 0.013818958
	# 25:     v94 0.014070219 0.013533950 0.015115108
	# 26:     v80 0.014031092 0.019813824 0.013081149
	# 27:    v115 0.014019021 0.009929631 0.015404249
	# 28:     v69 0.013599194 0.012135353 0.012542748
	# 29:    v111 0.013583978 0.015116541 0.012662393
	# 30:     v27 0.013524722 0.006978180 0.013380261
	# 31:     v35 0.013492059 0.012915855 0.013270586
	# 32:     v97 0.013360922 0.013216924 0.012353311
	# 33:     v32 0.013133133 0.011381274 0.015444131
	# 34:     v20 0.012703237 0.011291488 0.016261703
	# 35:     v36 0.012664602 0.035822245 0.010997338
	# 36:     v87 0.012577545 0.010342877 0.011755087
	# 37:     v26 0.012564801 0.009138752 0.016710370
	# 38:     v19 0.012550142 0.015649786 0.012592600
	# 39:     v61 0.012502667 0.009560384 0.011954495
	# 40:     v33 0.012327216 0.009555770 0.013230705
	# 41:     v84 0.012318151 0.008540191 0.011914614
	# 42:     v42 0.012097993 0.009792522 0.014875819
	# 43:    v103 0.012078207 0.008732913 0.011495857
	# 44:     v83 0.011836255 0.008585866 0.013549757
	# 45:     v53 0.011805727 0.009580763 0.014437122
	# 46:     v98 0.011489982 0.007313759 0.011027249
	# 47:     v13 0.011488041 0.007375909 0.011316390
	# 48:     v41 0.011487084 0.009616783 0.018594774
	# 49:     v59 0.011485407 0.007234706 0.011725176
	# 50:     v95 0.011484561 0.007673702 0.016999511
	# 51:     v46 0.011388457 0.008013602 0.011396153
	# 52:    v121 0.011330045 0.008676263 0.010628434
	# 53:      v7 0.011201775 0.010365223 0.010967427
	# 54:     v49 0.011200608 0.011620262 0.011794969
	# 55:     v17 0.011190288 0.012924361 0.013250646
	# 56:     v73 0.011070283 0.010539795 0.011067131
	# 57:     v11 0.010948246 0.008929718 0.012193784
	# 58:     v77 0.010732968 0.005582879 0.010209677
	# 59:    v128 0.010662001 0.014577282 0.010498819
	# 60:    v108 0.010659424 0.009786919 0.010060121
	# 61:     v65 0.010624633 0.006473361 0.010927545
	# 62:     v86 0.010483885 0.007215894 0.010827841
	# 63:     v15 0.010448719 0.007695361 0.009611454
	# 64:     v25 0.010171881 0.008737688 0.011256568
	# 65:    v104 0.010168397 0.007526519 0.009790921
	# 66:     v63 0.010090517 0.005644424 0.009392105
	# 67:      v5 0.010073977 0.011677454 0.010289440
	# 68:    v106 0.009880664 0.005758174 0.010907604
	# 69:     v67 0.009862317 0.005831736 0.013499905
	# 70:     v54 0.009794947 0.007013715 0.008903556
	# 71:     v43 0.009785659 0.007499949 0.010289440
	# 72:     v60 0.009114079 0.006041573 0.008734060
	# 73:    v117 0.009059887 0.009228731 0.008215600
	# 74:     v29 0.008949371 0.006057365 0.010847782
	# 75:    v105 0.008801570 0.007592856 0.008375126
	# 76:     v92 0.008531216 0.006609860 0.007896547
	# 77:     v96 0.008391379 0.009561033 0.007886577
	# 78:    v116 0.007562055 0.005104689 0.007158739
	# 79:     v64 0.007044864 0.008008173 0.006739982
	# 80:     v76 0.006895998 0.006538610 0.005922410
	# Feature        Gain       Cover   Frequence

	MaxGainF <- importance$Feature[1:11]
