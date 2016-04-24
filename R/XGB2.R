#XGB2 0.4576 LB = 0.45631  test # 0.45601 nround = 3700
                           #test2 0.45626 nround = 4000


	library(xgboost)
	library(Matrix)


	setwd("C:/Users/Iker/Desktop/bnp")
	train <- read.csv("train.csv" ,na.strings = c("NA",""),stringsAsFactors = F)
	test <- read.csv("test.csv" ,na.strings = c("NA",""),stringsAsFactors = F)
	test$target <- rep(10,nrow(test))
	full <- rbind(train,test)



#Index for data split

	Idx <- which(full$target == 10)

#Get NAS per row

	NAcount <- apply(full,1,function(x) sum(is.na(x)))
	# NonNAcount <- ncol(full) - NAcount
	# Count20 <- apply(full,1,function(x) length(grep("^20", x)))
	# NAratio <- NAcount/NonNAcount


#Fill NAs

	vector2 <- colnames(full)
	for (i in 1:length(vector2)){
			if (class(full[,vector2[i]]) == "numeric") {
					full[which(is.na(full[,vector2[i]])),vector2[i]] <- -1}
			else if(class(full[,vector2[i]]) == "integer") {
					full[which(is.na(full[,vector2[i]])),vector2[i]] <- -1}
			else {full[which(is.na(full[,vector2[i]])),vector2[i]] <- "Missing"}
	}

#Change variable classes
	y <- as.matrix(train$target)
	rm(train)

	y2 <- full$target

	full$target <- NULL

	full$v22 <- as.numeric(factor(full$v22))
	full$v62 <- as.numeric(full$v62)
	full$v72 <- as.numeric(full$v72)
	full$v129 <- as.numeric(full$v129)

	#Remove variables through VIF
	# 
	# nums <- sapply(full, is.numeric)
	# numtrain <- full[,nums == TRUE]
	# 
	# library(usdm)
	# multic <- vifstep(numtrain,th=10)
	# excluded <- attr(multic,"excluded")
	# 
	# excluded

	#cat(paste0('"', paste(excluded, collapse="\", \""), '"'))
	excluded <- c("v41", "v95", "v67", "v29", "v20", "v26", "v42", "v53", "v32", "v11", "v68", "v49", "v17", "v94", "v83", "v33", "v118", "v96", "v9", "v65", "v106", "v43", "v78", "v77", "v73", "v19", "v60", "v86", "v100", "v64", "v121", "v13", "v7", "v115", "v25", "v61", "v92", "v48", "v90", "v46", "v70", "v12", "v104", "v37", "v59", "v128", "v55", "v93", "v84", "v63", "v123", "v35", "v5", "v4", "v44", "v111", "v34", "v116", "v36", "v57", "v108", "v87", "v15", "v105", "v27", "v101", "v51", "v6", "v54", "v97", "v45", "v103", "v1", "v99", "v98", "v80", "v18", "v69", "v40", "v76", "v85", "v117", "v126")
	#v12 v34 v40 not correlated 

#Full without excluded variables

	full <- full[,setdiff(names(full),excluded)]

#Add NAs

	full <- cbind(full,NAcount)

	fullmat <- sparse.model.matrix(ID ~ .-1,data = full)
	trainmat <- fullmat[-Idx,]
	testmat <- fullmat[Idx,]

	trainmat <- xgb.DMatrix(trainmat,label = y)
	testmat <- xgb.DMatrix(testmat)

#Model cv

	param <- list(
			
			"objective"  = "binary:logistic"
			, "eval_metric" = "logloss"
			, "eta" = 0.005
			, "subsample" = 0.90
			, "colsample_bytree" = 0.35
			, "min_child_weight" = 1
			, "max_depth" = 12 
	)

	set.seed(111)
	nround.cv = 4000
	cv <- xgb.cv(param = param,data=trainmat,label=y,nfold=5,nrounds= nround.cv,predictions = T,verbose = T)
	min.error <- which.min(cv[,test.logloss.mean])
	set.seed(111)
	model1 <- xgboost(param = param,data = trainmat,label = y,nrounds = 3700)

#Predictions

	pred <- predict(model1,testmat)
	pred2 <- matrix(pred,nrow=1,ncol = length(pred))
	pred2 <- t(pred2)

#Submision

	gc()
	sub <- data.frame(ID = test$ID, PredictedProb = pred)
	write.csv(sub,"xgb2.csv",row.names = FALSE)

