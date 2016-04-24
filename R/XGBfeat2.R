#Xgb+newfeatures #Below 0.45 logloss


	library(xgboost)
	library(Matrix)


	setwd("C:/Users/Iker/Desktop/bnp")
	train <- read.csv("train.csv" ,na.strings = c("NA",""),stringsAsFactors = F)
	test <- read.csv("test.csv" ,na.strings = c("NA",""),stringsAsFactors = F)

	train$v47[train$v47 == "H" ]<- "J"
	test$v56[test$v56 == "AD"] <- "AE"
	test$v56[test$v56 == "AQ"] <- "AP"
	train$v56[train$v56 == "AT"] <- "Missing"
	train$v56[train$v56 == "AX"] <- "Missing"
	train$v56[train$v56 == "BO"] <- "Missing"
	train$v56[train$v56 == "CE"] <- "Missing"
	train$v56[train$v56 == "CG"] <- "Missing"
	train$v56[train$v56 == "CK"] <- "Missing"
	test$v56[test$v56 == "CR"] <- "Missing"
	test$v56[test$v56  == "CU"] <- "Missing"
	train$v56[train$v56 == "CZ"] <- "Missing"
	train$v56[train$v56 == "DB"] <- "Missing"
	train$v56[train$v56 == "DC"] <- "Missing"
	train$v56[train$v56 == "I"] <- "Missing"
	train$v56[train$v56 == "H"] <- "Missing"
	train$v56[train$v56 == "L"] <- "Missing"
	test$v56[test$v56 == "J"] <- "Missing"
	test$v56[test$v56 == "K"] <- "Missing"
	test$v56[test$v56 == "S"] <- "Missing"
	test$v56[test$v56 == "BB"] <- "Missing"
	train$v56[train$v56 == "X"] <- "Missing"
	train$v71[train$v71 == "A"] <- "Missing"
	train$v71[train$v71 == "D"] <- "Missing"
	train$v71[train$v71 == "K"] <- "Missing"
	test$v71[test$v71 == "E"] <- "Missing"
	test$v71[test$v71 == "H"] <- "Missing"
	test$v71[test$v71 == "J"] <- "Missing"

	train$v79[train$v79 == "L"] <- "M"


	test$v113[test$v113 == "K"] <- "AK" 



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
	y_true <- as.matrix(train$target)

	y2 <- full$target

	full$target <- NULL

	full$v22 <- as.numeric(factor(full$v22))


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


#Prepare trainmat

	train <- full[-Idx,]
	library(data.table)
	data <- train[,c("v72","v129","v62","v38","NAcount")] #target
	data2 <- train[,sapply(train,is.character)]
	data2 <- cbind(data,data2)
	data2 <- cbind(ID = train$ID,data2)
	data2 <- data.table(data2)
	vars <- names(data2)


	#for loop
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

#Prepare testmat


	test <- full[Idx,]
	library(data.table)
	data <- test[,c("v72","v129","v62","v38","NAcount")] #target
	data2 <- test[,sapply(test,is.character)]
	data2 <- cbind(data,data2)
	data2 <- cbind(ID = test$ID,data2)
	data2 <- data.table(data2)
	vars <- names(data2)


	#for loop
	vector <- names(test[,sapply(test,is.character)])
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
	test <- test[,setdiff(names(test),vector)]

	testmat <- sparse.model.matrix(ID ~ .-1,data = test)
	x <- do.call(cbind,mget(newVars))
	testmat <- cbind(testmat,x)
	testmat <- cbind(testmat,as.matrix(knn[114322:nrow(fullmat),]))

#Model

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
	nround.cv = 5000 
	cv <- xgb.cv(param = param,data=trainmat,label=y_true,nfold=5,nrounds= nround.cv,predictions = T,verbose = T)
	min.error <- which.min(cv[,test.logloss.mean])
	set.seed(111)
	model1 <- xgboost(param = param,data = trainmat,label = y_true,nrounds = min.error)


#Predictions

	pred <- predict(model1,testmat)

#Submission

	sub <- data.frame(ID = test$ID, PredictedProb = pred)
	write.csv(sub,"xgbFullfeat2.csv",row.names = FALSE)#0.4523

	model <- xgb.dump(model1,with.stats = TRUE)
	names <- dimnames(trainmat)[[2]]
	importance <- xgb.importance(names,model = model1)
	p <- xgb.plot.importance(importance[1:15])
	print(p)