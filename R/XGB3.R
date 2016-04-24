#XGB3 Validation test : 0.457286. LB =  0.45556 (nrounds = min.error)
                #train : 0.2574   LB =  0.45551 nrounds = 3500

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

#Remove variables 

	#cat(paste0('"', paste(excluded, collapse="\", \""), '"'))
	# excluded <- c('v8','v23','v25','v31','v36','v37','v46','v51','v53','v54','v63','v73','v75','v79','v81','v82','v89','v92','v95','v105','v107','v108','v109','v110','v116','v117','v118','v119','v123','v124','v128')
	excluded <- c("v41", "v95", "v67", "v29", "v20", "v26", "v42", "v53", "v32", "v11", "v68", "v49", "v17", "v94", "v83", "v33", "v118", "v96", "v9", "v65", "v106", "v43", "v78", "v77", "v73", "v19", "v60", "v86", "v100", "v64", "v121", "v13", "v7", "v115", "v25", "v61", "v92", "v48", "v90", "v46", "v70", "v12", "v104", "v37", "v59", "v128", "v55", "v93", "v84", "v63", "v123", "v35", "v5", "v4", "v44", "v111", "v34", "v116", "v36", "v57", "v108", "v87", "v15", "v105", "v27", "v101", "v51", "v6", "v54", "v97", "v45", "v103", "v1", "v99", "v98", "v80", "v18", "v69", "v40", "v76", "v85", "v117", "v126")

	#Full without excluded variables

	full <- full[,setdiff(names(full),excluded)]

# 
# #Feature creation script
# 
# data <- full[,excluded]
# full <- full[,setdiff(names(full),excluded)]
# 
# vector <- names(cut)
# names <- list()
# for ( i in 1:length(vector)){
#         b = i + 1
#         nam <- paste("Diff",i,"-",b,sep = "")
#         x <- data[,vector[i]] - data[,vector[b]]
#         assign(nam,x)
#         names[[i]] <- nam
#         
# }
# names <- do.call(rbind,names)
# 
# full <- data.frame(full,mget(names)) 


# names <- list()
# for ( i in 1:length(vector)){
#         b = i + 2
#         nam <- paste("Diff",i, "-",b,sep = "")
#         x <- data[,vector[i]] - data[,vector[b]]
#         assign(nam,x)
#         names[[i]] <- nam
#         
# }
# names <- do.call(rbind,names)
# 
# full <- data.frame(full,mget(names)) 



#Add NAs

	full <- cbind(full,NAcount)
	barplot(table(full$NAcount)) #Two clusters
	full$Na100<- ifelse(full$NAcount >= 100,1,0)
	full$Na10 <- ifelse(full$NAcount <= 10,1,0)
	full$Na10 <- factor(full$Na10)
	full$Na100 <- factor(full$Na100)


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
	nround.cv = 3500
	cv <- xgb.cv(param = param,data=trainmat,label=y,nfold=5,nrounds= nround.cv,predictions = T,verbose = T)
	min.error <- which.min(cv[,test.logloss.mean]) #3364
	set.seed(111)
	model1 <- xgboost(param = param,data = trainmat,label = y,nrounds = 3500)

#Predictions

pred <- predict(model1,testmat)
pred2 <- matrix(pred,nrow=1,ncol = length(pred))
pred2 <- t(pred2)

#Submission

sub <- data.frame(ID = test$ID, PredictedProb = pred)
write.csv(sub,"xgb3test.csv",row.names = FALSE)

#Variable importance

trainmat <- fullmat[-Idx,]
model <- xgb.dump(model1,with.stats = TRUE)
names <- dimnames(trainmat)[[2]]
importance <- xgb.importance(names,model = model1)
print(data.frame(importance))

