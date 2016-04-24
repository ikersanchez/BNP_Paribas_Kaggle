#Glmnet #0.48171

library(glmnet)
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
	#v12 v34 v40 not correlated # Check

	#Full without excluded variables

		full <- full[,setdiff(names(full),excluded)]

#Add NAs

	full <- cbind(full,NAcount)

	fullmat <- model.matrix(ID ~ .-1,data = full)

#KNN

	library(FNN)
	kn <- get.knn(fullmat,k = 4)
	dist <-  kn$nn.dist
	colnames(dist) <- c("distKNN1","distKNN2","distKNN3","distKNN4")# QUITE
	fullmat <- cbind(fullmat,dist)
	#
	trainmat <- fullmat[-Idx,]
	testmat <- fullmat[Idx,]

#model 

	rm(fullmat)

	cvfit = cv.glmnet(x = trainmat, y = y, family = "binomial", type.measure = "deviance")

	preds <- predict(cvfit, testmat, s = "lambda.min", type = "response")


	sub <- data.frame(ID = test$ID, PredictedProb = preds)
	colnames(sub) <- c("ID","PredictedProb")
	write.csv(sub,"glmnet1.csv",row.names = FALSE)
