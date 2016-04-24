#Neural network with h2o


setwd("C:/Users/Iker/Desktop/bnp")
train <- read.csv("train.csv" ,na.strings = c("NA",""),stringsAsFactors = F)
test <- read.csv("test.csv" ,na.strings = c("NA",""),stringsAsFactors = F)



#Get NAS per row

	NAcount <- apply(train,1,function(x) sum(is.na(x)))
	train <- cbind(train,NAcount)
	NAcount <- apply(test,1,function(x) sum(is.na(x)))
	test <- cbind(test,NAcount)


#Fill NAs

	vector2 <- colnames(train)
	for (i in 1:length(vector2)){
			if (class(train[,vector2[i]]) == "numeric") {
					train[which(is.na(train[,vector2[i]])),vector2[i]] <- -1}
			else {train[which(is.na(train[,vector2[i]])),vector2[i]] <- "Missing"}
	}


	vector2 <- colnames(test)
	for (i in 1:length(vector2)){
			if (class(test[,vector2[i]]) == "numeric") {
					test[which(is.na(test[,vector2[i]])),vector2[i]] <- -1}
			else {test[which(is.na(test[,vector2[i]])),vector2[i]] <- "Missing"}
	}


#Change variable classes


	train$v22 <- as.numeric(factor(train$v22))
	train$v62 <- as.numeric(train$v62)
	train$v72 <- as.numeric(train$v72)
	train$v129 <- as.numeric(train$v129)

	test$v22 <- as.numeric(factor(test$v22))
	test$v62 <- as.numeric(test$v62)
	test$v72 <- as.numeric(test$v72)
	test$v129 <- as.numeric(test$v129)

#Remove variables

	excluded <- c("v41", "v95", "v67", "v29", "v20", "v26", "v42", "v53", "v32", "v11", "v68", "v49", "v17", "v94", "v83", "v33", "v118", "v96", "v9", "v65", "v106", "v43", "v78", "v77", "v73", "v19", "v60", "v86", "v100", "v64", "v121", "v13", "v7", "v115", "v25", "v61", "v92", "v48", "v90", "v46", "v70", "v12", "v104", "v37", "v59", "v128", "v55", "v93", "v84", "v63", "v123", "v35", "v5", "v4", "v44", "v111", "v34", "v116", "v36", "v57", "v108", "v87", "v15", "v105", "v27", "v101", "v51", "v6", "v54", "v97", "v45", "v103", "v1", "v99", "v98", "v80", "v18", "v69", "v40", "v76", "v85", "v117", "v126")

#Full without excluded variables

	train <- train[,setdiff(names(train),excluded)]
	test <- test[,setdiff(names(test),excluded)]


#Deal with Factors

	test$target <- rep(10,nrow(test))
	full <- rbind(train,test)
	idx <- which(full$target == 10)

	full$ID <- as.numeric(full$ID)
	full$target <- as.numeric(full$target)

	full[, sapply(full, is.character)] <- lapply(full[, sapply(full, is.character)],function(x)  as.numeric(factor(x)))

	train <- full[-idx,]
	test <- full[idx,]
	test$target <- NULL

#Model

	library(h2o)
	h2o.init(max_mem_size = "2g")

	train <- as.h2o(train, destination_frame="train.hex")
	test <- as.h2o(test, destination_frame="test.hex")
	train$target<-as.factor(train$target)
	splits<-h2o.splitFrame(train,0.9,destination_frames = c("trainSplit","validSplit"),seed=111111111)

	model <- h2o.deeplearning(x=3:48,
							  y=2,
							  training_frame = splits[[1]],
							  validation_frame = splits[[2]],
							  classification_stop = -1,
							  activation="RectifierWithDropout",
							  hidden=c(1500,750,300),
							  hidden_dropout_ratio=c(0.5,0.5,0.5),
							  input_dropout_ratio=0.05,
							  epochs=50,
							  train_samples_per_iteration=2000,
							  max_w2=10,
							  seed=111)
#Predict

	p<-as.data.frame(h2o.predict(model,test))
	testIds<-as.data.frame(test$ID)
	submission<-data.frame(cbind(testIds,p$p1))
	colnames(submission)<-c("ID","PredictedProb")
	write.csv(submission,"nn1.csv",row.names=F)