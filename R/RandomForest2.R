#Random Forest 1 #local validation 0.457 #Lb = #0.462


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


#Dealing with factors

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


	rfModel<-h2o.randomForest(
			x = 3:51, 
			y= 2, 
			training_frame = train,
			#validation_frame = splits[[2]],
			model_id="h2oRF1",
			ntrees =3000, 
			max_depth = 50, 
			mtries =-1, 
			sample_rate = 2/3, 
			nbins = 20, 
			seed = 11111111) 

	summary(rfModel)


	p<-as.data.frame(h2o.predict(rfModel,test))
	testIds<-as.data.frame(test$ID)
	submission<-data.frame(cbind(testIds,p$p1))
	colnames(submission)<-c("ID","PredictedProb")
	write.csv(submission,"rf2.csv",row.names=F)


# 0.460 train loglogss  0.465 validation
# > print(as.data.frame(h2o.varimp(rfModel)))
# variable relative_importance scaled_importance   percentage
# 1       v50          3925035.50       1.000000000 0.1041359284
# 2       v52          2266920.25       0.577554076 0.0601441298
# 3      v114          1829639.88       0.466146071 0.0485425538
# 4       v22          1644786.75       0.419050159 0.0436381773
# 5      v112          1554976.88       0.396168869 0.0412554130
# 6      v125          1512186.50       0.385266961 0.0401201327
# 7       v14          1412895.00       0.359969993 0.0374858094
# 8       v21          1379218.12       0.351389975 0.0365923212
# 9       v10          1294011.50       0.329681477 0.0343316866
# 10      v56          1013619.31       0.258244623 0.0268925435
# 11     v107          1011850.75       0.257794038 0.0268456215
# 12      v91          1008714.19       0.256994921 0.0267624047
# 13     v113           982975.50       0.250437353 0.0260795262
# 14      v66           964821.12       0.245812076 0.0255978687
# 15      v30           794514.56       0.202422262 0.0210794301
# 16      v24           763897.56       0.194621823 0.0202671242
# 17      v79           759707.25       0.193554237 0.0201559501
# 18      v28           607817.19       0.154856482 0.0161261235
# 19       v2           606432.06       0.154503587 0.0160893744
# 20      v16           600673.69       0.153036498 0.0159365978
# 21     v127           594919.31       0.151570428 0.0157839273
# 22      v81           592782.62       0.151026054 0.0157272384
# 23     v122           591291.31       0.150646106 0.0156876721
# 24      v47           589482.50       0.150185266 0.0156396821
# 25     v120           574343.94       0.146328342 0.0152380378
# 26     v109           571982.44       0.145726692 0.0151753843
# 27      v58           570859.12       0.145440500 0.0151455815
# 28     v131           567175.31       0.144501957 0.0150478455
# 29     v119           555504.81       0.141528608 0.0147382130
# 30      v88           543581.56       0.138490865 0.0144218748
# 31     v130           522825.06       0.133202633 0.0138711798
# 32     v102           502355.69       0.127987553 0.0133281026
# 33      v82           455129.22       0.115955440 0.0120751274
# 34      v62           441791.28       0.112557270 0.0117212558
# 35      v71           416279.41       0.106057488 0.0110443950
# 36      v39           396421.06       0.100998083 0.0105175292
# 37      v72           392310.91       0.099950919 0.0104084818
# 38      v89           384880.06       0.098057728 0.0102113325
# 39       v8           381680.34       0.097242520 0.0101264401
# 40     v124           346596.84       0.088304130 0.0091956325
# 41      v31           334184.19       0.085141698 0.0088663098
# 42  NAcount           333970.31       0.085087208 0.0088606354
# 43     v129           283369.78       0.072195470 0.0075181423
# 44     v110           275393.56       0.070163330 0.0073065235
# 45      v75           242833.19       0.061867768 0.0064426575
# 46      v23           193575.22       0.049318081 0.0051357842
# 47      v38            63245.21       0.016113284 0.0016779718
# 48      v74            23527.54       0.005994225 0.0006242142
# 49       v3            14477.71       0.003688554 0.0003841110


variables <- c("v50","v52","v114","v22","v112","v125","v14","v21","v10","v56","v107","v91","v113","v66","v30")
names <- list()
for ( i in 1:length(variables)){
        nam <- paste("log",i,sep = "")
        x <- log(full[,variables[i]]+1)
        assign(nam,x)
        names[[i]] <- nam
        
}
names <- do.call(rbind,names)

full <- data.frame(full,mget(names)) 


names <- list()
for ( i in 1:length(variables)){
        nam <- paste("invers",i,sep = "")
        x <- 1/(full[,variables[i]])
        assign(nam,x)
        names[[i]] <- nam
        
}
names <- do.call(rbind,names)

full <- data.frame(full,mget(names)) 


names <- list()
for ( i in 1:length(variables)){
        nam <- paste("sqrt",i,sep = "")
        x <- sqrt(full[,variables[i]])
        assign(nam,x)
        names[[i]] <- nam
        
}
names <- do.call(rbind,names)

full <- data.frame(full,mget(names)) 

