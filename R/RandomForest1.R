#Random Forest 1 #local validation 0.457 #Lb = #0.465


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

	excluded <- c('v8','v23','v25','v31','v36','v37','v46','v51','v53','v54','v63','v73','v75','v79','v81','v82','v89','v92','v95','v105','v107','v108','v109','v110','v116','v117','v118','v119','v123','v124','v128')
	#Full without excluded variables

	train <- train[,setdiff(names(train),excluded)]
	test <- test[,setdiff(names(test),excluded)]


#Dealing with Factors

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
			x = 3:103, 
			y= 2, 
			training_frame = splits[[1]],
			validation_frame = splits[[2]],
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
	write.csv(submission,"rf1.csv",row.names=F)

# > print(as.data.frame(h2o.varimp(rfModel)))
# variable relative_importance scaled_importance   percentage
# 1        v50          2854910.25       1.000000000 0.0788293342
# 2        v52          1573151.50       0.551033610 0.0434376126
# 3        v22          1125060.12       0.394078982 0.0310649838
# 4       v112          1057179.50       0.370302184 0.0291906746
# 5        v12          1049822.75       0.367725308 0.0289875412
# 6       v125          1045112.75       0.366075518 0.0288574894
# 7        v14          1040105.56       0.364321632 0.0287192317
# 8        v40          1021570.00       0.357829112 0.0282074307
# 9       v114          1014155.75       0.355232095 0.0280027096
# 10       v34           993244.12       0.347907303 0.0274253011
# 11       v10           954117.56       0.334202297 0.0263449445
# 12       v21           946466.19       0.331522221 0.0261336760
# 13       v56           923063.81       0.323324985 0.0254874933
# 14       v91           916172.81       0.320911248 0.0252972201
# 15      v113           820849.31       0.287521933 0.0226651625
# 16       v47           820142.06       0.287274202 0.0226456340
# 17       v66           764844.31       0.267904854 0.0211187612
# 18       v24           613437.81       0.214871137 0.0169381487
# 19       v30           546564.81       0.191447284 0.0150916619
# 20       v62           446127.53       0.156266745 0.0123184034
# 21       v71           360342.38       0.126218460 0.0099497172
# 22      v129           344181.28       0.120557654 0.0095034796
# 23       v72           310514.81       0.108765175 0.0085738863
# 24       v87           288596.84       0.101087887 0.0079686908
# 25       v98           283012.47       0.099131827 0.0078144959
# 26        v5           275800.44       0.096605642 0.0076153584
# 27       v28           270639.09       0.094797759 0.0074728442
# 28       v70           260216.53       0.091147009 0.0071850581
# 29        v2           238887.16       0.083675890 0.0065961147
# 30       v58           236505.38       0.082841615 0.0065303493
# 31   NAcount           235391.95       0.082451612 0.0064996057
# 32      v120           235013.38       0.082319006 0.0064891525
# 33       v45           233863.06       0.081916082 0.0064573902
# 34       v69           231113.70       0.080953054 0.0063814753
# 35       v16           229710.25       0.080461461 0.0063427234
# 36      v115           228587.19       0.080068082 0.0063117136
# 37       v85           227008.55       0.079515126 0.0062681244
# 38       v78           226112.16       0.079201143 0.0062433734
# 39      v100           226007.70       0.079164556 0.0062404893
# 40        v1           223204.12       0.078182537 0.0061630773
# 41       v97           222639.09       0.077984621 0.0061474758
# 42        v9           221836.27       0.077703411 0.0061253082
# 43      v127           221534.03       0.077597547 0.0061169629
# 44      v122           221251.58       0.077498611 0.0061091639
# 45      v131           214170.41       0.075018262 0.0059136397
# 46       v88           211105.97       0.073944871 0.0058290249
# 47       v44           210846.31       0.073853920 0.0058218553
# 48      v101           210163.08       0.073614601 0.0058029900
# 49        v6           205895.14       0.072119654 0.0056851443
# 50       v59           203536.36       0.071293435 0.0056200140
# 51      v102           202399.89       0.070895360 0.0055886340
# 52       v80           201517.64       0.070586331 0.0055642735
# 53       v35           199480.64       0.069872824 0.0055080282
# 54       v99           197716.33       0.069254831 0.0054593123
# 55      v111           197368.53       0.069133007 0.0054497089
# 56      v103           194697.06       0.068197262 0.0053759448
# 57       v57           194165.14       0.068010944 0.0053612574
# 58       v27           190863.41       0.066854433 0.0052700904
# 59        v4           188167.41       0.065910095 0.0051956489
# 60      v106           185620.58       0.065018008 0.0051253263
# 61        v7           185354.84       0.064924928 0.0051179889
# 62       v84           184708.59       0.064698564 0.0051001447
# 63      v121           183097.31       0.064134175 0.0050556543
# 64       v61           181195.16       0.063467899 0.0050031322
# 65       v55           181165.08       0.063457364 0.0050023017
# 66       v68           180984.23       0.063394019 0.0049973083
# 67       v48           180890.97       0.063361350 0.0049947331
# 68       v83           178910.25       0.062667557 0.0049400418
# 69       v77           178853.89       0.062647816 0.0049384856
# 70       v15           177754.66       0.062262783 0.0049081337
# 71       v43           176396.78       0.061787155 0.0048706403
# 72       v64           176035.91       0.061660750 0.0048606758
# 73       v60           174601.86       0.061158441 0.0048210792
# 74       v26           174337.67       0.061065903 0.0048137845
# 75       v93           171307.78       0.060004612 0.0047301236
# 76       v33           170969.16       0.059886000 0.0047207735
# 77       v18           167240.83       0.058580065 0.0046178275
# 78       v13           167131.06       0.058541617 0.0046147967
# 79      v130           165952.55       0.058128814 0.0045822557
# 80       v94           165040.12       0.057809217 0.0045570621
# 81       v32           164895.12       0.057758427 0.0045530583
# 82       v42           164321.94       0.057557654 0.0045372316
# 83       v86           163789.62       0.057371199 0.0045225334
# 84       v11           161176.25       0.056455803 0.0044503733
# 85       v65           160689.36       0.056285258 0.0044369294
# 86       v49           160246.91       0.056130278 0.0044247125
# 87      v104           159538.53       0.055882153 0.0044051529
# 88       v17           158923.97       0.055666888 0.0043881837
# 89       v96           156725.53       0.054896833 0.0043274808
# 90       v90           156474.48       0.054808898 0.0043205489
# 91       v29           156393.78       0.054780630 0.0043183206
# 92      v126           156040.31       0.054656819 0.0043085606
# 93       v67           155821.23       0.054580082 0.0043025115
# 94       v41           155174.77       0.054353641 0.0042846613
# 95       v39           151358.25       0.053016816 0.0041792803
# 96       v20           150793.41       0.052818966 0.0041636839
# 97       v76           145968.70       0.051128999 0.0040304649
# 98       v19           102543.75       0.035918380 0.0028314219
# 99       v38            85755.49       0.030037894 0.0023678672
# 100       v3            21344.84       0.007476536 0.0005893704
# 101      v74            20656.77       0.007235522 0.0005703714