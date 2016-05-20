require(data.table)
require(reshape2)
# 0. Preprocessing
# Transform all of ":" and "\t" to ";"

# 1. input
poinew <- data.table(read.csv("poi_data_new",sep=";",fill=TRUE,head=FALSE))

# 2. rename columns
setnames(poinew,paste("X",seq(1:303),sep=""))

# 3. reshape 1
# 3.1 for each region, set two columns, namely type of POI and its corresponding quantity
# 3.1.1 build the column of types of POI
TypePOI <- poinew[,c(colnames(poinew)[seq(2,302,2)]),with=FALSE]
ColTypePOI <- as.vector(t(as.matrix(TypePOI)))
#3.1.2 build the column of quantities
Quantity <- poinew[,c(colnames(poinew)[seq(3,303,2)]),with=FALSE]
ColQuantity <- as.vector(t(as.matrix(Quantity)))
#3.1.3 build the column of regions
Region <- data.table(rep(poinew$X1,151))
setkey(Region,Region)
#3.1.4 new poinew
poireshape1 <- data.table(region=Region$Region,Type=ColTypePOI,Q=ColQuantity)

# 3. 
# 3.1 remove "" and NA and replace them with 0
poireshape1[is.na(poireshape1)] <- 0
poireshape1[Type==""] <- 0
poireshape1 <- poireshape1[Type!=0 & Q!=0]
poireshape1$Q <- as.integer(poireshape1$Q)
# 3.2 reshape2: dcast
poireshape2 <- dcast(poireshape1,region ~ Type)
poireshape2[is.na(poireshape2)] <- 0
write.csv(poireshape2,file="poireshape2.csv",row.names=FALSE)