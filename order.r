require(data.table)
require(lubridate)

ordertbwhole <- data.table()

for (i in 1:21) {

# 0. index
indextable <- data.table(1:21,c("order_data_2016-01-01","order_data_2016-01-02","order_data_2016-01-03","order_data_2016-01-04","order_data_2016-01-05","order_data_2016-01-06","order_data_2016-01-07","order_data_2016-01-08","order_data_2016-01-09","order_data_2016-01-10","order_data_2016-01-11","order_data_2016-01-12","order_data_2016-01-13","order_data_2016-01-14","order_data_2016-01-15","order_data_2016-01-16","order_data_2016-01-17","order_data_2016-01-18","order_data_2016-01-19","order_data_2016-01-20","order_data_2016-01-21"))

# 1. input
order1 <- data.table(read.delim(indextable[V1==i]$V2,sep="\t",fill=TRUE,head=FALSE))

# 2. clean
# 2.1 region hash --> region id
clustermap <- data.table(read.delim("cluster_map",sep="\t",fill=TRUE,head=FALSE))
setnames(clustermap,c("V4","O"))
setkey(order1,V4)
setkey(clustermap,V4)
order1 <- clustermap[order1]
order1[,V4:=NULL]
setnames(clustermap,c("V5","D"))
setkey(clustermap,V5)
setkey(order1,V5)
order1 <- clustermap[order1]
order1[,V5:=NULL]

# 2.2 destination id out of clustermap --> 67
order1[is.na(D)==TRUE]$D <- 67

# 2.3 time --> timeslot
order1[,h:=hour(fast_strptime(as.character(V7),"%Y-%m-%d %H:%M:%S"))]
order1[,m:=minute(fast_strptime(as.character(V7),"%Y-%m-%d %H:%M:%S"))]
order1[,s:=second(fast_strptime(as.character(V7),"%Y-%m-%d %H:%M:%S"))]
order1[,ts:=ceiling((h*60+m+s/60)/10)]
order1[h==0 & m==0 & s==0]$ts <- 1

## 2.4 eliminate the duplicated orders
#order1 <- data.table(unique(order1))

## 3. check if there are still partially duplicated orders (with same order ids but different order times or order origin place)
#order1[,rep:=.N,by=V1]

# 4. construct the table format
ordertb <- data.table(ts=rep(1:144,66))
setkey(ordertb,ts)
ordertb[,O:=rep(1:66,144)]

# 5.1 agg orders (demand), gaps (supply - demand), unique demand (# of driver IDs) and unique supply (# of passenger IDs, including the ones whose orders were not answered) (including the duplicated orders)
orderagg <- order1[,length(V1),keyby="ts,O"]
gapagg <- order1[V2=="NULL"][,length(V2),keyby="ts,O"]
setnames(gapagg,"V1","gap")
setnames(orderagg,"V1","demand")
# note: the columns of V1, V2 and V3 of order1 are not in the type of character. This is why aggregation calculation takes long time.
order1$V2 <- as.character(order1$V2)
driveragg <- order1[V2!="NULL"][,length(unique(V2)),by="ts,O"]
setnames(driveragg,"V1","driver")
order1$V3 <- as.character(order1$V3)
passagg <- order1[,length(unique(V3)),by="ts,O"]
setnames(passagg,"V1","customer")

# 5.2 transfer them into the format
setkey(ordertb,ts,O)
setkey(gapagg,ts,O)
ordertb <- gapagg[ordertb]

setkey(ordertb,ts,O)
setkey(orderagg,ts,O)
ordertb <- orderagg[ordertb]

ordertb[,supply:=demand-gap]

setkey(driveragg,ts,O)
setkey(ordertb,ts,O)
ordertb <- driveragg[ordertb]

setkey(passagg,ts,O)
setkey(ordertb,ts,O)
ordertb <- passagg[ordertb]

ordertb[is.na(ordertb)] <- 0

ordertbwhole <- rbind(ordertbwhole,ordertb)

}
