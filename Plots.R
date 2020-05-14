
####################################################################################################
rm(list = ls())
cat("\14")

####################################################################################################
RR = 141  #地区个数
NN = 65   #部门种类数（产品）
UU = 1    #要素种类数
TT = 52


RegionList = read.csv("Figure/RegionGTAP10.csv",header = T,stringsAsFactors = F)
SectorList = read.csv("Figure/SectorGTAP10.csv",header = T,stringsAsFactors = F)

RowN = data.frame(NID = 1:(RR*NN),
                  RID = sort(rep(1:RR,NN)),
                  SID = rep(1:NN,RR),
                  Region = matrix(t(matrix(rep(RegionList$Name,NN),nrow = RR)),ncol = 1),
                  Sector = matrix(rep(SectorList$Name,RR),ncol = 1),
                  stringsAsFactors = F)

Name = rep(NA,RR*NN)
for(i in 1:(RR*NN))
{
    Name[i] = paste(RowN$Region[i],RowN$Sector[i],sep = "--")
}
RowN = data.frame(RowN,Name,stringsAsFactors = F)
rm(Name)

ColN = data.frame(NID = (RR*NN+1):(RR*NN+RR),
                  RID = 1:RR,
                  SID = NN+1,
                  Region = RegionList$Name,
                  Sector = rep("FinalUse",RR),
                  stringsAsFactors = F)

Name = rep(NA,RR)
for(i in 1:RR)
{
    Name[i] = paste(ColN$Region[i],ColN$Sector[i],sep = "--")
}
ColN = data.frame(ColN,Name,stringsAsFactors = F)
rm(Name)
ColN = rbind(RowN,ColN)


#稳态（Benchmark）:加载稳态数据
load("Data/GTAP10_2014.RData")
MRIOT = MRIOT/52

IOZ_0 = matrix(MRIOT[1:(RR*NN),1:(RR*NN)],nrow = RR*NN)
IOF_0 = matrix(MRIOT[1:(RR*NN),(RR*NN+1):(RR*NN+RR)],nrow = RR*NN)
IOV_0 = matrix(MRIOT[(RR*NN+1):(RR*NN+UU),1:(RR*NN)],nrow = UU)
IOX_0 = apply(IOZ_0,2,sum) + apply(IOV_0,2,sum)

remove(MRIOT)

I_Sum = matrix(rep(diag(NN),RR),nrow = NN)

IOZ_C = I_Sum%*%IOZ_0
Z_Dis = IOZ_0/IOZ_C[rep(1:NN,RR),]

IOF_C = I_Sum%*%IOF_0
F_Dis = IOF_0/IOF_C[rep(1:NN,RR),]

E_CZ = IOZ_C%*%diag(1/IOX_0)
E_VA = IOV_0%*%diag(1/IOX_0)


#单个情景地图
Scenario = "CN80-2"
###################################################################################
IOX_TT = as.matrix(fread(paste("Result/IOX_",Scenario,".csv",sep = "")))
IOX_TT = IOX_TT[,-1]

IOV_TT = IOX_TT*E_VA[rep(1,TT),]

X_Sum = diag(RR)
X_Sum = X_Sum[sort(rep(1:RR,NN)),]

IOV_TT_Sum = IOV_TT%*%X_Sum
IOV_0_Sum = IOV_0%*%X_Sum

IOV_TT_Sum_total = apply(IOV_TT_Sum,2,sum)

IOV_Loss = 1-IOV_TT_Sum_total/IOV_0_Sum[1,]/52

VA_Loss = cbind(RegionList,IOV_Loss,Type = rep(0,RR))

for(i in 1:RR)
{
    if(VA_Loss$IOV_Loss[i]<0.001)
    {
        VA_Loss$Type[i] = 1
        next()
    }
    if(VA_Loss$IOV_Loss[i]<0.01)
    {
        VA_Loss$Type[i] = 2
        next()
    }
    if(VA_Loss$IOV_Loss[i]<0.1)
    {
        VA_Loss$Type[i] = 3
        next()
    }
    VA_Loss$Type[i] = 4
}


WorldShp = readOGR("Figure/WorldShp/TM_WORLD_BORDERS-0.3.shp")

Namelist = WorldShp$NAME
Num.region  = length(Namelist)

BridgeMat = read.csv("Figure/WorldShp/BridgeMat.csv")
names(BridgeMat) = c("id","Name","Code")


#Core Result
Rst = VA_Loss

Mydata = merge(BridgeMat, Rst, by="Code", all.x = TRUE)


WorldShp.f = fortify(WorldShp, region = "ISO3")

Merge.shp.coef = merge(WorldShp.f, Mydata, by = "id", all.x = TRUE)

Final.plot = Merge.shp.coef[order(Merge.shp.coef$order),]

#cnames = aggregate(cbind(long,lat)~id,data = Final.plot, FUN = function(x) mean(range(x)))


fontsize = 11

pWorldMap = ggplot() +
    geom_hline(aes(yintercept = 50),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_hline(aes(yintercept = 0),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_hline(aes(yintercept = -50),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = 0),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = -100),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = 100),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    
    geom_polygon(data = Final.plot,aes(x = long, y = lat, group = group, fill = IOV_Loss),
                 color = "Gray", size = 0.002) +
    
    scale_fill_gradient2(name = "VA Loss", limits = c(0,0.35), low = "steelblue2", high = "tomato",
                         mid = "white", midpoint = 0,breaks = pretty_breaks(n = 3)) + 

    theme(panel.background = element_blank(), #element_rect(fill = 'transparent',color = 'black'),
          axis.text = element_blank(), #element_text(face='bold',colour='black',size=fontsize,hjust=.5),
          axis.title = element_blank(), #element_text(face='bold',colour='black',size=fontsize,hjust=.5),
          legend.position=c('bottom'),
          legend.direction = c('horizontal'))+

    scale_x_continuous(limits = c(-180,180), breaks=seq(-100, 100, 100), 
                       labels = c("100W","0","100E")) +
    scale_y_continuous(limits = c(-60,90), breaks=seq(-50, 50, 50),
                       labels = c("50S","0","50N")) +
    xlab("")+
    ylab("")

pWorldMap


pdf(paste("Figure/",Scenario,".pdf",sep = ""),width=10*0.8, height=6*0.8)
print(pWorldMap)
dev.off()



ScenarioSet = c("CN20-2","CN40-2","CN60-2","CN80-2",
                  "CN20-4","CN40-4","CN60-4","CN80-4",
                  "CN20-6","CN40-6","CN60-6","CN80-6")
for(Scenario in ScenarioSet)
{
    ###################################################################################
    IOX_TT = as.matrix(fread(paste("Result/IOX_",Scenario,".csv",sep = "")))
    IOX_TT = IOX_TT[,-1]
    
    IOV_TT = IOX_TT*E_VA[rep(1,TT),]
    
    X_Sum = diag(RR)
    X_Sum = X_Sum[sort(rep(1:RR,NN)),]
    
    IOV_TT_Sum = IOV_TT%*%X_Sum
    IOV_0_Sum = IOV_0%*%X_Sum
    
    IOV_TT_Sum_total = apply(IOV_TT_Sum,2,sum)
    
    IOV_Loss = 1-IOV_TT_Sum_total/IOV_0_Sum[1,]/52
    
    VA_Loss = cbind(RegionList,IOV_Loss,Type = rep(0,RR))
    
    for(i in 1:RR)
    {
        if(VA_Loss$IOV_Loss[i]<0.001)
        {
            VA_Loss$Type[i] = 1
            next()
        }
        if(VA_Loss$IOV_Loss[i]<0.01)
        {
            VA_Loss$Type[i] = 2
            next()
        }
        if(VA_Loss$IOV_Loss[i]<0.1)
        {
            VA_Loss$Type[i] = 3
            next()
        }
        VA_Loss$Type[i] = 4
    }
    
    
    WorldShp = readOGR("Figure/WorldShp/TM_WORLD_BORDERS-0.3.shp")
    
    Namelist = WorldShp$NAME
    Num.region  = length(Namelist)
    
    BridgeMat = read.csv("Figure/WorldShp/BridgeMat.csv")
    names(BridgeMat) = c("id","Name","Code")
    
    
    #Core Result
    Rst = VA_Loss
    
    Mydata = merge(BridgeMat, Rst, by="Code", all.x = TRUE)
    
    
    WorldShp.f = fortify(WorldShp, region = "ISO3")
    
    Merge.shp.coef = merge(WorldShp.f, Mydata, by = "id", all.x = TRUE)
    
    Final.plot = Merge.shp.coef[order(Merge.shp.coef$order),]
    
    #cnames = aggregate(cbind(long,lat)~id,data = Final.plot, FUN = function(x) mean(range(x)))
    
    
    fontsize = 11
    
    pWorldMap = ggplot() +
        #geom_hline(aes(yintercept = 50),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
        #geom_hline(aes(yintercept = 0),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
        #geom_hline(aes(yintercept = -50),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
        #geom_vline(aes(xintercept = 0),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
        #geom_vline(aes(xintercept = -100),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
        #geom_vline(aes(xintercept = 100),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
        
        geom_polygon(data = Final.plot,aes(x = long, y = lat, group = group, fill = IOV_Loss),
                     color = "Gray", size = 0.002) +
        
        scale_fill_gradient2(name = "VA Loss", limits = c(0,0.35), low = "steelblue2", high = "tomato",
                             mid = "white", midpoint = 0,breaks = pretty_breaks(n = 3)) + 
        
        theme(panel.background = element_blank(), #element_rect(fill = 'transparent',color = 'black'),
              axis.text = element_blank(), #element_text(face='bold',colour='black',size=fontsize,hjust=.5),
              axis.title = element_blank(), #element_text(face='bold',colour='black',size=fontsize,hjust=.5),
              legend.position=c('bottom'),
              legend.direction = c('horizontal'))+
        
        scale_x_continuous(limits = c(-180,180), breaks=seq(-100, 100, 100), 
                           labels = c("100W","0","100E")) +
        scale_y_continuous(limits = c(-60,90), breaks=seq(-50, 50, 50),
                           labels = c("50S","0","50N")) +
        xlab("")+
        ylab("")
    
    pWorldMap
    
    
    pdf(paste("Figure/",Scenario,".pdf",sep = ""),width=9*0.8, height=6*0.8)
    print(pWorldMap)
    dev.off()
}








#单个情景曲线
Scenario = "NT60-4"
#################################################################################

X_Sum = diag(RR)
X_Sum = X_Sum[sort(rep(1:RR,NN)),]

Name = paste("Result/IOX_",Scenario,".csv",sep = "")
IOX_TT = as.matrix(fread(Name))
IOX_TT = IOX_TT[,-1]

temp = IOX_TT%*%X_Sum
temp0 = IOX_0%*%X_Sum

World = (temp%*%matrix(1,nrow = RR,ncol = 1))/sum(temp0)

temp = temp/matrix(rep(temp0,TT),nrow = TT,byrow = T)

colnames(temp) = RegionList[,3]

Region_to_show = c(4,6)

temp = temp[,Region_to_show]
temp = cbind(World,temp)
colnames(temp)[1] = c("World")
Region_Name = colnames(temp)

a = matrix(t(matrix(rep(colnames(temp),TT),nrow = length(temp[1,]))),ncol = 1)
a = factor(a,levels = Region_Name)

temp = cbind(rep(1:TT,length(temp[1,])),matrix(temp,ncol = 1))
temp = as.data.frame(temp)
temp = cbind(temp,a)

temp = temp[53:104,2]

tempa = temp[21:52]

sum((1-tempa)*sum(IOV_0))

temp0[4]*32



p1 = ggplot(temp,aes(x = V1,y = V2)) + 
    geom_line() +
    facet_wrap(vars(a)) +
    scale_y_continuous(limits = c(0, 1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
    ylab("Output") + xlab("Week")

p1    


#多个情景曲线
Scenario = c("CN80-2","CN60-4","CN40-6")
#################################################################################
X_Sum = diag(RR)
X_Sum = X_Sum[sort(rep(1:RR,NN)),]

tempall = data.frame()
count = 1
for(i in Scenario)
{
    Name = paste("Result/IOX_",i,".csv",sep = "")
    IOX_TT = as.matrix(fread(Name))
    IOX_TT = IOX_TT[,-1]
    
    temp = IOX_TT%*%X_Sum
    temp0 = IOX_0%*%X_Sum
    
    World = (temp%*%matrix(1,nrow = RR,ncol = 1))/sum(temp0)
    
    temp = temp/matrix(rep(temp0,TT),nrow = TT,byrow = T)
    
    colnames(temp) = RegionList[,3]
    
    Region_to_show = c(4,6,7,17,22,27,28,33,34,42,49,63,64,68,79,81,87,109,139)
    #Region_to_show = 1:141
    
    temp = temp[,Region_to_show]
    temp = cbind(World,temp)
    colnames(temp)[1] = c("World")
    Region_Name = colnames(temp)
    
    a = matrix(t(matrix(rep(colnames(temp),TT),nrow = length(temp[1,]))),ncol = 1)
    a = factor(a,levels = Region_Name)
    
    temp = cbind(rep(1:TT,length(temp[1,])),matrix(temp,ncol = 1))
    temp = as.data.frame(temp)
    temp = cbind(temp,a)
    
    if(count == 1)
    {
        tempall = temp
        count = 2
        next()
    }
    tempall = cbind(tempall,temp)
}

tempall = tempall[c(1,seq(2,ncol(tempall),3),3)]

a = c("CN80","CN60","CN40")
colnames(tempall) = c("Week",a,"Region")


p1 = ggplot(tempall,aes(x = Week)) + 
    geom_line(aes(y = CN80),color = "blue",alpha = 0.1) +
    geom_line(aes(y = CN60),color = "green") +
    geom_line(aes(y = CN40),color = "red",alpha = 0.1) +
    geom_vline(aes(xintercept = 4),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = 11),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = 14),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    facet_wrap(vars(Region)) +
    scale_y_continuous(limits = c(0, 1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
    ylab("Output") + xlab("Week")

p1


PDFName = "Figure/Output_CN.pdf"
CairoPDF(PDFName, width=15, height=10)
p1
dev.off()


#情景集合曲线
#################################################################################
ScenarioSetCN = c("CN20-2","CN40-2","CN60-2","CN80-2",
                  "CN20-4","CN40-4","CN60-4","CN80-4",
                  "CN20-6","CN40-6","CN60-6","CN80-6")
ScenarioSetNT = c("NT20-2","NT40-2","NT60-2","NT80-2",
                  "NT20-4","NT40-4","NT60-4","NT80-4",
                  "NT20-6","NT40-6","NT60-6","NT80-6") 
ScenarioSetGB = c("GB20-2","GB40-2","GB60-2","GB80-2",
                  "GB20-4","GB40-4","GB60-4","GB80-4",
                  "GB20-6","GB40-6","GB60-6","GB80-6")
X_Sum = diag(RR)
X_Sum = X_Sum[sort(rep(1:RR,NN)),]

tempall = data.frame()
count = 1
for(i in ScenarioSetGB)
{
    Name = paste("Result/IOX_",i,".csv",sep = "")
    IOX_TT = as.matrix(fread(Name))
    IOX_TT = IOX_TT[,-1]
    
    temp = IOX_TT%*%X_Sum
    temp0 = IOX_0%*%X_Sum
    
    World = (temp%*%matrix(1,nrow = RR,ncol = 1))/sum(temp0)
    
    temp = temp/matrix(rep(temp0,TT),nrow = TT,byrow = T)
    
    colnames(temp) = RegionList[,3]
    
    #Region_to_show = c(4,6,7,17,22,27,28,33,34,42,49,63,64,68,79,81,87,109,139)
    #Region_to_show = 1:141
    Region_to_show = c(4,6,68,64,81,28,33,125,49)
    
    temp = temp[,Region_to_show]
    temp = cbind(World,temp)
    colnames(temp)[1] = c("World")
    Region_Name = colnames(temp)
    
    a = matrix(t(matrix(rep(colnames(temp),TT),nrow = length(temp[1,]))),ncol = 1)
    a = factor(a,levels = Region_Name)
    
    temp = cbind(rep(1:TT,length(temp[1,])),matrix(temp,ncol = 1))
    temp = as.data.frame(temp)
    temp = cbind(temp,a)
    
    if(count == 1)
    {
        tempall = temp
        count = 2
        next()
    }
    tempall = cbind(tempall,temp)
}


tempall = tempall[c(1,seq(2,ncol(tempall),3),3)]

ScenarioName = c("CN202","CN402","CN602","CN802",
                 "CN204","CN404","CN604","CN804",
                 "CN206","CN406","CN606","CN806")

colnames(tempall) = c("Week",ScenarioName,"Region")


p1 = ggplot(tempall,aes(x = Week)) + 
    geom_line(aes(y = CN202),color = "green",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN402),color = "green",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN602),color = "green",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN802),color = "green",alpha = 1) +
    geom_line(aes(y = CN204),color = "blue",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN404),color = "blue",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN604),color = "blue",alpha = 1) +
    geom_line(aes(y = CN804),color = "blue",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN206),color = "red",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN406),color = "red",alpha = 1) +
    geom_line(aes(y = CN606),color = "red",alpha = 0.6,linetype = "dotted") +
    geom_line(aes(y = CN806),color = "red",alpha = 0.6,linetype = "dotted") +
    
    geom_vline(aes(xintercept = 3),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = 11),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    geom_vline(aes(xintercept = 15),linetype = 'dashed',alpha = 0.5,lwd = 0.5,color = 'black')+
    facet_wrap(vars(Region),nrow = 5,ncol = 2) +
    scale_y_continuous(limits = c(0, 1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
    ylab("Output") + xlab("Week")

p1


PDFName = "Figure/Scenario_NT.pdf"
CairoPDF(PDFName, width=7, height=11)
p1
dev.off()


#单个情景单个部门曲线
Scenario = "GB40-6"
#################################################################################

Name = paste("Result/IOX_",Scenario,".csv",sep = "")
IOX_TT = as.matrix(fread(Name))
IOX_TT = IOX_TT[,-1]

RID = 64
SID = 5
TID = (RID-1)*NN + SID

temp = IOX_TT[,TID]
temp0 = IOX_0[TID]

World = (temp%*%matrix(1,nrow = RR,ncol = 1))/sum(temp0)

temp = temp/matrix(rep(temp0,TT),nrow = TT,byrow = T)

colnames(temp) = RegionList[,3]

#Region_to_show = c(4,6,7,17,22,27,28,33,34,42,49,63,64,68,79,81,87,109,139)
#Region_to_show = 1:141
Region_to_show = c(6,33)

temp = temp[,Region_to_show]
temp = cbind(World,temp)
colnames(temp)[1] = c("World")
Region_Name = colnames(temp)

a = matrix(t(matrix(rep(colnames(temp),TT),nrow = length(temp[1,]))),ncol = 1)
a = factor(a,levels = Region_Name)

temp = cbind(rep(1:TT,length(temp[1,])),matrix(temp,ncol = 1))
temp = as.data.frame(temp)
temp = cbind(temp,a)


p1 = ggplot(temp,aes(x = V1,y = V2)) + 
    geom_line() +
    facet_wrap(vars(a)) +
    scale_y_continuous(limits = c(0, 1.1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
    ylab("Output") + xlab("Week")

p1


#损失统计
#####################################################################
ScenarioSetCN = c("CN20-2","CN40-2","CN60-2","CN80-2",
                  "CN20-4","CN40-4","CN60-4","CN80-4",
                  "CN20-6","CN40-6","CN60-6","CN80-6")
ScenarioSetNT = c("NT20-2","NT40-2","NT60-2","NT80-2",
                  "NT20-4","NT40-4","NT60-4","NT80-4",
                  "NT20-6","NT40-6","NT60-6","NT80-6") 
ScenarioSetGB = c("GB20-2","GB40-2","GB60-2","GB80-2",
                  "GB20-4","GB40-4","GB60-4","GB80-4",
                  "GB20-6","GB40-6","GB60-6","GB80-6")
X_Sum = diag(RR)
X_Sum = X_Sum[sort(rep(1:RR,NN)),]

count = 1
for(i in ScenarioSetGB)
{
    Name = paste("Result/IOX_",i,".csv",sep = "")
    IOX_TT = as.matrix(fread(Name))
    IOX_TT = IOX_TT[,-1]
    
    IOV_TT = IOX_TT*E_VA[rep(1,TT),]
    
    IOV_TT_Sum = IOV_TT%*%X_Sum
    IOV_0_Sum = IOV_0%*%X_Sum
    
    IOV_TT_Sum_total = apply(IOV_TT_Sum,2,sum)
    
    IOV_Loss = 1-IOV_TT_Sum_total/IOV_0_Sum[1,]/52
    
    IOV_Loss = data.frame(Region = RegionList$Name,
                          Value = IOV_Loss,
                          No = 1:141)
    
    World = data.frame(Region = "World",
                       Value = 1 - sum(IOV_TT)/sum(IOV_0)/52,
                       No = 0)
    
    IOV_Loss = rbind(World,IOV_Loss)
    
    if(count == 1)
    {
        VA_Loss = IOV_Loss
        count = 2
        next()
    }
    VA_Loss = cbind(VA_Loss,IOV_Loss)
    
}

VA_Loss = VA_Loss[c(1,seq(2,ncol(VA_Loss),3),3)]

ScenarioName = c("CN202","CN402","CN602","CN802",
                 "CN204","CN404","CN604","CN804",
                 "CN206","CN406","CN606","CN806")

colnames(VA_Loss) = c("RID",ScenarioName,"Region")

write.csv(VA_Loss,"Figure/VA_Loss_GB.csv",row.names = F)




#部门损失统计
#####################################################################
ScenarioSetCN = c("CN20-2","CN40-2","CN60-2","CN80-2",
                  "CN20-4","CN40-4","CN60-4","CN80-4",
                  "CN20-6","CN40-6","CN60-6","CN80-6")
ScenarioSetNT = c("NT20-2","NT40-2","NT60-2","NT80-2",
                  "NT20-4","NT40-4","NT60-4","NT80-4",
                  "NT20-6","NT40-6","NT60-6","NT80-6") 
ScenarioSetGB = c("GB20-2","GB40-2","GB60-2","GB80-2",
                  "GB20-4","GB40-4","GB60-4","GB80-4",
                  "GB20-6","GB40-6","GB60-6","GB80-6")

ScenarioSet = c("CN20-2","CN40-2","CN60-2","CN80-2",
                "CN20-4","CN40-4","CN60-4","CN80-4",
                "CN20-6","CN40-6","CN60-6","CN80-6",
                "NT20-2","NT40-2","NT60-2","NT80-2",
                "NT20-4","NT40-4","NT60-4","NT80-4",
                "NT20-6","NT40-6","NT60-6","NT80-6",
                "GB20-2","GB40-2","GB60-2","GB80-2",
                "GB20-4","GB40-4","GB60-4","GB80-4",
                "GB20-6","GB40-6","GB60-6","GB80-6")

count = 1
for(i in ScenarioSet)
{
    print(i)
    Name = paste("Result/IOX_",i,".csv",sep = "")
    IOX_TT = as.matrix(fread(Name))
    IOX_TT = IOX_TT[,-1]
    
    IOV_TT = IOX_TT*E_VA[rep(1,TT),]
    
    IOV_TT_Sum = apply(IOV_TT,2,sum)
    IOV_0_Sum = IOV_0*52
    
    a = 1-(IOV_TT_Sum/IOV_0_Sum)
    
    if(count == 1)
    {
        a = cbind(RowN,a[1,])
        VA_Loss_Sector = a
        count = 2
        next()
    }
    VA_Loss_Sector = cbind(VA_Loss_Sector,a[1,])
    
}

colnames(VA_Loss_Sector) = c(colnames(RowN),ScenarioSet)

write.csv(VA_Loss_Sector,"Figure/VA_Loss_Sector.csv",row.names = F)





#部门损失统计
#####################################################################
ScenarioSet = c("CN80-2","NT60-4","GB40-6")

count = 1
for(i in ScenarioSet)
{
    Name = paste("Result/IOX_",i,".csv",sep = "")
    IOX_TT = as.matrix(fread(Name))
    IOX_TT = IOX_TT[,-1]
    
    IOV_TT = IOX_TT*E_VA[rep(1,TT),]
    
    IOV_TT_Sum = apply(IOV_TT,2,sum)
    IOV_0_Sum = IOV_0*52
    
    a = IOV_TT_Sum/IOV_0_Sum
    
    if(count == 1)
    {
        a = cbind(RowN,a[1,])
        meanp = a
        count = 2
        next()
    }
    meanp = cbind(meanp,a[1,])
    
}

colnames(VA_Loss_Sector) = c("RID",ScenarioName,"Region")

write.csv(meanp,"Figure/meanp.csv",row.names = F)




#上下游
Scenario = "GB40-6_"
RID = 64
SID = 43
TID = (RID-1)*NN + SID

rstU = RowN
rstD = ColN

for(t in 1:TT)
{
    print(t)
    
    Name = paste(paste("F:/Temp/",Scenario,t,sep = ""),".csv",sep = "")
    MRIOTtemp = as.matrix(fread(file = Name))
    u = MRIOTtemp[,TID]
    d = matrix(MRIOTtemp[TID,],ncol = 1)
    
    rstU = cbind(rstU,u,stringsAsFactors = F)
    rstD = cbind(rstD,d,stringsAsFactors = F)
}



rst = rstU

temp = rst[,7:58]

temp = as.matrix(temp)

temp = sweep(temp,1,temp[,1],FUN = "/")

temp = cbind(RowN,Value = rowSums(temp)/TT,Size = rstU[,7]*52)

SL = 27:45
temp = subset(temp,temp$SID %in% SL)

tempa = subset(temp,temp$Size>100)

RL = unique(tempa$RID)
RL = sort(RL)

temp = subset(temp,temp$RID %in% RL)


temp = cbind(X = 1:nrow(temp),temp)


Y = lowess(temp$X,temp$Value,f=1/20,iter = 10)
plot(Y,ylim = c(0.1,1))
plot(temp$X,temp$Value)


RID2 = rep(0,nrow(temp))
for(i in 1:nrow(temp))
{
    RID2[i] = which(RL == temp$RID[i])
}

temp = cbind(temp,Y = Y$y,RID2)

write.csv(temp,"Figure/Fig2_Japan_Motor_UP_CN80-2.csv",row.names = F)



rst = rstD


temp = rst[,7:58]

temp = as.matrix(temp)

temp = sweep(temp,1,temp[,1],FUN = "/")

temp = cbind(ColN,Value = rowSums(temp)/TT,Size = rstD[,7]*52)

SL = c(27:45,50,62,64,66)
SL = 46:66
temp = subset(temp,temp$SID %in% SL)

tempa = subset(temp,temp$Size>500)

RL = unique(tempa$RID)
RL = sort(RL)

temp = subset(temp,temp$RID %in% RL)


temp = cbind(X = 1:nrow(temp),temp[order(temp$RID),])


Y = lowess(temp$X,temp$Value,f=1/20,iter = 10)
plot(Y,ylim = c(0.5,1))
plot(temp$X,temp$Value)


RID2 = rep(0,nrow(temp))
for(i in 1:nrow(temp))
{
    RID2[i] = which(RL == temp$RID[i])
}

temp = cbind(temp,Y = Y$y,RID2)

write.csv(temp,"Figure/Fig2_Japan_Motor_Down_CN80-2.csv",row.names = F)





#热图

rst = rstU

temp = rst[,7:58]

temp = as.matrix(temp)

temp = sweep(temp,1,temp[,1],FUN = "/")


temp = cbind(RowN,Size = rstU[,7]*52,temp)

SL = 27:45
temp = subset(temp,temp$SID %in% SL)

temp = subset(temp,temp$Size>150)


pheatmap(temp[,8:(TT+7)],cluster_rows = F,cluster_cols = F,labels_row = temp$Name)


write.csv(temp,"Figure/heatmapData_DEU_Motor.csv")


bk <- c(seq(0,0.5,by=0.01),seq(0.51,1,by=0.01))

p1 = pheatmap(temp[,8:(TT+7)],
              scale = "none",
              color = c(colorRampPalette(colors = c("red","yellow"))(length(bk)/2),colorRampPalette(colors = c("yellow","blue"))(length(bk)/2)),
              legend_breaks=seq(0,1,0.1),
              breaks=bk,cluster_rows = F,cluster_cols = F)

PDFName = "Figure/heatmapNT.pdf"
CairoPDF(PDFName, width=20, height=10)
p1
dev.off()


#复杂热图
load("Figure/LTF.RData")
Scenario = "GB40-6"
#################################################################################

Name = paste("Result/IOX_",Scenario,".csv",sep = "")
IOX_TT = as.matrix(fread(Name))
IOX_TT = IOX_TT[,-1]

RID = 64
SID = 43
TID = (RID-1)*NN + SID

Sect1 = c(1:14)  #0.5
Sect2 = c(15:45,49:55,61,65)  #1
Sect3 = c(46:48,56:60,62,63)  #0.1
RegionCN = c(4)
RegionNT = c(28,63,64,68,73,79,81,82,99)
RegionGB = setdiff(1:141,c(RegionCN,RegionNT))


temp = IOX_TT[,TID]
temp0 = IOX_0[TID]

output = temp/temp0

labor = Labor_Cons[SID,RID,]


tran_U = Transport_Cons_TT[,RID,]

tran_D = Transport_Cons_TT[TID,,]


mypaste = function(a)
{
    return(paste(a[1],a[2],sep = "."))
}

#上下游
Scenario = "GB40-6"

rstU = RowN
rstD = ColN

for(t in 1:TT)
{
    print(t)
    
    Name1 = paste("F:/Temp/",Scenario,"_max_",t,".csv",sep = "")
    Name2 = paste("F:/Temp/",Scenario,"_order_",t,".csv",sep = "")
    
    MRIOTtemp = as.matrix(fread(file = Name1))
    u = MRIOTtemp[,TID]
    
    MRIOTtemp = as.matrix(fread(file = Name2))
    d = matrix(MRIOTtemp[TID,],ncol = 1)
    
    rstU = cbind(rstU,u,stringsAsFactors = F)
    rstD = cbind(rstD,d,stringsAsFactors = F)
}



rst = rstU

temp = rst[,7:58]

temp = as.matrix(temp)

temp = sweep(temp,1,temp[,1],FUN = "/")


temp = cbind(RowN,Size = rstU[,7]*52,temp)

SL = 27:45
temp = subset(temp,temp$SID %in% SL)

temp_U = subset(temp,temp$Size>4000)

temp_U_name = apply(data.frame(b = "Capacity",a = temp_U$Name),1,mypaste)


tran_U = tran_U[temp_U$NID,]

tran_U_name = apply(data.frame(b = "Transport",a = temp_U$Name),1,mypaste)


rst = rstD

temp = rst[,7:58]

temp = as.matrix(temp)

temp = sweep(temp,1,temp[,1],FUN = "/")


temp = cbind(ColN,Size = rstD[,7]*52,temp)

SL = c(27:45,50,62,64,66)
temp = subset(temp,temp$SID %in% SL)

temp_D = subset(temp,temp$Size>4000)
temp_D_name = apply(data.frame(b = "Demand",a = temp_D$Name),1,mypaste)

tran_D = tran_D[temp_D$RID,]

tran_D_name = apply(data.frame(b = "Transport",a = temp_D$Name),1,mypaste)



a = temp_U[,8:59]
colnames(a) = NULL
a = as.matrix(a)
b = tran_U
colnames(b) = NULL
b = as.matrix(b)
c = temp_D[,8:59]
colnames(c) = NULL
c = as.matrix(c)
d = tran_D
colnames(d) = NULL
d = as.matrix(d)

heatmapdata = read.csv(file = "Figure/heatmap_G.csv")
heatmapdata = heatmapdata[,-1]

heatmapdata = rbind(output,labor,a,b,c,d)
heatmapdata[heatmapdata>1] = 1

cols = colorRampPalette(c("#D73027","#FEEDA3", "#4575B4"))(50)

pheatmap(heatmapdata,cluster_rows = F,cluster_cols = F,color = cols,
         labels_row = c("Output","Labor",temp_U_name,tran_U_name,temp_D_name,tran_D_name),fontsize = 9)

pheatmap(heatmapdata[,-1],cluster_rows = F,cluster_cols = F,color = cols,
         labels_row = heatmapdata[,1],fontsize = 7)


write.csv(data.frame(Name = c("Output","Labor",temp_U_name,temp_D_name),heatmapdata),file = "Figure/heatmap_J.csv",row.names = F)



