
####################################################################################################
rm(list = ls())
cat("\14")

library(readxl)
library(Matrix)
library(data.table)

####################################################################################################
RR = 4
NN = 3
RRNN = RR*NN
UU = 1 
TT = 40

col.names.1 = c("R1S1","R1S2","R1S3","R2S1","R2S2","R2S3",
                "R3S1","R3S2","R3S3","R4S1","R4S2","R4S3","R1F","R2F","R3F","R4F")
col.names.2 = col.names.1[1:RRNN]

MRIOTPath = "Data/MRIOTtest.xlsx"
MRIOT = as.matrix(read_excel(path = MRIOTPath, sheet = 1, range = "C3:R16", col_names = col.names.1))

zz.s = matrix(MRIOT[1:(RR*NN),1:(RR*NN)],nrow = RR*NN)
ff.s = matrix(MRIOT[1:(RR*NN),(RR*NN+1):(RR*NN+RR)],nrow = RR*NN)
va.s = matrix(MRIOT[(RR*NN+1):(RR*NN+UU),1:(RR*NN)],nrow = UU)
xx.s = matrix(apply(zz.s,2,sum) + apply(va.s,2,sum),nrow = 1)

OOO = matrix(nrow = TT,ncol = 3)

mat.index = as.matrix(read_excel(path = MRIOTPath, sheet = 2, range = "C3:N14", col_names = col.names.2))
no.input = apply(mat.index,2,max)

mat.key = as.matrix(read_excel(path = MRIOTPath, sheet = 5, range = "C3:N14", col_names = col.names.2))

merge.mat = function(mat,mat.index)
{
    RRNN = nrow(mat)
    merge.mat.a = function(x,temp)
    {
        for(i in 1:RRNN)
        {
            temp[mat.index[i,x]] = temp[mat.index[i,x]] + mat[i,x]
        }
        return(temp)
    }
    
    cl=makeCluster(detectCores() - 3)
    registerDoParallel(cl)
    
    temp = rep(0,RRNN)
    
    result = foreach(x = 1:RRNN,.combine = cbind) %dopar%  merge.mat.a(x,temp)
    
    stopImplicitCluster()
    stopCluster(cl)
    return(result)
}

dis.mat = function(mat,mat.index,mat.ezz)
{
    RRNN = nrow(mat)
    dis.mat.a = function(x,temp)
    {
        for(i in 1:RRNN)
        {
            temp[i] = mat[mat.index[i,x],x]*mat.ezz[i,x]
        }
        return(temp)
    }
    
    cl=makeCluster(detectCores() - 3)
    registerDoParallel(cl)
    
    temp = rep(0,RRNN)
    
    result = foreach(x = 1:RRNN,.combine = cbind) %dopar%  dis.mat.a(x,temp)
    
    stopImplicitCluster()
    stopCluster(cl)
    return(result)
}

merge.mat = function(mat,mat.index)
{
    R = nrow(mat); N = ncol(mat)
    temp = sparseMatrix(dims = c(R,N),i=c(1),j=c(1),x=c(0))
    for(i in 1:R)
    {
        for(j in 1:N)
        {
            temp[mat.index[i,j],j] = temp[mat.index[i,j],j] + mat[i,j]
        }
    }
    return(temp)
}

dis.mat = function(mat,mat.index,mat.ezz)
{
    R = nrow(mat); N = ncol(mat)
    temp = matrix(nrow = R,ncol = N)
    for(i in 1:R)
    {
        for(j in 1:N)
        {
            temp[i,j] = mat[mat.index[i,j],j]*mat.ezz[i,j]
        }
    }
    return(temp)
}

zz.s.c = merge.mat(zz.s,mat.index)

mat.stock = 4*zz.s.c

mat.stock.obj = mat.stock + zz.s.c

mat.order = cbind(zz.s,ff.s)

mat.ezc = sweep(zz.s.c,2,xx.s,FUN = "/")

mat.ezz = matrix(nrow = RRNN,ncol = RRNN)
for(i in 1:RRNN)
{
    for(j in 1:RRNN)
    {
        mat.ezz[i,j] = zz.s[i,j]/zz.s.c[mat.index[i,j],j]
    }
}


VA_TT = array(1,dim = c(NN,RR,TT))
temp = c(100,100,seq(100,40,length.out = 4),rep(40,15),seq(40,100,length.out = 4))/100
VA_TT[2,2,1:length(temp)] = VA_TT[2,2,1:length(temp)]*temp
FF_TT = array(rep(ff.s,TT),dim = c(RRNN,RR,TT))

StartTime = Sys.time()
xx.TT = matrix(0,nrow = TT,ncol = RRNN)
for(t in 1:TT)
{
    print(t)
    
    va.t = matrix(VA_TT[,,t],nrow = 1)
    
    mat.order.sum = rowSums(mat.order)
    
   
    xx.t = matrix(0,nrow = 1,ncol = RRNN)
    xx.t.opt = matrix(0,nrow = 1,ncol = RRNN)
    for(i in 1:RRNN)
    {
        temp = mat.stock[1:no.input,i]/mat.ezc[1:no.input,i]; 
        temp = subset(temp,as.logical(mat.key[1:no.input,i]))
        xx.t[i] = min(temp,va.t[i]*xx.s[i],mat.order.sum[i])
        #xx.t.opt[i] = min(va.t[i]*xx.s[i],mat.order.sum[i])
        #xx.t.opt[i] = va.t[i]*xx.s[i]
    }
    
    xx.t.dis = sweep(mat.order,1,mat.order.sum,FUN = "/")*t(xx.t[rep(1,RRNN+RR),])
    

    mat.stock.use = xx.t[rep(1,RRNN),]*mat.ezc  
    mat.stock.add = merge.mat(xx.t.dis[,1:RRNN],mat.index)   
    
    mat.stock.use.opt = xx.t.opt[rep(1,RRNN),]*mat.ezc
    
    mat.stock = mat.stock - mat.stock.use + mat.stock.add  
    
   
    #mat.stock.obj = 5*mat.stock.use.opt
    
    mat.stock.gap = mat.stock.obj - mat.stock 
    mat.stock.gap[which(mat.stock.gap<0)] = 0
    
    
    mat.order[,1:(RRNN)] = dis.mat(mat.stock.gap,mat.index,mat.ezz)
    
    mat.order[,(RRNN+1):(RRNN+RR)] = FF_TT[,,t]  

    xx.TT[t,] = xx.t
}

plot(rowSums(xx.TT))


