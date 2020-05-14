Production = function(StockMat,E_CZ,IOV_t,E_VA,OrderMat,IOX_0)
{
    ZX = StockMat/E_CZ   #(NN)*(RR*NN)
    
    temp = 1.25*IOX_0[rep(1,NN),]
    
    ZX[mat.key] = temp[mat.key]
    
    VX = IOV_t/E_VA   #(UU)*(RR*NN)
    
    OX = t(apply(OrderMat,1,sum))   #(1)*(RR*NN)
    
    return(apply(rbind(ZX,VX,OX),2,min))   #(1)*(RR*NN)
}

Production_max = function(StockMat,E_CZ,IOV_t,E_VA,IOX_0)
{
    ZX = StockMat/E_CZ   #(NN)*(RR*NN)
    
    temp = 1.25*IOX_0[rep(1,NN),]
    
    ZX[mat.key] = temp[mat.key]
    
    VX = IOV_t/E_VA   #(UU)*(RR*NN)
    
    return(apply(rbind(ZX,VX),2,min))   #(1)*(RR*NN)
}

OverProdSignFun = function(StockMat,E_Z,IOV_t,E_V,OrderMat,IOX_0)
{
    Result = matrix(0,nrow = UU,ncol = RR*NN)
    
    ZX = StockMat/E_Z   #(RR*NN)*(RR*NN)
    
    VX = IOV_t/E_V   #(UU)*(RR*NN)
    
    OX = t(apply(OrderMat,1,sum))   #(1)*(RR*NN)
    
    ZOX = rbind(ZX,OX)
    
    V1U = VX[rep(1,nrow(ZOX)),]<ZOX
    
    Result[1,which(apply(V1U,2,sum) == nrow(ZOX))] = 1
    
    V1D = VX[rep(1,nrow(ZOX)),]>ZOX
    
    Result[1,which(apply(V1D,2,sum) >0)] = -1
    
    temp = abs(OX-VX)/IOX_0
    
    return(Result*temp)   #(UU)*(RR*NN)
}

