
ScenarioSet = c("CN20-2","CN40-2","CN60-2","CN80-2",
                "CN20-4","CN40-4","CN60-4","CN80-4",
                "CN20-6","CN40-6","CN60-6","CN80-6",
                "NT20-2","NT40-2","NT60-2","NT80-2",
                "NT20-4","NT40-4","NT60-4","NT80-4",
                "NT20-6","NT40-6","NT60-6","NT80-6",
                "GB20-2","GB40-2","GB60-2","GB80-2",
                "GB20-4","GB40-4","GB60-4","GB80-4",
                "GB20-6","GB40-6","GB60-6","GB80-6")

for(Scenario in ScenarioSet)
{
    rm(list = setdiff(ls(),c("Scenario","ScenarioSet")))
    
    RR = 141  #Region
    NN = 65   #Sector
    RRNN = RR*NN
    NNRR = RR*NN
    UU = 1   #Factor
    TT = 52
    
    RegionCN = c(4)
    RegionNT = c(28,63,64,68,73,79,81,82,99)
    RegionGB = setdiff(1:141,c(RegionCN,RegionNT))
    
    Sect1 = c(1:14)  #0.5
    Sect2 = c(15:45,49:55,61,65)  #1
    Sect3 = c(46:48,56:60,62,63)  #0.1
    
    
    #Constraints
    Labor_Cons = array(1,dim = c(NN,RR,TT))
    
    Transport_Cons_TT = array(1,dim = c(NNRR,RR,TT))
    
    IOF_TT = array(rep(IOF_0,TT),dim = c(NNRR,RR,TT))
    
    ScenarioFlag1 = substring(Scenario,1,2)
    ScenarioFlag2 = as.numeric(substring(Scenario,3,4))
    ScenarioFlag3 = as.numeric(substring(Scenario,6,6))
    
    if(ScenarioFlag1 == "CN")
    {
        CN_Labor = c(100,100,
                     seq(100,100-ScenarioFlag2,length.out = 3),
                     rep(100-ScenarioFlag2,ScenarioFlag3*4),
                     seq(100-ScenarioFlag2,100,length.out = 8),
                     rep(100,TT-ScenarioFlag3*4-13))/100-1
        
        NT_Labor = rep(100,TT)/100-1
        
        GB_Labor = rep(100,TT)/100-1
        
        
        CN_Transport = c(100,100,
                         seq(100,100-ScenarioFlag2,length.out = 3),
                         rep(100-ScenarioFlag2,ScenarioFlag3*4),
                         seq(100-ScenarioFlag2,100,length.out = 8),
                         rep(100,TT-ScenarioFlag3*4-13))/100-1
        
        CN_Transport_G = c(100,100,
                           seq(100,50,length.out = 3),
                           rep(50,ScenarioFlag3*4+8),
                           seq(50,100,length.out = 4),
                           rep(100,TT-ScenarioFlag3*4-17))/100-1
        
        NT_Transport = rep(100,TT)/100-1
        
        NT_Transport_G = rep(100,TT)/100-1
        
        GB_Transport = rep(100,TT)/100-1
        
        GB_Transport_G = rep(100,TT)/100-1
        
        
        CN_IOF = c(100,100,
                   seq(100,1,length.out = 3),
                   rep(1,ScenarioFlag3*4),
                   seq(1,100,length.out = 8),
                   rep(100,TT-ScenarioFlag3*4-13))/100
        
        NT_IOF = rep(100,TT)/100
        
        GB_IOF = rep(100,TT)/100
    }
    
    if(ScenarioFlag1 == "NT")
    {
        
        CN_Labor = c(100,100,
                     seq(100,20,length.out = 3),
                     rep(20,2*4),
                     seq(20,100,length.out = 8),
                     rep(100,TT-2*4-13))/100-1
        
        NT_Labor = c(rep(100,9),
                     seq(100,100-ScenarioFlag2,length.out = 3),
                     rep(100-ScenarioFlag2,ScenarioFlag3*4),
                     seq(100-ScenarioFlag2,100,length.out = 8),
                     rep(100,TT-ScenarioFlag3*4-20))/100-1
        
        GB_Labor = rep(100,TT)/100-1
        
        
        CN_Transport = c(100,100,
                         seq(100,20,length.out = 3),
                         rep(20,2*4),
                         seq(20,100,length.out = 8),
                         rep(100,TT-2*4-13))/100-1
        
        CN_Transport_G = c(100,100,
                           seq(100,50,length.out = 3),
                           rep(50,2*4+8),
                           seq(50,100,length.out = 4),
                           rep(100,TT-2*4-17))/100-1
        
        NT_Transport = c(rep(100,9),
                         seq(100,100-ScenarioFlag2,length.out = 3),
                         rep(100-ScenarioFlag2,ScenarioFlag3*4),
                         seq(100-ScenarioFlag2,100,length.out = 8),
                         rep(100,TT-ScenarioFlag3*4-20))/100-1
        
        NT_Transport_G = c(rep(100,9),
                           seq(100,50,length.out = 3),
                           rep(50,ScenarioFlag3*4+8),
                           seq(50,100,length.out = 4),
                           rep(100,TT-ScenarioFlag3*4-24))/100-1
        
        GB_Transport = rep(100,TT)/100-1
        
        GB_Transport_G = rep(100,TT)/100-1
        
        
        CN_IOF = c(100,100,
                   seq(100,1,length.out = 3),
                   rep(1,2*4),
                   seq(1,100,length.out = 8),
                   rep(100,TT-2*4-13))/100
        
        NT_IOF = c(rep(100,9),
                   seq(100,1,length.out = 3),
                   rep(1,ScenarioFlag3*4),
                   seq(1,100,length.out = 8),
                   rep(100,TT-ScenarioFlag3*4-20))/100
        
        GB_IOF = rep(100,TT)/100-1
    }
    
    if(ScenarioFlag1 == "GB")
    {
        
        CN_Labor = c(100,100,
                     seq(100,20,length.out = 3),
                     rep(20,2*4),
                     seq(20,100,length.out = 8),
                     rep(100,TT-2*4-13))/100-1
        
        NT_Labor = c(rep(100,9),
                     seq(100,40,length.out = 3),
                     rep(40,4*4),
                     seq(40,100,length.out = 8),
                     rep(100,TT-4*4-20))/100-1
        
        GB_Labor = c(rep(100,13),
                     seq(100,100-ScenarioFlag2,length.out = 3),
                     rep(100-ScenarioFlag2,ScenarioFlag3*4),
                     seq(100-ScenarioFlag2,100,length.out = 8),
                     rep(100,TT-ScenarioFlag3*4-24))/100-1
        
        
        CN_Transport = c(100,100,
                         seq(100,20,length.out = 3),
                         rep(20,2*4),
                         seq(20,100,length.out = 8),
                         rep(100,TT-2*4-13))/100-1
        
        CN_Transport_G = c(100,100,
                           seq(100,50,length.out = 3),
                           rep(50,2*4+8),
                           seq(50,100,length.out = 4),
                           rep(100,TT-2*4-17))/100-1
        
        NT_Transport = c(rep(100,9),
                         seq(100,40,length.out = 3),
                         rep(40,4*4),
                         seq(40,100,length.out = 8),
                         rep(100,TT-4*4-20))/100-1
        
        NT_Transport_G = c(rep(100,9),
                           seq(100,50,length.out = 3),
                           rep(50,4*4+8),
                           seq(50,100,length.out = 4),
                           rep(100,TT-4*4-24))/100-1
        
        GB_Transport = c(rep(100,13),
                         seq(100,100-ScenarioFlag2,length.out = 3),
                         rep(100-ScenarioFlag2,ScenarioFlag3*4),
                         seq(100-ScenarioFlag2,100,length.out = 8),
                         rep(100,TT-ScenarioFlag3*4-24))/100-1
        
        GB_Transport_G = c(rep(100,13),
                           seq(100,50,length.out = 3),
                           rep(50,ScenarioFlag3*4+8),
                           seq(50,100,length.out = 4),
                           rep(100,TT-ScenarioFlag3*4-28))/100-1
        
        
        CN_IOF = c(100,100,
                   seq(100,1,length.out = 3),
                   rep(1,2*4),
                   seq(1,100,length.out = 8),
                   rep(100,TT-2*4-13))/100
        
        NT_IOF = c(rep(100,9),
                   seq(100,1,length.out = 3),
                   rep(1,4*4),
                   seq(1,100,length.out = 8),
                   rep(100,TT-4*4-20))/100
        
        GB_IOF = c(rep(100,13),
                   seq(100,1,length.out = 3),
                   rep(1,ScenarioFlag3*4),
                   seq(1,100,length.out = 8),
                   rep(100,TT-ScenarioFlag3*4-24))/100
    }
    
    
    if(ScenarioFlag1 == "CN")
    {
        for(i in Sect1)
        {
            Labor_Cons[i,4,] = 1 + 0.5*CN_Labor
        }
        for(i in Sect2)
        {
            Labor_Cons[i,4,] = 1 + CN_Labor
        }
        for(i in Sect3)
        {
            Labor_Cons[i,4,] = 1 + 0.1*CN_Labor
        }
        
        for(i in Sect1)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+0.5*CN_Transport
        }
        for(i in Sect2)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+CN_Transport
        }
        for(i in Sect3)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+0.1*CN_Transport
        }
        
        for(i in Sect1)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+0.5*CN_Transport_G
            }
        }
        for(i in Sect2)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+CN_Transport_G
            }
        }
        for(i in Sect3)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+0.5*CN_Transport_G
            }
        }
        
        for(i in Sect1)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+0.5*CN_Transport_G
            }
        }
        for(i in Sect2)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+CN_Transport_G
            }
        }
        for(i in Sect3)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+0.5*CN_Transport_G
            }
        }
        
        for(i in 1:RR)
        {
            IOF_TT[3*NN+51,i,] = IOF_TT[3*NN+51,i,]*CN_IOF
            IOF_TT[3*NN+61,i,] = IOF_TT[3*NN+61,i,]*CN_IOF
            if(i!=4)
            {
                IOF_TT[(i-1)*NN+51,4,] = IOF_TT[(i-1)*NN+51,4,]*CN_IOF
                IOF_TT[(i-1)*NN+61,4,] = IOF_TT[(i-1)*NN+61,4,]*CN_IOF
            }
        }
        
        IOF_TT[3*NN+64,4,] = IOF_TT[3*NN+64,4,]*(1+0.2*(1-CN_IOF))
    }
    
    if(ScenarioFlag1 == "NT")
    {
        
        for(i in Sect1)
        {
            Labor_Cons[i,4,] = 1 + 0.5*CN_Labor
        }
        for(i in Sect2)
        {
            Labor_Cons[i,4,] = 1 + CN_Labor
        }
        for(i in Sect3)
        {
            Labor_Cons[i,4,] = 1 + 0.1*CN_Labor
        }
        
        for(j in RegionNT)
        {
            for(i in Sect1)
            {
                Labor_Cons[i,j,] = 1 + 0.5*NT_Labor
            }
            for(i in Sect2)
            {
                Labor_Cons[i,j,] = 1 + NT_Labor
            }
            for(i in Sect3)
            {
                Labor_Cons[i,j,] = 1 + 0.1*NT_Labor
            }
        }
        
        for(i in Sect1)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+0.5*CN_Transport
        }
        for(i in Sect2)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+CN_Transport
        }
        for(i in Sect3)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+0.1*CN_Transport
        }
        
        for(i in Sect1)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+0.5*CN_Transport_G
            }
        }
        for(i in Sect2)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+CN_Transport_G
            }
        }
        for(i in Sect3)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+0.5*CN_Transport_G
            }
        }
        
        for(i in Sect1)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+0.5*CN_Transport_G
            }
        }
        for(i in Sect2)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+CN_Transport_G
            }
        }
        for(i in Sect3)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+0.5*CN_Transport_G
            }
        }
        
        
        for(k in RegionNT)
        {
            for(i in Sect1)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+0.5*NT_Transport
            }
            for(i in Sect2)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+NT_Transport
            }
            for(i in Sect3)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+0.1*NT_Transport
            }
            
            for(i in Sect1)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+NT_Transport_G)
                }
            }
            for(i in Sect2)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+0.5*NT_Transport_G)
                }
            }
            for(i in Sect3)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+0.5*NT_Transport_G)
                }
            }
            
            for(i in Sect1)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+0.5*NT_Transport_G)
                }
            }
            for(i in Sect2)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+NT_Transport_G)
                }
            }
            for(i in Sect3)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+0.5*NT_Transport_G)
                }
            }
        }
        
        for(i in 1:RR)
        {
            IOF_TT[3*NN+51,i,] = IOF_TT[3*NN+51,i,]*CN_IOF
            IOF_TT[3*NN+61,i,] = IOF_TT[3*NN+61,i,]*CN_IOF
            if(i!=4)
            {
                IOF_TT[(i-1)*NN+51,4,] = IOF_TT[(i-1)*NN+51,4,]*CN_IOF
                IOF_TT[(i-1)*NN+61,4,] = IOF_TT[(i-1)*NN+61,4,]*CN_IOF
            }
        }
        
        IOF_TT[3*NN+64,4,] = IOF_TT[3*NN+64,4,]*(1+0.2*(1-CN_IOF))
        
        
        for(j in RegionNT)
        {
            for(i in 1:RR)
            {
                IOF_TT[(j-1)*NN+51,i,] = IOF_TT[(j-1)*NN+51,i,]*NT_IOF
                IOF_TT[(j-1)*NN+61,i,] = IOF_TT[(j-1)*NN+61,i,]*NT_IOF
                if(i!=j)
                {
                    IOF_TT[(i-1)*NN+51,j,] = IOF_TT[(i-1)*NN+51,j,]*NT_IOF
                    IOF_TT[(i-1)*NN+61,j,] = IOF_TT[(i-1)*NN+61,j,]*NT_IOF
                }
            }
            
            IOF_TT[(j-1)*NN+64,j,] = IOF_TT[(j-1)*NN+64,j,]*(1+0.2*(1-NT_IOF))
        }
        
    }
    
    if(ScenarioFlag1 == "GB")
    {
        
        for(i in Sect1)
        {
            Labor_Cons[i,4,] = 1 + 0.5*CN_Labor
        }
        for(i in Sect2)
        {
            Labor_Cons[i,4,] = 1 + CN_Labor
        }
        for(i in Sect3)
        {
            Labor_Cons[i,4,] = 1 + 0.1*CN_Labor
        }
        
        
        for(j in RegionNT)
        {
            for(i in Sect1)
            {
                Labor_Cons[i,j,] = 1 + 0.5*NT_Labor
            }
            for(i in Sect2)
            {
                Labor_Cons[i,j,] = 1 + NT_Labor
            }
            for(i in Sect3)
            {
                Labor_Cons[i,j,] = 1 + 0.1*NT_Labor
            }
        }
        
        
        for(j in RegionGB)
        {
            for(i in Sect1)
            {
                Labor_Cons[i,j,] = 1 + 0.5*GB_Labor
            }
            for(i in Sect2)
            {
                Labor_Cons[i,j,] = 1 + GB_Labor
            }
            for(i in Sect3)
            {
                Labor_Cons[i,j,] = 1 + 0.1*GB_Labor
            }
        }
        
        
        for(i in Sect1)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+0.5*CN_Transport
        }
        for(i in Sect2)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+CN_Transport
        }
        for(i in Sect3)
        {
            Transport_Cons_TT[3*NN+i,4,] = 1+0.1*CN_Transport
        }
        
        for(i in Sect1)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+0.5*CN_Transport_G
            }
        }
        for(i in Sect2)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+CN_Transport_G
            }
        }
        for(i in Sect3)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[3*NN+i,j,] = 1+0.5*CN_Transport_G
            }
        }
        
        for(i in Sect1)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+0.5*CN_Transport_G
            }
        }
        for(i in Sect2)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+CN_Transport_G
            }
        }
        for(i in Sect3)
        {
            for(j in 1:RR)
            {
                if(j!=4)
                    Transport_Cons_TT[(j-1)*NN+i,4,] = 1+0.5*CN_Transport_G
            }
        }
        
        
        for(k in RegionNT)
        {
            for(i in Sect1)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+0.5*NT_Transport
            }
            for(i in Sect2)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+NT_Transport
            }
            for(i in Sect3)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+0.1*NT_Transport
            }
            
            for(i in Sect1)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+NT_Transport_G)
                }
            }
            for(i in Sect2)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+0.5*NT_Transport_G)
                }
            }
            for(i in Sect3)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+0.5*NT_Transport_G)
                }
            }
            
            for(i in Sect1)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+0.5*NT_Transport_G)
                }
            }
            for(i in Sect2)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+NT_Transport_G)
                }
            }
            for(i in Sect3)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+0.5*NT_Transport_G)
                }
            }
        }
        
        
        for(k in RegionGB)
        {
            for(i in Sect1)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+0.5*NT_Transport
            }
            for(i in Sect2)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+NT_Transport
            }
            for(i in Sect3)
            {
                Transport_Cons_TT[(k-1)*NN+i,k,] = 1+0.1*NT_Transport
            }
            
            for(i in Sect1)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+NT_Transport_G)
                }
            }
            for(i in Sect2)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+0.5*NT_Transport_G)
                }
            }
            for(i in Sect3)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(k-1)*NN+i,j,] = pmin(Transport_Cons_TT[(k-1)*NN+i,j,],1+0.5*NT_Transport_G)
                }
            }
            
            for(i in Sect1)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+0.5*NT_Transport_G)
                }
            }
            for(i in Sect2)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+NT_Transport_G)
                }
            }
            for(i in Sect3)
            {
                for(j in 1:RR)
                {
                    if(j!=k)
                        Transport_Cons_TT[(j-1)*NN+i,k,] = pmin(Transport_Cons_TT[(j-1)*NN+i,k,],1+0.5*NT_Transport_G)
                }
            }
        }
        
        
        for(i in 1:RR)
        {
            IOF_TT[3*NN+51,i,] = IOF_TT[3*NN+51,i,]*CN_IOF
            IOF_TT[3*NN+61,i,] = IOF_TT[3*NN+61,i,]*CN_IOF
            if(i!=4)
            {
                IOF_TT[(i-1)*NN+51,4,] = IOF_TT[(i-1)*NN+51,4,]*CN_IOF
                IOF_TT[(i-1)*NN+61,4,] = IOF_TT[(i-1)*NN+61,4,]*CN_IOF
            }
        }
        
        IOF_TT[3*NN+64,4,] = IOF_TT[3*NN+64,4,]*(1+0.2*(1-CN_IOF))
        
        
        for(j in RegionNT)
        {
            for(i in 1:RR)
            {
                IOF_TT[(j-1)*NN+51,i,] = IOF_TT[(j-1)*NN+51,i,]*NT_IOF
                IOF_TT[(j-1)*NN+61,i,] = IOF_TT[(j-1)*NN+61,i,]*NT_IOF
                if(i!=j)
                {
                    IOF_TT[(i-1)*NN+51,j,] = IOF_TT[(i-1)*NN+51,j,]*NT_IOF
                    IOF_TT[(i-1)*NN+61,j,] = IOF_TT[(i-1)*NN+61,j,]*NT_IOF
                }
            }
            
            IOF_TT[(j-1)*NN+64,j,] = IOF_TT[(j-1)*NN+64,j,]*(1+0.2*(1-NT_IOF))
        }
        
        
        for(j in RegionGB)
        {
            for(i in 1:RR)
            {
                IOF_TT[(j-1)*NN+51,i,] = IOF_TT[(j-1)*NN+51,i,]*GB_IOF
                IOF_TT[(j-1)*NN+61,i,] = IOF_TT[(j-1)*NN+61,i,]*GB_IOF
                if(i!=j)
                {
                    IOF_TT[(i-1)*NN+51,j,] = IOF_TT[(i-1)*NN+51,j,]*GB_IOF
                    IOF_TT[(i-1)*NN+61,j,] = IOF_TT[(i-1)*NN+61,j,]*GB_IOF
                }
            }
            
            IOF_TT[(j-1)*NN+64,j,] = IOF_TT[(j-1)*NN+64,j,]*(1+0.2*(1-GB_IOF))
        }
        
    }
}