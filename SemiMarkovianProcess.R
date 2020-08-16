data=round(runif(100,1,3))
data
e=matrix(1:100,nrow=1,ncol=100)
u=matrix(1:100,nrow=1,ncol=100)
m=matrix(1:100,nrow=1,ncol=100)
for( i in 1:100){
  u[i]=0
  m[i]=0
}
M=array(dim=c(3,3,17))
for(a in 1:3){
  for( j in 1:3){
    for(k in 1:17){
      M[a,j,k]=0
    }
  }
}
max=0
e=0
for( i in 1:99){
  if ((data[i])==(data[i+1])){
    e=e+1
    
  }else{
    
    m[i]=data[i+1]
    u[i]=data[i]
    for(a in 1:99){
      for( j in 1:99){
        for(k in 0:16){
          if(((data[a])==(u[i]))&((data[j])==(m[i]))&((k)==(e))){
            if(e>0){
              M[u[i],m[i],e+1]=M[u[i],m[i],e+1]+1
              if(e>max){
                max=e+1
              }
              e=0 }
          }
        }
      }
    }
    
  }
  
}

max
M
tm=matrix(c(rep(0,15)),3,3)
tm
tpm=matrix(c(rep(0,15)),3,3)
tpm
for(i in 1:length(data)-1){
  tm[data[i],data[i+1]]=(tm[data[i],data[i+1]]+1)
  tm
}
for(i in 1:3){
  for(j in 1:3){
    if(i==j){
      tm[i,j]=0
    }
  }
}

print(tm)

for(j in 1:3){
  tpm[j,]=(tm[j,]/sum(tm[j,]))
  
}
tpm
H=array(dim=c(3,3,17))
l= matrix(1:9,nrow=3,ncol=3)
for(a in 1:3){
  for( j in 1:3){
    for(k in 1:17){
      H[a,j,k]=0
      l[a,j]=0
    }
  }
}
for(k in 2:17){
  for(a in 1:3){
    for( j in 1:3){
      if(tm[a,j]>0){
        H[a,j,k]=M[a,j,k]/tm[a,j]
      }
    }
  }
}

for(a in 1:3){
  for( j in 1:3){
    for(k in 2:17){
      l[a,j]=l[a,j]- H[a,j,k]
      
    }
  }
}
for(a in 1:3){
  for( j in 1:3){
    
    H[a,j,1]=(1+l[a,j])
  }
}

for(k in 1:17){
  for(a in 1:3){
    for( j in 1:3){
      if(a==j){
        H[a,j,k]=0
      }
    }
  }
}
H
C=array(dim=c(3,3,17))
for(a in 1:17){
  for(j in 1:3){
    for(m in 1:3){
      C[j,m,a]=(tpm[j,m])*(H[j,m,a])
      
    }
  }
}
C
k=0
w=array(dim=c(3,3,17))
for(a in 1:17){
  for(j in 1:3){
    for(m in 1:3){
      w[j,m,a]=0
    }
  }
}
for(a in 1:17){
  for(j in 1:3){
    for(m in 1:3){
      w[j,j,a]=C[j,m,a]+k
      k=w[j,j,a]
    }
    k=0
  }
}
w
b=matrix(9,3,3)
for(a in 1:3){
  for(i in 1:3){
    if(a==i){
      b[a,i]=1
    }else{
      b[a,i]=0
    }
  }
}
b
Y=array(dim=c(3,3,17))
A=matrix(1:9,nrow=3,ncol=3)
for(i in 1:3){
  for(j in 1:3){
    for(a in 1:17){
      
      Y[i,j,a]=0
      
    }
  }
}
for(i in 1:3){
  for(j in 1:3){
    if(i==j){
      Y[i,j,1]=1-w[i,j,1]
    }
  }
}
for(i in 1:3){
  for(j in 1:3){
    for(a in 2:17){
      if(i==j){
        Y[i,j,a]=Y[i,j,a-1]-w[i,j,a]
      }
    }
  }
}
Y     
d=array(dim=c(3,3,17))
k=array(dim=c(3,3,17))
for(i in 1:3){
  for(j in 1:3){
    for(a in 1:17){
      k[i,j,a]=0
    }
  }
}
d[,,1]=Y[,,1]+C[,,1]
for(n in 2:17){
  for(i in 1:(n-1)){
    k[,,n]=k[,,n]+(C[,,i])%*%(d[,,(n-i)])
    
  }
  d[,,n]=Y[,,n]+k[,,n]+C[,,n]
}
d
plot(d)
