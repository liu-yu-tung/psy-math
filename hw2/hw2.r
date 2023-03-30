# same as modeling book
nll.condition=function(par,y) { 
    p=1:4
    p[1]=1-pnorm(par[2],par[1],1)
    p[2]=1-p[1]
    p[3]=1-pnorm(par[2],0,1)
    p[4]=1-p[3]
    return(-sum(y*log(p)))
}

# same as modeling book
nll.1=function(par10,y20) {
    model=1:10
    result=0.0
    par=1:2
    params=NULL
    
    print("params 1")
    for (i in 0:4) {
        start2=i*2+1
        end2=start2+1
        start4=i*4+1
        end4=start4+3
        model=optim(par=par10[start2:end2], nll.condition, y=y20[start4:end4])
        result=result+model$value
        params=c(params,model$par)
    }
    print(params)
    return(result)
}

# same as modeling book
nll.2=function(par10,y20) {
    result=0.0
    for (i in 0:4) {
        start1=1
        end1=start1+1+i
        start4=i*4+1
        end4=start4+3
        result=result+nll.condition(par10[c(start1,end1)], y20[start4:end4])
    }
    return(result)
}

data=c(404,96,301,199,348,152,235,265,287,213,183,317,251,249,102,398,148,352,20,480)
par10=1:10
for (i in 1:10) {
    par10[i]=0.5 # assume all d and g are 0.5
}
mod1=nll.1(par10, y20=data)
#mod1=optim(par,nll.1,y20=data)
par_m1 = mod1
par=1:6
for (i in 1:6) {
    par[i]=0.5 # assume all d and g are 0.5
}
mod2=optim(par,nll.2,y20=data)
par_m2 = mod2$par
print("params 2")
print(par_m2) # print the value of model 2
chi_square=qchisq(p=0.05, df=4, lower.tail=FALSE)
G_square=2*(mod2$value-mod1) # use simplyfied G ratio test
print(paste("chi_square:", chi_square))
print(paste("G_square:", G_square))
