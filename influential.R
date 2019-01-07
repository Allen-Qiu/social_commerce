# using LR make analysis
# removew multicollinearity

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)
library(pscl)
library(MKmisc)
library(ResourceSelection)
library(survey)
library(fmsb)

#--- functions ---
# balanced accuracy
bacc<-function(modle, test){
    # browser()
    res<-predict(modle,  newdata=test, type='response')
    res<-sapply(res, function(x){if(x>0.5) 1 else 0})
    idx<-which(test$label==1)
    pacc<-length(which(res[idx]==test$label[idx]))*1.0/length(res[idx])
    nacc<-length(which(res[-idx]==test$label[-idx]))*1.0/length(res[-idx])
    return ((pacc+nacc)/2)
}
vif_func<-function(in_frame,thresh=10,trace=T,...){
    if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
    
    #get initial vif value for all comparisons of variables
    vif_init<-NULL
    var_names <- names(in_frame)
    for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
    }
    vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
    
    if(vif_max < thresh){
        if(trace==T){ #print output of each iteration
            prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
            cat('\n')
            cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
        }
        return(var_names)
    }
    else{
        
        in_dat<-in_frame
        
        #backwards selection of explanatory variables, stops when all VIF values are below 'threshold'
        while(vif_max >= thresh){
            
            vif_vals<-NULL
            var_names <- names(in_dat)
            
            for(val in var_names){
                regressors <- var_names[-which(var_names == val)]
                form <- paste(regressors, collapse = '+')
                form_in <- formula(paste(val, '~', form))
                vif_add<-VIF(lm(form_in, data = in_dat, ...))
                vif_vals<-rbind(vif_vals,c(val,vif_add))
            }
            max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
            
            vif_max<-as.numeric(vif_vals[max_row,2])
            
            if(vif_max<thresh) break
            
            if(trace==T){ #print output of each iteration
                prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
                cat('\n')
                cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
                flush.console()
            }
            in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
            
        }
        
        return(names(in_dat))
        
    }
    
}

# --- load data ---
res<-read.table(file = "users.txt",sep = ",", header = T)

# --- normalized ---
data<-data.frame(fans=log(res$fans+1)/max(log(res$fans+1)), 
                 avgstars=res$avgstars, 
                 reviewcount=log(res$reviewcount+1)/max(log(res$reviewcount+1)), 
                 friends=log(res$friends+1)/max(log(res$friends+1)),
                 pagerank=res$pagerank/max(res$pagerank),
                 hub=res$hub/max(res$hub),
                 iselite=res$iselite
)

data$v<- log(res$v+1)/max(log(res$v+1))
data$c<- log(res$c+1)/max(log(res$c+1))
data$label<-res$lable

# --- before removewing multicollinearity ---
fullmodel<-glm(label~c+v+reviewcount+fans+avgstars+friends+iselite+pagerank,family = binomial("logit"), data=data)
print(paste("balanced ACC:",bacc(fullmodel,data)))

#calculate pseudo R2 for LR model
pR2(fullmodel)

#VIF
vif(fullmodel)

d=c('label')
m <- data[, !(names(data) %in% d)]
keep.dat<-vif_func(in_frame=m,thresh=5,trace=T)

# --- after removing multicollinearity ---
fullmodel<-glm(label~c+reviewcount+fans+avgstars+friends+iselite,family = binomial("logit"), data=data)
summary(fullmodel)
print(paste("balanced ACC:",bacc(fullmodel,data)))
pR2(fullmodel)
vif(fullmodel)




