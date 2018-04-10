# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of 
# a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num). 
# 
# The function reads the outcome-of-care-measures.csv ???le and returns a character vector with the name of 
# the hospital that has the ranking speci???ed by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5) would return a character vector containing the name of the hospital 
# with the 5th lowest 30-day death rate for heart failure. 
# 
# The num argument can take values "best", "worst", or an integer indicating the ranking 
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that state, 
# then the function should return NA. Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings.
# 
# Handling ties: It may occur that multiple hospitals have the same 30-day mortality rate for a given cause of 
# death. In those cases ties should be broken by using the hospital name.
# 



library(dplyr)
library(plyr)
rankhospital<-function(state,outcome,num){
        df<-read.csv('outcome-of-care-measures.csv', colClasses="character")     
        df<-as.data.frame(cbind(                                                 
                Hospital.Name=df[,2],
                State=df[,7],
                Mortality.HeartAttack=df[,11],
                Mortality.HeartFailure=df[,17],
                Mortality.Pneunomia=df[,23]
        ))
        
        df$Hospital.Name<-as.character(df[,1])
        df$State<-as.character(df[,2])
        df$Mortality.HeartAttack<-as.character(df[,3])
        df$Mortality.HeartFailure<-as.character(df[,4])
        df$Mortality.Pneunomia<-as.character(df[,5])
        
        if (!outcome %in% c("heart attack", "heart failure","pneumonia")){        
                stop("invalid outcome")
        }else if (!state %in% df[,"State"]){
                stop("invalid state")
        }
        df<-filter(df, df$State==(state))                        # filter data by state                         
        
        if (outcome=="heart attack"){                                             
                df<-filter(df, !df[,3]=="Not Available")         # exclude observations that contain NA values for a given outcome
                df[,3]<-as.numeric(df[,3])                        
                df<-arrange(df,df[,3],Hospital.Name)             # sort data first by outcome in ascending order, 
                                                                 # then by Hospital.Name in alphabetical order
                
                df<-                                             # add another variable called Rank that represents the ranks of the hospitals
                        mutate(df,Rank=as.numeric(c(1:length(df$Hospital.Name))))        
        }else if (outcome=="heart failure"){
                df<-filter(df, !df[,4]=="Not Available")
                df[,4]<-as.numeric(df[,4])
                df<-arrange(df,df[,4],Hospital.Name)
                df<-
                        mutate(df,Rank=as.numeric(c(1:length(df$Hospital.Name)))) 
        }else if (outcome=="pneumonia"){
                df<-filter(df, !df[,5]=="Not Available")
                df[,5]<-as.numeric(df[,5])
                df<-arrange(df,df[,5],Hospital.Name)
                df<-
                        mutate(df,Rank=as.numeric(c(1:length(df$Hospital.Name)))) 
        }
        if (num=="best"){
                hospital_name<-as.character(df$Hospital.Name[min(df$Rank)])
        }else if (num=="worst"){
                hospital_name<-as.character(df$Hospital.Name[max(df$Rank)])
        }else if (!num %in% df[,"Rank"]){
               hospital_name<-as.numeric("NA")
        }
        else{
                hospital_name<-as.character(df$Hospital.Name[df$Rank==num])
        }
        
        return(hospital_name)
}



# Test examples (not showing warning message for introcued NAs by coercion)

rankhospital("TX","heart failure", 4)
[1] "DETAR HOSPITAL NAVARRO"

rankhospital("MD","heart attack", "worst")
[1] "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN","heart attack", 5000)
[1] NA
