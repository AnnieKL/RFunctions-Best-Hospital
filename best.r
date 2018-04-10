

# This function reads the outcome-of-care-measures.csv file and returns a character vector with the
# name of the hospital that has the best (i.e. lowest) 30-day mortality rate for the specified outcome
# in that state.
#
# The hospital name is the name provided in the Hospital.Name variable.
#
# The outcome can be one of "heart attack", "heart failure", or "pneumonia".
#
# Hospitals that do not have data on a particular outcome are excluded from the set of hospitals when
# deciding the rankings.
#
# If there is a tie for the best hospital for a given outcome, then the hospital names should be sorted in
# alphabetical order and the first hospital in that set should be chosen.


library(dplyr)
library(plyr)
best<-function(state,outcome){
        df<-read.csv('outcome-of-care-measures.csv',colClasses="character")       # read data
        df<-as.data.frame(cbind(                                                  # transform data into a data frame containing only desired variables
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

        if (!outcome %in% c("heart attack", "heart failure","pneumonia")){        # check for validity of function arguments
                stop("invalid outcome")
        }else if (!state %in% df[,"State"]){
                stop("invalid state")
        }
        df<-filter(df, df$State==(state))                                         # filter data by function argument that specifies the state
        df<-arrange(df, Hospital.Name)                                            # arrange data by name of hospital in alphabetical order
        if (outcome=="heart attack"){                                             # find the number of row for which heart attack mortality rate is the lowest
                row_num<-which.min(as.numeric(df[,3], na.rm=TRUE))
        }else if (outcome=="heart failure"){                                      # find the number of row for which heart failure mortality rate is the lowest
                row_num<-which.min(as.numeric(df[,4], na.rm=TRUE))
        }else if (outcome=="pneumonia"){                                          # find the number of row for which pneumonia mortality rate is the lowest
                row_num<-which.min(as.numeric(df[,5], na.rm=TRUE))
        }
        hospital_name<-as.character(df$Hospital.Name[row_num])                    # create a variable named hospital_name which stores the name of the hospital
                                                                                  # that matches the lowest mortality rate for the specified outcome
        return(hospital_name)
}


# Test function examples (not showing warnings for NAs introduced via coercion)

best("MD","pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER"

best("TX","heart attack")
[1] "CYPRESS FAIRBANKS MEDICAL CENTER"

best("MD","heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE"

best("MD","heart attak")
Error in best("MD", "heart attak") : invalid outcome

best("MB","heart attack")
Error in best("MB", "heart attack") : invalid state
