##-----------------------------------------------------------------------------
##

# DATA
## For this programming assignment you will need to unzip this file and create 
## the directory 'specdata'. Once you have unzipped the zip file, do not make 
## any modifications to the files in the 'specdata' directory.
unzip(zipfile = "PA1.zip")

##-----------------------------------------------------------------------------
##

# Part 1 
## Write a function named 'pollutantmean' that calculates the mean of a pollutant
## (sulfate or nitrate) across a specified list of monitors. The function 
## 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
## particulate matter data from the directory specified in the 'directory' 
## argument and returns the mean of the pollutant across all of the monitors, 
## ignoring any missing values coded as NA
pollutantmean <- function(directory,polluntant,id= 1:332){
     ## 'directory' is a character vector of lenght 1 indicating the location
     ## of the CSV files
     if (basename(getwd()) != "specdata"){ setwd(directory)}
     
     
     ## 'polluntant' is a character vector of lenght 1 indicating the name of 
     ## the pollutant for which we will calculate the mean; either "sulfate" 
     ## or "nitrate".
     matrix_anl <- data.frame() 
     files_anal <- list.files(full.names = TRUE)
     for(i in 1:length(id)){
          matrix_anl <- rbind(matrix_anl,read.csv(files_anal[id[i]]))
     }
     ## 'id' is an integer vector indicating the monitor ID numbers to be used
     ## Return the mean of the polluntant across all monitors list in the 'id'
     ## vector (ignoring NA values)
     ## NOTE: Do not round the result!
     mean(matrix_anl[,polluntant], na.rm = TRUE)
     
}

##-----------------------------------------------------------------------------
##

# Part 2 
## Write a function that reads a directory full of files and reports the number 
## of completely observed cases in each data file. The function should return a 
## data frame where the first column is the name of the file and the second column 
## is the number of complete cases. A prototype of this function follows
complete <- function(directory,id= 1:332){
     ## 'directory' is a character vector of lenght 1 indicating the location
     ## of the CSV files
     if (basename(getwd()) != "specdata"){ setwd(directory)}
     
     # 'id' is an integer vector indicating the monitor ID numbers to be used
     idc <- vector(mode = "integer")
     nobs <- vector(mode = "integer")
     files_anal <- list.files(full.names = TRUE)
     for(i in 1:length(id)){
          idc <- rbind(idc,id[i])
          nobs <- rbind(nobs,sum(complete.cases(read.csv(files_anal[id[i]]))))
     }
     
     ## Return a data frame of the form:
     ## id nobs
     ## 1  117
     ## 2  1041
     ## ...
     ## where 'id' is the monitor ID number and 'nobs' is the number of complete
     ## cases
     data.frame(idc,nobs)
     
}

##-----------------------------------------------------------------------------
##

# Part 3 
## Write a function that takes a directory of data files and a threshold for 
## complete cases and calculates the correlation between sulfate and nitrate 
## for monitor locations where the number of completely observed cases 
## (on all variables) is greater than the threshold. The function should return 
## a vector of correlations for the monitors that meet the threshold requirement. 
## If no monitors meet the threshold requirement, then the function should return 
## a numeric vector of length 0. A prototype of this function follows

corr <- function(directory,threshold = 0){
     ## 'directory' is a character vector of lenght 1 indicating the location
     ## of the CSV files
     if (basename(getwd()) != "specdata"){ setwd(directory)}
     
     files_anal <- list.files(full.names = TRUE)
     vcorr <- vector(mode = "numeric")
     dfcomplete <- complete(directory)
     threshold_True <- dfcomplete[,2]>threshold
     com_cas <- dfcomplete [threshold_True,]
     vcorr <- append(vcorr,cor(com_cas[,2],com_cas[,3]))
     
     
     vcorr
     ## 'threshold' is a numeric vector of lenght 1 indicating the number of 
     ## completely observed observations (on all variables) required to compute 
     ## the correlation between nitrate and sulfate; the default is 0 
     
     ## Return a numeric vector of correlations 
     ## NOTE: Do not round the result!
}
