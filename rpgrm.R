pollutantmean <- function(directory,pollutant,id=1:332){
  files <- c(paste("00",1:9,".csv",sep=""),
             paste("0",10:99,".csv",sep=""), 
             paste(100:332,".csv",sep="")
  )
  if(length(id)>1){
    #Read first file to create variables in a data frame
    data <- read.csv(paste(directory,files[id[1]],sep="/"))
    
    #Read remaining files and rbind them to dataset
    for (f in id[2:length(id)]) {
      data <- rbind(data,read.csv(paste(directory, files[f], sep="/")))
    }
  }
  else{
    data <- read.csv(paste(directory,files[id],sep="/"))
  }
  
  r <- data[pollutant]
  mean(r[!is.na(r)])
}


complete <- function(directory,id=1:332){
  files <- c(paste("00",1:9,".csv",sep=""),
             paste("0",10:99,".csv",sep=""), 
             paste(100:332,".csv",sep=""))
  if(length(id)>1){
    i <- 1
    data1 <- read.csv(paste(directory,files[id[i]],sep="/"))
    good1 <- nrow(data1[complete.cases(data1),])
    dataframe <- data.frame(id[i],good1)
    for(i in 2:length(id))
    {
      data1 <- read.csv(paste(directory,files[id[i]],sep="/"))
     
       good1 <- nrow(data1[complete.cases(data1),])
      
       dataframe <- rbind(dataframe,data.frame(id[i],good1))
      
    }
  }
  else
  {
    data1 <- read.csv(paste(directory,files[id],sep="/"))
    good1 <- nrow(data1[complete.cases(data1),])
    dataframe <- data.frame(id,good1)
  }
  
  dataframe
}


corr <- function(directory, threshold = 0) {
  files_list <- dir(directory, full.names = TRUE)
  output <- numeric()
  for (i in files_list){
    corval <- 0        
    data <- read.csv(i) #Bind CSV files in data 
    value <- sum(complete.cases(data))
    data <- data[complete.cases(data),]
    if (value > threshold){ 
      sulfate <- data[["sulfate"]]
      nitrate <- data[["nitrate"]]
      corval <- cor(nitrate,sulfate,use="pairwise.complete.obs")            
      output <- c(output,corval)
    }
  }
  output
  
}

}