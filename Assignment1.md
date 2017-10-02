pollutantmean<-function(directory,pollutant,id=1:332){
filelist<-list.files(path=directory,pattern=".csv",full.names=TRUE)
values<-numeric()
for (i in id){
data<-read.csv(filelist[i])
values<-c(values,data[[pollutant]])
}
mean(values,na.rm=TRUE)
}
pollutantmean("specdata", "sulfate", 1:10)




Part2
corr <- function(directory, threshold=0){
files<-list.files(path=directory, pattern=".csv",full.names=TRUE)
values<-numeric()
for(i in 1:332){
data<-read.csv(files[i],header=TRUE)
s<-complete.cases(data)
data<-data[s,]
csum<-nrow(data)
if (csum>threshold){
correlation<-cor(data[["nitrate"]],data[["sulfate"]])
values<-c(values,correlation)
}
}
values
}

part3
corr <- function(directory, threshold=0){
files<-list.files(path=directory, pattern=".csv",full.names=TRUE)
values<-numeric()
for(i in 1:332){
data<-read.csv(files[i],header=TRUE)
s<-complete.cases(data)
data<-data[s,]
csum<-nrow(data)
if (csum>threshold){
correlation<-cor(data[["nitrate"]],data[["sulfate"]])
values<-c(values,correlation)
}
}
values
}
