#Getting and Cleaning Data Course Project - Guido Rattay


#You should create one R script called run_analysis.R that does the following.

    #Merges the training and the test sets to create one data set.

    #Extracts only the measurements on the mean and standard deviation for each measurement.

    #Uses descriptive activity names to name the activities in the data set

    #Appropriately labels the data set with descriptive variable names.

    #From the data set in step 4, creates a second, independent tidy data set 
    #with the average of each variable for each activity and each subject.

mean_and_sd<-c("TotalBodyAcc-mean-X","TotalBodyAcc-mean-Y","TotalBodyAcc-mean-Z",
               "TotalBodyAcc-std-X","TotalBodyAcc-std-Y","TotalBodyAcc-std-Z")

#test
X_test<-readLines("./test/X_test.txt")#2947 elements, 561 elements for each one
x_split<-strsplit(X_test," ")#creates a list of 2947 lists, of 561 elements each.
x_split_df<-do.call(rbind.data.frame, x_split)#list of lists to dataframe
test_x_mean_sd<-x_split_df[,3:8]#keeps mean and sd only
rm(X_test,x_split,x_split_df)#removes heavy and unused variables
colnames(test_x_mean_sd)<-mean_and_sd
label<-readLines("./test/y_test.txt")#numeric labels

#Change to real labels for activities
for(i in 1:length(label)){
  
  if(label[i]=="1") {label[i]<-"walking"}
  if(label[i]=="2") {label[i]<-"walking_upstairs"}
  if(label[i]=="3") {label[i]<-"walking_downstairs"}
  if(label[i]=="4") {label[i]<-"sitting"}
  if(label[i]=="5") {label[i]<-"standing" }
  if(label[i]=="6") {label[i]<-"laying" }
}
subject<-readLines("./test/subject_test.txt")#30 subjects
test_df<-data.frame(subject,label,test_x_mean_sd)
rm(test_x_mean_sd)

#training

X_train<-readLines("./train/X_train.txt")
x_split<-strsplit(X_train," ")
x_split_df<-do.call(rbind.data.frame, x_split)#list of lists to dataframe
train_x_mean_sd<-x_split_df[,3:8]#keeps mean and sd
rm(X_train,x_split,x_split_df)#removes heavy and unused variables
colnames(train_x_mean_sd)<-mean_and_sd
label<-readLines("./train/y_train.txt")#numeric labels
for(i in 1:length(label)){
  
  if(label[i]=="1") {label[i]<-"walking"}
  if(label[i]=="2") {label[i]<-"walking_upstairs"}
  if(label[i]=="3") {label[i]<-"walking_downstairs"}
  if(label[i]=="4") {label[i]<-"sitting"}
  if(label[i]=="5") {label[i]<-"standing" }
  if(label[i]=="6") {label[i]<-"laying" }
}
subject<-readLines("./train/subject_train.txt")#30 subjects
train_df<-data.frame(subject,label,train_x_mean_sd)
rm(train_x_mean_sd)

#combine into single df
library(dplyr)
df<-dplyr::bind_rows(train_df,test_df)
colnames(df)<-c("Subject","Activity","TotalBodyAcc-mean-X","TotalBodyAcc-mean-Y","TotalBodyAcc-mean-Z",
               "TotalBodyAcc-std-X","TotalBodyAcc-std-Y","TotalBodyAcc-std-Z")
df[order(df$Subject,df$Activity),]
head(df)

#Second, independent dataset:

#I need to change to numeric values. Could make it work with a loop.

num_df3<-as.numeric(as.character(df[,3]))
num_df4<-as.numeric(as.character(df[,4]))
num_df5<-as.numeric(as.character(df[,5]))
num_df6<-as.numeric(as.character(df[,6]))
num_df7<-as.numeric(as.character(df[,7]))
num_df8<-as.numeric(as.character(df[,8]))

num_df<-cbind(df[,1:2],num_df3,num_df4,num_df5,num_df6,num_df7,num_df8)
colnames(num_df)<-c("Subject","Activity","TotalBodyAcc-mean-X","TotalBodyAcc-mean-Y","TotalBodyAcc-mean-Z",
                "TotalBodyAcc-std-X","TotalBodyAcc-std-Y","TotalBodyAcc-std-Z")

subject_df<-data.frame()#empty dataframe, just the names

for(i in 1:length(levels(num_df$Subject))){
  
  #Iterate trough subjects, calculating the mean of the 6 variables for each activity
  
  standing_means<-colMeans(num_df[(num_df$Subject==i & num_df$Activity == "standing"),3:8],na.rm=TRUE)
  sitting_means<-colMeans(num_df[(num_df$Subject==i & num_df$Activity == "sitting"),3:8],na.rm=TRUE)
  laying_means<-colMeans(num_df[(num_df$Subject==i & num_df$Activity == "laying"),3:8],na.rm=TRUE)
  walking_means<-colMeans(num_df[(num_df$Subject==i & num_df$Activity == "walking"),3:8],na.rm=TRUE)
  walking_downstairs<-colMeans(num_df[(num_df$Subject==i & num_df$Activity == "walking_downstairs"),3:8],na.rm=TRUE)
  walking_upstairs<-colMeans(num_df[(num_df$Subject==i & num_df$Activity == "walking_upstairs"),3:8],na.rm=TRUE)
  
  #store the results in s_df dataframe
  s_df<-rbind.data.frame(standing_means,sitting_means,laying_means,
                               walking_means,walking_downstairs,walking_upstairs)
  colnames(s_df)<-c(1:6)
  #add the calculated means for each activity of subject i to the subject_df dataframe.
  subject_df<-dplyr::bind_rows(subject_df,s_df)
  colnames(subject_df)<-c(1:6)
  
}

#dataframe with the subject and activity columns
out_df<-data.frame(Subject = rep(c(1:length(levels(num_df$Subject))),each=6),
                   Activity = c("standing","sitting","laying","walking",
                                "walking_downstairs","walking_upstairs"))

tidy_df<-dplyr::bind_cols(out_df,subject_df)
colnames(tidy_df)<-c("Subject","Activity","mean TotalBodyAcc-mean-X","mean TotalBodyAcc-mean-Y","mean TotalBodyAcc-mean-Z",
                    "mean TotalBodyAcc-std-X","mean TotalBodyAcc-std-Y","mean TotalBodyAcc-std-Z")
head(tidy_df)
