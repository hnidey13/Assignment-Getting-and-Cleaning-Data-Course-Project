#=============================================================================
# Done -> 1. Merges the training and the test sets to create one data set.
# Done -> 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Done -> 3. Uses descriptive activity names to name the activities in the data set
# Done -> 4. Appropriately labels the data set with descriptive variable names.
# Done -> 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#=============================================================================

    path <- getwd()
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    f <- "Dataset.zip"
    if (!file.exists(path)) {
        dir.create(path)
    }
    download.file(url, file.path(path, f))
    executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
    parameters <- "x"
    cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, f), "\""))
    system(cmd)
    pathIn <- file.path(path, "UCI HAR Dataset")

    setwd(pathIn)
    list.files(pathIn, recursive = TRUE)

    library(data.table)
        #library(plyr) currently not used
    library(dplyr)


#===================================Test and Train data sets===================

# I.# Load main data sets and merge them (using rbind function)



    data_test <- data.table(read.table("./test/X_test.txt", fill = FALSE)) 
    data_train <- data.table(read.table("./train/X_train.txt", fill = FALSE)) 
    # Convert the resulting data frame to a data table (data table is faster than d.f.)
    # ncol = 561
    # nrow = 2947 test and 7352 train (all rows 10299)

    data_all <- rbind(data_test, data_train)
    rm(data_test, data_train)
    
# II.# Load subjects and merge them (using rbind function), subjects order
# correspond with measurments from main data sets (see above data_all)
    
subject_test <- data.table(read.table("./test/subject_test.txt", fill = FALSE))
subject_train <- data.table(read.table("./train/subject_train.txt", fill = FALSE))
    # ncol = 1
    # nrow = 2947 test and 7352 train subjects who performed measurment (all rows 10299)

    subject_all <- rbind(subject_test, subject_train)
    rm(subject_test, subject_train)
    setnames(subject_all, "V1", "SubjectName")
    
# III.# Load activity numbers and merge them (using rbind function), activity numbers order
# correspond with measurments from main data sets (see above data_all),
# next activity labels have to assign to relevant act. numbers

activity_numbers_test <- data.table(read.table("./test/y_test.txt", fill = FALSE))
activity_numbers_train <- data.table(read.table("./train/y_train.txt", fill = FALSE))
    # ncol = 1
    # nrow = 2947 test and 7352 train activity numbers (6 unique values from 1 to 6; all rows 10299)
    
    activity_numbers_all <- rbind(activity_numbers_test, activity_numbers_train)
    rm(activity_numbers_train, activity_numbers_test)
    
#===============================Common data sets===============================

# IV.# Feature labels is loaded. Number of rows correspond with number of columns
# in main data set (data_all). It has to be assigned.

features_labels <- data.table(read.table("./features.txt", fill = FALSE))
    # ncol = 2 (numbers and feature labels)
    # nrow = 561 feature labels
    
    # Selecting only meand and standard deviation, then a creat a code (V1, V2...)
    # and accordingly I select from the main data set collumns which they align with
    # the code name
    features_labels <- features_labels[grepl("mean\\(\\)|std\\(\\)", V2),]
    setnames(features_labels, old = c("V1", "V2"), new = c("featureNum", "featureName"))
    features_labels$Code <- features_labels[ , paste0("V",featureNum)]
    
    data_all <- data_all[, features_labels$Code, with = FALSE]
    # It is a data.table structure, attribute 'with = FALSE' allow to refer to
    # column names
    
# V. # Activity labels is loaded - please follow above section no. III

activity_labels <- data.table(read.table("./activity_labels.txt", fill = FALSE))
    # ncol = 2 (numbers and activity labels)
    # nrow = 6 activity labels

    # Merge both data sets: activity numbers and activity descriptions, use full_join from dplyr
    # assign correct activity description to relevant number (in first data set are 10299
    # activity numbers from 1 to 6 -> in second data set is only 6 rows with relevant
    # activity description in the second column). AtTribute 'by=' define what column will be used
    # to accordingly assign values from one column to the another one
activities_all <- full_join(activity_numbers_all, activity_labels, by="V1")
    # ncol = 2 ("ActNum" and "ActName")
    # nrow = 10299 activites (only labels for relevant measures)

    setnames(activities_all, old = c("V1", "V2"), new = c("ActNum", "ActName"))

    #remove data sets not longer used
    rm(activity_labels, activity_numbers_all)
   
    
# VI. # Merge using 'cbind' (add columns next to each other) measurment values and subject     
    
    data_all <- cbind(data_all, subject_all)
    data_all <- cbind(data_all, activities_all)
    # Setkey is not nessesary but it enable to reordering data and it is both fast 
    # (due to data.table's internal radix sorting) and memory efficient 
    # (only one extra column of type double is allocated). 
    # Keying a data.table physically reorders it based on those column(s) in RAM.

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    # THIS PART IS VERY IMPORTANT:
    #   1. FIRST WE HAVE TO SET KEY (3 COLUMNS) 
    #   2. WE USE KEYs IN MELT FUNCTION AND ALSO COLUMN "Code" TO SHAPE NEW DATA SET
    #   3. USE View(data_all) before and after the operation
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    setkey(data_all, SubjectName, ActNum, ActName)
    data_all <- data.table(melt(data_all, key(data_all), variable.name = "Code"))
    data_all <- full_join(data_all, features_labels, by="Code")
    
    # Remember below operations are from data.table package
    data_all <- data.table(data_all)
    data_all <- data_all[, list(SubjectName, ActName, value, featureName)]
    
    data_all$Signal <- ifelse(grepl("^t",data_all$featureName), 't', ifelse(grepl("^f",data_all$featureName), 'f', 'NA'))
    
# VII. # Function to decompose feture data into each parts of information
    deco_feat <- function(regx1, regx2, var1, var2)
    {
        ifelse(grepl(regx1,data_all$featureName), var1, ifelse(grepl(regx2,data_all$featureName), var2, 'NA'))
    }
    
    data_all$SignalDomain <- deco_feat("^t", "^f", 'Time', 'Freq')
    data_all$SignalSource <- deco_feat("Acc", "Gyro", 'Accelerometer', 'Gyroscope')
    data_all$Acceleration <- deco_feat("Body", "Gravity", 'Body', 'Gravity')
    data_all$Jerk <- deco_feat("Jerk", "NA", 'Jerk', 'NA')
    data_all$Magnitude <- deco_feat("Mag", "NA", 'Magnitude', 'NA')
    data_all$Variable <- deco_feat("mean()", "std()", 'Mean', 'STD')
    data_all$Axis <- ifelse(grepl("-X",data_all$featureName), 'X',
                            ifelse(grepl("-Y",data_all$featureName), 'Y',  
                                   ifelse(grepl("-Z",data_all$featureName), 'Z', 'NA')))
    
    colnames(data_all)[3] <- "Value"
    # Remove colum (no needed any longer)
    
    # using .N in data.table enable to group and count rows by specific variable
    # (using 'by='). In this case verification step has to be pefromed
    r1 <- nrow(data_all[,.N, by=featureName])
    r2 <- nrow(data_all[,.N, by=c("Signal", "SignalDomain", "SignalSource", 
                                  "Acceleration", "Jerk", "Magnitude", 
                                  "Variable", "Axis")])
    if(r1==r2)
        {
            data_all$featureName <- NULL 
        } else
        {
            "Variable with feature names not correct decomposed ."
        }
        
# VIII. Moving column with values at the end
    data_all <- data_all %>% select(-Value, everything())
    
    # Create a data set with the average of each variable for each activity and each subject.
    setkey(data_all, SubjectName, ActName, Signal, SignalDomain, SignalSource, Acceleration, Jerk, Magnitude, Variable, Axis)
    data_all_tidy <- data_all[, list(count = .N,  avg = mean(Value)), by = key(data_all)]
    
# IX. Make codebook.
    library(knitr)
    knit(input = "./run_analysis.R", output = "./Codebook.md", encoding="ISO8859-1", quiet = T)
    library(markdown)
    markdownToHTML("./Codebook.md", "./Codebook.html")
    
