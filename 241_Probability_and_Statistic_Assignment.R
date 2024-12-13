  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(BMA)
  library(relaimpo)
  library(corrplot)
  library(questionr)
  library(car)


  # read data from .csv file and choose suitable variable to analysis
  All_GPUs <- read.csv("D:/BTL_XSTK/All_GPUs.circ")

  # print first 5 line in console 
  head(All_GPUs, n = 5)

  # provides a compact summary of the structure of the object All_GPUs
  str(All_GPUs)

  # display the column names of the ALL_GPUs data frame  
  names(All_GPUs)


  #######################################################################3: Preprocessing data ##############################################################################
  # start
  # Cleaning data, create new dataset with key variables 
  new_GPUs <- All_GPUs[,c("Core_Speed","Max_Power","Memory","Memory_Bandwidth","Memory_Speed","Process")]
  # Print the first 10 line of the new dataset
  head(new_GPUs, n = 10)
  # provides a compact summary of the structure of the object new_GPUs -> contain key variable
  str(new_GPUs)

  # Remove leading and trailing spaces
  new_GPUs[] <- lapply(new_GPUs, trimws)
  # Replace blank spaces and "-" with NA
  new_GPUs[new_GPUs == ""] <- NA
  new_GPUs[new_GPUs == "-"] <- NA

  # Count the number of missing values in each variable
  missing_values <- colSums(is.na(new_GPUs))
  # Display the number of missing values for each variable
  print(missing_values)


  # clean data 
  # check unit function 
  check_units <- function(a, b) {
    check <- new_GPUs[, c(a)]
    print(check)
    
    #regex
    valid_indices <- grep(paste0("[^ ]+ ", b), check)
    
    if (length(valid_indices) > 0) {
      
      print("All values are valid")
    } else {
      print("The invalid values are:")
      invalid_indices <- setdiff(seq_along(check), valid_indices)
      invalid_values <- check[invalid_indices]
      print(invalid_values)
      
    }
  }

  # frequency of na 
  freq_na <- function(variable, var_name) {
    total <- length(variable)  # Total number of observations
    missing <- sum(is.na(variable))  # Number of missing values (NA)
    percentage <- (missing / total) * 100  # Calculate percentage
    cat(sprintf("The percentage of missing values in %s is %.4f%%.\n", var_name, percentage))
  }

  # find outliners: 
    find_outliers <- function(data, variable) {
    # Calculate the first and third quartiles, and the IQR
    Q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # Define the outlier and extreme outlier thresholds
    lower_threshold <- Q1 - 1.5 * IQR
    upper_threshold <- Q3 + 1.5 * IQR
    extreme_lower_threshold <- Q1 - 3 * IQR
    extreme_upper_threshold <- Q3 + 3 * IQR
    
    # Identify outliers and extreme outliers
    outliers <- data[[variable]][data[[variable]] < lower_threshold | data[[variable]] > upper_threshold]
    extreme_outliers <- data[[variable]][data[[variable]] < extreme_lower_threshold | data[[variable]] > extreme_upper_threshold]
    
    # Check if there are valid outliers
    if (length(outliers) == 0) {
      print("All values lie within the acceptable range of outliers.")
    } else {
      # If extreme outliers exist, print them
      if (length(extreme_outliers) > 0) {
        print(paste("Extreme outliers are:", toString(extreme_outliers)))
      } else {
        print("All values lie within the acceptable range of outliers.")
      }
    }
  }

  #remove extreme outliners 
  remove_outliers <- function(data, column, lower_bound = 0.25, upper_bound = 0.75, multiplier = 3) {
    # Extract the target column
    target_col <- data[[column]]
    
    # Calculate quartiles
    lower_quantile <- quantile(target_col, lower_bound, na.rm = TRUE)
    upper_quantile <- quantile(target_col, upper_bound, na.rm = TRUE)
    
    # Calculate IQR
    iqr <- upper_quantile - lower_quantile
    
    # Define thresholds
    lower_threshold <- lower_quantile - multiplier * iqr
    upper_threshold <- upper_quantile + multiplier * iqr
    
    # Filter the data to remove outliers
    filtered_data <- subset(data, target_col >= lower_threshold & target_col <= upper_threshold)
    
    # Return the filtered dataset
    return(filtered_data)
  }

  #########################################################3.1: Core_Speed##########################################################
  # check unit of Core_Speed: 
  check_units("Core_Speed", "MHz")
  #remove unit
  new_GPUs$Core_Speed <- as.numeric(gsub(" MHz", "", new_GPUs$Core_Speed))

  str(new_GPUs$Core_Speed)

  #calculate the frequency of missing value
  freq_na(new_GPUs$Core_Speed, "Core_Speed")

  # replace NA value with median 
  new_GPUs$Core_Speed <- ifelse(is.na(new_GPUs$Core_Speed), median(new_GPUs$Core_Speed, na.rm=TRUE), new_GPUs$Core_Speed)
  summary(new_GPUs$Core_Speed)

  ggplot(new_GPUs,aes(x="",y=Core_Speed))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  find_outliers(new_GPUs, "Core_Speed")

  #remove extreme outliners: 
  new_GPUs <- remove_outliers(new_GPUs, "Core_Speed")

  # summary and plot box graph after removing extreme value: 
  summary(new_GPUs$Core_Speed)
  ggplot(new_GPUs,aes(x="",y=Core_Speed))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  ##########################################################3.2: Memory##############################################################
  # check unit of Memory: 
  check_units("Memory", "MB")
  # remove unit
  new_GPUs$Memory <- as.numeric(gsub("MB","",new_GPUs$Memory))

  str(new_GPUs$Memory)

  #calculate the frequency of missing value
  freq_na(new_GPUs$Memory, "Memory")

  # replace NA with median value
  new_GPUs <- new_GPUs %>% mutate(Memory = if_else(is.na(Memory), median(Memory, na.rm = TRUE), Memory))
  summary(new_GPUs$Memory)

  ggplot(new_GPUs,aes(x="",y=Memory))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  find_outliers(new_GPUs, "Memory")

  #remove extreme outliners:
  new_GPUs <- remove_outliers(new_GPUs, "Memory")

  # Plot graph after removing the outliners 
  summary(new_GPUs$Memory)
  ggplot(new_GPUs,aes(x="",y=Memory))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  ################################################################3.3: Memory_Speed##############################################################################################
  # check unit of Core_Speed:
  check_units("Memory_Speed", "MHz")
  # remove unit
  new_GPUs$Memory_Speed <- as.numeric(gsub(" MHz","",new_GPUs$Memory_Speed))

  str(new_GPUs$Memory_Speed)

  # calculate the frequency of missing value
  freq_na(new_GPUs$Memory_Speed, "Memory_Speed")

  # replace the missing value with median 
  new_GPUs <- new_GPUs %>% mutate(Memory_Speed = if_else(is.na(Memory_Speed), median(Memory_Speed, na.rm = TRUE), Memory_Speed))
  summary(new_GPUs$Memory_Speed)

  ggplot(new_GPUs,aes(x="",y=Memory_Speed))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  find_outliers(new_GPUs, "Memory_Speed")

  #remove extreme outliners:
  new_GPUs <- remove_outliers(new_GPUs, "Memory_Speed")

  # Plot graph after removing the outliners 
  summary(new_GPUs$Memory_Speed)
  ggplot(new_GPUs,aes(x="",y=Memory_Speed))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")
  ############################################################3.4: Process##################################################################################################
  # Check unit of "nm" function 
  check_units_process <- function(column_name, unit) {
    # Extract the specified column from the dataset
    check <- new_GPUs[[column_name]]
    
    # Print the column being checked (optional)
    cat("Checking the units for column:", column_name, "\n")
    
    # Regex to find values ending with the correct unit (e.g., "nm")
    valid_indices <- grep(paste0("^\\d+", unit, "$"), check)
    
    # Check if all values are valid
    if (length(valid_indices) == length(check[!is.na(check)])) {
      print("All values are valid.")
    } else {
      # Identify invalid values
      invalid_indices <- setdiff(seq_along(check), valid_indices)
      invalid_values <- check[invalid_indices]
      print("The invalid values are:")
      print(invalid_values)
    }
  }
  check_units_process("Process", "nm")
  # remove unit
  new_GPUs$Process <- as.numeric(gsub("nm","",new_GPUs$Process))

  str(new_GPUs$Process)

  # calculate the frequency of missing value 
  freq_na(new_GPUs$Process, "Process")

  # replace missing value with median
  new_GPUs <- new_GPUs %>% mutate(Process = if_else(is.na(Process), median(Process, na.rm = TRUE), Process))
  summary(new_GPUs$Process)

  ggplot(new_GPUs,aes(x="",y=Process))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  find_outliers(new_GPUs, "Process")

  #remove extreme outliners: 
  new_GPUs <- remove_outliers(new_GPUs, "Process")

  # plot graph 
  summary(new_GPUs$Process)
  ggplot(new_GPUs,aes(x="",y=Process))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  #############################################3.5: Memory Bandwidh#########################################################
  check_units_bandwidth <- function(column_name) {
    # Extract the specified column from the dataset
    check <- new_GPUs[[column_name]]
    
    # Print the column being checked (optional)
    cat("Checking the units for column:", column_name, "\n")
    
    # Regex to match valid units with optional decimals (e.g., "64GB/sec" or "51.2GB/sec")
    valid_indices <- grep("^\\d+(\\.\\d+)?(GB/sec)$", check)
    
    # Check if all values are valid
    if (length(valid_indices) == length(check[!is.na(check)])) {
      print("All values are valid.")
    } else {
      # Identify invalid values
      invalid_indices <- setdiff(seq_along(check), valid_indices)
      invalid_values <- check[invalid_indices]
      print("The invalid values are:")
      print(invalid_values)
    }
  }
  check_units_bandwidth("Memory_Bandwidth")

  # converts all values originally expressed in "MB/sec" into the equivalent values in "GB/sec" to ensure uniformity in unit
  temp <- grep("MB/sec",new_GPUs$Memory_Bandwidth)
  for(i in temp) new_GPUs$Memory_Bandwidth[i] <- as.numeric(gsub("MB/sec","",new_GPUs$Memory_Bandwidth[i]))/1024
  temp <- grep("GB/sec",new_GPUs$Memory_Bandwidth)
  for (i in temp) new_GPUs$Memory_Bandwidth[i] <- as.numeric(gsub("GB/sec","",new_GPUs$Memory_Bandwidth[i]))
  new_GPUs$Memory_Bandwidth <- as.numeric(gsub("GB/sec","",new_GPUs$Memory_Bandwidth))

  # remove unit 
  new_GPUs$Process <- as.numeric(gsub("nm","",new_GPUs$Process))

  str(new_GPUs$Memory_Bandwidth)

  # calculate the frequency of missing value 
  freq_na(new_GPUs$Memory_Bandwidth, "Memory_Bandwidth")

  # replace missing value with median 
  new_GPUs <- new_GPUs %>% mutate(Memory_Bandwidth = if_else(is.na(Memory_Bandwidth), median(Memory_Bandwidth, na.rm = TRUE), Memory_Bandwidth))
  summary(new_GPUs$Memory_Bandwidth)

  ggplot(new_GPUs,aes(x="",y=Memory_Bandwidth))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  find_outliers(new_GPUs, "Memory_Bandwidth")

  #remove extreme outliners:
  new_GPUs <- remove_outliers(new_GPUs, "Memory_Bandwidth")

  # plot graph
  summary(new_GPUs$Memory_Bandwidth)
  ggplot(new_GPUs,aes(x="",y=Memory_Bandwidth))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  ##################################################3.6: Max_Power ######################################################################################
  # check unit 
  check_units("Max_Power", "Watts")
  # remove unit
  new_GPUs$Max_Power <- as.numeric(gsub("Watts","",new_GPUs$Max_Power))

  str(new_GPUs$Max_Power)

  # calculate the frequency of missing value 
  freq_na(new_GPUs$Max_Power, "Max_Power")

  # replace missing value with median 
  new_GPUs$Max_Power <- ifelse(is.na(new_GPUs$Max_Power), median(new_GPUs$Max_Power, na.rm=TRUE), new_GPUs$Max_Power)
  summary(new_GPUs$Max_Power)

  ggplot(new_GPUs,aes(x="",y=Max_Power))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  find_outliers(new_GPUs, "Max_Power")

  # Remove extreme outliners values 
  new_GPUs <- remove_outliers(new_GPUs,"Max_Power")

  # plot graph
  summary(new_GPUs$Max_Power)
  ggplot(new_GPUs,aes(x="",y=Max_Power))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="blue",outlier.color="red")

  ################################################3.7: check lan cuoi######################################################################
  # Count the number of missing values in each variable
  missing_values <- colSums(is.na(new_GPUs))
  # Display the number of missing values for each variable
  print(missing_values)

  # Check if any column contains negative values
  any(new_GPUs$Core_Speed < 0)
  any(new_GPUs$Memory < 0)
  any(new_GPUs$Memory_Bandwidth < 0)
  any(new_GPUs$Memory_Speed < 0)
  any(new_GPUs$Process < 0)
  any(new_GPUs$Max_Power < 0)

  # check for duplicate data using the distinct() function in a pipeline format
  str(new_GPUs)
  new_GPUs <- new_GPUs %>% dplyr::distinct()
  str(new_GPUs)

  # Convert the variables to log form.
  new_GPUs2 <- new_GPUs
  new_GPUs2[,c("Memory","Core_Speed","Max_Power","Memory_Bandwidth","Memory_Speed","Process")] <- log(new_GPUs2[,c("Memory","Core_Speed","Max_Power","Memory_Bandwidth","Memory_Speed","Process")])
  head(new_GPUs2)
  #####################################################################################end preprocessing data ####################################################################################


  #########################################################################4: data display################################################################
  # Use lapply() to apply the summary() function to each variable
  custom_summary <- function(x) {
    # Use the summary function to get basic parameters
    summary_stats <- summary(x)
    # Calculate standard deviation 
    std_dev <- sd(x, na.rm = TRUE)
    # Combine results from summary and standard deviation
    c(summary_stats, Std_Dev = std_dev)
  }
  stat <- lapply(new_GPUs, custom_summary)

  # Print results
  print(stat)
  par(mfrow = c(1, 2))
  hist(new_GPUs$Memory_Speed,xlab = "Memory Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of Memory Speed",xlim = c(0,2500))
  hist(new_GPUs2$Memory_Speed,xlab = "Memory Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Memory Speed)",xlim = c(4,8))
  par(mfrow = c(1, 1))

  par(mfrow = c(1, 2))
  hist(new_GPUs$Core_Speed,xlab = "Core Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of Core Speed",xlim = c(0,2000))
  hist(new_GPUs2$Core_Speed,xlab = "Core Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Core Speed)",xlim = c(4,8))
  par(mfrow = c(1, 1))

  par(mfrow = c(1, 2))
  hist(new_GPUs$Max_Power,xlab = "Max Power",ylab = "Frequency",col = "lightblue",main = "Histogram of Max Power",xlim = c(0,300))
  hist(new_GPUs2$Max_Power,xlab = "Max Power",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Max Power)",xlim = c(0,7))
  par(mfrow = c(1, 1))

  par(mfrow = c(1, 2))
  hist(new_GPUs$Memory,xlab = "Memory",ylab = "Frequency",col = "lightblue",main = "Histogram of Memory",xlim = c(0,9000))
  hist(new_GPUs2$Memory,xlab = "Memory",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Memory)",xlim = c(2,10))
  par(mfrow = c(1, 1))

  par(mfrow = c(1, 2))
  hist(new_GPUs$Memory_Bandwidth,xlab = "Memory Bandwidth",ylab = "Frequency",col = "lightblue",main = "Histogram of Memory Bandwidth",xlim = c(0,400))
  hist(new_GPUs2$Memory_Bandwidth,xlab = "Memory Bandwidth",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Memory Bandwidth)",xlim = c(0,6))
  par(mfrow = c(1, 1))

  par(mfrow = c(1, 2))
  hist(new_GPUs$Process,xlab = "Process",ylab = "Frequency",col = "lightblue",main = "Histogram of Process",xlim = c(10,60))
  hist(new_GPUs2$Process,xlab = "Process",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Process)",xlim = c(2,5))
  par(mfrow = c(1, 1))

  #Draw correlation chart
  correlation_matrix <- cor(new_GPUs)

  # Draw correlation graph
  corrplot(correlation_matrix, method = "circle", 
          type = "full", order = "hclust",
          tl.col = "black", tl.srt = 45, 
          diag = TRUE, 
          addCoef.col = "black")
  ######################################################################end demonstrating data####################################################################


  ###############################################################multiple linear regression############################################
  #######################################################data division and multicollinearity check################
  vif(lm(Memory_Speed~Memory+Core_Speed+Max_Power+Memory_Bandwidth+Process, data = new_GPUs2))
  set.seed(42) 


  trainingRowIndex <- sample(1:nrow(new_GPUs2), 0.6 * nrow(new_GPUs2))
  remainingData <- new_GPUs2[-trainingRowIndex, ]

  validationRowIndex <- sample(1:nrow(remainingData), 0.5 * nrow(remainingData))


  trainingData <- new_GPUs2[trainingRowIndex, ] # 60% training
  validationData <- remainingData[validationRowIndex, ] # 20% validation
  testData <- remainingData[-validationRowIndex, ] # 20% test


  print(nrow(trainingData))   # trainingData's rows
  print(nrow(validationData)) # validationData's rows
  print(nrow(testData))       # testData's rows


  ###############################################################5: choose module###############################################
  cols=colnames(trainingData)
  yvar=trainingData[,("Memory_Speed")]
  str(yvar)
  xvars <- trainingData[, !(colnames(trainingData) %in% "Memory_Speed"), drop = FALSE]
  bma=bicreg(xvars,yvar,strict=F,OR=2)
  print(summary(bma))
  ##################################find weight######################
  #m=lm(Memory_Speed~Memory+Core_Speed+Max_Power+Memory_Bandwidth+Process, data=trainingData)
  #calc.relimp(m,type="lmg",rela=T, rank=T)
  #################################make training data and test data###############################

  ################################construct model###################################
  #tao mo hinh tu training data
  lmMod<-lm(Memory_Speed~Memory+Core_Speed+Max_Power+Memory_Bandwidth+Process, data=trainingData)
  #shapiro.test(residuals(lmMod))
  cPred<-predict(lmMod,validationData)
  cPred1<-predict(lmMod,trainingData)
  #mean square error tu mo hinh
  #mse<-mean(lmMod$residuals^2)
  #mean square error tu testData
  mse_test=mean((validationData$Memory_Speed-cPred)^2)
  mse_test1=mean((trainingData$Memory_Speed-cPred1)^2)

  #iN MSE
  #print(paste("mse cua mo hinh: ",mse))
  print(paste("validationData' s mse: ",mse_test))
  print(paste("trainingData' s mse: ",mse_test1))
  #############################find m1,m2,.. b#################################
  print(summary(lmMod))

  ############################Draw charts###################################
  par(mfrow =c(2,2))
  plot(lmMod)
  par(mfrow = c(1,1))
  ##############################compare with initial dataset######################
  Memory_Speed=testData$Memory_Speed
  Memory=testData$Memory
  Core_Speed=testData$Core_Speed
  Max_Power=testData$Max_Power
  Memory_Bandwidth=testData$Memory_Bandwidth
  Process=testData$Process

  data_predict<-data.frame(Memory,Core_Speed,Max_Power,Memory_Bandwidth,Process)
  P<-lm(Memory_Speed~.,data=data_predict)
  summary(P)
  predict_Memory_Speed=predict(P)
  p=data.frame(predict_Memory_Speed,Memory_Speed)
  head(p,5)

  # prediction
  difference <- abs(predict_Memory_Speed - Memory_Speed) # Difference between prediction and reality
  within_threshold <- difference <= 0.5                  # Compare difference with threshold 0.5
  percentage_within_threshold <- mean(within_threshold) * 100 # Tính phần trăm

  # show results
  cat("Percentage of residuals being equal or smaller than 0.5:", percentage_within_threshold, "%\n")
  #############################Prediction##########################################################
  #make prediction of Memory_Speed when Memory=24000MB, Core_Speed=2235MHz, Max_Power=450 Watts, Memory_Bandwidth=1000GB/sec, Process=8nm
  #make dataframe
  x2<-24000;x3<-2235;x4<-450;x5<-1000;x6<-8
  y1<-data.frame("Memory"=log(x2),"Core_Speed"=log(x3),"Max_Power"=log(x4),"Memory_Bandwidth"=log(x5),"Process"=log(x6))
  predict_X<-predict(lmMod,y1,interval="confidence")
  head(predict_X)
  print("Solution: ")
  print(exp(predict_X))

