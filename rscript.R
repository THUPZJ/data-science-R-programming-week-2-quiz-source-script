pollutantmean <- function(directory, pollutant, id = 1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all moniters list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  sum <- 0.0
  count <- 0
  for (item in id)
  {
    item <- sprintf("%03d", item)
    file_name <- paste(directory, "/", item, ".csv", sep = "")
    temp <- read.csv(file_name)
    list_temp <- temp[pollutant]
    for (i in 1:nrow(list_temp))
    {
      if (!is.na(list_temp[i, 1]))
      {
        sum <- sum + list_temp[i, 1]
        count <- count + 1
      }
    }
  }
  sum / count
}
complete <- function(directory, id = 1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the mointor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  id_tmp <- id
  dataframe <- data.frame(id = id_tmp,
                          nobs = rep(0, length(id_tmp)))
  count <- 0
  for (ite in id_tmp)
  {
    count <- 0
    ite <- sprintf("%03d", ite)
    file_name <- paste(directory, "/", ite, ".csv", sep = "")
    temp <- read.csv(file_name)
    # print(temp)
    for (i in 1:nrow(temp))
    {
      if (!is.na(temp[i, 2]) & !is.na(temp[i, 3]))
      {
        # print(temp[i, 2])
        # print(temp[i, 3])
        count <- count + 1
      }
    }
    # print(count)
    # print(ite)
    index <- which(dataframe["id"] == as.integer(ite))
    # print(index)
    dataframe[index, 2] <- count
    
  }
  dataframe
}

corr <- function(directory, threshold = 0)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files 
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  id <- 1:332
  answer <- c()
  dataframe <- complete(directory, id)
  for(i in 1:332)
  {
    if(dataframe[i,2] >= threshold)
    {
      x <- c()
      y <- c()
      item <- sprintf("%03d", i)
      file_name <- paste(directory, "/", item, ".csv", sep = "")
      temp <- read.csv(file_name)
      for(j in 1:nrow(temp))
      {
        if(!is.na(temp[j,2]) & !is.na(temp[j,3]))
        {
          x <- append(x, temp[j,2])
          y <- append(y, temp[j,3])
        }
      }
      answer <- append(answer, cor(x, y))
    }
  }
  answer
}