# Formally assign SNA bootstrap resampling + Monte Carlo simulation function to global environment
esnar_bootstrap <- function(tidy.esnar2.data = tidy.esnar2.data,
                            seed = 2020,
                            operation = sd,
                            confidence_interval = c(0.001, 0.01,0.025,0.05,0.95,0.975, 0.99, 0.999),
                            upper_and_lower_intervals = c(0.05,0.95),
                            x_axis = x_axis,
                            net_size = 20){

  #####

  # tidy.esnar2.data = tidy.esnar2.data
  # seed = 2020
  # operation = sd
  # confidence_interval = c(0.001, 0.01,0.025,0.05,0.95,0.975, 0.99, 0.999)
  # upper_and_lower_intervals = c(0.05,0.95)
  # x_axis = x_axis
  # net_size = 20

  #####

  ### Set default for 'net_size' parameter if unspecified

  if(missing(net_size)){
    net_size = 20
  } else if(is.function(net_size)) {
    net_size = net_size
  }

  ### Set default for 'x-axis' parameter if unspecified

  if(missing(x_axis)){
    x_axis = paste("Pseudo-Sampling Distribution of the Standard Deviation")
  } else if(is.function(x_axis)) {
    x_axis = x_axis
  }

  ### Set default for 'operation' parameter if unspecified

  if(missing(operation)){
    operation = sd
  } else if(is.function(operation)) {
    operation = operation
  }

  ### Set default for 'seed' parameter if unspecified

  if(missing(seed)){
    seed = 2020
  } else{
    seed = seed
  }

  ### Set default for 'confidence_interval' parameter if unspecified

  if(missing(confidence_interval)){
    confidence_interval = c(0.001, 0.01,0.025,0.05,0.95,0.975, 0.99, 0.999)
  } else{
    confidence_interval = confidence_interval
  }

  ### Set default for 'upper_and_lower_intervals' parameter if unspecified

  if(missing(upper_and_lower_intervals)){
    upper_and_lower_intervals = c(0.05,0.95)
  } else{
    upper_and_lower_intervals = upper_and_lower_intervals
  }

  ### Assign inputs of 'upper_and_lower_intervals' to their own object:
  ###
  ###    'upper', which contains the upper bound of a specified CI
  ###    'lower', which contains the upper bound of a specified CI

  upper <- paste0(as.character(upper_and_lower_intervals[2]*100),"%")
  lower <- paste0(as.character(upper_and_lower_intervals[1]*100),"%")

  ##### == Creates data frame of Std.Devs for each egos'/participants' ACTUAL OBSERVED EXPERIMENTAL data across all esnar2 variables called 'aamobc.sd' == #####


  ### Perform the mathematical operation specified in the 'operation' parameter across each
  ###    of each ego's responses across all esnar2 variables. Skip over "NA' values. store
  ###    these values into the object, 'aamobc.sd'.

  aamobc.sd <- aggregate(tidy.esnar2.data[,3:ncol(tidy.esnar2.data)],
                         list(tidy.esnar2.data$egoID),
                         operation, na.rm = T)

  ### Change the name of the first column in 'aamobc.sd' from 'Group.1' to 'egoID'.

  colnames(aamobc.sd)[1] <-"egoID"

  ### Calculate the column mean of each esnar2 variable, which are stored in unique columns, within in the 'aamobc.sd' object.
  ###    This yields an average value for the mathematical calculation specified in the 'operation' parameter
  ###    of a given esnar2 variable.

  avg.aamobc.sd <- sapply(aamobc.sd, mean, na.rm = T)[-1]

  # Append the 'column mean' values stored in 'avg.aamobc.sd' as the bottom-most row of 'aamobc.sd'.

  aamobc.sd[nrow(aamobc.sd) + 1,
            1] <- "Column Means"

  aamobc.sd[nrow(aamobc.sd),
            2:ncol(aamobc.sd)] <-  avg.aamobc.sd

  ##### == Creates data frame of Std.Devs for each egos' participants' PSEUDO BOOTSTRAP RESAMPLED data across all esnar2 variables called '  pseudo.aamobc.bootstrap' == #####

  #####

  while (T) {

    ### Set seed value to obtain consistent results

    set.seed(seed)

    ### These lines set up the data frame, 'pseudo.aamobc.bootstrap', that will contained
    ###    the resampled esnar2 data. Data frame will consist of the same number of rows
    ###    in 'aamobc.sd' minus one, and the same number of columns.

    pseudo.aamobc.bootstrap <<- data.frame(matrix(ncol = ncol(aamobc.sd),
                                                  nrow = nrow(aamobc.sd[-1,])
    ))

    rownames(pseudo.aamobc.bootstrap) <- #rownames(aamobc.sd[-153,])
      rownames(aamobc.sd[-nrow(aamobc.sd),])

    colnames(pseudo.aamobc.bootstrap) <- colnames(aamobc.sd)

    pseudo.aamobc.bootstrap$egoID <- #aamobc.sd$egoID[-153]
      aamobc.sd$egoID[-nrow(aamobc.sd)]

    ### Create an empty object, 'mean_list'
    ### Create an empty list, 'CI95_list'

    mean_list <- NULL
    CI95_list <- list()

    for (i in 1:length(colnames(tidy.esnar2.data)[3:ncol(tidy.esnar2.data)])) {

      ### The first for-loop iterates across each esnar2 variable column in 'tidy.esnar2.data'.

      ### Create an object, 'es_alc' which stores all values stored within the currently indexed column in 'tidy.esnar2.data'.
      ### Create an empty list, 'alc.list'.


      es_alc <- tidy.esnar2.data[,2+i]
      alc.list <- list()

      for (j in 1:length(unique(tidy.esnar2.data$egoID))){

        ### The second for-loop iterates across every unique egoID in 'tidy.esnar2.data$egoID'

        ### From the currently populated 'es_alc' object, randomly sample (with replacement) a 'net-size' value,
        ###   and store them as their own instance in the list, 'alc_list'.

        alc.list[[j]] <- sample(es_alc, replace = T, size = net_size)

      }

      ### From the 'alc.list' object, perform the specific calculation in 'operation' upon each group of
      ###     20 values. This yields a list of specific parameters whose length equals the number of unique egoIDs.
      ###     Convert this list into a vector, and store the vector in the object, 'alc.vect'.
      ###     Calculate the specified quantiles/confidence intervals using 'alc.vect'. Store these confidence intervals in
      ###     the currently indexed instance of the 'CI95_list' object.

      alc.vect <- unlist(lapply(alc.list,operation,na.rm = T))

      CI95_list[[i]] <-  quantile(alc.vect, confidence_interval,na.rm = T)

      ### Plot histograms of the pseudo-distribution of 'alc.vect' for each esnar2 variable.

      hist(alc.vect, main = colnames(tidy.esnar2.data)[2+i],
           xlab = x_axis)

      ### Display red lines at the specified 'upper' and 'lower' quantiles to clearly demarcate CIs.

      abline(v = CI95_list[[i]][[lower]], col="red", lwd=3, lty=2)
      abline(v = CI95_list[[i]][[upper]], col="red", lwd=3, lty=2)

      ### Store current values of 'alc.vect' for the current esnar2 variable in its
      ###     corresponding column in the 'pseudo.aamobc.bootstrap' object.

      pseudo.aamobc.bootstrap[,1+i] <- alc.vect

      ### Calculate the average value of 'alc.vect' and append it to the 'mean_list' object. Ignore NA's in the calculation of the mean. This list
      ###      (when fully calculated) will be equivalent to the column means of the pseudo-data.

      mean_list <- c(mean_list,
                     mean(alc.vect, na.rm = T))

    }

    ### Append the 'Column Means' as the last row of the 'pseudo.aamobc.bootstrap' data frame.

    pseudo.aamobc.bootstrap[nrow(aamobc.sd),] <- c("Column Means",unlist(mean_list))

    ### End the current while-loop

    #hist.list <<- hist.list

    break

  }

  ### The object, '  pseudo.aamobc.bootstrap', contains the calculation of each parameter specified by 'operation' for each ego's pseudo-resampled data
  ###      across all 15 esnar2 variables.

  pseudo.aamobc.bootstrap <<- pseudo.aamobc.bootstrap

  ### View full 'pseudo.aamobc.bootstrap' data frame.

  View(pseudo.aamobc.bootstrap)

  #####

  while(T){

    ### Create data frame, 'aamobc_testdf', the same number of rows and
    ###    the same number of columns (minus one) of 'aamobc.sd'. This data
    ###    frame will have the same column names as 'aamobc.sd'. This data frame
    ###    will contain the outcomes of comparing an ego's actual experimental data
    ###    against the pseudo-distribution's confidence interval.

    aamobc_testdf <- data.frame(matrix(ncol = ncol(aamobc.sd),
                                       nrow = nrow(aamobc.sd[-1,])))



    colnames(aamobc_testdf) <- colnames(aamobc.sd)

    aamobc_testdf$egoID <- aamobc.sd$egoID[-#153
                                             nrow(aamobc.sd)
                                           ]

    ### Initialize an empty list, 'counts_list'

    counts_list <- list()

    for (x in 1:ncol(pseudo.aamobc.bootstrap[,-1])) {

      ### Iterate across all (except the first) columns in 'pseudo.aamobc.bootstrap'.

      for (y in 1:length(pseudo.aamobc.bootstrap$egoID[-#153
                                                       nrow(pseudo.aamobc.bootstrap)
                                                       ])) {

        ### Iterate across all (except the first) unique egoIDs in 'pseudo.aamobc.bootstrap'.

        ### These if-statements conduct the actual comparison of each participant's
        ###   observed data against the confidence intervals of the pseudo-distribution.
        ###
        ### For a given egoID, for a given esnar2 variable column, see whether the data in that cell
        ###   is 'bigger' than, 'smaller' than, contained within the CI ('normal'), or missing all together,
        ###   then put in the appropriate value into the corresponding cell in 'aamobc_testdf'.

        if(is.na(aamobc.sd[y ,1+x])){

          aamobc_testdf[y ,1+x] <- "missing"

          #print('missing')


        } else if(CI95_list[[x]][[lower]] > aamobc.sd[y ,1+x]){

          aamobc_testdf[y ,1+x] <- "small"

          #print("small")

        } else if(CI95_list[[x]][[upper]] < aamobc.sd[y ,1+x]) {

          aamobc_testdf[y ,1+x] <- "big"

          #print("big")

        } else {

          aamobc_testdf[y ,1+x] <- "normal"

          #print('normal')

        }

      }
    }

    for (i in 1:length(aamobc_testdf$egoID)) {

      ### After comparing all participants against the CI of the pseudo-distribution, keep a count of
      ###     number of number of unique test results, and store them in a unique instance in the
      ###     'counts_list' object.

      counts_list[[i]] <- table(as.character(aamobc_testdf[i,2:ncol(aamobc_testdf)]))

    }

    # Break out of while-loop.

    break

  }

  #####

  while (T) {

    ### Initialize an empty list called 'fake_list'.

    fake_list <- list()

    ### Create an empty data frame called 'fake_df' with 5 columns and a number of rows equal
    ###     to the length of 'aamobc.sd$egoID'. The five columns are named: "egoID","big",
    ###     "missing","normal","small".

    fake_df <- data.frame(matrix(nrow = length(aamobc.sd$egoID),ncol = 5))

    colnames(fake_df) <- c("egoID","big","missing","normal","small")

    ### Initialize a character vector, 'char_vect', of all except the first column header of
    ###     'fake_df'.

    char_vect <- colnames(fake_df)[-1]

    ### Create an empty data frame,'test_result_df', with the number of columns equal to length of 'charvect',
    ###    and the number of rows equal to the length of 'aamobc$egoID'. Set the column names of 'test_result_df'
    ###    to the same as 'char_vect'.

    test_result_df <- data.frame(matrix(ncol = length(char_vect),nrow = length(aamobc.sd$egoID)))

    colnames(test_result_df) <- char_vect

    ### Combine 'aamoboc.sd' and 'test_result_df' by their columns, and store them the new
    ###    object 'real.aamobc.bootstrap'.

    real.aamobc.bootstrap <<- cbind(aamobc.sd,test_result_df)

    for (i in 1:c(nrow(aamobc.sd) - 1)){

      ### For the total number of rows (minus one) of the 'aamobc.sd' data frame

      ### Take the currently-instanced row of the 'aamobc_testdf' and store the counts of unique values
      ###     in the object,'row_vect'.

      row_vect <- table(unlist(aamobc_testdf[i,]))


      for (j in 1:length(char_vect)) {

        ### For the total length of 'char_vect'.

        ### If the currently-indexed value of 'char_vect' isin the currently-indexed
        ###     entry of 'row_vect', then skip to the next iteration.
        ### Else, assign that index a value of 0 and move to the next index of 'char_vect'.

        if (row_vect[char_vect[j]] %in% row_vect[2:length(row_vect)]){

          next

        }

        else {

          row_vect[char_vect[j]] <- 0

        }

      }

      ### Assign the processed current instance of 'row_vect' -- ordered according to 'char_vect' --
      ###    to the corresponding row of and proper columns of the 'real.aamobc.bootstrap'.

      real.aamobc.bootstrap[i,char_vect] <- row_vect[char_vect]

    }

    ### Assign 'real.aamobc.bootstrap' to itself, and the global environment. Display the full
    ###    'real.aamobc.bootstrap' data frame.

    real.aamobc.bootstrap <<- real.aamobc.bootstrap

    View(real.aamobc.bootstrap)

    ### Break out of while-loop

    break

  }
}
