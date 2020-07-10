# Perform bootstrap resampling on poast-dichotomized absolute edge counts
post_dic_edge_bootstrap <- function(master.edge.list = master.edge.list,
                            seed = 2020,
                            operation = sum,
                            confidence_interval = c(0.001, 0.01,0.025,0.05,0.95,0.975, 0.99, 0.999),
                            upper_and_lower_intervals = c(0.05,0.95),
                            x_axis = x_axis
                            ){

   ### Set default for 'net_size' parameter if unspecified

  # if(missing(net_size)){
  #   net_size = 20
  # } else if(is.function(net_size)) {
  #   net_size = net_size
  # }

  ### Set default for 'x-axis' parameter if unspecified

  if(missing(x_axis)){
    x_axis = paste("Pseudo-Sampling Distribution of the Absolute Number of Edges")
  } else if(is.function(x_axis)) {
    x_axis = x_axis
  }

  ### Set default for 'operation' parameter if unspecified

  if(missing(operation)){
    operation = sum
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

  ##### == Creates data frame of Std.Devs for each egos'/participants' ACTUAL OBSERVED EXPERIMENTAL data across all esnar2 variables called 'aamobc.edge' == #####


  ### Perform the mathematical operation specified in the 'operation' parameter across each
  ###    of each ego's responses across all esnar2 variables. Skip over "NA' values. store
  ###    these values into the object, 'aamobc.edge'.

  aamobc.edge <- aggregate(master.edge.list$weights.Trans,
                         list(master.edge.list$egoID),
                         operation, na.rm = T)

  ### Change the name of the first column in 'aamobc.edge' from 'Group.1' to 'egoID'.

  colnames(aamobc.edge)[1] <-"egoID"

  ### Calculate the column mean of each esnar2 variable, which are stored in unique columns, within in the 'aamobc.edge' object.
  ###    This yields an average value for the mathematical calculation specified in the 'operation' parameter
  ###    of a given esnar2 variable.

  avg.aamobc.edge <- sapply(aamobc.edge, mean, na.rm = T)[-1]

  # Append the 'column mean' values stored in 'avg.aamobc.edge' as the bottom-most row of 'aamobc.edge'.

  aamobc.edge[nrow(aamobc.edge) + 1,
            1] <- "Column Mean"

  aamobc.edge[nrow(aamobc.edge),
            2] <-  avg.aamobc.edge

  ##### == Creates data frame of Std.Devs for each egos' participants' PSEUDO BOOTSTRAP RESAMPLED data across all esnar2 variables called '  pseudo.aamobc.bootstrap' == #####

  #####

    ### Set seed value to obtain consistent results

    set.seed(seed)

    ### These lines set up the data frame, 'pseudo.aamobc.bootstrap', that will contained
    ###    the resampled esnar2 data. Data frame will consist of the same number of rows
    ###    in 'aamobc.edge' minus one, and the same number of columns.

    pseudo.aamobc.bootstrap <<- data.frame(matrix(ncol = ncol(aamobc.edge),
                                                  nrow = nrow(aamobc.edge[-1,])
    ))

    rownames(pseudo.aamobc.bootstrap) <- #rownames(aamobc.edge[-153,])
      rownames(aamobc.edge[-nrow(aamobc.edge),])

    colnames(pseudo.aamobc.bootstrap) <- c(colnames(aamobc.edge)[1],'Pseudo-edges')

    pseudo.aamobc.bootstrap$egoID <- #aamobc.edge$egoID[-153]
      aamobc.edge$egoID[-nrow(aamobc.edge)]

    ### Create an empty object, 'mean_list'
    ### Create an empty list, 'CI95_list'

    mean_list <- NULL
    CI95_list <- list()


      ### The first for-loop iterates across each esnar2 variable column in 'master.edge.list'.

      ### Create an object, 'es_alc' which stores all values stored within the currently indexed column in 'master.edge.list'.
      ### Create an empty list, 'alc.list'.


      es_alc <- aamobc.edge$x[-nrow(aamobc.edge)]
      #alc.list <- list()


        ### The second for-loop iterates across every unique egoID in 'master.edge.list$egoID'

        ### From the currently populated 'es_alc' object, randomly sample (with replacement) a 'net-size' value,
        ###   and store them as their own instance in the list, 'alc_list'.

        alc.list <- sample(es_alc, replace = T, size = length(unique(aamobc.edge$egoID)[-nrow(aamobc.edge)]))



      ### From the 'alc.list' object, perform the specific calculation in 'operation' upon each group of
      ###     20 values. This yields a list of specific parameters whose length equals the number of unique egoIDs.
      ###     Convert this list into a vector, and store the vector in the object, 'alc.vect'.
      ###     Calculate the specified quantiles/confidence intervals using 'alc.vect'. Store these confidence intervals in
      ###     the currently indexed instance of the 'CI95_list' object.


      CI95_list <-  quantile(alc.list, confidence_interval,na.rm = T)

      ### Plot histograms of the pseudo-distribution of 'alc.vect' for each esnar2 variable.

      hist(alc.list, main = "Alter-Alter Edges",
           xlab = x_axis)

      ### Display red lines at the specified 'upper' and 'lower' quantiles to clearly demarcate CIs.

      abline(v = CI95_list[[lower]], col="red", lwd=3, lty=2)
      abline(v = CI95_list[[upper]], col="red", lwd=3, lty=2)

      ### Store current values of 'alc.vect' for the current esnar2 variable in its
      ###     corresponding column in the 'pseudo.aamobc.bootstrap' object.

      pseudo.aamobc.bootstrap[,2] <- alc.list

      ### Calculate the average value of 'alc.vect' and append it to the 'mean_list' object. Ignore NA's in the calculation of the mean. This list
      ###      (when fully calculated) will be equivalent to the column means of the pseudo-data.

      mean_list <- mean(alc.list, na.rm = T)



    ### Append the 'Column Means' as the last row of the 'pseudo.aamobc.bootstrap' data frame.

    pseudo.aamobc.bootstrap[nrow(aamobc.edge),] <- c("Column Means",mean_list)


  ### The object, '  pseudo.aamobc.bootstrap', contains the calculation of each parameter specified by 'operation' for each ego's pseudo-resampled data
  ###      across all 15 esnar2 variables.

  pseudo.aamobc.edge <<- pseudo.aamobc.bootstrap

  ### View full 'pseudo.aamobc.bootstrap' data frame.

  View(pseudo.aamobc.edge)

  #####

  while(T){

    ### Create data frame, 'aamobc_testdf', the same number of rows and
    ###    the same number of columns (minus one) of 'aamobc.edge'. This data
    ###    frame will have the same column names as 'aamobc.edge'. This data frame
    ###    will contain the outcomes of comparing an ego's actual experimental data
    ###    against the pseudo-distribution's confidence interval.

    aamobc_testdf <- data.frame(matrix(ncol = ncol(aamobc.edge),
                                       nrow = nrow(aamobc.edge[-1,])))



    colnames(aamobc_testdf) <- c(colnames(aamobc.edge)[1],'Test Results')

    aamobc_testdf$egoID <- aamobc.edge$egoID[-#153
                                             nrow(aamobc.edge)
                                           ]

    ### Initialize an empty list, 'counts_list'

    aamobc.edge[,3] <- NA

    counts_list <- list()

   # for (x in 1:ncol(pseudo.aamobc.bootstrap[,-1])) {

      ### Iterate across all (except the first) columns in 'pseudo.aamobc.bootstrap'.

      for (y in 1:length(pseudo.aamobc.bootstrap$egoID[-#153
                                                       nrow(pseudo.aamobc.bootstrap)
                                                       ])) {

        ### Iterate across all (except the last) unique egoIDs in 'pseudo.aamobc.bootstrap'.

        ### These if-statements conduct the actual comparison of each participant's
        ###   observed data against the confidence intervals of the pseudo-distribution.
        ###
        ### For a given egoID, for a given esnar2 variable column, see whether the data in that cell
        ###   is 'bigger' than, 'smaller' than, contained within the CI ('normal'), or missing all together,
        ###   then put in the appropriate value into the corresponding cell in 'aamobc_testdf'.

        if(is.na(aamobc.edge[y ,2])){

          aamobc.edge[y ,3] <- "missing"

          #print('missing')


        } else if(CI95_list[[lower]] > aamobc.edge[y ,2]){

          aamobc.edge[y ,3] <- "small"

          #print("small")

        } else if(CI95_list[[upper]] < aamobc.edge[y ,2]) {

          aamobc.edge[y,3] <- "big"


        } else {

          aamobc.edge[y ,3] <- "normal"


        }

      }

    colnames(aamobc.edge) <- c('egoID', 'number of actual ties', 'test result')

     real.aamobc.edge <<- aamobc.edge
     View(real.aamobc.edge)

    break

    }
}


post_dic_edge_bootstrap(master.edge.list)
