alter_tie_bootstrap <-
  function(rough.edge.list = rough.edge.list,
           seed = 2020,
           operation = var,
           confidence_interval = c(0.001, 0.01,0.025,0.05,0.95,0.975, 0.99, 0.999),
           upper_and_lower_intervals = c(0.05,0.95),
           x_axis = x_axis){
########################################################################################

### Set default for 'x-axis' parameter if unspecified

if(missing(x_axis)){
  x_axis = paste("Variance in alter-alter ties")
} else if(is.function(x_axis)) {
  x_axis = x_axis
}

### Set default for 'operation' parameter if unspecified

if(missing(operation)){
  operation = var
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

set.seed(seed)


real_var_df <- aggregate(rough.edge.list$weights,
                         list(rough.edge.list$egoID),
                         operation, na.rm = T)

colnames(real_var_df) <- c('egoID','variance')

while (T) {
  
  pseudo_var_df <- real_var_df
  pseudo_var_df$`variance` <- NA
  
  for(i in 1:nrow(real_var_df)){
    
    
    pseudo_var_df[i,2] <- 
      var(sample(rough.edge.list$weights,
             size = nrow(rough.edge.list[rough.edge.list$egoID == rough.edge.list$egoID[i],]), 
             replace = T),
          na.rm = T)
    
  }
  
  CI95_list <-  quantile(pseudo_var_df$variance, confidence_interval,na.rm = T)
  
  ### Plot histograms of the pseudo-distribution of 'alc.vect' for each esnar2 variable.
  
  hist(pseudo_var_df$variance, main = "Pseudo-Sampling Distribution of variance Within Alter-Alter Edges",
       xlab = x_axis)
  
  ### Display red lines at the specified 'upper' and 'lower' quantiles to clearly demarcate CIs.
  
  abline(v = CI95_list[[lower]], col="red", lwd=3, lty=2)
  abline(v = CI95_list[[upper]], col="red", lwd=3, lty=2)
  
  
  real_var_df$'test results' <- NA
  
  for (y in 1:length(real_var_df$egoID)) {
    
    ### Iterate across all (except the last) unique egoIDs in 'pseudo.aamobc.bootstrap'.
    
    ### These if-statements conduct the actual comparison of each participant's
    ###   observed data against the confidence intervals of the pseudo-distribution.
    ###
    ### For a given egoID, for a given esnar2 variable column, see whether the data in that cell
    ###   is 'bigger' than, 'smaller' than, contained within the CI ('normal'), or missing all together,
    ###   then put in the appropriate value into the corresponding cell in 'aamobc_testdf'.
    
    if(is.na(real_var_df[y ,2])){
      
      real_var_df[y ,3] <- "missing"
      
      #print('missing')
      
      
    } else if(CI95_list[[lower]] > real_var_df[y ,2]){
      
      real_var_df[y ,3] <- "small"
      
      #print("small")
      
    } else if(CI95_list[[upper]] < real_var_df[y ,2]) {
      
      real_var_df[y,3] <- "big"
      
      
    } else {
      
      real_var_df[y ,3] <- "normal"
      
    }
    
  }
  
  break
  
}

real_var_df <<- real_var_df
pseudo_var_df <<- pseudo_var_df 

View(real_var_df)
View(pseudo_var_df)

}
