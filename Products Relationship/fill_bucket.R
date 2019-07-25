seed <- "PRODUCT_P18"
ignore <- c()
init <- c(seed)

fill_bucket <- function(m, filters = NULL, ignore = NULL, howmany = 2) {

  m <- m %>%
    select(-CUSTOMER_ID) 
  
  if(length(ignore) > 0) {
    m <- m %>%
      select(-ignore)
  }  
  
  if(length(filters) > 0) {
    m <- m %>%
      filter_at(vars(filters), all_vars(. == 1))
  }

  if(length(filters) > 0) {
    tot <- colSums(m %>% select(-filters))
  }
  else{
    tot <- colSums(m)
  }
  
  candidate <- names(which.max(tot))

  filters <- c(filters, candidate)
  
  if(length(filters) >= howmany){
    n_users <- resultsOneHotEncodingGrouped %>%
      filter_at(vars(filters), any_vars(. == 1)) %>%
      summarise(n_distinct(CUSTOMER_ID)) %>%
      as.numeric()
    
    l <- list(bucket = filters, n_users = n_users)
    
    return(l)
  } 
  else
  {
    fill_bucket(resultsOneHotEncodingGrouped, filters, ignore, howmany)
  }
}

bucket1 <- fill_bucket(resultsOneHotEncodingGrouped, init, ignore, 6)
bucket2 <- fill_bucket(resultsOneHotEncodingGrouped, NULL, bucket1$bucket, 6)
bucket3 <- fill_bucket(resultsOneHotEncodingGrouped, NULL, c(bucket1$bucket, bucket2$bucket), 6)
bucket4 <- fill_bucket(resultsOneHotEncodingGrouped, NULL, c(bucket1$bucket, bucket2$bucket, bucket3$bucket), 6)
bucket5 <- fill_bucket(resultsOneHotEncodingGrouped, NULL, c(bucket1$bucket, bucket2$bucket, bucket3$bucket, bucket4$bucket), 6)
