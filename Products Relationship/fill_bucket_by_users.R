df <- resultsBinarizedWithFrequency

create_buckets <- function(b) {
  buckets <- purrr::map(.x = 1:b, ~list(products = NULL, n = 0))
  names(buckets) <- paste0("bucket", 1:6)
  return(buckets)
}

fill_user_bucket <- function(df, b) {
  buckets <- create_buckets(b)
  for (i in 1:nrow(df)) {
    min_idx <- which.min(as.numeric(purrr::map(buckets, ~.x$n)))
    buckets[[min_idx]]$products <- c(df[i, ]$PRODUCT, buckets[[min_idx]]$products)
    buckets[[min_idx]]$n <- as.numeric(buckets[[min_idx]]$n + df[i, ]$n)
    
  }
  return(buckets)
}

buckets <- fill_user_bucket(df, b = 6)

purrr::map(buckets, ~.x$n)

nms <- resultsOneHotEncodingGrouped %>%
        names()

nms <- nms[-1]

products <- c("PRODUCT_P1",
              "PRODUCT_P2",
              "PRODUCT_P3",
              "PRODUCT_P4",
              "PRODUCT_P5",
              "PRODUCT_P6",
              "PRODUCT_P7",
              "PRODUCT_P8",
              "PRODUCT_P9",
              "PRODUCT_P10",
              "PRODUCT_P11",
              "PRODUCT_P12",
              "PRODUCT_P13",
              "PRODUCT_P14",
              "PRODUCT_P15",
              "PRODUCT_P16",
              "PRODUCT_P17",
              "PRODUCT_P18",
              "PRODUCT_P19",
              "PRODUCT_P20",
              "PRODUCT_P21",
              "PRODUCT_P22",
              "PRODUCT_P23",
              "PRODUCT_P24",
              "PRODUCT_P25",
              "PRODUCT_P26",
              "PRODUCT_P27",
              "PRODUCT_P28",
              "PRODUCT_P29",
              "PRODUCT_P30")

mapping <- data.frame(bit = 1:length(products), products)

bits2prods <- function(b, mp = mapping, nbits = 30) {
  bits <- strsplit(b, "")[[1]]
  products <- as.character(mp$products[which(bits == "1")])
  return(products)
}

bucket1_configs <- purrr::map(buckets$bucket1$products, ~bits2prods(.x))
bucket2_configs <- purrr::map(buckets$bucket2$products, ~bits2prods(.x))
bucket3_configs <- purrr::map(buckets$bucket3$products, ~bits2prods(.x))
bucket4_configs <- purrr::map(buckets$bucket4$products, ~bits2prods(.x))
bucket5_configs <- purrr::map(buckets$bucket5$products, ~bits2prods(.x))
bucket6_configs <- purrr::map(buckets$bucket6$products, ~bits2prods(.x))