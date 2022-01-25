library(dplyr)
library(plyr)
library(reshape2)
library(readxl)
library(recommenderlab)

#recommender data####
file.list <- list.files(path = "Data/Input Data/System Report", pattern="*.xlsx", full.names = T)

#load the excel files, rename columns, select specific columns - takes about 6 minutes.
system.time(
     sales <- lapply(file.list, read_excel) %>%
          bind_rows(.id = "id") %>%
          dplyr::rename(sales, ShopName = 'Shop Name', OrderItem = 'Order Items',
                        OrderDate = 'Order Date', OrderQuantity = 'Order Quantity',
                        OrderPrice = 'Order Price', shopCode = ...22, Customer = "Customer Id") %>%
          dplyr::select(c("shopCode", "ShopName", "OrderDate", "Customer",
                          "OrderItem", "OrderQuantity", "OrderPrice")))

#load and process product info####

#Product data loading - takes about 3 minutes
system.time(productData <- read.csv("Data/Input Data/allProductData.csv",
                                    stringsAsFactors = F, header = T, comment.char = "",
                                    colClasses = c("product.name"="character","id"="character",
                                                   "product.brand"="character", "product.category"="character",
                                                   "enlisted.shop.code"="character","mrp"="numeric",
                                                   "seller.price"="numeric","e.price"="numeric")) %>%
                 dplyr::rename(OrderItem = 'product.name', shopCode = 'enlisted.shop.code') %>%
                 dplyr::filter(!grepl("express", shopCode, ignore.case = T)))


#merged product sales data
salesWprod <- merge(sales, productData, by=c("OrderItem", "shopCode"),all.x = F, all.y = F) %>%
     select(-c("X", "product.brand"))
salesWprod$OrderQuantity <- as.numeric(salesWprod$OrderQuantity)
salesWprod$seller.price <- as.numeric(salesWprod$seller.price)

write.csv(salesWprod, "Data/Input Data/Product/salesWprod.csv")

#
salesWprod <- read.csv("Data/Input Data/salesWprod.csv",
                       stringsAsFactors = F, header = T, comment.char = "")

customerCat <- ddply(salesWprod, .(Customer, product.category),
                     summarise, catCount = sum(OrderQuantity, na.rm = T),
                     customerBought = sum(catCount, na.rm = T))

#customerFilter <- ddply(customerCat, .(Customer), summarise,
#                       customerBought = sum(catCount, na.rm = T))

catFilter <- ddply(customerCat, .(product.category), summarise,
                   onlyCatCount = sum(catCount, na.rm = T))

customerCat <- merge(merge(customerCat, customerFilter), catFilter) %>%
     filter(customerBought > 200 & onlyCatCount > 100)

write.csv(customerCat, "Data/Input Data/Product/newCustomerCatCount.csv")

#recommender####

ratingData2 <- read.csv("Data/Input Data/Product/newCustomerCatCount.csv",
                        stringsAsFactors = F, header = T, comment.char = "")

names(ratingData2)
#names(ratingData2)
#ratingData <- read.csv("Data/Input Data/Product/customerCategoryMatrix.csv",
#                       stringsAsFactors = F, header = T, comment.char = "")

#select useful data
ratingMatrix <- dcast(ratingData2, Customer~product.category, value.var = "catCount", na.rm = F)
ratingMatrix[is.na(ratingMatrix)] <- 0
ratingMatrix <- as.matrix(ratingMatrix[,-1])
ratingMatrix <- abs(ratingMatrix)

ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

#normalize data
ratingMatrix <- normalize(ratingMatrix)
sum(rowMeans(normalized_ratings) > 0.00001)

#binarize data
binary_minimum_movies <- quantile(rowCounts(ratingMatrix), 0.90)
binary_minimum_users <- quantile(colCounts(ratingMatrix), 0.90)


#collaborative filtering
sampled_data <- sample(x = c(TRUE, FALSE),
                       size = nrow(ratingMatrix),
                       replace = TRUE,
                       prob = c(0.8, 0.2))
training_data <- ratingMatrix[sampled_data, ]
test_data <- ratingMatrix[sampled_data, ]

#recommendation system
recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30)) 
recommen_model
