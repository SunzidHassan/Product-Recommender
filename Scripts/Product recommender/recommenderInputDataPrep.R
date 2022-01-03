#####
#Libraries
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(tidyr)

#####
###output sales data with product info
## Unique product ID, name, brand, category
#Load product data
productData <- read.csv("Data/Output/allProductData.csv",
                        stringsAsFactors = F, header = T, comment.char = "",
                        colClasses = c("product.name"="character","id"="character",
                                       "product.brand"="character", "product.category"="character",
                                       "enlisted.shop.code"="NULL","mrp"="NULL",
                                       "seller.price"="NULL","evaly.price"="NULL"))

#unique product name, ID, brand, category
productNameID <- data.frame(productData[!duplicated(productData$id), ]) #keeping first product ID

#export data
#write.csv(mergedProductID, "Data/Output/productNameID.csv")

#unique products in sales data
soldProduct <- read.csv("Data/Output/Merged20_21SalesData210320.csv",
                        stringsAsFactors = F, header = T, comment.char = "",
                        colClasses = c("Invoice.No" = "NULL", "Order.Date" = "NULL", "Customer.Id" = "NULL", "Order.Status" = "NULL",
                                       "Total.Price" = "NULL", "Order.Quantity" = "NULL", "Order.Price" = "NULL", "Shop.Code" = "NULL",
                                       "LastUpdatedate" = "NULL", "Customer.Address"= "NULL", "Order.Time" = "NULL", "Shop.Number" = "NULL",
                                       "Payment.Method" = "NULL", "Last.Update" = "NULL", "Last.Update.Time" = "NULL"))

#unique sold product
unqSoldProduct <- data.frame(soldProduct[!duplicated(soldProduct$Order.Items), ])


#table of sales data order.item and product id
mergedProductID <- merge(unqSoldProduct,productNameID, by.x="Order.Items",by.y="product.name",all=FALSE)
mergedProductID <- select(mergedProductID, -2, -3)
head(mergedProductID)


#loading sales data: invoice, customerID, shop name, purchase date, order status, product name, price, units, total paid, last update time
salesData <- read.csv("Data/Output/Merged20_21SalesData210320.csv",
                      stringsAsFactors = F, header = T, comment.char = "",
                      colClasses = c("Customer.Address"= "NULL", "Order.Time" = "NULL", "Shop.Number" = "NULL",
                                     "Payment.Method" = "NULL", "Last.Update" = "NULL", "Last.Update.Time" = "NULL"))


#merge ID in overall sales data
mergedSalesID <- merge(salesData, mergedProductID, all = FALSE)
mergedSalesID$Order.Quantity <- as.numeric(mergedSalesID$Order.Quantity)
mergedSalesID$Order.Price <- as.numeric(mergedSalesID$Order.Price)

write.csv(mergedSalesID, "Data/Output/salesDataWProductID.csv")


#####
#import sales data with product info for further analysis 
salesDataWProductID <- read.csv("Data/Output/salesDataWProductID210320.csv", stringsAsFactors = F, header = T, comment.char = "")

#####
#Export 1: all prouct IDs against each customer
productsByCustomer <- salesDataWProductID %>% group_by(Customer.Id) %>% summarise(products = toString(id))

write.csv(productsByCustomer, "Data/Output/productByCustomer.csv")

#####
#Export 2: matrix with selected high monetary value products
#melt data
salesDataWProductID <- melt(salesDataWProductID, id=c("Order.Items", "X", "Invoice.No", "Order.Date", "Customer.Id",
                                                      "Order.Status", "Shop.Code", "LastUpdatedate", "id", "product.brand", "product.category"),
                            measure.vars = c("Total.Price", "Order.Quantity", "Order.Price"))


#sum of price of each item
totalOrderPrice <- dcast(salesDataWProductID, id ~ variable, sum)
totalOrderPrice <- select(totalOrderPrice, -2, -3)
totalOrderPrice <- rename(totalOrderPrice, totalProductSales = Order.Price)


#sum of price of each item
totalCustomerOrder <- dcast(salesDataWProductID, Customer.Id ~ variable, sum)
totalCustomerOrder <- select(totalCustomerOrder, -2, -3)
totalCustomerOrder <- rename(totalCustomerOrder, totalCustomerSales = Order.Price)

#merge total order price

salesDataIDcustomerProductSales <- merge(merge(salesDataWProductID, totalCustomerOrder, all = FALSE),
                                         totalOrderPrice, all = FALSE)

write.csv(salesDataIDcustomerProductSales, "Data/Output/salesDataIDcustomerProductSales.csv")


#####
##Filter aginst total product and customer sales
#read data
salesDataIDcustomerProductSales <- read.csv("Data/Output/salesDataIDcustomerProductSales.csv", stringsAsFactors = F, header = T, comment.char = "")
names(salesDataIDcustomerProductSales)

filteredSalesData <- filter(salesDataIDcustomerProductSales, totalCustomerSales > 200000 & totalProductSales > 100000) #results in 53387 customer * 15880 product

#####
#Export 3: make a matrix with high sales price products
customerProductMatrix <- table(filteredSalesData$Customer.Id, filteredSalesData$id)
dim(customerProductMatrix)
write.csv(customerProductMatrix, "Data/Output/customerProductMatrix.csv")

#####
#category recommender
#Load Libraries

#import sales data with product info for further analysis 
salesDataWProductID <- read.csv("Data/Output/salesDataWProductID210320.csv", stringsAsFactors = F, header = T, comment.char = "")
names(salesDataWProductID)
#####
#Export 4: all prouct IDs against each customer
categoryByCustomer <- salesDataWProductID %>% group_by(Customer.Id) %>% summarise(products = toString(product.category))

write.csv(categoryByCustomer, "Data/Output/categoryByCustomer.csv")

#####
#Export 5: matrix with selected high monetary value products
#melt data
salesDataWProductID <- melt(salesDataWProductID, id=c("Order.Items", "X", "Invoice.No", "Order.Date", "Customer.Id",
                                                      "Order.Status", "Shop.Code", "LastUpdatedate", "id", "product.brand", "product.category"),
                            measure.vars = c("Total.Price", "Order.Quantity", "Order.Price"))


#sum of price of each item
totalOrderPrice <- dcast(salesDataWProductID, product.category ~ variable, sum)
totalOrderPrice <- select(totalOrderPrice, -2, -3)
totalOrderPrice <- rename(totalOrderPrice, totalCategorySales = Order.Price)


#sum of price of each item
totalCustomerOrder <- dcast(salesDataWProductID, Customer.Id ~ variable, sum)
totalCustomerOrder <- select(totalCustomerOrder, -2, -3)
totalCustomerOrder <- rename(totalCustomerOrder, totalCustomerSales = Order.Price)

#merge total order price

salesDataIDcustomerCategorySales <- merge(merge(salesDataWProductID, totalCustomerOrder, all = FALSE),
                                          totalOrderPrice, all = FALSE)

write.csv(salesDataIDcustomerCategorySales, "Data/Output/salesDataIDcustomerCategorySales.csv")


#####
##Filter against total product and customer sales
#read data
salesDataIDcustomerCategorySales <- read.csv("Data/Output/salesDataIDcustomerCategorySales.csv", stringsAsFactors = F, header = T, comment.char = "")
names(salesDataIDcustomerCategorySales)

filteredSalesData <- filter(salesDataIDcustomerCategorySales, totalCustomerSales > 100000 & totalCategorySales > 10000) #results in 87115 customer * 1691 category

#####
#Export 6: make a matrix with high sales price products
customerCategoryMatrix <- table(filteredSalesData$Customer.Id, filteredSalesData$product.category)
dim(customerCategoryMatrix)
write.csv(customerCategoryMatrix, "Data/Output/customerCategoryMatrix.csv")

