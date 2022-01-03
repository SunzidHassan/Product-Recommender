library(readxl)
library(googlesheets4)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)


#Part 1: IMPORT####


##Product data####
prodFiles <- list.files("Data/Input Data/ProductData/New Product Data", pattern="*.csv", full.names = T)

productData <- lapply(prodFiles, fread, sep = "\t", header = TRUE, data.table = TRUE) %>%
                    bind_rows(.id = "id") %>%
                    select(-c("id", "product id", "product specifications",
                              'enlisted shop code', "evaly price")) %>%
                    rename(OrderItem = 'order item', ShopName = 'shop name', Brand = 'brand name',
                           unit.seller.price = 'seller price', unit.mrp = 'mrp', category = 'category name')

#the 3 operation takes about 21 seconds in i9
productData$OrderItem <- str_squish(productData$OrderItem)
productData$OrderItem <- tolower(productData$OrderItem)
productData$ShopName <- str_squish(productData$ShopName)
productData$ShopName <- tolower(productData$ShopName)

productData <- filter(productData, productData$unit.mrp > 0)
productData <- filter(productData, productData$unit.seller.price > 0)
productData <- filter(productData, productData$unit.mrp > productData$unit.seller.price)

##System data####

file.list <- list.files(path = "Data/Input Data/SystemReport/System Report Till May", pattern="*.xlsx", full.names = T)

#load the excel files, rename the last column as shopCode. Takes about 5 minutes for data since November.
sales <- lapply(file.list, read_excel) %>%
                    bind_rows(.id = "id")

rm(file.list, prodFiles)

# add month column, remove comma from last update time and add last update date column,

sales <-  mutate(sales, orderMonth = month(mdy(sales$`Order Date`)),
                                               orderWeek = week(mdy(sales$`Order Date`))) %>%
                    dplyr::select(c("Shop Name", "Order Items", "Order Quantity",
                                    "Order Price","orderMonth", "orderWeek")) %>%
                    dplyr::rename(ShopName = 'Shop Name', OrderItem = 'Order Items',
                                  OrderQuantity = 'Order Quantity', unit.OrderPrice = 'Order Price')

sales$unit.OrderPrice <- as.numeric(sales$unit.OrderPrice)
sales$OrderQuantity <- as.numeric(sales$OrderQuantity)

sales$OrderItem <- gsub(pattern= "\r|\n.*" , "", sales$OrderItem)
sales$OrderItem <- str_squish(sales$OrderItem)
sales$OrderItem <- tolower(sales$OrderItem)
sales$ShopName <- tolower(sales$ShopName)
sales$ShopName <- str_squish(sales$ShopName)

rm(prodFiles, file.list)


#total quantity sold against items
sales <- data.table(sales)
total <- sales[, .(Total.Quantity = sum(OrderQuantity, na.rm = T)), by = .(ShopName, OrderItem, orderMonth, orderWeek)]


unqItem <- sales %>% group_by(ShopName, OrderItem) %>%
                    mutate(itemCount = row_number()) %>%
                    filter(itemCount == 1) %>%
                    select(c("ShopName", "OrderItem", "unit.OrderPrice"))

data <- merge(merge(unqItem, productData, all = F, by = c("ShopName", "OrderItem")),
              total, all.x = T, all.y = F, by = c("ShopName", "OrderItem")) %>%
                    mutate(discount = (unit.mrp-unit.OrderPrice)/unit.mrp)

data <- dplyr::filter(data, data$unit.OrderPrice < data$unit.mrp)


#dictionary
item <- data[!duplicated(data$OrderItem),]
item <- item[order(item$OrderItem), ] %>%
                    select(c("OrderItem")) %>%
                    mutate(itemSerial = row_number())
min <- min(item$itemSerial)
max <- max(item$itemSerial)

item <- mutate(item, normalizedItemSerial = (max - itemSerial)/(max - min))


brand <- data[!duplicated(data$Brand),]
brand <- brand[order(brand$Brand), ] %>%
                    select(c("Brand")) %>%
                    mutate(brandSerial = row_number())
min <- min(brand$brandSerial)
max <- max(brand$brandSerial)

brand <- mutate(brand, normalizeBranddSerial = (max - brandSerial)/(max - min))

category <- data[!duplicated(data$category), ]
category <- category[order(category$category), ] %>%
                    select(c("category")) %>%
                    mutate(categorySerial = row_number())
min <- min(category$categorySerial)
max <- max(category$categorySerial)
category <- mutate(category, normalizedCategorySerial = (max - categorySerial)/(max - min))


discount <- data[!duplicated(data$discount), ]
discount <- discount[order(discount$discount), ] %>%
                    dplyr::filter(discount < .8) %>%
                    select(c("discount")) %>%
                    mutate(discountSerial = row_number())
min <- min(discount$discountSerial)
max <- max(discount$discountSerial)
discount <- mutate(discount, normalizedDiscountSerial = (max - discountSerial)/(max - min))

orderPrice <- data[!duplicated(data$unit.OrderPrice), ]
orderPrice <- orderPrice[order(orderPrice$unit.OrderPrice), ] %>%
                    select(c("unit.OrderPrice")) %>%
                    mutate(orderPriceSerial = row_number())
min <- min(orderPrice$orderPriceSerial)
max <- max(orderPrice$orderPriceSerial)
orderPrice <- mutate(orderPrice, normalizedOrderPriceSerial = (max - orderPriceSerial)/(max - min))

rm(min, max)

dictData <- merge(merge(merge(merge(merge(data, item, all.x = T, all.y = F),
                                brand, all.x = T, all.y = F),
                          category, all.x = T, all.y = F),
                    discount, all.x = T, all.y = F),
              orderPrice, all.x = T, all.y = F) %>%
                    dplyr::filter(discount < 0.8)


brandCor <- cor(dictData$normalizeBranddSerial, dictData$normalizedDiscountSerial)
categoryCor <- cor(dictData$normalizedCategorySerial, dictData$normalizedDiscountSerial)
discountCor <- cor(dictData$normalizedDiscountSerial, dictData$normalizedDiscountSerial)
itemCor <- cor(dictData$normalizedItemSerial, dictData$normalizedDiscountSerial)
orderPriceCor <- cor(dictData$normalizedOrderPriceSerial, dictData$normalizedDiscountSerial)

#
# 1. Prepare data: - Independent: item, brand, category, discount, price, avg. delivery time. Dependent: quantity sold.
# 2. Prepare dictionary for all the attributes, normalize the values.
# 3. Determine correlation coefficient for the following values.


