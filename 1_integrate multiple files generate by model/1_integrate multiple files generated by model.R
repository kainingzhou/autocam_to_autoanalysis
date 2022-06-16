#=== Task ======
# Adam's model could not automatically batch processing multiple image folders.
# Need to work folder by folder, therefore there will be multiple .csv files from different dates.
# This script is to fuse multiple files into one datasheet. It will be easier for later use.

#=== Tutorial ======
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once

#=== Note ======
# This script is assume we directly feed original large size images to the model. Therefore need to consider image order.
# another option is to rename and resize auto images first, then use these images for both Rootfly and Model estimation
# By doing so, no need to deal with the reversed order in the original images. 

# Set work directory
setwd("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/1_integrate multiple files generate by model")

# Load libraries needed
library(data.table) # use fread() function
library(tidyr)
library(dplyr)
library(xlsx) # Write data into excel
library(readxl)

# Get the files names
files = list.files(pattern="*.csv")

# convert all.csv files into one data.frame
all_pred = do.call(rbind, lapply(files, fread))
View(all_pred)

# Add one column named "image order" to overcome the error that in some sessions images are named from 002 instead of 001
x<-rep(c(1:42),times=26) # Use 42 because each folder always contains 42 images, use 26 because there are 26 sessions in total
all_pred$image_order <- x

# Export the dataframe as a .csv file
write.xlsx(all_pred, file = "all_pred.xlsx",
           sheetName = "raw data", row.names = FALSE, append = FALSE)

# First 126 rows do not contain camera number in the image name 
# because the engineer Ofer added camera number to the name string 126 days
# after using automated cameras in the field. In order to keep all rows consistent in format
# need to process first 126 rows and other rows separately

# First 126 rows:
all_pred_sort1 <- all_pred %>% slice(1:126) # Choose these rows
all_pred_sort1 <- separate(data = all_pred_sort1, col = image_name, into = c("code", "date", "time"), sep = "\\_") # Split the column 'image_name' into multiple columns
all_pred_sort1 <- subset(all_pred_sort1, select = -c(time, filename, pred_cls, pred_cls_literal, classes_agree, lengths_agree)) # Delete uncessary columns
View(all_pred_sort1)

# The rest of rows contain cam number in the file name. Split column of "filename" then delete unnecessary columns
all_pred_sort2 <- all_pred %>% slice(127:1092)
all_pred_sort2 <- separate(data = all_pred_sort2, col = image_name, into = c("code","cam","date", "time"), sep = "\\_")
all_pred_sort2 <- subset(all_pred_sort2, select = -c(cam, time, filename, pred_cls, pred_cls_literal, classes_agree, lengths_agree))
View(all_pred_sort2)

# Bind all rows then write as an append into the existing .xlsx file
all_pred_sort3 <- rbind(all_pred_sort1, all_pred_sort2)
View(all_pred_sort3)

# Delete all result from 033-042 images since they do not contain roots then write as an append into the existing .xlsx file
all_pred_sort4 <- subset(all_pred_sort3, !(image_order %in% c(33, 34, 35, 36, 37, 38, 39, 40, 41, 42)))
View(all_pred_sort4)

write.xlsx(all_pred_sort4, file = "all_pred.xlsx",
           sheetName = "images contain roots", row.names = FALSE, append = TRUE)

#=== Merge data with the real window number and DAP ======

# Load data
window_id <- read_excel("cam3_id.xlsx", sheet = "window_id") 
date_id <- read_excel("cam3_id.xlsx", sheet = "date_id") 

# Merge data frames
all_pred_sort5<- merge(all_pred_sort4, window_id, by = c('image_order')) %>% 
  merge(date_id, by=c('date'))
View(all_pred_sort5)

# Get column names
colnames(all_pred_sort5)

# The size of each image taken by auto cam is 1.388889 larger than those taken by manual MR
# Therefore, need to calculate the actually root length
all_pred_sort5$Pred_TRL<- all_pred_sort5$pred_len*1.388889

# Delete uncessary columns
all_pred_sort6 <- all_pred_sort5[, c("Window", "Pred_TRL", "DAP")] # Alert: Window need to start with capital 'W' inorder to consistent with Rootfly column name
View(all_pred_sort6)

# Write data into excel
write.xlsx(all_pred_sort6, file = "all_pred.xlsx",
           sheetName = "sorted data with id", row.names = FALSE, append = TRUE)

