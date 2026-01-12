### Function to read all Aeris files from a specified folder #################
Read_Data<- function(path){    #Open each Dat file in a list 
  
  listofdfs <- list() 
  listofdfs_Pic <- list() 
  
  list_of_files_All <- list.files(path = path,pattern = "*.txt",
                                  recursive=TRUE, full.names = TRUE) ##Name of all Aeris files
  
  list_of_files <- list_of_files_All[!grepl("Eng|spectralite", list_of_files_All)] #Not needed
  
  L<-length(list_of_files)

  list_of_files_Pic <- list.files(path = path,pattern = "*.dat",
                              recursive=TRUE) ##Name of all Picarro files
  L_P<-length(list_of_files_Pic)

  if (L_P == 0) {
  for(i in 1:L){ #Loop through the numbers of ID's instead of the ID's
    
    Aeris_Data <- read.csv(list_of_files[i], 
                           header = TRUE)
    listofdfs[[i]] <- Aeris_Data # save your dataframes into the list
    print(list_of_files[i])
  }
    Data<-bind_rows(listofdfs, .id = "column_label") ## Binds lists into a single dataframe
    
    return(Data) #Return the list of dataframes.
    
  }else{
  
    
for(i in 1:L_P){ #Loop through the numbers of ID's instead of the ID's
  
  Pic_Data <- read.table(paste0(path_NH3,"/", list_of_files_Pic[i]), 
                         header = TRUE)
  listofdfs_Pic[[i]] <- Pic_Data # save your dataframes into the list
  print(list_of_files_Pic[i])
  
}
  }
  Data<-bind_rows(listofdfs_Pic, .id = "column_label") ## Binds lists into a single dataframe
  
  return(Data) #Return the list of dataframes.
}
################################################################################