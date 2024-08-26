
suppressPackageStartupMessages({
  library("zoo")
  # library("mcp") # for bayesian changepoint analysis
  # library("rjags")
  library("pracma")
  library("scales")
  library("dplyr")
  library(ggplot2)
  library(hexbin)
  library(ggExtra)
  library("optparse")
  library("optimx")
  library("stringr")
})

start_time <- Sys.time()

option_list = list(
  make_option(c("-f", "--filepath"), type="character", default=NA, 
              help="Specify the path where your subgroup data is saved in string format e.g. \"/home/user/data/\"",
              metavar="character"),
  make_option(c("-s", "--selection"), type="character", default="all",
              help="Specify which dataset to run. Should be any of [all, male, female, premenop, postmenop]. By default you run on all datasets"
  )
); 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
if(is.na(opt$filepath)){
  stop("Please specify the path where your subgroup data is saved in string format e.g. \"/home/user/data/\"")
} else {
  subgroupdatasets <- list.files(paste0(opt$filepath), full.names = T)
}

selection <- opt$selection

if (! (selection %in% c("all", "male", "female", "premenop", "postmenop"))){
  stop("selection should be any of [all, male, female, premenop, postmenop]")
} else {
  print(paste("Selecting", selection))
}



# main_dir <- "~/code/Associatie_FerHb" #define your main directory 
# FileToUse<-"~/code/data/dataset7.rds" #define the dataset file you want to use for the analysis (1 of 6 datasets we agreed on)
export_folder <- "results_export" #define the name of the folder you want to export you results to

main_dir <- getwd()
source(file.path(main_dir, "1_Functions.R"))
# setwd(main_dir)  
# setting up the sub directory for bootstrap results and a folder with today's date
currentDate <- "2023-11-30"#Sys.Date()

#check if the folder for result export already exists, otherwise create it
if (file.exists(export_folder)){
  print("The folder for the result export already exists")
} else {
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, export_folder))
}


if (file.exists(file.path(export_folder, currentDate))){
  print("Today's sub folder already exists")
} else {
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, export_folder,currentDate))
}

# do the subgroup analyses:
for (i in 1:length(subgroupdatasets)) {
  FileToUse <- subgroupdatasets[i]
  print(paste0("Starting analysis using file", FileToUse))
  FolderDataset <- str_split_i(FileToUse, "/", -1)
  FolderDataset <- gsub('.rds', '', FolderDataset)
  
  if (file.exists(file.path(export_folder, currentDate, FolderDataset))){
    print("A sub folder with this dataset name already exists")
  } else {
    # create a new sub directory inside
    # the main path
    dir.create(file.path(main_dir, export_folder,currentDate, FolderDataset))
  }
  
  data<-readRDS(FileToUse)
  if(!is.factor(data$Geslacht)){data$Geslacht<-as.factor(data$Geslacht)}
  if(exists("export_data")){rm(export_data)}
  if(exists("export_data2")){rm(export_data2)}
  if(exists("rollmean_export")){rm(rollmean_export)}
  if(exists("rollmean_export2")){rm(rollmean_export2)}
  if(exists("sol_export")){rm(sol_export)}
  if(exists("sol_export2")){rm(sol_export2)}
  bsseed<-11          # seedvalue for the bootstrap
  
  rollmean_nr<-1000  # nr of samples to use in the rolling mean
  geompoint_transparency <- 0.05  #adjust this based on what looks good in your plots. For the Dutch, big, data, 0.05 is appropriate, but for smaller datasets a bigger number may be better
  
  #exporting results
  export_files <- T # Indicate whether you want to export the results of the analysis
  proportion <- 0.03 # proportion of your data that you will export. This really depends on how big your data is. We recommend a proportion that will still display the pattern/direction of the data.
  
  #second slope is set to constant by default
  #std is set to constant by default, by this function
  #which also sets p0 and ul, ll
  set_constant_slope_std(T, T)
  
  fit_and_plot <- function(refdata) {
    fitdata<<-refdata
    return (plotHbFer())
  }
  
  
  
  constant_vals<-c(TRUE,FALSE)
  
  
  ##only do this once
  refdataM <- select_males_only()
  refdataF <- select_females_only()
  refdatapostF <- select_postmenopausal_only()
  refdatapreF <- select_premenopausal_only()
  if(export_files){
    print("Exporting files")
    title=paste0("Male donors (n=", nrow(refdataM),")", sep="")
    exportData(refdataM)
    title=paste0("Female donors (n=", nrow(refdataF),")", sep="")
    exportData(refdataF)
    title=paste0("Premenopausal donors (n=", nrow(refdatapreF),")", sep="")
    exportData(refdatapreF)
    title=paste0("Postmenopausal donors (n=", nrow(refdatapostF),")", sep="")
    exportData(refdatapostF)
  }
  
  for(constant_slope_val in constant_vals){
    for(constant_std_val in constant_vals){
      set_constant_slope_std(constant_slope_val, constant_std_val)
      print("-------------")
      print(paste("Constant slope", constant_slope, "Constant std", constant_std))
      print("--------")
      
      if (selection == "male" || selection == "all") {
        print("male")
        print("-----")
        title=paste0("Male donors (n=", nrow(refdataM),")", sep="")
        pfit_male <- fit_and_plot(refdataM)
        if(constant_slope && constant_std){
          pfit_male_constant_slope_constant_std <- pfit_male
        }else if (constant_slope && ! constant_std){
          pfit_male_constant_slope_fit_std <- pfit_male
        } else if (!constant_slope && constant_std){
          pfit_male_fit_slope_constant_std <- pfit_male
        } else if (! constant_slope && ! constant_std){
          pfit_male_fit_slope_fit_std <- pfit_male
        }
        if(export_files){
          print("Exporting files")
          exportRollmean(rollmean_data)
          exportSolution(sol_data)
        }
      }
      if (selection == "female" || selection == "all") {
        print("female")
        print("-----")
        title=paste0("Female donors (n=", nrow(refdataF),")", sep="")
        pfit_female <- fit_and_plot(refdataF)
        if(constant_slope && constant_std){
          pfit_female_constant_slope_constant_std <- pfit_female
        }else if (constant_slope && ! constant_std){
          pfit_female_constant_slope_fit_std <- pfit_female
        } else if (!constant_slope && constant_std){
          pfit_female_fit_slope_constant_std <- pfit_female
        } else if (! constant_slope && ! constant_std){
          pfit_female_fit_slope_fit_std <- pfit_female
        }
        if(export_files){
          print("Exporting files")
          exportRollmean(rollmean_data)
          exportSolution(sol_data)
        }
      }
      if (selection == "premenop" || selection == "all") {
        print("premenop")
        print("-----")
        title=paste0("Premenopausal donors (n=", nrow(refdatapreF),")", sep="")
        pfit_premenop <- fit_and_plot(refdatapreF)
        if(constant_slope && constant_std){
          pfit_premenop_constant_slope_constant_std <- pfit_premenop
        }else if (constant_slope && ! constant_std){
          pfit_premenop_constant_slope_fit_std <- pfit_premenop
        } else if (!constant_slope && constant_std){
          pfit_premenop_fit_slope_constant_std <- pfit_premenop
        } else if (! constant_slope && ! constant_std){
          pfit_premenop_fit_slope_fit_std <- pfit_premenop
        }
        if(export_files){
          print("Exporting files")
          exportRollmean(rollmean_data)
          exportSolution(sol_data)
        }
      }
      if (selection == "postmenop" || selection == "all") {
        print("postmenop")
        print("-------")
        title=paste0("Postmenopausal donors (n=", nrow(refdatapostF),")", sep="")
        pfit_postmenop <- fit_and_plot(refdatapostF)
        if(constant_slope && constant_std){
          pfit_postmenop_constant_slope_constant_std <- pfit_postmenop
        }else if (constant_slope && ! constant_std){
          pfit_postmenop_constant_slope_fit_std <- pfit_postmenop
        } else if (!constant_slope && constant_std){
          pfit_postmenop_fit_slope_constant_std <- pfit_postmenop
        } else if (! constant_slope && ! constant_std){
          pfit_postmenop_fit_slope_fit_std <- pfit_postmenop
        }
        if(export_files){
          print("Exporting files")
          exportRollmean(rollmean_data)
          exportSolution(sol_data)
        }
      }
    }
  }
  print("...done with fit")
}


####

end_time <- Sys.time()

print("Time it took:")
print(end_time-start_time)
print("...done with everything closing")