
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
# library("optimx")
library("stringr")
})

start_time <- Sys.time()
 
option_list = list(
    make_option(c("-f", "--file"), type="character", default=NA, 
     help="Specify datafile in .rds format e.g. /home/user/data/dataset2.rds",
     metavar="character"),
    make_option(c("-b", "--bootstraps"), type="integer", default=0,
        help="number of bootstrap samples to run, default=0 is no bootstraps"),
    make_option(c("-s", "--selection"), type="character", default="all",
    help="Specify which dataset to run. Should be any of [all, male, female, premenop, postmenop]. By default you run on all datasets"
    )
); 
 
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
if(is.na(opt$file)){
    stop("Please specify a datafile like so Rscript Analysis_code.R -f dataset.rds")
} else {
    FileToUse <- opt$file
}

bootstrapsamples <- opt$bootstraps

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
sub_dir <- "bootstrap_results/"
currentDate <- Sys.Date()

FolderDataset <- str_split_i(FileToUse, "/", -1)
FolderDataset <- gsub(".rds", "", FolderDataset)

printbootstrap<-T  # Indicate whether you want to print the bootstrap results. For readable quarto files this is not recommended (recommended: F) 
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

# check if sub directory already exists, otherwise create it 
if (file.exists(sub_dir)){
      print("The folder for bootstrap results already exists")
} else {
        # create a new sub directory inside
        # the main path
        dir.create(file.path(main_dir, sub_dir))
}

#check if the folder for today's bootstrap results already exists, otherwise create it
if (file.exists(file.path(sub_dir,FolderDataset))){
      print("The dataset name's sub folder already exists")
} else {
        # create a new sub directory inside
        # the main path
        dir.create(file.path(main_dir, sub_dir,FolderDataset))
}

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


if (file.exists(file.path(export_folder, currentDate, FolderDataset))){
      print("A sub folder with this time stamp already exists")
} else {
        # create a new sub directory inside
        # the main path
        dir.create(file.path(main_dir, export_folder,currentDate, FolderDataset))
}

data<-readRDS(FileToUse)
if(!is.factor(data$Geslacht)){data$Geslacht<-as.factor(data$Geslacht)}





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
    # constant_std_val <- T
    # STD_CONSTANT <<- 1
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
####
### Bootstraps
###
if (bootstrapsamples > 0) {
    print("--------------")


    if (selection == "male" || selection == "all") {
        MaleBootstrapResultFile<-paste0("bootstrap_results/", FolderDataset, "/Male_Bootstrap_",bootstrapsamples,"_samp_",bsseed, "seed.RDS")
    }
    if (selection == "female" || selection == "all") {
        FemaleBootstrapResultFile<-paste0("bootstrap_results/", FolderDataset, "/Female_Bootstrap_",bootstrapsamples,"_samp_", bsseed, "seed.RDS")
    }
    if (selection == "premenop" || selection == "all") {
        PremenopausalBootstrapResultFile<-paste0("bootstrap_results/", FolderDataset, "/Premenopausal_Bootstrap_",bootstrapsamples,"_samp_", bsseed, "seed.RDS")
    }
    if (selection == "postmenop" || selection == "all") {
        PostmenopausalBootstrapResultFile<-paste0("bootstrap_results/", FolderDataset, "/Postmenopausal_Bootstrap_",bootstrapsamples,"_samp_", bsseed, "seed.RDS")
    }


    set.seed(bsseed)
    print(paste("Doing", bootstrapsamples, "bootstraps"))
    print(paste("seed =",bsseed))
    

    if (selection == "male" || selection == "all") {
        print("male")
        print("-----------")
        refdataM<-select_males_only()
        # STD_CONSTANT <<- tail(pfit_male_constant_slope_fit_std, n=1)
        bsxm <- compute_bss(refdataM, pfit_male_constant_slope_constant_std,
        pfit_male_fit_slope_constant_std, bootstrapsamples)
        saveRDS(bsxm, file=MaleBootstrapResultFile)
    }
    if (selection == "female" || selection == "all") {
        print("female")
        print("-----------")
        refdataF<-select_females_only()
        # STD_CONSTANT <<- tail(pfit_female_constant_slope_fit_std, n=1)
        bsxm <- compute_bss(refdataF, pfit_female_constant_slope_constant_std,
        pfit_female_fit_slope_constant_std, bootstrapsamples)
        saveRDS(bsxm, file=FemaleBootstrapResultFile)
    }
    if (selection == "premenop" || selection == "all") {
        print("premenop")
        print("-----------")
        refdatapreF<-select_premenopausal_only()
        # STD_CONSTANT <<- tail(pfit_premenop_constant_slope_fit_std, n=1)
        bsxm <- compute_bss(refdatapreF, pfit_premenop_constant_slope_constant_std,
        pfit_premenop_fit_slope_constant_std, bootstrapsamples)
        saveRDS(bsxm, file=PremenopausalBootstrapResultFile)
    }
    if (selection == "postmenop" || selection == "all") {
        print("postmenop")
        print("-----------")
        refdatapostF<-select_postmenopausal_only()
        # STD_CONSTANT <<- tail(pfit_postmenop_constant_slope_fit_std, n=1)
        bsxm <- compute_bss(refdatapostF, pfit_postmenop_constant_slope_constant_std,
        pfit_postmenop_fit_slope_constant_std, bootstrapsamples)
        saveRDS(bsxm, file=PostmenopausalBootstrapResultFile)
    }

}

if(bootstrapsamples>0){
  print("Plotting bootstrap results")
  if (selection == "male" || selection == "all") {
if(file.exists(MaleBootstrapResultFile)){
  
  # Read bootstrap results
  title=paste0("Male donors (n=", nrow(refdataM),")", sep="")
  bsxm<-readRDS(MaleBootstrapResultFile)
  plot_bootstraps(refdataM, bsxm, pfit_male_constant_slope_constant_std, pfit_male_fit_slope_constant_std)
}
}
  if (selection == "female" || selection == "all") {
if(file.exists(FemaleBootstrapResultFile)){
  
  # Read bootstrap results
  title=paste0("Female donors (n=", nrow(refdataF),")", sep="")
  bsxf<-readRDS(FemaleBootstrapResultFile)
  plot_bootstraps(refdataF, bsxf, pfit_female_constant_slope_constant_std, pfit_female_fit_slope_constant_std)
}
}
  if (selection == "premenop" || selection == "all") {
if(file.exists(PremenopausalBootstrapResultFile)){
    
    # Read bootstrap results
    title=paste0("Premenopausal donors (n=", nrow(refdatapreF),")", sep="")
    bsxpref<-readRDS(PremenopausalBootstrapResultFile)
    plot_bootstraps(refdatapreF, bsxpref, pfit_premenop_constant_slope_constant_std, pfit_premenop_fit_slope_constant_std)
}
}
  if (selection == "postmenop" || selection == "all") {
  if(file.exists(PostmenopausalBootstrapResultFile)){
    
    # Read bootstrap results
    title=paste0("Postmenopausal donors (n=", nrow(refdatapostF),")", sep="")
    bsxpostf<-readRDS(PostmenopausalBootstrapResultFile)
    plot_bootstraps(refdatapostF, bsxpostf, pfit_postmenop_constant_slope_constant_std, pfit_postmenop_fit_slope_constant_std)
  }
}}


end_time <- Sys.time()

print("Time it took:")
print(end_time-start_time)
print("...done with everything closing")
