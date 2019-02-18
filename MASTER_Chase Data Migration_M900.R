###################################################################
##### 	MULTIPLE REGRESSION REFRESHER COURSE - PERCONTOR   	  #####
###################################################################

### 0-INITIAL SETUP  ----------------------------------------------------------------------

  rm(list=ls()) #Remove lists
  options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package
  
  
  #Code Clocking
    section0.starttime <- Sys.time()
  
  #ESTABLISH DIRECTORIES 

    #M900
      #working.dir <- "X:/Dropbox/Household_encrypt/Financial/2019-02 Personal Accounting/"
      rproj.dir <- "C:/Users/willi/Documents/GIT PROJECTS/mult_regression_referesher_course/"
    
    #Thinkpad T470
      #working.dir <- "X:/Google Drive File Stream/My Drive/0. FLUX - MASTER & ADMIN/1. Flux Financial/2018-11 Flux Financial System Development/"
      #rproj.dir <- "C:/Users/WNF/Documents/GIT PROJECTS/2018-11-Flux-Financial-System"
    
    #Source Code Directory
      source.code.dir <- rproj.dir
    
    #Source Resources Director (raw data)
      source.data.dir <- rproj.dir
    
    #Outputs Directory
      outputs.dir <- 
        paste(
          working.dir,
          "4_outputs/",
          #"Output_",
          gsub(":",".",Sys.time()), 
          sep = ""
        )
        
  #LOAD SOURCE CODE
    setwd(rproj.dir)
    source("utils_wnf.r")
    
  #In case working on new R install that does not have packages installed
    #install.common.packages()
     if( !require( foreign ) ) install.packages("foreign")
    
  #Load libraries
    LoadCommonPackages()
    library(foreign)
    
  #Section Clocking
    section0.duration <- Sys.time() - section0.starttime
    section0.duration
    
#### 0-INITIAL SETUP OUTPUTS  ----------------------------------
  #working.dir: working directory - Google Drive folder "2018-08 Green Reports"
  #source.code.dir: directory for R project and all code files
  #source.inputs.dir: directory with config tables and powerpoint template
  #outputs.dir: where outputs will be stored


### 1-IMPORT  ----------------------------------------------------------------------

  #Import Setup
    setwd(source.data.dir)
    
  #Past Transactions Table
    happiness.tb <- 
      read.spss(
        file = "happiness.sav",
        to.data.frame = TRUE,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    
    card.tb <- 
      read.spss(
        file = "card.sav",
        to.data.frame = TRUE,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    
     nels.tb <- 
      read.spss(
        file = "nels.sav",
        to.data.frame = TRUE,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    
    
#### 1-IMPORT OUTPUTS ----------------------------------
  #happiness.tb
  #card.tb
  #nels.tb
    
### 1-IMPORT  ----------------------------------------------------------------------
  
 # Happiness Example
   lm(
      formula = happiness ~ age,
      data = happiness.tb
    )
     
 # Exercise 1: Card data
    summary(card.tb)
    card.lm <-
      lm(
        formula = wage ~ educ + exper + IQ,
        data = card.tb
      )
    summary(card.lm)
    
    
    
    
    
    
    
    
    
    
    
    
    