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
      if( !require( ggplot2 ) ) install.packages("ggplot2")    
    
  #Load libraries
    LoadCommonPackages()
    library(foreign)
    library(ggplot2)
    
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
    
### 2 - EXERCISES  ----------------------------------------------------------------------
  
 # Happiness Example
   lm(
      formula = happiness ~ age,
      data = happiness.tb
    )
     
 # Exercise 1: Card data
    #unstandardized coefficients
      summary(card.tb) #data summary
      
      card.lm <- #regression model object
        lm(
          formula = wage ~ educ + exper + IQ,
          data = card.tb
        )
      
      summary(card.lm) #regression table output
    
    #standardized coefficients
      card.standardized.lm <- #regression model object
        lm(
          formula = wage ~ educ + exper + IQ,
          data = card.tb %>% scale(.) %>% as_tibble()
        )
      summary(card.standardized.lm)  
      
    #Why has sample size changed?
      apply( # shows 949 NA values for "IQ" variable.
        card.tb[,names(card.tb) %in% c("wage","educ","exper","IQ")],
        2,
        function(x){length(which(is.na(x)))}
      )
      nrow(card.tb) - length(which(is.na(card.tb$IQ))) # shows 2061 viable data rows.
  
  #Exercise 2: NELS Data - Dummy Variables
    
    #Basic Bivariate Regression
      summary(nels.tb)
      
      nels.lm <- #regression model object
        lm(
          formula = bygrades ~ female,
          data = nels.tb
        )
      
      summary(nels.lm) #regression table output
      
    #NELS Multipel Regression
       nels.mult.lm <- #regression model object
        lm(
          formula = bygrades ~ female + asian + latino + black + firstgen + byfaminc,
          data = nels.tb
        )
      
      summary(nels.mult.lm) #regression table output
    
  # Scatter Plot with Regression Line
    dat <- nels.tb
    
    scatter.plot <- 
      ggplot(
        data = nels.tb,
        aes(x = byfaminc, y = bygrades)
      ) +
      geom_point(
        size = 2,
        shape = 23,
        color = "red",
        alpha = 0.5
      ) +
      geom_smooth(
        method = lm,
        se = TRUE
      ) +
      geom_rug() 
    scatter.plot
    
  # Exercise 3: Card data mutliple regression: Do men whose fathers have college 
    #degrees earn more than those who did not finish high school?
      
      card.father.lm <- #regression model object
        lm(
          formula = wage ~ educ + exper + IQ + f_hs + f_somecoll + f_coll,
          data = card.tb
        )
      
      summary(card.father.lm) #regression table output
      
    sd(card.tb$wage)
    
    
    
    
    
    