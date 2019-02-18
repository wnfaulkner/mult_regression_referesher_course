#############################################################
#####       STANDARD UTIL FUNCTIONS FOR R SCRIPTS       #####
#############################################################



#FUNCTIONS FOR CODE CLOCKING
  #TODO: MAKE UTILS FUNCTIONS THAT 
    #(A) DESIGNATE SYS.TIME() 'BOOKMARKS' OR 'WAYPOINTS'
    #(B) PRINT A TABLE OF WAYPOINTS IN ROWS AND COLUMNS REPRESENTING
      #(1) TIME SINCE MOST RECENT WAYPOINT
      #(2) CUMULATIVE TIME SINCE FIRST WAYPOINT
      #(3) TIME SINCE MOST RECENT WAYPOINT/TIME SINCE FIRST WAYPOINT (PERCENTAGE)

#FUNCTIONS FOR ERROR HANDLING --------------------
  #Return TRUE/FALSE if expression throws an error
    IsError <- function(.expr){
      result <-
        ifelse(
          tryCatch(
            .expr, 
            error = function(x) {return(TRUE)}
          ) == TRUE,
          TRUE,
          FALSE
        )
      return(result)
    }
    
  #Print standard loop messages
    PrintLoopMessages <- function(loop.index){ #loop.index = object which defines loop number (e.g. 'i' in most common cases)
      print(
        paste0(
          "LOOP '",loop.index, "' -- Loop num: ", eval(parse(text = loop.index))
        )
      )
    }
    
#FUNCTIONS FOR INITIAL CODE SETUP & LOADING DATA --------------------
  
  #Install commonly used packages
    InstallCommonPackages <- function(){
      install.packages('devtools')
      install.packages("readr")
      install.packages("data.table")
      install.packages("dplyr")
      install.packages('tidyr')
      install.packages("googlesheets")
      install.packages("stringr")
      install.packages('officer')
      install.packages("magrittr")
      install.packages('reshape2')
      install.packages('ggplot2')
      install.packages('xlsx')
      #install.packages('styler')
    }
  
  #Install commonly used packages
    LoadCommonPackages <- function(){
      library(devtools)
      library(readr)
      library(data.table)
      library(plyr)
      library(dplyr)
      library(tidyr)
      library(googlesheets)
      library(stringr)
      library(officer)
      library(magrittr)
      library(reshape2)
      library(ggplot2)
      library(xlsx)
      #library(styler)
    }

  #Select right 'n' characters of string
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
  #Find most recently modified file in a directory    
    MostRecentlyModifiedFilename <- function(
      title.string.match, 
      file.type, 
      dir
    ){
      setwd(dir)
      print(paste("File Directory: ", dir, sep = ""))
      match.files.v <-
        list.files()[
          grepl(tolower(title.string.match), tolower(list.files())) &  #match title string
          grepl(  #match file type
            tolower(file.type), 
            sapply(tolower(list.files()), 
            function(x){substrRight(x, nchar(file.type))})
          ) &           
          !grepl("\\~\\$", list.files())       #restrict to non-temporary files
        ]
      
      most.recent.match.file <- match.files.v[file.info(match.files.v)$mtime == sapply(match.files.v, function(x){file.info(x)$mtime}) %>% max]
      print(paste("File Name: ", most.recent.match.file, sep = ""))
      return(most.recent.match.file)
    }
    

    
#FUNCTIONS FOR MANIPULATING VECTORS & COLUMNS --------------------
  
  #Filter Vector based on condition
    FilterVector <- function(condition,vector.input){
      vector.input[condition]
    }
  
  #Remove NA from vector
    RemoveNA <- function(x){
      if(!is.null(dim(x))){stop("Input must be a vector.")}
      result <- x[!is.na(x)]
      return(result)
    }
  
  #Replace NAs in a vector with a replacement value
    SubNA <- function(vector,na.replacement){
      vector[is.na(vector)] <- na.replacement
      return(vector)
    }       
    
  #Number of times specified substring occurs within vector of character strings
    NumSubstringMatches <- function(pattern, vector){
      sapply(
        gregexpr( pattern, as.character(vector)),
        function(x) if( x[1]==-1 ){ 0 }else{ length(x) } 
      )
    }
    
  #'Multiple gsub' to find/replace multiple patterns in a vector
    #Test Inputs
      #pattern <- questions.sem.df$row.1[!is.na(questions.sem.df$q.changename)]
      #replacement <- questions.sem.df$q.changename[!is.na(questions.sem.df$q.changename)]
      #x <- names(resp1.df)
    
    mgsub <- function(
      pattern, 
      replacement, 
      x, 
      print.replacements = c(TRUE,FALSE)
    ){
      n = length(pattern)
      if (n != length(replacement)) {
        print(pattern)
        print(replacement)
        stop("Pattern and replacement do not have the same length.")
      }
      
      result = x
      num.replacements <- vector()
      
      for (i in 1:n) {
        result[grep(pattern[i], x)] <- replacement[i]
        num.replacements[i] <- length(grep(pattern[i], x))
      }
      
      if(!missing(print.replacements)){
        print.replacements <- match.arg(print.replacements)
        if(print.replacements){print(cbind(pattern,replacement,num.replacements))}
      }
      
      return(result)
    }
  
  #Unique values from multiple columns of a data frame (returns list)
    #TEST INPUTS
    #df <- resp.long.df.b
    #varnames <- loop.varnames.c
    
    UniqueValsFromColnames <- function(df, varnames){
      
      if(!(varnames %in% names(df))){
        stop(
          paste0(
            c(
              "Varnames: '",
              varnames,
              "' do not exist in table names. Table names: '", 
              paste0(names(df), collapse = ", "),
              "'."
            ),
            collapse = ""
          )
        )
      }
      
      result <- 
        df[,names(df) %in% varnames] %>%
        as.data.frame %>%
        lapply(., unique) %>%
        lapply(., RemoveNA) %>%
        lapply(., as.character) %>%
        lapply(., function(x) {strsplit(x, ",")}) %>%
        lapply(., unlist) %>%
        lapply(., unique)
      return(result)
    }
    
  #All combinations of unique values of variables in a data frame
    UniqueCombnFromColnames <- function(df, varnames){
      if(!(varnames %in% names(df))){
        stop(
          paste0(
            c(
              "Varnames: '",
              varnames,
              "' do not exist in table names. Table names: '", 
              paste0(names(df), collapse = ", "),
              "'."
            ),
            collapse = ""
          )
        )
      }
      
      result <- 
        UniqueValsFromColnames(df, varnames) %>%
        expand.grid(., stringsAsFactors = FALSE) %>%
        ReplaceNames(., current.names = names(.), new.names = varnames)
      return(result)
    }
    
  #Check which elements in a vector are different from the one before and return position of spots where values change
    VectorValueChangePositions <- function(x){
      check.mtx <-
        cbind(
          x[1:length(x)-1],
          x[2:length(x)]
        )
      check.mtxl <- matrix(nrow = nrow(check.mtx),ncol=ncol(check.mtx))
      for(i in 1:dim(check.mtx)[1]){
        
        check.mtx.i <- check.mtx[i,]
        
        if(any(is.na(check.mtx.i))){
          check.mtxl[i,] <- is.na(check.mtx.i)
        }else{
          check.mtxl[i,] <- check.mtx.i[1] == check.mtx.i[2]
        }
      }
      value.change.positions <- c(1,which(apply(check.mtxl,1,function(x){unlist(x[1]!=x[2])})),length(x))
      value.change.positions
      value.change.positions + 1
      
      result.ls <- list()
      for(j in 1:(length(value.change.positions)-1)){
        result.ls[[j]] <- 
          data.frame(
            start.position = ifelse(j == 1, 1, value.change.positions[j]+1), 
            end.position = value.change.positions[j+1]
          )
      }
      
      result <- 
        cbind(
          section.id = c(1:length(result.ls)),
          do.call(rbind, result.ls)
        )
      
      return(result)
    }
  
  #Collate a vector (e.g. "1, 1, 2, 2, 3, 3," -> "1, 2, 3, 1, 2, 3, 1, 2, 3")
    #vector <- output.df$slide.section.2
    
    CollateVector <- function(vector){
      collated.values <- 
        vector %>% 
        unique %>%
        .[order(.)] %>%
        RemoveNA %>% 
        rep(., length(vector %>% RemoveNA)/length(.))
      
      for(i in 1:length(vector)){
        
        if(i == 1){
          select.collated.value.num <- 1
        }
        
        if(!is.na(vector[i])){
          vector[i] <- collated.values[select.collated.value.num]
          select.collated.value.num <- select.collated.value.num + 1
        }
      }
      
      return(vector)  
    }  
 
  #Capitalize the first letter of each word in a substring
    FirstLetterCap_OneElement <- function(x){
      if(class(x) != "character"){print(paste("Warning: Input is of class ",toupper(class(x)),". Coercing to character vector.",sep = ""))}
      if(!is.null(dim(x))){print("Error: Input is not an atomic vector.")}
      
      s <- strsplit(as.character(x), " ")  %>% unlist
      paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
    }
    
    FirstLetterCap_MultElements <- function(x) {
      s <- strsplit(as.character(x), " ")
      sapply(s, FirstLetterCap_OneElement)
    }


#FUNCTIONS FOR CONVERTING VECTORS/VARIABLES TO DIFFERENT CLASSES -------------------- 
      
    #TESTING OBJECTS
      #x2 <- data.frame(
      #  factor.name = c("a","xa","222","xa"),
      #  character.name = c("char","a","b","cd"),
      #  logical.name = c(TRUE, FALSE,TRUE,FALSE),
      #  integer.name = c(1,2,3,4) %>% as.integer,
      #  numeric.name = c(1.1,2.2,3.3,4.4)
      #)
      #x2$character.name <- x2$character.name %>% as.character
      
      #x1 <- data.frame(numeric.name = c(1.1,2.2,3.3))
      
      #x <- x2
      
      #Convert to Factor  
        ConvertToFactor <- function(x){
          if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
          result <- as.factor(x)
          return(result)
        }
      
      #Convert to Character
        ConvertToCharacter <- function(x){
          if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
          result <- as.character(x)
          return(result)
        }
      
      #Convert to Logical
        ConvertToLogical <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        
        if(class(x) == "logical"){print("Input is already logical. No conversion performed.")}
        
        if(class(x) %in% c("factor","character","numeric","integer")){
          y <- as.character(x) %>% trimws(., which="both")
          unique.yvals <- unique(y)[order(unique(y))]
          
          if(length(unique.yvals) > 2){
            result <- x
            print("Input must have exactly two values. Input has more than two values. No changes made.")
          }
          
          if(length(unique.yvals) < 2){
            result <- x
            print("Input must have exactly two values. Input has less than two values.  No changes made.")
          }
          
          if(all(unique.yvals == c(0,1))){ #Factor is composed of 1 and 0 only
            result <- as.logical(y)
          }
          
          if(all(unique.yvals == c("FALSE","TRUE"))){ #Factor is composed of TRUE and FALSE only
            result <- as.logical(y)
          }
          
          if(length(unique(y)) == 2 & !all(unique.yvals == c(0,1)) & !all(unique.yvals == c("FALSE","TRUE"))){ #Factor is composed of two character strings not automatically convertible into logical
            print(unique.yvals)
            zero.val <- unique.yvals[readline(prompt = "Which value should be set as TRUE? (Enter 1 if first value, 2 if second value)") %>% as.numeric]
            y[which(y!=zero.val)] <- FALSE
            y[which(y==zero.val)] <- TRUE
            result <- as.logical(y)
          }
        }
        return(result)
      }
      
      #Convert to Numeric 
        ConvertToNumeric <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        
        if(class(x) %in% c("numeric","integer")){
          result <- as.numeric(x)
        }
        
        if(class(x) %in% c("factor")){
          if(as.numeric(as.character(x)) %>% unique %>% is.na %>% all){print("Warning: Converting character to number resulted in all NA. No numbers to convert")}
          result <- as.numeric(as.character(x))
        }
        
        if(class(x) %in% c("logical")){
          convert.LogicalToNumeric <- readline(prompt = "Input is logical. Would you like to convert to binary - FALSE = 0, TRUE = 1? (Y/N)")
          if(convert.LogicalToNumeric == "Y"){
            result <- as.numeric(x)
          }else{
            result <- x
            print("Cannot convert logical to numeric. No output.")
          }
        }
        
        if(class(x) %in% c("character")){
          if(as.numeric(x) %>% unique %>% is.na %>% any){print("Warning: Converting character to number resulted in all NA.")}
          result <- as.numeric(x)
        }
        return(result)
      }
      
      #Convert to Integer
        ConvertToInteger <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        
        if(class(x) %in% c("numeric","integer")){ result <- as.integer(x)}
        
        if(class(x) %in% c("factor")){
          y <- as.integer(as.character(x))
          if(y %>% unique %>% is.na){print("Warning: Converting character to integer resulted in all NA. No numbers to convert")}
          result <- y
        }
        
        if(class(x) %in% c("logical")){
          convert.LogicalToNumeric <- readline(prompt = "Input is logical. Would you like to convert to binary - FALSE = 0, TRUE = 1? (Y/N)")
          if(convert.LogicalToNumeric == "Y"){
            result <- as.numeric(x)
          }else{
            print("Cannot convert logical to numeric. No output.")
            result <- x
          }
        }
        
        if(class(x) %in% c("character")){
          if(as.integer(x) %>% unique %>% is.na){print("Warning: Converting character to integer resulted in all NA.")}
          result <- as.integer(x)
        }
        return(result)
      }
      
      #Convert to Date
        ConvertToDate <- function(x){
        if(!is.null(dim(x))){stop("Input is not an atomic vector.")}
        y <- as.character(x)
        result <- tryCatch({y %>% as.Date()},
                           error=function(e){
                             return(y)
                             message("Error: Unable to convert character to date format.")
                           }
        )
        return(result)
        }
      
      #Set Class of a named column
        SetColClass <- function(
          tb, 
          colname, 
          to.class = c("factor", "character", "logical", "numeric", "integer", "date")
        ){
          
          if(!(colname %in% names(tb))){
            stop(paste0("Colname '", colname, "' is not present in tb names."))
          }
          match.arg(to.class)
          
          #print(class(tb[[which(names(tb) == colname)]]) )
          
          if(to.class == "factor"){tb[[which(names(tb) == colname)]] <- ConvertToFactor(tb[[which(names(tb) == colname)]])}
          if(to.class == "character"){tb[[which(names(tb) == colname)]] <- ConvertToCharacter(tb[[which(names(tb) == colname)]])}
          if(to.class == "logical"){tb[[which(names(tb) == colname)]] <- ConvertToLogical(tb[[which(names(tb) == colname)]])}
          if(to.class == "numeric"){tb[[which(names(tb) == colname)]] <- ConvertToNumeric(tb[[which(names(tb) == colname)]])}
          if(to.class == "integer"){tb[[which(names(tb) == colname)]] <- ConvertToInteger(tb[[which(names(tb) == colname)]])}
          if(to.class == "date"){tb[[which(names(tb) == colname)]] <- ConvertToDate(tb[[which(names(tb) == colname)]])}
        
          return(tb)
        }

#FUNCTIONS FOR MANIPULATING DATA FRAMES & TABLES --------------------
  
  #Output variable names in data frame which can be converted to numeric       
    NumericVarnames <- function(df) {
        result <- 
          df %>%
          apply(., 2, unique) %>%
          sapply(., 
                 function(x){
                   ifelse(
                     length(x) == 1,
                     as.numeric(x) %>% is.na(.) %>% sum(.) < 1,
                     as.numeric(x) %>% is.na(.) %>% sum(.) <= 1
                   )
                 }
          ) %>%
          names(df)[.]
        return(result)
      }
  
  #Output variable names in data frame which are of user-defined class
     VarnamesOfClass <- function(
       dat,
       colclass = c("numeric","character","logical","factor","date","integer") 
     ){
        colclass <- match.arg(colclass)
        
        name.classes <- 
          lapply(dat, class) %>% 
          unlist %>% 
          as.vector(.)
        
        result <- 
          names(dat)[which(name.classes %in% colclass)]
        
        return(result)
      }
        
  #Order Data Frame by specific column
    OrderDfByVar <- function(df, order.by.varname, rev) {
      if(!exists("rev")){rev <- FALSE}
      
      df <- as.data.frame(df)
      
      if(rev == FALSE){
        result <- df[order(df[,names(df) == order.by.varname]),]
      }else{
        result <- df[rev(order(df[,names(df) == order.by.varname])),]
      }
        
      return(result)
    }
  
  #Manually order a table's rows using a character vector which contains all values in a names variable in the table
    #Test Inputs
      #tb = result 
      #tb.order.varnames = names(result)[names(result) == names(y.headers)] 
      #ordering.vectors.list = list(config.input$y.varname.order %>% strsplit(., ",") %>% unlist)
    
    ManualOrderTableByVectorsWithValuesCorrespondingToVariableInTable <- function(
      tb, #table to order
      tb.order.varnames, #names of variable in table with values corresponding to vector 
      ordering.vectors.list, #vectors with ordered values
      ...
    ){
      if(c(tb.order.varnames) %in% names(tb) %>% all %>% not){
        stop(paste0("No variable named '", tb.order.varname, "' in data table."))
      }
      
      if(length(tb.order.varnames) != length(ordering.vectors.list)){
        stop(
          paste0(
            "Number of tb.order.varnames (", 
            length(tb.order.varnames),
            ") does not match number of ordering vectors in the list (",
            length(ordering.vectors.list),")."
          )
        )
      }
      
      tb.ls <- list()
      
      for(i in 1:length(tb.order.varnames)){  
        
        if(i != 1){tb <- tb.ls[[i-1]]} #cumulatively build final table
        tb.order.varname.i <- tb.order.varnames[i]
        ordering.vector.i <- ordering.vectors.list[[i]]
        
        order.tb.i <- #one-column data frame with correctly ordered values
          ordering.vector.i %>% 
          as.data.frame(., stringsAsFactors = FALSE) %>% 
          mutate(num.var = paste(1:length(ordering.vector.i), ordering.vector.i,sep=".")) %>%
          ReplaceNames(
            df = ., 
            current.names = names(.), 
            new.names = c(tb.order.varname.i,paste0(tb.order.varname.i,".num"))
          )
        
        tb.ls[[i]] <- #same adding a column with those values and numbers in front
          left_join(
            tb,
            order.tb.i,
            by = tb.order.varname.i
          ) 
      }
      
      
      order.formula <- paste0("result.tb$", tb.order.varnames, ".num") %>% paste(., collapse = ",") %>% paste0("order(",.,")")
      
      result.tb <- tb.ls[[i]]
      result <- 
        result.tb[eval(expr = parse(text = order.formula)),] %>%
        select(SelectNamesIn(tb = ., condition = "NOT.IN", paste0(tb.order.varnames,".num")))
        
      return(result)
    }  
    
  #Replace names in a data frame
    ReplaceNames <- function(df,current.names, new.names) {
      
      #Data Checks
      if(!is.data.frame(df)){
        stop("Input not a data frame. Input must be of class 'data.frame'.")
      }
      
      #New Names Checks
      if(!exists("new.names")){
        new.names <- readline(prompt = "No new names defined. Enter a vector of new names to replace current names: ")
      }
      
      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }
      
      #Current Names Checks
      if(!exists("current.names")){
        
        if(length(names(df)) == length(new.names)){
          print("No current names to replace specified. All current names will be replaced.")
          current.names <- names(df)
        }
        
        if(length(names(df)) != length(new.names)){
          stop(
            paste(
              "No current names to replace specified. Current df has ",
              length(names(df)),
              " columns. New names is of length ",
              length(new.names),
              ".",
              sep = ""
            )
          )
        }
        
      } #End of if statement for when current.names not defined by user
      
      if(any(!current.names %in% names(df))){
        warning(
          paste(
            "One or more current.names were not found in input data frame: '",
            current.names[!current.names %in% names(df)],
            "'. ",
            sep = ""
          )
        )
      }
      
      #Actual Function: name replacement
      names(df)[names(df) %in% current.names] <- new.names
      return(df)
    }
  
  #Select names based on in/not in string vector
    SelectNamesIn <- function(tb, condition = c("IN","NOT.IN"), char.vector){
      condition <- match.arg(condition)
      if(condition == "IN"){return(names(tb)[names(tb) %in% char.vector])}
      if(condition == "NOT.IN"){return(names(tb)[!(names(tb) %in% char.vector)])}
    }
  
  #Select columns based on in/not in string vector
    SelectColsIn <- function(tb, condition = c("IN","NOT.IN"), char.vector){
      condition <- match.arg(condition)
      if(condition == "IN"){return(tb[,names(tb) %in% char.vector])}
      if(condition == "NOT.IN"){return(tb[,!(names(tb) %in% char.vector)])}
    }
  
  #Move named columns to the front/left side of table
    MoveColsLeft <- function(
      dat,
      colnames
    ){
      
      if(any(!(colnames %in% names(dat)))){
        stop(paste0("colnames '", colnames[!(colnames %in% names(dat))], "' missing from table names."))
      }
      
      result <- 
        cbind(
          dat %>% select(colnames),
          dat[,!(names(dat) %in% colnames)]
        ) %>% as_tibble()
      
      return(result)
    }
    
  #Move named columns to the back/right side of table
    MoveColsRight <- function(
      dat,
      colnames
    ){
      
      if(any(!(colnames %in% names(dat)))){
        stop(paste0("colnames '", colnames[!(colnames %in% names(dat))], "' missing from table names."))
      }
      
      result <- 
        cbind(
          dat[,!(names(dat) %in% colnames)],
          dat %>% select(colnames)
        ) %>% as_tibble()
      
      return(result)
    }
      
  #Left Join & Replace NAs
    left.join.NA <- function(.x, .y, .by, na.replacement) {
      result <- left_join(x = .x, y = .y, by = .by, stringsAsFactors = FALSE) %>% 
        mutate_all(funs(replace(., which(is.na(.)), na.replacement)))
      return(result)
    }

  #Unique Values of Named Variables in Data Frame
    #TODO: Generalize to graph code as well? So treat like a pivot table with arbitrary number of x.vars and y.vars, a summary var and a summary function.
    #TODO: Maybe would make it so could use a single config table?
    #TODO: Should generalize so that can handle arbitrary number of nested variables on both axes like pivot
    #Test Inputs
      #varnames <- graph.varnames.d
      #tb <- resp.long.tb %>% as_tibble()
    
    UniqueVariableValues <- function( tb, varnames){
      
      varnames <- as.character(varnames)
      tb <- as_tibble(tb)
      #all.cats.ls <- list()
      
      result <- apply(tb %>% select(varnames), 2, function(x) RemoveNA(unique(x)))
      
      return(result)
    }
    
  #Unique Values of Measures of Key Column (for long data)
    #Test Inputs
      #.data <- resp.long.df
      #.keyvarname <- 
    
    UniqueKeyVals <- function(.data, .keyvarname, .keyvals, .measurevarname) {
      keyvals <- as.character(.keyvals)
      keyvarname <- as.character(.keyvarname)
      measurevarname <- as.character(.measurevarname)
      tb <- as_tibble(.data)
      
      result <- 
        tb %>%
        filter(keyvarname %in% keyvals) %>%
        select(measurevarname) %>%
        unique(.)
        
        
      return(result)
    }
  
  #Reshaping data into long format based on splitting a column on a character
    #Test inputs
      #df =  resp.long.tb.c %>% GraphDataRestriction( dat = ., dat.config = config.graphs.df.d ) 
      #id.varname = "resp.id"
      #split.varname = "module"
      #split.char = ","
  
    SplitColReshape.ToLong <- function(df, id.varname, split.varname, split.char){ 
      
      if(!is.data.frame(df)){stop("Input not a data frame.")}
    
      if(!exists("split.char")){
        split.varname <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
      }
      
      if(!exists("split.varname")){
        split.varname <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
      }
      
      if(!exists("split.char")){
        split.char <- readline(prompt = "Enter the character(s) you would like to split your variable on: ")
      }
      
      if(dim(df)[1] == 0){
        result <- df
      }else{
        id.var <- df[,names(df)==id.varname]
        split.var <- df[,names(df)==split.varname]
        
        result <- 
          df %>% 
          mutate(new.split.var =
            #df[,names(df)==split.varname] = strsplit(df[,names(df)==split.varname],",")
              sapply(
                split.var, function(x){strsplit(x,split.char)}
              )
          ) %>% 
          unnest(new.split.var, .drop = FALSE) %>% 
          .[,names(.)[names(.) != split.varname]] %>%
          OrderDfByVar(
            df = ., 
            order.by.varname = id.varname,
            rev = FALSE
          ) %>%
          ReplaceNames(
            df = .,
            current.names = "new.split.var",
            new.names = split.varname
          )
      }  
      
      return(result)
    }

  #Display Unique Vector Values (with trimming to 25 characters, useful for ColClassConvert function)
    DisplayUniqueColVals <- function(x){
      if(paste(unique(x), collapse = ", ") %>% nchar <= 25){
        result <- paste(unique(x)[order(unique(x))], collapse = ", ")   
      }
      if(paste(unique(x), collapse = ", ") %>% nchar > 25){
        result <- paste(unique(x)[order(unique(x))], collapse = ", ") %>% substr(., 1, 30) %>% paste(.,"...",sep="")   
      }
      return(result)
    }
    

  #Convert data frame column classess according to user input
    #TODO: 1. MAKE SO CAN DESIGNATE ALL COLUMNS THE SAME; 2. MAKE SO CAN DESIGNATE ONLY CERTAIN COLUMNS WANT TO CHANGE (E.G. "end")
    #2. Prompt for classes of each column; have option to just say 'as-is' (already in correct format)
    #3. As each column is entered, bind and display with column names
    #4. Once all entered, convert to 
    ColClassConvert <- function(x){
      library(magrittr)
      if(!is.data.frame(x)){stop("Input not a data frame.")}
      
      #Display names of data.frame with class they are currently
        display.df <- lapply(x, class) %>% #! Makes a data frame storing lists
          as.matrix %>% 
          cbind(
            1:dim(x)[2],
            names(x),
            apply(x, 2, DisplayUniqueColVals) %>% unlist,
            .
          ) %>% 
          as.data.frame() %>%
          lapply(., unlist) %>%
          as.data.frame
        
        
        names(display.df) <- c("colnum","colname","unique.values","class")
        #display.df <- apply(display.df, 2, function(z){as.character(z) %>% trimws(., which = "both")})
        display.df$corrected.class <- ""
        
        print(display.df)
      
      #Loop to collect user input for what classes columns should be converted to
        #a <- 1 #LOOP TESTER
        for(a in 1:dim(display.df)[1]){
          corrected.class.a <- readline(prompt = paste("Enter corrected column class for '",display.df$colname[a],"' (Press enter to leave the same):",sep="")) %>% tolower
          
          if(!corrected.class.a %in% c("factor","character","logical","integer","numeric","date","")){
            print("Warning: input does not match valid column class. Enter one of the following: factor, character, logical, integer, numeric.") 
            corrected.class.a <- readline(prompt = paste("Enter corrected column class for '",display.df$colname[a],"':",sep=""))
          }
          
          if(corrected.class.a == ""){
            display.df$corrected.class[a] <- display.df$class[a] %>% as.character
          }else{
            display.df$corrected.class[a] <- corrected.class.a
          }
          print(display.df)
        }
      
      
      #Conversion loop by column of data frame
        progress.bar.b <- txtProgressBar(min = 0, max = 100, style = 3)
        progress.bar.b.max <- ncol(x)					
      
        for(b in 1:ncol(x)){ #START OF LOOP BY COLUMN
          if(display.df$class[b] == display.df$corrected.class[b]){next()}
          convert.to.b <- display.df$corrected.class[b]
          if(convert.to.b == "factor"){x[,b] <- ConvertToFactor(x[,b])}
          if(convert.to.b == "character"){x[,b] <- ConvertToCharacter(x[,b])}
          if(convert.to.b == "logical"){x[,b] <- ConvertToLogical(x[,b])}
          if(convert.to.b == "integer"){x[,b] <- ConvertToInteger(x[,b])}
          if(convert.to.b == "numeric"){x[,b] <- ConvertToNumeric(x[,b])}
          if(convert.to.b == "date"){x[,b] <- ConvertToDate(x[,b])}
            
          setTxtProgressBar(progress.bar.b, 100*b/progress.bar.b.max)	
          
        } #END OF LOOP BY COLUMN
        out
        close(progress.bar.b)
      
      #Display column names with new variable to confirm new classes
        display.df$confirm.converted.colclass <- lapply(x, class) %>% unlist
      
      #print(display.df)
      return(x)
    }
    

 
   

      