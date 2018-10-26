###
##  How this code works 
##
##  %%%%   CALL Build() => Update() => Get   %%%%
##
##  call the method Convert_to_PDF() to convert Text files into PDFs
##   DIR = <dir>           => Location of execution
##   all = FALSE           => Will not overwrite current PDFs, only performs for new ones       
##
##
##  call the method Build() to aquire every Light Novel URL listed on the website
##   Update_post = TRUE    => Will automatically call Update() post Build() execution
##   Genre=NA              => to be implemented
##   DIR = <dir>           => Location for execution
##
##  Call the method Update() to run though and update the Excel file holding the data
##   post_get = FALSE      => will call Get() with base parameters once finished executing update
##   url_list = <c()>      => List of URLs to update
##   DIR = <dir>           => Directory of which to complete and construct the table
##   
##  Call the method Get() to download everything that is new in the Update() tab
##   full = FALSE          => Will download everything, regardless of the current Update status
##   DIR = <dir>           => Directory where this will be generated
##
###
if(!("rvest" %in% installed.packages())){install.packages("rvest");library("rvest")}else{library("rvest")}
if(!("dplyr" %in% installed.packages())){install.packages("dplyr");library("dplyr")}else{library("dplyr")}
if(!(exists("PDF_Storage"))){PDF_Storage <- c()}else{PDF_Storage <- PDF_Storage}
Failed_URLs <- c()

Convert_to_PDF(DIR = dir, all = FALSE){
  if(!("Series" %in% dir())){stop("Series folder does not exist")}
  setwd("Series")
  Series_dir <- getwd()
  Series_List <- dir()
  setwd(dir)
  dir.create("PDFs")
  setwd("PDFs")
  PDF_dir <- getwd()
  for(x in 1:length(Series_List)){
    setwd(Series_dir)
    setwd(Series_List[x])
    book_list <- dir()
    source_dir <- getwd()
  }
}

Update  <-  function(post_get=FALSE, url_list, DIR = "~/Novel Planet Books"){
  #sets the WD to a default location or to a specified one. 
  if(!tryCatch({setwd(DIR);TRUE}, error = function(x){FALSE})){stop("Directory Set Failed!")}
  if(!("Sheet" %in% dir())){dir.create("Sheet")}
  setwd("Sheet")
  Changed <- c()
  if("Master_Sheet.csv" %in% dir()){
   Table <- read.csv("Master_Sheet.csv", stringsAsFactors = FALSE)
  }else{
   Name <- c("")
   url <- c("")
   Release_Date <- c(0)
   Genre <- c("")
   Completed <- c(FALSE)
   Translator <- c("")
   Description <- c("")
   Count <- c(0)
   #Chapters <- c(c(""))
   Update_needed <- c(FALSE)
   Table <- data.frame(Name, url, Release_Date, Genre, Completed, Translator, Description, Count, Update_needed)
   Table$Name <- ""
   Table$url <- ""
   Table$Release_Date <- 0
   Table$Genre <- ""
   Table$Completed <- FALSE
   Table$Translator <- ""
   Table$Description <- ""
   Table$Count <- 0
   Table$Update_needed <- TRUE
   write.csv(Table, "Master_Sheet.csv")
  }
  #affirms Table exists
  if(!exists("Table")){
    stop("Table does not exist!")}
  #Iterates through
  for(url in url_list){
    print(url)
    #Gets Data
    Sys.sleep(sample(0:1, 1)*.1)
    session <- html_session(url)
    name <- session %>% html_node(".title") %>% html_text()
    Release_Date <- as.numeric(gsub("\\D", "", session %>% html_node(".divReplaceP div:nth-child(1)") %>% html_text()))
    Genre <- gsub(".*Genre: |\\r|\\n", "", session %>% html_node("p:nth-child(3)") %>% html_text())
    Completed <- if(grepl("Ongoing", session %>% html_node("p:nth-child(6)") %>% html_text())){FALSE}else{TRUE}
    Translator <- gsub("\\r|\\n|^.*Translator: |^\\s*|\\s*$", "", session %>% html_node("p:nth-child(7)") %>% html_text())
    Description <- gsub("\\r|\\n|^\\s*|\\s*$", "", session %>% html_node(".post-contentDetails~ div+ div") %>% html_text())
    Count <- length(gsub("\\r|\\n|^\\s*|\\s*$", "", session %>% html_nodes(".rowChapter a") %>% html_text()))
    Chapter_Data <- gsub("\\r|\\n|^\\s*|\\s*$", "", session %>% html_nodes(".rowChapter a") %>% html_text())
    
    if(name %in% Table$Name){
      Corr_Row <- 0
      for(x in 1:nrow(Table)){if(Table$Name[x] == name){Corr_Row <- x}}
      if(Corr_Row == 0){stop("Critical error Found when determining integrety of Rows")}
      row <- Corr_Row
      if(Table$Genre[row]!= Genre){Table$Genre[row]<-Genre}
      if(Table$Completed[row] != Completed){Table$Completed[row] <- Completed}
      if(Table$Translator[row] != Translator){Table$Translator[row] <- Translator}
      if(Table$Description[row] != Description){table$Description[row] <- Description}
      if(Table$Count[row]!= Count){Table$Count[row]<-Count;Table$Update_needed[row] <- TRUE}
      if(paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), ".txt", sep="") %in% dir()){
        tbl <- read.table(paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), ".txt", sep=""), sep = "\n")
        spt <- as.vector(tbl[,1])
        if(length(spt)!=length(Chapter_Data)){Table$Update_needed[row] <- TRUE}
        else{
          for (x in 1:length(spt)){
            if(spt[x]!=Chapter_Data[x]){Table$Update_needed[row] <- TRUE}
          }
        }
      }
    } else {
      Row <- Table[1,]
      Row$Name[1] <- name
      Row$url[1] <- url
      tryCatch({Row$Release_Date[1] <- Release_Date},error=function(x){Row$Release_Date[1] <- NA})
      tryCatch({Row$Genre[1] <- Genre},error=function(x){Row$Genre[1] <- NA})
      tryCatch({Row$Completed[1] <- Completed},error=function(x){Row$Completed[1] <- NA})
      tryCatch({Row$Translator[1] <- Translator},error=function(x){Row$Translator[1] <- NA})
      tryCatch({Row$Description[1] <- Description},error=function(x){Row$Description[1] <- NA})
      Row$Count[1] <- Count
      Row$Update_needed[1] <- TRUE
      Table <- dplyr::bind_rows(Table, Row)
    }
    write(Chapter_Data, file = paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), ".txt", sep = ""))
    
  }
  write.csv(Table, "Master_Sheet.csv", row.names = FALSE)
  setwd("..")
}

Build <- function(Update_post = TRUE, Genre = NA, DIR = "~/Novel Planet Books"){
  if(!tryCatch({setwd(DIR);TRUE}, error = function(x){FALSE})){stop("Directory Set Failed!")}
  tryCatch({Session <- html_session("http://novelplanet.com/NovelList")}, error = function(x){stop("Failed to Access Main Page!")})
  page <- 1
  Last_Page <- FALSE
  repeat{
    print(paste("Running though page ", page, sep = ""))
    if(!(TRUE %in% grepl("Last", Session %>% html_nodes("#divJumpMain a") %>% html_text()))){Last_Page <- TRUE}
    url_List <- c()
    links <- Session %>% html_nodes(".title") %>% html_attr("href")
    links <- gsub("^", "http://novelplanet.com", links)      
    links2 <- c()
    
    for(url in links){
      head <- gsub("Novel/.*$", "Novel/", url)
      tail <- gsub("^.*Novel/", "", url)
      feathers <- c()
      for (x in 1:nchar(tail)){
        feathers <- c(feathers, charToRaw(substring(tail, x, x)))
      }
      set_tail <- ""
      for (feather in feathers){
        set_tail <- paste(set_tail, feather, sep = "%")
      }
      new_url <- paste(head, set_tail, sep = "")
      tryCatch({sesh <- html_session(new_url);links2 <- c(links2,sesh$url)}, error = function(x){print(paste("Error on", url))} )
    }
    if(!("All_URLs.txt" %in% dir())){
      write(links2, file = "All_URLs.txt")
    } else {
      Set <- read.table("All_URLs.txt", sep = "\n")
      urls <- as.vector(Set[,1])
      urls <- unique(c(urls, links2))
      write(urls, file = "All_URLs.txt")
    }
    if(Last_Page){break()}
    page <- page+1
    Session <- html_session(paste("http://novelplanet.com/NovelList?page=", page, "#divJumpMain", sep = ""))
  }
  
  if(Update_post){
    FULL_IMPORT <- read.table("All_URLs.txt", sep = "\n")
    urls <- as.vector(FULL_IMPORT[,1])
    d <- DIR
    Update(url_list = urls, DIR = d)
  }
  
}

Get <- function(DIR = "~/Novel Planet Books", full = FALSE, url_list = urls){
  if(!tryCatch({setwd(DIR);TRUE}, error = function(x){FALSE})){stop("Directory Set Failed!")}
  if(!("Sheet" %in% dir())){dir.create("Sheet")}
  setwd("Sheet")
  if(!("Master_Sheet.csv" %in%  dir())){Update(DIR = getwd(), url_list = urls)}
  Table <- read.csv("Master_Sheet.csv", header = TRUE, stringsAsFactors = FALSE)
  setwd("..")
  #Fills out new sheet
  URLs_to_use <- c()
  for(x in 2:nrow(Table)){
    if(Table$Update_needed[x] == TRUE||full == TRUE){
      URLs_to_use <- c(URLs_to_use, Table$url[x])
    }
  }
  
  if(!("Series" %in% dir())){dir.create("Series")}
  
  
  for(url in URLs_to_use){
    tryCatch({
    #Sys.sleep(sample(1:15, 1)*.1)
    setwd(DIR)
    setwd("Series")
    n <- ""
    Error <- FALSE
    session <- 0
    #This is where the bulk of the code happens
    session <- html_session(url)
    name <- session %>% html_node(".title") %>% html_text()
    saved_name <- paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), ".txt", sep = "")
    if(!(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name) %in% dir())){dir.create(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name))}
    setwd(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name))
    Chapter_Data <- gsub("\\r|\\n|^\\s*|\\s*$", "", session %>% html_nodes(".rowChapter a") %>% html_text())
    Links <- gsub("^", "novelplanet.com", session %>% html_nodes('.rowChapter a') %>% html_attr("href"))
    Lists_Volume <- TRUE %in% grepl("Volume", Chapter_Data, ignore.case = TRUE)
    Lists_Chapter <- TRUE %in% grepl("Chapter", Chapter_Data, ignore.case = TRUE)
    Lists_Part <- TRUE %in% grepl("part", Chapter_Data, ignore.case = TRUE)
    Has_Dot_Part <- TRUE %in% grepl("\\d+.\\d+", Chapter_Data)
    if(length(Chapter_Data)>=1){
      for(z in 1:length(Chapter_Data)){
        if (grepl(".pdf$", Chapter_Data[z])){PDF_Storage <<- c(PDF_Storage, Links[z]);Links[z] <- ""; Chapter_Data[z] <- ""}
      }
    } else {
      Error <- TRUE
    }
    Links <- unique(Links)
    Chapter_Data <- unique(Chapter_Data)
    
    spt <- Chapter_Data 
    vol <- 1
    base_chap <- 0
    Vol_Chain <- c()
    max_vol <- 0
    F<-c()
    vol_h <- 1
    short_vol_chain <- c()
    Chap_Chain <- c()
    if(length(spt)>=1&&!Error){
      for(z in length(spt):1){
        Sys.sleep(sample(0:1,1)*.1)
        vol_match <- regexpr("Volume \\d+", spt[z])
        if(vol_match[1] != -1){
          vol_h <- as.numeric(gsub("\\D", "", regmatches(spt[z], vol_match)))
          if(vol_h != vol){
            write(short_vol_chain, file = paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), " Volume ", vol, ".txt", sep = ""))
            short_vol_chain <- c()
          }
          vol <- vol_h
          ses <- html_session(Links[z])
          short_vol_chain <- c(short_vol_chain, paste("\\\\pagebreak\\\\\\\\", ses %>% html_node("#divReadContent") %>% html_text(), sep = ""))
        } else {
          Chap_Chain <- c(Chap_Chain, Links[z])
        }
      }
    } else {Error <- TRUE}
    if(length(short_vol_chain)!=0){
        write(short_vol_chain, file = paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), " Volume ", vol, ".txt",sep = ""))
      }
    if(length(Chap_Chain)!=0){
        count <- 1
        q <- 1
        text <- ""
        for(x in 1:length(Chap_Chain)){
          ses <- html_session(paste("https://",Chap_Chain[x],sep = ""))
          t <- ses %>% html_node("#divReadContent") %>% html_text()
          text <- paste(text, t, sep = "\\\\pagebreak\\\\\\\\")
          count <- count+1
          if(count > 10){
            write(text, file = paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), " Chapter_Comp_number_", q, ".txt", sep = ""))
            q <- q+1
            count <- 1
            text <- ""
          }
        }
        write(text, file = paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), " Chapter_Comp_number_", q, ".txt", sep = ""))
      }
    
    setwd("..")
    if(!Error){
    print(paste("Success Downloading ", name, sep = ""))
    for(x in 1:nrow(Table)){
      if(gsub("[^a-zA-Z0-9\\-\\_\\ ]|\\s", "", Table$Name[x], ignore.case = TRUE)==gsub("[^a-zA-Z0-9\\-\\_\\ ]|\\s", "", name, ignore.case = TRUE)){
        Table$Update_needed[x] <- FALSE
      }
    }
    setwd("..")
    setwd("Sheet")
    write.csv(Table, "Master_Sheet.csv", row.names = FALSE)
    setwd(DIR)
    if(!("PDF_Storage.txt" %in% dir())){
      write(c("First\n", "Second\n",PDF_Storage), file = "PDF_Storage.txt")
    }else{
      Set <- read.table("PDF_Storage.txt", sep = "\n") 
      Set <- as.vector(Set[,1])
      Set <- unique(c(Set, PDF_Storage))
      write(unique(Set), file = "PDF_Storage.txt")
    }
    setwd("..")
    } else {
      Failed_URLs <<- c(Failed_URLs, url)
    }
    }, error = function(x){
      Failed_URLs <<- c(Failed_URLs, url)
      setwd(DIR)
      if(!("PDF_Storage.txt" %in% dir())){
        write(c("First\n", "Second\n",PDF_Storage), file = "PDF_Storage.txt")
      }else{
        Set <- read.table("PDF_Storage.txt", sep = "\n") 
        Set <- as.vector(Set[,1])
        Set <- unique(c(Set, PDF_Storage))
        write(unique(Set), file = "PDF_Storage.txt")
      }
    })
  }
  if(!("PDF_Storage.txt" %in% dir())){
    write(c("First\n", "Second\n",PDF_Storage), file = "PDF_Storage.txt")
  }else{
    Set <- read.table("PDF_Storage.txt", sep = "\n") 
    Set <- as.vector(Set[,1])
    Set <- unique(c(Set, PDF_Storage))
    write(unique(Set), file = "PDF_Storage.txt")
  }
}

top_urls <- c(
          "http://novelplanet.com/Novel/Violet-Evergarden", 
          "http://novelplanet.com/Novel/Overlord-LN" # More can be added
          )




Build(DIR = "~/Novel Planet Books", Update_post = TRUE)
errors <- 0
bool <- TRUE
helper <- 0
while(bool){
  tryCatch({Get(DIR = "~/Novel Planet Books")},error = function(x){errors<<-errors+1;print("Failed")})
  Failed_URLs <- unique(Failed_URLs)
  if(helper == length(Failed_URLs)){bool <- FALSE}
  helper <- length(Failed_URLs)
}



#Update(url_list = top_urls,DIR = d)
#ptm<-proc.time()
#Get(DIR = d)
#proc.time()-ptm



#bool <- TRUE
#last_count <- length(Failed_URLs)
#while(bool){
#  Get()
#  if(length(Failed_URLs)==last_count){
#    bool <- FALSE
#  }
#}

x <- dir()
setwd("..")
setwd("sheet")
