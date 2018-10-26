#Stephen Kozak
#Convert To PDFs
List_of_new <- c()
Download_Call <- function(name, text, vol){  #This is just LaTex markdown!
  init_tex<-c("% By Jason Filippou",    ### This is my teacher's name. Taught at University of Maryland CMSC250, Fall 2016
              "\\documentclass[a4paper]{article}\n",
              "% PREAMBLE START",
              "\\usepackage{amsgen,amsmath,amstext,amsbsy,amsopn,amssymb, amsthm}",
              "\\usepackage{float}",
              "\\usepackage[colorlinks=true, urlcolor=blue,  linkcolor=blue, citecolor=blue]{hyperref}",
              "\\usepackage{caption}",
              "\\usepackage[rgb]{xcolor}",
              "\\usepackage[inline]{enumitem}",
              "\\usepackage[inner=1.5cm,outer=1.5cm,top=2cm,bottom=2cm]{geometry}",
              "\\usepackage[colorinlistoftodos]{todonotes}",
              "\\usepackage[bottom]{footmisc}",
              "\\usepackage{pbox}\n",
              "\\newcommand{\\tbd}{\\textbf{\\textcolor{red}{TBD}}}\n",
              "\\begin{document}\n",
              text,
              "\\end{document}")
  if(!(paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), "_", vol, ".tex", sep = "") %in% dir())){List_of_new <- c(List_of_new, paste(name, vol, sep = "         "))}
  write(init_tex, file = paste(gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), "_", vol, ".tex", sep = ""))
  x <- system(intern = TRUE,
              ignore.stdout = FALSE,
              ignore.stderr = FALSE,
              show.output.on.console = TRUE,
              invisible = FALSE,
              command = paste("pdflatex ", gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name), "_", vol, ".tex", sep = "")
  )
}

Download_Files <- function(url){
  library("rvest")
  # Path Setting
  if(!tryCatch({dir_Path <- "~/PDF_Collection_LNs"; setwd(dir_Path);TRUE},  error = function(x){FALSE}))
    {print("Directory not found, please ensure the correct path has been specified and \ have been changed to /"); stop()}
  
  if(!tryCatch({Main_Page <- html_session(url);TRUE}, error = function(x){FALSE}))
    {print("Failed to access the home Page for this novel")}
  
  if(!tryCatch({Series_Name <- gsub("\\s", "\\_", Main_Page %>% html_nodes("#firstHeading span") %>% html_text); TRUE}, error = function(x){FALSE}))
    {print(paste("Failed to obtain the Name for url", url))}
  
  if(!tryCatch({Listing_Of_Headers <- Main_Page %>% html_nodes(".mw-headline a") %>% html_attr("href"); TRUE}, error = function(x){FALSE}))
   {print(paste("Failed to extract Headers for", gsub("_", " ", Series_Name)))}
  
  if(!tryCatch({Listing_Of_Headers_Spec <- Main_Page %>% html_nodes("span+ a") %>% html_attr("href"); TRUE}, error = function(x){FALSE}))
  {print(paste("Failed to extract Spec Headers for", gsub("_", " ", Series_Name)))}
  
  if(!tryCatch({Listing_Of_Headers_TBL <- Main_Page %>% html_nodes(".wikitable .mw-headline a") %>% html_attr("href"); TRUE}, error = function(x){FALSE}))
  {print(paste("Failed to extract TBL Headers for", gsub("_", " ", Series_Name)))}
  
  if(!tryCatch({Listing_Of_Headers_con <- Main_Page %>% html_nodes("#mw-content-text h3 a") %>% html_attr("href"); TRUE}, error = function(x){FALSE}))
  {print(paste("Failed to extract TBL Headers for", gsub("_", " ", Series_Name)))}
  
  if(!tryCatch({Listing_Of_Headers_chld <- Main_Page %>% html_nodes(".mw-headline .text:nth-child(1)") %>% html_attr("href"); TRUE}, error = function(x){FALSE}))
  {print(paste("Failed to extract TBL Headers for", gsub("_", " ", Series_Name)))}
  
  duble <- unique(c(Listing_Of_Headers, Listing_Of_Headers_Spec, Listing_Of_Headers_TBL, Listing_Of_Headers_con))
  
  Links <- c()
  for(link in duble){
    if(grepl("/project/.*Volume", link)){
      Links <- c(Links, link)
    }
  }
  
  Links <- gsub("^", "https://www.baka-tsuki.org", Links)
  
  for(Link_To_Book in Links){
    tryCatch({
      session <- html_session(Link_To_Book);
      VOL <- gsub("\\D", "", gsub(".*Volume", "", Link_To_Book));
      Get_Text_Function(session, Series_Name, VOL);
      TRUE
    }, error = 
      function(x){print(paste("Failed to download Book ",
                              Series_Name,
                              " Vol number ",
                              gsub("\\D",
                                   "",
                                   gsub(".*Volume",
                                        "",
                                        Link_To_Book
    ))))})
  }
}

Get_Text_Function <- function(session, name, vol) {
  Text <- session %>% html_node("#content") %>% html_text()
  Parsed_Text <- c()
  Text <- gsub("\\[edit\\]", "", Text)
  len <- nchar(Text)
  b1 <- FALSE
  b2 <- FALSE
  b3 <- FALSE
  bX <- FALSE
  print(paste("Extracting ", gsub("_", " ", name), " Vol ", vol, sep = ""))
  while(Text != ""){
    Text_Line <- gsub("\\n.*$", "", Text)
    Text <- gsub("^[^\n]*\n", "", Text)
    #print(Text_Line)
    if(!grepl("^[\n|\t]*$", Text_Line)){
      Parsed_Text <- c(Parsed_Text, Text_Line)
    }
    
    End <- gsub("^[\t]*$", "", Text)
    if(End == ""){
      break()
    }
    if(!b1 && len*.75 > nchar(Text)){
      print("25%")
      b1 = TRUE
    }
    else if(!b2 && len*.5 > nchar(Text)){
      print("50%")
      b2 = TRUE
    }
    else if(!b3 && len*.25 > nchar(Text)){
      print("75%")
      b3 = TRUE
    }
  }
  print("100%")

  for (x  in 1:length(Parsed_Text)){
    Parsed_Text[x] <- gsub("%", "\\\\%", Parsed_Text[x])
    Parsed_Text[x] <- gsub("\\$", "\\\\$", Parsed_Text[x])
    Parsed_Text[x] <- gsub("#", "\\\\#", Parsed_Text[x])
    Parsed_Text[x] <- gsub("_", "\\\\_", Parsed_Text[x])
    Parsed_Text[x] <- gsub("\\[|\\]|\\(|\\)", "-", Parsed_Text[x])
    Parsed_Text[x] <- gsub("&", "\\\\&", Parsed_Text[x])
    Parsed_Text[x] <- gsub("$", "\\\\\\\\", Parsed_Text[x])
    if(x > 40){
      Parsed_Text[x] <- gsub("\\s*Prologue\\s*", " \\\\pagebreak\\\\\\\\\\\\textbf{Prologue}\\\\\\\\", Parsed_Text[x])
      if(grepl("\\s*Part\\s*\\d*", Parsed_Text[x])){
        Parsed_Text[x]<-gsub("^", "\\\\\\\\\\\\\\\\\\\\textbf{", gsub("$", "}\\\\\\\\", Parsed_Text[x]))
      }
      if(grepl("\\s*Chapter\\s*\\d*", Parsed_Text[x])){
        Parsed_Text[x]<-gsub("^", "\\\\pagebreak\\\\\\\\\\\\textbf{", gsub("$", "}\\\\\\\\", Parsed_Text[x]))
      }
    }
  }
  if(!(gsub("[^a-zA-Z0-9\\-\\_\\  ]", "", name) %in% dir())){dir.create( gsub("[^a-zA-Z0-9\\-\\_\\ ]", "", name))}
  setwd(gsub("[^a-zA-Z0-9\\-\\_\\  ]", "", name))
  Download_Call(name, Parsed_Text, vol)
  setwd("..")
}

urls <- c("https://www.baka-tsuki.org/project/index.php?title=CubexCursedxCurious",
          "https://www.baka-tsuki.org/project/index.php?title=Toradora!",
          "https://www.baka-tsuki.org/project/index.php?title=Ichiban_Ushiro_no_Daimaou",
          "https://www.baka-tsuki.org/project/index.php?title=Hikaru_ga_Chikyuu_ni_Itakoro......",
          "https://www.baka-tsuki.org/project/index.php?title=Maou_na_Ore_to_Fushihime_no_Yubiwa",
          "https://www.baka-tsuki.org/project/index.php?title=Silver_Cross_and_Draculea",
          "https://www.baka-tsuki.org/project/index.php?title=Ultimate_Antihero",
          "https://www.baka-tsuki.org/project/index.php?title=Ochitekita_Ryuuou_to_Horobiyuku_Majo_no_Kuni",
          "https://www.baka-tsuki.org/project/index.php?title=My_Vampire_Older_Sister_and_Zombie_Little_Sister",
          "https://www.baka-tsuki.org/project/index.php?title=Kamisama_no_Memochou",
          "https://www.baka-tsuki.org/project/index.php?title=The_World_God_Only_Knows",
          "https://www.baka-tsuki.org/project/index.php?title=Zero_no_Tsukaima",
          "https://www.baka-tsuki.org/project/index.php?title=The_Circumstances_Leading_to_Waltraute%27s_Marriage",
          "https://www.baka-tsuki.org/project/index.php?title=Clockwork_Planet",
          "https://www.baka-tsuki.org/project/index.php?title=Absolute_Duo",
          "https://www.baka-tsuki.org/project/index.php?title=High_School_DxD",
          "https://www.baka-tsuki.org/project/index.php?title=Rakuin_no_Monshou",
          "https://www.baka-tsuki.org/project/index.php?title=The_Weakness_of_Beatrice_the_Level_Cap_Holy_Swordswoman",
          "https://www.baka-tsuki.org/project/index.php?title=The_Unexplored_Summon_Blood_Sign",
          "https://www.baka-tsuki.org/project/index.php?title=Seikoku_no_Ryuu_Kishi",
          "https://www.baka-tsuki.org/project/index.php?title=Remembrances_for_a_certain_pilot",
          "https://www.baka-tsuki.org/project/index.php?title=Toaru_Majutsu_no_Index:_New_Testament",
          "https://www.baka-tsuki.org/project/index.php?title=Gakuen_Kino",
          "https://www.baka-tsuki.org/project/index.php?title=Shinmai_Maou_no_Keiyakusha",
          "https://www.baka-tsuki.org/project/index.php?title=Hyouka",
          "https://www.baka-tsuki.org/project/index.php?title=Tsuki_Tsuki!",
          "https://www.baka-tsuki.org/project/index.php?title=The_Reunion_With_Twelve_Fascinating_Goddesses",
          "https://www.baka-tsuki.org/project/index.php?title=Mushi_to_Medama",
          "https://www.baka-tsuki.org/project/index.php?title=Unlimited_Fafnir",
          "https://www.baka-tsuki.org/project/index.php?title=Masou_Gakuen_HxH",
          "https://www.baka-tsuki.org/project/index.php?title=Rokujouma_no_Shinryakusha!%3F",
          "https://www.baka-tsuki.org/project/index.php?title=Lillia_to_Treize",
          "https://www.baka-tsuki.org/project/index.php?title=Allison",
          "https://www.baka-tsuki.org/project/index.php?title=The_Longing_Of_Shiina_Ryo",
          "https://www.baka-tsuki.org/project/index.php?title=The_New_Gate",
          "https://www.baka-tsuki.org/project/index.php?title=Nogizaka_Haruka_no_Himitsu")

for (x in length(urls):1){
  Download_Files(urls[x])
}






#http://www.novelupdates.com/series-finder/?sf=1&cp=yes&sort=abc&order=asc

