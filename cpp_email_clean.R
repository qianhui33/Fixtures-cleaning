library(stringr)
library(stringi)
library(plyr)
library(dplyr)
library(readxl)

cargo_list<- read.csv("~/Desktop/ofe/data_clean/cargo.csv", head=FALSE,stringsAsFactors = FALSE)
cargo_list <- cargo_list[,1]
cargo_list <- gsub("\\+","\\\\+",cargo_list)
cargo1 <- paste0("( ",paste(cargo_list,collapse = " )|( ")," )")
cargo2 <- paste0("(",paste(cargo_list,collapse = ")|("),")")

fix_m <- 8

size <- "(P)?[0-9]{2,3}(\\.[0-9])?(KT)? |[0-9]{3}KB "
rate <- "RNR(([-/]RNR)+)?(([-/](WS|W|USD|U|LS)?( )?[0-9]+([.,][0-9]+)?(K|M)?)+)?( |$)|(WS|W|USD|U|LS)( )?[0-9]+([,.][0-9]+)?(K|M)?(([-/](WS|W|USD|U)?[0-9]+([.,][0-9]+)?(K|M)?)+)?(([-/]RNR)+)?|[0-9]+([.,][0-9]+)?(K|M)(([-/](WS|W|USD|U|LS)?[0-9]+([,.][0-9]+)?(K|M)?)+)?(([-/]RNR)+)?( |$)|PLATTS|PLATTA|OWN|COA|O/P"
laycan <- " (ELY|MID|END|([0-9]{1,2}-)?[0-9]{1,2})[-/ ]?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC) | (((ELY|MID|END|EL|EN)|([0-9]{1,2}-)?[0-9]{1,2})[-/][0-9]{1,2}(-[0-9]{1,2})?) | ([0-9]{1,2}[ /-](ELY|MID|END)) |DNR"
port <- "([A-Z]\\.)?[A-Z]+(((-|\\+|'|\\.| )[A-Z]+(\\.)?)+)?/(([A-Z]\\.)+)?[A-Z]+((( |-|\\+|'|\\.)([A-Z]\\.)?([A-Z]+)?)+)?"

##### Normal data-------------------
data <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "CPP", col_name = FALSE)
if(sum(is.na(data$X__2))>0) data <- data[-which(is.na(data$X__2)),]
data$X__2 <- gsub("\U00A0"," ",data$X__2)
data$X__2 <- gsub("\312|\\*|\\?|\\(2P\\)|/S |\\(.*\\)|<.*|/S |-( )?EX"," ",data$X__2)
data$X__2 <- gsub("\\s{2,}"," ",data$X__2)
data$X__2 <- gsub("\\+1P","",data$X__2)
data <- data[-which(sapply(data$X__2, nchar)<30),]
if (length(which(str_detect(data$X__2, "T/C")))>0) data <- data[-which(str_detect(data$X__2, "T/C")),]
if (length(which(str_detect(data$X__2, "MONTHS|DELIVERY|FORWARDED|FAIL")))>0) data <- data[-which(str_detect(data$X__2,"MONTHS|DELIVERY|FORWARDED|FAIL")),]
if (length(which(!str_detect(data$X__2, "[0-9]")))>0) data <- data[-which(!str_detect(data$X__2,"[0-9]")),]
data$X__2 <- toupper(data$X__2)

clean <- data.frame(Name = "", stringsAsFactors = FALSE)
clean$cargo_size <- ""
clean$cargo <- ""
clean$load_port <- ""
clean$discharge_port <- ""
clean$laycan <- ""
clean$rate<- ""
clean$charterer <- ""
clean$comments <- ""
clean$source <- ""
nosize <- ""
nocargo <- ""
norate <- ""
nolaycan <- ""
noport <- ""

for (i in 1:length(data$X__2)){
  if (str_detect(data$X__2[i], size)){
      sizep <- str_locate_all(data$X__2[i], size)[[1]]
      k <- substr(data$X__2[i], sizep[1,"start"],sizep[1,"end"])
    if(str_detect(k, "K")|str_detect(k, "P")) p <- 1
    else if (as.numeric(k) >= 15){
      if(str_detect(data$X__2[i], "^FPMC [0-9]|^FORMOSA [0-9]")) p <- 2
      else p <- 1
    }
    else p <- 2
    clean[i,2] <- substr(data$X__2[i], sizep[p,"start"],sizep[p,"end"])
  }
  else {
    sizep <- str_locate_all(data$X__2[i], "[0-9]{2}")[[1]]
    p <- 1
    clean[i,2] <- substr(data$X__2[i], sizep[p,"start"],sizep[p,"end"])
    nosize <- c(nosize, i)
  }
  if (str_detect(data$X__2[i], cargo1)){
    cargop <- str_locate_all(data$X__2[i], cargo1)[[1]]
    clean[i,3] <- substr(data$X__2[i], cargop[1,"start"],cargop[1,"end"])
  }
  else if (str_detect(data$X__2[i], cargo2)){
    cargop <- str_locate_all(data$X__2[i], cargo2)[[1]]
    clean[i,3] <- substr(data$X__2[i], cargop[1,"start"],cargop[1,"end"])
  }
  else{
    nocargo <- c(nocargo, i)
    clean[i,3] <- NA
  }
  if (sizep[p,"start"] < cargop[1,"start"])  clean[i,1] <- substr(data$X__2[i], 1, (sizep[p,"start"]-1))
  else if (sizep[p,"start"] > cargop[1,"start"])clean[i,1] <- substr(data$X__2[i], 1, (cargop[1,"start"]-1))
  else clean[i,1] <- paste(str_split(data$X__2[i]," ")[[1]][1:2], collapse = " ")
  if (str_detect(data$X__2[i], rate)){
    ratep <- str_locate_all(data$X__2[i], rate)[[1]]
    clean[i,7] <- substr(data$X__2[i], ratep[nrow(ratep),"start"],ratep[nrow(ratep),"end"])
    clean[i,8] <- substr(data$X__2[i], ratep[nrow(ratep),"end"]+1,nchar(data$X__2[i]))
  }
  else{
    norate <- c(norate, i)
    clean[i,7] <- NA
  }
  if (str_detect(data$X__2[i], laycan)){
    layp <- str_locate_all(data$X__2[i], laycan)[[1]]
    clean[i,6] <- substr(data$X__2[i], layp[1,"start"],layp[1,"end"])
  }
  else {
    nolaycan <- c(nolaycan, i)
    clean[i,6] <- NA
  }
  if (str_detect(data$X__2[i], port)){
    portp <- str_locate_all(data$X__2[i], port)[[1]]
    if (portp[1,"end"] < cargop[1,"start"])  k <- 2
    else k <- 1
    start <- max(cargop[1,"end"] + 1, portp[k, "start"])
    end <- min(portp[k,"end"], ratep[nrow(ratep),"start"]-1)
    clean[i,4] <- substr(data$X__2[i], start,end)
  }
  else {
    noport <- c(noport, i)
    if (str_detect(data$X__2[i], laycan)&(str_detect(data$X__2[i], rate))&(str_detect(data$X__2[i], cargo1))){
      clean[i,4] <- paste0(substr(data$X__2[i],cargop[1,"end"]+1,layp[1,"start"]-1),substr(data$X__2[i],layp[1,"end"]+1,ratep[1,"start"]-1))
    }
  }
  clean$source[i] <- data$X__1[i]
}

noport <- as.numeric(noport[-1])
noport
nolaycan <- as.numeric(nolaycan[-1])
nolaycan
norate <- as.numeric(norate[-1])
norate
nocargo <- as.numeric(nocargo[-1])
nocargo
clean$load_port <- gsub("^MID |^ELY ", "", clean$load_port)
port_spl <- str_split(clean$load_port, "/")
clean$load_port <- sapply(port_spl, "[[",1)
if (length(noport)==0) {clean$discharge_port <- sapply(port_spl, "[[",2)}else
  {clean$discharge_port[-noport] <- sapply(port_spl[-noport], "[[",2)}
clean$discharge_port <- gsub("RNR|USD|CNR|WS", "", clean$discharge_port)
clean$load_port <- gsub("^(MID |END |ELY )?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC) ", "", clean$load_port)
if(length(nolaycan)>0) clean <- clean[-nolaycan,]
clean$cargo_size <- gsub("KT", "", clean$cargo_size)


#### Encore 1-----------
data1 <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "ENCORE", col_name = FALSE)
#data1 <- read.csv("~/Desktop/ofe/cpp_29_jun.csv", head=FALSE, stringsAsFactors = FALSE)
data1 <- data1$X__1
if(sum(is.na(data1))>0) data1 <- data1[-which(is.na(data1))]
data1 <- gsub("\U00A0"," ",data1)
data1 <- gsub("\312|\\s{2,}|\\(.*\\)"," ",data1)
data1 <- gsub("]\\s{2,}]"," ",data1)
if (length(which(sapply(data1, nchar)<35))>0) data1 <- data1[-which(sapply(data1, nchar)<35)]
if (length(which(str_detect(data1, "T/C")))>0) data1 <- data1[-which(str_detect(data1, "T/C"))]
if (length(which(str_detect(data1, "MONTHS|DELIVERY|FORWARDED|ON HOLD")))>0) data1 <- data1[-which(str_detect(data1,"MONTHS|DELIVERY|FORWARDED|ON HOLD"))]
if (length(which(!str_detect(data1, "[0-9]")))>0) data1 <- data1[-which(!str_detect(data1,"[0-9]"))]
data1 <- toupper(data1)

clean1 <- clean[1,][-1,]
nosize <- ""
nocargo <- ""
norate <- ""
nolaycan <- ""
noport <- ""

for (i in 1:length(data1)){
  if (str_detect(data1[i], size)){
    sizep <- str_locate_all(data1[i], size)[[1]]
    clean1[i,2] <- substr(data1[i], sizep[1,"start"],sizep[1,"end"])
    clean1[i,8] <- substr(data1[i], 1,sizep[1,"start"]-1)
  }
  else {
    sizep <- str_locate_all(data1[i], "[0-9]{2}")[[1]]
    clean1[i,2] <- substr(data1[i], sizep[1,"start"],sizep[1,"end"])
    nosize <- c(nosize, i)
  }
  if (str_detect(data1[i], cargo1)){
    cargop <- str_locate_all(data1[i], cargo1)[[1]]
    clean1[i,3] <- substr(data1[i], cargop[1,"start"],cargop[1,"end"])
  }
  else if (str_detect(data1[i], cargo2)){
    cargop <- str_locate_all(data1[i], cargo2)[[1]]
    clean1[i,3] <- substr(data1[i], cargop[1,"start"],cargop[1,"end"])
  }
  else{
    nocargo <- c(nocargo, i)
    clean1[i,3] <- NA
  }
  
  if (str_detect(data1[i], rate)){
    ratep <- str_locate_all(data1[i], rate)[[1]]
    clean1[i,7] <- substr(data1[i], ratep[nrow(ratep),"start"],ratep[nrow(ratep),"end"])
    clean1[i,9] <- substr(data1[i], ratep[nrow(ratep),"end"]+1, nchar(data1[i]))
  }
  else{
    norate <- c(norate, i)
    clean1[i,7] <- NA
  }
  if (str_detect(data1[i], laycan)){
    layp <- str_locate_all(data1[i], laycan)[[1]]
    clean1[i,6] <- substr(data1[i], layp[1,"start"],layp[1,"end"])
  }
  else {
    nolaycan <- c(nolaycan, i)
    clean1[i,6] <- NA
  }
  clean1[i,1] <- substr(data1[i], layp[1,"end"]+1, ratep[nrow(ratep),"start"]-1)
  clean1[i,4] <- substr(data1[i], cargop[1,"end"]+1,layp[1,"start"]-1)
}

noport <- which(!str_detect(clean1$load_port, "/"))
nolaycan <- as.numeric(nolaycan[-1])
nocargo <- as.numeric(nocargo[-1])
norate <- as.numeric(norate[-1])
clean1$Name <- gsub("^-", "", clean1$Name)
clean1$load_port <- gsub("-$", "", clean1$load_port)
port_spl <- str_split(clean1$load_port, "/")
clean1$load_port <- sapply(port_spl, "[[",1)
clean1$source <-"C"
if (length(noport)==0) {clean1$discharge_port <- sapply(port_spl, "[[",2)}else
{clean1$discharge_port[-noport] <- sapply(port_spl[-noport], "[[",2)}
if(length(nolaycan)>0) clean1 <- clean1[-nolaycan,]


#### CPP HANS 2-------------------
data2 <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "HANS", col_name = FALSE,)
if(sum(is.na(data2[,5]))>0) data2 <- data2[-which(is.na(data2[,5])),]

clean2 <- cbind(data2[,2],data2[,3:10], data2[,1])
names(clean2) <- names(clean)
ai <- which(clean2$source == "AI"|clean2$source == "AR")
if(length(ai)>0){
  k1_ai <- ai[which(str_detect(clean2$laycan[ai] ,"^4[0-9]+"))]
  k2_ai <- ai[which(str_detect(clean2$laycan[ai] ,"^[0-9]{1,2}(-[0-9]{1,2})?$"))]
  clean2$laycan[k1_ai] <- paste0(format(as.Date(as.numeric(clean2$laycan[k1_ai]),origin = "1899-12-30"), "%d-%m"),"/", fix_m)
  clean2$laycan[k2_ai] <- paste0(clean2$laycan[k2_ai], "/",fix_m)
}
at <- which(clean2$source == "AT")
if(length(at)>0){
  at_lay <- as.Date(as.numeric(clean2$laycan[at]),origin = "1899-12-30")
  for(i in 1:length(at)){
    m <- as.numeric(format(at_lay[i], "%m"))
    d <- as.numeric(format(at_lay[i], "%d"))
    if(d == fix_m) clean2$laycan[at[i]] <- format(at_lay[i], "%m/%d")
    else if(m == fix_m) clean2$laycan[at[i]] <- format(at_lay[i], "%y/%m")
    else if(d == fix_m+1) clean2$laycan[at[i]] <- format(at_lay[i], "%m/%d")
    else if(m == fix_m+1) clean2$laycan[at[i]] <- format(at_lay[i], "%y/%m")
  }
}

k1 <- which(str_detect(clean2$laycan ,"^4[0-9]+"))
clean2$laycan[k1] <- as.character(as.Date(as.numeric(clean2$laycan[k1]),origin = "1899-12-30"))
k2 <- which(str_detect(clean2$laycan ,"^3[0-9]+$"))
clean2$laycan[k2] <- format(as.Date(as.numeric(clean2$laycan[k2]),origin = "1899-12-30"), "%d-%m/%y")
clean2$laycan <- as.character((clean2$laycan))
if(sum(clean2$laycan == "LayCan")>0) clean2 <- clean2[-which(clean2$laycan == "LayCan"),]

k_cg <- which(str_detect(clean2$cargo_size, " [A-Z]"))
clean2$cargo[k_cg] <- sapply(str_split(clean2$cargo_size[k_cg], " "), "[[", 2)
clean2$cargo_size[k_cg] <- gsub("[A-Z]+", "", clean2$cargo_size[k_cg])

if(any(str_detect(clean2$charterer, "W[0-9]+"))){
  source_s <- which(clean2$source == unique(clean2$source[which(str_detect(clean2$charterer, "W[0-9]+"))]))
  rate_s <- clean2$charterer[source_s]
  clean2$charterer[source_s] <- clean2$rate[source_s]
  clean2$rate[source_s] <- rate_s
}


##### CPP REUTERS 4-----------------
data4 <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "RT", col_name = FALSE)
if(sum(is.na(data4[,2]))>0) data4 <- data4[-which(is.na(data4[,2])),]
k1 <- which(is.na(data4[,11]))
data4[k1,11] <- data4[k1,10]
k2 <- which(is.na(data4[,13]))
data4[k2,13] <- data4[k2,12]
data4$X__15[which(is.na(data4$X__15))] <- ""
rate_r <- paste0(data4$X__14, data4$X__15)
clean4 <- cbind(data4[,2],data4[,7],data4[,9],data4[,11],data4[,13],data4[,16],rate_r,data4[,1], data4[,17],"N")
names(clean4) <- names(clean)
if(sum(is.na(clean4$cargo_size))>0) clean4 <- clean4[-which(is.na(clean4$cargo_size)),]
clean4$cargo_size <- as.numeric(clean4$cargo_size)/1000
clean4$laycan <- as.character(clean4$laycan)

# #### CPP HR 5--------------
# data6 <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "HR", col_name = FALSE)
# if(sum(is.na(data6[,2]))>0) data6 <- data6[-which(is.na(data6[,2])),]
# rate_h <- paste0(data6$X__8, data6$X__9)
# clean5 <- cbind(data6[,1],data6[3:7],rate_h,data6[,2],data6[,10],"E")
# names(clean5) <- names(clean)
# k1 <- which(str_detect(clean5$laycan ,"^4[0-9]+")) 
# clean5$laycan[k1] <- as.character(as.Date(as.numeric(clean5$laycan[k1]),origin = "1899-12-30"))
# k2 <- which(str_detect(clean5$laycan ,"^3[0-9]+"))
# clean5$laycan[k2] <- format(as.Date(as.numeric(clean5$laycan[k2]),origin = "1899-12-30"), "%d-%m/%y")

#### CPP GB 6-------------------
data6 <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "GB", col_name = FALSE)
data6 <- data6$X__1
data6 <- gsub("\U00A0"," ",data6)
data6 <- gsub("WEST AFRICA", "WEST AFRICA  ", data6)
data6 <- gsub("ARABIAN GULF", "ARABIAN GULF  ", data6)
data6 <- gsub("\\s{2,}", "  ", data6)
rplc <- which(str_detect(data6, "REPLACE"))
data6[rplc-1] <- paste0(data6[rplc-1], " RPLC")
fld <- which(str_detect(data6, "FAIL"))
data6[fld-1] <- paste0(data6[fld-1], " FLD")
data6 <- data6[-which(sapply(data6, nchar)<30&!str_detect(data6,rate))]
if(sum(is.na(data6))>0) data6 <- data6[-which(is.na(data6))]
data6 <- toupper(data6) %>% str_trim
clean6 <- clean[1,][-1,]
nosize <- ""
nocargo <- ""
norate <- ""
nolaycan <- ""
noport <- ""
len <- sapply(str_split(data6, "  "),length)

laycan1 <- "[0-9]+(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)|-DNR-"

i <- 0
for (m in 1:length(data6)){
  if(len[m]>3){
    i <- i+1
    if (str_detect(data6[m], size)){
      sizep <- str_locate_all(data6[m], size)[[1]]
      clean6[i,2] <- substr(data6[m], sizep[1,"start"],sizep[1,"end"])
      clean6[i,1] <- substr(data6[m], 1,sizep[1,"start"]-1)
    }
    else {
      nosize <- c(nosize, m)
      clean6[i,2] <- NA
    }
    if (str_detect(data6[m], cargo1)){
      cargop <- str_locate_all(data6[m], cargo1)[[1]]
      clean6[i,3] <- substr(data6[m], cargop[1,"start"],cargop[1,"end"])
    }
    else if (str_detect(data6[m], cargo2)){
      cargop <- str_locate_all(data6[m], cargo2)[[1]]
      clean6[i,3] <- substr(data6[m], cargop[1,"start"],cargop[1,"end"])
    }
    else{
      nocargo <- c(nocargo, m)
      clean6[i,3] <- NA
    }
    if (str_detect(data6[m], laycan1)){
      layp <- str_locate_all(data6[m], laycan1)[[1]]
      clean6[i,6] <- substr(data6[m], layp[1,"start"],layp[1,"end"])
    }
    else {
      nolaycan <- c(nolaycan, m)
      clean6[i,6] <- NA
    }
    if (str_detect(data6[m], rate)|str_detect(data6[m], " [0-9]+(\\.[0-9]+)? ")){
      if(str_detect(data6[m], rate)) ratep <- str_locate_all(data6[m], rate)[[1]]
      else{
        norate <- c(norate, m)
        ratep <- str_locate_all(data6[m], " [0-9]+(\\.[0-9]+)? ")[[1]]
      } 
      clean6[i,7] <- substr(data6[m], ratep[nrow(ratep),"start"],ratep[nrow(ratep),"end"])
      clean6[i,8] <- substr(data6[m], ratep[nrow(ratep),"end"]+1,nchar(data6[m]))
      if (str_detect(data6[m], laycan1)){
        clean6[i,4] <- substr(data6[m], layp[1,"end"]+1, ratep[nrow(ratep),"start"]-1)
        clean6[i,4] <- str_trim(clean6[i,4])
        if(str_detect(clean6[i,4], "  ")){
          clean6[i,5] <- str_split(clean6[i,4], "  ")[[1]][2]
          clean6[i,4] <- gsub("  .*", "", clean6[i,4])
        }
        else{
          ld_port <- str_split(clean6[i,4], " ")[[1]]
          clean6[i,5] <-ld_port[length(ld_port)]
          clean6[i,4] <- gsub(clean6[i,5], "",clean6[i,4])
        }
      } 
    }
    else{
      norate <- c(norate, m)
      clean6[i,7] <- NA
    }
  }
  else{
    add_d <- str_split(data6[m], "  ")[[1]]
    clean6[i,7] <- paste0(clean6[i,7], "/",add_d[length(add_d)])
    clean6[i,5] <- paste0(clean6[i,5], "-",add_d[length(add_d)-1])
  }
}
clean6$source <- "R"
noport <- as.numeric(noport[-1])
noport
nolaycan <- as.numeric(nolaycan[-1])
nolaycan
norate <- as.numeric(norate[-1])
norate
nocargo <- as.numeric(nocargo[-1])
nocargo
if(length(nolaycan)>0) clean6 <- clean6[-which(is.na(clean6$laycan)),]

####FT 7-------------------
data8 <- read_excel("~/Desktop/ofe/CPP 27 AUG.xlsx", sheet = "FT", col_name = FALSE)
data8 <- data8$X__1
on_na <- which(str_detect(data8, "ON SUBS|ON HOLD")|is.na(data8))
if(length(on_na)>0) data8 <- data8[-on_na]
for(i in 1:12) data8 <- gsub(toupper(month.name)[i], toupper(month.abb)[i], data8)

size <- "-( )?[0-9]+ "

clean7 <- clean[1,][-1,]
nosize <- ""
nocargo <- ""
norate <- ""
nolaycan <- ""
noport <- ""
for (i in 1:length(data8)){
  namep <- str_locate(data8[i], "/")
  clean7[i,1] <- substr(data8[i], 1, namep[1,1]-1)
  if (str_detect(data8[i], size)){
    sizep <- str_locate_all(data8[i], size)[[1]]
    clean7[i,2] <- substr(data8[i], sizep[1,"start"],sizep[1,"end"])
    clean7$charterer[i] <- substr(data8[i], namep[1,1]+1, sizep[1,"start"]-1)
  }
  else  nosize <- c(nosize, i)
  if (str_detect(data8[i], laycan)){
    layp <- str_locate_all(data8[i], laycan)[[1]]
    clean7[i,6] <- substr(data8[i], layp[nrow(layp),"start"],layp[nrow(layp),"end"])
  }
  else {
    nolaycan <- c(nolaycan, i)
    clean7[i,6] <- NA
  }
  if (str_detect(data8[i], cargo1)){
    cargop <- str_locate_all(data8[i], cargo1)[[1]]
    clean7[i,3] <- substr(data8[i], cargop[1,"start"],cargop[1,"end"])
    if(str_detect(data8[i], laycan)) clean7[i,4] <- substr(data8[i], cargop[1,"end"]+1, layp[nrow(layp),"start"]-1)
  }
  else if (str_detect(data8[i], cargo2)){
    cargop <- str_locate_all(data8[i], cargo2)[[1]]
    if(cargop[1,"end"]>sizep[1,"end"]){
      clean7[i,3] <- substr(data8[i], cargop[1,"start"],cargop[1,"end"])
      if(str_detect(data8[i], laycan)) clean7[i,4] <- substr(data8[i], cargop[1,"end"]+1, layp[1,"start"]-1)
    }
    else{
      nocargo <- c(nocargo)
      clean7[i,4] <- substr(data8[i], sizep[1,"end"]+1, layp[1,"start"]-1)
    } 
  }
  else{
    nocargo <- c(nocargo, i)
    clean7[i,3] <- NA
    clean7[i,4] <- substr(data8[i], sizep[1,"end"]+1, layp[1,"start"]-1)
  }
  if (str_detect(data8[i], rate)){
    ratep <- str_locate_all(data8[i], rate)[[1]]
    clean7[i,7] <- substr(data8[i], ratep[nrow(ratep),"start"],ratep[nrow(ratep),"end"])
    clean7[i,9] <- substr(data8[i], ratep[nrow(ratep),"end"]+1,nchar(data8[i]))
  }
  else{
    norate <- c(norate, i)
    clean7[i,7] <- NA
  }
}
nolaycan <- as.numeric(nolaycan[-1])
norate <- as.numeric(norate[-1])
nocargo <- as.numeric(nocargo[-1])
clean7$source <- "T"
if(length(nolaycan)>0) clean7 <- clean7[-nolaycan,]
noport <- which(!str_detect(clean7$load_port, "/"))
port_spl <- str_split(clean7$load_port, "/")
clean7$load_port <- sapply(port_spl, "[[",1)
if (length(noport)==0) {clean7$discharge_port <- sapply(port_spl, "[[",2)}else
{clean7$discharge_port[-noport] <- sapply(port_spl[-noport], "[[",2)}

###Combine all--------------
clean_all <- rbind(clean, clean4, clean2)
clean_all$comments[which(is.na(clean_all$comments))] <- ""
if(sum(which(clean_all$Name == "VESSEL"))>0) clean_all <- clean_all[-which(clean_all$Name == "VESSEL")]
test <- as.data.frame(clean_all[,1:2])
test[,3] <- "-"
test[,4:5] <- clean_all[,3:4]
test[,6:7] <- ""
test[,8] <- clean_all[,5]
test[,9:10] <- ""
test[,11:14] <- clean_all[,6:9]
test[,15] <- "2018-08-27"
test[,16] <- fix_m
test[,17] <- clean_all[,10]

