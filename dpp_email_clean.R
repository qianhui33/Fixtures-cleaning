library(stringr)
library(plyr)
library(dplyr)
library(readxl)
options(scipen=999) # remove scientific notation

size <- " [0-9]{3}KB | [0-9]{2}KT | [0-9]{2,3}(\\.[0-9])? |[0-9]{2,3}(\\.[0-9])?FO | RT "
rate <- "RNR(([-/]RNR)+)?(([-/](WS|W|USD|\\$|U)?( )?[0-9]+([,.][0-9]+)?( )?(K[ $]|M |MILL|LVL)?)+)?|(WS|W|USD|\\$|U)( )?[0-9]+([.,][0-9]+)?( )?(K[ $]|M |MILL|LVL)?(([-/](WS|W|USD|\\$)?( )?[0-9]+([,.][0-9]+)?( )?(K |M |MILL|LVL)?)+)?(([-/]RNR)+)?|[0-9]+([,.][0-9]+)?( )?(K[ $]?|M |MILL|LVL)(([-/](WS|W|USD|U|\\$)?( )?[0-9]+([.,][0-9]+)?( )?(K |M |MILL|LVL)?)+)?(([-/]RNR)+)?( PD)?|PLATTS|OWN|COA|O/P|RNR|TD20|R N R"
laycan <- " ((ELY|MID|END|([0-9]{1,2}-)?[0-9]{1,2})[-/ ]?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC))| (((ERLY|ELY|MID|END)|([0-9]{1,2}-)?[0-9]{1,2})[-/][0-9]{1,2}(-[0-9]{1,2})?) |DNR|PPT"
port <- "([A-Z]\\.)?[A-Z]+(((-|\\+|'|\\.| )[A-Z]+)+)?(\\+1)?( )?/(([A-Z]\\.)+)?[A-Z]+((( |-|\\+|'|\\.)([A-Z]\\.)?([A-Z]+)?)+)?"

fix_m <- 8

#### Create dataset test ####
data3 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "DPP", col_name = FALSE)
k_l_port <- which(!is.na(data3$X__4)&is.na(data3$X__2))
if(length(k_l_port)>0){
  for(i in 1:length(k_l_port)){
    if((k_l_port[i]-1) %in% k_l_port) data3$X__4[k_l_port[i]-2] <- paste0(data3$X__4[k_l_port[i]-2], "-", data3$X__4[k_l_port[i]])
    else data3$X__4[k_l_port[i]-1] <- paste0(data3$X__4[k_l_port[i]-1], "-", data3$X__4[k_l_port[i]])
  } 
}

k_d_port <- which(!is.na(data3$X__5)&is.na(data3$X__2))
if(length(k_d_port)>0){
  for(i in 1:length(k_d_port)){
    if((k_d_port[i]-1) %in% k_d_port) data3$X__5[k_d_port[i]-2] <- paste0(data3$X__5[k_d_port[i]-2], "-", data3$X__5[k_d_port[i]])
    else data3$X__5[k_d_port[i]-1] <- paste0(data3$X__5[k_d_port[i]-1], "-", data3$X__5[k_d_port[i]])
  }
}

k_char <- which(!is.na(data3$X__7)&is.na(data3$X__2))
if(length(k_char)>0){
  for(i in 1:length(k_char)){
    if(!is.na(data3$X__8[k_char[i]])) data3$X__8[k_char[i]-1] <- data3$X__8[k_char[i]]
  }
  for(i in 1:length(k_char)){
    if((k_char[i]-1) %in% k_char) data3$X__7[k_char[i]-2] <- paste0(data3$X__7[k_char[i]-2], "/", data3$X__7[k_char[i]])
    else{
      data3$X__7[k_char[i]-1] <- paste0(data3$X__7[k_char[i]-1], "/", data3$X__7[k_char[i]])
      if(!is.na(data3$X__8[k_char[i]])) data3$X__8[k_char[i]-1] <- data3$X__8[k_char[i]]
    }
  }
}


if(sum(is.na(data3[,6]))>0) data3 <- data3[-which(is.na(data3[,6])),]
if(ncol(data3) == 8) data3[,9] <- ""
if(ncol(data3) == 9) data3[,10] <- ""
clean <- cbind(data3[,2:3], data3[,10], data3[,4:9], data3[,1],"","", stringsAsFactors = FALSE)
names(clean) <- c("name", "cargo_size","cargo","load_port","discharge_port", "laycan","rate",
                  "charterer","comments","source","type", "laycan_end")

k1 <-which(str_detect(clean$laycan, "^4[0-9]+$"))
clean$laycan[k1] <- as.character(as.Date(as.numeric(clean$laycan[k1]),origin = "1899-12-30"))
k2 <-which(str_detect(clean$laycan, "^3[0-9]+$"))
clean$laycan[k2] <- format(as.Date(as.numeric(clean$laycan[k2]),origin = "1899-12-30"), "%d-%m/%y")

clean$cargo_size <- gsub(" KB", "KB", clean$cargo_size)
clean$cargo_size <- gsub("\\(|\\)", " ", clean$cargo_size)
if(length(which(clean$name == "Vessel"|clean$name=="VESSEL"))>0) clean <- clean[-which(clean$name == "Vessel"|clean$name=="VESSEL"),]
k <- which(str_detect(clean$cargo_size, " [A-Z]"))
size_spl <- str_split(clean$cargo_size[k], " ")
clean$cargo_size[k] <- sapply(size_spl, "[[",1)
clean$cargo[k] <- sapply(size_spl, "[[",2) 
clean$cargo[which(clean$comments == "FO")] <- "FO"

if(any(str_detect(clean$charterer, "W[0-9]+"))){
  source_s <- which(clean$source == unique(clean$source[which(str_detect(clean$charterer, "W[0-9]+"))]))
  rate_s <- clean$charterer[source_s]
  clean$charterer[source_s] <- clean$rate[source_s]
  clean$rate[source_s] <- rate_s
}


#### Encore 1-----------
data2 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "ENCORE", col_name = FALSE)
#data1 <- read.csv("~/Desktop/ofe/cpp_30_jun.csv", head=FALSE, stringsAsFactors = FALSE)
data2 <- data2$X__1
if(sum(is.na(data2))>0) data2 <- data2[-which(is.na(data2))]
data2 <- gsub("\U00A0"," ",data2)
data2 <- gsub("\312|\\s{2,}|\\(.*\\)"," ",data2)
data2 <- gsub("]\\s{2,}]"," ",data2)
if (length(which(sapply(data2, nchar)<35))>0) data2 <- data2[-which(sapply(data2, nchar)<35)]
if (length(which(str_detect(data2, "T/C")))>0) data2 <- data2[-which(str_detect(data2, "T/C"))]
if (length(which(str_detect(data2, "MONTHS|DELIVERY|FORWARDED|ON HOLD")))>0) data2 <- data2[-which(str_detect(data2,"MONTHS|DELIVERY|FORWARDED|ON HOLD"))]
if (length(which(!str_detect(data2, "[0-9]")))>0) data2 <- data2[-which(!str_detect(data2,"[0-9]"))]
data2 <- toupper(data2)

clean1 <- clean[1,][-1,]
nosize <- ""
nocargo <- ""
norate <- ""
nolaycan <- ""
noport <- ""

for (i in 1:length(data2)){
  if (str_detect(data2[i], size)){
    sizep <- str_locate_all(data2[i], size)[[1]]
    clean1[i,2] <- substr(data2[i], sizep[1,"start"],sizep[1,"end"])
    clean1[i,8] <- substr(data2[i], 1,sizep[1,"start"]-1)
  }
  else {
    sizep <- str_locate_all(data2[i], "[0-9]{2}")[[1]]
    clean1[i,2] <- substr(data2[i], sizep[1,"start"],sizep[1,"end"])
    nosize <- c(nosize, i)
  }
  if (str_detect(data2[i], cargo1)){
    cargop <- str_locate_all(data2[i], cargo1)[[1]]
    clean1[i,3] <- substr(data2[i], cargop[1,"start"],cargop[1,"end"])
  }
  else if (str_detect(data2[i], cargo2)){
    cargop <- str_locate_all(data2[i], cargo2)[[1]]
    clean1[i,3] <- substr(data2[i], cargop[1,"start"],cargop[1,"end"])
  }
  else{
    nocargo <- c(nocargo, i)
    clean1[i,3] <- NA
  }
  
  if (str_detect(data2[i], rate)){
    ratep <- str_locate_all(data2[i], rate)[[1]]
    clean1[i,7] <- substr(data2[i], ratep[nrow(ratep),"start"],ratep[nrow(ratep),"end"])
    clean1[i,9] <- substr(data2[i], ratep[nrow(ratep),"end"]+1, nchar(data2[i]))
  }
  else{
    norate <- c(norate, i)
    clean1[i,7] <- NA
  }
  if (str_detect(data2[i], laycan)){
    layp <- str_locate_all(data2[i], laycan)[[1]]
    clean1[i,6] <- substr(data2[i], layp[1,"start"],layp[1,"end"])
  }
  else {
    nolaycan <- c(nolaycan, i)
    clean1[i,6] <- NA
  }
  clean1[i,1] <- substr(data2[i], layp[1,"end"]+1, ratep[nrow(ratep),"start"]-1)
  clean1[i,4] <- substr(data2[i], cargop[1,"end"]+1,layp[1,"start"]-1)
}

noport <- which(!str_detect(clean1$load_port, "/"))
nolaycan <- as.numeric(nolaycan[-1])
nocargo <- as.numeric(nocargo[-1])
norate <- as.numeric(norate[-1])
clean1$name <- gsub("^-", "", clean1$name)
clean1$load_port <- gsub("-$", "", clean1$load_port)
port_spl <- str_split(clean1$load_port, "/")
clean1$load_port <- sapply(port_spl, "[[",1)
if (length(noport)==0) {clean1$discharge_port <- sapply(port_spl, "[[",2)}else
{clean1$discharge_port[-noport] <- sapply(port_spl[-noport], "[[",2)}
if(length(nolaycan)>0) clean1 <- clean1[-nolaycan,]



#### HR 2------------
data2 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "HR", col_name = FALSE)
if(sum(is.na(data2$X__2))>0) data2 <- data2[-which(is.na(data2$X__2)),]
data2$X__2 <- toupper(data2$X__2)
data2$X__2 <- gsub("\U00A0", " ",data2$X__2)
data2$X__2 <- gsub(" P/C |\\(P/C\\)", " PTC ", data2$X__2)
data2$X__2 <- gsub("\312|=|\\[.*\\]|\\([0-9]+(/[0-9]+)?\\)"," ",data2$X__2)
data2$X__2 <- gsub("\\s{2,}","  ",data2$X__2)
data2$X__2 <- gsub("R N R", "RNR",data2$X__2)
data2$X__2 <- gsub("EARLY", "ELY",data2$X__2)
data2$X__2 <- gsub(" O/O", "OOS",data2$X__2)
data2$X__2 <- gsub("WS |W ", "WS",data2$X__2)
if (length(which(sapply(data2$X__2, nchar)<=35))>0) data2 <- data2[-which(sapply(data2$X__2, nchar)<=35),]
if (length(which(str_detect(data2$X__2, "T/C")))>0) data2 <- data2[-which(str_detect(data2$X__2, "T/C")),]
if (length(which(str_detect(data2$X__2, "MONTHS|DELIVERY|FORWARDED|ON HOLD")))>0) data2<- data2[-which(str_detect(data2$X__2,"MONTHS|DELIVERY|FORWARDED|ON HOLD")),]
if (length(which(!str_detect(data2$X__2, "[0-9]")))>0) data2 <- data2[-which(!str_detect(data2$X__2,"[0-9]")),]
if (length(which(str_detect(data2$X__2, "FAILED|FIXED ON SUBS|NO DETAILS|NO DETS")))>0) data2 <- data2[-which(str_detect(data2$X__2,"FAILED|FIXED ON SUBS|NO DETAILS|NO DETS")),]

clean2 <- clean[1,][-1,]
nosize <- ""
norate <- ""
nolaycan <- ""
noport <- ""
cargo <- " NHC |-FO-|-HC-| FO |-COND-|-CBFS-"

for (i in 1:nrow(data2)){
  if (str_detect(data2$X__2[i], size)){
    if (str_locate_all(data2$X__2[i],size)[[1]][1,2] < nchar(data2$X__2[i])/1.5)  sizep <- str_locate_all(data2$X__2[i], size)[[1]]
    else sizep <- str_locate_all(data2$X__2[i], "[0-9]{2,} ")[[1]]
    ns <- 1
    if(substr(data2$X__2[i], sizep[1,"start"],sizep[1,"end"]) == " 19 " & str_detect(data2$X__2[i], "^FPMC")) ns <- 2
    clean2[i,2] <- substr(data2$X__2[i], sizep[ns,"start"],sizep[ns,"end"])
    clean2[i,1] <- substr(data2$X__2[i], 1,sizep[ns,"start"]-1)
  }
  else if (str_detect(data2$X__2[i], " [0-9]{2,}|[0-9]{2,} ")){
    sizep <- str_locate_all(data2$X__2[i], " [0-9]{2,}|[0-9]{2,} ")[[1]]
    clean2[i,2] <- substr(data2$X__2[i], sizep[1,"start"],sizep[1,"end"])
    clean2[i,1] <- substr(data2$X__2[i], 1,sizep[1,"start"]-1)
  }
  else{
    nosize <- c(nosize, i)
    if(!str_detect(data2$X__2[i], laycan)) next
  } 

  if (str_detect(data2$X__2[i], rate)& data2$X__1[i] != "AC"){
    ratep <- str_locate_all(data2$X__2[i], rate)[[1]]
    if(nrow(ratep)==1) nr <- 1
    else if(nrow(ratep)==2){
      if(nchar(data2$X__2[i])-ratep[2,"end"]<3) nr <- 1
      else nr <- 2
    }
    else nr <- 2
    clean2[i,7] <- substr(data2$X__2[i], ratep[nr,"start"],ratep[nr,"end"])
  }
  else if(str_detect(data2$X__2[i], " [0-9]+(\\.[0-9]+)?(/[0-9]+(\\.[0-9]+)?)? |RNR|COA|\\$[0-9]+(\\.[0-9]+)( )?M|\\$[0-9]{3,}( )?K")){
    ratep <- str_locate_all(data2$X__2[i], " [0-9]+(\\.[0-9]+)?(/[0-9]+(\\.[0-9]+)?)? |RNR|COA|\\$[0-9]+(\\.[0-9]+)( )?M|\\$[0-9]{3,}( )?K")[[1]]
    nr <- nrow(ratep)
    clean2[i,7] <- substr(data2$X__2[i], ratep[nr,"start"],ratep[nr,"end"])
  }
  else{
    norate <- c(norate, i)
    next
  }
  if (str_detect(data2$X__2[i], laycan)){
    layp <- str_locate_all(data2$X__2[i], laycan)[[1]]
    clean2[i,6] <- substr(data2$X__2[i], layp[1,"start"],layp[1,"end"])
    clean2[i,8] <- substr(data2$X__2[i], max(layp[nrow(layp),"end"], ratep[nr, "end"])+1, nchar(data2$X__2[i]))
  }
  else {
    nolaycan <- c(nolaycan, i)
    clean2[i,6] <- NA
  }
  if (str_detect(data2$X__2[i], port)){
    portp <- str_locate_all(data2$X__2[i], port)[[1]]
    if(nrow(portp)==1) np <- 1
    else if(portp[nrow(portp),"start"] < ratep[nr, "end"]) np <- 2
    else np <- 1
    clean2[i,4] <- substr(data2$X__2[i], portp[np,"start"],min(portp[np,"end"],ratep[nr, "start"]-1))
  }
  else {
    noport <- c(noport, i)
    if(str_trim(clean2[i,1])==""|is.na(clean2[i,1])){
      if(str_detect(data2$X__2[i], size)) clean2[i,4] <- substr(data2$X__2[i], sizep[1,"end"]+1, ratep[1,"start"]-1)
      else clean2[i,4] <- substr(data2$X__2[i], 1, ratep[1,"start"]-1)
    } 
    else if (str_detect(data2$X__2[i], size)&(str_detect(data2$X__2[i], rate))){
      if(abs(ratep[nr,"end"]-layp[nrow(layp),"start"])<4 | abs(layp[nrow(layp),"end"] - ratep[nr,"start"])<4){
        clean2[i,4] <- substr(data2$X__2[i], sizep[1,"end"]+1, min(ratep[nr, "start"], layp[nrow(layp), "start"])-1)
      }
      else clean2[i,4] <- substr(data2$X__2[i], max(layp[nrow(layp),"end"],sizep[1,"end"])+1, ratep[nr, "start"]-1)
    }
    else clean2[i,4] <- ""
  }
  if(str_detect(data2$X__2[i], cargo)){
    clean2[i,3] <- str_extract(data2$X__2[i], cargo)
  }
  else clean2[i,3] <- NA
  clean2[i,10] <- data2$X__1[i]
}

noport <- which(!str_detect(clean2$load_port, "/")) 
noport
nolaycan <- as.numeric(nolaycan[-1])
nolaycan
nosize <- as.numeric(nosize[-1])
nosize
norate <- as.numeric(norate[-1])
norate
clean2$name <- str_trim(clean2$name)
k_port <- which((clean2$name == ""|is.na(clean2$name))&!is.na(clean2$rate))

if(length(c(nolaycan,nosize, k_port))>0) clean2 <- clean2[-unique(c(nolaycan,nosize, k_port)),]
clean2$cargo[which(str_detect(clean2$cargo_size, "FO"))] <- "FO"
clean2$load_port <- gsub("^FO ", "", clean2$load_port)
if(sum(is.na(clean2$laycan))>0) clean2 <- clean2[-which(is.na(clean2$laycan)),]

clean2$comments[which(is.na(clean2$comments))] <- ""
clean2$type <- ""

##### HR1---------------------
data6 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "HR1", col_name = FALSE)
data6 <- data6[,which(colMeans(is.na(data6)) < 1)] 
clean2_1 <- cbind(data6[,7], data6[,2],data6[,3],data6[,4],"",data6[,5],data6[,9],
                  data6[,1],data6[,10],"E","",data6[,6])
names(clean2_1) <- names(clean)
clean2_1$laycan <- as.character(clean2_1$laycan)
clean2_1$laycan_end <- as.character(clean2_1$laycan_end)
k <- which(is.na(clean2_1$laycan))
clean2_1$laycan[k] <- clean2_1$laycan_end[k]
clean2 <- rbind(clean2, clean2_1)

##### Twitter 3-------------------
data4 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "TW", col_name = FALSE)
data4 <- data4$X__1
if(sum(is.na(data4))>0) data4 <- data4[-which(is.na(data4))]
data4 <- gsub("b'|,|\\[.*|'$", "", data4)
data4 <- toupper(data4)
status <- "ON SUBS|FULLY FIXED|FAILED"
dzone <- "/[A-Z']+ "
laycan <- "(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC) [0-9]+(-[0-9]+)?(-(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC) [0-9]+)?"
clean3 <- clean[1,][-1,]
nobuilt <- ""
norate <- ""
nostatus <- ""
nodzone <- ""
nolaycan <- ""
for (i in 1:length(data4)){
  builtp <- str_locate_all(data4[i], "\\([0-9]{4}\\)")[[1]]
  statusp <- str_locate_all(data4[i], status)[[1]]
  clean3[i,9] <- substr(data4[i], statusp[1,"start"], statusp[1,"end"])
  if(str_detect(data4[i], "\\([0-9]{4}\\)")) clean3[i,1] <- substr(data4[i],1, str_locate(data4[i], "\\([0-9]{4}\\)")[1,1]-1)
  else clean3[i,1] <- substr(data4[i],1,statusp[1,1]-1)
  if (str_detect(data4[i], dzone)){
    zonep <- str_locate_all(data4[i], dzone)[[1]]
    clean3[i,4] <- substr(data4[i], statusp[1,"end"]+1, zonep[1,"start"]-1)
    clean3[i,5] <- substr(data4[i], zonep[1,"start"]+1, zonep[1,"end"])
  }
  else nodzone <- c(nodzone, i)
  laycanp <- str_locate_all(data4[i], laycan)[[1]]
  clean3[i,6] <- substr(data4[i], laycanp[1,"start"], laycanp[1,"end"])
  if(str_detect(data4[i], rate)){
    ratep <- str_locate_all(data4[i], rate)[[1]]
    clean3[i,7] <- substr(data4[i], ratep[1,"start"], ratep[1,"end"])
    clean3[i,8]<- substr(data4[i], ratep[1,"end"]+1, laycanp[1,"start"]-1)
  }
  else {
    norate <- c(norate, i)
    if (str_detect(data4[i], dzone)) clean3[i,8] <- substr(data4[i], zonep[1,"end"]+1, laycanp[1,"start"]-1)
  }
  # clean3[i,9] <- substr(data4[i], laycanp[1,"end"]+1, nchar(data4[i]))
}
norate <- as.numeric(norate[-1])
clean3$rate[norate] <- "RNR"
k <- which(str_detect(clean3$rate, "\\$[0-9]+"))
clean3$rate[k] <- paste0("USD", as.numeric(str_extract(clean3$rate[k], "[0-9]+"))/1000000, "M")
clean3$cargo_size <- 260
clean3$cargo <- "CRUDE"
clean3$type <- "VLCC"
clean3$source <- "O"

##### DPP REUTERS 4-----------------
data5 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "RT", col_name = FALSE)
if(sum(is.na(data5[,2]))>0) data5 <- data5[-which(is.na(data5[,2])),]
k1 <- which(is.na(data5[,11]))
data5[k1,11] <- data5[k1,10]
k2 <- which(is.na(data5[,13]))
data5[k2,13] <- data5[k2,12]
data5$X__15[which(is.na(data5$X__15))] <- ""
rate_r <- paste0(data5$X__14, data5$X__15)
clean4 <- cbind(data5[,2],data5[,7],data5[,9],data5[,11],data5[,13],data5[,16],rate_r,data5[,1],data5[,17],"N",data5[,5], "")
names(clean4) <- names(clean)
clean4$laycan <- as.character(clean4$laycan)
clean4$cargo_size <- as.numeric(clean4$cargo_size)/1000
clean4$cargo[which(is.na(clean4$cargo))] <- "CRUDE"
no_port_info <- which((is.na(clean4$load_port)&is.na(clean4$discharge_port)))
if(length(no_port_info) > 0) clean4 <- clean4[-no_port_info,]


#### DPP GB  6--------------------
data8 <- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "GB", col_name = FALSE)
data8 <- data8$X__1
k <- which(str_detect(data8, "ON SUBS"))
if(length(k)>0) data8 <- data8[-k]
data8 <- gsub("\U00A0"," ",data8)
data8 <- gsub("\\s{2,}", "  ", data8)
rplc <- which(str_detect(data8, "REPLACE"))
data8[rplc-1] <- paste0(data8[rplc-1], " RPLC")
fld <- which(str_detect(data8, "FAIL"))
data8[fld-1] <- paste0(data8[fld-1], " FLD")
data8 <- data8[-which(sapply(data8, nchar)<30&!str_detect(data8,rate))]
if(sum(is.na(data8))>0) data8 <- data8[-which(is.na(data8))]
data8 <- toupper(data8) %>% str_trim
clean6 <- clean[1,][-1,]
nosize <- ""
nocargo <- ""
norate <- ""
nolaycan <- ""
noport <- ""
len <- sapply(str_split(data8, "  "),length)

size <- " [0-9]{2,} "
laycan1 <- "([0-9]+| )(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC) |-DNR-| ([0-9]{1,2}-)?[0-9]{1,2}/[0-9]{1,2} "
cargo <- " NHC | FO "
dwt_y <- " [0-9]{3}/[0-9]{2} "

i <- 0
for (m in 1:length(data8)){
  if(str_detect(data8[m], laycan1)){
    i <- i+1
    if (str_detect(data8[m], size)){
      sizep <- str_locate_all(data8[m], size)[[1]]
      clean6[i,2] <- substr(data8[m], sizep[1,"start"],sizep[1,"end"])
      if(str_detect(data8[m], " [0-9]{3}/[0-9]{2} ")){
        dwt_yp <- str_locate_all(data8[m], dwt_y)[[1]]
        clean6[i,1] <- substr(data8[m], 1, dwt_yp[1,"start"]-1)
      }
      else clean6[i,1] <- substr(data8[m], 1,sizep[1,"start"]-1)
    }
    else {
      nosize <- c(nosize, m)
      clean6[i,2] <- NA
    }
    if (str_detect(data8[m], cargo)){
      cargop <- str_locate_all(data8[m], cargo)[[1]]
      clean6[i,3] <- substr(data8[m], cargop[1,"start"],cargop[1,"end"])
    }
    else{
      clean6[i,3] <- NA
    }
    if (str_detect(data8[m], laycan1)){
      layp <- str_locate_all(data8[m], laycan1)[[1]]
      clean6[i,6] <- substr(data8[m], layp[1,"start"],layp[1,"end"])
    }
    else {
      nolaycan <- c(nolaycan, m)
      clean6[i,6] <- NA
    }
    if (str_detect(data8[m], rate)){
      ratep <- str_locate_all(data8[m], rate)[[1]]
      clean6[i,7] <- substr(data8[m], ratep[nrow(ratep),"start"],ratep[nrow(ratep),"end"])
      clean6[i,8] <- substr(data8[m], ratep[nrow(ratep),"end"]+1,nchar(data8[m]))
      if (str_detect(data8[m], laycan1)){
        clean6[i,4] <- substr(data8[m], layp[1,"end"]+1, ratep[nrow(ratep),"start"]-1)
        clean6[i,4] <- str_trim(clean6[i,4])
        if(str_detect(clean6[i,4], "  ")){
          clean6[i,5] <- str_split(clean6[i,4], "  ")[[1]][2]
          clean6[i,4] <- gsub("  .*", "", clean6[i,4])
        }
        else if(str_detect(clean6[i,4], "/")){
          clean6[i,5] <- str_split(clean6[i,4], "/")[[1]][2]
          clean6[i,4] <- gsub("/.*", "", clean6[i,4])
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
  else if(str_detect(data8[m], rate)){
    add_d <- str_split(data8[m], "  ")[[1]]
    clean6[i,7] <- paste0(clean6[i,7], "/",add_d[length(add_d)])
    clean6[i,5] <- paste0(clean6[i,5], "-",add_d[length(add_d)-1])
  }
  else next
}
clean6$source <- "R"
noport <- as.numeric(noport[-1])
noport
nolaycan <- as.numeric(nolaycan[-1])
nolaycan
norate <- as.numeric(norate[-1])
norate



##### RAFFLES 7----------
data9<- read_excel("~/Desktop/ofe/DPP 27 AUG.xlsx", sheet = "RAFFLES", col_name = FALSE)
data9 <- as.data.frame(data9[,which(colMeans(is.na(data9)) < 1)])
k <- which(str_detect(data9[,4], "^4[0-9]+$"))
data9[k,4] <- format(as.Date(as.numeric(data9[k,4]),origin = "1900-12-30"), "%d-%m")
lay_raff <- paste0(data9[,4], " ", data9[,5])
rate_raff <- paste0(data9[,8], " ", data9[,9])
data9[which(is.na(data9[,11])),11] <- data9[which(is.na(data9[,11])),12]
clean7 <- cbind(data9[,1:3], data9[,6:7], lay_raff, rate_raff, data9[,10], data9[,11], "AL", "", "")
names(clean7) <- names(clean)


####Combine all ---------------------
clean_all <- rbind(clean, clean2, clean4)
test <- as.data.frame(clean_all[,1:2])
test[,3] <- clean_all[,11]
test[,4:5] <- clean_all[,3:4]
test[,6:7] <- ""
test[,8] <- clean_all[,5]
test[,9:10] <- ""
test[,11] <- clean_all[,6]
test[,12] <- clean_all[,12]
test[,13:15] <- clean_all[,7:9]
test[,16] <- "2018-08-27"
test[,17] <- fix_m
test[,18] <- clean_all[,10]

