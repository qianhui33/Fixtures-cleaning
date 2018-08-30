library(stringr)
library(plyr)
library(dplyr)
library("googlesheets")
gs_auth()

names(test) <- c("VESSEL","CARGO_SIZE","VESSEL_TYPE","CARGO","LOAD_PORT","LOAD_COUNTRY","LOAD_ZONE","DISCHARGE_PORT","DISCHARGE_COUNTRY",
                 "DISCHARGE_ZONE","LAYCAN_START","LAYCAN_END","RATE","CHARTERER", "STATUS","UPDATE_DATE", "UPDATE_MONTH","SOURCE")
noname <- which(test$VESSEL == "")
if (length(noname)>0) test<- test[-noname,]
if(any(is.na(test$LAYCAN_START))) test <- test[-which(is.na(test$LAYCAN_START)),]
test <- unique(test)

vessel_info <- gs_read(gs_title("vessel"), ws=1)
vessel_info <- vessel_info[vessel_info$type_id %in% c(0,2:4,7),]
vessel_name <- toupper(vessel_info$name)
vessel_info$name <- toupper(vessel_info$name)

for (i in 1:ncol(test)){
  test[,i] <- gsub("\U00A0","", test[,i])
  test[,i] <- gsub("\U2013|\U2019","", test[,i])
  test[,i] <- str_trim(test[,i])
  test[,i] <- toupper(test[,i])
  test[,i] <- gsub("\\s+"," ", test[,i])
  test[,i] <- gsub("#N/A",NA, test[,i])
}
test$VESSEL_TYPE[test$VESSEL_TYPE == ""|is.na(test$VESSEL_TYPE)] <- "-"

#### Add STATUS ####
coload <- which(str_detect(test$STATUS, "COLOAD"))
test$CHARTERER[coload] <- paste0(test$CHARTERER[coload],"/", test$STATUS[coload])
test$CHARTERER[coload] <- gsub("COLOAD WITH ", "",test$CHARTERER[coload])
test$STATUS[coload] <- ""
s_detect <- c("FLD|FAILED| F$|DENIED", "HOLD", "SUB|/S| S$", "RPLC|REPLACE|REPL|RPL", "/COR| COR$|COR\\)|CORR$|CORR\\)|/CORR|\\*|CORRECT|UPDT|RATE ADD|UPDATE", " OLD", "OOS|O/O|OO$", 
              "REFXD|FXD|FIXED| C$|CFMD|CONFIRM|CONF$|^FX$| FX", "N/B", "CNCL|CANC|CANCLD", " DD|EXDD|EX D/D|D/D", "SWAPPED")
status_corr <- c("FLD", "HOLD", "SUB", "RPLC", "CORR", "OLD", "OOS", "FXD", "N/B", "CNCL", "DD", "SWAPPED")
status <- ""
for(i in 1:length(status_corr)){
  k <- which(str_detect(test$VESSEL, as.character(s_detect[i]))|
               str_detect(test$CHARTERER, as.character(s_detect[i]))|
               str_detect(test$STATUS, as.character(s_detect[i])))
  status[k] <- paste(status[k], status_corr[i], sep = " ")
}

part_c <- which(str_detect(test$VESSEL, "P/C|PART CARGO|PTC")|
                  str_detect(test$CHARTERER, "P/C|PART CARGO|PTC")|
                  str_detect(test$STATUS, "P/C|PART CARGO|PTC"))
if(length(status) < nrow(test)) status[(length(status)+1):nrow(test)] <- NA
status<- str_trim(gsub("^NA ", "", status))
status[which(is.na(status)|status=="")] <- "SUB"
test$STATUS <- status
test_c <- test

#### Clean CARGO_SIZE ####
if(any(is.na(test$CARGO_SIZE))) test <- test[-which(is.na(test$CARGO_SIZE)),]
for (i in 1:nrow(test)){
  k <- as.numeric(str_extract(test$CARGO_SIZE[i], "[0-9]{1,3}"))
  if (str_detect(test$CARGO_SIZE[i], "K")) test$CARGO_SIZE[i] <- round(k/7.33)
  else test$CARGO_SIZE[i] <- k
}


#### Clean CARGO ####
test$CARGO <- str_trim(test$CARGO)
test$CARGO[which(str_detect(test$CHARTERER, "\\(FO\\)"))] <- 'FO'
test$CARGO[which(is.na(test$CARGO)|test$CARGO==""|test$CARGO=="NULL"|test$CARGO=="NA")] <- "CRUDE"
test$CARGO <- gsub("\\?|-","", test$CARGO)
cargo_corr <- gs_read(gs_title("cargo_corr.csv"), ws=1)
uncorr <- ""
for (i in 1:nrow(test)){
  k <- which(cargo_corr$cargo == test$CARGO[i])
  if(length(k)>0) test$CARGO[i] <- as.character(cargo_corr$corr[k])
  else{
    uncorr <- c(uncorr,i)
    test$CARGO[i] <- paste0(test$CARGO[i], "*")
  } 
}
uncorr <- as.numeric(uncorr[-1])
test$CARGO[uncorr]
test$CARGO[part_c] <- paste0(test$CARGO[part_c], "(PTC)")
test_c <- test

#### Clean VESSEL NAME ####
test$VESSEL <- gsub("OO .*| OOS.*$|O/O.*$|\\-|\\?| LR2|\\|","",test$VESSEL) # Delete OOS and O/O
test$VESSEL <- gsub("[0-9]+/[0-9]+.*|\\(.*", "", test$VESSEL)  # Delete /... and (...)
test$VESSEL <- gsub(" COR$| CORR$|  SUB$| OLD$| FLD$| HOLD$|EX DD$|EXDD$|NO SIRE|FXD| REPL| PTC|PTC$| COND$|EX D/D|ON SUBS|
                    OOS |OOS$|RPLC|OO$|CHANGE TECH MGMNT$| LOWSTO MID|FO$|HC$|P/C|O/P|^M(\\.)?T(\\.)?( )?|^M(\\.)?V(\\.)?( )?","", test$VESSEL) 
test$VESSEL <- str_trim(test$VESSEL)
test$VESSEL <- gsub("\\s+"," ", test$VESSEL)

## find the frequancy of vessel name, get names appear only once and mark 
vessel_corr <- gs_read(gs_title("vessels_corr"), ws=1)
fq_table <- as.data.frame(table(test$VESSEL))
fq_table$test <- 1
for (i in 1:nrow(fq_table)){
  if (fq_table$Var1[i] %in% vessel_name) fq_table$test[i] <- 0
  else if(str_detect(fq_table$Var1[i], "TBN")) fq_table$test[i] <- 0
}

fq_table <- fq_table[which(fq_table$test==1),]
fq_table$corr <- ""
for (i in 1:nrow(fq_table)){
  if(fq_table$Var1[i] %in% vessel_corr$name){
    fq_table$test[i] <- 0
    cor <- which(vessel_corr$name == fq_table$Var1[i])
    test$VESSEL[which(test$VESSEL==fq_table$Var1[i])] <- as.character(vessel_corr$corr[cor])
    fq_table$corr[i]<- as.character(vessel_corr$corr[cor])
  }
  else{
    for(j in 1:3){
      cor <- agrep(fq_table$Var1[i], vessel_name, max = j, ignore.case=TRUE)
      if (length(cor)==1){
        fq_table$test[i] <- 0
        test$VESSEL[which(test$VESSEL==fq_table$Var1[i])] <- as.character(vessel_name[cor])
        fq_table$corr[i]<- as.character(vessel_name[cor])
        break
      }
      else if (length(cor) >1) break
    }
  }
}
test <- unique(test)
test_c <- test


#### Clean CHARTERER ####
char_corr <- gs_read(gs_title("char_corr"), ws=1)
if(any(test$CHARTERER == "CHARTERER")) test <- test[-which(test$CHARTERER == "CHARTERER"),]
dem_r <- which(str_detect(test$CHARTERER, "[0-9]+K"))
test$RATE[dem_r] <- paste0(test$RATE[dem_r], "+", str_extract(test$CHARTERER[dem_r], "[0-9]+K"))
test$CHARTERER <- gsub("K-LINE","K LINE", test$CHARTERER)
test$CHARTERER <- gsub("S-OIL","S OIL", test$CHARTERER)
test$CHARTERER <- gsub("A-Z","AZ", test$CHARTERER)
test$CHARTERER <- gsub("GS-CALTEX","GS CALTEX", test$CHARTERER)
test$CHARTERER <- gsub("R/S|D/C|L/S|\\(.*|\\?|,|-.*|\\,.*|\\*|^/|/.*|<.*|\\+[0-9]+(\\.[0-9]+)?","",test$CHARTERER)
test$CHARTERER <- gsub("FLD|RPLCMNT|( )?RPLC.*|RPTD| REPL|( )?FXD.*|FIXED| HOLD| COR(R)?$|LVL|FAILING| REPLD|RATE UPDATED| OLD$|DEM .*|CFMD$|\\$.*|[0-9]+K.*|\\)|RUN LATE|MIS RPT|RPT|LVLS| PTC|RUNNING LATE|
                       FFXD|OOS|REFIXED|DATES CHANGED| RPL|RATE ADD| EX .*|( )?SUBS.*| S$| C$| R$| SUB$| SUEZ| F$|SUB .*|TO RPLC.*|DEMM$| P$|RECENT|LEVLS|FX$","",test$CHARTERER)
test$CHARTERER <- str_trim(test$CHARTERER)
uncorr <- ""
k <- which(str_detect(test$CHARTERER, "^PD "))
test$RATE[k] <- paste0(test$RATE[k], "/D")
test$CHARTERER[k] <- gsub("^PD ", "", test$CHARTERER[k])
test$CHARTERER[which(test$CHARTERER == ""|is.na(test$CHARTERER))] <- "CNR"
for (i in 1:nrow(test)){
  if (test$CHARTERER[i] %in% char_corr$CHAR) test$CHARTERER[i] <- char_corr$CORR[which(char_corr$CHAR == test$CHARTERER[i])]
  else{
    uncorr <- c(uncorr,i)
    test$CHARTERER[i] <- paste0(test$CHARTERER[i], "*")
  }
}
uncorr <- as.numeric(uncorr[-1])
test$CHARTERER[uncorr]
test_c <- test


# Clean RATE --------------------
test$RATE <- str_trim(test$RATE)
test$RATE <- gsub("WS( )?|WU( )?|W |W/S( )?", "W", test$RATE)
test$RATE <- gsub("RBR|RNRRNR|^RN$|^NR|R N R","RNR", test$RATE)
test$RATE <- gsub("( )?/( )?","/",test$RATE)
test$RATE <- gsub("^U|VOY\\(\\$LS\\)|\\$|^LS( )?|^L/|VOY","USD", test$RATE)
test$RATE <- gsub("/U","/USD", test$RATE)
test$RATE <- gsub("( )?-( )?","-", test$RATE)
test$RATE <- gsub("\\(S/?S\\)","SS", test$RATE)
test$RATE <- gsub("\\(C/?C\\)","CC", test$RATE)
test$RATE <- gsub("USDSD","USD", test$RATE)
test$RATE <- gsub("OP|O/PO/P","O/P", test$RATE)
test$RATE = gsub("\\?|LVL|S |ST |TS |C |LA |G |AH |^NA|NA$|\\(.*\\)|USD$","",test$RATE)
k_usd <- which(str_detect(test$RATE, ",")&str_detect(test$RATE, "U")&!str_detect(test$RATE, "M"))
test$RATE[k_usd] <- gsub(",","", test$RATE[k_usd])
m_usd <- which(str_detect(test$RATE, ",")&str_detect(test$RATE, "U")&str_detect(test$RATE, "M"))
test$RATE[m_usd] <- gsub(",",".", test$RATE[m_usd])
k_w <- which(str_detect(test$RATE, ",")&str_detect(test$RATE, "W"))
test$RATE[k_w] <- gsub(",",".", test$RATE[k_w])
dot_r <- which(str_detect(test$RATE,"^\\."))
test$RATE[dot_r] <- paste0("0",test$RATE[dot_r])
test$RATE <- gsub("USD ", "USD", test$RATE)
ra <- which(str_detect(test$RATE,"^[0-9]"))
if (length(ra)>0){
  for (i in 1:length(ra)){
    if(str_detect(test$RATE[ra[i]], "M|K")|
       as.numeric(str_extract(test$RATE[ra[i]], "[0-9]+"))>200) test$RATE[ra[i]] <- paste0("USD",test$RATE[ra[i]])
    else test$RATE[ra[i]] <- paste0("W",test$RATE[ra[i]])
  }
}
nokm <- which(str_detect(test$RATE, "USD[0-9]")&(!str_detect(test$RATE, "M|K")))
if(length(nokm) >0){
  for(i in 1:length(nokm)){
    k <- as.numeric(str_extract(test$RATE[nokm[i]], "[0-9]+(\\.[0-9]+)?"))
    if(k >= 1000000) test$RATE[nokm[i]] <- paste0("USD", k/1000000, "M")
    else if(k > 1000) test$RATE[nokm[i]] <- paste0("USD", k/1000, "K")
    else if(k > 50) test$RATE[nokm[i]] <- paste0(test$RATE[nokm[i]], "K")
    else test$RATE[nokm[i]] <- paste0(test$RATE[nokm[i]], "M")
  }
}
if(sum(is.na(test$RATE))>0) test$RATE[which(is.na(test$RATE))] <- "RNR"
test$RATE <- gsub(" M|( )?MILL", "M", test$RATE)
test$RATE <- gsub(" K", "K", test$RATE)
test$RATE <- gsub("SS", "(SS)", test$RATE)
test$RATE <- gsub("CC", "(CC)", test$RATE)
test$RATE <- str_trim(test$RATE)
test <- unique(test)
test_c <- test

#### Standardize LOAD_PORT ####
sep <- which(str_detect(test$LOAD_PORT, "/"))
sep_port <- str_split(test$LOAD_PORT[sep], "/")
test$LOAD_PORT[sep] <- sapply(sep_port, "[[",1)
test$DISCHARGE_PORT[sep] <- sapply(sep_port, "[[",2)
for(i in 5:10){
  test[,i] <- gsub("-$", "", test[,i])
}

port_corr <- gs_read(gs_title("port_corr"), ws=1)
port_corr <- unique(port_corr)
port_full <- read.csv("~/Desktop/ofe/data_clean/full_port.csv", head=TRUE, stringsAsFactors = FALSE)
port_full <- port_full[-which(port_full[,12]=="R"|port_full[,12]=="A"),]
port_name <- toupper(port_full$name)
no_dp <- which(is.na(test$DISCHARGE_PORT) & str_detect(test$LOAD_PORT, "XROSS|X-|^CROSS "))
test$DISCHARGE_PORT[no_dp] <- gsub("XROSS|X-|^CROSS ", "",test$LOAD_PORT[no_dp])
cc <- which(str_detect(test$DISCHARGE_PORT, "VIACOGH"))
test$RATE[cc] <- paste0(test$RATE[cc], " CC")
test$DISCHARGE_PORT[cc] <- gsub("VIA.*", "", test$DISCHARGE_PORT[cc])

for (k in c(5,8)){
  test[,k] <- str_trim(test[,k])
  test[which(is.na(test[,k])),k] <- "-"
  test[,k] <- gsub("\\+1","-", test[,k])
  test[,k] <- gsub("\\+| OR |( )?-( )?|/","-", test[,k])
  test[,k] <- gsub("COME-BY-CHANCE","COME BY CHANCE", test[,k])
  test[,k] <- gsub("UST-LUGA","UST LUGA", test[,k])
  test[,k] <- gsub("PHILLIPINES","PHILIPPINES", test[,k])
  test[,k] <- gsub("BAYU-UNDAN","BAYU UNDAN", test[,k])
  test[,k] <- gsub("\\(.*|2P|-S$|-P$|-A$|-R\\.$|-$|\\?|^-","", test[,k])
  test[,k] <- gsub("OPT$|OTPS$|OPTTS$|OPS$|OP$", "OPTS", test[,k])
  test[,k] <- gsub("^X-|^X ","XROSS ", test[,k])
  test[,k] <- gsub("E-WC","E WC", test[,k])
  test[,k] <- gsub("^S ","S\\.", test[,k])
  test[,k] <- gsub("^W ","W\\.", test[,k])
  
  
  one <- which(!str_detect(test[,k],"-"))
  test[,k] <- str_trim(test[,k])
  
  if (any(test[,k]== "")) test[which(test[,k]== ""),k] <- "-"
  nomatch1 <- ""
  for (i in 1:length(one)){
    if (test[one[i], k] %in% port_name) test[one[i],k] < test[one[i],k]
    else if (test[one[i], k] %in% port_corr$port){
      loc <- which(port_corr$port== test[one[i],k])
      test[one[i],k] <- port_corr$corr[loc[1]]
      if (!is.na(port_corr$zones[loc[1]])){
        test[one[i],k+1] <- port_corr$corr[loc[1]]
        test[one[i],k+2] <- port_corr$zones[loc[1]]
      }
    }
    
    else nomatch1 <- c(nomatch1, one[i])
  }
  
  two <- which(str_detect(test[,k],"-"))
  nomatch2 <- ""
  if (length(two)>0){
    two_sp <- str_split(test[two,k],"-")
    for (i in 1:length(two)){
      cop <- two_sp[i]
      for (j in 1:length(cop[[1]])){
        loc <- which(port_corr$port == cop[[1]][j])
        if (length(loc) ==1) {
          cop[[1]][j] <- port_corr$corr[loc]
          if (!is.na(port_corr$zones[loc[1]]) & j==1){
            test[two[i],k+1] <- port_corr$zones[loc]
            test[two[i],k+2] <- port_corr$zones[loc]
          }
        }
        else nomatch2 <- c(nomatch2, two[i])
      }
      test[two[i],k]<- paste(cop[[1]], collapse = "-")
    }
  }
  check1 <- which(str_detect(test[,k], "^NW |^EC |^WC |^NORTH |^NC |^CENTRAL |^SOUTH |^STS "))
  test[check1, k+1] <- test[check1,k]
  test[check1, k+1] <- gsub("NW |EC |WC |NORTH |NC |CENTRAL |SOUTH |STS |-.*", "", test[check1, k+1])
}
nomatch1 <- as.numeric(nomatch1[-1])
nomatch2 <- as.numeric(nomatch2[-1])
test <- unique(test)
test_c <- test

#### Standardize LAYCAN ####
test$LAYCAN_START <- gsub("ERLY|EARLY", "ELY", test$LAYCAN_START)
test$LAYCAN_START <- gsub("2H", "END", test$LAYCAN_START)
test$LAYCAN_START <- gsub("\\?", "", test$LAYCAN_START)
test$LAYCAN_START  <- gsub("\\.1$", "/10", test$LAYCAN_START)
test$LAYCAN_START <- gsub("\\.|//", "/", test$LAYCAN_START)
month_abbr = toupper(month.abb[])
for(i in 1:12) test$LAYCAN <- gsub(toupper(month.name[i]), month_abbr[i], test$LAYCAN)
non <- ""
year <- 2018
for (i in 1:nrow(test)){
  if (str_detect(test$LAYCAN_START[i], "ELY|MID|END")){
    a <- str_extract(test$LAYCAN_START[i], "(ELY|MID|END)")
    day <- which(c("ELY","MID","END") == a)*10 - 5 # ELY-5, MID-15, END-25
    test$LAYCAN_START[i] <- gsub("ELY|MID|END", day, test$LAYCAN_START[i])
  }
  
  if(str_detect(test$LAYCAN_START[i], "2018-[0-9]+-[0-9]+")) next
  else if (test$LAYCAN_START[i] == "PPT") test$LAYCAN_START[i] <- as.character(as.Date(test$UPDATE_DATE[i],"%d/%m/%y"))
  else if (str_detect(test$LAYCAN_START[i], "[0-9]{1,2}( )?[/-]?( )?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)|(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[/ -][0-9]{1,2}")){
    a <- as.numeric(str_extract(test$LAYCAN_START[i], "([0-9]{1,2})"))
    b <- str_extract(test$LAYCAN_START[i], "(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)")
    month <- which(month_abbr == b)
    day <- a
    # year <- str_extract(test$UPDATE_DATE[i], "[0-9]{2}")
    test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
  }
  else if (str_detect(test$LAYCAN_START[i], "([0-9]+-[0-9]+)?( )?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC) [0-9]{1,2}-[0-9]{1,2}")){
    a1 <- as.numeric(str_extract(test$LAYCAN_START[i], "([0-9]{1,2})"))
    a1 <- as.numeric(str_extract(str_extract(test$LAYCAN_START[i], "-[0-9]{1,2}"), "[0-9]{1,2}"))
    b <- str_extract(test$LAYCAN_START[i], "(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)")
    month <- which(month_abbr == b)
    day <- a
    # year <- str_extract(test$UPDATE_DATE[i], "[0-9]{2}")
    test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
  }
  else if(str_detect(test$LAYCAN_START[i], "[0-9]+/[0-9]+/(17|16|18|2016|2017|2018)")){
    day <- str_extract(test$LAYCAN_START[i], "[0-9]+")
    month <- str_extract(str_extract(test$LAYCAN_START[i], "/[0-9]+"), "[0-9]+")
    if(as.numeric(month) == as.numeric(test$UPDATE_MONTH[i])|as.numeric(month) == as.numeric(test$UPDATE_MONTH[i])+1) test$LAYCAN_START[i] <- paste(year, month, day,sep="-")
    else  test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
  }
  else if(str_detect(test$LAYCAN_START[i], "[0-9]{4}-[0-9]{2}-[0-9]{2}") & as.numeric(str_extract(test$LAYCAN_START[i], "[0-9]{4}"))< 2018){
    day1 <- str_extract(test$LAYCAN_START[i], "[0-9]+$")
    day2 <- str_extract(str_extract(test$LAYCAN_START[i], "-[0-9]+"), "[0-9]+")
    month <- str_extract(str_extract(test$LAYCAN_START[i], "[0-9]{4}"), "[0-9]{2}$")
    test$LAYCAN_START[i] <- paste(year, month, day1, sep="-")
    test$LAYCAN_END[i] <- paste(year, month, day2, sep="-")
  }
  else if(str_detect(test$LAYCAN_START[i], "^[0-9]{1,2}[ -/][0-9]{1,2}$")){
    a <- as.numeric(str_extract(test$LAYCAN_START[i], "[0-9]+"))
    b <- as.numeric(str_extract(str_extract(test$LAYCAN_START[i], "[ -/][0-9]{1,2}"), "[0-9]{1,2}"))
    Lay_Month <- as.numeric(test$UPDATE_MONTH[i])
    # year <- str_extract(test$UPDATE_DATE[i], "[0-9]{2}")
    if ((a == Lay_Month & b == Lay_Month)|a == Lay_Month &!(b == Lay_Month)){
      month <- a
      day <- b
      test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
    }
    else if (!(a == Lay_Month) & b == Lay_Month){
      month <- b
      day <- a
      test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
    }
    else {
      if(Lay_Month < 12){
        if(a == Lay_Month+1 ){
          month <- a
          day <- b
          test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
        }
        else if (b == Lay_Month+1 ){
          month <- b
          day <- a
          test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
        }
        else if(a == Lay_Month-1){
          month <- a
          day <- b
          test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
        }
        else if (b == Lay_Month-1){
          month <- b
          day <- a
          test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
        }
        else non <- c(non, i)
      }
      else if (Lay_Month == 12){
        if(a == 1){
          month <- a
          day <- b
          test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
        }
        else if (b == 1){
          month <- b
          day <- a
          test$LAYCAN_START[i] <- paste(year, month, day, sep="-")
        }
        else non <- c(non, i)
      }
    }
  }
  else if(str_detect(test$LAYCAN_START[i],"[0-9]+-[0-9]+/[0-9]+")){
    d <- str_extract(test$LAYCAN_START[i],"[0-9]+/[0-9]+$")
    day2 <- as.numeric(str_extract(d, "[0-9]+"))
    day1 <- as.numeric(str_extract(test$LAYCAN_START[i], "[0-9]+"))
    month2 <- as.numeric(str_extract(str_extract(d, "/[0-9]{1,2}"), "[0-9]{1,2}"))
    if(day1 > day2) month1 <- month2-1
    else month1 <- month2
    test$LAYCAN_END[i] <- paste(year, month2, day2, sep="-")
    test$LAYCAN_START[i] <- paste(year, month1, day1, sep="-")
  }
  else if(str_detect(test$LAYCAN_START[i],"[0-9]+/[0-9]+-[0-9]+")){
    d <- str_extract(test$LAYCAN_START[i],"^[0-9]+/[0-9]+")
    month <- as.numeric(str_extract(d, "[0-9]+"))
    day1 <- as.numeric(str_extract(str_extract(d, "/[0-9]{1,2}"), "[0-9]{1,2}"))
    day2 <- as.numeric(str_extract(test$LAYCAN_START[i], "[0-9]+$"))
    test$LAYCAN_END[i] <- paste(year, month, day2, sep="-")
    test$LAYCAN_START[i] <- paste(year, month, day1, sep="-")
  }
  else non <- c(non, i)
}
non <- as.numeric(non[-1])
test$LAYCAN_START[non]
k <- which(test$LAYCAN_END== ""|is.na(test$LAYCAN_END))
test$LAYCAN_END[k] <- test$LAYCAN_START[k]
test$LAYCAN_END <- as.Date(test$LAYCAN_END, format = '%Y-%m-%d')
test$LAYCAN_START <- as.Date(test$LAYCAN_START, format = '%Y-%m-%d')
if(any(is.na(test$LAYCAN_START))) test <- test[-which(is.na(test$LAYCAN_START)),]
m_month <- which(test$LAYCAN > test$LAYCAN_END)
if(length(m_month)>0) print(m_month)


#### Matching IMO number --------------
imo <- vessel_info[,2:4]
imo$name <- toupper(imo$name)
test$IMO <- ""
test$DWT <- ""
for(i in 1:nrow(test)){
  if(str_detect(test$VESSEL[i],"TBN")) {
    test$IMO[i] <- 1111111
    test$DWT[i] <- 0
  }
  else{
  imo_match <- which(imo[,2]== test$VESSEL[i])
  if(length(imo_match)==0)  test$IMO[i] <- NA
  else if(length(imo_match)==1) {
    if(imo$dwt[imo_match]>as.numeric(test$CARGO_SIZE[i])*1000|abs(as.numeric(imo$dwt[imo_match])-as.numeric(test$CARGO_SIZE[i])*1000)<10000){
      test$IMO[i] <- imo$imo[imo_match]
      test$DWT[i] <- imo$dwt[imo_match]
    }
    else test$IMO[i] <- NA
  }
  else{
    dif <- abs(as.numeric(test$CARGO_SIZE[i])*1000-imo$dwt[imo_match])
    if(min(dif) < 10000){
      test$IMO[i] <- imo$imo[imo_match[which.min(dif)]]
      test$DWT[i] <- imo$dwt[imo_match[which.min(dif)]]
    }
    else test$IMO[i] <- "-"
  }
  }
}

#### Match VESSEL TYPE------------
test$DWT <- as.numeric(test$DWT)
test$VESSEL_TYPE[test$DWT <= 15000 & test$DWT != 0] <- "COASTER"
test$VESSEL_TYPE[test$DWT <= 35000 & test$DWT > 15001] <- "HANDYSIZE"
test$VESSEL_TYPE[test$DWT <= 55000 & test$DWT > 35001] <- "MR"
test$VESSEL_TYPE[test$DWT <= 80000 & test$DWT > 55001] <- "PANAMAX"
test$VESSEL_TYPE[test$DWT <= 125000 & test$DWT > 80001] <- "AFRAMAX"
test$VESSEL_TYPE[test$DWT <= 200000 & test$DWT > 125001] <- "SUEZMAX"
test$VESSEL_TYPE[test$DWT >= 200000] <- "VLCC"


# Add PORT ID and change port country and zone-------------
test$LOAD_PORT[which(test$LOAD_PORT =="")] <- "-"
test$LOAD_COUNTRY[which(is.na(test$LOAD_COUNTRY))] <- "-"
test$LOAD_ZONE[which(is.na(test$LOAD_ZONE))] <- "-"
test$DISCHARGE_COUNTRY[which(is.na(test$DISCHARGE_COUNTRY))] <- "-"
test$DISCHARGE_ZONE[which(is.na(test$DISCHARGE_ZONE))] <- "-"
load_port_id <- ""
load_country_id <- ""
load_zone_id <- ""
discharge_port_id <- ""
discharge_country_id <- ""
discharge_zone_id <- ""
country <- read.csv("~/Desktop/ofe/data_clean/port_country.csv", head=TRUE, stringsAsFactors = FALSE)
zone <- data.frame(unique(port_full[,c(9,11)]),stringsAsFactors = FALSE)[1:28,]
nomatch <-""
for (i in 1:nrow(test)){
  if (str_detect(test$LOAD_PORT[i], "-")) port <- str_split(test$LOAD_PORT[i], "-")[[1]][1]
  else port <- test$LOAD_PORT[i]
  if (port %in% port_name){
    po <- which(port_name == port)
    load_port_id[i] <- as.numeric(port_full$idports[po])
    test$LOAD_COUNTRY[i] <- port_full$country[po]
    load_country_id[i] <- port_full$country_code[po]
    if (is.null(port_full$idzones[po])){
      test$LOAD_ZONE[i] <- "-"
      load_zone_id[i] <- 0
    }
    else if(port_full$idzones[po] > 0){
      test$LOAD_ZONE[i] <- port_full$zone_short_name[po]
      load_zone_id[i] <- port_full$idzones[po]
    }
    else {
      test$LOAD_ZONE[i] <- "-"
      load_zone_id[i] <- 0
    }
  }
  else if (port %in% country[,1]|test$LOAD_COUNTRY[i] %in% country[,1]){
    if (port %in% country[,1]) po <- which(country[,1] == port)
    else po <- which(country[,1] == test$LOAD_COUNTRY[i])
    load_port_id[i] <- "-"
    test$LOAD_COUNTRY[i] <- country[po[1],1]
    load_country_id[i] <- country[po[1],2]
    if (country[po[1],3] > 0){
      test$LOAD_ZONE[i] <- country[po[1],4]
      load_zone_id[i] <- country[po[1],3]
    }
    else{
      test$LOAD_ZONE[i] <- "-"
      load_zone_id[i] <- 0
    }
  }
  else if (port %in% zone[,2]|test$LOAD_ZONE[i] %in% zone[,2]){
    if (port %in% zone[,2]) po <- which(zone[,2] == port)
    else po <- which(zone[,2] == test$LOAD_ZONE[i])
    load_port_id[i] <- 0
    load_country_id[i] <- "-"
    if (zone[po,1] > 0){
      test$LOAD_ZONE[i] <- zone[po,2]
      test$LOAD_COUNTRY[i] <- zone[po,2]
      load_zone_id[i] <- zone[po,1]
    }
    else{
      test$LOAD_ZONE[i] <- "-"
      load_zone_id[i] <- 0
    }
  }
  else{
    nomatch <- c(nomatch, i)
    load_port_id[i] <- 0
    load_country_id[i] <- "-"
    load_zone_id[i] <- 0
  }
}
k1 <- which(str_detect(test$LOAD_PORT, "^WC INDIA"))
k2 <- which(str_detect(test$LOAD_PORT, "^WC NORTH AMERICA"))
k3 <- which(str_detect(test$LOAD_PORT, "^NORTH CHINA"))
k4 <- which(str_detect(test$LOAD_PORT, "^EC NORTH AMERICA"))
k5 <- which(str_detect(test$LOAD_PORT, "^WC AFRICA"))
k6 <- which(str_detect(test$LOAD_PORT, "^EC CANADA"))
test$LOAD_COUNTRY[k5] <- "WAFR"
test$LOAD_ZONE[k1] <- "WCI"
test$LOAD_ZONE[k2] <- "WCNA"
test$LOAD_ZONE[k3] <- "KOR"
test$LOAD_ZONE[k4] <- "ECNA"
test$LOAD_ZONE[k5] <- "WAFR"
test$LOAD_ZONE[k6] <- "ECNA"
load_zone_id[k1] <- 7
load_zone_id[k2] <- 26
load_zone_id[k3] <- 2
load_zone_id[k4] <- 19
load_zone_id[k5] <- 12
load_zone_id[k6] <- 19
nomatch <- as.numeric(nomatch[-1])
nomatch <- ""

test$DISCHARGE_PORT <- gsub("^TA-","TRANSATLANTIC-",test$DISCHARGE_PORT)
test$DISCHARGE_PORT <- gsub("-TA-","-TRANSATLANTIC-",test$DISCHARGE_PORT)
test$DISCHARGE_PORT <- gsub("-TA$","-TRANSATLANTIC",test$DISCHARGE_PORT)
for (i in 1:nrow(test)){
  if(is.na(test$DISCHARGE_PORT[i])|test$DISCHARGE_PORT[i] == "-"){
    discharge_port_id[i] <- 0
    discharge_country_id[i] <- "-"
    discharge_zone_id[i] <- 0
  }
  else{
    if (str_detect(test$DISCHARGE_PORT[i], "-")) port <- str_split(test$DISCHARGE_PORT[i], "-")[[1]][1]
    else port <- test$DISCHARGE_PORT[i]
    if (port %in% port_name){
      po <- which(port_name == port)
      discharge_port_id[i] <- as.numeric(port_full$idports[po])
      test$DISCHARGE_COUNTRY[i] <- port_full$country[po]
      discharge_country_id[i] <- port_full$country_code[po]
      if(is.null(port_full$idzones[po])){
        test$DISCHARGE_ZONE[i] <- "-"
        discharge_zone_id[i] <- 0
      }
      else if(port_full$idzones[po] > 0){
        test$DISCHARGE_ZONE[i] <- port_full$zone_short_name[po]
        discharge_zone_id[i] <- port_full$idzones[po]
      }
      else {
        test$DISCHARGE_ZONE[i] <- "-"
        discharge_zone_id[i] <- 0
      }
    }
    else if (port %in% country[,1]|test$DISCHARGE_COUNTRY[i] %in% country[,1]){
      if (port %in% country[,1]) po <- which(country[,1] == port)
      else po <- which(country[,1] == test$DISCHARGE_COUNTRY[i])
      discharge_port_id[i] <- 0
      test$DISCHARGE_COUNTRY[i] <- country[po[1],1]
      discharge_country_id[i] <- country[po[1],2]
      if (country[po[1],3] > 0){
        test$DISCHARGE_ZONE[i] <- country[po[1],4]
        discharge_zone_id[i] <- country[po[1],3]
      }
      else{
        test$DISCHARGE_ZONE[i] <- "-"
        discharge_zone_id[i] <- 0
      }
    }
    else if (port %in% zone[,2]|test$DISCHARGE_ZONE[i] %in% zone[,2]){
      if (port %in% zone[,2]) po <- which(zone[,2] == port)
      else po <- which(zone[,2] == test$DISCHARGE_ZONE[i])
      discharge_port_id[i] <- 0
      discharge_country_id[i] <- "-"
      if (zone[po,1] > 0){
        test$DISCHARGE_ZONE[i] <- zone[po,2]
        test$DISCHARGE_COUNTRY[i] <- zone[po,2]
        discharge_zone_id[i] <- zone[po,1]
      }
      else{
        test$DISCHARGE_ZONE[i] <- "-"
        discharge_zone_id[i] <- 0
      }
    }
    else {
      nomatch <- c(nomatch, i)
      discharge_port_id[i] <- 0
      discharge_country_id[i] <- "-"
      discharge_zone_id[i] <- 0
    }
  }
}
k1 <- which(str_detect(test$DISCHARGE_PORT, "^WC INDIA"))
k2 <- which(str_detect(test$DISCHARGE_PORT, "^WC NORTH AMERICA"))
k3 <- which(str_detect(test$DISCHARGE_PORT, "^NORTH CHINA"))
k4 <- which(str_detect(test$DISCHARGE_PORT, "^EC NORTH AMERICA"))
k5 <- which(str_detect(test$DISCHARGE_PORT, "^WC AFRICA"))
k6 <- which(str_detect(test$DISCHARGE_PORT, "^EC CANADA"))
test$DISCHARGE_COUNTRY[k5] <- "WAFR"
test$DISCHARGE_ZONE[k1] <- "WCI"
test$DISCHARGE_ZONE[k2] <- "WCNA"
test$DISCHARGE_ZONE[k3] <- "KOR"
test$DISCHARGE_ZONE[k4] <- "ECNA"
test$DISCHARGE_ZONE[k5] <- "WAFR"
test$DISCHARGE_ZONE[k6] <- "ECNA"
discharge_zone_id[k1] <- 7
discharge_zone_id[k2] <- 26
discharge_zone_id[k3] <- 2
discharge_zone_id[k4] <- 19
discharge_zone_id[k5] <- 12
discharge_zone_id[k6] <- 19
nomatch <- as.numeric(nomatch[-1])
test$LOAD_COUNTRY <- gsub("^USA$|^AMERICA$","UNITED STATES OF AMERICA", test$LOAD_COUNTRY)
test$DISCHARGE_COUNTRY <- gsub("^USA$|^AMERICA$","UNITED STATES OF AMERICA", test$DISCHARGE_COUNTRY)


#### Add CARGO PRICE ####
dl_cp <- drive_download(as_id("1aXY8HwdV1PP9OhTwsu3T9WG18YTfbumG"),  overwrite = TRUE)
drive_download(dl_cp, path = "cargo_price.xlsx", overwrite = TRUE)
cargo_price<- read_excel("~/Desktop/ofe/cargo_price.xlsx", sheet = "daily_cleaned", col_name = TRUE)
cargo_price <- as.data.frame(cargo_price, stringAsFactors = FALSE)
cargo_price[,2] <- as.Date(as.numeric(cargo_price[,2]),origin = "1899-12-30")
test$UPDATE_DATE <- as.Date(test$UPDATE_DATE, format = "%Y-%m-%d")
cargo_value_mt <- ""
cargo_value_total <- ""
for (i in 1:nrow(test)){
  if(test$UPDATE_DATE[i] %in% cargo_price[,2]) a <- which(cargo_price[,2] == test$UPDATE_DATE[i])
  else a <- difftime(cargo_price[,1], test$UPDATE_DATE[i]) %>% abs %>% which.min
  cargo_i <- gsub("\\(.*|\\+.*", "", test$CARGO[i])
  if(test$LOAD_ZONE[i]=="-"|test$LOAD_ZONE[i] == "NULL") port_i <- "UKC"
  else port_i <- test$LOAD_ZONE[i]
  if(all(!str_detect(cargo_price[1,], cargo_i)|is.na(cargo_price[1,]))) cargo_i <- "CRUDE"
  if(any(str_detect(cargo_price[1,], cargo_i)&str_detect(cargo_price[2,], port_i)&!is.na(cargo_price[2,]))){
    b <- which(str_detect(cargo_price[1,], cargo_i)&str_detect(cargo_price[2,], port_i))
  }
  # else if(sum(str_detect(cargo_cp, "test")&str_detect(cargo_price[2,], test$LOAD_ZONE[i]))>0){
  #   b <- which(str_detect(cargo_cp,"test")&str_detect(cargo_price[2,], test$LOAD_ZONE[i]))
  # }
  else{
    cargo_value_mt[i] <- 0
    cargo_value_total[i] <- 0
    next
  }
  while(cargo_price[a,b] == 0) a <- a+1
  cargo_value_mt[i] <- round(as.numeric(cargo_price[a,b]), digits=2)
  cargo_value_total[i] <- round(as.numeric(cargo_value_mt[i]) * as.numeric(test$CARGO_SIZE[i]), digits=2)
}

#### Get FREIGHT PRICE -------------------
flat <- gs_read(gs_title("Flat Rate Matrix.xlsx"), ws=4)
flat <- as.data.frame(flat)
freight_price <- ""
test$CARGO_SIZE <- as.numeric(test$CARGO_SIZE)
rate <- "[0-9]+(\\.[0-9]+)?"
dp <- names(flat)
for (i in 1:nrow(test)){
  if (str_detect(test$RATE[i], "^USD[0-9]+(\\.[0-9]+)?")){
    rate1 <-  as.numeric(str_extract(str_extract(test$RATE[i], "USD[0-9]+(\\.[0-9]+)?"), rate))
    if (rate1 < 10) freight_price[i] <- round(rate1 *1000/test$CARGO_SIZE[i], digits=2)
    else freight_price[i] <- round(rate1/test$CARGO_SIZE[i], digits=2)
    next
  }
  else if (str_detect(test$RATE[i], "^W[0-9]+")){
    rate1 <-  as.numeric(str_extract(str_extract(test$RATE[i], "W[0-9]+(\\.[0-9]+)?"), rate))
    if(test$LOAD_PORT[i] %in% flat[,1]) a <- which(flat[,1]== test$LOAD_PORT[i])
    else if(test$LOAD_COUNTRY[i] %in% flat[,1]) a <- which(flat[,1]== test$LOAD_COUNTRY[i])
    else if(test$LOAD_ZONE[i] %in% flat[,1]) a <- which(flat[,1]== test$LOAD_ZONE[i])
    else{
      freight_price[i] <- 0 
      next
    } 
    
    if(test$DISCHARGE_PORT[i] %in% dp)  fr <- flat[a,which(dp == test$DISCHARGE_PORT[i])]
    else if (test$DISCHARGE_COUNTRY[i] %in% dp) fr <- flat[a,which(dp == test$DISCHARGE_COUNTRY[i])]
    else if (test$DISCHARGE_ZONE[i] %in% dp) fr <- flat[a,which(dp == test$DISCHARGE_ZONE[i])]
    else{
      freight_price[i] <- 0 
      next
    }
  }
  else{
    freight_price[i] <- 0 
    next
  }
  if(is.na(fr)) freight_price[i] <- 0
  else if(fr=="#N/A") freight_price[i] <- "-"
  else{
    if(is.na(test$DWT[i])|test$DWT[i] > 200000) fr <- as.numeric(str_extract(fr, "^[0-9]+\\.[0-9]+"))
    else fr <- as.numeric(str_extract(fr, "[0-9]+\\.[0-9]+$"))
    freight_price[i] <- round(rate1 * 0.01 *as.numeric(fr),digits=2)
  }
}
freight_price[which(freight_price == Inf)] <- 0

vessel_imo <- test$IMO
vessel_name <- test$VESSEL
cargo_size <- test$CARGO_SIZE
vessel_type <- test$VESSEL_TYPE
cargo <- test$CARGO
laycan_start <- test$LAYCAN_START
laycan_end <- test$LAYCAN_END
rate <- test$RATE
charterer <- test$CHARTERER
dwt <- test$DWT
status <- test$STATUS
load_port <- test$LOAD_PORT
load_country <- test$LOAD_COUNTRY
load_zone <- test$LOAD_ZONE
discharge_port <- test$DISCHARGE_PORT
discharge_country <- test$DISCHARGE_COUNTRY
discharge_zone <- test$DISCHARGE_ZONE
source <- test$SOURCE
terms <- test$TERMS
updated_date <- test$UPDATE_DATE
load_port_id <- as.numeric(load_port_id)
discharge_port_id <- as.numeric(discharge_port_id)
load_zone_id <- as.numeric(load_zone_id)
discharge_zone_id <- as.numeric(discharge_zone_id)
freight_price <- as.numeric(freight_price)


final <- data.frame(cbind(vessel_imo, vessel_name, cargo_size, cargo_unit="KMT",vessel_type, vessel_type_id = 0, cargo, cargo_type_id=0,cargo_value_mt, cargo_value_total), stringsAsFactors = FALSE)
final <- data.frame(cbind(final, load_port, load_port_id, load_country, load_country_id, load_zone, load_zone_id), stringsAsFactors = FALSE)
final <- data.frame(cbind(final, discharge_port, discharge_port_id, discharge_country, discharge_country_id, discharge_zone, discharge_zone_id, laycan_start, laycan_end), stringsAsFactors = FALSE)
final <- data.frame(cbind(final, rate,freight_price, charterer, dwt, status, updated_date, owner = "", cargo_id = "-", source), stringsAsFactors = FALSE)

for(i in c(12,16,18,22,26)){
  k <- which(is.na(final[,i]))
  if(length(k)>0) final[k,i] <- 0
  if(any(final[,i]=="-")) final[which(final[,i]=="-"),i] <- 0
}
for(i in c(11,13:15,17,19:21)){
  final[,i] <- as.character(final[,i])
  if(any(is.na(final[,i]))) final[which(is.na(final[,i])),i] <- "-"
  if(any(final[,i]==0)) final[which(final[,i]==0),i] <- "-"
  if(any(final[,i]=="")) final[which(final[,i]==""),i] <- "-"
}
final <- unique(final) %>% arrange(source)

#final$laycan_start[which(final$laycan_start == "2018-07-08")] <- "2018-08-07"
#final$laycan_end[which(final$laycan_end == "2018-07-08")] <- "2018-08-07"

####delete incomplete rows: which two of laycan/rate/charterer are missing--------------
rnr <- which(final$rate == "RNR")
cnr <- which(final$charterer == "CNR")
if(sum(rnr %in% cnr)>0) final <- final[-rnr[which(rnr %in% cnr)],]

dup <- unique(final[duplicated(final[,c(2,23)]),c(2,23)])
del_dup <- ""
for (i in 1:nrow(dup)){
  dupl <- which(final$vessel_name==dup[i,1]&final$laycan_start==dup[i,2])
  del_dupl <- which(final$charterer[dupl]=="CNR"|final$rate[dupl]=="RNR"|final$discharge_zone_id[dupl]==0)
  if (length(del_dupl)<length(dupl)) del_dup <- c(del_dup,dupl[del_dupl])
}
del_dup

if (length(del_dup)>1){
  final <- final[-as.numeric(del_dup[-1]),]
}


##### Remove duplicate ------------------
vessel_count<- as.data.frame(table(final$vessel_name))
vessel_unique <- vessel_count %>% filter(Freq==1)
final_subset1<- subset(final,final$vessel_name %in% vessel_unique$Var1)
vessel_select <- vessel_count %>% filter(Freq>1)
nr<-nrow(vessel_select)
final_subset2<-final[1,]
raw_subset2<- subset(final,final$vessel_name %in% vessel_select$Var1)
### Remove entry of same vessel where laycan difference within 3 days###
if(length(nr) >0){
  for(i in 1:nr){
    vessel<-as.character(vessel_select$Var1[i])
    vessel_group<- raw_subset2[which(raw_subset2$vessel_name==vessel),] %>% arrange(desc(load_port_id), desc(discharge_port_id))
    k1 <- which(vessel_group$charterer == "CNR"|str_detect(vessel_group$charterer, "\\*"))
    if(length(k1)>0& length(k1)< nrow(vessel_group)) vessel_group <- vessel_group[-k1,]
    k2 <- which(vessel_group$load_zone_id == 0)
    if(length(k2)>0 & length(k)<nrow(vessel_group)) vessel_group <- vessel_group[-k2,]
    k3 <- which(vessel_group$discharge_zone_id == 0)
    if(length(k3)>0& length(k3)<nrow(vessel_group)) vessel_group <- vessel_group[-k3,]
    n <- nrow(vessel_group)
    new_df<-vessel_group[1,]
    if(n==1) final_subset2<-rbind(final_subset2,new_df)
    else{
      for(j in 2:n){
        if(min(abs(vessel_group$laycan_start[j]- new_df$laycan_start))<6 &
           vessel_group$charterer[j] %in% new_df$charterer&
           vessel_group$discharge_zone[j]%in% new_df$discharge_zone &
           vessel_group$load_zone[j]%in% new_df$load_zone) next
        else new_df <- rbind(new_df, vessel_group[j,])
      }
      final_subset2<-rbind(final_subset2,new_df)
    }
  }
}
final_subset2<-final_subset2[-1,]
final <-rbind(final_subset2,final_subset1)

 
#####Remove exact duplicate----------------
dpp_recent1 <- read.csv("~/Desktop/ofe/dpp_recent1.csv", stringsAsFactors = F)
dpp_recent1$laycan_start <- as.Date(dpp_recent1$laycan_start, format = "%Y-%m-%d")
dpp_recent1$laycan_end <- as.Date(dpp_recent1$laycan_end, format = "%Y-%m-%d")
#dpp_recent1$laycan_start <- as.Date(dpp_recent1$laycan_start, format = "%d/%m/%y")
#dpp_recent1$laycan_end <- as.Date(dpp_recent1$laycan_end, format = "%d/%m/%y")
dpp_recent1 <- unique(dpp_recent1)
dup_f <- ""
for(i in 1:nrow(final)){
  k <- which(dpp_recent1$vessel_imo==final$vessel_imo[i]&
               dpp_recent1$charterer==final$charterer[i]&
               dpp_recent1$load_zone_id==final$load_zone_id[i]&
               dpp_recent1$discharge_zone_id==final$discharge_zone_id[i]&
               abs(dpp_recent1$laycan_start-final$laycan_start[i])<=5)
  if(length(k)>0) dup_f <- c(dup_f, i)
}
dup_f <- as.numeric(dup_f[-1])
dup_f
if(length(dup_f) >0) final_subset <- final[-dup_f,]


#### Track SOURCE-----------------
comp1 <- as.data.frame(table(clean_all$source))
comp2 <- as.data.frame(table(final$source))
comp3 <- as.data.frame(table(final_subset$source))
comp_all <- merge(x=comp1,y= comp2, by = "Var1", all=TRUE)
comp_all <- merge(x=comp_all,y= comp3, by = "Var1", all=TRUE)

write.csv(final_subset, "dpp_clean_27_aug.csv", row.names = FALSE, na = "")


#####Finalize the fixture--------------
data1 <- read.csv("~/Desktop/ofe/dpp_clean_27_aug_updated.csv", head=TRUE,stringsAsFactors = FALSE)
data1[,23] <- as.Date(data1[,23],format = "%d/%m/%y")
data1[,24] <- as.Date(data1[,24],format = "%d/%m/%y")
data1 <- unique(data1)
data1$USD <- str_extract(data1$rate, "USD[0-9]+(\\.[0-9]+)?")
data1$USD <- as.numeric(gsub("USD", "", data1$USD))
data1$W <- str_extract(data1$rate, "W[0-9]+(\\.[0-9]+)?")
data1$W <- as.numeric(gsub("W", "", data1$W))

dpp_recent1 <- read.csv("~/Desktop/ofe/dpp_recent1.csv", stringsAsFactors = F)
char_abbr<- read.csv("~/Desktop/ofe/data_clean/char_abbr.csv", stringsAsFactors = F)
dpp_recent1$laycan_start <- as.Date(dpp_recent1$laycan_start, format = "%Y-%m-%d")
dpp_recent1$laycan_end <- as.Date(dpp_recent1$laycan_end, format = "%Y-%m-%d")
#dpp_recent1$laycan_start <- as.Date(dpp_recent1$laycan_start, format = "%d/%m/%y")
#dpp_recent1$laycan_end <- as.Date(dpp_recent1$laycan_end, format = "%d/%m/%y")
n <- nrow(dpp_recent1)+1
dpp_recent1 <- rbind(dpp_recent1, data1)
dpp_recent1$cargo_size <- as.numeric(dpp_recent1$cargo_size)
time <- format(as.Date(data1$updated_date[1], "%d/%m/%y"), "%y%m%d")
for(i in n:nrow(dpp_recent1)) {
  if (dpp_recent1$charterer[i] %in% char_abbr$char)
    name <-
      char_abbr$name[which(char_abbr$char == dpp_recent1$charterer[i])]
  else name <- "CHAR"
  #time <- format(as.Date(test$UPDATE_DATE[1], format = "%d/%m/%Y"), "%y%m%d")
  char1 <- paste0("T2", name, time)
  same_char <-
    dpp_recent1[which(dpp_recent1$charterer[1:i - 1] == dpp_recent1$charterer[i]), ]
  if (nrow(same_char) == 0) {
    t <- which(str_detect(dpp_recent1$cargo_id[1:i - 1], char1))
    if (length(t) == 0)
      dpp_recent1$cargo_id[i] <- paste0(char1, "001")
    else{
      no <- str_extract(dpp_recent1$cargo_id[t], "[0-9]{2}$") %>% 
        as.numeric() %>% max()+1
      dpp_recent1$cargo_id[i] <- paste0(char1, sprintf('%0.3d', no))
    }
  }
  else if(dpp_recent1$charterer[i] == "CNR") {
    t <- which(str_detect(dpp_recent1$cargo_id[1:i - 1], char1))
    if (length(t) == 0)
      dpp_recent1$cargo_id[i] <- paste0(char1, "001")
    else{
      no <- str_extract(dpp_recent1$cargo_id[t], "[0-9]{2}$") %>% 
        as.numeric() %>% max()+1
      dpp_recent1$cargo_id[i] <- paste0(char1, sprintf('%0.3d', no))
    }
  }
  else if(dpp_recent1$charterer[i] == "UNIPEC"&dpp_recent1$discharge_port[i] == "CHINA"&
          dpp_recent1$load_port[i] %in% c("AG", "WC AFRICA")){
    k <- which(abs(same_char$laycan_start - dpp_recent1$laycan_start[i]) <= 5 &
                 same_char$vessel_imo == dpp_recent1$vessel_imo[i])
    if(length(k) >0) dpp_recent1$cargo_id[i] <- same_char$cargo_id[k[1]]
    else{
      t <- which(str_detect(dpp_recent1$cargo_id[1:i - 1], char1))
      if (length(t) == 0) dpp_recent1$cargo_id[i] <- paste0(char1, "001")
      else{
        no <- str_extract(dpp_recent1$cargo_id[t], "[0-9]{2}$") %>% 
          as.numeric() %>% max()+1
        dpp_recent1$cargo_id[i] <- paste0(char1, sprintf('%0.3d', no))
      }
    }
  }
  else{
    if (dpp_recent1$cargo_size[i] != 0) {
      k <- which(
        abs(same_char$cargo_size - dpp_recent1$cargo_size[i]) <= 10 &
          same_char$load_zone_id == dpp_recent1$load_zone_id[i] &
          abs(same_char$laycan_start - dpp_recent1$laycan_start[i]) <= 5
      )
      if (length(k) > 0) {
        if (dpp_recent1$vessel_imo[i] %in% same_char$vessel_imo[k]) {
          g <- which(same_char$vessel_imo[k] == dpp_recent1$vessel_imo[i])
          dpp_recent1$cargo_id[i] <- same_char$cargo_id[k[g[1]]]
        }
        else{
          t <- 0
          if (!is.na(dpp_recent1$W[i]) &
              !all(is.na(same_char$W[k]))) {
            g <- which.min(abs(same_char$W[k] - dpp_recent1$W[i]))
            if (abs(same_char$W[k[g[1]]] - dpp_recent1$W[i]) <= 5 &
                dpp_recent1$discharge_zone_id[i] == same_char$discharge_zone_id[k[g[1]]])
              t <- 1
          }
          else if (!is.na(dpp_recent1$USD[i]) &
                   !all(is.na(same_char$USD[k]))) {
            g <- which.min(abs(same_char$USD[k] - dpp_recent1$USD[i]))
            if (dpp_recent1$USD[i] < 10 &
                abs(same_char$USD[k[g[1]]] - dpp_recent1$USD[i]) < 0.1)
              t <- 1
            else if (dpp_recent1$USD[i] > 10 &
                     abs(same_char$USD[k[g[1]]] - dpp_recent1$USD[i]) <= 50)
              t <- 1
          }
          else if (dpp_recent1$discharge_zone_id[i] %in% same_char$discharge_zone_id[k]) {
            g <-
              which(same_char$discharge_zone_id[k] == dpp_recent1$discharge_zone_id[i])
            fix_s <- unique(same_char$cargo_id[k[g]])
            #if(length(fix_s)>1) print(i)
            for (j in 1:length(fix_s)) {
              l <-
                dpp_recent1$laycan_start[which(dpp_recent1$cargo_id[1:i - 1] == fix_s[j])]
              if (abs(max(l - dpp_recent1$laycan_start[i])) <= 5) {
                g <- which(same_char$cargo_id[k] == fix_s[j])
                t <- 1
                break
              }
            }
          }
          if (t == 1)
            dpp_recent1$cargo_id[i] <- same_char$cargo_id[k[g[1]]]
          else{
            t <- which(str_detect(dpp_recent1$cargo_id[1:i - 1], char1))
            if (length(t) == 0)
              dpp_recent1$cargo_id[i] <- paste0(char1, "001")
            else{
              no <- str_extract(dpp_recent1$cargo_id[t], "[0-9]{2}$") %>% 
                as.numeric() %>% max()+1
              dpp_recent1$cargo_id[i] <- paste0(char1, sprintf('%0.3d', no))
            }
          }
        }
      }
      else{
        t <- which(str_detect(dpp_recent1$cargo_id[1:i - 1], char1))
        if (length(t) == 0)
          dpp_recent1$cargo_id[i] <- paste0(char1, "001")
        else{
          no <- str_extract(dpp_recent1$cargo_id[t], "[0-9]{2}$") %>% 
            as.numeric() %>% max()+1
          dpp_recent1$cargo_id[i] <- paste0(char1, sprintf('%0.3d', no))
        }
      }
    }
    else{
      k <- which(
        same_char$load_zone_id == dpp_recent1$load_zone_id[i] &
          same_char$vessel_imo == dpp_recent1$vessel_imo[i] &
          abs(same_char$laycan_start - dpp_recent1$laycan_start[i]) < 5
      )
      if (length(k) > 0) {
        dpp_recent1$cargo_id[i] <- same_char$cargo_id[k[1]]
        dpp_recent1$cargo_size[i] <- same_char$cargo_size[k[1]]
      }
      else{
        t <- which(str_detect(dpp_recent1$cargo_id[1:i - 1], char1))
        if (length(t) == 0)
          dpp_recent1$cargo_id[i] <- paste0(char1, "001")
        else{
          no <- str_extract(dpp_recent1$cargo_id[t], "[0-9]{2}$") %>% 
            as.numeric() %>% max()+1
          dpp_recent1$cargo_id[i] <- paste0(char1, sprintf('%0.3d', no))
        }
      }
    }
  }
}


data1 <- data1[,-c(34:35)]
data1$cargo_id <- dpp_recent1$cargo_id[n:nrow(dpp_recent1)]

#### Match VESSEL TYPE------------
data1$dwt <- as.numeric(data1$dwt)
data1$vessel_type[data1$dwt <= 15000 & data1$dwt != 0] <- "COASTER(D)"
data1$vessel_type[data1$dwt <= 35000 & data1$dwt > 15001] <- "HANDYSIZE(D)"
data1$vessel_type[data1$dwt <= 55000 & data1$dwt > 35001] <- "MR(D)"
data1$vessel_type[data1$dwt <= 80000 & data1$dwt > 55001] <- "PANAMAX"
data1$vessel_type[data1$dwt <= 125000 & data1$dwt > 80001] <- "AFRAMAX"
data1$vessel_type[data1$dwt <= 200000 & data1$dwt > 125001] <- "SUEZMAX"
data1$vessel_type[data1$dwt >= 200000] <- "VLCC"
data1$vessel_type[which(data1$vessel_type == "")] <- "-"

##### Add Owner------------
for(i in 1:nrow(data1)){
  if(data1$vessel_imo[i] != 1111111){
    k <- which(vessel_info$imo == data1$vessel_imo[i])
    if(length(k) >0) data1$owner[i] <- as.character(vessel_info$X9[k])
    else print(i)
  }
}
data1$owner <- toupper(data1$owner)

write.csv(data1, "dpp_27_aug_updated1.csv", row.names = FALSE)


dpp_recent1 <- unique(dpp_recent1)
write.csv(dpp_recent1, "dpp_recent1.csv", row.names= FALSE)

#### Replace OCEANIA---------------------
data_clp <- data1
oceania_p <- read.csv("~/Desktop/ofe/data_clean/oceania_port.csv", stringsAsFactors = F)
for(i in 1:nrow(data_clp)){
  if(data_clp$load_port_id[i] != 0 & data_clp$load_port_id[i] %in% oceania_p$port_id) data_clp$load_zone[i] <- oceania_p$area[which(oceania_p$port_id == data_clp$load_port_id[i])]
  else if (data_clp$load_country_id == "NZL") data_clp$load_zone[i] <- "NZ"
  if(data_clp$discharge_port_id[i] != 0 & data_clp$discharge_port_id[i] %in% oceania_p$port_id) data_clp$discharge_zone[i] <- oceania_p$area[which(oceania_p$port_id == data_clp$discharge_port_id[i])]
  else if (data_clp$discharge_country_id == "NZL") data_clp$load_zone[i] <- "NZ"
}

write.csv(data_clp, "dpp_27_aug_updated1_clp.csv", row.names = FALSE)

#######Compare with records before------------------
dpp_recent <- read.csv("~/Desktop/ofe/dpp_recent.csv", stringsAsFactors = F)
# laycan_start <- as.Date(dpp_recent$laycan_start, format = "%d/%m/%y")
laycan_start <- as.Date(dpp_recent$laycan_start, format = "%Y-%m-%d")
test <- data1[1,]
for(i in 1:nrow(data1)){
  k <- which(dpp_recent$vessel_name == data1$vessel_name[i] & abs(laycan_start - data1$laycan_start[i])<4)
  if(length(k) > 0){
    if((data1$charterer[i] %in% dpp_recent$charterer[k]) &
       (data1$load_zone_id[i] %in% dpp_recent$load_zone_id[k]) &
       (data1$discharge_zone_id[i] %in% dpp_recent$discharge_zone_id[k])) next
    else test <- rbind(test, data1[i,])
  }
  else test <- rbind(test, data1[i,])
}
test <- test[-1,]
test <- test[,-c(30:33)]
dpp_recent$laycan_start <- as.Date(dpp_recent$laycan_start, format = "%Y-%m-%d")
dpp_recent$laycan_end <- as.Date(dpp_recent$laycan_end, format = "%Y-%m-%d")
#dpp_recent$laycan_start <- as.Date(dpp_recent$laycan_start, format = "%d/%m/%y")
#dpp_recent$laycan_end <- as.Date(dpp_recent$laycan_end, format = "%d/%m/%y")
dpp_recent <- rbind(dpp_recent, test)
write.csv(dpp_recent, "dpp_recent.csv", row.names= FALSE)

test$delivery_port <- "-"
test$redelivery_port <- "-"
test$period_min <- NA
test$period_max <- NA
test$charter_type <- "VOYAGE"
test$terms <- NA
test$loadport_id_vertified_ais <- NA
test$loadport_vertified_ais <- NA

write.csv(test, "dpp_20_aug_final.csv", row.names= FALSE, na = "")

