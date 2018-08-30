library(plyr)
library(randomForest)

#####Fixtures Score---------------
train <- read.csv("~/Desktop/ofe/fixture_score/dpp_historical_train_validated.csv", header=TRUE, stringsAsFactors=FALSE)
#train <- train[train$source!='-',]
dpp_vrf <- read.csv("~/Desktop/ofe/dpp_load_port_test_after0712.csv", stringsAsFactors=FALSE) %>% arrange(vessel_imo, desc(load_port_id), desc(discharge_zone_id), desc(updated_date))
new <- dpp_vrf[,c(1:33)]

#new$laycan_start <- as.Date(new$laycan_start, format="%d/%m/%Y")
#new$laycan_end <- as.Date(new$laycan_end, format="%d/%m/%Y")
#new$updated_date <- as.Date(new$updated_date, format="%d/%m/%Y")
new$laycan_start <- as.Date(new$laycan_start, format = "%Y-%m-%d")
new$laycan_end <- as.Date(new$laycan_end, format = "%Y-%m-%d")
new$updated_date <- as.Date(new$updated_date, format = "%d/%m/%y")
which(is.na(new$dwt))
which(is.na(new$freight_price))

row.names(train) <- NULL
train_X <- train[,c(1:33)]
train_X$laycan_start <- as.Date(train_X$laycan_start)
train_X$laycan_end <- as.Date(train_X$laycan_end)
train_X$updated_date <- as.Date(train_X$updated_date)
#train_X$dwt[is.na(train_X$dwt)] <- 115098

top_char <- as.data.frame(table(train_X$charterer))
top_char <- top_char[order(-top_char$Freq),]
top_char <- top_char[toupper(top_char$Var1) != "CNR",]
top_char <- top_char[1:20,1]

X <- rbind(train_X, new)
row.names(X) <- NULL
X <- X[,c(3,5,7,10,12,14,16,18,20,22,23,24,25,26,27,28,29,30,33)]
X$load_port_provided <- (X$load_port_id != 0)
X$load_country_provided <- (X$load_country_id != '-')
X$discharge_port_provided <- (X$discharge_port_id != 0)
X$discharge_country_id[X$discharge_country_id==''] <- '-'
X$discharge_country_provided <- (X$discharge_country_id != '-')
X$freight_price[is.na(X$freight_price)] <- 0
X$updated_in_advance <- difftime(X$laycan_start, X$updated_date, units="days")
X$rate_provided <- (X$rate != 'RNR')


X$charterer[!(X$charterer %in% top_char)] <- 'OTHERS' # change charterer to others, if the charterer is not in the top_char list

X$status[X$status=='CORR FXD'] <- 'FXD CORR'
X$status[X$status=='RPLC FXD'] <- 'FXD RPLC'
X$status[grepl("SUB", X$status)] <- 'SUB'
X$status[grepl("FLD", X$status)] <- 'FLD'

X <- X[,c(2,3,4,7,10,14,15,16,17,19,20,21,22,23,24,25)]

row_added <- rep(0,16)
X <- rbind(X, row_added)
X[nrow(X),c(1,2,4,5,7,9,10)] <- 'unknown'

col_cat = c(1,2,4,5,7,9,10)
for (i in 1:length(col_cat)){
  X[!(X[,col_cat[i]] %in% X[1:nrow(train),col_cat[i]]), col_cat[i]] <- 'unknown'
}
for (i in 1:length(col_cat)){
  X[,col_cat[i]] <- as.factor(X[,col_cat[i]])
}


X <- data.frame(model.matrix(~., data=X)[,-1])
newdata <- X[-(1:nrow(train)),]
newdata <- newdata[-nrow(newdata),]

load("~/Desktop/ofe/fixture_score/dpp_rf_0820.RData")
y_prob <- predict(rf, newdata=newdata, type='prob')
dpp_vrf$score <- y_prob[,2]

######Fixtures Selection -------------------
dpp_vrf$correctness <- ""
dpp_vrf$laycan_start <- as.Date(dpp_vrf$laycan_start, format="%d/%m/%y")
dpp_vrf$laycan_end <- as.Date(dpp_vrf$laycan_end, format="%d/%m/%y")
dpp_vrf$updated_date <- as.Date(dpp_vrf$updated_date, format="%d/%m/%y")

#### Delete TBN--------------------------
tbn <- which(dpp_vrf$vessel_imo=="1111111")
for(i in 1:length(tbn)){
  tbn_v <- which(dpp_vrf$cargo_id == dpp_vrf$cargo_id[tbn[i]] & dpp_vrf$vessel_imo != "1111111")
  if(length(tbn_v)>0) dpp_vrf$correctness[tbn[i]] <- -1
  else dpp_vrf$correctness[tbn[i]] <- "-"
}
slc_f <- dpp_vrf[1,]

vsl_list <- as.data.frame(table(dpp_vrf$vessel_imo))[-1,]
for(i in 1:nrow(vsl_list)){
  vsl_slc <- dpp_vrf[which(dpp_vrf$vessel_imo == vsl_list$Var1[i]),]
  while(length(which(vsl_slc$correctness == ""))>0){
    bs_v <- which(vsl_slc$correctness == "")[1]
    bs_v_sle <- which(abs(vsl_slc$laycan_start- vsl_slc$laycan_start[bs_v])<7 & vsl_slc$correctness == "")
    if(length(bs_v_sle) == 1){
      if(str_detect(vsl_slc$status[bs_v_sle], "FLD")) vsl_slc$correctness[bs_v_sle] <- -1
      else if(str_detect(vsl_slc$status[bs_v_sle], "FXD")) vsl_slc$correctness[bs_v_sle] <- 1
      else vsl_slc$correctness[bs_v_sle] <- "-"
    }
    else if(any(str_detect(vsl_slc$status[bs_v_sle], "FLD|FXD|CNCL"))){
      if(any(str_detect(vsl_slc$status[bs_v_sle], "FLD")&str_detect(vsl_slc$status[bs_v_sle], "FXD"))) print(i)
      fxd <- bs_v_sle[which(str_detect(vsl_slc$status[bs_v_sle], "FXD"))]
      if(length(fxd) >0){
        vsl_slc$correctness[fxd] <- 1
        fxd_dup <- bs_v_sle[which((vsl_slc$charterer[bs_v_sle] %in% vsl_slc$charterer[fxd]& 
                            (vsl_slc$load_zone_id[bs_v_sle] %in% vsl_slc$load_zone_id[fxd]|vsl_slc$rate[bs_v_sle] %in% vsl_slc$rate[fxd]))|
                           (vsl_slc$charterer[bs_v_sle] == "CNR"&vsl_slc$load_zone_id[bs_v_sle] %in% vsl_slc$load_zone_id[fxd]&
                              vsl_slc$discharge_zone_id[bs_v_sle] %in% vsl_slc$discharge_zone_id[fxd]))]
        fxd_dup <- fxd_dup[-which(fxd_dup %in% fxd)]
        # Combine info if any CNR RNR or missing value
        if(length(fxd_dup)>0){
          if(vsl_slc$rate[fxd] == "RNR" & any(vsl_slc$rate[fxd_dup] != "RNR")){
            dup_r <- fxd_dup[which(vsl_slc$rate[fxd_dup] != "RNR")]
            vsl_slc$rate[fxd] <- vsl_slc$rate[dup_r][1]
          }
          # if(vsl_slc$cargo[fxd] == "dpp" & any(vsl_slc$cargo[fxd_dup] != "dpp")){
          #   dup_c <- fxd_dup[which(vsl_slc$cargo[fxd_dup] != "dpp")]
          #   vsl_slc$cargo[fxd] <- vsl_slc$cargo[dup_c][1]
          # }
          if(vsl_slc$load_port_id[fxd] == 0 & any(vsl_slc$load_port_id[fxd_dup]>0 & vsl_slc$load_country_id[fxd_dup] %in% vsl_slc$load_zone_id[fxd] &
                                                  vsl_slc$load_zone_id[fxd_dup] %in% vsl_slc$load_zone_id[fxd])){
            dup_p <- fxd_dup[which(sl_slc$load_port_id[fxd_dup]>0 & vsl_slc$load_country_id[fxd_dup] %in% vsl_slc$load_zone_id[fxd] &
                                     vsl_slc$load_zone_id[fxd_dup] %in% vsl_slc$load_zone_id[fxd])]
            vsl_slc$load_port[fxd] <- vsl_slc$load_port[dup_p]
            vsl_slc$load_port_id[fxd] <- vsl_slc$load_port_id[dup_p]
          }
          vsl_slc$correctness[fxd_dup] <- -1
        }
      }
      fld <- bs_v_sle[which(str_detect(vsl_slc$status[bs_v_sle], "FLD|CNCL"))]
      if(length(fld) > 0){
        # Minimize the number of fld shows
        if(length(fld)>1){
          if(sum(duplicated(vsl_slc[fld,c(7, 12, 14, 16, 18, 20, 22, 25, 27)]))>0) fld <- fld[-which(duplicated(vsl_slc[fld,c(7,12, 14, 16, 18, 20, 22, 25, 27)]))]
          if(sum(duplicated(vsl_slc[fld,c(7, 12, 16, 18, 20, 27)]))>0) fld <- fld[-which(duplicated(vsl_slc[fld,c(7, 12, 16, 18, 20, 27)]))]
          if(sum(duplicated(vsl_slc[fld,c(16, 20, 27)])&(vsl_slc$cargo[fld]=="CRUDE"|vsl_slc$load_port_id[fld]==0|vsl_slc$discharge_port_id[fld]==0))>0) fld <- 
              fld[-which(duplicated(vsl_slc[fld,c(16, 20, 27)])&(vsl_slc$cargo[fld]=="CRUDE"|vsl_slc$load_port_id[fld]==0|vsl_slc$discharge_port_id[fld]==0))]
          if("CNR" %in% vsl_slc$charterer[fld] & sum(which(vsl_slc$charterer== "CNR")) < length(fld)) fld <- fld[-which(vsl_slc$charterer[fld] == "CNR")]
        }
        vsl_slc$correctness[fld] <- 0
        fld_dup <- bs_v_sle[which((vsl_slc$charterer[bs_v_sle] %in% vsl_slc$charterer[fld]& 
                            (vsl_slc$load_zone_id[bs_v_sle] %in% vsl_slc$load_zone_id[fld]|vsl_slc$rate[bs_v_sle] %in% vsl_slc$rate[fld]))|
                           (vsl_slc$charterer[bs_v_sle] == "CNR"&vsl_slc$load_zone_id[bs_v_sle] %in% vsl_slc$load_zone_id[fld]&
                              vsl_slc$discharge_zone_id[bs_v_sle] %in% vsl_slc$discharge_zone_id[fld]))]
        fld_dup <- fld_dup[-which(fld_dup %in% fld)]
        vsl_slc$correctness[fld_dup] <- -1
      }
    }
    else{
      no_dz <- which(vsl_slc$discharge_zone_id[bs_v_sle] == 0)
      if(length(no_dz)>0 & length(no_dz) < length(bs_v_sle)) dup_d <- unique(c(no_dz,which(duplicated(vsl_slc[bs_v_sle, c(16, 22, 27)]))))
      else dup_d <- which(duplicated(vsl_slc[bs_v_sle, c(16, 22, 27)]))
      if(length(dup_d) > 0){
        vsl_slc$correctness[bs_v_sle[dup_d]] <- -1
        # bs_v_sle <- bs_v_sle[-dup_d]
        vsl_slc$correctness[bs_v_sle[-dup_d]] <- "-"
      }
      else vsl_slc$correctness[bs_v_sle] <- "-"
      # if(any(vsl_slc$score[bs_v_sle]>0.59)){
      #   good_f <- which(vsl_slc$score[bs_v_sle]>0.59)
      #   vsl_slc$correctness[bs_v_sle[good_f]] <- "-"
      #   vsl_slc$correctness[bs_v_sle[-good_f]] <- -1
      # }
      # else{
      #   max_score <- which.max(vsl_slc$score[bs_v_sle])
      #   vsl_slc$correctness[bs_v_sle[max_score]] <- "-"
      #   vsl_slc$correctness[bs_v_sle[-max_score]] <- -1
      # } 
    }
  }
  slc_f <- rbind(slc_f, vsl_slc)
}

# fit <- rpart(dpp_vrf$zone_compare ~ cargo_size + cargo + load_country + load_zone + dwt, data = dpp_vrf ,car.test.frame)
# printcp(fit)
fxd_f <- which(slc_f$correctness == 1)
for(i in 1:length(fxd_f)){
  id_match <- which(slc_f$cargo_id == slc_f$cargo_id[fxd_f[i]] & slc_f$correctness == "-" & slc_f$laycan_start - slc_f$laycan_start[fxd_f[i]] < 5 &
                      slc_f$load_port_id == slc_f$load_port_id[fxd_f[i]] & slc_f$load_zone_id == slc_f$load_zone_id[fxd_f[i]] &
                      slc_f$discharge_port_id == slc_f$discharge_port_id[fxd_f[i]] & slc_f$discharge_zone_id == slc_f$discharge_zone_id[fxd_f[i]] &
                      (slc_f$cargo[fxd_f[i]] == "CRUDE"|slc_f$cargo == slc_f$cargo[fxd_f[i]]))
  if(length(id_match)>0){
    slc_f$correctness[id_match] <- 0
    slc_f$status[id_match] <- paste0(slc_f$status[id_match], "(FLD)")
  } 
}

slc_f <- slc_f[-which(slc_f$correctness == -1),]
slc_f$correctness[which(slc_f$correctness == "-")] <- slc_f$score[which(slc_f$correctness == "-")]
slc_f$correctness <- round(as.numeric(slc_f$correctness), digits = 3)
slc_f <- slc_f[,-34]

slc_f$delivery_port <- "-"
slc_f$redelivery_port <- "-"
slc_f$charter_type <- "VOYAGE"

write.csv(slc_f, "dpp_12_aug_to_20_aug_final.csv",row.names = FALSE)



###merge fixtures------------------
dpp1 <- read.csv("~/Desktop/ofe/dpp_24_jul_final.csv", stringsAsFactors = F)
dpp2 <- read.csv("~/Desktop/ofe/dpp_25_jul_final.csv", stringsAsFactors = F)
dpp3 <- read.csv("~/Desktop/ofe/dpp_26_jul_final.csv", stringsAsFactors = F)
dpp4 <- read.csv("~/Desktop/ofe/dpp_27_jul_final.csv", stringsAsFactors = F)
dpp5 <- read.csv("~/Desktop/ofe/dpp_30_jul_final.csv", stringsAsFactors = F)
dpp6 <- read.csv("~/Desktop/ofe/dpp_1_aug_final.csv", stringsAsFactors = F)
dpp_all <- rbind(dpp1,dpp2,dpp3,dpp4,dpp5,dpp6)
dpp_all$laycan_start <- as.Date(dpp_all$laycan_start, "%Y-%m-%d")
dpp_all$laycan_end <- as.Date(dpp_all$laycan_end, "%Y-%m-%d")
write.csv(dpp_all, "dpp_all.csv", row.names = FALSE)


dpp1 <- read.csv("~/Desktop/ofe/dpp_24_jul_final.csv", stringsAsFactors = F)
dpp2 <- read.csv("~/Desktop/ofe/dpp_25_jul_final.csv", stringsAsFactors = F)
dpp3 <- read.csv("~/Desktop/ofe/dpp_26_jul_final.csv", stringsAsFactors = F)
dpp4 <- read.csv("~/Desktop/ofe/dpp_27_jul_final.csv", stringsAsFactors = F)
dpp5 <- read.csv("~/Desktop/ofe/dpp_30_jul_final.csv", stringsAsFactors = F)
dpp6 <- read.csv("~/Desktop/ofe/dpp_1_aug_final.csv", stringsAsFactors = F)
dpp_all <- rbind(dpp1,dpp2,dpp3,dpp4,dpp5,dpp6)
dpp_all$laycan_start <- as.Date(dpp_all$laycan_start, "%Y-%m-%d")
dpp_all$laycan_end <- as.Date(dpp_all$laycan_end, "%Y-%m-%d")
write.csv(dpp_all, "dpp_all.csv", row.names = FALSE)