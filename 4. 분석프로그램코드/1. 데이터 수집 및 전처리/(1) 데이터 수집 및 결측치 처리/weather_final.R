##Source
library(stringr)
library(magrittr)
library(tidyverse)


#################################################
#################### minmax #####################
#################################################
#n : 처음, 마지막 날짜를 확인할 데이터명
#사용예제
#list = dbGetQuery(conn, "show tables")
#daily_list = list[str_detect(list[,1], "_dd"),] #"_dd$"로 잡으면 _dd_old 못 잡음
#하나씩 볼 경우 : minmax(daily_list[3])
#한꺼번에 볼 경우 : minmax(daily_list)
minmax <- function(name, prog = F){
  out = NULL
  for (n in name){
    y = dbGetQuery(conn, paste("select * from", n, "as a where 1=1 limit 0")) %>% colnames() %>% head(1)
    yr = dbGetQuery(conn, paste("select min(", y, "), max(", y, ") from ", n, " as a", sep = ""))
    tryCatch(
      {
        if (class(yr[1, 1]) == "character") yr = yr %>% str_extract("^[0-9]{4}-[0-9]{2}-[0-9]{2}") %>% str_replace_all("-", "") %>% as.numeric()
      },
      error = function(cnd){
        if (class(yr[1, 1]) == "character") yr = str_extract_all(yr, "[0-9]+")[[1]][1:3] %>% paste(collapse = "") %>% as.numeric()
        else yr = yr
      }
    )
    yr = matrix(yr, ncol = 2);rownames(yr) <- n
    if (prog == T) print(yr)
    out = rbind(out, yr)
  }
  return(out)
}


#################################################
################# preprocess ####################
#################################################
# n : table name ex) n = daily_list[1]
# output : 12-16년도 모든 파일 병합본 n.csv로 export 폴더에 저장
# + 기존 연월별 파일들 export에서 삭제(한꺼번에 하는 게 편하니까용,,)
preprocess <- function(n, Y = 2011:2016, M = 1:12, filename = NULL){
  final = NULL
  for (y in Y){
    for (m in M){
      file_path = paste("~/export/", n, "_", y, "_", m, ".csv", sep = "")
      tryCatch(
        {
          if (colnames(read.csv(file_path))[1] == "X") final = rbind(final, read.csv(file_path)[,-1])
          else final = rbind(final, read.csv(file_path))
        },
        error = function(e) print(paste("No such obs :", file_path))
      )
    }
  }
  
  #병합한 파일 export에 저장
  if (is.null(filename)) file_path = str_split(getwd(), "/")[[1]][1:4] %>% paste(collapse = "/") %>% paste("/export/", n, ".csv", sep = "")
  else file_path = str_split(getwd(), "/")[[1]][1:4] %>% paste(collapse = "/") %>% paste("/export/", filename, ".csv", sep = "")
  write.csv(final, file_path, row.names = F)
  #기존 연월별 파일 삭제
  file_path = paste(str_split(file_path, "/")[[1]][1:5], collapse = "/")
  #중간에 숫자가 들어가야만 삭제
  filename = list.files(file_path, pattern=str_replace("^{1}[0-9_]+.csv$", "[{]1[}]", n), full.names = TRUE)
  if (length(filename) != 0) file.remove(filename)
}




#################################################
################## getDDData ####################
#################################################
# 세 번의 수정을 거친 찐최종본
# n : 수집하고자 하는 table name
# Y : 수집하고자 하는 연도
# M : 수집하고자 하는 월
# ex) getDDData(daily_list[13], 2015, 1:12) > 2015년도 1월부터 12월까지의 데이터
getDDData <- function(n, Y, M){
  a.yr = dbGetQuery(conn, paste("select * from", n, "as a where 1=1 limit 0")) %>% colnames() %>% head(1)
  query = paste("select * from", n, "as a where", a.yr, "like '{3}%'")
  for (y in Y){
    for (m in M){
      ym = paste(y, str_pad(as.character(m), 2, "left", "0"), sep = "-")
      query = str_replace(query, "[{]3[}]", ym)
      df = dbGetQuery(conn, query)
      if (nrow(df) == 0) print(paste("0 elements in", n, ",", ym))
      else{
        print(paste(nrow(df), "elements in", n, ",", ym))
        file_path = str_split(getwd(), "/")[[1]][1:4] %>% paste(collapse = "/") %>% 
          paste("/export/", n, "_", y, "_", m, ".csv", sep = "")
        write.csv(df, file_path, row.names = F)
      }
      query = str_replace(query, ym, "{3}")
    }
  }
}


#################################################
################### getData #####################
#################################################
getData <- function(n, Y = 2011:2016, M = 1:12, except = NULL, fore = F){
  assign("n", n, envir = .GlobalEnv)
  assign("Y", Y, envir = .GlobalEnv)
  assign("M", M, envir = .GlobalEnv)
  if (!is.null(except)){
    varname = dbGetQuery(conn, paste("select * from", n, "as a where 1=1 limit 0")) %>% colnames()
    a.yr = varname[1]
    varname = paste(varname[-except], collapse = ", ")
    query = paste("select", varname, "from", n, "as a where", a.yr, "like '{3}%'")
  }
  else{
    a.yr = dbGetQuery(conn, paste("select * from", n, "as a where 1=1 limit 0")) %>% colnames() %>% head(1)
    query = paste("select * from", n, "as a where", a.yr, "like '{3}%'")
  }
  if (fore == T){
    query = paste("select a.tm_ef, a.stn_id, a.imi_code, a.imi_num from", n, "as a where a.imi_code like 'B01' and a.tm_ef like '{3}%'")
  }
  for (y in Y){
    for (m in M){
      ym = paste(y, str_pad(as.character(m), 2, "left", "0"), sep = "-")
      query = str_replace(query, "[{]3[}]", ym)
      #print(query)
      tryCatch(
        {
          df = dbGetQuery(conn, query)
          if (nrow(df) == 0) print(paste("0 elements in", n, ",", ym))
          else{
            print(paste(nrow(df), "elements in", n, ",", ym))
            file_path = paste("~/export/", n, "_", y, "_", m, ".csv", sep = "")
            write.csv(df, file_path, row.names = F)
          }
          query = str_replace(query, ym, "{3}")
          #print(query)
        },
        error = function(e){
          print(paste("error occured at", n, ",", ym))
          lastY = Y[Y > y]
          lastYM = rbind(cbind(y, M[M >= m]), cbind(rep(lastY, rep(length(M), length(lastY))), rep(M, length(lastY))))
          colnames(lastYM) <- c(n, "")
          write.csv(data.frame(lastYM), "~/lastYM.csv", row.names = F)
          .rs.restartR()
        }
      )
    }
  }
}

#getSingle <- function(a.yr, y, m){
#  ym = paste(y, str_pad(as.character(m), 2, "left", "0"), sep = "-")
#  query = str_replace(query, "[{]3[}]", ym)
#  tryCatch(
#    {
#      df = dbGetQuery(conn, query)
#      if (nrow(df) == 0) print(paste("0 elements in", n, ",", ym))
#      else{
#        print(paste(nrow(df), "elements in", n, ",", ym))
#        file_path = paste("~/export/", n, "_", y, "_", m, ".csv", sep = "")
#        write.csv(df, file_path, row.names = F)
#      }
#    },
#    error = function(e){
#      print(paste("error occured at", n, ",", ym))
#      lastY = Y[Y > y]
#      lastYM = rbind(cbind(y, M[M >= m]), cbind(rep(lastY, rep(length(M), length(lastY))), rep(M, length(lastY))))
#      colnames(lastYM) <- c(n, "")
#      write.csv(data.frame(lastYM), "~/lastYM.csv", row.names = F)
#      .rs.restartR()
#    }
#  )
#}

#################################################
################## if_error #####################
#################################################
if_error <- function(M = 1:12, N = NULL){
  #source("~/export/yslib_0620.R")
  YM = read.csv("~/lastYM.csv")
  n = colnames(YM)[1]
  assign("YM", YM, envir = .GlobalEnv)
  a.yr = dbGetQuery(conn, paste("select * from", n, "as a where 1=1 limit 0")) %>% colnames() %>% head(1)
  query = paste("select * from", n, "as a where", a.yr, "like '{3}%'")
  for (i in 1:dim(YM)[1]){
    y = YM[i, 1]; m = YM[i, 2]
    ym = paste(y, str_pad(as.character(m), 2, "left", "0"), sep = "-")
    query = str_replace(query, "[{]3[}]", ym)
    tryCatch(
      {
        df = dbGetQuery(conn, query)
        if (nrow(df) == 0) print(paste("0 elements in", n, ",", ym))
        else{
          print(paste(nrow(df), "elements in", n, ",", ym))
          file_path = paste("~/export/", n, "_", y, "_", m, ".csv", sep = "")
          write.csv(df, file_path, row.names = F)
        }
        query = str_replace(query, ym, "{3}")
      },
      error = function(e){
        print(paste("error occured at", n, ",", ym))
        lastYM = YM[i:dim(YM)[1],]
        colnames(lastYM) <- c(n, "")
        write.csv(data.frame(lastYM), "~/lastYM.csv", row.names = F)
        .rs.restartR()
      }
    )
  }
  if (!is.null(N)){
    print(paste("congrats! to the NEXT LEVEL :", N[1]))
    for (n in N) getData(n, 2011:2016, 1:12)
  }
  else print("congrats! to the NEXT LEVEL")
}