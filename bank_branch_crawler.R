library(magrittr)
library(httr)
library(rvest)
library(data.table)
library(stringr)
library(parallel)
library(addr2gps)
library(testthat)

set_config(config(ssl_verifypeer = 0L))

list(
  本國銀行 = "https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankType.jsp&type=1",
  外國銀行 = "https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankType.jsp&type=3"
)

get_bank_list <- function(bank_type = 1, ...) {
  # bank_type = 1
  
  ua <- user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")
  url <- sprintf("https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankType.jsp&type=%s", bank_type)
  doc <- GET(url, 
             ...,
             ua) %>% content("text") %>% read_html
  pages <- doc %>% html_nodes("#bankingform > div.col-md-1 > nav > ul > li > a") %>% 
    html_text()
  
  bank_page <- function(page, ...) {
    doc <- POST("https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankType.jsp",
                add_headers(
                  Referer = "https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankType.jsp"
                ),
                ...,
                ua,
                # set_cookies(
                #   JSESSIONID="17A0DC9945B19396DECE2BDB261BD458",
                #   cookiesession1="255EA488AOTNPAZMGAAPL3HLWVQPA52C"
                # ),
                body = list(
                  intpage = 3,
                  type = 1,
                  display = "null",
                  page = page,
                  pagesize=""
                ),
                encode = "form") %>% 
      content("text") %>% read_html()
    
    out <- data.table(
      bank_code = doc %>% html_nodes(".fcode_con") %>% html_text,
      bank_name = doc %>% html_nodes(".forganization_name_con a") %>% html_attr("title"),
      bank_info_url = doc %>% html_nodes(".forganization_name_con a") %>% html_attr("href") %>% 
        paste0("https://www.banking.gov.tw/ch/", .),
      addr = doc %>% html_nodes(".fadd_con") %>% html_text,
      tel_no = doc %>% html_nodes(".ftel_con") %>% html_text,
      bank_url = doc %>% html_nodes(".furl_con a:nth-child(1)") %>% html_attr("href")
    )
    out[, addr := addr %>% str_extract("^[^號]+號?")]
    out
  }
  out_list <- pages %>% lapply(bank_page, ...)
  out <- out_list %>% rbindlist(fill = TRUE)
  out
}


get_branch <- function(bank_code, ...) {
  # bank_code = 040
  bank_code = str_pad(bank_code[1], 3, pad = "0")
  
  ua <- user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")
  url <- sprintf("https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankSub.jsp&bank_no=%s",
                 bank_code)
  doc <- GET(url, ..., ua) %>% content("text") %>% read_html
  
  tryCatch({
    out <- data.table(
      bank_code = bank_code,
      branch_code = doc %>% html_nodes(".fscode_con") %>% html_text(),
      branch_name = doc %>% html_nodes(".fstitle_con a") %>% html_attr("title"),
      branch_url = doc %>% html_nodes(".fstitle_con a") %>% html_attr("href") %>% 
        paste0("https://www.banking.gov.tw/ch/", .)
    )},
    error = function(e) {
      message(sprintf("bank_code [%s] has no branch: %s", bank_code, url))
      out <<- NULL
    })
  out
}

get_all_branch <- function(bank_type = 1, ...) {
  message(">> get_bank_list() ...")
  bank_list <- get_bank_list(bank_type, ...)
  message(">> get_branch() ...")
  out_list <- bank_list[, bank_code] %>% lapply(get_branch, ...)
  out <- rbindlist(out_list)
  out
}


get_branch_info_ <- function(branch_code, ...) {
  # branch_code = "0040071"
  
  ua <- user_agent("Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")
  url <- sprintf("https://www.banking.gov.tw/ch/home.jsp?id=60&parentpath=0,4&mcustomize=FscSearch_BankSub2.jsp&bank_no=%s",
                 branch_code)
  doc <- GET(url, ..., ua) %>% content("text") %>% read_html
  # out <- doc %>% html_nodes("table .fsright") %>% html_text() %>% str_trim() %>% .[1:6] %>%
  #   as.list() %>%
  #   as.data.table %>%
  #   setnames(c("branch_code", "branch_name", "addr", "tel_no", "manager", "establish_date"))
  
  tryCatch({
    out <- data.table(
      bank_code = doc %>% html_node("th:contains('金融機構代碼')+td" %>% iconv(to = "UTF-8")) %>% html_text() %>% str_sub(1, 3),
      bank_name = doc %>% html_node("#fontcontent > div.page_content > h3") %>% html_text() %>% str_extract("^(.+?)(?=\u00bb)"),
      branch_code = doc %>% html_node("th:contains('金融機構代碼')+td" %>% iconv(to = "UTF-8")) %>% html_text(),
      branch_name = doc %>% html_node("th:contains('分支機構名稱')+td" %>% iconv(to = "UTF-8")) %>% html_text(),
      addr = doc %>% html_node("th:contains('地址')+td" %>% iconv(to = "UTF-8")) %>% html_text(),
      tel_no = doc %>% html_nodes("th:contains('電話')+td" %>% iconv(to = "UTF-8")) %>% html_text(),
      manager = doc %>% html_nodes("th:contains('負責人')+td" %>% iconv(to = "UTF-8")) %>% html_text(),
      establish_date = doc %>% html_nodes("th:contains('設立日期')+td" %>% iconv(to = "UTF-8")) %>% html_text() %>% str_trim(),
      extended_open_time = doc %>% 
        html_nodes("table .fscenter") %>% 
        html_text() %>% 
        .[1] %>% 
        str_replace_all("[//s\u00a0]+", " "),
      url = url
    )
    out[, addr := addr %>% str_extract("^[^號]+號?")]
  }, error = function(e) {
    message(e, "[url]: ", url)
    out <<- NULL
  })
  out
}

get_branch_info <- function(branch_code = "all", ...) {
  # branch_code = "0040071"
  testthat::expect_is(branch_code, "character")
  
  if (identical(branch_code, "all")) {
    message("Getting all branch codes...")
    branch_code <- get_all_branch(bank_type = 1, ...)[, branch_code]
  }
  message("Getting branch info...")
  
  cl <- parallel::makeCluster(detectCores()-1)
  on.exit(parallel::stopCluster(cl))
  clusterEvalQ(cl, {
    library(httr)
    library(rvest)
    library(data.table)
    library(magrittr)
    library(stringr)
  })
  
  out_list <- branch_code %>% parLapply(cl, ., get_branch_info_, ...)
  out <- rbindlist(out_list, fill = TRUE)
  out
}



# Test --------------------------------------------------------------------

branch_cd <- get_branch("812")[, branch_code]
branch_info <- get_branch_info(branch_cd)


system.time(
  out <- get_branch_info(branch_code = "all")
)
#    user   system  elapsed 
# 435.760    3.244 1498.964 
out %>% fwrite("data/bank_branch_info.csv")
message("Finished!")


# Get GPS -----------------------------------------------------------------

# Clean addr
bank <- fread("data/bank_branch_info.csv", colClasses = "character")
bank[addr %>% str_detect("^[^a-zA-Z0-9#]"),
     `:=`(addr = addr %>% 
            str_replace("(\\d+)(?:[-、.,之 ]+\\d+)*號", "\\1號"))]

# Get GPS coordinates
gps <- bank[addr %>% str_detect("^[^a-zA-Z0-9#]"), addr] %>% 
  addr2gps::get_gps(use_tor = F)
gps <- gps[!is.na(lat)]

# Merge
bank_gps <- gps[, .(addr, lat, lng, addr_norm)][bank, on = .(addr)]
bank_gps %>% fwrite("data/bank_branch_gps.csv")

# https://www.gps-coordinates.net/gps-coordinates-converter


