############################'
## 공통함수 정의
## - DESCRIPTION 작성: roxygen2::roxygenize()
############################.


#' `함수호출`
#'
#' 패키지에서 사용하는 함수 호출하기
#'
#' @return None
#' @examples
#' suppressWarnings(suppressMessages(call_library()))
#' rm(list=ls())
call_library <- function() {
    use.pkgs = c("crayon",
                 "data.table", "dplyr",
                 "forecast", "formattable",
                 "ggplot2", "glue",
                 "here", "highcharter",
                 "hash", "Hmisc",
                 "httr",
                 "janitor", "jrvFinance", "jsonlite",
                 "kableExtra",
                 "lubridate",
                 "openxlsx",
                 "PerformanceAnalytics", "plotly", "purrr",
                 "quantmod", "quantreg",
                 "R6", "readr", "readxl", "roptions", "RQuantLib", "rvest",
                 "stringi", "stringr",
                 "tibble", "tidyr",
                 "xml2", "xts",
                 "zoo")
    new.pkg = use.pkgs[!(use.pkgs %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {install.packages(new.pkg, dependencies = TRUE)}
    out = sapply(use.pkgs, require, character.only = TRUE)
}
suppressWarnings(suppressMessages(call_library()))


#' `파일위치`
#'
#' 작업공간의 폴더 및 파일 위치 지정
#'
#' @param home 작업공간 최상위 위치
#' @return home Path List
#' @examples
#' workPathBond(home)
#'
workPathBond <- function(home) {
    path = list(home = home,
                # func = file.path(home, 'func'),
                data = file.path(home, 'data'),
                issue= file.path(home, 'data', 'issue'),
                invst= file.path(home, 'data', 'invst'),
                trade= file.path(home, 'data', 'trade'))
    for (name in names(path)) ifelse(!dir.exists(path[[name]]), dir.create(path[[name]]), FALSE)
    return(path)
}



############################'
## `채권 세금 및 거래비용`
############################'
# constant_bond <-
#     list(fee_rate = 0.002,
#          tax_rate = 0.154)


############################'
## `KRX OTP 파라메타 및 URL`
############################'
# krx_otp_base <-
#     list(mktId = 'BND',
#          bndTpCd = 'TT',
#          basddTpCd = '2',
#          money = '1',
#          csvxls_isNo = 'false',
#          name = 'fileDown')

# krx_otp_urls <-
#     list(file = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd',
#          json = 'http://data.krx.co.kr/comm/bldAttendant/getJsonData.cmd',
#          down = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd')




#' #' Take an object to bizarro world
#' #'
#' #' @param x A vector.
#' #' @export
#' bizarro <- function(x, ...) {
#'     UseMethod("bizarro")
#' }
#' #' @export
#' bizarro.character <- function(x, ...) {
#'     letters <- strsplit(x, "")
#'     letters_rev <- lapply(letters, rev)
#'     vapply(letters_rev, paste, collapse = "", FUN.VALUE = character(1))
#' }

