#' KRX OTP 인수 생성
#'
#' @param ...
#'
#' @return
#' @examples
#' .getKrxOtp()
#'
.getKrxOtp <- function(...) {
    bas = list(mktId = 'BND',
               bndTpCd = 'TT',
               basddTpCd = '2',
               trdDd = getBizDate(),
               money = '1',
               csvxls_isNo = 'false',
               name = 'fileDown')
    gdata::update.list(bas, list(...))
}


#' KRX 텍스트 스크래핑
#'
#' 한국거래소(KRX) 텍스트 데이터 수집
#'
#' @param ... OTP 인수들
#'
#' @return 텍스트
#' @export
#' @examples
#' scrKrxTxt(dbnm = 'MDCSTAT10901')
#'
scrKrxTxt <- function(...) {
    otp = .getKrxOtp(...)
    txt = POST(query = otp, url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd')
    txt = html_text(read_html(txt))
    # class(gtxt) <- append(class(gtxt), 'scrKrxTxt')
    return(txt)
}


#' KRX 테이블 스크래핑
#'
#' 한국거래소(KRX) 테이블 데이터 수집
#'
#' @param ... OTP 인수들
#'
#' @return 데이터프레임
#' @export
#' @examples
#' scrKrxTab(dbnm = 'MDCSTAT09801') %>% head()
#'

# scrKrxTab <- function(dbnm, ...) {
#     otp = .getKrxOtp(url = glue('dbms/MDC/STAT/standard/{dbnm}'))
#     txt = POST(query = otp, url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd')
#     txt = html_text(read_html(txt))
#     tab = POST(query = list(code = txt),
#                 add_headers(referer = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'),
#                 url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd')
#     tab = html_text(read_html(tab, encoding = 'EUC-KR'))
#     tab = data.frame(read_csv(tab, show_col_types = FALSE))
#     return(tab)
# }


#' KRX 테이블 스크래핑
#'
#' 한국거래소(KRX) 테이블 데이터 수집
#'
#' @param ... OTP 인수들
#'
#' @return 데이터프레임
#' @export
#' @examples
#' scrKrxTab(dbnm = 'MDCSTAT09801') %>% head()
#'
scrKrxTab <- function(dbnm, ...) {
    otp = gdata::update.list(
        list(mktId = 'BND',
             bndTpCd = 'TT',
             basddTpCd = '2',
             trdDd = getBizDate(),
             money = '1',
             csvxls_isNo = 'false',
             name = 'fileDown',
             url = glue('dbms/MDC/STAT/standard/{dbnm}')),
        list(...))
    txt = POST(query = otp, url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd')
    txt = html_text(read_html(txt))
    tab = POST(query = list(code = txt),
               add_headers(referer = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'),
               url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd')
    tab = html_text(read_html(tab, encoding = 'EUC-KR'))
    tab = data.frame(read_csv(tab, show_col_types = FALSE))
    # class(tab) <- append(class(tab),"scrKrxTab")
    return(tab)
}


#' 장내채권 전종목 리스트
#'
#' @param obj 발행정보 오브젝트
#' @param ... 파라메터
#'
#' @examples
#' bond_list = bondList()
#'
bondList <- function(sect) {
    dat =
        scrKrxTab(dbnm = 'MDCSTAT10001') %>%
        select(표준코드, 한글종목명, 채권유형, 표면금리, 발행일, 상환일, 발행금액) %>%
        rename('채권CD' = 표준코드, '채권명' = 한글종목명) %>%
        mutate(발행금액 = 발행금액/100000000)
    if (missing(sect)) {
        return(dat)
    } else {
        return(dat %>% filter(채권유형 == sect))
    }
}


#' 채권종목조회
#'
#' @param x 채권코드 또는 채권이름름
#'
#' @return 데이터프레임
#' @export
#' @examples
#' seekBond('씨제이')
#'
seekBond <- function(x) {
    dat = bondList()
    rbind(dat %>% filter(str_detect(채권CD, x)), dat %>% filter(str_detect(채권명, x)))
}


#' 채권 전종목 매매가격 조회
#'
#' @return 데이터프레임
#' @export
#' @examples
#' bondPrice() %>% head()
#'
bondPrice <- function() {
    scrKrxTab(dbnm = 'MDCSTAT09801') %>%
        transmute(채권CD = 종목코드,
                  채권명 = 종목명,
                  현재가 = round(종가_가격),
                  평균가 = round(거래대금/거래량*10000),
                  현수익률 = 종가_수익률,
                  거래량 = round(거래량/1000),
                  거래금액 = round(거래대금/1000))
}


#' KRX-채권수익률(평균)
#'
#' @return 데이터프레임
#' @export
#' @examples
#' krxTresuryBondYield()
#'
krxTresuryBondYield <- function() {
    btp_ord = c('국채01년', '국채02년', '국채03년', '국채05년', '국채10년', '국채20년', '국채30년',
                'AA__03년', 'BBB_03년', 'CD_91일')
    for (i in 0:7) {
        dat = tryCatch(scrKrxTab(inqTpCd = 'T', trdDd = format(Sys.Date()-i, '%Y%m%d'), dbnm = 'MDCSTAT11401') %>%
                           filter(str_detect(채권종류, '국고채|회사채|CD')) %>%
                           mutate(채권종류 = factor(btp_ord, ordered = T, levels = btp_ord)) %>%
                           as.data.frame()
                       , error = function(e) e)
        if (inherits(dat, "error")) next
        if (!inherits(dat, "error")) break
    }
    message('기준일: ', format(Sys.Date()-i, '%Y-%m-%d'))
    return(dat)
}

