#' KRX 텍스트 스크래핑
#'
#' 한국거래소(KRX) 텍스트 데이터 수집
#'
#' @param ... OTP 인수들
#'
#' @return 텍스트
#' @export
#' @examples
#' scr_krx_txt(dbnm = 'MDCSTAT10901')
#'
scr_krx_txt <- function(...) {
    otp = gdata::update.list(krx_otp_base, list(...))
    txt = POST(query = otp, url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd')
    txt = html_text(read_html(txt))
    return(txt)
}


#' KRX 테이블 스크래핑
#'
#' 한국거래소(KRX) 테이블 데이터 수집
#'
#' @param ... OTP 인수들
#' @param dbnm KRX DB NAME
#'
#' @return data.frame
#' @export
#' @examples
#' scr_krx_tab(dbnm = 'MDCSTAT09801') %>% head()
#'
scr_krx_tab <- function(dbnm, ...) {
  new = list(url = glue('dbms/MDC/STAT/standard/{dbnm}'), ...)
  if (!any(names(new) == 'trdDd')) new$trdDd = get_biz_date()
  otp = gdata::update.list(krx_otp_base, new)
  txt = POST(query = otp, url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd')
  txt = html_text(read_html(txt))
  tab = POST(query = list(code = txt),
             add_headers(referer = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'),
             url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd')
  tab = html_text(read_html(tab, encoding = 'EUC-KR'))
  tab = data.frame(read_csv(tab, show_col_types = FALSE))
  return(tab)
}



#' 장내채권 전종목 리스트
#'
#' @param obj 발행정보 오브젝트
#' @param ... 파라메터
#'
#' @examples
#' scr_bond_list() %>% tail
#'
scr_bond_list <- function(sect) {
    dat =
        scr_krx_tab(dbnm = 'MDCSTAT10001') %>%
        select(표준코드, 한글종목명, 채권유형, 표면금리, 발행일, 상환일, 발행금액) %>%
        rename('ISU_CD'=표준코드, 'ISU_NM'=한글종목명, 'BTP_NM'=채권유형,
               'COUPON'=표면금리, 'ISU_DD'=발행일, 'RDM_DD'=상환일, 'ISU_AMT'=발행금액) %>%
        mutate(COUPON = COUPON/100,
               ISU_AMT = ISU_AMT/100000000)
    if (missing(sect)) {
        return(dat)
    } else {
        return(dat %>% filter(BTP_NM == sect))
    }
}


#' KRX 채권 발행정보 수집
#'
#' @param krcd 채권코드
#'
#' @return 발행정보
#' @export
#' @examples
#' scr_issue(krcd = 'KR6079161C75')
#'
scr_issue <- function(krcd) {
  otp = list(isuCd = krcd, bld = 'dbms/MDC/STAT/standard/MDCSTAT10901')
  htm = httr::POST(query = otp, url = 'http://data.krx.co.kr/comm/bldAttendant/getJsonData.cmd')
  htm = html_text(read_html(htm))
  txt = gsub("\"|output\":|\\{|\\}|\\[|\\]", "", htm)
  txt = gsub(",([[:digit:]])", "\\1", txt)

  isu = data.frame(str_split(txt, ",")) %>%
    setNames("keyvalue") %>%
    filter(str_count(keyvalue, ':') == 1) %>%
    separate("keyvalue", c("key", "value"), sep = ":") %>%
    filter(value != "") %>%
    pivot_wider(names_from = key, values_from = value) %>%
    as.data.frame()

  if (any(str_detect(names(isu), 'CREDIT_VALU'))) {
    isu = isu %>% unite(CREDIT, contains('CREDIT_VALU'), remove=T, sep='/')
  } else {
    isu = isu %>% mutate(CREDIT = NA_character_)
  }
  return(isu)
}



#' 채권 전종목 매매가격 조회
#'
#' @return 데이터프레임
#' @export
#' @examples
#' scr_bond_prices() %>% head()
#'
scr_bond_prices <- function() {
    scr_krx_tab(dbnm = 'MDCSTAT09801') %>%
        transmute(ISU_CD = 종목코드,
                  ISU_NM = 종목명,
                  PRC_PR = round(종가_가격),
                  PRC_AV = round(거래대금/거래량*10000),
                  BTX_RR = round(종가_수익률/100, 4),
                  TRD_VM = round(거래량/1000),
                  TRD_AT = round(거래대금/1000))
}


#' KRX-채권수익률(평균)
#'
#' @return 데이터프레임
#' @export
#' @examples
#' krx_tresury_bond_yield()
#'
krx_tresury_bond_yield <- function() {
    btp_ord = c('국채01년', '국채02년', '국채03년', '국채05년', '국채10년', '국채20년', '국채30년',
                'AA__03년', 'BBB_03년', 'CD_91일')
    for (i in 0:7) {
        dat = tryCatch(scr_krx_tab(inqTpCd = 'T', trdDd = format(Sys.Date()-i, '%Y%m%d'), dbnm = 'MDCSTAT11401') %>%
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

