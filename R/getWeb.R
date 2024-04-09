#' `웹페이지 호출`
#'
#' @param abb 'cnn': 공포지수,
#'            'fss': 금융감독원 신용평가,
#'            'krx': 한국거래소
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' callWebPage(abb = 'krx')
#' callWebPage(abb = 'fred', tprd = 'short')
#' callWebPage(abb = 'fss', name = '유니슨')
#'
callWebPage <- function(abb = c('cnn', 'fss', 'krx'), ...) {

    parms = list(...)

    if (abb == 'krx') {
        url = 'http://data.krx.co.kr'
    }
    if (abb == 'cnn') {
        url = 'https://money.cnn.com/data/fear-and-greed'
    }
    if (abb == 'fss') {
        url = 'https://www.fss.or.kr/fss/job/creditRatingReport/list.do?menuNo=200411'
        if (any(names(parms) == 'name')) {
            if (any(names(parms) != 'sdate')) sdate = today()-365*3
            if (any(names(parms) != 'edate')) edate = today()
            url =  glue(url, '&pageIndex=1&searchCompany=&searchAssGbn=&searchTargetCoNm={parms$name}
                &searchTargetTypeKor=&searchCnd=1&sdate={sdate}&edate={today()}')
        }
    }
    if (abb == 'fred') {
        url = 'https://fred.stlouisfed.org/series/T10Y2Y'
        if (any(names(parms) == 'tprd') & parms$tprd == 'short') {
            url =  'https://fred.stlouisfed.org/series/T10Y3M'
        }
    }
    utils::browseURL(url, browser = getOption("browser"))
}


#' `금융감독원 신용평가 조회`
#'
#' @param name 평가대상회사
#' @param sdt 공시시작일
#' @param edt 공시종료일
#'
#' @examples
#' webCreditReport()
#'
webCreditReport <- function(name, sdt = today()-365*3, edt = today()) {
    url = 'https://www.fss.or.kr/fss/job/creditRatingReport/list.do?menuNo=200411'
    if (!missing(name)) {
        url =  glue(url, '&pageIndex=1&searchCompany=&searchAssGbn=&searchTargetCoNm={name}
                &searchTargetTypeKor=&searchCnd=1&sdate={sdt}&edate={edt}')
    }
    utils::browseURL(url, browser = getOption("browser"))
}





