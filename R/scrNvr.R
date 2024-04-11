#' 기준영업일
#'
#' 네이버에서 영업일자를 추출하여 원하는 형식으로 출력
#'
#' @param biz_date
#' @param prev 기준일 보다 과거 일수의 날짜
#' @param dfm 파일형식 지정(text, date)
#'
#' @return Date 또는 텍스트
#' @export
#'
#' @examples
#' get_biz_date(prev = 1, dfm = 'date')
#'
get_biz_date <- function(biz_date, prev = 0, dfm = c("text", "date")) {
    if (missing(biz_date)) {
        oscrp = httr::GET("https://finance.naver.com/")
        ohtml = read_html(oscrp, encoding = "EUC-KR")
        otext = html_text(html_nodes(ohtml, xpath = "//*[@id=\"time\"]"))
        osubs = str_replace_all(str_match(otext, ("[0-9]+.[0-9]+.[0-9]+")), "\\.", "")
        biz_date = as.Date(osubs, "%Y%m%d") - prev
    }
    if (!is.Date(biz_date) & str_length(biz_date) == 8)
        biz_date = as.Date(biz_date, "%Y%m%d")
    if (!is.Date(biz_date) & str_length(biz_date) == 10)
        biz_date = as.Date(biz_date, "%Y-%m-%d")
    if (dfm[1] == "text")
        biz_date = format(biz_date, "%Y%m%d")
    if (dfm[1] == "date")
        biz_date = as.Date(biz_date, "%Y%m%d")
    return(biz_date)
}



#' 네이버모바일스크래핑
#'
#' @param cols 컬럼번호 또는 컬럼이름
#' @param category 수집대상 카테고리
#' @param reuters 로이터 코드
#' @param pages 페이지수
#' @param pagesize 페이지크기
#'
#' @return 데이터프레임
#' @export
#'
#' @examples
#' tail(get_nvr_market_index(cols=c('localTradedAt', 'closePrice')))
#'
get_nvr_market_index <- function(cols, category='exchange', reuters='FX_USDKRW', pages=10, pagesize=60) {
    for (i in 1:pages) {
        url = glue('https://m.stock.naver.com/front-api/v1/marketIndex/',
                   'prices?category={category}&reutersCode={reuters}&page={i}&pageSize={pagesize}')
        dat = jsonlite::read_json(url, simplifyVector = TRUE)
        dat = dat$result[,cols]
        # dat = dat[,cols]
        # rownames(dat) <- NULL
        if (i == 1) {out = dat} else {out = rbind(out, dat)}
    }
    for (k in 1:ncol(out)) {
        if (k==1) {
            out[,k] = as.Date(out[,k])
        } else {
            out[,k] = as.numeric(gsub(',', '', out[,k]))
        }
    }
    return(out)
}

