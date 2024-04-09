#' 투자 채권 조회
#'
#' @param accnt계정명
#' @param nth 파일선택
#'
#' @return 데이터프레임
#' @export
#' @examples
#' getBinvest()
#'
getBinvest <- function(accnt = 'M채권', nth = 1) {
    name =
        sort(list.files(work_path$invst, pattern = accnt, full.names = TRUE), decreasing = TRUE)[nth]
    mbnd =
        readRDS(name) %>% filter(str_detect(CODE, '^KR')) %>%
        mutate(보유수량 = 보유수량/1000,
               매입금액 = 매입금액/1000)
    for (i in seq_along(mbnd$CODE)) {
        rdmpt_dd = tryCatch(getIssue(mbnd[i, 'CODE'])$상환일, error=function(e) e)
        if (inherits(rdmpt_dd, 'error')) next
        mbnd[i, '상환일'] = rdmpt_dd
    }
    return(arrange(mbnd, 상환일))
}


#' 투자 주식 조회
#'
#' @param accnt 계정이름
#' @param nth 파일선택
#'
#' @return 데이터프레임
#' @export
#' @examples
#' myInvestStock()
#'
myInvestStock <- function(accnt = 'M채권', nth = 1) {
    name =
        sort(list.files(work_path$invst, pattern = accnt, full.names = TRUE), decreasing = TRUE)[nth]
    mstk  =
        readRDS(name) %>% filter(str_count(BDCD)==6)
    return(arrange(mstk, desc(매입금액)))
}



#' 계좌 정보 클립보드 저장
#'
#' @param accnt
#'
#' @return 데이터프레임
#' @export
#' @examples
#' myClpToRds()
#'
myClpToRds <- function(accnt='M채권') {
    clp =
        read.table(file = 'clipboard', header = FALSE, sep = '\t')

    clp1 =
        clp %>% filter(V1 %in% c('채권','주식')) %>%
        setNames(c('유가구분','종목번호','잔고번호','매입평균','현재가','손익금액','매도담보','선물대용')) %>%
        transmute(CODE  = 종목번호,
                  매입가 = formattable::comma(매입평균, 0),
                  민평가 = formattable::comma(현재가, 0))

    clp2 =
        clp %>% filter(V1 == '현금') %>%
        setNames(c('종목구분','종목명','보유수량','매입금액','세전평가금액','담보대출','대출잔액','대여')) %>%
        transmute(종목명,
                  보유수량 = formattable::comma(보유수량, 0),
                  매입금액 = formattable::comma(매입금액, 0))

    myasset =
        cbind(clp1, clp2) %>%
        mutate(기준일 = today()) %>%
        select(CODE, 종목명, 보유수량, 매입가, 민평가, 매입금액, 기준일)

    filename = paste0(accnt, '_', format(today(), '%Y%m%d'))
    saveRDS(myasset, file = file.path(work_path$invst, filename))
    return(myasset)
}
# myClpToRds()
