#' (한국투자)계좌별잔고조회-클립보드저장
#'
#' @param accnt Bond Account
#'
#' @return data.frame
#' @export
#' @examples
#' scr_clp2rds()
#'
scr_clp2rds <- function(accnt='M채권') {
  clp = read.table(file = 'clipboard', header = FALSE, sep = '\t')

  clp1 = clp %>% filter(V1 %in% c('채권','주식')) %>%
    setNames(c('유가구분','종목번호','잔고번호','매입평균','현재가','손익금액','매도담보','선물대용')) %>%
    transmute(CODE  = 종목번호,
              매입가 = formattable::comma(매입평균, 0),
              민평가 = formattable::comma(현재가, 0))

  clp2 = clp %>% filter(V1 == '현금') %>%
    setNames(c('종목구분','종목명','보유수량','매입금액','세전평가금액','담보대출','대출잔액','대여')) %>%
    transmute(종목명,
              보유수량 = formattable::comma(보유수량, 0),
              매입금액 = formattable::comma(매입금액, 0))

  myasset = cbind(clp1, clp2) %>%
    mutate(등록일 = today()) %>%
    select(CODE, 종목명, 보유수량, 매입가, 민평가, 매입금액, 등록일)

  filename = paste0('dat_balance_', format(today(), '%Y%m%d'), '.rda')
  saveRDS(myasset, file = file.path(dat_pth, filename))
  return(myasset)
}



#' (한국투자)거래내역조회-파일저장
#'
#' @param accnt Bond Account
#'
#' @return data.frame
#' @export
#' @examples
#' scr_xls2rds()
#'
scr_xls2rds <- function(accnt='M채권', nth=1) {
  name = sort(list.files(dat_pth, pattern = 'xlsx', full.names = TRUE), decreasing = TRUE)[nth]
  cnms = read_excel(name, sheet=1, skip=7, n_max=1)
  dats = read_excel(name, sheet=1, skip=9, col_names = colnames(cnms)) %>% as.data.frame()
  dat0 = dats[c(TRUE,FALSE),]
  dat1 = dats[c(FALSE,TRUE),] %>% setNames(as.character(cnms[1,]))

  outp = cbind(dat0, dat1) %>%
    filter(str_detect(거래종류, c('채권|사채'))) %>%
    transmute(Date = as.Date(거래일, '%Y.%m.%d'),
              ISU_NM = 종목명,
              TRD_TP = 거래종류,
              PRC_TD = 거래단가,
              INV_VM = 거래수량/1000,
              INV_AT = 거래금액/1000,
              FEE_AT = 수수료,
              TAX_AT = 거래세+세금,
              HOL_VM = 유가잔고)

  filename = paste0('dat_transaction_', parse_number(name), '.rda')
  saveRDS(outp, file = file.path(dat_pth, filename))
  return(outp)
}
