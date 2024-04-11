#' 투자 채권 조회
#'
#' @param accnt계정명
#' @param nth 파일선택
#'
#' @return 데이터프레임
#' @export
#' @examples
#' get_bond_balance()
#'
get_bond_balance <- function(nth = 1) {
  name = sort(list.files(dat_pth, pattern = 'balance', full.names = TRUE), decreasing = TRUE)[nth]
  mbnd = readRDS(name) %>% filter(str_detect(CODE, '^KR')) %>%
    transmute(ISU_CD = CODE,
              ISU_NM = 종목명,
              PRC_BY = 매입가,
              INV_VM = 보유수량/1000,
              INV_AT = 매입금액/1000,
              REG_DD = 등록일)
  abnd = get_dat_issue() %>% select(ISU_CD, RDM_DD)
  mbnd = mbnd %>% left_join(abnd, by = join_by(ISU_CD)) %>% relocate(RDM_DD, .before = REG_DD)
  return(arrange(mbnd, RDM_DD))
}




