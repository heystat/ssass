#' 개별 채권 매매가격 조회
#'
#' @param krcd 채권코드
#' @param nday 자료개수
#' @param ...
#'
#' @return time-series
#' @export
#' @examples
#' get_bond_price_ts(krcd='KR6079161C75')
#'
get_bond_price_ts <- function(krcd, nday=30, ...) {
  dat = scr_krx_tab(isuCd = krcd,
                    strtDd = format(Sys.Date() - nday, "%Y%m%d"),
                    endDd = format(Sys.Date(), "%Y%m%d"),
                    dbnm = 'MDCSTAT09901') %>%
    transmute(Date = as.Date(일자),
              PRC_PR = round(종가_가격),
              PRC_AV = round(거래대금/거래량*10000),
              ROR_PR = 종가_수익률,
              TRD_VM = round(거래량/1000),
              TRD_AT = round(거래대금/1000)) %>%
    arrange(Date) %>%
    fill(PRC_PR, PRC_AV)
  if (nrow(dat) == 0) {
    dat = data.frame('Date'   = Sys.Date()-365,
                     'PRC_PR'  = as.numeric(0),
                     'PRC_AV' = as.numeric(0),
                     'ROR_PR' = as.numeric(0),
                     'TRD_VM' = as.numeric(0),
                     'TRD_AT' = as.numeric(0))
  }
  return(dat)
}



#' 개별 채권 민평수익률 조회
#'
#' @param krcd 채권코드
#' @param nday 자료개수
#' @param ...
#'
#' @return 데이터프레임
#' @export
#' @examples
#' get_bond_price_eval(krcd='KR6002881C50')
#'
get_bond_price_eval <- function(krcd, nday=30, ...) {
  dat = scr_krx_tab(isuCd = krcd,
                    strtDd = format(Sys.Date() - nday, "%Y%m%d"),
                    endDd = format(Sys.Date(), "%Y%m%d"),
                    dbnm = 'MDCSTAT10601') %>%
    setNames(c('Date', 'ROR_EV', 'ROR_DF', 'PRC_EV', 'PRC_DF')) %>%
    # setNames(c('Date', '민평수익률', '민평수익률차이', '민평가', '민평가차이')) %>%
    transmute(Date = as.Date(Date),
              PRC_EV = round(PRC_EV),
              ROR_EV = round(ROR_EV/100)) %>%
    arrange(Date) %>%
    fill(PRC_EV, ROR_EV)
  if (nrow(dat) == 0) {
    dat = data.frame('Date' = Sys.Date()-365,
                     'PRC_EV' = as.numeric(0),
                     'ROR_EV' = as.numeric(0))
  }
  return(dat)
}


#' 현재 거래중인 채권 가격 및 거래량 조회
#'
#' @param sect 채권유형
#'
#' @export
#' @examples
#' get_isutrd()
#'
get_isutrd <- function(sect='회사채') {
  bnd_trd = scr_bond_prices() %>% select(-ISU_NM)
  new_isu = setdiff(bnd_trd$ISU_CD, get_dat_issue()$ISU_CD)
  for (i in seq_along(new_isu)) {cat(paste0(round(i / length(new_isu) * 100), '% completed'))
    set_issue(krcd = new_isu[i], out = FALSE)
    cat('\014')
  }
  dat_isu = get_dat_issue()
  isu_trd = merge(dat_isu, bnd_trd, by = 'ISU_CD')
  if (!missing(sect)) isu_trd = isu_trd %>% filter(BTP_NM == sect)
  return(isu_trd)
}



#' 국채수익률PLOT
#'
#' @examples
#' plotGovern()
#'
plot_govern = function() {
    bas_yield = krx_tresury_bond_yield() %>% filter(str_detect(채권종류, '국채'))
    ggbas = bas_yield %>% ggplot() +
        geom_point(aes(x = 채권종류, y = 수익률)) +
        ylim(floor(min(bas_yield[,2])), ceiling(max(bas_yield[,2])))
    ggplotly(ggbas)
}
