#' 개별 채권 매매가격 조회
#'
#' @param krcd 채권코드
#' @param nday 자료개수
#' @param ...
#'
#' @return 데이터프레임
#' @export
#' @examples
#' get_bond_price(krcd='KR6079161C75')
#'
get_bond_price <- function(krcd, nday=30, ...) {
    dat =
        scrKrxTab(isuCd = krcd,
                  strtDd = format(Sys.Date() - nday, "%Y%m%d"),
                  endDd = format(Sys.Date(), "%Y%m%d"),
                  dbnm = 'MDCSTAT09901') %>%
        transmute(Date = as.Date(일자),
                  Close = round(종가_가격),
                  평균가 = round(거래대금/거래량*10000),
                  수익률 = 종가_수익률,
                  거래량 = round(거래량/1000),
                  거래금액 = round(거래대금/1000)) %>%
        arrange(Date) %>%
        fill(Close, 평균가)
    if (nrow(dat) == 0) {
        dat =
            data.frame('Date' = Sys.Date()-365,
                       'Close' = as.numeric(0),
                       '평균가' = as.numeric(0),
                       '수익률' = as.numeric(0),
                       '거래량' = as.numeric(0),
                       '거래금액' = as.numeric(0))
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
#' indBondEvalPrice(krcd='KR6002881C50')
#'
indBondEvalPrice <- function(krcd, nday=30, ...) {
    dat =
        scrKrxTab(isuCd = krcd,
                  strtDd = format(Sys.Date() - nday, "%Y%m%d"),
                  endDd = format(Sys.Date(), "%Y%m%d"),
                  dbnm = 'MDCSTAT10601') %>%
        setNames(c('Date', '민평수익률', '민평수익률차이', '민평가', '민평가차이')) %>%
        transmute(Date = as.Date(Date),
                  민평가 = round(민평가),
                  민평수익률 = round(민평수익률/100)) %>%
        arrange(Date) %>%
        fill(민평가, 민평수익률)
    if (nrow(dat) == 0) {
        dat = data.frame('Date' = Sys.Date()-365,
                         '민평가' = as.numeric(0),
                         '민평수익률' = as.numeric(0))
    }
    return(dat)
}


#' 현재 거래중인 채권 가격 및 거래량 조회
#'
#' @param sect 채권유형
#' @param volumn 거래량
#'
#' @return
#' @export
#'
#' @examples
#' getTarget()
#'

# getTarget <- function(sect='회사채', volumn=100) {
#
#     bnd_cds = bondList(sect) %>% select(채권CD) %>% pull
#     bondPrice() %>%
#         filter(채권CD %in% bnd_cds) %>%
#         filter(거래량 >= volumn) %>%
#         select(채권CD, 채권명, 현재가, 평균가, 현수익률, 거래량, 거래금액) %>%
#         mutate(채권명 = str_sub(채권명, 1,10),
#                현수익률 = sprintf("%2.1f", 현수익률),
#                across(where(is.numeric), ~ formattable::comma(.x, digits=0)))
# }


#' 국채수익률PLOT
#'
#' @examples
#' plotGovern()
#'

# plotGovern = function() {
#     bas_yield = krxTresuryBondYield() %>% filter(str_detect(채권종류, '국채'))
#     ggbas = bas_yield %>% ggplot() +
#         geom_point(aes(x = 채권종류, y = 수익률)) +
#         ylim(floor(min(bas_yield[,2])), ceiling(max(bas_yield[,2])))
#     ggplotly(ggbas)
# }
