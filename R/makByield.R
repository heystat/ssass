#' 신용등급 FACTOR 변환
#'
#' @param credit (text)신용등급
#' @return (factor)신용등급
#' @examples
#' cnvt_crdt2fac('AAA+/AAA-')
#'
mak_yield_fixed <- function(bas_dd, # 기준일
                            bas_dd, # 기준일
                            ) {




  # 채권정보 ____________________________________________________________
  if (istd == 'ind') bnd_tabs = self$ind_istd(bdcd, ...)
  if (istd == 'mul') bnd_tabs = self$mul_istd(bdcd)

  dict = cbind(bnd_tabs[['name']], bnd_tabs[['cons']], bnd_tabs[['trade']])
  for (t in 1:length(dict)) {assign(names(dict[t]), dict[[t]])}

  # 경과이자 ____________________________________________________________
  prevC = coupons.prev(bas_dd, 상환일, 연지급수)
  nextC = coupons.next(매수일, 상환일, 연지급수)
  accrued = 10000 * 표면금리 * jrvFinance::yearFraction(prevC, 매수일, prevC, nextC, 연지급수, self$cnv)

  # 날짜정보 ____________________________________________________________
  start = jrvFinance::yearFraction(매수일, nextC, prevC, nextC, 연지급수, self$cnv)
  Year = c(0, seq(from = start, by = 1/연지급수, length.out = coupons.n(매수일, 상환일, 연지급수)))
  Date = c(매수일, coupons.dates(prevC, 상환일, 연지급수))

  # 세전현금 ____________________________________________________________
  세전CF = c(-매수가, rep(표면금리*10000/연지급수, length(Date)-1))
  세전CF[length(세전CF)] = (상환율 + 표면금리/연지급수) * 10000

  # 과세대상 ____________________________________________________________
  if (length(세전CF) >= 3) {과세대상 = c(0, 세전CF[2]-accrued, 세전CF[3:length(세전CF)])}
  if (length(세전CF) <  3) {과세대상 = c(0, 세전CF[2]-accrued)}
  redempt = 10000 * (상환율-1) * dayCount(매수일, 상환일, 2) / dayCount(발행일, 상환일, 2)
  과세대상[length(과세대상)] = redempt + 표면금리/연지급수 * 10000
  거래비용 = c(self$fee, rep(0, length(Date)-2), self$fee)
  원천세 = 과세대상 * tax_rate

  # 세후현금 ____________________________________________________________
  세후CF = 세전CF - 원천세 - 거래비용
  TCF = data.frame(Date=Date, Year=round(Year,1), 세전CF=round(세전CF,1), 과세대상=round(과세대상,1), 원천세=round(원천세,1), 거래비용=거래비용, 세후CF=round(세후CF,1))

  # 매도반영 ____________________________________________________________
  if (매도일 < 상환일) {
    prevT = coupons.prev(매도일, 상환일, 연지급수)
    nextT = coupons.next(매도일, 상환일, 연지급수)
    accrued = 10000 * 표면금리 * jrvFinance::yearFraction(prevT, 매도일, prevT, nextT, 연지급수, self$cnv)

    TCF = TCF[TCF$Date <= nextT,]
    TCF[nrow(TCF), 'Date'] = 매도일
    TCF[nrow(TCF), '세전CF'] = 매도가
    TCF[nrow(TCF), '과세대상'] = 10000 * 표면금리 * jrvFinance::yearFraction(prevT, 매도일, prevT, nextT, 연지급수, self$cnv)
    TCF[nrow(TCF), '거래비용'] = self$fee
    TCF[nrow(TCF), '원천세'] = TCF[nrow(TCF), '과세대상'] * tax_rate
    TCF[nrow(TCF), '세후CF'] = TCF[nrow(TCF), '세전CF'] - TCF[nrow(TCF), '원천세'] - TCF[nrow(TCF), '거래비용']
  }

  # 수익계산 ____________________________________________________________
  # issue =
  #   data.frame('BD_CD'=BD_CD,'종목명'=종목명,'신용등급'=신용등급,'신용FNG'=신용FNG,'발행형태'=발행형태,'옵션CD'=옵션CD,
  #              '만기일'=만기일,'상환일'=상환일,'표면금리'=표면금리,'상환율'=상환율, '확인'= ifelse(exists('수정일'), 수정일, NA_Date_)) %>%
  #   mutate(across(c(표면금리, 상환율), ~ formattable::percent(.x, 1)))

  # trade =
  #   data.frame('기준일'=기준일, '매수일'=매수일, '매도일'=매도일, '잔존일수'=dayCount(매수일,매도일,2),
  #              '민평가'=민평가, '매수가'=매수가, '매도가'=매도가, '거래량'=거래량, '거래금액'=거래금액)

  bnd_tabs[['yield']] =
    data.frame(IRR = suppressWarnings(irr(cf = TCF$세후CF, cf.t = TCF$Year)), ##내부수익률
               TRR = sum(TCF$세후CF)/(매수가+self$fee)) %>%                   ##총기간수익률
    mutate(ARR = TRR/max(TCF$Year),                                           ##연평균수익률
           MRR = (TRR/dayCount(매수일, 상환일, 2))*30) %>%
    relocate(IRR, ARR, MRR, TRR) %>%
    mutate(across(c(IRR, ARR, MRR, TRR), ~ formattable::percent(.x, 1)))

  bnd_tabs[['TCF']] =
    TCF %>%
    mutate(across(c(세전CF, 과세대상, 원천세, 거래비용, 세후CF), ~ formattable::comma(.x, digits=0)))

  return(bnd_tabs)
})

#' 채권종목조회
#'
#' @param x 채권코드 또는 채권이름름
#'
#' @return 데이터프레임
#' @export
#' @examples
#' seek_bond('씨제이')
#'
seek_bond <- function(x) {
  dat = get_bond_list()
  rbind(dat %>% filter(str_detect(ISU_CD, x)), dat %>% filter(str_detect(ISU_NM, x)))
}



#' KRX 채권 발행정보 수집
#'
#' @param krcd 채권코드
#' @param tax_md dvd(보유기간경과과세: 각자가 보유기간에 해당하는 세금을 부담),
#'               tot(발생분과세: 이자를 지급받는 자가 세금을 부담)
#'
#' @return 발행정보
#' @export
#' @examples
#' get_issue(krcd = 'KR6079161C75')
#'
get_issue <- function(krcd, tax_md = c('dvd', 'tot')) {
  isu = scr_issue(krcd) %>%
    mutate(MATURITY_DD = as.Date(REDMPT_DD),
           MATURITY_RTO = as.numeric(EXP_REDMPT_RTO)/100,
           INT_PAY_CALC_MMS = as.integer(INT_PAY_CALC_MMS),
           INT_PAY_CALC_CNT = as.integer(12/as.integer(INT_PAY_CALC_MMS)),
           GRT_YD = tryCatch(ifelse(!is.null(GRT_YD), as.numeric(GRT_YD)/100, NA_real_), error = function(e) NA_real_),
           OPTION_YN = tryCatch(ifelse(!is.null(IMBDOPT_CORPBND_NM), IMBDOPT_CORPBND_NM, 'RED'), error = function(e) 'RED'),
           OPTION = case_when(str_detect(OPTION_YN, 'Put|put|풋') ~ 'PUT',
                              str_detect(OPTION_YN, 'Call|call|콜') ~ 'CAL', TRUE ~ 'RED'),
           PUT_DD = tryCatch(case_when(!is.null(PUT_EXER_STRT_DD1) & (as.Date(PUT_EXER_STRT_DD1) > today()) ~ as.Date(PUT_EXER_STRT_DD1),
                                       !is.null(PUT_EXER_STRT_DD2) & (as.Date(PUT_EXER_STRT_DD2) > today()) ~ as.Date(PUT_EXER_STRT_DD2),
                                       TRUE ~ as.Date(REDMPT_DD)), error = function(e) NA_Date_),
           CALL_DD = tryCatch(case_when(!is.null(CALL_EXER_STRT_DD1) & (as.Date(CALL_EXER_STRT_DD1) > today()) ~ as.Date(CALL_EXER_STRT_DD1),
                                        !is.null(CALL_EXER_STRT_DD2) & (as.Date(CALL_EXER_STRT_DD2) > today()) ~ as.Date(CALL_EXER_STRT_DD2),
                                        TRUE ~ as.Date(REDMPT_DD)), error = function(e) NA_Date_),

           COUPON = as.numeric(COUPN_RT)/100,
           INT_NUMS = coupons.n(ISU_DD, MATURITY_DD, freq = INT_PAY_CALC_CNT),
           INT_PV = npv(cf = rep(10000 * COUPON/INT_PAY_CALC_CNT, INT_NUMS), rate = GRT_YD / INT_PAY_CALC_CNT)) %>%
    transmute(ISU_CD,
              ISU_NM,
              CREDIT,
              BTP_NM = BND_TP_NM,
              INT_MD = case_when(str_detect(INT_PAY_METHD_NM, '이표채') ~ '이표채',
                                 str_detect(INT_PAY_METHD_NM, '복리채') ~ '복리채', TRUE ~ NA),
              OPTION,
              INT_FQ = as.integer(12/as.integer(INT_PAY_CALC_MMS)),
              ISU_DD = as.Date(ISU_DD),
              RDM_DD =  case_when(OPTION == 'RED' ~ MATURITY_DD,
                                  OPTION == 'PUT' & !is.na(PUT_DD) & (PUT_DD < MATURITY_DD) ~ PUT_DD %m+% months(3),
                                  OPTION == 'CAL' & !is.na(CALL_DD) & (CALL_DD < MATURITY_DD) ~ CALL_DD %m+% months(3),
                                  TRUE ~ MATURITY_DD),
              COUPON = as.numeric(COUPN_RT)/100,
              RDM_RT = case_when(OPTION == 'RED' ~ MATURITY_RTO,
                                 OPTION == 'PUT' ~ tryCatch((10000 - INT_PV) * ((1 + GRT_YD / INT_PAY_CALC_CNT) ^ INT_NUMS) / 10000, error = function(e) 1),
                                 OPTION == 'CAL' ~ tryCatch((10000 - INT_PV) * ((1 + GRT_YD / INT_PAY_CALC_CNT) ^ INT_NUMS) / 10000, error = function(e) 1),
                                 TRUE ~ 1),
              RDM_RT = replace_na(RDM_RT, 1),
              TAX_MD = tax_md[1],
              INV_GD = 'ddd',
              REG_DD = Sys.Date(),
              DSCRPT = '') %>%
    as.data.frame()
  return(isu)
}


#' KRX 채권 발행정보 출력
#'
#' @param 채권코드
#' @examples
#' print.get_issue(krcd = 'KR6079161C75')
#'
print.get_issue <- function(krcd) {
  get_issue(krcd) %>%
    rename('채권CD'=ISU_CD,
           '채권명'=ISU_NM,
           '신용등급'=CREDIT,
           '채권유형'=BTP_NM,
           '발행형태'=INT_MD,
           '옵션CD'=OPTION,
           '연지급수'=INT_FQ,
           '발행일'=ISU_DD,
           '상환일'=RDM_DD,
           '표면금리'=COUPON,
           '상환율'=RDM_RT,
           '과세방법'=TAX_MD,
           '투자등급'=INV_GD,
           '등록일'=REG_DD,
           '고려사항'=DSCRPT)
}


#' 채권 발행정보 조회
#'
#' @param krcd
#' @param mthd
#'
#' @examples
#' set_issue(krcd = 'KR6079161C75')
#'
set_issue <- function(krcd) {
  isu_pth = file.path(dat_pth, 'dat_issue.rda')

  if (file.exists(isu_pth)) {
    dat_isu = read_rds(isu_pth)
  } else {
    ifelse(!dir.exists(dat_pth), dir.create(dat_pth), FALSE)
    write_rds(data.frame(), file = isu_pth)
  }

  if (any(dat_isu[['ISU_CD']] == krcd)) {
    ind_isu = dat_isu[dat_isu[['ISU_CD']] == krcd,]
  } else {
    ind_isu = get_issue(krcd)
    dat_isu = bind_rows(dat_isu, ind_isu)
    write_rds(dat_isu, file = isu_pth)
  }
  return(ind_isu)
}


#' 채권 발행정보 출력
#'
#' @param krcd
#' @param mthd
#'
#' @examples
#' print.set_issue(krcd = 'KR6079161C75')
#'
print.set_issue <- function(krcd, digits=5, ...) {
  ind_isu = set_issue(krcd)
  cat(blue("       채권CD : "), ind_isu$ISU_CD, "\n")
  cat(blue("       채권명 : "), ind_isu$ISU_NM, "\n")
  cat(blue("     신용등급 : "), ind_isu$CREDIT, "\n")
  cat(blue("     채권유형 : "), ind_isu$BTP_NM, "\n")
  cat(blue("     발행형태 : "), ind_isu$INT_MD, "\n")
  cat(blue("       옵션CD : "), ind_isu$OPTION, "\n")
  cat(blue("     연지급수 : "), ind_isu$INT_FQ, "\n")
  cat(blue("       발행일 : "), format(ind_isu$ISU_DD, format="%Y-%m-%d"), "\n")
  cat(blue("       상환일 : "), format(ind_isu$RDM_DD, format="%Y-%m-%d"), "\n")
  cat(blue("     표면금리 : "), ind_isu$COUPON, "\n")
  cat(blue("       상환율 : "), format(ind_isu$RDM_RT, digits=digits), "\n")
  cat(blue("     과세방법 : "), ind_isu$TAX_MD, "\n")
  cat(blue("     투자등급 : "), ind_isu$INV_GD, "\n")
  cat(blue("       등록일 : "), format(ind_isu$REG_DD, format="%Y-%m-%d"), "\n")
  cat(blue("     고려사항 : "), format(ind_isu$DSCRPT, format="%Y-%m-%d"), "\n")
  # print(ind_isu, row.names=FALSE, digits=digits)
  # invisible(ind_isu)
}


#' 채권 발행정보 파일 조회(dat_issue.rda)
#'
#' @param ...
#' @examples
#' get_dat_issue()
#'
get_dat_issue <- function(...) {
  isu_pth = file.path(dat_pth, 'dat_issue.rda')
  read_rds(isu_pth)
}


#' 채권 발행정보 업데이트
#'
#' @param krcd 채권코드
#' @param ... 업데이트목록
#' @examples
#' update_issue(krcd = 'KR6079161C75', INV_GD = 'ccc')
#' update_issue(krcd = 'KR6034832E28', INV_GD = 'ddd')
#'
update_issue <- function(krcd, ...) {
  new_val = list(...)
  ind_isu = set_issue(krcd) %>% mutate(REG_DD = Sys.Date())
  isu_pth = file.path(dat_pth, 'dat_issue.rda')

  for (i in seq_along(new_val)) {
    ind_isu[[names(new_val)[i]]] <- new_val[[i]]
  }

  dat_old = read_rds(isu_pth) %>% filter(ISU_CD != krcd)
  dat_new = bind_rows(dat_old, ind_isu) %>%
    select(colnames(dat_old)) %>%
    group_by(ISU_CD) %>%
    arrange(desc(REG_DD)) %>%
    filter(row_number()==1) %>%
    as.data.frame()

  write_rds(dat_new, file = isu_pth)
  print.get_issue(krcd)
}



#' KRX 신용등급 업데이트
#' @examples
#' update_credit()
#'
update_credit <- function() {
    isu_pth = file.path(dat_pth, 'dat_issue.rda')
    dat_isu = read_rds(isu_pth)
    for (i in 1:nrow(dat_isu)) {cat(paste0(round(i/nrow(dat_isu)*100), '% completed'))
      if (dat_isu[i, 'BTP_NM'] != '회사채') next
      new_crd = get_issue(dat_isu[i, 'ISU_CD'])$CREDIT

      if (!is.na(new_crd)) {
        if (dat_isu[i, 'CREDIT'] != new_crd) {
          print(paste(dat_isu[i, 'ISU_CD'], ' | ', dat_isu[i, 'ISU_NM'], ' | ', dat_isu[i, 'CREDIT'], '-->', new_crd))
          dat_isu[i, 'CREDIT'] <- new_crd
          dat_isu[i, 'REG_DD'] <- Sys.Date()
          saveRDS(isu, file.path(work_path$issue, krcd))
        }
      }
      if (i == nrow(dat_isu)) {cat(': Done\n')} else {cat('\014')}
      Sys.sleep(0.1)
    }
    write_rds(dat_isu, file = isu_pth)
}




#' 데이터셋 HELP 템플릿
#'
#' A subset of data from the World Health Organization Global Tuberculosis
#' Report ...
#'
#' @format ## `who`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"who"

