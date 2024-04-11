#' 신용등급 FACTOR 변환
#'
#' @param credit (text)신용등급
#' @return (factor)신용등급
#' @examples
#' cnvt_crdt2fac('AAA+/AAA-')
#'
cnvt_crdt2fac <- function(credit) {
  credit = str_split(trimws(credit), '/', simplify = T)
  grade = factor(credit, ordered=TRUE, levels = levels(crdt_gd))
  if (length(grade)>1) return(max(grade, na.rm = T))
  return(grade)
}


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

