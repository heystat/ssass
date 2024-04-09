#' 신용등급 FACTOR 변환
#'
#' @param credit 신용등급 텍스트
#'
#' @return factor
#' @examples
#' cnvt_crdt2fac('AAA+/AAA-')
#'
cnvt_crdt2fac <- function(credit) {
        cat_a = c('AAA+', 'AAA', 'AAA-', 'AA+', 'AA', 'AA-', 'A+', 'A', 'A-')
        cat_b = c('BBB+', 'BBB', 'BBB-', 'BB+', 'BB', 'BB-', 'B+', 'B', 'B-')
        cat_c = c('CCC+', 'CCC', 'CCC-', 'CC+', 'CC', 'CC-', 'C+', 'C', 'C-')
        crdt_order = c(cat_a, cat_b, cat_c)
        credit = str_split(trimws(credit), '/', simplify = T)
        grade = factor(credit, ordered=TRUE, levels=crdt_order)
        if (length(grade)>1) return(max(grade, na.rm = T))
        return(grade)
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
#' scrape_issue(krcd = 'KR6079161C75')
#'
scrape_issue <- function(krcd, tax_md = c('dvd', 'tot')) {

  otp = list(isuCd = krcd, bld = 'dbms/MDC/STAT/standard/MDCSTAT10901')
  htm = POST(query = otp, url = 'http://data.krx.co.kr/comm/bldAttendant/getJsonData.cmd')
  htm = html_text(read_html(htm))
  txt = gsub("\"|output\":|\\{|\\}|\\[|\\]", "", htm)
  txt = gsub(",([[:digit:]])", "\\1", txt)

  isu = data.frame(str_split(txt, ",")) %>%
    setNames("keyvalue") %>%
    filter(str_count(keyvalue, ':') == 1) %>%
    separate("keyvalue", c("key", "value"), sep = ":") %>%
    filter(value != "") %>%
    pivot_wider(names_from = key, values_from = value)

  if (any(str_detect(names(isu), 'CREDIT_VALU'))) {
    isu = isu %>% unite(CREDIT, contains('CREDIT_VALU'), remove=T, sep='/')
  } else {
    isu = isu %>% mutate(CREDIT = NA_character_)
  }

  isu = isu %>%
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
              REG_DD = Sys.Date()) %>%
    as.data.frame()
  return(isu)
}


#' KRX 채권 발행정보 출력
#'
#' @param 채권코드
#' @examples
#' scrape_issue.print(krcd = 'KR6079161C75')
#'
scrape_issue.print <- function(krcd) {
  scrape_issue(krcd) %>%
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
           '등록일'=REG_DD)
}


#' 채권 발행정보 조회
#'
#' @param krcd
#' @param mthd
#'
#' @examples
#' get_issue(krcd = 'KR6079161C75')
#'
get_issue <- function(krcd) {
  # system.file("data", "dat_issue", package = "ssass")
  data('dat_issue')

  if (any(dat_issue$ISU_CD==krcd)) {
    ind_issue = dat_issue[dat_issue$ISU_CD==krcd,]
  } else {
    ind_issue = scrape_issue(krcd)
    dat_issue = bind_rows(dat_issue, ind_issue)
    # dat_issue = ind_issue
    usethis::use_data(dat_issue, overwrite = TRUE)
  }
  return(ind_issue)
}
# get_issue(krcd = 'KR6080721D34')
# get_issue(krcd = 'KR6080721D34')
# get_issue(krcd = 'KR6034832E28')




#' 채권 발행정보 업데이트
#'
#' @param krcd 채권코드
#' @param ... 업데이트목록
#' @examples
#' updateIssue(krcd = 'KR6079161C75', CLASS = 'ccc')
#'
updateIssue <- function(krcd, ...) {

  data('dat_issue')
  new = list(...)
  isu = get_issue(krcd)

  # new = list(REG_DD = '2022-12-11', CLASS = 'ccc')
  # i=1

  # if (!any(dat_issue$ISU_CD==krcd)) {
  #   ind_issue = scrape_issue(krcd)
  #   dat_issue = merge(dat_issue, ind_issue)
  #   usethis::use_data(dat_issue, overwrite = TRUE)
  # }

  for (i in seq_along(new)) {
    isu[isu$ISU_CD==krcd, names(new)[i]] <- new[[i]]
  }

  dat_issue = bind_rows(dat_issue[isu$ISU_CD!=krcd,], isu)

  usethis::use_data(dat_issue, overwrite = TRUE)
  return(out)
}

updateIssue(krcd = 'KR6080721D34', CLASS = 'ccc')
updateIssue(krcd = 'KR6080721D34', CLASS = 'ccc')
updateIssue(krcd = 'KR6034832E28', CLASS = 'ccc')
updateIssue(krcd = 'KR6109961D71', CLASS = 'ccc')
dat_issue
krcd = 'KR6080721D34'

data(dat_issue)



#' World Health Organization TB data
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









#' KRX 신용등급 업데이트
#' @examples
#' updateCredit()
#'
updateCredit <- function() {

    krcd_list = list.files(file.path(work_path$issue), full.names=F)

    for (i in 1:length(krcd_list)) {cat(paste0(round(i/length(krcd_list)*100), '% completed'))
        isu = getIssue(krcd_list[i])

        if (isu$채권유형 == '회사채') {
            new = scrIssue(krcd_list[i])$CREDIT
            if (!is.na(new)) {
                if (isu$CREDIT != new) {
                    print(paste(isu$채권CD, ' | ', isu$채권명, ' | ', isu$CREDIT, '-->', new))
                    isu$CREDIT <- new
                    isu$등록일 <- Sys.Date()
                    saveRDS(isu, file.path(work_path$issue, krcd))
                }
            }
        }
        if (i == length(krcd_list)) {cat(': Done\n')} else {cat('\014')}
        Sys.sleep(0.1)
    }
}

