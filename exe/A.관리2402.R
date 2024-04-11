################################################################### #.
#' CLASS : 채권분석 패키지 초기화
#'
#'  funs : 1. Git & GitHub Connection
#'
################################################################### #.



############################'
## GIT Setting
############################.
# git config --global user.name "heystat"
# git config --global user.email "heystat@naver.com"
# git config --global --list

## set the user's global user.name and user.email
# usethis::use_git_config(user.name = "heystat", user.email = "heystat@naver.com")

# usethis::git_remotes()

# usethis::use_git_remote(
#   "origin",
#   "https://github.com/heystat/ssass.git",
#   overwrite = TRUE
# )

# usethis::use_git()
# usethis::use_github()



#공개키 조회
# file.exists("~/.ssh/id_rsa.pub")
# file.exists("C:/Users/heystat/.ssh/id_rsa.pub")


############################'
## 내부데이터(sysdata.rda)
############################.
# dat_pth  = file.path(dirname(rstudioapi::getSourceEditorContext()$path), 'data')
# fee_amt  = 5
# fee_rate = 0.002
# tax_rate = 0.154
# dat_pth  = file.path(dirname(rstudioapi::getSourceEditorContext()$path), 'data')
# crdt_gd  = factor(levels = c('AAA+', 'AAA', 'AAA-', 'AA+', 'AA', 'AA-', 'A+', 'A', 'A-',
#                              'BBB+', 'BBB', 'BBB-', 'BB+', 'BB', 'BB-', 'B+', 'B', 'B-',
#                              'CCC+', 'CCC', 'CCC-', 'CC+', 'CC', 'CC-', 'C+', 'C', 'C-'), ordered=TRUE)
# krx_otp_base = list(mktId = 'BND', bndTpCd = 'TT', basddTpCd = '2', money = '1', csvxls_isNo = 'false', name = 'fileDown')
#
# usethis::use_data(fee_amt, fee_rate, tax_rate, dat_pth, crdt_gd, krx_otp_base, internal = TRUE, overwrite = TRUE)


############################'
## 외부데이터
############################.
# issue <- scrKrxIssue(krcd = 'KR6079161C75')
# usethis::use_data(issue, overwrite = TRUE)

# system.file('issue', package = 'ssass')


############################'
## 채권 컬럼명 정의
############################.
bond_hanname <-
  list('ISU_CD'='채권CD',
       'ISU_NM'='채권명',
       'CREDIT'='신용등급',
       'BTP_NM'='채권유형',
       'INT_MD'='발행형태',
       'OPTION'='옵션CD',
       'INT_FQ'='연지급수',
       'ISU_DD'='발행일',
       'RDM_DD'='상환일',
       'RMN_DS'='잔존일수',

       'COUPON'='표면금리',
       'RDM_RT'='상환율',
       'TAX_MD'='과세방법',
       'INV_GD'='투자등급',
       'REG_DD'='등록일',
       'DSCRPT'='고려사항',

       'BAS_DD'='기준일',
       'PRC_PR'='현재가',
       'PRC_AV'='평균가',
       'BTX_RR'='세전수익률',
       'TRD_VM'='거래량',
       'TRD_AT'='거래금액',

       'PRC_EV'='민평가',
       'ROR_EV'='민평수익률',

       'INV_VM'='투자수량',
       'INV_AT'='투자금액',

       'TRD_DD'='매매일',
       'TRD_PC'='매매가',

       'BUY_DD'='매수일',
       'BUY_PC'='매수가',
       'SEL_DD'='매도일',
       'SEL_PC'='매도가',

       )




##////////////////////////////////////////////////////End of Documents.
