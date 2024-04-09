################################################################### #.
#' CLASS : 채권 발행정보 및 보유현황 관리
#'
#'  funs : 1. 발행정보 수정 및 적재
#'         2. 투자채권 업데이트 관리
#'
#' 참 고 : 1. KRX    : browseURL('http://data.krx.co.kr')
#'
#' + Fear: webFearGreed()
#' + Path: dirname(rstudioapi::getSourceEditorContext()$path)
#'
################################################################### #.



############################'
## 작업환경
############################.
# if (!exists('home')) home = dirname(rstudioapi::getSourceEditorContext()$path)
# work_path = workPathBond(home)



############################'
## GIT Setting
############################.
# git config --global user.name "heystat"
# git config --global user.email "heystat@naver.com"
# git config --global --list

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
# usethis::use_data(fee_amt, fee_rate, tax_rate, internal = TRUE, overwrite = TRUE)


############################'
## 외부데이터
############################.
issue <- scrKrxIssue(krcd = 'KR6079161C75')

usethis::use_data(issue, overwrite = TRUE)



############################'
## 원시데이터
############################.
# issue <- scrKrxIssue(krcd = 'KR6079161C75')

# system.file('issue', package = 'ssass')
?system.file



############################'
## 발행정보 파일 삭제(분기1회?)
############################.
# resetIssue(krcd = 'KR6079161C75')
# resetIssue(mthd = 'all')


############################'
## 보유채권현황적재(클립보드 --> RDS파일)
##  - 한국투자증권 > 계좌별잔고조회
############################.
# mbnd = myClpToRds(accnt='M채권')
# mbnd


############################'
## 보유채권파일조회(RDS파일)
##  - 현금화 가능 단기채 매수
############################.
mbnd = getBinvest()
mbnd
sum(mbnd$매입금액)
sum(mbnd$보유수량)



############################'
##' 보유채권관리(RDS파일 --> ISSUE)
############################.
# for (i in 1:nrow(mbnd)) {
#     i=1
#     krcd = mbnd[i, 'CODE']
#     news = mbnd[i, c('기준일', '보유수량','매입가','매입금액')]
#     issue= getIssue(krcd)
#     if (is.null(nrow(issue[['invst']]))) {
#         issue[['invst']] = data.frame(CLASS = NA_character_, 보유수량 = 0, 매입가 = 0, 매입금액=0)
#     } else {
#         issue[['invst']] = gdata::update.list(issue[['invst']], news)
#         issue$invst[['CLASS']] = 'A'
#         issue$name[['수정일']] = today()
#     }
#     saveRDS(issue, file.path(work_path$issue, krcd))
# }


############################'
## `채권종목조회`
############################.
seekBond('한국유니온제약') %>%
  mutate(발행금액 = comma(발행금액,0))


############################'
## 에스지이17
############################.
krcd = 'KR6255221D45'
updateIssue(krcd, CLASS='aaa')



############################'
## 씨제이 씨지브이32CB(신종)
############################.
krcd = 'KR6079161B68'
updateIssue(krcd, 상환일 = as.Date('2026-06-08'), 상환율 = 1.107456, 과세방법='발생분과세')
updateIssue(krcd, 과세방법='발생분과세', CLASS='aaa')


############################'
## 씨제이 씨지브이35CB(신종)
############################.
krcd = 'KR6079161C75'
updateIssue(krcd, 상환일 = as.Date('2027-07-21'), 상환율 = 1.134320118586, 과세방법='발생분과세')
updateIssue(krcd, 과세방법='발생분과세', CLASS='aaa')


############################'
## 씨제이 씨지브이39(신종)
############################.
krcd = 'KR6079162E31'
updateIssue(krcd, 상환일 = as.Date('2026-03-15'))


############################'
## `태영건설68`
#'   - 워크아웃신청 2023-12-28
#'   - 워크아웃시 회생확률 35%
#'     기대수익 = (10000*0.63*0.35 - 10000*1.00*0.65)
#'   - 상환일 3년 지연 예상
############################.
krcd = 'KR6009411B71'
updateIssue(krcd)
updateIssue(krcd, 상환일 = as.Date('2027-07-19'), 주의사항 = '상환일 3년 지연 예상')


############################'
## 케이지모빌리티122
############################.
krcd = 'KR6003622DC8'
updateIssue(krcd, 상환일 = as.Date('2025-12-05'), 상환율 = 1.061598, CLASS='ccc')


############################'
## 국고(19-6)
############################.
krcd = 'KR103502G990'
updateIssue(krcd, 신용GD = 'A', CLASS='aaa')


############################'
## 에이프로젠헬스케어앤게임즈6
############################.
krcd = 'KR6109961D71'
updateIssue(krcd, 상환일 = as.Date('2025-01-25'), 상환율 = 1.030953, CLASS='ccc')


############################'
## 동아에스티8CB
############################.
krcd = 'KR6170901B86'
updateIssue(krcd, 상환일 = as.Date('2024-08-03'), 상환율 = 1.030414, CLASS='bbb')


############################'
## 유니슨15
############################.
krcd = 'KR6018001D68'
updateIssue(krcd, 상환일 = as.Date('2024-12-13'), 상환율 = 1.046429, CLASS='aaa')


############################'
## 핸즈코퍼레이션2
############################.
krcd = 'KR6143211D60'
updateIssue(krcd, 상환일 = as.Date('2024-12-15'), 상환율 = 1.033702, CLASS='aaa')


############################'
## 에이치엘비생명과학9
############################.
krcd = 'KR6067631C64'
updateIssue(krcd, 상환일 = as.Date('2024-06-15'), 상환율 = 1.040707, CLASS='aaa')


############################'
## 삼척블루파워9
############################.
krcd = 'KR6150351D99'
updateIssue(krcd, CLASS='bbb')


############################'
## 이수앱지스8CB
############################.
krcd = 'KR6086891DC9'
updateIssue(krcd, 상환일 = as.Date('2025-12-22'), 상환율 = 1.041794, CLASS='bbb')


############################'
## 한국토지신탁44-2
############################.
krcd = 'KR6034832E28'
updateIssue(krcd, CLASS='aaa')


############################'
## 한국유니온제약3
############################.
krcd = 'KR6080721D34'
updateIssue(krcd, 상환일 = as.Date('2024-09-17'), 상환율 = 1.046429, CLASS='ccc')


############################'
##' `이원다이애그노믹스10 `
############################.
krcd = 'KR6245621D59'
updateIssue(krcd, 상환일 = as.Date('2024-05-11'), 상환율 = 1.030681, CLASS='aaa')




##////////////////////////////////////////////////////End of Documents.
