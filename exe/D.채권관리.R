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
## 보유채권현황적재(클립보드 --> RDS파일)
##  - 한국투자증권 > 계좌별잔고조회
############################.
# mbnd = myClpToRds(accnt='M채권')
# mbnd


############################'
## 보유채권파일조회(RDS파일)
##  - 현금화 가능 단기채 매수
############################.
# mbnd = getBinvest()
# mbnd
# sum(mbnd$매입금액)
# sum(mbnd$보유수량)



############################'
##' 보유채권관리(RDS파일 --> ISSUE)
############################.
# for (i in 1:nrow(mbnd)) {
#     i=1
#     krcd = mbnd[i, 'CODE']
#     news = mbnd[i, c('기준일', '보유수량','매입가','매입금액')]
#     issue= getIssue(krcd)
#     if (is.null(nrow(issue[['invst']]))) {
#         issue[['invst']] = data.frame(INV_GD = NA_character_, 보유수량 = 0, 매입가 = 0, 매입금액=0)
#     } else {
#         issue[['invst']] = gdata::update.list(issue[['invst']], news)
#         issue$invst[['INV_GD']] = 'A'
#         issue$name[['수정일']] = today()
#     }
#     saveRDS(issue, file.path(work_path$issue, krcd))
# }


############################'
## `채권종목조회`
############################.
seek_bond('한국유니온제약')


############################'
## 에스지이17
############################.
krcd = 'KR6255221D45'
update_issue(krcd, INV_GD='aaa')


############################'
## 씨제이 씨지브이32CB(신종)
############################.
krcd = 'KR6079161B68'
get_issue(krcd)
print.get_issue(krcd)
update_issue(krcd, RDM_DD = as.Date('2026-06-08'), RDM_RT = 1.107456, TAX_MD  = 'tot', INV_GD='aaa',
             DSCRPT = '현물출자완료 후 매도(6.5% 목표)')


############################'
## 씨제이 씨지브이35CB(신종)
############################.
krcd = 'KR6079161C75'
print.get_issue(krcd)
update_issue(krcd, RDM_DD = as.Date('2027-07-21'), RDM_RT = 1.134320118586, TAX_MD  = 'tot', INV_GD='aaa',
             DSCRPT = '현물출자완료 후 매도(6.5% 목표)')


############################'
## 씨제이 씨지브이39(신종)
############################.
krcd = 'KR6079162E31'
print.get_issue(krcd)
update_issue(krcd, RDM_DD = as.Date('2026-03-15'), INV_GD = 'ccc')


############################'
## `태영건설68`
#'   - 워크아웃신청 2023-12-28
#'   - 워크아웃시 회생확률 35%
#'     기대수익 = (10000*0.63*0.35 - 10000*1.00*0.65)
#'   - RDM_DD 3년 지연 예상
############################.
krcd = 'KR6009411B71'
update_issue(krcd)
update_issue(krcd, RDM_DD = as.Date('2027-07-19'), INV_GD='aaa', DSCRPT = '워크아웃으로 상환 3년 지연 예상')


############################'
## 케이지모빌리티122
############################.
krcd = 'KR6003622DC8'
update_issue(krcd, RDM_DD = as.Date('2025-12-05'), RDM_RT = 1.061598, INV_GD='ccc')


############################'
## 국고(19-6)
############################.
krcd = 'KR103502G990'
get_issue(krcd)
update_issue(krcd, CREDIT  = 'A', INV_GD='aaa')


############################'
## 에이프로젠헬스케어앤게임즈6
############################.
krcd = 'KR6109961D71'
update_issue(krcd, RDM_DD = as.Date('2025-01-25'), RDM_RT = 1.030953, INV_GD='ccc')


############################'
## 동아에스티8CB
############################.
krcd = 'KR6170901B86'
get_issue(krcd)
update_issue(krcd, RDM_DD = as.Date('2024-08-03'), RDM_RT = 1.030414, INV_GD='bbb')


############################'
## 유니슨15
############################.
krcd = 'KR6018001D68'
update_issue(krcd, RDM_DD = as.Date('2024-12-13'), RDM_RT = 1.046429, INV_GD='aaa')


############################'
## 핸즈코퍼레이션2
############################.
krcd = 'KR6143211D60'
update_issue(krcd, RDM_DD = as.Date('2024-12-15'), RDM_RT = 1.033702, INV_GD='aaa')


############################'
## 에이치엘비생명과학9
############################.
krcd = 'KR6067631C64'
update_issue(krcd, RDM_DD = as.Date('2024-06-15'), RDM_RT = 1.040707, INV_GD='bbb')


############################'
## 삼척블루파워9
############################.
krcd = 'KR6150351D99'
update_issue(krcd, INV_GD='bbb')


############################'
## 이수앱지스8CB
############################.
krcd = 'KR6086891DC9'
update_issue(krcd, RDM_DD = as.Date('2025-12-22'), RDM_RT = 1.041794, INV_GD='bbb')


############################'
## 한국토지신탁44-2
############################.
krcd = 'KR6034832E28'
update_issue(krcd, INV_GD='ccc')


############################'
## 한국유니온제약3
############################.
krcd = 'KR6080721D34'
update_issue(krcd, RDM_DD = as.Date('2024-09-17'), RDM_RT = 1.046429, INV_GD='ccc')


############################'
##' `이원다이애그노믹스10 `
############################.
krcd = 'KR6245621D59'
update_issue(krcd, RDM_DD = as.Date('2024-05-11'), RDM_RT = 1.030681, INV_GD='aaa',
             DSCRPT = '감사의견거절>채권거래정지>상환불확실')




##////////////////////////////////////////////////////End of Documents.
