#' 채권 발행/매매 정보
#'
#' 개별채권 발행정보+매매현황(상세)
#'
#' @param ... OTP 인수들
#' @param krcd 채권CD
#' @param eprc 민평가 포함여부
#'
#' @return data.frame
#' @export
#' @examples
#' krcd = 'KR6018001D68'
#' krcd = 'KR103502G990'
#' seekBond(krcd)
#' info = getIssuePrice(krcd)
#'

# getIssuePrice <- function(krcd, eprc=TRUE, nday=30,...) {
#     isu = getIssue(krcd)
#     prc = getBondPrice(krcd, nday=7) %>% last()
#     # remain = tryCatch(dayCount(prc$Date, isu$rdm_dd, 2), error = function(e) e)
#     # prc[['잔존기간']] = ifelse(remain >= 31, paste(red(round(remain/365.25*12)), white('개월')), paste(red(remain), white('일')))
#
#     if (eprc==TRUE) prc$민평가 = last(indBondEvalPrice(krcd, nday)$민평가)
#     trd = list(...)
#     if (is.null(trd$buy_dd)) trd$buy_dd <- Sys.Date()
#     if (is.null(trd$buy_pc)) trd$buy_pc <- prc$Close
#     if (is.null(trd$sel_dd)) trd$sel_dd <- isu$rdm_dd
#     if (is.null(trd$sel_pc)) trd$sel_pc <- isu$rdm_rt*10000
#     trd[['투자일수']] = dayCount(trd$buy_dd, trd$sel_dd, 2)
#
#     inv = tryCatch(getBinvest() %>% filter(CODE==krcd), error = function(e) data.frame())
#     return(list(issue = isu, price = prc, trade = as.data.frame(trd), invst = inv))
# }

# dat <- get_isutrd()
# dat %>% tail


#' 채권수익률(이표채)
#'
#' 개별채권 수익률 시뮬레이션
#'
#' @param ibnd 개별채권정보(issue, price, trade)
#' @param fee_amt 거래세금(원)
#' @param tax_rate 원천징수세율
#' @param ... 채권거래정보
#'
#' @return 데이터프레임
#' @export
#' @examples
#' krcd = 'KR6079161C75'
#' tabs = getIssuePrice(krcd = 'KR6079161C75', buy_pc = 8519)
#' yieldFB(tabs)
#'

# isu_trd <- get_isutrd()
# colnames(isu_trd) %>% tolower
# ind <- isu_trd %>% filter(ISU_CD == 'KR6079161B68')
#
# yieldFB(
# int_md=ind$INT_MD,
# option=ind$OPTION,
# int_fq=ind$INT_FQ,
# isu_dd=ind$ISU_DD,
# rdm_dd=ind$RDM_DD,
# coupon=ind$COUPON,
# rdm_rt=ind$RDM_RT,
# buy_dd=Sys.Date(),
# buy_pc=ind$PRC_AV,
# tax_md=ind$TAX_MD
# )

yieldFB <- function(int_md,
                    option,
                    int_fq,
                    isu_dd,
                    rdm_dd,
                    coupon,
                    rdm_rt,
                    buy_dd,
                    buy_pc,
                    sel_dd=NULL,
                    sel_pc=NULL,
                    tax_md,
                    rf=0.05,
                    tax_rate=0.154,
                    fee_amt=5, ...) {

  # 채권정보 ____________________________________________________________
  if (missing(buy_dd)) buy_dd = Sys.Date()
  if (rdm_dd>=Sys.Date()+365*100) rdm_dd = Sys.Date()+365*100

  # 현금흐름-이표채 _____________________________________________________
  if (int_md == '이표채') {

    # 경과이자 ____________________________________________________________
    prevC = coupons.prev(buy_dd, rdm_dd, int_fq)
    nextC = coupons.next(buy_dd, rdm_dd, int_fq)
    accrued = 10000 * coupon * jrvFinance::yearFraction(prevC, buy_dd, prevC, nextC, int_fq, 'ACT/ACT')

    # 날짜정보 ____________________________________________________________
    start = jrvFinance::yearFraction(buy_dd, nextC, prevC, nextC, int_fq, 'ACT/ACT')
    Year = c(0, seq(from = start, by = 1/int_fq, length.out = coupons.n(buy_dd, rdm_dd, int_fq)))
    Date = c(buy_dd, coupons.dates(prevC, rdm_dd, int_fq))

    # 세전현금 ____________________________________________________________
    btaxCF = c(-buy_pc, rep(coupon*10000/int_fq, length(Date)-1))
    btaxCF[length(btaxCF)] = (rdm_rt + coupon/int_fq) * 10000

    # taxable ____________________________________________________________
    if (length(btaxCF) >= 3) {taxable = c(0, btaxCF[2]-accrued, btaxCF[3:length(btaxCF)])}
    if (length(btaxCF) <  3) {taxable = c(0, btaxCF[2]-accrued)}

    # tax_md ____________________________________________________________
    if (tax_md=='dvd') {
      redempt = 10000 * (rdm_rt-1) * dayCount(buy_dd, rdm_dd, 2) / dayCount(isu_dd, rdm_dd, 2)
    } else if (tax_md=='tot') {
      redempt = 10000 * (rdm_rt-1)
    }
    taxable[length(taxable)] = redempt + coupon/int_fq * 10000
    trdFee = c(fee_amt, rep(0, length(Date)-2), fee_amt)
    TaxAmt = taxable * tax_rate

    # 세후현금 ____________________________________________________________
    ataxCF = btaxCF - TaxAmt - trdFee
    TCF = data.frame(Date=Date, Year=round(Year,1), btaxCF=round(btaxCF,1), taxable=round(taxable,1), TaxAmt=round(TaxAmt,1), trdFee=trdFee, ataxCF=round(ataxCF,1))

    # 매도반영 ____________________________________________________________
    if (!is.null(sel_dd)) {
      if (sel_dd < rdm_dd) {
        prevT = coupons.prev(sel_dd, rdm_dd, int_fq)
        nextT = coupons.next(sel_dd, rdm_dd, int_fq)
        accrued = 10000 * coupon * jrvFinance::yearFraction(prevT, sel_dd, prevT, nextT, int_fq, 'ACT/ACT')

        TCF = TCF[TCF$Date <= nextT,]
        TCF[nrow(TCF), 'Date'] = sel_dd
        TCF[nrow(TCF), 'btaxCF'] = sel_pc
        TCF[nrow(TCF), 'taxable'] = 10000 * coupon * jrvFinance::yearFraction(prevT, sel_dd, prevT, nextT, int_fq, 'ACT/ACT')
        TCF[nrow(TCF), 'trdFee'] = fee_amt
        TCF[nrow(TCF), 'TaxAmt'] = TCF[nrow(TCF), 'taxable'] * tax_rate
        TCF[nrow(TCF), 'ataxCF'] = TCF[nrow(TCF), 'btaxCF'] - TCF[nrow(TCF), 'TaxAmt'] - TCF[nrow(TCF), 'trdFee']
      }
    }
  }

  # 현금흐름-복리채 _____________________________________________________
  if (tax_md == '복리채') {

    # 날짜정보 ____________________________________________________________
    start = jrvFinance::yearFraction(buy_dd, nextC, prevC, nextC, int_fq, 'ACT/ACT')
    Year = c(0, seq(from = start, by = 1/int_fq, length.out = coupons.n(buy_dd, rdm_dd, int_fq)))
    Date = c(buy_dd, coupons.dates(prevC, rdm_dd, int_fq))

    # 세전현금 ____________________________________________________________
    prd_rt = dayCount(buy_dd, rdm_dd, 2) / dayCount(isu_dd, rdm_dd, 2)
    tot_int_fq = length(seq(from=isu_dd, to=rdm_dd, by=paste(12/int_fq, 'months')))-1
    rdmpt_amt = (1+as.numeric(coupon)/int_fq)^tot_int_fq*10000 ##확인필요
    btaxCF = c(-buy_pc, rep(0, length(Date)-2), rdmpt_amt)

    # taxable ____________________________________________________________
    taxable = c(rep(0, length(btaxCF)-1), (rdmpt_amt-10000)*prd_rt)
    trdFee = c(fee_amt, rep(0, length(Date)-2), fee_amt)
    TaxAmt = taxable * tax_rate

    # 세후현금 ____________________________________________________________
    ataxCF = btaxCF - TaxAmt - trdFee
    TCF = data.frame(Date=Date, Year=round(Year,1), btaxCF=round(btaxCF,1), taxable=round(taxable,1), TaxAmt=round(TaxAmt,1), trdFee=trdFee, ataxCF=round(ataxCF,1))
  }

  # 수익계산 ____________________________________________________________
  YIELD =
    data.frame(RMN_DS = suppressWarnings(dayCount(first(TCF$Date), last(TCF$Date), 2)),
               NPV = suppressWarnings(npv(cf = TCF$ataxCF[-1], cf.t = TCF$Year[-1], rate = rf)), ##현재가
               INN_RR = suppressWarnings(irr(cf = TCF$ataxCF, cf.t = TCF$Year)), ##내부수익률
               TOT_RR = sum(TCF$ataxCF)/(buy_pc+fee_amt)) %>%                    ##총기간수익률
    mutate(NPV = formattable::comma(round(NPV), digits=0),
           AVG_RR = TOT_RR/max(TCF$Year),                                           ##연평균수익률
           MON_RR = (TOT_RR/dayCount(buy_dd, rdm_dd, 2))*30) %>%
    relocate(NPV, INN_RR, AVG_RR, MON_RR, TOT_RR, RMN_DS) %>%
    mutate(across(c(INN_RR, AVG_RR, MON_RR, TOT_RR), ~ formattable::percent(.x, 1)))

  TCF = TCF %>%
    mutate(across(c(btaxCF, taxable, TaxAmt, trdFee, ataxCF), ~ formattable::comma(.x, digits=0)))

  return(list(YIELD=YIELD, TCF=TCF))
}


#' 실시간 채권수익률(이표채) 비교
#'
#' 실시간 채권 수익률 비교/평가
#'
#' @param ... OTP 인수들
#' @param krcd 채권CD
#' @param eprc 민평가 포함여부
#' @param fee_amt 거래세금(원)
#' @param tax_rate 원천징수세율
#'
#' @return 데이터프레임
#' @export
#' @examples
#' tab_bond_yield()
#'
tab_bond_yield <- function(sect='회사채', minrt = 0.00, maxrt = Inf, n = Inf) {

  outp = data.frame()
  bnds = get_isutrd(sect)

  for (i in 1:nrow(bnds)) {cat(paste0(round(i/nrow(bnds)*100), '% completed'))
    ind = tryCatch(yieldFB(int_md=bnds[i, 'INT_MD'],
                           option=bnds[i, 'OPTION'],
                           int_fq=bnds[i, 'INT_FQ'],
                           isu_dd=bnds[i, 'ISU_DD'],
                           rdm_dd=bnds[i, 'RDM_DD'],
                           coupon=bnds[i, 'COUPON'],
                           rdm_rt=bnds[i, 'RDM_RT'],
                           buy_dd=Sys.Date(),
                           buy_pc=bnds[i, 'PRC_AV'],
                           tax_md=bnds[i, 'TAX_MD']), error = function(e) e)
    if (inherits(ind, "error")) next
    ind_out = cbind(bnds[i,], ind$YIELD)
    if (nrow(outp) == 0) {outp = ind_out} else {outp = bind_rows(outp, ind_out)}
    if (i == nrow(bnds)) {cat(': Done\n')} else {cat('\014')}
  }

  outp %>%
    mutate(RMN_PD = ifelse(RMN_DS>=365.25, paste(round(RMN_DS/365.25, 1), 'Y'),
                         ifelse(RMN_DS>=90, paste(round(RMN_DS/30, 0), 'M'), paste(RMN_DS, 'D')))) %>%
    mutate(across(c(INN_RR, AVG_RR, MON_RR, TOT_RR, COUPON), ~ formattable::percent(.x, 1))) %>%
    mutate(across(c(RMN_DS, NPV, PRC_PR, PRC_AV, TRD_VM, TRD_AT), ~ formattable::comma(.x, digits=0))) %>%
    # rename(연수익률 = AVG_RR, 월수익률 = MON_RR, 총수익률 = TOT_RR) %>%
    mutate(RMN_YR = round(as.numeric(RMN_DS/365.25), 1)) %>%
    select(ISU_CD, ISU_NM, INV_GD, OPTION, CREDIT, RMN_YR, RMN_PD, COUPON,
           NPV, PRC_PR, TRD_VM, AVG_RR, MON_RR, TOT_RR) %>%
    arrange(desc(AVG_RR)) %>%
    filter(between(as.numeric(AVG_RR), minrt, maxrt)) %>%
    filter(row_number() <= n)
}


#' 실시간 채권수익률(이표채) 그래프
#'
#' @param tab 수익률 비교테이블
#' @param sect 채권분류
#' @param minrt 최소 수익률
#' @param maxrt 최대 수익률
#' @param n 테이블내 채권수
#'
#' @examples
#' tab = plot_bond_yield()
#' tab
#'
plot_bond_yield <- function(tab, sect='회사채', minrt = 0.00, maxrt = Inf, n = Inf) {
    if (missingArg(tab)) tab = tab_bond_yield(sect, minrt, maxrt, n)
    tab$CRD_GD = suppressWarnings(sapply(tab$CREDIT, cnvt_crdt2fac))

    gg_viz =
        ggplot(tab, aes(x=CRD_GD, y=AVG_RR, Name=ISU_NM, Period=RMN_YR,
                        Yield=AVG_RR, Price=PRC_PR, size=TRD_VM, color=INV_GD)) +
        geom_point(alpha=0.7) +
        scale_color_manual(values = c('aaa'='Firebrick', 'bbb'='orange', 'ccc'='gold', 'ddd'='grey70'))
    ggplotly(gg_viz, tooltip = c("Name", "Yield", "Period", "Price", "size"))
}




#' 채권 (발행/거래/투자) 정보 출력
#'
#' @param obj 발행정보 오브젝트
#' @param ... 파라메터
#'
#' @return 텍스트
#' @export
#' @examples
#' reportBvalue(krcd = 'KR6079161C75')
#'
reportBvalue <- function(krcd, ...) {

    txtColPrint <- function(key, val, vform, ksize=6, vside='right', kcol='blue', vcol='white', ntot=25, nadd=0) {

        key = format(key, width = ksize)

        if (vform=='cmm') {
            val=formattable::comma(val, 0)
        } else if (vform=='pct') {
            val=formattable::percent(val, 1)
        }
        if (kcol == 'blue' & vcol == 'white') {
            out = paste(paste(blue(key), str_pad(white(val), ntot-str_count(key)-str_count(val)+nadd, vside, pad=' ')))
        }
        if (kcol == 'blue' & vcol == 'red') {
            out = paste(paste(blue(key), str_pad(red(val), ntot-str_count(key)-str_count(val)+nadd, vside, pad=' ')))
        }
        invisible(out)
    }

    # 채권수익 ____________________________________________________________
    isu = get_issue(krcd)
    trd = last(get_bond_price_ts(krcd)) %>% left_join(get_bond_price_eval(krcd) %>% select(Date, PRC_EV), by = 'Date')
    inv = get_bond_balance() %>% filter(ISU_CD == krcd) %>% select(PRC_BY, INV_VM, INV_AT)

    nrow(inv)
    # arg = as.data.frame(list(...))
    arg = as.data.frame(list(buy_dd = Sys.Date()))
    if (nrow(arg)==0) {bnds = cbind(isu, trd)} else {bnds = cbind(isu, trd, arg)}

    if (!'BUY_DD' %in% names(bnds)) bnds$BUY_DD = Sys.Date()
    # if (!'SEL_PC' %in% names(bnds)) bnds$SEL_PC = Sys.Date()
    sel_pc


    tabs = tryCatch(yieldFB(int_md=bnds[1, 'INT_MD'],
                            option=bnds[1, 'OPTION'],
                            int_fq=bnds[1, 'INT_FQ'],
                            isu_dd=bnds[1, 'ISU_DD'],
                            rdm_dd=bnds[1, 'RDM_DD'],
                            coupon=bnds[1, 'COUPON'],
                            rdm_rt=bnds[1, 'RDM_RT'],
                            buy_dd=bnds[1, 'BUY_DD'],
                            buy_pc=bnds[1, 'PRC_AV'],
                            tax_md=bnds[1, 'TAX_MD']), error = function(e) e)

    # if (nrow(tabs$invst)==0) {
    #     dat = cbind(tabs$issue, tabs$price, tabs$trade, tabs$yield)
    # } else {
    #     dat = cbind(tabs$issue, tabs$price, tabs$trade, tabs$yield, tabs$invst %>% select(-c('rdm_dd','민평가')))
    # }
    #

    tdt = cbind(bnds, tabs$YIELD) %>%
        mutate(dsct = (PRC_EV-PRC_AV )/PRC_AV) %>%
        transmute(발행일 = as.character.Date(ISU_DD ), ##채권CD,
                  발행형태 = INT_MD,
                  CREDIT,
                  만기일 = '', #as.character.Date(만기일),
                  COUPON = as.character(formattable::percent(COUPON,1)),
                  옵션CD = OPTION ,
                  상환일 = as.character.Date(RDM_DD),
                  상환율 = as.character(formattable::percent(RDM_RT,1)),
                  CLASS  = INV_GD,
                  시세정보 = 'PRICE',
                  기준일 = as.character.Date(Date),
                  현재가 = as.character(formattable::comma(PRC_PR,0)),
                  잔존기간 = ifelse(RMN_DS >= 31, paste(round(RMN_DS/365.25*12),'개월'), paste(RMN_DS,'일')),
                  매수일 = as.character.Date(BUY_DD ),
                  민평가 = as.character(formattable::comma(PRC_EV,0)),
                  거래량 = as.character(formattable::comma(TRD_VM,0)),
                  매도일 = '', #as.character.Date(sel_dd),
                  민평DSC = as.character(formattable::percent(dsct,1)),
                  거래금액 = as.character(formattable::comma(TRD_AT,0)),
                  수익분석 = 'YIELD',
                  순현가 = as.character(formattable::comma(NPV,0)),
                  내부ROR = as.character(formattable::percent(INN_RR,1)),
                  총수익률 = as.character(formattable::percent(TOT_RR,1)),
                  할인율 = as.character(formattable::percent(NPV/PRC_AV-1,1)),
                  평균ROR = as.character(formattable::percent(AVG_RR,1)),
                  월수익률 = as.character(formattable::percent(MON_RR,1)),
                  YIELD7 = '',
                  YIELD8 = '',
                  YIELD9 = '')

    if (nrow(inv)==1) {
        tin = cbind(bnds, tabs$YIELD, inv) %>%
          mutate(RDM_PC = RDM_RT*10000) %>%
          mutate(현수익금 = (PRC_AV-PRC_BY )*INV_VM /10000,
                   현수익률 = (PRC_AV-PRC_BY )/PRC_BY ,
                   상환수익 = (RDM_PC-PRC_BY )*INV_VM /10000,
                   상환ROR = (RDM_PC-PRC_BY )/PRC_BY) %>%
            transmute(투자분석 = 'INVST',
                      매입가   = as.character(formattable::comma(PRC_BY,0)),
                      보유수량 = as.character(formattable::comma(INV_VM,0)),
                      매입금액 = as.character(formattable::comma(INV_AT,0)),
                      평균가   = as.character(formattable::comma(PRC_AV ,0)),
                      현수익금 = as.character(formattable::comma(현수익금,0)),
                      현수익률 = as.character(formattable::percent(현수익률,1)),
                      상환가2  = as.character(formattable::comma(RDM_PC,0)),
                      상환수익 = as.character(formattable::comma(상환수익,0)),
                      상환ROR  = as.character(formattable::percent(상환ROR,1)))
        tdt = cbind(tdt, tin)
    }

    # 결과출력 ____________________________________________________________
    keys = format(names(tdt))
    vals = format(unname(unlist(tdt[1,])), justify = 'right')

    cat('\n', black$bgYellow$bold('  ', str_pad(isu$ISU_NM, 40, 'right')),'\n')

    cat(green('\n채권정보\n'))
    cat(blue(paste(
        paste(blue(keys[1]), white(vals[1])),
        paste(blue(keys[2]), white(vals[2])),
        paste(blue(keys[3]), red(vals[3])),
        sep = '  |  '
    )), '\n')
    cat(blue(paste(
        paste(blue(keys[4]), white(vals[4])),
        paste(blue(keys[5]), white(vals[5])),
        paste(blue(keys[6]), white(vals[6])),
        sep = '  |  '
    )), '\n')
    cat(blue(paste(
        paste(blue(keys[7]), red(vals[7])),
        paste(blue(keys[8]), white(vals[8])),
        paste(blue(keys[9]), white(vals[9])),
        sep = '  |  '
    )), '\n')
    cat(green('\n시세정보\n'))
    cat(blue(paste(
        paste(blue(keys[11]), white(vals[11])),
        paste(blue(keys[12]), red(vals[12])),
        paste(blue(keys[13]), red(vals[13])),
        sep = '  |  '
    )), '\n')
    cat(blue(paste(
        paste(blue(keys[14]), white(vals[14])),
        paste(blue(keys[15]), white(vals[15])),
        paste(blue(keys[16]), white(vals[16])),
        sep = '  |  '
    )), '\n')
    cat(blue(paste(
        paste(blue(keys[17]), white(vals[17])),
        paste(blue(keys[18]), red(vals[18])),
        paste(blue(keys[19]), white(vals[19])),
        sep = '  |  '
    )), '\n')
    cat(green('\n수익분석\n'))
    cat(blue(paste(
        paste(blue(keys[21]), red(vals[21])),
        paste(blue(keys[22]), white(vals[22])),
        paste(blue(keys[23]), white(vals[23])),
        sep = '  |  '
    )), '\n')
    cat(blue(paste(
        paste(blue(keys[24]), white(vals[24])),
        paste(blue(keys[25]), red(vals[25])),
        paste(blue(keys[26]), white(vals[26])),
        sep = '  |  '
    )), '\n')

    if (nrow(inv)==1) {
        cat(green('\n투자분석\n'))
        cat(blue(paste(
            paste(blue(keys[31]), white(vals[31])),
            paste(blue(keys[32]), white(vals[32])),
            paste(blue(keys[33]), white(vals[33])),
            sep = '  |  '
        )), '\n')
        cat(blue(paste(
            paste(blue(keys[34]), white(vals[34])),
            paste(blue(keys[35]), red(vals[35])),
            paste(blue(keys[36]), white(vals[36])),
            sep = '  |  '
        )), '\n')
        cat(blue(paste(
            paste(blue(keys[37]), white(vals[37])),
            paste(blue(keys[38]), white(vals[38])),
            paste(blue(keys[39]), red(vals[39])),
            sep = '  |  '
        )), '\n')
    }
    cat(green('\n현금흐름\n'))
    tabs$TCF %>%
        mutate(across(c(btaxCF, taxable, TaxAmt, trdFee, ataxCF), ~ formattable::comma(.x, digits=0)))
}
