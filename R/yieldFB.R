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
getIssuePrice <- function(krcd, eprc=TRUE, nday=30,...) {
    isu = getIssue(krcd)
    prc = getBondPrice(krcd, nday=7) %>% last()
    # remain = tryCatch(dayCount(prc$Date, isu$상환일, 2), error = function(e) e)
    # prc[['잔존기간']] = ifelse(remain >= 31, paste(red(round(remain/365.25*12)), white('개월')), paste(red(remain), white('일')))

    if (eprc==TRUE) prc$민평가 = last(indBondEvalPrice(krcd, nday)$민평가)
    trd = list(...)
    if (is.null(trd$매수일)) trd$매수일 <- Sys.Date()
    if (is.null(trd$매수가)) trd$매수가 <- prc$Close
    if (is.null(trd$매도일)) trd$매도일 <- isu$상환일
    if (is.null(trd$매도가)) trd$매도가 <- isu$상환율*10000
    trd[['투자일수']] = dayCount(trd$매수일, trd$매도일, 2)

    inv = tryCatch(getBinvest() %>% filter(CODE==krcd), error = function(e) data.frame())
    return(list(issue = isu, price = prc, trade = as.data.frame(trd), invst = inv))
}


#' 채권수익률(이표채)
#'
#' 개별채권 수익률 시뮬레이션
#'
#' @param tabs 채권정보(issue, price, trade)
#' @param fee_amt 거래세금(원)
#' @param tax_rate 원천징수세율
#' @param ... 채권거래정보
#'
#' @return 데이터프레임
#' @export
#' @examples
#' krcd = 'KR6079161C75'
#' tabs = getIssuePrice(krcd = 'KR6079161C75', 매수가 = 8519)
#' yieldFB(tabs)
#'
yieldFB <- function(tabs, tax_rate=0.154, rf=0.05, fee_amt=5, ...) {

    # 채권정보 ____________________________________________________________
    # tabs = getIssuePrice(krcd, eprc, ...)
    info = cbind(tabs$issue, tabs$price, tabs$trade)
    for (t in 1:length(info)) {assign(names(info[t]), info[[t]])}

    # 현금흐름-이표채 _____________________________________________________
    if (발행형태 == '이표채') {

        # 경과이자 ____________________________________________________________
        prevC = coupons.prev(매수일, 상환일, 연지급수)
        nextC = coupons.next(매수일, 상환일, 연지급수)
        accrued = 10000 * 표면금리 * jrvFinance::yearFraction(prevC, 매수일, prevC, nextC, 연지급수, 'ACT/ACT')

        # 날짜정보 ____________________________________________________________
        start = jrvFinance::yearFraction(매수일, nextC, prevC, nextC, 연지급수, 'ACT/ACT')
        Year = c(0, seq(from = start, by = 1/연지급수, length.out = coupons.n(매수일, 상환일, 연지급수)))
        Date = c(매수일, coupons.dates(prevC, 상환일, 연지급수))

        # 세전현금 ____________________________________________________________
        세전CF = c(-매수가, rep(표면금리*10000/연지급수, length(Date)-1))
        세전CF[length(세전CF)] = (상환율 + 표면금리/연지급수) * 10000

        # 과세대상 ____________________________________________________________
        if (length(세전CF) >= 3) {과세대상 = c(0, 세전CF[2]-accrued, 세전CF[3:length(세전CF)])}
        if (length(세전CF) <  3) {과세대상 = c(0, 세전CF[2]-accrued)}

        # 과세방법 ____________________________________________________________
        if (과세방법=='보유분과세') {
            redempt = 10000 * (상환율-1) * dayCount(매수일, 상환일, 2) / dayCount(발행일, 상환일, 2)
        } else if (과세방법=='발생분과세') {
            redempt = 10000 * (상환율-1)
        }
        과세대상[length(과세대상)] = redempt + 표면금리/연지급수 * 10000
        거래비용 = c(fee_amt, rep(0, length(Date)-2), fee_amt)
        원천세 = 과세대상 * tax_rate

        # 세후현금 ____________________________________________________________
        세후CF = 세전CF - 원천세 - 거래비용
        TCF = data.frame(Date=Date, Year=round(Year,1), 세전CF=round(세전CF,1), 과세대상=round(과세대상,1), 원천세=round(원천세,1), 거래비용=거래비용, 세후CF=round(세후CF,1))

        # 매도반영 ____________________________________________________________
        if (매도일 < 상환일) {
            prevT = coupons.prev(매도일, 상환일, 연지급수)
            nextT = coupons.next(매도일, 상환일, 연지급수)
            accrued = 10000 * 표면금리 * jrvFinance::yearFraction(prevT, 매도일, prevT, nextT, 연지급수, 'ACT/ACT')

            TCF = TCF[TCF$Date <= nextT,]
            TCF[nrow(TCF), 'Date'] = 매도일
            TCF[nrow(TCF), '세전CF'] = 매도가
            TCF[nrow(TCF), '과세대상'] = 10000 * 표면금리 * jrvFinance::yearFraction(prevT, 매도일, prevT, nextT, 연지급수, 'ACT/ACT')
            TCF[nrow(TCF), '거래비용'] = fee_amt
            TCF[nrow(TCF), '원천세'] = TCF[nrow(TCF), '과세대상'] * tax_rate
            TCF[nrow(TCF), '세후CF'] = TCF[nrow(TCF), '세전CF'] - TCF[nrow(TCF), '원천세'] - TCF[nrow(TCF), '거래비용']
        }
    }

    # 현금흐름-이표채 _____________________________________________________
    if (발행형태 == '복리채') {

        # 날짜정보 ____________________________________________________________
        start = jrvFinance::yearFraction(매수일, nextC, prevC, nextC, 연지급수, 'ACT/ACT')
        Year = c(0, seq(from = start, by = 1/연지급수, length.out = coupons.n(매수일, 상환일, 연지급수)))
        Date = c(매수일, coupons.dates(prevC, 상환일, 연지급수))

        # 세전현금 ____________________________________________________________
        기간비율 = dayCount(매수일, 상환일, 2) / dayCount(발행일, 상환일, 2)
        이자횟수 = length(seq(from=발행일, to=상환일, by=paste(12/연지급수, 'months')))-1
        상환금액 = (1+as.numeric(표면금리)/연지급수)^이자횟수*10000 ##확인필요
        세전CF = c(-매수가, rep(0, length(Date)-2), 상환금액)

        # 과세대상 ____________________________________________________________
        과세대상 = c(rep(0, length(세전CF)-1), (상환금액-10000)*기간비율)
        거래비용 = c(fee_amt, rep(0, length(Date)-2), fee_amt)
        원천세 = 과세대상 * tax_rate

        # 세후현금 ____________________________________________________________
        세후CF = 세전CF - 원천세 - 거래비용
        TCF = data.frame(Date=Date, Year=round(Year,1), 세전CF=round(세전CF,1), 과세대상=round(과세대상,1), 원천세=round(원천세,1), 거래비용=거래비용, 세후CF=round(세후CF,1))
    }

    # 수익계산 ____________________________________________________________
    tabs[['yield']] =
        data.frame(NPV = suppressWarnings(npv(cf = TCF$세후CF[-1], cf.t = TCF$Year[-1], rate = rf)), ##현재가
                   IRR = suppressWarnings(irr(cf = TCF$세후CF, cf.t = TCF$Year)), ##내부수익률
                   TRR = sum(TCF$세후CF)/(매수가+fee_amt)) %>%                    ##총기간수익률
        mutate(NPV = round(NPV),
               ARR = TRR/max(TCF$Year),                                           ##연평균수익률
               MRR = (TRR/dayCount(매수일, 상환일, 2))*30) %>%
        relocate(NPV, IRR, ARR, MRR, TRR)
        # mutate(across(c(IRR, ARR, MRR, TRR), ~ formattable::percent(.x, 1)))

    tabs[['TCF']] =
        TCF %>%
        mutate(across(c(세전CF, 과세대상, 원천세, 거래비용, 세후CF), ~ formattable::comma(.x, digits=0)))

    return(tabs)
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
#' compareTable()
#'
compareTable <- function(sect='회사채', minrt = 0.00, maxrt = Inf, n = Inf) {

    outp = data.frame()
    bcds = bondList(sect)
    bnds = bondPrice() %>% filter(채권CD %in% bcds$채권CD)

    for (i in 1:nrow(bnds)) {cat(paste0(round(i/nrow(bnds)*100), '% completed'))

        isu = getIssue(bnds$채권CD[i])
        if (isu$발행형태 != '이표채') next

        prc = bnds[i, 3:7]
        trd = data.frame(매수일 = Sys.Date(),
                         매수가 = prc$현재가,
                         매도일 = isu$상환일,
                         매도가 = isu$상환율*10000)
        remain_days = tryCatch(dayCount(trd$매수일, trd$매도일, 2), error = function(e) e)
        if (inherits(remain_days, "error")) next
        trd[['잔존일수']] = remain_days
        tabs = list(issue = isu, price = prc, trade = trd)

        ind_tabs = tryCatch(yieldFB(tabs), error = function(e) e)
        if (inherits(ind_tabs, "error")) next
        ind_out = cbind(ind_tabs$issue, ind_tabs$price, ind_tabs$trade, ind_tabs$yield)
        if (nrow(outp) == 0) {outp = ind_out} else {outp = bind_rows(outp, ind_out)}
        if (i == nrow(bnds)) {cat(': Done\n')} else {cat('\014')}
        # Sys.sleep(0.5)
    }
    outp %>%
        mutate(잔존기간 = ifelse(잔존일수>=365.25, paste(round(잔존일수/365.25, 1), 'Y'),
                             ifelse(잔존일수>=90, paste(round(잔존일수/30, 0), 'M'), paste(잔존일수, 'D')))) %>%
        mutate(across(c(IRR, ARR, MRR, TRR, 표면금리), ~ formattable::percent(.x, 1))) %>%
        mutate(across(c(잔존일수, NPV, 현재가, 매도가, 거래량), ~ formattable::comma(.x, digits=0))) %>%
        rename(연수익률 = ARR, 월수익률 = MRR, 총수익률 = TRR, 상환가 = 매도가) %>%
        mutate(잔존년수 = round(as.numeric(잔존일수/365.25), 1)) %>%
        select('채권CD','채권명', 'CLASS', '옵션CD','신용GD','잔존년수','잔존일수','잔존기간','표면금리',
               'NPV', '현재가','상환가','거래량','연수익률', '월수익률', '총수익률') %>%
        arrange(desc(연수익률)) %>%
        filter(between(as.numeric(연수익률), minrt, maxrt)) %>%
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
#' tab = comparePlot()
#' tab
#'
comparePlot <- function(tab, sect='회사채', minrt = 0.00, maxrt = Inf, n = Inf) {
    if (missingArg(tab)) tab = compareTable(sect, minrt, maxrt, n)
    gg_viz =
        ggplot(dat, aes(x=신용GD,y=연수익률,Name=채권명,Period=잔존년수,
                        Yield=연수익률,Price=현재가,size=거래량,color=CLASS)) +
        geom_point(alpha=0.7) +
        scale_color_manual(values = c('aaa'='Firebrick', 'bbb'='orange', 'ccc'='gold', 'exact'='green', 'scrap'='grey70'))
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
    info = getIssuePrice(krcd, ...)
    tabs = yieldFB(info)

    if (nrow(tabs$invst)==0) {
        dat = cbind(tabs$issue, tabs$price, tabs$trade, tabs$yield)
    } else {
        dat = cbind(tabs$issue, tabs$price, tabs$trade, tabs$yield, tabs$invst %>% select(-c('상환일','민평가')))
    }
    tdt =
        dat %>%
        mutate(nday = dayCount(Sys.Date(), 상환일, 2),
               dsct = (민평가-Close)/Close) %>%
        transmute(발행일 = as.character.Date(발행일), ##채권CD,
                  발행형태,
                  CREDIT,
                  만기일 = as.character.Date(만기일),
                  Coupon = as.character(formattable::percent(표면금리,1)),
                  옵션CD,
                  상환일 = as.character.Date(상환일),
                  상환율 = as.character(formattable::percent(상환율,1)),
                  CLASS,
                  시세정보 = 'PRICE',
                  기준일 = as.character.Date(Date),
                  현재가 = as.character(formattable::comma(Close,0)),
                  잔존기간 = ifelse(nday >= 31, paste(round(nday/365.25*12),'개월'), paste(nday,'일')),
                  매수일 = as.character.Date(매수일),
                  민평가 = as.character(formattable::comma(민평가,0)),
                  거래량 = as.character(formattable::comma(거래량,0)),
                  매도일 = as.character.Date(매도일),
                  민평DSC = as.character(formattable::percent(dsct,1)),
                  거래금액 = as.character(formattable::comma(거래금액,0)),
                  수익분석 = 'YIELD',
                  순현가 = as.character(formattable::comma(NPV,0)),
                  내부ROR = as.character(formattable::percent(IRR,1)),
                  총수익률 = as.character(formattable::percent(TRR,1)),
                  할인율 = as.character(formattable::percent(NPV/Close-1,1)),
                  평균ROR = as.character(formattable::percent(ARR,1)),
                  월수익률 = as.character(formattable::percent(MRR,1)),
                  YIELD7 = 'YIELD7',
                  YIELD8 = 'YIELD8',
                  YIELD9 = 'YIELD9')

    if (nrow(tabs$invst)==1) {
        tin =
            dat %>%
            mutate(현수익금 = (평균가-매입가)*보유수량/10000,
                   현수익률 = (평균가-매입가)/매입가,
                   상환수익 = (매도가-매입가)*보유수량/10000,
                   상환ROR = (매도가-매입가)/매입가) %>%
            transmute(투자분석 = 'INVST',
                      매입가   = as.character(formattable::comma(매입가,0)),
                      보유수량 = as.character(formattable::comma(보유수량,0)),
                      매입금액 = as.character(formattable::comma(매입금액,0)),
                      평균가   = as.character(formattable::comma(평균가,0)),
                      현수익금 = as.character(formattable::comma(현수익금,0)),
                      현수익률 = as.character(formattable::percent(현수익률,1)),
                      상환가2  = as.character(formattable::comma(매도가,0)),
                      상환수익 = as.character(formattable::comma(상환수익,0)),
                      상환ROR  = as.character(formattable::percent(상환ROR,1)))
        tdt = cbind(tdt, tin)
    }

    # 결과출력 ____________________________________________________________
    keys = format(names(tdt))
    vals = format(unname(unlist(tdt[1,])), justify = 'right')

    cat('\n', black$bgYellow$bold('  ', str_pad(dat$채권명, 40, 'right')),'\n')

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

    if (nrow(tabs$invst)==1) {
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
        mutate(across(c(세전CF, 과세대상, 원천세, 거래비용, 세후CF), ~ formattable::comma(.x, digits=0)))
}
