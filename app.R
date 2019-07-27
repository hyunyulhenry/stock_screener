pkg = c('httr', 'rvest', 'stringr', 'readr', 'shiny',
        'dplyr', 'shinycssloaders', 'DT')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
  install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)

down_df = function() {
  # 최근 영업일 구하기
  url = 'https://finance.naver.com/sise/sise_deposit.nhn'
  
  biz_day = GET(url) %>%
    read_html(encoding = 'EUC-KR') %>%
    html_nodes(xpath =
                 '//*[@id="type_1"]/div/ul[2]/li/span') %>%
    html_text() %>%
    str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
    str_replace_all('\\.', '')
  
  # 산업별 현황 OTP 발급
  gen_otp_url =
    'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
  gen_otp_data = list(
    name = 'fileDown',
    filetype = 'csv',
    url = 'MKD/03/0303/03030103/mkd03030103',
    tp_cd = 'ALL',
    date = biz_day, # 최근영업일로 변경
    lang = 'ko',
    pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')
  otp = POST(gen_otp_url, query = gen_otp_data) %>%
    read_html() %>%
    html_text()
  
  # 산업별 현황 데이터 다운로드
  down_url = 'http://file.krx.co.kr/download.jspx'
  down_sector = POST(down_url, query = list(code = otp),
                     add_headers(referer = gen_otp_url)) %>%
    read_html() %>%
    html_text() %>%
    read_csv()
  
  ifelse(dir.exists('data'), FALSE, dir.create('data'))
  write.csv(down_sector, 'data/krx_sector.csv')
  
  # 개별종목 지표 OTP 발급
  gen_otp_url =
    'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
  gen_otp_data = list(
    name = 'fileDown',
    filetype = 'csv',
    url = "MKD/13/1302/13020401/mkd13020401",
    market_gubun = 'ALL',
    gubun = '1',
    schdate = biz_day, # 최근영업일로 변경
    pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")
  
  otp = POST(gen_otp_url, query = gen_otp_data) %>%
    read_html() %>%
    html_text()
  
  # 개별종목 지표 데이터 다운로드
  down_url = 'http://file.krx.co.kr/download.jspx'
  down_ind = POST(down_url, query = list(code = otp),
                  add_headers(referer = gen_otp_url)) %>%
    read_html() %>%
    html_text() %>%
    read_csv()
  
  KOR_ticker = merge(down_sector, down_ind,
                     by = intersect(names(down_sector),
                                    names(down_ind)),
                     all = FALSE
  )
  
  
  data = KOR_ticker %>%
    mutate(PER = parse_number(PER),
           PBR = parse_number(PBR),
           ROE = PBR / PER,
           ROE = round(ROE, 2),
           시총순위 = percent_rank(desc(`시가총액(원)`)),
           시총순위 = round(시총순위, 2)) %>%
    arrange(시총순위) %>%
    select(일자, 종목코드, 종목명, 종가, PER, PBR, 배당수익률, ROE, 시총순위)
}


# Define UI for app ----
ui = fluidPage(
  
  # App title ----
  titlePanel('Stock Screener'),
  
  # Sidebar to demonstrate various slider options ----
  sidebarPanel(
  
    # Input: Specification of range within  PER ----
    sliderInput("PER", "PER:",
                min = 0,
                max = 100,
                value = c(0,20),
                step = 0.01),
    
    # Input: Specification of range within PBR ----
    sliderInput("PBR", "PBR:",
                min = 0,
                max = 10,
                value = c(0, 2),
                step = 0.01),
    
    # Input: Specification of range within Div Yield ----
    sliderInput("DY", "배당수익률(%):",
                min = 0,
                max = 20,
                value = c(2, 5),
                step = 0.01),
    
    # Input: Specification of range within ROE ----
    sliderInput("ROE", "ROE:",
                min = 0,
                max = 2,
                value = c(0.1, 0.5),
                step = 0.01),
    
    # Input: Specification of range within Size ----
    sliderInput("SIZE", "SIZE(%):",
                min = 0,
                max = 1,
                value = c(0, 1),
                step = 0.01)
  ),
  
  mainPanel(
    dataTableOutput('screen_table') %>% withSpinner(color="#0dc5c1")
  )
)
   
# Define server logic to selected dataset ---- 
server = function(input, output) {  
  
  # Download Data ----
  data = down_df()
  
  # Filtered Table ----
  output$screen_table = DT::renderDataTable({
    data %>%
      filter(PER > input$PER[1] & PER < input$PER[2]) %>%
      filter(PBR > input$PBR[1] & PBR < input$PBR[2]) %>%
      filter(배당수익률 > input$DY[1] & 배당수익률 < input$DY[2]) %>%
      filter(ROE > input$ROE[1] & ROE < input$ROE[2]) %>%
      filter(시총순위 > input$SIZE[1] & 시총순위 < input$SIZE[2]) %>%
      DT::datatable(rownames= FALSE,
                    extensions = 'Buttons',
                    options = list(pageLength = 100,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                   ))
    })
}

# Create Shiny app ----
shinyApp(ui, server)
