#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   
   ## Sreality web crawling
   
   
   ################
   ## PARAMETERS ##
   ################
   
   ## parameters: explained in parameters.xlsx or in dataframes in parameters explanation section - line 140
   
   ## if parameter should be omitted, enter <- NULL
   
   ## just one number: MUST ENTER!!
   category_type_cb <- 1
   
   ##just one number: MUST ENTER!!
   category_main_cb <- 1
   
   ## can be vector, can be omitted
   category_sub_cb <- NULL
   
   ## can be vector, cen be omitted, i.e. c(11,12) meaning moravskoslezky, stredocesky
   locality_region_id <- 10
   
   ## can be vector, can be omitted, i.e. c(5003,5005,5009) meaning praha 3, 5, 9
   locality_district_id <- 5005
   
   
   #######################
   ## END OF PARAMETERS ##
   #######################
   
   
   
   ## libraries used
   
   require(curl) || install.packages('curl')
   require(jsonlite) || install.packages('jsonlite')
   require(dplyr) || install.packages('dplyr') 
   require(xlsx) || install.packages('xlsx') 
   
   library(curl)
   library(jsonlite)
   library(dplyr)
   library(xlsx)
   
   
   ## other possibly useful libraries
   
   #library(RCurl)
   #library(Rcrawler)
   #library(rvest)
   #library(scrapeR)
   #library(RSelenium)
   #library(XML)
   #library(rjson)
   #library(RJSONIO)
   #library(httr)
   #library(selectr)
   #library(ROAuth)
   #library(htmltidy)
   #library(tidyverse)
   
   
   
   ## functions
   
   build_url <- function(main, sub, type, district, region, page){
     url <- 'https://www.sreality.cz/api/cs/v2/estates?'
     
     url <- paste0(url, 'category_main_cb=', main)
     
     if (!is.null(sub)) url <- paste0(url, '&category_sub_cb=', paste0(sub, collapse = '%7C'))
     
     url <- paste0(url, '&category_type_cb=', type)
     
     if (!is.null(district)) url <- paste0(url, '&locality_district_id=', paste0(district, collapse = '%7C'))
     
     if (!is.null(region)) url <- paste0(url, '&locality_region_id=', paste0(region, collapse = '%7C'))
     
     tmstmp <- round(as.numeric(Sys.time())*1000)
     
     url <- paste0(url, '&page=', page, '&per_page=20&tms=',tmstmp)
   }
   
   
   get_basic_info <- function(page_content){
     RET <- list(isAuction = page_content$`_embedded`$estates$is_auction,
                 category_main_cb = page_content$`_embedded`$estates$seo$category_main_cb,
                 category_sub_cb = page_content$`_embedded`$estates$seo$category_sub_cb,
                 category_type_cb = page_content$`_embedded`$estates$seo$category_type_cb,
                 locality_url = page_content$`_embedded`$estates$seo$locality,
                 locality = page_content$`_embedded`$estates$locality,
                 url_ends = page_content$`_embedded`$estates$`_links`$self$href,
                 price = page_content$`_embedded`$estates$price,
                 price_name = page_content$`_embedded`$estates$price_czk$name,
                 #alt_price = page_content$`_embedded`$estates$price_czk$alt$value_raw,
                 #alt_price_unit = page_content$`_embedded`$estates$price_czk$alt$unit,
                 name = page_content$`_embedded`$estates$name,
                 lon = page_content$`_embedded`$estates$gps$lon,
                 lat = page_content$`_embedded`$estates$gps$lat) %>% as.data.frame()
     
     return(RET)
   }
   
   
   
   rebuild_url <- function(type, main, sub, locality_url, url_end_id){
     RET <- paste0('https://www.sreality.cz/detail/', type, '/', main, '/', sub, '/', locality_url, '/', url_end_id)
     return(RET)
   }
   
   
   
   
   ## basic info from search results
   
   page <- 1
   
   url <- build_url(category_main_cb, category_sub_cb, category_type_cb, locality_district_id, locality_region_id, page)
   
   page_content <- fromJSON(url)
   
   info <- get_basic_info(page_content)
   
   output_filename <- gsub(' ','_', iconv(page_content$title, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
   
   
   while (NROW(page_content$`_embedded`$estates) > 0){
     
     page <- page + 1
     
     url <- build_url(category_main_cb, category_sub_cb, category_type_cb, locality_district_id, locality_region_id, page)
     
     page_content <- fromJSON(url)
     
     this_info <- get_basic_info(page_content)
     
     info <- rbind(info, this_info)
     
     cat('.')
   }
   
   rm(this_info, page_content, page, url)
   
   info <- unique(info)
   
   
   ## parameters explanation and mapping to word equivalents
   
   category_type_cb_meaning <- as.data.frame(
     cbind(category_type_cb = c(1:3),
           Typ = c('prodej', 'pronajem', 'drazby')))
   
   category_main_cb_meaning <- as.data.frame(
     cbind(category_main_cb = c(1:5),
           Kategorie = c('byty', 'domy', 'pozemky', 'komercni', 'ostatni')))
   
   category_sub_cb_meaning <- as.data.frame(
     rbind(
       cbind(category_sub_cb = c(2:12,16),
             Podkategorie = c('1+kk', '1+1', '2+kk', '2+1', '3+kk', '3+1', '4+kk', '4+1', '5+kk', '5+1', '6 a vice', 'atypicky')),
       cbind(category_sub_cb = c(37,39,43,33,40,44,35),
             Podkategorie = c('rodinny', 'vila', 'chalupa', 'chata', 'na klic', 'zemedelska usedlost', 'pamatka/jine')),
       cbind(category_sub_cb = c(18:24,46,48),
             Podkategorie = c('komercni', 'bydleni', 'pole', 'lesy', 'louky', 'zahrady', 'ostatni', 'rybniky', 'sady/vinice')),
       cbind(category_sub_cb = c(25:31,38,32),
             Podkategorie = c('kancelare', 'sklady', 'vyroba', 'obchodni prostory', 'ubytovani', 'restaurace', 'zemedelsky', 'cinzovni dum', 'ostatni')),
       cbind(category_sub_cb = c(34,52,53,50,51,36),
             Podkategorie = c('garaz', 'garazove stani', 'mobilheim', 'vinny sklep', 'pudni prostor', 'ostatni'))))
   
   locality_region_id_meaning <- as.data.frame(
     cbind(locality_region_id = c(1:14),
           Kraj = c('jihocesky', 'plzensky', 'karlovarsaky', 'ustecky', 'liberecky', 'kralovehradecky', 'pardubicky',
                    'olomoucky', 'zlinsky', 'praha', 'stredocesky', 'moravskoslezky', 'vysocina', 'jihomoravsky')))
   
   locality_district_id_meaning <- as.data.frame(
     rbind(
       cbind(locality_district_id = c(1:7),
             Okres = c('ceske budejovice', 'cesky krumlov', 'jindrichuv hradec', 'pisek', 'prachatice', 'strakonice', 'tabor')),
       cbind(locality_district_id = c(8,11,13,12,14,15,17),
             Okres = c('domazlice', 'klatovy', 'plzen-jih', 'plzen-mesto', 'plzen-sever', 'rokycany', 'tachov')),
       cbind(locality_district_id = c(9,10,16),
             Okres = c('cheb', 'karlovy vary', 'sokolov')),
       cbind(locality_district_id = c(20,19,23:27),
             Okres = c('chomutov', 'decin', 'litomerice', 'louny', 'most', 'teplice', 'usti nad labem')),
       cbind(locality_district_id = c(18,21,22,34),
             Okres = c('ceska lipa', 'jablonec nad nisou', 'liberec', 'semily')),
       cbind(locality_district_id = c(28,30,31,33,36),
             Okres = c('hradec kralove', 'jicin', 'nachod', 'rychnov nad kneznou', 'trutnov')),
       cbind(locality_district_id = c(29,32,35,37),
             Okres = c('chrudim', 'pardubice', 'svitavy', 'usti nad orlici')),
       cbind(locality_district_id = c(46,42,43,40,44),
             Okres = c('jesenik', 'olomouc', 'prerov', 'prostejov', 'sumperk')),
       cbind(locality_district_id = c(39,41,45,38),
             Okres = c('kromeriz', 'uherske hradiste', 'vsetin', 'zlin')),
       cbind(locality_district_id = c(5001:5010),
             Okres = c('praha 1', 'praha 2', 'praha 3', 'praha 4', 'praha 5', 'praha 6', 'praha 7', 'praha 8', 'praha 9', 'praha 10')),
       cbind(locality_district_id = c(48:52, 54,53,55:59),
             Okres = c('benesov', 'beroun', 'kladno', 'kolin', 'kutna hora', 'melnik', 'mlada boleslav', 'nymburk', 'praha-vychod', 'praha-zapad', 'pribram', 'rakovnik')),
       cbind(locality_district_id = c(60:65),
             Okres = c('bruntal', 'frydek-mistek', 'karvina', 'novy jicin', 'opava', 'ostrava-mesto')),
       cbind(locality_district_id = c(66:70),
             Okres = c('havlickuv brod', 'jihlava', 'pelhrimov', 'trebic', 'zdar nad sazavou')),
       cbind(locality_district_id = c(71,74,72,73,75:77),
             Okres = c('blansko', 'breclav', 'brno-mesto', 'brno-venkov', 'hodonin', 'vyskov', 'znojmo'))))
   
   # parametry <- createWorkbook()
   # 
   # typ      <- createSheet(wb = parametry, sheetName = "Typ inzeratu")
   # main     <- createSheet(wb = parametry, sheetName = "Kategorie")
   # sub      <- createSheet(wb = parametry, sheetName = "Podkategorie")
   # region   <- createSheet(wb = parametry, sheetName = "Kraj")
   # district <- createSheet(wb = parametry, sheetName = "Okres")
   # 
   # setColumnWidth(typ,      c(1,2), 25)
   # setColumnWidth(main,     c(1,2), 25)
   # setColumnWidth(sub,      c(1,2), 25)
   # setColumnWidth(region,   c(1,2), 25)
   # setColumnWidth(district, c(1,2), 25)
   # 
   # cs <- CellStyle(parametry) +
   #   Font(parametry, heightInPoints=14, isBold = TRUE, color = 'blue') +
   #   Border(position = c("BOTTOM", "LEFT", "TOP", "RIGHT")) +
   #   Fill(backgroundColor="lavender", foregroundColor="lavender", pattern="SOLID_FOREGROUND") +
   #   Alignment(h="ALIGN_RIGHT")
   # 
   # addDataFrame(x = category_type_cb_meaning,     sheet = typ,      row.names = FALSE, colnamesStyle = cs)
   # addDataFrame(x = category_main_cb_meaning,     sheet = main,     row.names = FALSE, colnamesStyle = cs)
   # addDataFrame(x = category_sub_cb_meaning,      sheet = sub,      row.names = FALSE, colnamesStyle = cs)
   # addDataFrame(x = locality_region_id_meaning,   sheet = region,   row.names = FALSE, colnamesStyle = cs)
   # addDataFrame(x = locality_district_id_meaning, sheet = district, row.names = FALSE, colnamesStyle = cs)
   # 
   # saveWorkbook(parametry, "Parametry_sreality.xlsx")
   # 
   
   
   info$type <- as.character(category_type_cb_meaning$Typ[match(info$category_type_cb, category_type_cb_meaning$category_type_cb)])
   info$main <- as.character(category_main_cb_meaning$Kategorie[match(info$category_main_cb, category_main_cb_meaning$category_main_cb)])
   info$sub <- as.character(category_sub_cb_meaning$Podkategorie[match(info$category_sub_cb, category_sub_cb_meaning$category_sub_cb)])
   
   info <- info %>% mutate(aukce = case_when(isAuction ~ "ano",
                                             !isAuction ~ "ne"))
   
   info$url_end_id <- gsub("/cs/v2/estates/", "", info$url_ends)
   
   info$ad_url <- with(info, rebuild_url(type, main, sub, locality_url, url_end_id))
   
   rm(category_sub_cb_meaning, category_main_cb_meaning, category_type_cb_meaning, locality_region_id_meaning, locality_district_id_meaning)
   
   
   ## info from individual land advertisements
   
   detail.info <- data.frame(url_ends = info$url_ends, url_end_id = info$url_end_id)
   
   url_short_base <- 'https://www.sreality.cz/api'
   
   
   for (i in 1:NROW(detail.info)){
     
     url <- paste0(url_short_base, detail.info$url_ends[i])
     
     tryCatch({page_content <- fromJSON(url)
     
     detail.info$text[i] <- page_content$text$value
     
     detail.info$seller_name[i] <- ifelse((!is.null(page_content$`_embedded`$seller$`_embedded`$premise$name)),
                                          page_content$`_embedded`$seller$`_embedded`$premise$name, NA)
     
     detail.info$seller_www[i] <- ifelse((!is.null(page_content$`_embedded`$seller$`_embedded`$premise$www)),
                                         page_content$`_embedded`$seller$`_embedded`$premise$www, NA)
     
     detail.info$seller_ico[i] <- ifelse((!is.null(page_content$`_embedded`$seller$`_embedded`$premise$ico)),
                                         page_content$`_embedded`$seller$`_embedded`$premise$ico, NA)
     
     detail.info$okres_id[i] <- page_content$locality_district_id
     
     detail.info$locality[i] <- page_content$locality$value
     
     detail.info$alt_price[i] <- ifelse((!is.null(page_content$price_czk$alt)),
                                        page_content$price_czk$alt$value_raw, NA)
     
     detail.info$alt_price_unit[i] <- ifelse((!is.null(page_content$price_czk$alt)),
                                             page_content$price_czk$alt$unit, NA)
     
     items <- as.data.frame(page_content$items)
     
     detail.info$price_note[i] <- ifelse((!is.null(items[which(items$name %in% c("Celková cena", "Cena")),]) 
                                          && nrow(items[which(items$name %in% c("Celková cena", "Cena")),]) > 0),
                                         paste0(unlist(items[which(items$name %in% c("Celková cena", "Cena")),]$notes), collapse = ", "), NA)
     
     detail.info$price_note2[i] <- ifelse((!is.null(items[which(items$name == "Poznámka k cenì"),]) 
                                           && nrow(items[which(items$name == "Poznámka k cenì"),]) > 0),
                                          paste0(unlist(items[which(items$name == "Poznámka k cenì"),]$value), collapse = ", "), NA)
     
     detail.info$currency[i] <- ifelse((!is.null(items[which(items$name %in% c("Celková cena", "Cena")),]) 
                                        && nrow(items[which(items$name %in% c("Celková cena", "Cena")),]) > 0),
                                       paste0(unlist(items[which(items$name %in% c("Celková cena", "Cena")),]$currency), collapse = ", "), NA)
     
     detail.info$id[i] <- ifelse((!is.null(items[which(items$name %in% c("ID zakázky", "ID")),]) 
                                  && nrow(items[which(items$name %in% c("ID zakázky", "ID")),]) > 0),
                                 unlist(items[which(items$name %in% c("ID zakázky", "ID")),]$value), NA)
     
     detail.info$aktualizace[i] <- ifelse((!is.null(items[which(items$name == "Aktualizace"),]) 
                                           && nrow(items[which(items$name == "Aktualizace"),]) > 0),
                                          ifelse(unlist(items[which(items$name == "Aktualizace"),]$value) == "Dnes",
                                                 format(Sys.Date(), "%d.%m.%Y"), 
                                                 ifelse(unlist(items[which(items$name == "Aktualizace"),]$value) == "Vèera",
                                                        format(Sys.Date()-1, "%d.%m.%Y"),
                                                        unlist(items[which(items$name == "Aktualizace"),]$value))
                                          ),
                                          NA)
     
     detail.info$stavba[i] <- ifelse((!is.null(items[which(items$name == "Stavba"),]) 
                                      && nrow(items[which(items$name == "Stavba"),]) > 0),
                                     paste0(items[which(items$name == "Stavba"),]$value, collapse = ", "), NA)
     
     detail.info$stav_objektu[i] <- ifelse((!is.null(items[which(items$name == "Stav objektu"),]) 
                                            && nrow(items[which(items$name == "Stav objektu"),]) > 0),
                                           paste0(items[which(items$name == "Stav objektu"),]$value, collapse = ", "), NA)
     
     detail.info$poloha_domu[i] <- ifelse((!is.null(items[which(items$name == "Poloha domu"),]) 
                                           && nrow(items[which(items$name == "Poloha domu"),]) > 0),
                                          paste0(items[which(items$name == "Poloha domu"),]$value, collapse = ", "), NA)
     
     detail.info$vlastnictvi[i] <- ifelse((!is.null(items[which(items$name == "Vlastnictví"),]) 
                                           && nrow(items[which(items$name == "Vlastnictví"),]) > 0),
                                          paste0(items[which(items$name == "Vlastnictví"),]$value, collapse = ", "), NA)
     
     detail.info$typ_domu[i] <- ifelse((!is.null(items[which(items$name == "Typ domu"),]) 
                                        && nrow(items[which(items$name == "Typ domu"),]) > 0),
                                       paste0(items[which(items$name == "Typ domu"),]$value, collapse = ", "), NA)
     
     detail.info$podlazi[i] <- ifelse((!is.null(items[which(items$name == "Podlaží"),]) 
                                       && nrow(items[which(items$name == "Podlaží"),]) > 0),
                                      paste0(items[which(items$name == "Podlaží"),]$value, collapse = ", "), NA)
     
     detail.info$zastavena_plocha[i] <- ifelse((!is.null(items[which(items$name == "Plocha zastavìná"),]) 
                                                && nrow(items[which(items$name == "Plocha zastavìná"),]) > 0),
                                               paste0(items[which(items$name == "Plocha zastavìná"),]$value, collapse = ", "), NA)
     
     detail.info$uzitna_plocha[i] <- ifelse((!is.null(items[which(items$name == "Užitná plocha"),]) 
                                             && nrow(items[which(items$name == "Užitná plocha"),]) > 0),
                                            paste0(items[which(items$name == "Užitná plocha"),]$value, collapse = ", "), NA)
     
     detail.info$podlahova_plocha[i] <- ifelse((!is.null(items[which(items$name == "Plocha podlahová"),]) 
                                                && nrow(items[which(items$name == "Plocha podlahová"),]) > 0),
                                               paste0(items[which(items$name == "Plocha podlahová"),]$value, collapse = ", "), NA)
     
     detail.info$plocha_zahrady[i] <- ifelse((!is.null(items[which(items$name == "Plocha zahrady"),]) 
                                              && nrow(items[which(items$name == "Plocha zahrady"),]) > 0),
                                             paste0(items[which(items$name == "Plocha zahrady"),]$value, collapse = ", "), NA)
     
     detail.info$parkovani[i] <- ifelse((!is.null(items[which(items$name == "Parkování"),]) 
                                         && nrow(items[which(items$name == "Parkování"),]) > 0),
                                        paste0(items[which(items$name == "Parkování"),]$value, collapse = ", "), NA)
     
     detail.info$garaz[i] <- ifelse((!is.null(items[which(items$name == "Garáž"),]) 
                                     && nrow(items[which(items$name == "Garáž"),]) > 0),
                                    paste0(items[which(items$name == "Garáž"),]$value, collapse = ", "), NA)
     
     detail.info$sklep[i] <- ifelse((!is.null(items[which(items$name == "Sklep"),]) 
                                     && nrow(items[which(items$name == "Sklep"),]) > 0),
                                    paste0(items[which(items$name == "Sklep"),]$value, collapse = ", "), NA)
     
     detail.info$balkon[i] <- ifelse((!is.null(items[which(items$name == "Balkón"),]) 
                                      && nrow(items[which(items$name == "Balkón"),]) > 0),
                                     paste0(items[which(items$name == "Balkón"),]$value, collapse = ", "), NA)
     
     detail.info$vytah[i] <- ifelse((!is.null(items[which(items$name == "Výtah"),]) 
                                     && nrow(items[which(items$name == "Výtah"),]) > 0),
                                    paste0(items[which(items$name == "Výtah"),]$value, collapse = ", "), NA)
     
     detail.info$energeticka_narocnost[i] <- ifelse((!is.null(items[which(items$name == "Energetická nároènost budovy"),]) 
                                                     && nrow(items[which(items$name == "Energetická nároènost budovy"),]) > 0),
                                                    paste0(items[which(items$name == "Energetická nároènost budovy"),]$value, collapse = ", "), NA)
     
     detail.info$rok_kolaudace[i] <- ifelse((!is.null(items[which(items$name == "Rok kolaudace"),]) 
                                             && nrow(items[which(items$name == "Rok kolaudace"),]) > 0),
                                            paste0(items[which(items$name == "Rok kolaudace"),]$value, collapse = ", "), NA)
     
     detail.info$bezbarierovy[i] <- ifelse((!is.null(items[which(items$name == "Bezbariérový"),]) 
                                            && nrow(items[which(items$name == "Bezbariérový"),]) > 0),
                                           paste0(items[which(items$name == "Bezbariérový"),]$value, collapse = ", "), NA)
     
     detail.info$vybaveni[i] <- ifelse((!is.null(items[which(items$name == "Vybavení"),]) 
                                        && nrow(items[which(items$name == "Vybavení"),]) > 0),
                                       paste0(items[which(items$name == "Vybavení"),]$value, collapse = ", "), NA)
     
     detail.info$umisteni[i] <- ifelse((!is.null(items[which(items$name == "Umístìní objektu"),]) 
                                        && nrow(items[which(items$name == "Umístìní objektu"),]) > 0),
                                       paste0(items[which(items$name == "Umístìní objektu"),]$value, collapse = ", "), NA)
     
     detail.info$doprava[i] <- ifelse((!is.null(items[which(items$name == "Doprava"),]) 
                                       && nrow(items[which(items$name == "Doprava"),]) > 0),
                                      paste0(as.data.frame(items[which(items$name == "Doprava"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$komunikace[i] <- ifelse((!is.null(items[which(items$name == "Komunikace"),]) 
                                          && nrow(items[which(items$name == "Komunikace"),]) > 0),
                                         paste0(as.data.frame(items[which(items$name == "Komunikace"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$elektro[i] <- ifelse((!is.null(items[which(items$name == "Elektøina"),]) 
                                       && nrow(items[which(items$name == "Elektøina"),]) > 0),
                                      paste0(as.data.frame(items[which(items$name == "Elektøina"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$voda[i] <- ifelse((!is.null(items[which(items$name == "Voda"),]) 
                                    && nrow(items[which(items$name == "Voda"),]) > 0),
                                   paste0(as.data.frame(items[which(items$name == "Voda"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$topeni[i] <- ifelse((!is.null(items[which(items$name == "Topení"),]) 
                                      && nrow(items[which(items$name == "Topení"),]) > 0),
                                     paste0(as.data.frame(items[which(items$name == "Topení"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$plyn[i] <- ifelse((!is.null(items[which(items$name == "Plyn"),]) 
                                    && nrow(items[which(items$name == "Plyn"),]) > 0),
                                   paste0(as.data.frame(items[which(items$name == "Plyn"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$odpad[i] <- ifelse((!is.null(items[which(items$name == "Odpad"),]) 
                                     && nrow(items[which(items$name == "Odpad"),]) > 0),
                                    paste0(as.data.frame(items[which(items$name == "Odpad"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$telekomunikace[i] <- ifelse((!is.null(items[which(items$name == "Telekomunikace"),]) 
                                              && nrow(items[which(items$name == "Telekomunikace"),]) > 0),
                                             paste0(as.data.frame(items[which(items$name == "Telekomunikace"),]$value)[,2], collapse = ", "), NA)
     
     detail.info$nastehovani[i] <- ifelse((!is.null(items[which(items$name == "Datum nastìhování"),]) 
                                           && nrow(items[which(items$name == "Datum nastìhování"),]) > 0),
                                          unlist(items[which(items$name == "Datum nastìhování"),]$value), NA)
     
     detail.info$area[i] <- ifelse((!is.null(items[which(items$name %in% c("Plocha pozemku", "Plocha", "Rozloha")),]) 
                                    && nrow(items[which(items$name %in% c("Plocha pozemku", "Plocha", "Rozloha")),]) > 0),
                                   unlist(items[which(items$name %in% c("Plocha pozemku", "Plocha", "Rozloha")),]$value), NA)
     
     detail.info$area_unit[i] <- ifelse((!is.null(items[which(items$name %in% c("Plocha pozemku", "Plocha", "Rozloha")),]) 
                                         && nrow(items[which(items$name %in% c("Plocha pozemku", "Plocha", "Rozloha")),]) > 0),
                                        unlist(items[which(items$name %in% c("Plocha pozemku", "Plocha", "Rozloha")),]$unit), NA)
     
     rm(items)
     
     cat('.')},
     
     warning = function(w){print(paste0(' Warning: Page ', info$ad_url[i], ' not found! '))},
     error = function(e){cat(paste0(' Error: Page ', info$ad_url[i], ' not found! '))})
     
   }
   
   rm(url, url_short_base)
   
   
   locality_okres <- data.frame(loc = as.character(detail.info$locality),
                                split = grepl(', okres ', as.character(detail.info$locality)),
                                ulice = NA,
                                obec = NA,
                                okres = NA)
   
   ## split locality names containing the word okres
   subset_okres <- as.character(locality_okres$loc[which(locality_okres$split)])
   
   locality_split <- strsplit(subset_okres, rep(", okres ", times = NROW(subset_okres)))
   
   locality_okres$obec[which(locality_okres$split)]  <- t(as.data.frame(locality_split))[,1]
   locality_okres$okres[which(locality_okres$split)] <- t(as.data.frame(locality_split))[,2]
   
   ## split locality names not containing the word okres
   subset_not_okres <- as.character(locality_okres$loc[which(!locality_okres$split)])
   
   locality_split <- strsplit(subset_not_okres, rep(", ", times = NROW(subset_not_okres)))
   
   locality_okres$ulice[which(!locality_okres$split)] <- t(as.data.frame(locality_split))[,1]
   locality_okres$obec[which(!locality_okres$split)]  <- t(as.data.frame(locality_split))[,2]
   
   
   rm(locality_split, subset_okres, subset_not_okres)
   
   
   ## prepare output dataframe
   
   out.info <- data.frame(ID = detail.info$id,
                          Typ = info$type,
                          Kategorie = info$main,
                          Podkategorie = info$sub,
                          Aktualizace = detail.info$aktualizace,
                          Nazev = info$name,
                          Text = detail.info$text,
                          Lokalita = info$locality,
                          Okres_ID = detail.info$okres_id,
                          Okres = locality_okres$okres,
                          Obec = locality_okres$obec,
                          Ulice = locality_okres$ulice,
                          Cena = info$price,
                          Mena = detail.info$currency,
                          Cena_info = info$price_name,
                          Poznamka = detail.info$price_note,
                          Poznamka2 = detail.info$price_note2,
                          Jedn_cena = detail.info$alt_price,
                          Jednotka = detail.info$alt_price_unit,
                          Plocha = detail.info$area,
                          Jedn_plocha = detail.info$area_unit,
                          Aukce = info$aukce,
                          Web = info$ad_url,
                          Lon = info$lon,
                          Lat = info$lat,
                          Stavba = detail.info$stavba,
                          Stav_objektu = detail.info$stav_objektu,
                          Poloha_domu = detail.info$poloha_domu,
                          Vlastnictvi = detail.info$vlastnictvi,
                          Typ_domu = detail.info$typ_domu,
                          Podlazi = detail.info$podlazi,
                          Zastavena_plocha = detail.info$zastavena_plocha,
                          Uzitna_plocha = detail.info$uzitna_plocha,
                          Podlahova_plocha = detail.info$podlahova_plocha,
                          Plocha_zahrady = detail.info$plocha_zahrady,
                          Parkovani = detail.info$parkovani,
                          Garaz = detail.info$garaz,
                          Sklep = detail.info$sklep,
                          Vytah = detail.info$vytah,
                          Balkon = detail.info$balkon,
                          Energeticka_narocnost = detail.info$energeticka_narocnost,
                          Rok_kolaudace = detail.info$rok_kolaudace,
                          Bezbarierovy = detail.info$bezbarierovy,
                          Vybaveni = detail.info$vybaveni,
                          Telekomunikace = detail.info$telekomunikace,
                          Doprava = detail.info$doprava,
                          Komunikace = detail.info$komunikace,
                          Voda = detail.info$voda,
                          Plyn = detail.info$plyn,
                          Elektro = detail.info$elektro,
                          Topeni = detail.info$topeni,
                          Umisteni = detail.info$umisteni,
                          Odpad = detail.info$odpad,
                          Nastehovani = detail.info$nastehovani,
                          Prodejce_jmeno = detail.info$seller_name,
                          Prodejce_web = detail.info$seller_www,
                          Prodejce_ICO = detail.info$seller_ico)
   
   rm(locality_okres, info, detail.info)
   
   ## write output in xlsx
   
   output <- createWorkbook()
   
   sreality <- createSheet(wb = output, sheetName = "Sreality data")
   
   header <- CellStyle(output) +
     Font(output, heightInPoints=12, isBold = TRUE, color = 'blue') +
     Border(position = c("BOTTOM", "LEFT", "TOP", "RIGHT")) +
     Fill(backgroundColor="lavender", foregroundColor="lavender", pattern="SOLID_FOREGROUND") +
     Alignment(h="ALIGN_RIGHT")
   
   addDataFrame(x = out.info, sheet = sreality, row.names = FALSE, colnamesStyle = header)
   
   saveWorkbook(output, paste0(output_filename,'.xlsx'))
   
   rm(out.info, output, output_filename)
   
####################################################   
#          MODEL  random forest                    #
####################################################
   
   
   
   #Features 
   df <- out.info[which(out.info$Typ == 'prodej'),]
   df$Uzitna_plocha <- as.numeric(df$Uzitna_plocha)
   df$Stav_objektu <- as.factor(df$Stav_objektu)
   df$Vlastnictvi <- as.factor(df$Vlastnictvi)
   df$Obec <- as.factor(df$Obec)
   df$Podlazi <- as.factor(substring(df$Podlazi,1,1))
   df$Zahrada <- as.numeric(ifelse( is.na(df$Plocha_zahrady),0, df$Plocha_zahrady))
   df$Podkategorie <- as.factor(df$Podkategorie)
   df$Garaz <- ifelse(is.na(out.info$Garaz), 0,1)  
   
   #Train/Test
   sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
   df_train <- df[sample, ]
   df_test  <- df[-sample, ]
   
   #Formula
   formula <- as.formula('Cena~ Uzitna_plocha +Stav_objektu + Obec + Podlazi + Zahrada + Podkategorie + Garaz' )
   

   library(randomForest)
   
   randomForest <- randomForest(formula, df_train)
   df_test$predicted_CENA <- predict(randomForest, df_test)
   plot(df_test$Cena, df_test$predicted_CENA)
   
   
   df$predicted_CENA <- predict(randomForest, df)
   plot(df$Cena, df$predicted_CENA)
   
   
   plot(randomForest)
   
   
   df_my <- data.frame(
     Cena <- 4730000,
     Uzitna_plocha <- 58,
     Stav_objektu <- as.factor('Velmi dobrý'),
     Obec <- as.factor('Praha 9 - èást obce Vysoèany'), 
     Podlazi <- 2,
     Zahrada <- 0,
     Podkategorie <- as.factor('3+kk'),
     Garaz <-1
   )
   
   
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

