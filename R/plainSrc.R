####This File reprsents a package used for Index Management

### Overall approach of this package ###
# In order to avoid a static hard coding of the index members
# we use a web-based approach based on finanzen.de from there we
# get the index memebers for which we offer the data.

#import libariers
#library(tidyverse)
#library(rvest)
#library(jsonlite)


yahoo.get_members <-function(indexname) {
  baseURL<-paste0("https://finance.yahoo.com/quote/%5E",indexname,"/components?p=%5E",indexname)
  cat("\nGetting data from ", baseURL,"\n")
  page<-read_html(baseURL)
  result<-as_tibble((html_nodes(page, "div #Main table") %>% html_table())[[1]])
  result<-result %>% rename(symbol=Symbol,volume="Volume",company="Company Name") %>%
    mutate(volume=as.numeric(gsub(",","",volume))) %>%
    select(symbol,company,volume)
  result<-result%>% mutate(weight=volume/sum(volume))%>%
    select(symbol,company,weight)
  return (result)
}


yahoo.get_currency<-function(shares) {
  currencies<-c()
  for(share in shares){
    baseURL<-paste0("https://finance.yahoo.com/quote/",share,"?p=",share)
    page<-read_html(baseURL)
    #cat(share)
    currency_string<-html_nodes(page, "span[data-reactid=\"9\"]")[1] %>% html_text()
    currencies<-c(currencies,(substr(currency_string,nchar(currency_string)-3+1,nchar(currency_string))))
  }
  return (currencies)
}


yahoo.get_sector<-function(shares) {
  sectors<-c()
  for(share in shares){
    baseURL<-paste0("https://finance.yahoo.com/quote/",share,"/profile?p=",share)
    page<-read_html(baseURL)
    #cat(share)
    found_nodes<- html_nodes(page,"p[class=\"D(ib) Va(t)\"] span")
    if(length(found_nodes)==0){
      sectors<-c(sectors,NA)
    }else{
      sectors<-c(sectors,found_nodes[2] %>% html_text())
    }

  }
  return (sectors)
}


get_identifiers<-function(shares){
  result<-c()
  for(share in shares){
    baseURL<-paste0("https://stockmarketmba.com/analyze.php?s=",share)
    page<-read_html(baseURL)
    page_section<-html_nodes(page,"div.row.clearfix") %>% html_text()

    sedol_ident<-str_match(page_section,".*Sedol:\\s*([A-Za-z|0-9]*)\t.*")[,2]
    cusip_ident<-str_match(page_section,".*CUSIP:\\s*([A-Za-z|0-9]*)\t.*")[,2]

    if (length(sedol_ident)==0 || nchar(sedol_ident)==0 ) sedol_ident<-NA
    if (length(cusip_ident)==0 || nchar(cusip_ident)==0 ) cusip_ident<-NA

    result<-rbind(result,c(sedol_ident, cusip_ident))
  }

  return (result)
}

# get_identifiers(c("HLAH"))
# baseURL<-paste0("https://stockmarketmba.com/analyze.php?s=","HLAH")
# page<-read_html(baseURL)
# page_section<-html_nodes(page,"div.row.clearfix") %>% html_text()
# sedol_ident<-str_match(page_section,".*Sedol:\\s*([A-Za-z|0-9]*)\t.*")[,2]
# cusip_ident<-str_match(page_section,".*CUSIP:\\s*([A-Za-z|0-9]*)\t.*")[,2]
# length(cusip_ident)
# if (is.null(sedol_ident) ) sedol_ident<-NA
# if (is.null(cusip_ident)) cusip_ident<-NA



yahoo.get_data<-function(index){
  res<-yahoo.get_members(index)


  yahoo_data<-res %>% mutate(local_currency=yahoo.get_currency(symbol)) %>%
    mutate(sector=yahoo.get_sector(symbol))


  idents<-get_identifiers(yahoo_data$symbol)
  colnames(idents)<-c("identifier","sedol")

  yahoo_data<-
    yahoo_data %>%  add_column(as_tibble(idents))

  yahoo_data<-
    yahoo_data %>%
    mutate(shares_held=NA) %>%
    relocate(local_currency, .after=last_col()) %>%
    relocate(identifier, .after=company) %>%
    relocate(sedol, .after=identifier)

  return(yahoo_data)
}



fcs.get_data<-function(index_name,country_name){
  cat("ATTENTION: you are using fcsapi  with an access key. If you use that method too often, the the contingent might expire!")
  url <- paste0('https://fcsapi.com/api-v3/stock/indices?country=',country_name,'&access_key=dMFxnj34HWTgnCYxFCnW')
  # read url and convert to data.frame
  indexes <- fromJSON(txt=url)
  idx_id<-indexes$response[indexes$response$index_name==index_name,] %>% pull(index_id)

  url <- paste0('https://fcsapi.com/api-v3/stock/list?indices_id=',idx_id,'&access_key=dMFxnj34HWTgnCYxFCnW')

  # read url and convert to data.frame
  members <- fromJSON(txt=url)$response

  return(
  members %>% as_tibble %>% rename(symbol=short_name,company=name) %>%
    mutate(weight=NA, identifier=NA,sedol=NA,shares_held=NA,local_currency=ccy) %>%
    select(symbol,company,identifier,sedol,weight,sector,shares_held,local_currency)
  )
}


#' Get all members of an index
#'
#' gets the the members of a given index such as ATX. The library does not
#' limit the number of indices as long as the used data sources are supporting
#' them
#'
#' the method offers a source parameter to determine which
#' type of src should be used. The default one is yahoo.
#' Thereby, the members are taken from yahoo and the identifiers/sedol
#' are taken from stockmarketmba.com
#'
#' The second type of implemented source is fcs. Here, only fcs is used
#' as this data source  uses symbol names that are incompatible with
#' stockmarketmba.com. So no identifier and sedol are provided so far for that
#' source. Be careful: fcs uses an open API key which might be overused!!!
#'
#' Yahoo and fcs use completely different interfaces: for yahoo we were using
#' the data from the website while for fcs we were using the API
#'
#' stockmarketmba.com doesn't provide identifiers for all shares and symbols
#' so it might be `NA`
#'
#' Attention: fcs and yahoo use DIFFERENT symbols
#'
#' AS DIFFERENT KIND OF SOURCES ARE USED, THE QUERIES ARE PRETTY SLOW
#' @param index_name The name of the Index
#' @param src The source that should be use (Yahoo or fcs)
#' @param country The country of the specified index (necessary if fcs is used as source)
#' @import dplyr
#' @import rvest
#' @import jsonlite
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' get_index("DAX","fcs","germany")
#' get_index("ATX")
#' get_index("GDAXI") #identifiers are at least partly available
#' get_index("SOMA")
#' get_index("IXIC")
#' get_index("OMX")

get_index<-function(index_name,src="yahoo",country="germany"){
  if(src=="yahoo"){
    return (yahoo.get_data(index_name))
  }else if(src=="fcs"){
    return (fcs.get_data(index_name,country))
  }else{
    cat("The src you specified is not supported yet!")
  }
}

## debugging code sections


# fcs.get_data("DAX","germany")
#
#
# yahoo.get_data("ATX")
# yahoo.get_data("GDAXI")
# yahoo.get_data("OMX") #schwedischer leitindex
# yahoo.get_data("IBEX")
# yahoo.get_data("IXIC")
# yahoo.get_data("SOMA")
#
#
#
#
#
# get_identifiers(c("UNH","FNKO"))
#
# res<-yahoo.get_members("IXIC")
# yahoo_data<-res %>% mutate_at(vars(symbol),sector=yahoo.get_sector)
#
#
#
#
# get_identifiers<-function(shares){
#   result<-c()
#   for(share in shares){
#
#     result<-rbind(result,c("2", "2"))
#   }
#
#   return (result)
# }
#
#
# res
#
# #res<-
# res %>%  mutate(function(symbol) {
#   tmp<-get_identifiers(symbol)
#   return (list(identifier=tmp[,1],sedal=tmp[,2]))
# }
# )
#
#
#
# res %>%  add_column(get_identifiers(res$symbol))
#
# tmp<-get_identifiers(res$symbol)
# list(identifier=tmp[,1],sedal=tmp[,2])
#
# res %>%  names %>% length
# res %>% rename("sdf"=`[,2]`)
#
# length(html_nodes(page,"p[class=\"D(ib) Va(t)\"] span"))
#
#
# yahoo_data %>%
#   mutate(identifier=NA) %>%
#   mutate(sedol=NA) %>%
#   mutate(shares_held=NA) %>%
#   relocate(local_currency, .after=last_col()) %>%
#   relocate(identifier, .after=company) %>%
#   relocate(sedol, .after=identifier)
#
#
# #yahoo.get_members("GDAXI")
#
#
# #yahoo.get_currency("MMK.VI")
#
# #yahoo.get_currency((res %>% pull(symbol))[1:2])
#
#
# tq_get("FNKO")
# tq_get
#
# page<-read_html(baseURL)
#
#
# tq_index("Dow")
# tq_get("GS")
#
#
#
#
#
#
#
#
#
#
# yahoo_data<-res %>% mutate(local_currency=yahoo.get_currency(symbol)) %>%
#   mutate(sector=yahoo.get_sector(symbol))
#
#
# idents<-get_identifiers(yahoo_data$symbol)
# colnames(idents)<-c("identifier","sedol")
#
# yahoo_data<-
#   yahoo_data %>%  add_column(as.tibble(idents))
#
# yahoo_dat<-
#   yahoo_data %>%
#   mutate(shares_held=NA) %>%
#   relocate(local_currency, .after=last_col()) %>%
#   relocate(identifier, .after=company) %>%
#   relocate(sedol, .after=identifier)
#
#

