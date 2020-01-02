# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)


# get last page function
get_last_page <- function(html) {
  
  pages_data <- html %>% 
    html_nodes(".page") %>% 
    html_text() %>% 
    as.numeric()
  
  pages_data[length(pages_data)]
}

# extract information from page

# get price
get_price <- function(html) {
  price <- html %>% 
    html_nodes(".offer-price__number") %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist() %>% 
    `[[`(1) %>% 
    str_extract_all(., "\\d+") %>% 
    `[[`(1) 
  
  pasted_price <- paste0(price[1], price[2]) %>% 
    as.numeric()
  return(pasted_price)
}

get_params_label <- function(html) {
  html %>% 
    html_nodes(".offer-params__label") %>% 
    html_text() %>% 
    str_trim()
}

get_params_value <- function(html) {
  html %>% 
    html_nodes(".offer-params__value") %>% 
    html_text() %>% 
    str_trim()
}

get_data_table <- function(html) {
  # create table with basic info
  df <- data.frame(x = get_params_label(html),
                   y = get_params_value(html),
                   stringsAsFactors = FALSE)
  df <- setNames(data.frame(t(df[, -1])), df[, 1])
  df <- df %>% mutate(Cena = get_price(html))
  return(df)
}

get_data_from_url <- function(url) {
  pb$tick()$print()
  html <- try(read_html(url), silent = TRUE)
  if (!inherits(html, "try-error")) {
    get_data_table(html)
  }
}

scrape_table <- function(url) {
  
  # Read first page
  first_page <- read_html(url)
  
  # Extract number of pages
  latest_page_number <- get_last_page(first_page)
  
  # generate list of pages
  list_of_pages <- str_c(url, "&page=", 1:latest_page_number)
  
  # get pages
  pages <- lapply(list_of_pages, read_html)
  
  # generate list of offers
  list_of_offers <- lapply(pages, function(page) {
    page %>% 
      html_nodes(".adListingItem") %>% 
      html_attrs() 
  })
  
  list_of_offers <- lapply(list_of_offers, function(lista) {
    lapply(lista, function(offer) {
      offer %>% 
        as.list() %>% 
        purrr::pluck("data-href")
    }) 
  }) %>% 
    unlist()
  
  pb <<- progress_estimated(length(list_of_offers))
  
  # Apply function to extract info and bind it
  list_of_offers %>% 
    map(~get_data_from_url(.)) %>% 
    bind_rows()
}


lexus <- scrape_table("https://www.otomoto.pl/osobowe/gs/?search%5Bfilter_enum_fuel_type%5D=petrol&search%5Bnew_used%5D=on")

# plot price vs year of production
lexus %>% 
  mutate(`Rok produkcji` = as.numeric(`Rok produkcji`)) %>% 
  ggplot(.,
         aes(x = `Rok produkcji`,
             y = Cena)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(breaks = seq(min(lexus$`Rok produkcji`), max(lexus$`Rok produkcji`, by = 1))) +
  labs(title = "Cena vs rok produkcji dla Lexus GS na otomoto.pl")


url <- "https://www.otomoto.pl/osobowe/mazda/6/od-2016/?search%5Bfilter_float_year%3Ato%5D=2017&search%5Bfilter_enum_fuel_type%5D%5B0%5D=petrol&search%5Bfilter_enum_gearbox%5D%5B0%5D=automatic&search%5Bfilter_enum_gearbox%5D%5B1%5D=cvt&search%5Bfilter_enum_gearbox%5D%5B2%5D=dual-clutch&search%5Bfilter_enum_gearbox%5D%5B3%5D=semi-automatic&search%5Border%5D=created_at%3Adesc&search%5Bbrand_program_id%5D%5B0%5D=&search%5Bcountry%5D="
mazda <- scrape_table(url)

mazda %>% 
  mutate(`Rok produkcji` = as.numeric(`Rok produkcji`)) %>% 
  ggplot(.,
         aes(x = `Rok produkcji`,
             y = Cena)) +
  geom_boxplot() +
  theme_bw()

mazda %>% 
  filter(Typ == "Sedan") %>% 
  summarize(min = min(Cena),
            max = max(Cena),
            median = median(Cena))


# change przebieg to numeric
change_przebieg_to_numeric <- function(df) {
  km <- sapply(str_extract_all(df$Przebieg, "\\d+"), function(x) {
    as.numeric(paste0(x[1], x[2]))
  })
  df$Przebieg <- km
  df
}

# change and round pojemnosc to numeric
change_pojemnosc_to_numeric <- function(df) {
  pojemnosc <- sapply(str_extract_all(df$`Pojemność skokowa`, "\\d+"), function(x) {
    as.numeric(paste0(x[1], x[2]))
  })
  pojemnosc <- round(pojemnosc, -2)
  df$`Pojemność skokowa` <- pojemnosc
  df
}

mazda <- change_przebieg_to_numeric(mazda)
mazda <- change_pojemnosc_to_numeric(mazda)

ggplot(mazda %>% 
         filter(Przebieg < 60000,
                Cena > 75000),
       aes(x = Przebieg,
           y = Cena)) +
  geom_point() +
  facet_wrap(~`Pojemność skokowa`)
