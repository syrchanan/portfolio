# Script will: 
## scrape the web for TV Linear ratings
## collate them into a better looking format
## send them in an email for viewing

library(tidyverse)
library(rvest)
library(lubridate)
library(gt)
library(janitor)

# Scraping For Each Unfinished Rating Post####


#set url
url <- "https://www.mediaite.com/category/daily-ratings/"

#get headlines
heads <- read_html(url) %>% html_nodes(".o-chron-post__headline-link") %>% html_text()

#get archive
log <- read_lines("sent.txt", skip_empty_rows = TRUE)

#init
publish_range <- 0

#for every headline, check if it exists in the archive
for (i in 1:length(heads)) {
  
  if (heads[i] %in% log) {
    
    break
    
  } else {
    
    publish_range <- publish_range + 1
    
    write(heads[i], file = "sent.txt", append = TRUE, sep = "\n")
    
  }
}

#for each headline that has not been sent, get the link to the page for scraping
links <- read_html(url) %>% html_nodes(".o-chron-post__headline-link") %>% html_attr("href")

#subset the links to get just those desired ones
links_final <- links[1:publish_range]


# Scraping Each Individual Page for Actual Ratings ####


#create table functions
total <- function(table_num) {
  
  total_table_temp <- ratings[[table_num]] %>% 
    row_to_names(1) %>% 
    gt() %>% 
    tab_header(title = "Total Viewers (Live+SD X 1,000)") %>%
    tab_style(
      style = cell_borders(
        sides = c('left', 'right'),
        color = "#BBBBBB",
        weight = px(1),
        style = "solid"
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    tab_style(
      style = cell_fill(
        color = "#D3D3D3"
      ),
      locations = cells_body(
        columns = everything(),
        rows = c(2,4,6,8,10,12,14,16,18)
      )
    )
  
  return(total_table_temp)
}

p25_54 <- function(table_num) {
  
  p25_54_table_temp <- ratings[[table_num]] %>% 
    row_to_names(1) %>% 
    gt() %>% 
    tab_header(title = "P25-54 Demo (Live+SD X 1,000)") %>%
    tab_style(
      style = cell_borders(
        sides = c('left', 'right'),
        color = "#BBBBBB",
        weight = px(1),
        style = "solid"
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    tab_style(
      style = cell_fill(
        color = "#D3D3D3"
      ),
      locations = cells_body(
        columns = everything(),
        rows = c(2,4,6,8,10,12,14,16,18)
      )
    ) 
  
  return(p25_54_table_temp)
}

iter_length <- length(links_final)

#iterate over each page
for (z in 1:iter_length) {
  
  if (publish_range == 0) {
    break
  }
  
  url_iter <- links_final[z]
  
  #get tables
  ratings <- read_html(url_iter) %>% html_table()
  
  #count number of loops for days
  days <- length(ratings)/2
  
  #get the page body text for parsing
  body_text <- read_html(url_iter) %>% html_nodes("#post-body") %>% html_elements("p") %>% html_text()
  
  #init list
  summary_stats <- c()
  
  #seek out summary ratings
  for (j in 1:length(body_text)) {
    if (body_text[j] == "Total viewers:") {
      for (k in 0:7) {
        index <- j + k
        summary_stats <- append(x = summary_stats, values = body_text[index])
      }
    }
  }
  
  #list out number of tables
  if (days < 2) {
    formatted_tables <- list(total(1), p25_54(2))
  } else {
    formatted_tables <- list(total(1), p25_54(2), total(3), p25_54(4))
  }
  
  #save out each table as a png
  for (r in 1:length(formatted_tables)) {
    names_table <- paste0("table_",r,".png")
    gtsave(formatted_tables[[r]], names_table)
  }
  
  #need to have Java x64 downloaded before installing rJava so that the architecture of R and Java match
  #install.packages("rJava")
  library(rJava)
  #install.packages("mailR")
  library(mailR)
  
  #borrow header as subject line
  subject_line <- heads[z]
 
  #write body text to file
  if (days < 2) {
    write(
      c("Live+SD Total Day Averages",
      summary_stats[1:8],
      "Live+SD Prime Averages",
      summary_stats[9:16]
    ), file = "body.txt", append = FALSE, sep = "\n")
  } else {
    write(
      c("The following are in date order (5/26 first and 5/27 second, e.g.)",
      "\n",
      "Live+SD Total Day Averages",
      summary_stats[1:8],
      "Live+SD Prime Averages",
      summary_stats[9:16],
      "\n",
      "Live+SD Total Day Averages",
      summary_stats[17:24],
      "Live+SD Prime Averages",
      summary_stats[25:32]
    ), file = "body.txt", append = FALSE, sep = "\n")
  }
  
  #load up attachments as filepaths
  if (days < 2) {
    attachments <- c("table_1.png",
                     "table_2.png")
  } else {
    attachments <- c("table_1.png",
                     "table_2.png",
                     "table_3.png",
                     "table_4.png")
  }
  
  #normalizepaths to absolute for sending
  for (a in 1:length(attachments)) {
    attachments[a] <- normalizePath(attachments[a])
  }
  
  #get info
  info <- read_lines("emails_passwords.txt")
  
  #send email
  sender <- info[1]
  recipients <- c(info[2])
  send.mail(from = sender,
            to = recipients,
            subject = subject_line,
            body = readChar("body.txt", file.info("body.txt")$size),
            attach.files = c(attachments),
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = info[3],
                        passwd = info[4], ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
}
