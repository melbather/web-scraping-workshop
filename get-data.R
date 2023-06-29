#load libraries
library(rvest)
library(dplyr)
library(stringr)

#this Wikipedia page contains links to other Wikipedia pages that list notable deaths for each year starting in 1987
url <- "https://en.wikipedia.org/wiki/Lists_of_deaths_by_year"
wiki_page <- read_html(url)

#get all the links on the page
links <- wiki_page %>%
  html_elements("a") %>%
  html_attr("href")

#get only the links that point to wiki pages about notable deaths
wiki_links <- grep("^/wiki/Deaths_", links, value = TRUE)
#remove older years as they don't have information about age of death
wiki_links <- wiki_links[-grep("^/wiki/Deaths_in_.*_198[7-9]", wiki_links)]

#let's investigate what we get when we read one of these wiki links
(first_page_text <- read_html(paste0("https://en.wikipedia.org", wiki_links[2])) %>% 
    html_elements("li") %>%
    html_text())

#for the purpose of this workshop, we are going to narrow down the number of links we look at!
wiki_links_subset <- wiki_links[sample(1:length(wiki_links), 100)]

#empty character vector to store info
all_people <- character()

#loop through links to extract info
for(link in wiki_links_subset) {
  #full URL of the linked page, read linked page and get all text
  page_text <- read_html(paste0("https://en.wikipedia.org", link)) %>% 
    html_elements("li") %>%
    html_text()
  #list of people starts after entry list description
  start_pos <- grep("Name, age, country of citizenship", page_text) + 1
  #list stops before links to deaths in other months
  end_pos <- grep("\\^", page_text)[1] - 1
  #add to vector of people **note - this is not efficient!
  all_people <- c(all_people, page_text[start_pos:end_pos])
}


#splitting the result isn't as straightforward as you might expect because descriptions can contain commas
#and names can include titles which contain commas
#so write a function to do the splitting on the age (if it exists) 
split_data <- function(entry) {
  entry <- str_remove(entry, "\\[\\d+\\]") #remove any square brackets that were reference links
  if(grepl("\\d", entry)) {
    #find the position of the age - use this as an anchor for elements in the split
    age_pos <- grep("\\d{1,3}", str_split(entry, " ")[[1]])[1]
    split_entry <- str_split(entry, " ")[[1]]
    #name comes before age
    name <- str_remove(paste(split_entry[1:(age_pos-1)], collapse = " "), ",$")
    age <- str_remove(split_entry[age_pos], ",")
    #description comes after age
    description <- paste(split_entry[(age_pos+1):length(split_entry)], collapse = " ")
    c(name, age, description)
  }
}

#apply function to all people data entries and add to data frame created earlier
df <- data.frame(do.call(rbind, lapply(all_people, split_data)))
names(df) <- c("Name", "Age", "Description")


saveRDS(df, "deathdata.RDS")


