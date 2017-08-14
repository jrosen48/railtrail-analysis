library(rvest)
library(stringr)
library(tidyverse)

# Functions

finding_trails <- function(x) {
    y <- str_split(x, "/")
    type_of_page <- map_chr(y, ~ .[2])
    z <- x[type_of_page == "trail"]
    z <- z[!str_detect(z, "#")]
    z[!is.na(z)]
}

extract_numbers <- function(x) {
    x<- stringr::str_split(x, " ") 
    x <- map_chr(x, ~ .[1])
    x <- suppressWarnings(as.numeric(x))
    x
}

scrape_one_url <- function(url) {

        page <- read_html(paste0("https://www.traillink.com/", url))
        
}

process_page <- function(page) {
    
    Sys.sleep(1)
    
    results_df <- tibble(
        name =  vector(length = 1),
        distance = vector(length = 1),
        surface = vector(length = 1),
        category = vector(length = 1),
        mean_review = vector(length = 1),
        description = vector(length = 1),
        n_reviews = vector(length = 1),
        raw_reviews = vector(length = 1)
    )

    page <- read_html(paste0("https://www.traillink.com/", page))
 
    distance <- html_nodes(page, ".facts:nth-child(1) div:nth-child(3) span")
    distance <- html_text(distance)
    results_df$distance <- distance
    
    name <- html_nodes(page, ".small-12 .section-header")
    name <- html_text(name)
    results_df$name <- name
    
    surface <- html_nodes(page, ".facts+ .facts div:nth-child(1) span")
    surface <- html_text(surface)
    results_df$surface <- surface
    
    mean_review <- html_nodes(page, ".mid-up-test .fa-star")
    mean_review <- html_text(mean_review)
    results_df$mean_review <- length(mean_review)
    
    category <- html_nodes(page, ".facts+ .facts div:nth-child(2) span")
    category <- html_text(category)
    results_df$category <- category
    
    description <- html_nodes(page, ".wrap .medium-8")
    description <- html_text(description)
    results_df$description <- description
    
    n_reviews <- html_nodes(page, ".reviews a")
    n_reviews <- html_text(n_reviews)
    results_df$n_reviews <- n_reviews
    
    star <- html_nodes(page, "#trail-detail-reviews .medium-3 ")
    
    # Finding individual reviews
    
    out_vector <- vector(length = length(star))
    
    for (j in 1:length(star)) {
        x <- html_nodes(star[j], ".star")
        x <- length(html_text(x))
        x[x == 0] <- NA
        out_vector[j] <- x
    }

    mutate(results_df,
           raw_reviews = list(out_vector))
    
}

process_all_pages <- function(.x = vector_of_pages, .y = state_names, .pb = NULL) {
    
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    
    out <- map_df(vector_of_pages, possibly(process_page, NULL))
    
    write_rds(out, paste0("data/", state_names, ".rds"))
    
    return(out)
    
}

find_urls <- function(x) {
    
    urls <- read_html(paste0("https://www.traillink.com", x)) %>%
        html_nodes("a") %>% 
        html_attr("href") %>% 
        finding_trails()
    
    return(urls)
}

# Main analysis

state_urls <- read_html("https://www.traillink.com/find-trails") %>%
    html_nodes("a") %>%
    html_attr("href")

state_urls <- state_urls[str_sub(state_urls, start = 2, end = 6) == "state"]
state_urls <- state_urls[!is.na(state_urls)]

name <- state_urls %>%
    str_split("/") %>%
    map_chr( ~ .[3]) %>%
    str_split("-") %>%
    map_chr( ~ .[1])

list_of_trails <- map(state_urls, find_urls)
names(list_of_trails) <- name

pb <- progress_estimated(length(list_of_trails))
list_of_dfs <- map2_df(list_of_trails, name, process_all_pages, .id = "state", .pb = pb)