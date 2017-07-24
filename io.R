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

# Main function to scrape data for a page

scrape_data <- function(urls, n_to_scrape = NULL) {
    
    if (is.null(n_to_scrape)) n_to_scrape <- length(urls)
    
    results_df <- tibble(
        name =  vector(length = length(urls)),
        distance = vector(length = length(urls)),
        surface = vector(length = length(urls)),
        category = vector(length = length(urls)),
        mean_review = vector(length = length(urls)),
        description = vector(length = length(urls)),
        n_reviews = vector(length = length(urls))
    )
    
    out_list <- list()
    
    for (i in 1:n_to_scrape) {
        
        print(paste0("Processing ", i, " out of ", n_to_scrape))
        
        page <- read_html(paste0("https://www.traillink.com/", urls[i]))
        
        distance <- html_nodes(page, ".facts:nth-child(1) div:nth-child(3) span")
        distance <- html_text(distance)
        results_df$distance[i] <- distance
        
        name <- html_nodes(page, ".small-12 .section-header")
        name <- html_text(name)
        results_df$name[i] <- name
        
        surface <- html_nodes(page, ".facts+ .facts div:nth-child(1) span")
        surface <- html_text(surface)
        results_df$surface[i] <- surface
        
        mean_review <- html_nodes(page, ".mid-up-test .fa-star")
        mean_review <- html_text(mean_review)
        results_df$mean_review[i] <- length(mean_review)
        
        category <- html_nodes(page, ".facts+ .facts div:nth-child(2) span")
        category <- html_text(category)
        results_df$category[i] <- category
        
        description <- html_nodes(page, ".wrap .medium-8")
        description <- html_text(description)
        results_df$description[i] <- description
        
        n_reviews <- html_nodes(page, ".reviews a")
        n_reviews <- html_text(n_reviews)
        results_df$n_reviews[i] <- n_reviews
        
        star <- html_nodes(page, "#trail-detail-reviews .medium-3 ")
        
        # Finding individual reviews
        
        out_vector <- vector(length = length(star))
        
        for (j in 1:length(star)) {
            x <- html_nodes(star[j], ".star")
            x <- length(html_text(x))
            x[x == 0] <- NA
            out_vector[j] <- x
        }
        
        out_list[[i]] <- out_vector
        
        # results_df <- bind_cols(results_df, out_vector)
        
        Sys.sleep(1)
        
    }
    
    tmp <- enframe(out_list)
    
    results_df <- results_df[1:n_to_scrape, ]
    
    results_df <- mutate(results_df,
                         distance = extract_numbers(distance),
                         n_reviews = extract_numbers(n_reviews),
                         name = str_sub(name, end = -7L),
                         raw_reviews = tmp$value
                         )
    
    results_df
    
}

# Scraping

state_urls <- read_html("https://www.traillink.com/find-trails") %>%
    html_nodes("a") %>%
    html_attr("href")

state_urls <- state_urls[str_sub(state_urls, start = 2, end = 6) == "state"]
state_urls <- state_urls[!is.na(state_urls)]

scrape_every_state <- function(state_urls, which_state = NULL, n_to_scrape = NULL) {
    
    if (!is.null(which_state)) {
        state_urls <- state_urls[str_detect(state_urls, which_state)]
    }
    
    for (i in 1:length(state_urls)){
        
        name <- state_urls %>% 
            str_split("/") %>% 
            map_chr( ~ .[3]) %>% 
            str_split("-") %>% 
            map_chr( ~ .[1])
        
        print(paste0("Processing data for ", name[i], " (", i, "/", length(name), ")"))
        
        urls <- read_html(paste0("https://www.traillink.com", state_urls[i])) %>%
            html_nodes("a") %>% 
            html_attr("href") %>% 
            finding_trails()
        
        df <- scrape_data(urls, n_to_scrape)    
        
        write_rds(df, paste0("data/", name[i], ".rds"))
        
    }
}

scrape_every_state(state_urls, "mi")