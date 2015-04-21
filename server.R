library(shiny)
library(dplyr)
library(jsonlite)
library(stringr)


normalize_oname <- function(orgnames) {
    library(stringr)
    state_codes <- "^The| S\\.?A\\.?$|,S\\.?A\\.$|,A\\.?G\\.?$|A\\.?G\\.?$|A/S$|/?DE/?$|/BERMUDA/?$|/CAN/?$|/CA/?$|/CN/?$|/FL/?|/GA/?$|/NV/?$|/OH/?$|/\\s?NEW/?$|/NJ/?$|/MO/?$|/MD/?$|/PA/?$|/ME/?$|/MA/?$|/FI/?$|/?NEW/?$|/MI/?$|/NJ/?$|/NC/?$|/NY/?$|/TN/?$|/TX/?$|/SC/?$|N\\.?V\\.?$"
    entity_descripts <- "(\\b|,\\s?)(Inc\\.?|Corp\\.?|Ltd\\.?|.?L\\.?L\\.?C\\.?|.?L\\.?P\\.?|Co\\.?|Corporation|Incorporated|Limited Partnership|Group|Holdings?|Limited|Company|Trust|REIT|P\\.?L\\.?C\\.?|GP|Real Estate Investment Trust)$"    
    orgnames <- str_trim(sub(state_codes,"",orgnames,ignore.case = T))
    orgnames <- str_trim(sub(entity_descripts,"",orgnames,ignore.case=T))
    orgnames <- sub(",$","",orgnames)
    orgnames <- str_trim(sub(entity_descripts,"",orgnames,ignore.case=T))
    orgnames <- tolower(gsub("[[:punct:]]"," ",orgnames))
    orgnames <- gsub("\\s{2,}", " ", orgnames)
    return(orgnames)
}

buildSearchString <- function(searchString, exactString, orString, start = 1, numOfResults = 10) {
    require(dplyr)
    require(stringr)
    apiKey <- "#############################"
    customSearchEngineKey <- "######################"
    searchURL <- "https://www.googleapis.com/customsearch/v1?"
    toSearch <- paste0(searchURL, "key=", apiKey, "&cx=", customSearchEngineKey, "&q=")

    newSearchString <- gsub(" ", "%20", searchString)
    newOrString <- gsub(" ", "%20", orString)
    newExactString <- gsub(" ", "%20", exactString)

    toSearch <- paste0(toSearch, newSearchString) %>%
        paste0("&alt=json") %>%
        paste0("&start=", start) %>%
        paste0("&num=", numOfResults) %>%
        paste0("&searchType=image") %>%
#         paste0("&fields=searchInformation,items(title, link, snippet, mime, image/contextLink)")  %>%
        paste0("&exactTerms=", newExactString) %>%
        paste0("&imgType%5b%5d=face") %>%
        paste0("&imgType%5b%5d=photo") %>%
        paste0("&orTerms=", newOrString)
    return(toSearch)
}

get_response <- function(urls) {
    library(jsonlite)
    query_results <- vector(mode = "list", length = length(urls))
    for (i in 1:length(urls)) {
        if (is.null(query_results[[i]])) {
            response <- fromJSON(urls[i])
        }
        if (exists("response") && !is.null(response) &&
                response[["searchInformation"]]$formattedTotalResults != 0) {    
            query_results[[i]] <- cbind(response[["items"]][,c("link", "title", "snippet")], 
                                        source_url = response[["items"]][["image"]][,"contextLink"],
                                        num_results = response[["searchInformation"]]$totalResults,
                                        stringsAsFactors = F)
            query_results[[i]] <- cbind(response_order = row.names(query_results[[i]]),
                                        query_results[[i]],
                                        stringsAsFactors = F)
            rm(response)
            Sys.sleep(1.05)
        }
    }
    query_results <- do.call("rbind", query_results)
    query_results <- query_results %>% 
        mutate(response_order = as.numeric(response_order),
               num_results = as.numeric(num_results))
    return(query_results)
}


get_page <- function(urls) {
    library(rvest)
    library(dplyr)
    page_text <- vector(mode = "list", length = length(urls))
    i = 1
    for (url in urls) {
        source_url = url
        context_text[[i]] <- tryCatch({
                html(url) %>% 
                    html_text()
            }, error = function(e) {
                NA
            })
        i = i + 1
    }
    page_text <- do.call("rbind", page_text)
    page_text <- as.data.frame(page_text, stringsAsFactors = F)
    colnames(page_text)[1] <- "doc_text"
    return(page_text)
}

# Split string of organizations separated by a space and create a list to iterate through for
# regex matching.
split_names <- function(company_names) {
    library(dplyr)
    split_names <- str_split(company_names, " ")
    split_names <- lapply(split_names, str_replace_all, pattern = "-", replacement = " ")
    return(split_names)
}

detect_orgs <- function(field, orglist) {
    result = vector(mode = "numeric", length = length(field))
    for (i in 1:length(field))
        result[i] = sum(str_detect(field[i], ignore.case(orglist[[i]])))
    return(result)
}

detect_full_name <- function(field, name) {
    require(dplyr)
    field = field %>%
        str_replace_all("[[:punct:]]", "") %>%
        str_replace_all("\\s{1,}", "")
    name = name %>%
        str_replace_all("[[:punct:]]", "") %>%
        str_replace_all("\\s{1,}", "")
    return(str_detect(field, name))
}



shinyServer(function(input, output) {
    response_data <- reactive({
        urls <- buildSearchString(input$full, input$last, input$affiliated_orgs)
        image_results <- get_response(urls)
        image_results <- image_results %>%
            cbind(first_name = as.character(input$first), 
            last_name = as.character(input$last), 
            full_name = as.character(input$full), 
            orgsearch = as.character(input$affiliated_orgs), stringsAsFactors = F)
        image_results$full_name <- str_replace_all(image_results$full_name, "-", " ")
        image_results$split_orgs <- split_names(image_results$orgsearch)
        
        points <- image_results %>%
            mutate(link_name = 2 * (str_detect(link ,ignore.case(first_name)) * str_detect(link, ignore.case(last_name))),
                   snippet_name = str_detect(snippet ,ignore.case(first_name)) * str_detect(snippet, ignore.case(last_name)),
                   title_name = str_detect(title ,ignore.case(first_name)) * str_detect(title, ignore.case(last_name)),
                   context_name = str_detect(source_url ,ignore.case(first_name)) * str_detect(source_url, ignore.case(last_name)),
           link_orgs = detect_orgs(link, split_orgs),
           snippet_orgs = detect_orgs(snippet, split_orgs),
           title_orgs = detect_orgs(title, split_orgs),
           context_orgs = detect_orgs(source_url, split_orgs),
           link_full_name = 2 * as.numeric(detect_full_name(link, full_name)),
           snippet_full_name = as.numeric(detect_full_name(snippet, full_name)),
           title_full_name = as.numeric(detect_full_name(title, full_name)),
           context_full_name = as.numeric(detect_full_name(source_url, full_name)),
           points_order = abs(response_order - 11) / 2) %>%
            select(link_name:points_order)
    
        points <- points %>%
    mutate(count_names = link_name + snippet_name + title_name + context_name,
           count_orgs = (link_orgs + snippet_orgs + title_orgs + context_orgs) * count_names,
           count_full_names = link_full_name + title_full_name + context_full_name,
           sum = count_names + count_orgs + points_order + 1.5 * count_full_names)
           
image_results <- cbind(image_results, points) 

results_sorted <- image_results %>%
    arrange(desc(sum))

results_best <- head(results_sorted, 1)
})
    output$image = renderUI({
        results_best <- response_data()
        tags$img(src = results_best$link,
                 width = 400,
                 height = 400)
        })
        
        output$image_link <- renderPrint({
            results_best <- response_data()
            paste("Image URL:",results_best$link)
        })
        output$source_link <- renderPrint({
            results_best <- response_data()
            paste("Source URL:",results_best$source_url)
            })
})
