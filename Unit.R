# utils.R

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(shiny)
library(shinyjs)

# اعتبارسنجی آدرس URL
validate_url <- function(url) {
  if (is.na(url) || url == "") return(FALSE)
  grepl("^(http|https)://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(/.*)?$", url)
}

# تشخیص جنسیت از نام
detect_gender <- function(name) {
  if (is.na(name) || name == "") return(NULL)
  name <- tolower(str_trim(name))
  female_endings <- c("ه", "ا", "ین", "یا", "یه", "ان", "ی")
  male_names <- c("علی", "محمد", "حسین", "رضا", "مهدی", "احمد", "امیر")
  if (any(sapply(male_names, function(m) startsWith(name, tolower(m))))) {
    return("male")
  }
  if (any(sapply(female_endings, function(e) endsWith(name, e)))) {
    return("female")
  }
  return(NULL)
}

# اضافه کردن پیشوند به نام بر اساس جنسیت
prefix_name <- function(name, gender) {
  if (is.na(name) || name == "") return(name)
  if (!is.null(gender)) {
    if (gender == "male") return(paste("آقای", name))
    if (gender == "female") return(paste("خانم", name))
  }
  return(name)
}

# کوتاه‌سازی دسته‌ای لینک‌ها
shorten_url_batch <- function(urls, session, api_key, api_url) {
  headers <- add_headers(
    "X-API-KEY" = api_key,
    "Content-Type" = "application/json"
  )
  results <- vector("character", length(urls))
  batch_size <- 10
  error_count <- 0
  max_errors <- 3
  for (i in seq(1, length(urls), by = batch_size)) {
    if (error_count >= max_errors) {
      showNotification("خطا در کوتاه‌سازی بعضی لینک‌ها.", type = "warning", session = session)
      results[i:length(urls)] <- urls[i:length(urls)]
      break
    }
    batch <- urls[i:min(i + batch_size - 1, length(urls))]
    for (j in seq_along(batch)) {
      tryCatch({
        response <- POST(
          api_url,
          headers,
          body = toJSON(list(url = batch[j]), auto_unbox = TRUE),
          encode = "json",
          timeout(10)
        )
        if (status_code(response) == 200) {
          data <- content(response, as = "parsed")
          short_url <- if (!is.null(data$data$short_url)) {
            data$data$short_url
          } else if (!is.null(data$shortUrl)) {
            data$shortUrl
          } else if (!is.null(data$url)) {
            data$url
          } else {
            batch[j]
          }
          results[i + j - 1] <- short_url
        } else {
          results[i + j - 1] <- batch[j]
          error_count <- error_count + 1
        }
      }, error = function(e) {
        results[i + j - 1] <- batch[j]
        error_count <- error_count + 1
      })
    }
    Sys.sleep(1)
  }
  results
}

# بارگذاری پریست‌ها از فایل
load_presets <- function(presets_file) {
  if (file.exists(presets_file)) {
    presets <- fromJSON(presets_file)
    if (is.data.frame(presets)) presets else list()
  } else {
    list()
  }
}

# ذخیره پریست‌ها در فایل
save_presets <- function(presets, presets_file, session) {
  tryCatch({
    write_json(presets, presets_file, pretty = TRUE, auto_unbox = TRUE)
    showNotification("پریست با موفقیت ذخیره شد.", type = "message", session = session)
  }, error = function(e) {
    showNotification(paste("خطا در ذخیره پریست:", e$message), type = "error", session = session)
  })
}