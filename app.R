# app.R

library(shiny)
library(readxl)
library(writexl)
library(shinyjs)
library(base64enc)

source("utils.R")

# اگر کلید API را داری، از ENV بخوان
API_KEY <- Sys.getenv("YOUR_API_KEY") # برای امنیت، در محیط تعریف کن
API_URL <- "UTM_BUilder_URL"
PRESETS_FILE <- file.path(Sys.getenv("HOME"), "utm_presets.json")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("UTM Generator with Gender Prefix"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Excel File Upload"),
      div(
        id = "drop_zone",
        style = "border: 2px dashed #aaa; padding: 20px; text-align: center; cursor: pointer;",
        "فایل اکسل خود را اینجا بکشید و رها کنید یا کلیک کنید تا انتخاب شود"
      ),
      tags$input(id = "file_input", type = "file", accept = ".xlsx,.xls", style = "display: none;"),
      textOutput("file_path"),
      h4("UTM Parameters"),
      textInput("landing_page", "Landing Page URL:", ""),
      actionButton("paste_url", "Paste URL"),
      textInput("utm_source", "UTM Source:", "sms"),
      textInput("utm_campaign", "UTM Campaign:", "sms_campaign"),
      textInput("utm_content", "UTM Content:", "promotion"),
      h4("Presets"),
      textInput("preset_name", "Preset Name:", ""),
      actionButton("save_preset", "Save Preset"),
      selectInput("preset_select", "Select Preset:", choices = c("")),
      actionButton("load_preset", "Load Preset"),
      h4("Options"),
      checkboxInput("add_date", "Add Date", FALSE),
      selectInput("date_format", "Date Format:", 
                  choices = c("YYYYMMDD", "YYYY-MM-DD", "YYMMDD", "MMDDYYYY"),
                  selected = "YYYYMMDD"),
      checkboxInput("shorten_urls", "Shorten URLs", FALSE),
      checkboxInput("auto_export", "Auto Export", FALSE),
      actionButton("generate", "Generate URLs")
    ),
    mainPanel(
      h4("Results"),
      tableOutput("results_table"),
      downloadButton("export", "Export to Excel"),
      actionButton("quick_export", "Quick Export")
    )
  ),
  tags$script(HTML('
    $(document).on("dragover", "#drop_zone", function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).css("border-color", "#000");
    });
    $(document).on("dragleave", "#drop_zone", function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).css("border-color", "#aaa");
    });
    $(document).on("drop", "#drop_zone", function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).css("border-color", "#aaa");
      var files = e.originalEvent.dataTransfer.files;
      if (files.length > 0) {
        var file = files[0];
        if (file.name.match(/\\.(xlsx|xls)$/)) {
          var reader = new FileReader();
          reader.onload = function(event) {
            Shiny.setInputValue("file_data", {name: file.name, data: event.target.result});
          };
          reader.readAsDataURL(file);
        } else {
          alert("لطفاً فقط فایل‌های اکسل (.xlsx یا .xls) آپلود کنید.");
        }
      }
    });
    $(document).on("click", "#drop_zone", function() {
      $("#file_input").click();
    });
    $(document).on("change", "#file_input", function() {
      var file = this.files[0];
      if (file && file.name.match(/\\.(xlsx|xls)$/)) {
        var reader = new FileReader();
        reader.onload = function(event) {
          Shiny.setInputValue("file_data", {name: file.name, data: event.target.result});
        };
        reader.readAsDataURL(file);
      } else {
        alert("لطفاً فقط فایل‌های اکسل (.xlsx یا .xls) آپلود کنید.");
      }
    });
  '))
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    input_data = NULL,
    file_path = "",
    results = NULL,
    presets = load_presets(PRESETS_FILE)
  )
  
  # آپدیت پریست‌ها در UI
  observe({
    updateSelectInput(session, "preset_select", choices = c("", sapply(rv$presets, function(p) p$name)))
  })
  
  # بارگذاری فایل اکسل
  observeEvent(input$file_data, {
    file_info <- input$file_data
    if (!is.null(file_info)) {
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(base64decode(gsub("data:.*?base64,", "", file_info$data)), temp_file)
      rv$file_path <- temp_file
      output$file_path <- renderText(file_info$name)
      tryCatch({
        data <- read_excel(temp_file)
        if (ncol(data) >= 3) {
          colnames(data)[1:3] <- c("final_mobile", "CompanyId", "Name")
          data <- data[, 1:3]
        } else {
          stop("فایل اکسل باید حداقل سه ستون داشته باشد: final_mobile, CompanyId, Name")
        }
        data$final_mobile <- as.character(data$final_mobile)
        data$CompanyId <- as.character(data$CompanyId)
        data$Name <- as.character(data$Name)
        rv$input_data <- data
        showNotification(paste("تعداد", nrow(data), "رکورد بارگذاری شد."), type = "message")
      }, error = function(e) {
        showNotification(paste("خطا در خواندن فایل اکسل:", e$message), type = "error")
        rv$input_data <- NULL
        rv$file_path <- ""
        output$file_path <- renderText("")
      })
    }
  })
  
  # کلید چسباندن URL
  observeEvent(input$paste_url, {
    showNotification("Paste دستی در Shiny پشتیبانی نمی‌شود.", type = "warning")
  })
  
  # تولید UTM لینک
  observeEvent(input$generate, {
    if (is.null(rv$input_data)) {
      showNotification("ابتدا فایل اکسل را بارگذاری کنید.", type = "warning")
      return()
    }
    landing_page <- input$landing_page
    if (!validate_url(landing_page)) {
      showNotification("آدرس لندینگ‌پیج معتبر نیست.", type = "warning")
      return()
    }
    withProgress(message = "در حال ساخت لینک‌ها", value = 0, {
      rv$input_data$Name <- ifelse(is.na(rv$input_data$Name) | rv$input_data$Name == "", "Unknown", rv$input_data$Name)
      results <- list()
      utm_urls <- character()
      date_string <- ""
      if (input$add_date) {
        date_formats <- list(
          "YYYYMMDD" = "%Y%m%d",
          "YYYY-MM-DD" = "%Y-%m-%d",
          "YYMMDD" = "%y%m%d",
          "MMDDYYYY" = "%m%d%Y"
        )
        date_string <- format(Sys.time(), date_formats[[input$date_format]])
      }
      total_rows <- nrow(rv$input_data)
      for (i in 1:total_rows) {
        row <- rv$input_data[i, ]
        gender <- detect_gender(row$Name)
        prefixed_name <- prefix_name(row$Name, gender)
        utm_params <- list(
          utm_source = input$utm_source,
          utm_medium = row$final_mobile,
          utm_campaign = input$utm_campaign,
          utm_content = input$utm_content,
          company_id = row$CompanyId
        )
        if (date_string != "") utm_params$utm_date <- date_string
        query_string <- paste(names(utm_params), utm_params, sep = "=", collapse = "&")
        utm_url <- if (grepl("\\?", landing_page)) {
          paste0(landing_page, "&", query_string)
        } else {
          paste0(landing_page, "?", query_string)
        }
        utm_urls <- c(utm_urls, utm_url)
        results[[i]] <- list(
          final_mobile = row$final_mobile,
          CompanyId = row$CompanyId,
          Name = row$Name,
          Prefixed_Name = prefixed_name,
          UTM_URL = utm_url,
          Shortened_URL = utm_url
        )
        incProgress(1 / total_rows, detail = paste("در حال پردازش ردیف", i, "از", total_rows))
      }
      rv$results <- bind_rows(results)
      if (input$shorten_urls && length(utm_urls) > 0) {
        withProgress(message = "در حال کوتاه‌سازی لینک‌ها", value = 0, {
          shortened_urls <- shorten_url_batch(utm_urls, session, API_KEY, API_URL)
          for (j in seq_along(shortened_urls)) {
            rv$results$Shortened_URL[j] <- shortened_urls[j]
            incProgress(1 / length(utm_urls), detail = paste("کوتاه‌سازی", j, "از", length(utm_urls)))
          }
        })
      }
      output$results_table <- renderTable({
        rv$results
      }, striped = TRUE, hover = TRUE, bordered = TRUE)
      showNotification(paste(nrow(rv$results), "لینک تولید شد."), type = "message")
      if (input$auto_export) {
        quick_export()
      }
    })
  })
  
  # ذخیره پریست
  observeEvent(input$save_preset, {
    name <- input$preset_name
    if (name == "") {
      showNotification("نام پریست را وارد کنید.", type = "warning")
      return()
    }
    preset <- list(
      name = name,
      source = input$utm_source,
      campaign = input$utm_campaign,
      content = input$utm_content
    )
    idx <- which(sapply(rv$presets, function(p) p$name == name))
    if (length(idx) > 0) {
      rv$presets[[idx]] <- preset
    } else {
      rv$presets <- append(rv$presets, list(preset))
    }
    save_presets(rv$presets, PRESETS_FILE, session)
    updateSelectInput(session, "preset_select", choices = c("", sapply(rv$presets, function(p) p$name)))
    showNotification(paste("پریست", name, "ذخیره شد."), type = "message")
  })
  
  # بارگذاری پریست
  observeEvent(input$load_preset, {
    name <- input$preset_select
    if (name == "") {
      showNotification("یک پریست انتخاب کنید.", type = "warning")
      return()
    }
    preset <- rv$presets[[which(sapply(rv$presets, function(p) p$name == name))]]
    if (!is.null(preset)) {
      updateTextInput(session, "utm_source", value = preset$source)
      updateTextInput(session, "utm_campaign", value = preset$campaign)
      updateTextInput(session, "utm_content", value = preset$content)
      showNotification(paste("پریست", name, "بارگذاری شد."), type = "message")
    } else {
      showNotification(paste("پریست", name, "یافت نشد!"), type = "warning")
    }
  })
  
  # خروجی اکسل
  output$export <- downloadHandler(
    filename = function() {
      paste0("UTM_Links_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      if (is.null(rv$results)) {
        showNotification("لینکی برای خروجی نیست.", type = "warning")
        return()
      }
      write_xlsx(rv$results, file)
      showNotification(paste(nrow(rv$results), "لینک خروجی گرفته شد."), type = "message")
    }
  )
  
  # خروجی سریع اکسل به دسکتاپ
  quick_export <- function() {
    if (is.null(rv$results)) {
      showNotification("چیزی برای خروجی وجود ندارد.", type = "warning")
      return()
    }
    desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
    if (!dir.exists(desktop_path)) desktop_path <- Sys.getenv("HOME")
    file_path <- file.path(desktop_path, paste0("UTM_Links_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
    tryCatch({
      write_xlsx(rv$results, file_path)
      showNotification(paste(nrow(rv$results), "لینک روی دسکتاپ ذخیره شد."), type = "message")
    }, error = function(e) {
      showNotification(paste("خطا در خروجی گرفتن:", e$message), type = "error")
    })
  }
  observeEvent(input$quick_export, {
    quick_export()
  })
}

shinyApp(ui = ui, server = server)