# UTM Generator with Gender Prefix (R Shiny)

## Project Description

This Shiny application, developed in **R**, generates unique, personalized UTM links for each contact in your marketing campaigns. Designed for performance marketers and marketing data analysts, it automates UTM tagging, detects gender from Persian names to add a proper prefix (Mr./Ms.), allows for URL shortening via API, and enables easy Excel export.  
Use this app to streamline and personalize your SMS or digital campaigns.

---

## Features

- **Excel Upload:** Import contact lists from Excel files.
- **Dynamic UTM Creation:** Generate unique UTM links for each recipient using their mobile number and company ID.
- **Gender Detection:** Automatically add "Mr." or "Ms." (آقای/خانم) prefix for Persian names.
- **URL Shortening:** Integrate with Zaya.io API to bulk shorten links.
- **Date Parameterization:** Add formatted dates to UTMs if needed.
- **Preset Management:** Save and load common UTM parameter sets.
- **User-Friendly UI:** Simple Shiny interface with drag & drop file upload.
- **Excel Export:** Download results as an Excel file.

---

## Requirements

- R (version 4.0 or higher)
- The following R packages:  
  `shiny`, `readxl`, `writexl`, `dplyr`, `stringr`, `httr`, `jsonlite`, `base64enc`, `shinyjs`

To install all dependencies:
```r
install.packages(c(
  "shiny", "readxl", "writexl", "dplyr",
  "stringr", "httr", "jsonlite", "base64enc", "shinyjs"
))
Getting Started
1. Set Up Your API Key
For link shortening to work, get your API key from Zaya.io.

Do not include your key in the code. Instead, set it as an environment variable in your R session:

r
Sys.setenv(ZAYA_API_KEY = "YOUR_API_KEY_HERE")
2. Download Files
Save app.R and utils.R in the same directory.

3. Run the Application
In RStudio or your R console:

r
shiny::runApp()
Usage
Upload Excel File:

File must have at least three columns:
final_mobile (mobile number)
CompanyId
Name (person’s first name in Persian)
Example:

final_mobile	CompanyId	Name
09123456789	11	علی
09350001122	22	زهرا
Set UTM Parameters:

Fill in your desired UTM source, campaign, content, etc.
[Optional] Save or Load Presets:

Save current UTM settings for reuse, or quickly apply saved presets.
[Optional] Add Date Parameter:

Add a date string in your chosen format as part of the UTM parameters.
[Optional] Enable URL Shortening:

If enabled and your API key is set, shortened links will be generated automatically.
Generate UTM Links:

Click “Generate URLs” to process your file. The table of results will display below.
Export Results:

Download the generated table as an Excel file.
Use “Quick Export” to save directly to your desktop.
Project Structure
tp
.
├── app.R
├── utils.R
├── README.md
└── sample_file.xlsx    # (optional, for demo/testing)
Security & Best Practices
Never share your API key in public repositories.
All key values should be configured using environment variables.
Do not upload real user data to your public GitHub repository.
Example Excel File
apache
final_mobile,CompanyId,Name
09123456789,11,علی
09350001122,22,زهرا
Screenshots
Add screenshots or GIFs of your app UI here for better visibility (optional).


Author
Mehrdad Kabiri
https://www.linkedin.com/in/mehrdad-kabiri-performance-marketing

License
This project is open source under the MIT License.