# Survey Analysis tool for automated reporting (R script, R Markdown report, R shiny dashboard)
This project includes an R script retrieving data automatically from an online survey tool (SurveyGizmo) via API, several R Markdown files to summarise and visualize the survey results and a shiny dashboard to access the survey results interactively.

The tool was created to evaluate courses and analyzes data on multiple levels and contains reports for different levels of analyises (per module, per course, per year...)  

The R script, R Markdown files and dashboard can be used as templates to analyze any kind of survey data from SurveyGizmo. The variable names just need to be changed. 

To test the tool, you can download the excel file containing sample data.

## How to test the tool with sample data:

- Download the Excel file with the sample data ("SampleData.xlsx"), the R script to retriev and clean the data ("data.R"), the R Markdown report templates ("Report_Module.Rmd", "Report-Course[summary-of-modules]", "Report-Yearly.Rmd") + the stylesheet for Word ("Style_of_report.docx") and the two shiny dashboard files ("server.R", "ui.R")

- Save all files in one folder on your computer

- Knit the R Markdown files (to word, html or pdf) and run the shiny dashboard
