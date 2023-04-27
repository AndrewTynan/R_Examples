
Rmarkdown_template <- function(title, message, filename) {
  
  today <- format(Sys.time(), "%B %d, %Y")
  
  file_str <- paste("---
title: 'blah'
author: 'name'
date: ", today,"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## ", title,"

This is a test to write an .Rmd file. 

Our message is: ", message,"

```{r cars}
summary(cars)
```

", sep = "")
  
  file_str <- noquote(file_str)
  output_file <- file(noquote(paste("",filename,".Rmd", sep = "")))
  writeLines(file_str, output_file)
  close(output_file)		 
  
}