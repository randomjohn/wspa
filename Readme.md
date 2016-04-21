---
title: "Readme"
author: "John Johnson"
date: "March 5, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Data dictionary


UNDERSENT - previous criminal case, filter those out

Looking at number of days in jail
	* Date they were booked
	* Date the magistrate set bond amount
Attorney
	* At bond, not at trial
	* TBO - trial by officer
Pack.ID
	* Designed to stay with one person throughout their court record
Warrant.No
	* Connection to court data
Indictment.No - ?
Disposition
	* (PD) - paid bond - pay attn to these
	* (UNPD) - couldn't afford bond
	* Sentenced on - filter out - sentenced on another charge
	* Dismissed on - filter out - dismissed other charge - very convoluted
	* Recognizance bond - walk out, no money down, pay bond if don't show for court
	* No bond at all - so bad judge didn't set a bond, held anyway
	* Disposition is included - another charge going through
		* Change in the charge - less time in jail
  * The Disposition was included with the above - there are other cases
