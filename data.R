library(xlsx)
library(dplyr)
library(readr)


# import datasets ---------------------------------------------------------

bond_2012 <- read.xlsx("PSU9005r.2012.xlsx",1)
bond_2013 <- read.xlsx("PSU9005r.2013.xlsx",1)
bond_2014 <- read.xlsx("PSU9005r.2014.xlsx",1)
bond2 <- read.xlsx("PSU9005r.2012to2014.3587.xlsx",1)
bond3 <- read.xlsx("PSU9005r.2012to2014for3016.xlsx",1)
bond4 <- read.xlsx("PSU9005r.2012to2014for3017.xlsx",1)
main_group <- read.xlsx("Main Group.xlsx",1)
court_data <- read_csv("warrant_court_data.csv")

# utility functions -------------------------------------------------------


sym_diff <- function(df1,df2) {
  # utility function to see what variables two dfs don't have in common
  # was used in developing the ETL section below, but not used otherwise
  names1 <- names(df1)
  names2 <- names(df2)
  setdiff(base::union(names1,names2),base::intersect(names1,names2))
}

# data combining ------------------------------------------------------------
# this is some very light ETL, just to combine all the data into one dataset indexed
# by offense. The only derived variables are Year and Offense. Other derived variables are later.

# first time marijuana possession
bond_2012 <- cbind(2012,bond_2012)
names(bond_2012)[1] <- "Year"
bond_2013 <- cbind(2013,bond_2013)
names(bond_2013)[1] <- "Year"
bond_2014 <- cbind(2014,bond_2014)
names(bond_2014)[1] <- "Year"

bond1 <- rbind(bond_2012,bond_2013,bond_2014)
bond1 <- cbind("Marijuana",bond1)
names(bond1)[1] <- "Offense"

#####
# fix names so we can combine later
#####

# arresting officer
n <- grep("arrest",names(bond1),ignore.case = TRUE)
names(bond1)[n] <- "Arresting.Officer"
# release date
n <- grep("Release",names(bond2))
names(bond2)[n] <- c("Release.Date", "Release.Date.1")
# create year variable
bond2$Year <- as.numeric(format(as.Date(bond2$Booked.Date.1,"%m/%d/%Y"),"%Y"))
bond3$Year <- as.numeric(format(as.Date(bond3$Booked.Date.1,"%m/%d/%Y"),"%Y"))
bond4$Year <- as.numeric(format(as.Date(bond4$Booked.Date.1,"%m/%d/%Y"),"%Y"))
# booked time
# idea is booked.time = actual time
# remove booked.time.1
bond1$Booked.Time <- format(bond1$Booked.Time,"%H:%M")
bond2$Booked.Time <- with(bond2,levels(Booked.Time.1)[Booked.Time.1])
bond2$Booked.Time.1 <- NULL
bond3$Booked.Time <- format(as.POSIXct(strptime(bond3$Booked.Time.1,"%Y-%m-%d %H:%M:%S",tz="GMT")),"%H:%M")
bond3$Booked.Time.1 <- NULL
bond4$Booked.Time <- format(as.POSIXct(strptime(bond4$Booked.Time.,"%Y-%m-%d %H:%M:%S",tz="GMT")),"%H:%M")
bond4$Booked.Time. <- NULL
# pack id
n <- grep("Pack.Id",names(bond3),ignore.case = TRUE)
names(bond3)[n] <- "ID.Pack"
n <- grep("Pack.Id",names(bond4),ignore.case = TRUE)
names(bond4)[n] <- "ID.Pack"
# judge -> trial.judge
n <- grep("Judge",names(bond3),ignore.case = TRUE)
names(bond3)[n] <- "Trial.Judge"
n <- grep("Judge",names(bond4),ignore.case = TRUE)
names(bond4)[n] <- "Trial.Judge"
# release date
n <- grep("Estimated.Release",names(bond3),ignore.case = TRUE)
names(bond3)[n] <- c("Release.Date","Release.Date.1")
n <- grep("Estimated.Release",names(bond4),ignore.case = TRUE)
names(bond4)[n] <- c("Release.Date","Release.Date.1")
# warrant no
n <- grep("warrant",names(bond4),ignore.case=TRUE)
names(bond4)[n] <- "Warrant.No"

# dob.1, booked.date.1, release.date.1 all need to be in same format
convert_date <- function(date_vec,fmt="%m/%d/%Y") {
  d <- as.Date(date_vec,format=fmt)
  return(format(d,"%Y-%m-%d"))
}
bond2$DOB.1 <- convert_date(bond2$DOB.1)
bond2$Booked.Date.1 <- convert_date(bond2$Booked.Date.1)
bond2$Release.Date.1 <- convert_date(bond2$Release.Date.1)

bond3$DOB.1 <- convert_date(bond3$DOB.1)
bond3$Booked.Date.1 <- convert_date(bond3$Booked.Date.1)
bond3$Release.Date.1 <- convert_date(bond3$Release.Date.1)

bond4$DOB.1 <- convert_date(bond4$DOB.1)
bond4$Booked.Date.1 <- convert_date(bond4$Booked.Date.1)
bond4$Release.Date.1 <- convert_date(bond4$Release.Date.1)



# combine other offenses
bond2 <- cbind("Larceny",bond2)
names(bond2)[1] <- "Offense"
bond3 <- cbind("Meth/Cocaine",bond3)
names(bond3)[1] <- "Offense"
bond4 <- cbind("Meth/Cocaine 2nd off",bond4)
names(bond4)[1] <- "Offense"

bond_all <- rbind(bond1,bond2,bond3,bond4)

write_csv(bond_all,"all_bonds_raw.csv")

# for main group, which is the one Mike Owens developed
# remove some extraneous data from main_group
main_group <- main_group[1:92,]

# write out to CSV file
write_csv(main_group,"main_group.csv")

rm(bond_2012,bond_2013,bond_2014,bond1,bond2,bond3,bond4,n)

# data munging ------------------------------------------------------------

# define functions

bond_type <- function(text) {
  sub("^.*? ?(\\w+) [Bb]ond of .+$","\\1",text,perl=TRUE)
}

bond_amount <- function(text) {
  suppressWarnings(as.numeric(sub(".*[bB]ond of \\$([0-9,.]+) .*","\\1",text,perl=TRUE)))
}

bond_paid <- function(text) {
  # Paid bonds are marked with (PD) near the beginning, unpaid with (UNPD)
  pd_unpd <- sub("^\\s*\\((PD|UNPD)\\).*$","\\1",text,perl=TRUE)
  return(ifelse(pd_unpd=="PD",TRUE,FALSE))
}

other_charge <- function(text) {
  grepl("Sentenced|Dismissed on",text,perl=TRUE)
}

disposition <- function(text) {
  text2 <- ifelse(grepl("Sentenced",text,fixed=TRUE),"Sentenced",
    ifelse(grepl("Dismissed",text,fixed=TRUE),"Dismissed",
    ifelse(grepl("Cash Bond",text,fixed=TRUE),"Cash or Surety Bond",
    ifelse(grepl("Released on own Recognizance",text,fixed=TRUE),"Personal Recognizance","Other"))))
  return(text2)
}
sentence_amount <- function(text) {
  sub("^ *Sentenced on \\d{1,2}/\\d{1,2}/\\d{2,4} to +(\\d+ +yrs|\\d+ +mos|\\d+ +dys|Time Served|Probation|Youth Offender Act)$","\\1",text,perl=TRUE)
}

# apply functions
bond_all$Bond.Amount <- bond_amount(bond_all$Disposition.Description)
bond_all$Bond.Type <- bond_type(bond_all$Disposition.Description)
bond_all$Bond.Type[is.na(bond_all$Bond.Amount)] <- ""
bond_all$Disposition.Category <- disposition(bond_all$Disposition.Description)
bond_all$Sentence.Amount <- sentence_amount(bond_all$Disposition.Description)
bond_all$Sentence.Amount[bond_all$Disposition.Category!="Sentenced"] <- ""
bond_all$Disposition.Simple <- ifelse(bond_all$Disposition.Category=="Sentenced","Guilty","Not Guilty")
bond_all$Age <- as.numeric(with(bond_all,Booked.Date.1-DOB.1))/365.25
bond_all$Public.Defender <- ifelse(grepl("public def",bond_all$Attorney,ignore.case = TRUE),"Yes","No")
bond_all$Bond.Paid <- bond_paid(bond_all$Disposition.Description)
bond_all$Other.Charge <- other_charge(bond_all$Disposition.Description)

# merge on court data -----------------------------------------------------

bond_combined <- bond_all %>% 
  left_join(court_data %>% rename(Warrant.No=`Case Number`) %>% mutate(Warrant.No=as.character(Warrant.No))) %>% 
  rename(Case.Status=`Case Status`) %>% 
  mutate(Disposition.Simple=ifelse(grepl("Guilty",Case.Status,fixed=TRUE,ignore.case=FALSE),"Guilty","Not Guilty"))

# public defender

pd <- case_parties %>% 
  select(Case_Number,`Party Type`) %>% 
  filter(`Party Type` %in% c("Public Defender","Defendant Attorney")) %>% 
  group_by(Case_Number) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(Public_Defender=ifelse(`Party Type`=="Public Defender","Yes","No")) %>% 
  select(-one_of("Party Type")) %>% 
  rename(Warrant.No=Case_Number)

bond_combined <- bond_combined %>% 
  left_join(pd,by="Warrant.No")

# subset on single name ---------------------------------------------------

bond_all2 <- bond_all %>% 
  group_by(Name) %>% 
  slice(1)

bond_all3 <- bond_all %>% 
  group_by(ID.Pack) %>% 
  slice(1)

bond_combined2 <- bond_combined %>% 
  filter(Case.No!='UNDERSENT') %>% 
  group_by(Offense,ID.Pack) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(Offense,Year)

# save data ---------------------------------------------------------------



save(bond_all,bond_all2,bond_all3,bond_combined,bond_combined2,main_group,file="bond.RData")
