library(tidyverse)

library(readr)
cfs_at_schools_since_2018 <- read_csv("cfs_at_schools_since_2018.csv")
View(cfs_at_schools_since_2018)

dps <- cfs_at_schools_since_2018

# This is calls for service data from Denver Police for calls to police at Denver Public School campuses.
  # Data runs from 2018 to 03/09/23. 
  # From Andrea Webber, records with DPD:	
    # via Email: "Please remember that due to multiple schools sharing the same campus, we are not able to easily distinguish which specific school is being responded to, so all schools at the same campus will be listed for each call at a campus."
    # via text: "'Special assignment' means crowd control events such as parades, protests, etc"

############### Number of calls per year? ###############

  # Pull out unique cases

  dps %>% 
  distinct(master_incident_number, .keep_all = TRUE)

    # Down to 32,039 rows. 
      # Same with SQL, 32,039 rows. 

      # CREATE TABLE "filtered_dps" as
      # SELECT DISTINCT(master_incident_number), year
      # from dps
  
  filtered_dps <- dps %>% 
  distinct(master_incident_number, .keep_all = TRUE)

  filtered_dps %>% 
    group_by(year) %>% 
    summarize(count = n())
  
    # The number of calls to DPD from DPS is slightly up, but down from a high of 10,509 calls in 2018. 
  
       #  year count
       #  2018  10,509
       #  2019  9,522
       #  2020  3,469
       #  2021  3,352
       #  2022  4,013
       #  2023  1,174
  
    # Findings matched in SQL:
  
        # SELECT year, COUNT(YEAR)
        # from filtered_dps
        # group by year
  

  # Basic line graph to map it out:
    
      calls_year <- filtered_dps %>% 
        group_by(year) %>% 
        summarize(count = n())
      
      ggplot(calls_year) +
        geom_line(mapping = aes(x = year, y = count))

      # More calls to DPD in 2022 than 2020 and 2021, but waaaaaaay less than in 2018 and 2019. 
      
#### priority 1 and 2 calls ####
  
  # I am curious if the numbers change for some of the higher priority calls - 1 and 2 
      
      filtered_dps_0 <- filtered_dps %>% 
        filter(priority_number == "0") 
      
      filtered_dps_1 <- filtered_dps %>% 
        filter(priority_number == "1") 
          
      filtered_dps_2 <- filtered_dps %>% 
        filter(priority_number == "2") 
      
      
      filtered_dps_1_2 <- filtered_dps_2 %>% full_join(filtered_dps_1)
      
      filtered_dps_0_2 <- filtered_dps_0 %>% full_join(filtered_dps_1_2)
          
      # 4,072 rows 
        # Same in SQL, 4,026 rows. 
          # select * 
          #  from filtered_dps_2
          # where priority_number = "1"
          # or priority_number = "2"
     
      filtered_dps_0_2 %>% 
        group_by(year) %>% 
        summarize(count = n())  
     
      # Police are called for more serious incidents at about the same rate before SRO's were pulled
          
      
      # year count
      # 2018  1132
      #  2019   930
      #  2020   377
      #  2021   607
      #  2022   854
      #  2023   172
      
      # Echo'd in SQL:
          # CREATE TABLE "filtered_dps_2" as
          # SELECT DISTINCT(master_incident_number), year, priority_number
          # from dps
          
      # SELECT year, count(year)
      # from filtered_dps_2
      # where priority_number = "1"
      # or priority_number = "2"
      # or priority_number = "0"
      # group by year
      
      
    # Charting the data:
      
      calls_year_0_2 <- filtered_dps_0_2 %>% 
        group_by(year) %>% 
        summarize(count = n())
      
      ggplot(calls_year_0_2) +
        geom_line(mapping = aes(x = year, y = count))

###### what are the calls typically for? ##########

   # higher priority:    
        
      filtered_dps_0_2 %>% 
        group_by(problem) %>% 
        summarize(count = n()) %>% 
        View()
      
      # Looks like the more serious calls are typically for a suicidal person, fight or weapons calls. 
      
        # CREATE TABLE "problem" as
        # SELECT DISTINCT(master_incident_number), problem, priority_number
        # from dps
        
        # select problem, count(problem)
        # from problem
        # where priority_number = "1"
          # or priority_number = "2"
          # or priority_number = "0"
        # group by problem
        # order by count(problem) DESC
      
      # all calls
      
        filtered_dps %>% 
          group_by(problem) %>% 
          summarize(count = n()) %>% 
          View()
       
      # Mostly calls for special assignments like parades, self-initiated actions and assault. 
       
      # Echo'd in SQL
      
        # select problem, count(problem)
        # from problem
        # group by problem
        # order by count(problem) DESC
      
      
########## Where are most of the calls going to ? #######################
  # I believe there are repeats within the data if the exact school within the campus is not known. So using addresses for grouping will be the most accurate method

# More serious
        
        filtered_dps_0_2 %>% 
          group_by(cfs_address) %>% 
          summarize(count = n()) %>% 
          View()
        
        # Top 5: 
        # 5000 N Crown Blvd	107
        # 2960 N Speer Blvd	103
        # 951 N Elati St	96
        # 1820 N Lowell Blvd	95
        # 4800 N Telluride St	94
        
      # Echo'd by SQL: 
        #  CREATE TABLE "call_address_0_2" as
        #  SELECT DISTINCT(master_incident_number), cfs_address, priority_number, problem
        #   from dps
        #  where priority_number = "1"
        #  or priority_number = "2"
        # or priority_number = "0"
        
        # SELECT cfs_address, count(cfs_address)
        # from call_address_1_2
        # group by cfs_address
        # order by count(cfs_address) DESC
        
        
    # Montbello 
        
       montbello <- filtered_dps_1_2 %>% 
          filter(cfs_address == "5000 N Crown Blvd")
       
       montbello %>% 
         group_by(cde_school_name) %>% 
         summarize(count = n()) %>% 
         View()
       
          # just showing Montbello HS which is different than SQL. I think it's because he distinct function removed others
       
           montbello_all <- dps %>% 
              filter(cfs_address == "5000 N Crown Blvd")
              
           montbello_all %>% 
             filter(priority_number == "1"|priority_number == "2") %>% 
             View()
           
           # That worked. Shows other schools within the data
           
          montbello_1_2 <- montbello_all %>% 
             filter(priority_number == "1"|priority_number == "2")
        
          montbello_1_2 %>%  
          group_by(cde_school_name) %>% 
            summarize(count = n()) %>% 
            View()
          
          # Now echos SQL. Possible school response options include:
              # DCIS at Montbello            
              # Noel Community Arts School   
              # STRIVE Prep - Montbello 
           # SQL process:
              # CREATE TABLE "call_address_1_2_2" as
              # SELECT DISTINCT (master_incident_number), cfs_address, priority_number, problem, cde_school_name
              # from dps
              # where priority_number = "1"
              # or priority_number = "2"
          
              # SELECT DISTINCT (master_incident_number), cde_school_name
              # from call_address_1_2_2
              # where cfs_address = "5000 N Crown Blvd"
              # group by cde_school_name
          
          
# All calls        
        
        
        