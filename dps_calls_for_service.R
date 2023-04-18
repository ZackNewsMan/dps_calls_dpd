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
          
          
########## high priority calls to just schools who lost a SRO ########
   # List of campuses: https://www.dpsk12.org/sro-transition/schools-transitioning/ 
   # All out by end of 2021, some out by 2020    
          
     # Bruce Randolph: 
          # 3955 STEELE ST
     
        sro_1 <- filtered_dps_0_2 %>% 
          filter(school_address == "3955 STEELE ST")
          
     # DMLK Campus -- Dr. Martin Luther King Jr. early College
        # 19535 E 46TH AVE
          
        sro_2 <- filtered_dps_0_2 %>% 
          filter(school_address == "19535 E 46TH AVE")

     # DSST Cole High School	
        # 3240 HUMBOLDT ST
        
        sro_3 <- filtered_dps_0_2 %>% 
          filter(school_address == "3240 HUMBOLDT ST")
     
     #  East Campus -- East High School
        # 1600 CITY PARK ESPLANADE
        
        sro_4 <- filtered_dps_0_2 %>% 
          filter(school_address == "1600 CITY PARK ESPLANADE")
        
      # Evie Dennis Campus: Soar at Green Valley Ranch, DSST at Green Valley Ranch Middle & High School, Strive Prep Green Valley Ranch, Vista Academy
        # 4800 TELLURIDE ST #2
        
        sro_5 <- filtered_dps_0_2 %>% 
          filter(school_address == "4800 TELLURIDE ST #2")
        
      # George Washington Campus - George Washington High School, Delta
        # 655 S MONACO PKWY
        
        sro_6 <- filtered_dps_0_2 %>% 
          filter(school_address == "655 S MONACO PKWY")   
        
      # Henry Campus --   Bear Valley International School (BVIS), DSST Henry Middle School
        # 3005 S GOLDEN WAY
        
        sro_7 <- filtered_dps_0_2 %>% 
          filter(school_address == "3005 S GOLDEN WAY")
        
      # John F. Kennedy Campus
        # 2855 S LAMAR ST
        
        sro_8 <- filtered_dps_0_2 %>% 
          filter(school_address == "2855 S LAMAR ST")
       
      # Kepner Campus - 	Strive Prep Kepner, Kepner Beacon Middle School, Rocky Mountain Prep
        # 911 S HAZEL CT
        
        sro_9 <- filtered_dps_0_2 %>% 
          filter(school_address == "911 S HAZEL CT")
        
      # Kunsmiller Campus -- Kunsmiller Elementary, Middle, High School
        # 2250 S QUITMAN WAY
        
        sro_10 <- filtered_dps_0_2 %>% 
          filter(school_address == "2250 S QUITMAN WAY")
         
      #  Lake Campus -- Lake, Strive Prep-Lake
        # 1820 LOWELL BLVD
        
        sro_11 <- filtered_dps_0_2 %>% 
          filter(school_address == "1820 LOWELL BLVD")
        
      #  Lincoln Campus -- Abraham Lincoln High School, Respect Academy, Compass Academy
        # 2285 S FEDERAL BLVD
        
        sro_12 <- filtered_dps_0_2 %>% 
          filter(school_address == "2285 S FEDERAL BLVD")
      
      # Manual Campus -- Manual High School, McAuliffe Manual Middle School
        # 1700 E 28TH AVE
        
        sro_13 <- filtered_dps_0_2 %>% 
          filter(school_address == "1700 E 28TH AVE")
        
      #  Montbello Campus -- DCIS-Montbello Middle and High School, Noel Community Arts School (NCAS), Strive Prep Montbello
        # 5000 CROWN BLVD
        
        sro_14 <- filtered_dps_0_2 %>% 
          filter(school_address == "5000 CROWN BLVD")
        
      #  North Campus -- North High School, North Engagement Center
        # 2960 N SPEER BLVD
        
        sro_15 <- filtered_dps_0_2 %>% 
          filter(school_address == "2960 N SPEER BLVD")
        
      # Northfield Campus  -- Northfield High School, DSST Conservatory Green High School
       # 5500 N CENTRAL PARK BLVD
        
        sro_16 <- filtered_dps_0_2 %>% 
          filter(school_address == "5500 N CENTRAL PARK BLVD")
      
      # South Campus --  South High School  
        # 1700 E LOUISIANA AVE
        
        sro_17 <- filtered_dps_0_2 %>% 
          filter(school_address == "1700 E LOUISIANA AVE")
        
      # Thomas Jefferson Campus -- 	Thomas Jefferson High School  
        # 3950 S HOLLY ST
             
        sro_18 <- filtered_dps_0_2 %>% 
          filter(school_address == "3950 S HOLLY ST")
        
      # West Campus -- West Early College (WEC), West Leadership Academy (WLA)
        # 951 ELATI ST
          
        sro_19 <- filtered_dps_0_2 %>% 
          filter(school_address == "951 ELATI ST")

 ### Now let's put all the SRO calls together 
        
        
       sro_1_2 <- sro_2 %>% full_join(sro_1)
       
       sro_1_3 <- sro_3 %>% full_join(sro_1_2)
        
        sro_1_4 <- sro_4 %>% full_join(sro_1_3)
        
        sro_1_5 <- sro_5 %>% full_join(sro_1_4)
        
        sro_1_6 <- sro_6 %>% full_join(sro_1_5)
        
        sro_1_7 <- sro_7 %>% full_join(sro_1_6)
        
        sro_1_8 <- sro_8 %>% full_join(sro_1_7)
        
        sro_1_9 <- sro_9 %>% full_join(sro_1_8)
        
        sro_1_10 <- sro_10 %>% full_join(sro_1_9)
        
        sro_1_11 <- sro_11 %>% full_join(sro_1_10)
        
        sro_1_12 <- sro_12 %>% full_join(sro_1_11)
        
        sro_1_13 <- sro_13 %>% full_join(sro_1_12)
        
        sro_1_14 <- sro_14 %>% full_join(sro_1_13)
        
        sro_1_15 <- sro_15 %>% full_join(sro_1_14)
        
        sro_1_16 <- sro_16 %>% full_join(sro_1_15)
        
        sro_1_17 <- sro_17 %>% full_join(sro_1_16)
        
        sro_1_18 <- sro_18 %>% full_join(sro_1_17)
        
        sro_1_19 <- sro_19 %>% full_join(sro_1_18)
      
        # 1,331 rows in sro_1_19 
         # Same in SQL, 1331 rows
        
        
        # CREATE TABLE "filtered_dps" as
        # SELECT DISTINCT(master_incident_number), year, priority_number, school_address
        # from dps
        
        # CREATE TABLE "call_address_0_2" as
        # SELECT DISTINCT(master_incident_number), school_address, priority_number, problem, year
        # from dps
        # where priority_number = "1"
        # or priority_number = "2"
        # or priority_number = "0"
    
        # CREATE TABLE "sro_1_19" as
        # SELECT *
        #   FROM call_address_0_2
        # WHERE school_address = "3955 STEELE ST"
        # or school_address = "19535 E 46TH AVE"
        # or school_address = "3240 HUMBOLDT ST"
        # or school_address = "1600 CITY PARK ESPLANADE"
        # or school_address = "4800 TELLURIDE ST #2"
        # or school_address = "655 S MONACO PKWY"
        # or school_address = "3005 S GOLDEN WAY"
        # or school_address = "2855 S LAMAR ST"
        # or school_address = "911 S HAZEL CT"
        # or school_address = "2250 S QUITMAN WAY"
        # or school_address = "1820 LOWELL BLVD"
        # or school_address = "2285 S FEDERAL BLVD"
        # or school_address = "1700 E 28TH AVE"
        # or school_address = "5000 CROWN BLVD"
        # or school_address = "2960 N SPEER BLVD"
        # or school_address = "5500 N CENTRAL PARK BLVD"
        # or school_address = "1700 E LOUISIANA AVE"
        # or school_address = "3950 S HOLLY ST"
        # or school_address = "951 ELATI ST"
        
# Alright after all of that -- how many per year?
        
        sro_1_19 %>% 
        group_by(year) %>% 
          summarize(count = n())
        
        # Even at campuses that specifically lost SRO's, police were still called at around the same rate for urgent calls after losing them in 2020 and 2021 as they were before. 
          # Urgent = priority numbers 0,1 and 2.    
                  
              # year count
              #  2018   328
              #  2019   295
              #  2020   105
              #  2021   232
              #  2022   306
              #  2023    65
        
        # Findings echo'd in SQL:
        
            # SELECT year, count(year)
            # from sro_1_19
            # group by year
            
        
      # Let's map it:  
        
        calls_year_sro_0_2 <- sro_1_19 %>% 
          group_by(year) %>% 
          summarize(count = n())
        
        ggplot(calls_year_sro_0_2) +
          geom_line(mapping = aes(x = year, y = count))  
        
        
   # Publish underlying data:
        
        sro_1_19 %>% write_csv("sro_1_19.csv", na = "")
  
  # Group by campus
        
        sro_1_19 %>% 
          group_by(school_address) %>% 
          summarize(count = n()) %>% 
          View()
        
        # by school and year
        
        sro_1_19 %>% 
          group_by(school_address, year) %>% 
          summarize(count = n()) %>% 
          View()
        
        # Verified in SQL here:
          # SELECT school_address, year, count(school_address)
          # from sro_1_19
          # group by school_address, year
          
      # Gfx made here:   
        # https://public.flourish.studio/visualisation/13367866/
        
  # What were the problems that cops were called for at the SRO schools?
        
        sro_1_19 %>% 
          group_by(problem) %>% 
          summarize(count = n()) %>% 
          View()
        
        # Most of the more urgent calls at former-SRO schools were seeking help for fights, a suicidal person or weapons.
        
        
             
### all calls to SRO schools ####
  # trying to identify the percentage of calls that were high priority. so need to look at all of the calls going to the schools      
        
        # Bruce Randolph: 
        # 3955 STEELE ST
        
        all_sro_1 <- filtered_dps %>% 
          filter(school_address == "3955 STEELE ST")
        
        # DMLK Campus -- Dr. Martin Luther King Jr. early College
        # 19535 E 46TH AVE
        
        all_sro_2 <- filtered_dps %>% 
          filter(school_address == "19535 E 46TH AVE")
        
        # DSST Cole High School	
        # 3240 HUMBOLDT ST
        
        all_sro_3 <- filtered_dps %>% 
          filter(school_address == "3240 HUMBOLDT ST")
        
        #  East Campus -- East High School
        # 1600 CITY PARK ESPLANADE
        
        all_sro_4 <- filtered_dps %>% 
          filter(school_address == "1600 CITY PARK ESPLANADE")
        
        # Evie Dennis Campus: Soar at Green Valley Ranch, DSST at Green Valley Ranch Middle & High School, Strive Prep Green Valley Ranch, Vista Academy
        # 4800 TELLURIDE ST #2
        
        all_sro_5 <- filtered_dps %>% 
          filter(school_address == "4800 TELLURIDE ST #2")
        
        # George Washington Campus - George Washington High School, Delta
        # 655 S MONACO PKWY
        
        all_sro_6 <- filtered_dps %>% 
          filter(school_address == "655 S MONACO PKWY")   
        
        # Henry Campus --   Bear Valley International School (BVIS), DSST Henry Middle School
        # 3005 S GOLDEN WAY
        
        all_sro_7 <- filtered_dps %>% 
          filter(school_address == "3005 S GOLDEN WAY")
        
        # John F. Kennedy Campus
        # 2855 S LAMAR ST
        
        all_sro_8 <- filtered_dps %>% 
          filter(school_address == "2855 S LAMAR ST")
        
        # Kepner Campus - 	Strive Prep Kepner, Kepner Beacon Middle School, Rocky Mountain Prep
        # 911 S HAZEL CT
        
        all_sro_9 <- filtered_dps %>% 
          filter(school_address == "911 S HAZEL CT")
        
        # Kunsmiller Campus -- Kunsmiller Elementary, Middle, High School
        # 2250 S QUITMAN WAY
        
        all_sro_10 <- filtered_dps %>% 
          filter(school_address == "2250 S QUITMAN WAY")
        
        #  Lake Campus -- Lake, Strive Prep-Lake
        # 1820 LOWELL BLVD
        
        all_sro_11 <- filtered_dps %>% 
          filter(school_address == "1820 LOWELL BLVD")
        
        #  Lincoln Campus -- Abraham Lincoln High School, Respect Academy, Compass Academy
        # 2285 S FEDERAL BLVD
        
        all_sro_12 <- filtered_dps %>% 
          filter(school_address == "2285 S FEDERAL BLVD")
        
        # Manual Campus -- Manual High School, McAuliffe Manual Middle School
        # 1700 E 28TH AVE
        
        all_sro_13 <- filtered_dps %>% 
          filter(school_address == "1700 E 28TH AVE")
        
        #  Montbello Campus -- DCIS-Montbello Middle and High School, Noel Community Arts School (NCAS), Strive Prep Montbello
        # 5000 CROWN BLVD
        
        all_sro_14 <- filtered_dps %>% 
          filter(school_address == "5000 CROWN BLVD")
        
        #  North Campus -- North High School, North Engagement Center
        # 2960 N SPEER BLVD
        
        all_sro_15 <- filtered_dps %>% 
          filter(school_address == "2960 N SPEER BLVD")
        
        # Northfield Campus  -- Northfield High School, DSST Conservatory Green High School
        # 5500 N CENTRAL PARK BLVD
        
        all_sro_16 <- filtered_dps %>% 
          filter(school_address == "5500 N CENTRAL PARK BLVD")
        
        # South Campus --  South High School  
        # 1700 E LOUISIANA AVE
        
        all_sro_17 <- filtered_dps %>% 
          filter(school_address == "1700 E LOUISIANA AVE")
        
        # Thomas Jefferson Campus -- 	Thomas Jefferson High School  
        # 3950 S HOLLY ST
        
        all_sro_18 <- filtered_dps %>% 
          filter(school_address == "3950 S HOLLY ST")
        
        # West Campus -- West Early College (WEC), West Leadership Academy (WLA)
        # 951 ELATI ST
        
        all_sro_19 <- filtered_dps %>% 
          filter(school_address == "951 ELATI ST") 
        
        
        
     # Let's merge em    
        
        
        
        all_sro_1_2 <- all_sro_2 %>% full_join(all_sro_1)
        
        all_sro_1_3 <- all_sro_3 %>% full_join(all_sro_1_2)
        
        all_sro_1_4 <- all_sro_4 %>% full_join(all_sro_1_3)
        
        all_sro_1_5 <- all_sro_5 %>% full_join(all_sro_1_4)
        
        all_sro_1_6 <- all_sro_6 %>% full_join(all_sro_1_5)
        
        all_sro_1_7 <- all_sro_7 %>% full_join(all_sro_1_6)
        
        all_sro_1_8 <- all_sro_8 %>% full_join(all_sro_1_7)
        
        all_sro_1_9 <- all_sro_9 %>% full_join(all_sro_1_8)
        
        all_sro_1_10 <- all_sro_10 %>% full_join(all_sro_1_9)
        
        all_sro_1_11 <- all_sro_11 %>% full_join(all_sro_1_10)
        
        all_sro_1_12 <- all_sro_12 %>% full_join(all_sro_1_11)
        
        all_sro_1_13 <- all_sro_13 %>% full_join(all_sro_1_12)
        
        all_sro_1_14 <- all_sro_14 %>% full_join(all_sro_1_13)
        
        all_sro_1_15 <- all_sro_15 %>% full_join(all_sro_1_14)
        
        all_sro_1_16 <- all_sro_16 %>% full_join(all_sro_1_15)
        
        all_sro_1_17 <- all_sro_17 %>% full_join(all_sro_1_16)
        
        all_sro_1_18 <- all_sro_18 %>% full_join(all_sro_1_17)
        
        all_sro_1_19 <- all_sro_19 %>% full_join(all_sro_1_18)
        
        
 # How many of all calls were to schools that lost a SRO?
        
        all_sro_1_19 %>% 
          group_by(year) %>% 
          summarize(count = n())
        
       # The number of overall calls went up in 2022 compared to years prior, but down from highs in 2018 and 2019. 
        
       # year count
        #  2018 5009
        #  2019  4725
        #  2020  1570
        #  2021  1541
        #  2022  1635
        #  2023   554
         
     # Yearly counts echo'd in SQL:    
        # CREATE TABLE "filtered_dps" as
        # SELECT DISTINCT(master_incident_number), year, priority_number, school_address
        # from dps
        
        # CREATE TABLE "all_sro_1_19" as
        # SELECT *
        #  FROM filtered_dps
        # WHERE school_address = "3955 STEELE ST"
        # or school_address = "19535 E 46TH AVE"
        # or school_address = "3240 HUMBOLDT ST"
        # or school_address = "1600 CITY PARK ESPLANADE"
        # or school_address = "4800 TELLURIDE ST #2"
        # or school_address = "655 S MONACO PKWY"
        # or school_address = "3005 S GOLDEN WAY"
        # or school_address = "2855 S LAMAR ST"
        # or school_address = "911 S HAZEL CT"
        # or school_address = "2250 S QUITMAN WAY"
        # or school_address = "1820 LOWELL BLVD"
        # or school_address = "2285 S FEDERAL BLVD"
        # or school_address = "1700 E 28TH AVE"
        # or school_address = "5000 CROWN BLVD"
        # or school_address = "2960 N SPEER BLVD"
        # or school_address = "5500 N CENTRAL PARK BLVD"
        # or school_address = "1700 E LOUISIANA AVE"
        # or school_address = "3950 S HOLLY ST"
        # or school_address = "951 ELATI ST"
        
        # select year, count(year)
        # from all_sro_1_19
        # group by year
        
        
        
        
        
        
        
        
        
        
        
        