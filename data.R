# ---------- Explanation --------------------------------------------------------------------

# The script has several dataframes
  # x_data_gizmo (raw data from SurveyGizmo - not used for analysis)
  # data (clean and filtered survey data)

# dataframes according to level of analysis
  # data ->         individual level (one row per participant)
  # data_modules -> module level (one row per module)
  # data_courses -> course level (one row per course)
  # data_years ->   yearly summary (one row per year)
  # data_categories -> average scores from all courses across all years (by course category)

# ---------- Packages --------------------------------------------------------------------

# Packages which are needed 
  # install.packages("devtools")
  # library(devtools)                     
    # install.packages("Rsurveygizmo")  
  # install.packages("tidyverse")
  # install.packages("Hmisc")

  library(Rsurveygizmo)
  library(tidyverse)
  library(Hmisc)

# ----------- x_data_gizmo: Retrieved complete survey data (from SurveyGizmo or file) -------

# Retrieve survey data from excel file (SAMPLE DATA)
  library(readxl)
  x_data_gizmo <- read_excel("SampleData.xlsx")

    # ALTERNATIVE:
      # Retrieve survey data from SurveyGizmo server (via API)
        # x_data_gizmo <- pullsg(surveyid =  ,  # insert your surveyid        
                              #  api =  ,        # insert your api
                              #  completes_only = TRUE) 

# ----------- data: Clean and filtered survey data -------------------------------------------

# Create new dataframe
  data <- x_data_gizmo

# Replace missing values
  
  # replaces blanks with missing values (NA)
    for (i in seq_along(data)){
    if (class(data[[i]])[1]=="POSIXct") {data[[i]] = as.character(data[[i]])} 
    data[[i]][data[[i]]==""]<-NA           #loop, puts NA in all blanks
    }

    # replaces "Not applicable" with missing values (NA)
      for (i in seq_along(data)){
      if (class(data[[i]])[1]=="POSIXct") {data[[i]] = as.character(data[[i]])} 
      data[[i]][data[[i]]=="Not applicaple"]<-NA           
      }

    # puts blanks back in for module_code,location & lecturer (replace NAs with blanks)   
      data$urlmodulecode[is.na(data$urlmodulecode)==TRUE] <- ""  
      data$urllocation[is.na(data$urllocation)==TRUE] <- ""
      data$urllecturer[is.na(data$urllecturer)==TRUE] <- ""
      
# VARIABLES : SELECT, RENAME, CLASS CHANGE, RECODE & CREATION OF NEW VARIABLES

  # Select variables
    data <- dplyr::select(data,
                    'part_code','origin','gender','age',            
                    'urlcoursecode','urlcourseid','urlcoursetitle','urlstart','urlend',
                    'urlcoursecat','urlmodulecode','urlmoduletitle',
                    'urlnpart','urllocation','urllecturer',
                    "Q_dif1","Q_dif2",
                    "Q_rel1","Q_rel2","Q_rel3","OA_rel",
                    "Q_skill1","Q_skill2","Q_skill3",
                    "Q_flec1","Q_flec2","Q_flec3","Q_flec4","Q_flec5","Q_flec6","Q_flec7","OA_flec",
                    "Q_fmat1","Q_fmat2","OA_fmat",
                    "Q_ffac1","Q_ffac2","Q_ffac3", "Q_ffac4","OA_ffac",
                    "Q_elcon1","Q_elcon2","Q_elcon3","Q_elcon4","Q_elcon5","Q_elcon6","OA_elcon",
                    "Q_elpla1","Q_elpla2","Q_elpla3","OA_elpla",
                    "Q_elvis1","Q_elvis2","Q_elvis3","OA_elvis",
                    "elacceptance","OA_elexpec",
                    "OA_strength","OA_weakness","OA_com") 

  # Changes variable classes 
    # changes class to numeric (all questionnaire items)
      data <- mutate_at(data, 
                        vars("urlnpart","Q_dif1","Q_dif2","Q_rel1","Q_rel2","Q_rel3",
                            "Q_skill1","Q_skill2","Q_skill3",
                            "Q_flec1","Q_flec2","Q_flec3","Q_flec4","Q_flec5","Q_flec6","Q_flec7",
                            "Q_fmat1","Q_fmat2","Q_ffac1","Q_ffac2","Q_ffac3","Q_ffac4",
                            "Q_elcon1","Q_elcon2","Q_elcon3","Q_elcon4","Q_elcon5","Q_elcon6",
                            "Q_elpla1","Q_elpla2","Q_elpla3","Q_elvis1","Q_elvis2","Q_elvis3"),
                        as.numeric)  
            

    # Changes class to date (for all dates)
      data <- mutate_at(data,
                      vars("urlstart","urlend"),
                      as.Date)

    # Changes class to factor 
      data <- mutate_at(data, 
                        vars("elacceptance","urllecturer","urllocation","urlcourseid"),
                        as.factor)
        
      data$urlcoursecat <- factor(data$urlcoursecat, levels=c("long", "short","elearning"))
        


  # Changes variable names (for url variables and difficulty items)
    data <- dplyr::rename(data,
                          course_code = 'urlcoursecode',
                          module_code = 'urlmodulecode',
                          courseID = "urlcourseid",  
                          course_title = 'urlcoursetitle',
                          module_title = 'urlmoduletitle',
                          course_cat = 'urlcoursecat',
                          startdate = 'urlstart',
                          enddate = 'urlend',
                          location = 'urllocation',
                          lecturer = 'urllecturer',
                          npart = 'urlnpart',
                          lQ_dif1 = 'Q_dif1',
                          lQ_dif2 = 'Q_dif2'
                          )
    
  # Recode (all questionnaire items)
    # Original scores (1-6) transposed to new scale (0-100)
    
      # function for transposion 
        transpose <- function(x) {(x-1)*20}
    
      # Transpose all questionnaire items
        data <- data %>%  
          mutate_at(vars(starts_with("Q_")), funs(transpose))  # all questionnaire item names start with "Q_"
      
  # Creates new variables, for additional course data (duration, year & full module code)
    # Duration & year 
      data <- data %>%   
              mutate(duration = enddate - startdate) %>%
              mutate(year = lubridate::year(startdate))

    # Full module code (attaches the module code to the course code, "course_module")
      data <- mutate(data,
                    course_module=str_c(course_code, module_code, sep="_"))
      # Change class to factor
        as.factor(data$course_module)

# ----------------- data: individual level (one row per participant) ---------------------

# DIMENSIONS (MEAN SCORES OF SURVEY ITEMS)

  # Mean scores for: relevance, teaching quality, facilities, difficulty, skill development)
    data <- data %>% 
            mutate(
              # relevance
                relevance = rowMeans(data[,c("Q_rel1","Q_rel2","Q_rel3")]),
              # lecturer performance (Teaching Qualitiy) - face-to-face
                TQlect_f = rowMeans(data[,c("Q_flec1","Q_flec2","Q_flec3","Q_flec4","Q_flec5", 
                                     "Q_flec6","Q_flec7")]),
              # course material (Teaching Qualitiy) - face-to-face
                TQmat_f = rowMeans(data[,c("Q_fmat1","Q_fmat2")]),
              # course content (Teaching Qualitiy) - elearing
                TQcon_el = rowMeans(data[,c("Q_elcon1","Q_elcon2","Q_elcon3", 
                                     "Q_elcon4","Q_elcon5","Q_elcon6")]),
              # course visual outlook, usability (Teaching Qualitiy) - elearning
                TQvis_el = rowMeans(data[,c("Q_elvis1","Q_elvis2","Q_elvis3")]),
              # satisfaction with facilities - face-to-face
                facil_f = rowMeans(data[,c("Q_ffac1","Q_ffac2","Q_ffac3","Q_ffac4")],na.rm=TRUE),
              # satisfaction with facilities (el platform) - elearning
                plat_el = rowMeans(data[,c("Q_elpla1","Q_elpla2","Q_elpla3")]),
              # difficulty of the course
                difficulty = rowMeans(data[,c("lQ_dif1","lQ_dif2")]),
              # perceived skill development
                skills = rowMeans(data[,c("Q_skill1","Q_skill2","Q_skill3")])
                  )

  # Mean score for: Overall Teaching quality (face-to-face & elearning)
    data <- data %>%  
            mutate(
                  TQ_f = rowMeans(data[,c("TQlect_f","TQmat_f")]),
                  TQ_el = rowMeans(data[,c("TQcon_el","TQvis_el")])
                  )

#----------- data_modules: module level dataframe (one row per module) -------------------

# AVERAGE SCORES FOR MODULES (ACROSS PARTICIPANTS FROM THAT MODULE)
    
  # Create dataframe with averaged numeric variables 
    data_modules <- data %>%            
        group_by(course_module) %>%
        summarise_if(is.numeric, mean, na.rm=TRUE) %>%
          select(-"year")     # removes the averaged year

  # new variable for number of responses
    data_modules <- data_modules %>%
        full_join(count(data, course_module), by="course_module") %>%
          dplyr::rename(nresp='n')    

  # new variables that calculates reponse rate (rounded)
    data_modules$responserate <- round((data_modules$nresp / data_modules$npart),2)

  # new variable with labeled groups for level of difficulty
    data_modules$lev_difficulty <- NA
    data_modules$lev_difficulty[data_modules$difficulty < 1.5] = "very easy"
    data_modules$lev_difficulty[data_modules$difficulty >= 1.5 & data_modules$difficulty < 2.5] = "easy"
    data_modules$lev_difficulty[data_modules$difficulty >= 2.5 & data_modules$difficulty < 3.5] = "somewhat easy"
    data_modules$lev_difficulty[data_modules$difficulty >= 3.5 & data_modules$difficulty < 4.5] = "somewhat difficult"
    data_modules$lev_difficulty[data_modules$difficulty >= 4.5 & data_modules$difficulty < 5.5] = "difficult"
    data_modules$lev_difficulty[data_modules$difficulty >= 5.5] = "very difficult"

      as.factor(data_modules$lev_difficulty)    # makes new variable a factor

# ADD COURSE DATA VARIABLES 
      
  # creates data frame containint course information
    course_info <- data %>%                 
        group_by(course_module) %>%
          summarise(module_code = first(module_code),       
                    module_title = first(module_title),
                    lecturer = first(lecturer),
                    course_code = first(course_code),
                    courseID = first(courseID),
                    course_title = first(course_title),
                    course_cat = first(course_cat),
                    year = first(year),
                    startdate=min(startdate),
                    enddate=max(enddate),
                    duration=max(duration),
                    location = first(location),
                    year = first(year),
                    elacceptance = round(mean(elacceptance=="No"),2)
                    ) 
    
      # adds the course data to the other dataframe 
        data_modules <- left_join(course_info, data_modules, by="course_module") 
            remove("course_info")   # removes the course info dataframe

#----------- data_courses: course level dataframe (one row per course) --------------------------

# SUMMARISED DATA FRAME, AVERAGE SCORES FOR COURSES (ACROSS PARTICIPANTS FROM THAT COURSE)
            
  # Create dataframe with averaged numeric variables
    data_courses <- data_modules %>%
      group_by(course_code) %>%
        summarise_if(is.numeric, mean, na.rm=TRUE) %>%
            select(-"year")     # removes the averaged year
            
            # round responserate & nresponses
              data_courses$responserate <- round(data_courses$responserate, 2) 
              data_courses$nresp <- round(data_courses$nresp, 0)
            
  # add level of difficulty with labeled groups
    data_courses$lev_difficulty <- NA
    data_courses$lev_difficulty[data_courses$difficulty < 1.5] = "very easy"
    data_courses$lev_difficulty[data_courses$difficulty >= 1.5 & data_courses$difficulty < 2.5] = "easy"
    data_courses$lev_difficulty[data_courses$difficulty >= 2.5 & data_courses$difficulty < 3.5] = "somewhat easy"
    data_courses$lev_difficulty[data_courses$difficulty >= 3.5 & data_courses$difficulty < 4.5] = "somewhat difficult"
    data_courses$lev_difficulty[data_courses$difficulty >= 4.5 & data_courses$difficulty < 5.5] = "difficult"
    data_courses$lev_difficulty[data_courses$difficulty >= 5.5] = "very difficult"
              
      as.factor(data_courses$lev_difficulty)   # make new variable a factor

# ADD COURSE DATA VARIABLES 
        
  # creates data frame containint course information
    course_info <- data_modules %>%
        group_by(course_code) %>%
          summarise(courseID = first(courseID),       
                    course_title = first(course_title),
                    course_cat = first(course_cat),
                    startdate = min(startdate),
                    enddate = max(enddate),
                    location = first(location),
                    year = first(year)
                    )

    # adds course_info dataframe to the data_courses dataframe    
      data_courses <- left_join(course_info, data_courses, by="course_code") 
          remove("course_info")    # removes the helper dataset
        
    # add variable for course duration
      data_courses <- data_courses %>%
          mutate(duration = enddate - startdate)
 
    # add variable for lecturer list (lecturers from all modules of a course)
      lecturer <- data_modules %>%
        group_by(course_code) %>%
        summarise(lecturer_all = paste(lecturer, collapse=", "))
      
        # attaches this variable to the data_courses dataframe
        data_courses <- left_join(data_courses, lecturer, by="course_code")
          remove("lecturer")
      
#----------- data_years: Dataframe with yearly averages (one row per year) -----------------------

# SUMMARISES AVERAGE SCORES OF COURSES PER YEAR      
      
  # Creates summarised dataframe with mean scores out of numeric scores
    data_years <- data_courses %>%
        group_by(year) %>%
          summarise_if(is.numeric, mean, na.rm=TRUE)

      # round responserate
        data_years$responserate <- round(data_years$responserate, 2)
        
  # adds a dataframe cointaining 2 variables 
    # - list of all lecturers from that year 
    # - list of all locations from that year
        lecloc <- data_courses %>%
          group_by(year) %>%
          summarise(       
            lecturer_all = paste(lecturer_all, collapse=", "),
            locations_all = paste(location, collapse=", ")
          )

      # attaches the variables to the data_years dataframe
        data_years <- left_join(data_years, lecloc, by="year") 
    
  # corrects the variable for total number of participants (should not be mean score but sum)
    nparticipants <- data_courses %>%
        group_by(year) %>%
          summarise(npart_total = sum(npart))
    
      # attaches this variable to the data_years dataframe
        data_years <- left_join(nparticipants, data_years, by="year") %>%
          select(-c("npart","nresp"))

      # remove helper dataframes
        remove("lecloc","nparticipants")
  
  # new variable with labeled groups for level of difficulty
    data_years$lev_difficulty <- NA
    data_years$lev_difficulty[data_years$difficulty < 1.5] = "very easy"
    data_years$lev_difficulty[data_years$difficulty >= 1.5 & data_years$difficulty < 2.5] = "easy"
    data_years$lev_difficulty[data_years$difficulty >= 2.5 & data_years$difficulty < 3.5] = "somewhat easy"
    data_years$lev_difficulty[data_years$difficulty >= 3.5 & data_years$difficulty < 4.5] = "somewhat difficult"
    data_years$lev_difficulty[data_years$difficulty >= 4.5 & data_years$difficulty < 5.5] = "difficult"
    data_years$lev_difficulty[data_years$difficulty >= 5.5] = "very difficult"
        
        as.factor(data_years$lev_difficulty)    # makes new variable a factor
        
#------ data_categories: Dataframe with avg. scores per course category (across years) --------

# SUMMARISES AVERAGE SCORES OF COURSES PER CATEGORY      

  # Create dataframe summarising all courses (by category)
    data_categories <- data_courses %>%
      group_by(course_cat) %>%
        summarise_if(is.numeric, mean, na.rm=TRUE) %>%
          select(-"year")
        
    # corrects total number of participants
      nparticipants <- data_courses %>%
        group_by(course_cat) %>%
          summarise(npart_total = sum(npart))
    
        # adds the nparticipants variable to data_categories dataframe
          data_categories <- left_join(data_categories, nparticipants, by="course_cat") 
            data_categories <- select(data_categories, -c("npart", "nresp"))
              remove("nparticipants")
      
    # rounds npart & reponserate
      data_categories$responserate <- round(data_categories$responserate, 2)

  # percentage of people per category
    data_categories$npart_percent <- round(data_categories$npart_total / sum(data_categories$npart_total),2)
    
  # new variable with labeled groups for level of difficulty
    data_categories$lev_difficulty <- NA
    data_categories$lev_difficulty[data_categories$difficulty < 1.5] = "very easy"
    data_categories$lev_difficulty[data_categories$difficulty >= 1.5 & data_categories$difficulty < 2.5] = "easy"
    data_categories$lev_difficulty[data_categories$difficulty >= 2.5 & data_categories$difficulty < 3.5] = "somewhat easy"
    data_categories$lev_difficulty[data_categories$difficulty >= 3.5 & data_categories$difficulty < 4.5] = "somewhat difficult"
    data_categories$lev_difficulty[data_categories$difficulty >= 4.5 & data_categories$difficulty < 5.5] = "difficult"
    data_categories$lev_difficulty[data_categories$difficulty >= 5.5] = "very difficult"
    
    as.factor(data_categories$lev_difficulty)    # makes new variable a factor
    