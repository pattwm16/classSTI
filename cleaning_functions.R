# Cleanup function File

## Functions for Dealing with Missing Data

# Libraries
library(glue)

# had difficulty conditionally reassigning 0 to taken_pep if case had not heard of pep
# function: taken_pep_cleanup
# doc: consumes a dataframe, retuns 0 in column taken_pep if row has not heard of pep 
#      (e.g. heard_pep == 0)
taken_pep_cleanup <- function(elem){
  for (i in 1:nrow(elem)){
    if (is.na(elem$heard_pep[[i]])){
      print(glue("The value for the input at index {i} is missing"))
    }
    else if (is.na(elem$taken_pep[[i]]) & (elem$heard_pep[[i]] == 0)){
      elem$taken_pep[[i]] <- 0
      assign('total_msm', elem, envir=.GlobalEnv) # had issues with scope, had to reassign to global environment
    } 
  }
}

# function: taken_prep_cleanup
# doc: consumes a dataframe, retuns 0 in column taken_prep if row has not heard of prep 
#      (e.g. prep_heard == 0)
taken_prep_cleanup <- function(elem){
  for (i in 1:nrow(elem)){
    if (is.na(elem$prep_heard[[i]])){
      print(glue("The value for the input at index {i} is missing"))
    }
    else if (is.na(elem$taken_prep[[i]]) & (elem$prep_heard[[i]] == 0)){
      elem$taken_prep[[i]] <- 0
      assign('total_msm', elem, envir=.GlobalEnv) # had issues with scope, had to reassign to global environment
    } 
  }
}

# if no partners are reported, set all sex based features to 0
# function: sexless_cases_cleanup
# doc: consumes a dataframe, retuns 0 for sex related features if input case has no 
#      reported sexual encounters
sexless_cases_cleanup <- function(elem){
  for (i in 1:nrow(elem)){
    if ((is.na(elem$total_partners[[i]])) | (elem$total_partners[[i]] == 0)){
      elem$total_partners[[i]] <- 0
      elem$oral_sex_condom[[i]] <- 0
      elem$oral_sex_hiv[[i]] <- 0
      elem$oral_sex_idu[[i]] <- 0
      elem$male_anal_sex_both[[i]] <- 0
      elem$male_anal_insertive[[i]] <- 0
      elem$male_anal_receptive[[i]] <- 0
      elem$male_anal_sex_condom[[i]] <- 0
      elem$male_anal_receptive[[i]] <- 0
      elem$male_anal_sex_hiv[[i]] <- 0
      elem$male_anal_sex_idu[[i]] <- 0
      
  #had issues with scope, had to reassign to global environment
      assign('total_msm', elem, envir=.GlobalEnv) 
    }
  }
}

# function: gender_cleanup
# doc: consumes a dataframe, returns gender as male if the responded identified as male 
#      in sex_partners column
gender_cleanup <- function(elem){
  n = 0
  for (i in 1:nrow(elem)){
    if ((is.na(elem$gender[[i]])) & ((elem$sex_partners[[i]] == 4) | (elem$sex_partners[[i]] == 5))){
      elem$gender[[i]] <- 0
      assign('total_msm', elem, envir=.GlobalEnv) # reassign to global environment
      n = n + 1
    }
  }
  print(glue("{n} rows were replaced"))
}

# function: hiv_positive_cleanup
# doc: consumes a dataframe, if hiv_positive == 0 ("Yes"), then set hiv_risk to 4 
#      ("Not applicable") and set hiv_test to 1 ("Yes")
hiv_positive_cleanup <- function(elem){
  for (i in 1:nrow(elem)){
    if (is.na(elem$hiv_positive[[i]])){
      print(glue("The value for the input at index {i} is missing"))
    }
    else if (elem$hiv_positive[[i]] == 1){
      elem$hiv_risk[[i]] <- 4
      elem$hiv_test[[i]] <- 1
      assign('total_msm', elem, envir=.GlobalEnv) # reassign to global environment
    } 
  }
}

# function: oral_sex_cleanup
# doc: consumes a dataframe, if female_oral_sex == 0 and male_oral_sex == 0, 
#      then set all resultant oral sex features to 0
oral_sex_cleanup <- function(elem){
  n = 0
  for (i in 1:nrow(elem)){
    if (is.na(elem$female_oral_sex[[i]]) | is.na(elem$male_oral_sex[[i]])){
      print(i)
    }
    else if ((elem$female_oral_sex[[i]] == 0) && (elem$male_oral_sex[[i]] == 0)){
      elem$oral_sex_condom[[i]] <- 0
      elem$oral_sex_idu[[i]] <- 0
      elem$oral_sex_hiv[[i]] <- 0
      assign('total_msm', elem, envir=.GlobalEnv) # reassign to global environment
      n = n + 1
    }
  }
  print(glue("{n} rows were replaced"))
}