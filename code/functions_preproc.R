#Set of handy functions used for data loading, preprocessing, analyses and plotting
#Tailored to TMS in blind/sighted language project
#Jacek Matuszewski


## Main function to preprocess the data for "blind" or "sighted" group
# assumes that there is a main dir with conditions.csv file and "log_clean" dir containing log files 
# function loads all log files for blind or sighted subjects and returns 3 data frames: 
# raw data - all trials from all conditions
# rt data - reaction times summarized for each subject in each condition
# accuracy data - accuracy (error rates) summarized for each subject in each condition
# these data frames have to be prepared as empty variables in the global environment prior to running this function, 
# i.e. if you want to call tms_preproc("blind") first you need to specify blind_rt_data <- NULL; blind_accu_data <- NULL; 
# blind_raw_data <- NULL. These variables are then swapped by the data frames prepared in the function

# Arguments: 
# group = "blind" or "sighted"
# main_path = dir to main folder with subdir containing logs and a csv file with conditions
# sd_trim = severity of data trimming (default = slower or faster than 2.5 SD in each TMS condition)
tms_preproc <- function(group,
                        main_path,
                        sd_trim = 2.5) {
    
    #load packages? 
    library(tidyverse)
    library(rstatix)
    library(lubridate)
    library(stringr)
    
    #Read all logs from given group
    #files <- list.files(paste0(main_path, "log_clean/"), 
    files <- list.files(main_path,
                        pattern = paste0("^sub-", group, "_.*\\.txt$"),
                        ignore.case = TRUE)
    #Read df with site order and remove Gpjm_ from the subject code
    conditions <- read.csv(paste0(main_path,"/","TMS_conditions.csv"), header = TRUE, sep = ";")
        conditions$Subject <- gsub("sub-", "", conditions$Subject)
        
     #Empty data frame to be filled with data
    raw_data <- data.frame(stringsAsFactors = FALSE)
    accuracy_data <- data.frame(stringsAsFactors = FALSE)
    rt_data <-data.frame(stringsAsFactors = FALSE)
    
    #loop test
    for(i in seq_along(files)) {
        
        temp_data<-read.table(paste(main_path , files[i], sep = '/'), header=TRUE)
        #in which row of conditions this subject has Site_1 Site_2 values for sites?
        sub_order_index <- charmatch(sub("-.*", "", files[i]), conditions$Subject)  #i in a loop 
        
        #clean the data and create necessary columns
        temp_data_clean <- temp_data %>% 
            mutate(accuracy = sub("rm_", "", accuracy)) %>% 
            separate(target_code, c("TW", "Modality", "Lex")) %>% 
            mutate(Letters = as.numeric(str_sub(Lex, - 1, - 1))) %>% 
            mutate(Lex = gsub('.{1}$', '', Lex)) %>%  
            mutate(across(TW, as_factor)) %>%
            mutate(Modality = ifelse(Modality[1]== "tread" ||
                                         Modality[1]== "vread" ,
                                     str_sub(Modality,2,-1), Modality)) %>% 
            mutate(RT_log = log(reaction_time)) %>% 
            #add metadata (subject ID, Group, TMS site for this run)
            mutate(Subject = str_split(files[i], '-', simplify = TRUE)[,2]) %>%        # i in a loop 
            #mutate(Subject = gsub("_speech.*", "", Subject)) %>%  #fix Blind_6 who has _speech_V1 in sub name
            #mutate(Subject = gsub('sub-', '', Subject, ignore.case = TRUE)) %>%  
            mutate(Subject = sub("(.)", "\\U\\1", Subject, perl=TRUE)) %>%  #Capitalize Subject
            #Add SITE based on conditions - does it work for sighted?
            mutate(Site = ifelse(grepl("_1", str_split(files[i], '-', 
                                                       simplify = TRUE)[,2], fixed=FALSE),                   
                                 conditions$Site_1[charmatch(Subject,conditions$Subject)], 
                                 conditions$Site_2[charmatch(Subject,conditions$Subject)])) %>% 
            mutate(Group = group) %>% 
            mutate(Group = sub("(.)", "\\U\\1", Group, perl=TRUE)) %>%  #Capitalize Group
            mutate (Accuracy_01 = recode(accuracy, hit = 1, miss = 0, incorrect = 0)) %>% #add numeric accuracy column
            #reorder and pick columns
            dplyr::select(Subject, Group, Site, Modality, TW, Lex, Letters, 
                          RT =reaction_time, RT_log, Accuracy = accuracy, Accuracy_01)
        
        
        #TRIM the data? 
        temp_data_trimmed <- temp_data_clean %>% 
            filter(RT < (mean(RT) + (sd(RT)*2.5))) %>% 
            filter(RT > (mean(RT) - sd(RT)*2.5)) 
        
        #add this log to big data
        raw_data <-rbind(raw_data, temp_data_trimmed)
        
        ## EXTRACT ACCURACY SUMMARIES
        
        #YO THAT CAN GO WRONG BECAUSE OF TRIMMING! 
        count <- temp_data_trimmed %>%  #change to trimmed
            group_by(TW) %>% 
            count(Accuracy)%>% 
            pivot_wider(names_from=Accuracy, values_from=n) %>% 
            mutate(Accuracy_score = hit/20)
        
        #prepare accuracy table with metadata
        accu_temp <- temp_data_trimmed %>% 
            dplyr::select(Subject, Group, Site, Modality, TW)  %>% 
            group_by(TW) %>% 
            distinct(TW, .keep_all = TRUE) %>% 
            ungroup() %>% 
            mutate(Accuracy_score = count$Accuracy_score)
        
        accuracy_data <- rbind(accuracy_data, accu_temp)
        
        
        ## EXTRACT RT SUMMARIES, only for CORRECT
        rt_temp <- temp_data_trimmed %>% 
            dplyr::select(Subject, Group, Site, Modality, TW, RT, RT_log, Accuracy)  %>% 
            group_by(TW) %>% 
            filter(Accuracy == "hit") %>% 
            mutate(RT_mean = mean(RT)) %>% 
            mutate(RT_median = median(RT)) %>% 
            mutate(RT_log_mean = mean(RT_log)) %>% 
            mutate(RT_log_median = median(RT_log)) %>% 
            distinct(TW, .keep_all = TRUE) %>% 
            dplyr::select(-RT_log, -RT, -Accuracy)
        
        rt_data <- rbind(rt_data, rt_temp)
    }

    
    ## RETURN VALUES
    # empty data frames need to be specified in the global environment BEFORE RUNNING THE FUNCTION! 
    
    if(group == "blind") {
        
           blind_raw_data <<-  raw_data
           blind_rt_data <<- rt_data
           blind_accuracy_data <<- accuracy_data
    
    } else if(group == "sighted") {
            
            sighted_raw_data <<-  raw_data
            sighted_rt_data <<- rt_data
            sighted_accuracy_data <<- accuracy_data
        
    } else {
        print("Please choose group: blind or sighted")
    }
    
    return("subject preprocessed, please always check the data")
}



## Plotting functions? 


