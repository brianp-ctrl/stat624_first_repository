## Import the data
library(readxl)

df_STI_correct = read_excel("Requested_table/STI_complete_table.xlsx")
df_STI_correct$Date = as.Date(paste0("20",day(df_STI_correct$Date),"-",month(df_STI_correct$Date),"-01"))

# there is "HIV +" and "HIV+" in the column of df_STI_correct_Collapsed. Let's make them as one as "HIV+"
df_STI_correct$Prep = ifelse(df_STI_correct$Prep == "HIV +", "HIV+",df_STI_correct$Prep )
df_STI_correct = df_STI_correct%>%mutate(across(where(is.character),~ifelse(.x == "n/a", NA, .x)))
df_STI_correct = df_STI_correct%>%filter(Prep %in% c("yes","no"))
df_STI_correct_Collapsed = df_STI_correct


### Collapsing the df_STI data for same dates
i = 2
# Let's collapse this df_STI_correct. 
while(i <= nrow(df_STI_correct_Collapsed)){
  each_row = df_STI_correct_Collapsed[i,]
  prev_row = df_STI_correct_Collapsed[(i-1),]
  #check if the visits were not found
  
  # Check if the row is NA, which means that we can collapse them, and put it together with the row above 
  if(is.na(each_row$Prep)){
    
    #collapse Gon
    if(prev_row$Gonorrhea == 0 & each_row$Gonorrhea ==1){
      df_STI_correct_Collapsed[i-1,]$Gonorrhea = 1
    }
    #collapse Chlam
    if(prev_row$Chlamydia == 0 & each_row$Chlamydia ==1){
      df_STI_correct_Collapsed[i-1,]$Chlamydia = 1
    }
    #collapse syphilis
    if(prev_row$Syphilis == 0 & each_row$Syphilis ==1){
      df_STI_correct_Collapsed[i-1,]$Syphilis = 1
    }
    
    # remove the the row with NA
    df_STI_correct_Collapsed = df_STI_correct_Collapsed[-i,]
    
    # check if the visit is on the same day & collapse the STIs(For few rows in the beggining, there are some overalping days)
  }else if((each_row$Client_ID == prev_row$Client_ID)&(each_row$Date == prev_row$Date)){
    #collapse Gon
    if(prev_row$Gonorrhea == 0 & each_row$Gonorrhea ==1){
      df_STI_correct_Collapsed[i-1,]$Gonorrhea = 1
    }
    #collapse Chlam
    if(prev_row$Chlamydia == 0 & each_row$Chlamydia ==1){
      df_STI_correct_Collapsed[i-1,]$Chlamydia = 1
    }
    #collapse syphilis
    if(prev_row$Syphilis == 0 & each_row$Syphilis ==1){
      df_STI_correct_Collapsed[i-1,]$Syphilis = 1
    }
    
    # remove the the row with NA
    df_STI_correct_Collapsed = df_STI_correct_Collapsed[-i,]
  }else{
    i = i +1
  }
}

###### adding zipcode to the table...
zip = read_csv("patient_id_zipcode.csv")%>%select(-1)

df_STI_correct_Collapsed = left_join(df_STI_correct_Collapsed,zip, by = c("Client_ID" = "patient_id"))


