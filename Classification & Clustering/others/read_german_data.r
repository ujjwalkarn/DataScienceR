#' Read German Credit dataset
#'
#' Reads German Credit dataset from UCI repository.
#'

iv.readgd<- function() {

german_data <- read.table(file="http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data",
                 sep=" ", header=FALSE, stringsAsFactors=TRUE)
names(german_data) <- c('ca_status','duration','credit_history','purpose','credit_amount','savings',
               'present_employment_since','installment_rate_income','status_sex','other_debtors',
               'present_residence_since','property','age','other_installment','housing','existing_credits',
               'job','liable_maintenance_people','telephone','foreign_worker','gb')


# Status of existing checking account
german_data$ca_status <- factor(german_data$ca_status, levels=c("A11","A12","A13","A14"),                                 
                                 labels = c("(;0DM)","<0DM;200DM)","<200DM;)","No Acc."))

# Credit history
german_data$credit_history <- factor(german_data$credit_history, levels=c("A30","A31","A32","A33","A34"),
                                      labels = c(
                                          "no credits", #"no credits taken/all credits paid back duly",
                                          "paid off",   #"all credits at this bank paid back duly",
                                          "all paid",   #"existing credits paid back duly till now",
                                          "delay",      #"delay in paying off in the past",
                                          "critical"   #"critical account/other credits existing (not at this bank)"))
                                      ))
# Purpose
german_data$purpose <- factor(german_data$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46",
                                                            # "A47",
                                                            "A48","A49","A410"),
                              labels=c(
                                "car (new)",
                                "car (used)",
                                "furniture/equipment",
                                "radio/television",
                                "domestic appliances",
                                "repairs",
                                "education",
                                # "vacation",
                                "retraining",
                                "business",
                                "others"                              
                                ))

# Savings account/bonds
german_data$savings <- factor(german_data$savings, levels=c("A61","A62","A63","A64","A65"),
                              labels=c(
                                "(;100DM)",
                                "<100;500)",
                                "<500;1000)",
                                "<1000;)",
                                "unknown / no savings account"
                                ))

# Present employment since
german_data$present_employment_since <- factor(german_data$present_employment_since, levels=c("A71","A72","A73","A74","A75"),
                                               labels=c(
                                                 "unemployed",
                                                 "(;1)",
                                                 "<1;4)",
                                                 "<4;7)",
                                                 "<7;)"
                                                 ))
# Personal status and sex
german_data$status_sex <- factor(german_data$status_sex, levels=c("A91","A92","A93","A94","A95"),
                                 labels=c(
                                   "male:div./sep.", #"male:divorced/separated",
                                   "female:div./sep./marr.",#"female:divorced/separated/married",
                                   "male:single",
                                   "male:marr/wid.", # male:married/widowed
                                   "female:single"
                                   ))

# Other debtors / guarantors
german_data$other_debtors <- factor(german_data$other_debtors, levels=c("A101","A102","A103"),
                                    labels=c(
                                      "none",
                                      "co-applicant",
                                      "guarantor"                                  
                                      ))
# Property
german_data$property <- factor(german_data$property, levels=c("A121","A122","A123","A124"),
                               labels=c(
                                 "real estate", 
                                 "svngs. agrrement", # if not A121 : building society savings agreement/ life insurance 
                                 "car or other", # if not A121/A122 : car or other, not in attribute 6 
                                 "unknown/no")) # unknown / no property 
# Other installment plans
german_data$other_installment <- factor(german_data$other_installment, levels=c("A141","A142","A143"),
                                        labels=c(
                                          "bank",
                                          "stores",
                                          "none"))

# Housing
german_data$housing <- factor(german_data$housing, levels=c("A151","A152","A153"),
                              labels=c(
                                "rent",
                                "own",
                                "for free"))

# Job
german_data$job <- factor(german_data$job, levels = c("A171","A172","A173","A174"),
                          labels=c(
                            "unemp./unsk. nonr.", # unemployed/ unskilled - non-resident
                            "unsk. res.", # unskilled - resident
                            "skilled/off.", #"skilled employee / official
                            "mng/self emp, hig qual." # management/ self-employed/ highly qualified employee/ officer
                          ))

# Telephone
german_data$telephone <- factor(german_data$telephone, levels=c("A191","A192"),
                                labels=c(
                                  "none",
                                  "yes"))
# Foreign worker
german_data$foreign_worker <- factor(german_data$foreign_worker, levels=c("A201","A202"),
                                     labels=c(
                                       "yes",
                                       "no"))

# g/b
german_data$gb <- factor(german_data$gb, levels=c(2,1), labels=c("bad","good"))

german_data

}