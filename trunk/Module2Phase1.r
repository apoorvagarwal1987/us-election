*********************************************************************Data Loading****************************************************************************
library("data.table")
library("gdata") #for loading data from excel-sheets

#Information about the states
us_states_census_info = read.table("data/USA_States.txt" ,sep="," ,header =T)

# To collect the information like total population, number of precints for each county
us_alaska_precint_county_info = read.table("gis/study_17216_Alaska/AK_08_merge_final.tab", sep = "\t",header=T )

#To have the information about state and electoral
state_electoral = read.xls("data/state-electoral.xlsx",sheet=1,na.strings='NA')

*******************************************************************Generic Functions*********************************************************************

#Function to find the choice of people 
candidate_choice <- function(n){
    choice <- runif(n)
    flag <- 1
    while(flag == 1 ){
        sum <- 0
        for (i in 1:n){
            sum <- choice[i] + sum 
        }        
        if(sum <= 1.01 &&  sum >= 0.99){
            flag <- 0
        }
        else{
            choice <- runif(n)
        }
    }
    return(choice)
}

#Fraud -1 : Giving the Preference to one of the candidate by making the distribution of the votes in favour of that candidate and probability is decreased
# MANU, CHELSEA, ARSENAL.

vote_generation_precints_fraud1 <- function(){
    us_precint_info <- us_alaska_precint_county_info[c(6,8)]
    us_precint_info <- us_precint_info[us_precint_info$totpop!="0",]
    colnames(us_precint_info)[1] <- "Population" 
    colnames(us_precint_info)[2] <- "County-Code" 

    #Generates random votes for the number of candidates
    sections = c("Manu","Chelsea","Arsenal")
    sectionRandom <- list()
    candidate <- list()
    for(i in 1:nrow(us_precint_info)){
        choice <- candidate_choice(length(sections))
        choice <- as.list(sort(as.numeric(choice), decreasing= T))
        total_candidate <- 0
        for ( j in 1:(length(sections)-1)){
            candidate[[j]] <- as.integer(us_precint_info$Population[i] * choice[[j]])
            #print (as.integer(us_precint_info$Population[i]) * as.integer(choice[[j]]))
            total_candidate <- total_candidate + candidate[[j]]
        }
        candidate[[length(sections)]] = as.integer(as.numeric(us_precint_info$Population[i]) - total_candidate)
        #print (choice)
        sectionRandom[[i]] = as.integer(c(candidate[[1]] ,candidate[[2]] ,candidate[[3]]))
    }
    votes_distributed <-as.data.frame(do.call("rbind",sectionRandom))
    colnames(votes_distributed)[1] <- "MANU"
    colnames(votes_distributed)[2] <- "CHELSEA"
    colnames(votes_distributed)[3] <- "ARSENAL"

    #Combined the result with the precints information with the votes distribution
    votes_distributed_alaska <- c(us_precint_info,votes_distributed)
    temp <- as.data.frame(votes_distributed)
    #write.table(temp)
    return (votes_distributed_alaska)
}
