
*********************************************************************Data Loading****************************************************************************
library("data.table")
library("gdata") #for loading data from excel-sheets

#Information about the states
us_states_census_info = read.table("data/USA_States.txt" ,sep="," ,header =T)

# To collect the information like total population, number of precints for each county
us_alaska_precint_county_info = read.table("gis/study_17216_Alaska/AK_08_merge_final.tab", sep = "\t",header=T )

#To have the information about state and electoral
state_electoral = read.xls("data/state-electoral.xlsx",sheet=1,na.strings='NA')

#To review the warning
options(warn=1)

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


#Function to generate random votes for the precints level
vote_generation_precints <- function(){
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
        total_candidate <- 0
        for ( j in 1:(length(sections))){
            candidate[[j]] <- as.integer(us_precint_info$Population[i] * choice[j])
            #total_candidate <- total_candidate + as.integer(candidate[[j]])
        }
        #candidate[[length(sections)]] = as.integer(as.numeric(us_precint_info$Population[i]) - total_candidate)
        sectionRandom[[i]] = as.integer(c(candidate[[1]] ,candidate[[2]] ,candidate[[3]]))
    }
    votes_distributed <-as.data.frame(do.call("rbind",sectionRandom))
    colnames(votes_distributed)[1] <- "MANU"
    colnames(votes_distributed)[2] <- "CHELSEA"
    colnames(votes_distributed)[3] <- "ARSENAL"

    #Combined the result with the precints information with the votes distribution
    votes_distributed_alaska <- c(us_precint_info,votes_distributed)
    return (votes_distributed_alaska)
}

#Aggregation of the votes for all the precints for the particular at the county level
vote_aggregation_county <- function(){
    alaska_votes_precints <- vote_generation_precints()
    dt <- as.data.table(alaska_votes_precints)
    alaska_county_result <- data.frame(dt[,list(Precints = .N,Population = sum(Population),MANU = sum(MANU),CHELSEA = sum(CHELSEA),ARSENAL = sum(ARSENAL)),by="County-Code"])
    colnames(alaska_county_result)[1] <- "County-Code"
    return(alaska_county_result)
}

#Aggregation of the votes for all the counties for the particular state at the state level
result_state <- function(x){
    vote_tally <- vote_aggregation_county()
    electoral <- as.numeric(state_electoral[,state_electoral$State == x][2])
    state_total <- as.data.frame(cbind(x,electoral,nrow(vote_tally),sum(vote_tally[2]),t(t(cbind.data.frame(as.vector(colSums(vote_tally))))[,4:6])))
    colnames(state_total)[1] <- "State"
    colnames(state_total)[2] <- "Electoral"
    colnames(state_total)[3] <- "Counties"
    colnames(state_total)[4] <- "Precints"
    colnames(state_total)[5] <- "MANU"
    colnames(state_total)[6] <- "CHELSEA"
    colnames(state_total)[7] <- "ARSENAL"
    return (state_total)
}

#Electoral Allocation according to the vote count of candidate
electoral_allocation <- function(){
    state_result <- result_state("Alaska")
    winning_party_state <- max.col(state_result[c(5:7)])
    electoral_votes <- state_result[2]
    state_name <- state_result[1]
    electoral <- state_result[2]
    counties <- state_result[3]
    precints <- state_result[4]
    if (winning_party_state==1){
        manu <- electoral_votes
        chelsea <- 0
        arsenal <- 0
    }
    else if (winning_party_state == 2){
        manu <-  0
        chelsea <- electoral_votes
        arsenal <- 0
    }
    else{
        manu <-  0
        chelsea <- 0
        arsenal <- electoral_votes
    } 
    final_result <- as.data.frame(cbind(state_name,electoral,counties,precints,manu,chelsea,arsenal))
    colnames(final_result)[1] <- "State-Name"
    colnames(final_result)[2] <- "Electoral"
    colnames(final_result)[3] <- "Counties"
    colnames(final_result)[4] <- "Precints"
    colnames(final_result)[5] <- "MANU"
    colnames(final_result)[6] <- "CHELSEA"
    colnames(final_result)[7] <- "ARSENAL"
    return(final_result)
}

#Function to randomly do the election over particular state for n times
random_state_election <- function(n){
    random_vote <- list()
    for(i in 1:n){
        dataframe <- electoral_allocation()
        class.data  <- sapply(dataframe, class)
        factor.vars <- class.data[class.data == "factor"]
        for (colname in names(factor.vars)){
                dataframe[,colname] <- as.character(dataframe[,colname])
        }
        random_vote[[i]] = dataframe
    }
    random_results <-as.data.frame(do.call("rbind",random_vote))
    random_results[is.na(random_results)] <- 0
    return(random_results)
}

#Function to calculate how are the wins distributed over the candidate after n Tries
random_wins <- function(n){
    result_tally <- random_state_election(n)
    electoral <- as.numeric(result_tally[n,2])
    result_tally$MANU <- as.numeric(result_tally$MANU)
    result_tally$CHELSEA <- as.numeric(result_tally$CHELSEA)
    result_tally$ARSENAL <- as.numeric(result_tally$ARSENAL)
    fmanu <- sum(result_tally$MANU) / electoral
    fchelsea <- sum(result_tally$CHELSEA) / electoral
    farsenal <- sum(result_tally$ARSENAL) / electoral
    random_process_election <- as.data.frame(cbind(result_tally[n,1],n,fmanu,fchelsea,farsenal))
    colnames(random_process_election)[1] <- "State-Name"
    colnames(random_process_election)[2] <- "Tries"
    colnames(random_process_election)[3] <- "MANU-WINS"
    colnames(random_process_election)[4] <- "CHELSEA-WINS"
    colnames(random_process_election)[5] <- "ARSENAL-WINS"
    return (random_process_election)
}
   
#Function to set the batches of election 
batch_election <- function(batch, n ){
    full_election_temp <- list()
    for (i in 1:batch){
        full_election_temp[[i]] <- random_wins(n)
    }
    full_election <- as.data.frame(do.call("rbind",full_election_temp))
    return(full_election)    
}
********************************************************************************************************************************************************