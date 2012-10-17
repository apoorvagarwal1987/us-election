# Introduction of the frauds
#   1)  Making every precint to support MANU (FRAUD-1) -----------------------------------------------------------> Booth Capturing (100 %)
#   2)  Making a proportion of the precints support MANU (FRAUD-2) whenever the ( probability > 0.87 ) to provide fraudness only in some -----------> Giving Priority 
#   3)  Making Dean Man Voting -----------------------------------------------------------------------------------> Ballot Stuffing,voting impersonation
#   4)  Broken Voting Machine -------> means the machine tries to replace the votes of candidate B or C to convert it into vote of A.
#               4a) In this case machine replaces the votes of another candidate with votes for candidate A if the value of candidate A goes below certain threshold value
#               4b) In this machine will replace the votes of another candidate with votes for candidate A if candidate A has probability lower then both the other candidate


*********************************************************************Data Loading****************************************************************************
library("data.table")
library("gdata") #for loading data from excel-sheets

#Information about the states
us_states_census_info = read.table("data/USA_States.txt" ,sep="," ,header =T)

# To collect the information like total population, number of precints for each county
#us_precint_county_info = read.table("data/precints_census/02.tab", sep = "\t",header=T )
us_precint_county_info = read.xls("data/precints_census/02.xls",sheet=1,na.strings='NA')


#To have the information about state and electoral
state_electoral = read.xls("data/state-electoral.xlsx",sheet=1,na.strings='NA')

*******************************************************************Generic Functions*********************************************************************

#Function to find the choice of people authentic approach 
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

#Fraud Choice Function splitting the probability in the way that MANU gets highest probability and then CHELSEA followed by ARSENAL.
candidate_choice_fraud <- function(n){
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
    choice <- as.list(sort(as.numeric(choice), decreasing= T))
    return(choice)
}

#Fraud by doing the ballot stuffing which supports candiadate MANU and the voting for the Candidate CHELSEA and ARSENAL remain untouched.
candidate_choice_ballot_stuffing <- function(n){
    choice <- runif(n)
    flag <- 1
    while(flag == 1 ){
        sum <- 0
        for (i in 1:n){
            sum <- choice[i] + sum 
        }  
        fav_candidate <- choice[2] + choice[3]    
        diff_can12 <- choice[1] + choice[2]
        diff_can13 <- choice[1] + choice[3]
        if(sum <= 1.150 &&  sum >= 0.99 && choice[1] > choice[2] && choice[1] > choice[3] ){
            #print(choice)
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

vote_generation_precints_fraud1 <- function(us_precint_county_info){
    #us_precint_info <- us_precint_county_info[c(6,8)]
    us_precint_info <- as.data.frame(cbind(us_precint_county_info$VAP,us_precint_county_info$COUNTYFP_1))
    #us_precint_info <- us_precint_info[us_precint_info$totpop!="0",]
    us_precint_info <- as.data.frame(us_precint_info[us_precint_info[[1]] != "0", ])

    colnames(us_precint_info)[1] <- "Population" 
    colnames(us_precint_info)[2] <- "County-Code" 
    #Generates random votes for the number of candidates
    sections = c("Manu","Chelsea","Arsenal")
    sectionRandom <- list()
    candidate <- list()
    for(i in 1:nrow(us_precint_info)){
        # basically doing the booth capturing in the way that all the precints supports Candidate A(MANU)
        choice <- candidate_choice_fraud(length(sections))
        total_candidate <- 0
        for ( j in 1:(length(sections)-1)){
            candidate[[j]] <- as.integer(us_precint_info$Population[i] * choice[[j]])
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

vote_generation_precints_fraud2 <- function(us_precint_county_info){
    #us_precint_info <- us_precint_county_info[c(6,8)]
    us_precint_info <- as.data.frame(cbind(us_precint_county_info$VAP,us_precint_county_info$COUNTYFP_1))
    #us_precint_info <- us_precint_info[us_precint_info$totpop!="0",]
    us_precint_info <- as.data.frame(us_precint_info[us_precint_info[[1]] != "0", ])

    colnames(us_precint_info)[1] <- "Population" 
    colnames(us_precint_info)[2] <- "County-Code" 
    #Generates random votes for the number of candidates
    sections = c("Manu","Chelsea","Arsenal")
    sectionRandom <- list()
    candidate <- list()
    fraudness_ratio <- as.data.frame(runif(nrow(us_precint_info),min=0.1,max=0.9))
    for(i in 1:nrow(us_precint_info)){
        if(fraudness_ratio[i,] > 0.87){
            choice <- candidate_choice_fraud(length(sections))
        }
        else{
            choice <- candidate_choice(length(sections))
        }
        total_candidate <- 0
        for ( j in 1:(length(sections)-1)){
            candidate[[j]] <- as.integer(us_precint_info$Population[i] * choice[[j]])
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


# Inroduction of Fraud mechanism such that it tries to introduce the fraud in the voting machine by replacing some percentage of the votes of candidate
# either CHELSEA or ARSENAL to increase the vote count of other candidate MANU-WINS whenever the probability of liking the candidate MANU goes below certain threshold.
vote_generation_precints_fraud3 <- function(us_precint_county_info){
    #us_precint_info <- us_precint_county_info[c(6,8)]
    #us_precint_info <- us_precint_info[us_precint_info$totpop!="0",]
    us_precint_info <- as.data.frame(cbind(us_precint_county_info$VAP,us_precint_county_info$COUNTYFP_1))
    us_precint_info <- as.data.frame(us_precint_info[us_precint_info[[1]] != "0", ])
    colnames(us_precint_info)[1] <- "Population" 
    colnames(us_precint_info)[2] <- "County-Code" 
    #Generates random votes for the number of candidates
    sections = c("Manu","Chelsea","Arsenal")
    sectionRandom <- list()
    candidate <- list()
    increment_factor <- 0.45
    fraud_threshold <- 0.46
    for(i in 1:nrow(us_precint_info)){
        choice <- candidate_choice(length(sections))
        if (choice[1] < choice[2] && choice[1] < choice[3]){
            #print(choice)
            #write(choice,file="text.txt")
            if(choice[2] >= choice[3]){
                fraudness <- choice[2] * increment_factor
                choice[1] <- choice[1] + fraudness
                choice[2] <- choice[2] - fraudness
            }
            else{
                fraudness <- choice[3] * increment_factor
                choice[1] <- choice[1] + fraudness
                choice[3] <- choice[3] - fraudness
            }
            #print(choice)
        }
        else if(choice[1] < fraud_threshold){
            #print(choice)
            #write(choice,file="text.txt")
            if(choice[2] >= choice[3]){
                fraudness <- choice[2] * increment_factor
                choice[1] <- choice[1] + fraudness
                choice[2] <- choice[2] - fraudness
            }
            else{
                fraudness <- choice[3] * increment_factor
                choice[1] <- choice[1] + fraudness
                choice[3] <- choice[3] - fraudness
            }
            #print(choice)
        }
        else{}
        total_candidate <- 0
        for ( j in 1:(length(sections)-1)){
            candidate[[j]] <- as.integer(us_precint_info$Population[i] * choice[[j]])
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
#Aggregation of the votes for all the precints for the particular at the county level
vote_aggregation_county <- function(us_precint_county_info){
    alaska_votes_precints <- vote_generation_precints_fraud3(us_precint_county_info)
    dt <- as.data.table(alaska_votes_precints)
    alaska_county_result <- data.frame(dt[,list(Precints = .N,Population = sum(Population),MANU = sum(MANU),CHELSEA = sum(CHELSEA),ARSENAL = sum(ARSENAL)),by="County-Code"])
    colnames(alaska_county_result)[1] <- "County-Code"
    #write.table(alaska_county_result)
    return(alaska_county_result)
}

#Aggregation of the votes for all the counties for the particular state at the state level
result_state <- function(state,stateid,us_precint_county_info){
    vote_tally <- vote_aggregation_county(us_precint_county_info)
    electoral <- as.numeric(state_electoral[state_electoral$State == state,][2])
    state_total <- as.data.frame(cbind(state,electoral,nrow(vote_tally),sum(vote_tally[2]),t(t(cbind.data.frame(as.vector(colSums(vote_tally))))[,4:6])))
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
electoral_allocation <- function(state,stateid,us_precint_county_info){
    state_result <- result_state(state,stateid,us_precint_county_info)
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
random_state_election <- function(n,state,stateid,us_precint_county_info){
    random_vote <- list()
    for(i in 1:n){
        dataframe <- electoral_allocation(state,stateid,us_precint_county_info)
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
random_wins <- function(n,state,stateid,us_precint_county_info){
    result_tally <- random_state_election(n,state,stateid,us_precint_county_info)
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
batch_election <- function(batch, n,state,stateid){
    precints_info_files <- list.files(path="data/precints_census/")
    match <- paste(as.character(stateid),"xls",sep=".")
    for(i in precints_info_files){
        file_name <- paste("data/precints_census/",i,sep="/")
        x <- read.xls(i,sheet=1,na.strings ='NA')
        if(match == i){
            us_precint_county_info <- x
        }
        assign(i,x)

    }    
    full_election_temp <- list()
    for (i in 1:batch){
        full_election_temp[[i]] <- random_wins(n,state,stateid,us_precint_info)
    }
    full_election <- as.data.frame(do.call("rbind",full_election_temp))
    return(full_election)    
}


# Function to read all the files together
read_all <- function(){
    precints_info_files <- list.files(path="data/precints_census/")
    for(i in precints_info_files){
        file_name <- paste("data/precints_census/",i,sep="/")
        x <- read.xls(file_name,sheet=1,na.strings ='NA')
        assign(i,x)
    }
} 



#us_precint_county_info = read.xls("data/precints_census/02.xls",sheet=1,na.strings='NA')


********************************************************************************************************************************************************

#Function to generate random votes for the county level
vote_generation_county <- function(){
    sections = c("Manu","Chelsea","Arsenal")
    sectionRandom <- list()
    for(i in 1:nrow(us_county_precint_election)){
        sectionRandom[[i]] = as.numeric(table(sample(sections,us_county_precint_election$population[i],replace=T)))
    }
    votes_distributed <-as.data.frame(do.call("rbind",sectionRandom))
    colnames(votes_distributed)[1] <- "MANU"
    colnames(votes_distributed)[2] <- "CHELSEA"
    colnames(votes_distributed)[3] <- "ARSENAL"
    votes_distributed <- cbind(us_county_precint_election,votes_distributed)
    return (votes_distributed)
}