#*********************************************************************Data Loading****************************************************************************
library("data.table")
library("gdata") #for loading data from excel-sheet

#Information about the states
#us_states_census_info = read.table("data/USA_States.txt" ,sep="," ,header =T)

# To collect the information like total population, number of precints for each county
#us_precint_county_info = read.table("data/precints_census/02.tab", sep = "\t",header=T )

#us_precint_county_info = read.xls("data/precints_census/02.xls",sheet=1,na.strings='NA')


#To have the information about state and electoral
#state_electoral = read.xls("data/state-electoral.xlsx",sheet=1,na.strings='NA')

#na.omit(dat)

#To review the warning
options(warn=1)

#***************************Generic Functions*************************************

vote_generation_precints_policy_based <- function(us_precint_county_info,state_name){
	#us_precint_info <- us_precint_county_info[c(6,8)]
	us_precint_info <- as.data.frame(cbind(us_precint_county_info$VAP,us_precint_county_info$COUNTYFP_1))
	#us_precint_info <- us_precint_info[us_precint_info$totpop!="0",]
	us_precint_info <- as.data.frame(us_precint_info[us_precint_info[[1]] != "0", ])
	colnames(us_precint_info)[1] <- "Population" 
	colnames(us_precint_info)[2] <- "County-Code" 
	#Generates random votes for the number of candidates
	sections = c("Manu","Chelsea","Arsenal")
	sectionRandom <- list()
	for(i in 1:nrow(us_precint_info)){
		population_precint = us_precint_info$Population[i]
		candidate <- as.list(rep(0,3))
		for(person in 1:population_precint){
			vote_candidate = policy_choice(population_precint)
			if(vote_candidate == 1){
				candidate[[1]] <- as.list(as.integer(candidate[[1]]) + 1)
			}
			else if(vote_candidate == 2){
				candidate[[2]] <- as.list(as.integer(candidate[[2]]) + 1)
			}
			else if(vote_candidate == 3){
				candidate[[3]] <- as.list(as.integer(candidate[[3]]) + 1)
			}
			else{
				#Skip it
			}
		}
		sectionRandom[[i]] = as.integer(c(candidate[[1]] ,candidate[[2]] ,candidate[[3]]))		
	}
	votes_distributed <-as.data.frame(do.call("rbind",sectionRandom))
	colnames(votes_distributed)[1] <- "MANU"
	colnames(votes_distributed)[2] <- "CHELSEA"
	colnames(votes_distributed)[3] <- "ARSENAL"

	#Combined the result with the precints information with the votes distribution
	votes_distributed <- c(us_precint_info,votes_distributed)
	temp <- as.data.frame(votes_distributed)
	#write.table(temp)
	return (votes_distributed)
}

policy_choice <- function(precint_size=10) {
    ncandidates = 3
    npolicies = 8
    policy_weightage = matrix(c(1,1.2,0.3,0.4,0.7,0.1,0.2,0.9))
    candidate_policies = cbind(c(1,0,0),c(0,0.5,0),c(0,1,0.5),c(1,0,0),c(0,1,1),c(1,0,0),c(1,1,1),c(1,0,0.5))
    candidate_policies <- matrix(as.numeric(candidate_policies),ncandidates,npolicies)
    person_policy_liking = t(policy_weight(precint_size,npolicies))
    person_candidate_distance <- list()
    for(i in 1:ncandidates){
        diff = (candidate_policies[i,] - person_policy_liking)^2
        person_candidate_distance[[i]] = (diff %*% policy_weightage)[1]
    }
    person_candidate_distance <- as.matrix(person_candidate_distance)
    person_candidate_distance <- matrix(as.numeric(person_candidate_distance),ncandidates,1)
    #person_candidate_distance = candidate_policies %*% person_policy_liking
    candidate_voted = which(person_candidate_distance == min(person_candidate_distance), arr.ind = TRUE)[1]
    return(candidate_voted)

}

policy_weight <- function(precint_size = 500, npolicies = 8) {
	race = 0
	ethnicity = 0
	sex = 0 
	salary = 0
	age = 0
	policies <- list()
	for(i in 1:npolicies){
		policies[[i]] = runif(1)
	}
	policy_weight_matrix = as.matrix(policies)
	policy_weight_matrix <- matrix(as.numeric(policy_weight_matrix),npolicies,1)
	return (policy_weight_matrix)
}

#***************************Fraud 1*********************************************************
#	Simple frauds with just taking some votes from candidate 2,3  and adding it up 
#	into the votes for 1


vote_generation_precints_policy_based_fraud1 <- function(us_precint_county_info,state_name){
	#us_precint_info <- us_precint_county_info[c(6,8)]
	us_precint_info <- as.data.frame(cbind(us_precint_county_info$VAP,us_precint_county_info$COUNTYFP_1))
	#us_precint_info <- us_precint_info[us_precint_info$totpop!="0",]
	us_precint_info <- as.data.frame(us_precint_info[us_precint_info[[1]] != "0", ])
	colnames(us_precint_info)[1] <- "Population" 
	colnames(us_precint_info)[2] <- "County-Code" 
	#Generates random votes for the number of candidates
	sections = c("Manu","Chelsea","Arsenal")
	sectionRandom <- list()
	fraud_ratio = 0.2
	for(i in 1:nrow(us_precint_info)){
		population_precint = us_precint_info$Population[i]
		candidate <- as.list(rep(0,3))
		for(person in 1:population_precint){
			vote_candidate = policy_choice(population_precint)
			if(vote_candidate == 1){
				candidate[[1]] <- as.list(as.integer(candidate[[1]]) + 1)
			}
			else if(vote_candidate == 2){
				candidate[[2]] <- as.list(as.integer(candidate[[2]]) + 1)
			}
			else if(vote_candidate == 3){
				candidate[[3]] <- as.list(as.integer(candidate[[3]]) + 1)
			}
			else{
				#Skip it
			}
		}

		votes_donated_candidate2 = as.integer(candidate[[2]]) * fraud_ratio
		votes_donated_candidate3 = as.integer(candidate[[3]]) * fraud_ratio
		candidate[[1]] <- as.list(as.integer(candidate[[1]]) + votes_donated_candidate2 + votes_donated_candidate3)
		candidate[[2]] <- as.list(as.integer(candidate[[2]]) - votes_donated_candidate2)
		candidate[[3]] <- as.list(as.integer(candidate[[3]]) - votes_donated_candidate3)
		sectionRandom[[i]] = as.integer(c(candidate[[1]] ,candidate[[2]] ,candidate[[3]]))		
	}
	votes_distributed <-as.data.frame(do.call("rbind",sectionRandom))
	colnames(votes_distributed)[1] <- "MANU"
	colnames(votes_distributed)[2] <- "CHELSEA"
	colnames(votes_distributed)[3] <- "ARSENAL"

	#Combined the result with the precints information with the votes distribution
	votes_distributed <- c(us_precint_info,votes_distributed)
	temp <- as.data.frame(votes_distributed)
	#write.table(temp)
	return (votes_distributed)
}


#***************************Validation Functions*********************************************************
library("plyr")
library("stringr")
#Function for finding the 2nd Dig Benford Probability of the given number
benford_probability_2Dig <- function(digit){
	result <- 0
	for (i in 1:9){
		value <- 1 + (1/(10 * i + digit))
		result <- result + log10(value)
	}
   # print(result)
	return(result)
}

#Function for finding the 1st Dig Benford Probability of the given number
benford_probability_1Dig <- function(digit){
	result <- 0
	result <- log10(1 + (1/digit))
   # print(result)
	return(result)
}

benford_probability_range <- function(){
	benrange <- list()
	for(i in 0:9){
		num  <- benford_probability(i)
		benrange[i] <- num
	}
	return (benrange)
}

position_digit <- function(number,digit){
    len <- str_length(as.character(number))
    #print(number)
    if (len==1){
        num <- as.numeric(strsplit(as.character(number),"")[[1]])[1]
    }
    else{
        num <- as.numeric(strsplit(as.character(number),"")[[1]])[digit]
    }
    return(num)
}


benford_law_2BL <- function(county_result){
	#print(county_result)
	digit_occurences <- as.list(rep(0,10))
	for(i in 1:length(county_result)){
		digit <- position_digit(county_result[[i]],2)
		if(digit == 1){
			digit_occurences[[1]] <- as.list(as.integer(digit_occurences[[1]]) + 1)
		}
		else if(digit == 2){
			digit_occurences[[2]] <- as.list(as.integer(digit_occurences[[2]]) + 1)
		}
		else if(digit == 3){
			digit_occurences[[3]] <- as.list(as.integer(digit_occurences[[3]]) + 1)
		}
		else if(digit == 4){
			digit_occurences[[4]] <- as.list(as.integer(digit_occurences[[4]]) + 1)
		}
		else if(digit == 5){
			digit_occurences[[5]] <- as.list(as.integer(digit_occurences[[5]]) + 1)
		}
		else if(digit == 6){
			digit_occurences[[6]] <- as.list(as.integer(digit_occurences[[6]]) + 1)
		}
		else if(digit == 7){
			digit_occurences[[7]] <- as.list(as.integer(digit_occurences[[7]]) + 1)
		}
		else if(digit == 8){
			digit_occurences[[8]] <- as.list(as.integer(digit_occurences[[8]]) + 1)
		}
		else if(digit == 9){
			digit_occurences[[9]] <- as.list(as.integer(digit_occurences[[9]]) + 1)
		}
		else if(digit == 0){
			digit_occurences[[10]] <- as.list(as.integer(digit_occurences[[10]]) + 1)
		}
		else {
			return (-1)
		}
	}
	tot_rows <- length(county_result)
	total_prob <- 0
	for (i in 1:10){
		digit_frequency = as.integer(digit_occurences[[i]])
		if(i<10){
			prob <- benford_probability_2Dig(i)
		}
		else{
			prob <- benford_probability_2Dig(0)
		}

		numerator <- (digit_frequency - tot_rows * prob)^2
		denominator <- (tot_rows * prob)
		total_prob <- total_prob + (numerator/denominator)
	}
	return (total_prob)
}


benford_law_1BL <- function(county_result){
	#print(county_result)
	digit_occurences <- as.list(rep(0,10))
	for(i in 1:length(county_result)){
		digit <- position_digit(county_result[[i]],1)
		if(digit == 1){
			digit_occurences[[1]] <- as.list(as.integer(digit_occurences[[1]]) + 1)
		}
		else if(digit == 2){
			digit_occurences[[2]] <- as.list(as.integer(digit_occurences[[2]]) + 1)
		}
		else if(digit == 3){
			digit_occurences[[3]] <- as.list(as.integer(digit_occurences[[3]]) + 1)
		}
		else if(digit == 4){
			digit_occurences[[4]] <- as.list(as.integer(digit_occurences[[4]]) + 1)
		}
		else if(digit == 5){
			digit_occurences[[5]] <- as.list(as.integer(digit_occurences[[5]]) + 1)
		}
		else if(digit == 6){
			digit_occurences[[6]] <- as.list(as.integer(digit_occurences[[6]]) + 1)
		}
		else if(digit == 7){
			digit_occurences[[7]] <- as.list(as.integer(digit_occurences[[7]]) + 1)
		}
		else if(digit == 8){
			digit_occurences[[8]] <- as.list(as.integer(digit_occurences[[8]]) + 1)
		}
		else if(digit == 9){
			digit_occurences[[9]] <- as.list(as.integer(digit_occurences[[9]]) + 1)
		}
		else if(digit == 0){
			digit_occurences[[10]] <- as.list(as.integer(digit_occurences[[10]]) + 1)
		}
		else {
			return (-1)
		}
	}
	tot_rows <- length(county_result)
	total_prob <- 0
	for (i in 1:9){
		digit_frequency = as.integer(digit_occurences[[i]])
		if(i<10){
			prob <- benford_probability_1Dig(i)
		}
		else{
			prob <- benford_probability_1Dig(0)
		}

		numerator <- (digit_frequency - tot_rows * prob)^2
		denominator <- (tot_rows * prob)
		total_prob <- total_prob + (numerator/denominator)
	}
	return (total_prob)
}

verify_result <- function(){
	voted_data <- read.table("county_result.txt",sep="",header=F)
	Manu <- benford_law_1BL(as.list(voted_data[,4]))
	Chelsea <- benford_law_1BL(as.list(voted_data[,5]))
	Arsenal <- benford_law_1BL(as.list(voted_data[,6]))
	if(Manu == -1 | Chelsea == -1 | Arsenal == -1){
		print("Single digit voting cannot process data")

	}
	else{
		print(Manu)
		print(Chelsea)
		print(Arsenal)
		#test <- cbind(Manu,Chelsea,Arsenal)
		print(pchisq(Manu,df=9,lower=F))
		print(pchisq(Chelsea,df=9,lower=F))
		print(pchisq(Arsenal,df=9,lower=F))
	}
#    print(pchisq(Chelsea,df=9))
#    print(dchisq(Arsenal,df=9))
}

use_fdr <- function(total_counties = 1, threshold = 0.05, ncandidates = 3){
	updated_threshold = threshold / (total_counties * ncandidates ) 
	return(updated_threshold)
}

batch_verification <- function (size){
    count1 <- 0
    count2 <- 0
    count3 <- 0
    state_electoral = read.xls("data/state-electoral.xlsx",sheet=1,na.strings='NA',stringsAsFactors=F)
    for (i in 1:size){
       #ty <- vote_aggregation_county()		
       # voted_data <- read.table("county_result.txt",sep="",header=F)
        file_names= list.files(path = "./data/precints_census/", pattern = NULL, all.files = FALSE,full.names = FALSE,
        	 recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE)
       	for(index in 1:length(file_names)){
       		com_path  = paste("./data/precints_census",file_names[index],sep="/")
	       	us_precint_county_info = read.xls(com_path,sheet=1,na.strings='NA')
            state_id = substr(file_names[index],start=0,stop=nchar(file_names[index])-4)
            state_name = subset(state_electoral,state_electoral$State.ID == state_id)[1,2]
	        voted_data <- as.data.frame(vote_generation_precints_policy_based(us_precint_county_info,state_name))
	        threshold <- 0.05
	        total_counties = length(unique(voted_data$County.Code))
	        new_threshold = use_fdr(total_counties ,threshold)
            temp_voted_data = as.data.frame(cbind(voted_data[,1],voted_data[,2],voted_data[,3],voted_data[,4],voted_data[,5]))
            file_votes_state = paste("./vote-result",state_name,sep="/")
            write.table(temp_voted_data,file = file_votes_state)
            state_vote_data = read.table(file_votes_state,sep =" ",header=T)
	        county_id = unique(state_vote_data[[2]])
	        for (id in 1:length(county_id)){
	        	county_voted_data = subset(state_vote_data , V2 == county_id[id])
		        Manu <- benford_law_2BL(as.list(county_voted_data[,3]))
		        Chelsea <- benford_law_2BL(as.list(county_voted_data[,4]))
		        Arsenal <- benford_law_2BL(as.list(county_voted_data[,5]))
		        
		        #print(Manu)
		        #print(Chelsea)
		        #print(Arsenal)

		        if(Manu == -1 | Chelsea == -1 | Arsenal == -1){
		            cat("i :",i, "  Single digit voting cannot process data\n")
		        }
		        else{
		            Pmanu <- pchisq(Manu,df=9,lower=F)
		            Pchelsea <- pchisq(Chelsea,df=9,lower=F)
		            Parsenal <- pchisq(Arsenal,df=9,lower=F)
		            #test <- cbind(Manu,Chelsea,Arsenal)
		            if( Pmanu < new_threshold){
		                #print()
		                #print(pchisq(Chelsea,df=9,lower=F))
		                #print(pchisq(Arsenal,df=9,lower=F))
		                count1 <- count1 + 1
		                cat("Fraud in Can 1 for state :"	, state_name, " and county id is: ",county_id[id] , " threshold was:" , new_threshold,"\n")
		                fline = paste("Fraud in Candidate 1"," for state :"	, state_name, " and county id is: ",county_id[id] , " threshold was:" , new_threshold)
		                cat(fline,file="./vote-result/analysis.txt" , append = TRUE)

		                #cat ("Pmanu :",Pmanu," Pchelsea :",Pchelsea,"  Parsenal :",Parsenal,"\n")
		            }
		            else if( Pchelsea < new_threshold){
		                #print()
		                #print(pchisq(Chelsea,df=9,lower=F))
		                #print(pchisq(Arsenal,df=9,lower=F))
		                count2 <- count2 +1
		                cat("Fraud in Can 2 for state :"	, state_name, " and county id is: ",county_id[id] , " threshold was:" , new_threshold,"\n")
		                fline = paste("Fraud in Candidate 1"," for state :"	, state_name, " and county id is: ",county_id[id] , " threshold was:" , new_threshold)
		                cat(fline,file="./vote-result/analysis.txt" , append = TRUE)		                
		                #cat ("Pmanu :",Pmanu," Pchelsea :",Pchelsea,"  Parsenal :",Parsenal,"\n")
		            }
		            else if(Parsenal < new_threshold){
		                #print()
		                #print(pchisq(Chelsea,df=9,lower=F))
		                #print(pchisq(Arsenal,df=9,lower=F))
		                count3 <- count3 +1
		                cat("Fraud in Can 3 for state :"	, state_name, " and county id is: ",county_id[id] , " threshold was:" , new_threshold,"\n")		                
		                fline = paste("Fraud in Candidate 1"," for state :"	, state_name, " and county id is: ",county_id[id] , " threshold was:" , new_threshold)
		                cat(fline,file="./vote-result/analysis.txt" , append = TRUE)		                #cat ("Pmanu :",Pmanu," Pchelsea :",Pchelsea,"  Parsenal :",Parsenal,"\n")
	            	}
		            else{
		                cat ("Pmanu :",Pmanu," Pchelsea :",Pchelsea,"  Parsenal :",Parsenal, " threshold was:" , new_threshold, " for state :"	, state_name ," and county_id: ",county_id[id],"\n")
		            }	
	        	}
		    }
		    cat("\n\n")
		}		
	}
    #cat ("Detected 1:",count1, " Detected 2:",count2,"  Detected 3:",count3,"  out of ", size, " times")
    passed <- (size - (count1 + count2 + count3 ))
    election_verification <- as.data.frame(cbind(size , passed, count1 , count2 , count3 ) )
	colnames(election_verification)[1] <- "Tries"
	colnames(election_verification)[2] <- "Passes"
	colnames(election_verification)[3] <- "MANU-FAILED"
	colnames(election_verification)[4] <- "CHELSEA-FAILED"
	colnames(election_verification)[5] <- "ARSENAL-FAILED"
	return (election_verification)
}

group_verification <- function(n = 20, batch = 5){

	batch_verification_temp <- list()
	for (i in 1:batch){
		batch_verification_temp[[i]] <- batch_verification(n)
	}
	group_verification_result <- as.data.frame(do.call("rbind",batch_verification_temp))
	return(group_verification_result)   
}
