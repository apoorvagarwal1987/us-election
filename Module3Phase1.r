#*********************************************************************Data Loading****************************************************************************
library("data.table")
library("gdata") #for loading data from excel-sheet

#Information about the states
us_states_census_info = read.table("data/USA_States.txt" ,sep="," ,header =T)

# To collect the information like total population, number of precints for each county
#us_precint_county_info = read.table("data/precints_census/02.tab", sep = "\t",header=T )
us_precint_county_info = read.xls("data/precints_census/02.xls",sheet=1,na.strings='NA')


#To have the information about state and electoral
state_electoral = read.xls("data/state-electoral.xlsx",sheet=1,na.strings='NA')

#*******************************************************************Generic Functions*********************************************************************
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

vote_generation_precints_fraud1 <- function(){
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
		choice <- candidate_choice(length(sections))
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
	votes_distributed <- c(us_precint_info,votes_distributed)
	temp <- as.data.frame(votes_distributed)
	#write.table(temp)
	return (votes_distributed)
}

#Aggregation of the votes for all the precints for the particular at the county level
vote_aggregation_county <- function(){
	votes_precints <- vote_generation_precints_fraud3()
	dt <- as.data.table(votes_precints)
	county_result <- data.frame(dt[,list(Precints = .N,Population = sum(Population),MANU = sum(MANU),CHELSEA = sum(CHELSEA),ARSENAL = sum(ARSENAL)),by="County-Code"])
	colnames(county_result)[1] <- "County-Code"
	write.table(county_result)
	return(county_result)
}

#Aggregation of the votes for all the counties for the particular state at the state level
result_state <- function(state){
	vote_tally <- vote_aggregation_county()
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
electoral_allocation <- function(state){
	state_result <- result_state(state)
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
random_state_election <- function(n,state){
	random_vote <- list()
	for(i in 1:n){
		dataframe <- electoral_allocation(state)
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
random_wins <- function(n,state){
	result_tally <- random_state_election(n,state)
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
batch_election <- function(batch, n,state){
	# precints_info_files <- list.files(path="data/precints_census/")
	# #match <- paste(as.character(stateid),"xls",sep=".")
	# for(i in precints_info_files){
	#     file_name <- paste("data/precints_census/",i,sep="/")
	#     x <- read.xls(i,sheet=1,na.strings ='NA')
	#     # if(match == i){
	#     #     us_precint_county_info <- x
	#     # }
	#     assign(i,x)

	# }    
	full_election_temp <- list()
	for (i in 1:batch){
		full_election_temp[[i]] <- random_wins(n,state)
	}
	full_election <- as.data.frame(do.call("rbind",full_election_temp))
	return(full_election)    
}

#***********************************************************Validation Functions*****************************************************************************

library("plyr")
library("stringr")
#Function for finding the Benford Probability of the given number
benford_probability_2Dig <- function(digit){
	result <- 0
	for (i in 1:9){
		value <- 1 + (1/(10 * i + digit))
		result <- result + log10(value)
	}
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
	num <- as.numeric(strsplit(as.character(number),"")[[1]])[len-(digit-1)]
	return(num)
}

benford_law <- function(county_result){
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
		else {}
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

verify_result <- function(){
	Manu <- benford_law(as.list(voted_data[,4]))
	Chelsea <- benford_law(as.list(voted_data[,5]))
	Arsenal <- benford_law(as.list(voted_data[,6]))
	print(Manu)
	print(Chelsea)
	print(Arsenal)
	#test <- cbind(Manu,Chelsea,Arsenal)
	print(pchisq(Manu,df=9,lower=F))
	print(pchisq(Chelsea,df=9,lower=F))
	print(pchisq(Arsenal,df=9,lower=F))
	
#    print(pchisq(Chelsea,df=9))
#    print(dchisq(Arsenal,df=9))
}
