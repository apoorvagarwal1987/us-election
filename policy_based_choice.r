vote_generation_precints_policy_based <- function(){
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
	candidate_policies = cbind(c(1,0,0),c(0,0.5,0),c(0,1,0.5),c(1,0,0),c(0,1,1),c(1,0,0),c(1,1,1),c(1,0,0.5))
	candidate_policies <- matrix(as.numeric(candidate_policies),ncandidates,npolicies)
	person_policy_liking = policy_weight(precint_size,npolicies)
	person_candidate_distance = candidate_policies %*% person_policy_liking
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