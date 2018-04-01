# this is an implementation of the first example in clustering course, from page 8-11
# data table
personne <- matrix(c(50,11000,70,11100,60,11122,60,11074),nrow = 4,byrow = T)
rownames(personne) <- c('p1','p2','p3','p4')
colnames(personne) <- c('age','salaire')



#redefine function mad
z_mad <- function(row)
{
  new_row <- sum(abs(row-mean(row)))
  return(new_row)
  #new_row <- abs(row-mean(row))
  #return((1/length(row))*(sum(new_row)))
}

#score function
z_score <- function(row){
  score  <- (row-mean(row))/z_mad(row);
  return(score)
}

#calculate z-scores
age_score <- z_score(personne[,'age']);
salaire_score <- z_score(personne[,'salaire'])

#new table with scores (data table clone with scores values)
personne_score <- personne
personne_score[,'age'] <- age_score
personne_score[,'salaire'] <- salaire_score

# print data table
cat('matrix of values')
print(personne)
cat('\n')

#print distances table
cat('matrix of distances  [manhattan] \n')
print(dist(personne,method = 'manhattan'))
cat('\n')

#print score table
cat('matrix of scores \n')
print(personne_score)
cat('\n')

#print table of scores distances (using the manhattan formula)
cat('matrix of scores distances  [manhattan] \n')
print(dist(personne_score,method = 'manhattan'))



