## makeCacheMatrix function deploys calculated matrix with their solutions.
## cacheSolve function first searches if that matrix was calculated before and if so takes the answer from makeCacheMatrix. 
## But if it wasn't calculated than inverses the matrix and sends it to makeCacheMatrix to deploy it. 


## This function first initializes the variables,
## Sub function set defines the variable x as y and m as null in outside of the scope (that is how we can achieve them) 
## get function returns x and setinv function assigns m to inv which is the solution out of the scope
## getinv takes precalculated inverse of matrix
## list deploys our variables
makeCacheMatrix<- function(x=matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setinv <- function(inv) {m <<- inv}
  getinv <- function() {m}
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if matrix was calculated before and returns the solution if it deployed.
## First it searches the solution with getinv and sets it to m, if m is not null than function returns the solution.
## If it is not calculated before data is passed to x in makeCahceMatrix, and solution is assigned to m and it deployed with setinv
## Finally it returns the solution.

cacheSolve<- function(x, ...){
  m <- x$getinv()
  if(!is.null(m)){
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}

##Examples:
my_matrix = makeCacheMatrix(matrix(c(1,1,2,3,4,5,7,2,3),3,3))
my_matrix$get()
cacheSolve(my_matrix)
solve(matrix(c(1,1,2,3,4,5,7,2,3),3,3)) #Proving our codes work.

my_matrix$set(matrix(c(1,1,2,3), 2, 2))
cacheSolve(my_matrix)
my_matrix$getinv()
solve(matrix(c(1,1,2,3), 2, 2))

