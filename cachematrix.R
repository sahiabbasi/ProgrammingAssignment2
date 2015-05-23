## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invx <- NULL # to store the inverted matrix

set <- function(y){
  x<<-y # contained the value within the env
  invx<<-NULL
}
get <- function() x #get the matrix
setInv <- function(inv) invx # set the inv matrix
getInv <- function(inv) invx #get the inv matrix

#create a list to store the above objects
list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
## Assuming that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
mt<-x$getInv() # read the inv matrix given
if(!is.null(mt)){ # first check if cached
  message("retrieving from cache")
  return (mt) #return the cached matrix
}
mt2<-x$get() # Otherwise read inverse matrix
mt3<-solve(mt2)#solve the inv matrix
x$setInv(mt3) # set the inv matrix
mt3 #return the inv matrix
}
