## These functions accept a square matrix, and compute and cache 
##the inverse of this matrix. The cached inverse is can be called
## without having to actually recalculate the inverse of this
## matrix, thereby saving computation time. 
## These functions also demonstrate the concept of lexical 
## scoping in R and the working of the "<<-" operator. 


## makeCacheMatrix is the "caching" function. It contains four 
## functions within it to a) set a matrix, b) get the currently
## assigned matrix, c) set the inverse of this matrix and,
## d) get the inverse of this matrix. 
## subfunctions are put into a list to allow console access by
## function name

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## set the inverse to NULL when creating the matrix
  set <- function(y) { 
    ## this function sets our matrix object with the matrix passed
    x <<- y
    inv <<- NULL ## make sure we reset inverse to NULL every time
                 ## we set the value of the matrix
  }
  get <- function() {
    x ## returns the value of the matrix
  }
  setinverse <- function(inverse){
    inv <<- inverse #this is the "cached" value of the inverse
  }
  getinverse <- function(){
    inv ##this will return the inverse
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This is the inverse solver function that
## calculates the inverse of a matrix. 
## the function "ginv()" is used to calculate the inverse
## Install the MASS package to use ginv()
## once calculated this function passes this inverse to the 
## cached variable "inv" and stores it there. 
## future refrences to this (specific) inverse return this 
## cached value instead of having to recalculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data)
  x$setinverse(inv)
  inv
  
}

