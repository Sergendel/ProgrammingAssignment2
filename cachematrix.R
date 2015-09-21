# The assignment is to write a pair of functions that cache the inverse of a matrix


#  1. makeCacheMatrix: This function creates a special "matrix" object
#     that can cache its inverse.

# SOLUTION
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           # Initilization
  
  set <- function(y) {  # define set function
    x <<- y             # Use "<<-" super assignment 
    inv <<- NULL        # operator to chage x and inv.
  }
  get <- function() x   # get the matrix
  
  setinverse <- function(inverse) inv <<- inverse   # set th einverse
                                                    # Uses "<<-" to modify  inv
  
  getinverse <- function() inv                     # get the inverse   
  
                  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  # return th object
}





# 2. cacheSolve: This function computes the inverse of the special "matrix" returned 
#    by makeCacheMatrix above. 
#    If the inverse has already been calculated (and the matrix has not changed), 
#    then the cachesolve should retrieve the inverse from the cache.

#SOLUTION:
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                       # load the inverse from cashe 
  
  if(!is.null(inv)) {                                # Check if inv was already calculated
    message("getting inverse from cached data.")     # If yes - return the cashed value
    return(inv)
  }
                                              # otherwise:
  data <- x$get()                                     # get the matrix         
  inv <- solve(data)                                  # calculate the invrese of the matrix  
  x$setinverse(inv)                                   # store it in cash
  inv                                       
}