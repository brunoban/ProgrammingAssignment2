## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates the framework for the function makeCacheMatrix
## It creates the inv item inside the function and initially assigns it NULL
## These values will be retained outside the scope of the function, so we will be able to call them again

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Sets the default values for the function
  set <- function(y) {
    ## Stores the default values for the function in the scope inside the function
    x <<- y
    inv <<- NULL
  }
  ## Creates the setter and getter method for the function
  get <- function() x # Gets the value of the matrix
  setInvert <- function(solve) inv <<- solve  # Sets the inverted matrix
  getInvert <- function() inv # Gets the inverted matrix
  ## Lists the setter and getter methods for the function
  list(set = set, get = get,
       setInvert = setInvert,
       getInvert = getInvert)
}


## Write a short comment describing this function
## Checks if the inverted matrix already exists inside the object
## If it does, it retrieves that object from "cache". 
## If it doesn't, it creates the inverted matrix and stores it inside the object.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInvert() #Retrieves the inverted matrix from x
  if(!is.null(inv)) { 
    ##If the inverse matrix retrieved from x does not equal NULL (exists), retrieve its "cached" value
    message("getting cached data")
    return(inv)
  }
  #If it equals NULL (does not exist), create it and set it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setInvert(inv) # Set the inverted matrix inside the object. "Caches" it.
  inv
}
