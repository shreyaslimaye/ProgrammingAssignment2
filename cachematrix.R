## Put comments here that give an overall description of what your
## functions do

# The functions aim to achieve the following tasks
# Set value of a matrix
# Get value of a matrix
# Set the inverse
# Get the inverse

# Create a matrix from a vector and number of rows.
# This function creates a matrix that is used in the following two functions
createMatrix <- function(x1 = numeric(), x2){
  m1 <- matrix(x1, nrow=x2)
}

## Write a short comment describing this function

# Use the matrix created in createMat to get the value of matrix
makeCacheMatrix <- function(a1 = createMatrix(x1,x2)){
  m <- NULL
  set <- function(b1) {
    a1 <<- b1
    m <<- NULL
  }
  get = function() a1
  require(MASS)    # Required for the ginv function that gets the inverse
  setinverse = function(ginv) m <<- inverse
  getinverse = function() m
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

# The following function returns inverse matrix for the matrix in the above function
# If the inverse exists in repository, then it returns from cache
cacheSolve <- function(x1, ...) {
  
  inverse <- x1$getinverse()
  
  # Get cacheed inverse
  if (!is.null(inverse)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inverse)
  }
  
  # If not cacheed, calculate the inverse 
  matrix.data = x1$get()
  require(MASS)  # Required for the ginv function
  inverse = ginv(matrix.data, ...) #ginv function returns inverse
  
  # Use the setinverse function.
  x1$setinverse(inverse)
  
  return(inverse)
}


