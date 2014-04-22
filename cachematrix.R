## Put comments here that give an overall description of what your
## functions do


# We need a function to compute the inverse of matrices assuming that they
# are invertible (i.e, they have non-zero determinants). This is an intensive
# operation and can take time if it has to be repeatedly runto access the inverse.
# This would be the case if, for example, we did Z <- solve(M) and then used Z to
# access the inverse everytime. It would be faster if we could cache the inverse
# matrix in memory so that it does not have to be recalculated.
# What we will do here is create two functions which will
#    a) take a supplied matrix and store it in memory
#    b) compute an inverse and store it in cache
#    c) clear the cache when a new matrix is supplied
#    d) Recompte the inverse and again store it for use downstream


## Write a short comment describing this function


# In the makeCacheMatrix function below, we will achieve the following:
#    1) Set the value of the matrix x in the parent environment of the function
#    2) Define a list for storing the values of the matrix, its inverse using
# getter and setter functions.
# So we end with a list of 4 elements - the supplied matrix, a null matrix as
# its inverse , a get function to retireive the matrix and a get function to
# retreive its inverse
# The matrix and its inverse(as yet NULL) are stored in the parent environment
# of the makeCacheMatrix function
# The inverse is not computed  by this function.
# If a new matrix is supplied, the cahched inverse is removed from the parent 
# environment and we again start with a NULL matrix and continue the process
# of cache and retrieval

makeCacheMatrix <- function(x= matrix() ) {

      inv<- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Write a short comment describing this function

# ## This function will take the matrix created by the makeCacheMatrix function
# ## It will compute the inverse of the matrix using the solve() function in R
# ## If the inverse is NULL becasue there is no cached inverse from a prior 
# operation, it will get the supplied matrix from the makeCacheMatrix get 
# function and compute the inverse using inv <- solve(matrix,...). 
# IT will then use the setinverse function to store the inverse in the cache.
# When  this function is run the first time, it sets the value of the inverse 
# and when run the second time, it retrieved the cached matrix and prints 
# "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      matrix <- x$get()
      inv <- solve(matrix, ...)
      x$setinverse(inv)
      inv 
}

###############################################################################




