## Addi Faerber, October 5, 2018
## Coursera Data Science Course
## Week 3 - Programming Assignment 2: Lexical Scoping

##These functions create a matrix object with functions
##for getting and setting the inverse of a given matrix
##and caches the results within the function environment
##so subsequent calls for the inverse of the matrix
##will not have to recalculate the inverse

## makeCacheMatrix creates an object that holds a matrix (x) and the inverse of
## the matrix (i), along with four functions: set() to create the data object
## and get() to retrieve the object from memory, setinverse() and getinverse()
## to get and set the matrix inverse.
## makeCacheMatrix returns a list


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve pulls the inverse from an object created by makeCacheMatrix(). If
## no inverse is found, the inverse is calculated. If an inverse is found, the
## inverse is pulled from the makeCacheMatrix object and returned.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("Getting cached inverse")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix) #calcuate the inverse of the matrix x
  x$setinverse(i)
  i
}

