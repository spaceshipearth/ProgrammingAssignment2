###This R code creates a cache of the inverse matrix, as required by Assignment 2 

## Overall program descriptions: 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  # above. If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.

## 

# makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# Following the example in class, it performs 4 steps: 
# 1) sets the value of the matrix
# 2) gets the value of the matrix
# 3) sets the cache inverse of the matrix
# 4) gets the cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # sets the stored matrix
  matrixcache <- NULL
  set <- function(ynew) {
    x <<- ynew
    matrixcache <<- NULL
    
  }
  
  
  # returns the stored matrix
  get <- function() {
    x
  }
  
  
  # set the cache inverse 
  setCache <- function(solve) {
    matrixcache <<- solve
  }
  
  
  # get the cached inverse
  getCache <- function() {
    matrixcache
  }
  
  # return a list, as in the assignment example
  list(set = set, get = get, setCache = setCache, getCache = getCache)

}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

# Following the example in class, it performs 3 steps: 
# 1) get the cached value, if exists
# 2) return message when "getting cached data"
# 3) if cached doesn't exist, calculates inverse sets as cache 
matrixcacheSolve <- function(y, ...) {
  # get the cached value, if exists
  m <- y$getCache()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  
  # if cached value doesn't exist, caclulate the inverse and set cached
  data <- y$get()
  m <- solve(data)
  y$setCache(m)
  m
}
