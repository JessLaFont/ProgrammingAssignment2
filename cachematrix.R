# As per Programming Assignment 2, there are two functions written below. 
# The first, makeCacheMatrix, is designed to create a matix object and cache its inverse. 
# The second, cacheSolve, is designed to compute the inverse of the makeCacheMatrix. If the inverse has been calculated previously and the matrix has not changed, the cacheSolve function simply retrieves the cached data. 

# The makeCacheMatrix function creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The cacheSolve function computes the inverse of the matrix created by the makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed, cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message ("Retrieving cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

#Sample test:

# > x = rbind(c(2,-5), c(-5, 2))        ## Create a matrix

# > m = makeCacheMatrix(x)              ## Apply makeCacheMatrix function

# > m$get()                             ## Print Matrix Value

##      [,1] [,2]
# [1,]    2   -5
# [2,]   -5    2

# > cacheSolve(m)                       ## Apply caheSolve function

##       [,1]       [,2]                ## First run - calculates inverse
# [1,] -0.0952381 -0.2380952
# [2,] -0.2380952 -0.0952381

# > cacheSolve(m)                       ## Apply cacheSolve function again

## Retrieving cached data...            ## Retrieving Data message

##        [,1]       [,2]               ## returns cached data
# [1,] -0.0952381 -0.2380952
# [2,] -0.2380952 -0.0952381
> 