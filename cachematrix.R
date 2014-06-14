## R function is first class citizen, function can accept data and function.
## Hence, function B can be passed as argument for function A.
## R is not the only language with "higher order function" feature.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) m <<- inv # store inv into global environment
    get_inverse <- function() m
    list(set = set, get = get,  # return list of 4 function
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m # return inversed matrix
}

## create cacheable matrix object
#m <- makeCacheMatrix()

# initailize with a an easy to inspect matrix 
#m$set( matrix( c(0, 2, 2, 0 ), 2, 2))

# note use of parens to retrive the matrix part of the object
#m$get()
#     [,1] [,2]
#[1,]    0    2
#[2,]    2    0

# test the inverse cacher
#cacheSolve( m )
#       [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0

# and again... should be cached now
#cacheSolve( m )
#getting cached data
#     [,1] [,2]
#[1,]  0.0  0.5
#[2,]  0.5  0.0

# test that the inverse works and experiment with how to use the functions
# m$get() returns the matrix and cacheSolve(m) returns the inverse that we can 
# use like regular matrices to do things like multilplications...
#
# product of matrix mult should be identity matrix AND we should get the cached message
#m$get() %*% cacheSolve(m)
#getting cached data  <-- Yup... cached!
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1       <-- eye(), identity Matrix... diag(2) / eye(2) in matlab

# let R test identify for us
#all.equal( diag(2), m$get() %*% cacheSolve(m) )
# getting cached data <-- hey.. it's still cached
#[1] TRUE             <-- R agrees it's an identity