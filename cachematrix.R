################################################################################
## cachematrix.R
##
##              Data Science Specialization via Coursera
##
##                   Programming Assignment 2
##                        Lexical Scoping
##
##                  Author: Vincenc Podobnik
##
##
## Possible improvement:
##  Could cache inverse on makeCacheMatrix$set() by default
##
##
################################################################################




##
## Cashable Matrix wrapper. Creates a list object that pretends to be a
## cacheable matrix. Stores the inverse of the matrix in the
## "matrix.inverse" variable and exposes get/sets via the list.
##
makeCacheMatrix <- function(x = matrix()) {

    matrix.inverse <- NULL

    # Setting the matrix (or overwriting it) resets the inverse
    set <- function( matrix ){
        x <<- matrix
        matrix.inverse <<- NULL
    }

    get <- function() x

    get.inverse <- function() matrix.inverse
    set.inverse <- function( inverse )  matrix.inverse <<- inverse


    # List exposes the internal functions to the outside world
    list(
        set = set,
        get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse
    )

}



##
## Retrieves the cached matrix inverse if is exists, else calculates and caches
## the value of solve() for future use.
##
cacheSolve <- function(x, ...) {

    matrix.inverse <- x$get.inverse()


    ## If cached inverse is found return it.
    ##
    if(!is.null( matrix.inverse )) {
        message("Found cached data")
        return(matrix.inverse)
    }

    ## Generate the inverse and cache it for future
    ##
    data <- x$get()
    matrix.inverse <- solve(data, ...)
    x$set.inverse(matrix.inverse)

    ## Return the inverse
    matrix.inverse

}



## OPTIONAL ####################################################################
##
## The assignment does not require submitting code to actually test the
## validity of the functions. Nonetheless these are provided below in a
## commented block.
##
## To execute remove only the first '# ' from the beginning of each line below
##
################################################################################
#
#
# ## Create a simple matrix
# ##
# m <- matrix( 1:4, 2, 2 )
# m
#
# ## "Promote" the matrix to a cashable one
# ##
# cm <- makeCacheMatrix( m )
#
# ## Check it works
# ##
# cm <- makeCacheMatrix( m )
#
#
# ## Prove both matrices are equal. Expexted result is TRUE, TRUE, TRUE, TRUE
# ##
# m == cm$get()
#
#
# ## Lets see if it's working
# ##
# cacheSolve( cm )
#
# ## Proof the inverse really is an inverse
# ##
# m %*% cacheSolve( cm )

