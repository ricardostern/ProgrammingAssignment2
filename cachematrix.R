# John's Hopkins - R Programming Course (Coursera)
# Week 3 Coding Assignment - caching the invert matrix of a matrix
#
# Note: This assignment used as starting point the examples given in 
# the assignment page to cache the mean of a vector.

#
# makeCacheMatrix 
#
# Factory method to create an enhanced *object* that caches the inverse 
# of a matrix, by using R's lexical scoping.

makeCacheMatrix<- function(x = numeric()) 
{
    # Initialize cached inverse
    inv <- NULL

    # *set* member function
    # initializes the object x and its inverse
    set <- function(y) 
    {
        # initializes the value of x and inv in the parent scope
        x <<- y
        inv <<- NULL
    }
    
    # *get* member function
    # returns the value of the enhanced matrix
    get <- function() 
    {
        x
    }

    # *getsolve* member function
    # returns the inverse of a function - if cached, returns it from cache,
    # otherwise executes solve. 
    #
    # Note: We are not passing in the additional optional argument of solve
    # the inverse is only achieved when no other args are passed
    getsolve <- function() 
    {
        if(is.null(inv)) 
        {
            # not previously cached, cache the value
            inv <<- solve(get())
        }   
	  else
        {
            # tracing message only
            print("getting cached data")
        }

        # always return it from cache
        inv
    }

    # returns a list of all available member methods
    list(set = set, 
         get = get,
         getsolve = getsolve)
}

#
# cacheSolve
#
# returns the inverse of a function - if cached, returns it from cache,
# otherwise executes solve.
# 
# Note: Instead of having the logic for checking and updating the cache in this
# method we chose to add it to the getsolve() message, as it is closer to the data
# it modifies and, hence, would yield better maintainability. In fact, if the 
# assignment did not ask for a separate cacheSolve() function I would just have used
# x$getsolve(.

cacheSolve <- function(x) 
{
    x$getsolve()
}

