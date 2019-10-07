## makeCacheMatrix creates and returns a list of functions

makeCacheMatrix <- function(x = numeric()) 
{
    i <- NULL
  s <- function(y) 
{
          x <- y
          i <- NULL
  }
  g<- function() x
  sinv <- function(inverse) 
i <- inverse
  ginv <- function() i
  list(s = s,
       g = g,
       sinv = sinv,
       ginv = ginv)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) 
{
  i <- x$ginv()
  if (!is.null(i))
 {
          print("getting cached data")
          return(i)
  }
  data <- x$g()
  i <- solve(data, ...)
  x$sinv(i)
  i
}
