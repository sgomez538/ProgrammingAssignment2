makeCacheMatrix <- function(x = matrix()){
  inversa <- NULL
  set <- function(y){
    x<<-y
    inversa<<-NULL
    
  }
  get<-function(){x}
  setInversa<- function(inversacalculada){inversa<<-inversacalculada}
  getInversa<- function(){inversa}
  list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}

x<-makeCacheMatrix(matrix((1:4),  nrow = 2, ncol = 2))
x$get()

cacheSolve <- function(x, ...) {
  inversa <- x$getInversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setInversa(inversa)
  inversa
}