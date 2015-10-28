# refer to notes for motivation and plan for class

# defining the class
# this uses setClass()

# Class is character string for name
# represenation is deprecated: use slots
# prototype declares prototype values. Note optional- R
# gives adequate empty value if not declared

setClass(
  Class = "trajectories",
  slots = list(
    times = "numeric",
    traj = "matrix"),
  prototype = prototype(
    times = 1,
    traj = matrix(0))
  )

# not the best looking output, will see later how to improve

new(Class = "trajectories")

new(Class="trajectories", times=c(1,3), traj=matrix(1:4, ncol=2))


# example:
hospital1 <- new(Class = "trajectories")

hospital2 <- new(Class = "trajectories",
                 times = c(1, 3, 4, 5),
                 traj = rbind( c(15, 15.1, 15.2, 15.2),
                               c(16, 15.9, 16, 16.4),
                               c(15.2, NA, 15.3, 15.3),
                               c(15.7, 15.6, 15.8, 16))
                 )

hospital3 <- new( Class= "trajectories",
                  times=c(1: 10, (6: 16) *2),
                  traj=rbind(
                    matrix (seq (16,19, length=21), ncol=21, nrow=50, byrow=TRUE),
                    matrix (seq (15.8, 18, length=21), ncol=21, nrow=30, byrow=TRUE)
                  ) + rnorm (21*80,0,0.2)
                  )

# WARNINGS
# do not use @, attr or attributes to access slots! I guess we will see
# getter methods defined in a bit

# Removing class:

# make new class
setClass(
  Class = "trajectoriesB",
  slots = list(
    times = "numeric",
    traj = "matrix"),
  prototype = prototype(
    times = 1,
    traj = matrix(0))
)

removeClass("trajectoriesB")
new(Class = "trajectoriesB") # get error

# NOTE: removing class does not remove methods associated with class!!

# Empty Objects:
# numeric() and numeric(0)
# character and character(0)
# integer() and integer(0)

# factor() is empty factor. factor(0) is factor with level 0
# matrix() is matrix 1 line, 1 column NA
# matrix(nrow = 0, ncol = 0) is an empty matrix
# array() is an array with one line and one column contain NA
# NULL is null object (class NULL)

# to test if an object is empty, CHECK LENGTH == 0!!

## To see objects:

# slotNames give slot names
slotNames("trajectories")

# getSlots gives the name of the slots and their type.
getSlots("trajectories")

# getClass gives the names of slots and their type, but also heirs and ancestors
getClass("trajectories")

## METHODS

# we know plot reacts differently in different situations-
# because its a generic!
size <- rnorm(10, 1.70, 10)
weight <- rnorm(10, 70, 5)
group <- as.factor(rep(c("A","B"), 5))
par(mfrow=c(1, 2))
plot(size ~ weight)
plot(size ~ group)


# We want to define a plot method for our objects
# For that, one uses the function setMethod. It takes three arguments:
# 1. F is the name of the function which we are redefining.
# In our case, plot

# 2. signature is the class of the object to which it applies.
# We will speak more above that section 8.2 page 38

# 3. definition is the function to be used. In our case,
# it will simply be a matplot that will take in account times of measurements

setMethod(
  f = "plot",
  signature = "trajectories",
  definition = function(x, y, ...) {
    matplot(x@times,
            t(x@traj),
            xaxt = "n",
            type = "l",
            ylab = "",
            xlab = "",
            pch=1)
    axis(1, at = x@times)
  })

plot(hospital3)

# note above, we are obliged to have the arguments (x, y, ...)
# as the plot generic states this

## lets make a print method

setMethod(f = "print",
          signature = "trajectories",
          function(x, ...) {
            cat("*** Class Trajectories, method Print *** \n")
            cat("* Times = "); print(x@times)
            cat("* Traj = \n"); print(x@traj)
            cat("******* End Print (trajectories) ******* \n")
          }
          )

# for hospital 3, print is not so good. Lets make a show as well

setMethod("show","trajectories",
          function(object){
            cat("*** Class Trajectories, method Show *** \n")
            cat("* Times = "); print (object@times)
            nrowShow <- min(10, nrow(object@traj))
            ncolShow <- min(10, ncol(object@traj))
            cat("* Traj (limited to a matrix 10x10) = \n")
            if(length(object@traj) != 0){
              print(formatC(object@traj[1:nrowShow,1:ncolShow]), quote=FALSE)
            }else{}
            cat("******* End Show (trajectories) ******* \n")
          }
)

# Note there is a condition for empty objects.
# new() creates an object, then display it using show. In the case of new without
# any argument, the empty object is send to show (if no prototype).
# However, show as we conceived it cannot treat the empty object.
# More generally, all our methods must take into account the fact that they may have
# to deal with the empty object:


# note that we can get carried away and make summary, etc


# setGeneric

# if we want some methods specific to trajectories, we can define them ourselves

setGeneric (
  name = "countMissing",
  def = function(object){
    standardGeneric("countMissing")
  }
  )

# This adds countmssing to the generics known by R
# we can now make a method specific to class trajectories:

setMethod(
  f = "countMissing",
  signature = "trajectories",
  definition = function(object) {
    return(sum(is.na(object@traj)))
  })

countMissing(hospital2)

# note it is possible to 'lock' a generic by use of
# lockBinding()

## Seeing and getting methods

showMethods(classes = "trajectories")

getMethod(f = "plot", signature = "trajectories")

existsMethod(f = "plot", signature = "trajectories")


## COnstruction

# constructors are tools that aneable yp build correcy objexts- that is methods of creation
# (eg storing methods in slots)
# abd checking values in slots. I guess this is referring to getter and setters and the like?

# Inspectors
# these conytrfol for internal inconsistencies. validity arrgument in setClass

setClass(
  Class = "trajectories",
  slots = list(
    times = "numeric",
    traj = "matrix"),
  
    validity = function(object) {
      cat("~~~trajectories inspector~~~ \n")
      if(length(object@times) != ncol(object@traj)) {
        stop("[trajectories: validation] the number of temporal measurments is wrong")
      } else {
        
      }
    return(TRUE)
  }
)

new(Class = "trajectories", times = 1:2, traj = matrix(1:2, ncol = 2))

# get suitable error message
new(Class = "trajectories", times = 1:3, traj = matrix(1:2, ncol = 2))

# NOTE- the inspector is only called when create new object. If modify slots directly,
# no checking done. As will soon see, can use setters to slve this problem.


# INITIALIZER

# This is a method called at each object construction, allows us to set slots to values
# As an example, lets look to clean up the trajectories by giving names and times

# Note, we can use the inspector in the initilizer with a called to validObject

setMethod(
  f = "initialize",
  signature = "trajectories",
  definition = function(.Object, times, traj) {
    cat("~~~ trajectories: initialising ~~~ \n")
    if(!missing(traj)) {
      colnames(traj) <- paste("T", times, sep = "")
      rownames(traj) <- paste("I", 1:nrow(traj), sep = "")
      .Object@times <- times
      .Object@traj <- traj
      #call inspector
      validObject(.Object)
    }
    return(.Object)
  }
)

new(Class = "trajectories", times = c(1, 2, 4, 8), traj = matrix(1:8, nrow = 2))

new(Class = "trajectories", times = c(1, 2, 4), traj = matrix(1:8, nrow = 2))

# Note that the constructor does not eccessariliy have to take the slots of the object
# as argument (see notes for example)

# There may only be one initializer per class!!


# Constructor for user

# new() is not a friendly function, so can make user friendly constructors

trajConstr <- function(times, traj) {
  cat("~~~ trajectories: constructor ~~~ \n")
  new(Class = "trajectories", times = times, traj = traj)
}

trajConstr(time = c(1, 2, 4), traj = matrix(1:6, ncol = 3))

# you can define multiple constructors, depending on situation.
# for eaxmple in some cases, trajectories can be assumed to
# increase by a constatnt interval, so only times need specifying etc etc

# all constructors call the initializer- this is why need a global initialiser that
# can deal with all cases.

# Summary- during construction of an object, there are three places to carry out operations:
# in the construcion function
# in the initializer
# in the inspector

# the construction function is called by the user. it is general, can take various arguments,
# (not neccessatily object attributes), and always ends with a new()

# the initializer is called by new(). It is in charge of giving each slot a value. If it is
# not defined, R calls the default which assigns values for the slot, and calls the inspector

# the inspector controls the internal coherence of the object.it cannot modify, just check

# one suggestion is not to use the initializer- prepare with the constructor, then just call the
# default. This can be more efficient.

# Accessor

# using @ is bad, so use accessors to recover values of slots
# lets define our basic getters:

setGeneric("getTimes",
           function(object) {
             standardGeneric("getTimes")
           })

setMethod(f = "getTimes",
          signature = "trajectories",
          function(object){
            return(object@times)
          })

getTimes(hospital2)

setGeneric("getTraj",
           function(object) {
             standardGeneric("getTraj")
           })

setMethod(f = "getTraj",
          signature = "trajectories",
          function(object){
            return(object@traj)
          })

getTraj(hospital2)

# of course, we are free to define getters as complex as we like.
# lets just get the trajectories at inclusion time:

setGeneric("getTrajInclusion",
           function(object) {
             standardGeneric("getTrajInclusion")
           })

setMethod(f = "getTrajInclusion",
          signature = "trajectories",
          function(object){
            return(object@traj[, 1])
          })

getTrajInclusion(hospital2)

# setters

# setter is a method that assigns a value to a slot
# replacement operator? read up in hadley's book

# Note, contrary to notes, use setMethod with '<-' in the name
# all setReplaceMethod does is paste a '<-', so functionally equivalent
# but apparantly roxygen2 has issues with setReplaceMet

setGeneric("setTimes<-",
           function(object, value) {
             standardGeneric("setTimes<-")
           })

setMethod(f = "setTimes<-",
                 signature = "trajectories",
                 definition = function(object, value){
                   object@times <- value
                   # be sure to test is still valid!
                   validObject(object)
                   return(object)
                 })

getTimes(hospital2)
setTimes(hospital2) <- c(5, 6, 7, 8)
getTimes(hospital2)
setTimes(hospital2) <- c(5, 7, 8)
getTimes(hospital2)

## METHODS using several arguments

# we want top investigate interactions between object

setClass(
  Class = "Partition",
  representation = representation(
    nbGroups = "numeric",
    part = "factor"
  )
)

setGeneric(
  "getNbGroups",
  function(object) {
    standardGeneric("getNbGroups")
  }
)

setMethod(
  "getNbGroups",
  "Partition",
  function(object) {
    return(object@nbGroups)
  }
)

setGeneric(
  "getPart",
  function(object) {
    standardGeneric("getPart")
  }
)

setMethod(
  "getPart",
  "Partition",
  function(object){
    return(object@part)
  }
)


part1 <- new(Class = "Partition",
             nbGroups = 2,
             part = factor(c("A", "B", "A", "B"))
             )
part2 <- new(Class = "Partition",
             nbGroups = 2,
             part = factor(rep(c("A", "B"), c(50, 30)))
)

# AIM - we have ibject of class Trajectories and objects of class Partition
# as trajectories are associetd with groups, we would like to colour them differntly...
