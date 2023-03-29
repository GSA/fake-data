## FUNCTIONS TO MAKE UNIVARIATE FAKE DATA WITH SAME DISTRIBUTION AS REAL DATA

fakeCat <- function(v, n) {
    # v is integer, string, or factor.
    # n is the desired length of the fake vector.
    w <- as.character(v)
    dist <- prop.table(table(w))
    vals <- names(dist)
    fake.v <- sample(vals, n, prob=dist, replace=TRUE)
    if (is.factor(v)) fake.v <- factor(v)
    if (is.integer(v)) fake.v <- as.integer(v)
    return(fake.v)
}


fakeCont <- function(v, n) {
    # v is a variable for which a kernel density estimate makes sense.
    # n is the desired length of the fake vector.
    dist <- density(v)
    # The simulation follows one of the examples for density().
    # The default kernel is gaussian, which is why rnorm is used.  
    # Bandwidth is the SD of the # kernel.
    fake.v <- rnorm(n, mean=sample(v, n, replace=TRUE), sd=dist$bw)
    return(fake.v)                    
}

## Try them out.

# Integer variable
aCat <- sample(1:4, 1000, prob=c(0.1, 0.3, 0.2, 0.4), replace=TRUE)


# Factor
#aCat <- factor(aCat, labels=c("a","b","c","d"))  

# Character
#aCat <- sample(c("a","b","c"), 1000, prob=c(0.15, 0.25, 0.6), replace=TRUE)


fCat <- fakeCat(aCat, 1e6)
print(rbind(prop.table(table(aCat)),
            prop.table(table(fCat))))

aCont <- rchisq(1e5, 2)
fCont <- fakeCont(aCont, 1e5)

plot(density(aCont))
points(density(fCont), col="Red1", type="l")
