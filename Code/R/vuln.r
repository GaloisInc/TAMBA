n = 10
unif = rep(1,n)
unif = unif / sum(unif)
# unif is now normalized

# Uniform distribution on n elements
unif <- function(n) {
    unif = rep(1,n)
    unif = unif / sum(unif)
    return(unif)
}

norm <- function(v) {
    return(v / sum(v))
}

# A range query that returns 1 if a <= x < b (with x between 1 and n)
range <- function(n,a,b) {
    pre = rep(1,a-1)
    mid = rep(0,b-a)
    post = rep(1,n-b+1)
    v0 = c(pre,mid,post)
    pre = rep(0,a-1)
    mid = rep(1,b-a)
    post = rep(0,n-b+1)
    v1 = c(pre,mid,post)
    c = matrix(c(v0,v1),ncol=2)
    return(c)
}

colNorm <- function(ch) {
    for(i in 1:dim(ch)[2]) {
        s = sum(ch[,i])
        if(s != 0)
            ch[,i] = ch[,i]/sum(ch[,i])
    }
    return(ch)
}

colMax <- function(data) apply(data,2,max)

expVuln <- function(prior,channel) {
    post = prior * channel
    out_probs = colSums(post)
    post_dist = colNorm(post)
    max_probs = colMax(post_dist)
    return(sum(max_probs * out_probs))
}

maxVuln <- function(prior,channel) {
    post = prior * channel
    post_dist = colNorm(post)
    max_probs = colMax(post_dist)
    return(max(max_probs))
}

number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    if(missing(noBits)) {
        return(binary_vector)
    } else {
        binary_vector[-(1:(length(binary_vector) - noBits))]
    }
}

prior = unif(10)
channel = range(10,2,4)

vulnFor = function(channel,vulnFn) {
    size = dim(channel)[1]
    u = unif(size)
    vuln = numeric(2**size-1)
    for(i in 1:(2**size-1)) {
        prior = norm(number2binary(i,size) * u)
        vuln[i] <- vulnFn(prior,channel)
    }
    return(vuln)
}

mulLeakageFor = function(channel,vulnFn) {
    size = dim(channel)[1]
    u = unif(size)
    leakage = numeric(2**size-1)
    for(i in 1:(2**size-1)) {
        prior = norm(number2binary(i,size) * u)
        priorVuln = max(prior)
        leakage[i] <- vulnFn(prior,channel)/priorVuln
    }
    return(leakage)
}

mulLeakageForNonZero = function(channel,vulnFn) {
    size = dim(channel)[1]
    u = unif(size)
    leakage = numeric(2**size-1)
    for(i in 1:(2**size-1)) {
        prior = norm(number2binary(i,size) * u)
        priorVuln = max(prior)
        if(0 %in% colSums(prior * channel)) {
            leakage[i] = NA
            next
        }
        leakage[i] <- vulnFn(prior,channel)/priorVuln
    }
    return(leakage)
}

runTests = function(vulnFn, leakageFn) {
    Large = function(f) { return(leakageFn(range(10,1,5),f)) }
    Med1 = function(f) { return(leakageFn(range(10,1,3),f)) }
    Med2 = function(f) { return(leakageFn(range(10,5,7),f)) }
    Small = function(f) { return(leakageFn(range(10,5,6),f)) }
    for(i in list(list("Large",Large),list("Med1",Med1),
                  list("Med2",Med2),list("Small",Small))) {
        v = i[[2]](vulnFn)
        print(i[[1]])
        print(c("mean",mean(v, na.rm = TRUE)))
        print(c("summary",summary(v, na.rm = TRUE)))
    }
}

print("expVuln (with zeros)")
runTests(expVuln, mulLeakageFor)
print("maxVuln (with zeros)")
runTests(maxVuln, mulLeakageFor)

print("expVuln (non-zero)")
runTests(expVuln, mulLeakageForNonZero)
print("maxVuln (non-zeros)")
runTests(maxVuln, mulLeakageForNonZero)

