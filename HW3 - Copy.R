## Entropy function ##
Entropy <- function(vector){
  h1 = hist(vector, breaks = 100)
  p = h1$counts / sum(h1$counts)
  q = p[p > 0]
  entropy = sum(-q * log2(q))
  return (entropy)
}

###################################################
################## QUESTION 1 #####################
###################################################

table = read.table("HW1Table.txt", header = TRUE)
Entropy(table$X)
Entropy(table$Y)


###################################################
################## QUESTION 2 #####################
###################################################

############ A) Eyeball linear model ##############
p1 = 15 + 0.25*d$Solar.R
lines(d$Solar.R, p1, col = 2)
e1 = p1 - d$Ozone
Entropy(e1)

################ B) Linear model ##################
m2 = lm(d$Ozone~d$Solar.R)
c2 = coef(m2)
p2 = c2[1] + c2[2]*d$Solar.R
e2 = p2 - d$Ozone
Entropy(e2)

########## C) Second order polynomial #############
x2 = d$Solar.R*d$Solar.R
m3 = lm(d$Ozone~d$Solar.R + x2)
c3 = coef(m3)
p3 = c3[1] + c3[2]*d$Solar.R + c3[3]*x2
e3 = p3 - d$Ozone
Entropy(e3)

########## D) Generalized linear model #############
m4 = glm(d$Ozone~d$Solar.R, family = "poisson")
c4 = coef(m4)
p4 = exp(c4[1] + c4[2]*d$Solar.R)
e4 = p4 - d$Ozone
Entropy(e4)





