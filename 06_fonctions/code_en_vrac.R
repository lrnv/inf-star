#### Partie 6

poids = 78
taille= 1.79

imc <- function(poids, taille){
    imc = poids / taille^2
    return(imc)
}

imc(74, 1.72)

imc(61, 1.65)


# nom <- function(argument1, argument2) {
#     # bloc de calcul. 
#     resultat <- argument1 + argument2
    
#     # dernière instruction : retour
#     resultat
# }






centrer <- function(x, na.rm = TRUE) {
    x - mean(x, na.rm = na.rm)
}


centrer(c(1,2,3,4, NA))

# Appel avec spécification desa rgument dite "positionelle"
a = imc(71, 1.80)


# Appel par keywords
imc(taille = 1.72, poids = 74)


if(x > 5){
    # bloc d'instructions
    print(x)
}


# Version inline du block: 
if(x > 5) print(x)
div_par_5 <- function(x) {
    x/5
}
div_par_5 <- function(x) x/5




categorie_imc <- function(x) {
    if (is.na(x)) {
        return(NA_character_)
    }

    if (x < 18.5) {
        "insuffisance pondérale"
    } else if (x < 25) {
        "corpulence normale"
    } else {
        "surpoids ou obésité"
    }
}

message <- categorie_imc(imc(75, 1.80))



imc <- function(poids, taille) {
    stopifnot(all(poids > 0), all(taille > 0))
    poids / taille^2
}




#####

# Diffrence entre valeur retournée et valeur affichée

square <- function(x){
    x^2
}

square2 <- function(x){
    print(x^2)
    NULL
}


#########

f <- function(x) {
    y <- x * 2
    y + 1
}
res = f(4)

est_senior <- function(age, seuil = 65) {
    age >= seuil
}

est_senior(70, seuil = 76)
soeul = 80


####

2 + 3
c(2,3) + c(5,6,7)


imc(
    poids = c(60, 75, 90),
    taille = c(1.60, 1.75, 1.82)
)

pour_chaque <- function(f, x){
    y = numeric(length(x))
    for(i in seq_along(x)){
       y[i] = f(x[i])
    }
    return(y)
}


resume <- function(x, ...) {
    mean(x, ...)
}


resume(c(1, NA, 3), na.rm = TRUE)


centrer <- function(x, ...) {
    x - mean(x, ...)
}


imc(70, 1.75)
imc(c(60, 80), c(1.60, 1.80))
imc(NA, 1.75)


imc <- function(poids, taille) {
    stopifnot(all(poids > 0, na.rm=TRUE), all(taille > 0, na.rm=TRUE))
    poids / taille^2
}

imc(c(70, NA), c(1.78, 
1.80))


centrer_reduire <- function(x) {
    if(all(x == x[1])){
        return(numeric(length(x)))
    }
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
centrer_reduire(c(4, 4, 4))



#####
#Partie 7 vectodisation

x <- c(2, 4, 6, 8)
x + 1
x^2
sqrt(x)


x <- c(-2, 4, -1, 7)
x[x > 0]
x[x < 0] <- 0
x



m <- matrix(1:12, nrow = 3)
apply(m, MARGIN = 1, FUN = mean)
apply(m, MARGIN = 2, FUN = mean)
apply(m, MARGIN = 1:2, FUN = function(x) x^2)

# operations equivalentes: 
rowMeans(m)
colMeans(m)
m^2


# arguments supplementaires: 
m <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 2)
apply(m, 2, mean, na.rm = TRUE)


pour_chaque <- function(x, margin, f, ...){
    if(margin==1){
        y = numeric(nrow(x))
        for (i in seq_len(nrow(x))){
            y[i] = f(x[i,], ...)
        }
    } else if(margin==2){
        y = numeric(ncol(x))
        for (i in seq_len(ncol(x))){
            y[i] = f(x[,i], ...)
        }
    }
    return(y)
}


# lapply : verison liste

x <- list(
    a = 1:5,
    b = c(10, 20, 30),
    c = c(2, 4)
)
lapply(x, mean)

# ici la structure de sortie sera TOUJOURS une liste. 

sapply(x, \(x) x^2)

lapply(x, \(x) x^2)



donnees <- data.frame(age = c(35, 50, 65), tas = c(120, NA, 145))
sapply(donnees, mean, na.rm = TRUE)
lapply(donnees, summary)


etendue <- function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
sapply(donnees, etendue)
sapply(donnees, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))


x
f = \(x) x^2
sapply(x, f)
lapply(x, f)

# transformée une liste en vecteur ? 
str(unlist(x))

######## Simulation. 

set.seed(1234)
rnorm(10)


runif(10)

sample(c("A", "B"), size = 20, replace = TRUE)

mean(rexp(100000, rate = 0.2))

vec = replicate(
    1000,
    mean(rnorm(50, mean = 120, sd = 15))
)

mean(vec)
sd(vec)
sqrt(50/15) # valeu from LLN.

#### LLN 
set.seed(123)
m10 <- replicate(1000, mean(rnorm(10)))
m100 <- replicate(1000, mean(rnorm(100)))
sd(m10)
sd(m100)


####
set.seed(123)
z <- rnorm(1000000)
mean(z > 1.96) # should approach 0.025


# equiv to replicate. 
set.seed(123)
moyennes <- numeric(1000)
for (i in seq_along(moyennes)) {
    moyennes[i] <- mean(rnorm(300, mean = 100, sd = 15))
}
sd(moyennes)










x <- runif(1e6)
system.time({
    y <- sqrt(x)
})

my_sqrt <- function(x){
    y = numeric(length(x))
    for (i in seq_along(x)){
        y[i] = sqrt(x[i])
    }
    return(y)
}
system.time({
    y <- my_sqrt(x)
})

system.time({
    sqrt(2.3)
})


debut <- Sys.time()
y = my_sqrt(x)
fin <- Sys.time()
fin - debut


x <- runif(1)
bench::mark(
    sqrt(x),
    x^0.5,
    x^(1/2)
)

# attention, la fonctin mean() peut parfois ne pas performer correctement. 
x = runif(1000)
bench::mark(
    mean(x),
    sum(x),
    length(x),
    2L / 3L,
    sum(x) / length(x),
    check = FALSE
)



patients = readr::read_csv("data/patients.csv")

# On echantillonne des patients
# et on calcule l'IMC moyene sur l'échatillon.

mon_calcul <- function(df, N=10000){

    # op 1
    rez = sample(1:nrow(df), N, replace=TRUE)

    # op 2
    imcs = lapply(rez, function(i){
        df$poids_kg[i] / (df$taille_cm[i]/100)^2
    })

    # op 3
    mean(unlist(imcs), na.rm=TRUE)
}

mon_calcul_v2 <- function(df, N=10000){
    rez = sample(1:nrow(df), N, replace=TRUE)
    vec_p = df$poids_kg
    vec_t = df$taille_cm/100
    imcs = lapply(rez, function(i){
        vec_p[i] / vec_t[i]^2
    })
    mean(unlist(imcs), na.rm=TRUE)
}

profvis::profvis({
    resultat <- mon_calcul_v2(patients, 100000)
})


mon_calcul_v3 <- function(df, N=10000){
    rez = sample(1:nrow(df), N, replace=TRUE)
    vec_imc = df$poids_kg / (df$taille_cm/100)^2
    imcs = lapply(rez, function(i){
        vec_imc[i]
    })
    mean(unlist(imcs), na.rm=TRUE)
}

profvis::profvis({
    resultat <- mon_calcul_v3(patients, 100000)
})


mon_calcul_v4 <- function(df, N=10000){
    rez = sample(1:nrow(df), N, replace=TRUE)
    vec_imc = df$poids_kg / (df$taille_cm/100)^2
    imcs = vec_imc[rez]
    mean(unlist(imcs), na.rm=TRUE)
}

profvis::profvis({
    resultat <- mon_calcul_v4(patients, 100000)
})

mon_calcul_v5 <- function(df, N=10000){
    vec_imc = df$poids_kg / (df$taille_cm/100)^2
    mean(sample(vec_imc, N, replace=TRUE), na.rm=TRUE)
}

profvis::profvis({
    resultat <- mon_calcul_v5(patients, 100000)
})




#### Exercices partie 6

## Ex 1

imc <- function(poids, taille){
    poids / taille^2
}

imc(78, 1.79)
imc(
    c(78, 90, 65), 
    c(1.7, 1.8, 1.6)
)


## Ex 2

classe_age <- function(age, seuil = 65) {
    # if(age >= seuil){
    #     return("senior")
    # } else {
    #     return("autre")
    # }
    ifelse(age >= seuil, "senior", "autre")
}
age = c(1,2,3,4, 80)
classe_age(age)

### Ex 3 

resume_num <- function(x, na.rm=TRUE){
    return(c(
        moyenne = mean(x, na.rm=na.rm),
        mediane = median(x, na.rm=na.rm), 
        ecartype = sd(x, na.rm = na.rm), 
        minimum = min(x, na.rm=na.rm), 
        maximum = max(x, na.rm=na.rm)
    ))

    ## attention: c() différent de list()
}

resume_num(age)


### Ex 4 

centrer_reduire <- function(x, na.rm=TRUE){
    xbar = mean(x, na.rm=na.rm)
    sigma = sd(x, na.rm=na.rm)
    return((x-xbar)/sigma)
}

centrer_reduire_v2 <- function(x, ...){
    xbar = mean(x, ...)
    sigma = sd(x, ...)
    return((x-xbar)/sigma)
}

x = rnorm(1000, mean = 5)
x[17] <- NA
y = centrer_reduire_v2(x, na.rm=TRUE)


## Ex 5 

x <- list(a = 1:5, b = 6:10, c = c(2, 4, 8))

lapply(x, function(vec){
    max(vec) - min(vec)
})

# version courte:
lapply(x, \(vec) max(vec) - min(vec))



##### Feuille 7, ex 1

x <- c(-4, -1, 0, 3, 8)

y = pmax(x, 0)^2
# attention différence entre max et pmax: vectorisation.

y[y>10]

x[x<0] = 0

x = x^2

x[x > 10]

##### Ex 2 

x <- list(a = 1:5, b = 10:20, c = c(4, 8, 15, 16, 23, 42))

lapply(x, mean)
sapply(x, mean)
etendue <- function(x){
    max(x) - min(x)
}
lapply(x, etendue)


x = c(1,2,3,4,5)



donnees <- data.frame(
    age = c(35, 50, 65, NA),
    tas = c(120, NA, 145, 155),
    groupe = c("A", "B", "A", "B")
)

df_num = donnees[, sapply(donnees,is.numeric)]

donnees |>
    dplyr::select(
        dplyr::where(is.numeric)
    )

sapply(df_num, mean, na.rm=TRUE)

lapply(df_num, resume_num)





###" ex 4"

set.seed(2026)

moyennes = replicate(1000, mean(rnorm(30, mean=100, sd=15)))

mean(moyennes)
sd(moyennes)


moyennes = replicate(1000, mean(rnorm(300, mean=100, sd=15)))

mean(moyennes)
sd(moyennes)

# application sim:ple de al loi des grands nombres: la moyenne se concentre autour de sa vrai lvaleur lorsque le nombre d'echantillon augmente. 


m = matrix(1:20, nrow=5, ncol=4)

apply(m, 1, mean)
apply(m, 2, sum)
colSums(m)
all.equal(colSums(m), apply(m, 2, sum))



#### Parti 8 ex 1

x <- runif(1e7)
system.time({
    sqrt(x)
})

## Ex 2

x = runif(1000)
bench::mark(
    sqrt(x),
    x^0.5
)


method1 <- function(n){
    (1:n)^2
} 
method2 <- function(n){
    res = 1
    for (x in 2:n){
        res = c(res, x^2)
    }
    res
}
method3 <- function(n){
    res = numeric(n)
    for (i in 1:n){
        res[i] = i^2
    }
    res
}

n = 200
bench::mark(
    method1(n),
    method2(n),
    method3(n)
)


### Ex 4 
m = matrix(rnorm(1000*100), nrow=1000, ncol=100)

all.equal(
    apply(m, 2, mean),
    colMeans(m)
)


bench::mark(
    apply(m, 2, mean),
    colMeans(m)
)


lent <- function(n){
    rez = numeric(n)
    for (i in 1:n){
        m = matrix(1:n, nrow=n/10, ncol=10)
        rez[i] = sum(m) * i
    }
}

mieux <- function(n){
    rez = numeric(n)
    m = matrix(1:n, nrow=n/10, ncol=10)
    for (i in 1:n){
        rez[i] = sum(m) * i
    }
}

lent(10000)

profvis::profvis(mieux(10000))

