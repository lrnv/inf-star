temp <- c(36.7, 37.2, 38.4)

for (x in c(2, 4, 6)) {
    print(x^2)
}

x = 2
for (x in c(2, 4, 6)) {
    print(x^2)
}
x


ages <- c(34, 51, 68)
for (age in ages) {
    # je ne sais pas ou j'en suis dans le vecteur.
    print(age)
}

ages <- c(34, 51, 68)
taille <- c(1.72, 1.80, 1.65)
for (i in seq_along(ages)) {
    # j'en suis a l'élément i.
    print(ages[i], taille[i])
}

res <- c()
for (i in 1:10000) {
    res <- c(res, i^2)
}

res <- numeric(10000)
for (i in seq_along(res)) {
    res[i] <- i^2
}


patients = readr::read_csv("data/patients.csv")


vars <- c("age", "tas", "score")
for (nom in vars) {
    print(mean(patients[[nom]], na.rm = TRUE))
}


x <- c(2, NA, 5, 8)
for (val in x) {
    if (is.na(val)) next
    print(val^2)
}

# version vectorisée : 
x[!is.na(x)]^2


x <- c(2, NA, 5, 8)
res = c("","","","")
for (i in seq_along(x)){
    if (is.na(x[i])) {
        res[i] <- NA
        next
    }
    if (x[i] > 0){
        res[i] <- "positif"
    } else {
        rez[i] <- "neg ou nul"
    }
}

res


for (i in 1:100) {
    print(i)
    if (i > 30){
        break
    } 
}
print(i)



x <- 100
while (x > 1) {
    x <- x / 2
    print(x)
}


res <- numeric(length(ages))
for (i in seq_along(ages)) {
    res[i] <- ages[i] + 1
}

res = ages + 1


vecteur = c("a","b", "c")
for(i in seq_along(vecteur)){
    print(c(i, vecteur[i]))
}

for(ellement in vecteur){
    print(ellement)
}


###### 
# Exo 1


x <- c(2, 5, 8, 11)
for(val in x){
    print(val^2)
}
for(i in seq_along(x)){
    print(x[i]^2)
}

#### Exo 2

res = numeric(100)
for(i in seq_along(res)){
    res[i] <- i^2
}

res = 1:100
for(i in seq_along(res)){
    res[i] <- res[i]^2
}

all(res == (1:100)^2)


### Exo 3
patients

cols = c("age", "tas", "score")
m = numeric(3)
for(i in seq_along(cols)){
    m[i] = mean(patients[[cols[i]]], na.rm=TRUE)
}
print(m)

#### Exo 4. 
x <- 1000
cpt = 0
while (x > 10){
    cpt = cpt + 1
    print(x)
    x = x / 1.5
}

print(cpt)


#### Exo 5 

patients$age
vec = character(nrow(patients))
for (i in 1:nrow(patients)){
    if(patients$age[i] >= 65){
        vec[i] <- "senior"
    } else {
        vec[i] <- "autre"
    }
}

vec <- ifelse(patients$age >= 65, "senior", "autre")

### proposition d'un camarade. 
call <- numeric(120)
for (i in seq_along(patients$age)) {
    if (patients$age[i]>= 65) {
        call[i] <- "sénior"
    } else { 
        call[i] <- "autre"
    }
}

call



## Ex 6.a



x <- c(2, 5, 8, 11)
res <- numeric(length(x))
for (i in seq_along(x)) {
    res[i] <- x[i]^2
}



x <- c(2, 5, 8, 11)
res <- numeric(length(x))
for (i in seq_along(x)) {
    res[i] <- x[i]^2
}

# Ex 7

x <- c(4, NA, 9, -2, 16, 25, 36)
for (val in x){
    if(is.na(val) | val < 0) {
        print(paste0("valuer ignorée car manquante ou negative: ", val))
        next
    }
    print(sqrt(val))
    if (val > 25) {
        print(paste0("boucle stoppée car ", val, " > 25"))
        break
    }
}

paste0("chaine1", "chaine2", "chaine3")
paste("chaine1", "chaine2", "chaine3", sep=" ")



#### Ex 8


patients
### a)

vars <- c("age", "tas", "score")
n = length(vars)
moyennes = numeric(n)
ecarts_types = numeric(n)

for (i in seq_len(n)){
    moyennes[i] = mean(patients[[vars[i]]], na.rm=TRUE)
    ecarts_types[i] = sd(patients[[vars[i]]], na.rm=TRUE)
}

res = data.frame(
    col = vars, 
    m = moyennes, 
    sd = ecarts_types,
    colone_de_plus = NULL,
)

##### b) 

vec = character(length(patients$score))
for(i in seq_along(vec)){
    s = patients$score[i]
    if (is.na(s)){
        vec[i] <- "manquant"
    } else if ( s > 70 ) {
        vec[i] <- "élevé"
    } else {
        vec[i] <- "habituel"
    }
}
patients$classe_score = vec

table(vec)

patients |> 
    dplyr::count(classe_score)

vec2 = ifelse(
    is.na(patients$score),
    "manquant",
    ifelse(
        patients$score > 70,
        "élevé",
        "habituel"
    )
)

all(vec == vec2)


### code de votre camarade. 
ta_moyenne = numeric(3)
for(i in seq_along(vars)){
    ta_moyenne[i] = mean(patients[[vars[i]]],na.rm=T)
}
