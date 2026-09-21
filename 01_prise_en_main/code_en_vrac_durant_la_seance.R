
# Ceci est un commentaire

age_patient <- 42
tas_moyenne <- 128

patients <- read.csv("data/patients.csv")

head(patients, 10)

nrow(patients)

# R travaille dans un répertoire fixe. On peut changer de repertoire via des comandes trouvable sur internet. 

round(x = 3.14159, digits = 3)


rnorm(n = 5, mean = 10, sd = 2)


# Voire ?rnorm pour la presentation d'une varaible aléatoire dansR. 


ages <- rnorm(1000, 0, 1)
mean(ages)
median(ages)
min(ages)
max(ages)
hist(ages)


head(patients, 2)
str(patients)
names(patients)

ages <- patients$age
mean(ages)
median(ages)
min(ages)
max(ages)
hist(ages)

hist(patients$tas)

hist(patients$sexe)

table(patients$sexe)


#################
# Exo part 1. 


sqrt(16)

age <- 42
taille <- 1.72
poids <- 74

imc = poids/taille^2

round(imc, digits=1)

sqrt(144)
help()


###############
# Exo 3

age <- c(19, 24, 31, 47, 63)

mean(age)
median(age)
min(age)
max(age)
length(age)

age <- age + 1
hist(age)

#### Exo 4

install.packages("ggplot2")


######## Partie 2. 

age <- c(34, 51, 68, 42)
sexe <- c("F", "M", "F", "F")
positif <- c(TRUE, FALSE, TRUE, TRUE)

length(age)
class(age)
str(age)

seq(from = 1, to = 10, by = 2)

rep(0, times = 5)

rep(c("A", "B"), times = 3)


cond = age > 50
age[cond]

x[!is.na(x)] # le ! veut dire l'inverse d'une condition. 


patients <- data.frame(
id = 1:3,
age = c(34, 51, 68),
sexe = c("F", "M", "F")
)


readr::read_csv("data/patients.csv")
