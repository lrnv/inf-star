patients = readr::read_csv("data/patients.csv")


hist(patients$age)
boxplot(patients$tas)
plot(patients$age, patients$tas)
barplot(table(patients$traitement))


hist(
    patients$age,
    main = "Distribution de l'âge",
    xlab = "Âge (ans)",
    ylab = "Fréquence"
)


library(ggplot2)
ggplot(patients, aes(x = age, y = tas)) +
    geom_point()

ggplot(patients, aes(x = age)) +
    geom_histogram(binwidth=10)

ggplot(patients, aes(x = sexe, y = tas)) +
    geom_boxplot()

# aucun intéret.
ggplot(patients, aes(x = age, y = tas, alpha=temperature)) +
    geom_point()

ggplot(patients, aes(x = age, y = tas)) +
    geom_point(alpha=0.5)


ggplot(patients, aes(age, tas, color = sexe)) +
geom_point()

ggplot(patients, aes(age, tas)) +
geom_point(color = "steelblue")


ggplot(patients, aes(age/10, tas, color=sexe)) +
geom_point(alpha = 0.5) +
geom_smooth(method="lm") +
facet_wrap(~ traitement) +
labs(
    title = "Tension artérielle et âge",
    x = "Âge (en dixème d'années)",
    y = "TAS (unité ?)"
)


ggplot(patients, aes(x = traitement)) +
geom_bar()


prop <- patients |>
    dplyr::count(traitement) |>
    dplyr::mutate(p = n / sum(n))

ggplot(prop, aes(traitement, p)) +
geom_col()


p <- ggplot(patients, aes(age, tas)) +
geom_point()





patients = readr::read_csv("data/patients.csv")

# feuill d'xo 4

# ex 1

hist(patients$age)

boxplot(patients$tas)

plot(patients$age, patients$tas)


# ex 2

ggplot(
    data = patients,
    mapping = aes(
        x = age
    )) +
    geom_histogram()

ggplot(patients,aes(tas)) + geom_boxplot()

ggplot(data = patients, mapping = aes(x = age, y = tas)) +
    geom_point()

# Ex 3 
ggplot(patients,aes(score, groupe))+ geom_boxplot() + labs(
    x = "Score",
    y="Groupe",
    title = "Boxplot du score en fonction du groupe"
) +
theme_minimal()


p <- ggplot(patients, aes(x=age, y=tas, color=sexe)) +
    geom_point() +
    geom_smooth(method="lm") +
    labs(
        title = "Realtion entre age et TAS", 
        x = "Age (années)",
        y = "TAS"
    )



ggsave(p, filename = "my_plot.pdf")



ggplot(patients, aes(x=age, y=tas)) +
    geom_point() +
    geom_smooth(method="lm")


##########
# Atelier.

library(dplyr)
library(ggplot2)
patients = readr::read_csv("data/patients.csv")

dim(patients)
names(patients)
head(patients)

str(patients)
glimpse(patients)

summary(patients)


colSums(is.na(patients))

# Il y a 12 variables avec 120 observations, dont 3 poids, 2 tas et 3 score manquants. 

### Ex 2. 

patients_analyse <- patients |>
    select(age, sexe, groupe, taille_cm, poids_kg, tas, traitement, score) |>
    filter(!is.na(poids_kg)) |>
    filter(!is.na(tas)) |>
    filter(!is.na(score))|>
    mutate(
        taille_m = taille_cm/100,
        imc = poids_kg / taille_m^2,
        class_age = cut(age, breaks=c(0,40,65,Inf))
    ) |>
    arrange(desc(imc))

head(patients_analyse, 10)

patients_analyse |>
    group_by(class_age) |>
    summarise(n= n())

# ou:
table(patients_analyse$class_age)


#### Ex 3: 
patients_analyse |>
    group_by(sexe) |>
    summarise(
        nb_obs = n(),
        age_moyen = mean(age, na.rm=TRUE),
        tas_moyen = mean(tas, na.rm=TRUE),
        score_moyen = mean(score, na.rm=TRUE),
    )

patients_analyse |>
    group_by(traitement) |>
    summarise(
        nb_obs = n(),
        imc_moyen = mean(imc, na.rm=TRUE),
        score_moyen = mean(score, na.rm=TRUE),
    )

# Les hommes ont un score moyen plus elevé. 
# que les traitements intensif et standard ont le même imc moyen, mais pas le même score moyen (meilleur pour le traitment standard). 


ggplot(patients_analyse, aes(x = age)) +
    geom_histogram()

ggplot(patients_analyse, aes(x = score, y=traitement)) +
    geom_boxplot()

ggplot(patients_analyse, aes(x = age, y=tas, color=sexe)) +
    geom_point() +
    facet_wrap(~traitement)+
    geom_smooth(method="lm", se=FALSE) +
    labs(
        title = "..."
        x = "..."
        y = "..."
    )


#### 5. 

ggplot(patients_analyse, aes(x = groupe, y=imc, color=sexe)) +
    geom_boxplot()

ggplot(patients_analyse, aes(x = age, y=score)) +
    geom_point() +
    facet_wrap(~traitement) +
    geom_smooth(method="lm")

ggplot(patients_analyse, aes(x = age, y=tas, color=groupe)) +
    geom_point() +
    geom_smooth()

ggsave("mon_fichier.pdf")
