age <- c(34, 65, 72, 49)

cond = age >= 65

age[cond]

## ex 2

patients = readr::read_csv("data/patients.csv")


cond_age = patients$age >= 65
cond_smoke = patients$fumeur == "oui"

patients[cond_age & cond_smoke,]

selected_patients = patients[cond_age & cond_smoke, c("id","age","sexe","tas","traitement")]

mon_ordre = order(selected_patients$age, decreasing=TRUE)

selected_patients[mon_ordre,]


### Ex 3. 

patients$taille_m = patients$taille_cm / 100
patients$imc = patients$poids_kg / patients$taille_m^2


patients$age < 40
patients$age < 65

patients$class_age = cut(patients$age, breaks=c(0,40,65,Inf))


head(patients[,c("taille_m","imc","class_age")], 10)

mean(patients$age[patients$sexe == "F"], na.rm=TRUE)
mean(patients$age[patients$sexe == "M"], na.rm=TRUE)


by(patients$age, patients$sexe, mean, na.rm=TRUE)
by(patients$tas, patients$sexe, mean, na.rm=TRUE)
by(patients$score, patients$sexe, mean, na.rm=TRUE)






by(patients, patients$sexe, function(x){
    list(
        nb_patients = nrow(x),
        age_moyen = mean(x$age),
        tas_moyen = mean(x$tas, na.rm=TRUE),
        score_moyen = mean(x$score, na.rm=TRUE)
    )
})


# Exo 5

if(x > 0){
    print("x est bien positif")
}

# ou bien: 
if(x < 0){
} else {
    print("x est bien positif")
}





### fin exo 3.

### debut cours 03

age <- c(45, 67, 99)


age >= 65
age == 67
age != 50

age >= 65 & age < 80
age < 18 | age > 85
!(age >= 65)


age <- c(17, 19) 

age = 17
sexe = "F"

if(age > 18 & sexe == "F"){
    message("femme majeure")
} else {
    message("autre chose")
}



imc <- 27
if (imc < 18.5) {
    categorie <- "insuffisance pondérale"
} else if (imc < 25) {
    categorie <- "corpulence normale"
} else {
    categorie <- "surpoids ou obésité"
}
categorie

age <- c(12, 20, 70)
ifelse(age >= 18, "majeur", "mineur")


dplyr::if_else(age >= 18, "majeur", "mineur")



round(mean(c(12.1, 15.7, 18.4)), 1)

c(12.1, 15.7, 18.4) |>
    mean() |>
    round(1)


## Le pie prend la valeur retournée à sa gauche pour l'injecter comme premier argument de l'appel de fonction à sa droite. 

patients |>
    filter(age >= 65, fumeur == "oui") |>
    filter(tas != NA)


patients |>
    select(-id, -age) |>
    names()

patients |>
    arrange(desc(age))

patients |>
    mutate(
        taille_m = taille_cm / 100,
        imc = poids_kg / taille_m^2
    ) |>
    mutate(
        class_age = case_when(
            age < 40 ~ "< 40",
            age < 65 ~ "40-64",
            TRUE ~ "65+"
        )
    ) |>
    select(age, class_age)


patients |>
    group_by(sexe) |>
    summarise(
        n = n(),
        age_moyen = mean(age),
        tas_moyenne = mean(tas, na.rm = TRUE),
    )





patients |>
    filter(!is.na(poids_kg)) |>
    mutate(
        imc = poids_kg / (taille_cm / 100)^2
    ) |>
    group_by(sexe) |>
    summarise(
        n = n(),
        imc_moyen = mean(imc),
    ) |>
    arrange(desc(imc_moyen))


 
patients |>
    select(taille_cm, poids_kg) |>
    mutate(
        taille_m = taille_cm/100,
        imc = poids_kg / taille_m^2,
        taille_m = NULL,
    )

