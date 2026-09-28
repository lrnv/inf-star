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


