#A
str(aps2_v1clean)

#B
summary(aps2_v1clean$Duration_ms)
boxplot(aps2_v1clean$Duration_ms)

#C
hiphop = aps2_v1clean$Duration_ms[aps2_v1clean$Genre == 'Hiphop']
underground_rap = aps2_v1clean$Duration_ms[aps2_v1clean$Genre == 'Underground Rap']
trap_metal = aps2_v1clean$Duration_ms[aps2_v1clean$Genre == 'Trap Metal']

boxplot(hiphop, underground_rap, trap_metal,
        main = "Boxplot para cada gênero")

#D

d = cor(aps2_v1clean[c("Stream", "Energy", "Loudness", "Speechiness", "Acousticness", "Liveness")])
d_2 = correlations["Stream", ]

#E

trap_e = cor(aps2_v1clean[aps2_v1clean$Genre == "Trap Metal", c("Stream", "Energy", "Loudness", "Speechiness", "Acousticness", "Liveness")])
trap_e = trap_e["Stream", -1]


hip_e = cor(aps2_v1clean[aps2_v1clean$Genre == "Hiphop", c("Stream", "Energy", "Loudness", "Speechiness", "Acousticness", "Liveness")])
hip_e <- hip_e["Stream", -1]

under_e = cor(aps2_v1clean[aps2_v1clean$Genre == "Underground Rap", c("Stream", "Energy", "Loudness", "Speechiness", "Acousticness", "Liveness")])
under_e = under_e["Stream", -1]

#f

plot(aps2_v1clean$Stream[aps2_v1clean$Genre == "Underground Rap"] ~ aps2_v1clean$Energy[aps2_v1clean$Genre == "Underground Rap"])
abline(lm(aps2_v1clean$Stream[aps2_v1clean$Genre == "Underground Rap"] ~ aps2_v1clean$Energy[aps2_v1clean$Genre == "Underground Rap"]))


plot(aps2_v1clean$Stream[aps2_v1clean$Genre == "Hiphop"] ~ aps2_v1clean$Loudness[aps2_v1clean$Genre == "Hiphop"])
abline(lm(aps2_v1clean$Stream[aps2_v1clean$Genre == "Hiphop"] ~ aps2_v1clean$Loudness[aps2_v1clean$Genre == "Hiphop"]))
r2 = lm(aps2_v1clean$Stream[aps2_v1clean$Genre == "Hiphop"] ~ aps2_v1clean$Loudness[aps2_v1clean$Genre == "Hiphop"])
#G

glikes = tapply(aps2_v1clean$Likes, aps2_v1clean$Genre, mean)
barplot(glikes)

#H
you_spo <- cor(aps2_v1clean$Stream, aps2_v1clean$Likes)
plot(aps2_v1clean$Stream, aps2_v1clean$Likes)
abline(lm(aps2_v1clean$Likes ~ aps2_v1clean$Stream))

#I
you_spo2 <- cor(aps2_v1clean$Stream, aps2_v1clean$Views)
plot(aps2_v1clean$Stream, aps2_v1clean$Views)
abline(lm(aps2_v1clean$Views ~ aps2_v1clean$Stream))
