library(tidyverse)
# Question 1
prenoms %>% group_by(Année) %>% summarize(total = sum(Nombre)) %>% ggplot(aes(x=Année,y=total)) + geom_histogram(stat="identity")

# Question 2
prenoms %>% group_by(Année,Sexe) %>% summarize(total = sum(Nombre)) %>% ggplot(aes(x=Année,y=total,fill=Sexe)) + geom_col(position = "dodge")

# Question 3
prenoms[prenoms$Prénom == "Valentin",] %>% summarize(total = sum(Nombre))
subset(prenoms, Prénom=="Valentin") %>% summarize(total = sum(Nombre))

# Question 4
prenoms %>% group_by(Prénom) %>% summarize(total = sum(Nombre)) %>% arrange(-total) %>% head(n=10)

# Question 5
prenoms %>% group_by(Année,Sexe) %>% mutate(Ranking=rank(-Nombre,ties.method="last")) %>% filter(Ranking <= 5)%>% arrange(Année,Sexe,Ranking) %>% ggplot(aes(x=Année,y=Prénom)) + geom_point() + facet_wrap()

prenoms %>% mutate(Nb_Letters=length(Prénom[,]))

# Question 6
prenoms %>% group_by(Année) %>% summarize(moy_nb_lettres = mean(nchar(Prénom))) %>% ggplot(aes(x=Année,y=moy_nb_lettres)) + geom_histogram(stat="identity")

# Question 7

vowels <- c('a','e','i','o','u','y')
consonants <- letters[!(letters  %in% vowels)]
invowels <- function(name) {
  return(sum(strsplit(tolower(name),'')[[1]] %in% vowels)[1])
}

inconsonants <- function(name) {
  return(sum(strsplit(tolower(name),'')[[1]] %in% consonants)[1])
}

prenoms %>% 
  group_by(Année) %>% 
  summarise(moy_nb_vowels = mean(unlist(lapply(Prénom, invowels)), na.rm = TRUE ),moy_nb_consonants = mean(unlist(lapply(Prénom, inconsonants)), na.rm = TRUE ) ) %>% 
  pivot_longer(cols = starts_with("moy"), names_to="moy") %>%
  ggplot(aes(x=Année,y=value,fill=moy)) + geom_col(position = "dodge")
