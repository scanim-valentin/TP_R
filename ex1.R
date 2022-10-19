#1.1
# e1 <- c(2,5,0,8)
# e2 <- 1:200
# e3 <- -2*100:105
# e4 <- 2^1:7
# v <- (-1)^(0:49)
# e5 <- c(e2,-e3)
# e6 <- seq(0,1,length.out=70) 
# e7 <- rep(e1, 10) 
# r <- e2 - e3
 
#1.2
# vowels <- c('a','e','i','o','u','y')
letters <- letters
letters_in_vowels <- letters %in% vowels
vowel_numbers <- which(letters_in_vowels)
 non_vowel_numbers <- which(!letters_in_vowels)
 after_vowels <- letters[vowel_numbers+1]
 myname <- "valentin"
 myname_splitted <- strsplit(myname,'')[[1]]
 myname_letter_numbers <-  which(letters %in% myname_splitted)
 neibname <- "jean"
 neibname_splitted <- strsplit(neibname,'')[[1]]
 neibname_letter_numbers <-  which(letters %in% neibname_splitted)
 #winner <- c(myname,neibname)[which((c(FALSE,FALSE) || (mean(neibname_letter_numbers)>mean(neibname_letter_numbers))))]
 neibwinner <- mean(neibname_letter_numbers)>mean(neibname_letter_numbers)

#2.1
alphaframe <- data.frame(letters, which(letters %in% letters),letters_in_vowels)
alphaframe_myname <- alphaframe[myname_letter_numbers,]
msleep<-ggplot2::msleep
sanity <- msleep[,"sleep_total"]+msleep[,"awake"]
most_sleepy <- msleep[which.max(msleep$sleep_total),]$name
light_and_sleepy <- msleep[which(msleep$sleep_total > 12 & msleep$bodywt < 0.1),]$name

subset(msleep,bodywt <0.1)

average_weight <- mean(na.omit(msleep$brainwt / msleep$bodywt))
most_weight_ratio <- msleep[which.max(na.omit(msleep$brainwt / msleep$bodywt)),]$name

#2.2
reordered_msleep <- factor(msleep$conservation,ordered = TRUE, level=c("lc","domesticated","cd","nt","vu","en"))
threatened_mean <- mean( msleep[which(reordered_msleep > 'nt'),]$bodywt ) 
non_threatened_mean <- mean( msleep[which(reordered_msleep <= 'nt'),]$bodywt )
isThreatedGreater <- threatened_mean > non_threatened_mean 
#c("lc","domesticated","cd","nt","vu","en")
msleep_threatened <-msleep
msleep_threatened$threatened = reordered_msleep > 'nt'