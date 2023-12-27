library(gsignal)
library(fastICA)
library(tuneR)
library(readr)
library(seewave)
library(audio)

isScalar <- function(x)
  ifelse(is.character(x), nchar(x) == 1L, (is.atomic(x) && length(x) == 1L))
sawtooth <- function(t, width) {
  
  if (length(t) <= 0)
    stop("t must be a vector with length > 0")
  if (!isScalar(width) || width < 0 || width > 1)
    stop("width must be a scalar between 0 and 1")
  
  t <- (t / (2 * pi)) %% 1
  y <- rep(0L, length(t))
  
  if (width != 0) {
    y[t < width] <- 2 * t[t < width] / width - 1
  }
  
  if (width != 1) {
    y[t >= width] <- -2 * (t[t >= width] - width) / (1 - width) + 1
  }
  y
}

# -----------------------------------------------------------------------------

# Example 1

time <- seq(0, 8, length.out=1000)
s1 = sin(2 * time)  # sine wave
s2 = sawtooth(1 * pi * time)  # sawtooth wave
s3 = sign(sin(1 * time))  # square wave

S <- cbind(s1,s2,s3)
set.seed(1)
noise <- 0.2 * matrix( rnorm(3000,mean=0,sd=1),nrow = 1000)
# S_noise = S
S_noise <- S + noise
par(mfrow = c(3, 1))
plot(1:1000, S_noise[,1], type = "l",xlab = "S1", ylab = "", main = "Sine wave")  
plot(1:1000, S_noise[,2], type = "l", xlab = "S2", ylab = "", main = "Sawtooth wave") 
plot(1:1000, S_noise[,3], type = "l", xlab = "S3", ylab = "", main = "Square wave") 

par(mfrow = c(3, 3))
plot(1:1000, S_noise[,1], type = "l",xlab = "S1", ylab = "")  
plot(1:1000, S_noise[,2], type = "l", xlab = "S2", ylab = "", main = "Source") 
plot(1:1000, S_noise[,3], type = "l", xlab = "S3", ylab = "") 

# mixture matirx
A = rbind(c(2.0, 0.5, 1), c(0.25, 8, 0.5), c(1.5, 1/3, 2))
X = S_noise %*% t(A)
# par(mfcol = c(1, 3))
plot(1:1000, X[,1], type = "l",xlab = "X1", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "X2", ylab = "", main = "Mix")
plot(1:1000, X[,3], type = "l", xlab = "X3", ylab = "")

ICA_result = fastICA(X, n.comp = 3)
S1_extracted = ICA_result$S[, 1]
S2_extracted = ICA_result$S[, 2]
S3_extracted = ICA_result$S[, 3]
# par(mfcol = c(1, 3))
plot(1:1000, S1_extracted, type = "l", xlab = "S'1", ylab = "")
plot(1:1000, S2_extracted, type = "l", xlab = "S'2", ylab = "", main = "ICA")
plot(1:1000, S3_extracted, type = "l", xlab = "S'3", ylab = "")

# -----------------------------------------------------------------------------

# Example 2

#Read the wav.file
original1 <- readWave('/Users/klin26/清華大學/應用多變量分析/Group Report/107學測英語聽力測驗_final.wav')
original2 <- readWave('/Users/klin26/清華大學/應用多變量分析/Group Report/never gonna give you up_final.wav')
# original3 <- readWave('never gonna give you up_final.wav')
samp_rate_og1 <- original1@samp.rate
samp_rate_og2 <- original2@samp.rate
original1 <- original1@left
original2 <- original2@left

#Read the wav.file
mix_1_wave <- readWave('/Users/klin26/清華大學/應用多變量分析/Group Report/mix1.wav')
mix_2_wave <- readWave('/Users/klin26/清華大學/應用多變量分析/Group Report/mix2.wav')


#Save sample rate
samp_rate1 <- mix_1_wave@samp.rate
samp_rate2 <- mix_2_wave@samp.rate


#Plot the waves
plot(mix_1_wave)
plot(mix_2_wave)


#Extract raw audio from wav file
signal_1 <- mix_1_wave@left
signal_2 <- mix_2_wave@left


#Check the shortest
length_of_shortest <- min(length(signal_1),length(signal_2))
timeArray <- (0:(length_of_shortest-1))

#Plot the amplitude of the three mix waves
par(mfrow = c(2, 1))
plot(timeArray, signal_1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude',main="mix1")
plot(timeArray, signal_2, type='l', col='black', xlab='Time (ms)', ylab='Amplitude',main="mix2")

#Doing ICA
X <- cbind(signal_1, signal_2)
ica_result <- fastICA(X, n.comp = 2, row.norm=FALSE)
result_1_wave <- ica_result$S[,1]/max(ica_result$S[,1])
result_2_wave <- ica_result$S[,2]/max(ica_result$S[,2])


# Comparing the Mix's and ICA's Amplitude
par(mfrow = c(3, 2))
# Original
plot(timeArray, original1[1:(length_of_shortest)], type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = 'Original')
plot(timeArray, original2[1:(length_of_shortest)], type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = 'Original')
# Mix
plot(timeArray, signal_1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = 'Mix')
plot(timeArray, signal_2, type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = 'Mix')
# ICA
plot(timeArray, result_2_wave, type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = 'ICA')
plot(timeArray, result_1_wave, type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = 'ICA')

# Save the new files
savewav(result_1_wave, f = samp_rate1, filename = '/Users/klin26/清華大學/應用多變量分析/Group Report/result_signal_1.wav')
savewav(result_2_wave, f = samp_rate2, filename = '/Users/klin26/清華大學/應用多變量分析/Group Report/result_signal_2.wav')

# -----------------------------------------------------------------------------

# Question 2

# Simulate Cocktail Party Problem
time <- seq(0, 10, length.out=100)
S = data.frame(Time=time, 
               S1=sin(3*time), 
               S2=sawtooth(5*time, 1)
               # S3=sign(sin((1:2000)/50))
)
plot1 = ggplot(S,aes(x=Time,y=S1)) + geom_line(size=1.2, colour="darkblue")
plot2 = ggplot(S,aes(x=Time,y=S2)) + geom_line(size=1.2, colour="darkred")
# plot3 = ggplot(S,aes(x=Time,y=S3)) + geom_line(size=1.2, colour="darkgreen")
grid.arrange(plot1, plot2, ncol=1, nrow=2)

# Linear Transform X=AS
set.seed(1)
A = matrix(runif(4, -1, 1), 2, 2)
X = as.matrix(S[, 2:3]) %*% A
X = data.frame(Time=time,
               X1=X[, 1],
               X2=X[, 2]#,
               # X3=X[, 3]
)
#write.csv(X,file="/Users/klin26/清華大學/應用多變量分析/Group Report/ICA_Q2.csv",row.names = FALSE)
X <- read.csv("/Users/klin26/清華大學/應用多變量分析/Group Report/ICA_Q2.csv")


plot_mix1 = ggplot(X,aes(x=Time,y=X1)) + geom_line(size=1.2, colour="darkblue")
plot_mix2 = ggplot(X,aes(x=Time,y=X2)) + geom_line(size=1.2, colour="darkred")
# plot_mix3 = ggplot(X,aes(x=Time,y=X3)) + geom_line(size=1.2, colour="darkgreen")
grid.arrange(plot_mix1, plot_mix2, ncol=1, nrow=2)

# fastICA
X = X[, 2:3]
ICA_result = fastICA(X, n.comp = 2)
S.extracted = data.frame(Time=time, S1.extracted=ICA_result$S[, 1], S2.extracted=ICA_result$S[, 2])
plot_ica1 = ggplot(S.extracted, aes(x=Time, y=S1.extracted)) + geom_line(size=1.2, colour="darkblue")
plot_ica2 = ggplot(S.extracted, aes(x=Time, y=S2.extracted)) + geom_line(size=1.2, colour="darkred")
# plot_ica3 = ggplot(S.extracted, aes(x=Time, y=S3.extracted)) + geom_line(size=1.2, colour="darkgreen")
grid.arrange(plot_ica1, plot_ica2, ncol=1, nrow=2)


