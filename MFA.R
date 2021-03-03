nfile = file("data_2014.csv", open = "r")
n = 1
p = 20
N = 50
X = matrix(rnorm(N*p), N, p)
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  X[n,] <- scan(text=oneLine, quiet=T, sep=",")
  # Calcul it¡äeratif de la moyenne et de la variance pour les variables explicatives
  n = n+1
}
close(nfile)

G = cov(X)*(N-1)/N
ev <- eigen(G)

DR=matrix(rnorm(13*p), 13, p)
for(i in 1:13){
  DR[i,] = log(X[i+1,]/X[i,])

}

GDR = cov(DR)*(N-1)/N
evDR <- eigen(GDR)
evDR
