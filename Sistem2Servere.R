# functia lambda(t)
lambda <- function(t)
{
  if (0 <= t && t <= 4) {
    rez = 0.2 * t^3 + 0.3 * t^2 + 0.25 * t + 9
  } 
  else if (4< t && t <=6)
  {
    rez = 23
  }
  else if (t > 6)
  {
    rez = 3 * t^2 - 2* t + 1
  }
  return (rez)
}

lambda(1) # 9,75
lambda(2) # 12,3
lambda(3) # 17.85
lambda(4) # 27.6
lambda(5) # 23
lambda(6) # 23
lambda(7) # 134
lambda(8) # 177
lambda(9) # 226
lambda(10) # 281 -> const lambda

const_lambda = lambda(10)

generare_t <- function(s)
{
  t = s # ne plasam in timp la momentul s
  repeat{
    #generam U1, U2
    u1 <- runif(1)
    u2 <- runif(1)
    t = t -  1 / const_lambda * log(u1) # actualizam timpul curent
    if(u2 <= lambda(t) / const_lambda) { 
      break
    }
  }
  return (t)
}

# Generam binomial cu metoda inversa

binomial <- function (n, p) {
  
  return (rbinom(1,n,p)[1])
  
  
  #NU MERGE!!!!!!!!!!!!!!!!!
  cum <- 0
  up <- 0
  down <- 0
  rv <-0
  
  if(p<=0.0) return (0)
  if(p>=1.0) return (0)
  if(n<1) return (0)
  
  rv = runif(1,0,1)[1]
  
  repeat{
    
    if(up <= n)
    {
      cum = cum + binoProb(n,p,up)
      if(rv <= cum)
      {
        return (up)
      }
      up = up + 1
    }
    
    down = down - 1
    
    if(down >= 0)
    {
      cum = cum + binoProb(n,p,up)
      if(rv <= cum)
      {
        return(down)
      }
    }
    
    if(!((up <= n) || down >= 1))
    {
      break
    }
  }
  
  return (binomial(n,p))
}

binoProb <- function(n, p, i) {
  logSum <- 0.0
  runningTotal <- 1.0
  UNDERFLO = 1e-100
  for (i in 1:Nd)
    if (i > (n - i)) {
      for (j in 2:(n - i)) {
        runningTotal = runningTotal / j;
        if (runningTotal < UNDERFLO) {
          logSum = logSum + log(runningTotal);
          runningTotal = 1.0;
        }
      }
      for (j in (i+1):n) {
        runningTotal = runningTotal * j;
        if (runningTotal < UNDERFLO) {
          logSum = logSum + log(runningTotal);
          runningTotal = 1.0;
        }
      }
    } else {
      for (j in 2:i) {
        runningTotal = runningTotal / j;
        if (runningTotal < UNDERFLO) {
          logSum = logSum + log(runningTotal);
          runningTotal = 1.0;
        }
      }
      for (j in (n - i + 1):n) {
        runningTotal = runningTotal * j;
        if (runningTotal < UNDERFLO) {
          logSum = logSum + log(runningTotal);
          runningTotal = 1.0;
        }
      }
    }
  
  logSum = logSum + log(runningTotal)
  logSum = logSum + i * log(p)
  logSum = logSum + (n - i) * log(1 - p)
  
  return (exp(logSum))
}

# functie de densitate
df <- function(x)
{ 
  output = 0;
  if(x>0)
  {
    output= (2/3)*x*exp(((-x^2)/3));
  }
  
  return (output);
}


generare_y2 <- function(v=1) {
  
  X = runif(4500, 0, 10)
  U = runif(4500, 0, 1)
  
  count = 1
  accept = c()
  
  while(count <= 4500 & length(accept) < 1000){
    test_u = U[count]
    test_x = df(X[count])/((sqrt(2)/(sqrt(3)*sqrt(exp(1)))))*dunif(X[count],0,1)
    if (test_u <= test_x){
      accept = rbind(accept, X[count])
      count = count + 1
    }
    count = count + 1
  }
  return (accept[length(accept)])
}

print(generare_y2())

# 1) initializare
t = Na = Nd = 0
# S(n1, n2) = (0, 0)
n1 = 0 # numarul de studenti de la casierie
n2 = 0 # numarul de studenti de la secretariat
# generam t0 si atribuim tA
T0 <- generare_t(0) 
tA <- T0 # momentul sosirii urmatorului student 
t1 <- Inf
t2 <- Inf
# intializare
A1 <- c() # sosirea la casierie 
A2 <- c() # sosirea la secretariat
D <- c() # plecarea pacientului din sistem


while (t<5000) {
  # cazul 1
  # sosirea unui student nou 
  # verificam daca casieria este libera
  if (tA == min(tA, t1, t2)) {
    t <- tA
    Na <- Na + 1 # creste numarul de sosiri pana la momentul t 
    n1 <- n1 + 1 # creste numarul de studenti pentru casierie 
    Tt <- generare_t(t)
    tA <- Tt
    
    # daca n1 = 1 generam y1
    if(n1 == 1) { 
      # pacientul care tocmai a sosit va fi servit imediat de casierie
      # generam Y1~ Binom(30,0,2) prin metoda inversa
      y1 <- binomial(30,1/5)
      t1 <- t + y1
    }
    A1 <- c(A1,t)
  }
  
  
  # cazul 2 - t1 < ta, t1 <= t2
  # t1 < ta inseamna ca se elibereaza casierea inainte de sosirea
  # unui student nou
  
  # simulam y2 prin metoda inversa 
  if(t1 < tA && t1 <= t2) {
    t <- t1
    n1 <- n1 - 1 # scadem nr de studenti de pe casierie
    n2 <- n2 + 1 # crestem nr de pacienti de pe secretariat (pacientul merge mai departe la secretariat)
    
    if (n1 == 0) { 
      t1 <- Inf
    } else {  
      # generam Y1
      y1 <-  binomial(30,1/5)
      t1 <- t + y1
    }
    
    if (n2 == 1) { # generam Y2 
      y2 <- generare_y2()
      while (y2 <= 0) {
        y2 <- generare_y2()
      }
      t2 = t + y2
      print(t2);
    }
    
    A2 <- c(A2, t)
  }
  
  # cazul 3 
  # secretariatul se elibereaza inainte de a sosi un student nou si inainte de finalizarea activitatii la secretariat
  if (t2 < tA && t2 < t1) {
    t <- t2
    Nd <- Nd + 1 # creste numarul de plecari pana la momentul t 
    n2 <- n2 - 1 # se scade numarul de studenti 
    
    if (n2 == 0) {
      t2 <- Inf
    }
    if (n2 >= 1) {
      y2 <- generare_y2() 
      while(y2 <= 0) {
        y2 <- generare_y2()
      }
      t2 <- t + y2
    }
    D <- c(D, t)
  }
}

timp_petrecut_in_asteptare <- c()
for (i in 1:Nd) {
  timp_petrecut_in_asteptare <- c(timp_petrecut_in_asteptare, D[i] - A1[i])
}


timp_server1 <- c()
for (i in (length(A2) + 1) : Na) {
  timp_server1 <- c(timp_server1, t - A1[i])
}


timp_server2 <- c()
for (i in (length(D) + 1) : length(A2)) {
  timp_server2 <- c(timp_server2, t - A1[i])
}


timp_petrecut <- c(timp_server1, timp_server2, timp_petrecut_in_asteptare)
min(timp_petrecut)
max(timp_petrecut)
mean(timp_petrecut)
hist(timp_petrecut)
max(timp_petrecut_in_asteptare)
min(timp_petrecut_in_asteptare)
mean(timp_petrecut_in_asteptare)
hist(timp_petrecut_in_asteptare)
plot(timp_petrecut_in_asteptare)
