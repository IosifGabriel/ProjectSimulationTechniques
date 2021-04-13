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

get_currentTime()
{
  t = s # ne plasam in timp la momentul s
  repeat{
    #generam U1, U2
    u1 <- runif(1)
    u2 <- runif(1)
    t = t -  1 / const_lambda * log(u1) # actualizam timpul curent
    if(u2 <= lambda(t) / 281) { 
      break
    }
  }
  return (t)
}

# Generam binomial cu metoda inversa

binomial <- function (n, p) {
  alfa = p0 = (1-p)^n
  b = p(1-p)
  
  counter = 0
  P= alfa
  F= alfa
  
  repeat{
  U = runif(1, 9.74, 281)
  
  if(U <= F)
    break
  
  P = (n-i)/(i+1)*b*P
  F= F+P
  counter = counter + 1;
  }
  
  return (counter)
}
