a = c(-0.18,-0.2,-0.15,-0.2, -0.4, -0.5,-0.6,-0.55,-0.5,-0.3,-0.7,-1,-1.2,-1.3,-1.2,-1.1,-1,-0.8,-0.5)
plot(a, type='l')

df = spline(x = a, n = 200) 

df = data.frame(x = df$x, y = df$y)
plot(df, type= 'l')
df$z = df$y



for (i in 1:200){
  if(i<150){
    df$x1 = x(df$x[i], df$y[i]+0.02, col="red", pch=19)
  }
}


df$x1 = df$x
df$y1 = df$x

for (i in 1:200){
  #plot(df, type= 'l', xlab = '', ylab='')
    if(i<82){
      zakres = sample(50:82, size= 1)
      df$x1[i] = df$x[zakres]
      df$y1[i] = df$y[zakres]
    } else {
      df$x1[i] = df$x[i]
      df$y1[i] = df$y[i]
    }
}

plot(df$x, df$y, type = 'l')
points(df$x1, df$y1)
df
df$z = NA
df$z[20:30] = df$y[20:30]
plot(df, type= 'l')

library(ggplot2)
ggplot(df, aes(x = x, y = y)) +geom_line()+
  geom_point(aes(x = x, y = z))
