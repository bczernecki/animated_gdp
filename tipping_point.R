a = c(-0.18,-0.2,-0.15,-0.2, -0.4, -0.5,-0.6,-0.55,-0.5,-0.3,-0.7,-1,-1.2,-1.3,-1.2,-1.1,-1,-0.8,-0.5)
plot(a, type='l')

df = spline(x = a, n = 200) 

df = data.frame(x = df$x, y = df$y)
plot(df, type= 'l')
df$z = df$y





df$x1 = df$x
df$y1 = df$x

for (i in 1:200){
  #plot(df, type= 'l', xlab = '', ylab='')
    if(i<88){
      zakres = sample(50:88, size= 1)
      df$x1[i] = df$x[zakres]
      df$y1[i] = df$y[zakres]
    } else {
      df$x1[i] = df$x[i]
      df$y1[i] = df$y[i]
    }
}

for (i in 140:200){
  #plot(df, type= 'l', xlab = '', ylab='')
  
    zakres = sample(130:182, size= 1)
    df$x1[i] = df$x[zakres]
    df$y1[i] = df$y[zakres]
}

plot(df$x, df$y, type = 'l')
points(df$x1, df$y1)
df


df$stat = 1:200
# library(gganimate)
# library(ggplot2)
# anim = ggplot(df, aes(x = x, y = y)) +
#   geom_line(aes(x = x, y = y))+
#   geom_point(aes(x=x1,y = y1, group = stat))+
#   transition_states(stat, transition_length = 4, state_length = 1)
# 
# for_mp4 = animate(anim, 200, fps = 10,  width = 900, height = 750, 
#                   renderer = ffmpeg_renderer()) 
# anim_save("animation.mp4", animation = for_mp4 )



for (i in 1:200){
  png(filename = paste0(sprintf(fmt = "%03d", i),".png"))
  plot(x = df$x, y = df$y, type= 'l', xlab = '', ylab='')
  points(df$x1[i], df$y1[i], col='red', pch=19, cex=2)
  dev.off()
}
