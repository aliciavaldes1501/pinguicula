ping_17_data$n_seeds_rel<-with(ping_17_data,n_seeds/mean(n_seeds,na.rm=T))
ping_17_data$ffd_std<-as.numeric(scale(ping_17_data$FFD_corr))
ping_17_data$n_stems_std<-as.numeric(scale(ping_17_data$n_stems))

summary(lm(n_seeds_rel~n_stems_std*temp,
           ping_17_data)) # significant interaction!

plot(ggpredict(lm(n_seeds_rel~n_stems_std*temp,
                  ping_17_data)))

plot(ggpredict(lm(n_seeds_rel~n_stems_std*temp,
                  ping_17_data),terms=c("n_stems_std","temp")))
