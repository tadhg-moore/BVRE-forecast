pacman::p_load(ggplot2, hydroGOF, scoringRules, dplyr)

#Read in observations and simulation results
output <- #read.csv(paste0(tools::file_path_sans_ext(saved_file), ".csv"))
          read.csv(file.path(config$file_path$forecast_output_directory,"2019/met/bvre_H_2019_08_01_2019_09_01_F_16_20210612T175603.csv")) %>%
          filter(date >= as.Date("2019-09-01")) %>% mutate(day = as.Date(date) - as.Date(forecast_start_day)) %>% filter(depth %in% c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0))

output2 <- read.csv(file.path(config$file_path$forecast_output_directory, "2020/no ctd - thermistors only, met/bvre_H_2020_08_01_2020_09_01_F_16_20210617T162921.csv")) %>% 
  filter(date >= as.Date("2020-09-01")) %>% mutate(day = as.Date(date) - as.Date(forecast_start_day)) %>% filter(day==3)

forecast_eval <- data.frame(Depth=unique(output$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, year=2019)

for(i in 1:length(unique(output$depth))){
forecast_eval[i,2] <- hydroGOF::rmse(output$forecast_mean[output$depth[i]==unique(output$depth)],output$observed[output$depth[i]==unique(output$depth)])
forecast_eval[i,3]<- hydroGOF::pbias(output$forecast_mean[output$depth[i]==unique(output$depth)],output$observed[output$depth[i]==unique(output$depth)])
forecast_eval[i,4] <- mean(output$forecast_mean[output$depth[i]==unique(output$depth)]) - mean(output$observed[output$depth[i]==unique(output$depth)])
forecast_eval[i,5] <- mean(scoringRules::crps_sample(output$observed[output$depth[i]==unique(output$depth)],cbind(output$forecast_mean[output$depth[i]==unique(output$depth)],
                                         output$forecast_sd[output$depth[i]==unique(output$depth)])))
}

forecast_eval2 <- data.frame(Depth=unique(output2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, year=2020)

for(i in 1:length(unique(output2$depth))){
  forecast_eval2[i,2] <- hydroGOF::rmse(output2$forecast_mean[output2$depth[i]==unique(output2$depth)],output2$observed[output2$depth[i]==unique(output2$depth)])
  forecast_eval2[i,3]<- hydroGOF::pbias(output2$forecast_mean[output2$depth[i]==unique(output2$depth)],output2$observed[output2$depth[i]==unique(output2$depth)])
  forecast_eval2[i,4] <- mean(output2$forecast_mean[output2$depth[i]==unique(output2$depth)]) - mean(output2$observed[output2$depth[i]==unique(output2$depth)])
  forecast_eval2[i,5] <- mean(scoringRules::crps_sample(output2$observed[output2$depth[i]==unique(output2$depth)],cbind(output2$forecast_mean[output2$depth[i]==unique(output2$depth)],
                                                                                                                    output2$forecast_sd[output2$depth[i]==unique(output2$depth)])))
}

#visualize rmse over depth
ggplot(forecast_eval, aes(RMSE,-Depth)) + geom_point() + geom_path()

ggplot(output, aes(observed, forecast_mean, col=as.factor(depth), group=depth)) + geom_point() +
  geom_abline(slope=1)

#combine 2019 and 2020
forecast_eval_final <- rbind(forecast_eval,forecast_eval2)

#plot forecast skill comparison - all depths
ggplot(forecast_eval_final, aes(as.factor(year), RMSE, fill=as.factor(year))) + geom_boxplot() +xlab("") + 
  theme_light() + theme(text = element_text(size=24), legend.position = "none", axis.text = element_text(size=22, color="black")) + scale_fill_manual(values=c("#FF9966", "#66CC99"))
ggplot(forecast_eval_final, aes(as.factor(year), bias)) + geom_boxplot() +xlab("year")
ggplot(forecast_eval_final, aes(as.factor(year), CRPS)) + geom_boxplot() +xlab("year")

#separate by year and metric
ggplot(subset(forecast_eval_final, year=="2019"), aes(RMSE, as.factor(-Depth))) + geom_point()
ggplot(subset(forecast_eval_final, year=="2020"), aes(RMSE, as.factor(-Depth))) + geom_point()

ggplot(subset(forecast_eval_final, year=="2019"), aes(bias, as.factor(-Depth))) + geom_point()
ggplot(subset(forecast_eval_final, year=="2020"), aes(bias, as.factor(-Depth))) + geom_point()

ggplot(subset(forecast_eval_final, year=="2019"), aes(CRPS, as.factor(-Depth))) + geom_point()
ggplot(subset(forecast_eval_final, year=="2020"), aes(CRPS, as.factor(-Depth))) + geom_point()

#by depth
ggplot(forecast_eval_final, aes(RMSE, as.factor(-Depth), color=as.factor(year))) + geom_point(size=4) + ylab("Depth (m)") + 
  theme_light()  + theme(legend.title = element_blank(), text = element_text(size=24), legend.position=c(0.88, 0.13),  axis.text = element_text(size=22, color="black"), legend.text = element_text(size=24)) 
ggplot(forecast_eval_final, aes(bias, as.factor(-Depth), color=as.factor(year))) + geom_point() + ylab("Depth (m)") + theme(legend.title = element_blank()) 
ggplot(forecast_eval_final, aes(CRPS, as.factor(-Depth), color=as.factor(year))) + geom_point() + ylab("Depth (m)") + theme(legend.title = element_blank()) 

## 2020 forecast horizon RMSE figs
forecast_2020 <- read.csv(file.path(config$file_path$forecast_output_directory, "2020/no ctd - thermistors only, met/bvre_H_2020_08_01_2020_09_01_F_16_20210612T181549.csv")) %>% 
  filter(date >= as.Date("2020-09-01")) %>% mutate(day = as.Date(date) - as.Date(forecast_start_day))

forecast_2020_alldepths <- forecast_2020  %>%  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed))

forecast_2020_RMSE <- data.frame(day=unique(forecast_2020_alldepths$day), RMSE=NA)
for(i in 1:length(unique(forecast_2020_alldepths$day))){
  forecast_2020_RMSE[i,2] <- hydroGOF::rmse(forecast_2020_alldepths$avg_wc_temp[forecast_2020_alldepths$day[i]==unique(forecast_2020_alldepths$day)],forecast_2020_alldepths$avg_wc_obs[forecast_2020_alldepths$day[i]==unique(forecast_2020_alldepths$day)])
}

ggplot(forecast_2020_RMSE, aes(as.factor(day),RMSE)) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_light()  + theme(legend.title = element_blank(), text = element_text(size=20)) 

