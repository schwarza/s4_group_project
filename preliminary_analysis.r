library(dplyr)
library(ggplot2)
library(ppcor)
success_metric<-read.csv("citation_ranks.csv")
topic_switch<-read.csv("specter_dists_career_lengths.csv")
success_metric$total_output<-success_metric$PubCountDecade*success_metric$PercentileRankDecade
success_metric_decade1<-success_metric%>%
  filter(Decade==1)
success_metric_decade2<-success_metric%>%
  filter(Decade==2)
metric_match_idx<-match(success_metric_decade2$PID,success_metric_decade1$PID)
success_metric_decade2$PercentileRankDiff<-success_metric_decade2$PercentileRankDecade-success_metric_decade1$PercentileRankDecade[metric_match_idx]
success_metric_decade2$productivityDiff<-success_metric_decade2$PubCountDecade-success_metric_decade1$PubCountDecade[metric_match_idx]
success_metric_decade2$total_outputDiff<-success_metric_decade2$total_output-success_metric_decade1$total_output[metric_match_idx]
success_metric_merge<-success_metric_decade2[,c(2,7,8,9)]
topic_switch_mentor_with20career<-topic_switch%>%
  filter(IsMentor=="True")%>%
  filter(!is.na(TwoDecadeSpecter))
topic_success_idx<-match(topic_switch_mentor_with20career$PID,success_metric_merge$PID)
topic_switch_mentor_with20career$quality_diff<-success_metric_merge$PercentileRankDiff[topic_success_idx]
topic_switch_mentor_with20career$productivity_diff<-success_metric_merge$productivityDiff[topic_success_idx]
topic_switch_mentor_with20career$total_output_diff<-success_metric_merge$total_outputDiff[topic_success_idx]
topic_switch_mentor_with20career<-topic_switch_mentor_with20career%>%
  filter(!is.na(quality_diff))%>%
  filter(!is.na(productivity_diff))%>%
  filter(MinPubYear>=1950)%>%
  #filter(MinPubYear<=1990)%>%
  filter(gender!="unknown")
pcor.test(topic_switch_mentor_with20career$TwoDecadeSpecter,
          topic_switch_mentor_with20career$quality_diff,topic_switch_mentor_with20career$MinPubYear,
          method="spearman")
#rho=0.12,p=0.0003
png(filename="./quality_diff_vs_topic_switch.png",width=10,height=6,unit="in",res=600)
ggplot(topic_switch_mentor_with20career,aes(x=TwoDecadeSpecter,y=quality_diff))+
  geom_point()+
  geom_smooth()+
  xlab(label="Topic distance")+
  ylab(label="Average citation rank difference")+
  theme(axis.text.x=element_text(size=20,family="TT Arial",color="black"))+
  theme(axis.text.y=element_text(size=20,family="TT Arial",color="black"))+
  theme(axis.title.x=element_text(size=20,family="TT Arial",color="black"))+
  theme(axis.title.y=element_text(size=20,family="TT Arial",color="black",angle=90,hjust = 0.5,vjust=0.5))+
  geom_hline(aes(yintercept=0), color="red")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks=element_line(size=1))+
  theme(axis.ticks.length=unit(3,"mm"))
dev.off()

png(filename="./productivity_diff_vs_topic_switch.png",width=10,height=6,unit="in",res=600)
ggplot(topic_switch_mentor_with20career,aes(x=TwoDecadeSpecter,y=productivity_diff))+
  geom_point()+
  geom_smooth()+
  xlab(label="Topic distance")+
  ylab(label="Average productivity difference")+
  theme(axis.text.x=element_text(size=20,family="TT Arial",color="black"))+
  theme(axis.text.y=element_text(size=20,family="TT Arial",color="black"))+
  theme(axis.title.x=element_text(size=20,family="TT Arial",color="black"))+
  theme(axis.title.y=element_text(size=20,family="TT Arial",color="black",angle=90,hjust = 0.5,vjust=0.5))+
  geom_hline(aes(yintercept=0), color="red")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks=element_line(size=1))+
  theme(axis.ticks.length=unit(3,"mm"))
dev.off()
median(topic_switch_mentor_with20career$TwoDecadeSpecter[topic_switch_mentor_with20career$quality_diff<0])
#enrichment
quality_switch_relationship_adjust_for_year<-topic_switch_mentor_with20career%>%
  group_by(MinPubYear)%>%
  dplyr::summarise(switch_quality_cor=cor(quality_diff,TwoDecadeSpecter))%>%
  filter(!is.na(switch_quality_cor))
productivity_switch_relationship_adjust_for_year<-topic_switch_mentor_with20career%>%
  group_by(MinPubYear)%>%
  dplyr::summarise(switch_productivity_cor=cor(productivity_diff,TwoDecadeSpecter))%>%
  filter(!is.na(switch_productivity_cor))
sum(quality_switch_relationship_adjust_for_year$switch_quality_cor<0)
sum(productivity_switch_relationship_adjust_for_year$switch_productivity_cor<0)
binom.test(29,44,0.5)
#enrichment analysis:gender
cor.test(quality_switch_relationship_adjust_for_year$MinPubYear,
        quality_switch_relationship_adjust_for_year$switch_quality_cor,method="spearman")
median(topic_switch_mentor_with20career$TwoDecadeSpecter)
topic_switch_mentor_20_high<-topic_switch_mentor_with20career%>%
  filter(quality_diff>0)%>%
  filter(TwoDecadeSpecter>0.2474447)
topic_switch_mentor_20_low<-topic_switch_mentor_with20career%>%
  filter(quality_diff<=0)%>%
  filter(TwoDecadeSpecter>0.2474447)
table(topic_switch_mentor_20_low$gender)
47/250
table(topic_switch_mentor_20_high$gender)
21/111
#no disparity
mat<-matrix(c(47,21,250,111),nrow=2)
mat<-matrix(c(13,21,51,123),nrow=2)
fisher.test(mat)
#enrichment analysis:topic
Topic_data<-read.csv("kws_decade_greatest_freq.csv")
Topic_change_data<-read.csv("kws_decade_greatest_change.csv")
Topic_data_filtered<-Topic_data%>%
  filter(Level==0)%>%
  filter(Decade==1)
Topic_change_filtered<-Topic_change_data%>%
  filter(Level==0)
topic_idx_high<-match(topic_switch_mentor_20_high$PID,Topic_data_filtered$PID)
topic_idx_low<-match(topic_switch_mentor_20_low$PID,Topic_data_filtered$PID)
topic_switch_mentor_20_high$start<-Topic_data_filtered$NormalizedName[topic_idx_high]
topic_switch_mentor_20_low$start<-Topic_data_filtered$NormalizedName[topic_idx_low]
table(as.character(topic_switch_mentor_20_high$start))
table(as.character(topic_switch_mentor_20_low$start))
24/57
51/123
13/21
#no starting field diffferences.
topic_changeidx_high<-match(topic_switch_mentor_20_high$PID,Topic_change_filtered$PID)
topic_changeidx_low<-match(topic_switch_mentor_20_low$PID,Topic_change_filtered$PID)
topic_switch_mentor_20_high$change_topic<-Topic_change_filtered$NormalizedName[topic_changeidx_high]
topic_switch_mentor_20_low$change_topic<-Topic_change_filtered$NormalizedName[topic_changeidx_low]
table(as.character(topic_switch_mentor_20_high$change_topic))
table(as.character(topic_switch_mentor_20_low$change_topic))
26/64
46/85
mat<-matrix(c(26,64,46,85),nrow=2)
fisher.test(mat)
