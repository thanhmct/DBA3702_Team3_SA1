# DBA3702 V8 - reproducible bubble chart + equal-weight ranking
# Put hawker_data_2026.csv in the same folder as this R script.
# Running this script produces V9_2026_GRAPH.png and
# V9_2026_RANKING.csv in the same folder.
for (pkg in c('ggplot2','patchwork')) {
  if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')
}
library(ggplot2)
library(patchwork)

# Resolve the checked-in CSV beside this script from RStudio or Rscript.
script_path <- grep('^--file=', commandArgs(FALSE), value=TRUE)
script_path <- if (length(script_path)) sub('^--file=', '', script_path[1]) else ''
if (!nzchar(script_path) && requireNamespace('rstudioapi', quietly=TRUE) &&
    rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
}
outdir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
input <- file.path(outdir, 'hawker_data_2026.csv')
if (!file.exists(input)) stop('Missing hawker_data_2026.csv beside V9_2026_graph.R')
df <- read.csv(input, stringsAsFactors=FALSE)
required <- c('centre_id','centre_name','nearest_station_name','straight_line_distance_m',
              'avg_daily_tap_outs','subzone_population_2026','match_status')
stopifnot(all(required %in% names(df)))
for (col in c('straight_line_distance_m','avg_daily_tap_outs','subzone_population_2026')) df[[col]] <- as.numeric(df[[col]])
df <- subset(df, match_status=='MATCHED' & is.finite(straight_line_distance_m) &
                 is.finite(avg_daily_tap_outs) & is.finite(subzone_population_2026) &
                 straight_line_distance_m>=0 & avg_daily_tap_outs>=0 & subzone_population_2026>0)
stopifnot(nrow(df)>=5, !anyDuplicated(df$centre_id))
scale100 <- function(x) if (max(x)==min(x)) rep(50,length(x)) else 100*(x-min(x))/(max(x)-min(x))
df$distance_score <- 100-scale100(df$straight_line_distance_m)
df$activity_score <- scale100(df$avg_daily_tap_outs)
df$population_score <- scale100(df$subzone_population_2026)
df$combined_score <- (df$distance_score + df$activity_score + df$population_score)/3
df <- df[order(-df$combined_score,df$straight_line_distance_m,df$centre_name),]
df$overall_rank <- seq_len(nrow(df))
write.csv(df,file.path(outdir,'V9_2026_RANKING.csv'),row.names=FALSE)
top5 <- head(df,5)
mid_x <- median(df$straight_line_distance_m)
mid_y <- median(df$avg_daily_tap_outs)
df$zone <- ifelse(df$straight_line_distance_m<=mid_x & df$avg_daily_tap_outs>=mid_y,'Close + busy','Other')
fmt <- function(x) format(round(x),big.mark=',',trim=TRUE,scientific=FALSE)
max_x <- max(1600,max(top5$straight_line_distance_m)+100)
max_y <- max(df$avg_daily_tap_outs)*1.12
p1 <- ggplot(df,aes(straight_line_distance_m,avg_daily_tap_outs)) +
  annotate('rect',xmin=0,xmax=mid_x,ymin=mid_y,ymax=Inf,fill='#E8F7EF',alpha=.75) +
  geom_vline(xintercept=mid_x,linetype='dashed',colour='#8B96A5') +
  geom_hline(yintercept=mid_y,linetype='dashed',colour='#8B96A5') +
  geom_point(data=subset(df,zone=='Other'),aes(size=subzone_population_2026),colour='#A9B4C3',alpha=.48) +
  geom_point(data=subset(df,zone=='Close + busy'),aes(size=subzone_population_2026),colour='#17A673',alpha=.87) +
  geom_point(data=top5,shape=21,fill='#123B69',colour='white',stroke=1.1,size=6.8) +
  geom_text(data=top5,aes(label=overall_rank),colour='white',fontface='bold',size=4) +
  annotate('text',x=35,y=max_y*.97,label='CLOSE + BUSY',colour='#087A55',fontface='bold',hjust=0,size=5) +
  coord_cartesian(xlim=c(0,max_x),ylim=c(0,max_y)) +
  scale_x_continuous(breaks=seq(0,ceiling(max_x/400)*400,400),labels=fmt) +
  scale_y_continuous(labels=function(x) paste0(round(x/1000),'k')) +
  scale_size_area(max_size=12,name='Subzone residents (2026)',labels=fmt) +
  labs(title='MRT Accessibility & Local Market Potential',
       subtitle=paste0(nrow(df),' hawker centres | Green = close + busy | Numbers = top 5 by equal-weight score'),
       x='Distance to nearest MRT/LRT exit (m)',y='Average daily MRT/LRT station exits') +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face='bold',size=19,colour='#172B43'),
        plot.subtitle=element_text(size=10.5,colour='#536477'),
        axis.title=element_text(face='bold'),panel.grid.minor=element_blank(),
        panel.grid.major=element_line(colour='#E8EDF2'),legend.position='bottom')
top5$panel_y <- 5:1
top5$display_name <- vapply(top5$centre_name,function(s) paste(head(strwrap(s,width=38),2),collapse='\n'),character(1))
top5$station_display <- gsub(' (MRT|LRT) STATION$','',top5$nearest_station_name)
top5$detail <- paste0(fmt(top5$straight_line_distance_m),' m | ',fmt(top5$avg_daily_tap_outs),
                        ' exits/day | Score ',sprintf('%.1f',top5$combined_score))
p2 <- ggplot() +
  geom_text(data=top5,aes(x=.17,y=panel_y+.20,label=display_name),hjust=0,vjust=.5,
            fontface='bold',size=3.5,colour='#172B43',lineheight=.95) +
  geom_text(data=top5,aes(x=.17,y=panel_y-.10,label=station_display),hjust=0,size=3.0,colour='#365B7C') +
  geom_text(data=top5,aes(x=.17,y=panel_y-.31,label=detail),hjust=0,size=2.7,colour='#5D6C7D') +
  geom_point(data=top5,aes(x=.06,y=panel_y),size=8,colour='#123B69') +
  geom_text(data=top5,aes(x=.06,y=panel_y,label=overall_rank),colour='white',fontface='bold',size=4) +
  annotate('text',x=0,y=5.8,label='TOP 5 HAWKER CENTRES',fontface='bold',hjust=0,size=5,colour='#172B43') +
  annotate('text',x=0,y=5.5,label='Ranked across all centres',hjust=0,size=3.4,colour='#536477') +
  annotate('text',x=0,y=.25,label='Score = (Distance + Activity + Population) / 3',hjust=0,size=3.1,colour='#087A55') +
  coord_cartesian(xlim=c(-.02,1.1),ylim=c(.05,5.98),clip='off')+theme_void()+theme(plot.margin=margin(18,25,10,10))
final <- (p1+p2+plot_layout(widths=c(1.9,1.25))) +
  plot_annotation(caption=paste0('Equal weights (1/3 each), min-max normalized across ', nrow(df), ' centres. Straight-line distance; station tap-outs are not hawker footfall. Population: 2026; station activity: Sep 2026.'))
print(final)
ggsave(file.path(outdir,'V9_2026_GRAPH.png'),final,width=19,height=10,units='in',dpi=300,bg='white',limitsize=FALSE)
cat('Done. Files saved in:',outdir,'\n')
print(top5[,c('overall_rank','centre_name','combined_score')],row.names=FALSE)
