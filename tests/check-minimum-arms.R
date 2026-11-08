args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)])[1];source(file.path(dirname(script),'..','ggplot_draw_stat.R'))
file<-tempfile(fileext='.pdf');grDevices::pdf(file,width=6,height=4)
annotations<-data.frame(Group1.x=c(1,2,3),Group2.x=4,.key1=c('a','b','c'),.key2='d',.comparison=c('1-4','2-4','3-4'),Label='***')
geometry<-data.frame(key=letters[1:4],xmin=(1:4)-.35,xmax=(1:4)+.35,ymin=0,ymax=c(9,9.1,9,7.5),radius_mm=0)
checks<-0L
for(mode in c('auto','left'))for(short in c(FALSE,TRUE))for(stroke in c(.1,.3,1))for(minimum in c(0,1,4)){
 style<-.ds_style(NULL,stroke,8,'black',.5,.5,'sans','plain',1.2,3,3,mode,short,minimum)
 rows<-.ds_layout_panel(annotations,geometry,c(.5,4.5),c(0,16),120,90,style)
 lengths<-c(rows$ymax_adj-rows$Group1.y_adj,rows$ymax_adj-rows$Group2.y_adj)*90/16
 stopifnot(all(lengths>=minimum+stroke/2-1e-8))
 if(short)stopifnot(all(lengths<=max(2,minimum+stroke/2)+1e-8))
 for(i in seq_len(nrow(rows)))for(side in 1:2){column<-paste0('Group',side,'.y_adj');key<-if(side==1)annotations$.key1[i]else'd';stopifnot(rows[[column]][i]>=geometry$ymax[match(key,geometry$key)]+3*16/90-1e-8)}
 checks<-checks+1L
}
for(minimum in c(1,4)){
 style<-.ds_style(NULL,.3,8,'black',.5,.5,'sans','plain',1.2,3,NULL,'auto',FALSE,minimum)
 rows<-.ds_layout_panel(annotations,geometry,c(.5,4.5),c(0,16),120,90,style)
 stopifnot(all(c(rows$ymax_adj-rows$Group1.y_adj,rows$ymax_adj-rows$Group2.y_adj)*90/16>=minimum+.15-1e-8));checks<-checks+1L
}
stopifnot(inherits(try(.ds_style(NULL,.3,8,'black',.5,.5,'sans','plain',1.2,3,min_arm_mm=-1),silent=TRUE),'try-error'))
grDevices::dev.off();unlink(file)
cat('PASS',checks,'physical minimum-arm/clearance/short-cap cases across both row layouts and compact layout\n')
