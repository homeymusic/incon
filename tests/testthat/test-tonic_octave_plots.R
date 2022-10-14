plot_x_y <- function(x,y,labels,title,xlab,ylab) {
  png(paste0('./_plots/_png/',title,'.png'),width=1000,height=1000)
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  abline(0,1,col='lightgray')
  text(x,y,labels,pos=1)
  dev.off()

  svg(paste0('./_plots/_svg/',title,'.svg'))
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  abline(0,1,col='lightgray')
  text(x,y,labels,pos=1)
  dev.off()
}
flip_dissonance_to_consonance <- function(x) {
  x=dplyr::coalesce(x,0)
  if (x[1]>x[2]) x else max(x)-x
}
test_that('tonic octave plots work for all models for core intervals',{
  t = tibble::tibble(
    pitch = 0:12,
    pitch_name = c('ton','m2','M2','m3','M3','P4',
                   'tt',
                   'P5','m6','M6','m7','M7','oct')
  )
  all_models = list_models()
  working_models = all_models[! all_models %in% c('gill_09_harmonicity',
                                                  'bowl_18_min_freq_dist')]
  cat("\nworking ")
  for (model in working_models) {
    cat('.')

    tonic = 0:12 %>% purrr::map_dbl(function(pitch){
      interval = c(0,pitch)
      incon(interval,model)})
    tonic=flip_dissonance_to_consonance(tonic)

    octave = 0:12 %>% purrr::map_dbl(function(pitch){
      interval = c(pitch,12)
      incon(interval,model)})
    octave=flip_dissonance_to_consonance(octave)

    tonic_name  = paste0(model,'.tonic')
    octave_name = paste0(model,'.octave')
    t=tibble::add_column(t,
                         "{tonic_name}"  := tonic,
                         "{octave_name}" := octave)
  }
  for (model in working_models) {
    cat('.')
    labels = paste(t$pitch_name,"\n",t$pitch)
    tonic =  t[[paste0(model,'.tonic')]]
    octave = t[[paste0(model,'.octave')]]
    plot_x_y(x=tonic,y=octave,
             labels=labels,title=paste(model,'tonic-octave'),
             xlab='tonic',ylab='octave')
    plot_x_y(x=tonic,y=rev(tonic),
             labels=labels,title=paste(model,'tonic-rev_tonic'),
             xlab='tonic',ylab='rev(tonic)')
  }
  expect_true(TRUE)
})
