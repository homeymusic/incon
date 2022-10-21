rotate <- function(coordinates,angle) {
  checkmate::assert_numeric(angle)
  coordinates = t(coordinates)
  R = tibble::frame_matrix(
    ~chord,          ~.y,
    cos(angle), -sin(angle),
    sin(angle),  cos(angle)
  )
  (R %*% coordinates * cos(angle)) %>% zapsmall %>% t
}
plot_x_y <- function(x,y,labels,title,xlab,ylab,vertical_line=FALSE) {
  pdf(paste ('./_plots/_pdf/',title,'.pdf'))
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  if (vertical_line) {
    abline(v=0,col='lightgray')
  } else {
    abline(0,1,col='lightgray')
  }
  text(x,y,labels,pos=1)
  dev.off()

  png(paste ('./_plots/_png/',title,'.png'),width=1000,height=1000)
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  if (vertical_line) {
    abline(v=0,col='lightgray')
  } else {
    abline(0,1,col='lightgray')
  }
  text(x,y,labels,pos=1)
  dev.off()

  svg(paste0('./_plots/_svg/',title,'.svg'))
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  if (vertical_line) {
    abline(v=0,col='lightgray')
  } else {
    abline(0,1,col='lightgray')
  }
  text(x,y,labels,pos=1)
  dev.off()
}
flip_dissonance_to_consonance <- function(x) {
  x=dplyr::coalesce(x,0)
  if (x[1]>x[2]) x else max(x)-x
}
test_that('tonic octave plots work for all models for core intervals',{
  pitches = 60 + 0:12

  t = tibble::tibble(
    pitch = pitches,
    pitch_name = c('P1','m2','M2','m3','M3','P4',
                   'TT',
                   'P5','m6','M6','m7','M7','P8')
  )
  all_models = list_models()
  working_models = all_models[! all_models %in% c('gill_09_harmonicity',
                                                  'bowl_18_min_freq_dist')]

  cat("\nworking ")
  for (model in working_models) {
    cat('.')

    tonic = pitches %>% purrr::map_dbl(function(pitch){
      interval = c(pitches[1],pitch)
      incon(interval,model)})
    tonic=flip_dissonance_to_consonance(tonic)

    octave = pitches %>% purrr::map_dbl(function(pitch){
      interval = c(pitch,pitches[13])
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
    labels = paste(t$pitch_name)
    tonic =  t[[paste0(model,'.tonic')]]
    octave = t[[paste0(model,'.octave')]]
    plot_x_y(x=tonic,y=octave,
             labels=labels,title=paste(model,'tonic-octave'),
             xlab='tonic',ylab='octave')
    plot_x_y(x=tonic,y=rev(tonic),
             labels=labels,title=paste(model,'tonic-rev_tonic'),
             xlab='tonic',ylab='rev(tonic)')
  }
  for (model in working_models) {
    cat('.')
    labels = paste(t$pitch_name)
    tonic =  t[[paste0(model,'.tonic')]]
    rotated = cbind(tonic,rev(tonic)) %>% rotate(pi/4)
    brightness  = rotated[,1]
    affinity    = rotated[,2]
    plot_x_y(vertical_line=TRUE,x=brightness,y=affinity,
             labels=labels,title=paste(model,'tonic-rev_tonic rotated'),
             xlab='brightness',ylab='affinity')
  }
  expect_true(TRUE)
})
