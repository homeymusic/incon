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
plot_x_y <- function(x,y,labels,title,xlab,ylab,vertical_line=FALSE,slope=1) {
  pdf(paste ('./_plots/_pdf/',title,'.pdf'))
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  if (vertical_line) {
    abline(v=0,col='lightgray')
  } else {
    abline(0,slope,col='lightgray')
  }
  text(x,y,labels,pos=1)
  dev.off()

  png(paste ('./_plots/_png/',title,'.png'),width=1000,height=1000)
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  if (vertical_line) {
    abline(v=0,col='lightgray')
  } else {
    abline(0,slope,col='lightgray')
  }
  text(x,y,labels,pos=1)
  dev.off()

  svg(paste0('./_plots/_svg/',title,'.svg'))
  plot(x,y,main=title,xlab=xlab,ylab=ylab)
  if (vertical_line) {
    abline(v=0,col='lightgray')
  } else {
    abline(0,slope,col='lightgray')
  }
  text(x,y,labels,pos=1)
  dev.off()
}
flip <- function(x) {
  x=dplyr::coalesce(x,0)
  max(x)-x
}
slope <- function(rise,run) {
  if (is.na(run) || run == 0) {
    1
  } else {
    rise / run
  }
}
angle <- function(slope) {
  pi / 2 - atan(slope)
}
angle_label <- function(angle_rads) {
 angle_deg = angle_rads * 180 / pi
 trunc(angle_deg)
}
test_that('tonic octave plots work for all models for core intervals',{
  pitches     = 60 + 0:12
  pitch_names = c('P1','m2','M2','m3','M3','P4',
                  'TT',
                  'P5','m6','M6','m7','M7','P8')

  # models = incon_models %>% dplyr::filter(label=='hutch_78_roughness')
  models = incon_models
  for (i in 1:nrow(models)) {
    model_label = models[[i, 'label']]
    tonic_consonance = tonic_dissonance = octave_consonance = octave_dissonance = NULL

    t = pitches %>% purrr::map_dbl(function(pitch){
      interval = c(pitches[1],pitch)
      incon(interval,model_label)})
    o = pitches %>% purrr::map_dbl(function(pitch){
      interval = c(pitch,pitches[13])
      incon(interval,model_label)})

    if (models[[i,'consonance']]) {
      tonic_consonance  = t
      tonic_dissonance  = flip(t)
      octave_consonance = o
      octave_dissonance = flip(o)
    } else {
      tonic_consonance  = flip(t)
      tonic_dissonance  = t
      octave_consonance = flip(o)
      octave_dissonance = o
    }

    # tonic-octave dissonance
    slope = slope((octave_dissonance[1]+octave_dissonance[13])/2,(tonic_dissonance[1] + tonic_dissonance[13])/2)
    plot_x_y(x=tonic_dissonance,y=octave_dissonance,
             labels=pitch_names,title=paste(model_label,'1 tonic-octave dissonance'),
             xlab='tonic dissonance',ylab='octave dissonance',slope=slope)

    # tonic-octave consonance
    slope = slope((octave_consonance[1]+octave_consonance[13])/2, (tonic_consonance[1] + tonic_consonance[13])/2)
    plot_x_y(x=tonic_consonance,y=octave_consonance,
             labels=pitch_names,title=paste(model_label,'2 tonic-octave consonance'),
             xlab='tonic consonance',ylab='octave consonance',slope=slope)

    # rotated tonic-octave consonance
    angle = angle(slope)
    rotated = cbind(tonic_consonance,octave_consonance) %>% rotate(angle)
    brightness  = rotated[,1]
    affinity    = rotated[,2]
    plot_x_y(vertical_line=TRUE,x=brightness,y=affinity,
             labels=pitch_names,title=paste(model_label,paste('3 rotated', angle_label(angle), 'deg tonic-octave consonance')),
             xlab='brightness',ylab='affinity',slope=slope)

    # tonic-rev(tonic) consonance
    slope = slope((rev(tonic_consonance)[1]+rev(tonic_consonance)[13])/2,(tonic_consonance[1] + tonic_consonance[13])/2)
    plot_x_y(x=tonic_consonance,y=rev(tonic_consonance),
             labels=pitch_names,title=paste(model_label,'4 tonic-rev(tonic) consonance'),
             xlab='tonic consonance',ylab='rev(tonic consonance)',slope=slope)

    # rotated tonic-rev(tonic) consonance
    angle = angle(slope)
    rotated = cbind(tonic_consonance,rev(tonic_consonance)) %>% rotate(angle)
    brightness  = rotated[,1]
    affinity    = rotated[,2]
    plot_x_y(vertical_line=TRUE,x=brightness,y=affinity,
             labels=pitch_names,title=paste(model_label,paste('5 rotated', angle_label(angle), 'deg tonic-rev(tonic) consonance')),
             xlab='brightness',ylab='affinity',slope=slope)
    expect_true(TRUE)
  }
})
