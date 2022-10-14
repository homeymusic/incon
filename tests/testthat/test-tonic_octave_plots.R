test_that('tonic octave plots work for all models for core intervals',{
  t = tibble::tibble(
    pitch = 0:12,
    pitch_name = c('ton','m2','M2','m3','M3','P4',
                   'tt',
                   'P5','m6','M6','m7','M7','oct')
  )
  for (model in list_models()) {
    print(model)
    tonic_name  = paste0(model,'.tonic')
    octave_name = paste0(model,'.octave')

    tonic = 0:12 %>% purrr::map_dbl(function(pitch){
      interval = c(0,pitch)
      print(interval)
      incon(interval,model)})

    print(tonic)
    print(tonic_name)
    print(octave_name)
    t=tibble::add_column(t,
                         "{tonic_name}"  := tonic,
                         "{octave_name}" := rev(tonic))
  }
  print(t)
  for (model in list_models()) {
    png(paste0('./_plots/_png/',model,'.png'),width=1200,height=1200)
    x=t[[paste0(model,'.tonic')]]
    y=t[[paste0(model,'.octave')]]
    plot(x,y,main=model,cex=3)
    abline(0,1,col='lightgray')
    text(x,y,paste(t$pitch_name,"\n",t$pitch),pos=1)
    dev.off()
  }
  expect_true(TRUE)
})
