##### TOSCA Big Five and Life Goals (Co-)Development
##### Custom Functions

## helper functions ############################################################

# get loading parameters
load.params <- function(var, waves) {
  return(list(var.waves = paste0(var, '.', waves),
              var.i = paste0(var, '.i'),
              var.s = paste0(var, '.s'),
              var.q = paste0(var, '.q'),
              var.l = paste0(var, '.l')))
}

# get scaled linear loadings
get.linear.loadings <- function(waves) {
  if (identical(waves, 1:8)) return(c(0, 0.5, 1, 1.5, 2, 2.5, 3.5, 4.5)) # T1-8
  if (identical(waves, 1:4)) return(c(0, 0.5, 1, 1.5))                   # T1-4
  if (identical(waves, 4:8)) return(c(0, 0.5, 1, 2, 3))                  # T4-8
}

# get variable full name
get.var.fullname <- function(var.shortname) {
  return(c(extr = 'Extraversion',
           agre = 'Agreeableness',
           cons = 'Conscientiousness',
           neur = 'Neuroticism',
           open = 'Openness',
           pros = 'Prosperity',
           fame = 'Fame',
           attr = 'Attractiveness',
           pers = 'Personal Growth',
           rela = 'Relationship',
           soci = 'Society',
           phys = 'Physical Health',
           hedo = 'Hedonism',
           libe = 'Liberal Values',
           secu = 'Security',
           conf = 'Conformism',
           reli = 'Religiosity')[var.shortname])
}

# get variable color
get.var.color <- function(var.shortname) {
  return(c(extr = '#00ae3a',
           agre = '#C00000',
           cons = '#006cc4',
           neur = '#6c1d9d',
           open = '#ffb900',
           pros = '#00af37',
           fame = '#74209e',
           attr = '#ff6ec7',
           pers = '#fcfe00',
           rela = '#7e7e7e',
           soci = '#fc7400',
           phys = '#006cc4',
           hedo = '#ff0000',
           libe = 'Liberal Values',
           secu = 'Security',
           conf = 'Conformism',
           reli = 'Religiosity')[var.shortname])
}

# goal vector based on model type
goals.bivariate <- c('pros', 'fame', 'attr', 'pers', 'rela', 'soci', 'phys', 'hedo')
goals.regression <- c('libe', 'secu', 'conf', 'reli')

## for univariate growth models ################################################

# no change growth script
nochng.script <- function(var, waves) {
  list2env(load.params(var, waves),
           envir = environment())
  
  script <- paste(
    '# intercept loadings',
    paste(paste0(var.i, ' =~ 1*', var.waves), collapse = '\n'),
    '',
    
    '# latent means',
    paste(var.i, '~ 1'),
    '',
    
    '# latent variances',
    paste(var.i, '~~', var.i),
    '',
    
    '# constrain means to 0',
    paste(paste0(var.waves, ' ~ 0*1'), collapse = '\n'),
    '',
    
    '# constrain variances',
    paste(paste0(var.waves, ' ~~ ', var, '.theta*', var.waves), collapse = '\n'),
    '',
    
    sep = '\n'
  )
  
  return(script)
}

# linear growth script
linear.script <- function(var, waves) {
  list2env(load.params(var, waves),
           envir = environment())
  
  linear.loadings <- get.linear.loadings(waves)
  
  script <- paste(
    '# intercept loadings',
    paste(paste0(var.i, ' =~ 1*', var.waves), collapse = '\n'),
    '',
    
    '# slope loadings',
    paste(var.s, ' =~', paste0(linear.loadings, '*', var.waves), collapse = '\n'),
    '',
    
    '# latent means',
    paste(var.i, '~ 1'),
    paste(var.s, '~ 1'),
    '',
    
    '# latent variances',
    paste(var.i, '~~', var.i),
    paste(var.s, '~~', var.s),
    '',
    
    '# latent covariances',
    paste(var.i, '~~', var.s),
    '',
    
    '# constrain means to 0',
    paste(paste0(var.waves, ' ~ 0*1'), collapse = '\n'),
    '',
    
    '# constrain variances',
    paste(paste0(var.waves, ' ~~ ', var, '.theta*', var.waves), collapse = '\n'),
    '',
    
    sep = '\n'
  )
  
  return(script)
}

# quadratic growth script
quadra.script <- function(var, waves) {
  list2env(load.params(var, waves),
           envir = environment())
  
  linear.loadings <- get.linear.loadings(waves)
  quadra.loadings <- linear.loadings^2
  
  script <- paste(
    '# intercept loadings',
    paste(paste0(var.i, ' =~ 1*', var.waves), collapse = '\n'),
    '',
    
    '# linear change loadings',
    paste(var.s, ' =~', paste0(linear.loadings, '*', var.waves), collapse = '\n'),
    '',
    
    '# quadratic change loadings',
    paste(var.q, ' =~', paste0(quadra.loadings, '*', var.waves), collapse = '\n'),
    '',
    
    '# latent means',
    paste(var.i, '~ 1'),
    paste(var.s, '~ 1'),
    paste(var.q, '~ 1'),
    '',
    
    '# latent variances',
    paste(var.i, '~~', var.i),
    paste(var.s, '~~', var.s),
    paste(var.q, '~~', var.q),
    '',
    
    '# latent covariances',
    paste(var.i, '~~', var.s),
    paste(var.i, '~~', var.q),
    paste(var.s, '~~', var.q),
    '',
    
    '# constrain means to 0',
    paste(paste0(var.waves, ' ~ 0*1'), collapse = '\n'),
    '',
    
    '# constrain variances',
    paste(paste0(var.waves, ' ~~ ', var, '.theta*', var.waves), collapse = '\n'),
    '',
    
    sep = '\n'
  )
  
  return(script)
}

# latent basis growth script
latent.script <- function(var, waves) {
  list2env(load.params(var, waves),
           envir = environment())
  
  script <- paste(
    '# intercept loadings',
    paste(paste0(var.i, ' =~ 1*', var.waves), collapse = '\n'),
    '',
    
    '# slope loadings',
    paste(paste(var.l, ' =~', ifelse(waves == min(waves), paste0('0*', var.waves),
                                     ifelse(waves == max(waves), paste0('1*', var.waves),
                                            var.waves))), collapse = '\n'),
    '',
    
    '# latent means',
    paste(var.i, '~ 1'),
    paste(var.l, '~ 1'),
    '',
    
    '# latent variances',
    paste(var.i, '~~', var.i),
    paste(var.l, '~~', var.l),
    '',
    
    '# latent covariance',
    paste(var.i, '~~', var.l),
    '',
    
    '# constrain means to 0',
    paste(paste0(var.waves, ' ~ 0*1'), collapse = '\n'),
    '',
    
    '# constrain variances',
    paste(paste0(var.waves, ' ~~ ', var, '.theta*', var.waves), collapse = '\n'),
    '',
    
    sep = '\n'
  )
  
  return(script)
}

# get model results (including estimates and fit measures)
get.univariate.results <- function(model, 
                                   var,
                                   growth,
                                   waves,
                                   gender.index = FALSE) {
  
  # growth parameters
  growth.params <- list(nochng = c('i'),
                        linear = c('i', 's'),
                        quadra = c('i', 's', 'q'),
                        latent = c('i', 'l'))
  params <- growth.params[[growth]]
  
  # estimates
  if (is.na(gender.index)) {
    est <- parameterEstimates(model)
  } else {
    est <- subset(parameterEstimates(model), group == gender.index)
  }
  
  # init an empty vector
  results <- rep('', 28)
  
  # get linear loadings
  linear.loadings <- get.linear.loadings(waves)
  
  # define linear loading placement
  if (length(linear.loadings) == 8) loading.placement <- c(3:10)
  if (length(linear.loadings) == 4) loading.placement <- c(3:6)
  if (length(linear.loadings) == 5) loading.placement <- c(6:10)
  
  # grab slope loadings
  if (growth == 'nochng') {
    results[loading.placement] <- '—'
  }
  if (growth == 'linear') {
    results[loading.placement] <- linear.loadings
  }
  if (growth == 'quadra') {
    results[loading.placement] <- linear.loadings^2
  }
  if (growth == 'latent') {
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      free.loadings <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        free.loading <- est$est[est$lhs == paste0(var, '.l') & 
                                  est$rhs == paste0(var, '.', i.scaled)]
        free.loadings[i] <- formatC(free.loading,
                                    format = 'f',
                                    digit = 3)
      } 
    } else {
      free.loadings <- c()
      
      for (i in waves) {
        free.loading <- est$est[est$lhs == paste0(var, '.l') & 
                                  est$rhs == paste0(var, '.', i)]
        free.loadings[i] <- formatC(free.loading,
                                    format = 'f',
                                    digit = 3)
      }
    }
    
    # round 0.000 and 1.000
    free.loadings <- ifelse(as.numeric(free.loadings) == 0, '0',
                            ifelse(as.numeric(free.loadings) == 1, '1',
                                   free.loadings))
    
    results[loading.placement] <- free.loadings
  }
  
  ## grab means and variances
  
  # get star (check whether significant)
  get.star <- function(pvalue) {
    ifelse(is.na(pvalue), '', ifelse(pvalue < 0.05, '*', ''))
  }
  
  if ('i' %in% params) {
    mean <- est[est$lhs == paste0(var, '.i') & est$op == '~1', ]
    vari <- est[est$lhs == paste0(var, '.i') & est$op == '~~' & est$rhs == paste0(var, '.i'), ]
    results[12] <- paste0(
      formatC(mean$est, format = 'f', digit = 3), get.star(mean$pvalue), '\n(',
      formatC(vari$est, format = 'f', digit = 3), get.star(vari$pvalue), ')'
    )}
  if ('s' %in% params) {
    mean <- est[est$lhs == paste0(var, '.s') & est$op == '~1', ]
    vari <- est[est$lhs == paste0(var, '.s') & est$op == '~~' & est$rhs == paste0(var, '.s'), ]
    results[13] <- paste0(
      formatC(mean$est, format = 'f', digit = 3), get.star(mean$pvalue), '\n(',
      formatC(vari$est, format = 'f', digit = 3), get.star(vari$pvalue), ')'
    )}
  if ('q' %in% params) {
    mean <- est[est$lhs == paste0(var, '.q') & est$op == '~1', ]
    vari <- est[est$lhs == paste0(var, '.q') & est$op == '~~' & est$rhs == paste0(var, '.q'), ]
    results[14] <- paste0(
      formatC(mean$est, format = 'f', digit = 3), get.star(mean$pvalue), '\n(',
      formatC(vari$est, format = 'f', digit = 3), get.star(vari$pvalue), ')'
    )}
  if ('l' %in% params) {
    mean <- est[est$lhs == paste0(var, '.l') & est$op == '~1', ]
    vari <- est[est$lhs == paste0(var, '.l') & est$op == '~~' & est$rhs == paste0(var, '.l'), ]
    results[15] <- paste0(
      formatC(mean$est, format = 'f', digit = 3), get.star(mean$pvalue), '\n(',
      formatC(vari$est, format = 'f', digit = 3), get.star(vari$pvalue), ')'
    )}
  
  ## grab covariances (if applicable)
  cov.map <- list(
    'i~~s' = 17, # level, linear slope
    'i~~q' = 18, # level, quadratic slope
    's~~q' = 19, # linear slope, quadratic slope
    'i~~l' = 20  # level, total change
  )
  
  if (growth != 'nochng') {
    pairs <- combn(params, 2)
    
    for (pair in 1:ncol(pairs)) {
      x <- pairs[1, pair]
      y <- pairs[2, pair]
      
      key <- paste0(x, "~~", y)
      lhs <- paste0(var, '.', x)
      rhs <- paste0(var, '.', y)
      
      covar <- est[est$lhs == lhs & est$op == '~~' & est$rhs == rhs, ]
      
      if (nrow(covar) > 0 && key %in% names(cov.map)) {
        results[cov.map[[key]]] <- paste0(
          formatC(covar$est, format = 'f', digits = 3), get.star(covar$pvalue)
        )
      }
    }
  }
  
  ## grab fit measures
  results[22] <- paste0(round(fitMeasures(model)[['chisq']], 3))
  results[23] <- paste0(round(fitMeasures(model)[['aic']], 3))
  results[24] <- paste0(round(fitMeasures(model)[['bic']], 3))
  results[25] <- paste0(round(fitMeasures(model)[['df']], 3))
  results[26] <- paste0(formatC(fitMeasures(model)[['rmsea']], digits = 3, format = 'f'),
                        '\n[',
                        formatC(fitMeasures(model)[['rmsea.ci.lower']], digits = 3, format = 'f'),
                        ', ',
                        formatC(fitMeasures(model)[['rmsea.ci.upper']], digits = 3, format = 'f'),
                        ']')
  results[27] <- paste0(round(fitMeasures(model)[['cfi']], 3))
  
  # finally, replace all empty cells w/ '—'
  results[-c(1, 2, 11, 16, 21, 28)][results[-c(1, 2, 11, 16, 21, 28)] == ''] <- '—'
  
  return(results)
}

# export all univariate model results
export.univariate.results <- function(model.nochng, 
                                      model.linear,
                                      model.quadra,
                                      model.latent,
                                      gender = FALSE,
                                      waves) {
  
  # deparse attributes
  var <- strsplit(deparse(substitute(model.nochng)), '\\.')[[1]][1] # e.g., extr
  waves.notation <- deparse(substitute(waves))                      # 1:8
  
  # define gender index
  gender.index <- ifelse(gender == 'female', 1,
                         ifelse(gender == 'male', 2, NA))
  
  # grab model estimates
  model.nochng.est <- get.univariate.results(model.nochng, 
                                             var,
                                             'nochng', 
                                             waves,
                                             gender.index)
  model.linear.est <- get.univariate.results(model.linear, 
                                             var,
                                             'linear',
                                             waves,
                                             gender.index)
  model.quadra.est <- get.univariate.results(model.quadra,
                                             var,
                                             'quadra',
                                             waves,
                                             gender.index)
  model.latent.est <- get.univariate.results(model.latent, 
                                             var,
                                             'latent',
                                             waves,
                                             gender.index)
  
  ## compute fit changes
  
  # chisq delta
  chisq.nochng <- as.numeric(model.nochng.est[22])
  chisq.linear <- as.numeric(model.linear.est[22])
  chisq.quadra <- as.numeric(model.quadra.est[22])
  chisq.latent <- as.numeric(model.latent.est[22])
  
  # df delta
  df.nochng <- as.numeric(model.nochng.est[25])
  df.linear <- as.numeric(model.linear.est[25])
  df.quadra <- as.numeric(model.quadra.est[25])
  df.latent <- as.numeric(model.latent.est[25])
  
  # take differences
  model.nochng.est[28] <- '—'
  model.linear.est[28] <- paste(abs(chisq.linear - chisq.nochng), '/', abs(df.linear - df.nochng))
  model.quadra.est[28] <- paste(abs(chisq.quadra - chisq.linear), '/', abs(df.quadra - df.linear))
  model.latent.est[28] <- paste(abs(chisq.latent - chisq.linear), '/', abs(df.latent - df.linear))
  
  # define row labels (i.e., first column)
  row.labels <- c('',
                  'Slope Loadings',
                  'B(0)',
                  'B(0.5)',
                  'B(1)',
                  'B(1.5)',
                  'B(2)',
                  'B(2.5)',
                  'B(3.5)',
                  'B(4.5)',
                  'Means (Variances)', 
                  'Level', 
                  'Linear Slope', 
                  'Quadratic Slope',
                  'Total Change',
                  'Covariances',
                  'Level, Linear Slope',
                  'Level, Qudratic Slope',
                  'Linear Slope, Quadratic Slope',
                  'Level, Total Change',
                  'Goodness-of-Fit',
                  'Chi-square',
                  'AIC',
                  'BIC',
                  'Degrees of freedom',
                  'RMSEA [90%]',
                  'CFI',
                  'Fit changes')
  
  # combine all model estimates
  model.results <- data.frame(row.labels,
                              'No Growth' = model.nochng.est,
                              'Linear' = model.linear.est,
                              'Quadratic' = model.quadra.est,
                              'Latent Basis' = model.latent.est,
                              check.names = FALSE)
  
  # make first cell the variable full name
  colnames(model.results)[1] <- get.var.fullname(var)
  
  # gender tag
  if (is.na(gender)) {
    gender.tag <- ''
  } else {
    gender.tag <- paste0('_', gender)
  }
  
  # set file path (e.g., extr_1/8.xlsx)
  file.path <- here(paste0('Output/Primary/Analysis 2/Model Estimates'),
                    paste0(var, '_', waves.notation, gender.tag, '.xlsx'))
  
  # export
  write.xlsx(model.results, file.path)
}

# export univariate model results (gender-specific)
export.univariate.gender.results <- function(model.constr, 
                                             model.freely, 
                                             growth, 
                                             waves) {
  
  # deparse attributes
  var <- strsplit(deparse(substitute(model.constr)), '\\.')[[1]][1] # e.g., extr
  waves.notation <- deparse(substitute(waves))                      # 1:8
  
  # grab model estimates
  model.constr.est <- get.univariate.results(model.constr, 
                                             var,
                                             growth,
                                             waves,
                                             1) # note. female == male
  model.freely.male.est <- get.univariate.results(model.freely, 
                                                  var,
                                                  growth,
                                                  waves,
                                                  2)
  model.freely.female.est <- get.univariate.results(model.freely, 
                                                    var,
                                                    growth,
                                                    waves,
                                                    1)
  
  ## compute fit changes
  
  # chisq delta
  chisq.constr <- as.numeric(model.constr.est[22])
  chisq.freely <- as.numeric(model.freely.male.est[22])
  
  # df delta
  df.constr <- as.numeric(model.constr.est[25])
  df.freely <- as.numeric(model.freely.male.est[25])
  
  # take differences
  model.constr.est[28] <- '—'
  model.freely.male.est[28] <- paste(abs(chisq.constr - chisq.freely), '/', 
                                     abs(df.constr - df.freely))
  
  # define row labels (i.e., first column)
  row.labels <- c('',
                  'Slope Loadings',
                  'B(0)',
                  'B(0.5)',
                  'B(1)',
                  'B(1.5)',
                  'B(2)',
                  'B(2.5)',
                  'B(3.5)',
                  'B(4.5)',
                  'Means (Variances)', 
                  'Level', 
                  'Linear Slope', 
                  'Quadratic Slope',
                  'Total Change',
                  'Covariances',
                  'Level, Linear Slope',
                  'Level, Qudratic Slope',
                  'Linear Slope, Quadratic Slope',
                  'Level, Total Change',
                  'Goodness-of-Fit',
                  'Chi-square',
                  'AIC',
                  'BIC',
                  'Degrees of freedom',
                  'RMSEA [90%]',
                  'CFI',
                  'Fit changes')
  
  # combine all model estimates
  model.results <- data.frame(row.labels,
                              'Constrained' = model.constr.est,
                              'Freely (Male)' = model.freely.male.est,
                              'Freely (Female)' = model.freely.female.est,
                              check.names = FALSE)
  
  # make first cell the variable full name
  colnames(model.results)[1] <- get.var.fullname(var)
  
  # set file path (e.g., extr_1/8.xlsx)
  file.path <- here(paste0('Output/Sensitivity/Analysis 2/Model Estimates'),
                    paste0(var, '.xlsx'))
  
  # export
  write.xlsx(model.results, file.path)
}

## for bivariate growth models #################################################

# get covariance parameters (linear)
bivariate.linear.cov <- function(x, y, waves, 
                                 gender.analysis = FALSE) {
  
  x.params <- c('i', 's')
  y.params <- c('i', 's')
  
  script <- '# latent covariances (bivariate)'
  
  for (x.param in x.params) {
    for (y.param in y.params) {
      
      if (gender.analysis) {
        gender.coeffs <- paste0('c(female.', x.param, y.param,
                                ', male.', x.param, y.param, ')*')
      } else {
        gender.coeffs <- ''
      }
      
      script <- paste0(script, '\n',
                       x, '.', x.param, ' ~~ ', gender.coeffs,
                       y, '.', y.param)
    }
  }
  
  script <- paste0(script, '\n\n',
                   '# constrain covariances (bivariate)')
  
  for (i in waves) {
    script <- paste0(script, '\n',
                     x, '.', i, ' ~~ biva.theta*', 
                     y, '.', i)
  }
  
  return(script)
}

# get covariance parameters (best-fitting)
bivariate.bestfit.cov <- function(x, x.growth,
                                  y, y.growth,
                                  waves, 
                                  gender.analysis = FALSE) {
  
  growth.params <- list(nochng = c('i'),
                        linear = c('i', 's'),
                        quadra = c('i', 's', 'q'),
                        latent = c('i', 'l'))
  
  x.params <- growth.params[[x.growth]]
  y.params <- growth.params[[y.growth]]
  
  script <- '# latent covariances (bivariate)'
  
  for (x.param in x.params) {
    for (y.param in y.params) {
      
      if (gender.analysis) {
        gender.coeffs <- paste0('c(female.', x.param, y.param,
                                ', male.', x.param, y.param, ')*')
      } else {
        gender.coeffs <- ''
      }
      
      script <- paste0(script, '\n',
                       x, '.', x.param, ' ~~ ', gender.coeffs,
                       y, '.', y.param)
    }
  }
  
  script <- paste0(script, '\n\n',
                   '# constrain covariances (bivariate)')
  
  for (i in waves) {
    script <- paste0(script, '\n',
                     x, '.', i, ' ~~ biva.theta*', 
                     y, '.', i)
  }
  
  return(script)
}

# get bivariate growth script (linear)
bivariate.linear.script <- function(x, y, waves, gender.analysis = FALSE) {
  waves.notation <- deparse(substitute(waves))
  
  x.script <- get('linear.script')(x, waves)
  y.script <- get('linear.script')(y, waves)
  
  script <- paste(x.script, 
                  y.script,
                  bivariate.linear.cov(x, y, waves,
                                       gender.analysis),
                  sep = '\n')
}

# get bivariate growth script (best-fitting)
bivariate.bestfit.script <- function(x, y, waves, gender.analysis = FALSE) {
  waves.notation <- deparse(substitute(waves))
  
  x.growth <- univariate[[waves.notation]][univariate$var == x]
  y.growth <- univariate[[waves.notation]][univariate$var == y]
  
  x.func <- paste0(x.growth, '.script')
  y.func <- paste0(y.growth, '.script')
  
  x.script <- get(x.func)(x, waves)
  y.script <- get(y.func)(y, waves)
  
  script <- paste(x.script, 
                  y.script,
                  bivariate.bestfit.cov(x, x.growth,
                                        y, y.growth, waves,
                                        gender.analysis),
                  sep = '\n')
}

# get model results (including estimates and fit measures; linear)
get.bivariate.linear.results <- function(model, model.notation, waves.notation) {
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  # growth parameters
  x.params <- c('i', 's')
  y.params <- c('i', 's')
  
  # grab model parameters
  params <- standardizedSolution(model, ci = TRUE)
  
  # param mapping for readable labels
  param_map <- c(i = 'Level', s = 'Linear')
  
  # define all desired columns
  columns <- c(
    paste0('Level_',  x, ', Level_',  y),  # r(level x, level y)
    paste0('Linear_', x, ', Linear_', y),  # r(slope x, slope y)
    paste0('Level_',  x, ', Linear_', y),  # r(level x, slope y)
    paste0('Level_',  y, ', Linear_', x)   # r(slope y, level x)
  )
  
  # init results with '—'
  results <- setNames(as.list(rep('—', length(columns))), columns)
  
  # fill results where available
  for (x.param in x.params) {
    for (y.param in y.params) {
      x.par <- paste0(x, '.', x.param)
      y.par <- paste0(y, '.', y.param)
      
      # flexible matching
      est <- grepl(x.par, params$lhs) & params$op == '~~' & grepl(y.par, params$rhs)
      
      if (any(est)) {
        corr <- round(params$est[est], 3)
        ci.l <- round(params$ci.lower[est], 3)
        ci.u <- round(params$ci.upper[est], 3)
        pval <- round(params$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        # only for r(slope trait, level goal)
        if (x.param %in% c('s','q','l') & y.param == 'i') {
          colname <- paste0('Level_', y, ', ', param_map[[x.param]], '_', x)
        } else {
          # default: x first, y second
          colname <- paste0(param_map[[x.param]], '_', x, ', ', param_map[[y.param]], '_', y)
        }
        
        results[[colname]] <- paste0(corr, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  }
  
  table <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  
  return(table)
}

# get model results (including estimates and fit measures; best-fitting)
get.bivariate.bestfit.results <- function(model, model.notation, waves.notation) {
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  # variable growth type
  x.growth <- univariate[[waves.notation]][univariate$var == x]
  y.growth <- univariate[[waves.notation]][univariate$var == y]
  
  growth.params <- list(
    nochng = c('i'),
    linear = c('i', 's'),
    quadra = c('i', 's', 'q'),
    latent = c('i', 'l')
  )
  
  x.params <- growth.params[[x.growth]]
  y.params <- growth.params[[y.growth]]
  
  # grab model parameters
  params <- standardizedSolution(model, ci = TRUE)
  
  # param mapping for readable labels
  param_map <- c(
    i = 'level',
    s = 'linear',
    q = 'quadratic',
    l = 'total'
  )
  
  # define all desired columns
  all.cols <- c(
    # r(level trait, level goal)
    paste0('level_', x, ', level_', y),
    
    # r(slope trait, slope goal)
    paste0('linear_', x, ', linear_', y),
    paste0('linear_', x, ', quadratic_', y),
    paste0('linear_', x, ', total_', y),
    paste0('quadratic_', x, ', linear_', y),
    paste0('quadratic_', x, ', quadratic_', y),
    paste0('quadratic_', x, ', total_', y),
    paste0('total_', x, ', linear_', y),
    paste0('total_', x, ', quadratic_', y),
    paste0('total_', x, ', total_', y),
    
    # r(level trait, slope goal)
    paste0('level_', x, ', linear_', y),
    paste0('level_', x, ', quadratic_', y),
    paste0('level_', x, ', total_', y),
    
    # r(slope trait, level goal)
    paste0('level_', y, ', linear_', x),
    paste0('level_', y, ', quadratic_', x),
    paste0('level_', y, ', total_', x)
  )
  
  # init results with '—'
  results <- setNames(as.list(rep('—', length(all.cols))), all.cols)
  
  # fill results where available
  for (x.param in x.params) {
    for (y.param in y.params) {
      x.par <- paste0(x, '.', x.param)
      y.par <- paste0(y, '.', y.param)
      
      # flexible matching
      est <- grepl(x.par, params$lhs) & params$op == '~~' & grepl(y.par, params$rhs)
      
      if (any(est)) {
        corr <- round(params$est[est], 3)
        ci.l <- round(params$ci.lower[est], 3)
        ci.u <- round(params$ci.upper[est], 3)
        pval <- round(params$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        # only for r(slope trait, level goal)
        if (x.param %in% c('s','q','l') & y.param == 'i') {
          colname <- paste0('level_', y, ', ', param_map[[x.param]], '_', x)
        } else {
          # default: x first, y second
          colname <- paste0(param_map[[x.param]], '_', x, ', ', param_map[[y.param]], '_', y)
        }
        
        results[[colname]] <- paste0(corr, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  }
  
  table <- as.data.frame(results, stringsAsFactors = FALSE)
  return(table)
}

# export all bivariate model results (linear)
export.bivariate.linear.results <- function(model, waves) {
  
  # model and wave notations
  model.notation <- deparse(substitute(model))
  waves.notation <- deparse(substitute(waves))
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  var.fullname <- get.var.fullname(x)
  
  # get bivariate results
  model.est <- get.bivariate.linear.results(model, model.notation, waves.notation)
  
  # prepend variable name column
  model.results <- cbind(Variable = var.fullname, model.est)
  
  # set file path
  file.path <- here::here('Output/Primary/Analysis 3/Model Estimates/Linear',
                          paste0(x, '_', y, '.xlsx'))
  
  # export
  openxlsx::write.xlsx(model.results, file.path)
}

# export all bivariate model results (best-fitting)
export.bivariate.bestfit.results <- function(model, waves) {
  
  # model and wave notations
  model.notation <- deparse(substitute(model))
  waves.notation <- deparse(substitute(waves))
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  var.fullname <- get.var.fullname(x)
  
  # get bivariate results
  model.est <- get.bivariate.bestfit.results(model, model.notation, waves.notation)
  
  # prepend variable name column
  model.results <- cbind(Variable = var.fullname, model.est)
  
  # set file path
  file.path <- here::here('Output/Primary/Analysis 3/Model Estimates/Best-Fitting',
                          paste0(x, '_', y, '.xlsx'))
  
  # export
  openxlsx::write.xlsx(model.results, file.path)
}

# get model results (including estimates and fit measures)
get.bivariate.gender.results <- function(model, x, y, waves.notation, gender.index) {
  
  # linear growth params
  x.params <- c('i', 's')
  y.params <- c('i', 's')
  
  # grab model parameters
  if (is.na(gender.index)) {
    params <- standardizedSolution(model, ci = TRUE)
  } else {
    params <- subset(standardizedSolution(model, ci = TRUE), group == gender.index)
  }
  
  # define row labels for table (matches your second screenshot)
  row.labels <- c(
    paste0("Level_", x, ", Level_", y),
    paste0("Linear_", x, ", Linear_", y),
    paste0("Level_", x, ", Linear_", y),
    paste0("Level_", y, ", Linear_", x),
    'Goodness-of-Fit',
    'Chi-square',
    'AIC',
    'BIC',
    'Degrees of freedom',
    'RMSEA [90%]',
    'CFI',
    'Fit changes'
  )
  
  # initialize table with '—'
  results <- setNames(as.list(rep('—', length(row.labels))), row.labels)
  
  # fill in correlation estimates
  for (x.param in x.params) {
    for (y.param in y.params) {
      x.par <- paste0(x, '.', x.param)
      y.par <- paste0(y, '.', y.param)
      
      est <- params$op == '~~' &
        ((params$lhs == x.par & params$rhs == y.par) |
           (params$lhs == y.par & params$rhs == x.par))
      
      if (any(est)) {
        corr <- round(params$est[est], 3)
        ci.l <- round(params$ci.lower[est], 3)
        ci.u <- round(params$ci.upper[est], 3)
        pval <- round(params$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        # determine proper row label
        if (x.param == 'i' & y.param == 'i') {
          rowname <- paste0("Level_", x, ", Level_", y)
        } else if (x.param == 's' & y.param == 's') {
          rowname <- paste0("Linear_", x, ", Linear_", y)
        } else if (x.param == 'i' & y.param == 's') {
          rowname <- paste0("Level_", x, ", Linear_", y)
        } else if (x.param == 's' & y.param == 'i') {
          rowname <- paste0("Level_", y, ", Linear_", x)
        }
        
        results[[rowname]] <- paste0(corr, star, '\n', '[', ci.l, ', ', ci.u, ']')
      }
    }
  }
  
  # add fit measures
  results[['Chi-square']] <- round(fitMeasures(model)[['chisq']], 3)
  results[['AIC']] <- round(fitMeasures(model)[['aic']], 3)
  results[['BIC']] <- round(fitMeasures(model)[['bic']], 3)
  results[['Degrees of freedom']] <- round(fitMeasures(model)[['df']], 3)
  results[['RMSEA [90%]']] <- paste0(
    formatC(fitMeasures(model)[['rmsea']], digits=3, format='f'),
    '\n',
    '[', 
    formatC(fitMeasures(model)[['rmsea.ci.lower']], digits=3, format='f'),
    ', ',
    formatC(fitMeasures(model)[['rmsea.ci.upper']], digits=3, format='f'),
    ']'
  )
  results[['CFI']] <- round(fitMeasures(model)[['cfi']], 3)
  
  return(results)
}

# export bivariate results for constrained & freely estimated male/female models
export.bivariate.gender.results <- function(model.constr, model.freely, waves) {
  
  model.notation <- deparse(substitute(model.constr))
  waves.notation <- deparse(substitute(waves))
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  var.fullname <- get.var.fullname(x)
  
  # get estimates
  constr <- get.bivariate.gender.results(model.constr, x, y, waves.notation, 1)
  freely.m <- get.bivariate.gender.results(model.freely, x, y, waves.notation, 2)
  freely.f <- get.bivariate.gender.results(model.freely, x, y, waves.notation, 1)
  
  # compute fit change
  chisq.constr <- as.numeric(constr[['Chi-square']])
  chisq.freely <- as.numeric(freely.m[['Chi-square']])
  df.constr <- as.numeric(constr[['Degrees of freedom']])
  df.freely <- as.numeric(freely.m[['Degrees of freedom']])
  constr[['Fit changes']] <- '—'
  freely.m[['Fit changes']] <- paste0(round(abs(chisq.constr - chisq.freely), 3), 
                                      '/', abs(df.constr - df.freely))
  freely.f[['Fit changes']] <- '—'
  
  # combine into final table
  final <- data.frame(
    Comparison = names(constr),
    Constrained = unlist(constr),
    Freely.Male = unlist(freely.m),
    Freely.Female = unlist(freely.f),
    check.names = FALSE
  )
  
  # save to Excel
  file.path <- here::here('Output/Sensitivity/Analysis 3/Model Estimates',
                          paste0(x, '_', y, '.xlsx'))
  openxlsx::write.xlsx(final, file.path)
}

# get regression script (trait predicting goal)
trait.predict.linear.regress <- function(x, x.growth, y, gender.analysis = FALSE) {
  
  if (y == 'libe') {
    x.params <- c('i')
  } else {
    x.params <- c('i', 's')
  }
  
  script <- paste('# regress', y, 'on', x)
  
  for (x.param in x.params) {
    
    if (gender.analysis) {
      gender.coeffs <- paste0('c(female.', x.param, ', male.', x.param, ')*')
    } else {
      gender.coeffs <- paste0(x.param, '.theta*')
    }
    
    script <- paste0(script, '\n',
                     y, ' ~ ', gender.coeffs, x, '.', x.param)
  }
  
  return(script)
}

# get regression script (trait predicting goal)
trait.predict.bestfit.regress <- function(x, x.growth, y, gender.analysis = FALSE) {
  
  if (y == 'libe') {
    x.params <- c('i')
  } else {
    growth.params <- list(nochng = c('i'),
                          linear = c('i', 's'),
                          quadra = c('i', 's', 'q'),
                          latent = c('i', 'l'))
    
    x.params <- growth.params[[x.growth]]
  }
  
  script <- paste('# regress', y, 'on', x)
  
  for (x.param in x.params) {
    
    if (gender.analysis) {
      gender.coeffs <- paste0('c(female.', x.param, ', male.', x.param, ')*')
    } else {
      gender.coeffs <- paste0(x.param, '.theta*')
    }
    
    script <- paste0(script, '\n',
                     y, ' ~ ', gender.coeffs, x, '.', x.param)
  }
  
  return(script)
}

# get predictor script (trait predicting goal; linear)
trait.predict.linear.script <- function(x, y, waves, gender.analysis = FALSE) {
  waves.notation <- deparse(substitute(waves))
  
  x.script <- get('linear.script')(x, waves)
  
  script <- paste(x.script,
                  trait.predict.linear.regress(x, 'linear', y, gender.analysis),
                  sep = '\n')
}

# get predictor script (trait predicting goal; best-fitting)
trait.predict.bestfit.script <- function(x, y, waves, gender.analysis = FALSE) {
  waves.notation <- deparse(substitute(waves))
  
  x.growth <- univariate[[waves.notation]][univariate$var == x]
  x.func <- paste0(x.growth, '.script')
  x.script <- get(x.func)(x, waves)
  
  script <- paste(x.script,
                  trait.predict.bestfit.regress(x, x.growth, y, gender.analysis),
                  sep = '\n')
}

# get trait.predict regression results
trait.predict.table <- function(model, waves, tag) {
  waves.notation <- deparse(substitute(waves))
  
  # get variable names
  model.string <- deparse(substitute(model))
  x <- strsplit(model.string, '\\.')[[1]][1]
  y <- strsplit(model.string, '\\.')[[1]][2]
  
  # get variable growth
  x.growth <- univariate[[waves.notation]][univariate$var == x]
  
  growth.params <- list(nochng = c('i'),
                        linear = c('i', 's'),
                        quadra = c('i', 's', 'q'),
                        latent = c('i', 'l'))
  
  # get growth parameters
  x.params <- growth.params[[x.growth]]
  
  # grab parameters
  params <- parameterEstimates(model, ci = TRUE)
  
  script <- ''
  table <- matrix(ncol = 0, nrow = 0)
  
  for (x.param in x.params) {
    x.par <- paste0(x, '.', x.param)
    
    est <- params$lhs == y & 
      params$op == '~' &
      params$rhs == x.par
    
    corr <- round(params$est[est], 3)
    ci.l <- round(params$ci.lower[est], 3)
    ci.u <- round(params$ci.upper[est], 3)
    pval <- round(params$pvalue[est], 2)
    star <- ifelse(pval < 0.05, '*', '')
    
    table <- rbind(table,
                   data.frame(Association = paste0(y, ' ~ ', x.par),
                              Correlation = paste0(corr, star, ' [', ci.l, ', ', ci.u, ']')))
  }
  
  write.xlsx(table, file = here(paste0('Output/', tag, '/Analysis 3/Model Estimates'),
                                paste0('predict_', x, '_', y, '.xlsx')))
  
  return(script)
}

# get regression script (goal predicting trait)
goal.predict.regress <- function(x, y, y.growth, gender.analysis = FALSE) {
  growth.params <- list(linear = c('s'),
                        quadra = c('s', 'q'),
                        latent = c('l'))
  
  y.params <- growth.params[[y.growth]]
  
  script <- paste('# regress', y, 'on', x)
  
  for (y.param in y.params) {
    
    if (gender.analysis) {
      gender.coeffs <- paste0('c(female.', y.param, ', male.', y.param, ')*')
    } else {
      gender.coeffs <- paste0(y.param, '.theta*')
    }
    
    script <- paste0(script, '\n',
                     y, '.', y.param, ' ~ ', gender.coeffs, x)
  }
  
  return(script)
}

# get predictor script (goal predicting trait; linear)
goal.predict.linear.script <- function(x, 
                                       y, 
                                       waves, 
                                       gender.analysis = FALSE) {
  waves.notation <- deparse(substitute(waves))
  
  y.script <- get('linear.script')(y, waves)
  
  script <- paste(y.script,
                  goal.predict.regress(x, y, 'linear', gender.analysis),
                  sep = '\n')
}

# get predictor script (goal predicting trait; best-fitting)
goal.predict.bestfit.script <- function(x, y, waves, gender.analysis = FALSE) {
  waves.notation <- deparse(substitute(waves))
  
  y.growth <- univariate[[waves.notation]][univariate$var == y]
  y.func <- paste0(y.growth, '.script')
  y.script <- get(y.func)(y, waves)
  
  script <- paste(y.script,
                  goal.predict.regress(x, y, y.growth, gender.analysis),
                  sep = '\n')
}

# get goal.predict regression results
goal.predict.table <- function(model, waves, tag) {
  waves.notation <- deparse(substitute(waves))
  
  # get variable names
  model.string <- deparse(substitute(model))
  x <- strsplit(model.string, '\\.')[[1]][1]
  y <- strsplit(model.string, '\\.')[[1]][2]
  
  # get variable growth
  y.growth <- univariate[[waves.notation]][univariate$var == y]
  
  growth.params <- list(nochng = c('i'),
                        linear = c('i', 's'),
                        quadra = c('i', 's', 'q'),
                        latent = c('i', 'l'))
  
  # get growth parameters
  y.params <- growth.params[[y.growth]]
  
  # grab parameters
  params <- parameterEstimates(model, ci = TRUE)
  
  script <- ''
  table <- matrix(ncol = 0, nrow = 0)
  
  for (y.param in y.params) {
    y.par <- paste0(y, '.', y.param)
    
    est <- params$lhs == y.par & 
      params$op == '~' &
      params$rhs == x
    
    corr <- round(params$est[est], 3)
    ci.l <- round(params$ci.lower[est], 3)
    ci.u <- round(params$ci.upper[est], 3)
    pval <- round(params$pvalue[est], 2)
    star <- ifelse(pval < 0.05, '*', '')
    
    table <- rbind(table,
                   data.frame(Association = paste0(y.par, ' ~ ', x),
                              Correlation = paste0(corr, star, ' [', ci.l, ', ', ci.u, ']')))
  }
  
  write.xlsx(table, file = here(paste0('Output/', tag, '/Analysis 3/Model Estimates'),
                                paste0('predict_', x, '_', y, '.xlsx')))
  
  return(script)
}

# get regression model results (linear)
get.regression.linear.results <- function(model, 
                                          model.notation, 
                                          waves.notation,
                                          predictor) {
  
  # readable growth parameters
  param.readable <- c(
    i = 'Level',
    s = 'Linear')
  
  # grab model parameters
  model.parameters <- standardizedSolution(model, 
                                           type = 'std.all', 
                                           ci = TRUE, 
                                           level = 0.95)
  
  # define all desired columns
  all.cols <- c('level, level',
                'level, linear', 'level, quadratic', 'level, total')
  
  # init results with '—'
  results <- setNames(as.list(rep('—', length(all.cols))), all.cols)
  
  # grab estimates by x (predictor) and y (outcome)
  if (predictor == 'trait') {
    
    # e.g., extr predicting libe
    x <- strsplit(model.notation, '\\.')[[1]][1] # extr
    y <- strsplit(model.notation, '\\.')[[1]][2] # libe
    
    # x growth parameters
    x.params <- c('i', 's')
    
    for (x.param in x.params) {
      x.par <- paste0(x, '.', x.param) # extr.i
      
      # grab est
      est <- 
        (model.parameters[['lhs']] == y) & 
        (model.parameters[['op']] == '~') & 
        (model.parameters[['rhs']] == x.par)
      
      # grab est accessories
      if (any(est)) {
        beta <- round(model.parameters$est[est], 3)
        ci.l <- round(model.parameters$ci.lower[est], 3)
        ci.u <- round(model.parameters$ci.upper[est], 3)
        pval <- round(model.parameters$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        x.label <- paste0(param.readable[[x.param]], '_', x)
        y.label <- paste0('Level_', y)
        colname <- paste0(y.label, ', ', x.label)
        
        results[[colname]] <- 
          paste0(beta, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  } else if (predictor == 'goal') {
    
    # e.g., libe predicting extr
    x <- strsplit(model.notation, '\\.')[[1]][1] # libe
    y <- strsplit(model.notation, '\\.')[[1]][2] # extr
    
    # y growth parameters
    y.params <- c('i', 's')
    
    for (y.param in y.params) {
      y.par <- paste0(y, '.', y.param) # extr.i
      
      # grab est
      est <- 
        (model.parameters[['lhs']] == y.par) & 
        (model.parameters[['op']] == '~') & 
        (model.parameters[['rhs']] == x)
      
      # grab est accessories
      if (any(est)) {
        beta <- round(model.parameters$est[est], 3)
        ci.l <- round(model.parameters$ci.lower[est], 3)
        ci.u <- round(model.parameters$ci.upper[est], 3)
        pval <- round(model.parameters$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        y.label <- paste0(param.readable[[y.param]], '_', y)
        x.label <- paste0('Level_', x)
        colname <- paste0(x.label, ', ', y.label)
        
        results[[colname]] <- 
          paste0(beta, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  }
  
  table <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  
  return(table)
}

# get regression model results (best-fitting)
get.regression.bestfit.results <- function(model, 
                                           model.notation, 
                                           waves.notation,
                                           predictor) {
  
  # growth param map
  growth.params <- list(
    nochng = c('i'),
    linear = c('i', 's'),
    quadra = c('i', 's', 'q'),
    latent = c('i', 'l')
  )
  
  # readable growth parameters
  param.readable <- c(
    i = 'level',
    s = 'linear',
    q = 'quadratic',
    l = 'total'
  )
  
  # grab model parameters
  model.parameters <- standardizedSolution(model, 
                                           type = 'std.all', 
                                           ci = TRUE, 
                                           level = 0.95)
  
  # define all desired columns
  all.cols <- c('level, level',
                'level, linear', 'level, quadratic', 'level, total')
  
  # init results with '—'
  results <- setNames(as.list(rep('—', length(all.cols))), all.cols)
  
  # grab estimates by x (predictor) and y (outcome)
  if (predictor == 'trait') {
    
    # e.g., extr predicting libe
    x <- strsplit(model.notation, '\\.')[[1]][1] # extr
    y <- strsplit(model.notation, '\\.')[[1]][2] # libe
    
    # x growth parameters
    x.growth <- univariate[[waves.notation]][univariate$var == x]
    x.params <- growth.params[[x.growth]]
    
    for (x.param in x.params) {
      x.par <- paste0(x, '.', x.param) # extr.i
      
      # grab est
      est <- 
        (model.parameters[['lhs']] == y) & 
        (model.parameters[['op']] == '~') & 
        (model.parameters[['rhs']] == x.par)
      
      # grab est accessories
      if (any(est)) {
        beta <- round(model.parameters$est[est], 3)
        ci.l <- round(model.parameters$ci.lower[est], 3)
        ci.u <- round(model.parameters$ci.upper[est], 3)
        pval <- round(model.parameters$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        x.label <- param.readable[[x.param]]
        y.label <- 'level'
        colname <- paste0(y.label, ', ', x.label)
        
        results[[colname]] <- 
          paste0(beta, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  } else if (predictor == 'goal') {
    
    # e.g., libe predicting extr
    x <- strsplit(model.notation, '\\.')[[1]][1] # libe
    y <- strsplit(model.notation, '\\.')[[1]][2] # extr
    
    # y growth parameters
    y.growth <- univariate[[waves.notation]][univariate$var == y]
    y.params <- growth.params[[y.growth]]
    
    for (y.param in y.params) {
      y.par <- paste0(y, '.', y.param) # extr.i
      
      # grab est
      est <- 
        (model.parameters[['lhs']] == y.par) & 
        (model.parameters[['op']] == '~') & 
        (model.parameters[['rhs']] == x)
      
      # grab est accessories
      if (any(est)) {
        beta <- round(model.parameters$est[est], 3)
        ci.l <- round(model.parameters$ci.lower[est], 3)
        ci.u <- round(model.parameters$ci.upper[est], 3)
        pval <- round(model.parameters$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        y.label <- param.readable[[y.param]]
        x.label <- 'level'
        colname <- paste0(x.label, ', ', y.label)
        
        results[[colname]] <- 
          paste0(beta, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  }
  
  table <- as.data.frame(results, stringsAsFactors = FALSE)
  
  return(table)
}

# export regression model results
# important: before exporting corr, make sure there's only one predictor
# e.g., secu regressed on extr should only have extr.i OR extr.s as predictor
export.regression.linear.results <- function(model, predictor, waves) {
  
  # model and wave notations
  model.notation <- deparse(substitute(model))
  waves.notation <- deparse(substitute(waves))
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  # variable fullnames
  x.fullname <- get.var.fullname(x)
  y.fullname <- get.var.fullname(y)
  
  # get regression results
  model.est <- get.regression.linear.results(model, 
                                             model.notation, 
                                             waves.notation,
                                             predictor)
  
  # prepend variable name column
  model.results <- cbind(Variable = paste(y.fullname, '~', x.fullname), model.est)
  
  # set file path
  file.path <- here::here('Output/Primary/Analysis 3/Model Estimates/Linear',
                          paste0(x, '_', y, '.xlsx'))
  
  # export
  openxlsx::write.xlsx(model.results, file.path)
}

# export regression model results
export.regression.bestfit.results <- function(model, predictor, waves) {
  
  # model and wave notations
  model.notation <- deparse(substitute(model))
  waves.notation <- deparse(substitute(waves))
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  # variable fullnames
  x.fullname <- get.var.fullname(x)
  y.fullname <- get.var.fullname(y)
  
  # get regression results
  model.est <- get.regression.bestfit.results(model, 
                                              model.notation, 
                                              waves.notation,
                                              predictor)
  
  # prepend variable name column
  model.results <- cbind(Variable = paste(y.fullname, '~', x.fullname), model.est)
  
  # set file path
  file.path <- here::here('Output/Primary/Analysis 3/Model Estimates/Best-Fitting',
                          paste0(x, '_', y, '.xlsx'))
  
  # export
  openxlsx::write.xlsx(model.results, file.path)
}

# get model results (including estimates and fit measures)
get.regression.gender.results <- function(model,
                                          model.notation,
                                          waves.notation, 
                                          predictor,
                                          gender.index) {
  
  # readable growth parameters
  param.readable <- c(
    i = 'level',
    s = 'linear',
    q = 'quadratic',
    l = 'total'
  )
  
  # grab model parameters
  if (is.na(gender.index)) {
    params <- standardizedSolution(model, 
                                   ci = TRUE)
  } else {
    params <- subset(standardizedSolution(model, 
                                          ci = TRUE), 
                     group == gender.index)
  }
  
  # grab estimates by x (predictor) and y (outcome)
  if (predictor == 'trait') {
    
    # e.g., extr predicting libe
    x <- strsplit(model.notation, '\\.')[[1]][1] # extr
    y <- strsplit(model.notation, '\\.')[[1]][2] # libe
    
    # x growth parameters
    x.growth <- univariate[[waves.notation]][univariate$var == x]
    x.params <- c('i', 's')
    
    # define row labels for table (matches your second screenshot)
    row.labels <- c(
      paste0("level_", y, ", level_", x),
      paste0("level_", y, ", linear_", x),
      'Goodness-of-Fit',
      'Chi-square',
      'AIC',
      'BIC',
      'Degrees of freedom',
      'RMSEA [90%]',
      'CFI',
      'Fit changes')
    
    # init results with '—'
    results <- setNames(as.list(rep('—', length(row.labels))), row.labels)
    
    for (x.param in x.params) {
      x.par <- paste0(x, '.', x.param) # extr.i
      
      # grab est
      est <- 
        (params[['lhs']] == y) & 
        (params[['op']] == '~') & 
        (params[['rhs']] == x.par)
      
      # grab est accessories
      if (any(est)) {
        beta <- round(params$est[est], 3)
        ci.l <- round(params$ci.lower[est], 3)
        ci.u <- round(params$ci.upper[est], 3)
        pval <- round(params$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        x.label <- param.readable[[x.param]]
        y.label <- 'level'
        colname <- paste0(y.label, '_', y, ', ', x.label, '_', x)
        
        results[[colname]] <- 
          paste0(beta, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  } else if (predictor == 'goal') {
    
    # e.g., libe predicting extr
    x <- strsplit(model.notation, '\\.')[[1]][1] # libe
    y <- strsplit(model.notation, '\\.')[[1]][2] # extr
    
    # y growth parameters
    y.growth <- univariate[[waves.notation]][univariate$var == y]
    y.params <- c('i', 's')
    
    # define row labels for table (matches your second screenshot)
    row.labels <- c(
      paste0("level_", x, ", level_", y),
      paste0("level_", x, ", linear_", y),
      'Goodness-of-Fit',
      'Chi-square',
      'AIC',
      'BIC',
      'Degrees of freedom',
      'RMSEA [90%]',
      'CFI',
      'Fit changes')
    
    # init results with '—'
    results <- setNames(as.list(rep('—', length(row.labels))), row.labels)
    
    for (y.param in y.params) {
      y.par <- paste0(y, '.', y.param) # extr.i
      
      # grab est
      est <- 
        (params[['lhs']] == y.par) & 
        (params[['op']] == '~') & 
        (params[['rhs']] == x)
      
      # grab est accessories
      if (any(est)) {
        beta <- round(params$est[est], 3)
        ci.l <- round(params$ci.lower[est], 3)
        ci.u <- round(params$ci.upper[est], 3)
        pval <- round(params$pvalue[est], 2)
        star <- ifelse(pval < 0.05, '*', '')
        
        y.label <- param.readable[[y.param]]
        x.label <- 'level'
        colname <- paste0(x.label, '_', x, ', ', y.label, '_', y)
        
        results[[colname]] <- 
          paste0(beta, star, '\n', ' [', ci.l, ', ', ci.u, ']')
      }
    }
  }
  
  # add fit measures
  results[['Chi-square']] <- round(fitMeasures(model)[['chisq']], 3)
  results[['AIC']] <- round(fitMeasures(model)[['aic']], 3)
  results[['BIC']] <- round(fitMeasures(model)[['bic']], 3)
  results[['Degrees of freedom']] <- round(fitMeasures(model)[['df']], 3)
  results[['RMSEA [90%]']] <- paste0(
    formatC(fitMeasures(model)[['rmsea']], digits=3, format='f'),
    '\n',
    '[', 
    formatC(fitMeasures(model)[['rmsea.ci.lower']], digits=3, format='f'),
    ', ',
    formatC(fitMeasures(model)[['rmsea.ci.upper']], digits=3, format='f'),
    ']'
  )
  
  results[['CFI']] <- round(fitMeasures(model)[['cfi']], 3)
  
  return(results)
}

# export regression results for constrained & freely estimated male/female models
export.regression.gender.results <- function(model.constr, 
                                             model.freely, 
                                             predictor,
                                             waves) {
  
  # model and wave notations
  model.notation <- deparse(substitute(model.constr))
  waves.notation <- deparse(substitute(waves))
  
  # variable names
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  # variable fullnames
  x.fullname <- get.var.fullname(x)
  y.fullname <- get.var.fullname(y)
  
  # get regression results
  constr <- get.regression.gender.results(model.constr,
                                          model.notation,
                                          waves.notation, 
                                          predictor,
                                          1)
  freely.m <- get.regression.gender.results(model.freely,
                                            model.notation,
                                            waves.notation, 
                                            predictor,
                                            2)
  freely.f <- get.regression.gender.results(model.freely,
                                            model.notation,
                                            waves.notation, 
                                            predictor,
                                            1)
  
  # compute fit change
  chisq.constr <- as.numeric(constr[['Chi-square']])
  chisq.freely <- as.numeric(freely.m[['Chi-square']])
  df.constr <- as.numeric(constr[['Degrees of freedom']])
  df.freely <- as.numeric(freely.m[['Degrees of freedom']])
  constr[['Fit changes']] <- '—'
  freely.m[['Fit changes']] <- paste0(round(abs(chisq.constr - chisq.freely), 3), 
                                      '/', abs(df.constr - df.freely))
  freely.f[['Fit changes']] <- '—'
  
  # combine into final table
  final <- data.frame(
    Comparison = names(constr),
    Constrained = unlist(constr),
    Freely.Male = unlist(freely.m),
    Freely.Female = unlist(freely.f),
    check.names = FALSE
  )
  
  # save to Excel
  file.path <- here::here('Output/Sensitivity/Analysis 3/Model Estimates',
                          paste0(x, '_', y, '.xlsx'))
  
  # export
  openxlsx::write.xlsx(final, file.path)
}

## for plotting ################################################################

# get title
plot.title <- function(growth, constr.formal) {
  if (growth == 'nochng') title <- 'No Change'
  if (growth == 'linear') title <- 'Linear Growth'
  if (growth == 'quadra') title <- 'Quadratic Growth'
  if (growth == 'latent') title <- 'Latent Basis Growth'
  
  title <- paste(title, 'Trajectory of', constr.formal)
  
  return(title)
}

plot.subtitle <- function(waves) {
  subtitle <- paste0('T', waves[1], '–T', tail(waves, 1))
  
  return(subtitle)
}

# average trajectory
plot.average <- function(model, waves, color) {
  
  # model notation
  model.notation <- deparse(substitute(model))
  var <- strsplit(model.notation, '\\.')[[1]][1]
  growth <- strsplit(model.notation, '\\.')[[1]][2]
  
  # model parameters
  model.params <- parameterEstimates(model)
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # for linear models
  if (growth == 'linear') {
    
    # extract latent means
    model.i <- model.params$est[model.params$lhs == paste0(var, '.i') &
                                  model.params$op == '~1']
    model.s <- model.params$est[model.params$lhs == paste0(var, '.s') &
                                  model.params$op == '~1']
    
    # trajectory function
    model.func <- model.i + model.s*time
  }
  
  # for quadratic models
  if (growth == 'quadra') {
    
    # extract latent means
    model.i <- model.params$est[model.params$lhs == paste0(var, '.i') & 
                                  model.params$op == '~1']
    model.s <- model.params$est[model.params$lhs == paste0(var, '.s') & 
                                  model.params$op == '~1']
    model.q <- model.params$est[model.params$lhs == paste0(var, '.q') & 
                                  model.params$op == '~1']
    
    # trajectory function
    model.func <- model.i + model.s*time + model.q*time^2
  }
  
  # for latent basis models
  if (growth == 'latent') {
    
    # extract latent means
    model.i <- model.params$est[model.params$lhs == paste0(var, '.i') & 
                                  model.params$op == '~1']
    model.l <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                  model.params$op == '~1']
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      model.f <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        model.f[i] <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                         model.params$rhs == paste0(var, '.', i.scaled)]
      } 
    } else {
      model.f <- c()
      
      for (i in waves) {
        model.f[i] <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                         model.params$rhs == paste0(var, '.', i)]
      }
    }
    
    # trajectory function
    model.func <- model.i + model.l*model.f
  }
  
  # create plot data
  model.average.data <- data.frame(time = time,
                                   predicted = model.func)
  
  # assign global variable
  assign(paste0(var, '.average.data'), 
         model.average.data, 
         envir = .GlobalEnv)
  
  # grab variable attributes
  var.fullname <- get.var.fullname(var)
  var.color <- get.var.color(var)
  
  # grab sub/title
  title <- plot.title(growth, var.fullname)
  subtitle <- plot.subtitle(waves)
  
  # create plot
  model.plot <- 
    ggplot(model.average.data, aes(x = time.scaled, y = predicted)) +
    geom_line(color = var.color, linewidth = 1) +
    geom_point(color = var.color, size = 2) +
    labs(title = title, subtitle = subtitle,
         x = 'Time (Years)', y = var.fullname) + 
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic()
  
  # output
  return(model.plot)
}

# individual trajectories (i.e., spaghetti)
plot.spaghetti <- function(model, waves) {
  
  # model notation
  model.notation <- deparse(substitute(model))
  var <- strsplit(model.notation, '\\.')[[1]][1]
  growth <- strsplit(model.notation, '\\.')[[1]][2]
  
  # extract individual predicted values
  model.spaghetti.data <- as.data.frame(lavPredict(model))
  
  # save original row parameters
  rows <- nrow(model.spaghetti.data)
  model.spaghetti.data$row <- seq_len(rows)
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # make long to match number of timepoints
  model.spaghetti.data <- model.spaghetti.data[rep(1:rows, each = length(time)), ]
  
  # add time column
  model.spaghetti.data$time <- rep(time, times = rows)
  model.spaghetti.data$time.scaled <- model.spaghetti.data$time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # for linear models
  if (growth == 'linear') {
    
    # column notations
    model.i <- paste0(var, '.i')
    model.s <- paste0(var, '.s')
    
    # trajectory function
    model.spaghetti.data$predicted <- 
      model.spaghetti.data[[model.i]] + 
      model.spaghetti.data[[model.s]] * model.spaghetti.data$time
  }
  
  # for quadratic models
  if (growth == 'quadra') {
    
    # column notations
    model.i <- paste0(var, '.i')
    model.s <- paste0(var, '.s')
    model.q <- paste0(var, '.q')
    
    # trajectory function
    model.spaghetti.data$predicted <- 
      model.spaghetti.data[[model.i]] + 
      model.spaghetti.data[[model.s]] * model.spaghetti.data$time + 
      model.spaghetti.data[[model.q]] * model.spaghetti.data$time^2
  }
  
  # for latent basis models
  if (growth == 'latent') {
    
    # grab parameters
    model.params <- parameterEstimates(model)
    
    # column notations
    model.i <- paste0(var, '.i')
    model.l <- paste0(var, '.l')
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      model.f <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        model.f[i] <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                         model.params$rhs == paste0(var, '.', i.scaled)]
      } 
    } else {
      model.f <- c()
      
      for (i in waves) {
        model.f[i] <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                         model.params$rhs == paste0(var, '.', i)]
      }
    }
    
    # trajectory function
    model.spaghetti.data$predicted <- 
      model.spaghetti.data[[model.i]] + 
      model.spaghetti.data[[model.l]] * model.f
  }
  
  # assign global variable
  assign(paste0(var, '.spaghetti.data'), 
         model.spaghetti.data, 
         envir = .GlobalEnv)
  
  # grab variable attributes
  var.fullname <- get.var.fullname(var)
  var.color <- get.var.color(var)
  
  # grab sub/title
  title <- plot.title(growth, var.fullname)
  subtitle <- plot.subtitle(waves)
  
  # create plot
  model.spaghetti.plot <- 
    ggplot(model.spaghetti.data, 
           aes(x = time.scaled, 
               y = predicted, 
               group = row)) +
    geom_line(color = var.color, 
              linewidth = 0.25, 
              alpha = 0.3) +
    labs(title = title, 
         subtitle = subtitle,
         x = 'Time (Years)', 
         y = var.fullname) + 
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic()
  
  # output
  return(model.spaghetti.plot)
}

# combined (average + spaghetti)
plot.combined <- function(model, model.data, model.spaghetti, waves) {
  
  # model notation
  model.notation <- deparse(substitute(model))
  var <- strsplit(model.notation, '\\.')[[1]][1]
  growth <- strsplit(model.notation, '\\.')[[1]][2]
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # grab variable attributes
  var.fullname <- get.var.fullname(var)
  var.color <- get.var.color(var)
  
  # grab sub/title
  title <- plot.title(growth, var.fullname)
  subtitle <- plot.subtitle(waves)
  
  # create plot
  combined.plot <- 
    ggplot(model.spaghetti, 
           aes(x = time.scaled, 
               y = predicted, 
               group = row)) +
    geom_line(color = var.color, size = 0.25, alpha = 0.3) +
    geom_line(data = model.data, aes(x = time.scaled, y = predicted),
              inherit.aes = FALSE, color = 'black', size = 1) +
    geom_point(data = model.data, aes(x = time.scaled, y = predicted),
               inherit.aes = FALSE, color = 'black', size = 2) +
    labs(title = title, subtitle = subtitle,
         x = 'Time (Years)', y = var.fullname) + 
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic()
  
  # output
  return(combined.plot)
}

# level-slope (x level, y slope)
plot.level.slope <- function(model, gender.str = NA, waves,
                             x.formal, 
                             y.formal, y.growth.type) {
  
  # gender notation
  gender.index <- ifelse(gender.str == 'female', 1,
                         ifelse(gender.str == 'male', 2, NA))
  
  # construct notation
  model.notation <- deparse(substitute(model))
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  # x growth factor notation
  x.i <- paste0(x, '.i')
  
  ## y growth factor notations
  y.i <- paste0(y, '.i')
  
  if (y.growth.type == '.s') {
    y.s <- paste0(y, '.s')
  }
  
  if (y.growth.type == '.q') {
    y.s <- paste0(y, '.s')
    y.q <- paste0(y, '.q')
  }
  
  if (y.growth.type == '.l') {
    y.l <- paste0(y, '.l')
  }
  
  # extract latent variable scores
  if (is.na(gender.str)) {
    model.data <- as.data.frame(lavPredict(model, type = 'lv'))
  } else {
    model.data <- as.data.frame(lavPredict(model, type = 'lv')[[gender.index]])
  }
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # save original row parameters
  rows <- nrow(model.data)
  model.data$row <- seq_len(rows)
  
  # make long to match timepoints
  model.data <- model.data[rep(1:rows, each = length(time)), ]
  
  # add time column
  model.data$time <- rep(time, times = rows)
  model.data$time.scaled <- model.data$time  * 4
  
  ## create plot data
  if (y.growth.type == '.s') {
    model.data$predicted <- 
      model.data[[y.i]] +
      model.data[[y.s]] * time
  }
  
  if (y.growth.type == '.q') {
    model.data$predicted <- 
      model.data[[y.i]] +
      model.data[[y.s]] * time +
      model.data[[y.q]] * time^2
  }
  
  if (y.growth.type == '.l') {
    
    # grab parameters
    model.params <- parameterEstimates(model)
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      y.loadings <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        
        if (is.na(gender.str)) {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i.scaled)]
        } else {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i.scaled) &
                                              model.params$group == gender.index]
        }
      } 
    } else {
      y.loadings <- c()
      
      for (i in waves) {
        if (is.na(gender.str)) {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i)]
        } else {
          
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i) &
                                              model.params$group == gender.index]
        } 
      }
    }
    
    # trajectory function
    model.data$predicted <- 
      model.data[[y.i]] + 
      model.data[[y.l]] * y.loadings
  }
  
  # compute tertile cut-offs
  tertiles <- quantile(model.data[[x.i]],
                       probs = c(1/3, 2/3),
                       na.rm = TRUE)
  
  # create tertiles
  model.data <- model.data %>%
    mutate(x.group = case_when(
      .data[[x.i]] > tertiles[2] ~ 'Top 1/3',
      .data[[x.i]] > tertiles[1] & .data[[x.i]] <= tertiles[2] ~ 'Middle 1/3',
      .data[[x.i]] <= tertiles[1] ~ 'Bottom 1/3'
    )) %>% 
    filter(!is.na(x.group))
  
  # aggregate by tertile
  model.data.agg <- model.data %>%
    group_by(x.group, time.scaled) %>%
    summarize(predicted = mean(predicted, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(x.group = factor(x.group, levels = c('Top 1/3', 'Middle 1/3', 'Bottom 1/3')))
  
  # gender label (if applicable)
  gender.label <- if(is.na(gender.str)) {
    ''
  } else {
    paste0(' (', toTitleCase(gender.str), ')')
  }
  
  ## growth titles
  x.title <- paste('Initial Level of', x.formal)
  
  if (y.growth.type == '.s') {
    y.title <- paste('Linear Slope of', y.formal)
  }
  
  if (y.growth.type == '.q') {
    y.title <- paste('Quadratic Slope of', y.formal)
  }
  
  if (y.growth.type == '.l') {
    y.title <- paste('Latent Basis Slope of', y.formal)
  }
  
  # custom title
  title <- paste('Association Between', x.title, 'and', y.title, gender.label)
  
  # create plot
  model.plot <- 
    ggplot(model.data, 
           aes(x = time.scaled,
               y = predicted,
               group = row)) +
    geom_line(aes(color = x.group,
                  group = row),
              size = 0.25, 
              alpha = 0.025) +
    geom_line(data = model.data.agg, 
              aes(x = time.scaled, 
                  y = predicted, 
                  color = x.group, 
                  group = x.group),
              size = 1.2) +
    scale_color_manual(
      values = c('Top 1/3' = '#0072B2', 
                 'Middle 1/3' = '#009E73', 
                 'Bottom 1/3' = '#D55E00')) +
    labs(title = title,
         x = 'Time (Years)',
         y = y.title,
         color = paste('Level of', x.formal,'\n(T1)')) +
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic() + 
    coord_cartesian(clip = 'off') +
    theme(legend.position = "none")
  
  # output
  return(model.plot)
}

# slope-level (x slope, y level)
plot.slope.level <- function(model, 
                             gender.str = NA, 
                             waves,
                             x.formal, x.growth.type,
                             y.formal) {
  
  # gender notation
  gender.index <- ifelse(gender.str == 'female', 1,
                         ifelse(gender.str == 'male', 2, NA))
  
  # construct notation
  model.notation <- deparse(substitute(model))
  x <- strsplit(model.notation, '\\.')[[1]][1]
  y <- strsplit(model.notation, '\\.')[[1]][2]
  
  ## x growth factor notations
  x.i <- paste0(x, '.i')
  
  if (x.growth.type == '.s') {
    x.s <- paste0(x, '.s')
  }
  
  if (x.growth.type == '.q') {
    x.s <- paste0(x, '.s')
    x.q <- paste0(x, '.q')
  }
  
  if (x.growth.type == '.l') {
    x.l <- paste0(x, '.l')
  }
  
  # y growth factor notations
  y.i <- paste0(y, '.i')
  
  # extract latent variable scores
  if (is.na(gender.str)) { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv')) 
  } else { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv')[[gender.index]]) 
  }
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # save original row parameters
  rows <- nrow(model.data)
  model.data$row <- seq_len(rows)
  
  # make long to match timepoints
  model.data <- model.data[rep(1:rows, each = length(time)), ]
  
  # add time column
  model.data$time <- rep(time, times = rows)
  model.data$time.scaled <- model.data$time  * 4
  
  ## create plot data
  if (x.growth.type == '.s') {
    model.data$predicted <- 
      model.data[[x.i]] +
      model.data[[x.s]] * time
  }
  
  if (x.growth.type == '.q') {
    model.data$predicted <- 
      model.data[[x.i]] +
      model.data[[x.s]] * time +
      model.data[[x.q]] * time^2
  }
  
  if (x.growth.type == '.l') {
    
    # grab parameters
    model.params <- parameterEstimates(model)
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      x.loadings <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        
        if (is.na(gender.str)) {
          x.loadings[i] <- model.params$est[model.params$lhs == paste0(x, '.l') & 
                                              model.params$rhs == paste0(x, '.', i.scaled)]
        } else {
          x.loadings[i] <- model.params$est[model.params$lhs == paste0(x, '.l') & 
                                              model.params$rhs == paste0(x, '.', i.scaled) &
                                              model.params$group == gender.index]
        }
      } 
    } else {
      x.loadings <- c()
      
      for (i in waves) {
        if (is.na(gender.str)) {
          x.loadings[i] <- model.params$est[model.params$lhs == paste0(x, '.l') & 
                                              model.params$rhs == paste0(x, '.', i)]
        } else {
          x.loadings[i] <- model.params$est[model.params$lhs == paste0(x, '.l') & 
                                              model.params$rhs == paste0(x, '.', i) &
                                              model.params$group == gender.index]
        }
      }
    }
    
    # trajectory function
    model.data$predicted <- 
      model.data[[x.i]] + 
      model.data[[x.l]] * x.loadings
  }
  
  # compute tertile cut-offs
  tertiles <- quantile(model.data[[y.i]],
                       probs = c(1/3, 2/3),
                       na.rm = TRUE)
  
  # create tertiles
  model.data <- model.data %>%
    mutate(y.group = case_when(
      .data[[y.i]] > tertiles[2] ~ 'Top 1/3',
      .data[[y.i]] > tertiles[1] & .data[[y.i]] <= tertiles[2] ~ 'Middle 1/3',
      .data[[y.i]] <= tertiles[1] ~ 'Bottom 1/3'
    )) %>% 
    filter(!is.na(y.group))
  
  # aggregate by tertile
  model.data.agg <- model.data %>%
    group_by(y.group, time.scaled) %>%
    summarize(predicted = mean(predicted, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(y.group = factor(y.group, levels = c('Top 1/3', 'Middle 1/3', 'Bottom 1/3')))
  
  # gender label (if applicable)
  gender.label <- if(is.na(gender.str)) {
    ''
  } else {
    paste0(' (', toTitleCase(gender.str), ')')
  }
  
  ## growth titles
  y.title <- paste('Initial Level of', y.formal)
  
  if (x.growth.type == '.s') {
    x.title <- paste('Linear Slope of', x.formal)
  }
  
  if (x.growth.type == '.q') {
    x.title <- paste('Quadratic Slope of', x.formal)
  }
  
  if (x.growth.type == '.l') {
    x.title <- paste('Latent Basis Slope of', x.formal)
  }
  
  # custom title
  title <- paste('Association Between', y.title, 'and', x.title, gender.label)
  
  # create plot
  model.plot <- 
    ggplot(model.data, 
           aes(x = time.scaled,
               y = predicted,
               group = row)) +
    geom_line(aes(color = y.group,
                  group = row),
              size = 0.25, 
              alpha = 0.05) +
    geom_line(data = model.data.agg, 
              aes(x = time.scaled, 
                  y = predicted, 
                  color = y.group, 
                  group = y.group),
              size = 1.2) +
    scale_color_manual(
      values = c('Top 1/3' = '#0072B2', 
                 'Middle 1/3' = '#009E73', 
                 'Bottom 1/3' = '#D55E00')) +
    labs(title = title,
         x = 'Time (Years)',
         y = x.title,
         color = paste('Level of', y.formal,'\n(T1)')) +
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic() + 
    theme(legend.position = "right")
  
  # output
  return(model.plot)
}

# regression plot
# x goal level predicting y trait level
plot.goal.predicting.level <- function(model, gender.str = NA, waves,
                                       x.formal, y.formal) {
  
  # gender notation
  gender.index <- ifelse(gender.str == 'female', 1,
                         ifelse(gender.str == 'male', 2, NA))
  
  # construct notation
  model.notation <- deparse(substitute(model))
  x <- strsplit(model.notation, '\\.')[[1]][1] # e.g., libe
  y <- strsplit(model.notation, '\\.')[[1]][2] # e.g., extr
  
  # growth factor notation
  y.growth <- paste0(y, '.i') # e.g., extr.i
  
  # extract latent variable scores
  if (is.na(gender.str)) { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE))
  } else { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE)[[gender.index]])
  }
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # grab correlation
  model.params <- parameterEstimates(model, standardized = TRUE)
  
  if (is.na(gender.str)) {
    corr <- model.params$std.all[model.params$lhs == y.growth &
                                   model.params$op == '~' &
                                   model.params$rhs == x]
  } else {
    corr <- model.params$std.all[model.params$lhs == y.growth &
                                   model.params$op == '~' &
                                   model.params$rhs == x &
                                   model.params$group == gender.index]
  }
  
  # corr label
  corr.label <- paste0('r = ', round(corr, 3))
  
  # gender label (if applicable)
  gender.label <- if(is.na(gender.str)) {
    ''
  } else {
    paste0(' (', toTitleCase(gender.str), ')')
  }
  
  # growth titles
  x.title <- paste('Level of', x.formal)
  y.title <- paste('Level of', y.formal)
  
  # custom title
  title <- paste('Correlation Between', x.title, 'and', y.title, gender.label)
  
  # create plot
  model.plot <-
    ggplot(model.data,
           aes(x = !!sym(x),
               y = !!sym(y.growth))) +
    geom_jitter(width = 0.1, height = 0.05, alpha = 0.5) +
    labs(title = title,
         subtitle = corr.label,
         x = x.title,
         y = y.title) +
    scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
    scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
    theme_classic()
  
  # output
  return(model.plot)
}

# regression plot
# x goal level predicting y trait slope
plot.goal.predicting.slope <- function(model, gender.str = NA, waves,
                                       x.formal, 
                                       y.formal, y.growth.type) {
  
  # gender notation
  gender.index <- ifelse(gender.str == 'female', 1,
                         ifelse(gender.str == 'male', 2, NA))
  
  # construct notation
  model.notation <- deparse(substitute(model))
  x <- strsplit(model.notation, '\\.')[[1]][1] # e.g., libe
  y <- strsplit(model.notation, '\\.')[[1]][2] # e.g., extr
  
  ## y growth factor notation
  y.i <- paste0(y, '.i')
  
  if (y.growth.type == '.s') {
    y.s <- paste0(y, '.s')
  }
  
  if (y.growth.type == '.q') {
    y.s <- paste0(y, '.s')
    y.q <- paste0(y, '.q')
  }
  
  if (y.growth.type == '.l') {
    y.l <- paste0(y, '.l')
  }
  
  # extract latent variable scores
  if (is.na(gender.str)) { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE))
  } else { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE)[[gender.index]])
  }
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # save original row parameters
  rows <- nrow(model.data)
  model.data$row <- seq_len(rows)
  
  # make long to match timepoints
  model.data <- model.data[rep(1:rows, each = length(time)), ]
  
  # add time column
  model.data$time <- rep(time, times = rows)
  model.data$time.scaled <- model.data$time  * 4
  
  ## create plot data
  if (y.growth.type == '.s') {
    model.data$predicted <- 
      model.data[[y.i]] +
      model.data[[y.s]] * time
  }
  
  if (y.growth.type == '.q') {
    model.data$predicted <- 
      model.data[[y.i]] +
      model.data[[y.s]] * time +
      model.data[[y.q]] * time^2
  }
  
  if (y.growth.type == '.l') {
    
    # grab parameters
    model.params <- parameterEstimates(model)
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      y.loadings <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        
        if (is.na(gender.str)) {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i.scaled)]
        } else {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i.scaled) &
                                              model.params$group == gender.index]
        }
      } 
    } else {
      y.loadings <- c()
      
      for (i in waves) {
        if (is.na(gender.str)) {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i)]
        } else {
          
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i) &
                                              model.params$group == gender.index]
        } 
      }
    }
    
    # trajectory function
    model.data$predicted <- 
      model.data[[y.i]] + 
      model.data[[y.l]] * y.loadings
  }
  
  # compute tertile cut-offs
  tertiles <- quantile(model.data[[x]],
                       probs = c(1/3, 2/3),
                       na.rm = TRUE)
  
  # create tertiles
  model.data <- model.data %>%
    mutate(x.group = case_when(
      .data[[x]] >= tertiles[2] ~ 'Top 1/3',
      .data[[x]] >= tertiles[1] & .data[[x]] < tertiles[2] ~ 'Middle 1/3',
      .data[[x]] < tertiles[1] ~ 'Bottom 1/3'
    )) %>% 
    filter(!is.na(x.group))
  
  # aggregate by tertile
  model.data.agg <- model.data %>%
    group_by(x.group, time.scaled) %>%
    summarize(predicted = mean(predicted, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(x.group = factor(x.group, levels = c('Top 1/3', 'Middle 1/3', 'Bottom 1/3')))
  
  # gender label (if applicable)
  gender.label <- if(is.na(gender.str)) {
    ''
  } else {
    paste0(' (', toTitleCase(gender.str), ')')
  }
  
  ## growth titles
  x.title <- paste('Levels of', x.formal)
  
  if (y.growth.type == '.s') {
    y.title <- paste('Linear Slope of', y.formal)
  }
  
  if (y.growth.type == '.q') {
    y.title <- paste('Quadratic Slope of', y.formal)
  }
  
  if (y.growth.type == '.l') {
    y.title <- paste('Latent Basis Slope of', y.formal)
  }
  
  # custom title
  title <- paste('Association Between', x.title, 'and', y.title, gender.label)
  
  # create plot
  model.plot <- 
    ggplot(model.data, 
           aes(x = time.scaled,
               y = predicted,
               group = row)) +
    geom_line(aes(color = x.group,
                  group = row),
              size = 0.25, 
              alpha = 0.05) +
    geom_line(data = model.data.agg, 
              aes(x = time.scaled, 
                  y = predicted, 
                  color = x.group, 
                  group = x.group),
              size = 1.2) +
    scale_color_manual(
      values = c('Top 1/3' = '#0072B2', 
                 'Middle 1/3' = '#009E73', 
                 'Bottom 1/3' = '#D55E00')) +
    labs(title = title,
         x = 'Time (Years)',
         y = y.title,
         color = paste('Level of', x.formal,'\n(T3/T4)')) +
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic() + 
    coord_cartesian(clip = 'off')
  
  # output
  return(model.plot)
}

# regression plot
# x goal level predicted by y trait level
plot.goal.predicted.level <- function(model, gender.str = NA, waves,
                                      x.formal, y.formal) {
  
  # gender notation
  group.labels <- inspect(model, 'group.label')
  gender.index <- match(gender.str, group.labels)
  
  # construct notation
  model.notation <- deparse(substitute(model))
  x <- strsplit(model.notation, '\\.')[[1]][2] # e.g., agre
  y <- strsplit(model.notation, '\\.')[[1]][1] # e.g., conf
  
  # growth factor notation
  y.growth <- paste0(y, '.i') # e.g., agre.i
  
  # extract latent variable scores
  if (is.na(gender.str)) { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE))
  } else { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE)[[gender.index]])
  }
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # grab correlation
  model.params <- parameterEstimates(model, standardized = TRUE)
  
  if (is.na(gender.str)) {
    corr <- model.params$std.all[model.params$lhs == x &
                                   model.params$op == '~' &
                                   model.params$rhs == y.growth]
  } else {
    corr <- model.params$std.all[model.params$lhs == x &
                                   model.params$op  == "~" &
                                   model.params$rhs == y.growth &
                                   model.params$group == gender.index]
  }
  
  # corr label
  corr.label <- paste0('r = ', round(corr, 3))
  
  # gender label (if applicable)
  gender.label <- if(is.na(gender.str)) {
    ''
  } else {
    paste0(' (', toTitleCase(gender.str), ')')
  }
  
  # growth titles
  x.title <- paste('Level of', x.formal)
  y.title <- paste('Level of', y.formal)
  
  # custom title
  title <- paste('Correlation Between', x.title, 'and', y.title, gender.label)
  
  # create plot
  model.plot <-
    ggplot(model.data,
           aes(x = !!sym(x),
               y = !!sym(y.growth))) +
    geom_jitter(width = 0.1, height = 0.05, alpha = 0.5) +
    labs(title = title,
         subtitle = corr.label,
         x = x.title,
         y = y.title) +
    theme_classic()
  
  # output
  return(model.plot)
}

# regression plot
# x goal level predicted by y trait slope
plot.goal.predicted.slope <- function(model, 
                                      gender.str = NA, 
                                      waves,
                                      x.formal, 
                                      y.formal, y.growth.type) {
  
  # gender notation
  gender.index <- ifelse(gender.str == 'female', 1,
                         ifelse(gender.str == 'male', 2, NA))
  
  # construct notation
  model.notation <- deparse(substitute(model))
  x <- strsplit(model.notation, '\\.')[[1]][2] # e.g., extr
  y <- strsplit(model.notation, '\\.')[[1]][1] # e.g., libe
  
  ## y growth factor notation
  y.i <- paste0(y, '.i')
  
  if (y.growth.type == '.s') {
    y.s <- paste0(y, '.s')
  }
  
  if (y.growth.type == '.q') {
    y.s <- paste0(y, '.s')
    y.q <- paste0(y, '.q')
  }
  
  if (y.growth.type == '.l') {
    y.l <- paste0(y, '.l')
  }
  
  # extract latent variable scores
  if (is.na(gender.str)) { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE))
  } else { 
    model.data <- as.data.frame(lavPredict(model, type = 'lv', append.data = TRUE)[[gender.index]])
  }
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # save original row parameters
  rows <- nrow(model.data)
  model.data$row <- seq_len(rows)
  
  # make long to match timepoints
  model.data <- model.data[rep(1:rows, each = length(time)), ]
  
  # add time column
  model.data$time <- rep(time, times = rows)
  model.data$time.scaled <- model.data$time  * 4
  
  ## create plot data
  if (y.growth.type == '.s') {
    model.data$predicted <-
      model.data[[y.i]] +
      model.data[[y.s]] * time
  }
  
  if (y.growth.type == '.q') {
    model.data$predicted <- 
      model.data[[y.i]] +
      model.data[[y.s]] * time +
      model.data[[y.q]] * time^2
  }
  
  if (y.growth.type == '.l') {
    
    # grab parameters
    model.params <- parameterEstimates(model)
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      y.loadings <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        
        if (is.na(gender.str)) {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i.scaled)]
        } else {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i.scaled) &
                                              model.params$group == gender.index]
        }
      } 
    } else {
      y.loadings <- c()
      
      for (i in waves) {
        if (is.na(gender.str)) {
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i)]
        } else {
          
          y.loadings[i] <- model.params$est[model.params$lhs == paste0(y, '.l') & 
                                              model.params$rhs == paste0(y, '.', i) &
                                              model.params$group == gender.index]
        } 
      }
    }
    
    # trajectory function
    model.data$predicted <- 
      model.data[[y.i]] + 
      model.data[[y.l]] * y.loadings
  }
  
  # compute tertile cut-offs
  tertiles <- quantile(model.data[[x]],
                       probs = c(1/3, 2/3),
                       na.rm = TRUE)
  
  # create tertiles
  model.data <- model.data %>%
    mutate(x.group = case_when(
      .data[[x]] >= tertiles[2] ~ 'Top 1/3',
      .data[[x]] >= tertiles[1] & .data[[x]] < tertiles[2] ~ 'Middle 1/3',
      .data[[x]] < tertiles[1] ~ 'Bottom 1/3'
    )) %>% 
    filter(!is.na(x.group))
  
  # aggregate by tertile
  model.data.agg <- model.data %>%
    group_by(x.group, time.scaled) %>%
    summarize(predicted = mean(predicted, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(x.group = factor(x.group, levels = c('Top 1/3', 'Middle 1/3', 'Bottom 1/3')))
  
  # gender label (if applicable)
  gender.label <- if(is.na(gender.str)) {
    ''
  } else {
    paste0(' (', toTitleCase(gender.str), ')')
  }
  
  ## growth titles
  x.title <- paste('Levels of', x.formal)
  
  if (y.growth.type == '.s') {
    y.title <- paste('Linear Slope of', y.formal)
  }
  
  if (y.growth.type == '.q') {
    y.title <- paste('Quadratic Slope of', y.formal)
  }
  
  if (y.growth.type == '.l') {
    y.title <- paste('Latent Basis Slope of', y.formal)
  }
  
  # custom title
  title <- paste('Association Between', x.title, 'and', y.title, gender.label)
  
  # create plot
  model.plot <- 
    ggplot(model.data, 
           aes(x = time.scaled,
               y = predicted,
               group = row)) +
    geom_line(aes(color = x.group,
                  group = row),
              size = 0.25, 
              alpha = 0.05) +
    geom_line(data = model.data.agg, 
              aes(x = time.scaled, 
                  y = predicted, 
                  color = x.group, 
                  group = x.group),
              size = 1.2) +
    scale_color_manual(
      values = c('Top 1/3' = '#0072B2', 
                 'Middle 1/3' = '#009E73', 
                 'Bottom 1/3' = '#D55E00')) +
    labs(title = title,
         x = 'Time (Years)',
         y = y.title,
         color = paste('Level of', x.formal,'\n(T3/T4)')) +
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic() + 
    coord_cartesian(clip = 'off')
  
  # output
  return(model.plot)
}

# average trajectory (by gender)
plot.average.gender <- function(model, waves, gender.str) {
  
  # model notation
  model.notation <- deparse(substitute(model))
  waves.notation <- deparse(substitute(waves))
  var <- strsplit(model.notation, '\\.')[[1]][1]
  growth <- univariate[[waves.notation]][univariate$var == var]
  
  # gender notation
  gender <- ifelse(gender.str == 'female', 1, 2)
  
  # model parameters
  model.params <- parameterEstimates(model)
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # for linear models
  if (growth == 'linear') {
    
    # extract latent means
    model.i <- model.params$est[model.params$lhs == paste0(var, '.i') &
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    model.s <- model.params$est[model.params$lhs == paste0(var, '.s') &
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    
    # trajectory function
    model.func <- model.i + model.s*time
  }
  
  # for quadratic models
  if (growth == 'quadra') {
    
    # extract latent means
    model.i <- model.params$est[model.params$lhs == paste0(var, '.i') & 
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    model.s <- model.params$est[model.params$lhs == paste0(var, '.s') & 
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    model.q <- model.params$est[model.params$lhs == paste0(var, '.q') & 
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    
    # trajectory function
    model.func <- model.i + model.s*time + model.q*time^2
  }
  
  # for latent basis models
  if (growth == 'latent') {
    
    # extract latent means
    model.i <- model.params$est[model.params$lhs == paste0(var, '.i') & 
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    model.l <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                  model.params$op == '~1' &
                                  model.params$group == gender]
    
    # extract freely-estimated loadings
    if (length(waves) == 5) {
      model.f <- numeric(5)
      
      for (i in 1:5) {
        i.scaled <- i + 3
        model.f[i] <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                         model.params$rhs == paste0(var, '.', i.scaled) &
                                         model.params$group == gender]
      } 
    } else {
      model.f <- c()
      
      for (i in waves) {
        model.f[i] <- model.params$est[model.params$lhs == paste0(var, '.l') & 
                                         model.params$rhs == paste0(var, '.', i) &
                                         model.params$group == gender]
      }
    }
    
    # trajectory function
    model.func <- model.i + model.l*model.f
  }
  
  # create plot data
  model.data <- data.frame(time = time,
                           predicted = model.func)
  
  # assign global variable
  assign(paste0(var, '.', gender.str, '.average.data'), 
         model.data, 
         envir = .GlobalEnv)
  
  # grab variable attributes
  var.fullname <- get.var.fullname(var)
  var.color <- get.var.color(var)
  
  # grab sub/title
  title <- paste0(plot.title(growth, var.fullname), 
                  " (", toTitleCase(gender.str), ")")
  subtitle <- plot.subtitle(waves)
  
  # create plot
  model.plot <- 
    ggplot(model.data, aes(x = time.scaled, y = predicted)) +
    geom_line(color = var.color, size = 1) +
    geom_point(color = var.color, size = 2) +
    labs(title = title, subtitle = subtitle,
         x = 'Time (Years)', y = var.fullname) + 
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    theme_classic()
  
  # output
  return(model.plot)
}

# combined (male + female)
plot.combined.gender <- function(model, 
                                 male.average.data, 
                                 female.average.data, 
                                 waves) {
  
  # model notation
  model.notation <- deparse(substitute(model))
  waves.notation <- deparse(substitute(waves))
  var <- strsplit(model.notation, '\\.')[[1]][1]
  growth <- univariate[[waves.notation]][univariate$var == var]
  
  # time vector (and re-scaled for plot)
  time <- get.linear.loadings(waves)
  time.scaled <- time * 4
  
  # total waves in years
  total.years <- tail(time.scaled, 1)
  
  # grab variable attributes
  var.fullname <- get.var.fullname(var)
  
  # grab sub/title
  title <- paste0(var.fullname)
  subtitle <- plot.subtitle(waves)
  
  # create plot
  combined.plot <- ggplot() +
    geom_line(data = male.average.data, 
              aes(x = time.scaled, y = predicted, color = 'Male'),
              size = 1) +
    geom_point(data = male.average.data, 
               aes(x = time.scaled, y = predicted, color = 'Male'),
               size = 2) +
    geom_line(data = female.average.data, 
              aes(x = time.scaled, y = predicted, color = 'Female'),
              size = 1) +
    geom_point(data = female.average.data, 
               aes(x = time.scaled, y = predicted, color = 'Female'),
               size = 2) +
    labs(title = title, subtitle = subtitle,
         x = 'Time (Years)', y = var.fullname, color = 'Gender') + 
    scale_x_continuous(limits = c(0, total.years),
                       breaks = seq(0, total.years, 2)) +
    scale_y_continuous(limits = c(1, 4)) +
    scale_color_manual(values = c('Male' = '#006cc4', 'Female' = '#74209e')) +
    theme_classic()
  
  # output
  return(combined.plot)
}

# end of code ##################################################################