
#A 3D Barplot for displaying E,S, and C
#Its parameters are
#   1. data.df, is a data.frame where columns are E, S, and C
#   or different experiments for the same entropy-based measure;
#   rows correspond to different samples used.
#   2. p.title is a string for the title of the barplot.
#   3. axes.ls is a list containing labels for the different axes of the plot.
barplot3D <- function(data.df = data.frame(NA),
                      p.title = 'Title',
                      axes.ls = list(x.lab = 'Var 1', y.lab = 'Var 2', z.lab = 'Var 3')) 
{
  p.rows <- dim(data.df)[2]
  p.cols <- dim(data.df)[1]
  
  if(is.null(rownames(data.df)))
  {
    tmp.col.labels <- paste('V',seq(1:p.cols), sep='')
  }
  else
  {
    tmp.col.labels <- rownames(data.df)
  }
  
  if(is.null(colnames(data.df)))
  {
    tmp.row.labels <- paste('W',seq(1:p.rows), sep='')
  }
  else
  {
    tmp.row.labels <- colnames(data.df)
  }
  
  # Parameters of the CUBOID
  face.offs   <- 8
  c.btw.offs  <- 1
  
  col.x     <- 1
  col.y     <- 1
  col.z     <- 1
  
  x.offset  <- 1
  y.offset  <- 1
  
  # --------
  
  tmpl.x <- c(0, 0, col.x, col.x, 0, 0, col.x, col.x)+x.offset
  tmpl.y <- c(0, col.y, col.y, 0, 0, col.y, col.y, 0)+y.offset
  tmpl.z <- c(0, 0, 0, 0, col.z, col.z, col.z, col.z)
  
  tmpl.i <- c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2)
  tmpl.j <- c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3)
  tmpl.k <- c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6)
  
  x.init <- col.x + c.btw.offs
  y.init <- col.y + c.btw.offs
  
  x.df   <- as.data.frame(matrix(0, nrow = length(tmpl.x)*p.cols, ncol = p.rows))
  y.df   <- as.data.frame(matrix(0, nrow = length(tmpl.x)*p.cols, ncol = p.rows))
  z.df   <- as.data.frame(matrix(0, nrow = length(tmpl.x)*p.cols, ncol = p.rows))
  
  colnames(x.df) <- paste('x',seq(1:p.rows), sep='')
  colnames(y.df) <- paste('y',seq(1:p.rows), sep='')
  colnames(z.df) <- paste('z',seq(1:p.rows), sep='')
  
  for(h in 1:p.rows)
  {
    x.df[,h] <- c(tmpl.x, 
                  unlist(lapply(seq(x.init,(x.init*(p.cols-1)),by = x.init), 
                                function(i) tmpl.x+i)))
    y.df[,h] <- rep(tmpl.y+(y.init*(h-1)), times = p.cols)
    z.df[,h] <- unlist(lapply(1:dim(data.df)[1], function(k) tmpl.z*data.df[k,h]))
  }
  
  p.x.legends <- rep(rep(tmp.col.labels, times = 1, each = face.offs), times = 3)
  p.y.legends <- rep(tmp.row.labels, times = 1, each = dim(y.df)[1])
  p.z.legends <- as.character(unlist(round(z.df, digits = 2)))
  
  tmp.annotations <- lapply(1:length(p.x.legends), 
                            function(i) list(
                              x = p.x.legends[i],
                              y = p.y.legends[i],
                              z = p.z.legends[i]
                            ))
  
  poly.df <- as.data.frame(matrix(0, nrow = length(tmpl.i)*p.cols*p.rows, ncol = p.rows))
  colnames(poly.df) <- c('i','j','k')
  
  poly.df$i <- c(tmpl.i,
                 unlist(lapply(1:(p.cols*p.rows-1), function(i) tmpl.i + i*face.offs)))
  poly.df$j <- c(tmpl.j,
                 unlist(lapply(1:(p.cols*p.rows-1), function(i) tmpl.j + i*face.offs)))
  poly.df$k <- c(tmpl.k,
                 unlist(lapply(1:(p.cols*p.rows-1), function(i) tmpl.k + i*face.offs)))
  
  tmp.color <- c(0,0,0,0,0,0,0,0)
  tmp.color <- unlist(lapply(1:p.rows, function(k) rep(data.df[,k], times = 1, each = face.offs)))
  
  #--------
  
  tmp.offs <- (x.offset+x.offset+col.x)/2
  
  axx <- list(title = axes.ls$x.lab,
              titlefont = list(family = 'Droid Serif', size = 14, color = 'black'),
              tickfont = list(size=ifelse(p.cols>15, 11, 12)),
              tickangle = -90,
              tickvals = seq(tmp.offs,(x.offset+col.x)*p.cols,by = x.init), 
              ticktext = tmp.col.labels, 
              nticks = p.cols,
              gridcolor = 'black', #gridwidth = 1,
              range = c(0, (x.offset+col.x)*p.cols))
  
  tmp.offs <- (y.offset+y.offset+col.y)/2
  
  axy <- list(title = axes.ls$y.lab,
              titlefont = list(family = 'Droid Serif', size = 14, color = 'black'),
              tickfont = list(size=ifelse(p.cols>15, 11, 12)),
              tickvals = seq(tmp.offs,(y.offset+col.y)*p.rows,by = y.init), 
              ticktext = tmp.row.labels, 
              nticks = p.rows,  
              range = c(0,(y.offset+col.y)*p.rows))
  
  axz <- list(title = axes.ls$z.lab,
              nticks = 11,
              range = c(0,col.z+0.1))
  
  m <- list(l=150, r=20, b=10, t=10)
  
  p <- plot_ly(name = p.title,
               type = 'mesh3d',
               x = unlist(x.df),
               y = unlist(y.df), 
               z = unlist(z.df), 
               i = poly.df$i,  
               j = poly.df$j, 
               k = poly.df$k,
               # customdata = list(mocos = 'demonios'),
               intensity = tmp.color,
               color = tmp.color,
               vertexcolor = 'black',
               flatshading = T,
               colors = c('red', 'orange', 'yellow', 'green2', 'dodgerblue', 'blue'),
               hoverinfo = 'z+name+customdata', hoverlabel = list(bgcolor = 'red'),
               cauto =F, cmin = 0, cmax = 1,
               colorbar = list(title = p.title,
                               xpad = 15,
                               titleside = 'bottom')
  ) %>%
    layout(scene = list( aspectratio = list(x = 3,y = 2,z = 1),
                         xaxis=axx, 
                         yaxis=axy, 
                         zaxis=axz, 
                         camera = list(center = list(x = 0,y = 0,z = 0),
                                       eye = list(x = 4.96903462608,
                                                  y = 2.09022831971,
                                                  z = 2.405345349304)
                         )))
  
  return(p)
}

# 2D Simple Complexity plot
#Its parameters are
#   1. data.df,  is a data.frame where columns correspond to the experiments performed/ESC measures + 2
#   the two extra columns are named by default'labs' and 'ids': the first will be the name used 
#   in the x-axis as ticks labels, whereas the id correspond to a sequence from 1 to the maximum number
#   of experiments which will be used to order the experiments in the plot;
#   rows correspond to different samples used.
#   2. id.vars and xlab are the strings that will indicate how the two additional columns used for 
#   identification and ordering are named.
#   3. cplox.type accept two types of plot 
#   'ggplot' will display a static plot drawn using the ggplot package,
#   'plotly' will display an interactive plot drawn using the plotly package.
simple.cplox <- function(data.df = data.frame(x=0, y=0), 
                         id.vars = 'X', xlab = 'X', cplox.type = 'ggplot')
{
  if(cplox.type == 'ggplot')
  {
    ggg<- melt(data.df, id.vars = id.vars)
    
    fig <- ggplot(ggg,aes(x = ids, y = value, col = variable))+
      geom_point(size = 2)+
      geom_line()+
      scale_x_continuous(name="Maneuvers", 
                         limits=c(1,dim(data.df)[1]), 
                         breaks=seq(1,dim(data.df)[1],by=1), 
                         labels = data.df$labs) +
      scale_y_continuous(name=xlab, 
                         limits=c(0, 1), 
                         breaks=seq(0,1,by=0.05))  
  }else if(cplox.type == 'plotly')
  {
    ggg<- melt(data.df, id.vars = id.vars)
    
    fig <- ggplot(ggg,aes(x = ids, y = value, col = variable))+
      geom_point(size = 2)+
      geom_line()+
      scale_x_continuous(name="Maneuvers", 
                         limits=c(1,dim(data.df)[1]), 
                         breaks=seq(1,dim(data.df)[1],by=1), 
                         labels = data.df$labs) +
      scale_y_continuous(name=xlab, 
                         limits=c(0, 1), 
                         breaks=seq(0,1,by=0.05))
    fig <- ggplotly(fig)
  }
  
  return(fig)
}

# 2D Complexity Space
#Its parameters are
#   1. data.df,  is a data.frame where columns correspond to Environment C +  Controller C+ group
#   Environmental Complexity can consider only 1 or several more.
#   For the controller C there can only be one column. This column must be named 'ctrl'.
#   column 'group' is used to distinguish between different experimental setups.
#   rows correspond to different complexities obtained for different groups.
#   2. id.vars is a vector with two strings c('ctrl', 'group') 
#   These strings are used to identify the corresponding columns of the Controller complexity
#   and the different groups/experimental configurations tested.
#   3. envir.str is a string that will be used on the x-axis to display the Controlled name.
#   4. cplx.var1 is a string of the entropy-based measure tested for the environment used on the y-axis.
#   5. ctrl.str is the string that will be used on the y-axis to display the Controllers variable name
#   6. cplx.var2 is the string of the entropy-based measure tested for the controller
#   7. cplox.type accept two types of plot 
#   'ggplot' will display a static plot drawn using the ggplot package,
#   'plotly' will display an interactive plot drawn using the plotly package.
complexity.space <- function(tmp.df = data.frame(), 
                             id.vars = 'x1',
                             envir.str = 'Environ', cplx.var1 = 'E',
                             ctrl.str = 'Cntrl', cplx.var2 = 'E',
                             cplox.type = 'ggplot')
{
  # tmp.df[,5] <- as.factor(tmp.df[,5])
  mlt.df <- melt(tmp.df, id.vars = id.vars)
  
  env.str <- paste('Environment ', cplx.var1, ' (',envir.str , ')', collapse = '', sep='')
  ctrl.str<- paste('Controller ', cplx.var2, ' (',ctrl.str , ')', collapse = '', sep='')
  
  fig <- ggplot(mlt.df,aes(x = value, 
                           y = ctrl, 
                           color = as.factor(group), 
                           shape = variable, group = variable))
  fig <- fig + geom_point(size = 4)
  fig <- fig + scale_x_continuous(name=env.str, 
                                  limits=c(0, 1),  expand = c(0, 0),
                                  breaks=seq(0,1,by=0.05))
  fig <- fig + scale_y_continuous(name=ctrl.str, 
                                  limits=c(0, 1),  expand = c(0, 0),
                                  breaks=seq(0,1,by=0.05))
  fig <- fig + geom_abline(color = 'red', size = 1, linetype = 'dashed' ,
                           intercept = 0, slope = 1)
  fig <- fig + coord_fixed()
  fig <- fig + theme_bw()
  fig <- fig + guides(color=guide_legend('Group'), 
                      shape =guide_legend('Level'))
  if(cplox.type == 'plotly')
  {
    fig <- ggplotly(fig)
  }
  
  return(fig)
}