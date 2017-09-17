#Entropy-based Autopoiesis
autopoiesis <- function(controller.pmf, 
                        environ.pmf, 
                        c.pmf.no_states = NA, 
                        e.pmf.no_states = NA)
{
  c.cplxity <- complexity.discrete(controller.pmf, no_states = c.pmf.no_states)
  e.cplxity <- complexity.discrete(environ.pmf, no_states = e.pmf.no_states)
  
  a <- c.cplxity$C/e.cplxity$C
  
  res(a)
}

#Shannon's Entropy Function
Entropy <- function(margSttProb = 1, nonNAidxes = NA)
{
  h <- ifelse(test = any(is.na(nonNAidxes)),
              yes = -1*sum(margSttProb[nonNAidxes]*log2(margSttProb[nonNAidxes])),
              no = -1*sum(margSttProb[nonNAidxes]*log2(margSttProb[nonNAidxes]))
              )
  
  return(h)
}

#This function calculates Discrete Complexity Measures for discrete samples.
complexity.discrete <- function( pmfSample = rnorm(100000, mean = 0, sd = 1), 
                                 no_states = NA )
{
  #First, we get the number of observations contained in the sample.
  pmf.Len  <-ifelse(test = is.matrix(pmfSample)|is.data.frame(pmfSample), 
                    yes = dim(pmfSample)[1], 
                    no = length(pmfSample))
  
  #If the analyzed sample is trivial (i.e. only one value)
  #assign default values
  if(length(unique(pmfSample))==1)
  {
    resFit  <- data.frame(E = 0, 
                          S = 1, 
                          C = 0,
                          Entrop = 0)
    return(resFit)
  }
  
  #If the number of states of the PMF
  #is known beforehand
  if(!is.na(no_states))
  {
    #Calculate the marginal states probability
    #If is a matrix or a data.frame
    if(is.matrix(pmfSample)|is.data.frame(pmfSample))
    {
      ncol        <- dim(pmfSample)[2]
      if(is.numeric(pmfSample))
      {
        margSttProb <- sapply(1:ncol, 
                              function(i) table(cut(pmfSample[,i], 
                                                    breaks = no_states))/pmf.Len)
      }else
      {
        margSttProb <- sapply(1:ncol, 
                              function(i) table(pmfSample[,i])/pmf.Len)
      }
    }else if(is.table(pmfSample)) #Else if pmfSample is a table
    {
      no_states   <- length(pmfSample)
      margSttProb <- pmfSample/sum(pmfSample)
    }
    else #Else pmfSample is a vector
    {
      if(is.numeric(pmfSample))
      {
        margSttProb <- table(cut(pmfSample, breaks = no_states))/pmf.Len
      }else
      {
        margSttProb <- table(pmfSample)/pmf.Len
      }
    }
  }
  else 
  {
    #Use an heuristic to obtain the PMF
    #Obtain the system's unique states,
    #Get the length of the unique states, and,
    #Calculate the marginal states probability
    if(is.matrix(pmfSample)|is.data.frame(pmfSample))
    {
      ncol        <- dim(pmfSample)[2]
      no_states   <- sapply(1:ncol, 
                            function(i) length(unique(pmfSample[,i])))
      if(is.numeric(pmfSample))
      {
        margSttProb <- sapply(1:ncol, 
                              function(i) table(cut(pmfSample[,i], 
                                                    breaks = no_states[i]))/pmf.Len)
      }else
      {
        margSttProb <- sapply(1:ncol, 
                              function(i) table(pmfSample[,i])/pmf.Len)
      }
      
    }else if(is.table(pmfSample)) #Else if pmfSample is a table
    {
      no_states   <- length(pmfSample)
      margSttProb <- pmfSample/sum(pmfSample)
    }
    else #Else pmfSample is a vector
    {
      if(is.numeric(pmfSample))
      {
        no_states   <- length(unique(pmfSample))
        margSttProb <- table(cut(pmfSample, breaks = no_states))/pmf.Len
      }else
      {
        no_states   <- length(unique(pmfSample))
        margSttProb <- table(pmfSample)/length(pmfSample)
      }
    }
  }
  
  #Then, calculate entropy for all elements 
  #of the PMF with p(x)>0
  if(is.matrix(pmfSample)|is.data.frame(pmfSample))
  {
    idxes   <- sapply(1:length(margSttProb), function(i) which(margSttProb[[i]]> 0))
    entrop  <- sapply(1:length(idxes), function(i) Entropy(margSttProb[[i]], idxes[[i]]))
  }else
  {
    idxes   <- which(margSttProb > 0)
    entrop  <- Entropy(margSttProb, idxes)
    extrop  <- Entropy(1-margSttProb, idxes)
  }

  #Calculate ESC measures
  #Define the normalizing constant k
  kConst            <- 1/log2(no_states)
  emergence         <- kConst*entrop
  selfOrganization  <- 1 - emergence
  complexity        <- 4 * emergence * selfOrganization
    
  resFit  <- data.frame(E = emergence, 
                        S = selfOrganization, 
                        C = complexity,
                        Entrop = entrop)
  
  if(is.matrix(pmfSample)|is.data.frame(pmfSample))
  {
    rownames(resFit) <- colnames(pmfSample)
  }
  
  return(resFit)
}

