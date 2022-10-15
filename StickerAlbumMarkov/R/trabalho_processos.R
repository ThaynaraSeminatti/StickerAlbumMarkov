####funçao cadeia matriz de transiçao####

#### função para descobrir:
#n estimado de figurinhas para completar o album
#preço
#confere a probabilidade de acordo com a cadeia de markov
markov_prob_album<-
  function(n,  prob, valor_unit){
    ### n = numero de figurinhas no album desejado
    ### prob = probabilidade acerca do numero certo de figuinhas
    ### valor = valor de cada figurinha
  matrix_P<- matrix(data = rep(0,(n+1)^2),nrow = (n+1),ncol = (n+1))
  for(i in 1:(ncol(matrix_P)-1)){
    matrix_P[i,i+1]<-1
    matrix_P[1,2]<-1
    matrix_P[1,1]<-0
    matrix_P[i,i]<-((i-1)/n)##ok
    matrix_P[i,i+1]<-(n-(i-1))/n
    matrix_P[nrow(matrix_P),ncol(matrix_P)]<-1
  }
  #### estimativa de numero necessário para completar album
  alpha = 1-prob
  c = -log(alpha)
  probability = (n*log(n))+(n*c)
  n_estimado = as.integer(probability)
  
  #### aplicando markov
  lista_matrix <- list()
  x_0 = matrix(c(1,rep(0,n)),nrow = 1)
  lista_matrix[[1]]<-x_0%*%matrix_P
  
  
  for (j in 1:(n_estimado)) {
    lista_matrix[[1]]<-x_0%*%matrix_P
    lista_matrix[[j+1]]<-lista_matrix[[j]]%*%matrix_P
   }
  
  #### preço da figurinha 
  preco = paste('R$: ',round(valor_unit*n_estimado, 2), sep ='')
  
  return(list('nº estimado' = n_estimado, 'conferencia com a cadeia de markov' = lista_matrix[[n_estimado]][1,(n+1)],
              'Valor gasto para completar o album' = preco))
  }


#markov_prob_album(n = 670,prob = 0.99, valor_unit = 0.83)

