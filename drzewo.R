selekcja <- function(baza,N) {
  
  
  library("caret", lib.loc="E:/Pakiet R/install/R-3.2.4revised/library")
  library("rpart", lib.loc="E:/Pakiet R/install/R-3.2.4revised/library")
  library("rpart.plot", lib.loc="E:/Pakiet R/install/R-3.2.4revised/library")
  library("Rcpp", lib.loc="E:/Pakiet R/install/R-3.2.4revised/library")
  library("RoughSets", lib.loc="E:/Pakiet R/install/R-3.2.4revised/library")
  
  
test1 <- NULL
y <- NULL
TabelaSredniaIloscReduktow <- NULL
indeksReduktow <- 1
indeksSredniejilosciReduktow <- 0


tabelaWyjsciowa <- data.frame("Numer podzia³u"=integer(),
                              "Liczba reduktow"=integer(),
                              "Liczba Atrybutow Przed"=integer(),
                              "Liczba Atrybutow Po"=double(),
                              "ACC Drzewa"=double(),
                              "ACC Poddrzew"= double(),
                              "Czy redukt lepszy ?"=character(),
                              stringsAsFactors = FALSE)



gp <- runif(nrow(baza))
Baza <- baza[order(gp),]
  
  for(j in 0:(N-1)){
    indeks <-1
    for (i in 1:dim(Baza)[1]){
      if (i %% N == j ){
        test1[indeks] <- i
        indeks <- indeks+1
      }
    }
    szerokoscTablicy <- dim(Baza)[2]
    print("###############################################################")

    test <- Baza[test1,]
    trening <- Baza[-test1,]
    print(t)
    nazwaDecyzji <- paste0((colnames(Baza)[szerokoscTablicy]),"~.")
    traintree <- rpart(nazwaDecyzji,trening,method = "class")
    
    #rysowanie wykresu wyuczonego drzewa
    rpart.plot(traintree,type = 4,extra = 101)
    
    #przewidywanie wyniku naszego algorytmu
    p <- predict(traintree, test, type = "class")
    
    #wyliczanie macierzy pomy³ek oraz wyœwietlenie wyników
    decisionAttribut <- (szerokoscTablicy)
    
    o <- confusionMatrix(p,test[,decisionAttribut])
    print(o)
    DokladnoscDrzewa <- o$overall[1]
    y[indeksReduktow] <- DokladnoscDrzewa
    indeksReduktow <- (indeksReduktow + 1)

    
    #przygotowanie bazy do wyznaczenia reduktów
    decisiontable <- SF.asDecisionTable(dataset = trening, 
                                        decision.attr = szerokoscTablicy, indx.nominal = szerokoscTablicy)
    #Budowanie relacyjnej tablicy decyzyjnej
    tmp <- BC.discernibility.mat.RST(decisiontable, range.object = NULL)
    redukty <- FS.all.reducts.computation(tmp)
    print("-------------------------------------------------------------")
    print("redukty : ")
    print("-------------------------------------------------------------")
    print(redukty)
    ReductTableLength <- length(redukty$decision.reduct)
    

    for(k in 1:ReductTableLength){
      if(k == 1){
        SredniaJakosciReduktow = 0
        loscAtrybutowWReduktach = 0
      }
      print("-------------------------------------------------------------")
      print(paste("to s¹ obliczenia dla tabeli z³o¿onej z reduktów nr:", k));
      print("-------------------------------------------------------------")
      print("-------------------------------------------------------------")
      newReductTable <- SF.applyDecTable(decisiontable, redukty, 
                                         control = list(indx.reduct = k))
      SzerokoscReduktow <- (dim(newReductTable)[2])
      testRedukt <- newReductTable[test1,]
      treningRedukt <- newReductTable[-test1,]
      nazwaDecyzji1 <- paste0((colnames(newReductTable)[SzerokoscReduktow]),"~.")
      traintree1 <- rpart(nazwaDecyzji1,treningRedukt,method = "class")
      newp <- predict(traintree1,testRedukt, type = "class")
      decisionAttribut <- (SzerokoscReduktow)
      newo <- confusionMatrix(newp,testRedukt[,decisionAttribut])
      DokladnoscReduktow <- newo$overall[1]
      print(newo)
      SredniaJakosciReduktow <- (SredniaJakosciReduktow + DokladnoscReduktow)
      loscAtrybutowWReduktach <-(loscAtrybutowWReduktach + (SzerokoscReduktow)-1)

      
    }
    
    print(paste("dokladmosc klasyfikacji drzewa glownego",y[indeksReduktow-1]))
    
    
    SredniaIloscReduktow <- (loscAtrybutowWReduktach / ReductTableLength)
    TabelaSredniaIloscReduktow[j+1]<- SredniaIloscReduktow
    
    SredniaReduktow <- (SredniaJakosciReduktow / ReductTableLength)
    y[indeksReduktow] <- SredniaReduktow
    indeksReduktow <- (indeksReduktow +1)
    print(paste("Œrednia dok³adnosc drzew pochodnych",y[indeksReduktow-1]))
    
    if(indeksReduktow == (N*3)){
      
    }else{
  
    y[indeksReduktow] <- 0
    indeksReduktow <- (indeksReduktow +1)
    }
    
    if(DokladnoscDrzewa <= DokladnoscReduktow){
      wynik <-"TAK"
    }else{
      wynik <- "NIE"
    }
    print("IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
    print("Zestawienie otrzymanych wyników w postaci tabeli")
    sredniailoscAtrybutowDoTabeli <- TabelaSredniaIloscReduktow[j+1]
    tabelaWyjsciowa[nrow(tabelaWyjsciowa)+1,] <- c(j+1,
                                                   ReductTableLength,
                                                   szerokoscTablicy-1,
                                                   SredniaIloscReduktow,
                                                   DokladnoscDrzewa,
                                                   DokladnoscReduktow,
                                                   wynik)
 
    
  }
    print('')
    print(tabelaWyjsciowa)
    barplot(y,col="red")
    write.csv2(tabelaWyjsciowa,  file ="tabelaWyjsciowa.csv")
    
    

    return (tabelaWyjsciowa)

    }


