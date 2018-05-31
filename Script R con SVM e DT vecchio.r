# Librerie da importare ed installare se non presenti
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(caret)
library(ggthemes)
library(RColorBrewer)
library(fmsb)
library(rpart.plot)
library(ROCR)
library(magrittr)

# Importo il file csv con l'elenco di tutti i Pokemon e relative caratteristiche
pokemon<-read.csv("./pokemon.csv",sep=",",stringsAsFactors=F)

# Attribuisco un nome a parametri
colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")

Type.1<-c("Dragon","Steel","Flying","Psychic","Rock" ,"Fire","Electric" ,"Dark","Ghost" ,"Ground","Ice", "Water","Grass","Fighting", "Fairy" ,"Poison","Normal","Bug")
#color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")

# names contiene l'associazione id-nome del pokemon
names <- pokemon %>% select(id, Name)

# Modeling

# Rileggo il file con i combattimenti e risultati per fare il training del modello
combats<-read.csv('./combats.csv',sep=",",stringsAsFactors=F)

# Trova i nomi dei contendenti dati gli ID
combats$First_pokemon_name<-sapply(combats$First_pokemon, function(x) names$Name[match(x, names$id)])
combats$Second_pokemon_name<-sapply(combats$Second_pokemon, function(x) names$Name[match(x, names$id)])

# Vengono recuperati i parametri dei contendenti e calcolate le differenze su alcuni di essi
combats$First_pokemon_attack<-sapply(combats$First_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
combats$Second_pokemon_attack<-sapply(combats$Second_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
combats$Diff_attack<-combats$First_pokemon_attack - combats$Second_pokemon_attack

combats$winner_first_label<-ifelse(combats$Winner==combats$First_pokemon,'yes','no')

combats$First_pokemon_defense<-sapply(combats$First_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
combats$Second_pokemon_defense<-sapply(combats$Second_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
combats$Diff_defense<-combats$First_pokemon_defense - combats$Second_pokemon_defense

combats$First_pokemon_sp_defense<-sapply(combats$First_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
combats$Second_pokemon_sp_defense<-sapply(combats$Second_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
combats$Diff_sp_defense<-combats$First_pokemon_sp_defense - combats$Second_pokemon_sp_defense

combats$First_pokemon_sp_attack<-sapply(combats$First_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
combats$Second_pokemon_sp_attack<-sapply(combats$Second_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
combats$Diff_sp_attack<-combats$First_pokemon_sp_attack - combats$Second_pokemon_sp_attack

combats$First_pokemon_speed<-sapply(combats$First_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
combats$Second_pokemon_speed<-sapply(combats$Second_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
combats$Diff_speed<-combats$First_pokemon_speed - combats$Second_pokemon_speed

combats$First_pokemon_HP<-sapply(combats$First_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
combats$Second_pokemon_HP<-sapply(combats$Second_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
combats$Diff_HP<-combats$First_pokemon_HP - combats$Second_pokemon_HP

combats$First_pokemon_type<-sapply(combats$First_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
combats$Second_pokemon_type<-sapply(combats$Second_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
combats$First_pokemon_legendary<-sapply(combats$First_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])
combats$Second_pokemon_legendary<-sapply(combats$Second_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])

# salvo combats su csv

#scale numerical features
temp<- data.frame(combats %>% dplyr::select(winner_first_label,Diff_attack ,Diff_defense, Diff_sp_defense,Diff_sp_attack,Diff_speed ,Diff_HP, First_pokemon_legendary, Second_pokemon_legendary))
# Determino quali colonne di temp sono attributi di tipo numerico
ind <- sapply(temp, is.numeric)
# temp[ind] contiene solo le colonne di combats aventi valori di tipo numerico
temp[ind] <- lapply(temp[ind], scale) # Scala le colonne

# Garanzia di riproduzione dei risultati
set.seed(1234)

# Partiziono tra training e testing
split <- createDataPartition(y=temp$winner_first_label, p = 0.75, list = FALSE)
train <- temp[split,]
test <- temp[-split,]

trControl <- trainControl(method = "cv",number = 10)

res<-train(winner_first_label~.,data=train,method='svmLinear',trControl = trControl,metric='Accuracy') # la metrica per scegliere il modello migliore si basa sull'accuratezza

cm <- caret::confusionMatrix(res)

precision <- cm$table[1]/(cm$table[1]+cm$table[2])
recall <- cm$table[1]/(cm$table[1]+cm$table[3])
f <- (2*precision*recall)/(precision+recall)

test_pred <- predict(res, newdata = test)
cm_pred <- confusionMatrix(as.factor(test_pred), as.factor(test$winner_first_label))

precision_pred <- cm_pred$table[1]/(cm_pred$table[1]+cm_pred$table[2])
recall_pred <- cm_pred$table[1]/(cm_pred$table[1]+cm_pred$table[3])
f_pred <- (2*precision_pred*recall_pred)/(precision_pred+recall_pred)

# Rileggo il file con i combattimenti e risultati per fare il training del modello
real_test<-read.csv('./tests.csv',sep=",",stringsAsFactors=F)

# Trova i nomi dei contendenti dati gli ID
real_test$First_pokemon_name<-sapply(real_test$First_pokemon, function(x) names$Name[match(x, names$id)])
real_test$Second_pokemon_name<-sapply(real_test$Second_pokemon, function(x) names$Name[match(x, names$id)])

# Vengono recuperati i parametri dei contendenti e calcolate le differenze su alcuni di essi
real_test$First_pokemon_attack<-sapply(real_test$First_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
real_test$Second_pokemon_attack<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
real_test$Diff_attack<-real_test$First_pokemon_attack - real_test$Second_pokemon_attack

#real_test$winner_first_label<-ifelse(real_test$Winner==real_test$First_pokemon,'yes','no')

real_test$First_pokemon_defense<-sapply(real_test$First_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
real_test$Second_pokemon_defense<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
real_test$Diff_defense<-real_test$First_pokemon_defense - real_test$Second_pokemon_defense

real_test$First_pokemon_sp_defense<-sapply(real_test$First_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
real_test$Second_pokemon_sp_defense<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
real_test$Diff_sp_defense<-real_test$First_pokemon_sp_defense - real_test$Second_pokemon_sp_defense

real_test$First_pokemon_sp_attack<-sapply(real_test$First_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
real_test$Second_pokemon_sp_attack<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
real_test$Diff_sp_attack<-real_test$First_pokemon_sp_attack - real_test$Second_pokemon_sp_attack

real_test$First_pokemon_speed<-sapply(real_test$First_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
real_test$Second_pokemon_speed<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
real_test$Diff_speed<-real_test$First_pokemon_speed - real_test$Second_pokemon_speed

real_test$First_pokemon_HP<-sapply(real_test$First_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
real_test$Second_pokemon_HP<-sapply(real_test$Second_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
real_test$Diff_HP<-real_test$First_pokemon_HP - real_test$Second_pokemon_HP

real_test$First_pokemon_type<-sapply(real_test$First_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
real_test$Second_pokemon_type<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
real_test$First_pokemon_legendary<-sapply(real_test$First_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])
real_test$Second_pokemon_legendary<-sapply(real_test$Second_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])

#scale numerical features
temp_real_test<- data.frame(real_test %>% dplyr::select(Diff_attack ,Diff_defense, Diff_sp_defense,Diff_sp_attack,Diff_speed ,Diff_HP, First_pokemon_legendary, Second_pokemon_legendary))
# Determino quali colonne di temp sono attributi di tipo numerico
ind <- sapply(temp_real_test, is.numeric)
# temp[ind] contiene solo le colonne di real_test aventi valori di tipo numerico
temp_real_test[ind] <- lapply(temp_real_test[ind], scale) # Scala le colonne

test_real_pred <- predict(res, newdata = temp_real_test)

real_test$winner_first_label<-test_real_pred

# salvo real_test su csv

# stampo parametri
print(cm)
print(precision)
print(recall)
print(f)

print(cm_pred)
print(precision_pred)
print(recall_pred)
print(f_pred)

# Leggere file pokemonTypeComp.csv

atk<-c('Normal','Fire','Water','Electric','Grass','Ice','Fighting','Poison','Ground','Flying','Psychic','Bug','Rock','Ghost','Dragon','Dark','Steel','Fairy')
normal<-c(1,1,1,1,1,1,2,1,1,1,1,1,1,0,1,1,1,1)
fire<-c(1,0.5,2,1,0.5,0.5,1,1,2,1,1,0.5,2,1,1,1,0.5,0.5)
water<-c(1,0.5,0.5,2,2,0.5,1,1,1,1,1,1,1,1,1,1,0.5,1)
elec<-c(1,1,1,0.5,1,1,1,1,2,0.5,1,1,1,1,1,1,0.5,1)
grass<-c(1,2,0.5,0.5,0.5,2,1,2,0.5,2,1,2,1,1,1,1,1,1)
ice<-c(1,2,1,1,1,0.5,2,1,1,1,1,1,2,1,1,1,2,1)
fighting<-c(1,1,1,1,1,1,1,1,1,2,2,0.5,0.5,1,1,0.5,1,2)
poison<-c(1,1,1,1,0.5,1,0.5,0.5,2,1,2,0.5,1,1,1,1,1,0.5)
ground<-c(1,1,2,0,2,2,1,0.5,1,1,1,1,0.5,1,1,1,1,1)
flying<-c(1,1,1,2,0.5,2,0.5,1,0,1,1,0.5,2,1,1,1,1,1)
psychic<-c(1,1,1,1,1,1,0.5,1,1,1,0.5,2,1,2,1,2,1,1)
bug<-c(1,2,1,1,0.5,1,0.5,1,0.5,2,1,1,2,1,1,1,1,1)
rock<-c(0.5,0.5,2,1,2,1,2,0.5,2,0.5,1,1,1,1,1,1,2,1)
ghost<-c(0,1,1,1,1,1,0,0.5,1,1,1,0.5,1,2,1,2,1,1)
dragon<-c(1,0.5,0.5,0.5,0.5,2,1,1,1,1,1,1,1,1,2,1,1,2)
dark<-c(1,1,1,1,1,1,2,1,1,1,0,2,1,0.5,1,0.5,1,2)
steel<-c(0.5,2,1,1,0.5,0.5,2,0,2,0.5,0.5,0.5,0.5,1,0.5,1,0.5,0.5)
fairy<-c(1,1,1,1,1,1,0.5,2,1,1,1,0.5,1,1,0,0.5,2,1)
mytable<-data.frame(Attacking=atk,Normal=normal,Fire=fire,Water=water,Electric=elec,Grass=grass,Ice=ice,Fighting=fighting,Poison=poison,Ground=ground,Flying=flying,Psychic=psychic,Bug=bug,Rock=rock,Ghost=ghost,Dragon=dragon,Dark=dark,Steel=steel,Fairy=fairy)

mytable[ which(mytable$Attacking==combats$First_pokemon_type[2]),c(combats$Second_pokemon_type[2])]

makeAdvantage2<-function(type_1,type_2){
   val <- mytable[ which(mytable$Attacking==type_1),c(type_2)]
   if(val==0){
     return('no effect')
   }
   else if(val==0.5){
     return('not too effective')
   }
   else if(val==1){
     return('normal')
   }
   else if(val==2){
     return('effective')
   }
}

# Determino attributo advantage per la tabella combats
combats$advantage<-mapply(makeAdvantage2, combats$First_pokemon_type, combats$Second_pokemon_type)

# Distrubuzione percentuale sulla base di advantage in relazione al fatto di essere il pokemon che attacca per primo
combats %>% 
  dplyr::select(advantage, winner_first_label) %>% 
  dplyr::group_by(advantage,winner_first_label) %>% 
  dplyr::summarize(count=n()) %>% 
  dplyr::group_by(advantage) %>% 
  dplyr::mutate(perc = count / sum(count))

temp <- data.frame(combats %>% dplyr::select(winner_first_label,Diff_attack ,Diff_defense, Diff_sp_defense,Diff_sp_attack,Diff_speed ,Diff_HP, First_pokemon_legendary, Second_pokemon_legendary, advantage))
ind <- sapply(temp, is.numeric)
temp[ind] <- lapply(temp[ind], scale)

set.seed(2345)
split <- createDataPartition(y=temp$winner_first_label, p = 0.75, list = FALSE)
train <- temp[split,]
test <- temp[-split,]

res2.tree<-train(winner_first_label~Diff_attack+Diff_defense+Diff_sp_defense+Diff_sp_attack+Diff_speed+Diff_HP+First_pokemon_legendary+Second_pokemon_legendary+advantage,data=train,method='rpart',trControl = trainControl(method = "cv",number = 5, repeats=3))

# Calcolo predizione basandomi sulla probailità
probs2 <- predict(res2.tree, newdata=test, type='prob')
# Aggiungo a prob2 l'attributo contenente le informazione sul vincito (Winner_first_label)
probs2<-data.frame(cbind(probs2,winner_first = test$winner_first_label))
# Aggiungo attributo winner_first_num pari 0 se il primo pokemon ad attaccare non vince, 1 altrimenti
probs2$winner_first_num<-ifelse(probs2$winner_first=='no',0,1)

# Rappresento il modello graficamente
rpart.plot(res2.tree$finalModel)

# Mostro grafico sull'importanza delle feature
plot(caret::varImp(res2.tree))

# Calcolo Curva ROC

pred2<-ROCR::prediction(probs2$yes,probs2$winner_first_num)
perf2<-ROCR::performance(pred2,"tpr", "fpr")
roc2.data <- data.frame(fpr=unlist(perf2@x.values),tpr=unlist(perf2@y.values),model="tree1")
auc2 <- performance(pred2, measure = "auc")
auc2 <- round(auc2@y.values[[1]],3)
g2 <- ggplot(roc2.data, aes(x=fpr, ymin=0, ymax=tpr)) + 
  geom_ribbon(alpha=0.2) + geom_line(aes(y=tpr)) + 
  labs(title= paste0("ROC Curve for Tree with advantage feature w/AUC=", auc2),
       subtitle = "x-axis is False Positive Rate\ny-axis is True Positive rate") + 
  theme_fivethirtyeight()
g2 <- g2 + geom_segment(x = 0, y = 0, xend = 1, yend = 1, colour = 'red')
g2


cm <- caret::confusionMatrix(res2.tree)

precision <- cm$table[1]/(cm$table[1]+cm$table[2])
recall <- cm$table[1]/(cm$table[1]+cm$table[3])
f <- (2*precision*recall)/(precision+recall)

# Testing su dati reali

# Determino attributo advantage per la tabella real_test
real_test$advantage<-mapply(makeAdvantage2, real_test$First_pokemon_type, real_test$Second_pokemon_type)

