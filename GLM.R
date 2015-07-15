           getwd()
           setwd('C:/Users/Maja/Downloads/mva')
           getwd()
           baza<-read.table("plodnost.txt",header=T,sep=",")
           edit(baza)
           attach(baza)
           str(baza)
           #analiza dijagnoza u ovisnosti na godišnje doba
           
boxplot(g.doba[dijagnoza=="N"],g.doba[dijagnoza=="O"])   
boxplot(g.doba~dijagnoza,main="Kutijasti dijagram godišnjeg doba u odnosu na dijagnozu") 
summary(g.doba~dijagnoza) 
t.test(g.doba,dijagnoza)  )  
 boxplot(dob~dijagnoza,main="Kutijasti dijagram dobi ispitanika u odnosu na dijagnozu")
t.test(dob~dijagnoza)
 summary(dijagnoza~dob)
 boxplot(bolesti~dijagnoza,main="Kutijasti dijagram djeèjih bolesti ispitanika u odnosu na dijagnozu")
               chisq.test(table(bolesti,dijagnoza))
               table(bolesti,dijagnoza)
               boxplot(nesrece~dijagnoza,main="Kutijasti dijagram nesreca ispitanika u odnosu na dijagnozu")
               chisq.test(table(nesrece,dijagnoza))
               boxplot(operacije~dijagnoza,main="Kutijasti dijagram varijable operacije u odnosu na dijagnozu")
               chisq.test(table(operacije,dijagnoza))
               boxplot(temperatura~dijagnoza,main="Kutijasti dijagram varijable temperatura u odnosu na dijagnozu")
               chisq.test(table(temperatura,dijagnoza))
                      boxplot(alkohol~dijagnoza,main="Kutijasti dijagram varijable alkohol u odnosu na dijagnozu")
               chisq.test(table(alkohol,dijagnoza))
               
               boxplot(pusenje~dijagnoza,main="Kutijasti dijagram varijable pusenje u odnosu na dijagnozu")
               chisq.test(table(pusenje,dijagnoza))
               boxplot(sjedenje~dijagnoza,main="Kutijasti dijagram varijable pusenje u odnosu na dijagnozu")
               t.test(sjedenje~dijagnoza)
               
               
model<-glm(dijagnoza~g.doba*dob*bolesti*nesrece*operacije*temperatura*alkohol*pusenje*sjedenje,binomial) 
summary(model) 
m<-step(model)
anova(m,test="Chisq") 
drop1(model,test="Chisq")
anova(model,test="Chisq")  
model1<-glm(dijagnoza~g.doba+nesrece+alkohol*pusenje+bolesti,binomial) 
summary(model1)
anova(model1,test="Chisq")  



baza<-read.csv2('baza-vino.csv',header=T)
edit(baza)
attach(baza)
str(baza)

boxplot(alkohol~kvaliteta,main="Kutijasti dijagram alkohola u odnosu na dijagnozu") 
summary(kvaliteta~limunska.kiselina) 
t.test(alkohol,pH)  
mean(hlapljiva.kiselost[kvaliteta=="0"])
shapiro.test(fiksna.kiselost)
var.test(u.s.dioksid,alkohol,ratio=1)
t.test(u.s.dioksid,alkohol,var.equal=F)
model1<-glm(kvaliteta~alkohol+u.s.dioksid+hlapljiva.kiselost+fiksna.kiselost,binomial)
summary(model1)
step(model1)
1-pchisq(159.53-128.17,121-117)
step(model)
anova(model1,test="Chisq")
predikcije<-fitted.values(model1)
 par(mfrow=c(1,2))
boxplot(predikcije~kvaliteta)

 predikcije<-predict(model1)  #  skorovi u paketu ROCR
 edit(predikcije)
predikcije_vj<-predict(model1,type="response")      #  prediktirane vjerojatnosti u paketu ROCR
        edit(predikcije_vj)
pred_incid<-prediction(predikcije,incidence)

par(mfrow=c(1,2))
(fnr<-performance(pred_incid,"fnr") )

 (KS<-max(abs(attr(fnr,'y.values')[[1]]-attr(tnr,'y.values')[[1]])))
 

roc<-performance(pred_incid,"tpr","fpr")
plot(roc)
plot(fnr,col="red",ylab=" ")
tnr<-performance(pred_incid,"tnr")
plot(tnr,col="green",add=T,ylab=" ")

#model 2
model3<-glm(kvaliteta~alkohol*hlapljiva.kiselost+fiksna.kiselost*u.s.dioksid,binomial)
    summary(model3)
    step(model3)
    anova(model3,test="Chisq")
    1-pchisq(128.17-127.90,117-115)


