prod_init=read.csv(file='C:\\Users\\bryan\\Downloads\\productivity.csv', header=T , sep=',');
#1)
dim(prod_init)
nrow(prod_init) #600 lignes ie observations
ncol(prod_init) #15 colonnes ie 15 variables

#2)
which(rowSums(is.na(prod_init))==T) #la location des valeurs manquantes 
sum(is.na(prod_init))#nombre de observations manquantes 254

#3) 
#1ere méthode: supprimer ou mettre des 0 àla place
prod=na.omit(prod_init) #supp les lignes des valeurs manquantes
prod #280 observations sont concernées
dim(prod)
summary(prod)

#4)
dim(prod) #une fois NA supp on a 346 observations et 15 var (n'a pas changé)

actual=prod[,15] #affiche C15 de prod cad actual_prod
summary(actual) #min=0.2494, max=1.1005, Q1=0.6648, Q3=0.8004, med=0.7506, moy=0.7262
#moyenne relativement haute car dépasse 0.5 (moy classique), valeurs assez proches, quartiles proches

#5) 
#coeff de corr entre actual et autres variables
team=prod[,5] #1
t_p=prod[,6]#2
smv=prod[,7]#3
wip=prod[,8]#4 
over_time=prod[,9] #5 
incentive=prod[,10] #6
idle_time=prod[,11]#7
idle_men=prod[,12]#8
no_of_style=prod[,13]#9
no_of_workers=prod[,14]#10
actual=prod[,15]#11

X=matrix(c(team,t_p,smv,wip,over_time,incentive,idle_time,idle_men,no_of_style,no_of_workers,actual), byrow=F, ncol=11)
X
cor(X)
# ou 
cor(prod[5:15])

#7) #variance de chaque variable
var=apply(prod[5:15],2,var)
var

var(team) #12.59898
v1=var(prod[,5])
v1 #var de C5 de prod : 12.59898

v2=var(prod[5:15])
v2 #var des colonnes

var(t_p)#0.009639608
var(smv) #52.40240320
#var(wip)=2.451472e+06, var(over_time)=8.398011e+06, var(incentive)=787.615582
var(team)
var(t_p)
var(smv)
var(wip)
var(over_time)
var(incentive)
var(idle_time)
var(idle_men)
var(no_of_style)
var(no_of_workers)
var(actual)

#important de standardiser pr comparer ce qui est comparable. Permet une ???tude + approfondie et clair
#X #matrice des var num???riques 
#c1<-colSums(prod[5:15])
#scale(prod[5:15], center=F,scale=c1)
prod_scale=scale(prod[5:15])

#8 PCA
select_PCA=subset(prod_scale[0:346,0:11], select=c(2,3,5,6,10,11))
PCA_prod=prcomp(select_PCA,scale=TRUE, center=T)
PCA_prod$rotation #vect propres
#affichage des 2 PCA
PCA_prod$rotation[,1]
PCA_prod$rotation[,2]

screeplot(PCA_prod,type="lines") #nuage de points
biplot(PCA_prod)

#9) cercle de corrélation
install.packages(c("FactoMineR", "factoextra"));
install.packages("ade4");
library(FactoMineR)
cercle.PCA=PCA(select_PCA,scale.unit=TRUE,ncp=5,graph=TRUE)

#10)PVE
PCA_prod$sdev^2 #val propres
PVE=PCA_prod$sdev^2 / sum(PCA_prod$sdev^2)
PVE
cumsum(PVE)
plot(PVE, xlab="Composant Principal", ylab="PVE", type="b")
plot(cumsum(PVE), xlab="Composant Principal", ylab="PVE cumulatif", type="b")


#d) regression lin???aire

#11)
x<-prod[,6] #targeted prod
y<-prod[,15] #actual prod
prod.regsimple<-lm(y~x) #y=bo+b1*x+e
prod.regsimple #b0^=-0.06468 et b1^=1.08886 ie y=-0.06468+1.0886*x+e


#12)
prod.regsimple$coefficients #val b0 et b1

#moyenne de e
e=prod.regsimple$residuals #ttes les val de e
mean(e) #-2.285572*10^-18

#13)int conf 90%
confint(prod.regsimple,1,0.90) #int de b0
confint(prod.regsimple,2,0.90) #int de b1

#15)
summary(prod.regsimple)
#r^2=0.5008=50% de la variation de actual prod est expliqu???e par la variation de targeted prod
#r???gression satisfaisante mais mod???le pas otpimis??? car E autres variables qui peuvent affecter la variation de actual prod
#BILAN : on fait une RLM : On la r???alise en prenant en compte 6 variables, 
#on cherche ??? d???terminer quel nombre et quelles variables utiliser pour obtenir la r???gression la plus optimale.

#16) s???lection des variables
library(ISLR)
library(leaps)
attach(prod)

select_var=regsubsets(actual_productivity~.,prod[0:346,c(6:10,14)],nvmax = 6)
select_var
reg.summary=summary(select_var)
reg.summary

prod.regmul<-lm(actual_productivity~smv+targeted_productivity+incentive+no_of_workers)
summary(prod.regmul)

#names(prod.summary)

reg.summary$rsq#coeff de d???termination r2 des var
plot(reg.summary$rsq, type="b")

reg.summary$adjr2 #coeff de d???termination r2 ajust???
plot(reg.summary$adjr2, type="b")


which.max(reg.summary$adjr2) #la plus grande valeur de R2 adjusted : R2 + ???lev??? pr 4 variables

#17)on ???limine donc 2 variables ie les - fiables wip et overtime
#on garde: targeted prod, smv, incentive, no workers
coef(select_var,4) 


#18)R ajust??? plutot que normal car prend en compte le nombre de variables, 
#ce que ne fait pas R???, qui cro???t avec le nombre de variables.

plot(reg.summary$adjr2,xlab = "Number of Variables",ylab = "Adjusted RSquare",type = "l")
points(6,reg.summary$adjr2[6],col="red",cex=2,pch=20)

#19)RSS
coef(select_var,4) #coeff estim???s

reg.summary$rss
plot(reg.summary$rss,xlab = "Number of Variables",ylab = "RSS",type = "l")
plot(reg.summary$rsq,xlab = "Number of Variables",ylab = "RSquare",type = "l")
fit_mult.null <- lm(formula = actual_productivity ~ 1, data = product)
anova(fit_mult.null, fit_mult)

