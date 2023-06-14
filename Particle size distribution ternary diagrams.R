setwd("C:/Users/XXX/XXX/XXX") #place where all the raw csv from machine are located
output.file.name = "Granulometric_results.csv"
files <- list.files(pattern = "\\.csv$")

### Ouvrir toutes les library n?cessaires
library(dplyr)
library(grid)
library(ggplot2)
library(gridExtra)
library(ggtern)
library(devEMF)
library(soiltexture)
library(writexl)
library(tidyr)


#### SUPPRIMER LA FIN DES NOMS DE FICHIER DES CSV####
FinNomCsv = "-Cup000-000"

### Cr?ation des dataframes qui ont besoin d'une existence hors de la boucle
merged.df =data.frame()
merged.names = c("Echantillon","Units")
new.df = data.frame()

### Param?tres & seuils par chercheur (ajout FS, 2022-03-08)
seuil_argile <- 7
seuil_lim_fins <- 26
seuil_lim_gros <- 50
seuil_sablef <- 500

### Boucle de cr?ation du csv avec les bonnes colonnes, pas de texte dans les colonnes, et transpos?. 
### Comme files est une liste on peut faire cr?er un file pour chaque objet de la liste files

for(file in files)
  {
	## Vider le new.df ? chaque d?but de boucle
	new.df = data.frame()	
	
	## cr?er un dataframe ? partir du premier csv. On l'oblige ? avoir 5 colonnes car sinon il regarde les 6 premi?res lignes pour savoir combien de colonnes utiliser
	df = read.csv(file,header=FALSE, sep="\t",dec=".",col.names= paste0("V", seq_len(5)))
	df = as.matrix(df)
	
	## dia25 et dia75 sont deux vecteurs qui sont des bouts de la ligne 9, en la coupant par des stringsplit et ? la fin on rajoute une colonne vide pour l'obliger ? en avoir 3
	dia25 = strsplit(as.character(df[9,2]),"Microns")[[1]]
	dia25  = strsplit(dia25,"-")[[1]]	
	dia25[1]= strsplit(dia25[1],")")
  dia25=c("D25",paste0(dia25[[2]][1],"Microns"),"")
	
	dia75 = strsplit(as.character(df[9,3]),"Microns")[[1]]
	dia75  = strsplit(dia75,"-")[[1]]	
	dia75[1]= strsplit(dia75[1],")")
	dia75=c("D75",paste0(dia75[[2]][1],"Microns"),"")
	
	## cat1, cat2, cat3 et cat4 sont des vecteurs qui sont des bouts de la ligne 15, cat5 est un nouveau vecteur pour avoir directement la proportion d'argiles.
	cat1 = strsplit(as.character(df[15,2]),") ")[[1]]
	cat1[1] = sprintf("%% Sables grossiers (2000-%gµm)", seuil_sablef)
	cat1=c(cat1,"")

	cat2 = strsplit(as.character(df[15,3]),") ")[[1]]
	cat2[1] = sprintf("%% Sables grossiers (%g-%gµm)", seuil_sablef, seuil_lim_gros)
	cat2=c(cat2,"")

	cat3 = strsplit(as.character(df[15,4]),") ")[[1]]
	cat3[1] = sprintf("%% Limons grossiers (%g-%gµm)", seuil_lim_gros, seuil_lim_fins)
	cat3=c(cat3,"")

	cat4 = strsplit(as.character(df[15,5]),") ")[[1]]
	cat4[1] = sprintf("%% Limons fins (%g-%gµm)", seuil_lim_fins, seuil_argile)
	cat4=c(cat4,"")
	
  cat5_val = round((100 - sum(as.numeric(c(abs(as.numeric(cat1[2])),abs(as.numeric(cat2[2])),abs(as.numeric(cat3[2])),abs(as.numeric(cat4[2])))))),2)
  cat5_nam = sprintf("%% Argiles (<%gµm)", seuil_argile)
  cat5=c(cat5_nam, cat5_val, "")
	
	df[38,1] = "Diameter (µm)"
	df[38,2] = "Frequency (%)"
	df[38,3] = "Passant (%)"
	## ici on fait un new.df en rbind, en prenant les lignes que l'on souhaite en piquant 
	## dans le df initial et en prenant les nouveaux vecteurs qu'on a cr?? pour les mettre o? l'on veut et en ne gardant 
	## que les 3 premi?res colonnes (les autres sont maintenant vides car on a vir? la ligne 15
	new.df=rbind(df[10,1:3],dia25,c("D50",df[13,2:3]),dia75,c("D90",df[14,2:3]),cat1,cat2,cat3,cat4,cat5,df[19:20,1:3],df[38:nrow(df),1:3])
  
	## ici on remodifie le new.df en collant ? la suite des lignes d?j? existantes les fractions granulo et le %passant. 
	## Au passage on a donc plus que deux colonnes.
	new.df = rbind(new.df[,1:2],df[38:nrow(df),c(1,3)])

	## ici on va supprimer les chaines de caract?res microns2 et Microns. 
	# premi?re ?tape : conna?tre les positions o? il y a ces mots
	microns.index=grep("Microns",new.df[,2])
	# seconde ?tape : cr?e un nouveau vecteur vide mais qui a d?j? le bon nombre de lignes
	unit=rep("",nrow(new.df))
	# troisi?me ?tape : utiliser les positions connues en premi?re ?tape pour mettre au bon endroit les unit?s
	unit[microns.index]="microns"
  unit[13]="Diam?tre (phi)"
  unit[14:106]=round(((-log(0.001*as.numeric(new.df[14:106])))/log(2)),2)
  unit[108:200]=round(((-log(0.001*as.numeric(new.df[108:200])))/log(2)),2)
	# quatri?me ?tape : faire un rechercher/remplacer par rien dans la colonne 2
	new.df[1:16,2]=gsub("[A-Za-z%/(/)/-]","",new.df[1:16,2])
	new.df[1:16,2]=round(as.numeric(new.df[1:16,2]),4)
  
  # Ce if sert ? ne cr?er qu'une seule fois les deux premi?res colonnes dans le merged.df cr?? avant la boucle.
	if(nrow(merged.df)==0){
		merged.df=cbind(new.df[,1],unit)
		}
	
	# ici on combine le merge.df (qui s'agrandit ? chaque boucle car il n'est pas vid? en d?but de boucle) avec le new.df qui lui est vid? ? chaque d?but de boucle.
	merged.df = cbind(merged.df,new.df[,2])
	# ici on cr?e avec la boucle, au fur et ? mesure, un autre vecteur (une seule ligne). Ici on acolle donc le merged.names cr?? avant la boucle qui a 
	# deux colonnes avec ? chaque fois le nom du fichier csv split? au niveau du .csv (et le [[1]] sert ? dire qu'on s?lectionne la premi?re 
	# partie m?me si ici ?a ne sert ? rien car une fois qu'on a splitt? sur .csv il ne reste plus que la premi?re partie.)un nouveau vecteur qui est le stringsplit 
	merged.names = c(merged.names,strsplit(file,paste0(FinNomCsv,".csv"))[[1]] )
	# ici on donne un nom de colonne au dataframe qui est le vecteur cr?? juste au-dessus. J'imagine qu'un rbind fonctionnerait aussi.
	colnames(merged.df) = merged.names
	merged.df.xlsx=rbind(merged.names,merged.df)
}

#on transpose la dataframe
resultats = t(merged.df)

### Construction auto du fichier "newdata.csv"
col_passant <- which(resultats == "Passant (%)", arr.ind = TRUE)[1,2]
temp <- resultats[, (col_passant+1):ncol(resultats)] %>%
  as.data.frame() %>%
  mutate_all(as.numeric)
diametres <- temp[1, ]
ind_high_argile <- Position(function(x) x >= seuil_argile, diametres)
ind_high_lf <- Position(function(x) x >= seuil_lim_fins, diametres)
ind_high_lg <- Position(function(x) x >= seuil_lim_gros, diametres)
ind_high_sablef <- Position(function(x) x >= seuil_sablef, diametres)
coef_argile <- (seuil_argile - temp[1, ind_high_argile-1]) / (temp[1, ind_high_argile] - temp[1, ind_high_argile-1])
coef_lf <- (seuil_lim_fins - temp[1, ind_high_lf-1]) / (temp[1, ind_high_lf] - temp[1, ind_high_lf-1])
coef_lg <- (seuil_lim_gros - temp[1, ind_high_lg-1]) / (temp[1, ind_high_lg] - temp[1, ind_high_lg-1])
coef_sablef <- (seuil_sablef - temp[1, ind_high_sablef-1]) / (temp[1, ind_high_sablef] - temp[1, ind_high_sablef-1])

newdata <- data.frame(
  low_argile = temp[-c(1:2), ind_high_argile-1],
  high_argile = temp[-c(1:2), ind_high_argile],
  low_lf = temp[-c(1:2), ind_high_lf-1],
  high_lf = temp[-c(1:2), ind_high_lf],
  low_lg = temp[-c(1:2), ind_high_lg-1],
  high_lg = temp[-c(1:2), ind_high_lg],
  low_sablef = temp[-c(1:2), ind_high_sablef-1],
  high_sablef = temp[-c(1:2), ind_high_sablef]
) %>% mutate(
  passant_arg = low_argile + coef_argile * (high_argile - low_argile),
  passant_limonsf = low_lf + coef_lf * (high_lf - low_lf),
  passant_limonsg = low_lg + coef_lg * (high_lg - low_lg),
  passant_sablesf = low_sablef + coef_sablef * (high_sablef - low_sablef),
  pourcent_sablesf_sablesg = 100-passant_sablesf,
  pourcent_limonsg_sablesf = passant_sablesf - passant_limonsg,
  pourcent_limonsf_limonsg = passant_limonsg - passant_limonsf,
  pourcent_arg_limonsf = passant_limonsf - passant_arg,
  pourcent_arg = passant_arg
)

#?criture du newdata.csv si besoin de r?cup?rer les passants des valeurs particuli?res
write.csv2(newdata, "newdata.csv")

#Importation newdata dans fichier resultat
resultats.df = as.data.frame(resultats)
resultats.df[3:nrow(resultats.df),6:10]= newdata[,13:17]
file.remove("newdata.csv")

#Cr?er le nouveau csv en prenant comme nom de fichier ce qui est rentr? tout en haut. J'utilise write.table car write.csv est prot?g? 
# quant ? la modification de col.names et si je mets pas col.names=FALSE, j'ai une ligne en plus avec dia25, dia75 etc.
write.table(resultats.df,output.file.name, col.names=FALSE, sep=";")

########################################
###### Creation ternary diagram    #####
########################################

Echantillons=row.names(resultats.df[3:nrow(resultats.df),])
Sables = c(as.numeric(resultats.df[3:nrow(resultats.df),6])+as.numeric(resultats.df[3:nrow(resultats.df),7]))
Limons = c(as.numeric(resultats.df[(3:nrow(resultats.df)),8])+as.numeric(resultats.df[3:nrow(resultats.df),9]))
Argiles = c(as.numeric(resultats.df[(3:nrow(resultats.df)),10]))
CLAY = Argiles
SILT = Limons
SAND = Sables

pdf("Ternary with labels.pdf", height=10,width=10) 
Table_diag_tern = as.data.frame(cbind(Sables, Limons, Argiles))
row.names(Table_diag_tern) = Echantillons
diag_tern = ggtern() + geom_point(data=Table_diag_tern, aes(Sables,Argiles,Limons, color=Echantillons)) + geom_text(data=Table_diag_tern, size=3, hjust = 0.5, vjust=-0.5 , aes(Sables,Argiles,Limons, label=Echantillons))
diag_tern
dev.off()

pdf("Ternary without labels.pdf", height=10,width=10) 
Table_diag_tern = as.data.frame(cbind(Sables, Limons, Argiles))
row.names(Table_diag_tern) = Echantillons
diag_tern = ggtern() + geom_point(data=Table_diag_tern, aes(Sables,Argiles,Limons, color=Echantillons)) 
diag_tern
dev.off()

######################################################
###########   Triangles Textures #####################
######################################################

#### MODIFIER TEMPLATE####
##   Fetch the definition of the HYPRES texture triangle
usda <- soiltexture::TT.get( "USDA.TT")
##   Inspect the object to find the names of the polygons:
#usda
new.labels <- c( "Cl" = "Argile", "SiCl" = "Argile limoneuse", "SaCl" = "Argile sableuse", 
                 "ClLo" = "Limon argileux", "SiClLo" = "Limon argileux fin", "SaClLo" = "Limon argilo-sableux", "Lo" = "Limon",
                 "SiLo" = "Limon fin", "SaLo" = "Limon sableux","Si" = "Limon tres fin","LoSa" = "Sable limoneux", "Sa" = "Sable")
##   fetch the old labels (for later control)
old.labels <- names( usda[[ "tt.polygons" ]] ) 
##   Now replace the old labels by the new labels
names( usda[[ "tt.polygons" ]] ) <- new.labels[ 
  names( usda[[ "tt.polygons" ]] ) ]
##   We also change the main title of the triangle
#usda[[ "main" ]] <- "My texture triangle"
#   In the code below, the order in which you give the labels 
#   in principle does not matter, as long as each old label 
#  obtain a new label
##   Now control the old names and the new names:
#data.frame( "old" = old.labels, 
#            "new" = names( usda[[ "tt.polygons" ]] ) )
##Save your new template
soiltexture::TT.set( "USDA_fr.TT" = usda )
##Check on your new template
#soiltexture::TT.plot( class.sys = "USDA_fr.TT" )


############ TRACER LE TERNAIRE #########
Table_TT_plot = cbind(CLAY, SILT, SAND)
row.names(Table_TT_plot) = Echantillons
pdf("Diagramme_ternaire-texture.pdf", height=10,width=10) 
geo = TT.plot(class.sys   = "USDA_fr.TT",
        main = "Textural diagram without labels",
        #z.name = , #S'il y a une 4? colonne dans le data.frame pour une autre variable, on peut mettre ce nom.
        grid.show = FALSE, #montrer ou pas la grille tous les 10% (F : false)
        frame.bg.col = "white",
        #class.lab.show = "full",
        arrows.show = T,
        css.lab = c(
          sprintf("%% Argiles (0 - %g µm)", seuil_argile),
          sprintf("%% Limons (%g - %g µm)", seuil_argile, seuil_lim_gros),
          sprintf("%% Sables (%g - 2000 µm)", seuil_lim_gros)
        ),
        #blr.clock = c(T,T,T), #	Vector of logicals, eventually with NA values. Direction of increasing texture values on the BOTTOM, LEFT and RIGHT axis, respectively. A value of TRUE means that the axis direction is clockwise. A value of FALSE means that the axis direction is counterclockwise. A value of NA means that the axis direction is centripetal. Possible combinations are c(T,T,T); c(F,F,F); c(F,T,NA) and c(T,NA,F), for fully clockwise, fully counterclockwise, right centripetal and left centripetal orientations, respectively.
        lang = "fr",
        cex.lab = 0.7)
TT.points(tri.data=Table_TT_plot, geo=geo, col="red", pch=1, cex = 1)
dev.off()

pdf("Textural diagram without labels.pdf", height=10,width=10) 
geo = TT.plot(class.sys   = "USDA_fr.TT",
              main = "Textural diagram with labels",
              #z.name = , #S'il y a une 4? colonne dans le data.frame pour une autre variable, on peut mettre ce nom.
              grid.show = FALSE, #montrer ou pas la grille tous les 10% (F : false)
              frame.bg.col = "white",
              #class.lab.show = "full",
              arrows.show = T,
              css.lab = c(
                sprintf("%% Argiles (0 - %g µm)", seuil_argile),
                sprintf("%% Limons (%g - %g µm)", seuil_argile, seuil_lim_gros),
                sprintf("%% Sables (%g - 2000 µm)", seuil_lim_gros)
              ),
              #blr.clock = c(T,T,T), #	Vector of logicals, eventually with NA values. Direction of increasing texture values on the BOTTOM, LEFT and RIGHT axis, respectively. A value of TRUE means that the axis direction is clockwise. A value of FALSE means that the axis direction is counterclockwise. A value of NA means that the axis direction is centripetal. Possible combinations are c(T,T,T); c(F,F,F); c(F,T,NA) and c(T,NA,F), for fully clockwise, fully counterclockwise, right centripetal and left centripetal orientations, respectively.
              lang = "fr",
              cex.lab = 0.7)
TT.points(tri.data=Table_TT_plot, geo=geo, col="red", pch=1, cex = 1)
TT.text(tri.data = Table_TT_plot, geo = geo, labels = row.names(Table_TT_plot), cex=0.7, font = 2, col = "blue")
dev.off()

pdf("Textural diagram with labels.pdf", height=10,width=10) 
geo = TT.plot(class.sys   = "USDA_fr.TT",
              main = "Diagramme textural avec noms",
              #z.name = , #S'il y a une 4? colonne dans le data.frame pour une autre variable, on peut mettre ce nom.
              grid.show = FALSE, #montrer ou pas la grille tous les 10% (F : false)
              frame.bg.col = "white",
              #class.lab.show = "full",
              arrows.show = T,
              css.lab = c(
                sprintf("%% Argiles (0 - %g µm)", seuil_argile),
                sprintf("%% Limons (%g - %g µm)", seuil_argile, seuil_lim_gros),
                sprintf("%% Sables (%g - 2000 µm)", seuil_lim_gros)
              ),
              #blr.clock = c(T,T,T), #	Vector of logicals, eventually with NA values. Direction of increasing texture values on the BOTTOM, LEFT and RIGHT axis, respectively. A value of TRUE means that the axis direction is clockwise. A value of FALSE means that the axis direction is counterclockwise. A value of NA means that the axis direction is centripetal. Possible combinations are c(T,T,T); c(F,F,F); c(F,T,NA) and c(T,NA,F), for fully clockwise, fully counterclockwise, right centripetal and left centripetal orientations, respectively.
              lang = "fr",
              cex.lab = 0.7)
TT.points(tri.data=Table_TT_plot, geo=geo, col="red", pch=1, cex = 1)
dev.off()

########################################################################################################
###### Using category-specific colours or symbols when plotting points on the texture triangle  ########
########################################################################################################

## dummy texture dataset 
#my.text <- data.frame( 
#  "CLAY"  = c(05,60,15,05,25,05,25,45,65,75,13,47), 
#  "SILT"  = c(05,08,15,25,55,85,65,45,15,15,17,43), 
#  "SAND"  = c(90,32,70,70,20,10,10,10,20,10,70,10), 
#  "OC"    = c(20,14,15,05,12,15,07,21,25,30,05,28), 
#  "site"  = c( 5, 5, 5, 5, 6, 6, 6, 6, 9, 9, 9, 9) ) # ATTENTION TOUT LE DATAFRAME DOIT ETRE DES NOMBRES JE CROIS

##   List of categories
#cat_list <- unique( my.text[, "site" ] )

##   Attribute one colour per site
#col_list <- rainbow( n = length( cat_list ), 
#                     s = 0.8, v = 0.8 )
##   Add names to the colour list
#names( col_list ) <- as.character( cat_list )

##   Attribute one symbol (pch) per site
#pch_list <- 1:length( cat_list )
##   Add names to the symbol list
#names( pch_list ) <- as.character( cat_list )

## :: And plot it, with one color per site
#soiltexture::TT.plot( 
#  class.sys = "USDA-NCSS.TT", 
#  tri.data  = my.text, 
#  main      = "Soil texture data, per site", 
#  col       = col_list[ as.character( my.text[, "site" ] ) ], 
#  pch       = pch_list[ as.character( my.text[, "site" ] ) ] ) 

#legend( 
#  x       = "topleft", 
#  title   = "Site:", 
#  legend  = cat_list, 
#  pch     = pch_list, 
#  col     = col_list, 
#  pt.lwd  = 2, 
#  bty     = "n" )


######################### #################################
###########   GRAPHIQUES DISTRIBUTION #####################
###########################################################

maxfreq = as.numeric(max(merged.df[25:117, 3:ncol(merged.df)]))

for(file in files)
{
  ## Vider le new.df ? chaque d?but de boucle
  new.df = data.frame()	
  
  ## cr?er un dataframe ? partir du premier csv. On l'oblige ? avoir 5 colonnes car sinon il regarde les 6 premi?res lignes pour savoir combien de colonnes utiliser
  df = read.csv(file,header=FALSE, sep="\t",dec=".",col.names= paste0("V", seq_len(5)))
  df = as.matrix(df)
  
  ## dia25 et dia75 sont deux vecteurs qui sont des bouts de la ligne 9, en la coupant par des stringsplit et ? la fin on rajoute une colonne vide pour l'obliger ? en avoir 3
  dia25 = strsplit(as.character(df[9,2]),"Microns")[[1]]
  dia25  = strsplit(dia25,"-")[[1]]	
  dia25[1]= strsplit(dia25[1],")")
  dia25[1]= (dia25[[1]][2])
  dia25[1]= strsplit(as.character(dia25[1]),".00")
  dia25[1] =paste0("Diam?tre pour ",(dia25[[1]][1]),"%")
  dia25=c("D25",paste0(dia25[[2]][1],"Microns"),"")
  
  dia75 = strsplit(as.character(df[9,3]),"Microns")[[1]]
  dia75  = strsplit(dia75,"-")[[1]]	
  dia75[1]= strsplit(dia75[1],")")
  dia75[1]= (dia75[[1]][2])
  dia75[1]= strsplit(as.character(dia75[1]),".00")
  dia75[1] =paste0("Diam?tre pour ",(dia75[[1]][1]),"%")
  dia75=c("D75",paste0(dia75[[2]][1],"Microns"),"")
  
  ## cat1, cat2, cat3 et cat4 sont des vecteurs qui sont des bouts de la ligne 15, cat5 est un nouveau vecteur pour avoir directement la proportion d'argiles.
  cat1 = strsplit(as.character(df[15,2]),")")[[1]]
  cat1[1] = paste0(cat1[1],")")
  cat1=c(cat1,"")
  
  cat2 = strsplit(as.character(df[15,3]),")")[[1]]
  cat2[1] = paste0(cat2[1],")")
  cat2=c(cat2,"")
  
  cat3 = strsplit(as.character(df[15,4]),")")[[1]]
  cat3[1] = paste0(cat3[1],")")
  cat3=c(cat3,"")
  
  cat4 = strsplit(as.character(df[15,5]),")")[[1]]
  cat4[1] = paste0(cat4[1],")")
  cat4=c(cat4,"")
  
  cat5 = round((100 - sum(as.numeric(c(abs(as.numeric(cat1[2])),abs(as.numeric(cat2[2])),abs(as.numeric(cat3[2])),abs(as.numeric(cat4[2])))))),2)
  cat5=c("% Argiles (<7µm)",cat5,"")
  
  ## ici on fait un new.df en rbind, en prenant les lignes que l'on souhaite en piquant 
  ## dans le df initial et en prenant les nouveaux vecteurs qu'on a cr?? pour les mettre o? l'on veut et en ne gardant 
  ## que les 3 premi?res colonnes (les autres sont maintenant vides car on a vir? la ligne 15
  new.df=rbind(df[10,1:3],dia25,c("D50",df[13,2:3]),dia75,c("D90",df[14,2:3]),cat1,cat2,cat3,cat4,cat5,df[19:20,1:3],df[38:nrow(df),1:3])
  
  ## ici on remodifie le new.df en collant ? la suite des lignes d?j? existantes les fractions granulo et le %passant. 
  ## Au passage on a donc plus que deux colonnes.
  new.df = rbind(new.df[,1:2],df[38:nrow(df),c(1,3)])
  
  ## ici on va supprimer les chaines de caract?res microns2 et Microns. 
  # premi?re ?tape : conna?tre les positions o? il y a ces mots
  microns.index=grep("Microns",new.df[,2])
  # seconde ?tape : cr?e un nouveau vecteur vide mais qui a d?j? le bon nombre de lignes
  unit=rep("",nrow(new.df))
  # troisi?me ?tape : utiliser les positions connues en premi?re ?tape pour mettre au bon endroit les unit?s
  unit[microns.index]="microns"
  unit[13]="diameter(phi)"
  unit[14:106]=round(((-log(0.001*as.numeric(new.df[14:106])))/log(2)),2)
  unit[108:200]=round(((-log(0.001*as.numeric(new.df[108:200])))/log(2)),2)
  # quatri?me ?tape : faire un rechercher/remplacer par rien dans la colonne 2
  new.df[1:16,2]=gsub("[A-Za-z%/(/)/-]","",new.df[1:16,2])
  new.df[1:16,2]=round(as.numeric(new.df[1:16,2]),4)
  

#cr?ation des variables pour le graphique de distribution
distributionmicrons=data.frame(fractionsmicrons=as.numeric(new.df[14:106,1]),pourcents=as.numeric(new.df[14:106,2]),pourcentscumul=as.numeric(new.df[108:200,2]))

# Cr?ation du graphique de distribution , alpha=0.5
plotmicrons=ggplot(distributionmicrons, aes(fractionsmicrons,pourcents)) + geom_area(fill="grey80") + theme(plot.margin = margin (t=10, b= 10, l=10, r= 30)) + scale_y_continuous(expand = c(0,0)) + scale_x_log10("Diam?tre des grains (µm)",expand = c(0,0), breaks=c(seq(1e-02,1e-01,by=1e-02),seq(0.1,1,by=0.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,3000,by=1000)),labels = c(0.01,rep("",9),0.1,rep("",9),1,rep("",9),10,rep("",9),100,rep("",9),1000,rep(""),3000))
plotmicrons + geom_vline(xintercept=c(0.01,0.1,1,10,100,1000), colour = "grey50", linetype="longdash")
ggsave(filename=paste0(strsplit(file,paste0(FinNomCsv,".csv")),".pdf"), width=10, height=5)

# Cr?ation du graphique de courbe cumulative
plotmicronscumul=ggplot(distributionmicrons, aes(fractionsmicrons,pourcentscumul)) + geom_line() + theme(plot.margin = margin (t=10, b= 10, l=10, r= 30)) + scale_y_continuous(expand = c(0,0)) + scale_x_log10("Diam?tre des grains (µm)",expand = c(0,0),breaks=c(seq(1e-02,1e-01,by=1e-02),seq(0.1,1,by=0.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,3000,by=1000)),labels = c(0.01,rep("",9),0.1,rep("",9),1,rep("",9),10,rep("",9),100,rep("",9),1000,rep(""),3000))
plotmicronscumul + geom_vline(xintercept=c(0.01,0.1,1,10,100,1000), colour = "grey50", linetype="longdash")
ggsave(filename=paste0(strsplit(file,paste0(FinNomCsv,".csv")),"cumul.pdf"), width=10, height=5)

#cr?ation d'une figure regroupant les deux graphiques frequence et cumul?s
plotfreq = ggplot(distributionmicrons, aes(fractionsmicrons,pourcents)) + geom_area(fill="grey80") + theme(plot.margin = margin (t=10, b= 10, l=10, r= 30)) + scale_y_continuous(limits=c(0,maxfreq) , expand = c(0,0)) + scale_x_log10("Diam?tre des grains (µm)",expand=c(0,0), breaks=c(seq(1e-02,1e-01,by=1e-02),seq(0.1,1,by=0.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,3000,by=1000)),labels = c(0.01,rep("",9),0.1,rep("",9),1,rep("",9),10,rep("",9),100,rep("",9),1000,rep(""),3000))
plotcumul = ggplot(distributionmicrons, aes(fractionsmicrons,pourcentscumul)) + geom_line() + theme(plot.margin = margin (t=10, b= 10, l=10, r= 30)) + scale_y_continuous(expand = c(0,0)) + scale_x_log10("Diam?tre des grains (µm)",expand=c(0,0), breaks=c(seq(1e-02,1e-01,by=1e-02),seq(0.1,1,by=0.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,3000,by=1000)),labels = c(0.01,rep("",9),0.1,rep("",9),1,rep("",9),10,rep("",9),100,rep("",9),1000,rep(""),3000))
fullplot=arrangeGrob(plotfreq, arrangeGrob(plotcumul), ncol=1)
ggsave(filename=paste0(strsplit(file,paste0(FinNomCsv,".csv")),"full.pdf"),fullplot, width=10, height=10)
}


####### GRAPHIQUE TOUTES COURBES ENSEMBLE ###############

# Transformation tidy des donn?es de distribution
Echantillons=row.names(resultats.df[3:nrow(resultats.df),])
X = resultats.df[1,14:106] %>% mutate_all(as.numeric)
Y_freq = resultats.df[3:nrow(resultats.df),14:106] %>% mutate_all(as.numeric)
Y_cumul = resultats.df[3:nrow(resultats.df),108:200] %>% mutate_all(as.numeric)
Data_freq = as.data.frame(cbind(Echantillons,Y_freq))
Data_cumul = as.data.frame(cbind(Echantillons,Y_cumul))
row.names(Data_freq) = Echantillons
row.names(Data_cumul) = Echantillons
colnames(Data_freq) = c("Echantillon",X)
colnames(Data_cumul) = c("Echantillon",X)
Freq_tidy = pivot_longer(Data_freq,cols = 2:94,names_to = "Taille", values_to="Frequence")
Cumul_tidy = pivot_longer(Data_cumul,cols = 2:94,names_to = "Taille", values_to="Cumule")

# Graphiques
pdf("All frequency distributions.pdf", height=10,width=20) 
plot=ggplot(Freq_tidy, aes(as.numeric(Taille),Frequence, color=Echantillon)) + 
    geom_line(size = 1) +  
    theme(plot.margin = margin (t=10, b= 10, l=10, r= 30)) + 
    scale_y_continuous("Frequency (%)", expand = c(0,0)) + 
    scale_x_log10("Diameter of grains (µm)",expand = c(0,0), breaks=c(seq(1e-02,1e-01,by=1e-02),seq(0.1,1,by=0.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,3000,by=1000)),labels = c(0.01,rep("",9),0.1,rep("",9),1,rep("",9),10,rep("",9),100,rep("",9),1000,rep(""),3000)) +
    geom_vline(xintercept=c(0.01,0.1,1,10,100,1000), colour = "grey30", linetype="longdash")
plot
dev.off()

pdf("All cumulated distributions.pdf", height=10,width=20) 
plot=ggplot(Cumul_tidy, aes(as.numeric(Taille),Cumule, color=Echantillon)) + 
  geom_line(size=1) +  
  theme(plot.margin = margin (t=10, b= 10, l=10, r= 30)) + 
  scale_y_continuous("% cumulated",expand = c(0,0)) + 
  scale_x_log10("Diameter of grains (µm)",expand = c(0,0), breaks=c(seq(1e-02,1e-01,by=1e-02),seq(0.1,1,by=0.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,3000,by=1000)),labels = c(0.01,rep("",9),0.1,rep("",9),1,rep("",9),10,rep("",9),100,rep("",9),1000,rep(""),3000)) +
  geom_vline(xintercept=c(0.01,0.1,1,10,100,1000), colour = "grey30", linetype="longdash")
plot
dev.off()

