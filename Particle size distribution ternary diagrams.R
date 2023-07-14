
output.file.name = "Granulometric_results.csv"
files <- list.files(path = "./Raw data from machine",pattern = "\\.csv$")

### Necessary libraries
library(dplyr)
library(grid)
library(ggplot2)
library(gridExtra)
library(ggtern)
library(devEMF)
library(soiltexture)
library(writexl)
library(tidyr)


#### Delete the end of csv files from the machine if necessary####
FinNomCsv = "-Cup000-000"

### Creation of dataframes which need to be created out of loops
merged.df =data.frame()
merged.names = c("Sample","Units")
new.df = data.frame()

### Limits for different classes, different for different geologists (added by Fréderic Santos 2022-03-08)
clay_threshold <- 7
finesilt_threshold <- 26
coarsesilt_threshold <- 50
finesand_threshold <- 500

### Loop for creating the nice .csv out of the multiple bad csv from the machine

for(file in files)
  {
	## Empty the new.df for each new start of the loop
	new.df = data.frame()	
	
	## Creating the dataframe with the right number of columns (5)
	df = read.csv(file = paste0("./Raw data from machine/",file),header=FALSE, sep="\t",dec=".",col.names= paste0("V", seq_len(5)))
	df = as.matrix(df)
	
	## Quartile 1 and quartile 3 (diameter for respectively 25% and 75% of cumulated distribution) are prepared here from line 9 from the machine's csv.
	dia25 = strsplit(as.character(df[9,2]),"Microns")[[1]]
	dia25  = strsplit(dia25,"-")[[1]]	
	dia25[1]= strsplit(dia25[1],")")
  dia25=c("D25",paste0(dia25[[2]][1],"Microns"),"")
	
	dia75 = strsplit(as.character(df[9,3]),"Microns")[[1]]
	dia75  = strsplit(dia75,"-")[[1]]	
	dia75[1]= strsplit(dia75[1],")")
	dia75=c("D75",paste0(dia75[[2]][1],"Microns"),"")
	
	## Creation of 5 vectors for the t un nouveau vecteur pour avoir directement la proportionof Clay.
	cat1 = strsplit(as.character(df[15,2]),") ")[[1]]
	cat1[1] = sprintf("%% Coarse sand (2000-%gµm)", finesand_threshold)
	cat1=c(cat1,"")

	cat2 = strsplit(as.character(df[15,3]),") ")[[1]]
	cat2[1] = sprintf("%% Fine sand (%g-%gµm)", finesand_threshold, coarsesilt_threshold)
	cat2=c(cat2,"")

	cat3 = strsplit(as.character(df[15,4]),") ")[[1]]
	cat3[1] = sprintf("%% Coarse silt (%g-%gµm)", coarsesilt_threshold, finesilt_threshold)
	cat3=c(cat3,"")

	cat4 = strsplit(as.character(df[15,5]),") ")[[1]]
	cat4[1] = sprintf("%% Fine silt (%g-%gµm)", finesilt_threshold, clay_threshold)
	cat4=c(cat4,"")
	
  cat5_val = round((100 - sum(as.numeric(c(abs(as.numeric(cat1[2])),abs(as.numeric(cat2[2])),abs(as.numeric(cat3[2])),abs(as.numeric(cat4[2])))))),2)
  cat5_nam = sprintf("%% Clay (<%gµm)", clay_threshold)
  cat5=c(cat5_nam, cat5_val, "")
	
	df[38,1] = "Diameter (µm)"
	df[38,2] = "Frequency (%)"
	df[38,3] = "Passing (%)"
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
  unit[13]="Diameter (phi)"
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
results = t(merged.df)

### Construction auto du fichier "newdata.csv"
col_passing <- which(results == "Passing (%)", arr.ind = TRUE)[1,2]
temp <- results[, (col_passing+1):ncol(results)] %>%
  as.data.frame() %>%
  mutate_all(as.numeric)
diameters <- temp[1, ]
ind_high_clay <- Position(function(x) x >= clay_threshold, diameters)
ind_high_finesilt <- Position(function(x) x >= finesilt_threshold, diameters)
ind_high_coarsesilt <- Position(function(x) x >= coarsesilt_threshold, diameters)
ind_high_finesand <- Position(function(x) x >= finesand_threshold, diameters)
coef_clay <- (clay_threshold - temp[1, ind_high_clay-1]) / (temp[1, ind_high_clay] - temp[1, ind_high_clay-1])
coef_finesilt <- (finesilt_threshold - temp[1, ind_high_finesilt-1]) / (temp[1, ind_high_finesilt] - temp[1, ind_high_finesilt-1])
coef_coarsesilt <- (coarsesilt_threshold - temp[1, ind_high_coarsesilt-1]) / (temp[1, ind_high_coarsesilt] - temp[1, ind_high_coarsesilt-1])
coef_finesand <- (finesand_threshold - temp[1, ind_high_finesand-1]) / (temp[1, ind_high_finesand] - temp[1, ind_high_finesand-1])

newdata <- data.frame(
  low_clay = temp[-c(1:2), ind_high_clay-1],
  high_clay = temp[-c(1:2), ind_high_clay],
  low_finesilt = temp[-c(1:2), ind_high_finesilt-1],
  high_finesilt = temp[-c(1:2), ind_high_finesilt],
  low_coarsesilt = temp[-c(1:2), ind_high_coarsesilt-1],
  high_coarsesilt = temp[-c(1:2), ind_high_coarsesilt],
  low_finesand = temp[-c(1:2), ind_high_finesand-1],
  high_finesand = temp[-c(1:2), ind_high_finesand]
) %>% mutate(
  passing_clay = low_clay + coef_clay * (high_clay - low_clay),
  passing_finesilt = low_finesilt + coef_finesilt * (high_finesilt - low_finesilt),
  passing_coarsesilt = low_coarsesilt + coef_coarsesilt * (high_coarsesilt - low_coarsesilt),
  passing_finesand = low_finesand + coef_finesand * (high_finesand - low_finesand),
  pourcent_finesand_coarsesand = 100-passing_finesand,
  pourcent_coarsesilt_finesand = passing_finesand - passing_coarsesilt,
  pourcent_finesilt_coarsesilt = passing_coarsesilt - passing_finesilt,
  pourcent_clay_finesilt = passing_finesilt - passing_clay,
  pourcent_clay = passing_clay
)


# Import newdata in the main result file
results.df = as.data.frame(results)
results.df[3:nrow(results.df),6:10]= newdata[,13:17]

# Create the final csv (with write.table and not write.csv because I need the col.names = F possibility).
write.table(results.df,output.file.name, col.names=FALSE, sep=";")

########################################
###### Creation ternary diagram    #####
########################################

Samples=row.names(results.df[3:nrow(results.df),])
Sand = c(as.numeric(results.df[3:nrow(results.df),6])+as.numeric(results.df[3:nrow(results.df),7]))
Silt = c(as.numeric(results.df[(3:nrow(results.df)),8])+as.numeric(results.df[3:nrow(results.df),9]))
Clay = c(as.numeric(results.df[(3:nrow(results.df)),10]))
CLAY = Clay
SILT = Silt
SAND = Sand

pdf("Ternary with labels.pdf", height=10,width=10) 
Table_diag_tern = as.data.frame(cbind(Sand, Silt, Clay))
row.names(Table_diag_tern) = Samples
diag_tern = ggtern() + geom_point(data=Table_diag_tern, aes(Sand,Clay,Silt, color=Samples)) + geom_text(data=Table_diag_tern, size=3, hjust = 0.5, vjust=-0.5 , aes(Sand,Clay,Silt, label=Samples))
diag_tern
dev.off()

pdf("Ternary without labels.pdf", height=10,width=10) 
Table_diag_tern = as.data.frame(cbind(Sand, Silt, Clay))
row.names(Table_diag_tern) = Samples
diag_tern = ggtern() + geom_point(data=Table_diag_tern, aes(Sand,Clay,Silt, color=Samples)) 
diag_tern
dev.off()

######################################################
###########   Texture diagram #####################
######################################################

#### MODIFIER TEMPLATE####
##   Fetch the definition of the HYPRES texture triangle
usda <- soiltexture::TT.get( "USDA.TT")
##   Inspect the object to find the names of the polygons:
usda
new.labels <- c( "Cl" = "Clay", "SiCl" = "Silty clay", "SaCl" = "Sandy clay", 
                 "ClLo" = "Clayey silt", "SiClLo" = "clayey fine silt", "SaClLo" = "Sandy-clayey silt", "Lo" = "Silt",
                 "SiLo" = "Fine silt", "SaLo" = "Sandy silt","Si" = "Very fine silt","LoSa" = "Silty sand", "Sa" = "Sand")
##   fetch the old labels (for later control)
old.labels <- names( usda[[ "tt.polygons" ]] ) 
##   Now replace the old labels by the new labels
names(usda[["tt.polygons"]]) <- new.labels[ 
  names(usda[["tt.polygons"]])]
##   We also change the main title of the triangle
#usda[[ "main" ]] <- "My texture triangle"
#   In the code below, the order in which you give the labels 
#   in principle does not matter, as long as each old label 
#  obtain a new label
##   Now control the old names and the new names:
data.frame( "old" = old.labels, 
            "new" = names( usda[[ "tt.polygons" ]] ) )
##Save your new template
soiltexture::TT.set("USDA.TT" = usda)
##Check on your new template
soiltexture::TT.plot( class.sys = "USDA.TT" )


############ TRACER LE TERNAIRE #########
Table_TT_plot = cbind(CLAY, SILT, SAND)
row.names(Table_TT_plot) = Samples
pdf("Texture_diagram_without_labels.pdf", height=10,width=10) 
geo = TT.plot(class.sys   = "USDA.TT",
        main = "Texture diagram",
        #z.name = , #S'il y a une 4? colonne dans le data.frame pour une autre variable, on peut mettre ce nom.
        grid.show = FALSE, #montrer ou pas la grille tous les 10% (F : false)
        frame.bg.col = "white",
        #class.lab.show = "full",
        arrows.show = T,
        css.lab = c(
          sprintf("%% Clay (0 - %g µm)", clay_threshold),
          sprintf("%% Silt (%g - %g µm)", clay_threshold, coarsesilt_threshold),
          sprintf("%% Sand (%g - 2000 µm)", coarsesilt_threshold)
        ),
        #blr.clock = c(T,T,T), #	Vector of logicals, eventually with NA values. Direction of increasing texture values on the BOTTOM, LEFT and RIGHT axis, respectively. A value of TRUE means that the axis direction is clockwise. A value of FALSE means that the axis direction is counterclockwise. A value of NA means that the axis direction is centripetal. Possible combinations are c(T,T,T); c(F,F,F); c(F,T,NA) and c(T,NA,F), for fully clockwise, fully counterclockwise, right centripetal and left centripetal orientations, respectively.
        lang = "fr",
        cex.lab = 0.7)
TT.points(tri.data=Table_TT_plot, geo=geo, col="red", pch=1, cex = 1)
dev.off()

pdf("Texture_diagram_labels.pdf", height=10,width=10) 
geo = TT.plot(class.sys   = "USDA.TT",
              main = "Texture diagram",
              #z.name = , #S'il y a une 4? colonne dans le data.frame pour une autre variable, on peut mettre ce nom.
              grid.show = FALSE, #montrer ou pas la grille tous les 10% (F : false)
              frame.bg.col = "white",
              #class.lab.show = "full",
              arrows.show = T,
              css.lab = c(
                sprintf("%% Clay (0 - %g µm)", clay_threshold),
                sprintf("%% Silt (%g - %g µm)", clay_threshold, coarsesilt_threshold),
                sprintf("%% Sand (%g - 2000 µm)", coarsesilt_threshold)
              ),
              #blr.clock = c(T,T,T), #	Vector of logicals, eventually with NA values. Direction of increasing texture values on the BOTTOM, LEFT and RIGHT axis, respectively. A value of TRUE means that the axis direction is clockwise. A value of FALSE means that the axis direction is counterclockwise. A value of NA means that the axis direction is centripetal. Possible combinations are c(T,T,T); c(F,F,F); c(F,T,NA) and c(T,NA,F), for fully clockwise, fully counterclockwise, right centripetal and left centripetal orientations, respectively.
              lang = "fr",
              cex.lab = 0.7)
TT.points(tri.data=Table_TT_plot, geo=geo, col="red", pch=1, cex = 1)
TT.text(tri.data = Table_TT_plot, geo = geo, labels = row.names(Table_TT_plot), cex=0.7, font = 2, col = "blue")
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
  df = read.csv(file = paste0("./Raw data from machine/",file),header=FALSE, sep="\t",dec=".",col.names= paste0("V", seq_len(5)))
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
  
  ## cat1, cat2, cat3 et cat4 sont des vecteurs qui sont des bouts de la ligne 15, cat5 est un nouveau vecteur pour avoir directement la proportion d'Clay.
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
  cat5=c("% Clay (<7µm)",cat5,"")
  
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
Samples=row.names(resultats.df[3:nrow(resultats.df),])
X = resultats.df[1,14:106] %>% mutate_all(as.numeric)
Y_freq = resultats.df[3:nrow(resultats.df),14:106] %>% mutate_all(as.numeric)
Y_cumul = resultats.df[3:nrow(resultats.df),108:200] %>% mutate_all(as.numeric)
Data_freq = as.data.frame(cbind(Samples,Y_freq))
Data_cumul = as.data.frame(cbind(Samples,Y_cumul))
row.names(Data_freq) = Samples
row.names(Data_cumul) = Samples
colnames(Data_freq) = c("Sample",X)
colnames(Data_cumul) = c("Sample",X)
Freq_tidy = pivot_longer(Data_freq,cols = 2:94,names_to = "Size", values_to="Frequency")
Cumul_tidy = pivot_longer(Data_cumul,cols = 2:94,names_to = "Size", values_to="Cumulated")

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

