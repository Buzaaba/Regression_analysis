# Regression_analysis
## A. Table 4 Consumption

# Regression Analysis 
#1. Linear regression for consumption and Demographic
### Simple linear regression
df <- read.csv("Demog_Consump.csv")

summary(df)

# Checking NA's
dfmeans <- apply(df, 2, mean, na.rm=T)


plot(In_Call~Male,df)

fit1=lm(Calls_Per_BTS~Youth....35+Male+Middle_Age.35...65+Elidery....65,data=df2)
summary(fit1)
fit1
fit2=lm(Duration_Average~Male+Youth....35+Middle_Age.35...65+Elidery....65,data=df2)
summary(fit2)

fit3=lm(In_Call~Male+Female+Youth....35+Middle_Age.35...65+Elidery....65,data=df2)
summary(fit3)

# Regression Analysis 
#1. Linear regression for consumption and Education
# Load Data Education level

df_Education <- read.csv("V_Education.csv")

# filter No Education/Illiterate
No_Education = df_Education %>% filter(Education_level == 0)
write.csv(No_Education, file = "Illiterate.csv")


# filter primary

P_Education = df_Education %>% filter(Education_level == 1)
write.csv(P_Education, file = "Primary.csv")
Prim <- read.csv("Primary.csv")
# filter Secondary

S_Education = df_Education %>% filter(Education_level == 2)
write.csv(S_Education, file = "Secondary.csv")
Sec <- read.csv("Secondary.csv")
# filter University

U_Education = df_Education %>% filter(Education_level == 3)
write.csv(U_Education, file = "University.csv")
Uni <- read.csv("University.csv")

# Education file 

Educ <- read.csv("Edu.csv")
names(Educ)
colnames(Educ) <- c("Cluster")

# Merge all Education Levels with Edu
#1. Illiterate

Educ1 <- merge(Educ, No_Education, by = "Cluster", all = TRUE, sort = FALSE)
Educ2 <- merge(Educ1, Prim, by = "Cluster1", all = TRUE, sort = FALSE)
Educ3 <- merge(Educ2, Sec, by = "Cluster2", all = TRUE, sort = FALSE)
Educ4 <- merge(Educ3, Uni, by = "Cluster3", all = TRUE, sort = FALSE)
head(Educ4)

write.csv(Educ4, file = "Level_Educatio.csv")


# Loading Education level variable data

Education1 <- read.csv("Level_Educatio.csv")
names(Education1)

# Loading Consumption variable

Consumption <- read.csv("Cons_cluster_Location.csv")
names(Consumption)

# Merging the two by GPS cordianate latnum
Consu_Educ <- merge(Consumption, Education1, by = "latnum", all = TRUE, sort = FALSE)
write.csv(Consu_Educ, file = "Consuption_Education.csv")


# Regression Analysis 
#1. Linear regression for consumption and Demographic

summary(Consu_Educ)
Consu_Educ1 <- na.omit(Consu_Educ)

fit_Edu1 =lm(Calls_Per_BTS~Illiterate +Primary+Secondary+University,data=Consu_Educ)
summary(fit_Edu1)

fit_Edu2=lm(Duration_Average~Illiterate +Primary+Secondary+ University,data=Consu_Educ)
summary(fit_Edu2)

fit_Edu3=lm(In_Call~Illiterate +Primary+Secondary+University,data=Consu_Educ)
summary(fit_Edu3)

# Property ownership file

# 1.

Owns_Car <- read.csv("Own_car.csv")
Owns_Car1 <- Owns_Car[!(Owns_Car$has_Car == 0),] # for Mergeing
Owns_Car2 <- subset(Owns_Car1, select=c("Cluster", "Percent"))

#No_Car <- O_Frigo[!(O_Frigo$has_Refrigirator == 1),] # for Merging

#names(Owns_Car)

# 2.
Owns_Moto <- read.csv("Own_Moto.csv")
Owns_Moto1 <- Owns_Moto[!(Owns_Moto$has_Moto == 0),] # for Mergeing
Owns_Moto2 <- subset(Owns_Moto1, select=c("Cluster", "Percent"))
colnames(Owns_Moto2) <- c("Cluster1", "Percent")

# 3.
Owns_Bicycle <- read.csv("Own_Bicycle.csv")
Owns_Bicycle1 <- Owns_Bicycle[!(Owns_Bicycle$has_Bicycle == 0),] # for Mergeing
Owns_Bicycle2 <- subset(Owns_Bicycle1, select=c("Cluster", "Percent"))
colnames(Owns_Bicycle2) <- c("Cluster2", "Percent")


# 4.
Owns_Electricity <- read.csv("Own_Electricity.csv")
Owns_Electricity1 <- Owns_Electricity[!(Owns_Electricity$has_Electricity == 0),] # for Mergeing
Owns_Electricity2 <- subset(Owns_Electricity1, select=c("Cluster", "Percent"))
colnames(Owns_Electricity2) <- c("Cluster3", "Percent")


# 5.
Owns_Frigo <- read.csv("Own_Refrigirator.csv")
Owns_Frigo1 <- Owns_Frigo[!(Owns_Frigo$has_Refrigirator == 0),] # for Mergeing
Owns_Frigo2 <- subset(Owns_Frigo1, select=c("Cluster", "Percent"))
colnames(Owns_Frigo2) <- c("Cluster4", "Percent")


#5 TV

Owns_TV <- read.csv("Own_TV.csv")
Owns_TV1 <- Owns_TV[!(Owns_TV$has_TV == 0),] # for Mergeing
Owns_TV2 <- subset(Owns_TV1, select=c("Cluster", "Percent"))
colnames(Owns_TV2) <- c("Cluster5", "Percent")


# Radio

Owns_Radio <- read.csv("Own_Radio.csv")
Owns_Radio1 <- Owns_Radio[!(Owns_Radio$has_Radio == 0),] # for Mergeing
Owns_Radio2 <- subset(Owns_Radio1, select=c("Cluster", "Percent"))
colnames(Owns_Radio2) <- c("Cluster6", "Percent")


# Computer

Owns_Computer <- read.csv("Own_Computer.csv")
Owns_Computer1 <- Owns_Computer[!(Owns_Computer$Own_computer== 0),] # for Mergeing
Owns_Computer2 <- subset(Owns_Computer1, select=c("Cluster", "Percent"))
colnames(Owns_Computer2) <- c("Cluster9", "Percent")

# Merging property and Consumption
#Load Data
#1. Consumption Variable
df_Consu <- read.csv("Usage.csv")
df_Consu1 <- select(df_Consu, -1, -2, -4)
df_Consu1_Uni <- df_Consu1[!duplicated(df_Consu1$Lat),]

#2. Cordinates
df_cor <- read.csv("Cluster_bts.csv")
df_cor1 <- select(df_cor, -1)
df_cor1_Uni <- df_cor1[!duplicated(df_cor1$latnum),]

#3. Property Ownership

Prop <- read.csv("All_property.csv")
#Merge with cordinates
Cons_Prop <- merge(df_cor1_Uni, Prop, by = "latnum", all = TRUE, sort = FALSE)
Cons_Prop1 <- merge(df_Consu1_Uni, Cons_Prop, by = "Lat", all = TRUE, sort = FALSE)

Cons_prop2 <- na.omit(Cons_Prop1)

# Regression Analysis 

#1. Linear regression for consumption and Property ownership


summary(Cons_Prop1)
#Consu_Educ1 <- na.omit(Consu_Educ)
# Calls per BTS
fit_Prop1 =lm(Calls_Per_BTS~Bicycle+Moto+Car+TV+Radio+Electricity+Refrigirator+Computer, data=Cons_Prop1)
summary(fit_Prop1)

# Call_Duration
fit_prop2=lm(Duration_Average~Bicycle+Moto+Car+TV+Radio+Electricity+Refrigirator+Computer,data=Cons_Prop1)
summary(fit_prop2)

# Out going calls
fit_Prop3=lm(In_Call~Bicycle+Moto+Car+TV+Radio+Electricity+Refrigirator+Computer,data=Cons_Prop1)
summary(fit_Prop3)


## B. Table 5 Social network Variable
# Second Indepent Variable: Social network Variable

df_Social1 <- read.csv("Reciprocal_calls.csv")

#. Customers who recieve R1 atleast 1 call

Rec1 <- read.csv("At least 1 call.csv")
Rec_2 <- select(Rec1, -1, -2, -4, -6)
#. Merge with cordinates 
CRec_2 <- merge(df_cor, Rec_2, by = "Lat", all = TRUE, sort = FALSE)

#2. R2 atleast 2 calls

Reci2 <- read.csv("Calls_2.csv")
Rec_2C <- select(Reci2, -1, -2, -4, -6)
CRec_2C <- merge(df_cor, Rec_2C, by = "Lat", all = TRUE, sort = FALSE)

#3. R5 atleast 5 calls

Reci5 <- read.csv("Calls_5.csv")
Rec_5C <- select(Reci5, -1, -2, -4, -6)
CRec_5C <- merge(df_cor, Rec_5C, by = "Lat", all = TRUE, sort = FALSE)


#. Merging proporty ownership and reciprocals
R1 <- merge(CRec_2, Prop, by = "latnum", all = TRUE, sort = FALSE)
R2 <- merge(CRec_2C, Prop, by = "latnum", all = TRUE, sort = FALSE)
R5 <- merge(CRec_5C, Prop, by = "latnum", all = TRUE, sort = FALSE)

# Regression Analysis 

#1. Linear regression for consumption and Property ownership


#. Reciprocal and Demography
#1. Atleast 1 call and Demography
#RD_1 atleast 1
#RD_2 at least 2
#RD_5 at least 5
RD_1 <- merge(CRec_2, Demography, by = "latnum", all = TRUE, sort = FALSE)
RD_2 <- merge(CRec_2C, Demography, by = "latnum", all = TRUE, sort = FALSE)
RD_5 <- merge(CRec_5C, Demography, by = "latnum", all = TRUE, sort = FALSE)


# Regression for Reciprocal and Demography

fit_RD1 =lm(count1~Male+Female+Youth....35+Middle_Age.35...65+Elidery....65, data=RD_1)
summary(fit_RD1)

fit_RD2 =lm(count2~Male+Female+Youth....35+Middle_Age.35...65+Elidery....65, data=RD_2)
summary(fit_RD2)

# Call_Duration
fit_RD5 =lm(count5~Male+Female+Youth....35+Middle_Age.35...65+Elidery....65, data=RD_5)
summary(fit_RD5)

#. Reciprocal and Education level

#. Load data Education level
Educ_level <- read.csv("Level_Educatio.csv")
# RE_1 At least 1
# RE_2 at least 2
# RE_5 at least 5
RE_1 <- merge(CRec_2, Educ_level, by = "latnum", all = TRUE, sort = FALSE)
RE_2 <- merge(CRec_2C, Educ_level, by = "latnum", all = TRUE, sort = FALSE)
RE_5 <- merge(CRec_5C, Educ_level, by = "latnum", all = TRUE, sort = FALSE)


# Regression for Reciprocal and Education

fit_RE1 =lm(count1~Illiterate+Primary+Secondary+University, data=RE_1)
summary(fit_RE_1)

fit_RE2 =lm(count2~Illiterate+Primary+Secondary+University, data=RE_2)
summary(fit_RE2)

fit_RE5 =lm(count5~Illiterate+Primary+Secondary+University, data=RE_5)
summary(fit_RE5)

# Number of calls one makes

C_Numbers <- read.csv("Number of calls one makes.csv")
C_Numbers_Lat <- merge(df, C_Numbers, by = "btsid", all = TRUE, sort = FALSE)
C_Numbers_Lat_Final <- na.omit(C_Numbers_Lat)


## Table 6: Mobility Variable
#1, Number of different sites visited by an individual

S_visit <- read.csv("Caller_sites.csv")
S_visit <- select(S_visit, -1)
Site_location <- read.csv("df_bts.csv")
Site_location <- select(Site_location, -1, -2, -4, -6, -7, -8, -9)
Site_location_uni <- Site_location[!duplicated(Site_location$Caller),]

# Merging number of sites visted and Lat by caller
S_visited <- merge(Site_location_uni, S_visit, by = "Caller", all = TRUE, sort = FALSE)

# Merging sites visited and property ownership dataset
NSite_property <- merge(SV_latnum, Property, by = "latnum", all = TRUE, sort = FALSE)

# Regression for Total sites visited and property ownership, Demography and Education level

fit_SV =lm(sites_visted~., data=NSite_property)
summary(fit_SV)

fit_SV =lm(sites_visted~., data=Demo_SVisit)
summary(fit_SV)

fit_SE =lm(sites_visted~., data=Educ_level)
summary(fit_SE)

# Radius of gyration (Number of visits to a particular site) by an individua

RGyration <- read.csv("Radius of gyration.csv")

# Regression for Radius of gyration and property, Demography and Education level

fit_RG =lm(Frequency~., data=RGY_Prop)
summary(fit_RG)

fit_RG =lm(Frequency~., data=RGY_Demo)
summary(fit_RG)

fit_RGE =lm(Frequency~., data=RGY_Educ)
summary(fit_RGE)
