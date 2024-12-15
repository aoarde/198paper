### COHORT: WOMEN LIVING WITH OTHERS

#load packages
library(questionr)
library(dplyr)
library(labelled)
library(TraMineR) 
library(reshape2)
library(cluster)
library(WeightedCluster)
library(haven)
library(ggplot2)
library(descr)
library(RColorBrewer)
library(dplyr)
library(forcats)
install.packages("TraMineRextras")
library(TraMineRextras)

#set destination
getwd()
setwd("/Users/Mal/Desktop/198 PAPER")

#read data
VNM1 <- read_dta("1_cover_id_main.dta")
VNM2 <- read_dta("3_individual_id_main.dta")
VNM <- read_dta("4_diary_main.dta")

#group data first because easier to group using number representation
VNM$GrpAct <- "NA"
VNM$GrpAct [VNM$Q401==0101 | VNM$Q401==0102 | VNM$Q401==0103 | VNM$Q401==0104 | VNM$Q401==0198| VNM$Q401==0199 
            | VNM$Q401==0201| VNM$Q401==0202| VNM$Q401==0203| VNM$Q401==0204 | VNM$Q401==0205 | VNM$Q401==0298 | VNM$Q401==0299
            | VNM$Q401==0301| VNM$Q401==0302| VNM$Q401==0303| VNM$Q401==0304| VNM$Q401==0305| VNM$Q401==0398| VNM$Q401==0399
            | VNM$Q401==0401| VNM$Q401==0402 | VNM$Q401==0403| VNM$Q401==0404| VNM$Q401==0405| VNM$Q401==0498| VNM$Q401==0499
            | VNM$Q401==0501| VNM$Q401==0502| VNM$Q401==0503| VNM$Q401==0504| VNM$Q401==0505| VNM$Q401==0506| VNM$Q401==0507| VNM$Q401==0508| VNM$Q401==0509| VNM$Q401==0510| VNM$Q401==0598| VNM$Q401==0599] <- "At Work"
VNM$GrpAct[VNM$Q401==0601 | VNM$Q401==0602| VNM$Q401==0698 | VNM$Q401==0699 
           |VNM$Q401==0701 |VNM$Q401==0702|VNM$Q401==0798|VNM$Q401==0799|VNM$Q401==0801|VNM$Q401==0802|VNM$Q401==0803|VNM$Q401==0804|VNM$Q401==0805|VNM$Q401==0898|VNM$Q401==0899] <- "Unpaid Activities"
VNM$GrpAct[VNM$Q401==0901|VNM$Q401==0902|VNM$Q401==0903|VNM$Q401==0904|VNM$Q401==0905|VNM$Q401==0998|VNM$Q401==0999] <- "Education"
VNM$GrpAct[VNM$Q401==1001|VNM$Q401==1002|VNM$Q401==1003|VNM$Q401==1098|VNM$Q401==1099] <- "Socialization"
VNM$GrpAct[VNM$Q401==1101|VNM$Q401==1102|VNM$Q401==1103|VNM$Q401==1198|VNM$Q401==1199] <- "Attending Events"
VNM$GrpAct[VNM$Q401==1201|VNM$Q401==1202|VNM$Q401==1203|VNM$Q401==1298|VNM$Q401==1299] <- "Entertainment"
VNM$GrpAct[VNM$Q401==1301|VNM$Q401==1302|VNM$Q401==1398|VNM$Q401==1399] <- "Sports and Fitness"
VNM$GrpAct[VNM$Q401==1401|VNM$Q401==1402|VNM$Q401==1403|VNM$Q401==1404|VNM$Q401==1405|VNM$Q401==1498|VNM$Q401==1499] <- "Mass Media"
VNM$GrpAct[VNM$Q401==1501|VNM$Q401==1502|VNM$Q401==1503|VNM$Q401==1504|VNM$Q401==1505|VNM$Q401==1506|VNM$Q401==1598|VNM$Q401==1599] <- "Personal Care"
VNM$GrpAct[VNM$Q401==9998|VNM$Q401==9999] <- "Others / Unspecified"

VNM$GrpAct <- as_factor(VNM$GrpAct)
VNM$GrpAct <- fct_relevel(VNM$GrpAct, "At Work", "Unpaid Activities", "Education", "Socialization", 
                          "Attending Events", "Entertainment", "Sports and Fitness", 
                          "Mass Media", "Personal Care", "Others / Unspecified")

#make data labelled instead of categorical variables
VNM <- VNM %>% mutate(across(where(~ is.labelled(.)), as_factor))
VNM1 <- VNM1 %>% mutate(across(where(~ is.labelled(.)), as_factor))

#make new dataset
merge <- merge(VNM,VNM1, by="ID")
VNMMain <- subset(merge, select = c(ID, age.x, gender, TOTAL_HH_member, BEGIN, END, Duration, Q401, GrpAct))
VNMMain <- VNMMain %>% 
  dplyr::rename(ID = ID, 
                Age = age.x,
                Gender = gender,
                Number_of_Household_Members = TOTAL_HH_member,
                Begin_Time = BEGIN,
                End_Time = END,
                Duration = Duration,
                Main_Activity = Q401,
                Activity_Group = GrpAct)

#Cohort according to gender and hh members
VNMMain$COHORT <- "NA"
VNMMain$COHORT[VNMMain$Gender=="Female" & VNMMain$Number_of_Household_Members==1] <- "Women living alone"
VNMMain$COHORT[VNMMain$Gender=="Female" & VNMMain$Number_of_Household_Members>1] <- "Women living with others"
VNMMain$COHORT[VNMMain$Gender=="Male" & VNMMain$Number_of_Household_Members==1] <- "Men living alone"
VNMMain$COHORT[VNMMain$Gender=="Male" & VNMMain$Number_of_Household_Members>1] <- "Men living with others"

# Keep Women living alone
c_wlwo <- VNMMain %>% filter(VNMMain$COHORT == "Women living with others")

# Create sequences of activities per activity (result: long list of combined sequences)
sequences = rep(as.numeric(c_wlwo$Activity_Group), as.numeric(c_wlwo$Duration))

# Separate the long list into sequences (1440 min in each sequence)
seq = matrix(sequences, nrow = length(sequences) / 1440, ncol = 1440, byrow = TRUE)

# Specify the names for the activity variables
activities <- c()
for (i in 0:1439) {  # Change back to 1439 for 1440 columns
  activities <- c(activities, paste("var", i, sep = ""))
}

# Transform matrix to dataframe
data_wlwo <- as.data.frame(seq, row.names = unique(c_wlwo$ID))

# Use created variable names to name the columns in the dataframe
colnames(data_wlwo) <- activities

# Create ID
data_wlwo$id <- as.numeric(row.names(data_wlwo))

# Create time interval labels for each minute from 4:00 AM to 11:59 PM
start_time <- as.POSIXct("2021-11-08 04:00:00", tz = "GMT")  # 4:00 AM
end_time <- as.POSIXct("2021-11-09 03:59:00", tz = "GMT")    # 11:59 PM

# Create time interval labels
t_intervals_labels <- format(seq.POSIXt(start_time, end_time, by = "1 min"), "%H:%M", tz = "GMT")

# Check the first few time labels
head(t_intervals_labels)

### Define labels first and count:
labels <- c("At Work", "Unpaid Activities", "Education", "Socialization", 
            "Attending Events", "Entertainment", "Sports and Fitness", 
            "Mass Media", "Personal Care", "Others / Unspecified")
colourCount = length(labels)
getPalette = colorRampPalette(brewer.pal(8, "Set3"))

### let's see how our colours look like
axisLimit <- sqrt(colourCount)+1
colours=data.frame(x1=rep(seq(1, axisLimit, 1), length.out=colourCount), 
                   x2=rep(seq(2, axisLimit+1, 1), length.out=colourCount), 
                   y1=rep(1:axisLimit, each=axisLimit,length.out=colourCount), 
                   y2=rep(2:(axisLimit+1), each=axisLimit,length.out=colourCount), 
                   t=letters[1:colourCount], r=labels)

### Check the created pallette: 
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=colours, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5) +
  geom_text(data=colours, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) + 
  scale_fill_manual(values = getPalette(colourCount)) + theme(legend.position = "none")

# Convert the dataframe to a tibble 
final_wlwo <- as_tibble(data_wlwo) 

# Define the sequence object
seq_wlwo <- seqdef(final_wlwo,
                  var = activities,  
                  cnames = t_intervals_labels,  
                  alphabet = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),  
                  labels = labels,  
                  cpal = getPalette(colourCount),  
                  xtstep = 18,  
                  id = final_wlwo$id)  

seqiplot(seq_wlwo, border = NA, with.legend = "right", legend.prop=0.4)
seqIplot(seq_wlwo, border = NA, with.legend = "right", legend.prop=0.4, idxs = 1:4)

seqdplot(seq_wlwo, border = NA, with.legend = "right", legend.prop=0.4)

### Transitions
# transitions from state to state (in probabilities)
trate <- seqtrate(seq_wlwo)
round(trate, 2)
## heatmap of the transitions matrix
heatTrate=melt(trate)
head(heatTrate)
## plot the heatmap
ggplot(heatTrate, aes(Var2, Var1)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Transitions")

### Changing granularity
time15_seq_wlwo <- seqgranularity(seq_wlwo, tspan=15, method="mostfreq")
## plot the tempogram
seqdplot(time15_seq_wlwo, border = NA, with.legend = "right", legend.prop=0.4)

#transversal entropy of state distributions
#the number of valid states and the Shannon entropy of the transversal state distribution
# shows the measure of 'chaos' (diversity of activities) in the diaries
seqHtplot(time15_seq_wlwo, with.legend = "right", legend.prop=0.4)

# seqdist() = for pairwise dissimilarities
# seqsubm() = to compute own substitution matrix
#"TRATE" option, the costs are determined from the estimated transition rates
scost <- seqsubm(time15_seq_wlwo, method = "TRATE")
round(scost, 3)
## calculated in this way, all are close to 2 anyway (for this dataset) 2 is default
## or we can use the usual default one of constant 2:
ccost <- seqsubm(time15_seq_wlwo, method="CONSTANT", cval=2)
round(ccost, 3)
