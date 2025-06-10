# Paper Title: Socially Situated Transmission: The Bias to Transmit Negative Information is Moderated by the Social Context
# Authors: Fay, N., Walker, B., Kashima, Y., & Perfors, A.

# Data: EventValenceRatings

# Data: NegativityData
# Social Context: Repeated Reproduction, Reproduction Intent, Communication Intent, Social Interaction
# Chain Position: 1-4


# Libraries
library(psych) # Descriptive Statistics by Group using describeBy
library(tidyverse)
library(lmerTest)  # Data Analysis: Mixed Effects Modeling
library(ordinal) # Data Analysis: Mixed Effects Modeling for Ordinal Data
library(cowplot) # To combine Figures
library(yarrr) # Colour palette


# Participant & Chain Information
# --------------------------------------
# Participant Age Information
Summary <- group_by(NegativityData, pID)

# Capture Age for Each particpant
ParticipantAge <- summarize(Summary, count = n(), 
                Age = mean(Age, na.rm = T))
# (N=425)

ParticipantAge %>%
  filter(!is.na(Age)) %>%
  summarise_each(list(mean, sd, min, max), Age)
# Mean=22.5, SD=6.91, Min=18, Max=60

# Participant Gender Information
ParticipantGender <- unique(NegativityData[,c('pID','Gender')])
table(ParticipantGender$Gender)
# F=307, M=114, Fluid=1, Non-binary=1, NA=2

# #N Participants per SocialContext
Nparticipant <- unique(NegativityData[,c('pID','SocialContext')])
table(Nparticipant$SocialContext)

# #N Chains per SocialContext
Nchain <- unique(NegativityData[,c('ChainID','SocialContext')])
table(Nchain$SocialContext)

# -----------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# EVENT VALENCE: Compare Rated Strength of Positive & Negative Events
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

# CHECKING FOR OUTLIERS
# ----------------------

# Figure: Boxplot
# -------------------
# Order Conditions
EventValenceRatings$Valence <- factor(EventValenceRatings$Valence, levels = c("Positive", "Negative"))

BoxplotFig <- ggplot(EventValenceRatings, aes(x=Valence, y=Strength)) + theme_bw() 
BoxplotFig <- BoxplotFig +  geom_boxplot(aes(color = Valence, fill = Valence), outlier.size = 2.5, alpha = 0.5)
BoxplotFig <- BoxplotFig + labs(x="Event Valence", y="Event Valence Ratings (-50 to 50)") + 
  theme(title= element_text(size=14, face='bold'), 
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'))
# Change Legend Position & Increase Legend Font 
BoxplotFig <- BoxplotFig + theme(legend.position = "none", legend.text = element_text(size = 25, colour = "black"))
# Change y-axis scale
BoxplotFig <- BoxplotFig + expand_limits(y=c(-50,50)) + scale_y_continuous(breaks=seq(-50, 50, 10))
# Change Colour Manually
BoxplotFig <- BoxplotFig + scale_color_manual(values=c("#006A40FF", "#F08892FF"))
BoxplotFig <- BoxplotFig + scale_fill_manual(values=c("#006A40FF", "#F08892FF"))
BoxplotFig


# ****Outliers Identified****
# Winsorize Outliers Using the Inter-Quartile Range Rule (IQR). 
# Done Seperately for Positive & Negative Events

# Positive Events
# ----------------
PositiveEvents <- filter(EventValenceRatings, Valence=='Positive')

# Outliers in Positive Event Ratings
boxplot(PositiveEvents$Strength, plot=FALSE)$out

# Vector of Outliers (10 in total, or 2.27%) for Positive Event Ratings
PositiveOutliers <- boxplot(PositiveEvents$Strength, plot=FALSE)$out

# Remove Outliers from Positive Events DataFrame (using which() function)
PositiveEventsWINSORIZED <- PositiveEvents[-which(PositiveEvents$Strength %in% PositiveOutliers),]


# Negative Events
# ----------------
NegativeEvents <- filter(EventValenceRatings, Valence=='Negative')

# Outliers in Positive Event Ratings
boxplot(NegativeEvents$Strength, plot=FALSE)$out

# Vector of Outliers (16 in total, or 3.63%) for Positive Event Ratings
NegativeOutliers <- boxplot(NegativeEvents$Strength, plot=FALSE)$out

# Remove Outliers from Positive Events DataFrame (using which() function)
NegativeEventsWINSORIZED <- NegativeEvents[-which(NegativeEvents$Strength %in% NegativeOutliers),]


# Combine (Vertically) Winsorised Positive & Negative Event Ratings into a Single DataFrame
# ------------------------------------------------------------------------------------------
EventValenceRatingsWINSORIZED <- rbind(PositiveEventsWINSORIZED, NegativeEventsWINSORIZED)



# DESCRIPTIVE STATISTICS
# -----------------------
# Participant #
length(unique(EventValenceRatingsWINSORIZED$pID))
# 55

# Descripitive Stats for Strength Ratings (Positive, Negative)
describeBy(EventValenceRatingsWINSORIZED$Strength, 
           list(EventValenceRatingsWINSORIZED$Valence), 
           mat=TRUE, digits=2)
# Positive: M=34.39, SD=15.30
# Negative: M=-32.81, SD=15.67

# Descripitive Stats for ReverseCoded Strength Ratings (Positive, Negative)
describeBy(EventValenceRatingsWINSORIZED$Strength_ReverseCoded, 
           list(EventValenceRatingsWINSORIZED$Valence), 
           mat=TRUE, digits=2)
# Positive: M=34.39, SD=15.30
# Negative: M=32.81, SD=15.67



# Figure: Histogram
# -------------------
HistrogramFig <- ggplot(EventValenceRatingsWINSORIZED, aes(x=Strength)) + theme_bw()
HistrogramFig <- HistrogramFig + geom_histogram(aes(color = Valence, fill = Valence), alpha = 0.5, position = "identity", bins=25)
HistrogramFig <- HistrogramFig + labs(x="Event Valence Rating (-50 to 50)", y="Rating Frequency") + 
  theme(title= element_text(size=14, face='bold'), 
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'))
# Change Legend Position & Increase Legend Font 
HistrogramFig <- HistrogramFig + theme(legend.position = "none", legend.text = element_text(size = 25, colour = "black"))
# Change y-axis scale
HistrogramFig <- HistrogramFig + expand_limits(y=c(0,150)) + scale_y_continuous(breaks=seq(0, 150, 10))
# Change x-axis scale
HistrogramFig <- HistrogramFig + expand_limits(x=c(-50,50)) + scale_x_continuous(breaks=seq(-50, 50, 10))
# Change Colour Manually
HistrogramFig <- HistrogramFig + scale_color_manual(values=c("#006A40FF", "#F08892FF"))
HistrogramFig <- HistrogramFig + scale_fill_manual(values=c("#006A40FF", "#F08892FF"))
# Add Legend
HistrogramFig <- HistrogramFig + theme(legend.position = c(0.5, 0.95), legend.direction = "horizontal", 
                                       legend.title = element_blank(), 
                                       legend.key = element_rect(fill = "white", colour = "black"), 
                                       legend.text = element_text(face="bold", size = 14, colour = "black"))
HistrogramFig


# DotPlot Figures
# ----------------
# By-Participant Means (Winsorized & ReverseCoded)
# -------------------------------------------------
# Group Data by Participant & Valence
EventValenceRatings_ByParticipant <- group_by(EventValenceRatingsWINSORIZED, pID, Valence)

# Capture Means for the Reverse-Coded Ratings (by Participant)
EventValenceRatings_ByParticipant_MEAN <- summarize(EventValenceRatings_ByParticipant, count = n(), 
                                              Strength_ReverseCoded = mean(Strength_ReverseCoded, na.rm = T))

# Order Conditions
EventValenceRatings_ByParticipant_MEAN$Valence <- factor(EventValenceRatings_ByParticipant_MEAN$Valence, levels = c("Positive", "Negative"))


# By-Event Means (Winsorized & ReverseCoded)
# -------------------------------------------------
# Group Data by Event & Valence
EventValenceRatings_ByEvent <- group_by(EventValenceRatingsWINSORIZED, Event, Valence)


# Capture Means for the Reverse-Coded Ratings (by Participant)
EventValenceRatings_ByEvent_MEAN <- summarize(EventValenceRatings_ByEvent, count = n(), 
                                                    Strength_ReverseCoded = mean(Strength_ReverseCoded, na.rm = T))


# Order Conditions
EventValenceRatings_ByEvent_MEAN$Valence <- factor(EventValenceRatings_ByEvent_MEAN$Valence, levels = c("Positive", "Negative"))



# Dotplot Valence Figure (Winsorized & ReverseCoded)
# ---------------------------------------------------
EventValenceRatings_MEANFig <- ggplot(data=EventValenceRatings_ByParticipant_MEAN, aes(x=Valence, y=Strength_ReverseCoded))
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + stat_summary(fun=mean, geom="bar", position ="dodge", alpha=0.5, aes(fill=Valence))
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + theme_bw()
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + geom_violin(alpha = 0.5, color='grey50')
# Participant Mean Valence Ratings
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + geom_dotplot(aes(x=Valence, y=Strength_ReverseCoded, fill=Valence), 
                                                                        binaxis='y', stackdir='center', dotsize=1, alpha = 1, stroke=0.75, colour='grey15')
# Event Mean Valence Ratings
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + geom_point(data=EventValenceRatings_ByEvent_MEAN, aes(x=Valence, y=Strength_ReverseCoded, fill=Valence), 
                                                                       shape=24, size=3.5, alpha = 0.5, stroke=1, colour='black', position = position_nudge(x = +0.35))
# Add Boostrapped 95% CIs (normality NOT assumed) - Doing this AFTER adding the geom_points ensures the CIs are Forward in the Figure
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + stat_summary(fun.data=mean_cl_boot, geom='errorbar', 
                                                                        position = position_dodge(width=0.9), 
                                                                        color='grey15', size=1, width=0.4)
# Labels & Text Size
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + labs(x="Event Valence", y="Mean Valence Ratings for Positive and Negative Events") + 
  theme(title= element_text(size=14, face='bold'), 
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'))
# Change Legend Position & Increase Legend Font 
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + theme(legend.position = "none", legend.text = element_text(size = 25, colour = "black"))
# Change y-axis scale
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + coord_cartesian(ylim=c(0, 50)) + scale_y_continuous(breaks=seq(0, 50, 5))  
# Change Colour Manually
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + scale_color_manual(values=c("#006A40FF", "#F08892FF"))
EventValenceRatings_MEANFig <- EventValenceRatings_MEANFig + scale_fill_manual(values=c("#006A40FF", "#F08892FF"))                                                                      
EventValenceRatings_MEANFig


# COMBINE FIGURES
# ----------------
EventValenceFigsCombined <- plot_grid(BoxplotFig, HistrogramFig, EventValenceRatings_MEANFig, labels = c('A', 'B', 'C'), label_size = 20, nrow=1)
EventValenceFigsCombined

# Exported 700h x 1400w


# CUMULATIVE LINK MIXED EFFECTS MODEL
# ---------------------------------------
# Specify Response as a Factor
EventValenceRatingsWINSORIZED$Strength_ReverseCoded.F <- as.factor(EventValenceRatingsWINSORIZED$Strength_ReverseCoded)

# MAXIMAL Model
ValenceStrengthCLMM <- clmm (Strength_ReverseCoded.F ~ Valence + (1 + Valence | pID) + 
                               (1 | Event), data=EventValenceRatingsWINSORIZED)
summary(ValenceStrengthCLMM)
# Non-significant, (B=0.09, SE=0.26, Z=0.36, p=.721)



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# SURVIVAL OF POSITIVE & NEGATIVE EVENTS
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

# CREATE FIGURE
# -----------------
# Filter Positive & Negative statments
PosNeg <- filter(NegativityData, Valence %in% c("Positive", "Negative"))

# Calculate Mean %
# Group Data
PosNeg <- group_by(PosNeg, pID, ChainID, ChainPosition, SocialContext, Valence)

# Capture Means for the Distance Scores
PosNegPercent <- summarize(PosNeg, count = n(), 
                           Present = mean(Present, na.rm = T)
                           )

# Change to %
PosNegPercent$Present <- PosNegPercent$Present*100


# EVENT SURVIVAL FIGURE (FIGURE 1A)
# -----------------------------------
# Show available colour palattes
piratepal(palette = "all")
# Use this palatte
piratepal(palette = "info2") #Use this



# Change Order of SocialContexts:
PosNegPercent$SocialContext <- factor(PosNegPercent$SocialContext, levels = c("Repeated Reproduction", "Reproduction Intent",
                                                                      "Communication Intent", "Social Interaction"))

# Specify Valence as a Factor:
PosNegPercent$Valence <- factor(PosNegPercent$Valence, levels = c("Positive", "Negative"))


SurvivalFig <- ggplot (PosNegPercent, aes(x=ChainPosition, y=Present, group=ChainID)) + facet_wrap(~SocialContext, ncol=2)
SurvivalFig <- SurvivalFig + geom_line(aes(group=interaction(ChainID, Valence), color=Valence), 
                                 size=0.4, alpha=0.3, position = position_jitter(w=0, h=2))
# Labels & Text Size
SurvivalFig <- SurvivalFig + labs(x= "Chain Position", y= "Event Survival (%)") +  
  theme_bw() +
  theme(title= element_text(size=14, face='bold'),
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'),
        legend.title = element_text(size = 14, face='bold'),
        legend.text = element_text(size = 16, face='bold'))
# Change x-axis scale
SurvivalFig <- SurvivalFig + coord_cartesian(xlim=c(1, 4)) + 
  scale_x_continuous(breaks=seq(1, 4, 1))  # X-axis from 1-4 with 1-4 tick marks every 1
# Change y-axis scale
SurvivalFig <- SurvivalFig + coord_cartesian(ylim=c(0, 100)) + 
  scale_y_continuous(breaks=seq(0, 100, 20))  # Y-axis from 0-100 with 0-100 tick marks every 10 
# Add Geom Smooth 
SurvivalFig <- SurvivalFig + geom_smooth(method= "lm", aes(group=Valence, colour=Valence, fill=Valence), size=1.25, alpha = 0.5)
# Add mean points for each SocialContext
SurvivalFig <- SurvivalFig + stat_summary(fun=mean, geom="point", size=4, shape=21, stroke=0.5, colour='grey15',
                                    aes(group =Valence, fill=Valence))
# Change Legend Position & remove Legend title
SurvivalFig <- SurvivalFig + theme(legend.position="top") + theme(legend.title=element_blank())
# Change Colour Manually
SurvivalFig <- SurvivalFig + scale_color_manual(values=c("#006A40FF", "#F08892FF"))
SurvivalFig <- SurvivalFig + scale_fill_manual(values=c("#006A40FF", "#F08892FF"))
SurvivalFig
#---------------------------------------------------------------------------------------------


# STATISTICAL ANALYSIS: Unambiguous Events
# -----------------------------------------
# Logistic Mixed Effects Modeling

# ********************************** TRIAL USING OMNIBUS TEST **********************************

# Analysis 1: Repeated Reproduction vs. Reproduction Intent
Chain1to3 <- filter(NegativityData, ChainPosition != 4, Valence %in% c("Positive", "Negative"))

# Centre IVs
Chain1to3$Valence.C <- scale (Chain1to3$Valence.N, center=TRUE, scale=FALSE)
Chain1to3$ChainPosition.C <- scale (Chain1to3$ChainPosition, center=TRUE, scale=FALSE)
Chain1to3$EventPosition.C <- scale (Chain1to3$EventPosition, center=TRUE, scale=FALSE)

# Factor Code Social Context & Set Social Interaction as the Reference Group
Chain1to3$SocialContext <- factor(Chain1to3$SocialContext, levels = c("Repeated Reproduction", "Reproduction Intent", "Communication Intent", "Social Interaction"))
Chain1to3$SocialContext <- relevel(Chain1to3$SocialContext, ref = "Social Interaction")


# Mixed Effects Model (Model fails to converge)
Chain1to3GLMER <- glmer (Present~ SocialContext*Valence.C*ChainPosition.C + poly(EventPosition.C, 2) +
                           (1 | Event), data=Chain1to3, family=binomial)
summary(Chain1to3GLMER)

# ISSUE IS THAT THE MODEL IS TOO COMPLEX AND THEREFORE DOES NOT CONVERGE.  I COULD POINT OUT THAT THE INTERACITON EFFECT IS SEEN EACH TIME
# THE SOCIAL INTERACTION CONDITION IS COMPARED TO THE OTHER CONDITIONS


# ********************************** TRIAL USING OMNIBUS TEST **********************************


# -----------------------------------------------------------------------------------------------
# Analysis 1: Repeated Reproduction vs. Reproduction Intent
Analysis1 <- filter(NegativityData, SocialContext %in% c("Repeated Reproduction", "Reproduction Intent"), 
                    Valence %in% c("Positive", "Negative"))

# Centre IVs
Analysis1$SocialContext.C <- scale (Analysis1$SocialContext.N, center=TRUE, scale=FALSE)
Analysis1$Valence.C <- scale (Analysis1$Valence.N, center=TRUE, scale=FALSE)
Analysis1$ChainPosition.C <- scale (Analysis1$ChainPosition, center=TRUE, scale=FALSE)
Analysis1$EventPosition.C <- scale (Analysis1$EventPosition, center=TRUE, scale=FALSE)


# Mixed Effects Model with EventPosition as a Covariate (Linear & Quadratic)
# Random Effects Structure simplified to converge: 
#   ChainPosition removed from ChainID
#   SocialContext removed from Event
Analysis1GLMER <- glmer (Present~ SocialContext.C*Valence.C*ChainPosition.C + poly(EventPosition.C, 2) +
                           (1 + Valence.C | ChainID) + 
                           (1 | Event), data=Analysis1, family=binomial)
summary(Analysis1GLMER)


# Best Fitting Model
# --------------------
Analysis1BestFit <- glmer (Present~ SocialContext.C + Valence.C + SocialContext.C*ChainPosition.C +
                             (1 + Valence.C | ChainID) + 
                             (1 | Event), data=Analysis1, family=binomial)
summary(Analysis1BestFit)


# Understanding the SocialContext by Position Interaction
# -----------------------------------------------------
# Repeated Reproduction
R.Reproduction <- filter(NegativityData, SocialContext == "Repeated Reproduction", 
                                  Valence %in% c("Positive", "Negative"))

R.Reproduction$ChainPosition.C <- scale (R.Reproduction$ChainPosition, center=TRUE, scale=FALSE)

R.ReproductionGLMER <- glmer (Present~ ChainPosition.C + (1 | ChainID) + (1 | Event), data=R.Reproduction, family=binomial)
summary(R.ReproductionGLMER) # P=0.564


# Reproduction Intent
R.Intent <- filter(NegativityData, SocialContext == "Reproduction Intent", 
                     Valence %in% c("Positive", "Negative"))

R.Intent$ChainPosition.C <- scale (R.Intent$ChainPosition, center=TRUE, scale=FALSE)

R.IntentGLMER <- glmer (Present~ ChainPosition.C + (1 | ChainID) + (1 | Event), data=R.Intent, family=binomial)
summary(R.IntentGLMER) # B = -0.34, P < 0.001


# -----------------------------------------------------------------------------------------------

# Analysis 2: Reproduction Intent vs. Communication Intent
Analysis2 <- filter(NegativityData, SocialContext %in% c("Reproduction Intent", "Communication Intent"), 
                    Valence %in% c("Positive", "Negative"))

# Centre IVs
Analysis2$SocialContext.C <- scale (Analysis2$SocialContext.N, center=TRUE, scale=FALSE)
Analysis2$Valence.C <- scale (Analysis2$Valence.N, center=TRUE, scale=FALSE)
Analysis2$ChainPosition.C <- scale (Analysis2$ChainPosition, center=TRUE, scale=FALSE)
Analysis2$EventPosition.C <- scale (Analysis2$EventPosition, center=TRUE, scale=FALSE)


# Mixed Effects Model with EventPosition as a Covariate (Linear & Quadratic)
# Random Effects Structure simplified to converge: 
#   ChainPosition removed from ChainID
#   SocialContext removed from Event
Analysis2GLMER <- glmer (Present~ SocialContext.C*Valence.C*ChainPosition.C + poly(EventPosition.C, 2) +
                           (1 + Valence.C | ChainID) + 
                           (1 | Event), data=Analysis2, family=binomial)
summary(Analysis2GLMER)


# Best Fitting Model
Analysis2BestFit <- glmer (Present~ Valence.C + ChainPosition.C + SocialContext.C*ChainPosition.C +
                             (1 + Valence.C | ChainID) +
                             (1 | Event), data=Analysis2, family=binomial)
summary(Analysis2BestFit)


# Understanding the SocialContext by Position Interaction
# -----------------------------------------------------
# Communication Intent 
C.Intent <- filter(NegativityData, SocialContext == "Communication Intent", 
                     Valence %in% c("Positive", "Negative"))

C.Intent$ChainPosition.C <- scale (C.Intent$ChainPosition, center=TRUE, scale=FALSE)

C.IntentGLMER <- glmer (Present~ ChainPosition.C + (1 | ChainID) + (1 | Event), data=C.Intent, family=binomial)
summary(C.IntentGLMER) # # B = -0.54, P < 0.001


# -----------------------------------------------------------------------------------------------

# Analysis 3: Communication Intent vs Social Interaction
Analysis3 <- filter(NegativityData, SocialContext %in% c("Communication Intent", "Social Interaction"), 
                    Valence %in% c("Positive", "Negative"), ChainPosition != 4)

# Centre IVs
Analysis3$SocialContext.C <- scale (Analysis3$SocialContext.N, center=TRUE, scale=FALSE)
Analysis3$Valence.C <- scale (Analysis3$Valence.N, center=TRUE, scale=FALSE)
Analysis3$ChainPosition.C <- scale (Analysis3$ChainPosition, center=TRUE, scale=FALSE)
Analysis3$EventPosition.C <- scale (Analysis3$EventPosition, center=TRUE, scale=FALSE)


# Mixed Effects Model with EventPosition as a Covariate (Linear & Quadratic)
# Random Effects Structure simplified to converge: 
#   Valence & ChainPosition removed from ChainID
#   SocialContext removed from Event
Analysis3GLMER <- glmer (Present~ SocialContext.C*Valence.C*ChainPosition.C + poly(EventPosition.C, 2) +
                           (1 | ChainID) + 
                           (1 | Event), data=Analysis3, family=binomial)
summary(Analysis3GLMER)


# Best Fitting Model
Analysis3BestFit <- glmer (Present~ SocialContext.C*Valence.C + ChainPosition.C +
                             (1 | ChainID) +
                             (1 | Event), data=Analysis3, family=binomial)
summary(Analysis3BestFit)


# Understanding the SocialContext by Valence Interaction
# -----------------------------------------------------
# Positive Information
PositiveInfo <- filter(Analysis3, Valence == "Positive")

PositiveInfo$SocialContext.C <- scale (PositiveInfo$SocialContext.N, center=TRUE, scale=FALSE)

PositiveInfoGLMER <- glmer (Present~ SocialContext.C + (1 | ChainID) + (1 | Event), data=PositiveInfo, family=binomial)
summary(PositiveInfoGLMER) # P=0.173


# Negative Information
NegativeInfo <- filter(Analysis3, Valence == "Negative")

NegativeInfo$SocialContext.C <- scale (NegativeInfo$SocialContext.N, center=TRUE, scale=FALSE)

NegativeInfoGLMER <- glmer (Present~ SocialContext.C + (1 | ChainID) + (1 | Event), data=NegativeInfo, family=binomial)
summary(NegativeInfoGLMER) # P<0.001

# -----------------------------------------------------------------------------------------------


# VISUALISE BIAS (POSITIVE, NEGATIVE) IN EACH SocialContext
# ------------------------------------------------------
# ------------------------------------------------------

# Substract Mean Positive From Mean Negative Survival Score for each SocialContext
# Prediction is that Perference for Negativity Decreases first with Communicative Intent and then further with Social Interaction

# Calculate Means for Positive & Negative Information
# Calculate Mean Positive - Mean Negative for Each Chain
# Plot

# Filter
Bias <- filter(NegativityData, Valence %in% c("Positive", "Negative"))

# Calculate Mean Positive & Negative
Bias <- group_by(Bias, ChainID, SocialContext, Valence)

# Capture Means for the Distance Scores
BiasMEAN <- summarize(Bias, count = n(), 
                      Present = mean(Present, na.rm = T))

# Change to %
BiasMEAN$Present <- BiasMEAN$Present*100


# Change from Long Format to Wide Format
wideBiasMEAN <- BiasMEAN %>% spread(Valence, Present)

# Create Difference Score
wideBiasMEAN$Bias <- wideBiasMEAN$Positive - wideBiasMEAN$Negative


# FIGURE 1B
# ----------
# Show available colour palattes
piratepal(palette = "all")
# Use this palatte
piratepal(palette = "info2") # Go with this


# Order SocialContexts
wideBiasMEAN$SocialContext <- factor(wideBiasMEAN$SocialContext, levels = c("Repeated Reproduction", "Reproduction Intent",
                                                                    "Communication Intent", "Social Interaction"))


# Create a Vector of colours based on each Participant's Mean Bias Score (Positive - Negative)
# Use this to Color the dot points by Value
wideBiasMEAN <- mutate(wideBiasMEAN, ColourFig = ifelse(Bias >1, "#006A40FF",
                                                        ifelse(Bias <0, "#F08892FF", "#95828DFF")))


NegativityBiasFig <- ggplot(data=wideBiasMEAN, aes(x=SocialContext, y=Bias))
NegativityBiasFig <- NegativityBiasFig + stat_summary(fun.y=mean, geom="bar", position ="dodge", alpha=0.5, fill="red")
NegativityBiasFig <- NegativityBiasFig + theme_bw()
NegativityBiasFig <- NegativityBiasFig + geom_violin(alpha = 0.5, color='grey50')
# Geom_dotplot with points coloured by Value
NegativityBiasFig <- NegativityBiasFig + geom_dotplot(aes(x=SocialContext, y=Bias, fill=ColourFig), 
                                                      binaxis='y', stackdir='center', dotsize=0.9, alpha = 1, stroke=0.75, colour='grey15') + scale_fill_identity()
# Add Boostrapped 95% CIs (normality NOT assumed) - Doing this AFTER adding the geom_points ensures the CIs are Forward in the Figure
NegativityBiasFig <- NegativityBiasFig + stat_summary(fun.data=mean_cl_boot, geom='errorbar', 
                                                      position = position_dodge(width=0.9), 
                                                      color='grey15', size=0.75, width=0.5)
# Labels & Text Size
NegativityBiasFig <- NegativityBiasFig + labs(x=NULL, y="Overall Bias (positive event survival minus negative)") + 
  theme(title= element_text(size=14, face='bold'), 
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'))
# Change Legend Position & Increase Legend Font 
NegativityBiasFig <- NegativityBiasFig + theme(legend.position = "none", legend.text = element_text(size = 25, colour = "black"))
# Change y-axis scale
NegativityBiasFig <- NegativityBiasFig + coord_cartesian(ylim=c(-70, 50)) + scale_y_continuous(breaks=seq(-70, 50, 10))  
# Change x-axis labels
NegativityBiasFig <- NegativityBiasFig + scale_x_discrete(limit = c("Repeated Reproduction", "Reproduction Intent",
                                                                    "Communication Intent", "Social Interaction"), 
                                                                    labels = c("Repeated\nReproduction", "Reproduction\nIntent",
                                                                               "Communication\nIntent", "Social\nInteraction"))
# Change orientation of x-axis text
NegativityBiasFig <- NegativityBiasFig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
NegativityBiasFig

# ------------------------------------------------------

# Combined Survival Figure & Negativity Bias Figure
# --------------------------------------------------
Combined <- plot_grid(SurvivalFig, NegativityBiasFig, labels = c('A', 'B'), label_size = 20, nrow=1, rel_widths = c(1.75, 1))
Combined

# Exported 700h x 1400w
# -----------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# SURVIVAL, LOSS & TRANSFORMATION OF AMBIGUOUS EVENTS
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

# CREATE FIGURE
# -----------------
# Filter Ambiguous statments
AmbiguousDATA <- filter(NegativityData, Valence %in% c("Ambiguous"))

# KEY for AmbResult Values
  # 0 = Lost
  # 2 = Survived (remained ambiguous)
  # 3 = Negative Transformation
  # 4 = Positive Transformation

# Create Ambiguous1 Variable: 0=Lost, 2=Survives, 3&4 = Transformed
AmbiguousDATA$Ambiguous <- AmbiguousDATA$AmbResult.N

# Replace numeric values with Label
AmbiguousDATA$Ambiguous[AmbiguousDATA$Ambiguous == 0] <- "Lost"
AmbiguousDATA$Ambiguous[AmbiguousDATA$Ambiguous == 2] <- "Remained Ambiguous"
AmbiguousDATA$Ambiguous[AmbiguousDATA$Ambiguous == 3] <- "Transformed"
AmbiguousDATA$Ambiguous[AmbiguousDATA$Ambiguous == 4] <- "Transformed"


# FIGURE
# -----------------
# GENERATE COUNTS for Ambiguous Statements
Resolution <- AmbiguousDATA %>% 
  group_by(ChainID, ChainPosition, SocialContext) %>% 
  count(Ambiguous)

# Add 0 counts
Resolution <- Resolution %>% 
  ungroup %>%
  complete(nesting(ChainID, ChainPosition, SocialContext), Ambiguous, fill = list(n = 0))

# Calculate % for ambiguous counts
Resolution <- Resolution %>% mutate(Percent = (n/8)*100)

# Descripitive Stats for Ambiguous Information (Survived, Resolved, Lost)
describeBy(Resolution$Percent, 
           list(Resolution$Ambiguous), 
           mat=TRUE, digits=2)

# Descripitive Stats for Ambiguous Information (Survived, Resolved, Lost) across Chain Positions in the Inter-Individual SocialContexts
InterIndividual <- filter(Resolution, SocialContext %in% c("Reproduction Intent", "Communication Intent", "Social Interaction"))

describeBy(InterIndividual$Percent, 
           list(InterIndividual$Ambiguous, InterIndividual$ChainPosition), 
           mat=TRUE, digits=2)


# AMBIGUOUS INFORMATION: LOSS, SURVIVAL & TRANFORMATION (FIGURE 2A)
# -------------------------------------------------------------------
# Show available colour palattes
piratepal(palette = "all")
# Use this palatte
piratepal(palette = "info2")


# Change Order of SocialContexts:
Resolution$SocialContext <- factor(Resolution$SocialContext, levels = c("Repeated Reproduction", "Reproduction Intent",
                                                                "Communication Intent", "Social Interaction"))

# Specify Ambiguity as a Factor:
Resolution$Ambiguous <- factor(Resolution$Ambiguous, levels = c("Lost", "Remained Ambiguous", "Transformed"))


AmbiguousFig <- ggplot (Resolution, aes(x=ChainPosition, y=Percent, group=ChainID)) + facet_wrap(~SocialContext, ncol=2)
AmbiguousFig <- AmbiguousFig + geom_line(aes(group=interaction(ChainID, Ambiguous), color=Ambiguous), 
                                   size=0.4, alpha=0.3, position = position_jitter(w=0, h=2))
# Labels & Text Size
AmbiguousFig <- AmbiguousFig + labs(x= "Chain Position", y= "Ambiguous Events (%)") +  
  theme_bw() +
  theme(title= element_text(size=16, face='bold'),
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'),
        legend.title = element_text(size = 14, face='bold'),
        legend.text = element_text(size = 16, face='bold'))
# Change x-axis scale
AmbiguousFig <- AmbiguousFig + coord_cartesian(xlim=c(1, 4)) + 
  scale_x_continuous(breaks=seq(1, 4, 1))  # X-axis from 1-4 with 1-4 tick marks every 1
# Change y-axis scale
AmbiguousFig <- AmbiguousFig + coord_cartesian(ylim=c(0, 100)) + 
  scale_y_continuous(breaks=seq(0, 100, 20))  # Y-axis from 0-100 with 0-100 tick marks every 10 
# Add Geom Smooth 
AmbiguousFig <- AmbiguousFig + geom_smooth(method= "lm", aes(group=Ambiguous, color=Ambiguous, fill=Ambiguous), 
                                     size=1.25, alpha = 0.5)
# Add mean points for each SocialContext
AmbiguousFig <- AmbiguousFig + stat_summary(fun.y=mean, geom="point", size=4, shape=21, stroke=0.5, colour='grey15',
                                    aes(group =Ambiguous, fill=Ambiguous))
# Change Legend Position & Remove Legend Title
AmbiguousFig <- AmbiguousFig + theme(legend.position="top") + theme(legend.title=element_blank())
# Change Colour Manually: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
AmbiguousFig <- AmbiguousFig + scale_color_manual(values=c("#8AB8CFFF", "#5A5895FF", "#F2990CFF"))
AmbiguousFig <- AmbiguousFig + scale_fill_manual(values=c("#8AB8CFFF", "#5A5895FF", "#F2990CFF"))
AmbiguousFig



# Generate Means Collapsed Across SocialContext & Chain Position
# ------------------------------------------------------------
# Calculate Mean %
# Group Data
ResolutionCollapsed <- group_by(Resolution, ChainID, Ambiguous)

# Capture Means for the Distance Scores
ResolutionCollapsedMEANS <- summarize(ResolutionCollapsed, count = n(),
                                      Percent = mean(Percent, na.rm = T)
                                      )


# FIGURE 2B
# -----------
# Specify Order of SocialContexts:
ResolutionCollapsedMEANS$Ambiguous <- factor(ResolutionCollapsedMEANS$Ambiguous, levels = c("Lost", "Remained Ambiguous", "Transformed"))


ResolutionCollapsedFig <- ggplot(data=ResolutionCollapsedMEANS, aes(x=Ambiguous, y=Percent))
ResolutionCollapsedFig <- ResolutionCollapsedFig + stat_summary(fun.y=mean, geom="bar", position ="dodge", alpha=0.4, aes(fill=Ambiguous)) 
# Change Colour Manually: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
ResolutionCollapsedFig <- ResolutionCollapsedFig + scale_fill_manual(values=c("grey40", "forestgreen", "deeppink1")) # Add manual fill colours
ResolutionCollapsedFig <- ResolutionCollapsedFig + theme_bw()
# Geom_dotplot with points
ResolutionCollapsedFig <- ResolutionCollapsedFig + geom_dotplot(aes(x=Ambiguous, y=Percent, fill=Ambiguous), 
                                                      binaxis='y', binwidth=2.25, stackdir='center', dotsize=0.6, 
                                                      alpha = 1, stroke=0.75, colour='grey15')
# Add Violin Geom to give indication or data normality
ResolutionCollapsedFig <- ResolutionCollapsedFig + geom_violin(alpha = 0.0, colour='grey50')
# Change Colour Manually
ResolutionCollapsedFig <- ResolutionCollapsedFig + scale_color_manual(values=c("#8AB8CFFF", "#5A5895FF", "#F2990CFF"))
ResolutionCollapsedFig <- ResolutionCollapsedFig + scale_fill_manual(values=c("#8AB8CFFF", "#5A5895FF", "#F2990CFF"))
# Add Boostrapped 95% CIs (normality NOT assumed) - Doing this AFTER adding the geom_points ensures the CIs are Forward in the Figure
ResolutionCollapsedFig <- ResolutionCollapsedFig + stat_summary(fun.data=mean_cl_boot, geom='errorbar', 
                                                                position = position_dodge(width=0.9), 
                                                                color='grey15', size=0.75, width=0.3)
# Labels & Text Size
ResolutionCollapsedFig <- ResolutionCollapsedFig + labs(x=NULL, y="Ambiguous Events (%)") + 
  theme(title= element_text(size=14, face='bold'), 
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=12), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 14, face='bold'))
# Change Legend Position & Increase Legend Font 
ResolutionCollapsedFig <- ResolutionCollapsedFig + theme(legend.position = "none", legend.text = element_text(size = 25, colour = "black"))
# Change y-axis scale
ResolutionCollapsedFig <- ResolutionCollapsedFig + coord_cartesian(ylim=c(0, 100)) + scale_y_continuous(breaks=seq(0, 100, 10))
ResolutionCollapsedFig
# ------------------------------------------------------



# Combined Transformation Figure
# --------------------------------------------------
CombinedTransform <- plot_grid(AmbiguousFig, ResolutionCollapsedFig, labels = c('A', 'B'), label_size = 20, nrow=1, rel_widths = c(1.75, 1))
CombinedTransform

# Exported 700h x 1400w
#---------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# NEGATIVE & POSITIVE TRANSFORMATION OF AMBIGUOUS EVENTS
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# Filter Ambiguous statments
AmbiguousDATA1 <- filter(NegativityData, Valence %in% c("Ambiguous"))

# Determine if each Ambiguous Event was Positively OR Negatively resolved (or neither)
# Use logistic regression

# Missing Data Issue
#   1)  Some chains have missing data for chain positions
#       (because not all participants agreed to their data being used for research purposes)
#   2)  Social Interaction context contains 3 chain positions of data (not 4, like the other social contexts)

# Cross-Tabulate (using xtabs) to Assess Missing Data
mytable <- xtabs(~ChainID+ChainPosition, data=AmbiguousDATA1)
ftable(mytable) # print table

# Problem Chains (Missing Data on ChainPosition 1, 2 or 3): 
#   2, 14, 61, 63, 210, 300, 344, 364, 394, 398

# Chains Missing Data on ChainPosition 4 only
# Check which are not in the Social Interaction context (add * and indicate condition) 
#  [RR = Repeated Reproduction, RI = Reproduction Intent, CI = Communication Intent]:
#   0, 1, 11*[CI], 14*[RI], 30, 31, 32, 38*[RI], 39*[RI], 60, 90, 91, 92, 105*[RR], 120, 121, 122, 
#   152, 153*[RI], 180, 181, 182, 211, 212, 240, 242, 270, 271, 272, 301, 302, 
#   329*[RI], 330, 331, 332, 360, 361, 362, 365*[CI], 390, 391, 392  

mytable <- xtabs(~ChainID+SocialContext+ChainPosition, data=AmbiguousDATA1)
ftable(mytable) # print table

# Rather than Delete a further 8 chains, I will instead include only ChainPositions 1-3.
# This has the benefit of making the chains directly comparable across the different Social Contexts


# Data Processing
# ----------------
# Remove Problematic Chains & Remove Chain Position 4
AmbiguousDATArevised <- AmbiguousDATA1 %>% filter(ChainPosition < 4) %>% 
  filter(!ChainID %in% c(2, 14, 61, 63, 210, 300, 344, 364, 394, 398))

# Generate Counts
ResolutionALL <- AmbiguousDATArevised %>% 
  group_by(ChainID, SocialContext, Event) %>% 
  count(AmbResult)

# Add 0 counts
ResolutionALL <- ResolutionALL %>% 
  ungroup %>%
  complete(nesting(ChainID, SocialContext, Event), AmbResult, fill = list(n = 0))

# Filter to Include only Positive & Negative Resolutions
ResolutionALLPosNeg <- filter(ResolutionALL, AmbResult %in% c("Negative Transformation", "Positive Transformation"))

# Avoid Double Counting ambiguous events that were resolved & then transmitted as unambiguous positive or negative events.  
# Change count scores to 1
ResolutionALLPosNeg <- mutate(ResolutionALLPosNeg, Transform = ifelse(n > 0, 1, 0))

# Returns a list of All Ambiguous Events
# and if they were resolved Positively (1 or 0), Negatively (1 or 0), neither Positively or Negatively (0, 0)



# Generate Means for Figure
# --------------------------
# Show Means for Each Condition
ResolutionALLPosNeg <- ResolutionALLPosNeg %>% group_by(ChainID, SocialContext, AmbResult)

# Capture Means for the Distance Scores
ResolutionALLPosNegMEANS <- summarize(ResolutionALLPosNeg, count = n(),
                                      Mean = mean(Transform, na.rm = T))

# Change Proportion Score to %
ResolutionALLPosNegMEANS$Mean <- ResolutionALLPosNegMEANS$Mean*100


# FIGURE 3
# -----------
# Specify Order of Valence:
ResolutionALLPosNegMEANS$AmbResult <- factor(ResolutionALLPosNegMEANS$AmbResult, levels = c("Positive Transformation", "Negative Transformation"))

# Change Order of SocialContexts:
ResolutionALLPosNegMEANS$SocialContext <- factor(ResolutionALLPosNegMEANS$SocialContext, levels = c("Repeated Reproduction", "Reproduction Intent",
                                                                                                          "Communication Intent", "Social Interaction"))


ResolutionALLPosNegFig <- ggplot(data=ResolutionALLPosNegMEANS, aes(x=AmbResult, y=Mean)) + facet_grid(~SocialContext)
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + stat_summary(fun=mean, geom="bar", position ="dodge", alpha=0.5, aes(fill=AmbResult))
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + theme_bw()
# Add Violin Geom to give indication or data normality
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + geom_violin(alpha = 0.5, color='grey50')
# Geom_dotplot
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + geom_dotplot(aes(x=AmbResult, y=Mean, fill=AmbResult), 
                                                                binaxis='y', stackdir='center', dotsize=0.75, alpha = 1, stroke=0.75, colour='grey15')
# Add Boostrapped 95% CIs (normality NOT assumed) - Doing this AFTER adding the geom_points ensures the CIs are Forward in the Figure
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + stat_summary(fun.data=mean_cl_boot, geom='errorbar', 
                                                                position = position_dodge(width=0.9), 
                                                                color='grey14', size=0.75, width=0.3)
# Labels & Text Size
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + labs(x=NULL, y="Ambiguous Event Transformed (%)") + 
  theme(title= element_text(size=14, face='bold'), 
        axis.title.x= element_text(size=14), 
        axis.text.x= element_text(face="bold", size=12), 
        axis.title.y = element_text(size=14), 
        axis.text.y= element_text(face="bold", size=14), 
        strip.text.x= element_text(size = 14, face='bold'),
        strip.text.y= element_text(size = 16, face='bold'))
# Change Legend Position & Increase Legend Font 
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + theme(legend.position = "none", legend.text = element_text(size = 25, colour = "black"))
# Change y-axis scale
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + coord_cartesian(ylim=c(0, 80)) + scale_y_continuous(breaks=seq(0, 80, 10))  
# Change x-axis labels
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + scale_x_discrete(limit = c("Positive Transformation", "Negative Transformation"), 
                                                                    labels = c("Positive\nTransformation", "Negative\nTransformation"))
# Change Colour Manually
ResolutionALLPosNegFig <- ResolutionALLPosNegFig + scale_fill_manual(values=c("#006A40FF", "#F08892FF"))
ResolutionALLPosNegFig

# Exported 500h x 1400w
# -----------------------------------------------------------------------------------------------


# STATISTICAL ANALYSIS: Ambiguous Events
# -----------------------------------------
# Logistic Mixed Effects Modeling

# Create Numeric AmbResult Variable
ResolutionALLPosNeg <- mutate(ResolutionALLPosNeg, AmbResult.N = ifelse(AmbResult =="Negative Transformation", 1,0))

# Centre IV
ResolutionALLPosNeg$AmbResult.C <- scale (ResolutionALLPosNeg$AmbResult.N, center=TRUE, scale=FALSE)

# Model did not converge when ChainID was included as a random effect
# Model did not converge when Social Context or AmbResult was included as a random slope in the Event random effect
AmbiguousAnalysis <- glmer (Transform~ SocialContext*AmbResult.C +
                             (1 | Event), data=ResolutionALLPosNeg, family=binomial)
summary(AmbiguousAnalysis)


# Best Fitting Model
# --------------------
BestFitAmbiguousAnalysis <- glmer (Transform~ AmbResult.C +
                              (1 | Event), data=ResolutionALLPosNeg, family=binomial)
summary(BestFitAmbiguousAnalysis)
# -----------------------------------------------------------------------------------------------