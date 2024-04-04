##################TFM###################
###Activity measurament --> boxplots

##C group (with 19 individuals)
library(readxl)
control <- read_excel("Movement2.xlsx", sheet = 1)

control$Individual <- factor(control$Individual, levels = c("HOAM-76","HOAM-80","HOAM-84","HOAM-88","HOAM-92","HOAM-96","HOAM-100","HOAM-104","HOAM-108","HOAM-112","HOAM-116","HOAM-120","HOAM-124","HOAM-128","HOAM-132","HOAM-136","HOAM-140","HOAM-144","HOAM-148")) #To order individuals

plot1 <- ggplot(data = control, aes(x = Individual, y = Movement, fill = Individual)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 50)) +
  labs(x="Individuals",y="Movement (cm)", title= "Control group (C)", color = "Tematica") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 5))


##C-E group (with 19 individuals)
CE <- read_excel("Movement2.xlsx", sheet = 2)

CE$Individual <- factor(CE$Individual, levels = c("HOAM-77","HOAM-81","HOAM-85","HOAM-89","HOAM-93","HOAM-97","HOAM-101","HOAM-105","HOAM-109","HOAM-113","HOAM-117","HOAM-121","HOAM-125","HOAM-129","HOAM-133","HOAM-137","HOAM-141","HOAM-145","HOAM-149"))

plot2 <-ggplot(data = CE, aes(x = Individual, y = Movement, fill = Individual)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 50)) +
  labs(x="Individuals",y="Movement (cm)", title= "Control group with negative stimulus (C-E)", color = "Tematica") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 5))

##CL group (with 18 individuals)
TL <- read_excel("Movement2.xlsx", sheet = 3)

TL$Individual <- factor(TL$Individual, levels = c("HOAM-78","HOAM-82","HOAM-86","HOAM-90","HOAM-94","HOAM-98","HOAM-102","HOAM-106","HOAM-110","HOAM-114","HOAM-118","HOAM-126","HOAM-130","HOAM-134","HOAM-138","HOAM-142","HOAM-122","HOAM-146"))

plot3 <-ggplot(data = TL, aes(x = Individual, y = Movement, fill = Individual)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 50)) +
  labs(x="Individuals",y="Movement (cm)", title= "Long electroshock group without negative stimulus (TL)", color = "Tematica") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 5))

##TL-E group (with 18 individuals)
TLE <- read_excel("Movement2.xlsx", sheet = 4)

TLE$Individual <- factor(TLE$Individual, levels = c("HOAM-79","HOAM-83","HOAM-87","HOAM-91","HOAM-95","HOAM-99","HOAM-103","HOAM-107","HOAM-111","HOAM-115","HOAM-119","HOAM-127","HOAM-131","HOAM-135","HOAM-139","HOAM-143","HOAM-123","HOAM-147"))

plot4 <-ggplot(data = TLE, aes(x = Individual, y = Movement, fill = Individual)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, by = 50)) +
  labs(x="Individuals",y="Movement (cm)", title= "Long electroshock group with negative stimulus (TL-E)", color = "Tematica") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 5))

#Combine the graphics into a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

##Activity measurement --> Statistics (with all the data/groups together)
data <- read_excel("Movement2.xlsx", sheet = 5)
ag <- aggregate(Movement ~ Individual, data = data, FUN = mean) ##to calculate the movement mean per individual with aggregate function "aggregating the values of the "Movement" variable by the "Individual" factor, calculating the mean of the "Movement" values for each individual"
##output will be a new dataset showing the mean of "Movement" for each individual

##Plot --> histogram + Gauss distribution
ggplot(ag, aes(x = Movement)) +
  geom_histogram(aes(y = ..density..), binwidth = 27, colour = "black", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666") +  
  labs(title = "Histogram of Averages",
       x = "Average Movement (Cm)",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove background grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "transparent", color = NA),  # Set panel background to transparent
        axis.line = element_line(color = "black"),  # Adjust axis lines
        axis.ticks = element_line(color = "black"),  # Adjust tick marks
        plot.title = element_text(size = 20),  # Adjust title size
        axis.title = element_text(size = 16),  # Adjust axis title size
        axis.text = element_text(size = 14)) +  # Adjust axis text size
  scale_x_continuous(breaks = seq(0, max(ag$Movement), by = 50))  # Adjust x-axis ticks to every 50 units

##choose the range as a function of the density function (Gaussian bell) to select the most common/typical/standard values and discard outliers

por <- sum(ag$Movement >= 10 & ag$Movement <= 90) ##to know the number of individuals within this range

(por/ nrow(ag)) * 100 ##to know the % of individuals within this range

individualselected <- subset(ag, Movement >= 10 & Movement <= 90) ##To obtain a dataframe with the list of individuals that meet the requirement (presenting movement values within the specified range)

write.csv(individualselected, "individualselected.csv") #To obtain a csv file with the reduced list with the individuals selected

# Normality test --> Shapiro-Wilk (With a p-value < 0.05, we reject the null hypothesis of normality; the data is not normal)
normality_test <- shapiro.test(ag$Movement)

# Test result
print(normality_test)

##Data normalized
newag <- sqrt(ag$Movement)

ggplot(ag, aes(x = newag)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666") +  
  labs(title = "Histogram of Averages",
       x = "Average Movement (Cm)",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove background grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "transparent", color = NA),  # Set panel background to transparent
        axis.line = element_line(color = "black"),  # Adjust axis lines
        axis.ticks = element_line(color = "black"),  # Adjust tick marks
        plot.title = element_text(size = 20),  # Adjust title size
        axis.title = element_text(size = 16),  # Adjust axis title size
        axis.text = element_text(size = 14)) +  # Adjust axis text size
  scale_x_continuous(breaks = seq(0, max(ag$Movement), by = 50))  # Adjust x-axis ticks to every 50 units

normality_test <- shapiro.test(newag)
print(normality_test)
