

# Install ggdag and dagitty packages if not already installed
install.packages("ggdag")
install.packages("dagitty")

# Load the libraries
library(ggdag)
library(dagitty)

# Create the DAG structure
dag_data <- dagify(
  lung_cancer ~ smoking + age + coughing + wheezing + yellow_fingers + chest_pain + swallowing_difficulty + fatigue + chronic_disease + shortness_of_breath + anxiety ,
  coughing ~ smoking,
  smoking ~ alchol,
  alchol ~ peer_pressure,
  wheezing ~ allergy,
  shortness_of_breath ~ smoking,
  shortness_of_breath ~ anxiety,
  yellow_fingers ~ smoking,
  chronic_disease ~ age,
  fatigue ~ age,
  smoking ~ peer_pressure,
  labels = c(
    "lung_cancer" = "Lung Cancer",
    "smoking" = "Smoking",
    "coughing" = "Coughing",
    "wheezing" = "Wheezing",
    "yellow_fingers" = "Yellow Fingers",
    "chest_pain" = "Chest Pain",
    "swallowing_difficulty" = "Swallowing Difficulty",
    "fatigue" = "Fatigue",
    "chronic_disease" = "Chronic Disease",
    "peer_pressure" = "Peer Pressure",
    "age" = "Age",
    "shortness_of_breath" = "Shortness_of_breath",
    "alchol"="Alchol",
    "allergy" = "Allergy",
    "anxiety" = "Anxiety"
  )
)
tidy_dag <- tidy_dagitty(dag_data)
 # print(impliedConditionalIndependencies(dag_data))
 
# Create the DAG plot
dag_plot <- ggdag(tidy_dag, text = FALSE, use_labels = "label") +
  theme_dag()

# Save the plot to a file to ensure it's being generated
# ggsave("dag_plot.png", plot = dag_plot)

# Print the plot explicitly if in a non-interactive environment
plot(dag_plot)
