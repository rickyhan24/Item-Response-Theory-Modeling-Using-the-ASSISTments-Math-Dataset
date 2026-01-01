#Get Data
setwd("C:\\Users\\Richard Han\\Downloads\\Psychometrics Project") 
getwd() 
data <- read.csv("df_irt.csv",stringsAsFactors = FALSE) 
str(data)
head(data)

#install.packages(c("dplyr", "tidyr", "mirt"))


#Load libraries
library(dplyr)
library(tidyr)
library(mirt)

# df_irt columns: student_id, item_id, is_correct, Student Country

# 1) Ensure clean types
df <- data %>%
  mutate(
    student_id  = as.character(student_id),
    item_id     = as.character(item_id),
    is_correct  = as.integer(is_correct)
  ) %>%
  filter(is_correct %in% c(0,1))

# 2) Make sure it's one row per student-item (should already be true)
df_one <- df %>%
  group_by(student_id, item_id) %>%
  summarise(is_correct = first(is_correct), .groups="drop")

# 3) Wide response matrix (rows=students, cols=items, entries=0/1, missing=NA)
resp_wide <- df_one %>%
  pivot_wider(names_from = item_id, values_from = is_correct, values_fill = NA)

resp_mat <- as.data.frame(resp_wide)
rownames(resp_mat) <- resp_mat$student_id
resp_mat$student_id <- NULL
resp_mat[] <- lapply(resp_mat, function(x) as.numeric(x))
# we now have a student x item response matrix with dimensions 959 by 15
dim(resp_mat)

# 4) Fit 1PL (Rasch)
mod_1pl <- mirt(resp_mat, 1, itemtype="Rasch", verbose=FALSE)


# 5) Fit 2PL
mod_2pl <- mirt(resp_mat, 1, itemtype="2PL",
                technical=list(NCYCLES=2000),
                verbose=FALSE)

# ---- Extract parameters ----
# Item parameters
item_1pl <- as.data.frame(coef(mod_1pl, IRTpars=TRUE, simplify=TRUE)$items) %>%
  tibble::rownames_to_column("item_id")

item_2pl <- as.data.frame(coef(mod_2pl, IRTpars=TRUE, simplify=TRUE)$items) %>%
  tibble::rownames_to_column("item_id")

# Person abilities (theta)
theta_1pl <- fscores(mod_1pl, method="EAP")
theta_2pl <- fscores(mod_2pl, method="EAP")


# Examine difficulties
summary(item_1pl$b)
item_1pl_sorted <- item_1pl[order(item_1pl$b), ] 
item_1pl_sorted

summary(item_2pl$b)
item_2pl_sorted <- item_2pl[order(item_2pl$b), ] 
item_2pl_sorted

# Examine student abilities
# theta_1pl is the matrix from fscores(mod_1pl, method="EAP")
theta_df <- data.frame(
  student_id = rownames(resp_mat),
  theta      = as.numeric(theta_1pl[, 1])
)

head(theta_df)
summary(theta_df$theta)

theta_sorted_low <- theta_df[order(theta_df$theta), ]
head(theta_sorted_low, 10)      # lowest 10 ability students
tail(theta_sorted_low, 10) 

# same for 2pl
theta_df_2pl <- data.frame(
  student_id = rownames(resp_mat),
  theta      = as.numeric(theta_2pl[, 1])
)

head(theta_df_2pl)
summary(theta_df_2pl$theta)

theta_sorted_low_2pl <- theta_df[order(theta_df_2pl$theta), ]
head(theta_sorted_low_2pl, 10)      # lowest 10 ability students
tail(theta_sorted_low_2pl, 10) 


# ---- Quick checks / plots ----
summary(mod_1pl)
summary(mod_2pl)

hist(theta_df$theta,
     breaks = 25,
     main = "Distrbution of Student Abilities (1PL Model)",   # title
     xlab = "Student Ability (theta)",           # x-axis label
     ylab = "Number of Students",                 # y-axis label
     col  = "lightgray",                          # optional color
     border = "white")                            # optional border color


hist(theta_df_2pl$theta,
     breaks = 25,
     main = "Distrbution of Student Abilities (2PL Model)",   # title
     xlab = "Student Ability (theta)",           # x-axis label
     ylab = "Number of Students",                 # y-axis label
     col  = "lightgray",                          # optional color
     border = "white", 
     xlim = c(-2, 2)
     ) 


#Plot item-person map
# Person abilities (numeric vector)
theta_vec <- theta_df$theta

# Item difficulties (numeric vector)
b_vec <- item_1pl$b

# Histogram of abilities
hist(theta_vec,
     breaks = 20,
     col    = "grey80",
     border = "white",
     main   = "Item–Person Map (Rasch Model)",
     xlab   = "Latent ability / difficulty (θ)",
     ylab   = "Number of students")

# Get plotting area limits
usr <- par("usr")  # c(xmin, xmax, ymin, ymax)

# Plot items as vertical ticks near the bottom
points(
  x  = b_vec,
  y  = rep(usr[3] + 0.05 * (usr[4] - usr[3]), length(b_vec)),  # a bit above bottom
  pch = 124,   # vertical bar "|"
  col = "red",
  cex = 1.5
)

legend("topright",
       legend = c("Students", "Items"),
       pch    = c(15, 124),
       pt.cex = c(1.5, 1.5),
       col    = c("grey60", "red"),
       bty    = "n")

#do the same for 2pl
# Person abilities (numeric vector)
theta_vec_2pl <- theta_df_2pl$theta

# Item difficulties (numeric vector)
b_vec_2pl <- item_2pl$b

# Histogram of abilities
hist(theta_vec_2pl,
     breaks = 20,
     col    = "grey80",
     border = "white",
     main   = "Item–Person Map (2PL Model)",
     xlab   = "Latent ability / difficulty (θ)",
     ylab   = "Number of students",
     xlim = c(-2, 2)
     )

# Get plotting area limits
usr <- par("usr")  # c(xmin, xmax, ymin, ymax)

# Plot items as vertical ticks near the bottom
points(
  x  = b_vec_2pl,
  y  = rep(usr[3] + 0.05 * (usr[4] - usr[3]), length(b_vec_2pl)),  # a bit above bottom
  pch = 124,   # vertical bar "|"
  col = "red",
  cex = 1.5
)

legend("topright",
       legend = c("Students", "Items"),
       pch    = c(15, 124),
       pt.cex = c(1.5, 1.5),
       col    = c("grey60", "red"),
       bty    = "n")



#Plot CTT vs IRT Item Difficulty
library(dplyr)

item_p <- df%>%
  group_by(item_id) %>%
  summarise(p_value = mean(is_correct, na.rm = TRUE)) %>%
  ungroup()

#merge p-values with parameters
item_summary <- item_1pl %>%
  left_join(item_p, by = "item_id")

library(ggplot2)

ggplot(item_summary, aes(x = b, y = p_value)) +
  geom_point(color="darkred", size=3) +
  geom_smooth(method="lm", se=FALSE, color="blue") +
  labs(title="Difficulty vs Proportion Correct",
       x="IRT Difficulty (b)",
       y="Proportion Correct (p-value)") +
  theme_minimal()
#do the same for 2pl
item_summary_2pl <- item_2pl %>%
  left_join(item_p, by = "item_id")

library(ggplot2)

ggplot(item_summary_2pl, aes(x = b, y = p_value)) +
  geom_point(color="darkred", size=3) +
  geom_smooth(method="lm", se=FALSE, color="blue") +
  labs(title="Difficulty vs Proportion Correct",
       x="IRT Difficulty (b)",
       y="Proportion Correct (p-value)") +
  theme_minimal()

# Plot ICCs, Test information curve, Item information curves

#plot item characteristic curves for hardest, medium, and easiest items
library(dplyr)
library(ggplot2)

#-------------------------------------------------------
# 1. Sort items by difficulty b
#-------------------------------------------------------
item_sorted <- item_1pl %>% arrange(b)

easiest <- item_sorted[1, ]                      # lowest difficulty
middle  <- item_sorted[nrow(item_sorted)%/%2, ]  # median difficulty
hardest <- item_sorted[nrow(item_sorted), ]      # highest difficulty

#-------------------------------------------------------
# 2. Create theta grid
#-------------------------------------------------------
theta_grid <- seq(-3, 3, length.out = 200)

#-------------------------------------------------------
# 3. Probability function for 1PL/Rasch
#-------------------------------------------------------
p_1pl <- function(theta, b){
  1 / (1 + exp(-(theta - b)))
}

#-------------------------------------------------------
# 4. Data for ggplot
#-------------------------------------------------------
plot_data <- rbind(
  data.frame(theta=theta_grid,
             p=p_1pl(theta_grid, easiest$b),
             item=paste0(easiest$item_id," (b=",round(easiest$b,2),")")),
  
  data.frame(theta=theta_grid,
             p=p_1pl(theta_grid, middle$b),
             item=paste0(middle$item_id," (b=",round(middle$b,2),")")),
  
  data.frame(theta=theta_grid,
             p=p_1pl(theta_grid, hardest$b),
             item=paste0(hardest$item_id," (b=",round(hardest$b,2),")"))
)

#-------------------------------------------------------
# 5. Plot ICC curves
#-------------------------------------------------------
ggplot(plot_data, aes(x=theta, y=p, color=item)) +
  geom_line(size=1.2) +
  geom_vline(xintercept=0, linetype="dashed", color="gray50") +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="1PL Item Characteristic Curves (Skill 311)",
       x="Ability (θ)",
       y="P(correct)",
       color="Item") +
  theme_minimal() +
  theme(plot.title=element_text(size=14, face="bold"))

# do the same for 2pl
#-------------------------------------------------------
# 1. Sort items by difficulty b
#-------------------------------------------------------
item_sorted_2pl <- item_2pl %>% arrange(b)

easiest_2pl <- item_sorted_2pl[1, ]                      # lowest difficulty
middle_2pl  <- item_sorted_2pl[nrow(item_sorted_2pl)%/%2, ]  # median difficulty
hardest_2pl <- item_sorted_2pl[nrow(item_sorted_2pl), ]      # highest difficulty

#-------------------------------------------------------
# 2. Create theta grid
#-------------------------------------------------------
theta_grid <- seq(-3, 3, length.out = 200)

#-------------------------------------------------------
# 3. Probability function for 2PL
#-------------------------------------------------------
p_2pl <- function(theta, a, b){
  1 / (1 + exp(-a*(theta - b)))
}

#-------------------------------------------------------
# 4. Data for ggplot
#-------------------------------------------------------
plot_data_2pl <- rbind(
  data.frame(
    theta = theta_grid,
    p     = p_2pl(theta_grid, easiest_2pl$a, easiest_2pl$b),
    item  = paste0(easiest_2pl$item_id,
                   " (a=", round(easiest_2pl$a, 2),
                   ", b=", round(easiest_2pl$b, 2), ")")
  ),
  data.frame(
    theta = theta_grid,
    p     = p_2pl(theta_grid, middle_2pl$a, middle_2pl$b),
    item  = paste0(middle_2pl$item_id,
                   " (a=", round(middle_2pl$a, 2),
                   ", b=", round(middle_2pl$b, 2), ")")
  ),
  data.frame(
    theta = theta_grid,
    p     = p_2pl(theta_grid, hardest_2pl$a, hardest_2pl$b),
    item  = paste0(hardest_2pl$item_id,
                   " (a=", round(hardest_2pl$a, 2),
                   ", b=", round(hardest_2pl$b, 2), ")")
  )
)

#-------------------------------------------------------
# 5. Plot ICC curves
#-------------------------------------------------------
ggplot(plot_data_2pl, aes(x=theta, y=p, color=item)) +
  geom_line(size=1.2) +
  geom_vline(xintercept=0, linetype="dashed", color="gray50") +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="2PL Item Characteristic Curves (Skill 311)",
       x="Ability (θ)",
       y="P(correct)",
       color="Item") +
  theme_minimal() +
  theme(plot.title=element_text(size=14, face="bold"))

# Plot all ICC's in one plot
library(dplyr)
library(ggplot2)
library(viridis)

# 2) Build grid of theta values
theta_grid <- seq(-4, 4, length.out = 200)

# 3) 2PL ICC function
p_2pl <- function(theta, a, b) {
  1 / (1 + exp(-a * (theta - b)))
}

# 4) Expand to item × theta grid
plot_data <- tidyr::expand_grid(
  item_id = item_2pl$item_id,
  theta   = theta_grid
) %>%
  left_join(item_2pl, by = "item_id") %>%
  mutate(p = p_2pl(theta, a, b))

# 5) Plot all ICCs with a gradient palette
ggplot(plot_data, aes(x = theta, y = p, group = item_id, color = item_id)) +
  geom_line(alpha = 0.8, linewidth = 0.7) +
  scale_color_viridis_d(guide = "none") +  # drop legend if too many items
  labs(
    title = "2PL Item Characteristic Curves (All Items)",
    x     = "Ability (θ)",
    y     = "P(correct)"
  ) +
  theme_minimal()

# get mean, min, max discrimination
library(dplyr)

item_2pl %>%
  summarise(
    mean_a = mean(a),
    sd_a   = sd(a),
    min_a  = min(a),
    max_a  = max(a)
  )

item_2pl

# Test information curve
plot(mod_1pl, type="info")  
plot(mod_2pl, type="info")  
# Find where the peak occurs
theta_grid <- seq(-4, 4, .01)   # finer resolution grid
test_info <- testinfo(mod_1pl, Theta=theta_grid)

theta_peak <- theta_grid[which.max(test_info)]
peak_info  <- max(test_info)

theta_peak
peak_info
# do the same for 2pl
test_info_2pl <- testinfo(mod_2pl, Theta=theta_grid)

theta_peak_2pl <- theta_grid[which.max(test_info_2pl)]
peak_info_2pl  <- max(test_info_2pl)

theta_peak_2pl
peak_info_2pl


# Item information curves
plot(mod_1pl, type="infotrace")  
plot(mod_2pl, type="infotrace")  
# Plot IIC's in one plot
plot(
  mod_1pl,
  type = "infotrace",
  facet_items = FALSE
)
plot(
  mod_2pl,
  type = "infotrace",
  facet_items = FALSE
)











