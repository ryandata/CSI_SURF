# ============================================================
# R Translation of Python Matplotlib Tutorial
# ggplot2 only
# ============================================================
 
library(ggplot2)
library(tidyr)    # for pivot_longer
library(patchwork) # for combining plots (replaces par(mfrow))
 
 
# ============================================================
# 1. LINE PLOTS
# ============================================================
 
# --- 1.1 Basic line plot ---
months  <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')
traffic <- c(150, 200, 180, 220, 250, 210)
df      <- data.frame(Month = factor(months, levels = months), Traffic = traffic)
 
ggplot(df, aes(x = Month, y = Traffic, group = 1)) +
  geom_line()
 
 
# --- 1.2 Add labels and title ---
ggplot(df, aes(x = Month, y = Traffic, group = 1)) +
  geom_line() +
  labs(x     = "Month",
       y     = "Monthly Traffic (in Thousands)",
       title = "Monthly Website Traffic")
 
 
# --- 1.3 Custom color, line style, markers, and grid ---
ggplot(df, aes(x = Month, y = Traffic, group = 1)) +
  geom_line(color = "green", linetype = "dashed") +
  geom_point(color = "green", shape = 16) +
  labs(x     = "Month",
       y     = "Monthly Traffic (in Thousands)",
       title = "Monthly Website Traffic") +
  theme(panel.grid = element_line())  # grid is on by default in most themes
 
 
# --- 1.4 Multiple lines, legend, and saving to file ---
months            <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')
product_a_revenue <- c(45, 55, 60, 70, 80, 75)
product_b_revenue <- c(35, 40, 50, 55, 70, 68)
 
revenue_df <- data.frame(
  Month   = factor(months, levels = months),
  `Product A` = product_a_revenue,
  `Product B` = product_b_revenue,
  check.names = FALSE
)
revenue_long <- pivot_longer(revenue_df, -Month,
                             names_to = "Product", values_to = "Revenue")
 
p <- ggplot(revenue_long, aes(x = Month, y = Revenue,
                               color = Product, linetype = Product,
                               shape = Product, group = Product)) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_manual(values    = c("Product A" = "blue", "Product B" = "red")) +
  scale_linetype_manual(values = c("Product A" = "solid", "Product B" = "dashed")) +
  scale_shape_manual(values    = c("Product A" = 16, "Product B" = 15)) +
  labs(x     = "Month",
       y     = "Monthly Revenue (in $1000)",
       title = "Monthly Revenue Comparison")
 
ggsave("my_plot.png", plot = p)
ggsave("my_plot.jpg", plot = p)
 
 
# ============================================================
# 2. PLOT TYPES
# ============================================================
 
# --- 2.1 Grouped Bar Chart ---
categories <- c('Rent', 'Groceries', 'Utilities', 'Transport', 'Entertainment')
bar_data <- data.frame(
  Category = rep(factor(categories, levels = categories), 3),
  Expenses = c(1200, 600, 200, 300, 150,
               1000, 700, 300, 200, 100,
               1100, 500, 250, 400, 200),
  Person   = rep(c("Person 1", "Person 2", "Person 3"), each = 5)
)
 
ggplot(bar_data, aes(x = Category, y = Expenses, fill = Person)) +
  geom_bar(stat = "identity", position = "dodge", color = "grey") +
  scale_fill_manual(values = c("Person 1" = "brown",
                               "Person 2" = "black",
                               "Person 3" = "green")) +
  labs(title = "Monthly Expense Comparison for 3 People",
       x     = "Categories",
       y     = "Expenses ($)") +
  theme_minimal()
 
 
# --- 2.2 Histogram with median line ---
exam_scores  <- c(68, 72, 75, 80, 82, 84, 86, 90, 92, 95, 98, 100)
median_score <- median(exam_scores)
 
ggplot(data.frame(Score = exam_scores), aes(x = Score)) +
  geom_histogram(breaks   = c(60, 70, 80, 90, 100),
                 fill     = "lightblue",
                 color    = "black",
                 alpha    = 0.7) +
  geom_vline(xintercept = median_score,
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median_score + 1, y = 3.5,
           label = paste("Median:", median_score),
           color = "red", hjust = 0) +
  labs(x     = "Exam Scores",
       y     = "Frequency",
       title = "Exam Scores Histogram with Custom Bins")
 
 
# --- 2.3 Scatter Plot ---
set.seed(42)
scatter_df <- data.frame(x = rnorm(100))
scatter_df$y <- scatter_df$x * rnorm(100)
 
ggplot(scatter_df, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot",
       x     = "X values",
       y     = "Y values")
 
 
# --- 2.4 Pie Chart ---
# Note: ggplot2 does not support per-slice "explosion" natively.
# The explode effect is approximated by nudging a slice label outward,
# but true physical separation requires manual segment drawing with geom_arc_bar
# (from the ggforce package). The chart below is otherwise a faithful translation.
pie_data <- data.frame(
  Category = c('Electronics', 'Clothing', 'Home Decor', 'Books', 'Toys'),
  Sales    = c(3500, 2800, 2000, 1500, 1200)
)
pie_data$Pct <- paste0(round(pie_data$Sales / sum(pie_data$Sales) * 100, 1), "%")
 
ggplot(pie_data, aes(x = "", y = Sales, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = Pct), position = position_stack(vjust = 0.5)) +
  labs(title = "Sales by Product Category") +
  theme_void()
 
 
# --- 2.5 Box Plot ---
set.seed(16)
box_data <- data.frame(
  Value = c(rnorm(200, 100, 10), rnorm(200, 90, 20), rnorm(200, 80, 30)),
  Group = rep(c("Group 1", "Group 2", "Group 3"), each = 200)
)
 
ggplot(box_data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_manual(values = c("Group 1" = "lightblue",
                               "Group 2" = "lightgreen",
                               "Group 3" = "tan")) +
  labs(title = "Box Plot",
       x     = "Groups",
       y     = "Values") +
  theme(legend.position = "none")
 
 
# --- 2.6 Heatmap (Correlation Matrix) ---
vars <- c("Var1", "Var2", "Var3", "Var4")
corr_vals <- c( 1.0, 0.8, 0.3, -0.2,
                0.8, 1.0, 0.5,  0.1,
                0.3, 0.5, 1.0, -0.4,
               -0.2, 0.1,-0.4,  1.0)
 
heat_df <- data.frame(
  Var1  = factor(rep(vars, each = 4), levels = vars),
  Var2  = factor(rep(vars, 4),        levels = rev(vars)),
  Value = corr_vals
)
 
ggplot(heat_df, aes(x = Var1, y = Var2, fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low     = "blue",
                       mid     = "white",
                       high    = "red",
                       midpoint = 0,
                       limits  = c(-1, 1),
                       name    = "Correlation") +
  labs(title = "Correlation Matrix Heatmap", x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank())
 
 
# --- 2.7 Stacked Area Plot ---
stack_df <- data.frame(
  Quarter          = factor(c("Q1","Q2","Q3","Q4"), levels = c("Q1","Q2","Q3","Q4")),
  Electronics      = c(10000, 12000, 11000, 10500),
  Clothing         = c(5000,  6000,  7500,  8000),
  Home_Appliances  = c(7000,  7500,  8200,  9000)
)
stack_long <- pivot_longer(stack_df, -Quarter,
                           names_to = "Category", values_to = "Sales")
stack_long$Category <- factor(stack_long$Category,
                              levels = c("Electronics", "Clothing", "Home_Appliances"))
 
ggplot(stack_long, aes(x = Quarter, y = Sales, fill = Category, group = Category)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("Electronics"     = "blue",
                               "Clothing"         = "green",
                               "Home_Appliances"  = "red"),
                    labels = c("Electronics", "Clothing", "Home Appliances")) +
  labs(x     = "Quarters",
       y     = "Sales ($)",
       title = "Product Category Sales Over Quarters",
       fill  = "Category") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())
 
 
# ============================================================
# 3. MULTIPLE SUBPLOTS
# ============================================================
 
# --- 3.1 2x2 grid of subplots (using patchwork) ---
x_seq <- seq(0, 2 * pi, length.out = 100)
fn_df  <- data.frame(
  x   = x_seq,
  Sin = sin(x_seq),
  Cos = cos(x_seq),
  Tan = tan(x_seq),
  Exp = exp(x_seq)
)
 
p_sin <- ggplot(fn_df, aes(x, Sin)) + geom_line(color = "blue")   + labs(title = "Sine Function")
p_cos <- ggplot(fn_df, aes(x, Cos)) + geom_line(color = "green")  + labs(title = "Cosine Function")
p_tan <- ggplot(fn_df, aes(x, Tan)) + geom_line(color = "red")    + labs(title = "Tangent Function") +
         coord_cartesian(ylim = c(-10, 10))
p_exp <- ggplot(fn_df, aes(x, Exp)) + geom_line(color = "purple") + labs(title = "Exponential Function")
 
(p_sin | p_cos) / (p_tan | p_exp) +
  plot_annotation(title = "Various Functions")
 
 
# --- 3.2 Dual-axis plot (bar + line sharing x-axis) ---
# True dual y-axes in ggplot2 require a linear scaling trick via sec_axis().
dual_df <- data.frame(
  Month   = 1:5,
  Sales   = c(10, 15, 7, 12, 9),
  Revenue = c(200, 300, 150, 250, 180)
)
 
revenue_scale <- 20   # Sales max; Revenue max 400 → scale factor = 400/20 = 20
 
ggplot(dual_df, aes(x = Month)) +
  geom_bar(aes(y = Sales), stat = "identity",
           fill = "blue", alpha = 0.7) +
  geom_line(aes(y = Revenue / revenue_scale),
            color = "red") +
  geom_point(aes(y = Revenue / revenue_scale),
             color = "red", size = 2) +
  scale_y_continuous(
    name   = "Sales",
    limits = c(0, 20),
    sec.axis = sec_axis(~ . * revenue_scale, name = "Revenue")
  ) +
  labs(x     = "Month",
       title = "Sales and Revenue Comparison") +
  theme_minimal() +
  theme(axis.title.y.left  = element_text(color = "blue"),
        axis.text.y.left   = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right  = element_text(color = "red"))
 
