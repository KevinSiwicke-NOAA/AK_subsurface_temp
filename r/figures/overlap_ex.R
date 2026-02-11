# Load necessary libraries
library(ggplot2)
library(reshape2)
library(cowplot)
library(dplyr)

# Create a 5x5 grid with random values of 0 or 1
grida1 <- matrix(c(0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0), nrow = 5, ncol = 5)
gridb1 <- matrix(c(0,0,0,0,0,0,2,2,2,0,0,2,2,2,0,0,2,2,2,0,0,0,0,0,0), nrow = 5, ncol = 5)

grida2 <- matrix(c(0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0), nrow = 5, ncol = 5)
gridb2 <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,2,2,0,0,0,0,0,0), nrow = 5, ncol = 5)

grida3 <- matrix(c(0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0), nrow = 5, ncol = 5)
gridb3 <- matrix(c(0,0,0,0,0,0,0,2,2,2,0,0,2,2,2,0,0,2,2,2,0,0,0,0,0), nrow = 5, ncol = 5)

grida4 <- matrix(c(0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0), nrow = 5, ncol = 5)
gridb4 <- matrix(c(0,0,0,0,0,0,0,0,2,2,0,0,0,2,2,0,0,0,2,2,0,0,0,0,0), nrow = 5, ncol = 5)



grid_fun <- function(grid){
  grid_df <- as.data.frame(grid)
  grid_df$row <- 1:nrow(grid_df)
  grid_long <- melt(grid_df, id.vars = "row", variable.name = "col", value.name = "value")
  grid_long$col <- as.numeric(gsub("V", "", grid_long$col))
  return(grid_long)
}

ga1 <- grid_fun(grid = grida1) 
gb1 <- grid_fun(grid = gridb1)
ex1 <- rbind(ga1, gb1) |> group_by(row, col) |> summarize(grp = sum(value)) |> 
  mutate(grp = ifelse(grp == 0, "None", 
                      ifelse(grp == 1, "L_t only", 
                             ifelse(grp == 2, "T_t only", 
                                    ifelse(grp == 3, "L_t and T_t", NA)))))

ga2 <- grid_fun(grid = grida2) 
gb2 <- grid_fun(grid = gridb2)
ex2 <- rbind(ga2, gb2) |> group_by(row, col) |> summarize(grp = sum(value)) |> 
  mutate(grp = ifelse(grp == 0, "None", 
                      ifelse(grp == 1, "L_t only", 
                             ifelse(grp == 2, "T_t only", 
                                    ifelse(grp == 3, "L_t and T_t", NA)))))

ga3 <- grid_fun(grid = grida3)
gb3 <- grid_fun(grid = gridb3) 
ex3 <- rbind(ga3, gb3) |> group_by(row, col) |> summarize(grp = sum(value)) |> 
  mutate(grp = ifelse(grp == 0, "None", 
                      ifelse(grp == 1, "L_t only", 
                             ifelse(grp == 2, "T_t only", 
                                    ifelse(grp == 3, "L_t and T_t", NA)))))


ga4 <- grid_fun(grid = grida4)
gb4 <- grid_fun(grid = gridb4) 
ex4 <- rbind(ga4, gb4) |> group_by(row, col) |> summarize(grp = sum(value)) |> 
  mutate(grp = ifelse(grp == 0, "None", 
                      ifelse(grp == 1, "L_t only", 
                             ifelse(grp == 2, "T_t only", 
                                    ifelse(grp == 3, "L_t and T_t", NA)))))


# Plot the grid using ggplot2
plot1 <- ggplot(ex1, aes(x = col, y = row, fill = grp)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("None" = "white", "L_t only" = "grey50", "T_t only" = "grey90", "L_t and T_t" = "grey10")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "none")

plot2 <- ggplot(ex2, aes(x = col, y = row, fill = grp)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("None" = "white", "L_t only" = "grey50", "T_t only" = "grey90", "L_t and T_t" = "grey10")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "none")

plot3 <- ggplot(ex3, aes(x = col, y = row, fill = grp)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("None" = "white", "L_t only" = "grey50", "T_t only" = "grey90", "L_t and T_t" = "grey10")) +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.title = element_blank())

plot4 <- ggplot(ex4, aes(x = col, y = row, fill = grp)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("None" = "white", "L_t only" = "grey50", "T_t only" = "grey90", "L_t and T_t" = "grey10")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "none")

legend <- get_legend(plot3)
plots <- plot_grid(plot1, plot2, plot3 + theme(legend.position = 'none'), plot4, nrow = 2,
                   labels = c('A', 'B', 'C', 'D'))

plot_grid(plots, legend, ncol=1, rel_heights = c(1, .1))
ggsave("results/plots/overlap_example_plot.png", height = 6, width = 6, dpi = 300, bg = "white")
