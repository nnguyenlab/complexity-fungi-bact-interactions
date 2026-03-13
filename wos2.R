#Github March 9th, 2026
#BFI Interactions paper - Figure 1
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(scales)

#Feb 26, 2026 - changes wos search to bacteri* AND fung* AND interact*
df <- read_excel("gs_wos_search2.xlsx", sheet = "data") %>% rename( year = `Publication Years`,
                                                                    total = Count_Total, bfi   = Count_BFI) %>%
  mutate(year = as.integer(year), bfi = as.numeric(bfi), total = as.numeric(total)) %>%
  arrange(year)

df_long <- df %>% select(year, total, bfi) %>%
  pivot_longer(cols = c(total, bfi), names_to = "metric", values_to = "count") %>%
  mutate(metric = recode(metric, total = "Total Publications", bfi   = "Total Fungal-Bacterial Interactions Publications"))

p_lines <- ggplot(df_long, aes(x = year, y = count, color = metric)) +
  geom_line(linewidth = 1) + geom_point(size = 1, alpha = 0.8) +
  scale_color_manual(
    values = c("Total Publications" = "#0072B2",  
               "Total Fungal-Bacterial Interactions Publications" = "#D55E00")) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year",y = "Number of Publications") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        legend.text = element_text(size = 11),
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

p_lines

p_lines_log <- p_lines +
  scale_y_log10(labels = comma) +
  labs(y = "Number of Publications (log10 scale)")

p_lines_log

ggsave("p_lines_log2.pdf")
