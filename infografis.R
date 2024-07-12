library(tidyverse)
library(nycflights23)
library(patchwork)
# library()

data("airlines")
data("flights")
data("airports")

flights_new <- flights |> 
  left_join(airlines, by = join_by(carrier == carrier))


flights_new |> select(year) |> distinct()

avg_dep_delay <- flights_new |> group_by(name) |> 
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(avg_dep_delay))


plot1 <- ggplot(avg_dep_delay, aes(x = fct_reorder(name, avg_dep_delay), y = avg_dep_delay)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "tomato") +
  theme_minimal() +
  labs(title = "Average Departure Delay by Airlines",
       x = "",
       y = "Mean Delay (minutes)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 7),
        plot.title.position = "plot")

plot2 <- flights_new |> group_by(name) |> 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(avg_arr_delay)) |> 
  ggplot(aes(x = fct_reorder(name, avg_arr_delay), y = avg_arr_delay)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "#8956DA") +
  theme_minimal() +
  labs(title = "Average Arrival Delay by Airlines",
       x = "",
       y = "Mean Delay (minutes)",
       caption = "Source: USA Flight Database") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 7),
        plot.title.position = "plot")

plot_text <- ggplot(data.frame(x = 0, y = 0, text = "Sepanjang tahun 2023, terdapat beberapa maskapai dengan performa terbaik.\nDiantaranya secara berturut-turut adalah Allegiant Air, Republic Air, dan Endeavor air.\nKriteria penilaian kinerja maskapai dihitung berdasarkan rata-rata delay, baik berangkat maupun tiba."), 
                    aes(x = x, y = y, label = text)) +
  geom_text(hjust = 0, vjust=1) + 
  theme_void() +
  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.2, 0), breaks = 0.1) +
  labs(title = "Summary")

plot1 + plot_text/ plot2

flights_new |> select(month) |> distinct()

