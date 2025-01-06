library(tidyverse)

mu <- 5.75
sig = 1.2
n <- 8
#n <- 72

s2 <- round(rnorm(n, mu, sig), 1)
s2
mean(s2)

s3 <- round(rnorm(n, mu, sig), 1)
s3
mean(s3)

my_data <- tibble(
  id = 1:100,
  waits = c(5.9, 5.3, 5.2, rep(NA, 97))
)

for(i in 3:100){
  s <- round(rnorm(n, mu, sig), 1)
  my_data$waits[i] <- mean(s)
}

# for(i in 1:100){
#   s <- round(rnorm(n, mu, sig), 1)
#   my_data$waits[i] <- mean(s)
# }

my_data %>%
  #slice(1:3) %>%
  ggplot() + 
  geom_point(aes(x = waits, y = id)) + 
  ylim(c(0, 105)) +
  xlim(c(3.5, 7.5)) +
  labs(x = "Average Wait Time",
       y = "") +
  theme_bw(base_size = 18)

my_data8 <- my_data
my_data72 <- my_data

my_data <- (my_data8 %>%
              mutate(size = "n = 8")) %>%
  bind_rows(my_data72 %>%
              mutate(size = "n = 72"))

my_data %>%
  ggplot() + 
  geom_histogram(aes(x = waits, y = after_stat(density)),
                 color = "black",
                 fill = "purple",
                 bins = 15) + 
  geom_density(aes(x = waits),
               color = "black",
               fill = "purple",
               alpha = 0.6) + 
  facet_wrap(~size, ncol = 1) +
  labs(x = "Average Wait Time",
       y = "") + 
  theme_bw(base_size = 18)

my_big_data <- tibble(
  size = c(rep("n = 8", 5e4), rep("n = 72", 5e4)),
  n = c(rep(8, 5e4), rep(72, 5e4)),
  waits = rep(NA, 2*5e4)
)

for(i in 1:(2*5e4)){
    s <- round(rnorm(my_big_data$n[i], mu, sig), 1)
    my_big_data$waits[i] <- mean(s)
}

my_big_data %>%
  ggplot() + 
  geom_histogram(aes(x = waits, y = after_stat(density)),
                 color = "black",
                 fill = "purple") + 
  geom_density(aes(x = waits),
               color = "black",
               fill = "purple",
               alpha = 0.6) + 
  facet_wrap(~size, ncol = 1) +
  labs(x = "Average Wait Time",
       y = "") + 
  theme_bw(base_size = 18)

which.max(my_data72$waits)
max(my_data72$waits)

which.min(my_data72$waits)
min(my_data72$waits)

my_data72[45, ]

my_data72 %>%
  #slice(1:3) %>%
  ggplot() + 
  geom_point(aes(x = waits, y = id),
             alpha = 0.4) + 
  #geom_point(x = 6.108, y = 97, size = 3) +
  #geom_point(x = 5.436, y = 8, size = 3) +
  geom_point(x = 5.70, y = 45, size = 3) +
  ylim(c(0, 105)) +
  xlim(c(3.5, 7.5)) +
  labs(x = "Average Wait Time",
       y = "") +
  theme_bw(base_size = 18)



####CI Plots
my_data72 %>%
  mutate(
    lower_90 = waits - 1.645*1.2/sqrt(72),
    upper_90 = waits + 1.645*1.2/sqrt(72),
    lower_95 = waits - 1.96*1.2/sqrt(72),
    upper_95 = waits + 1.96*1.2/sqrt(72),
    lower_99 = waits - 2.58*1.2/sqrt(72),
    upper_99 = waits + 2.58*1.2/sqrt(72),
  ) %>%
  mutate(
    contains_90 = between(rep(5.75, 100), lower_90, upper_90),
    contains_95 = between(rep(5.75, 100), lower_95, upper_95),
    contains_99 = between(rep(5.75, 100), lower_99, upper_99)
  ) %>%
  ggplot() + 
  geom_vline(xintercept = 5.75,
             linetype = "dashed") +
  geom_segment(aes(x = waits - 1.96*1.2/sqrt(72),
                   xend = 5.75,
                   y = id,
                   yend = id,
                   color = contains_95,
                   lwd = ifelse(contains_95, 0.25, 0.35))) +
  geom_segment(aes(x = waits + 1.96*1.2/sqrt(72),
                   xend = 5.75,
                   y = id,
                   yend = id,
                   color = contains_95,
                   lwd = ifelse(contains_95, 0.25, 0.35))) +
  geom_point(aes(x = waits, y = id)) + 
  scale_color_manual(values = c("red", "#6F4E37")) + 
  labs(
    title = "95% Confidence Intervals",
    subtitle = "Critical Value 1.96",
    x = "Average Wait Time",
    y = "Sample Number"
  ) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "None") + 
  coord_cartesian(xlim = c(4.75, 6.75))

my_data72 %>%
  mutate(
    lower_90 = waits - 1.645*1.2/sqrt(72),
    upper_90 = waits + 1.645*1.2/sqrt(72),
    lower_95 = waits - 1.96*1.2/sqrt(72),
    upper_95 = waits + 1.96*1.2/sqrt(72),
    lower_99 = waits - 2.58*1.2/sqrt(72),
    upper_99 = waits + 2.58*1.2/sqrt(72),
  ) %>%
  mutate(
    contains_90 = between(rep(5.75, 100), lower_90, upper_90),
    contains_95 = between(rep(5.75, 100), lower_95, upper_95),
    contains_99 = between(rep(5.75, 100), lower_99, upper_99)
  ) %>%
  ggplot() + 
  geom_vline(xintercept = 5.75,
             linetype = "dashed") +
  geom_segment(aes(x = waits - 1.65*1.2/sqrt(72),
                   xend = 5.75,
                   y = id,
                   yend = id,
                   color = contains_90,
                   lwd = ifelse(contains_90, 0.25, 0.35))) +
  geom_segment(aes(x = waits + 1.65*1.2/sqrt(72),
                   xend = 5.75,
                   y = id,
                   yend = id,
                   color = contains_90,
                   lwd = ifelse(contains_90, 0.25, 0.35))) +
  geom_point(aes(x = waits, y = id)) + 
  scale_color_manual(values = c("red", "#6F4E37")) + 
  labs(
    title = "90% Confidence Intervals",
    subtitle = "Critical Value 1.65",
    x = "Average Wait Time",
    y = "Sample Number"
  ) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "None") + 
  coord_cartesian(xlim = c(4.75, 6.75))

my_data72 %>%
  mutate(
    lower_90 = waits - 1.645*1.2/sqrt(72),
    upper_90 = waits + 1.645*1.2/sqrt(72),
    lower_95 = waits - 1.96*1.2/sqrt(72),
    upper_95 = waits + 1.96*1.2/sqrt(72),
    lower_99 = waits - 2.33*1.2/sqrt(72),
    upper_99 = waits + 2.33*1.2/sqrt(72),
  ) %>%
  mutate(
    contains_90 = between(rep(5.75, 100), lower_90, upper_90),
    contains_95 = between(rep(5.75, 100), lower_95, upper_95),
    contains_99 = between(rep(5.75, 100), lower_99, upper_99)
  ) %>%
  ggplot() + 
  geom_vline(xintercept = 5.75,
             linetype = "dashed") +
  geom_segment(aes(x = waits - 2.58*1.2/sqrt(72),
                   xend = 5.75,
                   y = id,
                   yend = id,
                   color = contains_99,
                   lwd = ifelse(contains_99, 0.25, 0.35))) +
  geom_segment(aes(x = waits + 2.58*1.2/sqrt(72),
                   xend = 5.75,
                   y = id,
                   yend = id,
                   color = contains_99,
                   lwd = ifelse(contains_99, 0.25, 0.35))) +
  geom_point(aes(x = waits, y = id)) + 
  scale_color_manual(values = c("red", "#6F4E37")) + 
  labs(
    title = "98% Confidence Intervals",
    subtitle = "Critical Value 2.33",
    x = "Average Wait Time",
    y = "Sample Number"
  ) + 
  theme_bw(base_size = 18) +
  theme(legend.position = "None") + 
  coord_cartesian(xlim = c(4.75, 6.75))
