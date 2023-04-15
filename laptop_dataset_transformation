df <- CleandLaptopData |>
  tibble()

## Due to having too many missing values in the display size, I decided to replace it with average values

mean_dps <- df |>
  select(display_size) |>
  filter(display_size != "Missing") |>
  unlist() |>
  as.numeric()

mean_dps <- as.character(round(mean(mean_dps), 2))

## To select important product specifications that can be used for analyzing
df <- df |>
  select(brand, model, 
         processor_brand, processor_name,
         ram_gb, ram_type, ssd, hdd,
         os, os_bit, graphic_card_gb,
         weight, display_size, Touchscreen,
         latest_price, star_rating) |>
  mutate(display_size = replace(
    display_size, display_size == "Missing", mean_dps))

## Converting to the suitable data types
df$display_size <- as.double(df$display_size) ##double for display_size
df[, -c(13, 15, 16)] <- lapply(df[, -c(13, 15, 16)], as.factor) ##factor for others

## To sort the most numbers of 5 brands in the dataset 
freq_b <- table(df$brand)
sorted_b <- sort(freq_b, decreasing = TRUE)
print(names(sorted_b)[1:5])

## Results
[1] "ASUS"   "DELL"   "Lenovo" "HP"     "acer"

# convert brands to lowercase
df$brand <- as.factor(tolower(df$brand))

## To sort by rating
df <- df[order(df$star_rating, decreasing = TRUE), ]

## To filter only top five frequent brands
top5df <- df |>
  filter(brand %in% c("asus", "dell", "lenovo", "hp", "acer"))
  
## To find processor and ram type in products that get rating >= 4.0
pop_pcsr <- top5df |>
  filter(star_rating >= 4.0) |>
  select(3,4,6) |>
  group_by(processor_brand, processor_name, ram_type) |>
  summarise(n=n(), .groups = "drop") |>
  arrange(desc(n))
  
## To analyze suitable display size data
## Average and quartile display size
fstar <- top5df |>
  filter(star_rating >= 4.0)

## Average display size
avg_dis <- fstar |>
  summarise(avg_dps = mean(display_size))

## Results of average display size
1    15.2

## Quantile value of display size
q_dis <- fstar |>
        select(display_size)
q_dis <- quantile(q_dis, probs = c(0, .25, .5, .75, 1), na.rm = T)

## Results of all five brands
0%   25%   50%   75%  100% 
13.00 15.12 15.12 15.60 17.30

## Touchscreen ratio
t_dis <- fstar |> count(Touchscreen, sort = T) |>
        mutate(percent = n/sum(n)*100)
        
## Average latest product prices
avg_p <- fstar |>
  summarise(avg_p = mean(latest_price))
## Result for average latest product prices
1 67144

## Quantile value of latest product prices
q_p <- fstar |>
  select(latest_price)
q_p <- quantile(q_p, probs = c(0, .25, .5, .75, 1), na.rm = T)
## Results of all five brands
0%       25%       50%       75%      100% 
19990.00  44517.25  59490.00  75997.50 441990.00
