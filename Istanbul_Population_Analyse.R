calculate_annual_growth <- function(year_one,year_two,pop_y1, pop_y2,city) {
  annual_growth <- (((pop_y2 - pop_y1) / pop_y1) * 100) / (year_two-year_one)
  message <- paste("From", year_one, "to", year_two, "the population of", city, "grew by approximately", annual_growth, "% each year.")
  print(message)
  return(annual_growth)
}
# Write your code starting here:
city <- "Istanbul, Turkey"
pop_y1 <- 691000
pop_y2 <- 15029231
pop_y3 <- 983000
pop_y4 <- 8831800
year_one <- 1927
year_two <- 2017
year_three <- 1950
year_four <- 2000
pop_change <- pop_y2 - pop_y1 
pop_change2 <- pop_y4 - pop_y3 
percentage_gr <- (pop_change-pop_y1) *100
percentage_gr2 <- (pop_change2-pop_y3) *100

pop_list <- list(pop_y1, pop_y3, pop_y4, pop_y2)
year_list <- list(year_one,year_three,year_four,year_two)


annual_gr <- percentage_gr / (year_two - year_one)
annual_gr2 <- percentage_gr2 / (year_four - year_three)
cag <- calculate_annual_growth (year_one,year_two,pop_y1, pop_y2,city)
print (annual_gr)
cag2 = calculate_annual_growth (year_three,year_four,pop_y3, pop_y4,city)
print (annual_gr2)
plot(x = year_list, y = pop_list, xlab = "Yıllar", ylab = "Nüfus",col = "dark red")
lines(x=year_list, y=pop_list)
help("plot")
