library(tidyverse)
library(lubridate)
library(colormap) #for category colours

#results is a global object containing all the road results


#Make a copy of the results with only relevant columns selected
data <- results[,c("full_name", "race_name", "race_date", "placing", "category")]

#Extract the year from race dates and assign it to a new variable
data$season <- year(data$race_date)

category_names <- c("A", "AF", "B", "BF", "C", "CF", "D", "F", "U13F", "U13M", "U15F", "U15M", "U17F", "U17M", "U19F", "U19M")
#category_names <- c("A", "AF", "B", "BF", "C", "CF", "D", "F", "U19M", "U19F", "U17M", "U17F", "U15M", "U15F", "U13M", "U13F")
point_levels <- c("100", "85", "75", "60", "55", "50", "45", "40", "35", "30", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1", rep("1",40), "DNF", "DNS", "DSQ")
best_races_df <- data.frame(season = c('2017', '2016', '2015', '2014', '2013', '2012', '2011'), r_in_season = c(6,7,3,5,5,7,6))
point_df <- data.frame(placing = c(1:57, "DNF", "DSQ", "DNS"), points = as.numeric(c(point_levels[1:57],  rep(0,3))))

seasons <- unique(year(results$race_date))


#POINTS STANDINGS
#BNS points map 1 to 57 + special
data <- merge(x = data, y = point_df, by = "placing")
data <- merge(x = data, y = best_races_df, by = "season")

#Calculate the annual unique sizes of each category
cat_summary <- data %>%
  select(season, full_name, category, placing) %>%
  group_by(season, category) %>%
  summarise(n_unique = n_distinct(full_name)) %>%
  ungroup()


season_summary <- cat_summary %>%
  group_by(season) %>%
  summarize(n_sum = sum(n_unique))

#Calculate the points standings based on the best_races per season
standings <- data %>%
  group_by(season, category, full_name) %>%
  summarize(top_n = sum(head(sort(points, decreasing = TRUE), first(r_in_season)))) %>% #Sum the top n points where n is the first r_in_season in the group
  ungroup() %>%
  group_by(season, category) %>%
  mutate(rank = min_rank(-top_n))#Add a rank. Use min_rank to record gaps between ties, eg: 1 1 1 4 5



category_colours = data.frame(category = category_names)
category_colours$colour <- colormap(colormap = colormaps$viridis, nshades = 16)
category_colours$colour <- substr(category_colours$colour, 1, nchar(category_colours$colour) - 2) #remove trailing "FF"


svg_text <- '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:cc="http://creativecommons.org/ns#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" id="svg8" version="1.1" viewBox="0 0 1000 1000" height="1000" width="1000"> <defs id="defs2" /><metadata id="metadata5"><rdf:RDF><cc:Work rdf:about=""><dc:format>image/svg+xml</dc:format> <dc:type rdf:resource="http://purl.org/dc/dcmitype/StillImage" /><dc:title></dc:title></cc:Work> </rdf:RDF></metadata>'



y_cursor <- 900 #svg origin is at the top-left, this puts it at the bottom of the drawing


first_season_width <- season_summary$n_sum[1] #To be used for centering purposes

start_coordinates <- data.frame()

cat_x_gap <- 10
for(i in 1:length(seasons)) {
  season_filtered <- filter(cat_summary, season == seasons[i])

  season_width <- season_summary$n_sum[i]
  season_length <- nrow(season_filtered)


  #offset the x_cursor so ensure the seasons are centered vertically
  #abs in case it returns a decimal
  x_cursor <- ceiling(((first_season_width + (first_season_width * cat_x_gap)) - (season_width + (season_length * cat_x_gap))) / 2)

  for (j in 1:season_length) {
    #add 1 so the last in list isn't overhanging
    category_name <- season_filtered$category[j]

    width <- season_filtered$n_unique[j] + 1
    season_filtered[season_filtered$category == category_name,"x_1"] <- x_cursor
    season_filtered[season_filtered$category == category_name,"y_1"] <- y_cursor

    svg_text <- paste(svg_text,
    '<rect
        y="', y_cursor, '"
        x="', x_cursor, '"
        height="16"
        width="', width, '"
        style="stoke:none;fill:', filter(category_colours, category == category_name)$colour, ';" />',sep="")

    x_cursor <- x_cursor + width + cat_x_gap #add a gap betwen rectangles
  }

  start_coordinates <- rbind(start_coordinates, season_filtered[, c("season", "category", "x_1", "y_1")])
  y_cursor <- y_cursor - 200
}


unique_names <- unique(standings$full_name)

for (i in 1:length(unique_names)) {
  nam <- unique_names[i]

  individual_results <- filter(standings, full_name == nam)


  path <- '<path d="M '

  #Only those that have raced more than once will have a trajectory
  if (nrow(individual_results) >= 5) {
    for (j in 1:(nrow(individual_results)-1)) {
      res1 <- individual_results[j,]
      res2 <- individual_results[j+1,]

      start_coords1 <- filter(start_coordinates, season == res1$season, category == res1$category)
      start_coords2 <- filter(start_coordinates, season == res2$season, category == res2$category)

      #Structure Layout
      #M point1_x,point1_y C handle1_x,handle1_y handle2_x,handle2_y, point2_x,point2_y
      #M 396.5,900 C 396.5,811.93807 371.5,809.97234 371.5,716

      point1_x <- start_coords1$x_1 + res1$rank
      point1_y <- start_coords1$y_1

      point2_x <- start_coords2$x_1 + res2$rank
      point2_y <- start_coords2$y_1 + 16

      handle1_x <- point1_x
      handle1_y <- point1_y - (abs(point1_y - point2_y) / 2)

      handle2_x <- point2_x
      handle2_y <- handle1_y

      if (j == 1) {
        path <- paste(path, point1_x, ",", point1_y, " C", sep='')
      }

      path <- paste(path, " ", handle1_x, ",", handle1_y, " ",
                          handle2_x, ",", handle2_y, " ",
                          point2_x, ",", point2_y, sep="")


    }

    path <- paste(path, '" style="fill:none;stroke:', filter(category_colours, category == first(individual_results$category))$colour, ';stroke-width:1;stroke-opacity:0.5" />
              ', sep = '')

    svg_text <- paste(svg_text, path, sep = "")
  }

  print(paste(i, "/", length(unique_names), " | ", nam, sep = ""))
}



svg_text <- paste(svg_text, '</svg>', sep="")
cat(svg_text, file="graph_output.svg")