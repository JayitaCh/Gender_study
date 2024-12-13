install.packages("networkD3")
require(dplyr)
require(tidyverse)
require(networkD3)


# ################################################################# #
#### LOAD DATA AND REMOVE OTHER COLUMNS EXCEPT FOR SAFETY PARAMS  ####
# ################################################################# #
db <- read.csv('data/SP_Gendersafety.csv')

db_select <- db[,c(15,24:31)]

# Move the first column to the last
db_select <- db_select[, c(2:ncol(db_select), 1)]

db_select <- db_select %>%
  mutate(
    wait = ifelse(Choice == "Metro", swaitenv_metro, swaitenv_bus),
    safety = ifelse(Choice == "Metro", safety_metro, safety_bus),
    crowding = ifelse(Choice == "Metro", sboal_metro, sboal_bus),
    stop_access = ifelse(Choice == "Metro", saccstop_metro, saccstop_bus)
  )

# ################################################################# #
#### CREATE LINKS AND NODES TO CREATE SANKEY DIAGRAM  ####
# ################################################################# #

# Create flows between columns (source and target pairs)
links <- db_select %>%
  select(wait, safety, crowding, stop_access, Choice) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  mutate(Next_Variable = lead(Variable), 
         Next_Category = lead(Category)) %>%
  filter(!is.na(Next_Variable)) %>%
  group_by(Variable, Category, Next_Variable, Next_Category) %>%
  summarise(value = n(), .groups = "drop") %>%
  ungroup()
links <- links %>%
  arrange(desc(Variable))
links <- links[1:(nrow(links)-9), ]
links <- links[c(1:9,19:30,10:18), ]

# Create nodes dataframe
nodes <- data.frame(name = unique(c(links$Category, links$Next_Category)))

# Map source and target to node indices
links <- links %>%
  mutate(source = match(Category, nodes$name) - 1,
         target = match(Next_Category, nodes$name) - 1) %>%
  select(source, target, value)

# Add a 'group' column to each connection:
links$group <- as.factor(links$target)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(nodes$name)

# Create a color scale for the nodes
my_color <- 'd3.scaleOrdinal()
  .domain(["Staff/police presence or help booths in public transport stops", 
  "Other people present at the stop", "Waiting alone",
  "Crowded standing","Comfortable standing with adequate grab handles for support",
  "Seating space available","Boarding/alighting in crowded conditions",
  "Passengers following queue behaviour while boarding",
  "Access infrastructure as of now","Active and well-lit streets and footpaths",
  "Clear signages and boards","Bus","Metro","None"])
  .range(["#dd80de","#f1ade1","#b15bad",
  "#eecc7d","#f0ed7c","#d39e02",
  "#b7ff80","#a9e03a",
  "#fddcff","#ffb1a8","#fe6ab0",
  "#c7ecf9","#8ec0fe","#6ca3ef"])'

# Create Sankey diagram
sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  colourScale = my_color,
  units = "Individuals",
  fontSize = 16,
  nodeWidth = 40
)

# htmlwidgets::onRender(sankey_plot,         '
#   function(el,x) {
#     d3.selectAll(".node text").remove()
#     d3.selectAll(".node")
#       .append("foreignObject")
#       .attr("width", 100)
#       .attr("height", 50)
#       .html(function(d) { return d.name; })
#   }
#   '
# )

# Add custom JavaScript to wrap text
js_code <- '
function wrap(text, width) {
  text.each(function() {
    var text = d3.select(this),
        words = text.text().split(/\\s+/).reverse(),
        word,
        line = [],
        lineNumber = 0,
        lineHeight = 1.2, // ems
        y = text.attr("y"),
        dy = parseFloat(text.attr("dy")),
        tspan = text.text(null).append("tspan").attr("x", 0).attr("y", y).attr("dy", dy + "em").style("font-weight", "bold");
    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > width) {
        line.pop();
        tspan.text(line.join(" "));
        line = [word];
        tspan = text.append("tspan").attr("x", 0).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word).style("font-weight", "bold");
      }
    }
  });
}

d3.selectAll(".node text")
  .attr("x", function(d) { return d.dx/2; })  // Center horizontally
  .attr("y", function(d) { return d.dy/2; })  // Center vertically
  .attr("text-anchor", "left")  // Anchor text in the middle
  .style("font-family", "Verdana, sans-serif")  // Change font type
  .call(wrap, 200);  // Adjust the width as needed
  
d3.selectAll(".link")
  .style("stroke", d => d.source.color) // Color the links based on node
  .style("stroke-opacity", 0.5);  // Reduce the opacity of the links
'

sankey_plot <- htmlwidgets::onRender(sankey_plot, js_code)

# Plot the Sankey diagram
sankey_plot