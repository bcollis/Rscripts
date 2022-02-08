####Intor to R Graphics####
#We assume a data frame containing cross sectional data with columns of various types
#We need packages ggplot2 and scales
library(ggplot2)
library(scales)
rm(list = ls())

#Our Data
#We will use the Des Moines Housing data—with latitude and longitude of the street addresses
#We only look at two cities (Des Moines and West Des Moines) for simplicity
#Read in Data
df <- read.csv("des_moines_housing_with_latlon.csv", stringsAsFactors = FALSE)
#Subset to just Des Moines and WDM
df <- subset(df, city == "Des Moines" | city == "West Des Moines")

#Change data types to factor
df$city <- factor(df$city); df$zip <- factor(df$zip)
df$walkout <- factor(df$walkout); df$condition <- factor(df$condition)
df$AC <- factor(df$AC); df$sale_date <- as.Date(df$sale_date)

# Reorder condition to "Below Normal", "Normal", "Above Normal"
df$condition <- factor(df$condition, c("Below Normal", "Normal", "Above Normal"))

#Overall Plan
#We discuss four aspects of dealing with graphics:
  #1. Simple adjustments to qplot
  #2. Scales (for altering data-related aspects)
  #3. Themes (for altering non-data aspects)
  #4. Exporting (for including in reports, presentations, etc.)

####1. Simple adjustments to qplot####
#Aside: Saving Plots to a Variable
#You can save the results of qplot to a variable, say, p
#The plot can then be modified using +
#Or it can be printed to screen using p or print(p)
#We will see many examples

#Use qplot to build a bar plot
p <- qplot(condition, data = df, geom = "bar")
p

#Add fill color, outline color, transpararancy
#Use I(...) to “inhibit” R from treating your non-data choices as data related
p <- qplot(condition, data = df, geom = "bar",
           fill = I("lightblue"), # Add a light blue fill
           color = I("red"),      # Make color of rectangles red
           alpha = I(0.5))        # Set 50% transparancy
p


####2. Scales####
#Aesthetics
#Graphics are built by assigning data to various aesthetics
  #x and y
  #color
  #fill
  #alpha
  #size
  #shape
#A graphic has a separate scale for each aesthetic used in the graphic

#Scales for x and y
#For a numerical column of data (e.g., class numeric or integer), scale_x_continuous 
#yields the natural scale for the x axis. It has the following options:
  #name = name of axis
  #breaks = locations of tick marks along axis
  #labels = text labels at tick marks
  #limits = lower and upper limits of axis
#scale_x_log10 yields the logarithmic scale; has same options
#Analogous commands for y
#Leave defaults if no reason to change them

#Example -  Build a histogram plot, setting binwidth
p <- qplot(sale_price, data = df, geom = "histogram", binwidth = 25000)
p

# Create breaks for y axis
breaks <- 50*(0:10)
breaks

#Update plot features
p <- p + scale_x_continuous( # Update x axis
  name = "Sale Price",     # Update name, i.e., axis title
  labels = dollar)         # Label axis as dollars. Needs library(scales)

p <- p + scale_y_continuous( # Update y axis
  name = "Frequency",      # Update name, i.e., axis title
  breaks = breaks)         # Use our chosen breaks
p

#Example - Create a density plot
p <- qplot(sale_price, data = df, geom = "density")
p
#Update plot features
p <- p + scale_x_log10(      # Switch x axis to log (base 10) scale
  name = "Sale Price",     # Set axis title
  labels = dollar,         # Label axis as dollors
  limits = c(1, 10000000)) # Set limits of axis
p


#For a factor column of data, the analagous commands are scale_x_discrete 
#and scale_y_discrete with the same options:
  #name = name of axis
  #breaks = locations of tick marks along axis
  #labels = text labels at tick marks
  #limits = lower and upper limits of axis
#However, the breaks and limits options only make sense if the data can 
#be successfully coerced to numerical data

#Example - Recreate bar plot, add plot name
p <- qplot(condition, data = df, geom = "bar") +
  scale_x_discrete(name = "Condition of House")
p

#break error
p <- qplot(condition, data = df, geom = "bar") +
  scale_x_discrete(name = "Condition of House", breaks = c(0, 2, 4))
p # Results in blank x axis because breaks do not make sense


#Scales for color
#When altering color, you must know whether your color corresponds to a
#discrete or continuous scale
#Discrete (when the associated data is discrete)
  #scale_color_hue (idea: “evenly spaced” colors)
  #scale_color_brewer (idea: palettes up to 9 colors)
#Continuous (when the associated data is continuous)
  #scale_color_gradient
  #scale_color_gradientn
#The command scale_color_hue is the default discrete scale for (even if 
#you don’t specify a scale)
#But you can use it to change scale name, for example
#The command scale_color_discrete is a synonym

#Example - Create a scatter plot
p <- qplot(age, sale_price, data = df, geom = "point", color = city)
p
#Update the plot, note we are building on p created above
p <- p + scale_color_hue(  # Update the discrete color
  name = "City in Iowa") # Set the color scale name
p

#Example - Recreate bar plot with fill based on condition
p <- qplot(condition, data = df, geom = "bar", fill = condition)
p

#The command scale_color_brewer is based on the idea of palettes:
#Display all color options
RColorBrewer::display.brewer.all()


#Example - Recreate scatter plot
p <- qplot(age, sale_price, data = df, geom = "point", color = city)
p
#Add labels and colors
p <- p + scale_color_brewer( # Update the discrete color palette
  name = "City in Iowa",   # Set the color scale name
  palette = "Set1")        # Choose the palette
p

#Example - Introducing facets this creats two plots one for each facet
#Four columns are involed
p <- qplot(age, sale_price, data = df, geom = "point",
           facets = . ~ city, color = lat)
p
#Update plot details
p <- p + scale_color_gradient( # Update the continuous color gradient
  name = "Latitude",         # Set the scale name
  low = "red",               # Set color for low data values
  high = "blue")             # Set color for high data values
p

#The command scale_color_gradientn is like scale_color_gradient except 
#that the gradient can be between more than two colors
#But the syntax is more difficult

#Example - Recreate scatter plot, different fill
p <- qplot(age, sale_price, data = df, geom = "point", color = above)
p
colors <- c("blue", "white", "red") # Choose our colors

#Update plot details, not we are adding to p plot from above
p <- p + scale_color_gradientn( # Update the continuous color gradient
  name = "Above Ground SqFt", # Set the scale name
  labels = comma,             # Add commas in scale labels
  colours = colors)           # Set the colors. Note "colours" spelling!!
p

#Scales for fill
#Very similar to color except
#scale_fill_brewer
#scale_fill_gradient
#scale_fill_gradientn

#Example - Create density plot with facets
p <- qplot(sale_price, data = df, geom = "density", facets = . ~ city,
           fill = city)

#Update plot details, not we are adding to p plot from above
p <- p + scale_fill_brewer( # Update discrete fill
  name = "City in Iowa",  # Set name of scale
  palette = "Set1")       # Set palette
p

#Example - Recreate bar plot with facets
p <- qplot(condition, data = df, geom = "bar", facets = . ~ city, fill =
             condition)

#Update plot details, not we are adding to p plot from above
p <- p + scale_fill_brewer(     # Update discrete fill
  name = "Condition of Home", # Set name of scale
  palette = "Set1")           # Set palette
p

#Example - Create a heatmap
p <- qplot(age, sale_price, data = df, geom = "bin2d", log = "y")

#Update plot details, not we are adding to p plot from above
p <- p + scale_fill_gradient( # Update continuous fill
  name = "Frequency",       # Set scale name
  low = "blue",             # Set color for low values
  high = "red")             # Set color for high values
p

#Scales for alpha, shape, and size

#Example - Create scatter plot based on lon/lat (default when using x and y is scatter)
p <- qplot(lon, lat, data = df, na.rm = TRUE,
           alpha = above, shape = city, size = sale_price, color = zip)
#Update name and set limits to lat lon scale
p <- p + scale_x_continuous(name = "Longitude", limits = c(-93.8,-93.5))
p <- p + scale_y_continuous(name = "Latitude", limits = c(41.51, 41.66))

# Mostly just setting names of the various scales
p <- p + scale_alpha(name = "Above Ground SqFt", labels = comma)
p <- p + scale_shape(name = "City")
p <- p + scale_size(name = "Sale Price", labels = dollar)
p <- p + scale_color_hue(name = "Zip Code")
p


####3. Themes####
#What is a theme?
#The theme of a graphic controls all non-data elements
#Plot and axis titles
#Font sizes and faces
#Legend and guides
#Panel and grid
#Generic command is theme(<element> = <property>)
#We will learn by example
#This webpage contains a list of all elements that can be altered
#http://docs.ggplot2.org/current/theme.html

#Example - Recreate density plot
p <- qplot(sale_price, data = df, geom = "density", log = "x") 
p
#Update plot title
p <- p + ggtitle("Density of Sale Price") # Set overall plot title
#Update plot set size on and font on title
p <- p + theme(                 # Change the theme
  plot.title = element_text(  # Alter the text of the plot title
    size = 32,              # Set the font size to 32
    face = "bold"))         # Make the text bold
p
#Update x axis lable
p <- p + xlab("Sale Price") # Set x axis title
#Update plot set size on and font on title
p <- p + theme(                # Change the theme
  axis.title = element_text( # Alter the text of both (!) axis titles
    size = 24,             # Set the font size to 24
    face = "italic"))      # Make the text italics
p
#Update size of numbers
p <- p + theme(               # Change the theme
  axis.text = element_text( # Alter the numbers of both (!) axes
    size = 16))           # Set the font size to 16
#Angel numbers
p <- p + theme(                 # Change the theme
  axis.text.x = element_text( # Alter the numbers on the x axis
    angle = 45))            # Rotate the numbers 45 degrees
p

#Remove y axis details
p <- p + theme(axis.title.y = element_blank()) # Remove y axis title
p <- p + theme(axis.text.y = element_blank())  # Remove numbers on y axis
p <- p + theme(axis.ticks.y = element_blank()) # Remove tick marks on y axis
p

# Example - Recreat scatter plot
p <- qplot(age, sale_price, data = df, geom = "point", color = city)
p
#Move legend position
p <- p + theme(legend.position = "bottom") # Set legend position
# Valid choices are "top", "bottom", "left", "right", or "none"
p
#Remove items from plot
p <- p + theme(legend.position = "none")             # Remove legend
p <- p + theme(panel.grid.minor.y = element_blank()) # Remove y minor gridlines
p
p <- p + theme(panel.grid.minor.x = element_blank()) # Remove x minor gridlines
p <- p + theme(panel.grid.major = element_blank())   # Remove all major gridlines
p

#Add black outline to the plot
p <- p + theme(                  # Update theme
  panel.border = element_rect( # Change the rectangle defining the panel
    linetype = "solid",      # Choose a solid border
    color = "black",         # With color black
    fill = NA))              # No fill or else will lose plot
p
#Remove background color
p <- p + theme(panel.background = element_blank()) # Remove background
p


####4. Exporting#### 
#A Full Example
require(grid) # Install package 'grid' if necessary
p <- qplot(lon, lat, data = df, color = zip, size = sale_price, shape =
             city, xlim = c(-93.8,-93.5), ylim = c(41.51, 41.66), na.rm = TRUE)
p <- p + scale_shape("City")
p <- p + scale_color_hue(name = "Zip Code")
p <- p + scale_size("Sale Price", labels = dollar, range = c(0.5,4))
p <- p + ggtitle("The Real Estate Geography of (West) Des Moines")
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.grid = element_blank())
p <- p + theme(panel.border = element_rect(linetype = "solid", color =
                                             "black", fill = NA))
p <- p + theme(legend.text = element_text(size = 8))
p <- p + theme(legend.key.size = unit(0.12, "in"))
p <- p + theme(title = element_text(size = 12))
p <- p + theme(legend.spacing = unit(0, "in"))
p <- p + theme(legend.key = element_rect(fill = NA))
p <- p + theme(plot.title = element_text(face = "bold"))
p

#Exporting the Full Example
#The command ggsave can be used to export an R plot
#Options width and height indicate the aspect ratio
#Example of exporting to PDF (specify the units!)

ggsave(filename = "full_example.pdf", plot = p, width = 6, height = 4,
       units = "in")
ggsave(filename = "full_example.png", plot = p, width = 6, height = 4,
       dpi = 600)
