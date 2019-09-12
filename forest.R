#Setup libraries
library(stringr)      #for string manipulation
library(dplyr)        #for data manipulation
library(grid)         #for margins
library(ggplot2)      #for plotting
library(ggforestplot) #for adding the striped bands
library(patchwork)    #for arranging multiple ggplot objects
library(showtext)     #for nice fonts

#The input data frame assumes:
# Sub-group variable name (e.g. Sex) and it's levels (e.g. Male, Female) are in the same column (level)
# For rows of data where the sub-group variable name appears, set estimates to 999
# For a given sub-group variable (e.g. Sex), set the estimate, low and high values to 1 if it is the Reference level (e.g Female)
# Your data are pre-sorted in the order you wish the plot to appear

df <- data.frame(
  level   =c('Age','0-18','19-64','65+', 'Sex', 'Male','Female', 'Previous Cancer','Yes','No', "Income Quintile", "1st","2nd","3rd","4th","5th"),  estimate=c(999, 0.85, 1.15, 1, 999, 0.50, 1, 999, 1.5, 1, 999, 0.2, 0.4, 0.6, 0.8, 1),
  low     =c(999, 0.73, 1.05, 1, 999, 0.35, 1, 999, 1.25, 1, 999, 0.11, 0.31, 0.51, 0.71, 1),
  high    =c(999, 0.95, 1.25, 1, 999, 0.85, 1, 999, 1.75, 1, 999, 0.31, 0.52,0.74,0.91,1),
  n       =c(999, 21, 29, 10, 999, 44, 16, 999, 12, 48, 999, 20,10,10,10,10),
  stringsAsFactors=FALSE
)

#The above should be clear when you view the dataset.
head(df)

#Data Mgmt
#Add an ordering Variable - this ensures the plot is ordered according to how the data is currently sorted
df$order <- seq(nrow(df), 1, by = -1)


#(a) - Add padding to sub-group levels so they appear indented
#(b) - Add a variable that concatenates the OR + CIs for Display
#(c) - Add a variable that controls bold face for sub-group variables and levels
#(d) - Add a display for Reference levels of a sub-group variable
df = df %>%
  mutate(a = if_else(estimate==999, level, str_pad(level,8,side="left")),
         b = if_else(estimate==999, " ", paste(paste(df$estimate, sep=""), paste("(",df$low,"-",df$high,")",sep=""))),
         c = if_else(estimate==999, "bold","plain"),
         d = if_else(b=="1 (1-1)","Reference",b)
        )

#Clean up data-frame prior to plotting
df2 <- as.data.frame(df)
df2[df2 == 999] <- NA


#Load fonts for plot 
#This step isn't necessary if you're okay with the default ggplot2 font
#An internet connection is needed to load the fonts if you want them.

font_add_google("Raleway", "rale")
showtext_auto()

#Create plots by columns then assemble using patch work.
#You may need to adjust margins in the theme() arguement in each plot depending on your own data

#Column 1 - sub-group column
col1  <-ggplot(df2) +
        theme_linedraw() +
        geom_stripes(even = "#D9D3CE", odd = "#00000000",inherit.aes = FALSE, aes(y=order))  +
        geom_text(aes(label = a, x = 0, y=order, fontface=c), hjust = 0, size=4, family="rale") +
        theme(
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              plot.margin= grid::unit(c(0, 0, 0, 0), "in")
              ) +
      ylim(0, max(df2$order))  +
      xlim(0,0.02) +
      xlab(NULL)  +
      ylab(NULL)  +
      ggtitle(label="\nSub-Group") +
      theme(plot.title = element_text(size =15, face = "bold" ,family="rale"))

#Column 2 - the N column
col2 <- ggplot(df2) +
        theme_linedraw() +
        geom_stripes(even = "#D9D3CE", odd = "#00000000",inherit.aes = FALSE, aes(y=order))  +
        geom_text(aes(label = n, x = 0, y=order, fontface=c), hjust = 0, size=4, family="rale") +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin= grid::unit(c(0, 0, 0, -5), "in")) +
        ylim(0, max(df2$order))  +
        xlim(0,0.02) +
        xlab(NULL)  +
        ylab(NULL)  +
        ggtitle(label="\nNo. of Patients") +
        theme(plot.title = element_text(size =15, face = "bold", hjust = 0.001, family="rale"))

#Column 4 - the OR/CI Summary column
col4  <- ggplot(df2) +
         theme_linedraw() +
         geom_stripes(even = "#D9D3CE", odd = "#00000000",inherit.aes = FALSE, aes(y=order))  +
         geom_text(aes(label = d, x = 0, y=order), hjust = 0, size=4,family="rale") +
         theme(
           axis.text = element_blank(),
           axis.title = element_blank(),
           axis.ticks = element_blank(),
           panel.grid = element_blank(),
           panel.border = element_blank(),
           plot.margin= grid::unit(c(0, 0, 0, 0), "in")) +
         ylim(0, max(df2$order))  +
         xlim(0,0.02) +
         xlab(NULL)  +
         ylab(NULL) +
         ggtitle(label="\nAdjusted Hazard Ratios and 95% CIs") +
         theme(plot.title = element_text(size =15, face = "bold", hjust = 1.5,family="rale"))


#Column 3 - the forest plot column
col3 <-  ggplot(df2, aes(x = estimate, xmin = low, xmax = high, y = order)) +
         theme_bw() +
         geom_stripes(even = "#D9D3CE", odd = "#00000000",inherit.aes = FALSE, aes(y=order))  +
         geom_vline(xintercept = 1, linetype="dashed", color="grey60") +
         geom_errorbarh(height = 0.4, color = 'black') + 
         geom_point(shape=21, fill="red", color="black", size=2.5) +
         theme(
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.text.x=element_text(size=12, colour="black", family="rale"),
            axis.line.x=element_line(color="black"), 
            plot.margin= grid::unit(c(0, 0, 0, -5), "in"))  +
          ylim(0, max(df2$order)) +
          xlab(NULL)  +
          ylab(NULL) +
          scale_x_continuous(limits=c(0, round(max(df2$high, na.rm = TRUE))), breaks=seq(0,round(max(df2$high, na.rm = TRUE)),0.5))  
          
#for log2-scale, replace line 137 with: scale_x_continuous(trans='log2')



#Make the forest plot (requires patchwork)
forest_plot <- col1 + col2 + col3 + col4 + plot_layout(ncol=4, widths=c(1,1,0.5,0.25))

#Output to pdf (with pretty fonts)
pdf( "forest.pdf", width=12, height=6)
forest_plot
dev.off()
