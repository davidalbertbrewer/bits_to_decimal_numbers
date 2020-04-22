library(ggplot2)
library(latex2exp)

root_directory <- "C:/Users/dbrewer/PROJECTS/bits_to_decimal_numbers"
data_directory <- paste(root_directory, "/DATA/", sep="")
plot_directory <- paste(root_directory, "/PLOTS/", sep="")



bits <- 0
unique_decimal_numbers <- 1

bits_stop <- 256


#initialize variables
df <- data.frame(bits = bits
                 , unique_decimal_numbers = unique_decimal_numbers)
df_points <- data.frame(bits = numeric(0)
                        , unique_decimal_numbers = numeric(0))
# plot_list = list()

n <- 256
#start loop
for(n in 1:bits_stop){
  bits <- n
  unique_decimal_numbers <- 2^bits
  add_row <- data.frame(bits = bits
                        , unique_decimal_numbers = unique_decimal_numbers)
  df <- rbind(df, add_row)

  if(n <= 16){
    # x_labels <- df$bits
    x_labels <- 0:16
  }
  
  if(n > 16 & n <= 128){
    x_labels <- df$bits[seq(1, length(df$bits), 8)]
  }
  
  if(n > 128){
    x_labels <- df$bits[seq(1, length(df$bits), 32)]
  }
  
  if(unique_decimal_numbers < 10^3){
    value_label = 2^n
  }
  
  if(unique_decimal_numbers > 10^3 & unique_decimal_numbers < 10^5){
    value_label = "Thousands"
  }
  
  if(unique_decimal_numbers > 10^6 & unique_decimal_numbers < 10^9){
    # y_labels <- seq(0, unique_decimal_numbers, 1000000)
    value_label = "Millions"
  }

  if(unique_decimal_numbers >= 10^9 & unique_decimal_numbers < 10^12){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- y_labels/10^9
    # y_labels <- paste(y_labels, "B")
    value_label = "Billions"
  }

  if(unique_decimal_numbers >= 10^12 & unique_decimal_numbers < 10^15){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^12, 0)
    # y_breaks <- y_labels*10^12
    # y_labels <- paste(y_labels, "T")
    value_label = "Trillions"
  }

  if(unique_decimal_numbers >= 10^15 & unique_decimal_numbers < 10^18){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^15, 0)
    # y_breaks <- y_labels*10^15
    # y_labels <- paste(y_labels, "QUAD")
    value_label = "Quadrillions"
  }

  if(unique_decimal_numbers >= 10^18 & unique_decimal_numbers < 10^21){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^18, 0)
    # y_breaks <- y_labels*10^18
    # y_labels <- paste(y_labels, "QUINT")
    value_label = "Quintillions"
  }

  if(unique_decimal_numbers >= 10^21 & unique_decimal_numbers < 10^24){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^21, 0)
    # y_breaks <- y_labels*10^21
    # y_labels <- paste(y_labels, "SEXT")
    value_label = "Sextillions"
  }

  if(unique_decimal_numbers >= 10^24 & unique_decimal_numbers < 10^27){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^24, 0)
    # y_breaks <- y_labels*10^24
    # y_labels <- paste(y_labels, "SEPT")
    value_label = "Septillions"
  }

  if(unique_decimal_numbers >= 10^27 & unique_decimal_numbers < 10^30){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^27, 0)
    # y_breaks <- y_labels*10^27
    # y_labels <- paste(y_labels, "OCT")
    value_label = "Octillions"
  }

  if(unique_decimal_numbers >= 10^30 & unique_decimal_numbers < 10^33){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^30, 0)
    # y_breaks <- y_labels*10^30
    # y_labels <- paste0(y_labels, "NON")
    value_label = "Nonillions"
  }

  if(unique_decimal_numbers >= 10^33 & unique_decimal_numbers < 10^36){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^33, 0)
    # y_breaks <- y_labels*10^33
    # y_labels <- paste0(y_labels, "DEC")
    value_label = "Decillions"
  }

  if(unique_decimal_numbers >= 10^36 & unique_decimal_numbers < 10^39){
    # by <- unique_decimal_numbers/10
    # y_labels <- seq(0, unique_decimal_numbers, by)
    # y_labels <- round(y_labels/10^36, 0)
    # y_breaks <- y_labels*10^36
    # y_labels <- paste0(y_labels, "UND")
    value_label = "Undecillions"
  }

  by <- unique_decimal_numbers/5
  y_labels <- seq(0, unique_decimal_numbers, by)
  y_labels <- round(y_labels, 0)
  
  # add_point <- data.frame(bits = 16
  #                         , unique_decimal_numbers = 16^2)
  # df_points <- rbind(df_points, add_point)
  
  #create plot
  # title <- "Bits to Decimal is Exponential"
  title <- TeX('Bits to Decimal = 2^\\ number of bits$')
  
  plot <- 
    ggplot(data = df, 
           aes(x = df$bits, 
               y = df$unique_decimal_numbers)) +  
    geom_line(colour = "lightseagreen") +
    # geom_point(data = df_points
    #            , aes(x=df_points$bits
    #                  , y=df_points$unique_decimal_numbers)) +
    scale_x_discrete(limits=x_labels) +
    scale_y_continuous(#labels = function(y) format(y, scientific = TRUE)
                       breaks = y_labels
                       # labels = y_labels
                       #breaks = y_breaks
                       ) +
    xlab("Bits") +
    ylab("Unique Decimal Numbers") +
    ggtitle(title) +
    # ggtitle(paste0(title,"\n",subheader)) +
    theme(axis.text.x = element_text(size = 15, color = "#505050", angle = 90, hjust = 0)
          , axis.text.y = element_text(size = 15, color = "#505050")
          , plot.title = element_text(color="#505050",
                                      face="bold", size=15, hjust=.5)
          , axis.title = element_text(color="#505050", face="bold", size=15)
          , axis.line = element_line(color="#505050")
          , panel.background = element_rect(fill="#D3D3D3")
          , plot.background = element_rect(fill="#D3D3D3")
          , legend.title=element_blank()
          , panel.grid = element_blank())
  
  if(n == 16 | n == 24 | n == 32 | n == 40 | n == 64 | n == 128 | n == 256){
    add_point <- data.frame(bits = n
                            , unique_decimal_numbers = 2^n)
    df_points <- rbind(df_points, add_point)
  }
  
  if(n >= 16){
    plot <- plot + annotate("text", x = bits*.3, y = unique_decimal_numbers*.8
                            # , label = paste("Unique Decimal Numbers: ", "\n", value_label)
                            , label = paste(n, "bits = ", value_label, "\n",
                                            "of Unique Numbers")
                            , label = value_label
                            # , color="#505050"
                            , colour = "lightseagreen"
                            , size=5)
  }
  
  if(n < 16 & n >= 10){
    plot <- plot + expand_limits(x = 16)
    plot <- plot + annotate("text", x = 16*.3, y = unique_decimal_numbers*.8
                            # , label = paste("Unique Decimal Numbers: ", "\n", value_label)
                            , label = paste(n, "bits = ", value_label, "\n",
                                            "of Unique Numbers")
                            , label = value_label
                            # , color="#505050"
                            , colour = "lightseagreen"
                            , size=5)
  }
  
  if(n < 10){
    plot <- plot + expand_limits(x = 16)
    plot <- plot + annotate("text", x = 16*.3, y = unique_decimal_numbers*.8
                            # , label = paste("Unique Decimal Numbers: ", "\n", value_label)
                            , label = paste(n, "bits = ", value_label, "\n",
                                            "Unique Numbers")
                            , label = value_label
                            # , color="#505050"
                            , colour = "lightseagreen"
                            , size=5)
  }

  
  
  if(n >= 256){
    plot <- plot + annotate("text", x = 200, y = 2^256
                            , label = "Modern Encryption"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
    plot <- plot + expand_limits(x = 265)
  }
  
  if(n >= 129){
    plot <- plot + annotate("text", x = 120, y = 2^128+(unique_decimal_numbers*.1)
                            , label = "IP's in IPv6"
                            , col="Red")
    plot <- plot + annotate("text", x = 58, y = 2^64+(unique_decimal_numbers*.25)
                            , label = "Modern CPU's"
                            , col="Red")
    plot <- plot + annotate("text", x = 52, y = 2^40+(unique_decimal_numbers*.2)
                            , label = paste("Hackable", sQuote("Encryption"))
                            , col="Red")
    plot <- plot + annotate("text", x = 38, y = 2^32+(unique_decimal_numbers*.15)
                            , label = "IP's in IPv4"
                            , col="Red")
    plot <- plot + annotate("text", x = 28, y = 2^24+(unique_decimal_numbers*.1)
                            , label = "DVD Audio"
                            , col="Red")
    plot <- plot + annotate("text", x = 30, y = 2^16+(unique_decimal_numbers*.05)
                            , label = "SUPER NES"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
  }
  
  if(n >= 65 & n < 129){
    plot <- plot + annotate("text", x = 58, y = 2^64+(unique_decimal_numbers*.25)
                            , label = "Modern CPU's"
                            , col="Red")
    plot <- plot + annotate("text", x = 40, y = 2^40+(unique_decimal_numbers*.2)
                            , label = paste("Hackable", sQuote("Encryption"))
                            , col="Red")
    plot <- plot + annotate("text", x = 30, y = 2^32+(unique_decimal_numbers*.15)
                            , label = "IP's in IPv4"
                            , col="Red")
    plot <- plot + annotate("text", x = 24, y = 2^24+(unique_decimal_numbers*.1)
                            , label = "DVD Audio"
                            , col="Red")
    plot <- plot + annotate("text", x = 16, y = 2^16+(unique_decimal_numbers*.05)
                            , label = "SUPER NES"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
  }
  
  if(n >= 41 & n < 65){
    plot <- plot + annotate("text", x = 34, y = 2^40+(unique_decimal_numbers*.2)
                            , label = paste("Hackable", sQuote("Encryption"))
                            , col="Red")
    plot <- plot + annotate("text", x = 30, y = 2^32+(unique_decimal_numbers*.15)
                            , label = "IP's in IPv4"
                            , col="Red")
    plot <- plot + annotate("text", x = 24, y = 2^24+(unique_decimal_numbers*.1)
                            , label = "DVD Audio"
                            , col="Red")
    plot <- plot + annotate("text", x = 16, y = 2^16+(unique_decimal_numbers*.05)
                            , label = "SUPER NES"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
  }
  
  if(n >= 33 & n < 129){
    plot <- plot + annotate("text", x = 30, y = 2^32+(unique_decimal_numbers*.15)
                            , label = "IP's in IPv4"
                            , col="Red")
    plot <- plot + annotate("text", x = 24, y = 2^24+(unique_decimal_numbers*.1)
                            , label = "DVD Audio"
                            , col="Red")
    plot <- plot + annotate("text", x = 16, y = 2^16+(unique_decimal_numbers*.05)
                            , label = "SUPER NES"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
  }

  if(n >= 25 & n < 33){
    plot <- plot + annotate("text", x = 16, y = 2^16+(unique_decimal_numbers*.05)
                            , label = "SUPER NES"
                            , col="Red")
    plot <- plot + annotate("text", x = 24, y = 2^24+(unique_decimal_numbers*.1)
                            , label = "DVD Audio"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
  }
  
  if(n >= 17 & n < 25){
    
    plot <- plot + annotate("text", x = 16, y = 2^16+(unique_decimal_numbers*.05)
                            , label = "SUPER NES"
                            , col="Red")
    plot <- plot + geom_point(data = df_points
                              , aes(x=df_points$bits
                                    , y=df_points$unique_decimal_numbers)
                              , col="Red")
  }
  # plot
  
  # plot_list[[n]] = plot
  
  file_name = paste("bits_to_decimal_numbers_", n, ".png", sep="")
  file_name = paste(plot_directory, file_name, sep="")
  ggsave(plot
         , file=file_name
         , width = 14, height = 10, units = "cm")
  
}


# # Save plots to tiff. Makes a separate file for each plot.
# n <- 1
# for (n in 1:bits_stop) {
#   file_name = paste("bits_to_decimal_numbers_", n, ".tiff", sep="")
#   file_name = paste(plot_directory, file_name, sep="")
#   tiff(file_name)
#   print(plot_list[[n]])
#   dev.off()
# }
