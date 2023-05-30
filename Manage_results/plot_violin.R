plot_violin <- function(value, measure, graph_type, no_samples, no_sim = 100){
  
  library(ggplot2)
  if (no_samples == 250){
    path = "/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Plots/Zinegbin_sim100_samples250"
  }else{
    path = "/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Plots/Zinegbin_sim100_samples500"
  }
  
  orange <- "#e33112"
  green_olive <- "#808000"
  green <- "#04b83d"
  blue <- "#09b0de"
  violet <- "#6509de"
  pink <- "#de09b0"
  
  
  if(measure == 'AUPR'){
    
    Methods = c(rep('SE_gl', no_sim), rep('SE_mb', no_sim),
                rep('SPRING', no_sim), rep('HARMONIES', no_sim),
                rep('gCoda', no_sim), rep('COZINE', no_sim))
    value <- c(value[[1]], value[[2]], value[[3]], value[[4]], value[[5]], rep(-1,no_sim))
    data <- data.frame(value)
    
    ggplot(data = data, aes(x=Methods, y=value, fill = Methods)) + # fill=Methods allow to automatically dedicate a color for each group
      ylim(0,1)+
      xlab("Name of method") +
      ylab(measure) +
      geom_violin() +
      stat_summary(fun=median, geom="point", size=2)+
      scale_fill_manual(values=c(green_olive, green, blue, violet, pink)) +
      ggtitle(paste(graph_type, " no_samples=", no_samples))
    ggsave(path = path, filename = paste(graph_type,measure,no_samples,"cn_100",".tiff",sep='_'), height = 15, width = 20, units='cm', compression = "lzw", dpi = "print")
    
  }else if(no_samples == 500 && graph_type == 'scale_free' && (measure == 'Precision' || measure == 'F_score')){
    
    Methods = c(rep('SE_gl', no_sim), rep('SE_mb', no_sim),
                rep('SPRING', no_sim), rep('HARMONIES', no_sim),
                rep('gCoda', no_sim), rep('COZINE', no_sim))
    value <- c(value[[1]], value[[2]], value[[3]], value[[4]], value[[5]], value[[6]])
    data <- data.frame(value)
    ggplot(data = data, aes(x=Methods, y=value, fill = Methods)) + # fill=Methods allow to automatically dedicate a color for each group
      xlab("Name of method") +
      ylab(measure) +
      geom_violin() +
      stat_summary(fun=median, geom="point", size=2)+
      scale_fill_manual(values=c(orange, green_olive, green, blue, violet, pink)) +
      ggtitle(paste(graph_type, " no_samples=", no_samples))
    ggsave(path = path, filename = paste(graph_type,measure,no_samples,"cn_100",".tiff",sep='_'), height = 15, width = 20, units='cm', compression = "lzw", dpi = "print")
    
    
  }else if(no_samples == 250 && graph_type == 'scale_free' && (measure == 'Precision' || measure == 'F_score')){
    
    Methods = c(rep('SE_gl', no_sim), rep('SE_mb', no_sim),
                rep('SPRING', no_sim), rep('HARMONIES', no_sim),
                rep('gCoda', no_sim), rep('COZINE', no_sim))
    value <- c(value[[1]], value[[2]], value[[3]], value[[4]], value[[5]], value[[6]])
    data <- data.frame(value)
    ggplot(data = data, aes(x=Methods, y=value, fill = Methods)) + # fill=Methods allow to automatically dedicate a color for each group
      xlab("Name of method") +
      ylab(measure) +
      geom_violin() +
      stat_summary(fun=median, geom="point", size=2)+
      scale_fill_manual(values=c(orange, green_olive, green, blue, violet, pink)) +
      ggtitle(paste(graph_type, " no_samples=", no_samples))
    ggsave(path = path, filename = paste(graph_type,measure,no_samples,"cn_100",".tiff",sep='_'), height = 15, width = 20, units='cm', compression = "lzw", dpi = "print")
    
  }else{
    
    Methods = c(rep('SE_gl', no_sim), rep('SE_mb', no_sim),
                rep('SPRING', no_sim), rep('HARMONIES', no_sim),
                rep('gCoda', no_sim), rep('COZINE', no_sim))
    value <- c(value[[1]], value[[2]], value[[3]], value[[4]], value[[5]], value[[6]])
    data <- data.frame(value)
    ggplot(data = data, aes(x=Methods, y=value, fill = Methods)) + # fill=Methods allow to automatically dedicate a color for each group
      # ylim(0,1)+
      xlab("Name of method") +
      ylab(measure) +
      geom_violin() +
      stat_summary(fun=median, geom="point", size=2)+
      scale_fill_manual(values=c(orange, green_olive, green, blue, violet, pink)) +#Alphabetical order of the methods names
      ggtitle(paste(graph_type, " no_samples=", no_samples))
    ggsave(path = path, filename = paste(graph_type,measure,no_samples,"cn_100",".tiff",sep='_'), height = 15, width = 20, units='cm', compression = "lzw", dpi = "print")
  }
  
}
