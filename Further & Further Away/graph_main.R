library(BFTheme)
library(data.table)
library(ggplot2)
library(stringr)

###########################

plot.line.bf.new <- function(data,x,y,
                             group.by=NULL,
                             show.points = FALSE,
                             cat.order = NULL,
                             ingraph.labels = FALSE,
                             colours = NULL,
                             plot.title="",
                             plot.fig.num="",
                             plot.limit = NULL,
                             unit.x = "",
                             unit.y = "",
                             x.axis = "",
                             y.axis = "",
                             legend.title = "",
                             caption = "",
                             export = FALSE,
                             export.name = ""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  #This bit sets up the base Brookfield theme elements
  line.theme <- brookfield.base.theme()
  if(is.null(plot.limit)){
    plot.limit["min.y"] <- min(clone[,get(y)]) #Set the plot limit for minimum
    plot.limit["max.y"] <- max(clone[,get(y)])*1.05 #Set the max plot limit - add in a bit of buffer
  }
  names(plot.limit) <- c("min.y","max.y")
  ticks.seq.y <- set.ticks.seq(plot.limit["max.y"],plot.limit["min.y"],unit.y) #Set tick sequence based on the plot limits
  if(is.numeric(clone[,get(x)])){
    dum <- FALSE #Scale x continuous as opposed to discrete later on.
    ticks.seq.x <- unique(clone[,get(x)]) #Set the x axis ticks, if it's numeric then extract ordered set directly
  }
  else{ #But if it's not
    dum <- TRUE #Scale x discrete as opposed to continuous later on.
    if(!is.null(cat.order)){ #If a categorical order is not provided - alphabetica order will be used
      dum.vec <- clone[,get(x)]
      cat.ord.num <- seq(1,length(cat.order))
      names(cat.ord.num) <- cat.order
      dum.vec <- unname(cat.ord.num[dum.vec])
      clone[,x] <- factor(clone[,get(x)],levels = cat.order) #This makes sure every data has an ordered category
    }
  }
  if(is.null(group.by)){ #If there is only one group
    if(is.null(colours)){
      colours <- set.colours(1) #Only use one colour
    }
    p <- ggplot2::ggplot(clone,aes_string(x,y),colour=colours) + #Main plot object
      line.theme +
      ggplot2::geom_line(colour=colours,size=1.2)
  }
  else{
    if(is.null(colours)){
      colours <- set.colours(length(unique(clone[,get(group.by)]))) #Set colours according to the group
    }
    p <- ggplot2::ggplot(data,aes_string(x,y,colour=group.by,group=group.by,linetype="Scaleup")) + #Main plot object
      line.theme +
      ggplot2::geom_line(aes_string(colour=group.by),size=1.2) +
      ggplot2::scale_colour_manual(values=colours)
  }
  if(show.points){
    p <- p + geom_point(size=2.3) #Add in points
  }
  if(!dum){ #Scale continuous because underlying categories were numeric
    p <- p + scale_x_continuous(breaks = ticks.seq.x, labels = paste0(ticks.seq.x,unit.x))
    
  }
  if(length(unique(clone[,get(x)]))>= 10){ #If there are more than 10 groups, make the x axis certicle
    p <- p + theme(axis.text.x = ggplot2::element_text(angle=90,size=11, margin=ggplot2::margin(t=0,l=10),hjust=1,vjust=0.5))
  }
  
  num.row <- round(sum(nchar(as.character(unique(clone[,get(group.by)]))))/100)+1 #Set numnber of legend rows
  p <- p + labs(title=plot.fig.num,subtitle=plot.title,x=x.axis,y=y.axis,caption = caption) + #Add in all the captions
    guides(colour=guide_legend(title=legend.title,nrow=num.row,title.position = "top")) +
    scale_y_continuous(breaks = ticks.seq.y$breaks,labels = ticks.seq.y$labels)
  if(ingraph.labels){
    p <- p + directlabels::geom_dl(aes_string(label=group.by),method=list("last.points",
                                                                          fontfamily="RooneySans-Regular",
                                                                          cex = 0.8))
  }
  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p)
  }
  return(p)
}



############################

load("Graph_data/Figure_1.RDA")
load("Graph_data/Figure_1_support.RDA")

hybrid.examination.share.cor <- hybrid.examination.share[,.(prof.share,manu.share,mark.share,
                                                            health.share,sys.share,software.share,
                                                            workforce.share,data.share)]


hybrid.examination.share.cor <- cor(hybrid.examination.share.cor)

hybrid.examination.share[,variance:=-sqrt(var(c(prof.share+manu.share+mark.share+health.share,dig.share))),by=ConsolidatedONET]
onet.rank <- individual.ranking[,.(onet,harm.rank)]
setkey(onet.rank,onet)
setkey(hybrid.examination.share,ConsolidatedONET)
hybrid.examination.share <- hybrid.examination.share[onet.rank,nomatch=0]
summary(lm(variance ~ harm.rank,hybrid.examination.share))

hybrid.examination.share[,core.digital:="Other Occupations"]
hybrid.examination.share[software.share+sys.share>=10,core.digital:="Software and System Digital Occupations"]

figure.1 <- plot.scatter.bf(hybrid.examination.share,"harm.rank","variance",
                                           group.by = "core.digital",
                                           legend.title = "",
                                           p.size = "employment",
                                           colours = set.colours(2,categorical.choice = c("dark.blue","pink")),
                                           plot.title ="Hybridization of Digital Jobs",
                                           plot.fig.num = "Figure 1",
                                           caption = "Source: Author Calculations, from Vu, Lamb, Willoughby (2019)",
                                           x.axis = "Tech Intensity of Occupations",
                                           y.axis = "Hybridization Score of Occupations") +
  scale_x_continuous(breaks = c(0,250,750),labels = c("","More Tech Intensive","Less Tech Intensive")) +
  scale_y_continuous(limits = c(-80,0),breaks = c(-80,-50,-10,0),labels = c("","Less Hybrid","More Hybrid","")) +
  guides(colour = guide_legend(title = ""))





figure.2.data <- fread("Graph_data/Figure_2.csv")
figure.2 <- plot.line.bf(figure.2.data[REF_DATE>=1990],"REF_DATE","real_value",group.by = "country",
                         unit.y="$",
                         plot.fig.num = "Figure 2",
                         plot.title = "Almost Two Decades of Stagnation - Total Expenditure in R&D in Canada",
                         y.axis = "Real Total R&D Expenditure in $ Millions (2002 Constant $)",
                         x.axis = "Year",
                         caption = "Source: Current Dollar data from Statistics Canada Table 27-10-0273-01, deflated using Statistics Canada Table 18-10-0005-01") +
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015,2020)) +
  theme(axis.text.x = element_text(angle=0,hjust=0.5)) +
  guides(colour="none")
#############

fig.3.data <- fread("Graph_data/Figure_3.csv")
fig.3.data <- melt(fig.3.data,id.vars = c("geography","prices","naics","flows_stocks","assets"),variable.name = "Year",value.name="Value")
fig.3.data[,Year:=as.character(Year)]
fig.3.data[,Year:=str_remove(Year,"X")]
fig.3.data[,Year:=as.numeric(Year)]
figure.3 <- plot.line.bf(fig.3.data,"Year","Value",
                         group.by = "assets",
                         plot.title = "Investment in the Manufacturing Sector in Ontario",
                         colours = set.colours(3,categorical.choice = c("dark.blue","light.blue","magenta")),
                         plot.fig.num = "Figure 3",
                         caption = "Source: Statistics Canada CANSIM 031-0005 & 379-0030, reproduced from Lamb, Munro & Vu (2018) \nNote: Manufacturing includes NAICS 31-33.",
                         unit.y = "%",
                         y.axis = "Percent of Manufacturing GDP",
                         x.axis = "Year",
                         legend.title = "Investment Type")
figure.3 <- figure.3 + theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2), family = "GT-Pressura-Light",angle=0,hjust=0.5)) +
  scale_x_continuous(breaks = c(1997,2000,2005,2010,2015,2020),labels = c("1997","2000","2005","2010","2015","2020"))



##########
figure.4.data <- fread("Graph_Data/Figure_4.csv")

override.linetype <- c(1,3,1,3,1,3)
figure.4.data[,Scaleup:=reorder(as.factor(Scaleup),c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2))]
figure.4 <- plot.line.bf.new(figure.4.data,
                              "Year",
                              "Index",
                              group.by = "label",
                              cat.order = c("Kauffman Revenue Scale-up",
                                            "Kauffman Revenue Non-Scale-up",
                                            "OECD Employment Scale-up",
                                            "OECD Non-Employment Scale-up",
                                            "Kauffman Employment Scale-up",
                                            "Kauffman Employment Non-Scale-up"),
                              plot.title = "Trend in Share of R&D Spenders in Canada, Various Definitions",
                              plot.fig.num = "Figure 4",
                              y.axis = "Index (2011=100)",
                             x.axis = "Year",
                              caption = "Source: National Accounts Longitudinal Microfiles, Adapted from Vu, Denney & Kelly (2021)",
                              colours = set.colours(6,categorical.choice = c("dark.blue","dark.blue","pink","pink","light.blue","light.blue"))) +
  guides(colour = guide_legend(title = "",override.aes = list(linetype = override.linetype))) +
  scale_linetype(guide=FALSE)


figure.5.data <- fread("Graph_data/Figure_5.csv")


figure.5 <- plot.scatter.bf(figure.5.data,"harm.rank06","harm.rank16",deg.45=TRUE,
                                       x.axis = "Digital intensity ranking in 2006",
                                       y.axis ="Digital intensity ranking in 2016",
                                       plot.title = "Change in relative digital intensity, Canadian Occupations 2006-2016",
                                       plot.fig.num = "Figure 5",
                                       caption = "Source: Author Calculations")


figure.6<- plot.scatter.bf(figure.5.data,"harm.rank06","harm.rank16",deg.45=TRUE,
                                        group.by = "skill_groups_2",
                                        x.axis = "Digital intensity ranking in 2006",
                                        y.axis ="Digital intensity ranking in 2016",
                                        plot.title = "Change in relative digital intensity, Canadian Occupations 2006-2016, Skill Levels",
                           legend.title = "Skill Levels",
                                        plot.fig.num = "Figure 6",
                                        caption = "Source: Author Calculations")


figure.7.data <- fread("Graph_data/Figure_7.csv")
figure.7 <- plot.area.bf(figure.7.data,"year","probability",group.by="identity",stacked=FALSE,alpha=1,
                                     y.axis = "Probability of being a tech worker",
                                     x.axis = "Census Year",
                                     plot.fig.num = "Figure 7",
                                     logo = FALSE,
                                     plot.title = "Effect of specific identity to probability of being a tech worker in Canada",
                                     caption = "Source: Census microfiles, author calculations ") +
  scale_x_continuous(limits = c(2001,2017),breaks=c(2001,2006,2011,2016),label=c("2001","2006","2011","2016"),expand=c(0,0)) +
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15,0.2),label=c("0%","5%","10%","15%","20%"),expand=c(0,0))


figure.8.data <- fread("Graph_data/Figure_8.csv")
figure.8 <- plot.line.bf(figure.8.data,"year","probability",group.by="identity",
                                y.axis = "Probability of being a tech worker",
                                x.axis = "Census Year",
                                show.points = TRUE,
                                plot.title = "Effect of age on probabiliity of being a tech worker in Canada",
                                plot.fig.num = "Figure 8",
                                caption = "Source: Census microfiles, author calculations") +
  scale_x_continuous(breaks=c(2001,2006,2011,2016),label=c("2001","2006","2011","2016"),expand=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15,0.2),label=c("0%","5%","10%","15%","20%"),expand=c(0,0),limits = c(0,0.25))




figure.9.data <- fread("Graph_data/Figure_9.csv")
figure.9 <- plot.scatter.bf(figure.9.data,"relative.measure","AVG_WAGE",p.size="COUNT_WAGE",unit.y="$",
                            plot.title = "Pay and size for occupations for different digitial intensities, 2016",
                            plot.fig.num = "Figure 9",
                            caption = "Source: Census data combined with author analysis of O*Net data",
                            x.axis = "Digital intensity percentile",
                            y.axis = "Average Employment Income") +
  scale_x_continuous(breaks = c(-400,-300,-200,-100,0),
                     labels = c("0th","25th","50th","75th","100th"))
######################
figure.10.data <- fread("Graph_data/Figure_10.csv")

figure.10.data[,census.year.text:=as.character(census.year)]
figure.10.data[,hourly_wages:=exp(lnwage)]
figure.10.data[,digital.pie:=-pie]
figure.10 <- plot.scatter.bf(figure.10.data[hourly_wages<100],"digital.pie","hourly_wages",group.by="census.year.text",
                             y.axis = "Hourly wages",
                             unit.y = "$",
                             x.axis = "Digital intensity percentile",
                             plot.title = "Wage across digital intensity spectrum across the years, 2001-2016",
                             caption = "Source: Author calculations, Census microfiles 2001-2016",
                             plot.fig.num = "Figure 10",
                             legend.title = "Year") +
  scale_x_continuous(limits = c(-100,0),breaks = c(-100,-75,-50,-25,0),labels = c("0","25","50","75","100"))

######################
figure.11.data <- fread("Graph_data/Figure_11.csv")
figure.11 <- plot.line.bf(figure.11.data[term %in% c("sex","vismin.flag","immder","college")],"year","estimate",group.by="term",show.points = TRUE,
                          plot.title = "Evolution of pay gap in tech over time, selected characteristics",
                          plot.fig.num = "Figure 11",
                          caption = "Source: regression estimates using census microfiles from 2001, 2006, 2011 and 2016",
                          unit.y="$",
                          y.axis = "Hourly pay gap estimates")



#########################
figure.12.data <- fread("Graph_data/Figure_12.csv")
figure.12  <- plot.scatter.bf(figure.12.data[census.year==2016],"pie","dw3",
                              y.axis = "Log change in hourly wages - 2001-2016",
                              x.axis = "Wage percentile",
                              plot.fig.num = "Figure 12",
                              plot.title = "Changes in log hourly wage by wage percentile in Canada, 2001-2016",
                              caption = "Source: Author Calculations, Census Microfiles 2001-2016") +
  scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.1,0.2,0.3,0.4,0.5),expand=c(0,0))


figure.13 <- plot.scatter.bf(figure.12.data[census.year==2016],"pie","dw2",
                             y.axis = "Log change in hourly wages - 2006-2016",
                             x.axis = "Wage percentile",
                             plot.fig.num = "Figure 13",
                             plot.title = "Changes in log hourly wage by wage percentile in Canada, 2006-2016",
                             caption = "Source: Author Calculations, Census Microfiles 2006-2016") +
  scale_y_continuous(limits = c(-0.1,0.4),breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4),expand=c(0,0))


figure.14 <- plot.scatter.bf(figure.12.data[census.year==2006],"pie","dw",
                             y.axis = "Log change in hourly wages - 2001-2006",
                             x.axis = "Wage percentile",
                             plot.fig.num = "Figure 14",
                             plot.title = "Changes in log hourly wage by wage percentile in Canada, 2001-2006",
                             caption = "Source: Author Calculations, Census Microfiles 2001-2006") +
  scale_y_continuous(limits = c(-0.2,0.8),breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),expand=c(0,0))

######################
figure.15.data <- fread("Graph_data/Figure_15.csv")



figure.15 <- plot.scatter.bf(figure.15.data[census.year==2016],"pie","dw3",
                             y.axis = "Log change in hourly wages - 2001-2016",
                             x.axis = "Digital Intensity percentile",
                             plot.fig.num = "Figure 15",
                             caption = "Source: Author Calculations, Census Microfiles 2001-2016",
                             plot.title = "Changes in log hourly wage by digital intensity percentile in Canada, 2001-2016") 



figure.16 <- plot.scatter.bf(figure.15.data[census.year==2006],"pie","dw",
                             y.axis = "Log change in hourly wages - 2001-2006",
                             x.axis = "Digital intensity percentile",
                             plot.title = "Changes in log hourly wage by digital intensity percentile in Canada, 2001-2006",
                             plot.fig.num = "Figure 16",
                             caption = "Source: Author Calculations, Census Microfiles 2001-2006") +
  scale_y_continuous(limits = c(-0.5,0.7),breaks = c(-0.5,-0.25,0,0.25,0.5),expand=c(0,0))

figure.17 <- plot.scatter.bf(figure.15.data[census.year==2016],"pie","dw2",
                             y.axis = "Log change in hourly wages - 2006-2016",
                             x.axis = "Digital intensity percentile",
                             plot.title = "Changes in log hourly wage by digital intensity percentile in Canada, 2006-2016",
                             plot.fig.num = "Figure 17",
                             caption = "Source: Author Calculations, Census Microfiles 2006-2016") 

export.bf.plot("Figure_1.pdf",figure.1)
export.bf.plot("Figure_2.pdf",figure.2)
export.bf.plot("Figure_3.pdf",figure.3)
export.bf.plot("Figure_4.pdf",figure.4)
export.bf.plot("Figure_5.pdf",figure.5)
export.bf.plot("Figure_6.pdf",figure.6)
export.bf.plot("Figure_7.pdf",figure.7)
export.bf.plot("Figure_8.pdf",figure.8)
export.bf.plot("Figure_9.pdf",figure.9)
export.bf.plot("Figure_10.pdf",figure.10)
export.bf.plot("Figure_11.pdf",figure.11)
export.bf.plot("Figure_12.pdf",figure.12)
export.bf.plot("Figure_13.pdf",figure.13)
export.bf.plot("Figure_14.pdf",figure.14)
export.bf.plot("Figure_15.pdf",figure.15)
export.bf.plot("Figure_16.pdf",figure.16)
export.bf.plot("Figure_17.pdf",figure.17)

