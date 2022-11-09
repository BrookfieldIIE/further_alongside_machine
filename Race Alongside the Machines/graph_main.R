library(BFTheme)
library(data.table)
library(stringr)
library(ggbump)
library(ggplot2)

#########################
figure.1.data <- fread("Graph_data/Figure_1.csv")

figure.1 <- plot.line.bf(figure.1.data[element_name=="Structured versus Unstructured Work"],
                                        "year","value_scale",group.by="noc_06_21",
                                        plot.title = "Work Context: Structured vs Unstructured - Canadian Occupations 2006-2021",
                                        plot.fig.num = "Figure 1",
                                        x.axis = "Year",
                                        y.axis = "Normalized Score",
                                        caption="Source: O*Net, Author Calculations")

figure.2 <- plot.line.bf(figure.1.data[element_name=="Importance of Repeating Same Tasks"],
                                          "year","value_scale",group.by="noc_06_21",
                                          plot.title = "Work Context: Importance of Repeating Same Tasks - Canadian Occupations 2006-2021",
                                          plot.fig.num = "Figure 2",
                                          x.axis = "Year",
                                          y.axis = "Normalized Score",
                                          caption="Source: O*Net, Author Calculations")

figure.3 <- plot.line.bf(figure.1.data[element_name=="Pace Determined by Speed of Equipment"],
                                         "year","value_scale",group.by="noc_06_21",
                                         plot.title = "Work Context: Pace Determined by Speed of Equipment - Canadian Occupations 2006-2021",
                                         plot.fig.num = "Figure 3",
                                         x.axis = "Year",
                                         y.axis = "Normalized Score",
                                         caption="Source: O*Net, Author Calculations")

figure.4 <- plot.line.bf(figure.1.data[element_name=="Spend Time Making Repetitive Motions"],
                                       "year","value_scale",group.by="noc_06_21",
                                       plot.title = "Work Context: Spend Time Making Repetitive Motions - Canadian Occupations 2006-2021",
                                       plot.fig.num = "Figure 4",
                                       x.axis = "Year",
                                       y.axis = "Normalized Score",
                                       caption="Source: O*Net, Author Calculations")

figure.5 <- plot.line.bf(figure.1.data[element_name=="Importance of Being Exact or Accurate"],
                                        "year","value_scale",group.by="noc_06_21",
                                        plot.title = "Work Context: Importance of Being Exact or Accurate - Canadian Occupations 2006-2021",
                                        plot.fig.num = "Figure 5",
                                        x.axis = "Year",
                                        y.axis = "Normalized Score",
                                        caption="Source: O*Net, Author Calculations")

#####################
figure.6.data <- fread("Graph_data/Figure_6.csv")


figure.6 <- plot.line.bf(figure.6.data[element_name=="Structured versus Unstructured Work"],
                                        "year","value_scale",group.by="noc_16_21",
                                        plot.title = "Work Context: Structured vs Unstructured - Canadian Occupations 2016-2021",
                                        plot.fig.num = "Figure 6",
                                        x.axis = "Year",
                                        y.axis = "Normalized Score",
                                        caption="Source: O*Net, Author Calculations")

figure.7 <- plot.line.bf(figure.6.data[element_name=="Importance of Repeating Same Tasks"],
                                          "year","value_scale",group.by="noc_16_21",
                                          plot.title = "Work Context: Importance of Repeating Same Tasks - Canadian Occupations 2016-2021",
                                          plot.fig.num = "Figure 7",
                                          x.axis = "Year",
                                          y.axis = "Normalized Score",
                                          caption="Source: O*Net, Author Calculations")

figure.8 <- plot.line.bf(figure.6.data[element_name=="Pace Determined by Speed of Equipment"],
                                         "year","value_scale",group.by="noc_16_21",
                                         plot.title = "Work Context: Pace Determined by Speed of Equipment - Canadian Occupations 2016-2021",
                                         plot.fig.num = "Figure 8",
                                         x.axis = "Year",
                                         y.axis = "Normalized Score",
                                         caption="Source: O*Net, Author Calculations")

figure.9 <- plot.line.bf(figure.6.data[element_name=="Spend Time Making Repetitive Motions"],
                                       "year","value_scale",group.by="noc_16_21",
                                       plot.title = "Work Context: Spend Time Making Repetitive Motions - Canadian Occupations 2016-2021",
                                       plot.fig.num = "Figure 9",
                                       x.axis = "Year",
                                       y.axis = "Normalized Score",
                                       caption="Source: O*Net, Author Calculations")

figure.10 <- plot.line.bf(figure.6.data[element_name=="Importance of Being Exact or Accurate"],
                                        "year","value_scale",group.by="noc_16_21",
                                        plot.title = "Work Context: Importance of Being Exact or Accurate - Canadian Occupations 2016-2021",
                                        plot.fig.num = "Figure 10",
                                        x.axis = "Year",
                                        y.axis = "Normalized Score",
                                        caption="Source: O*Net, Author Calculations")

############################
figure.11.data <- fread("Graph_data/Figure_11.csv")

figure.11 <- ggplot(figure.11.data,aes(x=year,y=-I,colour=noc_title_2016)) +
  geom_bump() +
  scale_color_manual(values = c(set.colours(6,categorical.choice = c("dark.blue","light.blue","magenta",
                                                                     "green","pink","brown")),
                                set.colours(6,categorical.choice = c("dark.blue","light.blue","magenta",
                                                                     "green","pink","brown")),
                                set.colours(4,categorical.choice = c("dark.blue","light.blue","magenta",
                                                                     "green")))) +
  geom_point(size=1.5) +
  brookfield.base.theme() +
  labs(title = "Figure 11",
       subtitle = "Top 10 Most Digitally Intensive Jobs",
       caption = "Source: O*Net, Author Calculations") +
  xlab("Year") +
  ylab("Ranking") +
  scale_y_continuous(breaks = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1),labels=c("10th","9th","8th","7th","6th","5th","4th","3rd","2nd","1st"))

##########################
figure.12.data <- fread("Graph_data/Figure_12.csv")

figure.12 <- plot.scatter.bf(ranking.comb[!is.na(skill_level)],"perc.rank2006","perc.rank2021",deg.45=TRUE,
                             group.by="skill_level",
                             x.axis = "Digital Percentile in 2006",
                             y.axis = "Digital Percentile in 2021",
                             plot.fig.num = "Figure 12",
                             legend.title = "Skill levels",
                             caption = "Source: O*Net, Author Calculations",
                             plot.title = "Changes in relative digital intensity in Canada occupations 2006-2021")


##########################
figure.13.data <- fread("Graph_data/Figure_13.csv")

figure.13 <- plot.scatter.bf(figure.13.data,
                             "perc.change.21.06","var.tech_design",
                             plot.fig.num = "Figure 13",
                             caption = "Source: Author Calculations",
                             plot.title = "Variance in Technology Design score for Canadian Occupations - 2006-2021",
                             x.axis = "Changes in Digital Intensity Percentile",
                             y.axis = "Standard Deviation - Technology Design")


figure.14 <- plot.scatter.bf(figure.13.data,
                                    "perc.change.21.06","var.program",
                             plot.fig.num = "Figure 14",
                             caption = "Source: Author Calculations",
                                    plot.title = "Variance in Programming score for Canadian Occupations - 2006-2021",
                                    x.axis = "Changes in Digital Intensity Percentile",
                                    y.axis = "Standard Deviation in the Programming Skill")

figure.15 <- plot.scatter.bf(figure.13.data,
                                      "perc.change.21.06","var.comp_elec",
                             plot.fig.num = "Figure 15",
                             caption = "Source: Author Calculations",
                                      plot.title = "Variance in Computer and Electronics score for Canadian Occupations - 2006-2021",
                                      x.axis = "Changes in Digital Intensity Percentile",
                                      y.axis = "Standard Deviation - Computer and Electronics")

figure.16 <- plot.scatter.bf(figure.13.data,
                             "perc.change.21.06","var.interact_comp",
                             plot.fig.num = "Figure 16",
                             caption = "Source: Author Calculations",
                             plot.title = "Variance in Interacting with Computers score for Canadian Occupations - 2006-2021",
                             x.axis = "Changes in Digital Intensity Percentile",
                             y.axis = "Standard Deviation - Interacting with Computers")


figure.17 <- plot.scatter.bf(figure.13.data,
                                     "perc.change.21.06","var.eng_tech",
                             plot.fig.num = "Figure 17",
                             caption = "Source: Author Calculations",
                                     plot.title = "Variance in Engineering and Technology score for Canadian Occupations - 2006-2021",
                                     x.axis = "Changes in Digital Intensity Percentile",
                                     y.axis = "Standard Deviation - Engineering and Technology")

figure.18 <- plot.scatter.bf(figure.13.data,
                                  "perc.change.21.06","var.telco",
                             plot.fig.num = "Figure 18",
                             caption = "Source: Author Calculations",
                                  plot.title = "Variance in Telecommunications score for Canadian Occupations - 2006-2021",
                                  x.axis = "Changes in Digital Intensity Percentile",
                                  y.axis = "Standard Deviation - Telecommunications")


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
export.bf.plot("Figure_18.pdf",figure.18)
