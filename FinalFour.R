library(tidyverse)
library(ggplot2)
library(ggimage)
library(gsheet)
library(gt)
rm(list = ls())

setwd("/Users/jeremydumalig/Downloads/March\ Madness")

mbb <- read_csv("ncaa_sweet16_mbb_logs.csv")
wbb <- read_csv("ncaa_sweet16_wbb_logs.csv")

current <- 
  mbb %>%
  mutate(`3PA%` = 100 * `3PA` / FGA,
         `OPP 3PA%` = 100 * `OPP 3PA` / `OPP FGA`)
subtitle <- "NCAA Men's Basketball Tournament 2023 | Final Four"

my_theme <- 
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    plot.title = element_text(size=18,
                              face="bold"),
    plot.subtitle = element_text(size=14),
    plot.caption = element_text(size=9))

transparent <- function(img) {
  magick::image_fx(img, expression = "0.25*a", channel = "alpha")
}

ppp <-
  current %>%
  filter(Opponent == "Total") %>%
  ggplot(aes(x=`PPP`,
             y=`OPP PPP`)) +
  geom_hline(yintercept=mean(filter(current, Opponent == "Total")$`OPP PPP`), linetype="dashed") +
  geom_vline(xintercept=mean(filter(current, Opponent == "Total")$`PPP`), linetype="dashed") +
  geom_image(aes(image=Logo),
             image_fun=transparent,
             size=0.1,
             stat='identity') +
  geom_image(data=filter(current, FinalFour, Opponent == "Total"),
             aes(x=`PPP`,
                 y=`OPP PPP`,
                 image=Logo),
             size=0.1,
             stat='identity') +
  scale_y_reverse() +
  labs(title="Who runs the most efficient offense/defense?",
       subtitle=subtitle,
       x="Points Per Possession (PPP)",
       y="Opponent Points Per Possession (OPP PPP)",
       caption="Note: the y-axis is reversed to place teams who\nboth score and defend well in the top-right region") +
  my_theme

threes <-
  current %>%
  filter(Opponent == "Total") %>%
  ggplot(aes(x=`3PA%`,
             y=`OPP 3PA%`)) +
  geom_hline(yintercept=mean(filter(current, Opponent == "Total")$`OPP 3PA%`), linetype="dashed") +
  geom_vline(xintercept=mean(filter(current, Opponent == "Total")$`3PA%`), linetype="dashed") +
  geom_image(aes(image=Logo),
             image_fun=transparent,
             size=0.1,
             stat='identity') +
  geom_image(data=filter(current, FinalFour, Opponent == "Total"),
             aes(x=`3PA%`,
                 y=`OPP 3PA%`,
                 image=Logo),
             size=0.1,
             stat='identity') +
  scale_y_reverse() +
  labs(title="Who is shooting/allowing the most three-pointers?",
       subtitle=subtitle,
       x="3-Point Attempt Rate (3PA%)",
       y="Opponent 3-Point Attempt Rate (OPP 3PA%)",
       caption="Note: the y-axis is reversed to place teams who both shoot\nmany and prevent many three-pointers in the top-right region") +
  my_theme

freeThrows <-
  current %>%
  filter(Opponent == "Total") %>%
  ggplot(aes(x=`FTA%`,
             y=`OPP FTA%`)) +
  geom_hline(yintercept=mean(filter(current, Opponent == "Total")$`OPP FTA%`), linetype="dashed") +
  geom_vline(xintercept=mean(filter(current, Opponent == "Total")$`FTA%`), linetype="dashed") +
  geom_image(aes(image=Logo),
             image_fun=transparent,
             size=0.1,
             stat='identity') +
  geom_image(data=filter(current, FinalFour, Opponent == "Total"),
             aes(x=`FTA%`,
                 y=`OPP FTA%`,
                 image=Logo),
             size=0.1,
             stat='identity') +
  scale_y_reverse() +
  labs(title="Who is shooting/allowing the most free throws?",
       subtitle=subtitle,
       x="Free Throw Attempt Rate (FTA%)",
       y="Opponent Free Throw Attempt Rate (OPP FTA%)",
       caption="Note: the y-axis is reversed to place teams who both shoot\nmany and prevent many free throws in the top-right region") +
  my_theme

rebounding <-
  current %>%
  filter(Opponent == "Total") %>%
  ggplot(aes(x=`ORB%`,
             y=`DRB%`)) +
  geom_hline(yintercept=mean(filter(current, Opponent == "Total")$`DRB%`), linetype="dashed") +
  geom_vline(xintercept=mean(filter(current, Opponent == "Total")$`ORB%`), linetype="dashed") +
  geom_image(aes(image=Logo),
             image_fun=transparent,
             size=0.1,
             stat='identity') +
  geom_image(data=filter(current, FinalFour, Opponent == "Total"),
             aes(x=`ORB%`,
                 y=`DRB%`,
                 image=Logo),
             size=0.1,
             stat='identity') +
  labs(title="Who are the best/worst rebounding teams?",
       subtitle=subtitle,
       x="Offensive Rebound Rate (ORB%)",
       y="Defensive Rebound Rate (DRB%") +
  my_theme

turnovers <-
  current %>%
  filter(Opponent == "Total") %>%
  ggplot(aes(x=`TO%`,
             y=`OPP TO%`)) +
  geom_hline(yintercept=mean(filter(current, Opponent == "Total")$`OPP TO%`), linetype="dashed") +
  geom_vline(xintercept=mean(filter(current, Opponent == "Total")$`TO%`), linetype="dashed") +
  geom_image(aes(image=Logo),
             image_fun=transparent,
             size=0.1,
             stat='identity') +
  geom_image(data=filter(current, FinalFour, Opponent == "Total"),
             aes(x=`TO%`,
                 y=`OPP TO%`,
                 image=Logo),
             size=0.1,
             stat='identity') +
  scale_x_reverse() +
  labs(title="Who commits/forces the most turnovers?",
       subtitle=subtitle,
       x="Team Turnover Rate (TO%)",
       y="Opponent Turnover Rate (OPP TO%)",
       caption="Note: the x-axis is reversed to place teams who both\ncommit few and force many turnovers in the top-right region") +
  my_theme

table <- 
  current %>%
  filter(FinalFour, Opponent == "Total") %>%
  mutate(POSS = POSS / GP,
         `3PA%` = 100 * `3PA` / FGA,
         `OPP 3PA%` = 100 * `OPP 3PA` / `OPP FGA`,
         across(c(POSS, `3PA%`, `OPP 3PA%`, `FTA%`, `OPP FTA%`, `ORB%`, `DRB%`, `REB%`, `TO%`, `OPP TO%`), round, 1),
         across(c(PPP, `OPP PPP`), round, 2)) %>%
  select(TEAM, POSS, PPP, `OPP PPP`, `3PA%`, `OPP 3PA%`, `FTA%`, `OPP FTA%`, `ORB%`, `DRB%`, `REB%`, `TO%`, `OPP TO%`) %>%
  gt() %>%
  tab_spanner(label = "", columns = c(TEAM)) %>%
  tab_spanner(label = " ", columns = c(POSS, PPP, `OPP PPP`, `3PA%`, `OPP 3PA%`)) %>%
  tab_spanner(label = "  ", columns = c(`FTA%`, `OPP FTA%`)) %>%
  tab_spanner(label = "   ", columns = c(`DRB%`, `ORB%`, `REB%`)) %>%
  tab_spanner(label = "    ", columns = c(`TO%`, `OPP TO%`)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(columns = everything())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = TEAM)) %>%
  tab_header(title = md("Advanced Statistics Dashboard"),
             subtitle = subtitle)

ppp
threes
freeThrows
rebounding
turnovers
table
