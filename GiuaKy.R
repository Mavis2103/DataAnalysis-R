library(fmsb)
library(scales)
# library(png)
# library(RCurl)
# library(patchwork)

# Constant
wage_condition <- 200000
color_plots <- c("#00AFBB", "#FC4E07")

# Functions Global
beautiful_radarchart <- function(data,
                                 color = "#00AFBB",
                                 vlabels = colnames(data),
                                 vlcex = 1,
                                 title = NULL, ...) {
  radarchart(
    data,
    axistype = 1,
    pcol = color,
    pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "#ffffff00",
    vlcex = vlcex,
    vlabels = vlabels,
    title = title, ...
  )
}
players_selected <- function(list_names) {
  v <- c()
  p <- c()
  for (name in list_names) {
    find_index <- which(players_card$Name == name)
    p <- paste(p, players_card[find_index, "Best.Position"])
  }
  for (name in list_names) {
    find_index <- which(players_card$Name == name)
    # = position <- players_card[find_index, "Position"]
    if (grepl("GK", p)) {
      v <- rbind(v, players_card[find_index, c(
        "Photo", "Name",
        "GKDiving", "GKHandling", "GKKicking",
        "GKPositioning", "GKReflexes", "Reactions"
      )])
    } else {
      v <- rbind(v, players_card[find_index, c(
        "Photo", "Name",
        "Pace", "Shooting", "Passing",
        "Dribbling", "Defense", "Physical"
      )])
    }
    print(v)
  }
  return(v)
}

# Answer
fifa22 <- read.csv("./DataSet/FIFA22.csv")
colnames_fifa22 <- colnames(fifa22)


# Đếm sô lượng cầu thủ của mỗi quốc gia
# = print(table(fifa22$Nationality))

# Format value of col Wage €250K -> 250000
fifa22$Wage[TRUE] <-
  paste(regmatches(fifa22$Wage, regexpr("[0-9]+", fifa22$Wage)),
    ifelse(grepl("K", fifa22$Wage), "000", ""),
    sep = ""
  )
# Những cầu thủ Wage > 200000
rs_wage <- subset(fifa22, as.numeric(fifa22$Wage) > wage_condition)
# = print(rs_wage)
# = print(table(rs_wage$Club))


# index card
fifa22$Pace <-
  round(
    fifa22$Acceleration * 0.45
      + fifa22$SprintSpeed * 0.55
  )

fifa22$Shooting <-
  round(
    fifa22$Finishing * 0.45 + fifa22$LongShots * 0.2 +
      fifa22$Penalties * 0.05 + fifa22$Positioning * 0.05 +
      fifa22$ShotPower * 0.2 + fifa22$Volleys * 0.05
  )

fifa22$Passing <-
  round(fifa22$Crossing * 0.2 + fifa22$Curve * 0.05 + fifa22$FKAccuracy * 0.05 +
    fifa22$LongPassing * 0.15 + fifa22$ShortPassing * 0.35 +
    fifa22$Vision * 0.2)

fifa22$Dribbling <-
  round(fifa22$Agility * 0.1 + fifa22$Balance * 0.05 +
    fifa22$BallControl * 0.35 + fifa22$Composure * 0 +
    fifa22$Dribbling * 0.5 + fifa22$Reactions * 0)

fifa22$Defense <-
  round(fifa22$DefensiveAwareness * 0.3 + fifa22$HeadingAccuracy * 0.1 +
    fifa22$Interceptions * 0.2 + fifa22$SlidingTackle * 0.1 +
    fifa22$StandingTackle * 0.3)

fifa22$Physical <-
  round(fifa22$Aggression * 0.2 + fifa22$Jumping * 0.05 +
    fifa22$Stamina * 0.25 + fifa22$Strength * 0.5)

players_card <- fifa22[
  ,
  c(
    "ID", "Name", "Age", "Photo",
    "Nationality", "Flag", "Club",
    "Club.Logo",
    "Overall",
    "Best.Position",
    "Pace", "Shooting", "Passing",
    "Dribbling", "Defense", "Physical",
    "GKDiving", "GKHandling", "GKKicking",
    "GKPositioning", "GKReflexes", "Reactions"
  )
]

op <- par(mar = c(2, 3, 0, 3))
compare_list <- c("Cristiano Ronaldo", "L. Messi")


player <- players_selected(compare_list)
is_gk <- length(colnames(player)[grepl("GK", colnames(player))]) > 0
if (is_gk) {
  max_min <- data.frame(
    GKDiving = c(100, 0),
    GKHandling = c(100, 0),
    GKKicking = c(100, 0),
    GKPositioning = c(100, 0),
    GKReflexes = c(100, 0),
    Reactions = c(100, 0)
  )
} else {
  max_min <- data.frame(
    Pace = c(100, 0),
    Shooting = c(100, 0),
    Passing = c(100, 0),
    Dribbling = c(100, 0),
    Defense = c(100, 0),
    Physical = c(100, 0)
  )
}

rownames(max_min) <- c("Max", "Min")
if (is_gk) {
  radar_players <- rbind(max_min, player[, c(
    "GKDiving", "GKHandling", "GKKicking",
    "GKPositioning", "GKReflexes", "Reactions"
  )])
} else {
  radar_players <- rbind(max_min, player[, c(
    "Pace", "Shooting", "Passing",
    "Dribbling", "Defense", "Physical"
  )])
}
beautiful_radarchart(
  radar_players,
  color = color_plots,
)

legend(
  x = "bottom",
  legend = player[, "Name"], horiz = TRUE,
  bty = "n", pch = 20, col = color_plots,
  text.col = "black", cex = 1, pt.cex = 1.5,
)

par(op)

#Cauhoi 2: Thống kê 10 CLB có tổng lương cầu thủ cao nhất
club<-table(fifa22$Club)
club<-data.frame(club)
names(club)
colnames(club)[which(names(club)=="Var1")]<-"Club"
colnames(club)[which(names(club)=="Freq")]<-"Total Player"
names(club)
club_complete<-club[!(is.na(club$Club) | club$Club==""), ]
club_complete$Club
# club_complete<-subset(club_complete, select = -c(Wage) )
for (club in club_complete$Club) {
  find_clb <- fifa22[fifa22$Club == club,]
  club_complete$Wage[club_complete["Club"]==club] <- sum(as.numeric(find_clb$Wage))
}
club_complete <-club_complete[order(-club_complete$Wage),]
Top10Salary<- head(club_complete,10)
par(mar=c(10,6,2,6))
barplot(height=Top10Salary$Wage, names=Top10Salary$Club, 
        col=rgb(0.2,0.4,0.6,0.6),
        ylim=c(0,5000000),
        ylab="Salary", 
        main="Top 10 Salary", 
        width= 0.025,
        space= 1,
        cex.axis=0.6,
        cex.names=0.8,
        las=2
        )
#Cauhoi 3: Đội hình 11 cầu thủ có chỉ số cao nhất ở từng vị trí BEST WORLD XI 
#Cauhoi 4: Thống kê 10 Đội tuyển quốc gia có tổng giá trị cầu thủ cao nhất
#Cauhoi 5: 
