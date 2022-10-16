library(fmsb)
library(scales)
library(png)
# library(RCurl)
library(patchwork)
library(ggplot2)
library(magick)

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
fifa22 <- read.csv("DataSet/FIFA22.csv")
# fifa22 <- read.csv("C:\\Users\\ADMIN\\OneDrive - ddevlife\\Giaotrinh\\Nam4\\Ky1\\Data analysist\\Exam\\Mid\\DataAnalysis-R\\DataSet\\FIFA22.csv", encoding = 'utf8')
colnames_fifa22 <- colnames(fifa22)

# 1. Giải thích các biến (cột) trong dataset
# names(df) # liệt kê các tên cột
# str(df)
# ID (int): Id của cầu thủ
# Name (chr): Tên cầu thủ
# Age (int): Tuổi cầu thủ
# Photo (chr): Ảnh đại diện
# Nationality (chr): Quốc tịch
# Flag (chr): Quốc kỳ
# Overall (int): Chỉ số tổng quát
# Potential (int): Tiềm năng cầu thủ
# Club (chr): Câu lạc bộ đang đầu quân
# Club logo (chr): Logo câu lạc bộ
# Value (chr): Giá trị chuyển nhượng
# Wage (chr): Lương
# Special (int):
# Preferred Foot (chr): Chân thuận
# International Reputation (int): Chỉ số nổi tiếng
# Weak Foot (int): Chân không thuận
# Skill moves (int): Kỹ thuật
# Work rate (chr): Tỷ lệ tấn công/phòng thủ
# Body type (chr): Thể hình
# Real face (chr): Các cầu thủ có gương mặt giống với ngoài đời
# Position (chr): Vị trí chơi
# Jersey number (num): Số áo
# Joined (chr): Ngày gia nhập câu lạc bộ
# Loaned from (chr): Cho mượn từ
# Contract valid until (chr): Ngày đáo hạn hợp đồng
# Height (chr): Chiều cao
# Weight (chr): Cân nặng
# Crossing (int): Tỷ lệ tạt bóng qua mặt dối phương thành công
# Finishing (int): Chí số dứt điểm
# HeadingAccuracy (int): Tỷ lệ đánh đầu thành bàn
# ShortPassing (int): Tỷ lệ chuyền ngắn thành công
# Volleys (num): Tỷ lệ vô lê thành bàn
# Dribbling (int): Tỷ lệ đi (rê) bóng
# Curve (num): Cứa lòng
# FKAccuracy (int): Chỉ số đá phạt
# LongPassing (int): Chuyền dài
# BallControl (int): Kiếm soát bóng
# Acceleration (int): Chạy nước rút
# SprintSpeed (int): Tốc dộ chạy nước rút
# Agility (num): Độ linh hoạt, khả năng phản ứng, xử lý tình huống
# Reactions (int): Phản ứng trước các pha bóng
# Balance (num): Tỷ lệ giữ nhịp độ ổn định khi tăng tốc, giảm tốc hay không hoặc khả năng kiểm soát bóng tại thời điểm tranh chấp ra sao
# ShotPower (int): Lực sút
# Jumping (num): Nhảy
# Stamina (int): Thể lực
# Strength (int): Sức mạnh
# LongShots (int): Sút xa
# Aggression (int): Độ quyết đoán, mạnh bạo trong các pha bóng
# Interceptions (num): Đánh chặn
# Potisioning (num): Khả năng chọn vị trí
# Vision (num): Tầm nhìn
# Penalties (int): Đá phạt đền
# Composure (num): Sự bình tĩnh
# Marking (num): Kèm người
# StandingTackle (int): Tắc bóng
# SlidingTackle (num): Xoạc bóng
# GKDiving (int): Thủ môn đổ người
# GKHandling (int): Thủ môn bắt bóng
# GKKicking (int): Thủ môn phát bóng
# GKPositioning (int): Thủ môn chọn vị trí
# GKReflexes (int): Thủ môn phản xạ
# Best position (chr): Vị trí phù hợp nhất
# Best overall raing (int): Chỉ số tống quan tốt nhất
# Realease clause (chr): Phí giải phóng hợp đồng
# DefensiveAwareness (num): Nhận thức phòng ngự

# Đếm sô lượng cầu thủ của mỗi quốc gia
# = print(table(fifa22$Nationality))

# Format value of col Wage €250K -> 250000
fifa22$Wage[TRUE] <-
  paste(regmatches(fifa22$Wage, regexpr("[0-9]+", fifa22$Wage)),
    ifelse(grepl("K", fifa22$Wage), "000", ""),
    sep = ""
  )

# Format value of Value col €107.5M -> 107.5
fifa22$Value <-
  paste(as.numeric(regmatches(fifa22$Value, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",fifa22$Value))))
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
# names(club)
colnames(club)[which(names(club)=="Var1")]<-"Club"
colnames(club)[which(names(club)=="Freq")]<-"Total Player"
# names(club)
club_complete<-club[!(is.na(club$Club) | club$Club==""), ]
# club_complete$Club
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
position <- table(fifa22$Best.Position)
position <- data.frame(position)
colnames(position)[which(names(position)=="Var1")]<-"Position"
position<-subset(position, select = -c(Freq) )

for (pos in position$Position) {
  player_by_position <- fifa22[fifa22$Best.Position == pos,]
  # player_by_position <- player_by_position[player_by_position$Nationality == 'England',]
  best_ovr_player_by_position <- player_by_position$Name[player_by_position$Overall == max(player_by_position$Overall)]
  best_ovr_player_photo_by_position <- player_by_position$Photo[player_by_position$Overall == max(player_by_position$Overall)]
  position$Player[position['Position'] == pos] <- best_ovr_player_by_position
  position$Photo[position['Position'] == pos] <- best_ovr_player_photo_by_position
}
position

playerByPos <- function(pos) {
  return(position$Photo[position$Position == pos])
}
GK <- c(playerByPos('GK'))
Def <- c(playerByPos('LB'), playerByPos('CB'), playerByPos('RB'))
Mf <- c(playerByPos('LM'), playerByPos('CDM'), playerByPos('CM'), playerByPos('RM'))
St <- c(playerByPos('LW'), playerByPos('ST'), playerByPos('RW'))


BEST_WORLD_XI <- c(GK, Def, Mf, St)

# defining the x coordinates
xpos <- 1:500

# defining the y coordinates
ypos <- xpos

data_frame = data.frame(xpos = xpos,
                        ypos = ypos)

image <- "sanco.png"
pitch <- readPNG(image, native = TRUE)
# plotting the data
graph <- ggplot(data_frame, aes(xpos, ypos)) + geom_point()
test <- function() {
  player_photo <- vector('list', 2)
  i <- 1
 pos <- array(c(c(x=225, y=40),
         c(x=95, y=130),
         c(x=225, y=110),
         c(x=355, y=130),
         c(x=25, y=290),
         c(x=145, y=210),
         c(x=305, y=240),
         c(x=425, y=290),
         c(x=75, y=390),
         c(x=225, y=440),
         c(x=375, y=390)), dim = c(2,11))
    for (url in BEST_WORLD_XI) {
      player_photo[[url]] <- annotation_raster(image_read(url), xmin= pos[1,i], xmax=as.numeric(pos[1,i] + 50), ymin=pos[2,i], ymax=as.numeric(pos[2,i] + 50))
      i <- i + 1
    }
  return(player_photo)
}

pitchh_graph <- graph + annotation_raster(pitch, xmin=0, xmax=500, ymin=0, ymax=500) + test()
pitchh_graph +
  geom_text(x=250, y=30 , label='GK' , size=8) +
  geom_text(x=120, y=120, label='CB' , size=8) +
  geom_text(x=250, y=100, label='CB' , size=8) +
  geom_text(x=380, y=120, label='CB' , size=8) +
  geom_text(x=50 , y=280, label='LM' , size=8) +
  geom_text(x=170, y=200, label='CDM', size=8) +
  geom_text(x=330, y=230, label='CM' , size=8) +
  geom_text(x=450, y=280, label='RM' , size=8) +
  geom_text(x=100, y=380, label='LW' , size=8) +
  geom_text(x=250, y=430, label='ST' , size=8) +
  geom_text(x=400, y=380, label='RW' , size=8) +
  geom_text(x=250, y=510, label='BEST WORLD XI' , size=8)


#Cauhoi 4: Thống kê 10 Đội tuyển quốc gia có tổng giá trị cầu thủ cao nhất
national_team<-table(fifa22$Nationality)
national_team<-data.frame(national_team)
colnames(national_team)[which(names(national_team)=="Var1")]<-"National_team"
national_team_complete<-subset(national_team, select = -c(Freq) )

for (nt in national_team_complete$National_team) {
  find_national_team <- fifa22[fifa22$Nationality == nt,]
  national_team_complete$Total_value[national_team_complete["National_team"]==nt] <- 
    sum(as.numeric(find_national_team$Value))
}
national_team_complete <-national_team_complete[order(-national_team_complete$Total_value),]
Top10Value<- head(national_team_complete,10)
Top10Value
par(mar=c(10,6,2,6))
barplot(height=Top10Value$Total_value, names=Top10Value$National_team,
        col=rgb(0.2,0.4,0.6,0.6),
        ylim=c(0,600000),
        ylab="Value",
        main="Top 10 National team value",
        width= 0.025,
        space= 1,
        cex.axis=0.6,
        cex.names=0.8,
        las=2
        )

#Cauhoi 5: 
