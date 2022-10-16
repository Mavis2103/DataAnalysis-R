library(fmsb)
library(scales)
library(png)
# library(RCurl)
library(patchwork)
library(ggplot2)
library(magick)

# Hằng
wage_condition <- 200000
color_plots <- c("#00AFBB", "#FC4E07")

# Hàm chung
# custom lại radachart
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
# Lấy thông tin của các cầu thủ so sánh
players_selected <- function(list_names) {
  v <- c()
  p <- c()
  # Lấy vị trí của cầu thủ trong list name
  for (name in list_names) {
    find_index <- which(players_card$Name == name)
    p <- paste(p, players_card[find_index, "Best.Position"])
  }
  # Lấy
  for (name in list_names) {
    find_index <- which(players_card$Name == name)
    # = position <- players_card[find_index, "Position"]
    # Nếu là thủ môn
    if (grepl("GK", p)) {
      v <- rbind(v, players_card[find_index, c(
        "Photo", "Name",
        "GKDiving", "GKHandling", "GKKicking",
        "GKPositioning", "GKReflexes", "Reactions"
      )])
    } else { # Nếu không phải là thủ môn
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

# YÊU CẦU
# 1. Tìm dataset
fifa22 <- read.csv("DataSet/FIFA22.csv")
# fifa22 <- read.csv("C:\\Users\\ADMIN\\OneDrive - ddevlife\\Giaotrinh\\Nam4\\Ky1\\Data analysist\\Exam\\Mid\\DataAnalysis-R\\DataSet\\FIFA22.csv", encoding = 'utf8')
# Lấy tất cả tên cột
colnames_fifa22 <- colnames(fifa22)

# 2. Giải thích các biến (cột) trong dataset
# names(df): # liệt kê các tên cột
# str(df): # Xem kiểu dữ liệu cột
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

# 3. Làm sạch dữ liệu, xử lý các dữ liệu bị thiếu
# Format dữ liệu cột "Wage" từ €250K -> 250000
fifa22$Wage[TRUE] <-
  paste(regmatches(fifa22$Wage, regexpr("[0-9]+", fifa22$Wage)),
    ifelse(grepl("K", fifa22$Wage), "000", ""),
    sep = ""
  )

# Format dữ liệu cột "Value" từ €107.5M -> 107.5
fifa22$Value <-
  paste(as.numeric(regmatches(fifa22$Value, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",fifa22$Value))))
# Những cầu thủ Wage > 200000
rs_wage <- subset(fifa22, as.numeric(fifa22$Wage) > wage_condition)

# 4. Tạo các biến mới dựa vào các biến đã có và các điều kiện
# 5. Đặt các câu hỏi nghiên cứu
#Cauhoi 1: So sánh chỉ số 2 cầu thủ bất kỳ
# Tạo cột Pace
fifa22$Pace <-
  round(
    fifa22$Acceleration * 0.45
      + fifa22$SprintSpeed * 0.55
  )

# Tạo cột Shooting
fifa22$Shooting <-
  round(
    fifa22$Finishing * 0.45 + fifa22$LongShots * 0.2 +
      fifa22$Penalties * 0.05 + fifa22$Positioning * 0.05 +
      fifa22$ShotPower * 0.2 + fifa22$Volleys * 0.05
  )

# Tạo cột Passing
fifa22$Passing <-
  round(fifa22$Crossing * 0.2 + fifa22$Curve * 0.05 + fifa22$FKAccuracy * 0.05 +
    fifa22$LongPassing * 0.15 + fifa22$ShortPassing * 0.35 +
    fifa22$Vision * 0.2)

# Tạo cột Dribbling
fifa22$Dribbling <-
  round(fifa22$Agility * 0.1 + fifa22$Balance * 0.05 +
    fifa22$BallControl * 0.35 + fifa22$Composure * 0 +
    fifa22$Dribbling * 0.5 + fifa22$Reactions * 0)

# Tạo cột Defense
fifa22$Defense <-
  round(fifa22$DefensiveAwareness * 0.3 + fifa22$HeadingAccuracy * 0.1 +
    fifa22$Interceptions * 0.2 + fifa22$SlidingTackle * 0.1 +
    fifa22$StandingTackle * 0.3)

# Tạo cột Physical
fifa22$Physical <-
  round(fifa22$Aggression * 0.2 + fifa22$Jumping * 0.05 +
    fifa22$Stamina * 0.25 + fifa22$Strength * 0.5)

# Tạo dataframe player_card để lưu thông tin cầu thủ được sa sánh
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

# Chỉnh margin cho hình
op <- par(mar = c(2, 3, 0, 3))
# List các cầu thủ được so sánh
compare_list <- c("Cristiano Ronaldo", "L. Messi")

# Lấy cầu thủ
player <- players_selected(compare_list)
# Kiểm tra nếu là thủ môn
is_gk <- length(colnames(player)[grepl("GK", colnames(player))]) > 0
# Kiểm tra nếu là thủ môn thì lấy thông số của thủ môn so sánh
if (is_gk) {
  # Tạo dataframe max_min
  max_min <- data.frame(
    # Lấy min = 0, max = 100 cho từng chỉ số
    GKDiving = c(100, 0),
    GKHandling = c(100, 0),
    GKKicking = c(100, 0),
    GKPositioning = c(100, 0),
    GKReflexes = c(100, 0),
    Reactions = c(100, 0)
  )
# Nếu không phải là thủ môn thì lấy thông số của cầu thủ bình thường
} else {
  # Lấy min = 0, max = 100 cho từng chỉ số
  max_min <- data.frame(
    # Lấy min = 0, max = 100 cho từng chỉ số
    Pace = c(100, 0),
    Shooting = c(100, 0),
    Passing = c(100, 0),
    Dribbling = c(100, 0),
    Defense = c(100, 0),
    Physical = c(100, 0)
  )
}

# Đặt lại tên cột cho dataframe max_min
rownames(max_min) <- c("Max", "Min")
# Kiểm tra nếu là thủ môn thì gắn vào dataframe max_min thông số của thủ môn
if (is_gk) {
  radar_players <- rbind(max_min, player[, c(
    "GKDiving", "GKHandling", "GKKicking",
    "GKPositioning", "GKReflexes", "Reactions"
  )])
# Kiểm tra nếu không phải là thủ môn thì gắn vào dataframe max_min thông số của cầu thủ bình thường
} else {
  radar_players <- rbind(max_min, player[, c(
    "Pace", "Shooting", "Passing",
    "Dribbling", "Defense", "Physical"
  )])
}
# Vẽ radachart
beautiful_radarchart(
  radar_players,
  color = color_plots,
)
# Thêm tên cầu thủ phía dưới biểu đồ
legend(
  x = "bottom",
  legend = player[, "Name"], horiz = TRUE,
  bty = "n", pch = 20, col = color_plots,
  text.col = "black", cex = 1, pt.cex = 1.5,
)

# Chỉnh margin cho hình
par(op)

#Cauhoi 2: Thống kê 10 CLB có tổng lương cầu thủ cao nhất
# Tạo bảng club lấy từ cột "Club" ở fifa22 dataframe
club<-table(fifa22$Club)
# Tạo dataframe club
club<-data.frame(club)
# names(club)
# Đổi tên cột "Var1" -> "Club"
colnames(club)[which(names(club)=="Var1")]<-"Club"
# Đổi tên cột "Freq" -> "Total Player"
colnames(club)[which(names(club)=="Freq")]<-"Total Player"
# Loại những dữ liệu club na hoặc rỗng
club_complete<-club[!(is.na(club$Club) | club$Club==""), ]
# Xóa cột Wage
# club_complete<-subset(club_complete, select = -c(Wage) )
# Lặp qua từng club
for (club in club_complete$Club) {
  # Tìm tất cả club bằng club
  find_clb <- fifa22[fifa22$Club == club,]
  # Tạo cột Wage và gán giá trị bằng tổng cột Wage ở biến "find_clb"
  club_complete$Wage[club_complete["Club"]==club] <- sum(as.numeric(find_clb$Wage))
}
# Sắp xếp lại thứ tự cột Wage từ lớn đến bé
club_complete <-club_complete[order(-club_complete$Wage),]
# Lấy 10 dòng đầu tiên
Top10Salary<- head(club_complete,10)
# Chỉnh margin của biểu đồ
par(mar=c(10,6,2,6))
# Tạo biểu đồ cột với hàng ngang là tên câu lạc bộ, cột dọc là mức lương
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
# Tạo bảng position lấy từ cột "Position" ở fifa22 dataframe
position <- table(fifa22$Best.Position)
# Tạo dataframe position
position <- data.frame(position)
# Đổi tên cột "Var1" -> "Position"
colnames(position)[which(names(position)=="Var1")]<-"Position"
# Xóa cột Freq
position<-subset(position, select = -c(Freq) )

# Lặp qua từng position
for (pos in position$Position) {
  # Tìm tất cả cầu thủ theo vị trí
  player_by_position <- fifa22[fifa22$Best.Position == pos,]
  # Tìm cầu thủ theo quốc gia
  # player_by_position <- player_by_position[player_by_position$Nationality == 'England',]
  # Tìm cầu thủ có ovr cao nhất ở từng vị trí
  best_ovr_player_by_position <- player_by_position$Name[player_by_position$Overall == max(player_by_position$Overall)]
  # Tìm ảnh cầu thủ có ovr cao nhất ở từng vị trí
  best_ovr_player_photo_by_position <- player_by_position$Photo[player_by_position$Overall == max(player_by_position$Overall)]
  # Tạo cột "Player" và gán giá trị bằng biến best_ovr_player_by_position
  position$Player[position['Position'] == pos] <- best_ovr_player_by_position
  # Tạo cột "Photo" và gán giá trị bằng biến best_ovr_player_photo_by_position
  position$Photo[position['Position'] == pos] <- best_ovr_player_photo_by_position
}
# position
# Hàm lấy ảnh của cầu thủ theo từng vị trí
playerByPos <- function(pos) {
  return(position$Photo[position$Position == pos])
}
# Lấy ảnh thủ môn
GK <- c(playerByPos('GK'))
# Lấy ảnh hàng hậu vệ
Def <- c(playerByPos('LB'), playerByPos('CB'), playerByPos('RB'))
# Lấy ảnh hàng tiền vệ
Mf <- c(playerByPos('LM'), playerByPos('CDM'), playerByPos('CM'), playerByPos('RM'))
# Lấy ảnh hàng tiền đạo
St <- c(playerByPos('LW'), playerByPos('ST'), playerByPos('RW'))

# Tạo vector đội hình BEST_WORLD_XI
BEST_WORLD_XI <- c(GK, Def, Mf, St)

# Định nghĩa x của biểu đồ
xpos <- 1:500
# Định nghĩa y của biểu đồ
ypos <- xpos
# Tạo dataframe
data_frame = data.frame(xpos = xpos,
                        ypos = ypos)

# Lấy ảnh sân bóng
image <- "sanco.png"
# Đọc ảnh sân bóng
pitch <- readPNG(image, native = TRUE)
# Tạo biểu đồ bằng ggplot
graph <- ggplot(data_frame, aes(xpos, ypos)) + geom_point()
# Tạo hàm lấy ảnh từng cầu thủ
getPlayerPhoto <- function() {
  # Tạo vector
  player_photo <- vector('list', 2)
  # Tạo biến đếm i
  i <- 1
 #  Tạo array pos chứa vị trí của các ảnh cầu thủ
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
    # Lặp qua từng link ảnh
    for (url in BEST_WORLD_XI) {
      # Thêm ảnh vào biểu đồ bằng hàm annnotation_raster
      player_photo[[url]] <- annotation_raster(image_read(url), xmin= pos[1,i], xmax=as.numeric(pos[1,i] + 50), ymin=pos[2,i], ymax=as.numeric(pos[2,i] + 50))
      # Tăng biến đếm lên 1 sau mỗi vòng lặp
      i <- i + 1
    }
  # Trả về ảnh cầu thủ
  return(player_photo)
}

# Tạo biểu đồ và thêm ảnh vào
pitchh_graph <- graph + annotation_raster(pitch, xmin=0, xmax=500, ymin=0, ymax=500) + getPlayerPhoto()
# Thêm chữ vị trí của mỗi cầu thủ theo từng vị trí
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
# Tạo bảng national_team lấy từ cột "Nationality" ở fifa22 dataframe
national_team<-table(fifa22$Nationality)
# Tạo dataframe national_team
national_team<-data.frame(national_team)
# Đổi tên cột "var1" -> "National_team"
colnames(national_team)[which(names(national_team)=="Var1")]<-"National_team"
# Xóa cột Freq
national_team_complete<-subset(national_team, select = -c(Freq) )
# Lặp qua từng tuyển quốc gia
for (nt in national_team_complete$National_team) {
  # Tìm từng quốc gia
  find_national_team <- fifa22[fifa22$Nationality == nt,]
  # Tạo cột "Total_value" và gán giá trị bằng tổng cột "Value" ở biến find_national_team
  national_team_complete$Total_value[national_team_complete["National_team"]==nt] <- 
    sum(as.numeric(find_national_team$Value))
}
# Sắp xếp lại thứ tự của cột "Total_value" theo hướng giảm dần
national_team_complete <-national_team_complete[order(-national_team_complete$Total_value),]
# Lấy 10 dòng đầu tiên
Top10Value<- head(national_team_complete,10)
# Chỉnh lại margin của biểu đồ
par(mar=c(10,6,2,6))
# Tạo biểu đồ cột với hàng ngang là tên quốc, cột dọc là tổng giá trị cầu thủ
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
