###
library(httr)

library(stringr)
library(dplyr)
options(java.parameters = "-Xmx4g")
library(xlsx)

library(ggplot2)

library(jsonlite)
library(XML)

library(kormaps2014)

###

korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "UTF-8","CP949")
ggplot(korpop1,aes(map_id=code, fill=pop))+
  geom_map(map = kormap1, colour="black",size=0.1)+
  expand_limits(x= kormap1$long,y = kormap1$lat)+
  scale_fill_gradientn(colours = c('white','orange','red'))+
  ggtitle('2014년도 시도별 인구분포도')+
  coord_map()

###
# 통계분류포털 - 한국행정구역분류
# https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#
dong_code_df <- read.xlsx("data/01 한국행정구역분류_2022.4.1.기준.xlsx", 3)

colnames(dong_code_df) <- dong_code_df[2, ]
result_df <- dong_code_df[-1:-2, ] %>%
  select(`비고\n(7자리)`, 시도) %>%
  rename(code="비고\n(7자리)", sido="시도") %>% 
  filter(nchar(code)==2)

# 국토교통 통계누리 - 지적통계
# https://stat.molit.go.kr/portal/cate/statView.do?hRsId=24&hFormId=5408&hDivEng=&month_yn=
area_df <- read.table("data/02 행정구역별·소유구분별+국토이용현황_5408_1_220408.csv", fileEncoding="UTF-8-BOM", header=T, sep=",")
area_km_df <- area_df %>%
                filter(소유구분=="합계", LEVEL1=="면적", 구분!="합계") %>% 
                mutate_at("값", function(x) return(x %/% 1000000)) %>% 
                select(구분, 값) %>% 
                rename(sido="구분", area_km="값")
result_df <- result_df %>% left_join(area_km_df, by="sido")

# 통계청 - 인구주택총조사 - 연령 및 성별 인구
# https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1B040A3
population_df <- read.csv("data/03 연령_및_성별_인구__읍면동_2015_2020___시군구_20162019__220408.csv")
population_count <- population_df %>%
                     filter(연령별=="합계") %>% 
                     select(행정구역별.읍면동., X2020) %>% 
                     rename(sido="행정구역별.읍면동.", population="X2020")
result_df <- result_df %>% left_join(population_count, by="sido")


###
# 체육센터
# 공공데이터포털 - XML 서울올림픽기념국민체육진흥공단_전국체육시설 상세 정보
# https://www.data.go.kr/tcs/dss/selectApiDataDetailView.do?publicDataPk=15056537
exercise_df <- data.frame()
tryCatch(
  {
    exercise_df <- read.csv("data/04 서울올림픽기념국민체육진흥공단_전국체육시설 상세 정보.csv")
  },
  warning = function(e){
    
    # 총 개수 가져오기
    pageNo <- 1
    numOfRows <- 1
    service_key <- "Z%2F1jvIC4lqVQWmMvr1JQmMKHCE%2F8xe4KKnUg15p9aFKTPhfSz%2FSzxzrd3EUwukqdsIEY1ZZVruYWc%2FbuAHfMQA%3D%3D"
    url_origin <- "http://www.kspo.or.kr/openapi/service/sportsNewFacilInfoService/getNewFacilInfoList"
    url <- paste0(url_origin, "?serviceKey=", service_key, "&pageNo=", pageNo, "&numOfRows=", numOfRows)
    
    response <- GET(url)
    json_data <- content(response, type = 'text', encoding = "UTF-8")
    json_obj <- fromJSON(json_data)
    total_exercise <- json_obj$response$body$totalCount
    # total_exercise <- 81951
    
    # 전체 항목 가져오기
    numOfRows <- 5000
    
    tryCatch({
      for (i in 1:(total_exercise%/%numOfRows+1)){
        pageNo <- i
        url <- paste0(url_origin, "?serviceKey=", service_key, "&pageNo=", pageNo, "&numOfRows=", numOfRows)
        response <- GET(url)
        Sys.sleep(8)
        json_data <- content(response, type = 'text', encoding = "UTF-8")
        json_obj <- fromJSON(json_data)
        exercise_df <<- rbind(exercise_df, as.data.frame(json_obj$response$body$items))
        cat(i, (total_exercise%/%numOfRows+1), dim(as.data.frame(json_obj$response$body$items)), dim(exercise_df), "\n")
      }},
      error = function(e){}
    )
    
    write.csv(exercise_df, "data/04 서울올림픽기념국민체육진흥공단_전국체육시설 상세 정보.csv", row.names=F)
  },
  finally={
    exercise_count <- exercise_df %>%
      filter(item.faciGbNm=="공공" & item.faciStat=="정상운영") %>%
      count(item.cpNm) %>% 
      rename(sido="item.cpNm", exercise="n")
    
    result_df <- result_df %>% left_join(exercise_count, by="sido")
  }
)


###
# 도서관
# 공공데이터포털 - 문화체육관광부_전국공공도서관통계
# https://www.data.go.kr/data/15072345/fileData.do
library_csv <- read.csv("data/05 문화체육관광부_전국공공도서관통계_20201028.csv")
library_count <- library_csv %>% 
                   count(면적) %>%
                   rename(sido_short="면적", library="n")
result_df <- cbind(result_df %>% arrange(sido), library_count)[, c(1, 2, 6, 3, 4, 5, 7)] %>% arrange(code)

###
# 어린이집 혹은 유치원
# 어린이집정보공개포털
# http://info.childcare.go.kr/info/oais/openapi/OpenApiSlL.jsp
nursery_df <- read.xlsx("data/06 보육통계_어린이집현황_지역별.xls", 1)
nursery_count <- nursery_df %>%
                   select(시도, 계) %>% 
                   rename(sido="시도", nursery="계")
nursery_count[17, 1] = "제주특별자치도"

result_df <- result_df %>% left_join(nursery_count, by="sido")

# 교육부 - 유치원현황
# https://www.moe.go.kr/boardCnts/viewRenew.do?boardID=312&boardSeq=89289&lev=0&searchType=null&statusYN=W&page=1&s=moe&m=0301&opType=N
kindergarden_df <- read.xlsx("data/07 유치원+현황(2021+교육통계+기준).xlsx", 1)
kindergarden_count <- kindergarden_df %>%
                        slice(-1:-3) %>% 
                        select(1, 2) %>% 
                        rename(sido_short="X.2021.유치원.총괄현황.", kindergarden="NA.")

result_df <- result_df %>% left_join(kindergarden_count, by="sido_short")

###
# 노인요양시설
# 보건복지부
# http://www.mohw.go.kr/react/jb/sjb030301vw.jsp?PAR_MENU_ID=03&MENU_ID=032901&CONT_SEQ=366022
senior_care_df <- read.xlsx("data/08 2021_노인복지시설_현황_총괄표(광역시.도).xlsx", 2)
senior_care_count <- senior_care_df %>%
                      filter(!is.na(NA.)) %>%
                      select(가..노인주거복지시설.총괄표, NA..1) %>% 
                      rename(sido_short="가..노인주거복지시설.총괄표", senior_care="NA..1")

result_df <- result_df %>% left_join(senior_care_count[-1:-2,], by="sido_short")

###
# 주민건강센터
# 한국건강증진개발원
# https://www.khealth.or.kr/board/view?pageNum=1&rowCnt=10&no1=626&linkId=1002772&menuId=MENU00907&schType=0&schText=&searchType=&boardStyle=&categoryId=&continent=&country=&contents1=
health_center <- c(1, 11, 3, 14, 10, 1, 1, 2, 25, 16, 5, 11, 13, 20, 6, 13, 4)

result_df <- cbind(result_df, health_center)

###
# 공원
# 전국도시공원정보표준
# https://www.data.go.kr/data/15012890/standard.do
park_df <- read.csv("data/10 전국도시공원정보표준데이터.csv")
park_count <- park_df %>%
                select(관리번호, 제공기관명) %>% 
                mutate_at("제공기관명", function(x) return(str_split_fixed(park_df$제공기관명, " ", 2)[,1])) %>% 
                count(제공기관명) %>% 
                rename(sido="제공기관명", park="n")

park_count[10, 2] <- park_count[10, 2] + park_count[6,2] + park_count[9, 2]
park_count[13, 2] <- park_count[13, 2] + park_count[14, 2]
park_count[15, 2] <- park_count[15, 2] + park_count[16, 2]
park_count <- park_count[-which(park_count$sido %in% setdiff(park_count$sido, result_df$sido)), ]

result_df <- result_df %>% left_join(park_count, by="sido")

###
#ggplot(korpop1,aes(map_id=code, fill=pop))+
result_df <- result_df %>% mutate_at(c(1, 4:12), as.numeric)
scaleFactor <- max(result_df$population) / max(result_df$area_km)


# ggplot(result_df, aes(x=reorder(sido_short, code), width=.4)) +
#   geom_col(aes(y=population/library), fill="skyblue", position=position_nudge(x = -.2)) +
#   geom_col(aes(y=area_km/library * scaleFactor), fill="darkgreen", position=position_nudge(x = .2)) +
#   geom_label(aes(label = round(population/library), y =population/library), size=3, nudge_x = -0.2, nudge_y=2)+
#   geom_text(aes(label = round(population/library), y =population/library), colour="blue", nudge_x=-0.2, size=2) +
#   geom_text(aes(label = round(area_km/library, 2), y =area_km/library * scaleFactor), position = position_nudge(x=0.2), vjust = 0, size=2) +
#   scale_y_continuous(name="도서관 1개 당 사람", sec.axis=sec_axis(~./scaleFactor, name="도서관 1개 당 km")) +
#   xlab("시도")


#ggplot(result_df, aes(x=reorder(sido_short, code), width=.4)) +
#  geom_col(aes(y=population/nursery), fill="skyblue", position=position_nudge(x = -.2)) +
#  geom_col(aes(y=area_km/nursery * scaleFactor), fill="darkgreen", position=position_nudge(x = .2)) +
#  scale_y_continuous(name="어린이집 1개 당 사람", sec.axis=sec_axis(~./scaleFactor, name="어린이집 1개 당 km"))

# write.table(result_df, "result.csv", sep=",", row.names = F)

double_axis_bar <- function (data, y, ...){
  args = list(...)

  tryCatch({
    data_tmp <- data[, c("code", "sido_short", "population", "area_km", y)] %>% mutate_at(y, as.numeric)
  },
  error=function(e){
    stop("오류 발생: 숫자형 열을 지정해주세요.")
  })
  
  colnames(data_tmp) <- c(colnames(data_tmp)[-5], "data")
  data_tmp <- data_tmp %>%
    mutate(population=round(population/data, 2), area=round(area_km/data*scaleFactor, 2)) %>%
    select(code, sido_short, population, area)
  View(data_tmp)
  
  double_plot <- ggplot(data_tmp, aes(x=reorder(sido_short, code), width=.4)) +
    geom_col(aes(y=population, fill="population"), position=position_nudge(x = -.2), show.legend =TRUE) +
    geom_col(aes(y=area, fill="area") , position=position_nudge(x = .2), show.legend =TRUE) +
    scale_y_continuous(name=paste("people per", y), label=scales::comma, sec.axis=sec_axis(~./scaleFactor, name=paste("km^2 per", y), label=scales::comma)) +
    scale_fill_manual(name = "", values = c("population"="skyblue", "area"="dark blue")) +
    labs(title=gsub("_", " ", toupper(y)), x="") +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  print(double_plot)
  
  if ("save" %in% names(args) & args$save){
    ggsave(plot=double_plot, paste0("plots/", toupper(y), ".png"), dpi=100)
  }
}

write.csv(result_df, "result_df.csv")

double_axis_bar(result_df, "nursery", save=TRUE)
double_axis_bar(result_df, "senior_care", save=TRUE)
double_axis_bar(result_df, "exercise", save=TRUE)
double_axis_bar(result_df, "park", save=TRUE)
double_axis_bar(result_df, "kindergarden", save=TRUE)
double_axis_bar(result_df, "health_center", save=TRUE)
double_axis_bar(result_df, "library", save=TRUE)


###
result_df2 <- result_df %>% select(code, sido)

for (col in colnames(result_df)[6:12]){
  if ("km" %in% col){
    print(col)
  }
  tmp <- data.frame(round(result_df["population"] / result_df[col], 2), round(result_df["area_km"] / result_df[col], 2))
  colnames(tmp) <- paste(c("pop per", "km^2 per"), col)
  
  result_df2 <- cbind(result_df2, tmp)
}

result_df2 %>% std

mean_order <- sapply(result_df2, order) %>% rowMeans
result_df2 <- cbind(result_df2, mean_order)
write.csv(result_df2, "result2.csv")



result_df2 <- result_df2 %>%
                select(code, sido, mean_order) %>% 
                arrange(total_order)
ggplot(result_df2,aes(map_id=code, fill=mean_order)) +
  geom_map(map = kormap1, colour="black",size=0.1) +
  expand_limits(x= kormap1$long,y = kormap1$lat) +
  scale_fill_gradientn(colours = c("blue","ivory")) +
  labs(fill="평균 순위") +
  ggtitle("시도별 평균 순위") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_map()

ggsave("plots/MEAN_ORDER.png")
