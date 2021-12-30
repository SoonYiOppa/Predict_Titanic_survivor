library(ggplot2)
library(dplyr)

data <- read.csv(file = "C://LJH/PYDATAexam/predict_titanic_survivor/data/train.csv",
                 encoding="UTF-8", header = T)






# 타이타닉 탑승인원 남녀 막대 차트

sex <- count(data, Sex)

ggplot(data=sex, aes(x=Sex,y=n,fill=Sex))+
  geom_bar(stat = 'identity', position = position_dodge())+
  geom_text(aes(y=n, label=paste(n,"명")), position=position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name = "성별",
                      labels = c("여성","남성"))+
  scale_x_discrete(breaks= c("female","male"),labels = c("여성", "남성"))+
  labs(title = "타이타닉 탑승인원 성별",
       x= "성별",
       y="")


# 성별 생존자 막대 그래프

death <- subset(data,
              select = c("Survived","Sex"),
              subset = (Survived==0))
count_death <- count(surv,Sex)

surv <- subset(data,
                select = c("Survived","Sex"),
                subset = (Survived==1))
count_surv <- count(death,Sex)



Surv <- c("0","0","1","1")
Surv <- data.frame(Surv,rbind(count_death,count_surv))


ggplot(Surv, aes(x=Surv,y=n, group=Sex))+
  geom_col(aes(fill=Sex), position = position_dodge())+
  geom_text(aes(y=n, label=paste(n,"명")),position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name="성별",
                      labels = c("여성","남성"))+
  scale_x_discrete(breaks=c(0,1), labels=c("사망","생존"))+
  labs(title = "성별 생존 여부",
       x="생존여부",
       y="")


# 객실 등급
pclass <- count(data, Pclass)
Pclass <- as.character(pclass$Pclass)
class_freq <- pclass$n
pclass <- data.frame(Pclass,class_freq)


ggplot(data =  pclass, aes(x=Pclass, y=class_freq, fill=Pclass))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(y=class_freq, label=paste(class_freq,"명")),position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name="객실 등급",
                      labels = c("1등급","2등급","3등급"))+
  scale_x_discrete(breaks=c(1,2,3), labels=c("1등급","2등급","3등급"))+
  labs(title = "타이타닉 객실 등급별 탑승인원",
       x="객실 등급",
       y="")


#객실 등급별 생존 여부
death_class <- subset(data,
                      select = c("Survived","Pclass"),
                      subset = (Survived==0))
count_class_d <- count(death_class,Pclass)

surv_class <- subset(data,
                     select = c("Survived","Pclass"),
                     subset = (Survived==1))

count_class_s <-count(surv_class,Pclass)
Surv_c <- c("0","0","0","1","1","1")

surv_Pclass <- data.frame(Surv_c,rbind(count_class_d,count_class_s))
surv_Pclass$Pclass <- as.character(surv_Pclass$Pclass)


ggplot(surv_Pclass, aes(x=Surv_c,y=n, group=Pclass))+
  geom_col(aes(fill=Pclass), position = position_dodge())+
  geom_text(aes(y=n, label=paste(n,"명")),position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name="객실 등급",
                      labels = c("1등급","2등급","3등급"))+
  scale_x_discrete(breaks=c(0,1), labels=c("사망","생존"))+
  labs(title = "객실 등급별 생존 여부",
       x="생존여부",
       y="")



# 탑승선착장
embarked <- count(data, Embarked)
embarked <- embarked[c(-1),]

ggplot(data = embarked, aes(x=Embarked, y=n, fill=Embarked))+
  geom_bar(stat="identity", position =position_dodge())+
  geom_text(aes(y=n, label = paste(n,"명")), position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name = "선착장",
                      labels = c("Cherbourg","Queenstown","Southampton"))+
  scale_x_discrete(breaks=c("C","Q","S"), labels = c("Cherbourg","Queenstown","Southampton"))+
  labs(title = "탑승 선착장",
       x="",
       y="")

# 탑승 선착장별 생존여부
df_embarked <-subset(data,
                     select = c("Survived","Embarked"))

death_embarked <- count(subset(df_embarked,
                               subset = (Survived==0)),Embarked)
surv_embarked <- count(subset(df_embarked,
                              subset = (Survived==1)),Embarked)

surv_embarked <- surv_embarked[c(-1),]

surv_e <- rep(c("0","1"),each=3)
surv_Embarked <- data.frame(surv_e,rbind(death_embarked,surv_embarked))

ggplot(surv_Embarked, aes(x=surv_e,y=n, group=Embarked))+
  geom_col(aes(fill=Embarked), position = position_dodge())+
  geom_text(aes(y=n, label=paste(n,"명")),position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name="선착장",
                      labels = c("Cherbourg","Queenstown","Southampton"))+
  scale_x_discrete(breaks=c(0,1), labels=c("사망","생존"))+
  labs(title = "선착장별 생존 여부",
       x="생존여부",
       y="")



#생존 인원
survived <- count(data,Survived)
Survived <- as.character(survived$Survived)
survived_freq <- survived$n
survived <- data.frame(Survived, survived_freq)

ggplot(data = survived, aes(x=Survived, y=survived_freq, fill=Survived))+
  geom_bar(stat="identity", position =position_dodge())+
  geom_text(aes(y=survived_freq, label = paste(survived_freq,"명")), position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name = "생존사망자",
                      labels = c("사망자","생존자"))+
  scale_x_discrete(breaks=c("0","1"), labels = c("사망","생존"))+
  labs(title = "생존사망자 수",
       x="",
       y="")

#SibSp 생존여부 막대 그래프
death_sibsp <- count(subset(data,
                     select = c("Survived","SibSp"),
                     subset = (Survived == 0)),SibSp)

surv_sibsp <- count(subset(data,
                           select = c("Survived","SibSp"),
                           subset = (Survived == 1)),SibSp)
surv_s <- c("0","0","0","0","0","0","0","1","1","1","1","1")

df_SibSp <- data.frame(surv_s,rbind(death_sibsp,surv_sibsp))
df_SibSp$SibSp <- as.character(df_SibSp$SibSp)

ggplot(data = df_SibSp, aes(x=SibSp, y=n, fill=surv_s))+
  geom_bar(stat="identity", position =position_dodge())+
  geom_text(aes(y=n, label = paste(n,"명")), position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name = "형제/배우자",
                      labels = c("사망","생존"))+
  #scale_x_discrete(breaks=c("0","1"), labels = c("사망","생존"))+
  labs(title = "형제/배우자 동승별 생존사망자 수",
       x="형제/배우자 동승자 수",
       y="")


#Parch별 생존여부
death_parch <- count(subset(data,
                            select = c("Survived","Parch"),
                            subset = (Survived == 0)),Parch)

surv_parch <- count(subset(data,
                           select = c("Survived","Parch"),
                           subset = (Survived == 1)),Parch)
surv_p <- c("0","0","0","0","0","0","0","1","1","1","1","1")

df_Parch <- data.frame(surv_p,rbind(death_parch,surv_parch))
df_Parch$Parch <- as.character(df_Parch$Parch)

ggplot(data = df_Parch, aes(x=Parch, y=n, fill=surv_p))+
  geom_bar(stat="identity", position =position_dodge())+
  geom_text(aes(y=n, label = paste(n,"명")), position = position_dodge(0.9))+
  theme_minimal()+
  scale_fill_discrete(name = "부모/자녀",
                      labels = c("사망","생존"))+
  #scale_x_discrete(breaks=c("0","1"), labels = c("사망","생존"))+
  labs(title = "부모/자녀 동승별 생존사망자 수",
       x="부모/자녀 동승자 수",
       y="")