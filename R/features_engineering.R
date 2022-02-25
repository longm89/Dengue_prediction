#### adding the periodicity of 3 years variable
w <- 2*pi/52
Time <- 1:nrow(merged_iq_train)
Nfourier<-52
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*Time*i))
  assign(paste("sin", i, sep=""),sin(w*Time*i))
}
objects()
#####################insertion de la base de fourier dans la data.frame

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
paste("data.frame(merged_iq_train,",cos,",",sin,")",sep="")
merged_iq_train <- eval(parse(text=paste("data.frame(merged_iq_train,",cos,",",sin,")",sep="")))
names(merged_iq_train)

#################### add lag features
#for Iquitos
merged_iq_train <- merged_iq_train %>%
  mutate(lag_1_total_cases = lag(total_cases))
merged_iq_train[1, "lag_1_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_2_total_cases = lag(lag_1_total_cases))
merged_iq_train[1:2, "lag_2_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_3_total_cases = lag(lag_2_total_cases))
merged_iq_train[1:3, "lag_3_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_4_total_cases = lag(lag_3_total_cases))
merged_iq_train[1:4, "lag_4_total_cases"] = 0
merged_iq_train <- merged_iq_train %>%
  mutate(lag_5_total_cases = lag(lag(lag_3_total_cases)))
merged_iq_train[1:5, "lag_5_total_cases"] = 0

# for San Juan
merged_sj_train <- merged_sj_train %>%
  mutate(lag_1_total_cases = lag(total_cases))
merged_sj_train[1, "lag_1_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_2_total_cases = lag(lag_1_total_cases))
merged_sj_train[1:2, "lag_2_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_3_total_cases = lag(lag_2_total_cases))
merged_sj_train[1:3, "lag_3_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_4_total_cases = lag(lag_3_total_cases))
merged_sj_train[1:4, "lag_4_total_cases"] = 0
merged_sj_train <- merged_sj_train %>%
  mutate(lag_5_total_cases = lag(lag(lag_3_total_cases)))
merged_sj_train[1:5, "lag_5_total_cases"] = 0