getwd()

library(stringr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(reshape)
library(ggthemes)

#------------------------- Step 0: Declaration of Functions-----------------------------------------#

min_conversion<-function(x){
  p<-as.POSIXlt(x,origin="2014-12-31")
  p<-unclass(p)
  min<-p$hour*60+p$min
  return(min)
}

#---------------------------Step 1: Loading the data ------------------------------------------------#

emp_sur<-read.csv("employee_survey_data.csv",stringsAsFactors = F)

# emp_survey measures satisfaction at 3 levels: environment, job and worklife balance. 
# The levels in this dataset are ordered with 1 as lowest level of satisfaction and 4 the highest

man_sur<-read.csv("manager_survey_data.csv",stringsAsFactors = F)

# Feedback about employee by the manager. Last performace rating and current job involvement
# Levels in dataset are again as follows: 1: Worst, 4: Best

gen_data<-read.csv("general_data.csv",stringsAsFactors = F)

# This gives a hsitory of the employee as well as current factors that impact the work life equation
# Past jobs, years in current job, age, role, department highlight professional life parameters
# Similarly distance from home, education, marital status etc highlight personal life parameters

in_time<-read.csv("in_time.csv",stringsAsFactors = F)

out_time<-read.csv("out_time.csv",stringsAsFactors = F)


#------------------------------Processing timestamps from in and out time data--------------#

# Creating the following derived metrics: 

#1: Time(in minutes) spent in office
#2: Number of leaves
#3: Total working days of an employee
#4: Average working time per day of office attended; from in and out timestamps available

# The following code converts in time into minutes

in_time_min<-sapply(in_time[2:ncol(in_time)],function(x) min_conversion(x))
in_time_min<-cbind(in_time[,1],in_time_min)

# The following code converts out time into minutes

out_time_min<-sapply(out_time[2:ncol(out_time)],function(x) min_conversion(x))
out_time_min<-cbind(out_time[,1],out_time_min)

# Time spent in office is calculated as following

duration<-out_time_min-in_time_min
duration<-as.data.frame(duration)
duration$V1<-in_time$X

# Check and discard any column in duration that have more than 95% NAs as this indicates a holiday

tmp <- sort(sapply(duration, function(x) sum(length(which(is.na(x))))*100/nrow(duration)),decreasing = TRUE)
discard_column = names(tmp[tmp>95])
sprintf("Common holiday fell on : %s", discard_column)
duration_new <- (duration[,!(names(duration) %in% discard_column)])
tot_discard_column<-ncol(duration)-ncol(duration_new)

# duration_new is the new duration dataframe where holidays taken by the employee for himself are considered
# this dataframe has eliminated any common/national holidays

# Computes total time(in minutes) spent by an employee in office in an year
duration_new$totaltime<-rowSums(duration_new[,2:250],na.rm = T)  

# Computes the total leaves taken by an employee other than provided leaves
duration_new$leaves<-rowSums(is.na(duration_new[,2:250])) 

# Effective working days of an employee
duration_new$workingdays<-ncol(in_time)-1-tot_discard_column-duration_new$leaves 

# Average time spent per day in office in minutes
duration_new$average_minutes_per_day<-round(duration_new$totaltime/duration_new$workingdays) 

# The following are the derived metrics that need to be merged with main data on EmployeeID

duration_derived<-duration_new[,c("V1","totaltime","leaves","workingdays","average_minutes_per_day")]
colnames(duration_derived)[1]<-"EmployeeID"

#-------------------Timestamp processing and information extraction from it ends here------------------#


#-------------------Consolidated Dataset preparation begins here---------------------------------------#

length(unique(emp_sur$EmployeeID))
length(unique(man_sur$EmployeeID))  
length(unique(gen_data$EmployeeID))
length(unique(duration_derived$EmployeeID))

# Since all the dataframes have 4410 unique values, we can safely merge them without information loss

hr1<-merge(emp_sur,man_sur,by="EmployeeID")
hr2<-merge(hr1,gen_data,by="EmployeeID")
hr3<-merge(hr2,duration_derived,by="EmployeeID")

# Derived metric: Overtime, if yes then 1, otherwise 0
hr3$overtime<-ifelse((hr3$average_minutes_per_day-(hr3$StandardHours*60))>0,1,0)

# hr3 is the overall merged dataset for further data cleaning and analysis


#-----------------------------Data Cleaning Begins Here---------------------------------------------#

#1: Check for NAs and their treatment in each column

str(hr3)

sapply(hr3,function(x) sum(is.na(x)))

# Eliminate any row which has any value NA since we don't want to assume responses from employees or managers

hr4<-na.omit(hr3)

#2: Conversion to lower case characters of character type variables

char_columns<-sapply(hr4,is.character)
char_col<-hr4[,char_columns]
char_col_names<-colnames(char_col)
char_col_names

hr4[,char_col_names]<-sapply(hr4[,char_col_names],str_to_lower)

str(hr4)

#3: Eliminating redundant columns which have all values identical as these will be useless for model building

col_uv <- sapply(hr4, function(x) length(unique(x)))
col_uv
col_1<-names(col_uv[col_uv==1])
sprintf("Column with all vales same: %s",col_1)

hr5 <- hr4[, !names(hr4) %in% col_1]

#4: Check row duplication

which(duplicated(hr5))
# No duplicate rows, so no code needed for duplicate row elimination

str(hr5)

h<-hr5 # Creating simpler dataframe name for EDA

#------------------------------------Performing EDA-----------------------------------------#

# Monitor output variable: Attrition

colnames(h)

p1<-as.data.frame(prop.table(table(h$Attrition)))
colnames(p1)<-c("Status","Percent")
p1$Percent<-round(p1$Percent*100,1)

ggplot(p1,aes(x=Status,y=Percent, fill=Status,label=Percent))+
  geom_bar(stat = "identity")+geom_label(aes(label=Percent))+
  labs(title = "ATTRITION",x = "Status", y = "Percentage") + theme_economist_white()


# Theme Setting for barcharts
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="right")

# Plots of subjective parameters where employee is ranked or gives ranks

plot_grid(ggplot(h, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h,aes(x=factor(overtime),fill=Attrition))+geom_bar(position = "fill")+bar_theme1,
          align = "h")

# Plots on Current Job related parameters: 

plot_grid(ggplot(h, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=DistanceFromHome,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=factor(JobRole),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=factor(PercentSalaryHike),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")

plot_grid(ggplot(h, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=factor(leaves),fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")


# Plots related to Personal Parameters which are not related to current job directly

plot_grid(ggplot(h, aes(x=Age,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=Education,fill=Attrition))+ geom_bar(position = "fill"), 
          ggplot(h, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(h, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1, 
          ggplot(h, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1, 
          align = "h") 


write.csv(h,file = "hr_plots_data.csv")

#---------------------------------Outlier Treatment Begins Here----------------------------------#

char_col_names
hr6<-hr5[,!names(hr5) %in% char_col_names ]

boxplot(hr6[,2:5])
# No visible outliers

boxplot(hr6[,6:9])
# Performance rating boxplot shows outliers, but thats due to less number of 4 star performers, so we have to retain that

boxplot(hr6[,10:11])

# Monthtly income has outliers, so we need to treat them

quantile(hr6$MonthlyIncome,seq(0,1,0.01)) 

# There are 2 big transitions happening at 90%ile of around 15000 and 95%ile of around 8000. 
# Altering values at 90%ile will lead to 10% data change in column where as at 95%ile it will be only 5%
# So, we go ahead with 95%ile level data change.

hr6$MonthlyIncome[which(hr6$MonthlyIncome>178560)]<-178560

boxplot(hr6[,12:13])
quantile(hr6$NumCompaniesWorked,seq(0,1,0.01))

# There is no such big transition happening, so we need not eliminate the outlier from NumCompaniesworked
# even though there was a visible outlier

boxplot(hr6[,14:17])

quantile(hr6$StockOptionLevel,seq(0,1,0.01))  # Ignoring outliers due to insiginificant transitions

quantile(hr6$TotalWorkingYears,seq(0,1,0.01)) # Sharp hike from 35 to 40 at 99%ile level. So, we treat the outlier
hr6$TotalWorkingYears[which(hr6$TotalWorkingYears>35)]<-35

quantile(hr6$TrainingTimesLastYear,seq(0,1,0.01)) # No sharp rises at any level, no outlier treatment required

quantile(hr6$YearsAtCompany,seq(0,1,0.01)) # Sharp increases at 98%ile level, outlier to be treated
hr6$YearsAtCompany[which(hr6$YearsAtCompany>25)]<-25

boxplot(hr6[,18:21])

quantile(hr6$YearsSinceLastPromotion,seq(0,1,0.01)) # Sharp increases at 97%ile level
hr6$YearsSinceLastPromotion[which(hr6$YearsSinceLastPromotion>11)]<-11

quantile(hr6$YearsWithCurrManager,seq(0,1,0.01)) # Sharp increase at 99%ile level
hr6$YearsWithCurrManager[which(hr6$YearsWithCurrManager>14)]<-14

quantile(hr6$totaltime,seq(0,1,0.01)) # Sharp increase at 99%ile level
hr6$totaltime[which(hr6$totaltime>158215)]<-158215

# No visible outliers in leaves

boxplot(hr6[,22:23])

# No visible outlier in Workingdays

quantile(hr6$average_minutes_per_day,seq(0,1,0.01)) # Sharp increase at 99%ile level
hr6$average_minutes_per_day[which(hr6$average_minutes_per_day>654)]<-654

#-------------------------- Outlier Treatment Ends Here----------------------------------------#


#-----------------------------Checking correlation of continuous variables --------------------------------------------#

# Find the correlations
numeric_columns<-hr6[, unlist(lapply(hr6, is.numeric))]
cormatrix <- round(cor(numeric_columns, use="pairwise.complete.obs"), 2)
melted_cormatrix <- melt(cormatrix)

red=rgb(1,0,0); green=rgb(0,1,0); blue=rgb(0,0,1); white=rgb(1,1,1)

ggplot(data = melted_cormatrix, aes(x=melted_cormatrix[,1], y=melted_cormatrix[,2], fill=value)) + geom_tile() + scale_fill_gradient2(low = "red",mid = "white",high = "green") + 
  theme_economist_white()+theme(axis.text.x = element_text(angle = 90))


# Diag and lower triangle is made 0
diag(cormatrix) <- 0
cormatrix[lower.tri(cormatrix)] <- 0
fm <- as.data.frame(as.table(cormatrix))
names(fm) <- c("First", "Second", "Correlation")

# sort and print the top n correlations
df <- fm[order(abs(fm$Correlation), decreasing = T), ]
cor_res <- subset(df, ((df$Correlation >= 0.5 &
                          df$Correlation <= 1) |
                         (df$Correlation >= -1 &
                            df$Correlation <= -0.5) ))

cor_res

write.csv(cor_res,file = "correlation.csv")

# This above piece of code gives us the highly correlated variables which we can easily eliminate while 
# doing the multiple regression

#------------------------------- Correlation Checking ends here --------------------------------#

#------------------------------- Scaling and standardisation of data----------------------------#

colnames(hr5)
str(hr5)



#B: Scaling of continuous variables


cont_cols <- c( 'Age','DistanceFromHome', 'MonthlyIncome', 'NumCompaniesWorked', 'PercentSalaryHike', 
                'TotalWorkingYears','TrainingTimesLastYear','YearsAtCompany', 'YearsSinceLastPromotion',
                'YearsWithCurrManager', 'totaltime', 'leaves', 'workingdays', 'average_minutes_per_day'  )

hr5[, cont_cols] <- sapply(hr5[,cont_cols],scale )

#B: Creation of dummy variables

category_cols <- c('BusinessTravel',
                   'Department',
                   'Education',
                   'EducationField',
                   'EnvironmentSatisfaction',
                   'JobInvolvement',
                   'JobLevel',
                   'JobRole',
                   'JobSatisfaction',
                   'MaritalStatus',
                   'PerformanceRating',
                   'StockOptionLevel',
                   'WorkLifeBalance')

hr5[, category_cols] <- sapply(hr5[,category_cols],factor )

#1 :Environment Satisfaction

dummy_EnvironmentSatisfaction<-data.frame(model.matrix(~EnvironmentSatisfaction,data = hr5))[,-1]

#2 :Job Satisfaction

dummy_JobSatisfaction<-data.frame(model.matrix(~JobSatisfaction,data = hr5))[,-1]

#3 :Work Life Balance 

dummy_WorkLifeBalance<-data.frame(model.matrix(~WorkLifeBalance,data = hr5))[,-1]

#4 :Job Involvement

dummy_JobInvolvement<-data.frame(model.matrix(~JobInvolvement,data = hr5))[,-1]

#5 :Business Travel
dummy_BusinessTravel<-data.frame(model.matrix(~BusinessTravel,data = hr5))[,-1]

#6 :Department
dummy_Department<-data.frame(model.matrix(~Department,data = hr5))[,-1]

#7 :Education
dummy_Education<-data.frame(model.matrix(~Education,data = hr5))[,-1]

#8 :Education Field
dummy_EducationField<-data.frame(model.matrix(~EducationField,data = hr5))[,-1]

#9 :Job Level
dummy_JobLevel<-data.frame(model.matrix(~JobLevel,data = hr5))[,-1]

#10:Job Role
dummy_JobRole<-data.frame(model.matrix(~JobRole,data = hr5))[,-1]

#11:Marital Status
dummy_MaritalStatus<-data.frame(model.matrix(~MaritalStatus,data = hr5))[,-1]

#12:Stock Option Level
dummy_StockOptionLevel<-data.frame(model.matrix(~StockOptionLevel,data = hr5))[,-1]

#13:Performance Rating
dummy_PerformanceRating<-data.frame(model.matrix(~PerformanceRating,data = hr5))[, -1]

# Variables with only 2 levels

#1:Gender
hr5$Gender<- ifelse(hr5$Gender=="male",1,0)

#2:Converting output variable i.e. Attrition to 0s and 1s
hr5$Attrition<- ifelse(hr5$Attrition=="yes",1,0)

#3:overtime
# No need for conversion, 1 = Overtime, 0 = No overtime

dummy_df<-cbind(dummy_BusinessTravel,
                dummy_Department,
                dummy_Education,
                dummy_EducationField,
                dummy_EnvironmentSatisfaction,
                dummy_JobInvolvement,
                dummy_JobLevel,
                dummy_JobRole,
                dummy_JobSatisfaction,
                dummy_MaritalStatus,
                dummy_PerformanceRating,
                dummy_StockOptionLevel,
                dummy_WorkLifeBalance)

dummy_df<-cbind(dummy_df,hr5$Gender,hr5$Attrition,hr5$overtime)
colnames(dummy_df)[44:46]<-c("Gender","Attrition","overtime")

hr_final<-cbind(hr5$EmployeeID,hr5$Age,
                hr5$DistanceFromHome,
                hr5$MonthlyIncome,
                hr5$NumCompaniesWorked,
                hr5$PercentSalaryHike,
                hr5$TotalWorkingYears,
                hr5$TrainingTimesLastYear,
                hr5$YearsAtCompany,
                hr5$YearsSinceLastPromotion,
                hr5$YearsWithCurrManager,
                hr5$totaltime,
                hr5$leaves,
                hr5$workingdays,
                hr5$average_minutes_per_day,
                dummy_df)

colnames(hr_final)[1:15]<-c("EmployeeID","Age","DistanceFromHome",            
                            "MonthlyIncome","NumCompaniesWorked","PercentSalaryHike",           
                            "TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany",              
                            "YearsSinceLastPromotion","YearsWithCurrManager","totaltime",                   
                            "leaves","workingdays","average_minutes_per_day")

str(hr_final)

ifelse(is.character(hr_final)==F,"Go Ahead with Model Preparation","Recheck Data Cleaning")

#--------------------------FINAL DATAFRAME FOR MODEL PREPARATION IS NOW READY------------------------#

#--------------------------------Conducting Logistic Regression on Analysis_df----------------------------#


set.seed(100)
trainindices= sample(1:nrow(hr_final), 0.7*nrow(hr_final))
train = hr_final[trainindices,]
test = hr_final[-trainindices,]

# Creating the base model
model_1<-glm(Attrition~.,data = train[,-1],family = "binomial")
summary(model_1)

model_2<- stepAIC(model_1, direction="both")
summary(model_2)
x<-model_2
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))


# Eliminating Eliminating MaritalStatusmarried for VIF = 2.12 and high p-value = 0.155
# Among all insignificant variables this has highest VIF so we are eliminating this

model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + Education3 + Education4 + Education5 + 
               EducationFieldother + EducationFieldtechnical.degree + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobLevel5 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + 
               StockOptionLevel1 + WorkLifeBalance2 + WorkLifeBalance3 + 
               WorkLifeBalance4 + overtime, family = "binomial", data = train[,-1])

x<-model_3
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))


# Eliminating Education4 for VIF=1.37 and p-value=0.07
# Among all insignificant variables this has highest VIF so we are eliminating this

model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + Education3 + Education5 + 
               EducationFieldother + EducationFieldtechnical.degree + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobLevel5 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + StockOptionLevel1 + 
               WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
               overtime, family = "binomial", data = train[, -1])

x<-model_4
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobLevel5 for VIF=1.078 and p-value=0.071
# Among all insignificant variables this has highest VIF so we are eliminating this

model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + Education3 + Education5 + EducationFieldother + 
               EducationFieldtechnical.degree + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + StockOptionLevel1 + 
               WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
               overtime, family = "binomial", data = train[, -1])

x<-model_5
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))


# Eliminating Education5 for VIF= 1.059 and p-value=0.23
# Among all insignificant variables this has highest VIF so we are eliminating this

model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + Education3 + EducationFieldother + 
               EducationFieldtechnical.degree + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + StockOptionLevel1 + 
               WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
               overtime, family = "binomial", data = train[, -1])
x<-model_6
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))


# Eliminating EducationFieldtechnical.degree for VIF=1.04 and p-value =0.08
# Among all insignificant variables this has highest VIF so we are eliminating this

model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + EducationFieldother + 
               Education3 + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + StockOptionLevel1 + 
               WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
               overtime, family = "binomial", data = train[, -1])
x<-model_7
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating Education3 for VIF=1.02 and p-value=0.44
# Among all insignificant variables this has highest VIF so we are eliminating this

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + EducationFieldother + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + StockOptionLevel1 + 
               WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
               overtime, family = "binomial", data = train[, -1])
x<-model_8
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating StockOptionLevel1 for highest p-value 0.14, while all others have less than 0.05 p-value

model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTraveltravel_frequently + 
               BusinessTraveltravel_rarely + Departmentresearch...development + 
               Departmentsales + EducationFieldother + EnvironmentSatisfaction2 + 
               EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
               JobLevel2 + JobRolemanufacturing.director + JobRoleresearch.director + 
               JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
               JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
               overtime, family = "binomial", data = train[, -1])
x<-model_9
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating EducationFieldother for highest p-value 0f 0.051

model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
                JobLevel2 + JobRolemanufacturing.director + JobRoleresearch.director + 
                JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_10
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobRoleresearch.director for highest p-value = 0.044

model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
                JobLevel2 + JobRolemanufacturing.director + 
                JobRolesales.executive + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
  
x<-model_11
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobRolesales.executive for high p-value of 0.081

model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
                JobLevel2 + JobRolemanufacturing.director + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_12
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating PercentSalaryHike for p-value=0.068, rest all are significant

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobInvolvement3 + 
                JobLevel2 + JobRolemanufacturing.director + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_13
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobInvolvement3 for highest p-value = 0.028

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobLevel2 + JobRolemanufacturing.director + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_14
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))


# Eliminating JobLevel2 for highest p-value of 0.017

model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobRolemanufacturing.director + JobSatisfaction2 + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_15
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobSatisfaction2 for highest p-value = 0.01 among all the variables

model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobRolemanufacturing.director + JobSatisfaction3 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_16
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobSatisfaction3 for p-value = 0.021

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobRolemanufacturing.director + JobSatisfaction4 + MaritalStatussingle + 
                WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])
x<-model_17
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating JobRolemanufacturing.director for highest among all p-value = 0.008

model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 + MaritalStatussingle + 
                WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])
x<-model_18
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating TrainingTimesLastYear for p-value = 0.006

model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                BusinessTraveltravel_rarely + Departmentresearch...development + 
                Departmentsales + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 + MaritalStatussingle + 
                WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])
x<-model_19
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating BusinessTraveltravel_rarely for p-value = 0.0045

model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                Departmentresearch...development + Departmentsales + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction4 + MaritalStatussingle + 
                WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])
x<-model_20
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))


# Eliminating Departmentsales for highest p-value now of 0.00157

model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                Departmentresearch...development + EnvironmentSatisfaction2 + 
                EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction4 + MaritalStatussingle + 
                WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])

x<-model_21
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating Departmentresearch...development for p-value=0.016

model_22<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 + MaritalStatussingle + 
                WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])

x<-model_22
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Now All variables are *** so, however, the number of variables is still high and we shall further reduce that

# Eliminating Age for highest p-value = 1.913457e-04

model_23<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + 
                WorkLifeBalance4 + overtime, family = "binomial", data = train[, -1])

x<-model_23
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating WorkLifeBalance4 for highest p-value of 8.326471e-05

model_24<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance2 + WorkLifeBalance3 + 
                overtime, family = "binomial", data = train[, -1])

x<-model_24
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

# Eliminating Worklifebalance2 for highest p-value = 6.561460e-02

model_25<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTraveltravel_frequently + 
                EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                JobSatisfaction4 + MaritalStatussingle + WorkLifeBalance3 + 
                overtime, family = "binomial", data = train[, -1])
x<-model_25
summary(x)
f1 <- data.frame(summary(x)$coef[,"Pr(>|z|)"][-1])
f2 <- data.frame(vif(x))
View(cbind( f1, f2))

final_model<-model_25

# All the variables have *** values i.e. All are significant
# Thus, we can abort further parameter reduction process 
# and build the model on these parameters only

#-----------------------------Model Formulation is now over--------------------------#

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = test[,-1])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

test_pred_Attrition <- factor(ifelse(test_pred >= mean(test_pred), "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attrition,test_pred_Attrition)

# Let's use the probability cutoff of 3rd quartile 

test_pred_Attrition <- factor(ifelse(test_pred >= quantile( test_pred, 0.75), "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")

test_conf

# Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff)
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  u = union(predicted_Attrition, test_actual_Attrition)
  t = table(factor(predicted_Attrition, u), factor(test_actual_Attrition, u))
  conf <- confusionMatrix(t, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# ---- check from here


# Summary of test probability

summary(test_pred)

s = seq(0.0004612,0.85540 ,length=100)

OUT = matrix(0,100,3)

for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
} 



OUT <- as.data.frame(OUT)
OUT2 <- cbind(1:100, OUT)
colnames(OUT2) <- c("sno","sensitivity", "specificity", "accuracy")

ggplot( OUT2, aes(x=OUT2$sno) )+ geom_line(aes(x=OUT2$sno, y = OUT2$sensitivity, color="red")) + 
  geom_line(aes(x=OUT2$sno, y = OUT2$specificity, color="green")) + 
  geom_line( aes(x=OUT2$sno, y = OUT2$accuracy, color="blue")) + 
  xlab("Iterations") + ylab("Value")
 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# Let's choose a cutoff value of 0.1730 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.173, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)

ggplot(Attrition_decile,aes(x=factor(Attrition_decile$bucket),y=Attrition_decile$Gain,fill="red"))+geom_bar(stat = "identity")

ggplot(Attrition_decile,aes(x=factor(Attrition_decile$bucket),y=Attrition_decile$Cumlift,fill="red"))+geom_bar(stat = "identity")

#----------------------------------------END OF CODE--------------------------------------------#

# Conclusion: Till 4th Decile, almost 80% of the entire data of attrition is covered, which is a good indicator of Gain
# This ensures the effectivity of the process that by targeting just 40% population, HR manager can contain
# almost 80% of the total attrition