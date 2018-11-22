library(tree)
library(randomForest)
library(cvTools)
library(dplyr)

setwd('/home/thileepan/Dropbox/Edvancer/CDAP/Projects/real estate') #Linux

re_train = read.csv('housing_train.csv', stringsAsFactors = FALSE)
re_test = read.csv('housing_test.csv', stringsAsFactors = FALSE)


re_test$Price = NA
#re_test$Interest.Rate = NULL

re_train$data = 'train'
re_test$data = 'test'

re_all = rbind(re_train, re_test)

glimpse(re_all)

##Printing the number of categories in each categorical column in a dataframe

PrintCategories = function(dataframe, pred_var) {
  for (i in 1:(ncol(dataframe)-1)){
    if (class(dataframe[,i]) == 'character'){
      if(names(dataframe)[i] != pred_var){
        print(paste('Number of categories in', colnames(dataframe)[i] , ':' , n_distinct(unique(dataframe[,i]))))
        ##In the previous line, unique returns a vector, data frame or array like x with duplicate elements/rows removed.
      }
    }
  }
}

PrintCategories(re_all, 'Price')

table(re_all[,"Type"])
table(re_all[,"Method"])
table(re_all[, "CouncilArea"])
table(re_all[, "Postcode"])
## Function for creating dummies

CreateDummies = function(data, var, freq_cutoff=0){
  t=table(data[,var])
  t = t[t>freq_cutoff]
  t = sort(t)
  categories = names(t)[-1] # edited this part for the function to not omit the least occurring categorical variable
  
  for (cat in categories) {
    name = paste(var,cat, sep='_')
    name = gsub(" ", "", name)
    name = gsub("-", "_", name)
    name = gsub("\\?", "Q", name)
    name = gsub("<", "LT_", name)
    name = gsub("\\+", "", name)
    
    data[, name] = as.numeric(data[,var]==cat)
  }
  
  data[,var] = NULL
  return(data)
}

mymode=function(x){
  t=table(x)
  result=names(t)[which(t==max(t))]
  return(result)
}
#columns to create dummy variables for
char_columns = sapply(re_all, is.character)
cat_cols = names(re_all)[char_columns]
#cat_cols = cat_cols[!(cat_cols %in% c('data', 'Suburb', 'Address', 'SellerG'))]
cat_cols = cat_cols[!(cat_cols %in% c('data', 'Address'))]


for (col in cat_cols){
  re_all = CreateDummies(re_all, col, 100)
}

# re_all = CreateDummies(re_all, 'Type', 1500)
# re_all = CreateDummies(re_all, 'Method', 100) #*
# re_all = CreateDummies(re_all, 'SellerG', 500)
# re_all = CreateDummies(re_all, 'Suburb', 150) #*
# re_all = CreateDummies(re_all, 'CouncilArea', 200 )

#re_all$Address = NULL
re_all$Postcode = NULL

glimpse(re_all)

#Dropping all columns with nan values for Price column in train data

re_all = re_all [! ((is.na(re_all$Price)) & re_all$data == 'Train'),]
dim(re_all)

#Replacing NA values with mean

for(col in names(re_all)){
  if(sum(is.na(re_all[,col]))>0 & !(col %in% c('data', 'Price'))){
    re_all[is.na(re_all[,col]), col] = mymode(re_all[re_all$data == 'train', col])
    #re_all[is.na(re_all[,col]), col] = mymode(re_all[re_all$data == 'train', col])
  }
}

##The dataframe still has 1885 NA values that's because all the values are in test dataset. 
##We introduced them.
sum(is.na(re_all[re_all$data == 'test', "Price"])) 

#Seperating Train and test data

re_train = re_all %>%
  filter(data == 'train') %>%
  #select(-data,-Suburb ,-Address, -SellerG)
  select(-data ,-Address)

re_test = re_all %>%
  filter(data == 'test') %>%
  #select(-data,-Suburb,-Address,-SellerG, -Price)
  select(-data,-Address, -Price)


#Seperating training and CV data

# set.seed(2)
# s = sample(1:nrow(re_train), 0.7*nrow(re_train))
# re_train1 = re_train[s,]
# re_train2 = re_train[-s,]
# 
# glimpse(re_train1)
# glimpse(re_train2)

# param=list(mtry=c(20,25),
#            ntree=c(100,200),
#            maxnodes=c(10,15),
#            nodesize=c(5,10))

# param=list(mtry=c(5,10,15,20,25,35),
#            ntree=c(150,200,250,300, 350),
#            maxnodes=c(15,20,30,40,45),
#            nodesize=c(5,10)
# )

# param=list(mtry=c(20,25,35,40),
#            ntree=c(150,200,250,300,350),
#            maxnodes=c(30,40,45,50,55),
#            nodesize=c(10,15)
# )

# param=list(mtry=c(20,25,35,40),
#            ntree=c(250,300,350,400,450),
#            maxnodes=c(55,60,65,70,75),
#            nodesize=c(10,15,20)
# )

# param=list(mtry=c(15,20,25,35),
#            ntree=c(250,300,350,400,450),
#            maxnodes=c(55,60,65,70,75,80),
#            nodesize=c(10,15,20)
# )

param=list(mtry=c(15,20,25,35,50,60,70,80),
           ntree=c(250,300,350,400,450,500),
           maxnodes=c(45,55,60,65,70,75,80),
           nodesize=c(10,15,20,30,40,50,60)
)

expand.grid(param)


subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=200
my_params=subset_paras(param,num_trials)

myerror=9999999

for(i in 1:num_trials){
   print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest, Price~.,
             data =re_train,
             tuning =params,
             folds = cvFolds(nrow(re_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  #print(k)
  
  if(score.this<myerror){
     print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
     print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
   print('DONE')
  # uncomment the line above to keep track of progress
}

re.rf.final = randomForest(Price~., 
                           mtry = best_params$mtry,
                           ntree = best_params$ntree,
                           maxnodes = best_params$maxnodes,
                           nodesize = best_params$nodesize,
                           data = re_train)

price.pred.train = predict(re.rf.final, newdata = re_train)
rmse_val = ((price.pred.train)-(re_train$Price))^2 %>% mean()%>% sqrt()
rmse_val
212467/rmse_val

price.pred.test = predict(re.rf.final, newdata = re_test)
#rmse_val = ((price.pred.val)-(re_test$Price))^2 %>% mean()%>% sqrt()

d = importance(re.rf.final)
d = as.data.frame(d)
d$VariableName = rownames(d)
d %>%
  arrange(desc(IncNodePurity))

varImpPlot(re.rf.final)

re.rf.final

write.table(price.pred.test, 'all_variables_mean_high_mtry.csv', row.names = FALSE, col.names = c('Price'))

