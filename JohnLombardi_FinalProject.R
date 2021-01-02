rm(list=ls())
graphics.off()
path_to_data          = ('/Users/JohnLombardi/Desktop')
setwd(path_to_data)

install.packages("ggplot2")
library(ggplot2)

#1
  # I expect price to be negative because people do not want to pay a lot. 
  #Advertising exposure will be positive because if people see the ads more they will be more likely to buy. 
  #Quality is positive because if people perceive it to be high quality they are more likely to buy, but it depends on the consumer preference
  #gender is N/A for this question

#2
  dta = read.csv("/Users/JohnLombardi/Desktop/shopping_data_2020_11_25.csv")
  
  nrow(dta)
  # 10,000 observations
  
  ncol(dta)
  # 9 variables
      #the X variable can be used to refer to specific customer id

  
  #i)
        #using for loop
      for (i in 0:1) {
        female_average=mean(dta[dta$gender==i,2])
        male_average=mean(dta[dta$gender!=i,2])
      }
      print(paste(male_average,female_average))
        #make barplot
      barplot_1=barplot(c(male_average,female_average),ylim = c(0,12),names.arg = c('male','female'),xlab='Gender',ylab='Price')
      text(barplot_1,as.character(c(male_average,female_average)))
 
   #ii)    
        #using tapply
      tapply(X = dta$price, INDEX = dta$gender, FUN = mean)
        #change to matrix
      average_matrix=matrix(tapply(X = dta$price, INDEX = dta$gender, FUN = mean))
        #make bar plot with text
      bplot=barplot(c(average_matrix[1,1],average_matrix[2,1]),ylim = c(0,12),names.arg = c('male','female'),xlab='Gender',ylab='Price')
        text(bplot,as.character(average_matrix))
   
   #)  
        #ggplot scatter plot 
      ggplot(data=dta,aes(quality,price))+
        geom_point(color='pink',shape=23,position=position_jitter(),aes(alpha=0.25))+
        theme(text=element_text(family="Comic Sans MS"),panel.background = element_rect('black'),panel.grid.major = element_line(color = 'purple'),panel.grid.minor = element_line(color = 'purple'),plot.background = element_rect(fill  ='pink'),legend.position="none")

#3
  cor(dta)
  
  #)
    #The variable that is most correlated with quantity is price. 
    #i) This could be the case because if the product costs more, people are less likely to by a lot. The correlation is negative in value. The consumers are price sensitive.
  #) Yes, this correlation matrix is symmetric. 
  #) The most important Xs to explain what causes a consumer to buy more quantity of a product are x_3, then x_1, then x_2. That is in order of highest absolute correlation value.
  
  #)Optional
      #1) check if correlation matrix is symmetric
      mat=cor(dta)
      is_symmetric <- function(mat){
        ans = TRUE
        for(i in 1:nrow(mat)){
          for(j in 1:ncol(mat)){
            if(mat[i,j]!=mat[j,i]){
              ans = FALSE;break}
          }}
        return(ans)}
      is_symmetric(mat)
      
      #2)
                           
      corr_matrix_by_hand=function(dta){
      x=1
      y=1
      n=10000
      a=dta[,x]
      b=dta[,y]
      cor_equation=((n*sum(a*b)) - sum(a)*sum(b)) / sqrt(((n*sum(a^2)) - (sum(a)^2)) * ((n*sum(b^2))-(sum(b)^2)))
      result=vector("list",81)
      for (i in 1:81) {  
        a=dta[,x]
        b=dta[,y]
        print(paste(((n*sum(a*b)) - sum(a)*sum(b)) / sqrt(((n*sum(a^2)) - (sum(a)^2)) * ((n*sum(b^2))-(sum(b)^2)))))
        result[[i]]=((n*sum(a*b)) - sum(a)*sum(b)) / sqrt(((n*sum(a^2)) - (sum(a)^2)) * ((n*sum(b^2))-(sum(b)^2)))
        x=x+1
        if(x==10){
          x=1
         if(x==1){
           y=y+1
         } 
        }
      }
      #Produces some NAs --- to be seen as '1' or a correlation with Gender and X
      u=unlist(result)
      matrix(u,nrow = 9,ncol = 9)
      }
      
      corr_matrix_by_hand(dta)
      
cor(dta)
                    
#4 
 #i)
   r1=lm(formula=quantity~price,data=dta)
  summary(r1) 
  
  #quantity = 9.10515 + -0.18564price + ϵ
  
  r2=lm(formula=quantity~price+advertising,data=dta)
  summary(r2)
  #quantity = 9.166311 + -0.206743price + 0.063732advertising + ϵ
  
  r3=lm(formula=quantity~price+advertising+quality,data=dta)
  summary(r3)  
  #quantity = 8.86229 + -0.19625price + -0.08734advertising + 0.20732quality + ϵ  
  
  r4=lm(formula=quantity~price+advertising+quality+gender,data=dta)
  summary(r4)    
  #quantity = 8.78499 + -0.19630price + -0.08743advertising + 0.20791quality + 0.10764gender + ϵ 
  
 #ii)
  # I would take regression three because it has the highest adjusted R-squared value -  quantity = 8.86229 + -0.19625price + -0.08734advertising + 0.20732quality + ϵ
    
 #iii)
   # you can determine which regression fits the data better by which has the higher Adjusted R-squared value. Can also look at the plots.
    # Regression 3 fits the data the best with adjusted R-squared value of 0.04767
    plot(r1)
    plot(r2)
    plot(r3)
    plot(r4)
  
  #iv)
    reg_barplot=barplot(c(summary(r1)$adj.r.squared,summary(r2)$adj.r.squared,summary(r3)$adj.r.squared,summary(r4)$adj.r.squared),ylim = c(0,0.055),names.arg = c('r1','r2','r3','r4'),xlab='Regressions',ylab='Adjusted R-squared Vales')
    text(reg_barplot,as.character(c(summary(r1)$adj.r.squared,summary(r2)$adj.r.squared,summary(r3)$adj.r.squared,summary(r4)$adj.r.squared)))
    
  #v)
    #Based on Regression 4, I would recommend a decrease in price, since price is negatively correlated with quantity. And advertising is also negatively correlated with quantity, so you would not want to increase it.
    
  #vi)
    cor(dta)
    r5=lm(formula=quantity~price+advertising+quality+x_1,data=dta)
    r6=lm(formula=quantity~price+advertising+quality+x_1+x_2,data=dta)
    r7=lm(formula=quantity~price+advertising+quality+x_1+x_2+x_3,data=dta)
    # The X-named variables are relatively highly correlated with quantity, and are statistically significant in the regression. So it could be important to include them. However, since their true identities are unknown, it would not make sense to include them in the regression since we can't make a meaningful interpretation / analysis from that.
    
# Market Basket Analysis
  
  #1) Market Basket Analysis looks for items that are frequently purchased together. It shows relationships between purchased items and identifies patterns. Market basket analysis is important because it helps companies with store/website layout, pricing, communication/promotion design, cross marketing, find trending items, customized targeted advertising, and customer behavior. It is difficult to perform because it is complicated and requires multiple steps with some tricky math involved. Hard to analyze things that happen rarely.
      # A rule is essentially and if then model saying if item A is purchased, then item B is likely to be purchased. It is an itemlist of items purchased together. For example could be a customer that has transaction containing apples are likely to be interested in buying bannanas as well.
      # The support is the percentage of all the items in the itemset being in the transaction. 
      # The confidence is probability that a transaction contains item A also contains item B. It's the conditional probability. 
      # The lift of the rule is the ratio of the support of A co-occurring with B, divided by the probability that A and B co-occur if they are independent
      # To perform market basket analysis and find rules, apriori algorithm is used. This identifies itemsets bundles that occur with a specified support and confidence level. Then finding rules and calculate confidence of all rules
  
  #2)
    #Market basket Analysis code with comments
    # 1. Importing libraries                   # #####
   
    library(data.table) #Loads data.table package
    library(arules)     #Loads arules package
    library(arulesViz)  #loads arulesViz package
    library(lubridate)  #loads lubridate package
    library(ggplot2)    #loads ggplot2 package
    library(knitr)      #loads knitr package
    library(plyr)       #loads plyr package
    library(readxl)     #loads readxl package
    library(tidyverse)  #loads tidyverse package
    library(RColorBrewer) #loads RColorBrewer package
    
    
    path_to_data          = ('/Users/JohnLombardi/Desktop')  #sets the data path to my desktop
    setwd(path_to_data)  #sets the working directory to my desktop
    
    
    # 2. Importing and cleaning the data       # #####
    # complete.cases(data) returns a logical vector indicating which rows have no missing values.
    # Then use the vector to get only rows that are complete using retail[,].
    # mutate function is from dplyr package. It is used to edit or add new columns to dataframe. Here Description column is being converted to factor column. as.factor converts column to factor column. %>% is an operator with which you may pipe values to another function or expression
    # Converting character data to date. Store InvoiceDate as date in new variable
    # Extract time from InvoiceDate and store in another variable
    # Convert and edit InvoiceNo into numeric
    # set column InvoiceNo of dataframe transactionData  
    # set column Date of dataframe transactionData
    # Rename column to items
    # Show Dataframe transactionData
    
    retail                = as.data.table(read_excel('/Users/JohnLombardi/Desktop/Online_Retail.xlsx')) #import the data excel file from my desktop as a datatable and saves it as variable called retail
    retail                = retail[complete.cases(retail), ]  #throw away incomplete data by saving only the complete data rows using complete.cases function
    retail$Date           = as.Date(stringr::str_extract(string  = retail$InvoiceDate,pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")) #changes the year month and day to be formatted as a date recognizable by R using as.date function. Did this by extracting the string of 4 numbers 0-9, dash then 2 numbers 0-9, dash then 2 numbers 0-9
    retail$hour           = stringr::str_extract(string  = retail$InvoiceDate,pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2}") # does the same as above but for time. changes the hour minute second timestamp to be recognizable by R using string extract and as.date
    
    InvoiceNo             = retail$InvoiceNo #sets invoice number to variable InvoiceNo
    
    
        #This was in class exercise
        table(format(x=retail$Date,'%A')) # Finds how many times each day of the week occurs in the data. uses format function and '%A' to change the dates in numerical form to days of the week. ie. 'monday'... Then table() function puts them in a table and counts how many times each one appeared
   
        retail$one=1
        tapply(retail$one,format(x=retail$Date,'%A'),sum) # does the same as above, except using tapply to select retail date then format as day of the week then apply sum function each one.
    
    
        #what percentage of shopping trips come from each country
        table(retail$Country) #counts how many observations from each country using table
        barplot(sort(round(100*(table(retail$Country)/nrow(retail)),3),decreasing =TRUE)) #creates barplot showing the percentage of shopping trips that come from each country in decending order
    
    
    #Bind new columns TransTime and InvoiceNo into dataframe retail
        #the curse of dimensionality. The matrix or dataframe you create for the mb analysis may be too big for your computer to handle.
        nrow=500000
        ncol=2000
        object.size(matrix(0,nrow,ncol))/1e9 #to check if you have enough memory.
        #sparse matrix does not store the zeros. Sparse Matrix is way to get around this problem of curse of dimensionality
    
    #code to create sparse matrix.
    transactionData           = ddply(retail,c("InvoiceNo","Date"),function(df1)paste(df1$Description,collapse = ","))
    retail$Date               = as.Date(retail$InvoiceDate)
    TransTime                 = format(retail$InvoiceDate,"%H:%M:%S")
    transactionData$InvoiceNo = NULL
    transactionData$Date      = NULL
    colnames(transactionData) = c("items")
    transactionData   #the data is stored here, but it is now easier to operate on
    
    write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
    tr = read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')
    
    # 3. Item Frequency Plot                   # #####
    
      #see the frequency of items bought
    graphics.off() #clears the plot from our viewer. 
    item_frequency  = as.data.frame(itemFrequency(x = tr,type="relative")) #percentages
    item_frequency$prod_name = rownames(item_frequency) #create frequency chart
    item_frequency  = as.data.frame(itemFrequency(x = tr,type="absolute"))#counts
    
    head(item_frequency)#percentage that item shows up in transaction. See head of frequency chart. product name with frequency they show up in shopping trips
    
    colnames(item_frequency)[1]  = "frequency"
    item_frequency  = as.data.table(item_frequency) #makes item frequency as a data table
    itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot") #creates plot of the number frequency of the items
    itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot") #creates plot of the percentage frequency of the items
    
    # 4. Rule generation                       # #####
      #apriori algorithm takes two parameters that are the minimum support and minimum confidence. Also the maximum number of items that are included in the bundle
    min_supp          = 0.003   #ignore items that appear in only 0.03% of transactions
    min_conf          = 0.8     #the conditional probability. How confident that you buy two items together. Minimun conditional probility is 80%. If you buy A then you will buy B
    max_len           = 10      #that maximum length only 10 items in bundle
    association.rules = apriori(data      = tr,
                                parameter = list(supp   = min_supp,
                                                 conf   = min_conf,
                                                 maxlen = max_len))     #Runs apriori given with the tr data and the specified parameters from above
    summary(association.rules) #shows summary of apriori
    
    inspect(association.rules[1:10]) #table that shows info. Left hand side and right hand side share same support but not same confidence
    
    shorter.association.rules = apriori(data      = tr,
                                        parameter = list(supp   = min_supp,
                                                         conf   = min_conf,
                                                         maxlen = 3))  #does same as above but with max length set to three isntead of 10
    
    summary(shorter.association.rules) #same as above but with new max length
    inspect(shorter.association.rules[1:10]) #same as above but with new max length
    
    
    # Removing redundant rules
    # You can remove rules that are subsets of larger rules. Use the code below to remove such rules:
    subset.rules = which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
    length(subset.rules)  #see length
    
    
    # 5. Rule Visualization                    # #####
    # Filter rules with confidence greater than 0.4 or 40%
    # Plot SubRules
    graphics.off() #clear plot from viewer
    subRules = association.rules[quality(association.rules)$confidence>0.4] #takes rules with confidence greater than 0.4
    plot(subRules) #plots the filtered rules
    plot(subRules,method="two-key plot") #makes a two-key plot with the filtered rules
    top10subRules = head(subRules, n = 10, by = "confidence") #shows the top 10 filtered rules ordered by confidence
    
    
    
#3)
    
    min_supp          = 0.008  
    min_conf          = 0.7   
    max_len           = 10      
    association.rules = apriori(data      = tr,
                                parameter = list(supp   = min_supp,
                                                 conf   = min_conf,
                                                 maxlen = max_len))     
    summary(association.rules)
    
    inspect(association.rules[1:47]) 

    
    #    
      #1. Left hand side is Toilet Metal Sign. Right hand side is Bathroom Metal Sign. Support is 0.008471903. Confidence is 0.7580645. And it showed up 188 times. I found this rule interesting because it seems like they would be replacements for eachother. Like you would buy one or the other, but not both. So it is surprising to see such high confidence level. Also why you would buy toilet then bathroom, but not other way around.
      #2. Left hand side is Sugar. Right hand side is Coffee. I am surprised that the confidence is 1. I would have thought that there are a lot more people buying sugar but not coffee, perhaps using sugar to bake, not just put in coffee. I am surprised confidence is so high.
      #3. Left hand side Set 3 Retrospot Tea. Right hand side Coffee. I am surprised this has confidence of 1. I would think that mostly people either drink tea or coffee. Maybe not both. So surprising to see such high confidence. Also lower confidence if you switch the sides. So maybe people view tea as more of a side drink.
  
  