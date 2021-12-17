library(tidyverse)
library(plotly)
library(cowplot)

exp <- Fr[[10]]
a <- exp[101500:102500, ]#exp[60000:140000, ]
p <- ggplot(a, aes(x = t, y = Fr)) + geom_line()
p
q <- plot_ly(a, x = ~t, y = ~Fr)%>% add_lines()
q

#Exp 10
#start_entry - 80000
#end_entry   - 140000
#start_exit  - 777900          
#end_exit    - 870000


#Exp9
#start_entry - 90000
#end_entry   - 140000
#start_exit  - 960000          
#end_exit    - 1080000


#Exp8
#start_entry - 40000
#end_entry   - 90000
#start_exit  - 478800          
#end_exit    - 550000


#Exp7
#start_entry - 95000
#end_entry   - 140000
#start_exit  - 1501000         
#end_exit    - 1600000

#Exp6
#start_entry - 100000
#end_entry   - 130000
#start_exit  - 975000         
#end_exit    - 1100000


#Exp5
#start_entry - 80000
#end_entry   - 125000
#start_exit  - 765000       
#end_exit    - 860000

#Exp4
#start_entry - 1
#end_entry   - 25000
#start_exit  - 665000      
#end_exit    - 762500


#Exp3
#start_entry - 55000 
#end_entry   - 90000
#start_exit  - 490000      
#end_exit    - 552500


#Exp2
#start_entry - 20000 
#end_entry   - 50000
#start_exit  - 718000    
#end_exit    - 800000

# Exp1
# The plot looks almost filled. We need to
# investigate the beginning and edges to 
# understand the issue

#start_entry - 1
#end_entry   - 1,50,000
#start_exit  - 13,80,000 
#end_exit    - 15,50,000