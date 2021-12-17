library(plotly)

exp <- Fr[[8]]
a <- exp[37000:80000, ]
p <- ggplot(a, aes(x = index(Fr), y = Fr)) + geom_line()
p
q <- plot_ly(a, x = ~t, y = ~Fr)%>% add_lines()
q


#Exp1
#start_entry - 1
#end_entry   - 40,000
#start_exit  - 1,70,000 
#end_exit    - 2,10,000

#Exp2
#start_entry - 20,000 
#end_entry   - 62,000
#start_exit  - 1,97,500     
#end_exit    - 2,25,000

#Exp3
#start_entry - 1 
#end_entry   - 75,000
#start_exit  - 5,40,000      
#end_exit    - 6,25,000

#Exp4
#start_entry - 1
#end_entry   - 7,500
#start_exit  - 96,000      
#end_exit    - 1,15,000

#Exp5
#start_entry - 15,000
#end_entry   - 62,500
#start_exit  - 3,60,000       
#end_exit    - 4,25,000

#Exp6
#start_entry - 35,000
#end_entry   - 65,000
#start_exit  - 1,72,500         
#end_exit    - 1,95,000

#Exp7
#start_entry - 1
#end_entry   - 1,10,000
#start_exit  - 3,37,500        
#end_exit    - 3,95,000

#Exp8
#start_entry - 25,000 
#end_entry   - 57,000
#start_exit  - 1,64,000           
#end_exit    - 1,85,000

#Exp9
#start_entry - 25,000
#end_entry   - 72,500
#start_exit  - 2,43,000          
#end_exit    - 2,80,500

#Exp 10
#start_entry - 50,000
#end_entry   - 82,000
#start_exit  - 5,95,000          
#end_exit    - 6,80,000