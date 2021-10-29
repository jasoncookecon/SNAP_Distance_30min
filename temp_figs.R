
library(timevis)

data <- data.frame(
  id      = 1:17,
  state = c("AZ"  ,"CO"  , "HI" , "ID" , "IL" , "IN" , "KY" , "MD" , "MI" , "MS" , "ND" , "NJ" , "NY" , "OR" , "TN" , "UT" , "VA"),
  data_start   = c("2009","2012","2013","2010","2008","2004","2014","2006","2010","2010","2004","2006","2013","2009","2004","2012","2009"),
  data_end     = c("2018","2013","2018","2018","2016","2018","2018","2016","2016","2018","2018","2018","2018","2018","2018","2018","2013")
)

data_long <- data %>% pivot_longer(cols = starts_with("data_"),
                        names_to = "cat",
                        names_prefix = "data_"
)  %>% arrange(desc(state), value)

library(tidyverse)
theme_set(theme_bw())





factor_var <- factor(data_long$state, 
                     order = TRUE,
                     levels =c("VA","UT","TN","OR","NY","NJ","ND","MS","MI","MD","KY","IN","IL","ID","HI","CO","AZ"))

data_long <- data_long %>%
                mutate(state = factor_var)

data_long %>% 
  ggplot(aes(x= value, y= state)) +
  geom_line(aes(group = id),color="grey")+
  geom_point(aes(color=cat), size=3, show.legend = FALSE) +
  labs(y="State")+ labs(x="Year")+
  theme_classic(24)+
  scale_color_brewer(palette="Accent", direction=-1)



library(RColorBrewer)
display.brewer.pal(n = 4, name = 'RdBu')
brewer.pal(n = 4, name = 'RdBu')

library(kableExtra)
tbl_img <- data.frame(
  est = c("35*** (15)", "36* (19)"),
  ES = ""
)
tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(2, image = spec_image(
    c("cl_rur_nclcas1monchunew1_twfenov_s_ES.png", "op_urb_nsnasto1_twfenov_s_ES.png"), 50, 50))

tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(2, image = "cl_rur_nclcas1monchunew1_twfenov_s_ES.png")

tbl_img <- data.frame(
  name = c("kableExtra 1", "kableExtra 2"),
  logo = ""
)
tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(2, image = "es1")

tbl_img %>%
  mutate(images = paste0("<","img src=","figs/cl_rur_nclcas1monchunew1_twfenov_s_ES",".png",">")) %>%
  kable(format = "html", escape = F) %>%
  kable_styling(bootstrap_options = "striped")



library(knitr)
library(kableExtra)
library(tidyverse)

#df<-read.csv('Productdata.csv')
df<-data.frame(Amount= c('$25', '$45', '$75'), 
               Rate = c('1%', '1%', '3%'), 
               Location = c('Germany', 'Switzerland', 'England'),
               ImageName= c('GE.png', 'BE.png', 'CE.png'),
               Status = c('Sold','Unsold','Sold')
)

# Change to names of my local images
df$ImageName =c("mal2.jpg",
                "serenity2.jpg",
                "figs/cl_rur_nclcas1monchunew1_twfenov_s_ES.png")

# Add appropriate rmarkdown tagging
df$ImageName = sprintf("![](%s)", df$ImageName)

for (i in 1:nrow(df)) {
  
  # Select desired row
  d = df[i, ]
  
  # Change name of ImageName column to Status value
  names(d)[grep("ImageName", names(d))] = as.character(d[["Status"]])
  
  # Convert data to "long" format
  d = d %>% 
    select(-Status) %>% 
    gather(Product, value, Amount:Location) %>% 
    rename(` ` = value)
  
  # Render table using kableExtra for formatting
  print(kable(d, format="html") %>% 
          kable_styling(full_width=FALSE) %>% 
          collapse_rows(columns=1, valign="top"))
}


d<-data.frame(Type= c('Distance','Model','Avg. Estimate','Baseline Y','Event Study'), 
              cl_rur = c('<1','TWFE','5.63*** <br/> (1.16)', '.56','
![:scale 100%](figs/cl_rur_meaoffdismaf1_twfenov_s_ES_noaxis.png)'),
              cl_urb = c('<1','TWFE','5.63*** <br/> (1.16)', '.56','
![:scale 100%](figs/cl_urb_meaoffdismaf1_twfenov_s_ES_noaxis.png)'),
              op_rur = c('<1','TWFE','5.63*** <br/> (1.16)', '.56','
![:scale 100%](figs/op_rur_meaoffdismaf1_twfenov_s_ES_noaxis.png)'),
              op_urb = c('<1','TWFE','5.63*** <br/> (1.16)', '.56','
![:scale 100%](figs/op_urb_meaoffdismaf1_twfenov_s_ES_noaxis.png)')
)

d %>%
  kbl(booktabs = TRUE,col.names = c("Type","Closing<br/>Rural","Closing<br/>Urban","Opening<br/>Rural","Opening<br/>Urban"), escape = FALSE,
      align=c("l", 'c','c','c','c'), position="center"
  ) %>%
  kable_paper(full_width = FALSE) %>%
  kable_styling(full_width = FALSE, position="center") %>% 
  row_spec(0, align = "c") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "5em")

```{r , include=T, echo = F}
library(htmltools)
data <- data.frame(
  Label =c("Distance","Model","Avg. Estimate","Baseline Y"),
  cl_rur_nclnew1_twfenov_s_ES_noaxis = c("<1","TWFE","-3.17 (12.6)","224"),
  cl_urb_nclnew1_twfenov_s_ES_noaxis = c("<1","TWFE","-88.0 (62.5)","1,020"),
  op_rur_nclnew1_twfenov_s_ES_noaxis = c("<1","TWFE","35.9*** (11.9)","153"),
  op_urb_nclnew1_twfenov_s_ES_noaxis = c("<1","TWFE","294.2*** (60.7)","466")
)

reactable(data, 
          sortable = FALSE,
          #Default attributes applied to all columns
          defaultColDef = colDef(header = function(value) 
            gsub(".", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            #horizontal alignment
            align = "center",
            minWidth = 130,
            headerStyle = list(background = "rgba(0, 0, 0, 0.03)"),
            #verticle alignment
            vAlign = "center", 
            headerVAlign = "bottom",
            #Picture in footer matches the name of the column variable
            footer = function(value, name) {
              if (is.numeric(value) || name=="Label") return()
              image <- img(src = sprintf("figs/%s.png", name), height = "65px")
              tagList(
                div(style = list(display = "inline-block", width = "130px"), image)
              )
            }
          ),
          #Define Column-specific attributes
          columns = list(
            Label = colDef(name = "", align = "center"),
            cl_rur_nclnew1_twfenov_s_ES_noaxis = colDef(name = "Closing Rural", align = "center", style = list(background = "#FDDBC7")),
            cl_urb_nclnew1_twfenov_s_ES_noaxis = colDef(name = "Closing Urban", align = "center", style = list(background = "#D1E5F0")),
            op_rur_nclnew1_twfenov_s_ES_noaxis  = colDef(name = "Opening Rural", align = "center", style = list(background = "#FDDBC7")),
            op_urb_nclnew1_twfenov_s_ES_noaxis = colDef(name = "Opening Urban", align = "center", style = list(background = "#D1E5F0"))
          ),
          #Misc Table Options
          bordered = TRUE,
          highlight = TRUE,
          fullWidth = FALSE,
          theme = reactableTheme(
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            cellPadding = "8px 12px",
            style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
          )
)

```
