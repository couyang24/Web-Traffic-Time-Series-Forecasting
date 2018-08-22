library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggrepel') # visualisation
library('RColorBrewer') # visualisation
library('data.table') # data manipulation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('stringr') # string manipulation
library('purrr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('forecast') # time series analysis
library('prophet') # time series analysis


# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




train <- as.tibble(fread('input/train_1.csv'))

key <- as.tibble(fread('input/key_1.csv', nrows = 5))


key %>% head()
train %>% head()



c(ncol(train),nrow(train))
dim(train)


train %>% colnames() %>% head(5)

glimpse(key)

sum(is.na(train))/(ncol(train)*nrow(train))

tdates <- train %>% select(-Page)



foo <- train %>% select(Page) %>% rownames_to_column()

mediawiki <- foo %>% filter(str_detect(Page, "mediawiki"))

wikimedia <- foo %>% filter(str_detect(Page, "wikimedia"))

wikipedia <- foo %>% filter(str_detect(Page, "wikipedia")) %>% 
  
  filter(!str_detect(Page, "wikimedia")) %>%
  
  filter(!str_detect(Page, "mediawiki"))



wikipedia <- wikipedia %>%
  
  separate(Page, into = c("foo", "bar"), sep = ".wikipedia.org_") %>%
  
  separate(foo, into = c("article", "locale"), sep = -3) %>%
  
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  
  mutate(locale = str_sub(locale,2,3))



wikimedia <- wikimedia %>%
  
  separate(Page, into = c("article", "bar"), sep = "_commons.wikimedia.org_") %>%
  
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  
  add_column(locale = "wikmed")



mediawiki <- mediawiki %>%
  
  separate(Page, into = c("article", "bar"), sep = "_www.mediawiki.org_") %>%
  
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  
  add_column(locale = "medwik")



tpages <- wikipedia %>%
  
  full_join(wikimedia, by = c("rowname", "article", "locale", "access", "agent")) %>%
  
  full_join(mediawiki, by = c("rowname", "article", "locale", "access", "agent"))



sample_n(tpages, size = 5)






