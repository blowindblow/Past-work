# Libraries ---------------------------------------------------------------

library(tidyverse, quietly = T)
library(data.table, quietly = T)
library(forcats, quietly = T)
library(ggplot2, quietly = T)
library(ggrepel, quietly = T)
library(scales, quietly = T)
library(wesanderson, quietly = T)
library(gt, quietly = T)
library(gtsummary, quietly = T)  
library(flextable, quietly = T)  
library(ggsignif, quietly = T)  
library(FSA, quietly = T)
library(showtext)
library(ggtext)
library(multiCA)
library(DescTools)


# Wrap functions ----------------------------------------------------------

wrap7 <- wrap_format(7)
wrap8 <- wrap_format(8)
wrap9 <- wrap_format(9)
wrap10 <- wrap_format(10)
wrap12 <- wrap_format(12)
wrap15 <- wrap_format(15)
wrap18 <- wrap_format(18)
wrap20 <- wrap_format(20)
wrap50 <- wrap_format(50)
wrap30 <- wrap_format(30)


# ggplot theme related functions ------------------------------------------

geom_text_size <- function(x){
  x * 0.295
}
theme_font_size <- function(x = 10){
  ggplot2::theme(text = element_text(size = x))
}
theme_preset <- function(legend_position  = 'top', y = 0, x = 0, line_height = 1, caption_size = 8.5,panel_size=0.3){
  theme_minimal()+
  theme(panel.grid.major.y = element_line(colour = "grey", size=0.3),
        legend.position = legend_position,
        plot.caption = element_text(hjust = 0, lineheight = line_height, size = caption_size),
        panel.border = element_rect(colour = "black", fill=NA, size=panel_size),
        axis.title.y = element_text(margin = margin(t = 0, r = y, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = x, r = 0, b = 0, l = 0)))
}
theme_rank_legend_settings <-
  theme(legend.margin = margin(0,0,-2,0),
        legend.box.margin = margin(5, 6, -4, 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.4,'cm')) 

# ggplot: format scale_y_continuous to % and set max limit
y_scale <- function(max){
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max))
}

# ggplot: set size of title
title_size <- function(size) {theme(plot.title = element_text(size=size))}



# Stacked bar graph functions ---------------------------------------------
# stacked bar graphs for ranking for different groups + faceting into all the modes 
stacked_plot_grp <- function(df, group, annotation = NA, geomtext_size=3){
  r <<- df %>% 
    group_by({{ group }}, name) %>% 
    mutate(total=n()) %>% 
    group_by({{ group }}, name, value) %>% 
    summarise(percent=n()/total) %>% 
    distinct({{ group }}, name, value, .keep_all = T) %>% # only select distinct rows 
    mutate(value = as.factor(value))   
  
  r2 <<- df %>% 
    count({{group}}, name) %>% 
    distinct({{ group }}, n)
  
  r$name <<- factor(r$name)
  
  if (max(df$value) == 5) {
    label_names <- as_labeller(
      c(`rank.policy` = "Policy", `rank.elearning` = "eLearning",`rank.workshop` = "Workshop", 
        `rank.email` = "Email",`rank.audit` = "Audit")) 
    
    facet <- # order and label facets
      facet_grid(~factor(name, levels = c('rank.policy','rank.elearning','rank.workshop','rank.email','rank.audit')),
                 labeller = as_labeller(label_names)) 
    
    colours <- 
      scale_fill_manual(values = c("#3B9AB2", "#78B7C5","#EBCC2A", "#E1AF00", "#E86E00"))
    
  } else if (max(df$value) == 4) {
    label_names <- as_labeller(
      c(`q11_rank_policy` = "Policy", `q11_rank_epigeum` = "Epigeum",`q11_rank_dmp` = "DMP", 
        `q11_rank_compass` = "COMPASS"))
    
    facet <- # order and label facets
      facet_grid(~factor(name, levels = c('q11_rank_policy','q11_rank_epigeum','q11_rank_dmp','q11_rank_compass')),
                 labeller = as_labeller(label_names))   
    
    colours <- 
      scale_fill_manual(values =  c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00"))
    
  } else {
    label_names <- as_labeller(
      c(`q11_rank_policy` = "Policy", `q11_rank_epigeum` = "Epigeum",`q11_rank_dmp` = "DMP", 
        `q11_rank_compass` = "COMPASS", `q11_rank_grpo_email` = "GRPO email", 
        `q11_rank_grpo_workshop` = 'GRPO workshop', `q11_rank_audit` = 'GRPO audit')) 
    
    facet <- # order and label facets
      facet_grid(~factor(name, levels = c('q11_rank_policy','q11_rank_epigeum','q11_rank_dmp','q11_rank_compass','q11_rank_grpo_email','q11_rank_grpo_workshop','q11_rank_audit')),
                 labeller = as_labeller(label_names))  
    
    colours <- 
      scale_fill_manual(values =  wes_palette("Zissou1", 7,type = "continuous"))
  }
  
  ggplot(r, aes(x = {{ group }}, y = percent, fill = value )) +
    geom_col(position = 'stack') +
    labs(fill = 'Rank', x='', y='Percentage of respondents (%)') +
    {
      if (!is.na(annotation))
        geom_signif(
          data = annotation, inherit.aes = FALSE, manual = TRUE,
          aes(
            xmin = start,
            xmax = end,
            annotations = label,
            y_position = y
          ),
          vjust = 0.5,
          size = 0.3,
          tip_length = 0.01
        )
    } +
    facet +
    geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
    scale_y_continuous(labels = function(x) percent(abs(x)),n.breaks=8) +
    theme_preset() + 
    theme_font_size(9)+
    theme(legend.title = element_text(size=8), #change legend title font size
          legend.text = element_text(size=8)) +
    colours + 
    geom_text(aes(label = scales::label_percent(accuracy=0.1)(percent)),
              position = position_stack(vjust=0.5),
              size = geomtext_size) +
    theme_rank_legend_settings +
    coord_cartesian(clip = "off")
}

# stacked bar graphs for all modes
stacked_plot <- function(df, annotation = NA, geomtext_size=3){
  r <<- df %>% 
    group_by( name) %>% 
    mutate(total=n()) %>% 
    group_by( name, value) %>% 
    summarise(percent=n()/total) %>% 
    distinct( name, value, .keep_all = T) %>% # only select distinct rows 
    mutate(value = as.factor(value))   
  
  r2 <<- df %>% count(name)
  r$name <<- factor(r$name)
  
  if (max(df$value) == 5) {
    colours <- 
      scale_fill_manual(values = c("#3B9AB2", "#78B7C5","#EBCC2A", "#E1AF00", "#E86E00"))
    scale_x <- scale_x_discrete(limits=c('rank.policy','rank.elearning','rank.workshop','rank.email','rank.audit'),
                                labels= c('Policy','eLearning','Workshop','Email',"Audit"))
    
  } else if (max(df$value) == 4) {
    colours <- 
      scale_fill_manual(values =  c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00"))
    scale_x <- scale_x_discrete(limits=c('q11_rank_policy','q11_rank_epigeum','q11_rank_dmp','q11_rank_compass'),
                                labels= wrap10(c('Policy','Epigeum','DMP','Compass')))
  } else if (max(df$value) == 4) {
    colours <- 
      scale_fill_manual(values =  c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00"))
    scale_x <- scale_x_discrete(limits=c('q11_rank_grpo_email','q11_rank_grpo_workshop','q11_rank_audit'),
                                labels= wrap10(c('GRPO email','GRPO workshop','GRPO audit')))   
  } else {
    colours <- 
      scale_fill_manual(values =  wes_palette("Zissou1", 7,type = "continuous"))
    scale_x <- scale_x_discrete(limits=c('q11_rank_policy','q11_rank_epigeum','q11_rank_dmp','q11_rank_compass','q11_rank_grpo_email','q11_rank_grpo_workshop','q11_rank_audit'),
                                labels= wrap10(c('Policy','Epigeum','DMP','Compass',wrap10('GRPO email'),wrap10('GRPO workshop'), wrap10('GRPO audit'))))
  }
  
  ggplot(r, aes(x =name, y = percent, fill = value )) +
    geom_col(position = 'stack') +
    labs(fill = 'Rank', x='', y='Percentage of respondents (%)') +
    {
      if (!is.na(annotation))
        geom_signif(
          data = annotation, inherit.aes = FALSE, manual = TRUE,
          aes(
            xmin = start,
            xmax = end,
            annotations = label,
            y_position = y
          ),
          vjust = 0.5,
          size = 0.3,
          tip_length = 0.01
        )
    } +
    scale_x+
    geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
    scale_y_continuous(labels = function(x) percent(abs(x)),n.breaks=8) +
    theme_preset() + 
    theme_font_size(9)+
    theme(legend.title = element_text(size=8), #change legend title font size
          legend.text = element_text(size=8)) +
    colours + 
    geom_text(aes(label = scales::label_percent(accuracy=0.1)(percent)),
              position = position_stack(vjust=0.5),
              size = geomtext_size) +
    theme_rank_legend_settings +
    coord_cartesian(clip = "off")
}

# stacked bar graphs for ntu rank data 
stacked_plot_ntu <- function(df, annotation = NA, geomtext_size=3){
  r <<- df %>% 
    group_by( name) %>% 
    mutate(total=n()) %>% 
    group_by( name, value) %>% 
    summarise(percent=n()/total) %>% 
    distinct( name, value, .keep_all = T) %>% # only select distinct rows 
    mutate(value = as.factor(value))   
  
  r2 <<- df %>% count(name)
  r$name <<- factor(r$name)
  
  colours <- 
    scale_fill_manual(labels=c('1','2','2-3 [imputed]','3','4'), values = c("#3B9AB2", "#78B7C5","#d4d8db", "#E1AF00", "#E86E00"))
  scale_x <- scale_x_discrete(
    limits= wrap10(c('Policy','Epigeum','DMP','Compass')))
  
  
  ggplot(r, aes(x =name, y = percent, fill = value )) +
    geom_col(position = 'stack') +
    labs(fill = 'Rank', x='', y='Percentage of respondents (%)') +
    {
      if (!is.na(annotation))
        geom_signif(
          data = annotation, inherit.aes = FALSE, manual = TRUE,
          aes(
            xmin = start,
            xmax = end,
            annotations = label,
            y_position = y
          ),
          vjust = 0.5,
          size = 0.3,
          tip_length = 0.01
        )
    } +
    scale_x+
    geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
    scale_y_continuous(labels = function(x) percent(abs(x)),n.breaks=8) +
    theme_preset() + 
    theme_font_size(12)+
    theme(legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=12)) +
    colours + 
    geom_text(aes(label = scales::label_percent(accuracy=0.1)(percent)),
              position = position_stack(vjust=0.5),
              size = geom_text_size(12)) +
    theme_rank_legend_settings +
    coord_cartesian(clip = "off")
}
# debugonce(stacked_plot_ntu_grp2)
stacked_plot_ntu_grp2 <- function(df, group,  annotation = NA, geomtext_size=3){
  r <<- df %>% 
    group_by({{ group }}, name) %>% 
    mutate(total=n()) %>% 
    group_by({{ group }}, name, value) %>% 
    summarise(percent=n()/total) %>% 
    distinct({{ group }}, name, value, .keep_all = T) %>% # only select distinct rows 
    mutate(value = as.factor(value))   
  
  r2 <<- df %>% 
    count({{group}}, name) %>% 
    distinct({{ group }}, n)
  r2$label_name <- paste0(r2[[1]], '  (n=',r2[[2]], ')')
  label_table <-  setNames(r2[[3]],r2[[1]])
  
  r$name <<- factor(r$name)
  colours <- scale_fill_manual(labels=c('1','2','2-3 [imputed]','3','4'), 
                               values = c("#3B9AB2", "#78B7C5","#d4d8db", "#E1AF00", "#E86E00"))
  
  scale_x <- scale_x_discrete(limits=wrap10(c('Policy','Epigeum','DMP','Compass')))
  
  
  ggplot(r, aes(x =  name , y = percent, fill = value )) +
    geom_col(position = 'stack') +
    labs(fill = 'Rank', x='', y='Percentage of respondents (%)') +
    {
      if (!is.na(annotation))
        geom_signif(
          data = annotation, inherit.aes = FALSE, manual = TRUE,
          aes(
            xmin = start,
            xmax = end,
            annotations = label,
            y_position = y
          ),
          vjust = 0.5,
          size = 0.3,
          tip_length = 0.01
        )
    } +
    # both facet grip and facet wrap works. Note the diff in specification
    facet_grid(cols = vars({{group}}), labeller = as_labeller(label_table)) +
    # facet_wrap(vars({{group}}), nrow = 1, labeller = as_labeller(label_table)) +
    scale_x +
    geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
    scale_y_continuous(labels = function(x) percent(abs(x)),n.breaks=8) +
    theme_preset() + 
    theme_font_size(9)+
    theme(legend.title = element_text(size=8), #change legend title font size
          legend.text = element_text(size=8)) +
    colours + 
    geom_text(aes(label = scales::label_percent(accuracy=0.1)(percent)),
              position = position_stack(vjust=0.5),
              size = geomtext_size) +
    theme_rank_legend_settings +
    coord_cartesian(clip = "off")
}


# Statistical tests and table ---------------------------------------------

# conducts Mann whitney test is there's 2 groups and Kruskal wallis text if there's 3 groups
# also performs Dunn's post hoc test if KW test result is significant
rank_wilcox <- function(df, mode, group, post = 0){
  # if this is absent, group argument should not be a string (i.e field, not 'field')
  group <- as.name(group) # 'unquotes' group; 
  
  d <-  df %>% count({{ group }}, sort = TRUE) # arrange in desc order
  factors <- d[,1] %>% unlist() # obtains factors
  levels <- length(factors) # calculate how many factors are there
  sq <- seq(as.integer(d[1, -1]))  # obtain freq of the highest freq factor and create sequence
  # create list 
  lst <- list()
  for (i in 1:levels) {
    x <- df %>% 
      filter({{ group }} == factors[i]) %>% 
      select({{ mode }}) %>% 
      unlist(use.names = FALSE)
    # assign(paste0('x', i), x)
    name <- as.character(factors[[i]]) # name of list element
    lst[[name]] <- x[sq] # x[sq] ensures that NA values are added to match the longer vector
  }
  # lst<<-lst
  listframe <- do.call(data.frame, lst) # create dataframe from list (necessary for kruskal test)
  if (levels == 2) {
    wilcox.test(lst[[1]], lst[[2]])
    # wilcox.test(d[,1], d[,2])
  } else if (levels > 2) {
    kw <- kruskal.test(listframe)
    if (kw[[3]] <= 0.05) {
      posthoc <-
        dunnTest(value ~ name, 
                 data = listframe %>% 
                   pivot_longer(cols = 1:ncol(listframe)) %>% 
                   na.omit())
      if (post != 0){
        return(list(
          kw,
          posthoc))  
      } else {
        return(kw)
      }
    }
    return(kw)
  }
}


# gt objects: for every single cell in the table, bold value if it's below 0.05
# use case: bolding significant results in posthoc table
bold_signif <- function(gtobj, column){
  gtobj <- gtobj %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = {{column}}, 
                                     rows = {{column}} <= 0.05))
  gtobj
}

# create gt object for posthoc pairwise table (only for the 5 modes in general ranking q)
posthoctab <- function(data, title){
  rownames(data)=c('Policy','Workshop','Email','eLearning')
  data %>% signif(3) %>% as.data.frame() %>% gt(rownames_to_stub =TRUE) %>% 
    cols_label(
      rank.audit = "Audit",
      rank.policy = "Policy",
      rank.workshop = 'Workshop',
      rank.email = 'Email'
    )  %>% 
    tab_header(
      title = {{title}}  
    ) %>% 
    bold_signif(rank.audit) %>% 
    bold_signif(rank.policy) %>% 
    bold_signif(rank.workshop) %>% 
    bold_signif(rank.email)
}
posthoctab_rate <- function(data, title){
  rownames(data)=c('Email','Audit')
  data %>% signif(3) %>% as.data.frame() %>% gt(rownames_to_stub =TRUE) %>% 
    tab_header(
      title = {{title}}  
    ) %>% 
    bold_signif(TBL) %>% 
    bold_signif(Email) 
}
posthoctab_ntu <- function(data, title){
  rownames(data)=c('DMP','Epigeum','Compass')
  data %>% signif(3) %>% as.data.frame() %>% gt(rownames_to_stub =TRUE) %>% 
    tab_header(
      title = {{title}}  
    ) %>% 
    bold_signif(DMP) %>% 
    bold_signif(Epigeum) %>% 
    bold_signif(Policy)
}
posthoctab_lkc <- function(data, title){
  rownames(post[3][[1]])=c('DMP','Epigeum','Compass','GRPO email','GRPO workshop', 'GRPO audit')
  post[3][[1]] %>% signif(3) %>% as.data.frame() %>% gt(rownames_to_stub =TRUE) %>% 
    tab_header(
      title = {{title}}  
    ) %>% 
    bold_signif(DMP) %>% 
    bold_signif(Epigeum) %>% 
    bold_signif(Policy) %>% 
    bold_signif(Compass)
  # bold_signif('GRPO email') %>% 
  # bold_signif('GRPO workshop')
}

# friedman test + kendall W + Nemenyi post hoc test & table if significant (uses above function)
# able to filter specific subgroups
# Title of posthoc table defaults to name of subgroup
friedman <- function(data, cat = NULL, group = NULL, title = group){
  if (missing(cat)) {
    d <- data %>%
      select(1:5) %>%
      dplyr::arrange(rank.policy, rank.elearning, rank.workshop,rank.email, rank.audit) %>% 
      as.matrix()
  } else {
    d <- data %>%
      filter({{cat}} == group) %>%
      select(1:5) %>%
      dplyr::arrange(rank.policy, rank.elearning, rank.workshop,rank.email, rank.audit) %>% 
      as.matrix()
  }
  rownames(d) <- c(1:nrow(d))
  test <- PMCMRplus::friedmanTest(d) # friedman test
  kendall <- DescTools::KendallW(t(d), test = TRUE)  # kendall W test
  if (test[2] <= 0.05 ) { # conduct posthoc test if friedman test was significant
    post <- PMCMRplus::frdAllPairsNemenyiTest(d)
    table <- posthoctab(post[3][[1]], title)
    return(list(test, kendall, post, table))
  } else {
    return(list(test, kendall))
  }
}
friedman_rate <- function(data, cat = NULL, group = NULL, title = group){
  if (missing(cat)) {
    d <- data %>%
      select(TBL, Email, Audit) %>% 
      as.matrix()
  } else {
    d <- data %>%
      filter({{cat}} == group) %>%
      select(TBL, Email, Audit) %>% 
      as.matrix()
  }
  rownames(d) <- c(1:nrow(d))
  test <- PMCMRplus::friedmanTest(d) # friedman test
  kendall <- DescTools::KendallW(t(d), test = TRUE)  # kendall W test
  if (test[2] <= 0.05 ) { # conduct posthoc test if friedman test was significant
    post <<- PMCMRplus::frdAllPairsNemenyiTest(d)
    table <- posthoctab_rate(post[3][[1]], title)
    return(list(test, kendall, post, table))
  } else {
    return(list(test, kendall))
  }
}
friedman_ntu <- function(data, cat = NULL, group = NULL, title = group){
  if (missing(cat)) {
    d <- data %>%
      select(Policy, DMP, Epigeum, Compass) %>% 
      as.matrix()
  } else {
    d <- data %>%
      filter({{cat}} == group) %>%
      select(Policy, DMP, Epigeum, Compass) %>% 
      as.matrix()
  }
  rownames(d) <- c(1:nrow(d))
  test <- PMCMRplus::friedmanTest(d) # friedman test
  kendall <- DescTools::KendallW(t(d), test = TRUE)  # kendall W test
  if (test[2] <= 0.05 ) { # conduct posthoc test if friedman test was significant
    post <<- PMCMRplus::frdAllPairsNemenyiTest(d)
    table <- posthoctab_ntu(post[3][[1]], paste0(group, " (n = ", nrow(d),')'))
    return(list(test, kendall, post, table))
  } else {
    return(list(test, kendall))
  }
}
friedman_lkc <- function(data, cat = NULL, group = NULL, title = group){
  if (missing(cat)) {
    d <- data %>%
      select(Policy, DMP, Epigeum, Compass, 'GRPO email', 'GRPO workshop', 'GRPO audit') %>%
      as.matrix()
  } else {
    d <- data %>%
      filter({{cat}} == group) %>%
      select(Policy, DMP, Epigeum, Compass, 'GRPO email', 'GRPO workshop', 'GRPO audit') %>% 
      as.matrix()
  }
  rownames(d) <- c(1:nrow(d))
  test <- PMCMRplus::friedmanTest(d) # friedman test
  kendall <- DescTools::KendallW(t(d), test = TRUE)  # kendall W test
  if (test[2] <= 0.05 ) { # conduct posthoc test if friedman test was significant
    post <<- PMCMRplus::frdAllPairsNemenyiTest(d)
    table <- posthoctab_lkc(post[3][[1]], paste0(group, " (n = ", nrow(d),')'))
    return(list(test, kendall, post, table))
  } else {
    return(list(test, kendall))
  }
}

# friedman test + Nemenyi post hoc test (w/o posthoctab)
friedman_col <- function(data, col, cat = NULL, group = NULL){
  if (missing(cat)) {
    d <- data %>%
      select(col) %>%
      as.matrix()
  } else {
    d <- data %>%
      filter({{cat}} == group) %>%
      select(col) %>%
      as.matrix()
  }
  rownames(d) <- c(1:nrow(d))
  test <- PMCMRplus::friedmanTest(d)
  if (test[2] <= 0.05 ) {
    post <- PMCMRplus::frdAllPairsNemenyiTest(d)
    return(list(test, post))
  } else {
    return(list(test))
  }
}



# Graphs for multiple selection question ----------------------------------

# returns df (plot data) and graph
# have to define the exact dt before plugging it in 
# facet doesnt really work unless the exact plot to be used for ggplot is specified as dt
multresp <-function(dt, group = NULL, facet = 0){
  profile<-c('ID','pilot',"community","age", "college",'field',"english","years","years_ntu","gender",'current_role')
  cols <- c('value','total',group)
  
  if (missing(facet)) {
    plot <- melt(dt[pilot ==0,], id.vars = profile)[, total := length(unique(ID))][
      , .(count = .N,
          percent = .N/total),
      by = cols][
        complete.cases(value)
      ]
  } else {
    plot <- dt
  }
  
  if (missing(group)) {
    p <- ggplot(plot, aes(x = (reorder(wrap15(value), -percent)), y = percent)) +
      geom_col(fill='grey') 
  } else {
    p <- ggplot(plot, aes(x = (reorder(wrap15(value), -percent)), y = percent, fill = get(group))) +
      geom_col() +
      scale_fill_manual(values =  wes_palette("Chevalier1")[2:4])
  }
  
  graph <- p +
    geom_text(aes(label=paste(scales::label_percent(accuracy=0.1)(percent), '(', count, ')')), 
              position = position_stack(vjust=0.5),    # align position of labels to be mid of each block!
              size = 2.5, color = "black", show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_preset() +
    labs(x = '', y='', fill = group)
  if (!missing(facet)) {
    graph <- graph + facet_wrap(~variable)
  }
  return(list(graph,plot))
  # print(graph)
  # return(plot)
}

# For plotting graph about reasons for not participating in [mode]
multresp_top <-function(dt){
  profile<-c('ID','pilot',"community","age", "college",'field',"english","years","years_ntu","gender",'current_role')
  cols <- c('variable','value','total')
  
  plot <- melt(dt[pilot==0,], id.vars = profile)[, total := length(unique(ID))][
    , .(count = .N,
        percent = .N/total),
    by = cols][
      complete.cases(value)
    ]
  
  setDF(plot)
  plot$value <- replace(plot$value,grep("too much time",plot$value),"Too time- consuming")
  plot$value <- replace(plot$value,grep("interest",plot$value),"Lack of interest")
  plot$value <- replace(plot$value,grep("do not recall|not aware",plot$value),"Lack of awareness")
  plot$value <- replace(plot$value,grep("promoting",plot$value),"Not useful in promoting RI")
  plot$value <- replace(plot$value,grep("work",plot$value),"Not useful to my work")
  plot$value <- replace(plot$value,grep("mode of delivery",plot$value),"Mode of delivery is not engaging")
  plot$value <- replace(plot$value,grep("commitments",plot$value),"Other commitments")
  
  x<<-plot
  top_n(plot, n=3, percent) %>%
    ggplot(aes(x = (reorder(wrap15(value), -percent)), y = percent)) +
    geom_col(fill='grey') +
    geom_text(aes(label=paste(scales::label_percent(accuracy=0.1)(percent), '(', count, ')')), 
              position = position_stack(vjust=0.5),    # align position of labels to be mid of each block!
              size = 2.5, color = "black", show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_preset() +
    labs(x = '', y='')
  # return(list(graph,plot))
  # print(graph)
  # return(plot)
}

# specific for compulsory modes pol, epi, audit
# combines responses for what would encourage you to participate
sum_count <- function(a,b){
  rbind(a[[2]],b[[2]]) %>% 
    group_by(value) %>% 
    mutate(total = sum(total),
           count  = sum(count),
           percent = count/total) %>% 
    distinct(value, total, count, percent) %>% 
    # plot graph
    ggplot(aes(x = (reorder(wrap15(value), -percent)), y = percent)) +
    geom_col(fill='grey') +
    geom_text(aes(label=paste(scales::label_percent(accuracy=0.1)(percent), '(', count, ')')), 
              position = position_stack(vjust=0.5),    # align position of labels to be mid of each block!
              size = 2.5, color = "black", show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.05)) +
    labs(x = '', y='')
}



# Functions which were phased out -----------------------------------------
# the following functions are no longer used
# divergent plot is replaced by the conventional stacked plot
# line plot is an incomplete exploration
divergent_plot_grp <- function(df, group){
  r <- df %>% 
    group_by({{ group }}, name) %>% 
    mutate(total=n()) %>% 
    group_by({{ group }}, name, value) %>% 
    summarise(percent=n()/total) %>% 
    distinct({{ group }}, name, value, .keep_all = T) # only select distinct rows 
  
  if ((max(df$value) %% 2) == 0) {
    lower <- (max(df$value) %/% 2)
    mid <- lower + 0.5
    r <- merge(
      r %>% filter(value > mid) %>% mutate(percent = -percent), 
      r %>% filter(value < mid) ,
      all = T
    )
  } else {
    mid <- (max(df$value) %/% 2) + 1
    r <- merge(
      merge(
        r %>% filter(value > mid) %>% mutate(percent = -percent), 
        r %>% filter(value == mid) %>% mutate(percent = -percent / 2), 
        all = T
      ),
      merge(
        r %>% filter(value < mid) ,
        r %>% filter(value == mid) %>% mutate(percent = percent / 2),
        all = T
      ),
      all = T 
    )
  }
  
  r <- r %>% mutate(value = as.factor(value))
  
  if ((max(df$value) == 5)){
    # arranging levels to decide order of facet grid
    r$name <- factor(r$name, 
                     levels = c('rank.policy','rank.elearning','rank.workshop','rank.email','rank.audit'))
    
    # editing labels for the modes
    label_names <- as_labeller(
      c(`rank.policy` = "Policy", `rank.elearning` = "eLearning",`rank.workshop` = "Workshop", 
        `rank.email` = "Email",`rank.audit` = "Audit")) 
    
    ggplot(r, aes(x = {{ group }}, y = percent, fill = value )) +
      # ranks 4, 5 and half of rank 3 have percentages above 0, no change to direction as it is above x axis 
      geom_col(data = r %>% filter(percent>0), position = 'stack') +
      # ranks 1, 2 and half of rank 3 have percentages below 0, hence stack direction is reversed
      geom_col(data = r %>% filter(percent<0),position =  position_stack(reverse = TRUE )) +
      labs(fill = 'Rank', x='', y='Percentage') +
      facet_grid(~name, scales="free_x", labeller = as_labeller(label_names)) + # label the facets 
      geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
      scale_y_continuous(labels = function(x) percent(abs(x)),n.breaks=8) +
      scale_fill_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#E86E00"))+
      theme_minimal() +
      theme(panel.grid.major.y = element_line(colour = "lightgrey"),
            legend.position = 'top',
            panel.border = element_rect(colour = "black", fill=NA, size=0.05)
      ) +
      geom_text(data = r %>% filter(percent>0),
                aes(label=ifelse(value==mid,'',
                                 ifelse(percent > 0.063,  
                                        scales::label_percent(accuracy=0.1)(percent),
                                        ''))),
                position=position_stack(vjust = 0.5))  +
      geom_text_repel(data = r %>% filter(percent>0),
                      aes(label=ifelse(value==mid,'',
                                       ifelse(percent <= 0.063,  
                                              scales::label_percent(accuracy=0.1)(percent),
                                              ''))),
                      position=position_stack(vjust = 1),
                      direction = "y") +
      geom_text(data = r %>% filter(percent>0) %>% filter(value==mid),
                aes(label=scales::label_percent(accuracy=0.1)(2*percent)),
                y=-0.02)  +
      geom_text(data = r %>% filter(percent<0),
                aes(label=ifelse(value==mid,'',
                                 ifelse(percent < -0.063,  
                                        scales::label_percent(accuracy=0.1)(-percent),
                                        ''))),
                position=position_stack(vjust = 0.5,reverse = TRUE )) +
      geom_text_repel(data = r %>% filter(percent<0),
                      aes(label=ifelse(value==mid,'',
                                       ifelse(percent >= -0.063,  
                                              scales::label_percent(accuracy=0.1)(-percent),
                                              ''))),
                      position=position_stack(vjust = 1,reverse = TRUE),
                      direction = "y")
  } else {
    r$name <- factor(r$name, 
                     levels = c('q11_rank_policy','q11_rank_epigeum','q11_rank_dmp','q11_rank_compass'))
    
    # editing labels for the modes
    label_names <- as_labeller(
      c(`q11_rank_policy` = "Policy", `q11_rank_epigeum` = "Epigeum",`q11_rank_dmp` = "DMP", 
        `q11_rank_compass` = "COMPASS")) 
    
    ggplot(r, aes(x = {{ group }}, y = percent, fill = value )) +
      # ranks 4, 5 and half of rank 3 have percentages above 0, no change to direction as it is above x axis 
      geom_col(data = r %>% filter(percent>0), position = 'stack') +
      # ranks 1, 2 and half of rank 3 have percentages below 0, hence stack direction is reversed
      geom_col(data = r %>% filter(percent<0),position =  position_stack(reverse = TRUE )) +
      labs(fill = 'Rank', x='', y='Percentage') +
      facet_grid(~name, scales="free_x", labeller = as_labeller(label_names)) + # label the facets 
      geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
      scale_y_continuous(labels = function(x) percent(abs(x)),n.breaks=8) +
      scale_fill_manual(values =  wes_palette("Zissou1", max(df$value),type = "continuous"))+
      theme_minimal() +
      theme(panel.grid.major.y = element_line(colour = "lightgrey"),
            legend.position = 'top',
            panel.border = element_rect(colour = "black", fill=NA, size=0.05)
      ) +
      geom_text(data = r %>% filter(percent>0),
                aes(label=ifelse(percent > 0.063,  
                                 scales::label_percent(accuracy=0.1)(percent),
                                 '')),
                position=position_stack(vjust = 0.5))  +
      geom_text_repel(data = r %>% filter(percent>0),
                      aes(label=ifelse(percent <= 0.063,  
                                       scales::label_percent(accuracy=0.1)(percent),
                                       '')),
                      position=position_stack(vjust = 1),
                      direction = "y") +
      geom_text(data = r %>% filter(percent<0),
                aes(label=ifelse(percent < -0.063,  
                                 scales::label_percent(accuracy=0.1)(-percent),
                                 '')),
                position=position_stack(vjust = 0.5,reverse = TRUE )) +
      geom_text_repel(data = r %>% filter(percent<0),
                      aes(label= ifelse(percent >= -0.063,  
                                        scales::label_percent(accuracy=0.1)(-percent),
                                        '')),
                      position=position_stack(vjust = 1,reverse = TRUE),
                      direction = "y")
    
  }
}
divergent_plot <- function(df){
  r <- df %>% 
    group_by(name) %>% 
    mutate(total=n()) %>% 
    group_by(name, value) %>% 
    summarise(percent=n()/total) %>% 
    distinct(name, value, .keep_all = T) # only select distinct rows 
  
  
  # r <- df %>% 
  #   group_by(name, value) %>% 
  #   mutate(percent=n()) %>% 
  #   distinct(name, value, .keep_all = T) # only select distinct rows 
  
  if ((max(df$value) %% 2) == 0) {
    mid <- (max(df$value) %/% 2)
    r <- merge(
      r %>% filter(value > mid) %>% mutate(percent = -percent), 
      r %>% filter(value <= mid) ,
      all = T
    )
    r <- r %>% mutate(value = as.factor(value))
    
    text <- list(
      geom_text(data = r %>% filter(percent>0),
                aes(label=ifelse(percent > 0.063,  
                                 scales::label_percent(accuracy=0.1)(percent),
                                 '')),
                position=position_stack(vjust = 0.5)),
      geom_text_repel(data = r %>% filter(percent>0),
                      aes(label=ifelse(percent <= 0.063,  
                                       scales::label_percent(accuracy=0.1)(percent),
                                       '')),
                      position=position_stack(vjust = 1),
                      direction = "y"),
      geom_text(data = r %>% filter(percent<0),
                aes(label=ifelse(percent < -0.063,  
                                 scales::label_percent(accuracy=0.1)(-percent),
                                 '')),
                position=position_stack(vjust = 0.5,reverse = TRUE )),
      geom_text_repel(data = r %>% filter(percent<0),
                      aes(label=ifelse(percent >= -0.063,  
                                       scales::label_percent(accuracy=0.1)(-percent),
                                       '')),
                      position=position_stack(vjust = 1,reverse = TRUE),
                      direction = "y")
    )
    
  } else {
    mid <- (max(df$value) %/% 2) + 1
    r <- merge(
      merge(
        r %>% filter(value > mid) %>% mutate(percent = -percent), 
        r %>% filter(value == mid) %>% mutate(percent = -percent / 2), 
        all = T
      ),
      merge(
        r %>% filter(value < mid) ,
        r %>% filter(value == mid) %>% mutate(percent = percent / 2),
        all = T
      ),
      all = T 
    )
    r <- r %>% mutate(value = as.factor(value))
    
    text <- list(
      geom_text(data = r %>% filter(percent>0),
                aes(label=ifelse(value==mid,'',
                                 ifelse(percent > 0.063,  
                                        scales::label_percent(accuracy=0.1)(percent),
                                        ''))),
                position=position_stack(vjust = 0.5)),
      geom_text_repel(data = r %>% filter(percent>0),
                      aes(label=ifelse(value==mid,'',
                                       ifelse(percent <= 0.063,  
                                              scales::label_percent(accuracy=0.1)(percent),
                                              ''))),
                      position=position_stack(vjust = 1),
                      direction = "y"),
      geom_text(data = r %>% filter(percent>0) %>% filter(value==mid),
                aes(label=scales::label_percent(accuracy=0.1)(2*percent)),
                y=-0.02),
      geom_text(data = r %>% filter(percent<0),
                aes(label=ifelse(value==mid,'',
                                 ifelse(percent < -0.063,  
                                        scales::label_percent(accuracy=0.1)(-percent),
                                        ''))),
                position=position_stack(vjust = 0.5,reverse = TRUE )),
      geom_text_repel(data = r %>% filter(percent<0),
                      aes(label=ifelse(value==mid,'',
                                       ifelse(percent >= -0.063,  
                                              scales::label_percent(accuracy=0.1)(-percent),
                                              ''))),
                      position=position_stack(vjust = 1,reverse = TRUE),
                      direction = "y") 
    )
    
  }
  
  
  ggplot(r, aes(x= name,y = percent, fill = value )) +
    # ranks 4, 5 and half of rank 3 have percentages above 0, no change to direction as it is above x axis 
    geom_col(data = r %>% filter(percent>0), position = 'stack') +
    # ranks 1, 2 and half of rank 3 have percentages below 0, hence stack direction is reversed
    geom_col(data = r %>% filter(percent<0),position =  position_stack(reverse = TRUE )) +
    labs(fill = 'Rank', y='Percentage', x = '') +
    geom_hline(yintercept = 0, linetype='dotted') + # x axis at percentage = 0
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),n.breaks=8) +
    scale_fill_manual(values =  wes_palette("Zissou1", 7,type = "continuous"))+
    # c('#6B82B2',"#3B9AB2", "#78B7C5", '#9AD072',
    #   "#EBCC2A", "#E1AF00", "#E86E00")
    theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "lightgrey"),
          legend.position = 'top',
          panel.border = element_rect(colour = "black", fill=NA, size=0.05)
    ) +
    text
}
line_plot <- function(mode, cat){
  setDF(rank)
  line_plot_graph <<- rank %>% dplyr::group_by({{mode}},{{cat}})  %>% dplyr::mutate(count=n()) %>% 
    dplyr::group_by({{mode}}) %>% dplyr::mutate(percent=count/n()) %>% 
    dplyr::group_by({{mode}},{{cat}},count,percent) %>% dplyr::summarise() %>% dplyr::ungroup() 
  line_plot_last <- line_plot_graph %>% filter({{mode}} == 5)
  ggplot(line_plot_graph, 
         aes(x={{mode}},y=percent,fill={{cat}}, 
             label = wrap7(paste0(scales::label_percent(accuracy=0.1)(percent),
                                  '  (', count, ')')))) +
    geom_col() +
    # geom_point()+
    # geom_text_repel(size= 3.5,
    #                 direction='y',
    #                 # force=1,
    #                 # force_pull = 1,
    #                 # box.padding = 1,
    #                 # point.padding = 1,
    #                 # min.segment.length = 5
    #                 ) +
    geom_text(position = position_stack(vjust = 0.5), size= 3.5)+
    labs(y='', x='') +
    theme_minimal()+
    theme(axis.text=element_text(size=10),
          panel.grid.major.y = element_line(colour = "lightgrey"))+
    theme(legend.position="bottom") +
    guides(color = 'none') +
    # scale_x_discrete(expand = c(.1, .1))+
    scale_x_continuous(labels = wrap_format(5),
                       expand = c(.1, 0)) +
    scale_y_continuous(labels = scales::percent)
  #                    expand = c(.2, .2),
  #                    sec.axis = dup_axis(
  #                      breaks = line_plot_last$percent,
  #                      labels = line_plot_last[[substitute(cat)]]
  #                    ))
}



