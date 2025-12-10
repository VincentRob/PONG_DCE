require(mlogit)
library(dplyr)
library(data.table)
library(ggplot2)
library(jsonlite)
library(scales) 
library(patchwork)
library(car)
library(knitr)

# Background ----
source("Figure_theme.R")

font.axis.lbls <- 20
font.ttls <- 20
theme_pong <- theme_minimal() +
  theme(
    axis.text.y = element_text(size = font.axis.lbls,color = "black"), 
    axis.text.x = element_text(size = font.axis.lbls,color = "black"),
    axis.title.x = element_text(size = font.ttls,color = "black"),
    axis.title.y = element_text(size = font.ttls,color = "black"),
    legend.title = element_text(size = font.ttls,color = "black"),
    legend.text = element_text(size = font.axis.lbls,color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = font.ttls + 2,hjust = 0.5,color = "black")
  )

source(file = "functions_pong_soft_launch.R")

input_specs <- read.csv("input/input_specs.csv")
setDT(input_specs)

if (input_specs[,anyDuplicated(.SD),.SDcols = c("survey_name","data_path")]){
  stop("Sample-data file pairs should be unique")
}

survey.names =c("pong_dutch_report")#,"full_launch_panelclix") # names(files.path)#c("soft_launch_pooled")

invest.list.lvl <- list("insu" = 5000,"heat_networks" = 2000, "heat_pumps" = 7000)

# Continuous or discrete costs
bin.costs.cont <- TRUE

# For heterogeneity analysis, replace missing by Hoorn when applicable
hoorn.file.name <- "Hoorn_complete" 

lang = "nl" # default is english, en

# Costs columns
reg.onetimecosts <- "(one_time_costs)|(one_time_amount)"
reg.monthcosts <- "heating_costs"
reg.costs <- paste(reg.onetimecosts,reg.monthcosts,sep="|")

for (survey.name in survey.names){
  
  dir.out <- paste0("output/",survey.name,"/")
  dir.create(dir.out)
  dir.out.figs <- paste0(dir.out,"figures/")
  dir.create(dir.out.figs)
  
  data = data.table()
  for (file.name in input_specs[survey_name == survey.name,unique(data_path)]){
    data_tmp <- readxl::read_excel(paste0(file.name)) %>% as.data.table()
    data_tmp[,survey_name := gsub("^.*/|\\.xlsx$", "", file.name)]
    data_tmp[,survey_name_4desctiptives := copy(survey_name)]
    
    # Remove emoji
    names(data_tmp) <- sapply(names(data_tmp),\(x) gsub("[^\x01-\x7F]", "", x)) 
    
    data <- rbindlist(list(data,data_tmp),use.names = TRUE, fill = TRUE)
  }
  
  
  # Descriptives - completion ----
  col.comp <- names(data)[grepl("(Date submitted)|(Response ID)|(Date started)|(Last page)|(code that is in your letter)|(which device)|(What type of residence)|(^consent)",names(data))]
  data.comp <- janitor::clean_names(data[,.SD,.SDcols = col.comp])
  
  if ("start_to_continue_please_enter_the_code_that_is_in_your_letter_4_digits_followed_by_3_letters" %in% names(data.comp)){
    data.comp[grepl("^15",start_to_continue_please_enter_the_code_that_is_in_your_letter_4_digits_followed_by_3_letters),muni := "hoorn"]
    data.comp[grepl("^37",start_to_continue_please_enter_the_code_that_is_in_your_letter_4_digits_followed_by_3_letters),muni := "medemblik"]
  } else {
    data.comp[,muni := "No muni"]
  }
  
  data.comp[,.(.N,
               more_than_17_pages_completed = sum(lastpage_last_page>16),
               owner_occupied_house = sum(introduction1_what_type_of_residence_do_you_live_in == "Owner-occupied house"),
               consent = sum(consent == "I agree and want to participate",na.rm = TRUE),
               device_computer = sum(introduction3_on_which_device_are_you_playing_this_choice_game == "Computer",na.rm=TRUE)
  ),
  by=.(muni,complete = !is.na(submitdate_date_submitted) & !is.na(consent) & consent == "I agree and want to participate")]
  
  # Keep complete responses ----
  #data <- data[consent. == "I agree and want to participate" & `lastpage. Last page` == data[,max(`lastpage. Last page`,na.rm=TRUE)]]
  data <- data[consent. == "I agree and want to participate" & `lastpage. Last page` >17]
  #stop("Ad hoc data filter below, remove it before to proceed")
  #data <- data[`ConstructionPeriod2. What is the construction period of your dwelling?` %in% c("1991-2005","2006-2025")]
  
  # Outliers ----
  nm.out <- names(data)[grepl("Group time",names(data)) & grepl("(Explanation of the Choice Game)|(Introduction Game of choice)",names(data))]
  #if (length(nm.out) != 3 ) stop("There should be 3: intro, explanation tech and explanation insu")
  for (nm in nm.out){
    tab <- data[,.SD,.SDcols = nm]
    fig.nm <- gsub("[^A-Za-z0-9]", "_", nm)
    
    ggplot(tab,aes(x = get(nm))) + geom_density() + coord_cartesian(xlim = c(0, 300)) + theme.graphs + labs(x = stringr::str_to_sentence(gsub("\\_"," ",fig.nm)))
    ggsave(paste0(dir.out.figs, "fig_",fig.nm, ".png"),units = "cm",height = height, width = width)
  }
  
  sink(paste0(dir.out, "outliers.txt" ))
  cat(paste0("Before removing short introduction times: ",dim(data)[1]))
  
  #data <- data[get(nm.out[grepl("Introduction",nm.out)]) > 30 & get(nm.out[grepl("Tech",nm.out)]) > 30,]
  
  cat(paste0("After removing short introduction times: ",dim(data)[1]))
  sink()
  
  # Descriptives ----
  col.desc <- names(data)[!grepl("(Choice\\ [0-9])|(Question time)|(groupTime)|(randNumber)|(choice screen)|(GXQ00001)|(randSet1)|(pid$)|(cid$)|(^GXQ)|(^r[0-9])|(Date)|(Seed)|(If yes\\, how many\\?)|(Technology A)|(Technology B)|(Thank you for your participation)|(Total time)",names(data))] # |(please enter the code that is in your letter) |(zipcode)
  data.desc <- data[,.SD,.SDcols = col.desc]
  
  #old.nms <- names(data.desc)[grepl("\\[multiple",names(data.desc))]
  #setnames(data.desc,old = old.nms,new = gsub("(.*?)\\ \\[multipl.*","\\1",old.nms))
  old.nms <- names(data.desc)
  new.nms <- gsub(".*?\\.(.*)","\\1",names(data.desc))
  new.nms[new.nms == ""] <- old.nms[new.nms == ""]
  
  names(data.desc) <- new.nms
  
  ## Groups respondents ----
  nm.lottery <- names(data.desc)[grepl("lottery",names(data.desc))]
  if (length(nm.lottery)>0){
    data.desc[,lottery_quantile := cut(get(nm.lottery),breaks = quantile(get(nm.lottery),c(0,0.25,0.75,1),na.rm=TRUE),include.lowest=TRUE)]
    data.desc[,lottery_quantile := cut(get(nm.lottery),breaks = quantile(get(nm.lottery),c(0,0.25,0.75,1),na.rm=TRUE),include.lowest=TRUE)]
  }
  
  data.desc[,WOZ := as.integer(stringr::str_extract(` What is the WOZ value of your dwelling?`,"[0-9]{3}"))]
  data.desc[,WOZ_quantile := cut(WOZ,breaks = quantile(WOZ,c(0,0.33,0.66,1),na.rm=TRUE),include.lowest=TRUE)]
  
  nm.var <- " What is the construction period of your dwelling?"
  data.desc[, constr_period := fcase(
    get(nm.var) %in% c("1945 or before"), "1 - 1945 or before",
    get(nm.var) %in% c("1946-1970", "1971-1990"), "2 - 1946-1990",
    get(nm.var) %in% c("1991-2005"), "3 - 1991-2005",
    get(nm.var) %in% c( "2006-2025"),  "4 - 2006-2025")]
  
  nm.var <- " Which best describes your employment situation?"
  nm.var2 <- " Which best describes the employment situation of your partner?"
  job.var.work <- c("Working - Full Time (at least 32 hours per week)","Working - Part Time")
  job.var.home <- c("Keeping house or being home maker",
                    "Retired",
                    "Unemployed and looking for work",
                    "Receiving/Awaiting approval for disability payments")
  if (nm.var %in% names(data.desc)){
    data.desc[, work_status := fcase(
      get(nm.var) %in% job.var.home & (get(nm.var2) %in% job.var.home | is.na(get(nm.var2))), "4 - all_home",
      (get(nm.var) %in% job.var.home & get(nm.var2) %in% job.var.work) | (get(nm.var2) %in% job.var.home & get(nm.var) %in% job.var.work), "2 - one_working_one_home",
      get(nm.var) %in% job.var.work & get(nm.var2) %in% job.var.work, "1 - two_working",
      get(nm.var) %in% job.var.work & is.na(get(nm.var2)), "3 - single_worker",
      !is.na(get(nm.var)) | !is.na(get(nm.var2)),"5 - other"
    )]
    print(data.desc[,.N,by=.(work_status,get(nm.var),get(nm.var2))][order(work_status,N)])
  }
  # Age
  nm.var <- " To which age group do you belong?"
  nm.var2 <- " To which age group does your partner belong?"
  if (!(nm.var2 %in% names(data.desc))) nm.var2 <- nm.var
  data.desc[, age_max := max(c(stringr::str_extract(get(nm.var),"[0-9]{2}"),
                               stringr::str_extract(get(nm.var2),"[0-9]{2}")),
                             na.rm=TRUE),by=" Response ID"]
  data.desc[age_max >= 55,age_max := "55+"]
  data.desc[age_max <= 35,age_max := "35-"]
  print(data.desc[,.N,by=.(age_max,get(nm.var),get(nm.var2))][order(age_max,N)])
  
  
  # Household type 
  nm.var <- " Who is currently part of your household?"
  data.desc[, hh_type := if_else(grepl("child",get(nm.var)),"Children","No children")]
  
  # Device
  nm.var <- " On which device are you playing this choice game?"
  data.desc[, device := if_else(grepl("Smartphone",get(nm.var)),"Smartphone","Computer/tablet")]
  
  # Education
  nm.var <- " What is your highest completed education?"
  data.desc[, education := fcase(
    grepl("master",get(nm.var)), "4 - master or higher",
    grepl("bachelor",get(nm.var)), "3 - bachelor",
    grepl("HAVO",get(nm.var)), "2 - HAVO",
    grepl("VMBO|Primary",get(nm.var)), "1 - VMBO or lower",
    default = "0 - Other"
  )]
  print(data.desc[,.N,by=.(education,get(nm.var))][order(education,N)])
  
  # Municipality
  nm.var <- " To continue, please enter the code that is in your letter (4 digits followed by 3 letters)"
  if (nm.var %in% names(data.desc)){
    data.desc[, muni := fcase(
      grepl("15",get(nm.var)), "Hoorn",
      grepl("37",get(nm.var)), "Medemblik",
      default = "Other"
    )]
    print(data.desc[,.N,by=.(muni,get(nm.var))][order(muni,N)])
  }
  
  # Home improvement
  nm.vars <- names(data.desc)[grepl("Do you have solar panels",names(data.desc)) | (grepl("improvements",names(data.desc)) & grepl("insulation",names(data.desc)))]
  data.desc[,nb_house_improvement := as.character(sum(.SD == "Yes",na.rm=TRUE)),by=` Response ID`,.SDcols = nm.vars]
  data.desc[nb_house_improvement >= 3,nb_house_improvement := "3+"]
  
  ## Map descriptives ----
  # Read JSON mapping descriptives
  fljson <- paste0("input/mapping_descriptives.json")
  mapping <- jsonlite::read_json(fljson)
  
  
  # Loop over each question and rename
  for (new_q_name in names(mapping)) {
    
    old_q_names <- unlist(mapping[[new_q_name]][["question_tags"]])
    
    old.nms <- names(data.desc)
    new.nms <- old.nms
    
    if (!any(new.nms %in% old_q_names)) {
      warning(paste0('`',new_q_name,"` not mapped"))
      next
    }
    new.nms[new.nms %in% old_q_names] <- new_q_name
    if (length(old.nms) != length(new.nms)) stop('new and old names should have same lengths')
    names(data.desc) <- new.nms
    
    # Merge duplicated columns
    if (any(duplicated(names(data.desc)))){
      setnames(data.desc,make.unique(names(data.desc)))
      base_names <- gsub("\\.\\d+$", "", names(data.desc))
      dup_names <- unique(base_names[duplicated(base_names)])
      for (name in dup_names) {
        cols_to_merge <- names(data.desc)[base_names == name]
        data.desc[, (name) := do.call(fcoalesce, .SD), .SDcols = cols_to_merge]
      }
      
      cols_to_drop <- names(data.desc)[base_names %in% dup_names]
      cols_to_keep <- setdiff(names(data.desc), cols_to_drop)
      cols_to_keep <- union(cols_to_keep, dup_names)  # keep merged columns
      data.desc <- data.desc[, ..cols_to_keep]
    }
    
    # Loop over each level and rename
    if ("answers" %in% names(mapping[[new_q_name]])){
      for (new_answer_name in names(mapping[[new_q_name]][["answers"]])) {
        old_answer_names <- unlist(mapping[[new_q_name]][["answers"]][[new_answer_name]]$answer_tags)
        
        data.desc[get(new_q_name) %in% old_answer_names, (new_q_name) := new_answer_name]
        
      }
    }
    
  }
  
  grp.q <- sapply(mapping, \(x) x$question_group)
  grp.q.omit <- unlist(sapply(mapping, \(x) x$question_mask))
  grp.q.omit <- grp.q.omit[grp.q.omit == 1]
  
  grp.q <- grp.q[names(grp.q)[names(grp.q) %in% names(grp.q.omit)]]
  ## Descriptives tables ----
  for (grp in unique(unlist(grp.q))){
    if ("grp" %in%  names(unlist(grp.q.omit))) next
    
    if (grp == "bias_monet"){
      question_cols <- names(grp.q[grp.q == grp])
      for (monet_question in question_cols){
        if (!(monet_question %in% names(data.desc))) next
        tab <- data.desc[,.(Answer = get(monet_question))]
        
        if (grepl("50 per month",monet_question)){
          nm.fig <- "wtp_50_savings_per_month"
          bd <- 10
          mx <- 10000
        }else if (grepl("lottery",monet_question)){
          nm.fig <- "value_1_year_wait"
          bd <- 10
          mx <- 1000
        } else {stop(sprintf("Give name to question `%s`",monet_question))}
        
        median_val <- median(tab$Answer, na.rm = TRUE)
        
        ggplot(tab, aes(x = Answer)) +
          geom_histogram(binwidth = bd, fill = grey2, col = grey3) +
          scale_x_continuous(limits = c(NA, mx)) +
          theme.graphs +
          labs(
            x = stringr::str_to_sentence(gsub("\\_", " ", nm.fig)),
            title = paste0("Median: ", round(median_val, 2))
          )
        
        
        ggsave(paste0(dir.out.figs, paste0("fig_",nm.fig), ".png"), units = "cm",height = height, width = width)
        
        tab.mean <- tab[,.(Mean = mean(Answer,na.rm=TRUE),Median = median(Answer,na.rm=TRUE))]
        
        sink(paste0(dir.out,nm.fig, ".tex"))
        print(xtable::xtable(tab.mean), type = "latex")
        sink()
        
      }
      
      next
    }
    
    # Subset question columns by group
    question_cols <- names(grp.q[grp.q == grp])
    question_cols <- question_cols[question_cols %in% names(data.desc)]
    
    # Melt only those columns
    dt_long <- melt(data.desc[, ..question_cols], 
                    measure.vars = question_cols,
                    variable.name = "Question", value.name = "Answer")
    
    # Count answer frequencies
    tab <- dt_long[, .N, by = .(Question, Answer)][order(Question, -N)]
    tab[,Share := sprintf("%.0f%% (%.d)",100*N/sum(N),N),by=Question]
    
    tab <- tab %>% purrr::map_df(rev) %>% as.data.table()
    #tab[, Share := gsub("%", "\\\\%", Share)]
    
    # Add number to positive negative scale
    scale.list <- list(pos.neg = c("Very negative","Negative","Neutral","Positive","Very positive"),
                       shares = c("Almost no one", "Less than a quarter","Less than half", "About half", "More than half","More than three quarters","Almost everyone")
    )
    for (nm in names(scale.list)){
      tab[!is.na(match(Answer,scale.list[[nm]])),Answer := paste0(match(Answer,scale.list[[nm]])," - ",Answer)]  
    }
    
    
    # Re order answers within each question
    tab <- tab[, .SD[order(Answer)], by = Question]
    
    tab[,Answer := gsub("[^\x01-\x7F]", "", Answer)] 
    
    
    # Blank out repeated question values for LaTeX formatting
    tab[, Question_display := ifelse(.I == 1 | Question != shift(Question), Question, "")]
    
    # Remove scale question instruction
    tab[,Question_display := gsub("On a scale from 1 to 5, to what extent do you agree with the following statements? ","",Question_display,fixed = TRUE)]
    
    
    tab.body <-  tab[, .(Descriptives = c(as.character(head(Question,1)), Answer),Share = c("", Share)), by = .(Group = Question)][,-"Group"]
    latex_body <- knitr::kable(tab.body,
                               format = "latex",
                               longtable  = TRUE,
                               booktabs = TRUE) %>%  kableExtra::kable_styling(latex_options = "basic")
    latex_body <- paste0("\\scriptsize",latex_body)
    lines <- strsplit(latex_body, "\n")[[1]]
    lines <- lines[lines != "\\addlinespace"]
    idx <- which(grepl("^\\ \\&",lines) & !shift(grepl("^\\ \\&",lines), type = "lead"))
    for (i in rev(idx)){
      lines <- append(lines, "\\addlinespace", after = i)
    }
    
    # Indent levels
    idx.lvl <- grepl("\\& .*\\(.*\\)",lines)
    lines[idx.lvl] <- paste0("\\hspace{1em} ",lines[idx.lvl])
    
    writeLines(paste(lines, collapse = "\n"), paste0(dir.out,"descriptives_",grp,".tex"))
    
  }
  
  # Biases figure ----
  grp.ht.list <- fromJSON("input/mapping_bias_figure.json")
  
  for (bs in names(grp.ht.list)){
    col.bias <- gsub("desc_","",unlist(grp.ht.list[[bs]]))
    for (muni in data.desc[,unique(` Municipality`)]){
      dt.bais <- data.desc[get(" Municipality") == muni,.SD,.SDcols =col.bias]
      setnames(dt.bais,col.bias,names(grp.ht.list[[bs]]))
      #setnames(dt.bais,col.bias,sub(".*\\[(.*)\\].*", "\\1", col.bias))
      
      dt.long <- melt(dt.bais,
                      measure.vars = names(dt.bais),
                      variable.name = "Question",
                      value.name = "Response"
      )
      
      # Map sentiment responses to agreement scale
      dt.long[, Response := fcase(
        Response == "1 - Very negative",       "1 - strongly disagree",
        Response == "2 - Negative",            "2",
        Response == "3 - Neutral",             "3 - neither agree nor disagree",
        Response == "4 - Positive",            "4",
        Response == "5 - Very positive",       "5 - strongly agree",
        default = Response  # keep existing values as-is
      )]
      
      # Count per Question + Response
      dt.prop <- dt.long[, .N, by = .(Question, Response)]
      dt.prop[, Share := N / sum(N), by = Question]
      
      # Questions order 
      dt.prop[, Question := factor(Question, levels = rev(names(grp.ht.list[[bs]])))]
      
      # Reverse factor levels to get "strongly disagree" first (bottom/left)
      dt.prop[, Response := factor(Response, levels = rev(sort(unique(dt.prop$Response))))]
      
      # Try multiple widths and find the minimum one that gives max 2 lines
      for (w in 20:80) {
        dt.prop[, Question_wrapped := stringr::str_wrap(Question, width = w)]
        max_lines <- max(stringr::str_count(dt.prop$Question_wrapped, "\n") + 1)
        
        if (max_lines <= 2) {
          cat("Minimum width for max 2 lines:", w, "\n")
          
          # Re-factor to preserve ordering
          dt.prop[, Question_wrapped := factor(Question_wrapped, 
                                               levels = unique(Question_wrapped[order(Question)]))]
          break
        }
      }
      
      ggplot(dt.prop, aes(x = Question_wrapped, y = Share, fill = Response)) +
        geom_bar(stat = "identity", position = "stack", color = "white") +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          sec.axis = dup_axis(trans = ~1 - ., labels = scales::percent_format(accuracy = 1), name = NULL)
        ) +
        scale_fill_brewer(palette = "RdYlBu", direction = -1) +
        labs(
          x = NULL,
          y = "Share of responses",
          fill =NULL  
        ) +
        coord_flip() +
        theme_pong + 
        theme(
          legend.position = c(0.25, -0.2),  # x < 0 moves left outside plot; y < 0 moves below plot
          legend.direction = "horizontal",
          plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
        ) + 
        guides(fill = guide_legend(reverse = TRUE))
      
      
      ggsave(paste0(dir.out.figs,"fig_desc_biases",bs,"_",muni,".png"),width = width*1.6,height = height, units = "cm")
      
    }
  }
  # Descriptives figures ----
  clean_str <- \(x) stringr::str_to_sentence(gsub("_"," ",x))
  
  plot_percent_bar <- function(data, var_name_str, max_limit = 1.05) {
    var_sym <- sym(var_name_str)
    
    ggplot(data, aes(x = !!var_sym)) +
      geom_bar(aes(y = (..count..) / sum(..count..))) +
      geom_text(aes(
        label = scales::percent((..count..) / sum(..count..), accuracy = 1),
        y = (..count..) / sum(..count..)
      ), stat = "count", hjust = -0.1, size = font.axis.lbls * 0.35) +
      coord_flip(clip = "off") +
      labs(
        x = NULL,
        y = NULL,
        fill = NULL,
        title = stringr::str_wrap(var_name_str, width = 50)
      ) + 
      theme_pong +
      theme(
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10)
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        name = "Share of respondents",
        limits = c(0, max_limit)
      )
  }
  
  
  grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")
  
  data.desc.adhoc <- copy(data.desc)
  names(data.desc.adhoc) <- trimws(names(data.desc.adhoc))
  
  for (grp.ht in names(grp.ht.list)){
    
    for (desc in grp.ht.list[[grp.ht]]){
      
      colnm <- gsub("desc_", "", desc)
      if (!(colnm %in% names(data.desc.adhoc))) next
      dt.plt <- data.desc.adhoc[, .(col = get(colnm))]
      
      new.nm <- names(grp.ht.list[[grp.ht]][which(grp.ht.list[[grp.ht]] == desc)])
      setnames(dt.plt, "col", new.nm)
      
      # Calculate max limit (e.g., 5% above max proportion or fixed upper bound)
      max_limit <- max((table(dt.plt[[new.nm]]) / nrow(dt.plt))) * 1.1
      
      p <- plot_percent_bar(dt.plt, new.nm, max_limit = max_limit)
      
      fig.nm <- paste0("fig_",grp.ht,"_",new.nm)
      fig.nm <- gsub("[^A-Za-z0-9]", "_", fig.nm)
      ggsave(paste0(dir.out.figs,fig.nm,".png"),p,width = width,height = height, units = "cm")
      
    }
  }
  
  # Results ----
  # Replace missings with Hoorn
  varnm <- data.desc[survey_name == hoorn.file.name,sapply(.SD,\(x) all(is.na(x)))]
  varname.not.hoorn <- names(varnm[varnm == TRUE])
  print(varname.not.hoorn)
  data.desc[,(varname.not.hoorn) := lapply(.SD,as.character),.SDcols = varname.not.hoorn]
  data.desc[survey_name == hoorn.file.name,(varname.not.hoorn) := "Hoorn"]
  
  # Convert to plain data.frame and clean names
  names(data.desc) <- trimws(names(data.desc))          # remove spaces around names
  names(data.desc) <- gsub("\\s+", " ", names(data.desc))  # replace spaces with underscores
  
  
  options(modelsummary_format_numeric_latex = "plain")
  #options(modelsummary_align = "ldd")
  options(modelsummary_factory_latex = 'kableExtra')
  
  # Data args
  cst <- list(id_q = "id_question",
              id_r = "id_respondent",
              ch = "choice",
              pk = "package")
  
  # logit args
  ref.lvl <- "_1$"
  stats.table <- c("S.E." = "({std.error}){stars}")
  
  # WTP args
  savings.lvl = 1
  invest.name = "one_time_amount"
  savings.name = "heating_costs"
  ref.invest.wtp = 2 # Investment cost reference level, also used for simulations
  
  # IRR args
  irr.years <- c(1,5,10,15,25) # Horizon for IRR calculation
  irr.lvl = 3 # Energy savings reference level 
  
  
  # Save descriptives ----
  write.csv2(data.desc,paste0(dir.out,"data_descriptives.csv"),quote = TRUE,row.names = FALSE)
  
  # Store monetary coefficients (vom value of money)
  dt.coefs <- data.table()
  
  ## Insulation results ----
  game <- "insu"
  dt.insu <- get_df_logit(data,game,cst,ref.lvl,data.desc)
  
  
  write.csv2(dt.insu,paste0(dir.out,"data_mlogit_insu.csv"),quote = TRUE,row.names = FALSE)
  
  col.att.int <- names(dt.insu)[!(names(dt.insu) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","idx")) & !grepl("^desc_|_continuous$",names(dt.insu))]
  
  if (bin.costs.cont == TRUE){
    col.att.int <- col.att.int[!grepl(reg.costs,col.att.int)]
    col.att.int <- c(col.att.int,names(dt.insu)[grepl(reg.costs,names(dt.insu)) & grepl("_continuous$",names(dt.insu)) ])
  }
  
  formula.insu <- paste0("choice ~ ",paste0(col.att.int,collapse = " + ")," | 0 ")
  
  logit_interact.insu <- mlogit(
    formula = as.formula(formula.insu),
    dt.insu
  )
  
  print(summary(logit_interact.insu,digits=2))
  
  col.ref <- paste0(unique(gsub("_[0-9]$","",col.att.int[grepl("_[0-9]$",col.att.int)])),gsub(".*(_[0-9]).*","\\1",ref.lvl))
  coef_names <- get_json_mapping_4_coefs(game,c(col.att.int,col.ref))
  
  coef_names <- translate_coef_names(coef_names,game,lang=lang)
  
  rows.refs <- get_row_refs(coef_names,col.att.int,col.ref,ref.lvl,stats.table)
  
  
  tab.tex <- modelsummary::modelsummary(list(Logit = logit_interact.insu), 
                                        coef_map = coef_names[names(coef_names) %in% col.att.int], 
                                        add_rows = rows.refs,
                                        shape = term ~ statistic,
                                        estimate = "{estimate}",
                                        statistic = stats.table,
                                        output = "latex",
                                        stars = c("*" = .1, "**" = .05, "***" = 0.01))
  
  
  tab.tex <- fix_midrules(tab.tex)
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
  dt.cfs.tmp[,group := "all"]
  dt.cfs.tmp <- map_levels_to_keys(
    dt = dt.cfs.tmp,
    game = game,
    lang = lang
  )
  dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
  
  ## Insulation heterogeneity ----
  
  grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")
  
  for (grp.ht in names(grp.ht.list)){
    # With reference level
    run_ht_insu_with_ref_level(grp.ht.list[[grp.ht]],grp.ht)
    
    # Without reference level
    for (ht.var in names(grp.ht.list[[grp.ht]])){
      
      if (!(grp.ht.list[[grp.ht]][[ht.var]] %in% names(dt.insu))) next
      
      logit_interact.insu.ht <- mlogit(
        formula = as.formula(gsub("none",paste0("`",grp.ht.list[[grp.ht]][[ht.var]],"`:none"),formula.insu)),
        dt.insu
      )
      
      tab.tex <- modelsummary::modelsummary(list(Logit = logit_interact.insu.ht), 
                                            coef_rename = coef_names[names(coef_names) %in% col.att.int], 
                                            add_rows = rows.refs,
                                            shape = term ~ statistic,
                                            estimate = "{estimate}",
                                            statistic = stats.table,
                                            output = "latex",
                                            stars = c("*" = .1, "**" = .05, "***" = 0.01))
      
      tab.tex <- fix_midrules(tab.tex)
      
      writeLines(tab.tex, paste0(dir.out, "results_heterogeneity_no_reference_", game,"_",ht.var, ".tex"))
      
      dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
      dt.cfs.tmp[,group := ht.var]
      dt.cfs.tmp[grepl(":None",Term),Term := gsub(grp.ht.list[[grp.ht]][[ht.var]],paste0(ht.var," "),Term,fixed = TRUE)]
      
      dt.cfs.tmp <- map_levels_to_keys(
        dt = dt.cfs.tmp,
        game = game,
        lang = lang
      )
      
      dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
      
    }
  }
  
  ## Tech results ----
  game <- "tech"
  dt.tech <- get_df_logit(data,game,cst,ref.lvl,data.desc)
  
  write.csv2(dt.tech,paste0(dir.out,"data_mlogit_tech.csv"),quote = TRUE,row.names = FALSE)
  
  col.mdl <- names(dt.tech)[!(names(dt.tech) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","idx")) & !grepl("(^desc_)|(^support_)",names(dt.tech))]
  if (bin.costs.cont == TRUE){
    col.mdl <- col.mdl[!grepl(reg.costs,col.mdl)]
    col.mdl <- c(col.mdl,names(dt.tech)[grepl(reg.costs,names(dt.tech)) & grepl("continuous$",names(dt.tech)) ])
  }
  col.interact.tech <- col.mdl[!grepl("^none$|^tech$|^heating",col.mdl)]
  col.rest <- setdiff(col.mdl,col.interact.tech)
  col.rest[col.rest == "tech"] <- "factor(tech)"
  
  formula.tech <- paste0("choice ~ ",paste0(col.rest[col.rest != "none"],collapse = " + ")," + ",paste0(paste0(col.interact.tech,"*factor(tech)"),collapse = " + ")," + none| 0 ")
  #formula.tech <- gsub("one_time_costs_continuous\\*factor\\(tech\\)","one_time_costs_continuous:factor(tech)",formula.tech)
  
  logit_interact.tech <- mlogit(
    formula = as.formula(formula.tech),
    dt.tech
  )
  
  print(summary(logit_interact.tech,digits=2))
  
  col.att.int <- names(logit_interact.tech$coefficients)
  col.ref <- paste0(unique(gsub("_[0-9]$","",col.att.int[grepl("_[0-9]$",col.att.int) & !grepl("factor\\(tech\\)",col.att.int)])),gsub(".*(_[0-9]).*","\\1",ref.lvl))
  
  tech.map <- get_json_mapping_4_coefs(game,c(col.att.int,col.ref))
  tech.map[names(tech.map) == "factor(tech)1"] = "Heat pump"
  tech.map <- c(tech.map[!grepl("^(Heat net)|(Heat pump)|(None)",tech.map)],
                tech.map[grepl("^(Heat pump)",tech.map)],
                tech.map[grepl("^(Heat net)",tech.map)],
                tech.map[grepl("^(None)",tech.map)])
  
  tech.map <- translate_coef_names(tech.map,game,lang=lang)
  
  rows.refs <- get_row_refs(c(tech.map[names(tech.map) %in% col.att.int],tech.map[names(tech.map) %in% col.ref]),col.att.int,col.ref,ref.lvl,stats.table)
  
  tab.tex <- modelsummary::modelsummary(list(Logit = logit_interact.tech), 
                                        coef_map = tech.map[names(tech.map) %in% col.att.int], 
                                        add_rows = rows.refs,
                                        shape = term ~ statistic,
                                        estimate = "{estimate}",
                                        statistic = stats.table,
                                        output = "latex",
                                        stars = c("*" = .1, "**" = .05, "***" = 0.01))
  
  tab.tex <- fix_midrules(tab.tex)
  
  dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
  dt.cfs.tmp[,group := "all"]
  dt.cfs.tmp <- map_levels_to_keys(
    dt = dt.cfs.tmp,
    game = game,
    lang = lang
  )
  dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  ## Tech heterogeneity ----
  grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")
  
  for (grp.ht in names(grp.ht.list)){
    # With reference 
    run_ht_tech_with_ref_level(grp.ht.list[[grp.ht]],grp.ht)
    
    # Without reference level
    for (ht.var in names(grp.ht.list[[grp.ht]])){
      
      var_in_dt <- grp.ht.list[[grp.ht]][[ht.var]]
      if (!(var_in_dt %in% names(dt.tech))) next
      
      logit_interact.tech.ht <- mlogit(
        formula = as.formula(gsub("(none|None)",paste0("`",var_in_dt,"`:none"),formula.tech)),
        dt.tech
      )
      
      tab.tex <- modelsummary::modelsummary(list(Logit = logit_interact.tech.ht), 
                                            coef_rename = tech.map[names(tech.map) %in% col.att.int], 
                                            add_rows = rows.refs,
                                            shape = term ~ statistic,
                                            estimate = "{estimate}",
                                            statistic = stats.table,
                                            output = "latex",
                                            stars = c("*" = .1, "**" = .05, "***" = 0.01))
      
      tab.tex <- fix_midrules(tab.tex)
      
      dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
      dt.cfs.tmp[,group := ht.var]
      dt.cfs.tmp[grepl(":(None|none)",Term),Term := gsub(var_in_dt,paste0(ht.var," "),Term,fixed = TRUE)]
      dt.cfs.tmp <- map_levels_to_keys(
        dt = dt.cfs.tmp,
        game = game,
        lang = lang
      )
      
      # Get n obs hetero
      dt.n <- data.table(dt.tech %>%
                           group_by(V1 = .data[[var_in_dt]]) %>%
                           summarise(count = n_distinct(`desc_Response ID`)))
      dt.n[, key_term := paste0("none_",ht.var," ",V1)]
      dt.cfs.tmp[dt.n,n_respondents := i.count,on = "key_term" ]
      
      # Statistical test
      dt.none <- dt.cfs.tmp[grepl(":none",Term),]
      idx.ref <- dt.none[,first(which(n_respondents == max(n_respondents)))]
      
      nm.ref.in.logit <- names(logit_interact.tech.ht$coefficients)[gsub("[^[:alnum:]]", "", names(logit_interact.tech.ht$coefficients)) == gsub("[^[:alnum:]]", "", gsub(paste0(ht.var," "),var_in_dt,dt.none[idx.ref,Term]))] 
      for (none_not_ref in dt.none[-idx.ref,Term]){
        nm.noref.in.logit <- names(logit_interact.tech.ht$coefficients)[gsub("[^[:alnum:]]", "", names(logit_interact.tech.ht$coefficients)) == gsub("[^[:alnum:]]", "", gsub(paste0(ht.var," "),var_in_dt,none_not_ref))] 
        
        if (length(length(nm.noref.in.logit))!=1 | length(length(nm.ref.in.logit))!=1){
          stop("None coefficient not properly identified")
        }
        
        tst <- linearHypothesis(logit_interact.tech.ht, paste0(nm.ref.in.logit," = ",nm.noref.in.logit))
        dt.cfs.tmp[Term == none_not_ref,pval_none := tst$`Pr(>Chisq)`[2]]
      }
      
      dt.cfs.tmp[Term == dt.none[idx.ref,Term],pval_none := Inf]
      
      #
      dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
      
    }
  }
  
  
  # Coef table ----
  
  print(dt.coefs)
  
  # Step 1: Extract attribute group name (everything before the colon)
  dt.coefs[, Attribute := sub(":.*", "", Term)]
  dt.coefs[grepl("(None|none)$",key_term),Attribute := "None"]
  
  # Step 2: Extract numeric cost values for cost-related rows (including references)
  
  dt.coefs[, CostValue := NA_real_]
  dt.coefs[grepl(reg.costs, key_term, ignore.case = TRUE), 
           CostValue := sapply(Term, extract_cost_value)]
  
  # Step 3: Find reference CostValue per Attribute group
  # Reference rows have "(reference)" in Term
  refs <- dt.coefs[grepl("\\((reference)|(referentie)\\)", Term), .(ReferenceCost = CostValue[!is.na(CostValue)][1]), by = .(Attribute,game,group)]
  
  # Step 4: Join reference costs back
  dt.coefs <- merge(dt.coefs, refs, by = c("Attribute","game","group"), all.x = TRUE)
  
  # Step 5: Calculate relative cost level for cost rows (excluding references)
  dt.coefs[, CostLevelRelative := NA_real_]
  dt.coefs[grepl(reg.costs, key_term, ignore.case = TRUE), 
           CostLevelRelative := CostValue - ReferenceCost]
  
  
  dt.coefs <- dt.coefs[order(group,game,grepl("cost",Attribute),Attribute),]
  
  # Predictions ----
  dt.grid <- data.table()
  non_financials_list <- list(baseline = NULL,
                              power = "power_outages_2",
                              supplier = "supplier_2",
                              power_supplier = c("power_outages_2","supplier_2"))
  
  gm = "tech"
  for (charac in dt.coefs[game == gm,unique(group)] ){ #dt.coefs[game == gm,unique(group)]
    for (none_coef in dt.coefs[group == charac & grepl("^none(_.*)?$",key_term) & game == gm,key_term]){
      for (nf in names(non_financials_list)){
        
        if (bin.costs.cont == FALSE){
          warning("Not implemented for discrete costs")
          next
        }
        dt.pw <- dt.coefs[group == charac & game == gm & (grepl(reg.costs,key_term) | key_term == none_coef) ,]
        
        savings <- \(x) x*dt.pw[key_term == "heating_costs_continuous",Estimate] +
          (x)*dt.pw[key_term == "heating_costs_negativecontinuous",Estimate]*ifelse(x < 0,1,0)
        
        invest <- \(x) x*dt.pw[key_term == "one_time_costs_continuous",Estimate]*ifelse(x < 6 | x > 10,1,0) +
          (x)*dt.pw[key_term == "factor(tech)1:one_time_costs_continuous",Estimate]*ifelse(x > 10,1,0) +
          6*dt.pw[key_term == "one_time_costs_continuous",Estimate]*ifelse(x >= 6 & x <=10,1,0)
        
        none_fct <- \(x) dt.coefs[key_term == none_coef & game == gm & group == charac,Estimate] -
          dt.coefs[key_term == "factor(tech)1" & game == gm & group == charac,Estimate]*(ifelse(x > 10,1,0) + 0*ifelse(x > 6 & x < 10,1,0)*(x-6)/4)
        
        if (length(non_financials_list[[nf]]) == 0){
          non_financials <- \(x) 0
        } else {
          kynf <- paste0(unlist(non_financials_list[nf]),collapse = "|")
          non_financials <- \(x) ifelse(x > 10,dt.coefs[grepl(kynf,key_term) & game == gm & group == charac,sum(Estimate)],dt.coefs[grepl(kynf,key_term) & !grepl("factor\\(tech\\)1",key_term) & game == gm & group == charac,sum(Estimate)])
        }
        
        share_predicted <- \(sav,inv) exp(invest(inv) + savings(sav) + non_financials(inv))/(exp(invest(inv) + savings(sav) + non_financials(inv))+exp(none_fct(inv)))
        
        inv_vals <- seq(0, 30, by = 2)
        sav_vals <- seq(-0.6, 2.4, by = 0.6)
        
        grid <- expand.grid(inv = inv_vals, sav = sav_vals)
        setDT(grid)
        
        # Compensation to arrive at 50 and 70%
        for (trgt in c(0.5,0.7)){
          root <- uniroot(\(x) share_predicted(0,x) - trgt, interval = c(-100, 100))
          grid[,(paste0("price_",round(100*trgt),"_approval")) := root$root]
          
        }
        
        grid[,n_respondents := dt.pw[key_term == none_coef,n_respondents]]
        grid[,pval_none := dt.pw[key_term == none_coef,pval_none]]
        grid[,Estimate := dt.pw[key_term == none_coef,Estimate]]
        
        grid$share_predicted <- with(grid,
                                     exp(invest(inv) + savings(sav)) /
                                       (exp(invest(inv) + savings(sav)) + exp(none_fct(inv)))
        )
        
        # Predicted support heterogeneity for a 50 and 70% average support package
        # if (nf == "baseline"){
        #   package.support.ht <- list("50prct_no_savings_low_cost" = c(0,4.303483),
        #                              "44prct_2400_savings_high_cost" = c(2.4,20))
        # } else if (nf == "power"){
        #   package.support.ht <- list("50prct_no_savings_low_cost" = c(0,3.199034),
        #                              "44prct_2400_savings_high_cost" = c(2.4,15.48219))
        # } else if (nf == "power_supplier"){
        #   package.support.ht <- list("50prct_no_savings_low_cost" = c(0,3.601989),
        #                              "44prct_2400_savings_high_cost" = c(2.4,13.97133))
        # } else {
        #   stop("Package not defined for this non financial scenario")
        # }
        
        package.support.ht <- list("50prct_no_savings_low_cost" = c(0,4.29),
                                   "44prct_2400_savings_high_cost" = c(2.4,20))
        
        grid[, (paste0("support_",names(package.support.ht))) := lapply(package.support.ht, function(v) share_predicted(v[1], v[2]))]
        
        grp <- gsub("^none$","All",gsub("^none_","",none_coef))
        
        grid[, c("game", "hetero","group","attributes_non_financials") := .(gm, charac,grp,nf)]
        
        dt.grid <- rbindlist(list(dt.grid,grid),use.names = TRUE,fill = TRUE)   
      }
    }
  }
  
  if (dt.grid[,.N,by=.(game,hetero,group,inv,sav,attributes_non_financials)][,any(N != 1)]){stop("Duplicated predictions")}
  
  # Heterogeneity table ----
  
  #export_WTP_heterogeneity  = dt.grid[inv == 0 & sav == 0,.(Characteristic = hetero,Group = gsub(hetero,"",group),`Predicted support` = paste0(round(share_predicted*100),"%"),`Median WTP unburdening` = round(price_50_approval,1),`None` = round(Estimate,2),`None p-value` = ifelse(!is.finite(pval_none),"Reference",as.character(round(pval_none,2))),`N respondents` = n_respondents),by=.I][,-"I"]
  export_WTP_heterogeneity  = dt.grid[inv == 4 & sav == 0 & attributes_non_financials == "baseline",.(Characteristic = hetero,Group = gsub(hetero,"",group),`Predicted support` = paste0(round(share_predicted*100),"%"),`None` = round(Estimate,2),`None p-value` = ifelse(!is.finite(pval_none),"Reference",as.character(round(pval_none,2))),`N respondents` = n_respondents),by=.I][,-"I"]
  export_WTP_heterogeneity <- export_WTP_heterogeneity[order(-Characteristic),]
  
  cat(
    paste0(
      "\\tiny\n",
      kable(export_WTP_heterogeneity, 
            format = "latex", 
            booktabs = FALSE, 
            longtable = TRUE, 
            linesep = "", 
            caption = "WTP Heterogeneity Table")
    ),
    file = paste0(dir.out, "WTP_heterogeneity_table.tex")
  )
  
  
  # Heat map ----
  support_colors <- c(
    "0–10%"   = "#d73027",  # red
    "10–20%"  = "#f46d43",  # orange-red
    "20–30%"  = "#fdae61",  # orange
    "30–40%"  = "#fee08b",  # light yellow
    "40–50%"  = "#ffffbf",  # bright yellow
    "50–60%"  = "#a6d96a",  # light green
    "60–70%"  = "#66bd63",  # medium green
    "70–80%"  = "#1a9d4c",  # darker green
    "80–90%"  = "#006837",  # deep green
    "90–100%" = "#004529"   # very dark green
  )
  
  dt.grid[, share_cat := cut(share_predicted,
                             breaks = seq(0, 1, by = 0.1),  # 0.0, 0.1, 0.2, …, 1.0
                             labels = names(support_colors),
                             include.lowest = TRUE,
                             right = TRUE
  )]
  
  
  
  
  dt.grid$inv_f <- factor(dt.grid$inv)
  dt.grid$sav_f <- factor(dt.grid$sav)
  
  # Custom y-axis label function
  y_labels <- function(y) {
    y_num <- as.numeric(y) * 1000       # annual savings in euros
    monthly <- round(y_num / 12)        # monthly savings
    paste0(round(y_num / 1000, 1), "k (", monthly, "/maand)")
  }
  
  # Custom x-axis label function
  x_labels <- function(x) {
    x_num <- as.numeric(x) * 1000       # convert factor to numeric euro amount
    paste0(round(x_num / 1000, 0), "k") # show in 'k' format
  }
  
  charac = "all"
  grp = "All"
  
  # Heat map
  ggplot(dt.grid[group == grp & attributes_non_financials == "baseline",], aes(x = inv_f, y = sav_f, fill = share_cat)) +
    geom_tile(color = "white", width = 0.9, height = 0.9) +
    scale_fill_manual(
      values = support_colors,
      name = "Draagvlak in %"  
    ) +
    scale_x_discrete(labels = x_labels) +  
    scale_y_discrete(labels = y_labels) + 
    coord_equal() +
    labs(
      title = NULL,
      x = "Eenmalige investeringskosten",
      y = "Besparingen per jaar"      
    ) +
    theme_minimal(base_size = sz.txt) +
    theme(
      axis.text.y = element_text(hjust = 0),  # left-align y-axis labels
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = NA, color = NA)
    )
  
  
  ttl = if_else(charac == "all","All",grp)
  fig.nm <- paste0("fig_hm_",gm,"_",ttl)
  fig.nm <- gsub("[^A-Za-z0-9]", "_", fig.nm)
  
  ggsave(paste0(dir.out.figs,fig.nm,".png"),width = width,height = height, units = "cm")
  
  # Frontier ----
  # Keep only 50%
  dt.front <- dt.grid[share_cat == "50–60%",.(sav_f = min(sav)),by=.(game,hetero,group,inv_f)][order(game,hetero,group,as.numeric(inv_f),sav_f),]
  dt.front[,sav_f := as.factor(sav_f)]
  
  # add bottom right corner
  dt.front[,shift_invest := NULL]
  dt.front[,shift_invest := ifelse(sav_f != lead(sav_f) & game == lead(game) & hetero == lead(hetero) & group == lead(group),
                                   as.numeric(as.character(lead(inv_f))),NA_real_)]
  
  
  dt.front <- rbindlist(list(dt.front,dt.front[!is.na(shift_invest),.(game,hetero,group,inv_f = shift_invest,sav_f)]),fill = TRUE,use.names = TRUE)
  dt.front <- dt.front[order(game,hetero,group,as.numeric(inv_f),sav_f),]
  dt.front[group == "Municipality Medemblik",]
  
  ht <- "Municipality"
  ggplot(dt.front[hetero == ht & group != "Municipality Other",],
         aes(x = as.numeric(as.character(inv_f)), y = as.numeric(as.character(sav_f)),color = group)) +
    geom_line(size = 2) +     labs(
      title = NULL,
      x = "Eenmalige investeringskosten",
      y = "Besparingen per jaar",
      color = "50% support lines"
    ) +
    scale_x_continuous(breaks = dt.front[,unique(as.numeric(as.character(inv_f)))] ) + 
    scale_y_continuous(breaks = dt.front[,unique(as.numeric(as.character(sav_f)))] ) + 
    theme_minimal(base_size = sz.txt) +
    theme(
      axis.text.y = element_text(hjust = 0),  # left-align y-axis labels
      axis.text.x = element_text(angle = 45, hjust = 1),
      #panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = NA, color = NA)
    )
  
  # Packages average predictions ----
  
  # 1 - low cost package 
  dt.s <- rbindlist(list(unique(dt.grid[group == "All" & attributes_non_financials %in% c("baseline","power"),],by=c("game","hetero","group","attributes_non_financials"))[,.(game,hetero,group,attributes_non_financials,predicted = support_50prct_no_savings_low_cost,scenario = "support_50prct_no_savings_low_cost")],
                         unique(dt.grid[group == "All" & attributes_non_financials %in% c("baseline","power_supplier"),],by=c("game","hetero","group","attributes_non_financials"))[,.(game,hetero,group,attributes_non_financials,predicted = support_44prct_2400_savings_high_cost,scenario = "support_44prct_2400_savings_high_cost")]))
  
  dt.s[scenario == "support_50prct_no_savings_low_cost", dutch_scenario := "Warmte-aanbod 4Keuro investering, 0 euro/maand besparing"]
  dt.s[scenario == "support_44prct_2400_savings_high_cost", dutch_scenario := "Warmte-aanbod 20Keuro investering, 200euro/maand besparing"]
  
  dt.s[scenario == "support_50prct_no_savings_low_cost" & attributes_non_financials == "baseline", dutch_attributes_non_financials := "Nooit stroomstoring"]
  dt.s[scenario == "support_50prct_no_savings_low_cost" & attributes_non_financials == "power", dutch_attributes_non_financials := "1 dag/jaar stroomstoring"]
  dt.s[scenario == "support_44prct_2400_savings_high_cost" & attributes_non_financials == "baseline", dutch_attributes_non_financials := "Nooit stroomstoring - Vrije keuze warmteleverancier"]
  dt.s[scenario == "support_44prct_2400_savings_high_cost" & attributes_non_financials == "power_supplier", dutch_attributes_non_financials := "1 dag/jaar stroomstoring - Geen vrije keuze"]
  
  dt.s[,dutch_scenario := factor(dutch_scenario,levels = unique(dutch_scenario))]
  
  dt.s[, share_cat_ht := cut(predicted,
                             breaks = seq(0, 1, by = 0.1),  # 0.0, 0.1, 0.2, …, 1.0
                             labels = names(support_colors),
                             include.lowest = TRUE,
                             right = TRUE
  )]
  ggplot(dt.s, aes(x = dutch_attributes_non_financials, y = predicted,fill = share_cat_ht)) +
    geom_col() +
    scale_fill_manual(
      values = support_colors,
      name = "Draagvlak in %"  
    ) +
    #geom_hline(yintercept = as.numeric(gsub(".*([0-9]{2})prct.*","\\1",scenario))/100) + 
    coord_flip() +
    scale_y_continuous(limits = c(0,0.8),labels = scales::percent) + 
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = sz.txt) +
    guides(
      fill = guide_legend(
        title.position = "top",
        label.position = "bottom",
        keywidth = 1,
        keyheight = 1
      )
    ) +
    theme(
      axis.text.y = element_text(hjust = 0,color = "black"),
      text = element_text(color = "black"),          # all text in black
      axis.text.x = element_text(angle = 90, hjust = 1,color = "black"),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.text = element_text(angle = 0, hjust = 0),
      panel.background = element_blank(),   # blank panel
      plot.background = element_blank()     # blank overall background
    ) +
    facet_wrap(~dutch_scenario, ncol = 1, strip.position = "top", scales = "free_y")  # free_y ensures only relevant x labels show
  
  
  fig.nm <- paste0("packages_best_worst_high_low_cost")
  ggsave(paste0(dir.out.figs,fig.nm,".pdf"),width = width*1.5,height = height*0.8, units = "cm")
  
  # Support heterogeneity bars ----
  grps.ht <- list(perception_people = c("Municipality","Construction year","Education","Climate-conscious","Reactance municipality"),
                  perception_heat_transition = names(grp.ht.list$perception_heat_transition))
  
  dutch_mapping <- data.table(readxl::read_excel("C:\\Users\\20204133\\Documents\\GitHub\\PONG_DCE\\input\\group_names_to_translate.xlsx"))
  dutch_mapping[, dutch := paste0(`Dutch Translation matching the figure`," ",Statement)]
  
  dt.grid[,answer_dutch := NULL]
  dt.grid[,hetero_dutch := NULL]
  
  dt.grid[dutch_mapping, hetero_dutch := get("i.Dutch Translation matching the figure"), on = "group"]
  dt.grid[dutch_mapping, answer_dutch := get("i.Statement"), on = "group"]
  
  dt.grid[,answer_dutch := paste0(answer_dutch," (",n_respondents,")")]
  
  dt.grid[,answer_dutch := factor(answer_dutch,levels = dt.grid[,unique(answer_dutch)])]
  
  library(forcats)
  
  for (nf in c("baseline")){
    
    for (nm in names(grps.ht)){
      lst.scenarios <- "support_50prct_no_savings_low_cost"# names(dt.grid)[grepl("^support_",names(dt.grid))]
      for (scenario in lst.scenarios){
        
        dt.s <- unique(dt.grid[grepl(paste0("^(",paste0(unlist(grps.ht[nm]),collapse = "|"),")"),group) & attributes_non_financials == nf,],by=c("game","hetero","group"))[,.SD,.SDcols = c("hetero","group","game","pval_none","n_respondents",scenario,"hetero_dutch","answer_dutch")]
        dt.s <- dt.s[group != "Education 0 - Other",]
        dt.s[,hetero_dutch := factor(hetero_dutch,levels = dt.s[, hetero_dutch[match( grps.ht[[nm]], hetero)]])]
        dt.s[,levels := gsub(paste0(hetero," "),"",group),by=.I]
        
        dt.s[, share_cat_ht := cut(get(scenario),
                                   breaks = seq(0, 1, by = 0.1),  # 0.0, 0.1, 0.2, …, 1.0
                                   labels = names(support_colors),
                                   include.lowest = TRUE,
                                   right = TRUE
        )]
        
        # Bars
        
        dt_plot <- copy(dt.s)
        dt_plot$answer_dutch <- factor(dt_plot$answer_dutch, levels = unique(dt_plot$answer_dutch))  # drop unused levels
        
        ggplot(dt_plot, aes(x = answer_dutch, y = !!sym(scenario), fill = share_cat_ht)) +
          geom_col() +
          scale_fill_manual(
            values = support_colors,
            name = "Draagvlak in %"  
          ) +
          geom_hline(yintercept = 0.5001) + 
          coord_flip() +
          scale_y_continuous(limits = c(0,0.8), labels = scales::percent) + 
          labs(x = NULL, y = NULL) +
          theme_minimal(base_size = sz.txt) +
          guides(
            fill = guide_legend(
              title.position = "top",
              label.position = "bottom",
              keywidth = 1,
              keyheight = 1
            )) +
          theme(
            axis.text.y = element_text(hjust = 0,color = "black"),
            text = element_text(color = "black"),       
            axis.text.x = element_text(angle = 90, hjust = 1,color = "black"),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5),
            strip.text = element_text(angle = 0, hjust = 0)
          ) +
          facet_wrap(~hetero_dutch, ncol = 1, strip.position = "top", scales = "free_y")  # free_y ensures only relevant x labels show
        
        fig.nm <- paste0("bars_ht_",scenario,"_",nf,"_",nm)
        ggsave(paste0(dir.out.figs,fig.nm,".pdf"),width = width*1.5,height = height*2, units = "cm")
      }
    }
  }
}

