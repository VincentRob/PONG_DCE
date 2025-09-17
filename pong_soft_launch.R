require(mlogit)
library(dplyr)
library(data.table)
library(ggplot2)
library(jsonlite)
library(scales) 
library(patchwork)

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

survey.names =c("pong_wp3_tables")#,"full_launch_panelclix") # names(files.path)#c("soft_launch_pooled")

invest.list.lvl <- list("insu" = 5000,"heat_networks" = 2000, "heat_pumps" = 7000)

# For heterogeneity analysis, replace missing by Hoorn when applicable
hoorn.file.name <- "Hoorn_complete" 

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
  
  # keep complete responses ----
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
  col.desc <- names(data)[!grepl("(Choice\\ [0-9])|(Question time)|(groupTime)|(randNumber)|(choice screen)|(GXQ00001)|(randSet1)|(pid$)|(cid$)|(^GXQ)|(^r[0-9])|(Date)|(Seed)|(zipcode)|(If yes\\, how many\\?)|(Technology A)|(Technology B)|(Thank you for your participation)|(Total time)|(please enter the code that is in your letter)",names(data))]
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
  
  # Home improvement
  nm.vars <- names(data.desc)[grepl("Do you have solar panels",names(data.desc)) | (grepl("improvements",names(data.desc)) & grepl("insulation",names(data.desc)))]
  data.desc[,nb_house_improvement := sum(.SD == "Yes",na.rm=TRUE),by=` Response ID`,.SDcols = nm.vars]
  
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
    dt.bais <- data.desc[,.SD,.SDcols =col.bias]
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
    
    
    ggsave(paste0(dir.out.figs,"fig_desc_biases",bs,".png"),width = width*1.6,height = height, units = "cm")
    
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
  
  col.att.int <- names(dt.insu)[!(names(dt.insu) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","idx")) & !grepl("^desc_",names(dt.insu))]
  col.ref <- paste0(unique(gsub("_[0-9]$","",col.att.int[grepl("_[0-9]$",col.att.int)])),gsub(".*(_[0-9]).*","\\1",ref.lvl))
  
  formula.insu <- paste0("choice ~ ",paste0(col.att.int,collapse = " + ")," | 0 ")
  logit_interact.insu <- mlogit(
    formula = as.formula(formula.insu),
    dt.insu
  )
  
  print(summary(logit_interact.insu,digits=2))
  
  
  coef_names <- get_json_mapping_4_coefs(game,c(col.att.int,col.ref))
  rows.refs <- get_row_refs(coef_names,col.att.int,col.ref,ref.lvl,stats.table)
  
  tab.tex <- modelsummary::modelsummary(list(Logit = logit_interact.insu), 
                                        coef_map = coef_names[names(coef_names) %in% col.att.int], 
                                        add_rows = rows.refs,
                                        shape = term ~ statistic,
                                        estimate = "{estimate}",
                                        statistic = stats.table,
                                        output = "latex",
                                        stars = c("*" = .1, "**" = .05, "***" = 0.01))
  
  
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
  dt.cfs.tmp[,group := "all"]
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
      
      dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
      dt.cfs.tmp[,group := ht.var]
      dt.cfs.tmp[grepl(":None",Term),Term := gsub(grp.ht.list[[grp.ht]][[ht.var]],paste0(ht.var," "),Term,fixed = TRUE)]
      dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
      
    }
  }
  
  ## Tech results ----
  game <- "tech"
  dt.tech <- get_df_logit(data,game,cst,ref.lvl,data.desc)
  
  write.csv2(dt.tech,paste0(dir.out,"data_mlogit_tech.csv"),quote = TRUE,row.names = FALSE)
  
  col.mdl <- names(dt.tech)[!(names(dt.tech) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","idx")) & !grepl("(^desc_)|(^support_)",names(dt.tech))]
  col.interact.tech <- col.mdl[grepl("cost",col.mdl)]
  col.rest <- setdiff(col.mdl,col.interact.tech)
  col.rest[col.rest == "tech"] <- "factor(tech)"
  
  formula.tech <- paste0("choice ~ ",paste0(col.rest[col.rest != "none"],collapse = " + ")," + ",paste0(paste0(col.interact.tech,":factor(tech)"),collapse = " + ")," + none| 0 ")
  
  logit_interact.tech <- mlogit(
    formula = as.formula(formula.tech),
    dt.tech
  )
  
  print(summary(logit_interact.tech,digits=2))
  
  col.att.int <- names(logit_interact.tech$coefficients)
  col.ref <- paste0(unique(gsub("_[0-9]$","",col.att.int[grepl("_[0-9]$",col.att.int)])),gsub(".*(_[0-9]).*","\\1",ref.lvl))
  
  tech.map <- get_json_mapping_4_coefs(game,c(col.att.int,col.ref))
  tech.map[names(tech.map) == "factor(tech)1"] = "Heat pump"
  tech.map <- c(tech.map[!grepl("^(Heat net)|(Heat pump)|(None)",tech.map)],
                tech.map[grepl("^(Heat pump)",tech.map)],
                tech.map[grepl("^(Heat net)",tech.map)],
                tech.map[grepl("^(None)",tech.map)])
  
  rows.refs <- get_row_refs(c(tech.map[names(tech.map) %in% col.att.int],tech.map[names(tech.map) %in% col.ref]),col.att.int,col.ref,ref.lvl,stats.table)
  
  tab.tex <- modelsummary::modelsummary(list(Logit = logit_interact.tech), 
                                        coef_map = tech.map[names(tech.map) %in% col.att.int], 
                                        add_rows = rows.refs,
                                        shape = term ~ statistic,
                                        estimate = "{estimate}",
                                        statistic = stats.table,
                                        output = "latex",
                                        stars = c("*" = .1, "**" = .05, "***" = 0.01))
  
  dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
  dt.cfs.tmp[,group := "all"]
  dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  ## Tech heterogeneity ----
  grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")
  
  for (grp.ht in names(grp.ht.list)){
    # With reference 
    run_ht_tech_with_ref_level(grp.ht.list[[grp.ht]],grp.ht)
    
    # Without reference level
    for (ht.var in names(grp.ht.list[[grp.ht]])){
      
      if (!(grp.ht.list[[grp.ht]][[ht.var]] %in% names(dt.tech))) next
      
      logit_interact.tech.ht <- mlogit(
        formula = as.formula(gsub("(none|None)",paste0("`",grp.ht.list[[grp.ht]][[ht.var]],"`:none"),formula.tech)),
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
      
      dt.cfs.tmp <- parse_modelsummary_latex(tab.tex,game)
      dt.cfs.tmp[,group := ht.var]
      dt.cfs.tmp[grepl(":(None|none)",Term),Term := gsub(grp.ht.list[[grp.ht]][[ht.var]],paste0(ht.var," "),Term,fixed = TRUE)]
      dt.coefs <- rbindlist(list(dt.coefs,dt.cfs.tmp),use.names = TRUE,fill = TRUE)   
      
    }
  }
  
  
  # Coef table ----
  
  print(dt.coefs)
  
  # Costs columns
  reg.onetimecosts <- "(time amount)|(time costs)"
  reg.monthcosts <- "(eating costs)"
  reg.costs <- paste(reg.onetimecosts,reg.monthcosts,sep="|")
  
  # Step 1: Extract attribute group name (everything before the colon)
  dt.coefs[, Attribute := sub(":.*", "", Term)]
  dt.coefs[grepl("(None|none)$",Term),Attribute := "None"]
  
  # Step 2: Extract numeric cost values for cost-related rows (including references)
  
  dt.coefs[, CostValue := NA_real_]
  dt.coefs[grepl(reg.costs, Term, ignore.case = TRUE), 
           CostValue := sapply(Term, extract_cost_value)]
  
  # Step 3: Find reference CostValue per Attribute group
  # Reference rows have "(reference)" in Term
  refs <- dt.coefs[grepl("\\(reference\\)", Term), .(ReferenceCost = CostValue[!is.na(CostValue)][1]), by = .(Attribute,game,group)]
  
  # Step 4: Join reference costs back
  dt.coefs <- merge(dt.coefs, refs, by = c("Attribute","game","group"), all.x = TRUE)
  
  # Step 5: Calculate relative cost level for cost rows (excluding references)
  dt.coefs[, CostLevelRelative := NA_real_]
  dt.coefs[grepl(reg.costs, Term, ignore.case = TRUE), 
           CostLevelRelative := CostValue - ReferenceCost]
  
  
  dt.coefs <- dt.coefs[order(group,game,grepl("cost",Attribute),Attribute),]
  
  # Attributes importance ----
  
  get_50prct_support_package <- function(dt){
    dt <- copy(dt)
    dt[Attribute == "None",Estimate := -Estimate]
    dt[is.na(Estimate),Estimate := 0]
    
    # Split by attribute
    split_list <- split(dt[grepl(paste0(reg.costs,"|none|None"),Attribute),], by = "Attribute")
    
    # Get all combinations (Cartesian product)
    combos <- do.call(CJ, lapply(split_list, function(x) seq_len(nrow(x))))
    
    results <- combos[, {
      idx_list <- as.list(.SD[1])  # ← use only current row
      selected <- rbindlist(Map(function(tbl, i) tbl[i], split_list, idx_list), fill = TRUE)
      
      list(
        combo = paste(selected$Term, collapse = " | "),
        total = sum(selected$Estimate, na.rm = TRUE)
      )
    }, by = seq_len(nrow(combos)), .SDcols = names(split_list)][, seq_len := NULL]
    
    # Example best_combo string (from your results)
    target.support <- 0.65
    best_combo <- results[which.min(abs(target.support-1/(1+exp(-total))))]$combo
    
    # Split by " | " into terms
    chosen_terms <- stringr::str_split(best_combo, " \\| ")[[1]]
    
    # Extract attribute names from dt
    dt[, is_selected := 0]
    
    # For each attribute, mark the selected term
    for (term in chosen_terms) {
      # Find the attribute for this term (extract substring before ":")
      attr_name <- stringr::str_extract(term, "^[^:]+")
      
      # Mark rows in dt where Attribute & Term match selected combo term
      dt[Attribute == attr_name & Term == term, is_selected := 1]
    }
    
    dt[Attribute == "None",Estimate := -Estimate]
    
    dt[Attribute %in% dt[,sum(is_selected),by=Attribute][V1 == 0,Attribute], is_selected := as.integer(grepl("reference",Term) & Estimate == 0)]
    
    if (dt[,sum(is_selected) != 1,by=Attribute][,any(V1)]) stop("All attributes should have one selected level")
    
    return(dt)
  }
  
  for (gm in c("tech","insu")){
    
    dt.find.50prct <- dt.coefs[game == gm & group == "all" & !grepl("Heat pump",Term),.(Attribute,Term,Estimate,Stars)]
    dt.find.50prct[Stars %in% c("","*") ,Estimate := 0]
    if (gm == "insu"){
      dt.find.50prct <- dt.find.50prct[Attribute != "Nuisance",]
    }
    # Find package with about 50% support 
    dt <- get_50prct_support_package(dt.find.50prct)
    
    dt[,baseline_support := exp(dt[is_selected == 1 & Attribute != "None",sum(Estimate)]) / (exp(dt[is_selected == 1 & Attribute != "None",sum(Estimate)]) + exp(dt[is_selected == 1 & Attribute == "None",sum(Estimate)]))]
    
    for (i in seq_len(dim(dt)[1])){
      if (dt[i, Attribute] == "None" | dt[i, is_selected] == 1 | dt[i, grepl(reg.costs,Term)]) next
      dt[,new_select := copy(is_selected)]
      dt[Attribute %in% dt[i,Attribute], new_select := 0]
      dt[i, new_select := 1]
      new_sup <- exp(dt[new_select == 1 & Attribute != "None",sum(Estimate)]) / (exp(dt[new_select == 1 & Attribute != "None",sum(Estimate)]) + exp(dt[new_select == 1 & Attribute == "None",sum(Estimate)]))
      dt[i, new_supp := new_sup]
      dt[,new_select := NULL]
    }
    
    dt.exp <- dt[!(Attribute == "None" | grepl(reg.costs,Term)),]
    dt.exp[,Term := gsub(" \\(reference\\)","",Term)]
    dt.exp[is_selected == 1,Term := paste0(Term," (reference)")]
    dt.exp[,diff := new_supp - baseline_support]
    dt.exp[,Term := stringr::str_to_sentence(gsub(paste0(Attribute,": "),"",Term)),by=.I]
    
    # Your data: dt.exp with column Term
    max_two_lines <- function(width) {
      all(sapply(dt.exp$Term, function(txt) {
        length(stringr::str_split(stringr::str_wrap(txt, width = width), "\n")[[1]]) <= 2
      }))
    }
    
    # Find the minimum width that satisfies the condition
    wrap_width <- min(Filter(max_two_lines, 10:max(nchar(dt.exp$Term))))
    
    # Apply wrapping
    dt.exp[, Term := stringr::str_wrap(Term, width = 90)]
    
    dt.exp[is.na(diff),diff := 0.001]
    dt.exp[is.na(new_supp) & is_selected == 1,new_supp := baseline_support]
    
    # Add reference
    dt.exp <- rbindlist(list(dt.exp,dt.exp[1,.(Attribute = "Reference",Term = "Reference",new_supp = baseline_support)]),use.names = TRUE,fill = TRUE)
    
    dt.exp <- dt.exp[!grepl("\\(reference\\)", Term), ]
    # Short names
    if (gm == "tech"){
      dt.exp[grepl("2 months of",Term),Term := "More nuisance"]
      dt.exp[grepl("one day a year",Term),Term := "Power outage"]
      dt.exp[grepl("one fixed heat supplier",Term),Term := "One supplier"]
      dt.exp[, Attribute := factor(Attribute, levels = c("Reference", "Supplier", "Nuisance", "Power outages"))]
    } else {
      dt.exp[grepl("trees\\.",Term),Term := "Less CO2 savings"]
      dt.exp[grepl("less problems with street noise",Term),Term := "No cooling, less noise"]
      dt.exp[grepl("draft-free\\.",Term),Term := "No cooling"]
      dt.exp[grepl("help with selecting the contractor",Term),Term := "Some help"]
      dt.exp[grepl("arrange everything yourself",Term),Term := "No help"]
      dt.exp[, Attribute := factor(Attribute, levels = c("Reference", "Comfort", "Support", "Co2"))]
      
    }
    
    ggplot(dt.exp, 
           aes(x = new_supp, y = reorder(Term, new_supp))) +
      geom_col(fill = grey2) +
      geom_text(
        aes(label = scales::percent(new_supp, accuracy = 1), x = new_supp + 0.005),
        hjust = 0,
        size = font.axis.lbls * 0.35
      ) +
      facet_grid(Attribute ~ ., scales = "free_y", space = "free", switch = "y") + 
      coord_cartesian(xlim = c(0.40, 0.80)) +
      theme_minimal() +
      labs(x = NULL, y = NULL, title = NULL) +
      theme_pong +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text.y.left = element_blank(),        # <- Hide facet labels
        strip.background = element_blank(),         # <- Optional: remove background
        panel.spacing = unit(1, "lines")
      )
    
    ggsave(paste0(dir.out.figs,"fig_nonmonet_importance_",gm,".png"),width = width,height = height, units = "cm")
    
    
    dt[is_selected == 1 & Attribute != "None",Term_ref := gsub(" \\(reference\\)","",Term)]
    dt[,Term_ref := stringr::str_to_sentence(gsub(paste0(Attribute,": "),"",Term_ref)),by=.I]
    
    sink(paste0(dir.out.figs,"tab_nonmonet_importance_",gm,".txt"))
    print(dt.exp)
    
    print(dt)
    sink()
    
    
  }
  
  
  # Fig heatmap ----
  for (gm in c("insu","tech")){
    for (charac in c("Education","all","Construction year") ){ #dt.coefs[game == gm,unique(group)]
      for (grp in dt.coefs[group == charac & Attribute == "None" & game == gm,unique(gsub(":(None|none)","",Term))]){
        
        # All cominations of monetary
        if (gm == "insu"){
          dt.pw <- dt.coefs[group == charac & game == gm,CJ(invest = Term[grepl("ne time",Attribute)],savings = Term[grepl("eating costs",Attribute)])]
        } else {
          dt.pw <- rbindlist(list(dt.coefs[group == charac & game == gm,CJ(invest = Term[grepl("Heat pump",Attribute) & grepl("one time",Attribute)],savings = Term[grepl("Heat pump",Attribute) & grepl("heating costs",Attribute)])],
                                  dt.coefs[group == charac & game == gm,CJ(invest = Term[grepl("Heat network",Attribute) & grepl("one time",Attribute)],savings = Term[grepl("Heat network",Attribute) & grepl("heating costs",Attribute)])]))
        }
        
        # Add monetary coefficients
        dt.pw[dt.coefs[group == charac & game == gm,],`:=` (coef.invest = i.Estimate,
                                                            ref.invest = i.ReferenceCost,
                                                            amount.invest = i.CostValue),on=c(invest = "Term")]
        dt.pw[dt.coefs[group == charac & game == gm,],`:=` (coef.savings = i.Estimate,
                                                            ref.savings = i.ReferenceCost,
                                                            amount.savings = i.CostValue),on=c(savings = "Term")]
        # Add none
        if (gm == "insu"){
          dt.pw[, none := dt.coefs[Attribute == "None" & game == gm & group == charac & grepl(grp,Term),Estimate] ]
        } else {
          dt.pw[grepl("Heat network",invest), none := dt.coefs[Attribute == "None" & game == gm & group == charac & grepl(grp,Term),Estimate] ]
          dt.pw[grepl("Heat pump",invest), none := dt.coefs[Attribute == "None" & game == gm & group == charac & grepl(grp,Term),Estimate] - dt.coefs[Attribute == "Heat pump" & game == gm & group == charac,Estimate] ]
        }
        
        dt.pw <- dt.pw[order(grepl("Heat pump",invest),amount.invest,amount.savings),]
        dt.pw[ref.invest == amount.invest,coef.invest := 0]
        dt.pw[ref.savings == amount.savings,coef.savings := 0]
        
        dt.pw.plt <- copy(dt.pw)
        reg.toclean <- "(Heat pump)|(Heat network)|( \\(reference\\))|(One time costs\\:)|(One time amount\\:)|(Heating costs\\:)"
        dt.pw.plt[,savings := stringr::str_to_sentence(gsub(reg.toclean,"",savings))]
        dt.pw.plt[,invest := stringr::str_to_sentence(gsub(reg.toclean,"",invest))]
        dt.pw.plt[,savings := stringr::str_to_sentence(gsub(reg.toclean,"",savings))]
        dt.pw.plt[,invest := stringr::str_to_sentence(gsub(reg.toclean,"",invest))]
        
        dt.pw.plt[,savings := stringr::str_to_sentence(gsub(" than now\\.","",savings))]
        dt.pw.plt[,savings := stringr::str_to_sentence(gsub(" euros","€",savings))]
        
        dt.pw.plt[, savings := factor(savings, levels = unique(savings[order(amount.savings)]))]
        dt.pw.plt[, invest := factor(invest, levels = unique(invest[order(amount.invest)]))]
        
        dt.pw.plt[,s := exp(coef.invest + coef.savings)/(exp(coef.invest + coef.savings)+exp(none))]
        
        support_colors <- c(
          "25% or less"   = "#d73027",
          "25-50%"  = "#fc8d59",
          "50-75%"  = "#b2df8a",
          "75% or more" = "#1a9850" 
        )
        
        dt.pw.plt[, s_cat := cut(s,
                                 breaks = c(0, 0.25, 0.5, 0.75, 1),
                                 labels = names(support_colors),
                                 include.lowest = TRUE)]
        ttl = if_else(charac == "all","All",grp)
        ggplot(dt.pw.plt, aes(x = savings, y = invest, fill = s_cat)) +
          geom_tile(color = "white") +
          scale_fill_manual(values = support_colors, name = "Support") +
          coord_fixed() +
          theme_minimal() +
          labs(
            title = paste0(ttl),
            x = "Savings",
            y = "Investment costs"
          ) +
          theme_pong + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        fig.nm <- paste0("fig_hm_",gm,"_",ttl)
        fig.nm <- gsub("[^A-Za-z0-9]", "_", fig.nm)
        
        ggsave(paste0(dir.out.figs,fig.nm,".png"),width = width,height = height, units = "cm")
      }
    }
  }
  
  
  
  
  
  dt.coefs[,(c("CostLevel","CostValue","ReferenceCost")) := NULL]
  
  # Table for BEcrowd ----
  dt.bcrwd.tot <- data.table(attribute_nr = numeric(),
                             attribute_text = character(),
                             level_nr = numeric(),
                             level_text = character(),
                             game = character())
  names.merge <- c("attribute_text", "level_text", "game" )
  
  for (grp in dt.coefs[,unique(group)]){
    
    for (gm in c("insu","tech")){
      for (grp.lvl in dt.coefs[game == gm & group == grp & Attribute == "None",unique(Term)]){
        grp.nm <- paste0("weight_",gsub(" ","_",gsub("(:|none|None)","",grp.lvl)))
        grp.nm <- gsub("_$","",grp.nm)
        
        dt.bcrwd <- dt.coefs[game == gm & group == grp & (Attribute != "None" | Term == grp.lvl ),]
        if (dt.bcrwd[Attribute == "None",.N] != 1) stop("There should be exactly one none level")
        dt.bcrwd[Attribute == "None",Term := "None"]
        
        # Clean level names
        for (i in 1:dim(dt.bcrwd)[1]){
          dt.bcrwd[i,(names(dt.bcrwd)[grepl("Term",names(dt.bcrwd))]) := lapply(.SD,\(x) stringr::str_to_sentence(gsub(paste0("(",Attribute,": )| (\\(reference\\))"),"",x))),.SDcols = (names(dt.bcrwd)[grepl("Term",names(dt.bcrwd))])]
        }
        
        # Attribute 99 for valuation
        dt.bcrwd.val <- dt.bcrwd[,.(Attribute = "Valuation",Term = "thousand euro",Estimate = NA_real_)]
        dt.bcrwd <- rbindlist(list(dt.bcrwd,dt.bcrwd.val),use.names = TRUE,fill = TRUE)
        
        dt.bcrwd.to.csv <- dt.bcrwd[,.(attribute_text = Attribute,level_text = Term,weight = Estimate)]
        setnames(dt.bcrwd.to.csv,"weight",grp.nm)
        #dt.bcrwd.to.csv[is.na(get(grp.nm)),(grp.nm) := 0]
        
        # Remove nuisance
        if (gm == "insu"){
          dt.bcrwd.to.csv <- dt.bcrwd.to.csv[attribute_text != "Nuisance"]
        }
        
        # Distinguish HP and HN
        if (gm == "tech"){
          dt.bcrwd.to.csv[, tech := stringr::str_extract(attribute_text, "Heat pump|Heat network")]
        }
        
        # Rename levels
        names.bcrwd <- read_json(paste0("input/mapping_",gm,"_becrowd.json"))
        for (cl in names(names.bcrwd)){
          old.nm <- names(names.bcrwd[[cl]])
          if (length(old.nm)>0){
            if (cl == "attribute_text"){
              dt.bcrwd.to.csv <- dt.bcrwd.to.csv[order(match(get(cl),old.nm)),]
              dt.bcrwd.to.csv[get(cl) %in% old.nm & get(cl) != "None", attribute_nr := .GRP,by=attribute_text]
            }
            dt.bcrwd.to.csv[get(cl) %in% old.nm, (cl) := as.character(names.bcrwd[[cl]][get(cl)])]
            
          }
        }
        
        if ("tech" %in% names(dt.bcrwd.to.csv)){
          dt.bcrwd.to.merge <- data.table()
          for (tc in dt.bcrwd.to.csv[,unique(na.omit(tech))]){
            dt.bcrwd.to.csv.tec <- dt.bcrwd.to.csv[is.na(tech) | tech == tc,][,-"tech"]
            if (tc == "Heat pump"){
              dt.bcrwd.to.csv.tec[level_text == "None", (grp.nm) := get(grp.nm) - dt.bcrwd.to.csv.tec[level_text == "Heat pump",get(grp.nm)]]
              dt.bcrwd.to.csv.tec <- dt.bcrwd.to.csv.tec[level_text != "Heat pump"]
            }
            dt.bcrwd.to.csv.tec[!(attribute_text %in% c("None of these","Valuation")), attribute_nr := .GRP,by=attribute_text]
            
            lvl <- if_else(tc == "Heat pump",invest.list.lvl$heat_pumps,invest.list.lvl$heat_networks)/1000
            
            #dt.bcrwd.to.csv.tec[attribute_text == "Valuation", (grp.nm) := dt.bcrwd.to.csv.tec[attribute_text == "What is the one-time amount I have to pay?" & level_nr == 0,get(grp.nm)]/lvl]
            
            #write.csv(dt.bcrwd.to.csv.tec,paste0(dir.out, "becrowd_",gm,"_",gsub(" ","_",tolower(tc)),"_table.csv"),row.names = FALSE,quote = TRUE)
            dt.bcrwd.to.csv.tec[,game := tc]
            dt.bcrwd.to.merge <- rbindlist(list(dt.bcrwd.to.merge,dt.bcrwd.to.csv.tec))
          }
        } else {
          # Add valuation
          dt.bcrwd.to.csv[attribute_text == "Valuation", (grp.nm) := dt.bcrwd.to.csv[attribute_text == "What will it cost me to insulate my house?" & level_text == "5,000 euros.",get(grp.nm)]/(invest.list.lvl$insu/1000)]
          
          # write.csv(dt.bcrwd.to.csv,paste0(dir.out, "becrowd_",gm,"_table.csv"),row.names = FALSE,quote = TRUE)
          dt.bcrwd.to.csv[,game := "Insulation"]
          dt.bcrwd.to.merge <- copy(dt.bcrwd.to.csv)
        }
        
        # if (grp.nm %in% names(dt.bcrwd.tot)){
        #   dt.bcrwd.tot <- rbindlist(list(dt.bcrwd.tot,dt.bcrwd.to.merge),use.names = TRUE,fill = TRUE)
        # } else { 
        #   }
        
        setnames(dt.bcrwd.to.merge,grp.nm,"wtoadd")
        dt.bcrwd.tot <- merge(dt.bcrwd.tot,dt.bcrwd.to.merge[,.SD,.SDcols = c(names.merge,"wtoadd")],by = names.merge,all=TRUE)
        if (grp.nm %in% names(dt.bcrwd.tot)){
          dt.bcrwd.tot[,(grp.nm) := fcoalesce(get(grp.nm),wtoadd)]
        } else {
          dt.bcrwd.tot[,(grp.nm) := wtoadd]
        }
        dt.bcrwd.tot[,wtoadd := NULL]
        
        if (dt.bcrwd.tot[,any(duplicated(.SD)),.SDcols = names.merge[!grepl("_nr",names.merge)]]) stop("Duplicated in dt.bcrwd.tot")
      }
    }
  }
  for (gm in dt.bcrwd.tot[,unique(game)]){
    dt.bcrwd.tot[game == gm & !(attribute_text %in% c("None of these","Valuation")) ,attribute_nr := .GRP,by=.(attribute_text)]
    dt.bcrwd.tot[game == gm & attribute_text == "None of these",attribute_nr := 0]
    dt.bcrwd.tot[game == gm,level_nr := (1:.N)-1,by=.(attribute_text)]
  }
  dt.bcrwd.tot[attribute_text == "Valuation",attribute_nr := 99]
  dt.bcrwd.tot[attribute_text == "Valuation",level_nr := 1]
  
  if (dt.bcrwd.tot[,any(duplicated(.SD)),.SDcols = names(dt.bcrwd.tot)[!grepl("^wei",names(dt.bcrwd.tot))]]) stop("Duplicated in dt.bcrwd.tot")
  
  
  dt.bcrwd.tot <- dt.bcrwd.tot[order(game,attribute_nr,level_nr),]
  
  wghts <- c("weight","weight_People_PONG_positive_5_-_More_than_half" )
  wghts <- wghts[wghts %in% names(dt.bcrwd.tot)]
  write.csv(dt.bcrwd.tot[game == "Heat pump",.SD,.SDcols = c("attribute_nr","attribute_text","level_nr","level_text",wghts)],
            paste0(dir.out, "becrowd_test_table.csv"),
            row.names = FALSE,
            quote = TRUE,  
            na = "")
  
  
  # IRR ----
  
  dt.coefs[grepl(reg.onetimecosts,Term),unit := 1000]
  dt.coefs[grepl(reg.monthcosts,Term),unit := 10]
  
  
  dt.coefs[,value_money := unit*Estimate/CostLevelRelative]
  
  dt.irr.tp <- dt.coefs[grepl(reg.costs,Term),.(unit = unique(unit),median_utility = median(value_money, na.rm = TRUE)),by=.(cost_month = grepl(reg.monthcosts,Term))]
  dt.irr.tp[,(paste0(irr.years," years")) := lapply(irr.years,
                                                    \(x) irr(unit[cost_month == FALSE]*median_utility[cost_month == TRUE]/median_utility[cost_month == FALSE],
                                                             unit[cost_month == TRUE]*12,
                                                             x
                                                    )),]
  dt.irr.tp[,game := "all"]
  
  dt.irr.tp.mdl <- dt.coefs[grepl(reg.costs,Term),.(unit = unique(unit),median_utility = median(value_money, na.rm = TRUE)),by=.(game,netwrk = grepl("Heat network",Attribute),cost_month = grepl(reg.monthcosts,Term))]
  
  dt.irr.tp.mdl[,(paste0(irr.years," years")) := lapply(irr.years,
                                                        \(x) irr(unit[cost_month == FALSE]*median_utility[cost_month == TRUE]/median_utility[cost_month == FALSE],
                                                                 unit[cost_month == TRUE]*12,
                                                                 x
                                                        )),by=.(game,netwrk)]
  
  dt.irr.tp.all <- rbindlist(list(dt.irr.tp.mdl,dt.irr.tp),use.names = TRUE,fill = TRUE)
  dt.irr.tp.all[game == "insu",model := "Insulation"]
  dt.irr.tp.all[game == "all",model := "All"]
  dt.irr.tp.all[game == "tech" & netwrk == FALSE,model := "Heat pumps"]
  dt.irr.tp.all[game == "tech" & netwrk == TRUE,model := "Heat network"]
  
  dt.irr.tp.all[,(paste0(irr.years," years")) := lapply(.SD,\(x) paste0(round(100*x,1),"%")),.SDcols = paste0(irr.years," years")]
  
  dt.irr.tp.all <- dt.irr.tp.all[order(model,-unit),]
  
  dt.irr.data <- dt.irr.tp.all[,.(Model=model,`Unit (in euro)` = unit,`Median utility change` = round(median_utility,3))]
  dt.irr.values <- dt.irr.tp.all[,.SD,.SDcols = c("model",paste0(irr.years," years"))]
  dt.irr.values <- dt.irr.values[!duplicated(model),]
  setnames(dt.irr.values,"model","Model")
  
  print_table <- function(tab,tab.name){
    
    sink(paste0(dir.out, tab.name))
    cat("\\begin{table}[ht]\n\\centering\n\\scriptsize\n")
    print(xtable::xtable(tab),
          type = "latex",
          include.rownames = FALSE,
          floating = FALSE)  # Avoid nested \begin{table}
    cat("\\end{table}\n")
    sink()
  }
  
  print_table(dt.irr.values,"results_irr.tex")
  
  print_table(dt.irr.data,"results_irr_data.tex")
  
  
  # WTP ----
  # Use use median value across all models
  dt.wtp <- dt.coefs[,.(game,Attribute,Term,Estimate)]
  dt.wtp[,`WTP (1,000 euro)` := round(Estimate/abs(dt.irr.tp.all[game == "all" & cost_month == FALSE,median_utility]),1)]
  
  print_table(dt.wtp[,.(Term,`WTP (1,000 euro)`)],"results_wtp.tex")
  
  
  ### Simulations ----
  dt.cf.simu <- copy(dt.coefs)
  dt.cf.simu[is.na(Estimate) & grepl("reference",Term),Estimate := 0]
  
  pack <- dt.cf.simu[,.(level = if_else(all(Attribute != "None"),sample(Term,1),NA_character_)),by=.(game,Attribute)]
  pack[,package := "package1"]
  
  pack.tmp <- dt.cf.simu[,.(level = if_else(all(Attribute != "None"),sample(Term,1),NA_character_)),by=.(game,Attribute)]
  pack.tmp[,package := "package2"]
  pack <- rbindlist(list(pack,pack.tmp))
  
  pack.tmp <- dt.cf.simu[,.(level = if_else(all(Attribute == "None"),sample(Term,1),NA_character_)),by=.(game,Attribute)]
  pack.tmp[,package := "none"]
  pack <- rbindlist(list(pack,pack.tmp))
  
  # Choose either HP or HT
  pack[game == "tech",Attribute := if_else(grepl(sample(c("Heat network","Heat pump"),1),Attribute),NA_character_,Attribute),by=package]
  pack <- pack[!is.na(Attribute),]
  
  pack[dt.cf.simu,Estimate := i.Estimate,on = c("level" = "Term","game","Attribute")]
  
  tab.simu <- pack[,.(u = sum(Estimate,na.rm=TRUE)) ,by=.(package,game)][,p := exp(u)/sum(exp(u)),by=game]
  
  pack_wide <- reshape(pack[,.(Attribute,game,level,package)],direction = "wide",idvar = c("game","Attribute"),timevar = "package")
  setnames(pack_wide,names(pack_wide),gsub("level\\.","",names(pack_wide)))
  
  for (cl in c("u","p")){
    tab.simu_wide <- reshape(tab.simu[,.(package,game,cl = round(get(cl),2))],direction = "wide",idvar = c("game"),timevar = "package")
    setnames(tab.simu_wide,names(tab.simu_wide),gsub("cl\\.","",names(tab.simu_wide)))
    tab.simu_wide[,Attribute := if_else(cl == "u","utility","probability")]
    
    pack_wide <- rbindlist(list(pack_wide,tab.simu_wide),use.names = TRUE,fill = TRUE)
    
  }
  pack_wide <- pack_wide[order(game,Attribute %in% c("utility","probability"),Attribute == "None"),]
  
  for (i in 1:dim(pack_wide)[1]){
    pack_wide[i,(names(pack_wide)[grepl("package",names(pack_wide))]) := lapply(.SD,\(x) gsub(paste0("(",Attribute,": )| (\\(reference\\))"),"",x)),.SDcols = (names(pack_wide)[grepl("package",names(pack_wide))])]
  }
  
  print(pack_wide)
  sink(paste0(dir.out, "results_simu.tex"))
  print(xtable::xtable(pack_wide), type = "latex", size = "\\tiny", include.rownames = FALSE)
  sink()
  
}

