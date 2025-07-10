require(mlogit)
library(dplyr)
library(data.table)
library(ggplot2)
library(jsonlite)

# Background ----
source("Figure_theme.R")

source(file = "functions_pong_soft_launch.R")

input_specs <- read.csv("input/input_specs.csv")
setDT(input_specs)

if (input_specs[,anyDuplicated(.SD),.SDcols = c("survey_name","data_path")]){
  stop("Sample-data file pairs should be unique")
}

survey.names =c("all_panelclix_hoorn","full_launch_panelclix_first_sample") # names(files.path)#c("soft_launch_pooled")

invest.list.lvl <- list("insu" = 5000,"heat_networks" = 2000, "heat_pumps" = 7000)

for (survey.name in survey.names){
  
  dir.out <- paste0("output/",survey.name,"/")
  dir.create(dir.out)
  
  data = data.table()
  for (file.name in input_specs[survey_name == survey.name,unique(data_path)]){
    data_tmp <- readxl::read_excel(paste0(file.name)) %>% as.data.table()
    data_tmp[,survey_name := gsub("^.*/|\\.xlsx$", "", file.name)]
    data <- rbindlist(list(data,data_tmp),use.names = TRUE, fill = TRUE)
  }
  
  
  # Remove emoji
  names(data) <- sapply(names(data),\(x) gsub("[^\x01-\x7F]", "", x)) 
  
  # Outliers ----
  # 1 Response time: overall too short; information page too short 
  # 2 Responses unrealistic: always refuse 
  
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
  
  # Descriptives - respondent characteristics ----
  col.desc <- names(data)[!grepl("(Choice\\ [0-9])|(Question time)|(groupTime)|(randNumber)|(choice screen)|(GXQ00001)|(randSet1)|(pid$)|(cid$)|(^GXQ)|(^r[0-9])|(Response ID)|(Date)|(Seed)|(zipcode)|(If yes\\, how many\\?)|(Technology A)|(Technology B)|(Thank you for your participation)|(Total time)|(please enter the code that is in your letter)",names(data))]
  data.desc <- janitor::clean_names(data[,.SD,.SDcols = col.desc])
  names.desc <- setNames(col.desc,names(data.desc))
  
  tab.desc <- data.table()
  for (cl in names(data.desc)){
    tab.tmp <- data.desc[survey_name %in% data.desc[!is.na(get(cl)),unique(survey_name)] ,.(Q = names.desc[cl] ,Q_clean=cl,.N),by = c(answer = cl)]
    survey_count <- data.desc[survey_name %in% data.desc[!is.na(get(cl)),unique(survey_name)],.N,by = c(answer = cl,survey_name = "survey_name")]
    survey_count[,N_s := sum(N),by=survey_name]
    survey_count[,N_tot := sum(N_s),by=answer]
    survey_count <- unique(survey_count[,.(answer,N_tot)])
    
    tab.tmp <- tab.tmp[survey_count,on="answer"]
    tab.desc <- rbindlist(list(tab.tmp,tab.desc),use.names = TRUE,fill = TRUE)
  }
  tab.desc[,Q_org := copy(Q)]
  tab.desc[grepl("\\[multiple",Q_org), `:=` (Q = gsub("(.*?)\\ \\[multipl.*","\\1",Q_org), answer = paste0(gsub(".*\\[(.*)\\].*","\\1",Q_org)," - ",answer))]
  tab.desc[,Q := gsub(".*?\\.(.*)","\\1",Q)]
  tab.desc[,unique(Q)]
  tab.desc[Q == "",Q := Q_clean]
  
  # Descriptives mapping template
  # Assign question ranks
  tab.desc[, question_rank := .GRP, by = Q]
  
  # Build result list
  result_list <- setNames(
    lapply(split(tab.desc, by = "Q", keep.by = TRUE), function(group) {
      # Unique answers per question, preserving order
      unique_answers <- unique(group$answer)
      
      answers_list <- setNames(
        lapply(seq_along(unique_answers), function(i) {
          ans <- unique_answers[i]
          list(
            answer_tags = list(ans),
            answer_rank = i
          )
        }),
        unique_answers
      )
      
      list(
        question_tags = list(rep(group$Q[1], 2)),
        question_rank = group$question_rank[1],
        answers = answers_list,
        question_group = NA
      )
    }),
    unique(tab.desc$Q)
  )
  
  cat(toJSON(result_list, pretty = TRUE, auto_unbox = TRUE))
  
  # Output JSON
  write(toJSON(result_list, pretty = TRUE, auto_unbox = TRUE), file = paste0(dir.out,"mapping_descriptives_template.json"))
  
  # Read JSON mapping descriptives
  fljson <- paste0("input/attributes_levels_mapping/mapping_descriptives.json")
  mapping <- jsonlite::read_json(fljson)
  
  
  # Loop over each question and rename
  tab.desc[,Q_mapped := NA_character_]

  for (new_q_name in names(mapping)) {
    
    old_q_names <- unlist(mapping[[new_q_name]][["question_tags"]])
    
    if (tab.desc[,any(Q %in% old_q_names & !is.na(Q_mapped))]) stop(sprintf("Already mapped: %s \n\t Please fix %s.",
                                                                            paste0(old_q_names,collapse ="; "),
                                                                            fljson))
    
    grp_q <- ifelse(is.null(mapping[[new_q_name]][["question_group"]]),NA_character_,mapping[[new_q_name]][["question_group"]])
    tab.desc[Q %in% old_q_names,`:=` (Q_mapped = new_q_name,
                                      question_rank_mapped = mapping[[new_q_name]][["question_rank"]],
                                      question_group_mapped = grp_q)]
    
    # Loop over each level and rename
    for (new_answer_name in names(mapping[[new_q_name]][["answers"]])) {
      old_answer_names <- unlist(mapping[[new_q_name]][["answers"]][[new_answer_name]]$answer_tags)
      
      tab.desc[Q %in% old_q_names & answer %in% old_answer_names,`:=` (answer_mapped = new_answer_name,
                                                                       answer_rank_mapped = mapping[[new_q_name]][["answers"]][[new_answer_name]]$answer_rank)]

    }
    
    # if (tab.desc[,any(Q %in% old_q_names & is.na(answer_mapped) & !is.na(answer))]) stop(sprintf("Mapping missing for question `%s`, answer `%s` \n\t Please add mapping for this answer in %s.",
    #                                                                         paste0(old_q_names,collapse ="; "),
    #                                                                         tab.desc[Q %in% old_q_names & is.na(answer_mapped) & !is.na(answer),paste0(answer,collapse ="; ")],
    #                                                                         fljson))
    
    tab.desc[Q %in% old_q_names & is.na(answer_mapped) & !is.na(answer),answer_mapped := answer]
  }
  
  # Merge 
  cols.unique <- c("Q_mapped","answer_mapped")
  tab.desc.merged <- tab.desc[!is.na(Q_mapped),.(N = sum(N),
                                                 N_total = sum(N_tot),
                                                 question_rank_mapped = unique(question_rank_mapped),
                                                 question_group_mapped = unique(question_group_mapped),
                                                 answer_rank_mapped = unique(answer_rank_mapped)),by=cols.unique]
  tab.desc.merged[,nrows := .N,by=cols.unique]
  
  if (tab.desc.merged[,any(nrows != 1)]) stop("nrows should have only ones; fix it")
  tab.desc.merged[,nrows := NULL]
  tab.desc.merged <- tab.desc.merged[order(question_rank_mapped,answer_rank_mapped,Q_mapped,answer_mapped),]
  
  #tab.desc.merged[,N_total := data[,uniqueN(`id. Response ID`)]]
  
  for (grp in tab.desc.merged[,unique(na.omit(question_group_mapped))]){
    
    if (grp == "bias_monet"){
      for (monet_question in tab.desc.merged[question_group_mapped == "bias_monet",unique(Q_mapped)]){
        tab <- tab.desc.merged[Q_mapped == monet_question,.(answer_mapped,N)][,.(Answer = rep(as.integer(answer_mapped),N))]
        
        if (grepl("50 per month",monet_question)){
          nm.fig <- "wtp_50_savings_per_month"
          bd <- 10
        }else if (grepl("lottery",monet_question)){
          nm.fig <- "value_1_year_wait"
          bd <- 10
        } else {stop(sprintf("Give name to question `%s`",monet_question))}
        
        ggplot(tab,aes(x = Answer)) + geom_histogram(binwidth = bd, fill=grey2, col=grey3) + theme.graphs + labs(x = stringr::str_to_sentence(gsub("\\_"," ",nm.fig)))
        ggsave(paste0(dir.out, nm.fig, ".pdf"),device = "pdf", units = "cm",height = height, width = width)
        
        tab.mean <- tab[,.(Mean = mean(Answer,na.rm=TRUE),Median = median(Answer,na.rm=TRUE))]
        
        sink(paste0(dir.out,nm.fig, ".tex"))
        print(xtable::xtable(tab.mean), type = "latex")
        sink()
        
      }

      next
    }
    
    tab <- tab.desc.merged[question_group_mapped == grp,.(Question = Q_mapped, Answer = answer_mapped, Share = sprintf("%.0f%% (%.d)", 100*N/N_total,N))]
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
    
    # Blank out repeated question values for LaTeX formatting
    tab[, Question_display := ifelse(.I == 1 | Question != shift(Question), Question, "")]
    
    # Remove scale question instruction
    tab[,Question_display := gsub("On a scale from 1 to 5, to what extent do you agree with the following statements? ","",Question_display,fixed = TRUE)]
    
    
    tab.body <- tab[, .(Question = c(Question_display[1], Answer),Share = c("", Share)), by = .(Group = Question)][,-"Group"]
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
    
    writeLines(paste(lines, collapse = "\n"), paste0(dir.out,"descriptives_",grp,".tex"))
    
  }
  
  # Results ----
  
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
  
  # Store monetary coefficients (vom value of money)
  dt.coefs <- data.table()
  
  ## Insulation results ----
  att.mn <- c("comfort","support","nuisance","co2","one_time_amount","heating_costs")
  game <- "insu"
  dt.insu <- get_df_logit(data,game,att.mn,cst,ref.lvl)
  
  col.att.int <- names(dt.insu)[!(names(dt.insu) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","idx"))]
  col.ref <- paste0(unique(gsub("_[0-9]$","",col.att.int[grepl("_[0-9]$",col.att.int)])),gsub(".*(_[0-9]).*","\\1",ref.lvl))
  
  logit_interact.insu <- mlogit(
    formula = as.formula(paste0("choice ~ ",paste0(col.att.int,collapse = " + ")," | 0 ")),
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
  
  
  parse_modelsummary_latex <- function(tex_output,game) {
    # Split LaTeX into lines
    tex_lines <- unlist(strsplit(tex_output, "\n"))
    
    # Find positions of first and second \midrule
    midrule_lines <- grep("\\\\midrule", tex_lines)
    if (length(midrule_lines) < 2) {
      stop("Expected at least two \\midrule lines in the LaTeX output.")
    }
    
    # Extract only coefficient lines (between first and second \midrule)
    start_idx <- midrule_lines[1] + 1
    end_idx <- midrule_lines[2] - 1
    table_lines <- c(tex_lines[start_idx:end_idx],tex_lines[end_idx+2]) # end_idx+2 to include none coef
    
    # Clean formatting
    table_lines <- gsub("\\\\", "", table_lines)
    table_lines <- gsub("&", "|", table_lines)
    table_lines <- trimws(table_lines)
    table_lines <- table_lines[table_lines != ""]
    
    # Split rows into columns
    split_lines <- strsplit(table_lines, "\\|")
    df <- do.call(rbind, lapply(split_lines, function(x) {
      x <- trimws(x)
      length(x) <- 3  # ensure exactly 3 columns
      return(x)
    }))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- c("Term", "Estimate", "StdError")
    
    # Convert Estimate to numeric (remove stars)
    df$Estimate <- suppressWarnings(as.numeric(gsub("\\*+", "", df$Estimate)))
    
    # Optional: keep stars separately
    df$Stars <- regmatches(df$StdError, gregexpr("\\*+", df$StdError))
    df$Stars <- sapply(df$Stars, function(x) if (length(x)) x else "")
    
    # Clean StdError: remove parentheses and stars
    df$StdError <- gsub("\\(|\\)|\\*", "", df$StdError)
    df$StdError <- suppressWarnings(as.numeric(df$StdError))
    
    setDT(df)
    df[,game := game]
    
    return(df)
  }
  
  
  
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  dt.coefs <- rbindlist(list(dt.coefs,parse_modelsummary_latex(tab.tex,game)),use.names = TRUE,fill = TRUE)   
  
  ## Tech results ----
  att.mn <- c("support","nuisance","supplier","power_outages","co2","one_time_amount","heating_costs")
  game <- "tech"
  dt.tech <- get_df_logit(data,game,att.mn,cst,ref.lvl)
  
  col.mdl <- names(dt.tech)[!(names(dt.tech) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","idx"))]
  col.interact.tech <- col.mdl[grepl("cost",col.mdl)]
  col.rest <- setdiff(col.mdl,col.interact.tech)
  col.rest[col.rest == "tech"] <- "factor(tech)"
  
  logit_interact.tech <- mlogit(
    formula = as.formula(paste0("choice ~ ",paste0(col.rest[col.rest != "none"],collapse = " + ")," + ",paste0(paste0(col.interact.tech,":factor(tech)"),collapse = " + ")," + none| 0 ")),
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
  
  dt.coefs <- rbindlist(list(dt.coefs,parse_modelsummary_latex(tab.tex,game)),use.names = TRUE,fill = TRUE)   
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  
  print(dt.coefs)
  
  # Costs columns
  reg.onetimecosts <- "(time amount)|(time costs)"
  reg.monthcosts <- "(eating costs)"
  reg.costs <- paste(reg.onetimecosts,reg.monthcosts,sep="|")
  
  # Step 1: Extract attribute group name (everything before the colon)
  dt.coefs[, Attribute := sub(":.*", "", Term)]
  
  # Step 2: Extract numeric cost values for cost-related rows (including references)
  extract_cost_value <- function(term) {
    # Handle 'same as now'
    if (grepl("same as now", term, ignore.case = TRUE)) return(0)
    
    # Extract full numeric value, handling commas as thousand separators
    term_clean <- gsub(",", "", term)  # remove thousand separators
    num <- as.numeric(sub(".*?(\\d+(\\.\\d+)?).*", "\\1", term_clean))
    
    if (is.na(num)) return(NA_real_)
    
    if (grepl("less than now", term, ignore.case = TRUE)) {
      return(-abs(num))
    } else if (grepl("more than now", term, ignore.case = TRUE)) {
      return(abs(num))
    } else {
      # One-time cost or fixed value
      return(num)
    }
  }
  
  dt.coefs[, CostValue := NA_real_]
  dt.coefs[grepl(reg.costs, Term, ignore.case = TRUE), 
           CostValue := sapply(Term, extract_cost_value)]
  
  # Step 3: Find reference CostValue per Attribute group
  # Reference rows have "(reference)" in Term
  refs <- dt.coefs[grepl("\\(reference\\)", Term), .(ReferenceCost = CostValue[!is.na(CostValue)][1]), by = Attribute]
  
  # Step 4: Join reference costs back
  dt.coefs <- merge(dt.coefs, refs, by = "Attribute", all.x = TRUE)
  
  # Step 5: Calculate relative cost level for cost rows (excluding references)
  dt.coefs[, CostLevelRelative := NA_real_]
  dt.coefs[grepl(reg.costs, Term, ignore.case = TRUE), 
           CostLevelRelative := CostValue - ReferenceCost]
  
  dt.coefs[,(c("CostLevel","CostValue","ReferenceCost")) := NULL]
  
  dt.coefs <- dt.coefs[order(game,grepl("cost",Attribute),Attribute),]
  
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
  dt.wtp <- dt.coefs[!grepl("cost",Term),.(game,Attribute,Term,Estimate)]
  dt.wtp[,`WTP (1,000 euro)` := round(Estimate/abs(dt.irr.tp.all[game == "all" & cost_month == FALSE,median_utility]),1)]
  
  print_table(dt.wtp[,.(Term,`WTP (1,000 euro)`)],"results_wtp.tex")
  
  
}

