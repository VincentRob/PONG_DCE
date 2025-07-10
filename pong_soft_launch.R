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

survey.names =c("all_panelclix_hoorn") # names(files.path)#c("soft_launch_pooled")

invest.list.lvl <- list("insu" = 5000,"heat_networks" = 2000, "heat_pumps" = 7000)

for (survey.name in survey.names){
  
  dir.out <- paste0("output/",survey.name,"/")
  dir.create(dir.out)
  
  data = data.table()
  for (file.name in input_specs[survey_name == survey.name,unique(data_path)]){
    data_tmp <- readxl::read_excel(paste0(file.name)) %>% as.data.table()
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
  col.desc <- names(data)[!grepl("(Choice\\ [0-9])|(Question time)|(groupTime)|(randNumber)|(choice screen)|(GXQ00001)|(randSet1)|(pid$)|(cid$)|(^GXQ)|(^r[0-9])|(Response ID)|(Date)|(Seed)|(zipcode)|(If yes\\, how many\\?)|(Technology A)|(Technology B)|(lottery)|(Thank you for your participation)|(Total time)|(save 50 euros)|(please enter the code that is in your letter)",names(data))]
  data.desc <- janitor::clean_names(data[,.SD,.SDcols = col.desc])
  names.desc <- setNames(col.desc,names(data.desc))
  
  tab.desc <- data.table()
  for (cl in names(data.desc)){
    tab.desc <- rbindlist(list(data.desc[,.(Q = names.desc[cl] ,Q_clean=cl,.N),by = c(answer = cl)],tab.desc),use.names = TRUE,fill = TRUE)
  }
  tab.desc[,Q_org := copy(Q)]
  tab.desc[grepl("\\[multiple",Q_org), `:=` (Q = gsub("(.*?)\\ \\[multipl.*","\\1",Q_org), answer = paste0(gsub(".*\\[(.*)\\].*","\\1",Q_org)," - ",answer))]
  tab.desc[,Q := gsub(".*?\\.(.*)","\\1",Q)]
  tab.desc[,unique(Q)]
  
  # Descriptives mapping template
  # Add rank based on unique Q
  tab.desc[, question_rank := .GRP, by = Q]
  
  # Get unique questions
  questions <- unique(tab.desc[, .(Q, question_rank)])
  
  # Build nested result
  result_list <- setNames(
    lapply(split(tab.desc, by = "Q", keep.by = TRUE), function(group) {
      answers_list <- setNames(
        lapply(seq_len(nrow(group)), function(i) {
          ans <- group$answer[i]
          list(
            answer_tags = list(ans),
            answer_rank = i
          )
        }),
        group$answer
      )
      
      list(
        question_tags = list(rep(group$Q[1], 2)),
        question_rank = group$question_rank[1],
        answers = answers_list
      )
    }),
    unique(tab.desc$Q)
  )
  
  # Output JSON
  write(toJSON(result_list, pretty = TRUE, auto_unbox = TRUE), file = paste0(dir.out,"mapping_descriptives_template.json"))
  
  tab.grps <- read.csv2(paste0(dir.out,"descriptives_mapping_complete.csv")) 
  setDT(tab.grps)
  tab.desc[tab.grps,Group := i.Group,on=c("Q" = "Question")]
  
  tab.desc[,N_total := sum(N,na.rm = TRUE),by=Q_org]
  
  for (grp in tab.desc[,unique(na.omit(Group))]){
    
    if (grp == "bias_monet"){
      # All answers
      tab <-  data.desc[,.(Answer = last9_if_you_could_save_50_euros_per_month_from_now_on_by_better_insulating_your_home_how_much_would_you_be_willing_to_pay_to_have_that_insulation_installed)]
      
      ggplot(tab,aes(x = Answer)) + geom_histogram(binwidth = 500, fill=grey2, col=grey3) + theme.graphs
      ggsave(paste0(dir.out, "wtp_50_eur.pdf"),device = "pdf", units = "cm",height = height, width = width)
      
      tab.mean <- tab[,.(Mean = mean(Answer,na.rm=TRUE),Median = median(Answer,na.rm=TRUE))]
      
      sink(paste0(dir.out, "wtp_50_eur_mean.tex"))
      print(xtable::xtable(tab.mean), type = "latex")
      sink()
      
      # Answers by insulation groups
      nm.wall <- names(data.desc)[grepl("hollow_wall_insulation",names(data.desc))]
      nm.roof <- names(data.desc)[grepl("roof_and_or_floor_insulation",names(data.desc))]
      nm.glass <- names(data.desc)[grepl("high_efficiency_double_or_triple_glazing",names(data.desc))]
      
      if (length(nm.glass) == 1 & length(nm.roof) == 1 & length(nm.wall) == 1){
        
        tab <-  data.desc[,.(Answer = last9_if_you_could_save_50_euros_per_month_from_now_on_by_better_insulating_your_home_how_much_would_you_be_willing_to_pay_to_have_that_insulation_installed,
                             insu_wall  = get(nm.wall),
                             insu_roof_floor = get(nm.roof),
                             insu_glass = get(nm.glass))]
        
        tab[, any_insulation := as.factor(insu_wall == "Yes" | insu_glass == "Yes" | insu_roof_floor == "Yes")]
        
        ggplot(tab,aes(x = Answer,group = any_insulation, fill = any_insulation)) + geom_histogram(binwidth = 500, col=grey3) + theme.graphs
        ggsave(paste0(dir.out, "wtp_50_eur_insu.pdf"),device = "pdf", units = "cm",height = height, width = width)
        
        
        tab.mean <- tab[,.(Mean = mean(Answer,na.rm=TRUE),Median = median(Answer,na.rm=TRUE)),by=.(any_insulation)]
        
        sink(paste0(dir.out, "wtp_50_eur_insu_mean.tex"))
        print(xtable::xtable(tab.mean), type = "latex")
        sink()
        
      }
      
      
      next
    }
    
    tab <- tab.desc[Group == grp,.(Question = Q, Answer = answer, Share = sprintf("%.0f%% (%.d)", 100*N/N_total,N))]
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

