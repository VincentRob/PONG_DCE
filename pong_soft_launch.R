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

survey.names =c("panelclix_hoorn","full_launch_panelclix") # names(files.path)#c("soft_launch_pooled")

invest.list.lvl <- list("insu" = 5000,"heat_networks" = 2000, "heat_pumps" = 7000)

for (survey.name in survey.names){
  
  dir.out <- paste0("output/",survey.name,"/")
  dir.create(dir.out)
  
  data = data.table()
  for (file.name in input_specs[survey_name == survey.name,unique(data_path)]){
    data_tmp <- readxl::read_excel(paste0(file.name)) %>% as.data.table()
    data_tmp[,survey_name := gsub("^.*/|\\.xlsx$", "", file.name)]
    
    # Remove emoji
    names(data_tmp) <- sapply(names(data_tmp),\(x) gsub("[^\x01-\x7F]", "", x)) 
    
    data <- rbindlist(list(data,data_tmp),use.names = TRUE, fill = TRUE)
  }
  
  
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
  col.desc <- names(data)[!grepl("(Choice\\ [0-9])|(Question time)|(groupTime)|(randNumber)|(choice screen)|(GXQ00001)|(randSet1)|(pid$)|(cid$)|(^GXQ)|(^r[0-9])|(Date)|(Seed)|(zipcode)|(If yes\\, how many\\?)|(Technology A)|(Technology B)|(Thank you for your participation)|(Total time)|(please enter the code that is in your letter)",names(data))]
  data.desc <- data[,.SD,.SDcols = col.desc]
  
  #old.nms <- names(data.desc)[grepl("\\[multiple",names(data.desc))]
  #setnames(data.desc,old = old.nms,new = gsub("(.*?)\\ \\[multipl.*","\\1",old.nms))
  old.nms <- names(data.desc)
  new.nms <- gsub(".*?\\.(.*)","\\1",names(data.desc))
  new.nms[new.nms == ""] <- old.nms[new.nms == ""]
  
  names(data.desc) <- new.nms
  
  # Map descriptives
  # Read JSON mapping descriptives
  fljson <- paste0("input/mapping_descriptives.json")
  mapping <- jsonlite::read_json(fljson)
  
  
  # Loop over each question and rename
  for (new_q_name in names(mapping)) {
    
    old_q_names <- unlist(mapping[[new_q_name]][["question_tags"]])
    
    old.nms <- names(data.desc)
    new.nms <- old.nms
    
    if (!any(new.nms %in% old_q_names)) stop(paste0('`',new_q_name,"` not mapped"))
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
  
  for (grp in unique(unlist(grp.q))){
    
    if (grp == "bias_monet"){
      question_cols <- names(grp.q[grp.q == grp])
      for (monet_question in question_cols){
        tab <- data.desc[,.(Answer = get(monet_question))]
        
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
    
    # Subset question columns by group
    question_cols <- names(grp.q[grp.q == "desc"])
    
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
    
    
    tab.body <-  tab[, .(Question2 = c(as.character(head(Question,1)), Answer),Share = c("", Share)), by = .(Group = Question)][,-"Group"]
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
  
  # Lottery quartiles ----
  nm.lottery <- names(data.desc)[grepl("lottery",names(data.desc))]
  data.desc[,lottery_quantile := cut(get(nm.lottery),breaks = quantile(get(nm.lottery),c(0,0.25,0.75,1),na.rm=TRUE),include.lowest=TRUE)]
  
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
  ref.lvl <- "_2$"
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
  game <- "insu"
  dt.insu <- get_df_logit(data,game,cst,ref.lvl,data.desc)
  
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
  
  ## Insulation heterogeneity ----
  run_ht_insu <- function(grp.ht.list,run.name){
    logit.insu.list <- list(Main = logit_interact.insu)
    coef_names_ht <- c()
    for (grp.ht in names(grp.ht.list)){
      dt.insu.tmp <- copy(dt.insu)
      setnames(dt.insu.tmp, old = grp.ht.list[[grp.ht]], new = grp.ht)
      
      logit_interact.insu.ht <- mlogit(
        formula = as.formula(gsub("none",paste0("`",grp.ht,"`:none"),formula.insu)),
        dt.insu.tmp
      )
      logit.insu.list[[grp.ht]] <- logit_interact.insu.ht
      nm.tmp <- names(logit_interact.insu.ht$coefficients)
      nm.tmp.org <- nm.tmp[grepl(grp.ht,nm.tmp)]
      nm.tmp.new <- gsub(grp.ht,"",nm.tmp.org)
      nm.tmp.new <- gsub("``","",nm.tmp.new)
      names(nm.tmp.new) <- gsub("`","",nm.tmp.org)
      coef_names_ht <- c(coef_names_ht,nm.tmp.new)
    }
    
    tab.tex <- modelsummary::modelsummary(logit.insu.list, 
                                          coef_rename = c(coef_names[names(coef_names) %in% col.att.int],coef_names_ht), 
                                          add_rows = `attr<-`(do.call(cbind, c(list(rows.refs), rep(list(rows.refs[names(rows.refs) != "term"]), length(logit.insu.list)-1))), "position", attr(rows.refs, "position")),
                                          shape = term ~ statistic,
                                          estimate = "{estimate}",
                                          statistic = stats.table,
                                          output = "latex",
                                          stars = c("*" = .1, "**" = .05, "***" = 0.01)) 
    tab.tex <- add_tinysize(tab.tex,"^\\\\begin\\{table\\}")
    lines <- unlist(strsplit(tab.tex, "\n"))
    filtered_lines <- lines[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}", lines)]
    
    writeLines(filtered_lines, paste0(dir.out, "results_heterogeneity_", game,"_",run.name, ".tex"))
    
  }
  
  grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")
  
  for (grp.ht in names(grp.ht.list)){
    run_ht_insu(grp.ht.list[[grp.ht]],grp.ht)
  }
  
  ## Tech results ----
  game <- "tech"
  dt.tech <- get_df_logit(data,game,cst,ref.lvl,data.desc)
  
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
  
  dt.coefs <- rbindlist(list(dt.coefs,parse_modelsummary_latex(tab.tex,game)),use.names = TRUE,fill = TRUE)   
  
  writeLines(add_scriptsize(tab.tex,"^\\\\begin\\{table\\}"), paste0(dir.out, "results_", game, ".tex"))
  
  ## Tech heterogeneity ----
  run_ht_tech <- function(grp.ht.list,run.name){
    logit.tech.list <- list(Main = logit_interact.tech)
    coef_names_ht <- c()
    for (grp.ht in names(grp.ht.list)){
      dt.tech.tmp <- copy(dt.tech)
      setnames(dt.tech.tmp, old = grp.ht.list[[grp.ht]], new = grp.ht)
      
      logit_interact.tech.ht <- mlogit(
        formula = as.formula(gsub("none",paste0("`",grp.ht,"`:none"),formula.tech)),
        dt.tech.tmp
      )
      logit.tech.list[[grp.ht]] <- logit_interact.tech.ht
      nm.tmp <- names(logit_interact.tech.ht$coefficients)
      nm.tmp.org <- nm.tmp[grepl(grp.ht,nm.tmp)]
      nm.tmp.new <- gsub(grp.ht,"",nm.tmp.org)
      nm.tmp.new <- gsub("``","",nm.tmp.new)
      names(nm.tmp.new) <- gsub("`","",nm.tmp.org)
      coef_names_ht <- c(coef_names_ht,nm.tmp.new)
    }
    
    tab.tex <- modelsummary::modelsummary(logit.tech.list, 
                                          coef_rename = c(tech.map[names(tech.map) %in% col.att.int],coef_names_ht), 
                                          add_rows = `attr<-`(do.call(cbind, c(list(rows.refs), rep(list(rows.refs[names(rows.refs) != "term"]), length(logit.tech.list)-1))), "position", attr(rows.refs, "position")),
                                          shape = term ~ statistic,
                                          estimate = "{estimate}",
                                          statistic = stats.table,
                                          output = "latex",
                                          stars = c("*" = .1, "**" = .05, "***" = 0.01)) 
    tab.tex <- add_tinysize(tab.tex,"^\\\\begin\\{table\\}")
    lines <- unlist(strsplit(tab.tex, "\n"))
    filtered_lines <- lines[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}", lines)]
    
    writeLines(filtered_lines, paste0(dir.out, "results_heterogeneity_", game,"_",run.name, ".tex"))
    
  }
  
  grp.ht.list <- fromJSON("input/mlogit_heterogeneity_groups.json")
  
  for (grp.ht in names(grp.ht.list)){
    run_ht_tech(grp.ht.list[[grp.ht]],grp.ht)
  }
  
  
  
  
  # Coef table ----
  
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
  
  # Table for BEcrowd ----
  dt.bcrwd <- dt.coefs[game == "insu",]
  
  for (i in 1:dim(dt.bcrwd)[1]){
    dt.bcrwd[i,(names(dt.bcrwd)[grepl("Term",names(dt.bcrwd))]) := lapply(.SD,\(x) stringr::str_to_sentence(gsub(paste0("(",Attribute,": )| (\\(reference\\))"),"",x))),.SDcols = (names(dt.bcrwd)[grepl("Term",names(dt.bcrwd))])]
  }
  
  dt.bcrwd[Attribute != "None",Attribute_int := .GRP,by=Attribute]
  dt.bcrwd[Attribute == "None",Attribute_int := 0]
  dt.bcrwd[,Level_int := (1:.N)-1,by=Attribute]
  
  dt.bcrwd.val <- dt.bcrwd[,.(Attribute_int = 99,Attribute = "Valuation",Term = "thousand euro",Level_int = 1,Estimate = Estimate[Attribute == "One time amount" & Term == "10,000 euros."]/5)]
  dt.bcrwd <- rbindlist(list(dt.bcrwd,dt.bcrwd.val),use.names = TRUE,fill = TRUE)
  
  dt.bcrwd.to.csv <- dt.bcrwd[,.(attribute_nr = Attribute_int,attribute_text = Attribute, level_nr = Level_int,level_text = Term,weight = Estimate)]
  dt.bcrwd.to.csv[is.na(weight),weight := 0]
  
  write.csv2(dt.bcrwd.to.csv,paste0(dir.out, "becrowd_table.csv"),row.names = FALSE,quote = FALSE)
  
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

