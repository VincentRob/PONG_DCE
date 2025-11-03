indent_levels <- function(lines, indent = "\\hspace{1em}") {
  # Find first \midrule to avoid indenting the header row
  mid_idx <- which(grepl("^\\s*\\\\midrule\\b", lines))[1]
  before_mid <- if (is.na(mid_idx)) integer(0) else seq_len(mid_idx - 1)
  
  # "Level" rows:
  # - do NOT start with a LaTeX command (\)
  # - are AFTER \midrule
  # - have a non-empty right-hand side (some non-space after & before the trailing \\)
  is_level <- grepl("^\\s*[^\\\\].*?&\\s*\\S.*\\\\\\\\\\s*$", lines, perl = TRUE)
  is_level[before_mid] <- FALSE
  
  # Add indent after any leading spaces. Use a function so backslashes are preserved.
  lines[is_level] <- sub(
    "^(\\s*)",
    function(m) paste0(m, indent),
    lines[is_level],
    perl = TRUE
  )
  
  lines
}

add_scriptsize <- function(tab, insert_after_pattern) {
  # Split the LaTeX content into lines
  lines <- strsplit(tab, "\n")[[1]]
  
  # Find the line index to insert \scriptsize after
  insert_idx <- grep(insert_after_pattern, lines)
  
  # Insert \scriptsize if a match is found
  if (length(insert_idx) > 0) {
    lines <- append(lines, "\\scriptsize", after = insert_idx)
  }
  
  # Recombine the lines into a single LaTeX string
  return(paste(lines, collapse = "\n"))
}

add_tinysize <- function(tab, insert_after_pattern) {
  # Split the LaTeX content into lines
  lines <- strsplit(tab, "\n")[[1]]
  
  # Find the line index to insert \scriptsize after
  insert_idx <- grep(insert_after_pattern, lines)
  
  # Insert \scriptsize if a match is found
  if (length(insert_idx) > 0) {
    lines <- append(lines, "\\tiny", after = insert_idx)
  }
  
  # Recombine the lines into a single LaTeX string
  return(paste(lines, collapse = "\n"))
}

get_json_mapping_4_coefs <- function(game,col.att.int){
  # Function to replace number with label
  
  fljson <- paste0("input/mapping_",game,".json")
  
  mapping <- jsonlite::read_json(fljson)
  
  # Reverse the mapping: integer -> label
  reverse_mapping <- lapply(names(mapping),\(x) setNames(list("a" = sapply(mapping[[x]]$levels,\(y) y$rank)),x))
  reverse_mapping <- do.call(c, reverse_mapping)
  
  rename_col <- function(colname) {
    parts <- strsplit(colname, "_")[[1]]
    base <- paste(parts[-length(parts)], collapse = "_")
    tech = as.integer(gsub("factor\\(tech\\)([0-9]).*","\\1",base))
    base <- gsub("factor\\(tech\\)[0-9]\\:","",base)
    code <- as.integer(parts[[length(parts)]])
    if (!is.null(reverse_mapping[[base]]) && code %in% reverse_mapping[[base]]) {
      label <- names(reverse_mapping[[base]][which(reverse_mapping[[base]] == code)])
      if (!is.na(tech) & (tech %in% c(0,1))){
        label <- label[grepl(paste0("tech",tech),label)]
        label <- gsub(paste0("tech",tech," "),"",label)
        label.sentence <- stringr::str_to_sentence(paste0(if_else(tech == 1,"Heat pump ","Heat network "),gsub("_"," ",base), ": ", label))
      } else {
        label.sentence <- stringr::str_to_sentence(paste0(gsub("_"," ",base), ": ", label))
      }
      
      return(label.sentence)
    }
    return(colname)
  }
  
  # Apply to all column names
  new_cols <- sapply(col.att.int, rename_col)
  new_cols["none"] <- "None"
  
  return(new_cols)
}

wtp_logit <- function(logit_interact,invest.lvl,savings.lvl,invest.name = "one_time_amount",savings.name = "heating_costs",ref = 2){
  
  logit_wtp <- copy(logit_interact)
  wtp.lvl <- -logit_wtp$coefficients[grepl(paste0(invest.name,"_",ref),names(logit_wtp$coefficients)) & !is.na(logit_wtp$coefficients)]/invest.lvl
  
  if (length(wtp.lvl) != 1) stop("More than one level found")
  
  logit_wtp$coefficients <- logit_wtp$coefficients/wtp.lvl*if_else(grepl(savings.name,names(logit_wtp$coefficients)),1/savings.lvl,1)*if_else(grepl(invest.name,names(logit_wtp$coefficients)),1/invest.lvl,1)
  logit_wtp$coefficients <- round(logit_wtp$coefficients,2)
  
  return(logit_wtp)
}

irr <- function(cst,cf,yrs) {
  if (is.null(cst) | is.null(cf)) return(NA);
  if (cst <=0 | cf <=0) return(NA);
  q <- unlist(mapply(\(cf.var) uniroot(\(x) cf.var*x*(1-x**yrs)/(1-x)-cst,c(0,1000),extendInt = "yes",maxiter = 1e9)$root,cf),use.names = FALSE); return(1/q-1)  
} 

aar <- function(cst,cf,yrs) {if (cst <=0 | cf <=0) return(NA);
  q = ((yrs*cf)/cst)^(1/yrs)-1; return(q)
} 


get_row_refs <- function(coef_names,col.att.int,col.ref,ref.lvl,stats.table = NA){
  rows.refs <- data.frame(term = paste0(coef_names[names(coef_names) %in% col.ref],ifelse(lang == "en"," (reference)"," (referentie)")),
                          estimate = "")
  if (!gbutils::isNA(stats.table)){
    for (i in 1:length(stats.table)){
      rows.refs[stats.table[[1]]] = ""
    }
  }
  
  shft.tmp <- as.integer(gsub(".*([0-9]{1}).*","\\1",ref.lvl)) - 1 
  
  rows.refs.pos <- sapply(gsub(ref.lvl,"",names(coef_names[names(coef_names) %in% col.ref])),\(x) first(which(grepl(x,names(coef_names[names(coef_names) %in% col.att.int]),fixed = TRUE))) + shft.tmp)
  rows.refs.pos <- rows.refs.pos + 0:(length(rows.refs.pos)-1)
  attr(rows.refs, "position") <- rows.refs.pos
  return(rows.refs)
}


map_levels_to_keys <- function(dt, game, lang = "nl", term_col = "Term", new_col = "key_term", override_dir = "input") {
  # Make a copy of the input dt
  dt_copy <- copy(dt)
  
  # Build path to JSON override file based on game
  json_path <- file.path(override_dir, paste0("coef_name_override_", game, ".json"))
  
  if (!file.exists(json_path)) {
    stop("Override JSON file not found for game: ", game, " at path: ", json_path)
  }
  
  # Load JSON mapping
  mapping <- fromJSON(json_path, simplifyVector = FALSE)
  
  # Build lookup: text in chosen language -> key
  lookup <- sapply(names(mapping), function(k) mapping[[k]][[lang]])
  names(lookup) <- names(mapping)
  
  # Ensure dt_copy is a data.table
  if (!is.data.table(dt_copy)) dt_copy <- as.data.table(dt_copy)
  
  # Add new column with technical key
  dt_copy[, (new_col) := {
    sapply(get(term_col), function(term) {
      term_clean <- sub("\\s*\\(referentie\\)$", "", term)
      match_key <- names(lookup)[lookup == term_clean]
      if (length(match_key) == 1) match_key else NA_character_
    })
  }]
  
  # Return the modified copy
  return(dt_copy)
}

translate_coef_names <- function(coef_names, game, lang = "nl", override_dir = "input") {
  # Validate language
  stopifnot(lang %in% c("en", "nl"))
  
  # Build path to JSON override file
  override_path <- file.path(override_dir, paste0("coef_name_override_", game, ".json"))
  
  if (!file.exists(override_path)) {
    warning("No override JSON found at: ", override_path)
    return(coef_names)
  }
  
  # Load override bilingual mapping
  override <- fromJSON(override_path, simplifyVector = FALSE)
  
  # Build lookup from English → target language
  lookup <- sapply(override, function(x) x[[lang]], USE.NAMES = FALSE)
  names(lookup) <- sapply(override, function(x) x$en, USE.NAMES = FALSE)
  
  # Replace English values with chosen language, if available
  reencoded <- vapply(coef_names, function(v) {
    if (v %in% names(lookup)) lookup[[v]] else v
  }, character(1L))
  
  return(reencoded)
}


simu_subsid <- function(cf.simu,invest.lvl,ref.invest.wtp,invest.name,game,ref.lvl){
  
  # 2 packages: insu good and none
  pack <- data.table(level = c("package",names(cf.simu)))
  
  lvls <- pack[,unique(na.omit(as.integer(gsub(".*_([0-9])$","\\1",level))))]
  lvls <- sort(c(lvls,as.integer(gsub(".*([0-9]).*","\\1",ref.lvl))))
  
  if (anyDuplicated(lvls) != 0) stop("Duplicate packages")
  
  for (lvl in lvls){
    pack[,(paste0("pack_",lvl)) := as.integer(grepl(paste0("_",lvl,"$"),level))]
  }
  pack[,pack_none := 0]
  pack[level == "none",pack_none := 1]
  
  # pivot
  pack.t <- data.table(t(pack),keep.rownames = TRUE)
  setnames(pack.t,old = names(pack.t),new = as.character(pack.t[rn == "level",]))
  pack.t <- pack.t[level != "level",]
  nm.to.int <- names(pack.t)[names(pack.t) != "level"]
  pack.t[,(nm.to.int) := lapply(.SD,as.integer),.SDcols = nm.to.int]
  
  # Scenarios
  tab.simu <- rbindlist(list(cbind(pack.t,data.table(subsid = 0,id=1)),
                             cbind(pack.t,data.table(subsid = 1,id=2)),
                             cbind(pack.t,data.table(subsid = 2,id=3))))
  
  tab.simu[,`:=` (u.simu= NULL,p.simu= NULL,choice.simu = NULL)]
  
  # Pre subsidy utility
  tab.simu[,u.simu := as.matrix(tab.simu[,.SD,.SDcols = names(cf.simu)]) %*% as.matrix(t(cf.simu))]
  
  # Add subsidies
  tab.simu[,u.simu := u.simu - as.integer(none == 0)*subsid*cf.simu[,get(paste0(invest.name,"_",ref.invest.wtp))]]
  
  tab.simu[,p.simu := exp(u.simu)/(exp(u.simu) + exp(u.simu[level == "pack_none"])),by=.(id)]
  
  tab.simu <- tidyr::pivot_wider(tab.simu[,.(level,subsid,p.simu)],names_from = "level",values_from = "p.simu")
  setDT(tab.simu)
  tab.simu[,(names(tab.simu)[!(names(tab.simu) %in% "subsid")]) := lapply(.SD,\(x) paste0(round(100*x,1),"%")),
           .SDcols = (names(tab.simu)[!(names(tab.simu) %in% "subsid")])]
  
  tab.simu[,subsid := subsid * invest.lvl]
  tab.simu[,pack_none := NULL]
  nm <- ifelse(game == "insu","Insulation","Gas-free heating")
  setnames(tab.simu,old = names(tab.simu), new = c("Subsidy level",paste0(nm," ",lvls)))
  
  return(tab.simu)
}

get_id_question <- function(choice.str) gsub("^(.*?)\\..*$","\\1",choice.str)

transform_to_mlogit_data <- function(dt.ce.int,ref.lvl.reg,data.desc = NULL){
  # Unique ID
  dt.ce.int[, qid := paste(id_respondent, id_question, sep = "_")]
  
  # To dummies
  col.att.int <- names(dt.ce.int)[!(names(dt.ce.int) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid","tech"))]
  dt.ce.dum  <- fastDummies::dummy_cols(dt.ce.int,select_columns = col.att.int, remove_selected_columns  = TRUE)
  
  # Drop none level 
  dt.ce.dum[,(names(dt.ce.dum)[grepl("_0$",names(dt.ce.dum))]) := NULL]
  
  # None column
  dt.ce.dum[,none := as.integer(package == " Arrange independently later")]
  
  # Drop one level per question
  cols.ref <- names(dt.ce.dum)[grepl(ref.lvl.reg,names(dt.ce.dum))]
  dt.ce.dum[,(cols.ref) := NULL]
  
  col.att.dum <- names(dt.ce.dum)[!(names(dt.ce.dum) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk,"qid"))]
  
  dt.ce.dum[,package_int := as.integer(factor(package))]
  dt.ce.dum[,qid_int := as.integer(factor(qid))]
  dt.ce.dum <- dt.ce.dum[order(qid,-package),]
  dt.ce.dum[,choice := as.integer(choice)]
  
  if(!is.null(data.desc)){
    setnames(data.desc,names(data.desc),trimws(names(data.desc)))
    cols.to.merge <- names(data.desc)[!(names(data.desc) %in% names(dt.ce.dum))]
    if (data.desc[,class(`Response ID`) == "character"] & dt.ce.dum[,class(id_respondent) == "numeric"] ) data.desc[,`Response ID` := as.integer(`Response ID`)]
    dt.ce.dum[data.desc,(paste0("desc_",cols.to.merge)) := mget(paste0("i.",cols.to.merge)),on = c("id_respondent" = "Response ID")]
    col.att.dum <- c(col.att.dum,paste0("desc_",cols.to.merge))
  }
  
  dt.ce.to.logit <- dfidx(as.data.frame(dt.ce.dum[,.SD,.SDcols = c("qid_int","package_int",cst$ch,col.att.dum) ]),shape = "long",choice = cst$ch, idx = c("qid_int","package_int"))
  
  return(dt.ce.to.logit)
}


data_to_choice_situations <- function(choice.str.all,n.att,n.choices,n.headers, none_package_is_one_level = NA){
  choice.att.lvl.all <- data.table()
  for (choice.str in choice.str.all){
    choice.split <- strsplit(choice.str,split = "  ")[[1]]
    choice.split <- choice.split[gsub(paste0("^\\ |",intToUtf8(160)),"",choice.split) != ""]
    
    choice.header <- choice.split[1:n.headers]
    packages <- choice.header[3:n.headers] 
    if (!is.na(none_package_is_one_level)){
      choice.split <- choice.split[!grepl(none_package_is_one_level,choice.split)]
      #packages <- choice.header[3:(n.headers-1)] 
    }
    
    choice.table <- choice.split[(n.headers+1):(n.att*(n.choices+1)+n.headers)]
    choice.attributes <- choice.table[seq(from = 1, to = length(choice.table), by = n.choices +1)]
    choice.att.lvl <- data.table(id_question = get_id_question(choice.str),package = packages)
    for (ch in choice.attributes){
      idx <- which(choice.table == ch)
      choice.lvl <- choice.table[(idx+1):(idx + n.choices)]
      if (!is.na(none_package_is_one_level)){
        choice.lvl <- c(choice.lvl,none_package_is_one_level)
      }
      choice.att.lvl[,(ch) := choice.lvl]
    }
    choice.att.lvl.all <- rbindlist(list(choice.att.lvl.all,choice.att.lvl),use.names = TRUE,fill = TRUE)
  }
  return(choice.att.lvl.all)
}

data_to_choices <- function(choice.str.all){
  choice.choice.all <- data.table()
  for (choice.str in choice.str.all){
    choice.choice <- data[!is.na(get(choice.str)),
                          .(id_respondent = `id. Response ID`, id_question = get_id_question(choice.str), choice = get(choice.str) )]
    choice.choice.all <- rbindlist(list(choice.choice.all,choice.choice),use.names = TRUE,fill = TRUE)
  }
  return(choice.choice.all)
}


merge_choices_and_situations <- function(data,game,str_question,n.att,n.choices,n.headers,cst,none_package_is_one_level = NA){
  choice.str <- names(data)[grepl("Choice\\ [0-9]",names(data)) & grepl(str_question,names(data))]
  dt.situ <- data_to_choice_situations(choice.str,n.att, n.choices, n.headers,none_package_is_one_level)
  
  # Read the JSON with integer values filled in
  fljson <- paste0("input/mapping_",game,".json")
  category_map <- jsonlite::read_json(fljson)
  
  # rename attributes
  map_attributes <- lapply(category_map,\(x) unlist(x[["attributes"]],use.names = FALSE))
  col.att <- names(dt.situ)[!(names(dt.situ) %in% c(cst$id_q,cst$id_r,cst$ch,cst$pk))]
  new_names <- lapply(col.att, \(x) names(map_attributes)[(which(sapply(map_attributes,\(y) x %in% y)))])
  names(new_names) <- col.att
  
  nb.mtch <- sapply(new_names,\(x) length(x))
  if (any(nb.mtch != 1)){
    stop(sprintf(
      "Attributes level mapping failed for %s.\n\t Please update %s.\n\t Details: \n\t Not found:%s\n\t Duplicates: %s.",
      game,
      fljson,
      paste(names(nb.mtch[nb.mtch == 0]), collapse = ";"),
      paste(names(nb.mtch[nb.mtch > 1]), collapse = ";")
    ))
  }
  new_names <- unlist(new_names,use.names = TRUE)
  
  # Merge duplicates
  for (nmdup  in which(duplicated(new_names))){
    col_ref <- names(new_names[head(which(new_names == new_names[nmdup]),1)])
    for (col_dup in names(new_names[nmdup])){
      if(dt.situ[,any(!is.na(get(col_ref)) & !is.na(get(col_dup)))]) stop("Merged columns should not overlap")
      dt.situ[,(col_ref) := fcoalesce(get(col_ref), get(col_dup))]
      dt.situ[,(col_dup) := NULL]
    }
  }
  
  setnames(dt.situ,new = new_names,old = names(new_names), skip_absent=TRUE)
  
  
  # If tech, merge monetary levels and then interact in model
  if (game == "tech") {
    dt.situ[,tech := as.integer(grepl("((31)|(24)|(17)|(10))\\,",one_time_costs))]
    cols.costs <- names(dt.situ)[grepl("cost|amount",names(dt.situ))]
    dt.situ[,(cols.costs) := lapply(.SD,\(x) paste0("tech",tech," ",trimws(x))),.SDcols = cols.costs]
  }
  
  # Loop over each attribute (column) from the JSON
  for (shortname in names(category_map)) {
    if (shortname %in% names(dt.situ)) {
      
      # Get the levels and their tags/ranks
      levels_info <- category_map[[shortname]]$levels
      
      for (lvl in seq_along(levels_info)){
        if (dt.situ[,!any(trimws(get(shortname)) %in% trimws(unlist(levels_info[[lvl]]$tags)))]){
          print(dt.situ[, unique(get(shortname))])
          stop(paste0("Update mapping of ",shortname, " in ",fljson))
        }
        dt.situ[trimws(get(shortname)) %in% trimws(unlist(levels_info[[lvl]]$tags)), (shortname) := levels_info[[lvl]]$rank]
      } 
    }
  }
  
  # Remove duplicated choices (happens when two surveys, with identical packages but different names, are merged)
  dt.situ <- dt.situ[!duplicated(dt.situ),]
  
  if (any(dt.situ[,.N != 1,by = c("id_question","package")][,V1])) stop("Different attributes-levels for the same package not allowed")
  
  dt.choice <- data_to_choices(choice.str)
  
  
  dt.ce <- merge.data.table(dt.choice,dt.situ,by = cst$id_q,all = TRUE,allow.cartesian=TRUE) 
  dt.ce <- dt.ce[!is.na(id_respondent)] # Delete questions not asked
  dt.ce[, choice := ifelse(gsub(" ","",choice) == gsub(" ","",package), 1, 0)]
  dt.ce <- janitor::clean_names(dt.ce)
  
  if (dt.ce[,sum(choice)==1,by=c(cst$id_q,cst$id_r)][,any(!V1)])(stop("Need one choice per respodent-question"))
  
  return(dt.ce)
}

get_df_logit <- function(data,game,cst,ref.lvl,data.desc = NULL){
  
  if (game == "insu"){
    str_question = "Home improvement"
    n.att = 6
    n.choices = 2
    n.headers = 5
    none_package_is_one_level = " No change,until you switch."
  } else if (game == "tech"){
    str_question = "Gas-free"
    n.att = 6
    n.choices = 3
    n.headers = 5
    none_package_is_one_level = NA
  }
  dt.insu <- merge_choices_and_situations(data,game,str_question,n.att,n.choices,n.headers,cst,none_package_is_one_level)
  
  dt.insu.to.logit <- transform_to_mlogit_data(dt.insu,ref.lvl, data.desc)
  return(dt.insu.to.logit)
}

run_ht_insu_with_ref_level <- function(grp.ht.list,run.name){
  logit.insu.list <- list(Main = logit_interact.insu)
  coef_names_ht <- c()
  
  for (grp.ht in names(grp.ht.list)){
    
    dt.insu.tmp <- copy(dt.insu)
    if (!(grp.ht.list[[grp.ht]] %in% names(dt.insu.tmp))) next
    setnames(dt.insu.tmp,grp.ht.list[[grp.ht]],grp.ht)
    
    dt.insu.dum  <- fastDummies::dummy_columns(dt.insu.tmp[grp.ht],
                                               remove_most_frequent_dummy = TRUE, 
                                               remove_selected_columns  = TRUE)
    nm.all <- fastDummies::dummy_columns(dt.insu.tmp[grp.ht],
                                         remove_selected_columns = TRUE,
                                         ignore_na = TRUE,
                                         return_generated_variables = TRUE)
    
    nm.all <- nm.all[!grepl("idx\\.",nm.all)]
    dt.insu.dum <- dt.insu.dum %>% select(names(dt.insu.dum)[!grepl("idx\\.",names(dt.insu.dum))])
    
    common <- Reduce(intersect, strsplit(nm.all, "_"))
    ref.lvl.ht <- setdiff(nm.all,names(dt.insu.dum))
    if (length(ref.lvl.ht) > 1) stop(paste0(grp.ht,"More than 1 reference level"))
    ref.lvl.ht <- gsub(paste0("_?", common, collapse = "|"), "", ref.lvl.ht)
    ref.lvl.ht <- gsub("\\.|\\_"," ",ref.lvl.ht)
    
    
    names(dt.insu.dum) <- paste0(names(dt.insu.dum)," (ref",ref.lvl.ht,")")
    
    dt.insu.tmp <- copy(dt.insu)
    dt.insu.tmp[, names(dt.insu.dum)] <- dt.insu.dum
    
    logit_interact.insu.ht <- mlogit(
      formula = as.formula(gsub("none",paste0(paste0("`",names(dt.insu.dum),"`:none",collapse = " + ")," + none"),formula.insu)),
      dt.insu.tmp
    )
    logit.insu.list[[grp.ht]] <- logit_interact.insu.ht
    nm.tmp <- names(logit_interact.insu.ht$coefficients)
    nm.tmp.org <- nm.tmp[grepl(common,nm.tmp)]
    nm.tmp.new <- gsub(common,"",nm.tmp.org)
    nm.tmp.new <- gsub("``","",nm.tmp.new)
    names(nm.tmp.new) <- gsub("`","",nm.tmp.org)
    nm.tmp.new <- gsub("(\\:none)|(none\\:)","",nm.tmp.new)
    nm.tmp.new <- gsub("`_","`",nm.tmp.new)
    nm.tmp.old <- names(nm.tmp.new)
    nm.tmp.new[nm.tmp.new != "none"] <- paste0("none:",nm.tmp.new[nm.tmp.new != "none"])
    names(nm.tmp.new) <- nm.tmp.old
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

run_ht_tech_with_ref_level <- function(grp.ht.list,run.name){
  logit.tech.list <- list(Main = logit_interact.tech)
  coef_names_ht <- c()
  for (grp.ht in names(grp.ht.list)){
    
    dt.tech.tmp <- copy(dt.tech)
    if (!(grp.ht.list[[grp.ht]] %in% names(dt.tech.tmp))) next
    setnames(dt.tech.tmp,grp.ht.list[[grp.ht]],grp.ht)
    
    dt.tech.dum  <- fastDummies::dummy_columns(dt.tech.tmp[grp.ht],
                                               remove_most_frequent_dummy = TRUE, 
                                               remove_selected_columns  = TRUE)
    nm.all <- fastDummies::dummy_columns(dt.tech.tmp[grp.ht],
                                         remove_selected_columns = TRUE,
                                         ignore_na = TRUE,
                                         return_generated_variables = TRUE)
    
    nm.all <- nm.all[!grepl("idx\\.",nm.all)]
    dt.tech.dum <- dt.tech.dum %>% select(names(dt.tech.dum)[!grepl("idx\\.",names(dt.tech.dum))])
    
    common <- Reduce(intersect, strsplit(nm.all, "_"))
    ref.lvl.ht <- setdiff(nm.all,names(dt.tech.dum))
    if (length(ref.lvl.ht) > 1) stop(paste0(grp.ht,"More than 1 reference level"))
    ref.lvl.ht <- gsub(paste0("_?", common, collapse = "|"), "", ref.lvl.ht)
    ref.lvl.ht <- gsub("\\.|\\_"," ",ref.lvl.ht)
    
    
    names(dt.tech.dum) <- paste0(names(dt.tech.dum)," (ref",ref.lvl.ht,")")
    
    dt.tech.tmp <- copy(dt.tech)
    dt.tech.tmp[, names(dt.tech.dum)] <- dt.tech.dum
    
    logit_interact.tech.ht <- mlogit(
      formula = as.formula(gsub("none",paste0(paste0("`",names(dt.tech.dum),"`:none",collapse = " + ")," + none"),formula.tech)),
      dt.tech.tmp
    )
    logit.tech.list[[grp.ht]] <- logit_interact.tech.ht
    nm.tmp <- names(logit_interact.tech.ht$coefficients)
    nm.tmp.org <- nm.tmp[grepl(common,nm.tmp)]
    nm.tmp.new <- gsub(common,"",nm.tmp.org)
    nm.tmp.new <- gsub("``","",nm.tmp.new)
    names(nm.tmp.new) <- gsub("`","",nm.tmp.org)
    nm.tmp.new <- gsub("(\\:none)|(none\\:)","",nm.tmp.new)
    nm.tmp.new <- gsub("`_","`",nm.tmp.new)
    nm.tmp.old <- names(nm.tmp.new)
    nm.tmp.new <- paste0("none:",nm.tmp.new)
    names(nm.tmp.new) <- nm.tmp.old
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

extract_cost_value <- function(term) {
  # Handle 'same as now'
  if (grepl("(same as now)|(hetzelfde als nu)", term, ignore.case = TRUE)) return(0)
  
  # Extract full numeric value, handling commas as thousand separators
  term_clean <- gsub(",", "", term)  # remove thousand separators
  num <- as.numeric(sub(".*?(\\d+(\\.\\d+)?).*", "\\1", term_clean))
  
  if (is.na(num)) return(NA_real_)
  
  if (grepl("(less than now)|(minder dan nu)", term, ignore.case = TRUE)) {
    return(-abs(num))
  } else if (grepl("(more than now)|(meer dan nu)", term, ignore.case = TRUE)) {
    return(abs(num))
  } else {
    # One-time cost or fixed value
    return(num)
  }
}


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

