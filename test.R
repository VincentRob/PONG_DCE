library(data.table)
library(jsonlite)

# Sample input
dt <- data.table(
  question = c("What is R?", "What is R?", "What is Python?", "What is Python?", "What is Python?"),
  answer = c("A programming language", "Used for statistics", "A programming language", "Used for ML", "Widely used")
)

# Assign a unique rank to each question group
dt[, question_rank := .GRP, by = question]

# Get unique questions
questions <- unique(dt[, .(question, question_rank)])

# Build the nested list using question text as key
result_list <- setNames(
  lapply(1:nrow(questions), function(i) {
    q <- questions[i]
    answers_for_q <- dt[question == q$question, unique(answer)]
    
    # Create answers list with each answer as a named key
    answers_list <- setNames(
      lapply(answers_for_q, function(ans) {
        list(
          answer_tags = list(ans),
          answer_rank = 1
        )
      }),
      answers_for_q
    )
    
    list(
      question_tags = list(rep(q$question, 2)),
      question_rank = q$question_rank,
      answers = answers_list
    )
  }),
  questions$question
)

# Output pretty JSON
cat(toJSON(result_list, pretty = TRUE, auto_unbox = TRUE))
