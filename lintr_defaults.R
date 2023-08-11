library(lintr)

# Excluded linters
# closed_curly_linter
# camel_case_linter
# implicit_integer_linter
# paren_brace_linter
# pipe_continuation_linter
# trailing_blank_lines_linter
# commented_code_linter
# object_usage_linter,
# extraction_operator_linter

filename <- "ui.R"
lint(filename, linters=list(
  absolute_path_linter(),
  assignment_linter(),
  brace_linter(),
  commented_code_linter(),
  commas_linter(),
  cyclocomp_linter(),
  equals_na_linter(),
  function_left_parentheses_linter(),
  infix_spaces_linter(),
  line_length_linter(),
  nonportable_path_linter(),
  object_length_linter(),
  object_name_linter("camelCase"),
  object_usage_linter(),
  paren_body_linter(),
  quotes_linter("\""),
  semicolon_linter(),
  seq_linter(),
  spaces_inside_linter(),
  spaces_left_parentheses_linter(),
  T_and_F_symbol_linter(),
  todo_comment_linter(),
  trailing_blank_lines_linter(),
  trailing_whitespace_linter(),
  undesirable_function_linter(),
  undesirable_operator_linter(),
  unnecessary_concatenation_linter(),
  vector_logic_linter(),
  whitespace_linter()
))
