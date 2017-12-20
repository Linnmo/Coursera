#
#' @title Praise WQ
#' @name WQschaumal
#' @description Build friendly R packages that
#' praises wq seriously, hahaha
#' inspired by Praise

adjective <- c(
  "adorable",
  "attrative",
  "astonishing",
  "breathtaking",
  "brilliant",
  "charming",
  "chic",
  "classy",
  "cute",
  "delightful",
  "distinctive",
  "epic",
  "excellent",
  "exclusive",
  "extraordinary",
  "fabulous",
  "fancy",
  "fantabulous",
  "fascinating",
  "geometric",
  "glorious",
  "irresistible",
  "just wow",
  "lovely",
  "luminous",
  "magnificent",
  "mathematical",
  "metal",
  "mind-blowing",
  "proper",
  "solid",
  "special",
  "spectacular",
  "serious",
  "terrific",
  "tremendous",
  "tubular",
  "ultimate",
  "unique",
  "wicked",
  "wise",
  "wonderful",
  "wondrous"
)

adverb_manner <- c(
  "seriously"
)
adverb <- adverb_manner


exclamation <- c(
  "Ahh",
  "Aww",
  "Mmm"
)

rpackage <- c(
  "code",
  "library (or package?)",
  "package",
  "program",
  "project",
  "software",
  "R package"
)

smiley <- c(
  ":)",
  ":D",
  ";)"
)

praise_parts <- list(
  adjective = adjective,
# adverb = adverb,
# adverb_manner = adverb_manner,
#  created = created,
#  creating = creating,
  exclamation = exclamation,
  rpackage = rpackage,
  smiley = smiley
)


WQ <- function(template = "WQ, you are seriously ${adjective}! If you happen to see this <3, I have something to tell you.. It is never late.") {
  while (is_template(template)) {
    template <- replace_one_template(template)
  }
  template
}


template_pattern <- "\\$\\{([^\\}]+)\\}"


is_template <- function(x) grepl(template_pattern, x)


replace_one_template <- function(template) {
  match <- regexpr(template_pattern, template, perl = TRUE)

  template1 <- substring(
    template,
    match,
    match + attr(match, "match.length") - 1L
  )

  part <- substring(
    template,
    attr(match, "capture.start"),
    attr(match, "capture.start") + attr(match, "capture.length") - 1L
  )

  match_case_sub(
    template1,
    part,
    sample(praise_parts[[tolower(part)]], 1),
    template
  )
}


match_case_sub <- function(pattern, part, replacement, text) {
  if (toupper(part) == part) {
    replacement <- toupper(replacement)
  } else if (capitalize(part) == part) {
    replacement <- capitalize(replacement)
  }

  sub(pattern, replacement, text, fixed = TRUE)
}

capitalize <- function(x) {
  paste0(
    toupper(substring(x, 1, 1)),
    substring(x, 2)
  )
}
