# count words
sources <- c("Rmd/00-preface.Rmd",
            "Rmd/01-intro.Rmd",
            "Rmd/02-data.Rmd",
            "Rmd/03-comparability.Rmd",
            "Rmd/04-equategroups.Rmd",
            "Rmd/05-modelingequates.Rmd",
            "Rmd/06-ability.Rmd",
            "Rmd/07-sdgindicator.Rmd",
            "Rmd/08-ontrack.Rmd",
            "Rmd/09-discussion.Rmd",
            "Rmd/Appendix-references.Rmd")

words <- sapply(sources, FUN = wordcountaddin::word_count)
data.frame(words)

# excludes references
sum(words)
