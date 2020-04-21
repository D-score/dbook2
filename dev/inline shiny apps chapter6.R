#inline shiny apps (CH6)

library(ggplot2)
library(dplyr)
library(dmetric)
library(gtools)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

# d by study
coh_ls <-
  as.character(unlist(mixedsort(unique(
    dmetric::model_lean$dscore["cohort"]
  ))))
#show the sem by cohort
coh_ls <- coh_ls[!is.na(coh_ls)]

count_coh <- reactiveVal(1)

observeEvent(input$run1,{
  old_count = count_coh()
  count_coh(old_count + 1)
})
observeEvent(input$prev1,{
  old_count = count_coh()
  count_coh(old_count - 1)
})

observeEvent(input$cohort_list,{
  count_coh(which(coh_ls == input$cohort_list))
})

observe({
  if (length(count_coh()) > 0) {
    updateSelectInput(session,
                      "cohort_list",
                      choices = coh_ls,
                      selected = coh_ls[count_coh()])
  }
  if (length(count_coh()) == 0) {
    updateSelectInput(session,
                      "cohort_list",
                      choices = coh_ls,
                      selected = coh_ls[1])
  }
})



fluidRow(box(
  column(width = 5, offset = 0,
         selectInput(
           inputId = "cohort_list",
           label = "cohort list",
           choices = ""
         ))))

fluidRow(
  renderPlot({

    print(
      dmetric::plot_d_a_group(
        data = dmetric::gcdg_lean,
        model = dmetric::model_lean,
        by = "cohort",
        ref_name = "gcdg",
        grouping = coh_ls[which(coh_ls == input$cohort_list)],
        print_model_name = FALSE
      )
    )

  })

)


fluidRow(
  column(1, offset = 1, actionButton("prev1", "Prev")),
  column(1, offset = 5, actionButton("run1", "Next"))
)


library(ggplot2)
library(dplyr)
library(dmetric)
library(gtools)
library(shiny)
library(shinyWidgets)
library(shinydashboard)


model <- dmetric::model_lean
data_input <- dmetric::gcdg_lean

# d by study
coh_ls <-
  as.character(unlist(mixedsort(unique(
    dmetric::model_lean$dscore["cohort"]
  ))))
#show the sem by cohort
coh_ls <- coh_ls[!is.na(coh_ls)]

sem_daz <- dmetric::model_lean$dscore %>%
  mutate(agemos = a *12,
         low_d = d - sem,
         high_d = d +sem,
         low_daz = dscore::daz(low_d, x = a),
         high_daz = dscore::daz(high_d, x = a))

sem_daz_a <- sem_daz %>%
  mutate(agecat = cut(agemos, breaks = 0:max(agemos, na.rm=TRUE)),
         agenum = as.numeric(agecat))%>%
  group_by(agenum, cohort) %>%
  summarize (daz = mean(daz, na.rm = TRUE),
             mean_a = mean(a, na.rm=TRUE), #voor omzetting naar daz
             mean_d = mean(d, na.rm = TRUE),
             var_betw = var(d, na.rm = TRUE),
             var_with = sum(sem^2, na.rm = TRUE),
             n =  sum(!is.na(sem))
  ) %>% ungroup() %>%
  mutate(sem_pool = sqrt((var_with + var_betw)/ (n-1)),
         low_d = mean_d - sem_pool,
         high_d = mean_d + sem_pool,
         mean_daz = dscore::daz(mean_d, x = mean_a),
         low_daz = dscore::daz(low_d, x = mean_a),
         high_daz = dscore::daz(high_d, x = mean_a))


count_coh <- reactiveVal(1)

observeEvent(input$run1,{
  old_count = count_coh()
  count_coh(old_count + 1)
})
observeEvent(input$prev1,{
  old_count = count_coh()
  count_coh(old_count - 1)
})

observeEvent(input$cohort_list,{
  count_coh(which(coh_ls == input$cohort_list))
})

observe({
  if (length(count_coh()) > 0) {
    updateSelectInput(session,
                      "cohort_list",
                      choices = coh_ls,
                      selected = coh_ls[count_coh()])
  }
  if (length(count_coh()) == 0) {
    updateSelectInput(session,
                      "cohort_list",
                      choices = coh_ls,
                      selected = coh_ls[1])
  }
})



fluidRow(box(
  column(width = 5, offset = 0,
         selectInput(
           inputId = "cohort_list",
           label = "cohort list",
           choices = ""
         ))))

fluidRow(
  renderPlot({
    print(
      ggplot()+
        geom_point(data = subset(sem_daz,cohort == coh_ls[which(coh_ls == input$cohort_list)]),aes(x = agemos, y = daz), color = "lightgrey")+
        geom_errorbar(data = subset(sem_daz,cohort == coh_ls[which(coh_ls == input$cohort_list)]),aes(x = agemos, ymin = low_daz, ymax = high_daz), color = "lightgrey")+
        geom_point(data = subset(sem_daz_a,cohort == coh_ls[which(coh_ls == input$cohort_list)]), aes(x = agenum, y = mean_daz))+
        geom_errorbar(data = subset(sem_daz_a,cohort == coh_ls[which(coh_ls == input$cohort_list)]),aes(x = agenum, ymin = low_daz, ymax = high_daz))+
        xlab("age(month)")+ ylab("daz(sem)")+
        ylim(c(-2,2))+xlim(c(0,48))+
        ggtitle("All data points")

    )})

)


fluidRow(
  column(1, offset = 1, actionButton("prev1", "Prev")),
  column(1, offset = 5, actionButton("run1", "Next"))
)




#ggplot(data = sem_daz_a )+
#  geom_point(aes(x=agenum, y=daz)) +
#  geom_errorbar(aes(x = agenum, ymin=low_daz, ymax=high_daz), width=.1)+
#  xlab("age(months)")+
#  ylim(c(-2,2))

#ggplot(data = sem_daz_a )+
#  geom_point(aes(x=agenum, y=mean_daz)) +
#  geom_errorbar(aes(x = agenum, ymin=low_daz, ymax=high_daz), width=.1)+
#  xlab("age(months)")+ ylab("daz")
#  ylim(c(-2,2))

#ggplot(data = sem_daz_a )+
#  geom_point(aes(x=agenum, y=mean_d)) +
#  geom_errorbar(aes(x = agenum, ymin=low_d, ymax=high_d), width=.1)+
#  xlab("age(months)") + ylab("D-score")

