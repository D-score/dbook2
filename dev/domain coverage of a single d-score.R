
<!--
  ### Domain coverage of a single D-score

  The domain information for an individual D-score can also be calculated. To do
that we need the following information:

  * what items were administered to measure the D-score
* to which domains are the items linked
* what part of the information provides each item to the calculated D-score.


In Table \@ref(tab:example_part1) the item scores for a 1,5 year old child from the China cohort is displayed. This child has a D-score of 57.12. For each item, experts have indicated to what domain(s) the item is linked. Items can link to multiple domains and based on expert voting, we have a proportional loading for each item to a domain.


```{r example_part1, echo = FALSE}
library(ddomain)
library(dplyr)
library(tidyr)
library(dscore)
library(ggplot2)
library(kableExtra)
#select a person from the data
#subject <- unique(dmetric::gcdg_lean$itm$subjid) [sample(25410, 1)]
subject <- 4100485

itembank <- builtin_itembank[builtin_itembank$key == "gsed",]

ex1_resp <- gcdg_lean$itm[gcdg_lean$itm$subjid == subject, c("item", "value")] %>% left_join(itembank, by = "item") %>% select(item, label, value, tau) %>% drop_na(tau) %>%   left_join(ddomain::get_domaintable("gcdg"), by = "item") %>% select(item, label, value, Fine.Motor, Gross.Motor, Expressive, Receptive, Cognitive, Adaptive)

ex1_resp[,c("Fine.Motor", "Gross.Motor", "Expressive", "Receptive", "Cognitive", "Adaptive")] <- round(ex1_resp[,c("Fine.Motor", "Gross.Motor", "Expressive", "Receptive", "Cognitive", "Adaptive")],2)

options(knitr.table.format = "html")
kable(ex1_resp,
      caption = "Responses for example child",
      row.names = FALSE,
      col.names = c("Item", "Label", "Score", "Fine Motor", "Gross Motor", "Expressive", "Receptive", "Cognitive", "Adaptive")) %>%
  kableExtra::kable_styling() %>%
  kableExtra::scroll_box(width = "100%", height = "300px")
```
&nbsp;

For each item we can calculate the relative information that it contributes to the D-score of 57.12. In the paragraph [Item information in dbook I](https://stefvanbuuren.name/dbook1/sec-iteminformation.html) we described
how to calculate the item information for a D-score We can combine the
proportional domain loading to the relative information that the item provides
to the D-score of the child. In Figure \@ref(fig:example_part2) this is
displayed. For example the item *Attends to story* contributes to 13% of the
D-score and the item is rates as partly Receptive, partly Cognitive and partly
Adaptive.


```{r example_part2, results = 'hide', fig.keep = 'all', warning = FALSE, fig.cap = '(ref:example_part2)', message = FALSE, echo = FALSE}

example1 <-
  gcdg_lean$itm[gcdg_lean$itm$subjid %in% subject,] %>%
  mutate(age = agedays/365.25) %>%
  pivot_wider(id_cols= c("subjid", "agedays", "age"),
              names_from = "item",
              values_from = "value")

itembank <- dscore::builtin_itembank[dscore::builtin_itembank$key == "gsed", c("key", "item","label", "tau")]

domain_colors <- dmodel::get_color_domain("gcdg")

#items + scores >> dscore
dscore_ex1 <-
  dscore::dscore(
    data = example1,
    items = colnames(example1)[-c(1:3)],
    itembank = itembank
  )
dscore_data <- data.frame(subjid = example1$subjid, dscore_ex1)

#using votes to weight domain contribution per item
item_information <- example1 %>%
  select(-"agedays",-"age")%>%
  pivot_longer(cols = -c("subjid"),
               names_to = "item",
               values_to = "value",
               values_drop_na = TRUE)%>%
  left_join(itembank, by = "item") %>%
  #mutate(tau = dscore::get_tau(items = .data$item, key = "gsed", itembank = itembank))%>%
  drop_na(.data$tau)%>%
  left_join(dscore_data, by = "subjid")%>%
  mutate(info = dinstrument::info(beta = .data$d, delta = .data$tau),
         rel_info = info / sum(info)) %>%
  left_join(ddomain::get_domaintable("gcdg"), by = "item") %>%
  select(item, label, rel_info, Fine.Motor, Gross.Motor, Expressive, Receptive, Cognitive, Adaptive) %>%
  mutate("Fine Motor" = rel_info * Fine.Motor,
         "Gross Motor" = rel_info * Gross.Motor,
         "Expressive" = rel_info * Expressive,
         "Receptive" = rel_info * Receptive,
         "Cognitive" = rel_info * Cognitive,
         "Adaptive" = rel_info * Adaptive) %>%
  select("item", "label", "rel_info", "Fine Motor", "Gross Motor", "Expressive", "Receptive", "Cognitive", "Adaptive")


plot_data <- item_information %>%
  pivot_longer(cols = c("Fine Motor", "Gross Motor", "Expressive", "Receptive", "Cognitive", "Adaptive"), names_to = "domain", values_to = "contribution") %>%
  arrange(rel_info)


plot_data$label <- factor(plot_data$label, levels = unique(plot_data$label[order(plot_data$rel_info)]))

ggplot(data = plot_data, aes(x = label, y = contribution, color = domain, fill = domain)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip()

```
(ref:example_part2) Relative domain contribution per item for a D-score.


The relative information that each item holds per domain can be summed. That way
we get what domain information is composed in the D-score for this child.
Figure \@ref(fig:example_part3) displays the domain information for the D-score
of the example. About 50% of the information was Cognitive, about 40% was Fine
motor and additionally partly adaptive and partly receptive information was
used.

```{r example_part3, results = 'hide', fig.keep = 'all', warning = FALSE, fig.cap = '(ref:example_part3)', message = FALSE, echo = FALSE}
##total domain contribution for Dscore
d_domain_contribution <- item_information %>%
  pivot_longer(cols = c("Fine Motor", "Gross Motor", "Expressive", "Receptive", "Cognitive", "Adaptive"), names_to = "domain", values_to = "contribution") %>%
  group_by(domain) %>% summarize(proportion = sum(contribution, na.rm = TRUE))


# Compute the cumulative percentages (top of each rectangle)
d_domain_contribution$ymax = cumsum(d_domain_contribution$proportion)
# Compute the bottom of each rectangle
d_domain_contribution$ymin = c(0, head(d_domain_contribution$ymax, n=-1))
# Make the plot
ggplot(d_domain_contribution, aes(fill=domain, ymin = ymin, ymax = ymax, xmin = 3, xmax=4)) +
  geom_rect()+
  scale_colour_continuous( domain_colors)+
  coord_polar(theta = "y")+
  theme_void()+
  xlim(c(2, 4))+
  geom_text(x=2,aes(y = 0, label = paste("D =", dscore_data$d[1])))



```
(ref:example_part3) Relative domain contribution for a D-score.







```{r eval=FALSE, include=FALSE, echo = FALSE}
subject <- 4100485
example1 <-
  gcdg_lean$itm[gcdg_lean$itm$subjid %in% subject,] %>%
  mutate(age = agedays/365.25) %>%
  pivot_wider(id_cols= c("subjid", "agedays", "age"),
              names_from = "item",
              values_from = "value")

itembank <- dscore::builtin_itembank[dscore::builtin_itembank$key == "gsed", c("key", "item","label", "tau")]


domain_contr <- function(data,
                         itembank = dscore::builtin_itembank[dscore::builtin_itembank$key == "gsed", c("key", "item","label", "tau")],
                         domaintable = ddomain::get_domaintable("gcdg"),
                         vote_weight = TRUE){

  #items + scores >> dscore
  dscore_ex1 <-
    dscore::dscore(
      data = example1,
      items = colnames(example1)[-c(1:3)],
      itembank = itembank
    )
  dscore_data <- data.frame(subjid = example1$subjid, dscore_ex1)

  #using votes to weight domain contribution per item
  item_information <- example1 %>%
    select(-"agedays",-"age")%>%
    pivot_longer(cols = -c("subjid"),
                 names_to = "item",
                 values_to = "value",
                 values_drop_na = TRUE)%>%
    left_join(itembank, by = "item") %>%
    drop_na(.data$tau)%>%
    left_join(dscore_data, by = "subjid")%>%
    mutate(info = dinstrument::info(beta = .data$d, delta = .data$tau)) %>%
    left_join(ddomain::get_domaintable("gcdg"), by = "item")



  if(vote_weight){
    domain_contr <-   item_information %>%
      select(subjid, a, d, info, Fine.Motor, Gross.Motor, Expressive, Receptive, Cognitive, Adaptive) %>%
      mutate("Fine Motor" = info * Fine.Motor,
             "Gross Motor" = info * Gross.Motor,
             "Expressive" = info * Expressive,
             "Receptive" = info * Receptive,
             "Cognitive" = info * Cognitive,
             "Adaptive" = info * Adaptive) %>%
      pivot_longer(cols = c("Fine Motor", "Gross Motor", "Expressive", "Receptive", "Cognitive", "Adaptive"), names_to = "domain", values_to = "contribution") %>%
      group_by(subjid, a, d, domain) %>% summarize(proportion = sum(contribution, na.rm = TRUE) / sum(info, na.rm = TRUE))
  }

  if(!vote_weight){
    #using votes to select most voted domain per item (one domain per item)
    info_sum <- item_information %>% group_by(subjid) %>% summarize(info_sum = sum(info, na.rm=TRUE))
    domain_contr <- item_information %>%
      left_join(info_sum, by = "subjid") %>%
      select(subjid, a, d, info, voted_domain, info_sum) %>%
      #mutate(voted_domain = recode(voted_domain, "Cognitive" = "COG",
      #                                              "Expressive" = "EXP",
      #                                              "Fine Motor" = "FM",
      #                                              "Gross Motor" = "GM",
      #                                              "Receptive" = "REC",
      #                                              "Adaptive" = "ADP"))%>%
      group_by(subjid, a,d,voted_domain) %>% summarize(proportion = sum(info) / info_sum[1])
  }

  domain_contr

}

domain_contribution <- domain_contr(data = example1)

domain_colors <- dmodel::get_color_domain("gcdg")
#names(domain_colors)[1:5] <- c("COG", "EXP", "FM", "GM", "REC")

ggplot(domain_contribution, aes(fill=domain, y=(d*proportion), x=(a*12))) +
  geom_bar(position="stack", stat="identity")+
  xlab("Age")+ylab("D-score")+
  scale_colour_manual(values = domain_colors)

domain_contr_i <- domain_contribution #%>% filter(subjid == 4702253)

# Compute the cumulative percentages (top of each rectangle)
domain_contr_i$ymax = cumsum(domain_contr_i$proportion)
# Compute the bottom of each rectangle
domain_contr_i$ymin = c(0, head(domain_contr_i$ymax, n=-1))
# Make the plot
ggplot(domain_contr_i, aes(fill=domain, ymin = ymin, ymax = ymax, xmin = 3, xmax=4)) +
  geom_rect()+
  scale_colour_continuous( domain_colors)+
  coord_polar(theta = "y")+
  theme_void()+
  xlim(c(2, 4))+
  geom_text(x=2,aes(y = 0, label = domain_contr_i$d[1]))



```

--->

  <!--
  Reconsider if we want to add this here. The residual plots do not give us a lot of information and I think they are strongly related to the number of items that are asked per domain. So more related to the number of items in de domain-score vs d-score than the score accuracy itself....
--->
