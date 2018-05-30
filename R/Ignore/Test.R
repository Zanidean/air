library(olapR)
library(tidyverse)

setwd("P:/Charts")

inst <- c("MH")

offered <- applications("Unique Applicant Static",
                        c("Provider", "Offered", "Gender"),
               inst) %>% filter(Offered == "Offered") %>%
  spread(Offered, `Unique Applicant Static`)

qualified <- applications("Unique Applicant Static",
                          c("Provider", "Qualified", "Gender"),
                        inst) %>%
  filter(Qualified == "Qualified") %>%
  spread(Qualified, `Unique Applicant Static`)

df <- full_join(offered,
                qualified,
                by = c("Academic Year", "Provider", "Gender")) %>%
  group_by(`Academic Year`, Provider, Gender) %>%
  mutate(`Acceptance Rate` = Offered/Qualified) %>%
  filter(Gender != "Unspecified")


ggplot(df, aes(gsub("-20", "-", `Academic Year`), `Acceptance Rate`,
           colour = `Gender`,
           group = `Gender`)) +
  geom_line(inherit.aes = T) +
  geom_point(size = 1) +
  xlab("Academic Year") +
  scale_color_brewer(palette = "Set1") +
  facet_grid(`Provider`~., scales = "free") +
  theme(axis.text.x = element_text(angle = 90))

ggsave("Plot.pdf", plot = last_plot(), height = 10, width = 10)
