#code for install R packages.

library(devtools)

use_gpl_license(version = 3, include_future = TRUE)

#add packages.
use_package("tidyverse", type = "depends")

#add data.
load("signatures.rda")

#loading data
use_data(signatures)


use_data_raw("hg38ToHg19.over.chain")


#add vignette
usethis::use_vignette("ggboxplot", title = "ggboxplot - add p-value in the boxplot")


#5) How do I Document My Functions?
devtools::document()

#4) Edit your code and load your code
devtools::load_all()

#install packages.
devtools::install()
devtools::install(build_vignettes = T)



#(1)ggpoints

#data
set.seed(100)
x = rnorm(20)
y = x*1.1 + 1 + rnorm(20, mean = 0.01)

data = data.frame(x = x, y = y)

#plot all data
ggpoints(x = x, y = y)
ggpoints(data = data, x = "x", y = "y")

#plot lines
ggpoints(x = x, y = y, annotations = c("lm", "lm.line"))

#adjust the position of label
ggpoints(x = x, y = y, annotations = c("lm", "lm.line"), label.x.npc = 0.1, label.y.npc = 0.9)


#(2)liftOverData

load("data/STAD_ASCAT_hg38.rda")
data = data[sample(1:nrow(data), 200),  ]
stad_ascat = data
save(stad_ascat, file = "data/ASCAT_hg38_examples.rda")

#Prepare data
data("ASCAT_hg38_examples")
hg38ToHg19.over.chain = system.file(package="Jerry", "extdata", "hg38ToHg19.over.chain")

head(stad_ascat)

stad_ascat_hg19 = liftOverData(data = stad_ascat,
             chainFiles = hg38ToHg19.over.chain,
             chrVersion = "hg19")
head(stad_ascat_hg19)

#plot_survival


library(survival)
library(ggsurvfit)
library(gtsummary)
library(ezcox)

head(lung )

#inst time status age sex ph.ecog ph.karno pat.karno meal.cal wt.loss
#1    3  306      2  74   1       1       90       100     1175      NA
#2    3  455      2  68   1       0       90        90     1225      15
#3    3 1010      1  56   1       0       90        90       NA      15
#4    5  210      2  57   1       1       90        60     1150      11
#5    1  883      2  60   1       0      100        90       NA       0
#6   12 1022      1  74   1       1       50        80      513       0

#get HR values.
zz = lung |>
  mutate(
    status = recode(status, `1` = 0, `2` = 1),
    time = time
  ) |>
  ezcox(
    covariates = c("pat.karno", "meal.cal", "wt.loss"),
    controls = c("age", "sex"),
    time = "time",
    status = "status",
    return_models = TRUE
  )

mds = get_models(zz)

show_models(mds, drop_controls = TRUE,  merge_models = T)

#Table of summary
tbl_regression(mds$`Surv ~ pat.karno + age + sex` , exp = TRUE)


surv.list = plot_survival(
  lung |>
    mutate(
      status = recode(status, `1` = 0, `2` = 1),
      time = time
    ),
  variables = c("pat.karno", "age"),
  time = "time",
  status = "status",
  legend.labs = c("High","Low"),
  cutoff = "best",
  minprop = 0.2,
  return.data = T
)

surv.list$plot.list

#The median survival days.
gtsummary::tbl_survfit(
  surv.list$surv.fit$pat.karno.g,
  probs  = 0.5,
  label_header = "**Median survival (95% CI)**"
)

gtsummary::tbl_survfit(
  surv.list$surv.fit$age.g,
  probs  = 0.5,
  label_header = "**Median survival (95% CI)**"
)



