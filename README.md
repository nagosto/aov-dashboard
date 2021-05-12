Shiny dashboard to explore some anova exercises. Custom analyses can be made by filling empty tables.

**To launch**

Shiny needs to be loaded : 

```{r}
if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
```
Then run:
```
runGitHub("nagosto/aov-dashboard", ref="main")
```

or to clone the repository, run instead:

```{r}
library(usethis)

if (!require("usethis")) {
    install.packages("usethis")
    library(usethis)
}
create_from_github(
  "nagosto/aov-dashboard",
  protocol='https')
runApp()
```

