
library(plumber)
# 'plumber.R' is the location of the file shown above
pr("other/api/plumber.R") %>%
  pr_run(port=80,host = "0.0.0.0")

# ngrok http http://127.0.0.1:8008
