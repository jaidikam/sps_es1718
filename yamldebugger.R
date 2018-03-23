library(yamldebugger)


workdir = "C:/Users/B_JD/Desktop/sps_ws1718"

d_init = yaml.debugger.init(workdir, show_keywords = TRUE)

qnames = yaml.debugger.get.qnames(d_init$RootPath)

d_results = yaml.debugger.run(qnames, d_init)

OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")


