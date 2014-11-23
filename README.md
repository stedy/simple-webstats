simple-webstats
===============

Simple stats for visualizing web traffic to a server. Initially developed for server log following the standard format:
`LogFormat "%v:%p %h %l %u %t \"%r\" %>s %O \"%{Referer}i\" \"%{User-Agent}i\""`

I then run a shell script that uses [littler](http://dirk.eddelbuettel.com/code/littler.html)

`r -e "knitr::knit2html('log_summarize.Rmd')"`
