;;; private/writer/+news.el -*- lexical-binding: t; -*-

(provide '+news)

(defun crawl-eluniversal-goscript ()
  "Crawl the news"
  (interactive)
  (shell-command "cd ~/go/src/github.com/gocolly/colly/_examples && go run eluniversal/eluniversal.go >> ~/results.json")
  (find-file "~/results.json")
  )

(defun scrapy-venezuela-news ()
  "Crawl the news about Venezuela from some popular newspapers"
  (interactive)
  (shell-command "cd ~/Dropbox/shared/InformationCenter && scrapy crawl eluniversal")
  (shell-command "cd ~/Dropbox/shared/InformationCenter && scrapy crawl elnacional")
  (shell-command "cd ~/Dropbox/shared/InformationCenter && scrapy crawl correodelorinoco")
  (shell-command "cd ~/Dropbox/shared/InformationCenter && scrapy crawl ultimanoticias")
  )

(defun insert-venezuela-news ()
  "Crawl the news about Venezuela from some popular newspapers"
  (interactive)
  (insert-file "~/Dropbox/shared/InformationCenter/eluniversal_results.txt")
  (insert-file "~/Dropbox/shared/InformationCenter/elnacional_results.txt")
  (insert-file "~/Dropbox/shared/InformationCenter/correodelorinoco_results.txt")
  (insert-file "~/Dropbox/shared/InformationCenter/ultimanoticias_results.txt")
  (mark-whole-buffer)
  (fill-paragraph)
  )

(defun puppeteer-universal-news ()
  "Crawl the news about Venezuela from some popular newspapers with puppeteer"
  (interactive)
  (shell-command "cd /Users/linuxing3/workspace/cp-work-puppeteer && node src/universal/universal.js")
  )

(defun insert-universal-news ()
  "Crawl the news about Venezuela from some popular newspapers with puppeteer"
  (interactive)
  (insert-file "/Users/linuxing3/workspace/cp-work-puppeteer/static/eluniversal.org")
  )
