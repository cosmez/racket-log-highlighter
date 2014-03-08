#lang rackjure
;;; Simple IRC Log Highlighter
;;; output example: https://dl.dropboxusercontent.com/u/18071353/racket-logs/racket-logs.html
(require xml net/url)
(define *logs-url* "http://racket-lang.org/irc-logs/racket/")

;;; convert every log.txt line into an xexpr
(define (highlight line) 
  (define (format-time message-time)
    `(span ((class "time") (id ,message-time)) (a ((href ,(str "#" message-time))) ,message-time)))
  (define (format-message message)
    message)
  (match line
    [(regexp #px"(\\d{2}:\\d{2}) (\\w+):(.*)" (list _ msg-time nick msg-text)) 
     `(span ((class "message"))  ,(format-time msg-time) " " (span ((class "nick")) ,nick) ":" ,(format-message msg-text ))] ;normal msg
    [(regexp #px"(\\d{2}:\\d{2}) (\\w+) (.*)" (list _ msg-time nick msg-text)) 
     `(span ((class "me")) ,(format-time msg-time) " " (span ((class "nick")) ,nick) ,(format-message msg-text ))] ;me
    [(regexp #px"(\\d{2}:\\d{2}) \\((\\w+)\\)(.*)" (list _ msg-time status msg-text)) 
     `(span ((class "notice")) ,(format-time msg-time) " " (span ((class "status")) ,status) " " ,(format-message msg-text )) ] ;notices
    [else line]))

;;; converts every txt to html
(define (highlighter url-string)
  (define (format-urls content)
    (regexp-replace* #rx"http://[^[ |^<]+" content (λ (url) (str "<a href=\"" url "\">" url "</a>"))))
  (define url (string->url url-string))
  (define filename (~> url url-path third path/param-path (string-replace ".txt" ""))) ;get the filename from the url-path
  (define html-filename (str filename ".html"))
  (define html-contents
    (xexpr->string 
     `(html (head (title "Log For ",filename)
                  (link ((rel "stylesheet") (type "text/css") (href "style.css"))))
            (body 
             ,@(map 
                (compose highlight)
                (~> url get-pure-port port->lines))))))
  (with-output-to-file html-filename  #:exists 'replace
    #λ(write-string (format-urls html-contents)))
  html-filename)

;;;downloads an url as a string
(define (download-url url)
  (~> url string->url get-pure-port port->string))

;;;Links every log html file in the main page
(define (hyperlink html)
  (displayln html)
  `(li (a ((href ,html))  ,html)))

(~> `(html 
      (head (title "#racket freenode logs")
            (link ((rel "stylesheet") (type "text/css") (href "style.css"))))
      (body 
       (ul
        ,@(map
           #λ(~>> %1 (str *logs-url*) highlighter hyperlink)
           (remove-duplicates 
            (regexp-match*
             #px"\\d{8}\\.txt"
             (download-url *logs-url*)))))))
    xexpr->string (display-to-file "racket-logs.html" #:exists 'replace))
