#lang racket/base

(require racket/system)

(define %hostname:null-input-port
  ;; Note: "%hostname:null-input-port" was taken from an example in Racket 5.3
  ;; documentation.
  (make-input-port 'null
                   (lambda (s) eof)
                   (lambda (skip s progress-evt) eof)
                   void
                   (lambda () never-evt)
                   (lambda (k progress-evt done-evt)
                     (error "no successful peeks!"))))

(define (%hostname:system*/string #:error-name    error-name
                                  #:use-exn?      use-exn?
                                  #:trim-newline? trim-newline?
                                  #:command       command
                                  #:args          args)
  (let* ((stdout-os  (open-output-string))
         (stderr-os  (open-output-string))
         (ok?        (parameterize ((current-output-port stdout-os)
                                    (current-error-port  stderr-os)
                                    (current-input-port  %hostname:null-input-port))
                       (apply system* command args)))
         (stdout-str (get-output-string stdout-os))
         (stderr-str (get-output-string stderr-os)))
    (if ok?
        (if (equal? "" stderr-str)
            (if trim-newline?
                (regexp-replace #rx"\r?\n$" stdout-str "")
                stdout-str)
            (if use-exn?
                (error error-name
                       "shell command ~S had stderr ~S and stdout ~S"
                       (cons command args)
                       stderr-str
                       stdout-str)
                #f))
        (if use-exn?
            (error error-name
                   "shell command ~S failed with stderr ~S and stdout ~S"
                   (cons command args)
                   stderr-str
                   stdout-str)
            #f))))

(provide get-full-hostname)
(define (get-full-hostname)
  (or (getenv "HOSTNAME")
      (%hostname:system*/string #:error-name    'get-full-hostname
                                #:use-exn?      #f
                                #:trim-newline? #t
                                #:command       "/bin/hostname"
                                #:args          '("-f"))
      (%hostname:system*/string #:error-name    'get-short-hostname
                                #:use-exn?      #f
                                #:trim-newline? #t
                                #:command       "hostname"
                                #:args          '("-f"))))

(provide get-short-hostname)
(define (get-short-hostname)
  ;; TODO: Possibly use (getenv "HOSTNAME") when available, and check whether
  ;; it has any dots in it.
  (or (%hostname:system*/string #:error-name    'get-short-hostname
                                #:use-exn?      #f
                                #:trim-newline? #t
                                #:command       "/bin/hostname"
                                #:args          '("-s"))
      (%hostname:system*/string #:error-name    'get-short-hostname
                                #:use-exn?      #f
                                #:trim-newline? #t
                                #:command       "hostname"
                                #:args          '())))

(define %hostname:parse-ipv4-addrs-from-ifconfig-rx
  (let* ((octet "[0-9](?:[0-9](?:[0-9])?)?"))
    (regexp (string-append "[ \t]"
                           "inet"
                           "[ \t]+"
                           "(?:"        ; <A?
                           "addr:"
                           "[ \t]*"
                           ")?"         ; >A?
                           "("          ; <1
                           "(?:"        ; <B
                           "(127)"      ; =2
                           "|"          ; |B
                           octet
                           ")"          ; >B
                           "\\."
                           octet
                           "\\."
                           octet
                           "\\."
                           octet
                           ")"          ; >1
                           ))))

(define (%hostname:parse-ipv4-addrs-from-ifconfig
         in
         #:normal?    (normal?    #t)
         #:localhost? (localhost? #f))
  (let loop ((reverse-results '()))
    (cond ((regexp-try-match %hostname:parse-ipv4-addrs-from-ifconfig-rx
                             in)
           => (lambda (m)
                (apply (lambda (whole addr onetwoseven)
                         (if (if onetwoseven
                                 localhost?
                                 normal?)
                             (loop (cons (bytes->string/latin-1 addr) reverse-results))
                             (loop reverse-results)))
                       m)))
          (else (reverse reverse-results)))))

;b===m* structure/get-ipv4-addrs
; NAME
;  get-ipv4-addrs
; DESCRIPTION
;  Geeft het ipv4 adres van een machine.
;
;  Zie http://www.neilvandyke.org/racket-hostname/
;e===
(provide get-ipv4-addrs)
(define (get-ipv4-addrs #:normal?    (normal?    #t)
                        #:localhost? (localhost? #f))
  (let ((command "/sbin/ifconfig"))
    (cond ((with-handlers ((exn:fail?
                            (lambda (e)
                              (log-warning (format "get-ipv4-addrs: command ~S failed: ~S"
                                                   command
                                                   (exn-message e)))
                              #f)))
             (process* command))
           => (lambda (lst)
                (apply (lambda (stdout-in stdin-out pid stderr-in proc)
                         (dynamic-wind
                           void
                           (lambda ()
                             (with-handlers ((exn:fail?
                                              (lambda (e)
                                                (log-warning (format "get-ipv4-addrs: error while parsing output of command ~S: ~S"
                                                                     command
                                                                     (exn-message e)))
                                                '())))
                               (%hostname:parse-ipv4-addrs-from-ifconfig
                                stdout-in
                                #:normal?    normal?
                                #:localhost? localhost?)))
                           (lambda ()
                             (with-handlers ((exn:fail? void))
                               (proc 'kill)))))
                       lst)))
          (else '()))))
