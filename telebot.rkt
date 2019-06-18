#lang racket ; -*- mode: scheme; -*-
;; telebot.rkt -- a simple Telegram bot written in Racket
;; Copyright (C) 2019  mftrhu
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require net/http-client
         json
         data/heap
         (except-in racket/date date->string)
         (only-in srfi/19 string->date date->string lax-date?)
         racket/struct)

(define *api-domain* "api.telegram.org")

(struct tg-bot (token [admin #:mutable]
                      [offset #:mutable]
                      [queue #:mutable]
                      [commands #:mutable]
                      [state #:mutable]))

;; mk-tg-bot -- token: string, admin: list --> tg-bot
;; Creates and returns a `tg-bot' initialized with `token' and `admin'.
(define (mk-tg-bot token admin )
  (tg-bot token admin 0
          ;; queue should be a min-heap of (scheduled_time . thunk)
          (make-heap (lambda (a b) (<= (car a) (car b))))
          ;; commands should be an hash (**not** hasheq)
          (make-hash)
          ;; state should be another hash - this time for random variables
          (make-hash)))

;; heap-pop-min! -- heap: heap --> heap element
;; Utility function to remove the minimum from the given `heap' **and**
;; return its value.
(define (heap-pop-min! heap)
  (begin0
   (heap-min heap)
   (heap-remove-min! heap)))

;; heap-peek -- heap: heap --> boolean
;; Returns the topmost element of `heap', or #f if `heap' is empty.
(define (heap-peek heap)
  (if (> (heap-count heap) 0)
      (heap-min heap)
      #f))

(define error-value
  (case-lambda
   [(what) `(error ,what)]
   [(what more)
    `(error ,what ,(cond [(list? more) (format "~a" more)]
                         [(exn? more)  (format "(~a)" (exn-message more))]
                         [else (format "(~a)" more)]))]))

;; with-timeout -- timeout: number, thunk: thunk
;; Shamelessly stolen from <https://github.com/racket/racket/blob/master/racket/collects/version/check.rkt#L22>
;; Takes a number of seconds and a `thunk', and executes the `thunk' with
;; the given `timeout', returning the result of its execution.
(define (with-timeout timeout thunk)
  (define result #f)
  (define r
    (sync/timeout
     timeout
     (thread (λ ()
               (set! result
                     (with-handlers
                      ([void (λ (e)
                               (error-value "internal error" e))])
                      (thunk)))))))
  (if r result (error-value "timeout")))

;; first-rest -- string --> string, string
;; Splits a string in first and rest, where first is all the characters up
;; to the first whitespace character.
(define (first-rest string)
  (let ([match (regexp-match #px"(.+?) +(.+)" string)])
    (when match
      (values (second match) (third match)))))

;; dateify -- when: date or lax-date --> date
;; If passed a lax-date, turns it into a date.
(define (dateify when)
  (if (lax-date? when)
      (let* ([now (seconds->date (current-seconds))]
             [unpacked (struct->list when)]
             [hours (fourth unpacked)]
             [minutes (third unpacked)])
        (struct-copy date now
                     [hour hours]
                     [minute minutes]
                     [second 0]))
      when))

;; try-string->timestamp -- string: string, datefmt: string --> integer
(define (try-string->timestamp string datefmt)
  (with-handlers ([exn:fail? (lambda (ex) #f)])
    (date->seconds (dateify (string->date string datefmt)))))

;; try-string->number -- string: any --> integer
;; Tries to parse string as a number, or returns 0.
(define (try-string->number string)
  (with-handlers ([exn:fail? (lambda (ex) 0)])
    (or (string->number string) 0)))

;; timedelta-->timestamp -- string: string --> integer
;; Returns the timestamp from now to the given time offset, specified in
;; the [DDd][HHh][MMm] format.  An empty offset will simply result in now
;; being returned.
(define (timedelta->timestamp string)
  (with-handlers ([exn:fail? (lambda (ex) #f)])
    (let* ([match (regexp-match #px"^(?:(\\d+)d)?(?:(\\d+)h)?(?:(\\d+)m)?$" string)]
           [days (try-string->number (second match))]
           [hours (try-string->number (third match))]
           [minutes (try-string->number (fourth match))])
      (+ (current-seconds)
         (* days 86400)
         (* hours 3600)
         (* minutes 60)))))

;; parse-time-string -- when: string
;; Tries to parse a time string with various strategies
(define (parse-time-string when)
  (cond
    [(try-string->timestamp when "~Y-~m-~d")]
    [(try-string->timestamp when "~H:~M")]
    [(timedelta->timestamp when)]
    [else #f]))

;; api-call -- bot: tg-bot, endpoint: string, payload: optional jsexpr --> jsexpr
;; Makes an API call to the Telegram servers with the given `payload'.
(define (api-call bot endpoint (payload '()))
  (with-timeout 30 (lambda ()
                     (define-values (status headers in)
                       (http-sendrecv "api.telegram.org"
                                      (string-append "/bot" (tg-bot-token bot) "/" endpoint)
                                      #:ssl? #t #:version "1.1" #:method "POST"
                                      #:headers '("Content-Type: application/json")
                                      #:data (jsexpr->string payload)
                                      ))
                     (let [(response (read-json in))]
                       (close-input-port in)
                       response))))

;; get-me -- bot: tg-bot --> jsexpr
(define (get-me bot)
  (api-call bot "getMe"))

;; raw-get-updates -- bot: tg-bot --> jsexpr
;; Returns an hasheq containing up to 100 of the last updates from Telegram.
;; N.B.: in #hasheq(), '("a" "b") or (list "a" "b") != jsexpr, just use ("a" "b")
(define (raw-get-updates bot)
  (api-call bot "getUpdates"
            (hasheq 'offset (tg-bot-offset bot)
                    'limit 100
                    'timeout 10
                    'allowed_updates '("message"))))

;; get-updates -- bot: tg-bot --> list
(define (get-updates bot)
  (let ([updates (raw-get-updates bot)])
    (if (and (hash? updates) (hash-has-key? updates 'result))
        (hash-ref updates 'result)
        (begin
          (sleep 10)
          '()))))

;; clean-updates -- bot: tg-bot --> void (jsexpr)
;; "Flushes" out the update queue for `bot' - avoids things like the bot
;; getting in a shutdown loop.
(define (clean-updates bot)
  (api-call bot "getUpdates"
            (hasheq 'offset (tg-bot-offset bot)
                    'limit 1
                    'timeout 0
                    'allowed_updates '("message"))))

;; make-message -- payload: text or jsexpr --> jsexpr
(define (make-message payload)
  (if (string? payload)
      (hasheq 'text payload)
      payload))

;; send-message -- bot: tg-bot, who: number, payload: string or jsexpr
;; If `payload' is a string, builds an hasheq from it.  It then updates it
;; with `who' (the ID of the chat to send the message to), and does an API
;; call to send the result.
(define (send-message bot who payload)
  (let* ([payload (make-message payload)]
         [payload (hash-set payload 'chat_id who)])
    (api-call bot "sendMessage" payload)))

;; reply-to -- bot: tg-bot, message: jsexpr, payload: any
;; Dispatches `send-message' to send `payload' in reply to the user who
;; sent the message `message'.
(define (reply-to bot message payload)
  (let* ([who (hash-ref (hash-ref message 'chat) 'id)]
         [msg_id (hash-ref message 'message_id)]
         [payload (make-message payload)]
         [payload (hash-set payload 'reply_to_message_id msg_id)])
    (send-message bot who payload)))

;; send-to-admin -- bot: tg-bot, payload: string or jsexpr
(define (send-to-admin bot payload)
  (send-message bot (first (tg-bot-admin bot)) payload))

;; by-admin? -- bot: tg-bot, message: jsexpr
;; Returns #t if `message' has been sent by one of the admins of `bot'.
(define (by-admin? bot message)
  (let ([from (hash-ref (hash-ref message 'chat) 'id)])
    (member from (tg-bot-admin bot))))

;; is-text? -- message: jsexpr --> boolean
(define (is-text? message)
  (hash-has-key? message 'text))

(define (add-command bot command function)
  (hash-set! (tg-bot-commands bot) command function))

(define (default-command bot params message)
  (reply-to bot message "Command not understood."))

;; handle-message -- bot: tg-bot, loop: label, message: jsexpr
;; Does whatever it needs to do to handle the given `message'.
(define (handle-message bot message)
  ;; We'll just silently ignore messages by anyone but the admin(s)
  (when (and (by-admin? bot message) (is-text? message))
    (let* ([text (hash-ref message 'text)]
           [match (regexp-match #px"^/(\\w+)( +(.+))?$" text)])
      (if match
          ;; The message contains a command - pull out the relevant matches
          (let* ([command (second match)]
                 [params (or (fourth match) "")]
                 ;; Look `command' up in the bot `commands' hash, use
                 ;; `default-command' if nothing comes up.
                 [function (hash-ref (tg-bot-commands bot) command
                                     (lambda () default-command))])
            (function bot params message))
          ;; The message is just plain old text
          (reply-to bot message (string-append "Message received: " text))))))

;; enqueue -- bot: tg-bot, when: integer, thunk: thunk
;; Puts `thunk' in `bot's task `queue', to be executed after `when' (which
;; should be an UNIX timestamp).
(define (enqueue bot when thunk)
  (heap-add! (tg-bot-queue bot) (cons when thunk)))

;; housekeep -- bot: tg-bot
;; Does any "housekeeping" tasks needed once every "tick".
(define (housekeep bot)
  (let ([now (current-seconds)]
        [next-event (heap-peek (tg-bot-queue bot))])
    (when (and next-event (>= now (car next-event)))
      ;; Remove the event from the heap and execute it
      (heap-remove-min! (tg-bot-queue bot))
      ;; Execute the thunk by passing the thunk itself as a parameter
      ;; Allows things like a thunk re-enqueueing itself
      ((cdr next-event) (cdr next-event)))))

(define (get-var bot var (default #f))
  (if (hash-has-key? (tg-bot-state bot) var)
      (hash-ref (tg-bot-state bot) var)
      default))

(define (set-var! bot var value)
  (hash-set! (tg-bot-state bot) var value))

;; chime -- bot: tg-bot --> thunk
;; Pings the admin with a message and re-enqueues itself for the next hour
(define (chime bot)
  (lambda (thunk)
    (let ([now (date->string (current-date) "~H:~M")]
          [next-hour (* (+ (quotient (current-seconds) 3600) 1) 3600)])
      (when (get-var bot 'enabled)
        (send-to-admin bot (format "It's ~a." now)))
      (displayln (format "DBG :: enqueueing for ~a" next-hour))
      (enqueue bot next-hour thunk))))

;; Find the directory the bot is running from
;; We need this - for now - because the token and the list of admins are
;; stored in the files `token' and `admins' in that directory.
(define-values (dir name is-dir)
  (split-path (find-system-path 'run-file)))

;; Initialize the bot
(let* ([bot (mk-tg-bot (string-trim (file->string (build-path dir "token")))
                       (file->list (build-path dir "admins")))])
  (set-var! bot 'enabled #t)
  ;; Define the bot commands
  (add-command bot "shutdown"
               (lambda (bot params message)
                 (clean-updates bot)
                 (reply-to bot message "ACK :: shutting down")
                 (raise "Bot shut down by admin.")))
  (add-command bot "info"
               (lambda (bot params message)
                 (cond
                  [(string=? "state" params)
                   (reply-to bot message (jsexpr->string (tg-bot-state bot)))]
                  [else
                   (reply-to bot message (jsexpr->string (get-me bot)))])))
  (add-command bot "hello"
               (lambda (bot params message)
                 (reply-to bot message "ACK :: waking up")
                 (set-var! bot 'enabled #t)))
  (add-command bot "bye"
               (lambda (bot params message)
                 (reply-to bot message "ACK :: going to sleep")
                 (set-var! bot 'enabled #f)))
  (add-command bot "ping"
               (lambda (bot params message)
                 (define-values (time text)
                   (first-rest params))
                 (if (parse-time-string time)
                     (enqueue bot (parse-time-string time)
                              (lambda (thunk)
                                (send-to-admin bot text)))
                     (reply-to bot message (format "Bad time string: ~a" time)))))
  ;; Define the "scheduled" commands
  (let ([chime (chime bot)])
    (chime chime))
  ;; Start the loop
  (let loop ()
    ;; Go through the task queue
    (housekeep bot)
    ;; Get new updates, if any
    (displayln "Getting updates")
    (for ([update (get-updates bot)])
         (set-tg-bot-offset! bot (+ (hash-ref update 'update_id) 1))
         (handle-message bot (hash-ref update 'message)))
    (loop)))
