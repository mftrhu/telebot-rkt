#lang racket ; -*- mode: scheme; -*-
(require net/http-client json data/heap)

(define *api-domain* "api.telegram.org")

(struct tg-bot (token [admin #:mutable] [offset #:mutable] [queue #:mutable]))

;; mk-tg-bot -- token: string, admin: list --> tg-bot
;; Creates and returns a `tg-bot' initialized with `token' and `admin'.
(define (mk-tg-bot token admin )
  (tg-bot token admin 0
          ;; queue should be a min-heap of (scheduled_time . thunk)
          (make-heap (lambda (a b) (<= (car a) (car b))))))

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

;; api-call -- bot: tg-bot, endpoint: string, payload: optional jsexpr --> jsexpr
;; Makes an API call to the Telegram servers with the given `payload'.
(define (-api-call bot endpoint (payload '()))
  (define-values (status headers in)
    (http-sendrecv "api.telegram.org"
                   (string-append "/bot" (tg-bot-token bot) "/" endpoint)
                   #:ssl? #t #:version "1.1" #:method "POST"
                   #:headers '("Content-Type: application/json")
                   #:data (jsexpr->string payload)
                 ))
  (let [(response (read-json in))]
    (close-input-port in)
    response))

(define (--api-call bot endpoint (payload '()))
  (define cust (make-custodian))
    ;; Watcher thread:
  (thread (lambda ()
            (sleep 20)
            (displayln "Request timed out")
            (custodian-shutdown-all cust)))
  (parameterize ([current-custodian cust])
    (touch (future (lambda ()
              (define-values (status headers in)
                (http-sendrecv "api.telegram.org"
                               (string-append "/bot" (tg-bot-token bot) "/" endpoint)
                               #:ssl? #t #:version "1.1" #:method "POST"
                               #:headers '("Content-Type: application/json")
                               #:data (jsexpr->string payload)
                               ))
              (let [(response (read-json in))]
                (close-input-port in)
                response)))))
  )

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

;; get-updates -- bot: tg-bot --> jsexpr
;; Returns an hasheq containing up to 100 of the last updates from Telegram.
;; N.B.: in #hasheq(), '("a" "b") or (list "a" "b") != jsexpr, just use ("a" "b")
(define (get-updates bot)
  ;;(displayln "Polling for updates...")
  (api-call bot "getUpdates"
            (hasheq 'offset (tg-bot-offset bot)
                    'limit 100
                    'timeout 10
                    'allowed_updates '("message"))))

;; clean-updates -- bot: tg-bot
;; "Flushes" out the update queue for `bot' - avoids things like the bot
;; getting in a shutdown loop.
(define (clean-updates bot)
  (api-call bot "getUpdates"
            (hasheq 'offset (tg-bot-offset bot)
                    'limit 1
                    'timeout 0
                    'allowed_updates '("message"))))

;; make-message -- payload: text or hasheq
(define (make-message payload)
  (if (string? payload)
      (hasheq 'text payload)
      payload))

;; send-message -- bot: tg-bot, who: number, payload: string or hasheq
;; If `payload' is a string, builds an hasheq from it.  It then updates it
;; with `who' (the ID of the chat to send the message to), and does an API
;; call to send the result.
(define (send-message bot who payload)
  (let* ([payload (make-message payload)]
         [payload (hash-set payload 'chat_id who)])
    (api-call bot "sendMessage" payload)))

;; reply-to -- bot: tg-bot, message: hasheq, payload: any
;; Dispatches `send-message' to send `payload' in reply to the user who
;; sent the message `message'.
(define (reply-to bot message payload)
  (let* ([who (hash-ref (hash-ref message 'chat) 'id)]
         [msg_id (hash-ref message 'message_id)]
         [payload (make-message payload)]
         [payload (hash-set payload 'reply_to_message_id msg_id)])
    (send-message bot who payload)))

;; send-to-admin -- bot: tg-bot, payload: string or hasheq
(define (send-to-admin bot payload)
  (send-message bot (first (tg-bot-admin bot)) payload))

;; by-admin? -- bot: tg-bot, message: hasheq
;; Returns #t if `message' has been sent by one of the admins of `bot'.
(define (by-admin? bot message)
  (let ([from (hash-ref (hash-ref message 'chat) 'id)])
    (member from (tg-bot-admin bot))))

(define (is-text? message)
  (hash-has-key? message 'text))

;; handle-message -- bot: tg-bot, loop: label, message: hasheq
;; Does whatever it needs to do to handle the given `message'.
(define (handle-message bot message)
  ;; We'll just silently ignore messages by anyone but the admin(s)
  (when (and (by-admin? bot message) (is-text? message))
    (let ([text (hash-ref message 'text)])
      (cond
        [(regexp-match "^/shutdown$" text)
         (clean-updates bot)
         (reply-to bot message "ACK :: shutting down")
         (raise "Bot shut down by admin.")]
        [(regexp-match "^/info$" text)
         (reply-to bot message (jsexpr->string (get-me bot)))]
        [else
         (reply-to bot message (string-append "Message received: " text))]))))

;; enqueue -- bot: tg-bot, when: integer, thunk: thunk
;; Puts `thunk' in `bot's task `queue', to be executed after `when' (which
;; should be an UNIX timestamp).
(define (enqueue bot when thunk)
  (heap-add! (tg-bot-queue bot) (cons when thunk)))

;; housekeep -- bot: tg-bot
;; Does any "housekeeping" tasks needed once every "tick".
(define (housekeep bot)
  (displayln (format "Housekeeping - last tick ~s" last-tick))
  (let ([now (current-seconds)]
        [next-event (heap-peek (tg-bot-queue bot))])
    (when (and next-event (>= now (car next-event)))
      ;; Remove the event from the heap and execute it
      (heap-remove-min! (tg-bot-queue bot))
      ((cdr next-event)))))

;; Find the directory the bot is running from
;; We need this - for now - because the token and the list of admins are
;; stored in the files `token' and `admins' in that directory.
(define-values (dir name is-dir)
  (split-path (find-system-path 'run-file)))

;; Initialize the bot
(let* ([bot (mk-tg-bot (string-trim (file->string (build-path dir "token")))
                       (file->list (build-path dir "admins")))])
  (enqueue bot 0 (lambda () (send-to-admin bot "NFO :: Bot started")))
  (enqueue bot (+ (current-seconds) 10) (λ () (send-to-admin bot "NFO :: 10+ seconds have passed")))
  ;; Start the loop
  (let loop ()
    ;; Go through the task queue
    (housekeep bot)
    ;; Get the new updates, if any
    (displayln "Getting updates")
    (let* ([updates (get-updates bot)]
           [updates (if (hash? updates)
                        updates
                        (begin
                          (sleep 10)
                          #hasheq((result . ()))))]
           [updates (hash-ref updates 'result)])
      (when (not (empty? updates))
        ;; Updates available - iterate over them and handle them
        (for ([update updates])
             (set-tg-bot-offset! bot (+ (hash-ref update 'update_id) 1))
             (handle-message bot (hash-ref update 'message))))
      ;; Stay in the loop
      (loop))))