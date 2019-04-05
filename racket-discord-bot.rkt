#lang racket

(require racket-cord)

;; bot token for the discord bot.
;; here we read from local file "secret.txt"
(define bot-token (read-line (open-input-file "secret.txt")))

;; discord client.
(define discord-client (make-client bot-token #:auto-shard #t))

;; discord mail struct
(struct discord-mail (client content channel))

;; we are posting messages to discord through a single thread.
;; we use the discord-mail struct to package the relevant info into
;; a value for the thread channel.
;; this isn't relevant to an echo bot but will be the approach used
;; to implement text-based games with shared state as this is cleaner than
;; using mutexes everywhere.

;; for example:
;; 1. receive a command from a discord server.
;; 2. using a channel, queue up an event for the event thread.
;; 3. the event thread processes events in the order they arrive, updating
;; the game state as necessary.
;; 4. the event thread sends a message to response thread through a channel (whatever is appropriate).
;; 5. the response thread posts messages to discord.

;; response thread as above
(define message-thread
  (thread
   (thunk
    (let loop ()
      (let ([mail (thread-receive)])
        (http:create-message (discord-mail-client mail)
                             (discord-mail-channel mail)
                             (discord-mail-content mail))
        (loop))))))

;; on message
(on-event
 'message-create discord-client
 (lambda (client message)
   (unless (string=? (user-id (message-author message))
                     (user-id (client-user client)))
     (cond
       [(string-prefix? (message-content message) "!echo ")
        (thread-send message-thread
                     (discord-mail client
                                   (string-trim (message-content message) "!echo ")
                                   (message-channel-id message)))]))))
               
;; debug output thread.
(define dr (make-log-receiver discord-logger 'debug))

(thread
 (thunk
  (let loop ()
    (let ([v (sync dr)])
      (printf "[~a] ~a\n"
              (vector-ref v 0)
              (vector-ref v 1)))
  (loop))))

;; start the bot.
(start-client discord-client)