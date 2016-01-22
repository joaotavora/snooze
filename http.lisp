;;; HTTP Verbs

(defpackage #:snooze-verbs
  (:use)
  (:export #:http-verb
           #:get
           #:patch
           #:post
           #:put
           #:delete
           #:content-verb
           #:receiving-verb
           #:sending-verb)
  (:documentation "The taxonomy of HTTP verbs used by snooze. In addition to the verbs themselves it groups verbs in content, sending and receiving verbs. Sending and Receiving are always from the server's perspective. Hence GET is sending to client and POST and PUT are receiving from client."))

(cl:defclass snooze-verbs:http-verb      () ())
(cl:defclass snooze-verbs:delete         (snooze-verbs:http-verb) ())
(cl:defclass snooze-verbs:content-verb   (snooze-verbs:http-verb) ())
(cl:defclass snooze-verbs:receiving-verb (snooze-verbs:content-verb) ()
  (:documentation "Indicates that the request carries information from the client to the server."))
(cl:defclass snooze-verbs:sending-verb   (snooze-verbs:content-verb) ()
  (:documentation "Indicates that the request is asking information from the server."))
(cl:defclass snooze-verbs:patch          (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:post           (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:put            (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:get            (snooze-verbs:sending-verb) ())
