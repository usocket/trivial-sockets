;;;; This is a port to MCL 5.0 using the opentransport library.
   
;;; Unfortunately opentransport has a different way of dealing with
;;; passive (server) side. The semantics of open-server and
;;; accept-connection is therefor not quite right. It won't actually start
;;; listening until the first accept-connection.

(in-package :trivial-sockets)

(require "OPENTRANSPORT")


(defclass MCL-LISTENER-SOCKET ()
  ((port :initarg :port :accessor mcl-listener-port)
   (stream :initform nil :accessor listener-stream)
   (reuse-address :initarg :reuse-address :accessor reuse-address)))


(defun open-stream (peer-host peer-port 
                                &key (local-host :any) (local-port 0)
			        (external-format :default)
			        (element-type 'base-character)
			        (protocol :tcp))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (unless (eql local-host :any)
    (error 'unsupported :feature :local-host))
  (unless (eql local-port 0)
    (error 'unsupported :feature :local-port))
  (handler-bind ((error
                  (lambda (c) (error 'socket-error :nested-error c))))
    (ccl::open-tcp-stream peer-host peer-port :element-type element-type
                          :connect-timeout 60)))


(defun open-server (&key (host :any) (port 0)
			 (reuse-address t)
			 (backlog 1)
			 (protocol :tcp))
  "Returns a SERVER object"
  (declare (ignore backlog))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (eql host :any)
    (error 'unsupported :feature :host))
  (when (eql port 0)
    (error 'unsupported :feature `(:port 0)))
  (let ((listener (make-instance 'mcl-listener-socket :port port :reuse-address reuse-address)))
    ;;(accept-connection listener)
    listener))


(defun close-server (server)
  (when (listener-stream server)
    (close (listener-stream server))))


(defun accept-connection (server
                             &key
			     (external-format :default)
			     (element-type 'base-character))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (do ((s (listener-stream server))
       (new nil))
      (new new)
    (when s
      (flet ((ready-p (s)
               (not (eql (ccl::opentransport-stream-connection-state s) :unbnd))))
        (unless (ready-p s)
          (ccl:process-wait "waiting" #'ready-p s))))
    
    (let ((state (and s (ccl::opentransport-stream-connection-state s))))
      (when (member state '(:incon :dataxfer))
        (setq new s
              state nil))
      (when (member state '(nil :uninit :closed))
        (or (mcl-listener-port server)
            (error "MCL OpenTransport needs explicit port number for listener stream"))
        (setq s (setf (listener-stream server)
                      (ccl::open-tcp-stream nil (mcl-listener-port server) 
                                            :element-type element-type
                                            :reuse-local-port-p (reuse-address server))))))))
