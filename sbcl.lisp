(in-package :trivial-sockets)

(defun resolve-hostname (name)
  (cond
   ((eql name :any) #(0 0 0 0))
   ((typep name '(vector number 4)) name)
   (t (car (sb-bsd-sockets:host-ent-addresses
	    (sb-bsd-sockets:get-host-by-name name))))))

(defun open-stream (peer-host peer-port 
			      &key (local-host :any) (local-port 0)
			      (external-format :default)
			      (element-type 'character)
			      (protocol :tcp))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (handler-bind ((sb-bsd-sockets:socket-error 
		  (lambda (c) (error 'socket-error :nested-error c)))
		 (sb-bsd-sockets:name-service-error 
		  (lambda (c) (error 'socket-error :nested-error c))))
    (let ((s (make-instance 'sb-bsd-sockets:inet-socket
			    :type :stream
			    :protocol protocol))
	  (me (resolve-hostname local-host)))
      (unless (and (equal me #(0 0 0 0)) (eql local-port 0))
	(sb-bsd-sockets:socket-bind s me local-port))
      (sb-bsd-sockets:socket-connect
       s (resolve-hostname peer-host) peer-port)
      (sb-bsd-sockets:socket-make-stream  s :input t :output t
					  :element-type element-type
					  :buffering :full))))

(defun open-server (&key (host :any) (port 0)
			 (reuse-address t)
			 (backlog 1)
			 (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket 
			     :type :stream
			     :protocol protocol)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address sock) t))
    (sb-bsd-sockets:socket-bind  sock (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen sock backlog)
    (multiple-value-bind (h p) (sb-bsd-sockets:socket-name sock)
      (declare (ignore h))
      (values sock p))))

(defun close-server (server)
  (sb-bsd-sockets:socket-close server))

(defun accept-connection (socket
			  &key
			  (external-format :default)
			  (element-type 'character))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (let ((s (sb-bsd-sockets:socket-accept socket)))
    (sb-bsd-sockets:socket-make-stream s
				       :input t :output t
				       :element-type element-type
				       :buffering :full)))

