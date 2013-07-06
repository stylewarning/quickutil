;;;; clean-up-quickutil.lisp
;;;; Copyright (c) 2013 Robert Smith

;;; After QUICKUTIL-CLIENT is loaded, which depends on
;;; QUICKUTIL-UTILITIES, we don't actually want to keep
;;; QUICKUTIL-UTILITIES around. We simply want to ensure that it can
;;; be loaded and that it exists.
(quickutil-client-management:unload-quickutil-utilities :verbose nil)
