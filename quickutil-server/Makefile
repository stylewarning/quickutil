LISP=sbcl
SERVER_PORT=8080
SWANK_PORT=4005
PROJECT_ROOT=$(abspath $(dir $(lastword $(MAKEFILE_LIST))))
ASDF_PATH=$(abspath $(dir $(shell find $(PROJECT_ROOT)/.. -name "*.asd")))

all: start

start:
	$(call $(LISP), \
		(ql:quickload :quickutil-server) (ql:quickload :swank), \
		(quickutil-server:start :debug nil :port $(SERVER_PORT)) \
		(swank:create-server :port $(SWANK_PORT) :style :spawn :dont-close t))

#
# Lisp Implementation

define sbcl
	sbcl --noinform --disable-debugger \
    --eval '(setf asdf:*central-registry* (append (list $(foreach asd,$(ASDF_PATH),#P"$(asd)/")) asdf:*central-registry*))' \
		--eval '(progn $1)' \
		--eval '(progn $2)'
endef

define ccl
	ccl --quiet --batch \
    --eval '(setf asdf:*central-registry* (append (list $(foreach asd,$(ASDF_PATH),#P"$(asd)/")) asdf:*central-registry*))' \
		--eval '(progn $1)' \
		--eval '(progn $2)'
endef
