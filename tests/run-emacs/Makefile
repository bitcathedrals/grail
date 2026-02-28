test-elisp-notation:
	@echo "testing running elisp with : notation"
	run-emacs -v : '(message "hello world!")'

test-elisp-raw:
	@echo "testing running elisp without notation"
	run-emacs -v -e '(message "hello world, its raw")'

test-launch:
	@echo "testing launch..."
	run-emacs -v

test-emacs-create-frame:
	@echo "testing emacs creating a frame"
	run-emacs -v -c

test-emacs-create-frame-no-wait:
	@echo "testing emacs creating frame no wait"
	run-emacs -v -n -c local.c

test-emacs-diff:
	@echo "testing emacs diff"
	run-emacs -v ^ local.c upstream.c

test-emacs-diff-3way:
	@echo "testing emacs diff with ancestor"
	run-emacs -v ^ local.c upstream.c ancestor.c

elisp-tests: test-elisp-notation test-elisp-raw
	@echo "elisp tests"
