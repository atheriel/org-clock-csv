test: org-clock-csv-tests.el
	emacs -batch -f package-initialize -l ert -L . -l $< -f ert-run-tests-batch-and-exit

.PHONY: test
