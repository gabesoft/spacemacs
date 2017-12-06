ELC=$(shell find -path './elpa/*/org*' -type f -name '*.elc')

# Clear the org compiled files to avoid version mismatch errors in Spacemacs
# due to conflicts between org-mode shipped with Emacs and version 9.0
clear-org-files:
	$(RM) $(ELC)

.PHONY: clear-org-files