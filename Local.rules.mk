# This block must be duplicated in XEmacs.rules and the toplevel Makefile.

ifeq (${XEMACS_PACKAGES_BASE},)
___ := $(error XEMACS_PACKAGES_BASE not defined??)
endif

___ := $(shell [ -f ${XEMACS_PACKAGES_BASE}/Local.rules ] && echo exists)

Local.rules : ${XEMACS_PACKAGES_BASE}/Local.rules
.PHONY : Local.rules

${XEMACS_PACKAGES_BASE}/Local.rules : ${XEMACS_PACKAGES_BASE}/Local.rules.template
ifneq (${___},exists)
	cp -p $< $@
	$(error You must edit "$(notdir $@)" and customize it for this build host.)
else
	diff -u $@ $<
	$(error "$(notdir $<)" has been updated or is newer than "$(notdir $@)".  Merge the changes into your "$(notdir $@)".)
endif
