ifeq (${XEMACS_PACKAGES_BASE},)
___ := $(error XEMACS_PACKAGES_BASE not defined??)
endif

___ := $(shell [ -f ${XEMACS_PACKAGES_BASE}/Local.rules ] && echo exists)

Local.rules : ${XEMACS_PACKAGES_BASE}/Local.rules
.PHONY : Local.rules

${XEMACS_PACKAGES_BASE}/Local.rules : ${XEMACS_PACKAGES_BASE}/Local.rules.template
ifneq (${___},exists)
	command := $(shell cp -p $< $@)
	$(error You must edit "$(notdir $@)" and customize it for this build host.)
else
	$(error "$(notdir $<)" has been updated or is newer than "$(notdir $@)".  Merge the changes into your "$(notdir $@)".)
endif
