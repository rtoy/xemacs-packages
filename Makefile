# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# 
SUBDIRS = xemacs-packages

# Use a Local.rules file to specify what you wish to have installed
XEMACS_PACKAGES_BASE := $(shell pwd)

all:: bytecompile

include Local.rules.mk
-include Local.rules

ifeq ($(BUILD_WITHOUT_MULE),)
SUBDIRS += mule-packages
endif

.PHONY: all all-bytecompile autoloads bytecompile bindist-real bindist clean distclean elcclean html World install just-install

# The toplevel has slightly different rules so we do not use iterate.rules
# directly

ALL_TARGETS= $(SUBDIRS:=/all.target)
AUTOLOADS_TARGETS= $(SUBDIRS:=/autoloads.target)
BYTECOMPILE_TARGETS= $(SUBDIRS:=/bytecompile.target)
BINDIST_TARGETS= $(SUBDIRS:=/bindist.target)
CLEAN_TARGETS= $(SUBDIRS:=/clean.target)
DISTCLEAN_TARGETS= $(SUBDIRS:=/distclean.target)
ELCCLEAN_TARGETS= $(SUBDIRS:=/elcclean.target)
HTML_TARGETS= $(SUBDIRS:=/html.target)
JUST_INSTALL_TARGETS = $(XEMACS_PACKAGES:=/XEMACS.install)
ifeq ($(BUILD_WITHOUT_MULE),)
JUST_INSTALL_TARGETS += $(MULE_PACKAGES:=/MULE.install)
endif

# At some point we might have dependencies here...

%.target:
	[ -d $(*D) ] && $(MAKE) $(MFLAGS) -C $(*D) $(*F)

%.install:
	[ -d $(*D) ] && $(MAKE) $(MFLAGS) -C $(*D) STAGING=$($(*F:=_STAGING)) install

all-bytecompile: autoloads bytecompile html

autoloads: $(AUTOLOADS_TARGETS)

bytecompile: $(BYTECOMPILE_TARGETS)

bindist-real: $(BINDIST_TARGETS)

bindist: bindist-real

clean: $(CLEAN_TARGETS)

distclean: $(DISTCLEAN_TARGETS)

elcclean:
	$(XEMACS) $(VANILLA) -batch -l package-clean.el

html: $(HTML_TARGETS)

World: distclean install

install: all just-install

just-install: $(JUST_INSTALL_TARGETS)


