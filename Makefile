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

# These packages must be built first and in this order.
#SPECIAL = libs/xemacs-base libs/fsf-compat mule/mule-base libs/mail-lib \
#	comm/vm libs/dired libs/efs libs/apel prog/cc-mode comm/w3 \
#	comm/net-utils oa/edit-utils oa/speedbar

# The rest require no further special treatment
SUBDIRS = libs comm games prog wp os oa

# Stuff to install, by default nothing.  Please set these variables
# in Local.rules
# XEmacs packages to install
XEMACS_PACKAGES =
# Where to install them
XEMACS_STAGING =
# Mule packages to install
MULE_PACKAGES =
# Where to install them
MULE_STAGING =
# Should we build the mule package?
BUILD_WITHOUT_MULE =

# Use a Local.rules file to specify what you wish to have installed
sinclude Local.rules

ifeq ('$(BUILD_WITHOUT_MULE)','')
SUBDIRS += mule
endif

all:
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} autoloads; \
	done
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} bytecompile; \
	done

.PHONY: all bindist clean distclean install autoloads

autoloads:
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} autoloads; \
	done

World: distclean install

install: all
ifneq ($(MULE_PACKAGES),'')
	for dir in $(MULE_PACKAGES); do \
		$(MAKE) STAGING=$(MULE_STAGING) $(MFLAGS) -C $${dir} install; \
	done
endif
ifneq ($(XEMACS_PACKAGES),'')
	for dir in $(XEMACS_PACKAGES); do \
		$(MAKE) STAGING=$(XEMACS_STAGING) $(MFLAGS) -C $${dir} install; \
	done
endif

bindist:
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} autoloads; \
	done
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} bindist; \
	done

clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} clean; \
	done

distclean:
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} distclean; \
	done
