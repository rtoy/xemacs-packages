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
SUBDIRS = libs comm games prog wp os oa mule

all:
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} autoloads; \
	done
	for dir in $(SUBDIRS); do \
		$(MAKE) $(MFLAGS) -C $${dir} bytecompile; \
	done

.PHONY: all bindist clean distclean

bindist:
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
