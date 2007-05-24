# Common Makefile material for package Makefiles
# Copyright (C) 1997 Free Software Foundation Inc.
# Copyright (C) 2002, 2003 Ben Wing.

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
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# Requires XEmacs 21.0-beta19 or greater and GNU Make 3.78 or greater.

ifeq ($(XEMACS_RULES_INCLUDED),)
XEMACS_RULES_INCLUDED := t

XEMACS_PACKAGES_BASE := $(shell while [ ! -f XEmacs.rules ]; do \
                                    cd ..;	\
                                done;		\
                                pwd)

DEPENDS_FILE_NAME = pdepends.mk

include ${XEMACS_PACKAGES_BASE}/meta-targets.rules

bytecompile:: compile

include ${XEMACS_PACKAGES_BASE}/Local.rules.mk
-include ${XEMACS_PACKAGES_BASE}/Local.rules
endif

# Local Variables:
# mode: makefile
# End:
