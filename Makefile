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

SUBDIRS = xemacs-packages

XEMACS_PACKAGES_BASE:= $(shell while [ ! -f XEmacs.rules ]; do \
				cd ..;	\
				done;	\
				pwd)

all: autoloads bytecompile

include Local.rules.mk
-include Local.rules

ifeq ($(BUILD_WITHOUT_MULE),)
SUBDIRS += mule-packages
endif

include meta-iterate.rules

