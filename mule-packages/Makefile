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

# So we can get at $MULE_PACKAGES
include ../Local.rules.inc

ifeq ('$(MULE_PACKAGES)','mule-packages')
PACKAGES := mule-base locale leim lookup edict egg-its skk \
	latin-unity latin-euro-standards mule-ucs
else
PACKAGES := $(MULE_PACKAGES)
endif

ASSUMED_PRESENT = xemacs-base fsf-compat viper apel cookie elib dired

compile::

include ../iterate.rules
