#!/bin/awk -f
#
# This file is part of XEmacs.
#
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
# Written by Jerry James
# December 18, 2002
#
# Produce a list of macros and the packages/files in which they are defined.
# Usage: find $package_root -type f -name \*.el | \
#        xargs awk -f gen-macro-list.awk > macro.list
#
# Find all defmacro and defsubst instances (including defmacro*, etc.)
/\(def(macro|subst)/ {
  for (i = 1; i <= NF; i++)
    if ($i ~ "\\(def(macro|subst)") {
      mac = i + 1
      # If the macro name contains a comma, we don't know how to deal with it
      if ($mac !~ ",")
	# This is not correct for pathnames with multiple xemacs-packages
	# elements, but fixing it is too painful
	printf("%s\t%s\n", $mac,
	       substr(FILENAME, match(FILENAME, "(xemacs|mule)-packages")))
      break
    }
}
