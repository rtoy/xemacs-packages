#!/bin/awk -f
#
# This file is part of XEmacs.
#
# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
#
# Written by Jerry James
# December 18, 2002
#
# Updated January 14, 2004 to also report the type of definition miscompiled
# (defmacro, defsubst, etc.).
#
# Find undefined function messages in the smoketest log and try to match them
# against the macro list.  Invoke this script in a directory containing
# macro.list, the output of gen-macro-list.awk.
# Usage: awk -f find-macro-err.awk < $package_build_log
#
# Read macro.list into an array and save RS and FS
BEGIN {
  OrigRS = RS
  OrigFS = FS
  while ((getline < "macro.list") > 0) {
    macro[$1] = $2
    macrotype[$1] = $3
  }
  close("macro.list")
}
# Track the current package/file name from the log
/Compiling .*\.\.\./ {
  # Get rid of the trailing dots
  split($2, path, "[ .]") - 1
  fil = substr(path[1], match(path[1], "(xemacs|mule)-packages")) ".el"
}
# Find single undefined functions
/is not known to be defined/ {
  if ($4 in macro)
    printf("%s (%s)\n  Definition: %s\n  Miscompile: %s\n",
	   $4, macrotype[$4], macro[$4], fil)
}
# Find multiple undefined functions
/are not known to be defined/ {
  RS = "Wrote"
  FS = ",?[ \t\n\f]+"
  getline
  for (i = 1; i <= NF; i++)
    if ($i !~ "^[ \t\n\f]*$" && $i in macro)
      printf("%s (%s)\n  Definition: %s\n  Miscompile: %s\n",
	     $i, macrotype[$i], macro[$i], fil)
  RS = OrigRS
  FS = OrigFS
}
