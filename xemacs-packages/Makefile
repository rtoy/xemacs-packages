# This file is part of XEmacs.

# Copyright (C) 2001, 2002 Steve Youngs
# Copyright (C) 2004 Ben Wing.

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

# So we can get at $NONMULE_PACKAGES
include ../Local.rules.inc

# At one point, we needed the list of packages to be in a very particular
# order because of autoload issues.  This is no longer true -- we build all
# autoloads first, before any compilation, so they're available to all
# packages, and we arrange things so that out-of-date .elc files are
# ignored.  However, it's useful to order the more basic stuff first,
# to speed up compilation.  We use four groups:
#
# (a) The most basic stuff, listed in dependency order.
# (b) Basic libraries used by lots of other code, with few or no dependencies
#     other than in (a); listed alphabetically.
# (c) Some other things with more dependencies but used by lots of packages;
#     listed alphabetically.
# (d) Everything else, listed alphabetically.

ifeq ('$(NONMULE_PACKAGES)','xemacs-packages')
PACKAGES := xemacs-base fsf-compat mail-lib \
	\
	apel cc-mode debug dired ecrypto edit-utils elib eterm ispell \
	os-utils sh-script texinfo text-modes xlib \
	\
	edebug efs net-utils xemacs-devel \
	\
	cedet-common speedbar eieio ede semantic cogre \
	\
	Sun ada auctex bbdb build c-support calc calendar clearcase \
	cookie crisp dictionary docbookide easypg ecb ediff edt emerge \
	erc escreen eshell eudc footnote forms fortran-modes \
	frame-icon games general-docs gnats gnus guided-tour haskell-mode \
	hm--html-menus hyperbole ibuffer idlwave igrep ilisp \
	jde mailcrypt mew mh-e mine misc-games mmm-mode \
	ocaml oo-browser pc pcl-cvs pcomplete perl-modes pgg \
	prog-modes ps-print psgml psgml-dtds python-modes re-builder \
	reftex riece rmail ruby-modes sasl scheme sgml \
	sieve slider sml-mode sounds-au sounds-wav strokes \
	supercite textools time tm tooltalk tpu tramp \
	vc vc-cc vhdl view-process viper vm w3 x-symbol \
	xetla xslide xslt-process xwem zenirc
else
PACKAGES := $(NONMULE_PACKAGES)
endif

compile::

include ../iterate.rules
