###############################################################################
#                                                                             #
#                            Call-Flow-Control                                #
#                                                                             #
#                                Make File                                    #
#                                                                             #
#                       Copyright (C) 2012-, AdaHeads K/S                     #
#                                                                             #
#  This is free software;  you can redistribute it  and/or modify it          #
#  under terms of the  GNU General Public License as published  by the        #
#  Free Software  Foundation;  either version 3,  or (at your option) any     #
#  later version.  This software is distributed in the hope  that it will     #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty    #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        #
#  General Public License for  more details.                                  #
#  You should have  received  a copy of the GNU General  Public  License      #
#  distributed  with  this  software;   see  file COPYING3.  If not, go       #
#  to http://www.gnu.org/licenses for a complete copy of the license.         #
#                                                                             #
###############################################################################

PREFIX?=/usr/local/call-flow

RELEASE=`git tag | tail -n1`
GIT_REV=`git rev-parse --short HEAD`
BINARY=call_flow_control

ifeq ($(PROCESSORS),)
PROCESSORS=`(test -f /proc/cpuinfo && grep -c ^processor /proc/cpuinfo) || echo 1`
endif

all:
	gnatmake -j${PROCESSORS} -P ${BINARY}

debug:
	BUILDTYPE=Debug gnatmake -j${PROCESSORS} -P ${BINARY}

clean: cleanup_messy_temp_files
	gnatclean -P ${BINARY}
	BUILDTYPE=Debug gnatclean -P ${BINARY}

install: all
	install --directory        ${PREFIX}/bin
	install --target-directory=${PREFIX}/bin exe/${BINARY}

install-default-config:
	@install --directory ${PREFIX}/conf
	@install             exe/configuration/main.conf.dist  ${PREFIX}/conf/main.conf
	@install             exe/configuration/config.ini.dist ${PREFIX}/conf/config.ini
	@install             exe/configuration/mime.types      ${PREFIX}/conf/mime.types
	@echo Default config deployed at ${PREFIX}/conf - go there and finish the config.

git-head: all
	cp exe/${BINARY} exe/${BINARY}-${RELEASE}-${GIT_REV}
	echo ${BINARY}-${RELEASE}-${GIT_REV} > release.latest

tests: all
	@./src/tests/build
	@./src/tests/run

cleanup_messy_temp_files:
	find . -name "*~" -type f -print0 | xargs -0 -r /bin/rm

fix-whitespace:
	@find src -name '*.ad?' | xargs --no-run-if-empty egrep -l '	| $$' | grep -v '^b[~]' | xargs --no-run-if-empty perl -i -lpe 's|	|        |g; s| +$$||g'
