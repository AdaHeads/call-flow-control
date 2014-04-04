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

PROJECT=call_flow_control

GENERATED_EXECUTABLES=exe/$(PROJECT)

GENERATED_SOURCES=black

PREFIX?=/usr/local/call-flow

RELEASE=`git tag | tail -n1`
GIT_REV=`git rev-parse --short HEAD`

ifeq ($(PROCESSORS),)
   PROCESSORS=`(test -f /proc/cpuinfo && grep -c ^processor /proc/cpuinfo) || echo 1`
endif

all: build

build: fix-whitespace $(GENERATED_SOURCES)
	gnatmake -j$(PROCESSORS) -p -P $(PROJECT)

install: test
	@install --directory        $(PREFIX)/bin
	@install --target-directory=$(PREFIX)/bin $(GENERATED_EXECUTABLES)

install-default-config:
	@install --directory $(PREFIX)/conf
	@install --directory $(PREFIX)/session
	@install             exe/configuration/main.conf.dist  $(PREFIX)/conf/main.conf
	@install             exe/configuration/config.ini.dist $(PREFIX)/conf/config.ini
	@install             exe/configuration/mime.types      $(PREFIX)/conf/mime.types
	@echo Default config deployed at $(PREFIX)/conf - go there and finish the config.

git-head: all
	for exe in $(GENERATED_EXECUTABLES); do cp -p $${exe} $${exe}-$(RELEASE)-$(GIT_REV); done
	echo $(PROJECT)-$(RELEASE)-$(GIT_REV) > release.latest

test: build metrics
	@./tests/build
	@./tests/run

clean:
	gnatclean -P $(PROJECT) || true
	find . -type f \( -name "*~" -o -name "*.o" -o -name "*.ali" \) -print0 | xargs -0 -r /bin/rm
	if [ ! -z "$(GENERATED_SOURCES)" ]; then rm -rf $(GENERATED_SOURCES); fi
	rmdir bin || true
	rmdir obj || true

distclean: clean
	rm -f $(GENERATED_EXECUTABLES)
	rm -f obj/*.ad[sb].metrix
	rmdir bin || true
	rmdir obj || true

fix-whitespace:
	@find src tests -name '*.ad?' | xargs --no-run-if-empty egrep -l '	| $$' | grep -v '^b[~]' | xargs --no-run-if-empty perl -i -lpe 's|	|        |g; s| +$$||g'

metrics:
	@gnat metric -P $(PROJECT)

-include Makefile.project_rules

.PHONY: all build test install clean distclean fix-whitespace metrics

