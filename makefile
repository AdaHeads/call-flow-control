###############################################################################
#                                                                             #
#                                  Alice                                      #
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

all:
	gnatmake -P alice

debug:
	BUILDTYPE=Debug gnatmake -P alice

clean:
	gnatclean -P alice
	BUILDTYPE=Debug gnatclean -P alice

tests:
	@for gpr in $$(find src/tests -type f -name \*.gpr); do gnatmake -P $$gpr 1>$$gpr.output 2>$$gpr.errors || echo $$(echo $$gpr | cut -d/ -f3) failed building.; done | uniq
	@for exe in $$(find src/tests -type f -perm +100); do ./$$exe 1>$$exe.output 2>$$exe.errors || echo $$(echo $$exe | cut -d/ -f3) failed executing a test.; done | uniq

cleanup_messy_temp_files:
	rm *~ src/*~

