# $Id$
#

# Attempt to create the .applTop link

APPLIC_TOP:=.applTop$(shell \
                      if [ ! -r .applTop ]; then			\
			if [ -r ../.applTop ]; then ln -s ../.applTop;  \
			elif [ -r config/CONFIG.Defs ]; then ln -s . .applTop; \
		        fi ; \
		      fi )

include $(APPLIC_TOP)/config/CONFIG

APPLIC_DIR_TYPE = sys

include $(APPLIC_TOP)/config/RULES.Dirs

