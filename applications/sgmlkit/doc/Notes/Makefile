#SGMLDIR=/home/norman/s/src/sgml/w/sgml
# relies on env var STARLINK_SGML_DIR being set correctly
include $(STARLINK_SGML_DIR)/lib/Makefile

#JADEFLAGS=-V nochunks

#SSN-unassigned.html: Notes.html

Notes.stamp: Notes.sgml $(STARLINK_SGML_DIR)/dsssl/sl/html Notes2.summary

Notes.single.html: Notes.sgml

Notes2.stamp: Notes2.sgml Notes.summary

Notes2.single.html: Notes2.sgml

Notes.tar: SSN-unassigned.html Notes.single.html
	tar cf $@ Notes.single.html SSN-unassigned* Notes.sgml

