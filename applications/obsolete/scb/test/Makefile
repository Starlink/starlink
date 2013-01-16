#  Name:
#     Makefile
#
#  Purpose:
#     Test file for SCB indexing.
#
#  Type:
#     Description file for the make utility.
#
#  Description:
#     This makefile will make the test programs which form the package
#     used to test the SCB indexing.
#
#  Notes:
#     Note that it is never necessary to build this test package; the
#     presence of the source code as files which are syntactically correct 
#     and (in some sense) representative of files found elsewhere in the
#     Starlink source code collection is all that is required.

#  Default target.

bins: scb_c_test scb_f_test

test: scb_c_test scb_f_test
	@echo " "
	./scb_c_test
	@echo " "
	./scb_f_test

#  Tidy

clean:
	rm -f scb_c_test.o scb_f_test.o scb_locase.o \
              scb_c_test   scb_f_test


#  Rules for building test programs.

scb_c_test: scb_c_test.o
	cc -o $@ scb_c_test.o

scb_f_test: scb_f_test.o scb_locase.o
	f77 -o $@ scb_f_test.o scb_locase.o


#  Suffix rules.

.c.o:
	cc -g -c -I/star/include $<

.f.o:
	f77 -g -c -fno-second-underscore $<

