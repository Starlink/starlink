# You will need to change this to the location of perl.h, XSUB.h, EXTERN.h
# which are all part of your perl5 libraries.
PERL5 = /local/lib/perl5/sun4-solaris/5.003/CORE/

lib: arrays.c
	cc -I${PERL5} -c arrays.c; ar r libarrays.a arrays.o

