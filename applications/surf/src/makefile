# makefile for REDS package
OBJ=	reds.o reds_bessel_rebin.o reds_crosstalk.o reds_extinction.o \
	reds_flatfield.o reds_get_demod.o reds_get_flat.o reds_modify.o \
	reds_photom.o reds_rebin.o reds_reduce_switch.o reds_restore.o \
	reds_write_demod.o reds_write_flat.o reds_write_photom.o
.f.o:	f77 -c $*.f
MAIN:	$(OBJ)
	alink $(OBJ) \
	-L/jcmt_sw/sun4_Solaris/lib -lsculib -lV77 \
	-L$(IRAS90_SOURCE) -lira `ndf_link_adam` \
	`fio_link_adam` `sla_link_adam`
	rm *.o
