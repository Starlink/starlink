FC    = f77
FFLAGS = -O4 -C
CFLAGS = 

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= gethlp.f kpg1_agfnd.f  kpg1_asref.f  kpg1_macol.f  kpg1_qidat.f \
	kpg1_agatc.f  kpg1_agref.f  kpg1_ivci.f   kpg1_qcol.f \
	kpg1_hmsg.f kpg1_palci.f kpg1_scrsz.f gthlpi.f pthlpo.f \
	kpg1_nmcol.f sread.f ctm_xcols.f

CSOURCES= kpg1_trmsz.c
CFLAGS = -I/star/include

OBJS = ${FSOURCES:.f=.o}
COBJ = ${CSOURCES:.c=.o}

.f.o:
	${FC} ${FFLAGS} -c $<

lib:	${OBJS} ${CSOURCES:.c=.o}
	ar r libsurf_kap.a $?

clean:	
	/bin/rm -f libsurf_kap.a *.o

install: lib
	${INSTALL} ${INSTOPTS} libsculib.a ${INSTDIR}

