/************************************************************************
     Together, f77_wrap1.c and f77_wrap2.c contain C wrappers for all
     the CFITSIO routines prototyped in fitsio.h, except for the
     generic datatype routines and features not supported in fortran
     (eg, unsigned integers), a few routines prototyped in fitsio2.h,
     which only a handful of FTOOLS use, plus a few obsolete FITSIO
     routines not present in CFITSIO.  This file allows Fortran code
     to use the CFITSIO library instead of the FITSIO library without
     modification.  It also gives access to new routines not present
     in FITSIO.  Fortran FTOOLS must continue using the old routine
     names from FITSIO (ie, ftxxxx), but most of the C-wrappers simply
     redirect those calls to the corresponding CFITSIO routines (ie,
     ffxxxx), with appropriate parameter massaging where necessary.
     The main exception are read/write routines ending in j (ie, long
     data) which get redirected to C routines ending in k (ie, int
     data). This is more consistent with the default integer type in
     Fortran. f77_wrap1.c primarily holds routines operating on whole
     files and extension headers.  f77_wrap2.c handle routines which
     read and write the data portion, plus miscellaneous extra routines.
     
        File created by Peter Wilson (HSTX), Oct-Dec. 1997
************************************************************************/

#include "fitsio2.h"
#include "f77_wrap.h"

/*------------ read primary array or image elements -------------*/
FCALLSCSUB8(ffgpvb,FTGPVB,ftgpvb,FITSUNIT,LONG,LONG,LONG,BYTE,BYTEV,PLOGICAL,PINT)
FCALLSCSUB8(ffgpvi,FTGPVI,ftgpvi,FITSUNIT,LONG,LONG,LONG,SHORT,SHORTV,PLOGICAL,PINT)
FCALLSCSUB8(ffgpvk,FTGPVJ,ftgpvj,FITSUNIT,LONG,LONG,LONG,INT,INTV,PLOGICAL,PINT)
FCALLSCSUB8(ffgpvk,FTGPVK,ftgpvk,FITSUNIT,LONG,LONG,LONG,INT,INTV,PLOGICAL,PINT)
FCALLSCSUB8(ffgpve,FTGPVE,ftgpve,FITSUNIT,LONG,LONG,LONG,FLOAT,FLOATV,PLOGICAL,PINT)
FCALLSCSUB8(ffgpvd,FTGPVD,ftgpvd,FITSUNIT,LONG,LONG,LONG,DOUBLE,DOUBLEV,PLOGICAL,PINT)


#define ftgpfb_LOGV_A6 A4
FCALLSCSUB8(ffgpfb,FTGPFB,ftgpfb,FITSUNIT,LONG,LONG,LONG,BYTEV,LOGICALV,PLOGICAL,PINT)

#define ftgpfi_LOGV_A6 A4
FCALLSCSUB8(ffgpfi,FTGPFI,ftgpfi,FITSUNIT,LONG,LONG,LONG,SHORTV,LOGICALV,PLOGICAL,PINT)

#define ftgpfj_LOGV_A6 A4
FCALLSCSUB8(ffgpfk,FTGPFJ,ftgpfj,FITSUNIT,LONG,LONG,LONG,INTV,LOGICALV,PLOGICAL,PINT)

#define ftgpfk_LOGV_A6 A4
FCALLSCSUB8(ffgpfk,FTGPFK,ftgpfk,FITSUNIT,LONG,LONG,LONG,INTV,LOGICALV,PLOGICAL,PINT)

#define ftgpfe_LOGV_A6 A4
FCALLSCSUB8(ffgpfe,FTGPFE,ftgpfe,FITSUNIT,LONG,LONG,LONG,FLOATV,LOGICALV,PLOGICAL,PINT)

#define ftgpfd_LOGV_A6 A4
FCALLSCSUB8(ffgpfd,FTGPFD,ftgpfd,FITSUNIT,LONG,LONG,LONG,DOUBLEV,LOGICALV,PLOGICAL,PINT)

FCALLSCSUB9(ffg2db,FTG2DB,ftg2db,FITSUNIT,LONG,BYTE,LONG,LONG,LONG,BYTEV,PLOGICAL,PINT)
FCALLSCSUB9(ffg2di,FTG2DI,ftg2di,FITSUNIT,LONG,SHORT,LONG,LONG,LONG,SHORTV,PLOGICAL,PINT)
FCALLSCSUB9(ffg2dk,FTG2DJ,ftg2dj,FITSUNIT,LONG,INT,LONG,LONG,LONG,INTV,PLOGICAL,PINT)
FCALLSCSUB9(ffg2dk,FTG2DK,ftg2dk,FITSUNIT,LONG,INT,LONG,LONG,LONG,INTV,PLOGICAL,PINT)
FCALLSCSUB9(ffg2de,FTG2DE,ftg2de,FITSUNIT,LONG,FLOAT,LONG,LONG,LONG,FLOATV,PLOGICAL,PINT)
FCALLSCSUB9(ffg2dd,FTG2DD,ftg2dd,FITSUNIT,LONG,DOUBLE,LONG,LONG,LONG,DOUBLEV,PLOGICAL,PINT)

FCALLSCSUB11(ffg3db,FTG3DB,ftg3db,FITSUNIT,LONG,BYTE,LONG,LONG,LONG,LONG,LONG,BYTEV,PLOGICAL,PINT)
FCALLSCSUB11(ffg3di,FTG3DI,ftg3di,FITSUNIT,LONG,SHORT,LONG,LONG,LONG,LONG,LONG,SHORTV,PLOGICAL,PINT)
FCALLSCSUB11(ffg3dk,FTG3DJ,ftg3dj,FITSUNIT,LONG,INT,LONG,LONG,LONG,LONG,LONG,INTV,PLOGICAL,PINT)
FCALLSCSUB11(ffg3dk,FTG3DK,ftg3dk,FITSUNIT,LONG,INT,LONG,LONG,LONG,LONG,LONG,INTV,PLOGICAL,PINT)
FCALLSCSUB11(ffg3de,FTG3DE,ftg3de,FITSUNIT,LONG,FLOAT,LONG,LONG,LONG,LONG,LONG,FLOATV,PLOGICAL,PINT)
FCALLSCSUB11(ffg3dd,FTG3DD,ftg3dd,FITSUNIT,LONG,DOUBLE,LONG,LONG,LONG,LONG,LONG,DOUBLEV,PLOGICAL,PINT)

     /*  The follow LONGV definitions have +1 appended because the   */
     /*  routines use of NAXIS+1 elements of the long vectors.       */

#define ftgsvb_LONGV_A4 A3+1
#define ftgsvb_LONGV_A5 A3+1
#define ftgsvb_LONGV_A6 A3+1
#define ftgsvb_LONGV_A7 A3+1
FCALLSCSUB11(ffgsvb,FTGSVB,ftgsvb,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,BYTE,BYTEV,PLOGICAL,PINT)

#define ftgsvi_LONGV_A4 A3+1
#define ftgsvi_LONGV_A5 A3+1
#define ftgsvi_LONGV_A6 A3+1
#define ftgsvi_LONGV_A7 A3+1
FCALLSCSUB11(ffgsvi,FTGSVI,ftgsvi,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,SHORT,SHORTV,PLOGICAL,PINT)

#define ftgsvj_LONGV_A4 A3+1
#define ftgsvj_LONGV_A5 A3+1
#define ftgsvj_LONGV_A6 A3+1
#define ftgsvj_LONGV_A7 A3+1
FCALLSCSUB11(ffgsvk,FTGSVJ,ftgsvj,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,INT,INTV,PLOGICAL,PINT)

#define ftgsvk_LONGV_A4 A3+1
#define ftgsvk_LONGV_A5 A3+1
#define ftgsvk_LONGV_A6 A3+1
#define ftgsvk_LONGV_A7 A3+1
FCALLSCSUB11(ffgsvk,FTGSVK,ftgsvk,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,INT,INTV,PLOGICAL,PINT)

#define ftgsve_LONGV_A4 A3+1
#define ftgsve_LONGV_A5 A3+1
#define ftgsve_LONGV_A6 A3+1
#define ftgsve_LONGV_A7 A3+1
FCALLSCSUB11(ffgsve,FTGSVE,ftgsve,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,FLOAT,FLOATV,PLOGICAL,PINT)

#define ftgsvd_LONGV_A4 A3+1
#define ftgsvd_LONGV_A5 A3+1
#define ftgsvd_LONGV_A6 A3+1
#define ftgsvd_LONGV_A7 A3+1
FCALLSCSUB11(ffgsvd,FTGSVD,ftgsvd,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,DOUBLE,DOUBLEV,PLOGICAL,PINT)


/*   Must handle LOGICALV conversion manually   */
void Cffgsfb( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, unsigned char *array, int *flagval, int *anynul, int *status );
void Cffgsfb( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, unsigned char *array, int *flagval, int *anynul, int *status )
{
   char *Cflagval;
   long nflagval;
   int i;
 
   for( nflagval=1, i=0; i<naxis; i++ )
      nflagval *= (trc[i]-blc[i])/inc[i]+1;
   Cflagval = F2CcopyLogVect(nflagval, flagval );
   ffgsfb( fptr, colnum, naxis, naxes, blc, trc, inc, array, Cflagval, anynul, status );   
   C2FcopyLogVect(nflagval, flagval, Cflagval);
}
#define ftgsfb_LONGV_A4 A3+1
#define ftgsfb_LONGV_A5 A3+1
#define ftgsfb_LONGV_A6 A3+1
#define ftgsfb_LONGV_A7 A3+1
FCALLSCSUB11(Cffgsfb,FTGSFB,ftgsfb,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,BYTEV,INTV,PLOGICAL,PINT)

/*   Must handle LOGICALV conversion manually   */
void Cffgsfi( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, short *array, int *flagval, int *anynul, int *status );
void Cffgsfi( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, short *array, int *flagval, int *anynul, int *status )
{
   char *Cflagval;
   long nflagval;
   int i;
 
   for( nflagval=1, i=0; i<naxis; i++ )
      nflagval *= (trc[i]-blc[i])/inc[i]+1;
   Cflagval = F2CcopyLogVect(nflagval, flagval );
   ffgsfi( fptr, colnum, naxis, naxes, blc, trc, inc, array, Cflagval, anynul, status );   
   C2FcopyLogVect(nflagval, flagval, Cflagval);
}
#define ftgsfi_LONGV_A4 A3+1
#define ftgsfi_LONGV_A5 A3+1
#define ftgsfi_LONGV_A6 A3+1
#define ftgsfi_LONGV_A7 A3+1
FCALLSCSUB11(Cffgsfi,FTGSFI,ftgsfi,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,SHORTV,INTV,PLOGICAL,PINT)

/*   Must handle LOGICALV conversion manually   */
void Cffgsfk( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, int *array, int *flagval, int *anynul, int *status );
void Cffgsfk( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, int *array, int *flagval, int *anynul, int *status )
{
   char *Cflagval;
   long nflagval;
   int i;
 
   for( nflagval=1, i=0; i<naxis; i++ )
      nflagval *= (trc[i]-blc[i])/inc[i]+1;
   Cflagval = F2CcopyLogVect(nflagval, flagval );
   ffgsfk( fptr, colnum, naxis, naxes, blc, trc, inc, array, Cflagval, anynul, status );   
   C2FcopyLogVect(nflagval, flagval, Cflagval);
}
#define ftgsfk_LONGV_A4 A3+1
#define ftgsfk_LONGV_A5 A3+1
#define ftgsfk_LONGV_A6 A3+1
#define ftgsfk_LONGV_A7 A3+1
FCALLSCSUB11(Cffgsfk,FTGSFK,ftgsfk,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,INTV,INTV,PLOGICAL,PINT)

#define ftgsfj_LONGV_A4 A3+1
#define ftgsfj_LONGV_A5 A3+1
#define ftgsfj_LONGV_A6 A3+1
#define ftgsfj_LONGV_A7 A3+1
FCALLSCSUB11(Cffgsfk,FTGSFJ,ftgsfj,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,INTV,INTV,PLOGICAL,PINT)

/*   Must handle LOGICALV conversion manually   */
void Cffgsfe( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, float *array, int *flagval, int *anynul, int *status );
void Cffgsfe( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, float *array, int *flagval, int *anynul, int *status )
{
   char *Cflagval;
   long nflagval;
   int i;
 
   for( nflagval=1, i=0; i<naxis; i++ )
      nflagval *= (trc[i]-blc[i])/inc[i]+1;
   Cflagval = F2CcopyLogVect(nflagval, flagval );
   ffgsfe( fptr, colnum, naxis, naxes, blc, trc, inc, array, Cflagval, anynul, status );   
   C2FcopyLogVect(nflagval, flagval, Cflagval);
}
#define ftgsfe_LONGV_A4 A3+1
#define ftgsfe_LONGV_A5 A3+1
#define ftgsfe_LONGV_A6 A3+1
#define ftgsfe_LONGV_A7 A3+1
FCALLSCSUB11(Cffgsfe,FTGSFE,ftgsfe,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,FLOATV,INTV,PLOGICAL,PINT)

/*   Must handle LOGICALV conversion manually   */
void Cffgsfd( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, double *array, int *flagval, int *anynul, int *status );
void Cffgsfd( fitsfile *fptr, int colnum, int naxis, long *naxes, long *blc, long *trc, long *inc, double *array, int *flagval, int *anynul, int *status )
{
   char *Cflagval;
   long nflagval;
   int i;
 
   for( nflagval=1, i=0; i<naxis; i++ )
      nflagval *= (trc[i]-blc[i])/inc[i]+1;
   Cflagval = F2CcopyLogVect(nflagval, flagval );
   ffgsfd( fptr, colnum, naxis, naxes, blc, trc, inc, array, Cflagval, anynul, status );   
   C2FcopyLogVect(nflagval, flagval, Cflagval);
}
#define ftgsfd_LONGV_A4 A3+1
#define ftgsfd_LONGV_A5 A3+1
#define ftgsfd_LONGV_A6 A3+1
#define ftgsfd_LONGV_A7 A3+1
FCALLSCSUB11(Cffgsfd,FTGSFD,ftgsfd,FITSUNIT,INT,INT,LONGV,LONGV,LONGV,LONGV,DOUBLEV,INTV,PLOGICAL,PINT)

FCALLSCSUB6(ffggpb,FTGGPB,ftggpb,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB6(ffggpi,FTGGPI,ftggpi,FITSUNIT,LONG,LONG,LONG,SHORTV,PINT)
FCALLSCSUB6(ffggpk,FTGGPJ,ftggpj,FITSUNIT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB6(ffggpk,FTGGPK,ftggpk,FITSUNIT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB6(ffggpe,FTGGPE,ftggpe,FITSUNIT,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB6(ffggpd,FTGGPD,ftggpd,FITSUNIT,LONG,LONG,LONG,DOUBLEV,PINT)

/*--------------------- read column elements -------------*/
/*   To guarantee that we allocate enough memory to hold strings within
     a table, call FFGTCL first to obtain width of the unique string
     and use it as the minimum string width.  Also test whether column
     has a variable width in which case a single string is read
     containing all its characters, so only declare a string vector
     with 1 element.                                                     */

#define ftgcvs_STRV_A7 NUM_ELEMS(velem)
CFextern VOID_cfF(FTGCVS,ftgcvs)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,LONG,LONG,LONG,STRING,PSTRINGV,PLOGICAL,PINT,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGCVS,ftgcvs)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,LONG,LONG,LONG,STRING,PSTRINGV,PLOGICAL,PINT,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(INT,2)
   QCF(LONG,3)
   QCF(LONG,4)
   QCF(LONG,5)
   QCF(STRING,6)
   QCF(PSTRINGV,7)
   QCF(PLOGICAL,8)
   QCF(PINT,9)

   fitsfile *fptr;
   int colnum, *anynul, *status, velem, type;
   long firstrow, firstelem, nelem;
   long repeat;
   unsigned long gMinStrLen=80L;  /* gMin = width */
   char *nulval, **array;

   fptr =      TCF(ftgcvs,FITSUNIT,1,0);
   colnum =    TCF(ftgcvs,INT,2,0);
   firstrow =  TCF(ftgcvs,LONG,3,0);
   firstelem = TCF(ftgcvs,LONG,4,0);
   nelem =     TCF(ftgcvs,LONG,5,0);
   nulval =    TCF(ftgcvs,STRING,6,0);
   /*  put off variable 7 (array) until column type is learned  */
   anynul =    TCF(ftgcvs,PLOGICAL,8,0);
   status =    TCF(ftgcvs,PINT,9,0);
   
   ffgtcl( fptr, colnum, &type, &repeat, (long *)&gMinStrLen, status );
   if( type<0 ) velem = 1;   /*  Variable length column  */
   else velem = nelem;

   array = TCF(ftgcvs,PSTRINGV,7,0);

   ffgcvs( fptr, colnum, firstrow, firstelem, nelem, nulval, array,
           anynul, status );

   RCF(FITSUNIT,1)
   RCF(INT,2)
   RCF(LONG,3)
   RCF(LONG,4)
   RCF(LONG,5)
   RCF(STRING,6)
   RCF(PSTRINGV,7)
   RCF(PLOGICAL,8)
   RCF(PINT,9)
}



#define ftgcl_LOGV_A6 A5
FCALLSCSUB7(ffgcl,FTGCL,ftgcl,FITSUNIT,INT,LONG,LONG,LONG,LOGICALV,PINT)

#define ftgcvl_LOGV_A7 A5
FCALLSCSUB9(ffgcvl,FTGCVL,ftgcvl,FITSUNIT,INT,LONG,LONG,LONG,LOGICAL,LOGICALV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvb,FTGCVB,ftgcvb,FITSUNIT,INT,LONG,LONG,LONG,BYTE,BYTEV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvi,FTGCVI,ftgcvi,FITSUNIT,INT,LONG,LONG,LONG,SHORT,SHORTV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvk,FTGCVJ,ftgcvj,FITSUNIT,INT,LONG,LONG,LONG,INT,INTV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvk,FTGCVK,ftgcvk,FITSUNIT,INT,LONG,LONG,LONG,INT,INTV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcve,FTGCVE,ftgcve,FITSUNIT,INT,LONG,LONG,LONG,FLOAT,FLOATV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvd,FTGCVD,ftgcvd,FITSUNIT,INT,LONG,LONG,LONG,DOUBLE,DOUBLEV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvc,FTGCVC,ftgcvc,FITSUNIT,INT,LONG,LONG,LONG,FLOAT,FLOATV,PLOGICAL,PINT)
FCALLSCSUB9(ffgcvm,FTGCVM,ftgcvm,FITSUNIT,INT,LONG,LONG,LONG,DOUBLE,DOUBLEV,PLOGICAL,PINT)

#define ftgcx_LOGV_A6 A5
FCALLSCSUB7(ffgcx,FTGCX,ftgcx,FITSUNIT,INT,LONG,LONG,LONG,LOGICALV,PINT)

/*   To guarantee that we allocate enough memory to hold strings within
     a table, call FFGTCL first to obtain width of the unique string
     and use it as the minimum string width.  Also test whether column
     has a variable width in which case a single string is read
     containing all its characters, so only declare a string vector
     with 1 element.                                                     */

#define ftgcfs_STRV_A6 NUM_ELEMS(velem)
#define ftgcfs_LOGV_A7 A5
CFextern VOID_cfF(FTGCFS,ftgcfs)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,LONG,LONG,LONG,PSTRINGV,LOGICALV,PLOGICAL,PINT,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTGCFS,ftgcfs)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FITSUNIT,INT,LONG,LONG,LONG,PSTRINGV,LOGICALV,PLOGICAL,PINT,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FITSUNIT,1)
   QCF(INT,2)
   QCF(LONG,3)
   QCF(LONG,4)
   QCF(LONG,5)
   QCF(PSTRINGV,6)
   QCF(LOGICALV,7)
   QCF(PLOGICAL,8)
   QCF(PINT,9)

   fitsfile *fptr;
   int colnum, *anynul, *status, velem, type;
   long firstrow, firstelem, nelem;
   long repeat;
   unsigned long gMinStrLen=80L;  /* gMin = width */
   char **array, *nularray;
 
   fptr =      TCF(ftgcfs,FITSUNIT,1,0);
   colnum =    TCF(ftgcfs,INT,2,0);
   firstrow =  TCF(ftgcfs,LONG,3,0);
   firstelem = TCF(ftgcfs,LONG,4,0);
   nelem =     TCF(ftgcfs,LONG,5,0);
   /*  put off variable 6 (array) until column type is learned  */
   nularray =  TCF(ftgcfs,LOGICALV,7,0);
   anynul =    TCF(ftgcfs,PLOGICAL,8,0);
   status =    TCF(ftgcfs,PINT,9,0);
   
   ffgtcl( fptr, colnum, &type, &repeat, (long*)&gMinStrLen, status );
   if( type<0 ) velem = 1;   /*  Variable length column  */
   else velem = nelem;

   array = TCF(ftgcfs,PSTRINGV,6,0);

   ffgcfs( fptr, colnum, firstrow, firstelem, nelem, array, nularray,
           anynul, status);

   RCF(FITSUNIT,1)
   RCF(INT,2)
   RCF(LONG,3)
   RCF(LONG,4)
   RCF(LONG,5)
   RCF(PSTRINGV,6)
   RCF(LOGICALV,7)
   RCF(PLOGICAL,8)
   RCF(PINT,9)
}



#define ftgcfl_LOGV_A6 A5
#define ftgcfl_LOGV_A7 A5
FCALLSCSUB9(ffgcfl,FTGCFL,ftgcfl,FITSUNIT,INT,LONG,LONG,LONG,LOGICALV,LOGICALV,PLOGICAL,PINT)

#define ftgcfb_LOGV_A7 A5
FCALLSCSUB9(ffgcfb,FTGCFB,ftgcfb,FITSUNIT,INT,LONG,LONG,LONG,BYTEV,LOGICALV,PLOGICAL,PINT)

#define ftgcfi_LOGV_A7 A5
FCALLSCSUB9(ffgcfi,FTGCFI,ftgcfi,FITSUNIT,INT,LONG,LONG,LONG,SHORTV,LOGICALV,PLOGICAL,PINT)

#define ftgcfj_LOGV_A7 A5
FCALLSCSUB9(ffgcfk,FTGCFJ,ftgcfj,FITSUNIT,INT,LONG,LONG,LONG,INTV,LOGICALV,PLOGICAL,PINT)

#define ftgcfk_LOGV_A7 A5
FCALLSCSUB9(ffgcfk,FTGCFK,ftgcfk,FITSUNIT,INT,LONG,LONG,LONG,INTV,LOGICALV,PLOGICAL,PINT)

#define ftgcfe_LOGV_A7 A5
FCALLSCSUB9(ffgcfe,FTGCFE,ftgcfe,FITSUNIT,INT,LONG,LONG,LONG,FLOATV,LOGICALV,PLOGICAL,PINT)

#define ftgcfd_LOGV_A7 A5
FCALLSCSUB9(ffgcfd,FTGCFD,ftgcfd,FITSUNIT,INT,LONG,LONG,LONG,DOUBLEV,LOGICALV,PLOGICAL,PINT)

/*   Must handle LOGICALV conversion manually   */
void Cffgcfc( fitsfile *fptr, int colnum, long firstrow, long firstelem, long nelem, float *array, int *nularray, int *anynul, int *status );
void Cffgcfc( fitsfile *fptr, int colnum, long firstrow, long firstelem, long nelem, float *array, int *nularray, int *anynul, int *status )
{
   char *Cnularray;
 
   Cnularray = F2CcopyLogVect(nelem*2, nularray );
   ffgcfc( fptr, colnum, firstrow, firstelem, nelem, array, Cnularray, anynul, status );   
   C2FcopyLogVect(nelem*2, nularray, Cnularray );
}
FCALLSCSUB9(Cffgcfc,FTGCFC,ftgcfc,FITSUNIT,INT,LONG,LONG,LONG,FLOATV,INTV,PLOGICAL,PINT)

/*   Must handle LOGICALV conversion manually   */
void Cffgcfm( fitsfile *fptr, int colnum, long firstrow, long firstelem, long nelem, double *array, int *nularray, int *anynul, int *status );
void Cffgcfm( fitsfile *fptr, int colnum, long firstrow, long firstelem, long nelem, double *array, int *nularray, int *anynul, int *status )
{
   char *Cnularray;
 
   Cnularray = F2CcopyLogVect(nelem*2, nularray );
   ffgcfm( fptr, colnum, firstrow, firstelem, nelem, array, Cnularray, anynul, status );   
   C2FcopyLogVect(nelem*2, nularray, Cnularray );
}
FCALLSCSUB9(Cffgcfm,FTGCFM,ftgcfm,FITSUNIT,INT,LONG,LONG,LONG,DOUBLEV,INTV,PLOGICAL,PINT)

FCALLSCSUB6(ffgdes,FTGDES,ftgdes,FITSUNIT,INT,LONG,PLONG,PLONG,PINT)

FCALLSCSUB6(ffgtbb,FTGTBB,ftgtbb,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB6(ffgtbb,FTGTBS,ftgtbs,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)

/*------------ write primary array or image elements -------------*/
FCALLSCSUB6(ffpprb,FTPPRB,ftpprb,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB6(ffppri,FTPPRI,ftppri,FITSUNIT,LONG,LONG,LONG,SHORTV,PINT)
FCALLSCSUB6(ffpprk,FTPPRJ,ftpprj,FITSUNIT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB6(ffpprk,FTPPRK,ftpprk,FITSUNIT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB6(ffppre,FTPPRE,ftppre,FITSUNIT,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB6(ffpprd,FTPPRD,ftpprd,FITSUNIT,LONG,LONG,LONG,DOUBLEV,PINT)

FCALLSCSUB7(ffppnb,FTPPNB,ftppnb,FITSUNIT,LONG,LONG,LONG,BYTEV,BYTE,PINT)
FCALLSCSUB7(ffppni,FTPPNI,ftppni,FITSUNIT,LONG,LONG,LONG,SHORTV,SHORT,PINT)
FCALLSCSUB7(ffppnk,FTPPNJ,ftppnj,FITSUNIT,LONG,LONG,LONG,INTV,INT,PINT)
FCALLSCSUB7(ffppnk,FTPPNK,ftppnk,FITSUNIT,LONG,LONG,LONG,INTV,INT,PINT)
FCALLSCSUB7(ffppne,FTPPNE,ftppne,FITSUNIT,LONG,LONG,LONG,FLOATV,FLOAT,PINT)
FCALLSCSUB7(ffppnd,FTPPND,ftppnd,FITSUNIT,LONG,LONG,LONG,DOUBLEV,DOUBLE,PINT)

FCALLSCSUB7(ffp2db,FTP2DB,ftp2db,FITSUNIT,LONG,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB7(ffp2di,FTP2DI,ftp2di,FITSUNIT,LONG,LONG,LONG,LONG,SHORTV,PINT)
FCALLSCSUB7(ffp2dk,FTP2DJ,ftp2dj,FITSUNIT,LONG,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB7(ffp2dk,FTP2DK,ftp2dk,FITSUNIT,LONG,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB7(ffp2de,FTP2DE,ftp2de,FITSUNIT,LONG,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB7(ffp2dd,FTP2DD,ftp2dd,FITSUNIT,LONG,LONG,LONG,LONG,DOUBLEV,PINT)

FCALLSCSUB9(ffp3db,FTP3DB,ftp3db,FITSUNIT,LONG,LONG,LONG,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB9(ffp3di,FTP3DI,ftp3di,FITSUNIT,LONG,LONG,LONG,LONG,LONG,LONG,SHORTV,PINT)
FCALLSCSUB9(ffp3dk,FTP3DJ,ftp3dj,FITSUNIT,LONG,LONG,LONG,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB9(ffp3dk,FTP3DK,ftp3dk,FITSUNIT,LONG,LONG,LONG,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB9(ffp3de,FTP3DE,ftp3de,FITSUNIT,LONG,LONG,LONG,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB9(ffp3dd,FTP3DD,ftp3dd,FITSUNIT,LONG,LONG,LONG,LONG,LONG,LONG,DOUBLEV,PINT)

#define ftpssb_LONGV_A4 A3
#define ftpssb_LONGV_A5 A3
#define ftpssb_LONGV_A6 A3
FCALLSCSUB8(ffpssb,FTPSSB,ftpssb,FITSUNIT,LONG,LONG,LONGV,LONGV,LONGV,BYTEV,PINT)

#define ftpssi_LONGV_A4 A3
#define ftpssi_LONGV_A5 A3
#define ftpssi_LONGV_A6 A3
FCALLSCSUB8(ffpssi,FTPSSI,ftpssi,FITSUNIT,LONG,LONG,LONGV,LONGV,LONGV,SHORTV,PINT)

#define ftpssj_LONGV_A4 A3
#define ftpssj_LONGV_A5 A3
#define ftpssj_LONGV_A6 A3
FCALLSCSUB8(ffpssk,FTPSSJ,ftpssj,FITSUNIT,LONG,LONG,LONGV,LONGV,LONGV,INTV,PINT)

#define ftpssk_LONGV_A4 A3
#define ftpssk_LONGV_A5 A3
#define ftpssk_LONGV_A6 A3
FCALLSCSUB8(ffpssk,FTPSSK,ftpssk,FITSUNIT,LONG,LONG,LONGV,LONGV,LONGV,INTV,PINT)

#define ftpsse_LONGV_A4 A3
#define ftpsse_LONGV_A5 A3
#define ftpsse_LONGV_A6 A3
FCALLSCSUB8(ffpsse,FTPSSE,ftpsse,FITSUNIT,LONG,LONG,LONGV,LONGV,LONGV,FLOATV,PINT)

#define ftpssd_LONGV_A4 A3
#define ftpssd_LONGV_A5 A3
#define ftpssd_LONGV_A6 A3
FCALLSCSUB8(ffpssd,FTPSSD,ftpssd,FITSUNIT,LONG,LONG,LONGV,LONGV,LONGV,DOUBLEV,PINT)

FCALLSCSUB6(ffpgpb,FTPGPB,ftpgpb,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB6(ffpgpi,FTPGPI,ftpgpi,FITSUNIT,LONG,LONG,LONG,SHORTV,PINT)
FCALLSCSUB6(ffpgpk,FTPGPJ,ftpgpj,FITSUNIT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB6(ffpgpk,FTPGPK,ftpgpk,FITSUNIT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB6(ffpgpe,FTPGPE,ftpgpe,FITSUNIT,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB6(ffpgpd,FTPGPD,ftpgpd,FITSUNIT,LONG,LONG,LONG,DOUBLEV,PINT)

FCALLSCSUB5(ffppru,FTPPRU,ftppru,FITSUNIT,LONG,LONG,LONG,PINT)
FCALLSCSUB4(ffpprn,FTPPRN,ftpprn,FITSUNIT,LONG,LONG,PINT)

/*--------------------- write column elements -------------*/
#define ftpcls_STRV_A6 NUM_ELEM_ARG(5)
FCALLSCSUB7(ffpcls,FTPCLS,ftpcls,FITSUNIT,INT,LONG,LONG,LONG,STRINGV,PINT)

#define ftpcll_LOGV_A6 A5
FCALLSCSUB7(ffpcll,FTPCLL,ftpcll,FITSUNIT,INT,LONG,LONG,LONG,LOGICALV,PINT)
FCALLSCSUB7(ffpclb,FTPCLB,ftpclb,FITSUNIT,INT,LONG,LONG,LONG,BYTEV,PINT)
FCALLSCSUB7(ffpcli,FTPCLI,ftpcli,FITSUNIT,INT,LONG,LONG,LONG,SHORTV,PINT)
FCALLSCSUB7(ffpclk,FTPCLJ,ftpclj,FITSUNIT,INT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB7(ffpclk,FTPCLK,ftpclk,FITSUNIT,INT,LONG,LONG,LONG,INTV,PINT)
FCALLSCSUB7(ffpcle,FTPCLE,ftpcle,FITSUNIT,INT,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB7(ffpcld,FTPCLD,ftpcld,FITSUNIT,INT,LONG,LONG,LONG,DOUBLEV,PINT)
FCALLSCSUB7(ffpclc,FTPCLC,ftpclc,FITSUNIT,INT,LONG,LONG,LONG,FLOATV,PINT)
FCALLSCSUB7(ffpclm,FTPCLM,ftpclm,FITSUNIT,INT,LONG,LONG,LONG,DOUBLEV,PINT)
FCALLSCSUB6(ffpclu,FTPCLU,ftpclu,FITSUNIT,INT,LONG,LONG,LONG,PINT)

#define ftpclx_LOGV_A6 A5
FCALLSCSUB7(ffpclx,FTPCLX,ftpclx,FITSUNIT,INT,LONG,LONG,LONG,LOGICALV,PINT)

#define ftpcns_STRV_A6 NUM_ELEM_ARG(5)
FCALLSCSUB8(ffpcns,FTPCNS,ftpcns,FITSUNIT,INT,LONG,LONG,LONG,STRINGV,STRING,PINT)

FCALLSCSUB8(ffpcnb,FTPCNB,ftpcnb,FITSUNIT,INT,LONG,LONG,LONG,BYTEV,BYTE,PINT)
FCALLSCSUB8(ffpcni,FTPCNI,ftpcni,FITSUNIT,INT,LONG,LONG,LONG,SHORTV,SHORT,PINT)
FCALLSCSUB8(ffpcnk,FTPCNJ,ftpcnj,FITSUNIT,INT,LONG,LONG,LONG,INTV,INT,PINT)
FCALLSCSUB8(ffpcnk,FTPCNK,ftpcnk,FITSUNIT,INT,LONG,LONG,LONG,INTV,INT,PINT)
FCALLSCSUB8(ffpcne,FTPCNE,ftpcne,FITSUNIT,INT,LONG,LONG,LONG,FLOATV,FLOAT,PINT)
FCALLSCSUB8(ffpcnd,FTPCND,ftpcnd,FITSUNIT,INT,LONG,LONG,LONG,DOUBLEV,DOUBLE,PINT)

FCALLSCSUB6(ffpdes,FTPDES,ftpdes,FITSUNIT,INT,LONG,LONG,LONG,PINT)

FCALLSCSUB6(ffptbb,FTPTBB,ftptbb,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)
   /*  Add extra entry point to ffptbb... ftptbs obsolete  */
FCALLSCSUB6(ffptbb,FTPTBS,ftptbs,FITSUNIT,LONG,LONG,LONG,BYTEV,PINT)

FCALLSCSUB4(ffirow,FTIROW,ftirow,FITSUNIT,LONG,LONG,PINT)
FCALLSCSUB4(ffdrow,FTDROW,ftdrow,FITSUNIT,LONG,LONG,PINT)
#define ftdrws_LONGV_A2 A3
FCALLSCSUB4(ffdrws,FTDRWS,ftdrws,FITSUNIT,LONGV,LONG,PINT)
FCALLSCSUB5(fficol,FTICOL,fticol,FITSUNIT,INT,STRING,STRING,PINT)

#define fticls_STRV_A4 NUM_ELEM_ARG(3)
#define fticls_STRV_A5 NUM_ELEM_ARG(3)
FCALLSCSUB6(fficls,FTICLS,fticls,FITSUNIT,INT,INT,STRINGV,STRINGV,PINT)
FCALLSCSUB4(ffmvec,FTMVEC,ftmvec,FITSUNIT,INT,LONG,PINT)
FCALLSCSUB3(ffdcol,FTDCOL,ftdcol,FITSUNIT,INT,PINT)
FCALLSCSUB6(ffcpcl,FTCPCL,ftcpcl,FITSUNIT,FITSUNIT,INT,INT,INT,PINT)

/*********************************************************************/
/*                     Iterator Functions                            */
/*********************************************************************/

/* Use a simple ellipse prototype for Fwork_fn to satisfy finicky compilers */
typedef struct {
   void *userData;
   void (*Fwork_fn)(PLONG_cfTYPE *total_n, ...);
} FtnUserData;

/*        Declare protoypes to make C++ happy       */
int Cwork_fn(long, long, long, long, int, iteratorCol *, void *);
void Cffiter( int n_cols, int *units, int *colnum, char *colname[], 
	      int *datatype, int *iotype,
              long offset, long n_per_loop, void *Fwork_fn,
	      void *userData, int *status);

/******************************************************************/
/*  Cffiter is the wrapper for CFITSIO's ffiter which takes most  */
/*  of its arguments via a structure, iteratorCol.  This routine  */
/*  takes a list of arrays and converts them into a single array  */
/*  of type iteratorCol and passes it to CFITSIO.  Because ffiter */
/*  will be passing control to a Fortran work function, the C     */
/*  wrapper, Cwork_fn, must be passed in its place which then     */
/*  calls the Fortran routine after the necessary data            */
/*  manipulation.  The Fortran routine is passed via the user-    */
/*  supplied parameter pointer.                                   */
/******************************************************************/

void Cffiter( int n_cols, int *units, int *colnum, char *colname[], 
	      int *datatype, int *iotype,
              long offset, long n_per_loop, void *Fwork_fn,
	      void *userData, int *status)
{
   iteratorCol *cols;
   int i;
   FtnUserData FuserData;

   FuserData.Fwork_fn = (void(*)(PLONG_cfTYPE *,...))Fwork_fn;
   FuserData.userData = userData;

   cols = (iteratorCol *)malloc( n_cols*sizeof(iteratorCol) );
   if( cols==NULL ) {
      *status = MEMORY_ALLOCATION;
      return;
   }
   for(i=0;i<n_cols;i++) {
      cols[i].fptr     = gFitsFiles[ units[i] ];
      cols[i].colnum   = colnum[i];
      strncpy(cols[i].colname,colname[i],70);
      cols[i].datatype = datatype[i];
      cols[i].iotype   = iotype[i];
   }

   ffiter( n_cols, cols, offset, n_per_loop, Cwork_fn, 
	   (void*)&FuserData, status );
   free(cols);
}
#define ftiter_STRV_A4 NUM_ELEM_ARG(1)
FCALLSCSUB11(Cffiter,FTITER,ftiter,INT,INTV,INTV,STRINGV,INTV,INTV,LONG,LONG,PVOID,PVOID,PINT)

/*-----------------------------------------------------------------*/
/*  This function is called by CFITSIO's ffiter and serves as the  */
/*  wrapper for the Fortran work function which is passed in the   */
/*  extra user-supplied pointer.  It breaks up C's iteratorCol     */
/*  into several separate arrays.  Because we cannot send an       */
/*  array of pointers for the column data, we instead send *many*  */
/*  arrays as final parameters.                                    */
/*-----------------------------------------------------------------*/

int Cwork_fn( long total_n, long offset,       long first_n,    long n_values,
	      int n_cols,   iteratorCol *cols, void *FuserData )
{
   int  *units, *colnum, *datatype, *iotype, *repeat;
   char **sptr;
   void **ptrs;
   int  i,j,k,nstr,status=0;
   long *slen;

#ifdef vmsFortran
   /*  Passing strings under VMS require a special structure  */
   fstringvector *vmsStrs;
#endif

   /*  Allocate memory for all the arrays.  Grab all the int's  */
   /*  at once and divide up among parameters                   */

   ptrs  = (void**)malloc(2*n_cols*sizeof(void*));
   if( ptrs==NULL )
      return( MEMORY_ALLOCATION );
   units = (int*)malloc(5*n_cols*sizeof(int));
   if( units==NULL ) {
      free(ptrs);
      return( MEMORY_ALLOCATION );
   }
   colnum   = units + 1 * n_cols;
   datatype = units + 2 * n_cols;
   iotype   = units + 3 * n_cols;
   repeat   = units + 4 * n_cols;

   nstr = 0;
   slen = (long*)(ptrs+n_cols);
#ifdef vmsFortran
   vmsStrs = (fstringvector *)calloc(sizeof(fstringvector),n_cols);
   if( vmsStrs==NULL ) {
      free(ptrs);
      free(units);
      return( MEMORY_ALLOCATION );
   }
#endif

   for(i=0;i<n_cols;i++) {
      for(j=0;j<MAXFITSFILES;j++)
	 if( cols[i].fptr==gFitsFiles[j] )
	    units[i] = j;
      colnum[i]   = cols[i].colnum;
      datatype[i] = cols[i].datatype;
      iotype[i]   = cols[i].iotype;
      repeat[i]   = cols[i].repeat;

      if( datatype[i]==TLOGICAL ) {
	 /*  Don't forget first element is null value  */
	 ptrs[i] = (void *)malloc( (n_values*repeat[i]+1)*4 );
	 if( ptrs[i]==NULL ) {
	    free(ptrs);
	    free(units);
	    return( MEMORY_ALLOCATION );
	 }
	 for( j=0;j<=n_values*repeat[i]; j++ )
	    ((int*)ptrs[i])[j] = C2FLOGICAL( ((char*)cols[i].array)[j]);
      } else if ( datatype[i]==TSTRING ) {
	 sptr = (char**)cols[i].array;
	 slen[nstr] = sptr[1] - sptr[0];
	 for(j=0;j<=n_values;j++)
	    for(k=strlen( sptr[j] );k<slen[nstr];k++)
	       sptr[j][k] = ' ';
#ifdef vmsFortran
	 vmsStrs[nstr].dsc$a_pointer         = sptr[0];
	 vmsStrs[nstr].dsc$w_length          = slen[nstr];
	 vmsStrs[nstr].dsc$l_m[0]            = n_values+1;
	 vmsStrs[nstr].dsc$l_arsize          = slen[nstr] * (n_values+1);
	 vmsStrs[nstr].dsc$bounds[0].dsc$l_u = n_values+1;
	 vmsStrs[nstr].dsc$a_a0              = sptr[0] - slen[nstr];
	 ptrs[i] = (void *)(vmsStrs+nstr);
#else
	 ptrs[i] = (void *)sptr[0];
#endif
	 nstr++;
      } else
	 ptrs[i] = (void *)cols[i].array;
   }

   if(!status) {
              /*  Handle Fortran function call manually...  */
	      /*  cfortran.h cannot handle all the desired  */
              /*  'ptrs' nor the indirect function call.    */

      PLONG_cfTYPE a1,a2,a3,a4;    /* Do this in case longs are */ 
      FtnUserData *f;              /* not the same size as ints */

      a1 = total_n;
      a2 = offset;
      a3 = first_n;
      a4 = n_values;
      f  = (FtnUserData *)FuserData;

      f->Fwork_fn(&a1,&a2,&a3,&a4,&n_cols,units,colnum,datatype,
		  iotype,repeat,&status,f->userData,
		  ptrs[ 0], ptrs[ 1], ptrs[ 2], ptrs[ 3], ptrs[ 4],
		  ptrs[ 5], ptrs[ 6], ptrs[ 7], ptrs[ 8], ptrs[ 9],
		  ptrs[10], ptrs[11], ptrs[12], ptrs[13], ptrs[14],
		  ptrs[15], ptrs[16], ptrs[17], ptrs[18], ptrs[19],
		  ptrs[20], ptrs[21], ptrs[22], ptrs[23], ptrs[24] );
   }

   /*  Check whether there are any LOGICAL or STRING columns being outputted  */
   nstr=0;
   for( i=0;i<n_cols;i++ ) {
      if( iotype[i]!=InputCol ) {
	 if( datatype[i]==TLOGICAL ) {
	    for( j=0;j<=n_values*repeat[i];j++ )
	       ((char*)cols[i].array)[j] = F2CLOGICAL( ((int*)ptrs[i])[j] );
	    free(ptrs[i]);
	 } else if( datatype[i]==TSTRING ) {
	    for( j=0;j<=n_values;j++ )
	       ((char**)cols[i].array)[j][slen[nstr]-1] = '\0';
	 }
      }
      if( datatype[i]==TSTRING ) nstr++;
   }

   free(ptrs);
   free(units);
#ifdef vmsFortran
   free(vmsStrs);
#endif
   return(status);
}


/*--------------------- WCS Utilities ----------------------------*/
FCALLSCSUB10(ffgics,FTGICS,ftgics,FITSUNIT,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PSTRING,PINT)
FCALLSCSUB12(ffgtcs,FTGTCS,ftgtcs,FITSUNIT,INT,INT,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PSTRING,PINT)
FCALLSCSUB13(ffwldp,FTWLDP,ftwldp,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,STRING,PDOUBLE,PDOUBLE,PINT)
FCALLSCSUB13(ffxypx,FTXYPX,ftxypx,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,STRING,PDOUBLE,PDOUBLE,PINT)

/*------------------- Conversion Utilities -----------------*/
/*                 (prototyped in fitsio2.h)                */
/*----------------------------------------------------------*/

CFextern VOID_cfF(FTI2C,fti2c)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),LONG,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTI2C,fti2c)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),LONG,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(LONG,1)
   QCF(PSTRING,2)
   QCF(PINT,3)
   char str[21];

   ffi2c( TCF(fti2c,LONG,1,0) 
          TCF(fti2c,PSTRING,2,1) 
          TCF(fti2c,PINT,3,1) );

   sprintf(str,"%20s",B2);
   strcpy(B2,str);

   RCF(LONG,1)
   RCF(PSTRING,2)
   RCF(PINT,3)
}

CFextern VOID_cfF(FTL2C,ftl2c)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),LOGICAL,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTL2C,ftl2c)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),LOGICAL,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(LOGICAL,1)
   QCF(PSTRING,2)
   QCF(PINT,3)
   char str[21];

   ffl2c( TCF(ftl2c,LOGICAL,1,0) 
          TCF(ftl2c,PSTRING,2,1) 
          TCF(ftl2c,PINT,3,1) );

   sprintf(str,"%20s",B2);
   strcpy(B2,str);

   RCF(LOGICAL,1)
   RCF(PSTRING,2)
   RCF(PINT,3)
}

FCALLSCSUB3(ffs2c,FTS2C,fts2c,STRING,PSTRING,PINT)

CFextern VOID_cfF(FTR2F,ftr2f)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FLOAT,INT,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTR2F,ftr2f)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FLOAT,INT,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FLOAT,1)
   QCF(INT,2)
   QCF(PSTRING,3)
   QCF(PINT,4)
   char str[21];

   ffr2f( TCF(ftr2f,FLOAT,1,0) 
          TCF(ftr2f,INT,2,1) 
          TCF(ftr2f,PSTRING,3,1) 
          TCF(ftr2f,PINT,4,1) );

   sprintf(str,"%20s",B3);
   strcpy(B3,str);

   RCF(FLOAT,1)
   RCF(INT,2)
   RCF(PSTRING,3)
   RCF(PINT,4)
}

CFextern VOID_cfF(FTR2E,ftr2e)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FLOAT,INT,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTR2E,ftr2e)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),FLOAT,INT,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(FLOAT,1)
   QCF(INT,2)
   QCF(PSTRING,3)
   QCF(PINT,4)
   char str[21];

   ffr2e( TCF(ftr2e,FLOAT,1,0) 
          TCF(ftr2e,INT,2,1) 
          TCF(ftr2e,PSTRING,3,1) 
          TCF(ftr2e,PINT,4,1) );

   sprintf(str,"%20s",B3);
   strcpy(B3,str);

   RCF(FLOAT,1)
   RCF(INT,2)
   RCF(PSTRING,3)
   RCF(PINT,4)
}

CFextern VOID_cfF(FTD2F,ftd2f)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),DOUBLE,INT,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTD2F,ftd2f)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),DOUBLE,INT,PSTRING,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(DOUBLE,1)
   QCF(INT,2)
   QCF(PSTRING,3)
   QCF(PINT,4)
   char str[21];

   ffd2f( TCF(ftd2f,DOUBLE,1,0) 
          TCF(ftd2f,INT,2,1) 
          TCF(ftd2f,PSTRING,3,1) 
          TCF(ftd2f,PINT,4,1) );

   sprintf(str,"%20s",B3);
   strcpy(B3,str);

   RCF(DOUBLE,1)
   RCF(INT,2)
   RCF(PSTRING,3)
   RCF(PINT,4)
}

CFextern VOID_cfF(FTD2E,ftd2e)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),DOUBLE,INT,PSTRING,PINT,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0));
CFextern VOID_cfF(FTD2E,ftd2e)
CFARGT14(NCF,DCF,ABSOFT_cf2(VOID),DOUBLE,INT,PSTRING,PINT,PINT,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0,CF_0))
{
   QCF(DOUBLE,1)
   QCF(INT,2)
   QCF(PSTRING,3)
   QCF(PINT,4)
   QCF(PINT,5)
   char str[21];
   int *vlen;

   vlen = TCF(ftd2e,PINT,4,0);

   /*  C version of routine doesn't use the 4th parameter, vlen  */
   ffd2e( TCF(ftd2e,DOUBLE,1,0) 
          TCF(ftd2e,INT,2,1) 
          TCF(ftd2e,PSTRING,3,1) 
          TCF(ftd2e,PINT,5,1) );

   *vlen = strlen(B3);
   if ( *vlen<20 ) {
      sprintf(str,"%20s",B3);  /* right justify if vlen<20 characters */
      strcpy(B3,str);
      *vlen = 20;
   }

   RCF(DOUBLE,1)
   RCF(INT,2)
   RCF(PSTRING,3)
   RCF(PINT,4)
   RCF(PINT,5)
}

FCALLSCSUB3(ffc2ii,FTC2II,ftc2ii,STRING,PLONG,PINT)
FCALLSCSUB3(ffc2ll,FTC2LL,ftc2ll,STRING,PINT,PINT)
FCALLSCSUB3(ffc2rr,FTC2RR,ftc2rr,STRING,PFLOAT,PINT)
FCALLSCSUB3(ffc2dd,FTC2DD,ftc2dd,STRING,PDOUBLE,PINT)
FCALLSCSUB7(ffc2x,FTC2X,ftc2x,STRING,PSTRING,PLONG,PINT,PSTRING,PDOUBLE,PINT)
FCALLSCSUB3(ffc2s,FTC2S,ftc2s,STRING,PSTRING,PINT)
FCALLSCSUB3(ffc2i,FTC2I,ftc2i,STRING,PLONG,PINT)
FCALLSCSUB3(ffc2r,FTC2R,ftc2r,STRING,PFLOAT,PINT)
FCALLSCSUB3(ffc2d,FTC2D,ftc2d,STRING,PDOUBLE,PINT)
FCALLSCSUB3(ffc2l,FTC2L,ftc2l,STRING,PINT,PINT)

/*------------------ Byte-level read/seek/write -----------------*/
/*                   (prototyped in fitsio2.h)                   */
/*---------------------------------------------------------------*/

FCALLSCSUB4(ffmbyt,FTMBYT,ftmbyt,FITSUNIT,LONG,LOGICAL,PINT)
FCALLSCSUB4(ffgbyt,FTGCBF,ftgcbf,FITSUNIT,LONG,PVOID,PINT)
FCALLSCSUB4(ffgbyt,FTGBYT,ftgbyt,FITSUNIT,LONG,PVOID,PINT)

FCALLSCSUB4(ffpbyt,FTPCBF,ftpcbf,FITSUNIT,LONG,PVOID,PINT)
FCALLSCSUB4(ffpbyt,FTPBYT,ftpbyt,FITSUNIT,LONG,PVOID,PINT)


/*-------------- Additional missing FITSIO routines -------------*/
/*                   (abandoned in CFITSIO)                      */
/*---------------------------------------------------------------*/

void Cffcrep( char *comm, char *comm1, int *repeat );
void Cffcrep( char *comm, char *comm1, int *repeat )
{
/*
   check if the first comment string is to be repeated for all keywords
   (if the last non-blank character is '&', then it is to be repeated)

   comm    input comment string
   OUTPUT PARAMETERS:
   comm1   output comment string, = COMM minus the last '&' character
   repeat  TRUE if the last character of COMM was the '&' character

   written by Wm Pence, HEASARC/GSFC, June 1991
   translated to C by Peter Wilson, HSTX/GSFC, Oct 1997
*/

   int len;

   *repeat=FALSE;
   len=strlen(comm);
       /* cfortran strips trailing spaces so only check last character  */
   if( len && comm[ len-1 ]=='&' ) {
      strncpy(comm1,comm,len-1);  /*  Don't copy '&'  */
      comm1[len-1]='\0';
      *repeat=TRUE;
   }
   return;
}
FCALLSCSUB3(Cffcrep,FTCREP,ftcrep,STRING,PSTRING,PLOGICAL)


/*------------------ Test floats for NAN values -----------------*/
/*                     (defined in fitsio2.h)                    */
/*---------------------------------------------------------------*/

int Cfnan( float *val );
int Cfnan( float *val )
{
   int code;

#if BYTESWAPPED
   short *sptr = (short*)val + 1;
#else
   short *sptr = (short*)val;
#endif

   code = fnan(*sptr);
   if( code==2 ) *val = 0.0;   /* Underflow */

   return( code!=0 );
}
FCALLSCFUN1(LOGICAL,Cfnan,FTTRNN,fttrnn,PFLOAT)


int Cdnan( double *val );
int Cdnan( double *val )
{
   int code;

#if BYTESWAPPED
   short *sptr = (short*)val + 3;
#else
   short *sptr = (short*)val;
#endif

   code = dnan(*sptr);
   if( code==2 ) *val = 0.0;   /* Underflow */

   return( code!=0 );
}
FCALLSCFUN1(LOGICAL,Cdnan,FTTDNN,fttdnn,PDOUBLE)

/*-------- Functions no longer supported... normally redundant -----------*/
/*                Included only to support older code                     */
/*------------------------------------------------------------------------*/

void Cffempty(void);
void Cffempty(void)
{ return; }
FCALLSCSUB0(Cffempty,FTPDEF,ftpdef)
FCALLSCSUB0(Cffempty,FTBDEF,ftbdef)
FCALLSCSUB0(Cffempty,FTADEF,ftadef)
FCALLSCSUB0(Cffempty,FTDDEF,ftddef)


/*-------- Functions which use the lex and yacc/bison parser code -----------*/
/*---------------------------------------------------------------------------*/

#define fttexp_LONGV_A7 A3
FCALLSCSUB8(fftexp,FTTEXP,fttexp,FITSUNIT,STRING,INT,PINT,PLONG,PINT,LONGV,PINT)

#define ftfrow_LOGV_A6 A4
FCALLSCSUB7(fffrow,FTFROW,ftfrow,FITSUNIT,STRING,LONG,LONG,PLONG,LOGICALV,PINT)

#define ftfrwc_LOGV_A8 A6
FCALLSCSUB9(fffrwc,FTFRWC,ftfrwc,FITSUNIT,STRING,STRING,STRING,STRING,LONG,DOUBLEV,LOGICALV,PINT)

FCALLSCSUB4(ffsrow,FTSROW,ftsrow,FITSUNIT,FITSUNIT,STRING,PINT)
FCALLSCSUB9(ffcrow,FTCROW,ftcrow,FITSUNIT,INT,STRING,LONG,LONG,PVOID,PVOID,PLOGICAL,PINT)
FCALLSCSUB6(ffcalc,FTCALC,ftcalc,FITSUNIT,STRING,FITSUNIT,STRING,STRING,PINT)

/*--------------------- grouping routines ------------------*/

FCALLSCSUB4(ffgtcr,FTGTCR,ftgtcr,FITSUNIT,STRING,INT,PINT)
FCALLSCSUB4(ffgtis,FTGTIS,ftgtis,FITSUNIT,STRING,INT,PINT)
FCALLSCSUB3(ffgtch,FTGTCH,ftgtch,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffgtrm,FTGTRM,ftgtrm,FITSUNIT,INT,PINT)
FCALLSCSUB4(ffgtcp,FTGTCP,ftgtcp,FITSUNIT,FITSUNIT,INT,PINT)
FCALLSCSUB4(ffgtmg,FTGTMG,ftgtmg,FITSUNIT,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffgtcm,FTGTCM,ftgtcm,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffgtvf,FTGTVF,ftgtvf,FITSUNIT,PLONG,PINT)
FCALLSCSUB4(ffgtop,FTGTOP,ftgtop,FITSUNIT,INT,PFITSUNIT,PINT)
FCALLSCSUB4(ffgtam,FTGTAM,ftgtam,FITSUNIT,FITSUNIT,INT,PINT)
FCALLSCSUB3(ffgtnm,FTGTNM,ftgtnm,FITSUNIT,PLONG,PINT)
FCALLSCSUB3(ffgmng,FTGMNG,ftgmng,FITSUNIT,PLONG,PINT)
FCALLSCSUB4(ffgmop,FTGMOP,ftgmop,FITSUNIT,LONG,PFITSUNIT,PINT)
FCALLSCSUB5(ffgmcp,FTGMCP,ftgmcp,FITSUNIT,FITSUNIT,LONG,INT,PINT)
FCALLSCSUB5(ffgmtf,FTGMTF,ftgmtf,FITSUNIT,FITSUNIT,LONG,INT,PINT)
FCALLSCSUB4(ffgmrm,FTGMRM,ftgmrm,FITSUNIT,LONG,INT,PINT)
