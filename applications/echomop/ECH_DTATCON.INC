C+
C     D T A _ T C O N 
C
C     Conversion type codes for the various supported
C     VAX data types.  These are the same codes as used
C     by STL_FMTCON, the Starlink general type conversion
C     routine.
C
C     Also type codes for the various types supported by
C     the data structure routines, as defined in Starlink
C     paper SGP18.
C
C                                   KS / CIT  19th Sept 1983
C     Modified:
C
C     9th Nov 1988.  TYP_UWRD and TYP_DSUSHORT added. KS / AAO.
C     1st Mar 1990.  TYP_LGCL and TYP_DSLGCL added.  KS/AAO.
C+
      INTEGER TYP_BYTE,TYP_WORD,TYP_LONG,TYP_REAL
      INTEGER TYP_DBLE,TYP_CHAR,TYP_UWRD,TYP_LGCL
      PARAMETER (TYP_BYTE=6,TYP_WORD=2,TYP_LONG=3,TYP_REAL=4,
     :             TYP_DBLE=5,TYP_CHAR=0,TYP_UWRD=7,TYP_LGCL=8)
C
      INTEGER TYP_DSBIT,TYP_DSBYTE,TYP_DSSHORT,TYP_DSINT
      INTEGER TYP_DSLONG,TYP_DSFLOAT,TYP_DSDOUBLE,TYP_DSCHAR
      INTEGER TYP_DSUSHORT,TYP_DSLGCL,TYP_DSFILE,TYP_DSTRUCT
C
C     Note - the values used for the TYP_DS.. codes are used to
C     index into the HTYPE array defined in DTATYPES.INC.
C
      PARAMETER (TYP_DSBIT=1,TYP_DSBYTE=2,TYP_DSSHORT=3,
     :           TYP_DSINT=4,TYP_DSLONG=5,TYP_DSFLOAT=6,
     :           TYP_DSDOUBLE=7,TYP_DSCHAR=8,TYP_DSFILE=9,
     :           TYP_DSUSHORT=10,TYP_DSLGCL=11,TYP_DSTRUCT=0)
