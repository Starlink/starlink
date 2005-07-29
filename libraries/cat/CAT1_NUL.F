#include <config.h>
*+
*  Name:
*     CAT1_NUL
*  Purpose:
*     Default null values for StarBase.
*  Language:
*     FORTRAN 77.
*  Type of Module:
*     INCLUDE FILE.
*  Description:
*     An include file of parametric constants to hold the default
*     null values used by StarBase.  These default null values are
*     deliberately identical to the Starlink null values, in terms
*     of which they are defined.
*
*     See SUN/39 for a description of the Starlink null values.
*
*     See also StarBase/ACD/3.4, p25, Table 4.2 for further discussion
*     of the StarBase null values.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     11/4/95  (ACD): Changed the name given in the 'Name:' section of
*        the prologue to reflect Unix rather than VMS conventions.
*     25/11/96 (ACD): Incorporated a portable and standard declaration
*        for CAT1__DNULC from the Linux port of CAT.
*  Global Constants:
C      INCLUDE 'PRM_PAR'
*-
      
*
*    Null values for the numeric data types.  These parameters are
*    defined in terms of the equivalent Starlink null value.

      BYTE CAT1__DNULUB             ! Unsigned byte.
      PARAMETER (CAT1__DNULUB = VAL__BADUB)

      BYTE CAT1__DNULB              ! Byte.
      PARAMETER (CAT1__DNULB = VAL__BADB)

      INTEGER*2 CAT1__DNULUW        ! Unsigned word.
      PARAMETER (CAT1__DNULUW = VAL__BADUW)

      INTEGER*2 CAT1__DNULW         ! Word.
      PARAMETER (CAT1__DNULW = VAL__BADW)

      INTEGER CAT1__DNULI           ! Integer.
      PARAMETER (CAT1__DNULI = VAL__BADI)

      REAL CAT1__DNULR              ! Real.
      PARAMETER (CAT1__DNULR = VAL__BADR)

      DOUBLE PRECISION CAT1__DNULD  ! Double precision.
      PARAMETER (CAT1__DNULD = VAL__BADD)

*
*    Null values for the non-numeric data types.  There are no Starlink
*    null values for these data types, so local values are used.  Note
*    that the ASCII standard defines a byte with a value of 0 (that is,
*    all its bits set to zero) as null.  This value is adopted as the
*    default for data type CHARACTER.
      LOGICAL CAT1__DNULL
#if FC_HAVE_HEX_NUMBERS
      PARAMETER (CAT1__DNULL = 'A55A5AA5'X )
#else
      PARAMETER (CAT1__DNULL = X'A55A5AA5')
#endif

      CHARACTER CAT1__DNULC*1
*     PARAMETER (CAT1__DNULC = CHAR(0) )
      PARAMETER (CAT1__DNULC = '\000' )
