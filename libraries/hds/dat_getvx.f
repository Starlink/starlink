*+ DAT_GETVC - Read values from an object as if it were a vector
      subroutine dat_getvC(loc, maxval, values, actval, status)
*    Description :
*     This routine reads the values from a primitive object as if it 
*     were vectorized (i.e. regardless of its actual dimensionality).
*     There is a routine for each access type,
*
*        DAT_GETVD    DOUBLE PRECISION
*        DAT_GETVR    REAL
*        DAT_GETVI    INTEGER
*        DAT_GETVL    LOGICAL
*        DAT_GETVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL DAT_GETVC(LOC, MAXVAL; VALUES, ACTVAL, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     MAXVAL=INTEGER
*           Expression specifying the maximum number of values that
*           can be held in the array, VALUES.
*     VALUES(MAXVAL)=CHARACTER*(*)
*           Array to receive the values associated with the object.
*           It must be of sufficient size to contain them all.
*     ACTVAL=INTEGER
*           Variable to receive the actual number of values read.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Vectorize the object using DAT_VEC, then use DAT_GET1C.
*     Be careful to conform to Fortran 77 standard, with regard to 
*     passing arrays to subprograms.
*    Authors :
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     31-Aug-1983:  Standardise.  (UCL::SLW)
*     05.11.1984:   Remove calls to error system (REVAD::BDK)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
*    Import :
      character*(*) loc			! Object Locator
      integer maxval			! Maximum number of values
*    Export :
      CHARACTER*(*) values(*)			! Array to receive values
      integer actval			! Actual number of values read
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) vec	! Vector locator
*-

      if (status .eq. SAI__OK) then
         call dat_vec(loc, vec, status)
         if (status .ne. SAI__OK) then
            continue
         else
            call dat_get1C(vec, maxval, values, actval, status)
            call dat_annul(vec, status)
         endif
      endif

      end
