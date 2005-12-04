*+ DAT_PUTVC - Write values to an object as if it were a vector
      subroutine dat_putvC(loc, nval, values, status)
*    Description :
*     Write the values into a primitive object as if it were
*     vectorized (i.e. regardless of its actual dimensionality).
*     There is a routine for each access type,
*
*        DAT_PUTVD    DOUBLE PRECISION
*        DAT_PUTVR    REAL
*        DAT_PUTVI    INTEGER
*        DAT_PUTVL    LOGICAL
*        DAT_PUTVC    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL DAT_PUTVC(LOC, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written into the object.   It must match the actual object
*           size.
*     VALUES(NVAL)=CHARACTER*(*)
*           Array to containing the values to be written into the
*           object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Vectorize the object using DAT_VEC, then use DAT_PUTC.
*     Be careful to conform to Fortran 77 standard, with regard to passing
*     arrays to subprograms.
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
      integer nval			! Number of values to be written
      CHARACTER*(*) values(*)			! Vector to supply values
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
            call dat_put1C(vec, nval, values, status)
            call dat_annul(vec, status)
         endif
      endif

      end
