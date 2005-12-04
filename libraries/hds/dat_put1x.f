*+ DAT_PUT1C - Write vector object values
      subroutine dat_put1C(loc, nval, values, status)
*    Description :
*     Write the values into a vector primitive object.
*     There is a routine for each access type,
*
*        DAT_PUT1D    DOUBLE PRECISION
*        DAT_PUT1R    REAL
*        DAT_PUT1I    INTEGER
*        DAT_PUT1L    LOGICAL
*        DAT_PUT1C    CHARACTER[*n]
*
*     If the object data type differs from the access type, then
*     conversion is performed.
*    Invocation :
*     CALL DAT_PUT1C(LOC, NVAL, VALUES; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a primitive
*           data object.
*     NVAL=INTEGER
*           Expression specifying the number of values that are to be
*           written.   This must match the object size.
*     VALUES(NVAL)=CHARACTER*(*)
*           Array containing the values to be written into the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_SIZE and DAT_PUTC.
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
      integer dims(1)
*-

      if (status .eq. SAI__OK) then
         call dat_size(loc, dims(1), status)
         if (nval .ne. dims(1)) then
            status = DAT__BOUND
         else
            call dat_putC(loc, 1, dims(1), values, status)
         endif
      endif

      end
