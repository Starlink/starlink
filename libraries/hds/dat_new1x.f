*+ DAT_NEW1D - Create vector component
      subroutine dat_new1D(loc, name, len, status)
*    Description :
*     This routine creates a vector structure component.
*     There is a routine for each access type,
*
*        DAT_NEW1D    DOUBLE PRECISION
*        DAT_NEW1I    INTEGER
*        DAT_NEW1L    LOGICAL
*        DAT_NEW1R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (1-D).
*    Invocation :
*     CALL DAT_NEW1D(LOC, NAME, LEN; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be 
*           created in the structure.
*     LEN=INTEGER
*           Expression specifying the length of the vector.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW1.
*    Authors :
*     Sid Wright (UCL::SLW)
*    History :
*     31-Aug-1983:  Original.  (UCL::SLW)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     12-NOV-1991 (RFWS): Bug fix: changed _DOUBLE PRECISION to _DOUBLE.
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc			! Object Locator
      character*(*) name		! Component Name
      integer len			! length of the vector
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new1(loc, name, '_DOUBLE', len, status)
      endif

      end
*+ DAT_NEW1I - Create vector component
      subroutine dat_new1I(loc, name, len, status)
*    Description :
*     This routine creates a vector structure component.
*     There is a routine for each access type,
*
*        DAT_NEW1D    DOUBLE PRECISION
*        DAT_NEW1I    INTEGER
*        DAT_NEW1L    LOGICAL
*        DAT_NEW1R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (1-D).
*    Invocation :
*     CALL DAT_NEW1I(LOC, NAME, LEN; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be 
*           created in the structure.
*     LEN=INTEGER
*           Expression specifying the length of the vector.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW1.
*    Authors :
*     Sid Wright (UCL::SLW)
*    History :
*     31-Aug-1983:  Original.  (UCL::SLW)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     12-NOV-1991 (RFWS): Bug fix: changed _INTEGER to _INTEGER.
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc			! Object Locator
      character*(*) name		! Component Name
      integer len			! length of the vector
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new1(loc, name, '_INTEGER', len, status)
      endif

      end
*+ DAT_NEW1L - Create vector component
      subroutine dat_new1L(loc, name, len, status)
*    Description :
*     This routine creates a vector structure component.
*     There is a routine for each access type,
*
*        DAT_NEW1D    DOUBLE PRECISION
*        DAT_NEW1I    INTEGER
*        DAT_NEW1L    LOGICAL
*        DAT_NEW1R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (1-D).
*    Invocation :
*     CALL DAT_NEW1L(LOC, NAME, LEN; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be 
*           created in the structure.
*     LEN=INTEGER
*           Expression specifying the length of the vector.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW1.
*    Authors :
*     Sid Wright (UCL::SLW)
*    History :
*     31-Aug-1983:  Original.  (UCL::SLW)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     12-NOV-1991 (RFWS): Bug fix: changed _LOGICAL to _LOGICAL.
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc			! Object Locator
      character*(*) name		! Component Name
      integer len			! length of the vector
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new1(loc, name, '_LOGICAL', len, status)
      endif

      end
*+ DAT_NEW1R - Create vector component
      subroutine dat_new1R(loc, name, len, status)
*    Description :
*     This routine creates a vector structure component.
*     There is a routine for each access type,
*
*        DAT_NEW1D    DOUBLE PRECISION
*        DAT_NEW1I    INTEGER
*        DAT_NEW1L    LOGICAL
*        DAT_NEW1R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (1-D).
*    Invocation :
*     CALL DAT_NEW1R(LOC, NAME, LEN; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be 
*           created in the structure.
*     LEN=INTEGER
*           Expression specifying the length of the vector.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW1.
*    Authors :
*     Sid Wright (UCL::SLW)
*    History :
*     31-Aug-1983:  Original.  (UCL::SLW)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     12-NOV-1991 (RFWS): Bug fix: changed _REAL to _REAL.
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc			! Object Locator
      character*(*) name		! Component Name
      integer len			! length of the vector
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new1(loc, name, '_REAL', len, status)
      endif

      end
