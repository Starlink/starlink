*+ DAT_NEW0D - Create scalar component
      subroutine dat_new0D(loc, name, status)
*    Description :
*     This routine creates a scalar structure component.
*     There is a routine for each access type,
*
*        DAT_NEW0D    DOUBLE PRECISION
*        DAT_NEW0I    INTEGER
*        DAT_NEW0L    LOGICAL
*        DAT_NEW0R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_NEW0D(LOC, NAME; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be
*           created in the structure.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW0.
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
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new0(loc, name, '_DOUBLE', status)
      endif

      end
*+ DAT_NEW0I - Create scalar component
      subroutine dat_new0I(loc, name, status)
*    Description :
*     This routine creates a scalar structure component.
*     There is a routine for each access type,
*
*        DAT_NEW0D    DOUBLE PRECISION
*        DAT_NEW0I    INTEGER
*        DAT_NEW0L    LOGICAL
*        DAT_NEW0R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_NEW0I(LOC, NAME; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be
*           created in the structure.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW0.
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
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new0(loc, name, '_INTEGER', status)
      endif

      end
*+ DAT_NEW0L - Create scalar component
      subroutine dat_new0L(loc, name, status)
*    Description :
*     This routine creates a scalar structure component.
*     There is a routine for each access type,
*
*        DAT_NEW0D    DOUBLE PRECISION
*        DAT_NEW0I    INTEGER
*        DAT_NEW0L    LOGICAL
*        DAT_NEW0R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_NEW0L(LOC, NAME; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be
*           created in the structure.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW0.
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
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new0(loc, name, '_LOGICAL', status)
      endif

      end
*+ DAT_NEW0R - Create scalar component
      subroutine dat_new0R(loc, name, status)
*    Description :
*     This routine creates a scalar structure component.
*     There is a routine for each access type,
*
*        DAT_NEW0D    DOUBLE PRECISION
*        DAT_NEW0I    INTEGER
*        DAT_NEW0L    LOGICAL
*        DAT_NEW0R    REAL
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).
*    Invocation :
*     CALL DAT_NEW0R(LOC, NAME; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the name of the component to be
*           created in the structure.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Use DAT_NEW0.
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
*    Export :
*    Status return :
      integer status			! Status Return
*-

      if (status .eq. SAI__OK) then
         call dat_new0(loc, name, '_REAL', status)
      endif

      end
