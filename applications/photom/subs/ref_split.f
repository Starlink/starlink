*-----------------------------------------------------------------------
*+ REF_SPLIT - split an object path name into components

      SUBROUTINE REF_SPLIT( PATH, MAXLEVS, NLEV, COMPONENT, STATUS )

*    Description :
*     This routine splits an HDS object path name into its
*     individual components.
*    Invocation :
*     CALL REF_SPLIT(PATH, MAXLEVS; NLEV, COMPONENT, STATUS)
*    Parameters :
*     PATH=CHARACTER*(*)
*           The path name
*     MAXLEVS=INTEGER
*           The maximum number of components to be extracted
*     COMPONENT(*)=CHARACTER*(*)
*           Array to contain the individual component names
*     NLEV=INTEGER
*           The number of components in the path name.
*     STATUS=INTEGER
*           Variable holding the status value.   If this variable
*           is not SAI__OK on input, then the routine will return
*           without action.   If the routine fails to complete,
*           this variable will be set to an appropriate error
*           number.
*    Method :
*     Get the components of PATH separated by '.' and store
*     them in COMPONENT. Count them in NLEV.
*    Authors :
*     A.Chipperfield:  (RAL::AJC)
*     Nick Eaton:      (DUVAD::NE)
*    History :
*     17-Feb-1987 :    Original (RAL::AJC)
*      3-Mar-1992 :    Replaced archaic call to SAI_PAR (NE)

*    Type definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

      INCLUDE 'DAT_PAR'

      INCLUDE 'DAT_ERR'


*    Import :
      CHARACTER*(*)  PATH               ! Object path name
      INTEGER        MAXLEVS            ! Maximum no. of components

*    Export :
      INTEGER NLEV                      ! Number of components
      CHARACTER*(*) COMPONENT(*)        ! Individual components

*    Status return :
      INTEGER STATUS			! Status Return

*    Local variables :
      INTEGER START                     ! Point to start of component
      INTEGER FINISH                    ! Point to end of component
      LOGICAL MORE                      ! If more components
*-

      IF (STATUS .NE. SAI__OK) RETURN

      MORE = .TRUE.
      START = 1
      NLEV = 0

      DOWHILE (MORE .AND. (NLEV.LT.MAXLEVS))
         FINISH = INDEX(PATH(START:), '.') + START -1

         IF(FINISH.GT.START) THEN
*         Normal component found
           NLEV = NLEV + 1
           COMPONENT(NLEV) = PATH(START:FINISH-1)
           START = FINISH + 1

         ELSE IF ( FINISH .LT. START) THEN
*          No '.' found - last component
            NLEV = NLEV + 1
            COMPONENT(NLEV) = PATH(START:)
            MORE = .FALSE.

         ELSE
*          Two consecutive '.' - an error
            STATUS = DAT__NAMIN
            MORE = .FALSE.
         ENDIF
      ENDDO

*    Check that the maximum no. of components was not exceeded
      IF ((NLEV .EQ. MAXLEVS) .AND. MORE) THEN
         STATUS = DAT__NAMIN
      ENDIF

      END

* $Id$
