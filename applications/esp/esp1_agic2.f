


      SUBROUTINE ELF1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     ELF1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,PICID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA' or
*     'ELLFOU' picture. Activates SGS so that PGPLOT and normal SGS
*     routines can be used. PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        The name of the graphics device being considered.
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NAME = INTEGER (Given)
*        Defines whether DATA or ELLFOU pictures are to looked at.
*        0=DATA 1=ELLFOU
*     NDF1 = INTEGER (Returned)
*        NDF identifier for the picture required.
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:
      CHARACTER *(6) GRADEV           ! Graphics device name
      INTEGER NAME                    ! Defines whether pictures of name
                                      ! DATA or ELLFOU are to used
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.
      INTEGER NDFS                    ! Should the NDF identifier be
                                      ! returned?

*  Arguments Given and Returned:
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER NDF1                    ! An NDF identifier
      INTEGER PICID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL GOTLOC                  ! What type of identifier?
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default value.
      DEVCAN=.FALSE.

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC(GRADEV,'UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA
*      or ELLFOU later will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Create a new SGS_ZONE from current picture of correct name.
         IF (NAME.EQ.0) THEN

*         Find most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)

         ELSE

*         Find most recent ELLFOU results display.
            CALL AGI_RCL('ELLFOU',PICID,STATUS)

         END IF

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN
            DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)

*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a  locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the reference to an HDS.',STATUS)
               CALL ERR_FLUSH(STATUS)
               GOTO 9999
            END IF

*         Check to see if the identifier has been supplied in the old
*         DAT__SZLOC length format.
            CALL DAT_VALID(IDENT1(1:DAT__SZLOC),GOTLOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

*         Use NDF_FIND in a manner suiatble for the type of
*         identifier found.
            IF (GOTLOC) THEN
               IDENT=IDENT1(1:DAT__SZLOC)
               CALL NDF_FIND(IDENT,' ',NDF1,STATUS)
            ELSE
               CALL NDF_FIND(DAT__ROOT,IDENT1,NDF1,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :                      'Could not get the image NDF identifier.',
     :                      STATUS)
               GOTO 9999
            END IF

*         Display the name of the file in question.
            CALL NDF_MSG('IN2',NDF1)
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','Using ^IN2 as the input NDF.',STATUS)

         END IF

      END IF

 9999 CONTINUE


*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL(GRADEV,STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END


      SUBROUTINE ELP1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     ELP1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,PICID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA' or
*     'ELLPRO' picture. Activates SGS so that PGPLOT and normal SGS
*     routines can be used. PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        The name of the graphics device being used.
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NAME = INTEGER (Given)
*        Defines whether DATA or ELLPRO pictures are to looked at.
*        0=DATA 1=ELLPRO
*     NDF1 = INTEGER (Returned)
*        NDF identifier for the picture required.
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:
      CHARACTER *(6) GRADEV           ! Graphics device name
      INTEGER NAME                    ! Defines whether pictures of name
                                      ! DATA or ELLPRO are to used
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.
      INTEGER NDFS                    ! Should the NDF identifier be
                                      ! returned?

*  Arguments Given and Returned:
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER NDF1                    ! An NDF identifier
      INTEGER PICID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL   GOTLOC                ! Was a locator found?
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default value.
      DEVCAN=.FALSE.

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC(GRADEV,'UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA
*      or ELLPRO later will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Create a new SGS_ZONE from current picture of correct name.
         IF (NAME.EQ.0) THEN

*         Find most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)

         ELSE

*         Find most recent ELLPRO results display.
            CALL AGI_RCL('ELLPRO',PICID,STATUS)

         END IF

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN
            DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)

*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the HDS reference to an image.',
     :          STATUS)
               CALL ERR_FLUSH(STATUS)
               GOTO 9999
            END IF

*         Check to see if the identifier has been supplied in the old
*         DAT__SZLOC length format.
            CALL DAT_VALID(IDENT1(1:DAT__SZLOC),GOTLOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

*         Use NDF_FIND in a manner suiatble for the type of
*         identifier found.
            IF (GOTLOC) THEN
               IDENT=IDENT1(1:DAT__SZLOC)
               CALL NDF_FIND(IDENT,' ',NDF1,STATUS)
            ELSE
               CALL NDF_FIND(DAT__ROOT,IDENT1,NDF1,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :                      'Could not get the image NDF identifier.',
     :                      STATUS)
               GOTO 9999
            END IF

*         Display the name of the file in question.
            CALL NDF_MSG('IN2',NDF1)
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','Using ^IN2 as the input NDF.',STATUS)

         END IF

      END IF

 9999 CONTINUE


*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL(GRADEV,STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END


      SUBROUTINE GAU1_AGIC2(GRADEV,ONOFF,NDFS,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     GAU1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_AGIC2(GRADEV,ONOFF,NDFS,NDF1,DEVCAN,PICID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA'.
*     Activates SGS so that PGPLOT and normal SGS routines can be used.
*     PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        The name of the graphics device being used.
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NDF1 = INTEGER (Returned)
*        NDF identifier for the picture required.
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:
      CHARACTER *(6) GRADEV           ! Graphics device name
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.
      INTEGER NDFS                    ! Should the NDF identifier be
                                      ! returned?

*  Arguments Given and Returned:
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER NDF1                    ! An NDF identifier
      INTEGER PICID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL   GOTLOC                ! Was a locator found?
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default value.
      DEVCAN=.FALSE.

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC(GRADEV,'UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA
*      will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'AGI database search failed.',STATUS)
             GOTO 9999
         END IF

*      Create a new SGS_ZONE from most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN
            DEVCAN=.TRUE.
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the HDS reference to an image.',
     :          STATUS)
                GOTO 9999
            END IF

*         Check to see if the identifier has been supplied in the old
*         DAT__SZLOC length format.
            CALL DAT_VALID(IDENT1(1:DAT__SZLOC),GOTLOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

*         Use NDF_FIND in a manner suiatble for the type of
*         identifier found.
            IF (GOTLOC) THEN
               IDENT=IDENT1(1:DAT__SZLOC)
               CALL NDF_FIND(IDENT,' ',NDF1,STATUS)
            ELSE
               CALL NDF_FIND(DAT__ROOT,IDENT1,NDF1,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :                      'Could not get the image NDF identifier.',
     :                      STATUS)
               GOTO 9999
            END IF

*         Display the name of the file in question.
            CALL NDF_MSG('IN2',NDF1)
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','Using ^IN2 as the input NDF.',STATUS)

         END IF

      END IF

 9999 CONTINUE


*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL(GRADEV,STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END


      SUBROUTINE GRA1_AGIC2(ONOFF,DEVCAN,PICID,STATUS)
*+
*  Name:
*     GRA1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS and  displaying graphs using PGPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRA1_AGIC2(ONOFF,DEVCAN,PICID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'GRAPHS'
*     picture. Activates SGS so that PGPLOT and normal SGS
*     routines can be used. PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors

*  Arguments Given:
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.

*  Arguments Given and Returned:
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER PICID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default value.
      DEVCAN=.FALSE.

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC('DEVICE','UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for GRAPHS
*      later will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Create a new SGS_ZONE from current picture of correct name.

*      Find most recent GRAPHS results display.
         CALL AGI_RCL('GRAPHS',PICID,STATUS)

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN
            DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)

      END IF

 9999 CONTINUE


*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL('DEVICE',STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END


      SUBROUTINE SEC1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     SEC1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,PICID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA' or
*     'SECTOR' picture. Activates SGS so that PGPLOT and normal SGS
*     routines can be used. PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        The name of the graphiccs device used.
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NAME = INTEGER (Given)
*        Defines whether DATA or SECTOR pictures are to looked at.
*        0=DATA 1=SECTOR
*     NDF1 = INTEGER (Returned)
*        NDF identifier for the picture required.
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:
      CHARACTER *(6) GRADEV           ! Graphics device name
      INTEGER NAME                    ! Defines whether pictures of name
                                      ! DATA or SECTOR are to used
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.
      INTEGER NDFS                    ! Should the NDF identifier be
                                      ! returned?

*  Arguments Given and Returned:
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER NDF1                    ! An NDF identifier
      INTEGER PICID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL GOTLOC                  ! What type of identifer?
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default value.
      DEVCAN=.FALSE.

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC(GRADEV,'UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA
*      or SECTOR later will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Create a new SGS_ZONE from current picture of correct name.
         IF (NAME.EQ.0) THEN
*         Find most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)
         ELSE
*         Find most recent SECTOR results display.
            CALL AGI_RCL('SECTOR',PICID,STATUS)
         END IF

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN
            DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)

*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the reference to an HDS.',STATUS)
               CALL ERR_FLUSH(STATUS)
               GOTO 9999
            END IF

*         Check to see if the identifier has been supplied in the old
*         DAT__SZLOC length format.
            CALL DAT_VALID(IDENT1(1:DAT__SZLOC),GOTLOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

*         Use NDF_FIND in a manner suiatble for the type of
*         identifier found.
            IF (GOTLOC) THEN
               IDENT=IDENT1(1:DAT__SZLOC)
               CALL NDF_FIND(IDENT,' ',NDF1,STATUS)
            ELSE
               CALL NDF_FIND(DAT__ROOT,IDENT1,NDF1,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :                      'Could not get the image NDF identifier.',
     :                      STATUS)
               GOTO 9999
            END IF

*         Display the name of the file in question.
            CALL NDF_MSG('IN2',NDF1)
            CALL MSG_OUT(' ','Using ^IN2 as the input NDF.',STATUS)

         END IF

      END IF

 9999 CONTINUE


*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL(GRADEV,STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END
