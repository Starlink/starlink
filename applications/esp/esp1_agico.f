

      SUBROUTINE ELF1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     ELF1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-NOV-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:
      INTEGER AGIID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE
               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999
            END IF


         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) GOTO 9999

         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Save the current viewport in the AGI database.
        CALL AGP_SVIEW('ELLFOU','Galaxy Profile',AGIID,STATUS)

*      Close down PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE ELP1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     ELP1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-NOV-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:
      INTEGER AGIID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE

               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)

            END IF


         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF

         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Save the current viewport in the AGI database.
         CALL AGP_SVIEW('ELLPRO','Galaxy Profile',AGIID,STATUS)

*      Close down PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE GAU1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     GAU1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-NOV-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:
      INTEGER AGIID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE

               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)

            END IF


         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF

         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Close down PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE GRA1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     GRA1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRA1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-July-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:
      INTEGER AGIID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
      real x1,x2,y1,y2
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE

*            Update the graph display.
               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)

            END IF

         ELSE
*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)
            END IF
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
*         Inquire what the viewport size is.
            CALL PGQVP(1,X1,X2,Y1,Y2)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Save the current viewport in the AGI database.
         CALL AGP_SVIEW('GRAPHS','Galaxy Profile',AGIID,STATUS)

*      Close down PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE HIS1_AGICO(ONOFF,AGIID,STATUS)
*+
*  Name:
*     HIS1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_AGICO(ONOFF,AGIID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1).

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-July-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:
      INTEGER AGIID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enure that the whole screen is used.
         CALL AGI_IBASE(AGIID,STATUS)
         CALL AGI_SELP(AGIID,STATUS)

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)

*      Create a new viewport.
         CALL AGP_NVIEW(.TRUE.,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_REP(' ','Using PGPLOT+AGI has failed.',STATUS)
            GOTO 9999
         END IF

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Save the current viewport in the AGI database.
         CALL AGP_SVIEW('HISTPEAK','Histogram plot',AGIID,STATUS)

*      Close down PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.
            CALL AGI_CANCL('DEVICE',STATUS)

         ELSE

*         Annul the AGI parameter association.
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE SEC1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     SEC1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-July-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:
      INTEGER AGIID                   ! An AGI picture identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS)

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE

*            Update the graph display.
               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS)

            END IF

         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF

         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Save the current viewport in the AGI database.
         CALL AGP_SVIEW('SECTOR','Galaxy Profile',AGIID,STATUS)

*      Close down PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END
