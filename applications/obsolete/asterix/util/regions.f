*+
      SUBROUTINE REGIONS( STATUS )
*  Name:
*     REGIONS

*  Purpose:
*     Writes a spatial description (ARD) file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REGIONS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     REGIONS allows a user to either create or add to a spatial
*     description (ARD) file. The user may specify any of a standard
*     list of shapes, the centre of the shape, its extent and
*     whether it is an INCLUDE or an EXCLUDE region.

*  Authors:
*     Richard Saxton (LTVAD::RDS)

*  History:
*     19-MAR-1992  - Original version.
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*      1 Feb 95 : V1.8-1 Upgrade to new ARD (RJV)
*  Bugs:
*     {note_any_bugs_here}
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local constants :
      INTEGER MAXLST
        PARAMETER (MAXLST = 20)
*  Local Variables:
      CHARACTER*80 TEXT                  ! ARD text
      CHARACTER*132 AFILE                 ! ARD filename
      CHARACTER*20 SHAPE                 ! Shape of structure
      CHARACTER*2 CLP                    ! The loop variable as a character
      CHARACTER*3 XPARAM,YPARAM          ! Parameter variables

      INTEGER ARDID			 ! GRP id for ARD text
      INTEGER AUNIT                      ! Logical unit of ARD file
      INTEGER OUNIT                      ! Logical unit of old ARD file
      INTEGER NLIST                      ! No. of arguments for a shape.
      INTEGER LP,NCHAR,NPTS,L            !

      LOGICAL JUMPOUT                    ! Leave big loop ?
      LOGICAL LEAVE                      ! Leave loop ?
      LOGICAL EXCLUDE                    ! Enter this in the file as an
*                                        ! exclude region ?
      LOGICAL MORE                       ! Write another structure ?
      LOGICAL LNEW                       ! Create a new file ?

      REAL XCENT,YCENT                   ! Centre of a shape
      REAL IRAD                          ! Inner radius of an annulus
      REAL ORAD                          ! Outer radius of an annulus
      REAL PHI                           ! Orientation of an ellipse, anti-
*                                        ! clkwise from the X axis.
      REAL SMINAX, SMAJAX                ! Semi-minor and major axes of ellipse
      REAL XWIDTH,YWIDTH                 ! Width of each side in a box
      REAL XEND1,XEND2,YEND1,YEND2       ! Ends of the line
      REAL LIST(MAXLST)                  ! Arguments for each command
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start ASTERIX
      CALL AST_INIT()

*  get id for ARD text
      CALL ARX_OPEN('WRITE',ARDID,STATUS)

*  Enquire whether a new ARD file is to be created
      CALL USI_GET0L('NEW', LNEW, STATUS)

*  Get filename
      CALL USI_GET0C('ARDFILE',AFILE,STATUS)

*  Open the ARD file as required
      IF (LNEW) THEN
         CALL FIO_OPEN(AFILE, 'WRITE', 'LIST' , 0, AUNIT, STATUS)
      ELSE
         CALL FIO_OPEN(AFILE, 'APPEND', 'LIST' , 0, AUNIT, STATUS)
      ENDIF

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','Error opening spatial description '/
     &               /'file', STATUS)
         GOTO 999
      ENDIF

*  Loop until the user exits
      JUMPOUT = .FALSE.
      DO WHILE (.NOT. JUMPOUT)

*    Enquire whether this is an INCLUDE or EXCLUDE region
         CALL USI_GET0L('EXCLUDE', EXCLUDE, STATUS)
         CALL USI_CANCL('EXCLUDE', STATUS)

         IF (STATUS .NE. SAI__OK) GOTO 999

*    Get shape.
         CALL USI_GET0C('SHAPE', SHAPE, STATUS)
         CALL USI_CANCL('SHAPE', STATUS)

         IF (STATUS .NE. SAI__OK) GOTO 999

*    Convert shape to uppercase
         CALL CHR_UCASE(SHAPE)

*    Get parameters which go with this shape
*     BOX:
         IF (INDEX(SHAPE, 'BOX') .NE. 0) THEN

            NLIST = 4

*       Get coordinates of box centre
            CALL USI_GET0R('XCENT', LIST(1), STATUS)
            CALL USI_GET0R('YCENT', LIST(2), STATUS)
            CALL USI_CANCL('XCENT', STATUS)
            CALL USI_CANCL('YCENT', STATUS)

*       Get width of box
            CALL USI_GET0R('XWIDTH', LIST(3), STATUS)
            CALL USI_GET0R('YWIDTH', LIST(4), STATUS)
            CALL USI_CANCL('XWIDTH', STATUS)
            CALL USI_CANCL('YWIDTH', STATUS)

            IF (STATUS .NE. SAI__OK) GOTO 999

*       add description into ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'BOX',NLIST,LIST,STATUS)

*    CIRCLE:
         ELSEIF (INDEX(SHAPE, 'CIRCLE') .NE. 0) THEN

            NLIST = 3

*       Get coordinates of circle centre
            CALL USI_GET0R('XCENT', LIST(1), STATUS)
            CALL USI_GET0R('YCENT', LIST(2), STATUS)
            CALL USI_CANCL('XCENT', STATUS)
            CALL USI_CANCL('YCENT', STATUS)

*       Get radius of circle
            CALL USI_GET0R('RADIUS', LIST(3), STATUS)
            CALL USI_CANCL('RADIUS', STATUS)

            IF (STATUS .NE. SAI__OK) GOTO 999

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'CIRCLE',NLIST,LIST,STATUS)

*    ELLIPSE:
         ELSEIF (INDEX(SHAPE, 'ELLIPSE') .NE. 0) THEN

            NLIST = 5

*       Get coordinates of ellipse centre
            CALL USI_GET0R('XCENT', LIST(1), STATUS)
            CALL USI_GET0R('YCENT', LIST(2), STATUS)
            CALL USI_CANCL('XCENT', STATUS)
            CALL USI_CANCL('YCENT', STATUS)

*       Get semi-minor and semi-major axes
            CALL USI_GET0R('SMAJOR_AXIS', LIST(3), STATUS)
            CALL USI_GET0R('SMINOR_AXIS', LIST(4), STATUS)
            CALL USI_CANCL('SMAJOR_AXIS', STATUS)
            CALL USI_CANCL('SMINOR_AXIS', STATUS)

*       Get orientation of ellipse
            CALL USI_GET0R('ORIENT', LIST(5), STATUS)
            CALL USI_CANCL('ORIENT', STATUS)

            IF (STATUS .NE. SAI__OK) GOTO 999

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'ELLIPSE',NLIST,LIST,
     :                                                     STATUS)

*    POLYGON:
         ELSEIF (INDEX(SHAPE, 'POLYGON') .NE. 0) THEN

*       Get number of points
            LEAVE = .FALSE.
            DO WHILE (.NOT. LEAVE)

               CALL USI_GET0I('NPTS', NPTS, STATUS)
               CALL USI_CANCL('NPTS', STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

*         Test if the number of pixels is within range
               IF (NPTS .LT. 1 .OR. NPTS .GT. MAXLST/2.) THEN
                  CALL MSG_SETI('MAX', MAXLST)
                  CALL MSG_OUT(' ','** Number out of range - must be '/
     &                         /'between 1 and ^MAX **', STATUS)
               ELSE
                  LEAVE = .TRUE.
               ENDIF

            ENDDO

*       Get position of each point
            DO LP=1,NPTS

*          Create parameters for each point
               CALL CHR_ITOC(LP, CLP, NCHAR)

               XPARAM = 'X' // CLP(1:NCHAR)
               YPARAM = 'Y' // CLP(1:NCHAR)

*          ask user for each point on the polygon
               CALL USI_GET0R(XPARAM, LIST(1+(LP-1)*2), STATUS)
               CALL USI_GET0R(YPARAM, LIST(LP*2), STATUS)
               CALL USI_CANCL(XPARAM, STATUS)
               CALL USI_CANCL(YPARAM, STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

            ENDDO

*       Set the number of trailing values in the command variable
            NLIST = NPTS * 2

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'POLYGON',NLIST,LIST,
     :                                                     STATUS)

*    COLUMN:
         ELSEIF (INDEX(SHAPE, 'COLUMN') .NE. 0) THEN

*       Get number of columns
            LEAVE = .FALSE.
            DO WHILE (.NOT. LEAVE)

               CALL USI_GET0I('NCOL', NLIST, STATUS)
               CALL USI_CANCL('NCOL', STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

*         Test if the number of pixels is within range
               IF (NLIST .LT. 1 .OR. NLIST .GT. MAXLST/2.) THEN
                  CALL MSG_SETI('MAX', MAXLST)
                  CALL MSG_OUT(' ','** Number out of range - must be '/
     &                         /'between 1 and ^MAX **', STATUS)
               ELSE
                  LEAVE = .TRUE.
               ENDIF

            ENDDO

*       Get X position of each column
            DO LP=1,NLIST

*          Create parameters for each column
               CALL CHR_ITOC(LP, CLP, NCHAR)

               XPARAM = 'X' // CLP(1:NCHAR)

*          ask user for each column point
               CALL USI_GET0R(XPARAM, LIST(LP), STATUS)
               CALL USI_CANCL(XPARAM, STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

            ENDDO

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'COLUMN',NLIST,LIST,
     :                                                    STATUS)

*    ROW:
         ELSEIF (INDEX(SHAPE, 'ROW') .NE. 0) THEN

*       Get number of rows
            LEAVE = .FALSE.
            DO WHILE (.NOT. LEAVE)

               CALL USI_GET0I('NROW', NLIST, STATUS)
               CALL USI_CANCL('NROW', STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

*         Test if the number of pixels is within range
               IF (NLIST .LT. 1 .OR. NLIST .GT. MAXLST/2.) THEN
                  CALL MSG_SETI('MAX', MAXLST)
                  CALL MSG_OUT(' ','** Number out of range - must be '/
     &                         /'between 1 and ^MAX **', STATUS)
               ELSE
                  LEAVE = .TRUE.
               ENDIF

            ENDDO


*       Get Y position of each row
            DO LP=1,NLIST

*          Create parameters for each column
               CALL CHR_ITOC(LP, CLP, NCHAR)

               YPARAM = 'Y' // CLP(1:NCHAR)

*          ask user for each row point
               CALL USI_GET0R(YPARAM, LIST(LP), STATUS)
               CALL USI_CANCL(YPARAM, STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

            ENDDO

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'ROW',NLIST,LIST,STATUS)

*    LINE:
         ELSEIF (INDEX(SHAPE, 'LINE') .NE. 0) THEN

            NLIST = 4

*       Get coordinates of one end of the line
            CALL USI_GET0R('XEND1', LIST(1), STATUS)
            CALL USI_GET0R('YEND1', LIST(2), STATUS)
            CALL USI_GET0R('XEND2', LIST(3), STATUS)
            CALL USI_GET0R('YEND2', LIST(4), STATUS)
            CALL USI_CANCL('XEND1', STATUS)
            CALL USI_CANCL('XEND2', STATUS)
            CALL USI_CANCL('YEND1', STATUS)
            CALL USI_CANCL('YEND2', STATUS)

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'LINE',NLIST,LIST,STATUS)

*    PIXELS:
         ELSEIF (INDEX(SHAPE, 'PIXEL') .NE. 0) THEN

*       Get number of pixels
            LEAVE = .FALSE.
            DO WHILE (.NOT. LEAVE)

               CALL USI_GET0I('NPIX', NPTS, STATUS)
               CALL USI_CANCL('NPIX', STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

*         Test if the number of pixels is within range
               IF (NPTS .LT. 1 .OR. NPTS .GT. MAXLST/2.) THEN
                  CALL MSG_SETI('MAX', MAXLST)
                  CALL MSG_OUT(' ','** Number out of range - must be '/
     &                         /'between 1 and ^MAX **', STATUS)
               ELSE
                  LEAVE = .TRUE.
               ENDIF

            ENDDO

*       Get X,Y position of each pixel
            DO LP=1,NPTS

*          Create parameters for each column
               CALL CHR_ITOC(LP, CLP, NCHAR)

               XPARAM = 'X' // CLP(1:NCHAR)
               YPARAM = 'Y' // CLP(1:NCHAR)

*          ask user for each pixel position
               CALL USI_GET0R(XPARAM, LIST(1+(LP-1)*2), STATUS)
               CALL USI_GET0R(YPARAM, LIST(LP*2), STATUS)
               CALL USI_CANCL(XPARAM, STATUS)
               CALL USI_CANCL(YPARAM, STATUS)

               IF (STATUS .NE. SAI__OK) GOTO 999

            ENDDO

            NLIST = NPTS * 2

*       add description to ARD text
            CALL ARX_GENPUT(ARDID,0,EXCLUDE,'PIXEL',NLIST,LIST,STATUS)

*    ANNULUS:
         ELSEIF (INDEX(SHAPE, 'ANNULUS') .NE. 0) THEN

*       Get coordinates of annulus centre
            CALL USI_GET0R('XCENT', XCENT, STATUS)
            CALL USI_GET0R('YCENT', YCENT, STATUS)
            CALL USI_CANCL('XCENT', STATUS)
            CALL USI_CANCL('YCENT', STATUS)

*       Get inner radius of annulus
            CALL USI_GET0R('INNRAD', IRAD, STATUS)
            CALL USI_GET0R('OUTRAD', ORAD, STATUS)
            CALL USI_CANCL('INNRAD', STATUS)
            CALL USI_CANCL('OUTRAD', STATUS)

            IF (STATUS .NE. SAI__OK) GOTO 999

*       add description to ARD text
            IF (EXCLUDE) THEN
              TEXT=' .NOT. (CIRCLE( '
            ELSE
              TEXT=' (CIRCLE( '
            ENDIF
            L=CHR_LEN(TEXT)

            CALL MSG_SETR('XC',XCENT)
            CALL MSG_SETR('YC',YCENT)
            CALL MSG_SETR('RAD',ORAD)
            CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD )',TEXT,L)

            CALL ARX_PUT(ARDID,0,TEXT(:L),STATUS)


            TEXT='     .AND..NOT.CIRCLE('
            L=CHR_LEN(TEXT)

            CALL MSG_SETR('XC',XCENT)
            CALL MSG_SETR('YC',YCENT)
            CALL MSG_SETR('RAD',IRAD)
            CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD ))',TEXT,L)
            IF (EXCLUDE) THEN
              L=L+1
              TEXT(L:)=')'
            ENDIF

            CALL ARX_PUT(ARDID,0,TEXT(:L),STATUS)



         ELSE

*   Shape unrecognised inform the user
            CALL MSG_SETC('SHAPE', SHAPE)
            CALL MSG_OUT(' ','Shape : ^SHAPE unrecognised - no '/
     &                /'addition has been made to the ARD file', STATUS)

         ENDIF

*   Ask if another entry is to be added
         CALL USI_GET0L('MORE', MORE, STATUS)
         CALL USI_CANCL('MORE', STATUS)

         IF (STATUS .NE. SAI__OK) GOTO 999

*   If no more then leave the loop
         IF (.NOT. MORE) JUMPOUT = .TRUE.

      ENDDO

*   Write out text and close the ARD file
      CALL ARX_WRITEF(ARDID,AUNIT,STATUS)
      CALL ARX_CLOSE(ARDID,STATUS)
      CALL FIO_CLOSE(AUNIT, STATUS)

999   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
