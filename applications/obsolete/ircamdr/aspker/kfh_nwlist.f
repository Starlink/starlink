
*+  KFH_NWLIST - Output image data on to a file.
      SUBROUTINE KFH_NWLIST(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,STATUS)
*    Description :
*     This subroutine writes to a file given by the
*     user the data contained in an image.
*    Invocation :
*     CALL KFH_NWLIST(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,STATUS)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           This array contains the image data.
*     XDIM = INTEGER
*           The X-dimension of the image.
*     YDIM = INTEGER
*           The Y-dimension of the image.
*     X1 = INTEGER
*           The lower X bound of the region of the image.
*     Y1 = INTEGER
*           The lower Y bound of the region of the image.
*     X2 = INTEGER
*           The upper X bound of the region of the image.
*     Y2 = INTEGER
*           The upper Y bound of the region of the image.
*     STATUS = INTEGER
*           The status value on entry.
*    Method :
*     The file name to which the image data is to be
*     written is requested from the user. This name
*     is checked for its validity as a file name (i.e.
*     invalid characters , length , etc.). If the name
*     is OK , then this is used , otherwise a default
*     name of IMAGE.LIS is employed. Also if a valid
*     file name does not have a file type , the routine
*     appends a '.LIS' to it. The image data is then
*     listed.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     20 September 1983
*     25-May-1994 Changed STR$UPCASE to CHR_UCASE (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CHR_ERR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM                       ! The X-dimension of the image.
      INTEGER YDIM                       ! The Y-dimension of the image.
      INTEGER X1                         ! The lower X bound of the
*                                        ! region of the image.
      INTEGER Y1                         ! The lower Y bound of the
*                                        ! region of the image.
      INTEGER X2                         ! The upper X bound of the
*                                        ! region of the image.
      INTEGER Y2                         ! The upper Y bound of the
*                                        ! region of the image.
      REAL IMAGE(XDIM,YDIM)              ! Array holding the image data.
      LOGICAL REPEAT                     ! Flag which simulates a REPEAT..
*                                        ! UNTIL loop using a DO WHILE
*                                        ! loop.
      CHARACTER*20 OUTFIL                ! Variable which holds the user's
*                                        ! file name.
      INTEGER PTR1                       ! Pointer to characters in the
*                                        ! file name.
      INTEGER PTR2                       ! Pointer to characters in the
*                                        ! file name.
      CHARACTER*20 FILE                  ! The validated or default file
*                                        ! name.
      INTEGER FLAG                       ! Flag indicating an incorrect
*                                        ! file name or an exceeded length
*                                        ! in the file name.
      INTEGER SPACE                      ! Variable holding the position
*                                        ! of the first space in the file
*                                        ! name.
      CHARACTER*1 CHARAC                 ! Buffer to contain each character
*                                        ! of the file name in turn.
      REAL FACTOR                        ! Factor by which pixel values
*                                        ! are multiplied before output.
*-

*
*    If the status is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Prompt the user for an output file.
*

         REPEAT = .TRUE.

         DO WHILE (REPEAT)

            CALL PAR_GET0C('FILENAME',OUTFIL,STATUS)
            CALL CHR_UCASE( OUTFIL )

*
*          If the answer is not legal , warn the user
*          and try again.
*

            IF (STATUS.NE.SAI__OK) THEN

               CALL ERR_ANNUL(STATUS)
               CALL PAR_CANCL('FILENAME',STATUS)
               CALL MSG_OUT('MESG1','Enter file name',STATUS)

            ELSEIF (OUTFIL(1:1).EQ.'1'.OR.OUTFIL(1:1).EQ.'2'.OR.
     :       OUTFIL(1:1).EQ.'3'.OR.OUTFIL(1:1).EQ.'4'.OR.
     :       OUTFIL(1:1).EQ.'5'.OR.OUTFIL(1:1).EQ.'6'.OR.
     :       OUTFIL(1:1).EQ.'7'.OR.OUTFIL(1:1).EQ.'8'.OR.
     :       OUTFIL(1:1).EQ.'9'.OR.OUTFIL(1:1).EQ.'0') THEN

                   CALL PAR_CANCL('FILENAME',STATUS)
                   CALL MSG_OUT('MESG2','File name cannot begin'/
     :              /' with a number',STATUS)

*
*          If all is well , then exit the loop.
*

            ELSE

               REPEAT = .FALSE.

            ENDIF

         END DO

         CALL PAR_CANCL('FILENAME',STATUS)

*
*       Check file name for correct length and characters.
*

         IF (INDEX(OUTFIL,'.').EQ.0.AND.INDEX(OUTFIL,' ').GT.10)
     :    THEN

            CALL MSG_OUT('MESG3','File name too long',STATUS)
            FILE = 'IMAGE.LIS'
            CALL MSG_OUT('MESG4','File name set to default value'/
     :       /' of IMAGE.LIS',STATUS)

         ELSE

            PTR1 = 1
            PTR2 = 1
            FILE = ' '
            FLAG = 0

            DO WHILE (FLAG.EQ.0)

               CHARAC = OUTFIL(PTR1:PTR1)

               IF (CHARAC.EQ.'A'.OR.CHARAC.EQ.'B'.OR.CHARAC.EQ.'C'.OR.
     :          CHARAC.EQ.'D'.OR.CHARAC.EQ.'E'.OR.CHARAC.EQ.'F'.OR.
     :          CHARAC.EQ.'G'.OR.CHARAC.EQ.'H'.OR.CHARAC.EQ.'I'.OR.
     :          CHARAC.EQ.'J'.OR.CHARAC.EQ.'K'.OR.CHARAC.EQ.'L'.OR.
     :          CHARAC.EQ.'M'.OR.CHARAC.EQ.'N'.OR.CHARAC.EQ.'O'.OR.
     :          CHARAC.EQ.'P'.OR.CHARAC.EQ.'Q'.OR.CHARAC.EQ.'R'.OR.
     :          CHARAC.EQ.'S'.OR.CHARAC.EQ.'T'.OR.CHARAC.EQ.'U'.OR.
     :          CHARAC.EQ.'V'.OR.CHARAC.EQ.'W'.OR.CHARAC.EQ.'X'.OR.
     :          CHARAC.EQ.'Y'.OR.CHARAC.EQ.'Z'.OR.CHARAC.EQ.'1'.OR.
     :          CHARAC.EQ.'2'.OR.CHARAC.EQ.'3'.OR.CHARAC.EQ.'4'.OR.
     :          CHARAC.EQ.'5'.OR.CHARAC.EQ.'6'.OR.CHARAC.EQ.'7'.OR.
     :          CHARAC.EQ.'8'.OR.CHARAC.EQ.'9'.OR.CHARAC.EQ.'0'.OR.
     :          CHARAC.EQ.'.'.OR.CHARAC.EQ.' ') THEN

                  IF (CHARAC.NE.' ') THEN

                     FILE(PTR1:PTR1) = CHARAC
                     PTR1 = PTR1 + 1

                  ELSEIF (PTR1.NE.1) THEN

                         FLAG = 1

                  ENDIF

                  PTR2 = PTR2 + 1

                  IF (PTR2.GT.10) THEN

                     FLAG = 1

                  ENDIF

               ELSE

                  CALL MSG_SETC('INVAL',CHARAC)
                  CALL MSG_OUT('MESG5','Invalid character "^INVAL"'/
     :             / ' in file name',STATUS)
                  FILE = 'IMAGE.LIS'
                  CALL MSG_OUT('MESG6','File name set to default'/
     :             /' value of IMAGE.LIS',STATUS)
                  FLAG = 1

               ENDIF

            END DO

         ENDIF

*
*       If the file type is omitted , then append the file
*       type to the filename.
*

         IF (INDEX(FILE,'.').EQ.0) THEN

            SPACE = INDEX(FILE,' ')
            FILE(SPACE:) = '.LIS'

         ENDIF

*
*       Output filename.
*

         CALL MSG_SETC('FILE',FILE)
         CALL MSG_OUT('FILNAM','The name of the output file'/
     :    /' is ^FILE',STATUS)

*
*       Prompt the user for the factor.
*

         FACTOR = 1.0

*
*       List the image data.
*

         CALL KFH_NLIST(IMAGE,XDIM,YDIM,X1,Y1,X2,Y2,FILE,FACTOR)

      ENDIF

      END
