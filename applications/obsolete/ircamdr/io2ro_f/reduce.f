*+  REDUCE - Actual conversion routine
      SUBROUTINE REDUCE( STATUS )
*    Description:
*     This calls HDS to convert the data from I+O to RO files
*     In UNIX the case of the filenames might be a problem.  The
*     routine starts by assuming a name like:
*       directory name(from parameter system)/
*       o(or i or ro)date_obsno
*       .sdf
*     If the file isn't found then it puts everything but the '.sdf'
*     in upper case and tries again.  If it still isn't found then
*     the '.sdf' is also put into upper case.  If the data is chopped,
*     the output RO files are created with a lower case 'a(or b).sdf'
*     independent of the case of the rest of the name.
*    Invocation :
*     CALL REDUCE( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*     MJC: Malcolm J. Currie
*    History :
*     18-May-1994: Original version                                (PND)
*     01-Aug-1994: Added error catches, more comments, changed copying from
*                  VMS specific IO_COPYFILE to HDS_COPY, filename now passed
*                  as parameter, reduced number of times O file opened (SKL)
*     23-Sept-1994 Added case sensitivity for UNIX                (SKL@JACH)
*     1999 Sept 29: Writes .ok file to RODIR, invoking revised RONAME
*                   subroutine. (MJC)

*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'              ! Defines SAI__OK etc
      INCLUDE 'DAT_PAR'              ! Necessary for non-VMS
      INCLUDE 'CHR_ERR'
*    Status :
      INTEGER STATUS                 ! Inherited global ADAM status
*    Import-Export :
*    External functions :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER
     :  CLEN,                        ! Length of string
     :  CASE,                        ! Flag for upper/lower case chop
                                     ! filename
     :  FD,                          ! Fortran file descriptor
     :  L1,
     :  LSTAT                        ! Local status
      CHARACTER*80
     :  DATE_OBS,                    ! UT date and obs number string for
                                     ! name
     :  NAME,                        ! Object name
     :  ANAME,                       ! Object name
     :  BNAME,                       ! Object name
     :  INAME,                       ! Integration name
     :  ONAME,                       ! Observation name
     :  RNAME,                       ! Reduced observation name
     :  RODIR                        ! RO directory name
      CHARACTER * ( 5 ) OBSNUM       ! Observation number
      CHARACTER*(DAT__SZLOC)
     :  ILOC,                        ! Locator to top-level of I-file
     :  ILOCA,                       ! Locator to top-level of I-file, A chop
     :  ILOCB,                       ! Locator to top-level of I-file  B chop
     :  RLOC,                        ! Locator to top-level of RO-file
     :  OLOC,                        ! Locator to top-level of O-file
     :  MRLOC,                       ! Locator to MORE level of RO-file
     :  MOLOC,                       ! Locator to MORE level of O-file
     :  MFRLOC,                      ! Locator to MORE.FITS level of RO-file
     :  MFOLOC                       ! Locator to MORE.FITS level of O-file
      LOGICAL
     :  O_OK,                        ! T observation file exits
     :  INT_OK,                      ! T if integration exists
     :  CHOP_MODE                    ! T if chop beams A and B exist
*-

*   Return if status on entry is not SAI__OK
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the observation name in three formats

      CALL PAR_GET0C( 'DATE_OBS', DATE_OBS, STATUS )

      CALL RONAME( DATE_OBS, INAME, ONAME, RNAME, RODIR, OBSNUM,
     :             STATUS )

      IF (STATUS .EQ. SAI__OK) THEN

        CALL MSG_SETC( 'RNAME', RNAME )
        CALL MSG_OUT( ' ',
     :             'Attempting to create reduced observation ^RNAME',
     :                STATUS )

        CHOP_MODE = .FALSE.
        INT_OK    = .FALSE.
        O_OK    = .FALSE.

*       see if O file exists

	L1 = CHR_LEN(ONAME)
	PRINT *, 'search name = ', oname(1:l1)//'.sdf'
        INQUIRE( FILE=ONAME(1:L1)//'.sdf', EXIST=O_OK )

*     If file isn't found allow for possibility of upper case or
*     mixed case filenames

        IF ( .NOT. O_OK ) THEN

*          CALL CHR_UCASE( ONAME )
	  L1 = CHR_LEN(ONAME)
          INQUIRE( FILE=ONAME(1:L1)//'.sdf', EXIST=O_OK )

          IF ( .NOT. O_OK ) THEN
	    L1 = CHR_LEN(ONAME)
            INQUIRE( FILE=ONAME(1:L1)//'.SDF', EXIST=O_OK )
          END IF

        END IF

        IF ( O_OK ) THEN

*       get locators to O file and its FITS array

          CALL HDS_OPEN( ONAME, 'READ', OLOC, STATUS )
          CALL DAT_FIND( OLOC, 'MORE', MOLOC, STATUS )
          CALL DAT_FIND( MOLOC, 'FITS', MFOLOC, STATUS )

          IF (STATUS .EQ. SAI__OK) THEN

*         see if I file exits

	    L1 = CHR_LEN(INAME)
            INQUIRE( FILE=INAME(1:L1)//'.sdf', EXIST=INT_OK )

*         If file isn't found allow for possibility of upper case or
*         mixed case filenames

            IF ( .NOT. INT_OK ) THEN

*              CALL CHR_UCASE( INAME )
	      L1 = CHR_LEN(INAME)
              INQUIRE( FILE=INAME(1:L1)//'.sdf', EXIST=INT_OK )

              IF ( .NOT. INT_OK ) THEN
	        L1 = CHR_LEN(INAME)
                INQUIRE( FILE=INAME(1:L1)//'.SDF', EXIST=INT_OK )
              END IF

            END IF

            IF ( INT_OK ) THEN

*           copy I file to RO file

              CALL HDS_OPEN( INAME, 'READ', ILOC, STATUS )
              CALL DAT_NAME( ILOC, NAME, STATUS )
              CALL HDS_COPY( ILOC, RNAME//'.sdf', NAME, STATUS )
              CALL DAT_ANNUL( ILOC, STATUS )

              IF (STATUS .NE. SAI__OK) THEN
                CALL ERR_REP('ERR',
     :          'Error copying I into RO file', STATUS )
                CALL DAT_ANNUL( OLOC, STATUS )
                RETURN
              END IF

            ELSE

	      L1 = CHR_LEN(INAME)
              INQUIRE( FILE=INAME(1:L1)//'a.sdf', EXIST=INT_OK )

*            try to allow for upper/lower case varieties

              IF ( INT_OK ) THEN
                CASE = 1
              ELSE
	        L1 = CHR_LEN(INAME)
                INQUIRE( FILE=INAME(1:L1)//'A.sdf', EXIST=INT_OK )
                IF ( INT_OK ) THEN
                  CASE = 2
                ELSE
	          L1 = CHR_LEN(INAME)
                  INQUIRE( FILE=INAME(1:L1)//'A.SDF', EXIST=INT_OK )
                  IF ( INT_OK ) THEN
                    CASE = 3
                  END IF
                END IF
              END IF

              IF ( INT_OK ) THEN

                CHOP_MODE = .TRUE.

*               copy I file to RO file for both  A and B beams

                IF ( CASE .EQ. 1 ) THEN
                  CALL HDS_OPEN( INAME//'a.sdf', 'READ', ILOCA,
     :                           STATUS )
                  CALL HDS_OPEN( INAME//'b.sdf', 'READ', ILOCB,
     :                           STATUS )
                ELSE IF ( CASE .EQ. 2 ) THEN
                  CALL HDS_OPEN( INAME//'A.sdf', 'READ', ILOCA,
     :                           STATUS )
                  CALL HDS_OPEN( INAME//'B.sdf', 'READ', ILOCB,
     :                           STATUS )
                ELSE IF ( CASE .EQ. 3 ) THEN
                  CALL HDS_OPEN( INAME//'A.SDF', 'READ', ILOCA,
     :                           STATUS )
                  CALL HDS_OPEN( INAME//'B.SDF', 'READ', ILOCB,
     :                           STATUS )
                END IF

                CALL DAT_NAME( ILOCA, ANAME, STATUS )
                CALL DAT_NAME( ILOCB, BNAME, STATUS )

*              The RO files will be given a lower case 'a' and 'b' '.sdf'

                CALL HDS_COPY( ILOCA, RNAME//'a.sdf', ANAME, STATUS )
                CALL HDS_COPY( ILOCB, RNAME//'b.sdf', BNAME, STATUS )

                CALL DAT_ANNUL( ILOCA, STATUS )
                CALL DAT_ANNUL( ILOCB, STATUS )

                IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_REP('ERR',
     :            'Error copying I into RO files', STATUS )
                  CALL DAT_ANNUL( OLOC, STATUS )
                  RETURN
                END IF

              ELSE
                CALL MSG_SETC( 'INAME', INAME )
                CALL MSG_OUT('ERR',
     :                       'Unable to find integration file ^INAME',
     :                        STATUS )
                CALL DAT_ANNUL( OLOC, STATUS )
                RETURN
              END IF
            END IF
          ELSE
            CALL MSG_SETC( 'ONAME', ONAME )
            CALL ERR_REP('ERR',
     :          'Error getting FITS info from observation file ^ONAME',
     :                    STATUS )
            CALL DAT_ANNUL( OLOC, STATUS )
            RETURN
          END IF
        ELSE
          CALL MSG_SETC( 'ONAME', ONAME )
          CALL MSG_OUT('ERR', 'Unable to find observation file ^ONAME',
     :                  STATUS )
          RETURN
        END IF
      ELSE
        CALL ERR_REP('ERR', 'Error getting file names', STATUS )
        RETURN
      END IF


*   Complete conversion of RO file by moving the I file FITS array to
*   an INT_FITS extension, and copy O file FITS to the RO FITS extension

*   If in chop mode, deal with both beams else straight conversion

      IF ( CHOP_MODE ) THEN

*     Do beam A

        CALL HDS_OPEN( RNAME//'a', 'UPDATE', RLOC, STATUS )
        CALL DAT_FIND( RLOC, 'MORE', MRLOC, STATUS )
        CALL DAT_FIND( MRLOC, 'FITS', MFRLOC, STATUS )
        CALL DAT_MOVE( MFRLOC, MRLOC, 'INT_FITS', STATUS )
        CALL DAT_COPY( MFOLOC, MRLOC, 'FITS', STATUS )

        CALL DAT_ANNUL( RLOC, STATUS )

        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CLEN = CHR_LEN( ONAME )
          CALL MSG_SETC( 'ONAME', ONAME(1:CLEN) )
          CLEN = CHR_LEN( RNAME )
          CALL MSG_SETC( 'RNAME', RNAME(1:CLEN)//'a' )
          CALL MSG_OUT( ' ',
     :       '*** FAILED to copy FITS info from ^ONAME to ^RNAME',
     :                  STATUS )
        ELSE
          CLEN = CHR_LEN( INAME )
          CALL MSG_SETC( 'INAME', INAME(1:CLEN)//'a' )
          CLEN = CHR_LEN( RNAME )
          CALL MSG_SETC( 'RNAME', RNAME(1:CLEN)//'a' )
          CALL MSG_OUT( ' ', 'Converted ^INAME to ^RNAME OK', STATUS )
        END IF

*     Do beam B

        CALL HDS_OPEN( RNAME//'b', 'UPDATE', RLOC, STATUS )
        CALL DAT_FIND( RLOC, 'MORE', MRLOC, STATUS )
        CALL DAT_FIND( MRLOC, 'FITS', MFRLOC, STATUS )
        CALL DAT_MOVE( MFRLOC, MRLOC, 'INT_FITS', STATUS )
        CALL DAT_COPY( MFOLOC, MRLOC, 'FITS', STATUS )

        CALL DAT_ANNUL( RLOC, STATUS )
        CALL DAT_ANNUL( OLOC, STATUS )

        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CLEN = CHR_LEN( ONAME )
          CALL MSG_SETC( 'ONAME', ONAME(1:CLEN) )
          CLEN = CHR_LEN( RNAME )
          CALL MSG_SETC( 'RNAME', RNAME(1:CLEN)//'b' )
          CALL MSG_OUT( ' ',
     :      '*** FAILED to copy FITS info from ^ONAME to ^RNAME',
     :      STATUS )
        ELSE
          CLEN = CHR_LEN( INAME )
          CALL MSG_SETC( 'INAME', INAME(1:CLEN)//'b' )
          CLEN = CHR_LEN( RNAME )
          CALL MSG_SETC( 'RNAME', RNAME(1:CLEN)//'b' )
          CALL MSG_OUT( ' ', 'Converted ^INAME to ^RNAME OK', STATUS )
        END IF

      ELSE

        CALL HDS_OPEN( RNAME, 'UPDATE', RLOC, STATUS )
        CALL DAT_FIND( RLOC, 'MORE', MRLOC, STATUS )
        CALL DAT_FIND( MRLOC, 'FITS', MFRLOC, STATUS )
        CALL DAT_MOVE( MFRLOC, MRLOC, 'INT_FITS', STATUS )
        CALL DAT_COPY( MFOLOC, MRLOC, 'FITS', STATUS )

        CALL DAT_ANNUL( RLOC, STATUS )
        CALL DAT_ANNUL( OLOC, STATUS )

        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL MSG_SETC( 'ONAME', ONAME )
          CALL MSG_SETC( 'RNAME', RNAME )
          CALL MSG_OUT( ' ',
     :      '*** FAILED to copy FITS info from ^ONAME to ^RNAME',
     :      STATUS )
        ELSE
          CALL MSG_SETC( 'INAME', INAME )
          CALL MSG_SETC( 'RNAME', RNAME )
          CALL MSG_OUT( ' ', 'Converted ^INAME to ^RNAME OK', STATUS )
        END IF
      END IF

*   Write the ok file.
      IF ( STATUS .EQ. SAI__OK ) THEN
         LSTAT = SAI__OK
         CALL FIO_OPEN( RODIR( : CHR_LEN( RODIR ) ) // '.' //
     :                  DATE_OBS( : CHR_LEN( DATE_OBS ) ) // '_ok',
     :                  'WRITE', 'NONE', 0, FD, LSTAT )
         CALL FIO_CLOSE( FD, LSTAT )
      END IF

*   Exit this subroutine
      END
