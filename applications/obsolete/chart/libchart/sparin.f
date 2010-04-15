       SUBROUTINE SPARIN( FILENAME, STATUS )
*+
*  Name:
*     SPARIN

*  Purpose:
*     Read the parameter file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPARIN( FILENAME, STATUS )

*  Description:
*     This subroutine reads the parameter file created by the xSET
*     programs and stores the results in the common block used by the
*     existing CHART program.

*  Arguments:
*     FILENAME = CHARACTER * ( * ) (Returned)
*        The name of the file containing the field centres
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     {facility_or_package}...

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1983 (PMA):
*        Original version.
*     10-DEC-1991 (PMA):
*        Changed calls to CTOI to CHR_CTOI
*        Changed calls to CTOR to CHR_CTOR
*     11-DEC-1991 (PMA):
*        Converted to an ADAM-style subroutine
*        Declared previously undeclared variables
*        Changed declaration of COMMON block to INCLUDE statement
*     18-FEB-1993 (AJJB):
*        STATUS argument added, COMMON blocks put in INCLUDE files, and
*        WRUSER call changed to MSG_OUT.
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statement, with a call to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the READONLY specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     23-APR-1993 (AJJB):
*        Removed the adding of '.DAT' to filenames.
*     2-JUN-1993 (AJJB):
*        Replaced IFAIL error reporting system with STATUS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'             ! Main CHART common block

*  Global Variables:
      INCLUDE 'CONVF'            ! Conversion factors
*        TWOPI = DOUBLE PRECISION (Write)
*           Two times pi
*        HALFPI = DOUBLE PRECISION (Write)
*           Half of pi
*        RDSA = DOUBLE PRECISION (Write)
*           Radians per second of arc
*        RDST = DOUBLE PRECISION (Write)
*           Radians per second of time
*        RDDG = DOUBLE PRECISION (Write)
*           Radians per degree

*  Arguments Given & Returned:
      INTEGER STATUS

*  Arguments Returned:
      CHARACTER * ( * ) FILENAME

*  External References:
      EXTERNAL TRULEN
      INTEGER TRULEN             ! [external_description]

*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! Parameters
      CHARACTER * ( 50 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 50 ) VAL2    ! [local_variable_description]
      CHARACTER * ( 50 ) TEXT    ! [local_variable_description]
      INTEGER NPAR
      INTEGER NPOS
      INTEGER I
      INTEGER N
      INTEGER L
      REAL EQ
      REAL WIDTH

*.
*  Status check:
      IF ( STATUS .NE. SAI__OK ) RETURN

C
C   Read all the current parameters into the array PARAMS;
C   NPAR parameters were found.
C
      CALL GETPARAMS(PARAMS,NPAR, STATUS)
C
C   First get the run label.
C
      CALL GETDEFLT(PARAMS,NPAR,'LABEL',VALUE,NPOS, STATUS )
C
C   If by some error no value is found, then set it to spaces.
C
      IF (NPOS.EQ.0) THEN
         VALUE=' '
      END IF
C
C   Read the characters into the integer array used by the other
C   subroutines (because FINGS is used?)
C
      CALL CHR_UCASE(VALUE)
      READ (VALUE,'(10A)') IDCHAR
C
C   then get the search area.
C
      CALL GETDEFLT(PARAMS,NPAR,'SAREA',VALUE,NPOS, STATUS )
C
C   If by some error there is no value, then put in the default.
C
      IF (NPOS.GT.0) THEN
         CALL CHR_CTOR(VALUE,WIDTH,STATUS)
      ELSE
         WIDTH=2.0
      END IF
      SIZE=WIDTH/2.0
C
C   Now sort out the catalogues.
C
C   First the mode.
C
      CALL GETDEFLT(PARAMS,NPAR,'MODE',VALUE,NPOS, STATUS )
C
C   In this version the fall back position is that the CSI mode
C   will be selected in case of any error.
C
      IF (NPOS.GT.0) THEN
         IF (VALUE(1:1).EQ.'A'.OR.VALUE(1:1).EQ.'P') THEN
            CATRUN=.TRUE.
            NONS=.FALSE.
         ELSE IF (VALUE(1:1).EQ.'N') THEN
            NONS=.TRUE.
            CATRUN=.FALSE.
         ELSE
            CATRUN=.FALSE.
            NONS=.FALSE.
         END IF
      ELSE
         CATRUN=.FALSE.
         NONS=.FALSE.
      END IF
C
C   Now the epoch and equinox.
C
      CALL GETDEFLT(PARAMS,NPAR,'EPOCH',VALUE,NPOS, STATUS )
      IF (NPOS.GT.0) THEN
         CALL CHR_CTOR(VALUE,EPOCH, STATUS)
      ELSE
         EPOCH=1950.0
      END IF
      CALL GETDEFLT(PARAMS,NPAR,'EQUINOX',VALUE,NPOS, STATUS )
      IF (NPOS.GT.0) THEN
         CALL CHR_CTOR(VALUE,EQUOUT, STATUS)
      ELSE
         EQUOUT=1950.0
      END IF
C
C   Now handle any selection of catalogues within the Astrometric mode
C
      DO I = 0,10
          ICAT1(I)=-1
      ENDDO
      IF (CATRUN) THEN
         CALL GETDEFLT(PARAMS,NPAR,'MODE',VALUE,NPOS, STATUS )
C
C    Test if private catalogue to be used
C
         IF(VALUE(1:1).EQ.'P') THEN
             ICAT1(0) = 0
         ELSE
             CALL GETDEFLT(PARAMS,NPAR,'CATALOGUES',VALUE,NPOS, STATUS )
C
C      If no entry is found for CATALOGUES, assume all three catalogues
C      are to be searched. This section needs to be changed when new
C      catalogues are added. J.V.Carey 1984 April 11
C
             IF (NPOS.GT.0) THEN
                IF (INDEX(VALUE,'A').NE.0) THEN
                    ICAT1(1)=1
                    ICAT1(2)=0
                ENDIF
                IF (INDEX(VALUE,'S').NE.0) ICAT1(2)=2
                IF (INDEX(VALUE,'P').NE.0) ICAT1(3)=3
                IF (INDEX(VALUE,'ALL').NE.0) THEN
                    ICAT1(1)=1
                    ICAT1(2)=2
                    ICAT1(3)=3
                END IF
             ELSE
                ICAT1(1)=1
                ICAT1(2)=2
                ICAT1(3)=3
             END IF
         ENDIF
      END IF
C
C   Now handle other selection criteria.
C
C   (Only valid if CSI or ASTR mode)
C
C   (but first set up fall back values)
C
      FAINT=26.0
      MAXNUM=2000
      CHOOSE=.FALSE.
      IDENTS=.TRUE.
      IF (.NOT.NONS) THEN
C
C      Find the selection criteria.
C
         CALL GETDEFLT(PARAMS,NPAR,'SELECTION',VALUE,NPOS, STATUS )
         IF (NPOS.EQ.0) THEN
C
C         If by error none is found then set no selection, in effect.
C
            FAINT=26.0
            MAXNUM=2000
            CHOOSE=.FALSE.
         ELSE
C
C         Otherwise look at the criteria in turn - first by NUMBER
C
            IF (VALUE(1:1).EQ.'N') THEN
               CALL GETDEFLT(PARAMS,NPAR,'NUMBER',VAL2,NPOS, STATUS )
               CALL CHR_CTOI(VAL2,MAXNUM, STATUS)
            ELSE IF (VALUE(1:1).EQ.'M') THEN
C
C            Then by MAGNITUDE
C
               CALL GETDEFLT(PARAMS,NPAR,'MAGNITUDE',VAL2,NPOS, STATUS )
               CALL CHR_CTOR(VAL2,FAINT, STATUS)
            ELSE IF (VALUE(1:1).EQ.'C'.AND..NOT.CATRUN) THEN
C
C            and finally (if in CSI mode) by CATALOGUE
C
               CALL GETDEFLT(PARAMS,NPAR,'CATALOGUES',VAL2,NPOS, STATUS
     :         )
               CALL CATINF(VAL2, STATUS)
C
C            If it fails to make sense of the input, re-set
C            to no selection by catalogue.
C
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CHOOSE=.TRUE.
               ELSE
                  CHOOSE=.FALSE.
                  CALL ERR_FLUSH( STATUS )
               END IF
            END IF
         END IF
      END IF
C
C   This completes the parameter entry - now get the field centres
C
C   First decide where they are to come from.
C   BATCH is TRUE if from a file, called FILENAME
C   BATCH is FALSE if from the terminal.
C
      CALL GETDEFLT(PARAMS,NPAR,'FIELDS',VALUE,NPOS, STATUS )
      IF (NPOS.EQ.0) THEN
C
C      If there was an error then assume the terminal.
C
         BATCH=.FALSE.
      ELSE
         IF (VALUE(1:4).EQ.'TERM') THEN
            BATCH=.FALSE.
         ELSE
            BATCH=.TRUE.
            L=TRULEN(VALUE)
            FILENAME=VALUE(1:L)
         END IF
      END IF
C
C   The scale factor (really used for plotting) is nevertheless
C   needed at this stage so that positions on the plate in mm.
C   may be printed.
C
      CALL GETDEFLT(PARAMS,NPAR,'SCALE',VALUE,NPOS, STATUS )
      IF (NPOS.EQ.0) THEN
         SCALE=0.0
      ELSE
         CALL CHR_CTOR(VALUE,SCALE, STATUS)
         IF ( STATUS.NE.0) SCALE=0.0
      END IF
C
C   Finally handle the question of extra (or supplemetary) objects.
C
      CALL GETDEFLT(PARAMS,NPAR,'EXTRA',VALUE,NPOS, STATUS )
      IF (NPOS.EQ.0.OR.INDEX(VALUE,'NONE').NE.0) THEN
         SUPP=.FALSE.
      ELSE
         SUPP=.TRUE.
         L=TRULEN(VALUE)
         TEXT=VALUE(1:L)

* This statement :
*
*        OPEN (UNIT=11,FILE=TEXT,STATUS='OLD',READONLY,ERR=400)
*
* is now replaced by this call (see History):

         CALL FILEOPEN( 11, TEXT, 'OLD', ' ', ' ', .FALSE., 0, .TRUE.,
     :                STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 400

C
C      First read the equinox of the positions.
C
         READ (11,'(A)') VAL2
         CALL CHR_CTOR(VAL2,EQ, STATUS )
C
C      Maximum number of extra objects is 1000
C
         DO N=1,1001
C
C         Exit will probably be by hitting the end of the file
C
            READ (11,'(A)',END=300) TEXT
            CALL CONVRA(TEXT,1,50,OWNOBJ(1,N), STATUS )
            READ (11,'(A)') TEXT
            CALL CONVDEC(TEXT,1,50,OWNOBJ(2,N), STATUS )
         END DO
  300    CONTINUE
         NUMSUPP=N-1
         IF (ABS(EQ-EQUOUT).GT.1E-6) THEN
            DO N=1,NUMSUPP
               CALL PRECES(OWNOBJ(1,N),OWNOBJ(2,N),OWNOBJ(1,N),
     :                     OWNOBJ(2,N),EQ,EQUOUT)
            END DO
         END IF
C
C      Skip error handling.
C
         GO TO 500
  400    CONTINUE
         CALL MSG_OUT(' ', 'Failed to find file of extra objects',
     :    STATUS)
         SUPP=.FALSE.
  500    CONTINUE
         CLOSE (UNIT=11)
      END IF

      END
