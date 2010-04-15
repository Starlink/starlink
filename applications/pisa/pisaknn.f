      SUBROUTINE PISAKNN( STATUS )
*+
*  Name:
*     PISAKNN

*  Purpose:
*     PISAKNN use the results of PISAPEAK to discriminate objects
*     into two classes.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAKNN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     PISAKNN uses KNN (k nearest neighbours) distribution-free
*     multivariate discrimination to classify objects into two classes.
*     The classes are seeded by supplying two files which contain the
*     indices of objects typical to the class in question (>5,
*     approximately equal numbers of each). Each object then propagates
*     its class to the other objects on the basis of which class of the
*     2*k nearest neighbours (in the parameter space of the PISAPEAK
*     results) of each of the unclassified objects is most common. This
*     procedure is iterated until all objects are assigned and have a
*     stable class or until a maximum number of iterations is exceeded.
*     The results of the discrimination are written into two output
*     files, one for each class.

*  Usage:
*     PISAKNN PEAKDATA SEED1 SEED2 K CLASS1 CLASS2 NITER

*  ADAM Parameters:
*     CLASS1 = FILENAME (Write)
*        Name of a file to contain the indices of the objects selected
*        for membership of class 1. [CLASS1.DAT]
*     CLASS2 = FILENAME (Write)
*        Name of a file to contain the indices of the objects selected
*        for membership of class 2. [CLASS2.DAT]
*     ELLIP = _LOGICAL (Read)
*        If `true' then the ellipticities are used in the analysis. If
*        `false' then they are excluded. Using ellipticities may
*        increase the weighting of some (small) round galaxies as stars.
*        [TRUE]
*     K = _INTEGER (Read)
*        The number of nearest neighbours about the current values
*        which are to be used in classifying an object. The class
*        used is the most frequently encountered in this range of
*        objects. If classes 1 and 2 are equally frequent then the
*        object classification is not changed. [1]
*     NITER = _INTEGER (Read)
*        The maximum number of iterations allowed to classify and
*        reclassify objects. [10]
*     PEAKDATA = FILENAME (Read)
*        Name of a file containing the results of the PISAPEAK
*        parameter transformation. This file must contain at least
*        five columns which have the values:
*        (object index)
*        (radius ratio)
*        (intensity-peak ratio)
*        (ellipticity)
*        (absolute value of intensity weighted cross moment)
*        in that order. [PISAPEAK.DAT]
*     SEED1 = FILENAME (Read)
*        Name of a file containing the indices of the objects to seed
*        class1. The file can contain any number of columns but must
*        have the object indices in column one. [SEED1.DAT]
*     SEED2 = FILENAME (Read)
*        Name of a file containing the indices of the objects to seed
*        class2. The file can contain any number of columns but must
*        have the object indices in column one. [SEED2.DAT]

*  Examples:
*     PISAKNN PISAPEAK S1 S2 3 C1 C2 5
*        This performs a KNN analysis on file PISAPEAK, using the
*        indices in files S1 and S2 as seeds for classes 1 and 2
*        respectively. The new classifications are assigned using the
*        nearest 6 neighbours (2K). The maximum number of iterations
*        allowed is 5. After the maximum number of iterations is
*        exceeded or the classifications become stable the indices of
*        the class 1 objects are written to file C1 and class 2 to C2.

*  Notes:
*     -  The seed objects are always returned in their initial classes.
*     -  The maximum number of objects allowed in any input file is
*        10000


*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAR-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system status codes
      INCLUDE 'FIO_PAR'          ! FIO parameters

*  Status:
      INTEGER STATUS             ! Global status

*  External functions:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! length of a string excluding
                                 ! trailing blanks

*  Local constants:
      INTEGER MAXENT             ! maximum number of entries in input
                                 ! data files
      PARAMETER ( MAXENT = 10000 ) ! Needs implementing dynamically
                                   ! when PSX arrives

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! File name
      CHARACTER *132 BUF         ! buffer to read file data lines into
      DOUBLE PRECISION  ELL(MAXENT)    ! object ellipticity
      DOUBLE PRECISION  IBYP(MAXENT)   ! ratio of total to peak
                                       ! intensity to model ratio
      DOUBLE PRECISION  PRATIO(MAXENT) ! ratio of radius to model radius
      DOUBLE PRECISION  SXY(MAXENT)    ! intensity weighted radius
                                       ! (absolute value )
      INTEGER CLASS(MAXENT)      ! current class of object INDEX
      INTEGER CLASS1(MAXENT)     ! current classes of the objects
      INTEGER CLASS2(MAXENT)     ! whose variables are stored in
      INTEGER CLASS3(MAXENT)     ! pratio, ibyp, ell and sxy
      INTEGER CLASS4(MAXENT)     ! respectively
      INTEGER I, J, L            ! loop variables
      INTEGER IFAIL              ! nag library routines status indicator
      INTEGER IFS1,
     :        IFS2,
     :        IFS3,
     :        IFS4,
     :        IFS5               ! FIO file descriptors
      INTEGER INDEX(MAXENT)      ! indices of objects
      INTEGER INDEX1(MAXENT)     ! indices of ordered pratio objects
      INTEGER INDEX2(MAXENT)     ! indices of ordered ibyp objects
      INTEGER INDEX3(MAXENT)     ! indices of ordered ell objects
      INTEGER INDEX4(MAXENT)     ! indices of ordered sxyobjects
      INTEGER IPCL1, IPCL2, IPCL3, IPCL4 ! pointers to elements of arrays
      INTEGER IINDEX(MAXENT)      ! rank of ordering
      INTEGER ITER               ! current number of iterations
      INTEGER K                  ! Number of neighbours to include when
                                 ! classifying an object
      INTEGER NCL1, NCL2         ! number of objects in seed class 1 and
                                 ! 2 data
      INTEGER ICL1, ICL2         ! current count of neighbours in
                                 ! classes 1 and 2
      INTEGER NITER              ! maximum number of interations to
                                 ! perform
      INTEGER NOBJ1              ! number of input objects
      LOGICAL CHANGE             ! set when no more objects change class
      LOGICAL OPNF1,
     :        OPNF2,
     :        OPNF3,
     :        OPNF4,
     :        OPNF5              ! set if files are open
      LOGICAL USEELL             ! set to true if the user wants ellipticities
                                 ! in the analysis

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open input file and read in the data.
      CALL PSA1_ASFIO( 'PEAKDATA', 'READ', 'LIST', 0, IFS1, OPNF1,
     :                 STATUS )
*  Read in the data.
      CALL RDPIPE( IFS1, MAXENT, INDEX, PRATIO, IBYP, ELL, SXY,
     :             NOBJ1, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Get the number of neighbours to look for classes.
      CALL PAR_GET0I( 'K', K, STATUS )

*  Set the initial classification to unclassified = '0'.
      DO 2 I =1, NOBJ1
         CLASS( I ) = 0
2     CONTINUE

*  Open the file containing SEED1 indices.
      CALL PSA1_ASFIO( 'SEED1', 'READ', 'LIST', 0, IFS2, OPNF2,
     :                 STATUS )

*  Read in the indices.
      CALL RDPIIN( IFS2, BUF, MAXENT, INDEX1, NCL1, STATUS )

*  Assign these objects to their corresponding columns in main data
*  note that '1' indicates class 1 objects, '-1' indicates unmodifyable
*  class 1 objects. The same applies to class 2.
      DO 5 I = 1, NCL1
         DO 4 J = 1, NOBJ1
           IF( INDEX( J ) .EQ. INDEX1( I ) ) THEN
              CLASS( J ) = -1
           END IF
4        CONTINUE
5     CONTINUE

*  Read in the second class indices
      CALL PSA1_ASFIO( 'SEED2', 'READ', 'LIST', 0, IFS3, OPNF3,
     :                 STATUS )

*  Read in the indices.
      CALL RDPIIN( IFS3, BUF, MAXENT, INDEX2, NCL2, STATUS )

*  Assign objects classes using these indices.
      DO 7 I = 1, NCL2
         DO 8 J = 1, NOBJ1
           IF( INDEX( J ) .EQ. INDEX2( I ) ) THEN
              CLASS( J ) = -2
           END IF
8        CONTINUE
7     CONTINUE

*  Find out if the user wants to include ellipticity in the analysis.
      CALL PAR_GET0L( 'ELLIP', USEELL, STATUS )

*  Set all classes to the seed values, set all corresponding indices
*  before sorting.
      DO 9 J = 1, NOBJ1
        INDEX1(J) = INDEX(J)
        INDEX2(J) = INDEX(J)
        INDEX3(J) = INDEX(J)
        INDEX4(J) = INDEX(J)
        CLASS1(J) = CLASS(J)
        CLASS2(J) = CLASS(J)
        CLASS3(J) = CLASS(J)
        CLASS4(J) = CLASS(J)
9     CONTINUE

*  Sort the variables into ascending order, reorder the classes and
*  indexes, accordingly. Note that the variables themselves are not
*  actually sorted.
      CALL PDA_QSIAR( NOBJ1, PRATIO, IINDEX )
      CALL PDA_RINPI( IINDEX, NOBJ1, CLASS1, IFAIL )
      CALL PDA_RINPI( IINDEX, NOBJ1, INDEX1, IFAIL )

      CALL PDA_QSIAR( NOBJ1, IBYP, IINDEX )
      CALL PDA_RINPI( IINDEX, NOBJ1, CLASS2, IFAIL )
      CALL PDA_RINPI( IINDEX, NOBJ1, INDEX2, IFAIL )

      CALL PDA_QSIAR( NOBJ1, ELL, IINDEX )
      CALL PDA_RINPI( IINDEX, NOBJ1, CLASS3, IFAIL )
      CALL PDA_RINPI( IINDEX, NOBJ1, INDEX3, IFAIL )

      CALL PDA_QSIAR( NOBJ1, SXY, IINDEX )
      CALL PDA_RINPI( IINDEX, NOBJ1, CLASS4, IFAIL )
      CALL PDA_RINPI( IINDEX, NOBJ1, INDEX3, IFAIL )

*  DISCRIMINATION SECTION - iterates until classes of objects are no
*  longer changing or NITER is exceeded.
*
      CALL PAR_GET0I( 'NITER', NITER, STATUS )
      ITER = 0
10001 CONTINUE
        IF( ITER .LT. NITER ) THEN
          ITER = ITER + 1

*  Look at indexes one at a time in main list then look for the
*  equivalents in the sorted lists.
          CHANGE = .FALSE.
          DO 11 I = 1, NOBJ1
             ICL1 = 0
             ICL2 = 0
             DO 12 J = 1, NOBJ1
                IF( INDEX1( J ) .EQ. INDEX( I ) ) THEN
                   IPCL1 = J

*  Radius ratio variable.
*  Look neighbours ( +/- K ) add up contributions from assigned
*  classes.
                   DO 13 L = MAX( 1, J - K ) , MIN( J + K, NOBJ1 )
                      IF( ABS( CLASS1( L ) ) .EQ. 1 ) THEN
                         ICL1 = ICL1 + 1
                      ELSE IF ( ABS( CLASS1( L ) ) .EQ. 2 ) THEN
                         ICL2 = ICL2 + 1
                      END IF
13                 CONTINUE
                END IF
                IF( INDEX2( J ) .EQ. INDEX( I ) ) THEN
                   IPCL2 = J

*  Intensity peak ratio variable.
*  Look neighbours ( +/- K ) add up contributions from assigned
*  classes.
                   DO 14 L = MAX( 1, J - K ) , MIN( J + K, NOBJ1 )
                      IF( ABS( CLASS2( L ) ) .EQ. 1 ) THEN
                         ICL1 = ICL1 + 1
                      ELSE IF ( ABS( CLASS2( L ) ) .EQ. 2 ) THEN
                         ICL2 = ICL2 + 1
                      END IF
14                 CONTINUE
                END IF
                IF( INDEX3( J ) .EQ. INDEX( I ) ) THEN
                   IPCL3 = J

*  Ellipticity variable.
*  Look neighbours ( +/- K ) add up contributions from assigned
*  classes. Use only if required
                   IF( USEELL ) THEN
                      DO 15 L = MAX( 1, J - K ) , MIN( J + K, NOBJ1 )
                         IF( ABS( CLASS3( L ) ) .EQ. 1 ) THEN
                            ICL1 = ICL1 + 1
                         ELSE IF ( ABS( CLASS3( L ) ) .EQ. 2 ) THEN
                            ICL2 = ICL2 + 1
                         END IF
15                    CONTINUE
                   END IF
                END IF
                IF( INDEX4( J ) .EQ. INDEX( I ) ) THEN
                   IPCL4 = J

*  Intensity weighted moment variable.
*  Look neighbours ( +/- k ) add up contributions from assigned
*  classes.
                   DO 16 L = MAX( 1, J - K ) , MIN( J + K, NOBJ1 )
                      IF( ABS( CLASS4( L ) ).EQ. 1 ) THEN
                         ICL1 = ICL1 + 1
                      ELSE IF ( ABS( CLASS4( L ) ) .EQ. 2 ) THEN
                         ICL2 = ICL2 + 1
                      END IF
16                 CONTINUE
                END IF
12           CONTINUE

*  Assign new class for this object, if it is possible.
             IF( ICL1 .GT. 0 .OR. ICL2 .GT. 0 ) THEN

*  Have class to assign. If class has changed record this.
                IF( CLASS( I ) .GE. 0 ) THEN

*  Modifyable class, not one of the seed values.
                   IF( ICL2 .GT. ICL1 ) THEN
                      IF(CLASS( I ) .NE. 2 ) CHANGE =.TRUE.
                      CLASS( I ) = 2
                      CLASS1( IPCL1 ) = 2
                      CLASS2( IPCL2 ) = 2
                      CLASS3( IPCL3 ) = 2
                      CLASS4( IPCL4 ) = 2
                   ELSE IF( ICL2 .LT. ICL1 ) THEN
                      IF(CLASS( I ) .NE. 1 ) CHANGE =.TRUE.
                      CLASS( I ) = 1
                      CLASS1( IPCL1 ) = 1
                      CLASS2( IPCL2 ) = 1
                      CLASS3( IPCL3 ) = 1
                      CLASS4( IPCL4 ) = 1
                   ELSE
*  Do not change the object classification.
                   END IF
                END IF
             END IF
11        CONTINUE
          IF( CHANGE )  THEN

*  Not iterated try again.
             GO TO 10001
          ELSE

*  Iterated finish this section.
             GO TO 10002
          END IF
        ENDIF
10002 CONTINUE

*  Write out results of analysis.
*  Open file for class 1 indices.
      CALL PSA1_ASFIO( 'CLASS1', 'WRITE', 'LIST', 0, IFS4, OPNF4,
     :                 STATUS )

*  Open file for class 2 indices.
      CALL PSA1_ASFIO( 'CLASS2', 'WRITE', 'LIST', 0, IFS5, OPNF5,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Write out the results.
      ICL1 = 0
      ICL2 = 0
      DO 10 I = 1, NOBJ1
         BUF = ' '
         WRITE( BUF, 101 )INDEX( I )
101      FORMAT( 1X, I7 )
         IF ( ABS( CLASS( I ) ) .EQ. 1 ) THEN
            ICL1 = ICL1 + 1
            CALL FIO_WRITE( IFS4, BUF( :CHR_LEN( BUF )), STATUS )
         ELSE IF( ABS( CLASS( I ) ) .EQ. 2 ) THEN
            ICL2 = ICL2 + 1
            CALL FIO_WRITE( IFS5, BUF( :CHR_LEN( BUF )), STATUS )
         END IF
 10    CONTINUE

*  Write out an informational message on how many entries have been made
*  to each class.
      CALL FIO_FNAME( IFS4, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_SETI( 'CLASS1_ENTRIES', ICL1 )
      CALL MSG_SETI( 'NEW_ENTRIES', ICL1 - NCL1 )
      CALL MSG_OUT( 'MESSAGE_OUTPUT',
     : ' ^CLASS1_ENTRIES entries written to file ^FNAME'//
     : ' of which ^NEW_ENTRIES were previously unclassified', STATUS )

      CALL FIO_FNAME( IFS5, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_SETI( 'CLASS2_ENTRIES', ICL2 )
      CALL MSG_SETI( 'NEW_ENTRIES', ICL2 - NCL2 )
      CALL MSG_OUT( 'MESSAGE_OUTPUT',
     : ' ^CLASS2_ENTRIES entries written to file ^FNAME'//
     : ' of which ^NEW_ENTRIES were previously unclassified', STATUS )

*  All done. Arrive directly here if an error condition has occured.
999   CONTINUE

*  Close open files and exit with error message if appropriate.
      IF( OPNF1 ) THEN
         CALL FIO_CLOSE( IFS1, STATUS )
         CALL PAR_CANCL( 'PEAKDATA', STATUS )
      END IF
      IF( OPNF2 ) THEN
         CALL PAR_CANCL( 'SEED1', STATUS )
         CALL FIO_CLOSE( IFS2, STATUS )
      END IF
      IF( OPNF3 ) THEN
         CALL PAR_CANCL( 'SEED2', STATUS )
         CALL FIO_CLOSE( IFS3, STATUS )
      END IF
      IF( OPNF4 ) THEN
         CALL PAR_CANCL( 'CLASS1', STATUS )
         CALL FIO_CLOSE( IFS4, STATUS )
      END IF
      IF( OPNF5 ) THEN
         CALL PAR_CANCL( 'CLASS2', STATUS )
         CALL FIO_CLOSE( IFS5, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAKNN_ERR',
     :   'PISAKNN: Error classifying objects.',
     :   STATUS )
      END IF

      END
* $Id$
