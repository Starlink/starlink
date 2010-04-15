      SUBROUTINE FIND29( POUTDE, MENU, STATUS )
*+
*  Name:
*     FIND29

*  Purpose:
*     The subroutine creates the output files which can be read by the
*     Boresight Survey Data Extraction Program, EXCRDD. This subroutine
*     creates a sorted list of scans for each plate record and calls the
*     subroutine FIND23 to actually output the required HDS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND29( POUTDE, MENU, STATUS )

*  Description:
*
*     The subroutine creates the output files which can be read by the
*     Boresight Survey Data Extraction Program, EXCRDD.This subroutine
*     creates a sorted list of scans for each plate record and calls the
*     subroutine FIND23 to actually output the required HDS file.
*
*     -  Each plate record is used to generate a separate output file.
*     Although it is possible that a single plate number may have
*     several plate records (see below).
*
*     -  All the scans which need to be extracted from this plate
*     are identified using the cross linkages between the plate record,
*     and source common and scan common.
*
*     -  Each scan identified is added to a linked list, which is sorted
*     by their start UCTS time. This corresponds to the order of the
*     data on the IRAS Survey plate. The first scan in this list is
*     identified by a pointer in the plate record, the pointer being
*     the position of the first scan in scan common. From the first
*     scan onward each identified scan in scan common contains a
*     pointer to the next one in the time order, again the pointer is
*     the position of this following scan in scan common.
*
*     -  The finish time of each scan in the linked list is compared
*     with the start time of the next in the time order. If these times
*     are found to overlap the scan overlap flag is set.
*
*     -  An HDS file for each plate is created using FIND23
*
*  Arguments:
*     POUTDE = CHARACTER * ( * ) (Given)
*        Parameter OUTDESCFILE for output file to contain description
*        of plate files created and tapes required for them
*     MENU   = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from FINDCRDD main menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm:
*
*     For each plate record
*
*     -  For  each source in the plate common record.
*
*        If the source is required
*
*        -  For each scan in the source common record.
*
*           If the scan is required
*
*           - Enter the scan into its correct position in the chain of
*           pointers which form a list of scans ordered by their start
*           UTCS time. This involves:-
*
*              If the scan is the first scan identified for this source
*              put its scan common position identifier into the plate
*              record's first scan position variable.
*
*              If the scan not the first scan required for this plate
*              follow the chain of pointers, starting at that in the
*              plate record, and followed by the sequence pointed to
*              in the scan next position pointers. Examine the start
*              UTCS of each scan and compare with that of the scan to be
*              entered until the correct position is found. Break the
*              chain at this point and relink to include scan to be
*              entered in the pointer sequence.
*
*     -  When all the scans for this plate have been entered go through
*     the sorted list of scans and determine if the start time of the
*     next scan is before the end UTCS time of this scan. If so mark
*     the first scan as being overlapped.
*
*     -  Call FIND23 to write plate record output HDS file
*
*     -  A file 'FINDCRDD_PLATES_REQ' is created containing a list of
*     the plates for which HDS files have been created including the
*     number of scans per source, and the tape id from which data should
*     be extracted.
*
*  External Routines Used:
*     FINDCRDD:
*        FIND23
*     CHR:
*        CHR_ITOC
*     FIO:
*        FIO_OPEN, FIO_WRITE
*     MSG:
*        MSG_FMTC, MSG_FMTI, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_DEF0C, PAR_GET0C

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     26-MAR-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors
      INCLUDE 'PSX_ERR'          ! PSX errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * ) POUTDE

*  Arguments Given and Returned
      CHARACTER * ( 1 ) MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER ENDVAL             ! Value of pointer which indicates end
                                 ! of linked list
      PARAMETER ( ENDVAL = 99999 )

*  Local Variables:
      CHARACTER * ( FIO__SZMOD ) ACMODE ! Access mode for FIO
      CHARACTER * ( 80 ) BUFFER  ! Output buffer for use with FIO
      INTEGER CFSCAN             ! Scan to be compared with current scan
      CHARACTER * ( FIO__SZFNM) DESFIL   ! Actual name of file for
                                         ! description of plate files
                                         ! created.
      CHARACTER * ( 19 ) DESNAM  ! Default name of file for description
                                 ! of plate files created.
      INTEGER FD                 ! File descriptor obtained from FIO
      LOGICAL FISCPL             ! First plate for this plate flag
      INTEGER IPLAT              ! Do loop variable for plates
      INTEGER ISCAN              ! Do loop variable for scans
      INTEGER ISOURC             ! Do loop variable for sources
      INTEGER LACFSC             ! Last scan that was compared with
                                 ! current scan
      CHARACTER * ( 30 ) MACHIN  ! Name of the hardware of the computer
      CHARACTER * ( 6 ) NAMCHR   ! To hold the character version of the
                                 ! plate number or plate subsection
      INTEGER NAMLEN             ! To hold the length of the character
                                 ! version of the plate number or plate
                                 ! subsection
      INTEGER NSCPOS             ! Next scan index in linked list
      CHARACTER * ( 30 ) NODENM  ! Node name of the computer
      CHARACTER * ( 10 ) OUTNAM  ! Output file name for plate HDS file
      INTEGER RECSZ              ! Record size for FIO (0=default)
      CHARACTER * ( 30 ) RELEAS  ! Version of the operating system
      INTEGER SCPOS              ! Current scan index
      INTEGER SOPOS              ! Current source index
      CHARACTER * ( 30 ) SYSNAM  ! Name of the operating system
      CHARACTER * ( 30 ) VERSON  ! Subversion of the operating system
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set all the scan cross linkage pointers to a value that indicates end
*  of linked list
      DO 100 ISCAN = 1, NOSCAN
         SCNSCP( ISCAN ) = ENDVAL
 100  CONTINUE

*  *********************************************************************
*  Create a file in which to put description of plate files created
*  ********************************************************************
*  Use default Fortran maximum record size ( 133 bytes)
      RECSZ = 0

*  Use access mode write
      ACMODE = 'WRITE'

*  Generate a suitable dynamic default file name for the description of
*  plate files created file
      DESNAM = 'FINDCRDD_PLATES_REQ'

*  Determine the operating system being used
      CALL PSX_UNAME( SYSNAM, NODENM, RELEAS, VERSON, MACHIN, STATUS )

*  If the system is not VMS change the name to lower case
      IF ( SYSNAM .NE. 'VMS' ) THEN
         CALL CHR_LCASE( DESNAM )
      END IF

*  Enter the generated name as the default for the parameter
      CALL PAR_DEF0C( POUTDE, DESNAM, STATUS )

*  Open the description of plates created file
      CALL FIO_ASSOC( POUTDE, ACMODE, 'FORTRAN', RECSZ, FD, STATUS )

*  *********************************************************************
*  For each plate common record
*  *********************************************************************
      DO 600 IPLAT = 1, NOPLAT

*  Form name for output HDS object
         OUTNAM(1:9) = 'PLATE0000'
         CALL CHR_ITOC( PLNUM( IPLAT ), NAMCHR, NAMLEN )
         OUTNAM( 10 - NAMLEN :9) = NAMCHR
         OUTNAM(10:10)  = ' '
         IF ( PLSUBI( IPLAT ) .NE. ' ' ) THEN
            OUTNAM( 10:10) = PLSUBI( IPLAT )
         END IF

*  If the system is not VMS change the name to lower case
         IF ( SYSNAM .NE. 'VMS' ) THEN
            CALL CHR_LCASE( OUTNAM )
         END IF

*  Display the plate name(is the same as the HDS file name)
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_FMTC( 'C1', 'A10', OUTNAM )
         CALL MSG_OUT( ' ','^C1', STATUS )

*  Write the plate name on description of plates file
         BUFFER = '
     :                               '
         WRITE ( BUFFER(2:11), '(A10)' ) OUTNAM
         CALL FIO_WRITE( FD, BUFFER, STATUS )

*  Set first scan for this plate flag to true
         FISCPL = .TRUE.

*  Initialise the count of scans for this plate to zero
         PLNOSC( IPLAT ) = 0

*  *********************************************************************
*  For each source associated with that plate record ( PLNOSO is the
*  number of sources associated with this plate)
*  *********************************************************************
         DO 400 ISOURC = 1, PLNOSO( IPLAT )

*  Put the array index for the current source within the plate common
*  into a more convienient variable
            SOPOS = PLSOI( IPLAT, ISOURC)

*  Check whether source is required
            IF ( .NOT. SOMADE( SOPOS ) ) THEN

*  Display the source name and the number of scans
               CALL MSG_FMTC( 'C1', 'A8', SONAME( SOPOS ))
               CALL MSG_FMTI( 'I1', 'I3', SONOSC( SOPOS ) )
               CALL MSG_OUT( ' ', 'Source ^C1 has ^I1 scans', STATUS )

*  Write the the source name and the number of scans on description
*  of plates file
               BUFFER = ' Source           has     scans
     :                                           '
               WRITE ( BUFFER(9:17), '(A8)' ) SONAME( SOPOS )
               WRITE ( BUFFER(23:25), '(I3)' ) SONOSC( SOPOS )
               CALL FIO_WRITE( FD, BUFFER, STATUS )

*  *********************************************************************
*  For each scan associated with that source ( SONOSC is the number of
*  scans assoicated with that source )
*  *********************************************************************
               DO 300 ISCAN = 1, SONOSC( SOPOS )

*  Put the array index for the current scan within the source common
*  into a more convienient variable
                  SCPOS = SOSCI( SOPOS, ISCAN )

*  Check whether scan is required
                  IF ( SCRQFL( SCPOS ) ) THEN

* **********************************************************************
* Form a linked list of required scans for this plate in start time
* order
* **********************************************************************

*  Is the current scan the first to be considered for this plate?
                     IF ( FISCPL ) THEN

*  Set the pointer for the first scan for the plate (in start UTCS
*  order) to point to the current scan temporarily
                        PLFSCP( IPLAT ) = SCPOS

*  Set the first scan to be considered flag off
                        FISCPL = .FALSE.

                     ELSE

*  If the current scan is not the first to be considered for this
*  plate:-
*  First check whether the current scan's UTCS start time is less than
*  the start time of the scan pointed to by the first scan for the plate
*  (First put the scan it is to be compared with into a suitable
*  variable)
                        CFSCAN = PLFSCP( IPLAT )
                        IF ( SCSTUT( SCPOS ) .LT.
     :                                     SCSTUT( CFSCAN ) ) THEN

*  Set the pointer for the first scan for the plate (in start UTCS
*  order) to point to the current scan temporarily
                           PLFSCP( IPLAT ) = SCPOS

*  Set the pointer for the position of the next scan in the current scan
*  record to be that of the scan compared
                           SCNSCP( SCPOS ) = CFSCAN

                        ELSE
*  Else follow the chain of next scan in start UTCS order pointers until
*  a start UCTS time is found greater than the current scan start UTCS
*  or an end of linked list value is found.
 200                       CONTINUE

*  Check whether the pointer to the next scan in the comparison
*  scan indicates the end of the linked list
                           IF ( SCNSCP( CFSCAN ) .EQ. ENDVAL  ) THEN

*  Set the pointer in the last scan compared to the current scan
                              SCNSCP( CFSCAN ) = SCPOS
                           ELSE

*  Store the compare scan value as the last compare scan
                              LACFSC = CFSCAN

*  Set a new value of the scan to be compared as the next scan pointer
*  in the present scan to be compared
                              CFSCAN = SCNSCP( CFSCAN )

*  Check whether the current scan's UTCS start time is less than
*  the start time of the scan to be compared
                              IF ( SCSTUT( SCPOS ) .LT.
     :                                     SCSTUT( CFSCAN ) ) THEN

*  Set the pointer in the last scan compared to the current scan
                                 SCNSCP( LACFSC ) = SCPOS

*  Set the pointer in the current scan to be the comparison scan
                                 SCNSCP( SCPOS ) = CFSCAN

*  Else go to check next pointer in linked list
                              ELSE
                                 GO TO 200
                              END IF

                           END IF
                        END IF
                     END IF

*  Add one to the count of scans for this plate
                     PLNOSC( IPLAT ) = PLNOSC( IPLAT ) + 1

*  End of for if scan is required
                  END IF

*  End of for each scan marked as belonging to the current source
 300           CONTINUE

*  End if for if source is required
            END IF

*  End of for each source marked as belonging to current plate
 400     CONTINUE

* **********************************************************************
*  Check scan list for overlapping scans and generate overlap flags
* **********************************************************************

*  If the pointer in the plate record pointing to the first scan record
*  in the linked list is not the end list marker
         IF ( PLFSCP( IPLAT ) .NE. ENDVAL ) THEN

*  Set the current scan marker to the scan pointed to by the plate
*  record
            SCPOS = PLFSCP( IPLAT )

*  This is the top of the loop for processing each scan in linked list
 500        CONTINUE

*  If the scan pointed to by the current scan is not the end list marker
            IF ( SCNSCP( SCPOS ) .NE. ENDVAL ) THEN

*  Set the next scan marker to the scan pointed to by the current scan
               NSCPOS = SCNSCP( SCPOS )

*  If the start time of the next scan is before the end time of the
*  current scan
               IF ( SCENUT( SCPOS ) .GE. SCSTUT( NSCPOS ) ) THEN

*  Set the current scan overlapped flag to .TRUE. ie overlapped
                  SCOVFL( SCPOS ) = .TRUE.

               ELSE
*  Set the current scan overlapped flag to .FALSE. ie  not overlapped
                  SCOVFL( SCPOS ) = .FALSE.

               END IF

*  Set the current scan position to that of the next scan in the list
*  for processing that scan
               SCPOS = NSCPOS

*  Go to the top of the loop for processing each scan in linked list
               GO TO 500

*  If the scan pointed to by the current scan is the end list marker
            ELSE

*  Set the current scan overlapped flag to .FALSE. ie  not overlapped
               SCOVFL( SCPOS ) = .FALSE.

            END IF
         END IF

* **********************************************************************
* Call FIND23 to prepare an HDS output file containing scan requirements
* for EXCRDD for this plate record.
* **********************************************************************
         CALL FIND23( IPLAT, FD, OUTNAM, STATUS )

*  End of for each plate record loop
 600  CONTINUE

*  Set main menu variable to displaying the main menu
      MENU = 'M'

      END
