
      SUBROUTINE PERIOD_OGIP(YPTR, MXCOL, MXSLOT,
     :                       NPTSARRAY, INFILEARRAY,
     :                       YERRORARRAY, DETRENDARRAY)

*+
*  Name:
*     PERIOD_OGIP

*  Purpose:
*     Routine to read OGIP FITS table data into a PERIOD data slot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PERIOD_OGIP(YPTR, MXCOL, MXSLOT,
*                      NPTSARRAY, JUNK1, JUNK2,
*                      INFILEARRAY, YERRORARRAY, DETRENDARRAY)

*  Description:
*     The user enters the name of the file to be considered. The code
*     then opens the file and looks at the first line to check the
*     file is of a simple FITS format.
*
*     An attempt is then made to read the values for FITS keywords
*     DETNAM, INSTRUMENT, TELESCOPE and OBJECT. These are displayed
*     for the user.
*
*     The program then looks through the file to find all the extensions.
*     It displays the details (number of points
*     EXTNAME value etc) of the extensions found and asks the users to
*     select one to be read. The headings for each of the rows present is
*     then displayed and the user can then select which slot the data shall
*     go in and the which column will represent X, Y and Y errors.

*  Notes:
*     No attempt has been made to adopt the Starlink parameter system.
*
*     Several parts of this code were 'borrowed' from PERIOD_INPUT.F
*     which use several techniques that are frowned on in the Starlink
*     Programmers Standard. This usage has been retained to keep the
*     program style consistent with that found in the rest of PERIOD.
*     Hopefully, this will mean that the program will be of a style
*     familiar to PERIOD's author (Vik Dhillon), thereby making it easier
*     for him to adjust the code if he so desires.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1994 (GJP)
*     (Original version)
*     10-MAR-1997 (GJP)
*     Removed NVEC variable.
C     Converted to Double Precision (KPD), August 2001
C     Modified to incorporate dynamic memory allocation for major
C      data/work array(s) and/or use of such arrays (KPD), October 2001

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

*   PLT declarations.
      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

*   PERIOD_INPUT declarations.
*   Similar to those used in other PERIOD packages.
      INTEGER JUNK1PTR, JUNK2PTR
      INTEGER NUMROWS
      INTEGER SLOT
      INTEGER KEYPTR, PERIOD_GET1DINT
      INTEGER YSLOT1, THISROW, IFAIL
      INTEGER OKCUREXT, XICUREXT, YICUREXT
      CHARACTER*72 INFILEARRAY(MXSLOT)
      LOGICAL YERRORARRAY(MXSLOT), YERROR, DETRENDARRAY(MXSLOT)

*   Local variables.
      CHARACTER*80 AGAINS           ! Loop again user input
      CHARACTER*80 COMMENT          ! Unused string returned by FITSIO
      CHARACTER*40 DETN             ! Contents of DETNAM keyword
C     CHARACTER*40 EXTN(26)         ! Contents of the EXTNAME keyword
      INTEGER EXTNPTR               ! Pntr to contents of EXTNAME keyword
      CHARACTER*80 FILENAME         ! OGIP file to be examined
      CHARACTER*20 HEAD1(3)         ! File type array
C     CHARACTER*40 HDC(26)          ! Contents of HDUCLAS1 keyword
      INTEGER HDCPTR                ! Pntr to contents of HDUCLAS1 keyword
C     CHARACTER*40 HDCC(26)         ! Contents of HDUCLASS keyword
      INTEGER HDCCPTR               ! Pntr to contents of HDUCLASS keyword
C     CHARACTER*40 HDC2(26)         ! Contents of HDUCLAS2 keyword
      INTEGER HDC2PTR               ! Pntr to contents of HDUCLAS2 keyword
      CHARACTER*40 INST             ! Contents of INSTRUMENT keyword
      CHARACTER*72 JUNK             ! Rubbish
      CHARACTER*40 OBJE             ! Contents of OBJECT keyword
      CHARACTER*80 REC1             ! The first line of the file
      CHARACTER*80 STRING           ! An 80 character string
      CHARACTER*40 STRING2          ! A 40 character string
      CHARACTER*40 TELE             ! Contents of the TELESCOPE keyword
      CHARACTER*72 TEMPS            ! A temporary string
      CHARACTER*10 TTYPE            ! Identifiers for the table columns
      DOUBLE PRECISION DVAL1        ! Values from keywords
      DOUBLE PRECISION DVAL2        ! Values from keywords
      DOUBLE PRECISION DVAL3        ! Values from keywords
      DOUBLE PRECISION NULL         ! Determines treatment of undefined
                                    ! values
      LOGICAL TEST1                 ! Flag for undefined values
      LOGICAL TEST2                 ! Flag for undefined values
      LOGICAL TEST3                 ! Flag for undefined values
      INTEGER AGAIN                 ! Read again flag
      INTEGER BLOCKSIZE             ! Blocksize! Not used
      INTEGER COLNX                 ! Column representing X
      INTEGER COLNY                 ! Column representing Y
      INTEGER COLNZ                 ! Column representing Y errors
      INTEGER CUREXT                ! Chosen extension number
      INTEGER EXTENS                ! Number of extensions
C     INTEGER HDT(26)               ! Type of table present
      INTEGER HDTPTR                ! Pntr to type of table present
                                    ! ie binary or ASCII
      INTEGER HDUTYPE               ! Extension table type
      INTEGER I                     ! Loop variable
      INTEGER NFOUND                ! Number of keywords found
      INTEGER NKEYS                 ! Number of keywords found
      INTEGER NSPACE                ! Not used
C     INTEGER OKAY(26)              ! Identifies usable extensions
      INTEGER OKAYPTR               ! Pntr to info on usable extensions
      INTEGER READWRITE             ! Read/write IO mode
      INTEGER ROW                   ! Row of the table to examine
      INTEGER SOFAR                 ! Counter used during displays
      INTEGER STATUS                ! FITSIO status flag
      INTEGER STATUS1               ! FITSIO status flag
      INTEGER STATUS2               ! FITSIO status flag
      INTEGER STATUS3               ! FITSIO status flag
      INTEGER UNIT                  ! IO unit number
C     INTEGER XI(26)                ! Number of rows in an extension
      INTEGER XIPTR                 ! Pntr to no. of rows in an extension
C     INTEGER YI(26)                ! Number of columns in an extension
      INTEGER YIPTR                 ! Pntr to no. of columns in an extension
*.

*   Set the unit number.
      UNIT=12

*   Set names for the two type of table that might be found.
      HEAD1(1)='An ASCII file.'
      HEAD1(2)='A binary table.'
      HEAD1(3)='An undefined table'

* Attempt to read the file.

 10   CONTINUE

*   Get the file name.
      WRITE(*,*) ' '
      WRITE(*,'(X,A,$)')
     : 'Enter name of OGIP FITS file (<CR> to quit) : '
      READ (*,'(A)',ERR=10) TEMPS

*   Abort if requested.
      IF(TEMPS(1:1).EQ.' ') GOTO 900

*   Keep the file name.
      FILENAME=TEMPS

*   Cycle around if the user wants to get more data from the same file.
      AGAIN=1
      DO WHILE(AGAIN.NE.0)

*      Set the FITSIO status flag.
         STATUS=0

*      Open the OGIP FITS file requested.
         READWRITE=0
         CALL FTOPEN(UNIT,FILENAME,READWRITE,BLOCKSIZE,STATUS)
         IF(STATUS.NE.0) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*) '** ERROR: Could not open/read the file.'
            WRITE(*,*) ' '
            GOTO 10
         END IF

*      Find the number of keywords in the first HDU.
         CALL FTGHSP(UNIT,NKEYS,NSPACE,STATUS)
         IF((STATUS.NE.0).OR.(NKEYS.EQ.0)) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*)
     :         '** ERROR: Could not find any keywords in the HDU.'
            WRITE(*,*) ' '
            GOTO 10
         END IF

*      Initialise the values for INSTRUMENT etc.
         REC1=' '
         INST=' Unknown'
         OBJE=' Unknown'
         TELE=' Unknown'
         DETN=' Unknown'

*      Try to read the first line
         STATUS=0
         CALL FTGREC(UNIT,1,REC1,STATUS)
         CALL PERIOD_CASE(REC1, .TRUE.)
         IF((REC1(1:6).NE.'SIMPLE').OR.(REC1(30:30).NE.'T')) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*)
     :         '** ERROR: File is not simple FITS.'
            WRITE(*,*) ' '
            GOTO 10
         END IF

*      Try to read the telescope details.

*      Telescope.
         STATUS=0
         CALL FTGKEY(UNIT,'TELESCOP',STRING,COMMENT,STATUS)
         IF(STATUS.EQ.0) THEN
            CALL PERIOD_STRIP(STRING,STRING2)
            TELE=STRING2
         END IF

*      Instrument.
         STATUS=0
         CALL FTGKEY(UNIT,'INSTRUME',STRING,COMMENT,STATUS)
         IF(STATUS.EQ.0) THEN
            CALL PERIOD_STRIP(STRING,STRING2)
            INST=STRING2
         END IF

*      Object.
         STATUS=0
         CALL FTGKEY(UNIT,'OBJECT',STRING,COMMENT,STATUS)
         IF(STATUS.EQ.0) THEN
            CALL PERIOD_STRIP(STRING,STRING2)
            OBJE=STRING2
         END IF

*      Detector name.
         STATUS=0
         CALL FTGKEY(UNIT,'DETNAM',STRING,COMMENT,STATUS)
         IF(STATUS.EQ.0) THEN
            CALL PERIOD_STRIP(STRING,STRING2)
            DETN=STRING2
         END IF

*      Display details of the file found so far.
         WRITE(*,*) ' '
         WRITE(*,*) 'File details are...'
         WRITE(*,*) ' '
         WRITE(*,*) 'Telescope:  ',TELE
         WRITE(*,*) 'Instrument: ',INST
         WRITE(*,*) 'Detector:   ',DETN
         WRITE(*,*) 'Object:     ',OBJE
         WRITE(*,*) ' '

C      Determine number of extensions, prior to dynamic array allocation

*      Look through/for extensions.
         EXTENS=0
         STATUS=0
         DO WHILE (STATUS.EQ.0)

*         Jump to the required extension.
            CALL FTMAHD(UNIT,EXTENS+1,HDUTYPE,STATUS)

*         Increment the extension counter.
             EXTENS=EXTENS+1

         END DO

         EXTENS=EXTENS-1

*      Close down the file.
         CALL FTCLOS(UNIT,STATUS)

*      Abort due to lack of suitable extensions.
         IF(EXTENS.EQ.0)THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*) '** ERROR: No extensions found!'
            WRITE(*,*) ' '
            GOTO 10
         END IF

C      Dynamically allocate memory.
         CALL PERIOD_ALLOC('_CHAR', EXTENS*40, HDCCPTR)
         CALL PERIOD_ALLOC('_CHAR', EXTENS*40, EXTNPTR)
         CALL PERIOD_ALLOC('_CHAR', EXTENS*40, HDCPTR)
         CALL PERIOD_ALLOC('_CHAR', EXTENS*40, HDC2PTR)
         CALL PERIOD_ALLOC('_INTEGER', EXTENS, HDTPTR)
         CALL PERIOD_ALLOC('_INTEGER', EXTENS, XIPTR)
         CALL PERIOD_ALLOC('_INTEGER', EXTENS, YIPTR)
         CALL PERIOD_ALLOC('_INTEGER', EXTENS, OKAYPTR)

*      Open the OGIP FITS file again.
         STATUS=0
         READWRITE=0
         CALL FTOPEN(UNIT,FILENAME,READWRITE,BLOCKSIZE,STATUS)
         CALL PERIOD_OGIPEXTNS(%VAL(CNF_PVAL(XIPTR)),
     :                         %VAL(CNF_PVAL(YIPTR)),
     :                         %VAL(CNF_PVAL(HDTPTR)),
     :                         %VAL(CNF_PVAL(OKAYPTR)),
     :                         %VAL(CNF_PVAL(HDCPTR)),
     :                         %VAL(CNF_PVAL(HDCCPTR)),
     :                         %VAL(CNF_PVAL(HDC2PTR)),
     :                         %VAL(CNF_PVAL(EXTNPTR)),
     :                         HEAD1, EXTENS, UNIT)

*      Close down the file.
         CALL FTCLOS(UNIT,STATUS)
         CALL PERIOD_DEALL(HDTPTR)
         CALL PERIOD_DEALL(HDC2PTR)
         CALL PERIOD_DEALL(HDCPTR)
         CALL PERIOD_DEALL(EXTNPTR)
         CALL PERIOD_DEALL(HDCCPTR)

*      Let the user select the extension to be used.
 150     CONTINUE
         CUREXT=0
         WRITE(*,'(X,A,$)') 'Enter the extension number ' //
     :                      'to be considered (0 to quit) : '
         READ (*,*,ERR=150) CUREXT

         IF(CUREXT.EQ.0) THEN
            CALL PERIOD_DEALL(OKAYPTR)
            CALL PERIOD_DEALL(YIPTR)
            CALL PERIOD_DEALL(XIPTR)
            GOTO 900
         END IF

         IF((CUREXT.LT.0).OR.(CUREXT.GT.EXTENS)) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*)
     :       '** ERROR: No extension of that number!'
            WRITE(*,*) ' '
            GOTO 150
         END IF

         OKCUREXT = PERIOD_GET1DINT(CUREXT, %VAL(CNF_PVAL(OKAYPTR)),
     :                              EXTENS)

         IF(OKCUREXT.EQ.1) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*)
     :       '** ERROR: That extension is unusable!'
            WRITE(*,*) ' '
            GOTO 150
         END IF

         XICUREXT = PERIOD_GET1DINT(CUREXT, %VAL(CNF_PVAL(XIPTR)),
     :                              EXTENS)
         YICUREXT = PERIOD_GET1DINT(CUREXT, %VAL(CNF_PVAL(YIPTR)),
     :                              EXTENS)

         CALL PERIOD_DEALL(OKAYPTR)
         CALL PERIOD_DEALL(YIPTR)
         CALL PERIOD_DEALL(XIPTR)

*      Open the OGIP FITS file again.
         STATUS=0
         READWRITE=0
         CALL FTOPEN(UNIT,FILENAME,READWRITE,BLOCKSIZE,STATUS)

*      Go to the desired extension.
         STATUS=0
         CALL FTMAHD(UNIT,CUREXT,HDUTYPE,STATUS)

*      Read the TTYPE keywords giving the names of the columns.
         WRITE(*,*) 'Column        Contents '
         SOFAR=0
         DO 200 I=1,XICUREXT

*         Obtain the column header title.
            TTYPE=' '
            CALL FTGKNS(UNIT,'TTYPE',I,1,TTYPE,NFOUND,STATUS)
            WRITE(*,*) I,'             ',TTYPE

*         Avoid a fast screen scroll.
            SOFAR=SOFAR+1
            IF(SOFAR.EQ.20) THEN
               WRITE(*,'(X,A,$)') 'Press enter to continue '
               READ (*,'(A)') JUNK
               SOFAR=0
            END IF

 200     CONTINUE

*      Let the user select the column to be used.
 250     CONTINUE
         COLNX=0
         WRITE(*,'(X,A,$)') 'Enter the TTYPE column number ' //
     :                      'to be read as X (0 to quit) : '
         READ (*,*,ERR=250) COLNX
         IF(COLNX.EQ.0) GOTO 900

         IF((COLNX.LT.0).OR.(COLNX.GT.XICUREXT)) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*)
     :       '** ERROR: No column of that number!'
            WRITE(*,*) ' '
            GOTO 250
         END IF

*      Let the user select the column to be used.
 300     CONTINUE
         COLNY=0
         WRITE(*,'(X,A,$)') 'Enter the TTYPE column number ' //
     :                      'to be read as Y (0 to quit) : '
         READ (*,*,ERR=300) COLNY
         IF(COLNY.EQ.0) GOTO 900

         IF((COLNY.LT.0).OR.(COLNY.GT.XICUREXT)) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*)
     :       '** ERROR: No column of that number!'
            WRITE(*,*) ' '
            GOTO 300
         END IF

*      Only offer this option if more than 2 columns are present.
         IF(XICUREXT.GT.2) THEN

*      Let the user select the column to be used.
 350        CONTINUE
            COLNZ=0
            WRITE(*,'(X,A,$)') 'Enter the TTYPE column number ' //
     :                   'to be read as Y errors (0 to ignore) : '
            READ (*,*,ERR=350) COLNZ

*      Only continue if the user wants this option.
            YERROR=.FALSE.
            IF(COLNZ.NE.0) THEN
               YERROR=.TRUE.
               IF((COLNZ.LT.0).OR.(COLNZ.GT.XICUREXT)) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE(*,*) ' '
                  WRITE(*,*)
     :             '** ERROR: No column of that number!'
                  WRITE(*,*) ' '
                  GOTO 350
               END IF

            END IF

         END IF
         WRITE(*,*) ' '

*      Let the user select the data slot to be used.
 400     CONTINUE
         SLOT=0
         WRITE(*,'(X,A,$)') 'Enter the slot number into which' //
     :                      ' the data should be read (0 to quit) : '
         READ (*,*,ERR=400) SLOT
         IF(SLOT.EQ.0) GOTO 900
         IF((SLOT.LT.0).OR.(SLOT.GT.MXSLOT)) THEN
             CALL PERIOD_WRITEBELL()
             WRITE(*,*) ' '
             WRITE(*,*) '** ERROR: Maximum slot number=', MXSLOT
             WRITE(*,*) ' '
             GO TO 400
         END IF

C      Determine number of data values, prior to dynamic array allocation

*      Tell the user what is going on.
         WRITE(*,*)
         WRITE(*,*) 'Estimating data quantities for Slot=',SLOT
         WRITE(*,*) ' '

*      Read all the columns.
         NUMROWS=0

         DO 500 ROW=1,YICUREXT

*         Read the values.

*         Read the X axis.
            STATUS1=0
            NULL=1.0D0
            DVAL1=0.0D0
            CALL FTGCVD(UNIT,COLNX,ROW,
     :                  1,1,NULL,DVAL1,TEST1,STATUS1)

*         Read the Y axis.
            STATUS2=0
            NULL=1.0D0
            DVAL2=0.0D0
            CALL FTGCVD(UNIT,COLNY,ROW,
     :                  1,1,NULL,DVAL2,TEST2,STATUS2)

*         Read the Y errors axis.
            IF(YERROR) THEN
               STATUS3=0
               NULL=1.0D0
               DVAL3=0.0D0
               CALL FTGCVD(UNIT,COLNZ,ROW,
     :                     1,1,NULL,DVAL3,TEST3,STATUS3)
            ELSE
               STATUS3=0
               DVAL3=0.0
               TEST3=.FALSE.
            END IF

*         Check the status values. Reject the line if any are bad.
            IF((STATUS1.EQ.0).AND.(STATUS2.EQ.0)
     :                       .AND.(STATUS3.EQ.0)) THEN

*         Check the undefined values. Reject the line if any are found.
               IF((.NOT.TEST1).AND.(.NOT.TEST2)
     :                       .AND.(.NOT.TEST3)) THEN
                  NUMROWS=NUMROWS+1
               END IF

            END IF

 500     CONTINUE

*      Close the file.
         CALL FTCLOS(UNIT,STATUS)

*      Deal with a bad number of rows read.
         WRITE(*,*) 'Read ',NUMROWS,' out of ',YICUREXT
         IF(NUMROWS.LT.4) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) ' '
            WRITE(*,*) '** ERROR: Too few valid rows were found.'
            WRITE(*,*) ' '
            GO TO 900
         END IF

C      Dynamically allocate memory.
         IF ( NPTSARRAY(SLOT).NE.0 )
     :      CALL PERIOD_DEALL(YPTR(SLOT))
         CALL PERIOD_ALLOC('_DOUBLE', NUMROWS*MXCOL, YPTR(SLOT))

         CALL PERIOD_ALLOC('_DOUBLE', NUMROWS*MXCOL, JUNK2PTR)
         CALL PERIOD_ALLOC('_DOUBLE', NUMROWS, JUNK1PTR)
         CALL PERIOD_ALLOC('_INTEGER', NUMROWS, KEYPTR)

         YSLOT1 = YPTR(SLOT)

*      Open the OGIP FITS file again.
         STATUS=0
         READWRITE=0
         CALL FTOPEN(UNIT,FILENAME,READWRITE,BLOCKSIZE,STATUS)

*      Go to the desired extension.
         STATUS=0
         CALL FTMAHD(UNIT,CUREXT,HDUTYPE,STATUS)

*      Tell the user what is going on.
         WRITE(*,*)
         WRITE(*,*) 'Reading data into Slot=',SLOT
         WRITE(*,*) ' '

*      Read all the columns.

         THISROW = 0

         DO 600 ROW=1,YICUREXT

*         Read the values.

*         Read the X axis.
            STATUS1=0
            NULL=1.0D0
            DVAL1=0.0D0
            CALL FTGCVD(UNIT,COLNX,ROW,
     :                  1,1,NULL,DVAL1,TEST1,STATUS1)

*         Read the Y axis.
            STATUS2=0
            NULL=1.0D0
            DVAL2=0.0D0
            CALL FTGCVD(UNIT,COLNY,ROW,
     :                  1,1,NULL,DVAL2,TEST2,STATUS2)

*         Read the Y errors axis.
            IF(YERROR) THEN
               STATUS3=0
               NULL=1.0D0
               DVAL3=0.0D0
               CALL FTGCVD(UNIT,COLNZ,ROW,
     :                     1,1,NULL,DVAL3,TEST3,STATUS3)
            ELSE
               STATUS3=0
               DVAL3=0.0
               TEST3=.FALSE.
            END IF

*         Check the status values. Reject the line if any are bad.
            IF((STATUS1.EQ.0).AND.(STATUS2.EQ.0)
     :                       .AND.(STATUS3.EQ.0)) THEN

*         Check the undefined values. Reject the line if any are found.
               IF((.NOT.TEST1).AND.(.NOT.TEST2)
     :                       .AND.(.NOT.TEST3)) THEN
                  THISROW = THISROW + 1

*               Store the values.
                  CALL PERIOD_OGIPSTORE(DVAL1, DVAL2, DVAL3,
     :                                  THISROW,
     :                                  %VAL(CNF_PVAL(JUNK2PTR)),
     :                                  NUMROWS, MXCOL)
               END IF

            END IF

 600     CONTINUE

*      Close the file.
         CALL FTCLOS(UNIT,STATUS)

         IFAIL = 0

         CALL PERIOD_OGIPSORTCYCLE(%VAL(CNF_PVAL(YSLOT1)),
     :                             NUMROWS, MXCOL,
     :                             %VAL(CNF_PVAL(JUNK2PTR)),
     :                             %VAL(CNF_PVAL(JUNK1PTR)),
     :                             %VAL(CNF_PVAL(KEYPTR)), IFAIL)

         IF ( IFAIL.EQ.1 ) THEN

            CALL PERIOD_DEALL(KEYPTR)
            CALL PERIOD_DEALL(JUNK1PTR)
            CALL PERIOD_DEALL(JUNK2PTR)
            CALL PERIOD_DEALL(YPTR(SLOT))

            NPTSARRAY(SLOT)=0
            WRITE(*,*) ' '
            WRITE(*,*) '** WARNING: Nothing output to Slot=', SLOT
            WRITE(*,*) ' '
            GO TO 900
         END IF

         WRITE(*,*) ' '
         WRITE(*,*) '** OK: Filled Slot=', SLOT
         WRITE(*,*) ' '
         INFILEARRAY(SLOT)='OGIP FITS File=' // TEMPS

         YERRORARRAY(SLOT)=YERROR
         DETRENDARRAY(SLOT)=.FALSE.
         NPTSARRAY(SLOT)=NUMROWS
         NUMROWS=0

         CALL PERIOD_DEALL(KEYPTR)
         CALL PERIOD_DEALL(JUNK1PTR)
         CALL PERIOD_DEALL(JUNK2PTR)

 650     CONTINUE
         WRITE(*, '(X,A,$)') 'Would you like to read the ' //
     :                       'file again ? [N]  : '
         READ (*, '(A)', ERR=650) AGAINS
         CALL PERIOD_CASE(AGAINS, .TRUE.)

         AGAIN=0
         IF(AGAINS(1:1).EQ.'Y') AGAIN=1

      END DO

 900  CONTINUE


      RETURN
      END
