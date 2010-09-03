      SUBROUTINE SURF_SCUCAT (STATUS)
*+
*  Name:
*     SCUCAT

*  Purpose:
*     Concatenate photometry datasets for further processing

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SCUCAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*       The global status

*  Description:
*     This routine reads in a list of user specified files and concatenates
*     their data, variance and quality arrays so that KAPPA routines like
*     STATS and KSTEST can analyse a complete set of photometry observations.
*     If METHOD=SEPARATE (default) data for each individual bolometer is
*     written to a different file.
*     If a file contained data for H7 and H9 then two output files would
*     be created (eg test_h7 and test_h9 - if the OUT parameter was set to
*     `test'). For each new bolometer a new file is created (existing
*     files are overwritten) and data is appended to these files when
*     more data for these bolometers is supplied.
*     If METHOD=CATALL all data is written to a single file regardless
*     of the bolometer.

*  Usage:
*     scucat out in

*  ADAM Parameters:
*     BOL = CHAR (Read)
*       If the input file is an NDF (and not an HDS container as
*       expected) then this parameter should be given to tell the software
*       the bolometer that should be associated with this data.
*     IN = NDF (Read)
*       The input dataset(s). This parameter is requested repeatedly
*       until a NULL (!) value is given. The input dataset can either
*       be output from SCUPHOT or an NDF file. The GRP system is
*       used so that multiple files can be specified in response to
*       this parameter (eg wildcards or comma-separated list) or read
*       a text file (use the ^ character)
*     LOOP = LOGICAL (Read)
*       Turns the looping on (default is true) or off (false)
*     METHOD = CHAR (Read)
*       Concatentation method for bolometers.
*         SEPARATE: Store each bolometer (h7, c14 etc) in a separate
*                   output file based on OUT and the bolometer name.
*                   This is the default.
*         CATALL:   Combine all data into one file regardless of
*                   bolometer name.
*     MSG_FILTER = CHAR (Read)
*       Message filter level. Default is NORM.
*     OUT = CHAR (Write)
*       The root name of the output NDF if METHOD=SEPARATE.
*       The output filename if METHOD=CATALL

*  Examples:
*     scucat test phot
*       This routine will copy the data from phot to test_<bol>,
*       reducing multiple bolometers to individual files.
*       If the input set contained data
*       for bolometer H7 the output file will be test_h7.sdf.
*       The program will then ask for another data set.
*     scucat test ext_long noloop
*       This will copy all the data from ext_long.sdf to test_<bol>.sdf
*       and will then exit without asking further questions.
*     scucat in='^input.lis' out=test noloop
*       Read list of input files from the text file input.lis and
*       write the result using a root of "test".


*  Notes:
*     - SCUCAT can process output data from scuphot (eg file.sdf as an
*       HDS container containing NDF files with the names <bol>_peak) or
*       NDF files.
*     - If given an NDF the data array is vectorized so that the output
*       is 1-dimensional regardless of the shape of the input file.
*     - This task can also be used to simplify further processing of the
*       photometry data even if no data is to be concatenated (in this case
*       the task would be identical to the kAPPA task NDFCOPY).

*  Implementation Status
*     - NDF sections can not be used
*     - All input pixels are propogated to the output file

*  Related Applications:
*     SURF: SCUPHOT;
*     KAPPA: NDFCOPY, KSTEST

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.27  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.26  1999/08/03 20:01:41  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.25  1999/07/14 04:50:46  timj
*     Correct type of ADDED variable
*
*     Revision 1.24  1999/05/15 01:48:42  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*
*     Revision 1.23  1998/04/23 04:03:44  timj
*     Add METHOD parameter to decide on
*     whether to keep bolometers separate or not. Start writing HISTORY.
*
*     Revision 1.22  1998/04/23 01:05:51  timj
*     Use GRP for filename input.
*
*     Revision 1.21  1997/09/04 19:40:53  timj
*     Use SCULIB_GET_FILENAME
*
*     Revision 1.20  1997/06/27 23:16:39  timj
*     Tweak the header.
*
*     Revision 1.19  1997/06/13 00:10:44  timj
*     Use SURF_PAR and change name
*
*     Revision 1.18  1997/06/13 00:09:58  timj
*     Now read NDFs as well.
*     Close each file after writing instead of keeping NDF identifier.
*     Have LOOP and BOLS parameters.
*
*     Revision 1.17  1997/06/11 01:08:55  timj
*     Update documentation
*
*     Revision 1.16  1997/05/27 22:21:33  timj
*     BOL_LIST now ' ' - Alpha does not like null strings
*
*     Revision 1.15  1997/05/22 21:01:45  timj
*     Allow for 200 components in an input NDF.
*     (Necessary when using the ALLBOLS parameter in SCUPHOT)
*
*     Revision 1.14  1997/04/30 03:01:46  timj
*     Add MSG_OUTIF.
*
*     Revision 1.13  1997/03/31 21:18:29  timj
*     Use PACKAGE and TSKNAME.
*
*     Revision 1.12  1997/01/17 00:32:36  timj
*     Spell fixes to header.
*
c     Revision 1.11  1997/01/09  18:35:02  timj
c     Write a new NDF for each bolometer found in input files.
c
c     Revision 1.10  1996/12/06  18:31:59  timj
c     Now recognises multiple bolometers in file and writes out all bolometer
c     data to concatenated data.
c
c     Revision 1.9  1996/11/02  01:42:34  timj
c     Fix bug in Author/History header
c
c     Revision 1.8  1996/11/02  01:24:00  timj
c     Change Name to SCUCAT (from REDS_SCUCAT)
c
c     Revision 1.7  1996/10/30  20:23:05  timj
c     Add modern STARLINK header.
c     Replace SCULIB_COPY? with VEC_
c     Annul LOC after use.
c
c     Revision 1.6  1996/10/24  21:29:09  timj
c     Fixed GLOBAL default problem (use DUMMY PARAMETER)
c
c     Revision 1.5  1996/10/19  00:07:04  timj
c     Use GLOBAL.sdf and DAT_ASSOC, remove FILENAME(FILE)
c
c     Revision 1.4  1996/10/17  18:13:49  timj
c     Open OUT after reporting information on first input file
c
c     Revision 1.3  1996/09/18  19:14:18  timj
c     Change from CONCAT to SCUCAT
c
c     Revision 1.2  1996/09/18  02:16:51  timj
c     Add bad bit mask
c
c     Revision 1.1  1996/09/18  02:02:07  timj
c     Initial revision
c
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*     Type definitions:

      IMPLICIT NONE             ! No implicit typing allowed


*     Global constants:
      INCLUDE 'SAE_PAR'         ! SSE global definitions
      INCLUDE 'NDF_PAR'         ! NDF_ public constants
      INCLUDE 'MSG_PAR'         ! MSG__ constants
      INCLUDE 'PRM_PAR'         ! VAL__ constants
      INCLUDE 'PAR_ERR'         ! PAR_ error codes
      INCLUDE 'DAT_PAR'         ! Data-system constants
      INCLUDE 'SURF_PAR'        ! SCUBA constants
      INCLUDE 'GRP_PAR'         ! GRP__ constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*     Status:
      INTEGER STATUS

*     External references:
      INTEGER  CHR_LEN          ! Length of string
      EXTERNAL CHR_LEN

*     Local constants:
      INTEGER MAXCMP            ! Max number of bolometers in an HDS
      PARAMETER (MAXCMP = 200)
      INTEGER MAXBOLS           ! Max number of bolometers used
      PARAMETER (MAXBOLS = SCUBA__NUM_CHAN * SCUBA__NUM_ADC )
      CHARACTER * 10 TSKNAME    ! Name of task
      PARAMETER (TSKNAME = 'SCUCAT')

*     Local variables:
      CHARACTER*80  AXLABEL     ! Axis label
      INTEGER       ADDED       ! Number added to group this time
      BYTE          BADBIT      ! Bad bit mask
      CHARACTER*3   BOL(MAXCMP) ! Name of bolometers present in NDF
      INTEGER      BOL_INDEX(MAXCMP) ! Which entry in BOLUSED is the current bol
      CHARACTER*3   BOLUSED(MAXBOLS) ! Name of bolometers used so far
      CHARACTER*(MAXCMP*4) BOL_LIST ! List of all bolometers in NDF
      INTEGER       EL          ! Number of input data points
      INTEGER       FILE        ! File count
      CHARACTER *132 FILENAME   ! Filename
      LOGICAL       FLAG        ! GRP flag
      INTEGER       GRP         ! Group counter
      INTEGER       I           ! Loop counter
      INTEGER       IERR        ! Location of error during VEC_ copy
      INTEGER       IGRP        ! Group identifier
      INTEGER       INDEX       ! Bolometer reference in big bol list
      INTEGER       IN_NDF(MAXCMP) ! Input NDF identifiers
      INTEGER       IN_DATA_PTR ! Pointer to D array
      INTEGER       IN_VAR_PTR  ! Pointer to V array
      INTEGER       IN_QUAL_PTR ! Pointer to Q array
      INTEGER       IPOSN       ! Position in string
      INTEGER       ITEMP       ! Temporary integer
      CHARACTER*80  LABEL       ! data label
      INTEGER       LBND(MAXBOLS) ! Lower bound of output array
      CHARACTER*(DAT__SZLOC) LOC ! Locator to root HDS file
      LOGICAL       LOOP        ! value of loop parameter
      LOGICAL       LOOPING     ! Controls read loop
      INTEGER       LOWER       ! Lower bound of section
      CHARACTER*10  MODE        ! Access mode for output
      CHARACTER*10  METHOD      ! Concatenation method
      INTEGER       N           ! Bolometer counter
      CHARACTER*15  NAME(MAXCMP) ! Names of NDFs
      INTEGER       NBOLUSED    ! Number of bolometers catted
      INTEGER       NCOMP       ! Number of components in HDS
      INTEGER       NERR        ! Number of errors during VEC_ copy
      CHARACTER*(DAT__SZLOC) NLOC ! Loc to  Bol NDF inside HDS file
      INTEGER       NMEMBERS    ! Number of members in group
      INTEGER       NUM_NDF     ! Counter of each sub NDF
      INTEGER       N_PHOT      ! Number of bolometers in an HDS
      CHARACTER*(132) OUTFILE   ! Name of output file
      CHARACTER*(132) OUTROOT   ! Rootname of output file
      INTEGER       OUTPLACE    ! Output place holder
      LOGICAL       OUT_EXIST(MAXBOLS) ! Have we used the output NDF yet
      INTEGER       OUT_NDF     ! Output NDF identifiers
      INTEGER       OUT_APTR    ! Pointer to Axis array
      INTEGER       OUT_DATA_PTR ! Pointer to D array
      INTEGER       OUT_VAR_PTR ! Pointer to V array
      INTEGER       OUT_QUAL_PTR ! Pointer to Q array
      LOGICAL       READING     ! Logical to control file reading
      LOGICAL       READPARAM   ! Have we read the IN parameter?
      INTEGER       SEC_NDF     ! NDF identifier to NDF section
      CHARACTER*40  TITLE       ! Title of observation
      LOGICAL       THERE       ! A bolometer has already been catted
      CHARACTER*(DAT__SZTYP) TYPE ! Type of structure associated with locator
      INTEGER       UBND(MAXBOLS) ! Upper bound of output array
      CHARACTER*80  UNITS       ! units
      LOGICAL       USE_OUT     ! Have I opened an output file
*.

      IF (STATUS .NE. SAI__OK) RETURN




*     start up the NDF system and read in the input photometry data

      CALL NDF_BEGIN

      USE_OUT = .FALSE.
      READING = .TRUE.
      LOOPING = .TRUE.
      FILE = 0
      NBOLUSED = 0
      READPARAM = .FALSE.

*     Initialise the output NDFs
      DO I = 1, MAXBOLS
         OUT_EXIST(I) = .FALSE.
      END DO

*     Ask for the concatenation method
      CALL PAR_CHOIC('METHOD', 'SEPARATE','SEPARATE,CATALL', .TRUE.,
     :     METHOD, STATUS)

*     Ask for the output root name
      CALL PAR_GET0C('OUT', OUTROOT, STATUS)

*     Start loop
      DO WHILE (LOOPING)

*     Obviously we would like to think we are reading something at this
*     point.

         READING = .TRUE.

*     Check to see if we are looping
*     Make sure that we do loop once if there was an eror from the read
         CALL PAR_GET0L('LOOP', LOOP, STATUS)


*     read the name of the file to be read

*     Read in the GLOBAL value first
         IF (.NOT.READPARAM) THEN
            CALL SCULIB_GET_FILENAME('DUMMY', FILENAME, STATUS)
            CALL PAR_DEF0C('IN', FILENAME, STATUS)
         ELSE
*     Make sure the parameter is cancelled and DEFAULT (NULL) used
            CALL PAR_CANCL('IN', STATUS)
            CALL PAR_UNSET('IN', 'DEFAULT', STATUS)
         END IF

*     Read in the latest file

         IF (STATUS .EQ. SAI__OK) THEN
*     CALL PAR_GET0C('IN', FILENAME, STATUS)

*     Cancel the IN parameter if we have looped round and are
*     reading in the second file
*            IF (FILE .EQ. 1) CALL PAR_CANCL('IN', STATUS)

*     Create a new group
            CALL GRP_NEW('Input files', IGRP, STATUS )

*     Read in the group members
            CALL GRP_GROUP('IN', GRP__NOID, IGRP, NMEMBERS, ADDED,
     :           FLAG, STATUS)

*     Set a flag to say that we have read the parameter once
            READPARAM = .TRUE.

            IF (STATUS .EQ. PAR__NULL .OR. FILENAME.EQ.'!') THEN
*     Finish looping
               CALL ERR_ANNUL(STATUS)
               LOOPING = .FALSE.
               READING = .FALSE.

            ELSE IF (STATUS .EQ. PAR__ABORT) THEN
*     Just finish with bad status
               LOOPING = .FALSE.
               READING = .FALSE.

            ELSE
*     Loop through all group members

               IF (STATUS .NE. SAI__OK) NMEMBERS = 0

*     Loop over all members of group

               DO GRP = 1, NMEMBERS

                  CALL GRP_GET(IGRP, GRP, 1, FILENAME, STATUS)

                  N_PHOT = 0

*     Increment the file counter
                  IF (STATUS .EQ. SAI__OK) FILE = FILE + 1


*     Try to open the HDS file and find out how many components are there
                  CALL HDS_OPEN(FILENAME, 'READ', LOC, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL DAT_NCOMP(LOC, NCOMP, STATUS)

                     DO I = 1, MIN(NCOMP, MAXCMP)
                        CALL DAT_INDEX(LOC, I, NLOC, STATUS)

                        CALL DAT_NAME(NLOC, NAME(I), STATUS)

                        ITEMP = CHR_LEN(NAME(I))
                        CALL CHR_FIND(NAME(I), '_PEAK', .FALSE., ITEMP)

*     A photometry observation
                        IF (ITEMP .GT. 0 .AND.
     :                       ITEMP .LT. CHR_LEN(NAME(I))) THEN
                           N_PHOT = N_PHOT + 1
                           BOL(N_PHOT) = NAME(I)(:ITEMP-1)
                           CALL CHR_LCASE(BOL(N_PHOT))

*     Get an NDF identifier for the NDF
                           CALL NDF_FIND(NLOC, ' ', IN_NDF(N_PHOT),
     :                          STATUS)
                        END IF

                        CALL DAT_ANNUL(NLOC, STATUS)
                     END DO

*     Check that we actually read some data in (just need to look at
*     N_PHOT and STATUS)
*     It is also possible that the user has given an NDF as input
*     so we need to make sure we can deal with that

                     IF (N_PHOT .LE. 0) THEN

*     Find the type of structure associated with this Locator

                        CALL DAT_TYPE(LOC, TYPE, STATUS)

*     If type is an NDF then open it
*     Note that I dont care about dimensionality of the NDF

                        IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'NDF') THEN
                           N_PHOT = 1 ! Can only be one input bolometer

                           CALL NDF_FIND(LOC, ' ', IN_NDF(N_PHOT),
     :                          STATUS)

                           IF (STATUS .EQ. SAI__OK) THEN

*     Now I need to actually ask for the bolometer name
*     so long as we are processing data separately
*     (if we are combining all then no need to ask)

                              IF (METHOD .EQ. 'SEPARATE') THEN
                                 CALL PAR_GET0C('BOL', BOL(N_PHOT),
     :                                STATUS)
                                 CALL CHR_LCASE(BOL(N_PHOT))
                                 CALL PAR_CANCL('BOL', STATUS)
                              ELSE
                                 BOL(N_PHOT) = 'UNKNOWN'
                              END IF

                              IF (STATUS .EQ. PAR__ABORT) THEN

                                 N_PHOT = 0
                                 LOOPING = .FALSE.
                                 READING = .FALSE.

                              ELSE IF (STATUS .NE. SAI__OK) THEN

                                 N_PHOT = 0
                                 READING = .FALSE.
                                 CALL ERR_ANNUL(STATUS)

                              ELSE
*     Everything is okay so I set the LOOP

                                 LOOPING = LOOP

                              END IF

                           ELSE

                              N_PHOT = 0
                              READING = .FALSE.
                              CALL MSG_SETC('TASK',TSKNAME)
                              CALL MSG_SETC('FILE',FILENAME)

                              CALL ERR_REP(' ', '^TASK: Error '//
     :                             'opening the NDF ^FILE', STATUS)

                              CALL ERR_FLUSH(STATUS)
                              FILE = FILE - 1

                           END IF

*     If the locator wasn't associated with an NDF then just report
*     back that I dont know what to do with the file

                        ELSE

                           IF (STATUS .EQ. SAI__OK) THEN
                              STATUS = SAI__ERROR
                           END IF

                           CALL MSG_SETC('TASK',TSKNAME)
                           CALL MSG_SETC('FILE',FILENAME)

                           CALL ERR_REP(' ', '^TASK: ^FILE does not '//
     :                          'seem to contain photometry data '//
     :                          'or an NDF', STATUS)
                           CALL ERR_FLUSH(STATUS)
                           FILE = FILE - 1
                           READING = .FALSE.

                        END IF

                     ELSE

*     End looping if LOOP is set (since we read a file okay)
                        LOOPING = LOOP

                     END IF

*     Finish with LOC
                     CALL DAT_ANNUL(LOC, STATUS)

                  ELSE
                     CALL MSG_SETC('TASK',TSKNAME)
                     CALL MSG_SETC('FILE',FILENAME)
                     STATUS = SAI__ERROR
                     CALL ERR_REP(' ', '^TASK: Failed to open file '//
     :                    '^FILE', STATUS)

                     CALL ERR_FLUSH(STATUS)
                     FILE = FILE - 1
                  END IF

*     END IF

*     Have I found a photometry observation
                  IF (N_PHOT .GT. 0 .AND. STATUS .EQ. SAI__OK) THEN

*     Sort the bolometer list

                     BOL_LIST = ' '
                     IPOSN = 0
                     DO I =  1, N_PHOT
                        IF(IPOSN.GT.0)
     :                       CALL CHR_APPND(', ',BOL_LIST,IPOSN)
                        CALL CHR_APPND(BOL(I),BOL_LIST, IPOSN)
                     END DO

                     CALL MSG_SETC('BOL',BOL_LIST)
                     CALL MSG_SETC('PKG', PACKAGE)
                     CALL MSG_OUTIF(MSG__NORM, ' ',
     :                    '^PKG: Found data for the following '//
     :                    'bolometers: ^BOL', STATUS)

*     Find out if we already used this one
                     DO I = 1, N_PHOT
                        THERE = .FALSE.
                        DO ITEMP = 1, NBOLUSED
                           IF (BOLUSED(ITEMP) .EQ. BOL(I)) THEN
                              THERE = .TRUE.
                              BOL_INDEX(I) = ITEMP
                           END IF
                        END DO
                        IF (.NOT.THERE) THEN
                           NBOLUSED = NBOLUSED + 1
                           BOLUSED(NBOLUSED) = BOL(I)
                           BOL_INDEX(I) = NBOLUSED
                        END IF
                     END DO

                  ELSE

                     READING = .FALSE.

                  END IF

*     END of If (status ok)
*     END IF

                  IF (READING.AND. STATUS .EQ. SAI__OK) THEN

*     Loop through all bolometers
                     DO NUM_NDF = 1, N_PHOT

*     Map the input file
                        CALL NDF_SQMF(.FALSE., IN_NDF(NUM_NDF), STATUS)
                        CALL NDF_MAP(IN_NDF(NUM_NDF), 'QUALITY',
     :                       '_UBYTE','READ', IN_QUAL_PTR, EL, STATUS)
                        CALL NDF_MAP(IN_NDF(NUM_NDF), 'Data,','_REAL',
     :                       'READ', IN_DATA_PTR, EL, STATUS)
                        CALL NDF_MAP(IN_NDF(NUM_NDF), 'Variance',
     :                       '_REAL', 'READ', IN_VAR_PTR, EL, STATUS)

*     Give some information about this data

                        IF (NUM_NDF .EQ. 1) THEN

*     Name of source, label and units
                           CALL NDF_CGET(IN_NDF(NUM_NDF), 'Title',
     :                          TITLE, STATUS)
                           CALL NDF_CGET(IN_NDF(NUM_NDF), 'Units',
     :                          UNITS, STATUS)
                           CALL NDF_CGET(IN_NDF(NUM_NDF), 'Label',
     :                          LABEL, STATUS)
                           CALL NDF_ACGET(IN_NDF(NUM_NDF), 'Label',
     :                          1,AXLABEL, STATUS)

*     Number of integrations
                           CALL MSG_SETI('NINT', EL)
                           CALL MSG_SETC('TITLE', TITLE)
                           CALL MSG_SETC('UNT', UNITS)
                           CALL MSG_SETC('LAB', AXLABEL)
                           CALL MSG_SETC('PKG', PACKAGE)

                           CALL MSG_OUTIF(MSG__NORM, ' ',
     :                          '^PKG: This is a PHOTOM '//
     :                          'observation of ^TITLE. There are '//
     :                          '^NINT ^LAB. (units=^UNT)', STATUS)
                        END IF

*     Current index of bolometer in full bolometer list

                        INDEX = BOL_INDEX(NUM_NDF)

*     I cant simply store the NDF identifiers of opened files.
*     There are too many open files in some cases (ie if ALLBOLS
*     is used for SCUPHOT). I therefore have to completely close
*     and open each file every time I want to append data to it.
*     This is a bit of an overhead but I cant think of anyway
*     to avoid it if I can not have 100 files open at once (and the
*     associated NDF identifiers). Hopefully this will also solve
*     the other problem I have with ALLBOLS in that input data takes
*     91 NDF identifiers before we even start!
*     Simply use an array of logicals to determine whether I have
*     already appended data to a given ndf. Shouldnt be too much
*     of a complication even if we lose efficiency.

*     Open the output file

*     First work out the filename

                        IF (METHOD .EQ. 'SEPARATE') THEN

                           IPOSN = CHR_LEN(OUTROOT)
                           OUTFILE = OUTROOT
                           CALL CHR_APPND('_', OUTFILE, IPOSN)
                           CALL CHR_APPND(BOLUSED(INDEX) ,OUTFILE,
     :                          IPOSN)

                        ELSE IF (METHOD .EQ. 'CATALL') THEN

                           OUTFILE = OUTROOT
                           INDEX = 1  ! Always use same index

                        END IF


                        IF (OUT_EXIST(INDEX)) THEN
*     Open a file which we have opened sometime earlier
                           CALL NDF_OPEN(DAT__ROOT, OUTFILE, 'UPDATE',
     :                          'OLD', OUT_NDF, OUTPLACE, STATUS)

                        ELSE
*     In this case we must create a new file

*     Dimensions
                           LBND(INDEX) = 1
                           UBND(INDEX) = 2

*     Make NDF
                           CALL NDF_PLACE(DAT__ROOT, OUTFILE, OUTPLACE,
     :                          STATUS)
                           CALL NDF_NEW('_REAL', 1, LBND(INDEX),
     :                          UBND(INDEX), OUTPLACE, OUT_NDF, STATUS)
                           UBND(INDEX) = 0

*     Dont forget to register the file as open
                           IF (STATUS .EQ. SAI__OK) THEN
                              OUT_EXIST(INDEX) = .TRUE.
                           END IF

*     Start history recording
                           CALL NDF_HCRE(OUT_NDF, STATUS)

*     Setup the axis and title labels
*     These should be propagated from the first input
*     image since the data may have been calibrated
*     Do not want to check that units are consistent...
                           CALL NDF_ACPUT(AXLABEL, OUT_NDF,
     :                          'LABEL', 1, STATUS)
                           CALL NDF_CPUT(UNITS, OUT_NDF, 'UNITS',
     :                          STATUS)
                           CALL NDF_CPUT(TITLE, OUT_NDF, 'Title',
     :                          STATUS)
                           CALL NDF_CPUT(LABEL, OUT_NDF, 'LAB',
     :                          STATUS)

*     Set all bits to bad
                           BADBIT = VAL__BADUB
                           CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)
*     Just in case
                           CALL NDF_SBAD(.TRUE., OUT_NDF,
     :                          'Data,Variance' ,STATUS)

                        END IF


*     Change the bounds
                        UBND(INDEX) = UBND(INDEX) + EL
                        CALL NDF_SBND(1, LBND(INDEX), UBND(INDEX),
     :                       OUT_NDF, STATUS)

*     Get the new NDF section
                        LOWER = UBND(INDEX) - EL + 1
                        CALL NDF_SECT(OUT_NDF, 1, LOWER,
     :                       UBND(INDEX), SEC_NDF, STATUS)

*     Map the output data arrays
                        MODE = 'WRITE'
                        CALL NDF_MAP(SEC_NDF, 'QUALITY', '_UBYTE', MODE,
     :                       OUT_QUAL_PTR, ITEMP, STATUS)
                        CALL NDF_MAP(SEC_NDF, 'Data', '_REAL', MODE,
     :                       OUT_DATA_PTR, ITEMP, STATUS)
                        CALL NDF_MAP(SEC_NDF, 'Variance', '_REAL', MODE,
     :                       OUT_VAR_PTR, ITEMP, STATUS)

*     And map the axis
*     This is pretty inefficient. It seems that I cant just
*     fill the section with integers. (Ive tried it by mapping SEC_NDF)
*     but it works if I map the whole axis and overwrite it every time.
*     Hopefully people wont notice this!!!
                        CALL NDF_AMAP(OUT_NDF, 'CENTRE', 1, '_INTEGER',
     :                       MODE, OUT_APTR, ITEMP, STATUS)

*     Copy the input data into the output data
                        CALL VEC_RTOR(.FALSE., EL,
     :                                %VAL(CNF_PVAL(IN_DATA_PTR)),
     :                       %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :                       IERR, NERR, STATUS)
                        CALL VEC_RTOR(.FALSE., EL,
     :                                %VAL(CNF_PVAL(IN_VAR_PTR)),
     :                       %VAL(CNF_PVAL(OUT_VAR_PTR)),
     :                       IERR, NERR, STATUS)
                        CALL VEC_UBTOUB(.FALSE., EL,
     :                                  %VAL(CNF_PVAL(IN_QUAL_PTR)),
     :                       %VAL(CNF_PVAL(OUT_QUAL_PTR)),
     :                       IERR, NERR, STATUS)

*     Copy the axis values (just integers)
*     Should really just copy from LOWER to UPPER of a section
*     but see above.
                        ITEMP = 0
                        DO N = 1, UBND(INDEX)
                           CALL VEC_ITOI(.FALSE., 1, N,
     :   %VAL(CNF_PVAL(OUT_APTR) + (ITEMP * VAL__NBI)),
     :                          IERR, NERR, STATUS)
                           ITEMP = ITEMP + 1
                        END DO

*     Tidy up

                        CALL NDF_UNMAP(IN_NDF(NUM_NDF), '*', STATUS)
                        CALL NDF_ANNUL(IN_NDF(NUM_NDF), STATUS)

*     Unmap the output array
                        CALL NDF_UNMAP(SEC_NDF, '*', STATUS)
                        CALL NDF_ANNUL(SEC_NDF, STATUS)

*     and close the output NDF
                        CALL NDF_ANNUL(OUT_NDF, STATUS)

                     END DO

                  END IF

               END DO           ! End of GRP DO

*     Close the GROUP
               CALL GRP_DELET(IGRP, STATUS)

            END IF

         END IF

*     break out of loop if status has gone bad

         IF (STATUS .NE. SAI__OK) THEN
            LOOPING = .FALSE.
            READING = .FALSE.
         END IF

      END DO


*     Check that we concatenated some data
*     Report an error if we didnt do anything

      IF (NBOLUSED .LE. 0) THEN

         CALL MSG_SETC('TASK', TSKNAME)
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', '^TASK: there was no '//
     :           'input data', STATUS)
         ELSE
            CALL ERR_REP(' ','^TASK: No data found', STATUS)
         END IF

      END IF

      CALL NDF_END(STATUS)

      END
