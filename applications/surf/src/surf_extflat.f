      SUBROUTINE SURF_EXTFLAT (STATUS)
*+
*  Name:
*     EXTRACT_FLAT

*  Purpose:
*     Extract a flatfield from a SCUBA demodulated data file

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_EXTFLAT( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine extracts the flatfield information from a SCUBA
*     demodulated data file and writes it out in a format suitable for
*     use by CHANGE_FLAT.
*     The full flatfield is extracted: Bolometer positions and relative
*     responsivities.

*  Usage:
*     extract_flat in out

*  ADAM parameters:
*     IN = NDF (Read)
*        The name of the NDF containing the demodulated data with the
*        required flatfield.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = FILE (Write)
*        The name of the ascii file to which the flatfield information
*        will be written

*  Examples:
*     flatfield 19971017_dem_0002 oldflat.dat
*        This will read the flatfield from 19971017_dem_0002.sdf and
*        write it to a text file

*  Related Applications:
*     SURF: CHANGE_FLAT, FLATFIELD

*  Algorithm:
*        The data array of the IN file should have dimensions (N_BOLS,N_POS) 
*     for most observing modes, where N_BOLS was the number of bolometers 
*     measured and N_POS the number of times they were measured. In PHOTOM 
*     mode the data array will have dimensions (N_BOLS,N_POS,3), where the 
*     last dimension reflects the 3 different reduction algorithms used by 
*     the REDUCE_SWITCH application depending which `beam' the bolometer was 
*     assumed to be working in.
*        The identities of the bolometers taking the data are read from the
*     .MORE.SCUBA.BOL_CHAN and .MORE.SCUBA.BOL_ADC arrays in the IN file
*     and the appropriate flatfield values read from the .MORE.SCUBA.BOL_CALB
*     array. The data for each bolometer are multiplied by the corresponding
*     flatfield value and written to the OUT file.
*        The application will not run on input files that have not previously
*     been processed by the REDUCE_SWITCH application or which have already
*     had FLATFIELD run on them.

*  Authors:
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (JACH)

*  History:
*     $Id$
*     18-JUN-1996: Original version.
*     $Log$
*     Revision 1.1  1997/10/28 02:52:41  timj
*     Initial revision
*
*     Revision 1.12  1997/09/03 23:58:31  timj
*     Automatically supply the output filename.
*
*     Revision 1.11  1997/07/03 19:05:40  timj
*     Propogate axes to output
*
*     Revision 1.10  1997/06/12 23:49:12  timj
*     Doc updates
*     Use SURF_PAR and change name
*
*     Revision 1.9  1997/04/30 02:31:08  timj
*     Rationalise calling.
*     Add MSG_OUTIF, PKG and TSKNAME.
*
*     Revision 1.8  1997/03/06 20:03:05  timj
*     tweak header
*
c Revision 1.7  1996/11/02  01:39:40  timj
c Remove space from History : header
c
c Revision 1.6  1996/11/02  01:23:28  timj
c Change name to FLATFIELD
c
c Revision 1.5  1996/10/31  18:22:55  timj
c Added modern Starlink header.
c
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'                ! SSE global definitions
      INCLUDE 'DAT_PAR'                ! Data-system constants
      INCLUDE 'SURF_PAR'               ! SURF constants
      INCLUDE 'MSG_PAR'                ! MSG__ constants

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      CHARACTER * 13   TSKNAME         ! Name of this task
      PARAMETER (TSKNAME = 'EXTRACT_FLAT')
      INTEGER RECLEN                   ! length of record written to file
      PARAMETER (RECLEN = 132)         !


*    External functions:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN


*    Local variables :
      BYTE             BADBIT          ! Bad bit mask
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      REAL             BOL_CALB (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! flat-field values for bolometers
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU4 Nasmyth coord of bolometers
      INTEGER          BOL_QUAL (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! quality of bolometers
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! bolometer types
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      INTEGER          DIMX (MAX_DIM)  ! expected array dimensions
      INTEGER          FD              ! File descriptor
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                       ! array of FITS keywords
      CHARACTER*80     FLAT            ! name of file that originally contained
                                       ! the flatfield
      CHARACTER*132    FNAME           ! Name of input file
      INTEGER          I               ! DO loop index
      CHARACTER*3      ID              ! Bolometer ID
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                       ! locator to FITS extension in input
                                       ! file
      INTEGER          IN_NDF          ! NDF index of input file
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                       ! locator to SCUBA extension in input
                                       ! file
      INTEGER          ITEMP           ! scratch integer
      INTEGER          J               ! Do loop index
      CHARACTER*(RECLEN) LINE          ! Line to be written to file
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NTICKS          ! Number of ticks since some data
      INTEGER          N_BOL           ! number of bolometers measured in input
                                       ! array
      INTEGER          N_FITS          ! number of items in FITS array
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      CHARACTER*3      REF             ! Reference pixel
      INTEGER          RUN_NUMBER      ! run number of input file

*  Local Data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)


*  start up the NDF system and read in the input demodulated file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', IN_NDF, STATUS)

*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)

* Read in badbit mask
      CALL NDF_BB(IN_NDF, BADBIT, STATUS)


*     Look for the SCUBA extension containing the flatfield

      IN_SCUBAX_LOC = DAT__NOLOC
      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','This file does not seem to be a SCUBA'//
     :           'data file', STATUS)

         END IF
      END IF

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file contains '//
     :        'too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :  STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM,' ', 
     :     '^PKG: run ^RUN was a ^MODE observation of ^OBJECT',
     :     STATUS)


*  get the number of bolometers and the name of the flatfield 

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOL, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'FLAT',
     :  FLAT, STATUS)


*  get the bolometer description arrays

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOL, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

*     Now get the additional information required for the flatfield

      NDIM = 2
      DIMX (1) = SCUBA__NUM_CHAN
      DIMX (2) = SCUBA__NUM_ADC

      CALL CMP_GETNR (IN_SCUBAX_LOC, 'BOL_CALB', NDIM, DIMX, BOL_CALB, 
     :     DIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :       (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :       (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: .SCUBA.BOL_CALB '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :        STATUS)
         END IF
      END IF

      CALL CMP_GETNI (IN_SCUBAX_LOC, 'BOL_QUAL', NDIM, DIMX, BOL_QUAL, 
     :     DIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :       (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :       (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: .SCUBA.BOL_QUAL '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :        STATUS)
         END IF
      END IF

*  flatfield the data 

      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_SETC ('FLAT', FLAT)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: extracting flatfield ^FLAT', STATUS)

*     Open an output file (Would be nice to be able to write to STDOUT)
      CALL PAR_DEF0C('FILE', FLAT, STATUS)
      CALL FIO_ASSOC('FILE', 'WRITE', 'LIST', 0, FD, STATUS)

*     Write out a small header before we get to the numbers

*     The command to open the flatfield file
      CALL FIO_WRITE(FD, 'proc SET_BOLS', STATUS)

*     General information
      CALL FIO_WRITE(FD, '{ flat field data file written by '//
     :     'the '//PACKAGE// ' task '//TSKNAME, STATUS)

*     Date written
      LINE = '{ Written on: '
      ITEMP = CHR_LEN(LINE)
      CALL PSX_TIME(NTICKS, STATUS)
      CALL PSX_CTIME(NTICKS, LINE(ITEMP+2:), STATUS)
      CALL FIO_WRITE(FD, LINE, STATUS)

*     Input file
      CALL FIO_WRITE(FD,'{ Extracted from file '//FNAME, STATUS)

*     Original flatfield name
      CALL FIO_WRITE(FD,'{ Original flatfield name: '//FLAT, STATUS)


      CALL FIO_WRITE(FD, ' ', STATUS)

*     Write the table header
      CALL FIO_WRITE(FD,
     :     '{      Name Type         dU3         dU4         Calib  '//
     :     '     Theta       A           B         Qual', STATUS)

*     Dont know the reference pixel
      REF = 'UNK'


*     Loop through each good bolometer and print out the flatfield

      DO J = 1, SCUBA__NUM_ADC
         DO I = 1, SCUBA__NUM_CHAN

            IF (BOL_TYPE(I,J) .NE. 'BAD') THEN

*     Get the name of the 
               CALL SCULIB_BOLNAME(J, I, ID, STATUS)

*     Construct the flatfield line
               WRITE(LINE,10) ID, BOL_TYPE(I,J), BOL_DU3(I,J), 
     :              BOL_DU4(I,J), BOL_CALB(I,J), 0.0, 0.0, 0.0, 
     :              BOL_QUAL(I,J), REF

*     Write the line
               CALL FIO_WRITE(FD, LINE, STATUS)


            END IF

         END DO
      END DO

*     Format for the flatfield
 10   FORMAT(' SETBOL ',a3,1x,a11,1x,6(E11.4,1x),1x,i1,3x,'0.0 0',2x,a3)


*     End the file
      CALL FIO_WRITE(FD, 'end proc', STATUS)


*     Close the file
      CALL FIO_CANCL('FILE', STATUS)

*  annul locators and array identifiers and close the file

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)

      CALL NDF_ANNUL (IN_NDF, STATUS)

      CALL NDF_END (STATUS)
 
      END
