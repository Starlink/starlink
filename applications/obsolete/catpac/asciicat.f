      SUBROUTINE ASCIICAT( STATUS )
*+
*  Name:
*     ASCIICAT

*  Purpose:
*     Create a catalogue from an ASCII file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASCIICAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create a new catalogue that contains the data from an ASCII file.
*     ASCIICAT allows you to put your own data into a catalogue. The
*     data must be tabular. Before running this application you should
*     examine your data and decide which columns in the data
*     are to included in the catalogue. For each of these fields note
*     the start position and the FORTRAN format that should be used to
*     read the data in this field. A wide range of sexagesimal formats are
*     also available. A good idea for finding the correct
*     start positions is to copy the first few entries of the data into
*     a separate file and include a first line containing
*     123456789012.....
*     Create in information file for the catalogue containing
*     information about each field. Name, format, units, comments and
*     start position.
*     EG.
*     STARNAME
*     A6
*     none
*     "Name of the star"
*     1
*     MAG
*     I2
*     mag
*     "Magnitude of the star"
*     7
*     etc
*
*     The
*     application first prompts for the name of the catalogue being
*     created and the name of the file from which the data is to be
*     read and then for the information file.
*
*     This application creates a Regular catalogue which contains only
*     real columns. Virtual columns, Catnotes, Cathelp and Parameters
*     may be added to the catalogue later. Columns may only be scalar
*     values, no arrays or structures in this version. Columns can not
*     have an assert expression and all the values are none null.

*  Usage:
*     ASCIICAT INPUT DATAFILE INFOFILE

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue to be created.
*     DATAFILE = _CHAR (Read)
*        Name of the file containing the tabular ascii data.
*     INFOFILE = _CHAR (Read)
*        Name of the file containing the field information.

*  Example:
*     ASCIICAT TEST TESTDATA.DAT TESTINFO.DAT

*  Notes:
*     This application performs no checking on your column definition.
*     This allows a degree of flexibility when interpreting your data.
*     You may, for example, have a column STARID with format A10
*     starting at position 15 and column STARNUM with format I4 starting
*     at position 21.

*  Authors:
*     ARW: Alan Wood (STARLINK)

*  History:
*     11-OCT-1991 (ARW):
*        Original version.

*  Bugs:
*     None known.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'PAR_ERR'   ! PAR errors
      INCLUDE 'CHP_PAR'   ! CHP constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 256 ) BUFF1
      INTEGER START
      INTEGER FD
      INTEGER NUMC1
      INTEGER LENGTH
      INTEGER LENGTH1
      LOGICAL CHR_LEN
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name.
      CHARACTER * ( 32 ) FILENAME ! Name of file containing the
      CHARACTER * ( 32 ) DATAFILE ! Name of file containing the
                                           ! tabular ASCII data.
      CHARACTER * ( CHP__SZCNAME ) CNAMES(CHP__NUMCOLS) ! Name of the
                                                        ! columns.
      CHARACTER * ( CHP__SZCFMT ) CFORMATS(CHP__NUMCOLS) ! Format of
                                                           ! the columns.
      CHARACTER * ( CHP__SZCFMT ) INCFORMATS(CHP__NUMCOLS) ! Format of
                                                           ! the columns.
      CHARACTER * ( CHP__SZCUNIT ) CUNITS(CHP__NUMCOLS) ! Units of the
                                                       ! columns.
      CHARACTER * ( CHP__SZCCMT ) CCOMMENTS(CHP__NUMCOLS) ! Comments of the
                                                         ! the columns.
      CHARACTER * ( CHP__SZCNAME ) CNAME ! Name of the column.
      CHARACTER * ( CHP__SZCFMT ) CFORMAT! Format of the column.
      CHARACTER * ( CHP__SZCUNIT ) CUNIT ! Units of the column.
      CHARACTER * ( CHP__SZCCMT ) CCOMMENT ! Comment for the column.
      INTEGER STARTPOS(CHP__NUMCOLS) ! Start positions of columns in the
                                     ! data.
      INTEGER STPOS ! Start position of field.
      INTEGER NUMCOL ! Number of columns.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0c('DATAFILE', datafile, status)
       call par_get0c('INFOFILE', filename, status)
*
*   Loop prompting for the column details
*
*
      call fio_open(filename, 'READ', 'FORTRAN', 256, fd, status)
      numcol = 1
      do while (status .eq. SAI__OK)
        call fio_read(fd, buff1, numc1, status)
        cnames(numcol) = buff1(1:numc1)
        call fio_read(fd, buff1, numc1, status)
        incformats(numcol) = buff1(1:numc1)
        call fio_read(fd, buff1, numc1, status)
        cunits(numcol) = buff1(1:numc1)
        call fio_read(fd, buff1, numc1, status)
        ccomments(numcol) = buff1(1:numc1)
        call fio_read(fd, buff1, numc1, status)
        call chr_ctoi(buff1(1:numc1), start, status)
        startpos(numcol) = start
*
            numcol = numcol + 1
       enddo
        call err_annul(status)
        numcol = numcol - 2
*
        call fio_close(fd, status)
       if (status .eq. SAI__OK) then
*
*    Make the call.
*
         call chu_incat(incat, datafile, numcol, cnames, incformats,
     :  cunits, ccomments, startpos, status)
*
       call chp_close(status)
*
*    Display the results
*
        if (status .eq. SAI__OK) then
         call msg_setc('catname',incat)
         call msg_out('message 1','The catalogue ^catname has been
     : created', status)
        else
         call err_rep('message 2','An unidentified error ocurred in '//
     : 'CHP_INCAT.', status)
        endif
       endif
*
      end
