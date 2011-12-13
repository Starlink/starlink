      SUBROUTINE GASP2NDF( STATUS )
*+
*  Name:
*     GASP2NDF

*  Purpose:
*     Converts an image in GASP format to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GASP2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts a GAlaxy Surface Photometry (GASP)
*     format file into an NDF.

*  Usage:
*     gasp2ndf in out shape=?

*  ADAM Parameters:
*     IN = FILENAME (Read)
*        A character string containing the name of GASP file to convert.
*        The extension should not be given, since ".dat" is assumed.
*     OUT = NDF (Write)
*        The name of the output NDF.
*     SHAPE( 2 ) = _INTEGER (Read)
*        The dimensions of the GASP image (the number of columns
*        followed by the number of rows).  Each dimension must be in the
*        range 1 to 1024.  This parameter is only used if supplied on
*        the command line, or if the header file corresponding to the
*        GASP image does not exist or cannot be opened.

*  Examples:
*     gasp2ndf m31_gasp m31
*        Convert a GASP file called m31_gasp.dat into an NDF called m31.
*        The dimensions of the image are taken from the header file
*        m31_gasp.hdr.
*     gasp2ndf n1068 ngc1068 shape=[256,512]
*        Take the pixel values in the GASP file n1068.dat and create
*        the NDF ngc1068 with dimensions 256 columns by 512 rows.

*  Notes:
*     -  A GASP image is limited to a maximum of 1024 by 1024 elements.
*     It must be two dimensional.
*     -  The GASP image is written to the NDF's data array.  The data
*     array has type _WORD.  No other NDF components are created.
*     -  If the header file is corrupted, you must remove the offending
*     ".hdr" file or specify the shape of the GASP image on the command
*     line, otherwise the application will continually abort.

*  References:
*     GASP documentation (MUD/66).

*  Related Applications:
*     CONVERT: NDF2GASP.

*  Implementation Deficiencies:
*     -  Does not support wildcards.

*  Keywords:
*     CONVERT, GASP

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-JAN-1993 (RAHM):
*        Original version.
*     12-JUL-1993 (RAHM):
*        Tidied up code ready for release in CONVERT package.
*     1993 July 27 (MJC):
*        Removed PSX calls and used RIO.  Used one shape parameter.
*        Tidied the prologue and code.  Improved method for deciding
*        whether to use the header file or not.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of as character string
                                 ! excluding trailing blanks

*  Local Constants:
      INTEGER NDIM               ! Number of dimensions
      PARAMETER ( NDIM = 2 )

      INTEGER FILLEN             ! Number of dimensions
      PARAMETER ( FILLEN = 132 )

*  Local Variables:
      INTEGER DEFSHA( NDIM )     ! Default shape of input image
      INTEGER DIMS( NDIM )       ! Dimensions of input image
      INTEGER EL                 ! Number of elements in GASP image
      INTEGER FD                 ! File descriptor
      CHARACTER * ( FILLEN ) GASPFI ! Name of GASP file
      CHARACTER * ( FILLEN ) HDRFIL ! Name of GASP header file
      INTEGER IOVAL              ! IOSTAT, I/O error indicator
      REAL JUNK                  ! Unwanted stuff from header file
      INTEGER LBND( NDIM )       ! Lower bounds array
      INTEGER NAMLEN             ! String length
      INTEGER NDF                ! NDF identifier
      INTEGER PNTRO( 1 )         ! Pointer to mapped NDF data array
      REAL RDIMS( NDIM )         ! Dimensions of input image from header
      INTEGER SSTATE             ! State of the SHAPE parameter
      LOGICAL THERE              ! Header file exists
      INTEGER UBND( NDIM )       ! Upper bounds array
      INTEGER UNIT               ! FIle unit number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the GASP file.
      CALL PAR_GET0C( 'IN', GASPFI, STATUS )
      CALL MSG_BLANK( STATUS )

*  Decide from where to get the dimensions of the GASP image.
*  ==========================================================
*
*  Test if the shape is supplied on the command line or the header
*  file does not exist to decide to get the dimensions through the
*  parameters.  If not attempt to get the values from a GASP header
*  file of the same name as the image file.
      CALL PAR_STATE( 'SHAPE', SSTATE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Form the header file's name.
      NAMLEN =  CHR_LEN( GASPFI )
      CALL CHR_MOVE( GASPFI, HDRFIL )
      CALL CHR_MOVE( '.hdr', HDRFIL( NAMLEN+1: ) )

*  Inquire it's existence.
      INQUIRE( FILE=HDRFIL, EXIST=THERE )

*  Set the default shapes to be out of range, hence no dynamic default
*  is set.
      DEFSHA( 1 ) = -1
      DEFSHA( 2 ) = -1

*  Obtain the dimensions between limits through the SHAPE parameter.
      IF ( SSTATE .EQ. PAR__ACTIVE .OR. .NOT. THERE ) THEN
         CALL PAR_GDR1I( 'SHAPE', NDIM, DEFSHA, 1, 1024, .FALSE.,
     :                   DIMS, STATUS )
      ELSE

*  Start a new error status.
         CALL ERR_MARK

*  Try to open the GASP header file for the dimension information.
*  Since the file is opened for read access, the recordsize should not
*  be specified (set to zero), however in practice...  If we cannot open
*  the header file then we must prompt for the values.
         CALL RIO_OPEN( HDRFIL, 'READ', 'UNFORMATTED', 16, FD, STATUS )

*  Flush the error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_SETC( 'HDRFIL', HDRFIL )
            CALL MSG_OUT( ' ', 'Cannot open GASP header file '/
     :        /'^HDRFIL.  Use the SHAPE parameter.', STATUS )
            CALL MSG_BLANK( STATUS )

*  Obtain the shape of the GASP file.
            CALL PAR_GDR1I( 'SHAPE', NDIM, DEFSHA, 1, 1024, .FALSE.,
     :                      DIMS, STATUS )

         ELSE

*  Inquire the unit number of the header file.
            CALL FIO_UNIT( FD, UNIT, STATUS )

*  Read the dimensions from the header file.
            READ( UNIT=UNIT, REC=1, IOSTAT=IOVAL ) RDIMS( 1 ), JUNK,
     :                                             RDIMS( 2 ), JUNK

*  In case of an error reading the file, make a contextual error report
*  and abort.
            IF ( IOVAL .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_FIOER( 'IOVAL', IOVAL )
               CALL MSG_SETC( 'HDRFIL', HDRFIL )
               CALL ERR_REP( 'GASP2NDF_FILEREAD',
     :           'GASP2NDF: Error reading the header file ^HDRFIL. '/
     :           /'Error was: "^IOVAL".  Delete the file or specify '/
     :           /'the image shape on the command line.', STATUS )

*  Close the header file.
               CALL RIO_CLOSE( FD, STATUS )
               GOTO 999
            END IF

*  For some reason, the number of columns and rows in the file are
*  stored in the header file as reals.  Convert these to the nearest
*  integer.
            DIMS( 1 ) = NINT( RDIMS( 1 ) )
            DIMS( 2 ) = NINT( RDIMS( 2 ) )

*  Report the dimensions in verbose mode.
            CALL MSG_SETI( 'NCOLS', DIMS( 1 ) )
            CALL MSG_SETI( 'NROWS', DIMS( 2 ) )
            CALL MSG_OUTIF( MSG__VERB, ' ',
     :        'Dimensions are ^NCOLS by ^NROWS', STATUS )
         END IF

*  Release the error context.
         CALL ERR_RLSE

*  Close the header file.
         CALL RIO_CLOSE( FD, STATUS )

      END IF

*  Create the NDF.
*  ===============

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Set up the bounds.
      LBND( 1 ) = 1
      UBND( 1 ) = DIMS( 1 )
      LBND( 2 ) = 1
      UBND( 2 ) = DIMS( 2 )

*  Create the simple NDF.
      CALL NDF_CREAT( 'OUT', '_WORD', NDIM, LBND, UBND, NDF, STATUS )

*  Map the NDF's data array to short signed integers, correspoding to
*  HDS type _WORD.
      CALL NDF_MAP( NDF, 'Data', '_WORD', 'WRITE', PNTRO, EL, STATUS )

*  Pass mapped array to READGASP.
      CALL CON_RGASP( GASPFI, DIMS( 1 ), DIMS( 2 ),
     :                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                STATUS )

*  Unmap the NDF's data array.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Close the NDF context.
      CALL NDF_END( STATUS )

  999 CONTINUE

*  Write the standard contextual error report.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GASP2NDF_ERR',
     :     'GASP2NDF: Unable to convert the GASP image to an NDF.',
     :     STATUS )
      END IF

      END
