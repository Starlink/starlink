      PROGRAM SIMPLIFY

*  Usage:
*     simplify <in file> <out file>

*  Description:
*     Reads a Mapping from "in file" (as an AST dump), and writes out the
*     simplified Mapping to "out file".

*  Parameters:
*     in file
*        A text file containing an AST dump of a Mapping.
*     out file
*        The output file. Contains an AST dump of the simplified Mapping
*        on exit.


      IMPLICIT NONE
      INCLUDE 'AST_PAR'
      EXTERNAL SOURCE, SINK

      INTEGER STATUS, OBJECT, IARGC, CHAN, CHR_LEN, OC, SMAP
      CHARACTER FILE*80, OFILE*80, LINE*255

      STATUS = 0
*
* Check command line arguments have been supplied.
*
      IF( IARGC() .LT. 2 ) THEN
         WRITE(*,*) 'Usage: simplify <in file> <out file>'
         RETURN
      END IF

*
* Open the name of the input text file.
*
      CALL GETARG( 1, FILE )

* Attempt to read an object from the text file as an AST dump.
      OPEN( UNIT=10, FILE=FILE, STATUS='OLD' )
      CHAN = AST_CHANNEL( SOURCE, AST_NULL, ' ', STATUS )
      OBJECT = AST_READ( CHAN, STATUS )
      CALL AST_ANNUL( CHAN, STATUS )
      CLOSE( 10 )

*
* Abort if no object was read.
*
      IF( OBJECT .EQ. AST__NULL ) THEN
         WRITE(*,*) 'simplify: no Mapping could be read from ',
     :              file( : chr_len( file ) )
         RETURN

*
* Otherwise write out the simplified Mapping
*
      ELSE
         CALL GETARG( 2, OFILE )
         CALL DELETEFILE( OFILE )

         SMAP = AST_SIMPLIFY( OBJECT, STATUS )

         OPEN( UNIT=10, FILE=OFILE, STATUS='NEW' )
         CHAN = AST_CHANNEL( AST_NULL, SINK, ' ', STATUS )
         IF( AST_WRITE( CHAN, SMAP, STATUS ) .NE. 1 ) THEN
            WRITE(*,*) 'simplify: Simplified Mapping read from ',
     :                 file( : chr_len( file ) ),' could not be '//
     :                 'written out.'
         END IF
         CALL AST_ANNUL( CHAN, STATUS )
         CALL AST_ANNUL( SMAP, STATUS )
         CLOSE( 10 )
      END IF


      END


*
*  Delete a file if it exists.
*
      SUBROUTINE DELETEFILE( FILNAM )
      IMPLICIT NONE

      CHARACTER FILNAM*(*)
      LOGICAL EXISTS

      INQUIRE ( FILE   = FILNAM,
     :          EXIST  = EXISTS )

      IF( EXISTS ) THEN
         OPEN ( UNIT=10, FILE=FILNAM, STATUS='OLD' )
         CLOSE ( 10, STATUS='DELETE' )
      END IF

      END


*
*  SOURCE FUNCTION FOR AST_CHANNEL.
*
      SUBROUTINE SOURCE( STATUS )
      IMPLICIT NONE
      INTEGER STATUS
      CHARACTER BUFFER*200
      READ( 10, '(A)', END=99 ) BUFFER
      CALL AST_PUTLINE( BUFFER, LEN( BUFFER ), STATUS )
      RETURN
 99   CALL AST_PUTLINE( BUFFER, -1, STATUS )
      END

*
*  SINK FUNCTION FOR AST_CHANNEL.
*
      SUBROUTINE SINK( STATUS )
      IMPLICIT NONE
      INTEGER STATUS, L
      CHARACTER BUFFER*200

      CALL AST_GETLINE( BUFFER, L, STATUS )
      IF( L .GT. 0 ) WRITE( 10, '(A)' ) BUFFER( : L )

      END
