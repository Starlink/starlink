      SUBROUTINE CATINF( STRING, STATUS )
*+
*  Name:
*     CATINF

*  Purpose:
*     Check the string of selected catalogues

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CATINF( STRING, STATUS )

*  Description:
*     Check the String of Selected Catalogues is Non-Blank and set up
*     the array of bit numbers as necessary: If List starts with 'NOT'
*     then the list is put in 'NOCAT' - the bits which must not be set,
*     otherwise in 'YESCAT' which is the bits which must be set.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The character string which may contain catalogue names
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     UNK: Someone (Somewhere)
*     {enter_new_authors_here}

*  History:
*     Sometime (UNK):
*        Original version.
*     16-NOV-1992 (PMA):
*        Tidied up code.
*        Added STATUS argument
*     2-MAR-1993 (AJJB):
*        Added STATUS argument to CATSEL calls
*     2-JUN-1993 (AJJB):
*        Replaced IFAIL error reporting mechanism with STATUS.
*     3-JUN-1993 (AJJB):
*        It now outputs error messages if the catalogue string contains
*        no recognised abbreviations, and only outputs the messages
*        informing the user which catalogues will be used if there are
*        valid abbreviations in the string.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'MAIN'             ! CHART main common block
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 4 ) IDNAM( 80 ) ! Catalogue identifiers
      INTEGER LENG               ! Length of string
      INTEGER K                  ! Loop counter
      INTEGER N                  ! Number of catalogues found
      CHARACTER*200 TEXT         ! Internal buffer for msg. to user

*  Local Data:
      DATA IDNAM/'HD','AGK','HZ','CPC','YZ','CCFS','BOSS',' ','SAO'
     : ,'ADS','IDS','GCRV','YBS','N30','FK4','JSK','A+B','KDY',
     : 'UVBY','BAY','USNP','GCTP','GCVS','UBV','NGC','IC','CLA','CLB'
     : ,'LS','IRC','Cel','GEN','U+F','SB','BE*','GL','UM','FE'
     : ,'RY','JP11','HGAM','AMAS','MSS','S+J',12*' ',
     : 'DM2','YZO','MMAG',21*' '/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO K = 1, 50
         YESCAT( K ) = 0
         NOCAT( K ) = 0
      END DO
      IF ( LENG( STRING ) .NE. 0 ) THEN
         IND = INDEX( STRING, 'NOT' )
         IF ( IND .NE. 0 ) THEN
            CALL CATSEL( STRING, NOCAT, N, IND+3, IDNAM, IDBYTE, STATUS)

* If catalogues were recognised OK, print a message to the user

            IF ( STATUS .EQ. SAI__OK ) THEN

               WRITE(TEXT, 900)
     :           ( IDNAM( NOCAT( K ) )( :LENG( IDNAM( NOCAT( K ) ) ) ),
     :           K = 1 , N )
               CALL MSG_OUT( ' ', TEXT, STATUS )

* otherwise, flush error message written in CATSEL
            ELSE

               CALL ERR_FLUSH( STATUS )

            ENDIF

900         FORMAT( 1X, 'OK - Stars will only be selected if they are',
     :         ' not in ', 50(A:' or ') )
         ELSE
            CALL CATSEL( STRING, YESCAT, N, 1, IDNAM, IDBYTE, STATUS )

* If catalogues were recognised OK, print a message to the user

            IF ( STATUS .EQ. SAI__OK ) THEN

               WRITE(TEXT, 902)
     :          ( IDNAM( YESCAT( K ) )( :LENG( IDNAM( YESCAT( K ) ) ) ),
     :          K = 1 , N )
               CALL MSG_OUT( ' ', TEXT, STATUS )

* otherwise, flush error message written in CATSEL
            ELSE

               CALL ERR_FLUSH( STATUS )

            ENDIF


902         FORMAT( 1X, 'OK - Stars will only be selected if they are',
     :         '  in ', 50(A:' and ') )
         ENDIF

*  Set number of catalogues selected in main common block.
         NCH = N
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP ( ' ', 'CATINF: Empty CATALOGUES string' //
     :                  ' - not attempting selection by catalogue.',
     :                  STATUS )
      ENDIF

      END
