************************************************************************
*+  AGI_1CINIT - Initialise the AGI common blocks.

      SUBROUTINE AGI_1CINIT ( STATUS )

*    Description :
*     Initialise the AGI common blocks which contain the references to
*     the picture identifiers. The definitions are held in the INCLUDE
*     file 'AGI_PFREE'.
*
*    Invocation :
*     CALL AGI_1CINIT( STATUS )
*
*    Method :
*     Check status on entry.
*     Set the variables to zero of blank strings.
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     Aug 1988
*     Dec 1989  Added CIDIID
*     Jul 1990  Added CROOT
*     Oct 1990  Added picture deferral
*     Nov 1990  Added nest level and clear flags
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_pfree'

*    Local variables :
      INTEGER I
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Clear out all the common block entries, but not the free list.
         DO I = 1, FRELEN
            CGRAWK( I ) = ' '
            CAGIWK( I ) = ' '
            PTNAME( I ) = ' '
            CPICNM( I ) = 0
            CIDIID( I ) = 0
            CLEVEL( I ) = 0
            CDEPS( I ) = 0
         ENDDO

*   Initialise the clear flags
         DO I = 1, CLRLEN
            CLEARF( I ) = 0
         ENDDO

*   Initialise other flags
         CURPID = 0
         CROOT = 0
         CNUMID = 0
         CISDEP = .FALSE.

      ENDIF

      END

