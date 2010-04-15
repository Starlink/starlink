*+  RED4_SEEK_ITEM - Determine if an item in a data structure exists.
      SUBROUTINE RED4_SEEK_ITEM( STRUCTURE, ITEM, FOUND, STATUS )
*    Description :
*     This routine searches the named data structure for a particular
*     item and returns a flag indicating if the item exists. Any
*     internal errors will be take to indicate that the item does not
*     exist, and a bad status will not be returned.
*
*     The routine has been produced to simplify the high-level code by
*     removing the frequently used low level DTA calls and checks.
*    Invocation :
*      CALL RED4_SEEK_ITEM( STRUCTURE, ITEM, FOUND, STATUS )
*    Parameters :
*     STRUCTURE     = CHARACTER*(*)( READ )
*        The DTA address of the structure to be searched.
*     ITEM          = CHARACTER*(*)( READ )
*        The name of the item within the structure to be searched for.
*        This may be a primitive item, or the name of another structure.
*     FOUND         = LOGICAL( WRITE )
*        Logical flag indicating if the item has been found.
*        Returned .TRUE. if the item has been found, .FALSE. if it has not.
*     STATUS        = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     21-Sep-1990: Original version.               (SMB)
*     23-Feb-1993: Conform to error strategy       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
      CHARACTER*(*)
     :  STRUCTURE,         ! DTA address of structure to be searched
     :  ITEM               ! Name of item to be searched for
*    Export :
      LOGICAL
     :  FOUND              ! Flag indicating if the item has been found
*    Status :
      INTEGER
     :  STATUS             ! Global status
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER DTA__OK                    ! DTA success status
      PARAMETER ( DTA__OK = 0 )
      INTEGER MAXN                       ! Maximum value of N (for safety)
      PARAMETER( MAXN = 1000 )
*    Local variables :
      CHARACTER*15
     :  NTH_ITEM           ! Name of Nth item in structure
      INTEGER
     :  N,                 ! Item number
     :  DTA_STATUS         ! DTA status
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialise the DTA status, the item number and the FOUND flag.
      DTA_STATUS = DTA__OK
      N = 1
      FOUND = .FALSE.

*   Continue looping until an error occurs, the specified item is
*   found, or N exceeds its safety limit.
      DO WHILE ( ( DTA_STATUS .EQ. DTA__OK ) .AND.
     :           ( .NOT.FOUND ) .AND.
     :           ( N .LT. MAXN ) )

*      Obtain the name of the Nth item in the structure.
         CALL DTA_NMVAR( STRUCTURE, N, NTH_ITEM, DTA_STATUS )

*      If this has worked, check the name against the item required.
*      If the names match, the item has been found.
         IF ( ( DTA_STATUS .EQ. DTA__OK ) .AND.
     :        ( NTH_ITEM .EQ. ITEM ) ) THEN

            FOUND = .TRUE.
         END IF

*      Next N
         N = N + 1
      END DO

      END
