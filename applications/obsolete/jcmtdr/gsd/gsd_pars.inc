*    GSDPARS
* 
*    Parameter definitions for the GSD system
*
*    Requires prior inclusion of PRM_PAR for the VAL__BADx.
*
      INTEGER GSD__MAXFILE     ! Maximum number of GSD files open at any time
      PARAMETER (GSD__MAXFILE=100) 
*
*    Item descriptors
      INTEGER GSD__MAXDIM       ! Maximum number of dimensions
      PARAMETER (GSD__MAXDIM=5) ! 5 => 64 byte descr. to give proper alignment
      INTEGER GSD__MXDIM        ! Old name
      PARAMETER (GSD__MXDIM=GSD__MAXDIM) 
      INTEGER GSD__SZNAME       ! Size of name
      PARAMETER (GSD__SZNAME=15)
      INTEGER GSD__SZUNIT       ! Size of item unit
      PARAMETER (GSD__SZUNIT=10)
*
*    Parameter codes
*
*    Data types and length of type in bytes...
      INTEGER GSD__BYTE        ! Byte 
      PARAMETER (GSD__BYTE      = 1)
      INTEGER GSD__SZBYTE
      PARAMETER (GSD__SZBYTE    = 1)
      INTEGER GSD__LOGICAL     ! Logical*1
      PARAMETER (GSD__LOGICAL   = 2)
      INTEGER GSD__SZLOGICAL
      PARAMETER (GSD__SZLOGICAL = 1)
      INTEGER GSD__WORD        ! Integer*2
      PARAMETER (GSD__WORD      = 3)
      INTEGER GSD__SZWORD
      PARAMETER (GSD__SZWORD    = 2) 
      INTEGER GSD__INTEGER     ! Integer*4
      PARAMETER (GSD__INTEGER   = 4)
      INTEGER GSD__SZINTEGER
      PARAMETER (GSD__SZINTEGER = 4)
      INTEGER GSD__REAL        ! Real*4
      PARAMETER (GSD__REAL      = 5)
      INTEGER GSD__SZREAL
      PARAMETER (GSD__SZREAL    = 4)
      INTEGER GSD__DOUBLE      ! Real*8
      PARAMETER (GSD__DOUBLE    = 6)
      INTEGER GSD__SZDOUBLE
      PARAMETER (GSD__SZDOUBLE  = 8)
      INTEGER GSD__CHAR        ! Character*(GSD__SZCHAR)
      PARAMETER (GSD__CHAR      = 7)
      INTEGER GSD__SZCHAR
      PARAMETER (GSD__SZCHAR    = 16)
*
*    Bad data values.
*    These are PRIMDAT bad values, not the traditional VAX/GSD bad
*    values. There are no bad values for logical and character any more.
*
      BYTE BBAD
      PARAMETER (BBAD = VAL__BADB)
      INTEGER*2 WBAD
      PARAMETER (WBAD = VAL__BADW)
      INTEGER*4 IBAD
      PARAMETER (IBAD = VAL__BADI)
      REAL*4 RBAD
      PARAMETER (RBAD = VAL__BADR)
      REAL*8 DBAD
      PARAMETER (DBAD = VAL__BADD)
*
*
*  Index array
*
      INTEGER GSD__SZINDEX                 ! Size of INTEGER*4 array of item 
      PARAMETER (GSD__SZINDEX = 2)         ! indices
      INTEGER GSD__INDEX_FD		   ! Location of FD in the index
      PARAMETER (GSD__INDEX_FD = 1)
      INTEGER GSD__INDEX_NUMBER            ! Location of item NUMBER in index
      PARAMETER (GSD__INDEX_NUMBER = 2)
*
