*.......................................................................
*   Common block storage for memory configuration

*   Configuration mode
      INTEGER CONMOD

*   Number of memories of all types
      INTEGER CNMEM

*   Memory types
      INTEGER CMEMTY( 0 : MAXMEM - 1 )

*   Memory sizes in x
      INTEGER CMEMSX( 0 : MAXMEM - 1 )

*   Memory sizes in y
      INTEGER CMEMSY( 0 : MAXMEM - 1 )

*   Memory priority list.
      INTEGER CMEMPR( 0 : MAXMEM - 1 )

*   Memory ITT depths
      INTEGER CITTDE( 0 : MAXMEM - 1 )

      COMMON / IKN_COMEM / CONMOD, CNMEM, CMEMTY, CMEMSX, CMEMSY,
     :                     CMEMPR, CITTDE

      SAVE / IKN_COMEM /

