
/* Functions for keeping track of allocated strings in the current unit. */
      void uclear();
      void unew();
      void uadd( char *item );
      char *ucontent();

/* Configuration flags. */
      int strict;

/* Function for string concatenation. */
      char *scat( int n, ... );

/* Generic linked list type. */
      struct element {
         char *text;
         struct element *next;
      };
      typedef struct element ELEMENT;


/* $Id$ */
