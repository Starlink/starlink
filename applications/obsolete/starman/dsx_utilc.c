/***************************************************
 DSX_UTILC.C

  Contains-
-
 C_PRINTO    Print out text
 C_TYHELP    Put out help
 C_GCMDLST   Squeeze command list and get defaults
 C_GET1I     Get 1 integer from command line
 C_GET2I     Get 2 integers from command line
 C_GET1B     Get 1 Boolean from command line
-
*/


/* Include files */

#include <stdlib.h>
#include <string.h>


#if defined(vms)
#include "starman_x11_xlib"
#else
#include <X11/Xlib.h>
#include <malloc.h>
#endif

#include "f77.h"
#include "cnf.h"

#define Bool int

/* Fortran routines to be used */

extern void F77_EXTERNAL_NAME(printo)
    ( CHARACTER(c1_0) TRAIL(c1_0) );

DECLARE_CHARACTER(c1_0,2000);

extern void F77_EXTERNAL_NAME(tyhelp)
    ( CHARACTER_ARRAY(ca1_1), INTEGER(i1_1), CHARACTER(c2_1),
      INTEGER_ARRAY(ia1_1), INTEGER_ARRAY(ia2_1), INTEGER_ARRAY(ia3_1),
      INTEGER(i2_1) TRAIL(ca1_1) TRAIL(c2_1) );

DECLARE_CHARACTER_ARRAY(ca1_1,68,200);
DECLARE_CHARACTER(c2_1,10000);
DECLARE_INTEGER_ARRAY(ia1_1,200);
DECLARE_INTEGER_ARRAY(ia2_1,200);
DECLARE_INTEGER_ARRAY(ia3_1,200);
DECLARE_INTEGER(i1_1);
DECLARE_INTEGER(i2_1);

extern void F77_EXTERNAL_NAME(gcmdlst)
    ( CHARACTER(c1_2), CHARACTER(c2_2), INTEGER(i1_2), INTEGER(i2_2),
      INTEGER(i3_2), INTEGER(i4_2), INTEGER(i5_2), INTEGER(i6_2),
      INTEGER_ARRAY(ia1_2), INTEGER_ARRAY(ia2_2), INTEGER_ARRAY(ia3_2)
      TRAIL(c1_2) TRAIL(c2_2)  );

DECLARE_CHARACTER(c1_2,2000);
DECLARE_CHARACTER(c2_2,2000);
DECLARE_INTEGER(i1_2);
DECLARE_INTEGER(i2_2);
DECLARE_INTEGER(i3_2);
DECLARE_INTEGER(i4_2);
DECLARE_INTEGER(i5_2);
DECLARE_INTEGER(i6_2);
DECLARE_INTEGER_ARRAY(ia1_2,200);
DECLARE_INTEGER_ARRAY(ia2_2,200);
DECLARE_INTEGER_ARRAY(ia3_2,200);

extern void F77_EXTERNAL_NAME(get1i)
    ( CHARACTER(param_3), INTEGER(i1_3), INTEGER(i2_3), INTEGER(i3_3),
      INTEGER(i4_3) TRAIL(param_3) );

DECLARE_CHARACTER(param_3,512);
DECLARE_INTEGER(i1_3);
DECLARE_INTEGER(i2_3);
DECLARE_INTEGER(i3_3);
DECLARE_INTEGER(i4_3);

extern void F77_EXTERNAL_NAME(get2i)
    ( CHARACTER(param_4), INTEGER(i1_4), INTEGER(i2_4), LOGICAL(b1_4),
      INTEGER(i3_4), INTEGER(i4_4) TRAIL(param_4) );

DECLARE_CHARACTER(param_4,512);
DECLARE_INTEGER(i1_4);
DECLARE_INTEGER(i2_4);
DECLARE_INTEGER(i3_4);
DECLARE_INTEGER(i4_4);
DECLARE_LOGICAL(b1_4);

extern void F77_EXTERNAL_NAME(get1b)
    ( CHARACTER(param_5), LOGICAL(b1_5), LOGICAL(b2_5) TRAIL(param_5) );
DECLARE_CHARACTER(param_5,512);
DECLARE_LOGICAL(b1_5);
DECLARE_LOGICAL(b2_5);


DECLARE_CHARACTER(istr,2000);
DECLARE_CHARACTER(istra,10000);
DECLARE_CHARACTER(ostr,2000);


DECLARE_LOGICAL(b2);



/***********************************************************************
  C_PRINTO -- Buffers call to Fortran rountine printo  (Xwindows)

  Patrick Morris        leeds             1992 jan
*/

void c_printo ( text )

   char *text;
{
/* C-- */
/* Cbegin */

     cnf_exprt ( text, c1_0, c1_0_length );

     (void) F77_CALL(printo) ( CHARACTER_ARG(c1_0) TRAIL_ARG(c1_0) );

}


/**************************************************************************
 C_TYHELP -- Put out help

    a j penny                    ral         1990 jan
*/

c_tyhelp ( cca1, i1_1, cc2, ia1_1, ia2_1, ia3_1, i2_1 )

char* cca1;
char* cc2;

/* i: i: i: i: i: i: i: */
{
/* C-- */
/* Cbegin */


     cnf_exprt ( cca1, istra, istra_length );
     cnf_exprt ( cc2, istr, istr_length );

     (void) F77_CALL(tyhelp) ( CHARACTER_ARRAY_ARG(istra), INTEGER_ARG(&i1_1),
                               CHARACTER_ARG(istr), INTEGER_ARRAY_ARG(&ia1_1),
                               INTEGER_ARRAY_ARG(&ia2_1), INTEGER_ARRAY_ARG(&ia3_1),
                               INTEGER_ARG(&i2_1) TRAIL_ARG(istra) TRAIL_ARG(istra) );

}



/**************************************************************************
 C_GCMDLST -- Squeeze command list and get defaults

    a j penny                    ral         1990 jan
*/

c_gcmdlst ( cc1, cc2, i1_2, i2_2, i3_2, i4_2, i5_2, i6_2, ia1_2, ia2_2,
            ia3_2 )

char* cc1;
char* cc2;
/* i: o: i: o: o: o: o: o: o: o: o: */
{
/* C-- */
/* Cbegin */


      cnf_exprt ( cc1, istr, istr_length );

      (void) F77_CALL(gcmdlst) ( CHARACTER_ARG(istr), CHARACTER_ARG(ostr),
                INTEGER_ARG(&i1_2), INTEGER_ARG(&i2_2), INTEGER_ARG(&i3_2),
                INTEGER_ARG(&i4_2),   INTEGER_ARG(&i5_2),  INTEGER_ARG(&i6_2),
                INTEGER_ARRAY_ARG(&ia1_2), INTEGER_ARRAY_ARG(&ia2_2),
                INTEGER_ARRAY_ARG(&ia3_2) TRAIL_ARG(istr) TRAIL_ARG(ostr) );

      cnf_imprt ( ostr, ostr_length, cc2 );

}


/**************************************************************************
 C_GET1I -- Get 1 integer from command line

    a j penny                    ral         1990 jan
*/

   c_get1i ( text, iv1, iv2, iv3, iv4 )

    char  *text;	/* i: */
    int   *iv1;		/* o: */
    int   iv2;		/* i: */
    int   iv3;		/* i: */
{
/* C-- */
/* Cbegin */

      cnf_exprt ( text, param_3, param_3_length );
      i2_3 = iv2; i3_3 = iv3; i4_3 = iv4;

      (void) F77_CALL(get1i) ( CHARACTER_ARG(param_3), INTEGER_ARG(&i1_3),
                               INTEGER_ARG(&i2_3), INTEGER_ARG(&i3_3),
                               INTEGER_ARG(&i4_3) TRAIL_ARG(param_3) );

      *iv1 = i1_3;
}


/**************************************************************************
 C_GET2I -- Get 2 integers from command line

    a j penny                    ral         1990 jan
*/

   c_get2i ( text, iv1, iv2, bv1, iv3, iv4 )

    char  *text;	/* i: */
    int   *iv1;		/* i/o: */
    int   *iv2;		/* i/o: */
    Bool  bv1;		/* i: */
    int   iv3;		/* i: */
    int   iv4;		/* i: */
{
/* C-- */

/* Cbegin */

         cnf_exprt ( text, param_4, param_4_length );
         if ( bv1 )
            b1_4 = F77_TRUE;
         else
            b1_4 = F77_FALSE;
         i1_4 = *iv1; i2_4 = *iv2; i3_4 = iv3; i4_4 = iv4;

         (void) F77_CALL(get2i) ( CHARACTER_ARG(param_4), INTEGER_ARG(&i1_4),
                 INTEGER_ARG(&i2_4), LOGICAL_ARG(&b1_4), INTEGER_ARG(&i3_4),
                 INTEGER_ARG(&i4_4) TRAIL_ARG(param_4) );

         *iv1 = i1_4; *iv2 = i2_4;

}


/**************************************************************************
 C_GET1B -- Get 1 Boolean from command line

    a j penny                    ral         1990 jan
*/

    c_get1b ( text, bout, bin )

    char   *text;	/* i: */
    Bool   *bout;	/* o: */
    Bool   bin;		/* i: */
{
/* C-- */
/* Cbegin */

        cnf_exprt ( text, param_5, param_5_length );
        if ( bin )
           b2_5 = F77_TRUE;
        else
           b2_5 = F77_FALSE;

        (void) F77_CALL(get1b) ( CHARACTER_ARG(param_5), LOGICAL_ARG(&b1_5),
                                 LOGICAL_ARG(&b2_5) TRAIL_ARG(param_5) );

        if ( F77_ISTRUE( b1_5 ) )
           *bout = True;
        else
           *bout = False;

}
