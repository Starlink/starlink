/*
*+
*  Name:
*     tag.c
*
*  Purpose:
*     Harness routine for yacc.
*
*  Description:
*     This file provides a main() function which calls yyparse.
*-
*/

main( int argc, char *argv[] ) {
   int yyparse();
   
   switch( argc ) {
      case 3:
         if ( freopen( argv[ 2 ], "w", stdout ) != NULL ) {
            perror( argv[ 2 ] );
            exit( 1 );
         }
      case 2:
         if ( freopen( argv[ 1 ], "r", stdin ) != NULL ) {
            perror( argv[ 1 ] );
            exit( 1 );
         }
      default:
   }
         
   return( yyparse() );

}
