#include "ast.h"
#include <stdio.h>

main(){
   char *pickle1;
   char *pickle2;
   AstSkyFrame *sf = astSkyFrame( " " );
   AstFrame *bf = astFrame( 2, "Domain=SKY" );
   AstFrameSet *fs = astConvert( bf, sf, " " );

   if( fs ) {
      pickle1 = astToString( fs );
      AstFrameSet *fs2 = astFromString( pickle1 );
      pickle2 = astToString( fs2 );
      if( pickle1 && pickle2 ) {
         if( strcmp( pickle1, pickle2 ) && astOK ) {
            astError( AST__INTER, "Error 1\n" );
         }
      } else if( astOK ) {
         astError( AST__INTER, "Error 2\n" );
      }


      pickle1 = astFree( pickle1 );
      pickle2 = astFree( pickle2 );

      if( fs2 && !astEqual( fs, fs2 ) && astOK ) {
         astError( AST__INTER, "Error 3\n" );
      }

   } else if( astOK ){
      astError( AST__INTER, "Error 4\n"  );
   }

   if( astOK ) {
      printf(" All Object tests passed\n");
   } else {
      printf("Object tests failed\n");
   }
}
