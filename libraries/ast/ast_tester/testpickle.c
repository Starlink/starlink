#include "ast.h"
#include <stdio.h>

main(){
   char *pickle1;
   char *pickle2;
   AstSkyFrame *sf = astSkyFrame( " " );
   AstFrame *bf = astFrame( 2, "Domain=SKY" );
   AstFrameSet *fs = astConvert( bf, sf, " " );

   if( fs ) {
      pickle1 = astPickle( fs );
      AstFrameSet *fs2 = astUnpickle( pickle1 );
      pickle2 = astPickle( fs2 );
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

astShow( fs );
astShow( fs2 );
         astError( AST__INTER, "Error 3\n" );
      }

   } else if( astOK ){
      astError( AST__INTER, "Error 4\n"  );
   }

   if( astOK ) {
      printf(" All astPickle tests passed\n");
   } else {
      printf("astPickle tests failed\n");
   }
}
