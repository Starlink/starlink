#define astCLASS testconvert

#include "ast_err.h"
#include "error.h"
#include "object.h"
#include "skyframe.h"
#include "specframe.h"
#include "cmpframe.h"
#include "frame.h"
#include "unitmap.h"
#include "permmap.h"

main(){
   int status_value = 0;
   int *status = &status_value;

   AstFrameSet *fs;

   AstSkyFrame *sf = astSkyFrame( " ", status );
   AstSpecFrame *df = astSpecFrame( " ", status );
   AstCmpFrame *cf = astCmpFrame( df, sf, " ", status );
   AstFrame *bf = astFrame( 2, "Domain=SKY", status );
   AstFrame *target, *template;


   fs = astConvert( bf, sf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), bf ) && astOK ) {
         astError( AST__INTER, "Error 1\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), sf ) && astOK ) {
         astError( AST__INTER, "Error 2\n",  status );
      } else if( !astIsAUnitMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 3\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 4\n",  status );
   }

   fs = astConvert( sf, bf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), sf ) && astOK ) {
         astError( AST__INTER, "Error 5\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), bf ) && astOK ) {
         astError( AST__INTER, "Error 6\n",  status );
      } else if( !astIsAUnitMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 7\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 8\n",  status );
   }


   astSetDomain( bf, "NOTSKY" );
   fs = astConvert( bf, sf, " " );
   if( fs ) {
      astShow( fs );
      astError( AST__INTER, "Error 9\n",  status );
   }

   fs = astConvert( sf, bf, " " );
   if( fs ) {
      astShow( fs );
      astError( AST__INTER, "Error 10\n",  status );
   }

   astClearDomain( bf );

   fs = astConvert( bf, sf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), bf ) && astOK ) {
         astError( AST__INTER, "Error 11\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), sf ) && astOK ) {
         astError( AST__INTER, "Error 12\n",  status );
      } else if( !astIsAUnitMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 13\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 14\n",  status );
   }

   fs = astConvert( sf, bf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), sf ) && astOK ) {
         astError( AST__INTER, "Error 15\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), bf ) && astOK ) {
         astError( AST__INTER, "Error 16\n",  status );
      } else if( !astIsAUnitMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 17\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 18\n",  status );
   }


   fs = astConvert( bf, cf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), bf ) && astOK ) {
         astError( AST__INTER, "Error 19\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), cf ) && astOK ) {
         astError( AST__INTER, "Error 20\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 21\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 22\n",  status );
   }

   fs = astConvert( cf, bf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), cf ) && astOK ) {
         astError( AST__INTER, "Error 23\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), bf ) && astOK ) {
         astError( AST__INTER, "Error 24\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 25\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 26\n",  status );
   }


   astSetDomain( bf, "NOTSKY" );
   fs = astConvert( bf, cf, " " );
   if( fs ) {
      astShow( fs );
      astError( AST__INTER, "Error 27\n",  status );
   }

   fs = astConvert( cf, bf, " " );
   if( fs ) {
      astShow( fs );
      astError( AST__INTER, "Error 28\n",  status );
   }


   astSetDomain( bf, "SKY" );
   fs = astConvert( bf, cf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), bf ) && astOK ) {
         astError( AST__INTER, "Error 29\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), cf ) && astOK ) {
         astError( AST__INTER, "Error 30\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 31\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 32\n",  status );
   }

   fs = astConvert( cf, bf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), cf ) && astOK ) {
         astError( AST__INTER, "Error 33\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), bf ) && astOK ) {
         astError( AST__INTER, "Error 34\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 35\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 36\n",  status );
   }


   fs = astConvert( sf, cf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), sf ) && astOK ) {
         astError( AST__INTER, "Error 37\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), cf ) && astOK ) {
         astError( AST__INTER, "Error 38\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 39\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 40\n",  status );
   }

   fs = astConvert( cf, sf, " " );
   if( fs ) {
      if( !astEqual( astGetFrame( fs, AST__BASE ), cf ) && astOK ) {
         astError( AST__INTER, "Error 41\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), sf ) && astOK ) {
         astError( AST__INTER, "Error 42\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 43\n",  status );
      } 
   } else {
      astError( AST__INTER, "Error 44\n",  status );
   }


   fs = astFindFrame( sf, cf, " " );
   if( !fs && astOK ) {
      astError( AST__INTER, "Error 45\n",  status );
   }

   fs = astFindFrame( cf, sf, " " );
   if( fs && astOK ) {
      astError( AST__INTER, "Error 46\n",  status );
   }

   astSetMaxAxes( sf, 3 );
   astSetMinAxes( sf, 1 );

   fs = astFindFrame( cf, sf, " " );
   if( !fs && astOK ) {
      astError( AST__INTER, "Error 47\n",  status );
   } else {
      if( !astEqual( astGetFrame( fs, AST__BASE ), cf ) && astOK ) {
         astError( AST__INTER, "Error 48\n",  status );
      } else if( !astEqual( astGetFrame( fs, AST__CURRENT ), sf ) && astOK ) {
         astError( AST__INTER, "Error 49\n",  status );
      } else if( !astIsAPermMap( astGetMapping( fs, AST__BASE, AST__CURRENT ) ) ) {
         astError( AST__INTER, "Error 50\n",  status );
      } 
   }

   target = astFrame( 2, "Domain=ARDAPP", status );
   template = (AstFrame *) astSkyFrame( "System=GAPPT", status );
   fs = astFindFrame( target, template, " " );
   if( fs && astOK ) {
      astError( AST__INTER, "Error 51\n",  status );
   }




   if( astOK ) {
      printf(" All astConvert tests passed\n");
   } else {
      printf("astConvert tests failed\n");
   }
}
