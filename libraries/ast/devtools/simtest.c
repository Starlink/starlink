#define astCLASS simtest

#include <stdio.h>
#include <string.h>
#include "globals.h"
#include "object.h"
#include "fitschan.h"
#include "mapping.h"
#include "cmpmap.h"
#include "error.h"
#include "winmap.h"
#include "tranmap.h"
#include "frameset.h"
#include "pointset.h"
#include "f77.h"
#include "memory.h"
#include "switchmap.h"
#include "unitmap.h"

void DisplayList( AstMapping *, int, int, int * );
void sink( const char * );
const char *source( void );
void tran( AstMapping *, int * );
void diff( AstMapping *, int * );
void tranm( AstMapping **, int *, int, int, int * );

extern F77_SUBROUTINE(kpg1_asreg)( INTEGER(status) );


FILE *fd=NULL;

main(){
   char file[ 50 ], text[ 100 ];
   const char *class, *enc0;
   char *attrs, *kvalue;
   AstWinMap *winmap;
   AstFitsChan *fc, *fc2;
   AstChannel *ch, *channel;
   AstMapping *map, *map2, *tmap;
   AstObject *obj, *obj2;
   int opt, nin, nout, i;
   DECLARE_INTEGER(lstatus);
   int *status;

   lstatus = 0;
   status = &lstatus;

/*   F77_CALL(kpg1_asreg)( INTEGER_ARG(&lstatus) ); */

   printf( "Input FITS or AST file [| fitschan attrs] > ");
   scanf( "%s", file );

   attrs = strchr( file, '|' );
   if( attrs ) {
      *attrs = 0;
      attrs++;
   } else {
      attrs = "Carlin=0";
   }

   fd = fopen( file, "r" );
   if( fd ){
      ch = (AstChannel *) astFitsChan( NULL, sink, attrs, status );
      if( astOK ) {
         fc = (AstFitsChan *) ch;
         while( astOK && fgets( text, 100, fd ) ){
            astPutFits( fc, text, 0 );
         }
         astClearCard( fc );

         enc0 = astGetC( fc, "encoding" );
         printf("Using %s encoding\n", enc0 );
         obj = astRead( fc );
      }
      if( !obj || !astOK ){
         printf("Attempting to read file %s as an AST file...\n", file );
         astClearStatus;
         ch = astChannel( source, NULL, "", status );

         fclose( fd );
         fd = NULL;
         fd = fopen( file, "r" );
         obj = astRead( ch );
         if( obj != NULL ) {
            fc = NULL;
         } else {
            astClearStatus;
            printf("Reverting to FitsChan and using a dummy UnitMap. \n", file );
            obj = (AstObject *) astUnitMap( 1, "", status );
            astAnnul( ch );
            ch = (AstChannel *) fc;
         }

      }
      fclose( fd );
      fd = NULL;
   } else {
      astError( 1, "Cannot open file '%s'.", status, file );
   }

   class = obj ? astGetClass( obj ) : NULL;
   if( obj && class ){
      printf( "%s %s has been read from file '%s'.\n",
              strspn( class, "AEIOUaeiou" )?"An":"A", class, file );
   }

   if( obj && astIsAFitsChan( obj ) ) {
      fc = (AstFitsChan *) obj;
      astClearCard( fc );
      enc0 = astGetC( fc, "encoding" );
      printf("Using %s encoding\n", enc0 );
      obj = astRead( fc );
   }

   if( obj && astIsAMapping( obj ) ){
      map = (AstMapping *) obj;

      while( astOK ){
         printf("\n"
                "0 - Quit\n"
                "1 - Transform points\n"
                "2 - Simplify\n"
                "3 - Display\n"
                "4 - Write\n"
                "5 - Shift and scale\n"
                "6 - Set mapping attributes\n"
                "7 - Get mapping attributes\n"
                "8 - Dump the FitsChan\n"
                "9 - Differentiate\n"
                "10 - Find a FITS keyword\n"
                "\n"
                "Option > " );
         opt = 0;
         scanf( "%d", &opt );

         if( opt == 0 ){
            break;

         } else if( opt == 1 ){
            tran( map, status );

         } else if( opt == 2 ){
            map2 = astSimplify( map );
            map = astAnnul( map );
            map = map2;
            obj = (AstObject *) map2;

         } else if( opt == 3 ){
            if( astIsAFrameSet( map ) ) {
               tmap = astGetMapping( map, AST__BASE, AST__CURRENT );
            } else {
               tmap = astClone( map );
            }
            DisplayList( tmap, 1, astGetInvert( tmap ), status );
            tmap = astAnnul( tmap );

         } else if( opt == 4 ){
            printf( "\n  Output FITS file > ");
            scanf( "%s", file );
            fd = fopen( file, "w" );
            if( fd ){
               if( fc ) {
                  printf("  Encoding (orig=%s)> ",enc0 );
                  scanf( "%s", text );
                  if( !strcmp( enc0, text ) && fc != NULL ){
                     printf("Using existing FitsChan.\n");
                     fc2 = astClone( fc );
                     fc = astAnnul( fc );
                     ch = NULL;
                     astClearCard( fc2 );
                     astSet( fc2, "encoding=%s", status, text );
                  } else {
                     printf("Creating new FitsChan.\n");
                     fc2 = astFitsChan( NULL, sink, "encoding=%s", status, text );
                     astPutFits( fc2, "NAXIS1  = 1", 1 );
                     astPutFits( fc2, "NAXIS2  = 1", 1 );
                     astPutFits( fc2, "NAXIS3  = 1", 1 );
                     astPutFits( fc2, "NAXIS4  = 1", 1 );
                  }
               } else {
                  printf("  Encoding for new FitsChan > " );
                  scanf( "%s", text );
                  fc2 = astFitsChan( NULL, sink, "encoding=%s", status, text );
                  astPutFits( fc2, "NAXIS1  = 1", 1 );
                  astPutFits( fc2, "NAXIS2  = 1", 1 );
                  astPutFits( fc2, "NAXIS3  = 1", 1 );
                  astPutFits( fc2, "NAXIS4  = 1", 1 );
               }
               if( !astWrite( fc2, obj ) ){
                  printf("No object written to file '%s'.\n", file );
               }
               fc2 = astAnnul( fc2 );
               fclose( fd );
               fd = NULL;
            } else {
               astError( 1, "Cannot open file '%s'.", status, file );
            }

         } else if( opt == 5 ){
            nin = astGetNin( map );
            winmap = astWinMap( nin, NULL, NULL, NULL, NULL, "", status );

            printf( "\n   Enter %d axis shift values > ", nin );
            for( i = 0; i < nin; i++ ) scanf( "%lg", (winmap->a) + i );

            printf( "\n   Enter %d axis scale values > ", nin );
            for( i = 0; i < nin; i++ ) scanf( "%lg", (winmap->b) + i );

            if( !strcmp( class, "FrameSet" ) ){
               astInvert( winmap );
               astRemapFrame( obj, AST__BASE, winmap );
            } else {
               map2 = (AstMapping *) astCmpMap( winmap, map, 1, "", status );
               map = astAnnul( map );
               map = map2;
               obj = (AstObject *) map2;
            }
            winmap = astAnnul( winmap );

         } else if( opt == 6 ) {
            printf("Enter attribute specifications: ");
            scanf( "%s", text );
            astSet( map, text, status );
            if( !astOK ) astClearStatus;

         } else if( opt == 7 ) {
            printf("Enter attribute name: ");
            scanf( "%s", text );
            printf( "%s = %s\n", text, astGetC( map, text ) );
            if( !astOK ) astClearStatus;

         } else if( opt == 8 ) {
            printf( "\n  Output text file > ");
            scanf( "%s", file );
            fd = fopen( file, "w" );
            if( fd ){
               channel = astChannel( NULL, sink, "Full=1", status );
               astDump( ch, channel );
               channel = astAnnul( channel );
               fclose( fd );
               fd = NULL;
            } else {
               printf("Failed to open output file '%s'.\n", file );
               perror(" ");
            }

         } else if( opt == 9 ){
            diff( map, status );

         } else if( opt == 10 ){
            if( fc ) {
               printf("Enter FITS keyword: ");
               scanf( "%s", text );
               if( astGetFitsS( fc, text, &kvalue ) ) {
                  printf("%s = '%s'\n", text, kvalue );
               } else {
                  printf("%s not found\n", text );
               }
            } else {
               printf("No FITS keywords are available\n", text );
            }
         }
      }

   } else {
      astError( 1, "Object should be a Mapping).", status );
   }

   if( ch ) ch = astAnnul( ch );
   obj = astAnnul( obj );

}

void DisplayList( AstMapping *map0, int series, int invert, int *status ){
   int nmap, i, inv, res, junk;
   AstMapping *mi, *mf, **map_list, *m, *m2, *map, *smap, *tm1, *tm2;
   AstCmpMap *cm;
   AstTranMap *tm;
   AstChannel *channel;
   int invi, invf, *invert_list, opt, old_inv, nin, in[20], *out, nout;
   int isswitch, tfwd, tinv;
   const char *class;
   char file[ 50 ];
   double dv;

   nmap = 0;
   map_list = NULL;
   invert_list = NULL;
   map = astClone( map0 );

   isswitch = astIsASwitchMap( map );
   if( isswitch ) {
      astSwitchList( (AstSwitchMap *) map, invert, &nmap, &map_list,
                     &invert_list, status );
   } else {
      astMapList( map, series, invert, &nmap, &map_list, &invert_list );
   }

   while( 1 ){
      if( isswitch ) {
         printf("\nSwitchMap: (fwd selector, inv selector, routes )\n" );
         printf("  -3 - <transform points>\n");
      } else {
         printf("\nMappings in %s:\n", series?"series":"parallel" );
         printf("  -8 - <invert>\n");
         printf("  -7 - <write list to text file>\n");
         printf("  -6 - <split>\n");
         printf("  -5 - <simplify whole list>\n");
         printf("  -4 - <merge a single mapping>\n");
         printf("  -3 - <transform points>\n");
         printf("  -2 - <show to text file>\n");
         printf("  -1 - <show to screen>\n");
      }
      printf("  0 - <up>\n");
      for( i = 0; i < nmap; i++ ){
         m = map_list[ i ];
         class = m ? astGetClass( m ) :"(NULL)";
         printf( "  %d - %s\n", i + 1, class );
      }
      printf("\n  Option > ", 0 );
      opt = 0;
      scanf("%d", &opt );

      if ( opt == -8 ) {

         for( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );
         map_list = astFree( map_list );
         invert_list = astFree( invert_list );
         nmap = 0;

         invert = ( invert == 0 );
         if( isswitch ) {
            astSwitchList( (AstSwitchMap *) map, invert, &nmap,
                            &map_list, &invert_list, status );
         } else {
            astMapList( map, series, invert, &nmap, &map_list, &invert_list );
         }

      } else if ( opt == -7 && !isswitch ) {
         printf( "\n  Output text file > ");
         scanf( "%s", file );
         fd = fopen( file, "w" );
         if( fd ){
            channel = astChannel( NULL, sink, "", status );
            for( i = 0; i < nmap; i++ ) {
               inv = astGetInvert( map_list[ i ] );
               astSetInvert(  map_list[ i ], invert_list[ i ] );
               astDump( map_list[ i ], channel );
               astSetInvert(  map_list[ i ], inv );
            }
            channel = astAnnul( channel );
            fclose( fd );
            fd = NULL;
         } else {
            printf("Failed to open output file.\n" );
         }

      } else if( opt == -6 && !isswitch ) {
         old_inv = astGetInvert( map );
         astSetInvert( map, invert );

         printf( "\n   Pick how many input axes (max=%d)? > ",
                 astGetNin( map ) );
         scanf( "%lg", &dv );
         nin = dv;

         printf( "\n   Enter %d zero-based axis indices > ", nin );
         for( i = 0; i < nin; i++ ) {
            scanf( "%lg", &dv );
            in[ i ] = dv;
         }

         out = astMapSplit( map, nin, in, &smap );
         astSetInvert( map, old_inv );

         if( out ) {
            printf( "Corresponding outputs are: " );
            nout = astGetNout( smap );
            for( i = 0; i < nout; i++ ) printf( "%d ", out[ i ] );
            printf("\n");
            out = astFree( out );
            map = astAnnul( map );
            map = smap;
         } else {
            printf( "Selected inputs do not correspond to a subset of outputs\n");
         }

         for( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );
         map_list = astFree( map_list );
         invert_list = astFree( invert_list );

         nmap = 0;
         astMapList( map, series, invert, &nmap, &map_list, &invert_list );

      } else if( opt == -5 && !isswitch ) {
         old_inv = astGetInvert( map );
         astSetInvert( map, invert );
         smap = astSimplify( map );
         if( smap == map ) printf("\n  No simplification performed\n" );
         astSetInvert( map, old_inv );
         map = astAnnul( map );
         map = smap;

         for( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );
         map_list = astFree( map_list );
         invert_list = astFree( invert_list );

         nmap = 0;
         astMapList( map, series, invert, &nmap, &map_list, &invert_list );

      } else if( opt == -4 && !isswitch ) {
         printf("\n  Mapping index > ", 0 );
         scanf("%d", &i );
         i--;
         res = astMapMerge( map_list[ i ], i, series, &nmap,
                            &map_list, &invert_list );
         if( res != -1 ) {
            printf( "Index of first modified Mapping: %d\n", res + 1 );
         } else {
            printf( "No mappings modified\n" );
         }

      } else if( opt == -3 && series && !isswitch ) {

         if( invert == astGetInvert( map ) ) {
            tfwd = astGetTranForward( map );
            tinv = astGetTranInverse( map );
         } else {
            tinv = astGetTranForward( map );
            tfwd = astGetTranInverse( map );
         }

         if( !tfwd ) {
            printf( "\n   No forward transformation available\n" );

         } else if( !tinv ) {
            printf( "\n   No inverse transformation available\n" );
            tranm( map_list, invert_list, nmap, 0, status );

         } else {
            tranm( map_list, invert_list, nmap, 1, status );

         }

      } else if( opt == -3 && isswitch ) {

         if( invert == astGetInvert( map ) ) {
            tfwd = astGetTranForward( map );
            tinv = astGetTranInverse( map );
         } else {
            tinv = astGetTranForward( map );
            tfwd = astGetTranInverse( map );
         }

         if( !tfwd ) {
            printf( "\n   No forward transformation available\n" );

         } else if( !tinv ) {
            printf( "\n   No inverse transformation available\n" );
            tranm( &map, &invert, 1, 0, status );

         } else {
            tranm( &map, &invert, 1, 1, status );

         }

      } else if( opt == -3 && !series && !isswitch ) {
         old_inv = astGetInvert( map );
         astSetInvert( map, invert );
         tran( map, status );
         astSetInvert( map, old_inv );

      } else if ( opt == -2 && !isswitch ) {

         for( i = 0; i < nmap; i++ ) {
            inv = astGetInvert( map_list[ i ] );
            astSetInvert(  map_list[ i ], invert_list[ i ] );
            if( i == 0 ) {
               tm1 = astClone( map_list[ 0 ] );
            } else {
               tm2 = (AstMapping *) astCmpMap( tm1, map_list[ i ], series, "", status );
               tm1 = astAnnul( tm1 );
               tm1 = tm2;
            }
            astSetInvert(  map_list[ i ], inv );
         }

         printf( "\n  Output text file > ");
         scanf( "%s", file );
         fd = fopen( file, "w" );
         if( fd ){
            channel = astChannel( NULL, sink, "", status );
            old_inv = astGetInvert( tm1 );
            astSetInvert( tm1, invert );
            astDump( tm1, channel );
            astSetInvert( tm1, old_inv );
            channel = astAnnul( channel );
            fclose( fd );
            fd = NULL;
         } else {
            printf("Failed to open output file.\n" );
         }
         tm1= astAnnul( tm1 );

      } else if ( opt == -1 && !isswitch ) {


         for( i = 0; i < nmap; i++ ) {
            inv = astGetInvert( map_list[ i ] );
            astSetInvert(  map_list[ i ], invert_list[ i ] );
            if( i == 0 ) {
               tm1 = astClone( map_list[ 0 ] );
            } else {
               tm2 = (AstMapping *) astCmpMap( tm1, map_list[ i ],
                                               series, "", status );
               tm1 = astAnnul( tm1 );
               tm1 = tm2;
            }
            astSetInvert(  map_list[ i ], inv );
         }

         channel = astChannel( NULL, NULL, "Full=1", status );
         old_inv = astGetInvert( tm1);
         astSetInvert( tm1, invert );
         astDump( tm1, channel );
         astSetInvert( tm1, old_inv );
         channel = astAnnul( channel );
         tm1= astAnnul( tm1 );

      } else if( opt > 0 && opt <= nmap && map_list[ opt - 1 ] ){
         m2 = map_list[ opt - 1 ];
         inv = invert_list[ opt - 1 ];
         class = astGetClass( m2 );

         if( m2 && astIsAFrameSet( m2 ) ){
            m = astGetMapping( m2, AST__BASE, AST__CURRENT );
            class = astGetClass( m );

         } else if( astIsATranMap( m2 ) ){

            astDecompose( m2, &mf, &mi, &invf, &invi, &junk );

            if( inv ) {
               m = mi;
               inv = invi;
            } else {
               m = mf;
               inv = invf;
            }
            class = astGetClass( m );

         } else {
            m = astClone( m2 );
         }

         if( strcmp( class, "CmpMap" ) ){
            DisplayList( m, 1, inv, status );
         } else {
            cm = (AstCmpMap *) m;
            DisplayList( m, cm->series, inv, status );
         }
      } else {
         break;
      }
   }

   for( i = 0; i < nmap; i++ ) map_list[ i ] = astAnnul( map_list[ i ] );
   map_list = astFree( map_list );
   invert_list = astFree( invert_list );
   nmap = 0;

}


void sink( const char *text ){
   if( fd && text ) fprintf( fd, "%s\n", text );
}

const char *source( void ){
   static char text[2000];
   static char *ptext;
   if( fd ) {
      ptext = fgets( text, 2000, fd );
   } else {
      ptext = NULL;
   }
   return ptext;
}

void tran( AstMapping *map, int *status ){
   char text[2000];
   AstFrame *infrm, *outfrm;
   AstPointSet *pset1, *pset2;
   double **ptr1, **ptr2;
   int nin, nout, i, ok;

   if( !astGetTranForward( map ) ) {
      printf( "\n   No forward transformation available\n" );
      return;
   }

   if( !astGetTranInverse( map ) ) {
      printf( "\n   No inverse transformation available\n" );
   }

   nin = astGetNin( map );
   nout = astGetNout( map );
   pset1 = astPointSet( 1, nin, "", status );
   ptr1 = astGetPoints( pset1 );
   pset2 = astPointSet( 1, nout, "", status );
   ptr2 = astGetPoints( pset2 );

   if( astIsAFrameSet( map ) ) {
      infrm = astGetFrame( map, AST__BASE );
      outfrm = astGetFrame( map, AST__CURRENT );
   } else if( astIsAFrame( map ) ) {
      infrm = (AstFrame *) map;
      outfrm = (AstFrame *) map;
   } else {
      infrm = NULL;
      outfrm = NULL;
   }

   if( astOK ){
      ok = 0;
      if( infrm != NULL ) {
         ok = 1;
         for( i = 0; i < nin; i++ ) {
            printf( "\n   Enter %s value (or <bad>) > ", astGetLabel( infrm, i ) );
            scanf( "%s", text );
            if( !astUnformat( infrm, i, text, ptr1[ i ] ) ) {
               ok = 0;
               printf( "Bad formatted value supplied! Now try unformatted axis values...\n");
               break;
            }
         }
      }

      if( !ok ) {
         printf( "\n   Enter %d axis values (BAD=999) > ", nin );
         for( i = 0; i < nin; i++ ) {
            scanf( "%lg", ptr1[ i ] );
            if( ptr1[ i ][ 0 ] == 999.0 ) ptr1[ i ][ 0 ] = AST__BAD;
         }
      }

      astTransform( map, pset1, 1, pset2 );
      printf( "\n   Forward: [ " );
      for( i = 0; i < nin; i++ ) {
         if( ptr1[ i ][ 0 ] == AST__BAD ) {
            printf( "999.0 " );
         } else {
            printf( "%.*g ", DBL_DIG, ptr1[ i ][ 0 ] );
         }
      }

      printf( "] -> [ " );
      for( i = 0; i < nout; i++ ) {
         if( ptr2[ i ][ 0 ] == AST__BAD ) {
            printf( "999.0 " );
         } else {
            printf( "%.*g ", DBL_DIG, ptr2[ i ][ 0 ] );
         }
      }
      printf( "]\n" );

      if( infrm != NULL && outfrm != NULL ) {
         printf( "            ( " );
         for( i = 0; i < nin; i++ ) {
            printf( "%s ", astFormat( infrm, i, ptr1[ i ][ 0 ] ) );
         }
         printf( ") -> ( " );
         for( i = 0; i < nout; i++ ) {
            printf( "%s ", astFormat( outfrm, i, ptr2[ i ][ 0 ] ) );
         }
         printf( ") formatted\n" );
      }


      if( astGetTranInverse( map ) ) {
         astTransform( map, pset2, 0, pset1 );
         printf( "\n   Inverse: [ " );
         for( i = 0; i < nout; i++ ) {
            if( ptr2[ i ][ 0 ] == AST__BAD ) {
               printf( "999.0 " );
            } else {
               printf( "%.*g ", DBL_DIG, ptr2[ i ][ 0 ] );
            }
         }

         printf( "] -> [ " );
         for( i = 0; i < nin; i++ ) {
            if( ptr1[ i ][ 0 ] == AST__BAD ) {
               printf( "999.0 " );
            } else {
               printf( "%.*g ", DBL_DIG, ptr1[ i ][ 0 ] );
            }
         }
         printf( "]\n" );


         if( infrm != NULL && outfrm != NULL ) {
            printf( "            ( " );
            for( i = 0; i < nout; i++ ) {
               printf( "%s ", astFormat( outfrm, i, ptr2[ i ][ 0 ] ) );
            }
            printf( ") -> ( " );
            for( i = 0; i < nin; i++ ) {
               printf( "%s ", astFormat( infrm, i, ptr1[ i ][ 0 ] ) );
            }
            printf( ") formatted\n" );
         }
      }
   }
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );
}

void tranm( AstMapping **map, int *inv_list, int nmap, int inv, int *status ){
   AstPointSet *pset1, *pset2;
   double **ptr1, **ptr2;
   int nin, nout, i, j, inv0;

   inv0 = astGetInvert( map[ 0 ] );
   astSetInvert( map[ 0 ], inv_list[ 0 ] );
   nin = astGetNin( map[ 0 ] );
   astSetInvert( map[ 0 ], inv0 );

   pset1 = astPointSet( 1, nin, "", status );
   ptr1 = astGetPoints( pset1 );

   if( astOK ){
      printf( "\n   Enter %d axis values (BAD=999) > ", nin );
      for( i = 0; i < nin; i++ ) {
         scanf( "%lg", ptr1[ i ] );
         if( ptr1[ i ][ 0 ] == 999.0 ) ptr1[ i ][ 0 ] = AST__BAD;
      }

      printf("Forward...\n");
      for( j=0; j<nmap; j++ ) {
         inv0 = astGetInvert( map[ j ] );
         astSetInvert( map[ j ], inv_list[ j ] );

         nout = astGetNout( map[ j ] );
         pset2 = astPointSet( 1, nout, "", status );
         ptr2 = astGetPoints( pset2 );

         astTransform( map[ j ], pset1, 1, pset2 );

         printf( " Mapping %d gave [ ", j+1 );
         for( i = 0; i < nout; i++ ) {
            if( ptr2[ i ][ 0 ] == AST__BAD ) {
               printf( "999.0 " );
            } else {
               printf( "%.*g ", DBL_DIG, ptr2[ i ][ 0 ] );
            }
         }

         printf( "]\n" );

         pset1 = astAnnul( pset1 );
         astSetInvert( map[ j ], inv0 );

         pset1 = pset2;
         ptr1 = ptr2;
         nin = nout;
      }

      printf("Inverse...\n");

      for( j=nmap - 1; j>=0 && inv; j-- ) {
         inv0 = astGetInvert( map[ j ] );
         astSetInvert( map[ j ], inv_list[ j ] );

         nin = astGetNin( map[ j ] );
         pset2 = astPointSet( 1, nin, "", status );
         ptr2 = astGetPoints( pset2 );

         astTransform( map[ j ], pset1, 0, pset2 );

         printf( " Mapping %d gave [ ", j+1 );
         for( i = 0; i < nin; i++ ) {
            if( ptr2[ i ][ 0 ] == AST__BAD ) {
               printf( "999.0 " );
            } else {
               printf( "%.*g ", DBL_DIG, ptr2[ i ][ 0 ] );
            }
         }
         printf( "]\n" );

         pset1 = astAnnul( pset1 );
         astSetInvert( map[ j ], inv0 );

         pset1 = pset2;
         ptr1 = ptr2;
         nout = nin;
      }

      pset1 = astAnnul( pset1 );

   }
}




void diff( AstMapping *map, int *status ){
   char text[200];
   AstFrame *infrm, *outfrm;
   int nin, nout, i, ok, ax1, ax2;
   double at[2000];


   nin = astGetNin( map );
   nout = astGetNout( map );

   if( astIsAFrameSet( map ) ) {
      infrm = astGetFrame( map, AST__BASE );
      outfrm = astGetFrame( map, AST__CURRENT );
   } else if( astIsAFrame( map ) ) {
      infrm = (AstFrame *) map;
      outfrm = (AstFrame *) map;
   } else {
      infrm = NULL;
      outfrm = NULL;
   }

   if( astOK ){
      ok = 0;
      if( infrm != NULL ) {
         ok = 1;
         for( i = 0; i < nin; i++ ) {
            printf( "\n   Enter %s value (or <bad>) > ", astGetLabel( infrm, i ) );
            scanf( "%s", text );
            if( !astUnformat( infrm, i, text, at + i ) ) {
               ok = 0;
               printf( "Bad formatted value supplied! Now try unformatted axis values...\n");
               break;
            }
         }
      }

      if( !ok ) {
         printf( "\n   Enter %d axis values (BAD=999) > ", nin );
         for( i = 0; i < nin; i++ ) {
            scanf( "%lg", at + i );
            if( at[ i ] == 999.0 ) at[ i ] = AST__BAD;
            printf("(got %.*g)\n", DBL_DIG, at[ i ] );
         }

      }

      for( i = 0; i < nin; i++ ) {
         printf("(%d: got %.*g)\n", i, DBL_DIG, at[ i ] );
      }

      printf( "\n   Enter output to differentiate (zero-based index): > " );
      scanf( "%d", &ax1 );
      printf( "\n   Enter input to vary (zero-based index): > " );
      scanf( "%d", &ax2 );
      printf( "%.*g \n", DBL_DIG, astRate( map, at, ax1, ax2 ) );
   }
}

