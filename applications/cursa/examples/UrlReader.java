import java.io.*;
import java.net.*;

//+
// UrlReader
//
// A simple Java program to retrieve the contents of a URL and write
// them to standard output.
//
// Author:
//   A C Davenhall (Edinburgh)
// History:
//   27/2/97 (ACD): Original version.
//-

class UrlReader
{  public static void main (String argv[] )
   {  try
      {  String urlName = argv[0];
         URL    url     = new URL(urlName);

         InputStream iStream = url.openStream();

         int b;

         while( (b = iStream.read())  !=  -1)
         {  System.out.write( (byte)b );
         }
      }
      catch (Exception ex)
      {  ex.printStackTrace();
      }
   }
}
