import java.io.File;
import javax.xml.transform.Source;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
// import javax.xml.transform.sax.SAXSource;

/**
 * Basic Java program to perform an XML transformation using an XSLT
 * stylesheet.  Usage is simple:
 * <pre>
 *    java Transformer [input.xml] stylesheet.xslt [output.xml]
 * </pre>
 *
 * If the <code>input.xml</code> argument is omitted, input comes
 * from <code>stdin</code>, and similarly for <code>output.xml</code>.
 */
public class Transformer {

    public static void main(String args[]) {
        Source xmlIn = null;
        Result xmlOut = null;
        File xsltFile = null;

        switch (args.length) {
          case 1:
            xmlIn = new StreamSource(System.in);
            xsltFile = new File(args[0]);
            xmlOut = new StreamResult(System.out);
            break;

          case 2:
            xmlIn = new StreamSource(new File(args[0]));
            xsltFile = new File(args[1]);
            xmlOut = new StreamResult(System.out);
            break;

          case 3:
            xmlIn = new StreamSource(new File(args[0]));
            xsltFile = new File(args[1]);
            xmlOut = new StreamResult(new File(args[2]));
            break;

          default:
            Usage();
            break;
        }

        assert xmlIn != null && xmlOut != null && xsltFile != null;

        try {
            javax.xml.transform.Transformer trans
                    = javax.xml.transform.TransformerFactory
                    .newInstance()
                    .newTransformer(new StreamSource(xsltFile));

            // Do the transformation
            trans.transform(xmlIn, xmlOut);
            
        } catch (javax.xml.transform.TransformerException e) {
            System.err.println("Transformer exception: " + e);
        }   
    }

    private static void Usage() {
        System.err.println("Usage: java Transformer [input.xml] stylesheet.xslt [output.xml]");
        System.exit(1);
    }
}

            
