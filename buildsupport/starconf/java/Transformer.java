import java.io.File;
import javax.xml.transform.Source;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import java.util.regex.Pattern;
import java.util.regex.Matcher;


/**
 * Basic Java program to perform an XML transformation using an XSLT
 * stylesheet.  Usage is simple:
 * <pre>
 *    java Transformer stylesheet.xslt [param=value...]
 *    java Transformer input.xml stylesheet.xslt [param=value...]
 *    java Transformer input.xml stylesheet.xslt output.xml [param=value...]
 * </pre>
 *
 * If the <code>input.xml</code> argument is omitted, input comes
 * from <code>stdin</code>, and similarly for <code>output.xml</code>.
 *
 * <p>The possibly multiple <code>param=value</code> are parsed as parameters
 * which are passed on to the transformation.  The acceptable syntax for
 * the parameters does not (at present) permit any namespace specification.
 */
public class Transformer {

    public static void main(String args[]) {
        int nfiles = args.length; // default assumes all args are filenames

        // XSLT params may be full QNames.  They're specified in
        // <http://www.w3.org/TR/REC-xml-names/>.  The Java Transformer class
        // will accept a namespace declaration in {...}, but we don't allow
        // that (yet).
        Pattern var = Pattern.compile("([a-zA-Z_][a-zA-Z0-9._-]*)=(.*)");


        // Work through the arguments, stopping when we first find a
        // variable setting (matching pattern 'var').  Save this index
        // as the number of files in the list.
        for (int i=0; i<args.length; i++) {
            Matcher m = var.matcher(args[i]);
            if (m.matches()) {
                nfiles = i;
                break;
            }
        }

        Source xmlIn = null;    // input to the transformation
        Result xmlOut = null;   // output from the transformation
        File xsltFile = null;   // XSLT source specifying the transformation

        switch (nfiles) {
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

            // Now work through the remaining arguments, expecting/demanding
            // that each of them is a param=value setting.
            for (int i=nfiles; i<args.length; i++) {
                Matcher m = var.matcher(args[i]);
                if (m.matches()) {
                    trans.setParameter(m.group(1), m.group(2));
                } else {
                    Usage();
                }
            }

            // Do the transformation
            trans.transform(xmlIn, xmlOut);
            
        } catch (javax.xml.transform.TransformerException e) {
            System.err.println("Transformer exception: " + e);
        }   
    }

    private static void Usage() {
        System.err.println("Usage: java Transformer [input.xml] stylesheet.xslt [output.xml] [param=value...]");
        System.exit(1);
    }
}

            
