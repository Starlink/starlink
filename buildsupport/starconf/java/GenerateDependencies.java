import org.w3c.dom.*;
import java.io.PrintStream;
import java.io.IOException;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import java.util.regex.Pattern;


/**
 * Generates a Makefile fragment containing the dependencies expressed
 * in a componentinfo XML file.  For use only within the project, as
 * its behaviour might change with little warning.
 *
 * <p>Usage:
 * <pre>
 * java GenerateDependencies --makefile=Makefile.dependencies componentset.xml
 * </pre>
 * <p>The input XML file must conform to the
 * <code>componentinfo.dtd</code> DTD, with a top-level element of
 * <code>&lt;componentset&gt;</code>.
 *
 * <p>All options:
 * <dl>
 * <dt><code>--buildsequence=filename</code>
 * <dd>Writes a file which lists all the non-obsolete,
 * non-buildsupport, components, in a sequence which respects the
 * dependencies read from the input file.
 * <dt><code>--makefile[=filename]</code>
 * <dd>Names the output makefile.  If no argument is given, or if the
 * argument is "-", the makefile is written to stdout.
 * <dt><code>--flatdeps=filename</code>
 * <dd>Write an XML file containing just the flattened build and
 * sourceset dependencies.  There is no pre-defined DTD for this, but
 * the structure should be obvious, and it will be well-formed.  The
 * structure of this is as yet provisional, and if there is further
 * derived information which it would be useful to generate here,
 * both the structure of the file and the name of this option might change.
 * <dt><code>--verbose</code>
 * <dd>Turn on chattering.  Lots of it.
 * </dl>
 * <p>At least one of <code>--makefile</code>,
 * <code>--buildsequence</code> or <code>--flatdeps</code> must be specified.
 *
 * @author Norman Gray
 */
public class GenerateDependencies {

    private static boolean verbose = false;
    private static boolean testMode = false;

    /**
     * Global status, returned at end, and incremented internally on errors.
     */
    private static int globalStatus = 0;

    public static void main (String[] args) {
        String xmlinput = null;
        PrintStream makefileStream = null;
        PrintStream buildSequenceStream = null;
        PrintStream flatdepsStream = null;
        Pattern optionPattern = Pattern.compile("^--([a-z]*)(=(.*))?");

        try {

            for (int i=0; i<args.length; i++) {
                java.util.regex.Matcher m = optionPattern.matcher(args[i]);
                if (m.matches()) {
                    String opt = m.group(1);
                    if (opt.equals("buildsequence")) {
                        String fn = m.group(3);
                        if (fn == null || fn.length() == 0)
                            // no argument
                            Usage();
                        buildSequenceStream = openStream
                                (fn, (verbose ? "Build sequence" : null));
                    } else if (opt.equals("makefile")) {
                        String makefileName = m.group(3);
                        if (makefileName == null
                            || makefileName.length()==0
                            || makefileName.equals("-")) {
                            makefileStream = System.out;
                        } else {
                            makefileStream = openStream
                                (makefileName,(verbose ? "Makefile" : null));
                        }
                    } else if (opt.equals("flatdeps")) {
                        String fn = m.group(3);
                        if (fn == null || fn.length() == 0)
                            Usage();
                        flatdepsStream = openStream
                                (fn, (verbose ? "Flattened dependencies" : null));
                    } else if (opt.equals("test")) {
                        testMode = true;
                    } else if (opt.equals("verbose")) {
                        verbose = true;
                    } else {
                        Usage();
                    }
                } else {
                    if (xmlinput != null)
                        Usage();
                    else
                        xmlinput = args[i];
                }
            }

            if (xmlinput == null)
                Usage();

            if (makefileStream == null
                && buildSequenceStream == null
                && flatdepsStream == null)
                Usage();

            javax.xml.parsers.DocumentBuilder db
                    = javax.xml.parsers.DocumentBuilderFactory
                    .newInstance().newDocumentBuilder();
            Document dom = db.parse(xmlinput);
            Element componentset = dom.getDocumentElement();
            NodeList componentList
                    = componentset.getElementsByTagName("component");

            for (int i=0; i<componentList.getLength(); i++) {
                // Run through all of the <component> elements,
                // creating a Component object for each.  Discard the
                // return value below.
                Component.newComponent((Element)componentList.item(i));
            }

        } catch (IOException e) {
            System.err.println("IOException: " + e);
        } catch (org.xml.sax.SAXException e) {
            System.err.println("SAXException: " + e);
        } catch (javax.xml.parsers.ParserConfigurationException e) {
            System.err.println("Can't create parser: " + e);
        }


        // We have now ingested the set of components and their
        // dependencies.  So now run through the set of components in
        // allComponents, writing out the Makefile fragment.  We
        // include all <sourceset> and <include> dependencies, plus
        // <link> dependencies if the <dependencies> element has
        // buildincludeslink="yes"

        if (makefileStream != null)
            makeMakefile(makefileStream);

        if (buildSequenceStream != null) {
            List buildSequence = makeBuildSequence();
            for (Iterator li = buildSequence.iterator(); li.hasNext(); ) {
                buildSequenceStream.println((Component)li.next());
            }
        }

        if (flatdepsStream != null)
            makeFlatdeps(flatdepsStream);

        System.exit(globalStatus);
    }

    private static void Usage() {
        System.err.println("GenerateDependencies [--test] [--verbose] "+
                           "[--makefile[=filename]] [--buildsequence=filename] "+
                           "[--flatdeps=filename] xml-file");
        System.err.println("    At least one of --makefile, --buildsequence " +
                           "or --flatdeps must be specified");
        System.exit(1);
    }

    /**
     * Opens the named file as a PrintStream.
     * @param fn the name of the file to open
     * @param name if non-null, a message is written to System.err
     * describing the action.
     * @return an open PrintStream
     * @throws IOException if there is a problem opening the stream
     */
    private static PrintStream openStream(String fn, String name)
            throws IOException {
        try {

            PrintStream newStream
                    = new PrintStream(new java.io.FileOutputStream(fn));
            if (name != null) {
                System.err.println(name + " written to " + fn);
            }
            return newStream;

        } catch (java.io.FileNotFoundException e) {
            throw new IOException("Can't create file: " + e);
        }
    }

    /**
     * Display a set's contents.  Mostly, but not entirely, a debugging
     * method.
     */
    private static String showSet(Set s) {
        return showSet(s, "{", "}");
    }

    private static String showSet(Set s, String prefix, String suffix) {
        StringBuffer sb = new StringBuffer(prefix);
        if (s == null)
            sb.append(" <null>");
        else
            for (Iterator si=s.iterator(); si.hasNext(); )
                sb.append(' ').append(si.next().toString());
        sb.append(' ');
        sb.append(suffix);
        return sb.toString();
    }

    /**
     * Generate a Makefile go the given stream, expressing all the
     * various types of dependencies.  At its core, this just calls
     * getCompleteDependencies on each component in turn, and writes
     * those out as Makefile dependencies, but it also emits slightly
     * variant versions for buildsupport, configure and obsolete dependencies,
     * and adds various cosmetic and convenience targets.
     *
     * @param makefile an open stream to receive the makefile
     */
    private static void makeMakefile(PrintStream makefile) {

        String manifestString = "$(MANIFESTS)/";
        String newlineString = " \\\n\t\t";
        String makeBuildsupportString =
            "\t    && if test -f bootstrap; then \\\n" +
            "\t       ./bootstrap > bootstrap.log; \\\n" +
            "\t    fi \\\n" +
            "\t    && if test -n \"$$BUILDSUPPORT_PREFIX\"; then \\\n" +
            "\t        ./configure --prefix=$$BUILDSUPPORT_PREFIX \\\n" +
            "\t            >configure-output.log; \\\n" +
            "\t    elif test ! -f Makefile; then \\\n" +
            "\t        { t=\"Directory unconfigured but BUILDSUPPORT_PREFIX undefined\";\\\n " +
            "\t          echo $$t >configure-output.log; echo $$t >&2; \\\n" +
            "\t          exit 1; }; \\\n\t    else \\\n" +
            "\t        echo \"No configuration necessary\" >configure-output.log; \\\n" +
            "\t    fi";
        String runConfigureString = "\t  && $(CONFIG_CPT)";

        String makeString = "\t  && $(MAKE_CPT)";
        String cdString = "\tcd ";

        if (testMode) {
            manifestString = "";
            newlineString = "\n  ";
            makeBuildsupportString = "  (buildsupport)";
            runConfigureString = "  (configdep)";
            makeString = "  --";
            cdString = "  path=";
        }

        // Banner at the top of the output file
        if (! testMode) {
            String[] banner = {
                "# This file is generated by GenerateDependencies.java.",
                "# It must be included into another makefile,",
                "# and that file should define the variable MANIFESTS",
                "# to point to the directory which contains the collection",
                "# of manifest files, typically /star/manifests.",
                "#",
                "# If the environment variable BUILDSUPPORT_PREFIX is defined",
                "# then the buildsupport tools will be REconfigured when they",
                "# are built; if not, they must be configured already.",
                "#",
                "# As well as defining each of the manifest targets, this",
                "# file also defines the following targets:",
                "#   buildsupport       - build each of the buildsupport tools",
                "#   clean-buildsupport - clean the buildsupport tools",
                "#   configure-deps     - build the set of components which",
                "#                        must be built before ./configure",
                "# It additionally defines the variable SUPPORTED_COMPONENTS,",
                "# containing the list of manifest files corresponding to",
                "# components marked `supported' in their component.xml file.",
                "# No other targets or variables defined here should be",
                "# regarded as persistent or otherwise relied upon.",
                "",
                "",
            };
            for (int i=0; i<banner.length; i++)
                makefile.println(banner[i]);

            makefile.println("CONFIG_CPT=test -f config.status \\\n" +
                             "  || " +
                             "./configure >configure-output.log 2>configure-output.log.err \\\n" +
                             "  || " +
                             "{ cat configure-output.log.err; false; }");
            makefile.println("MAKE_CPT=(make && make install) >make.log 2>make.log.err \\\n" +
                             "  || " +
                             "{ cat make.log.err; false; }");
            makefile.println();
        }

        java.util.List allbuildsupport = new java.util.LinkedList();

        for (Iterator ci = Component.allComponents(); ci.hasNext(); ) {
            Component c = (Component) ci.next();
            boolean is_obsolete = false;

            // For each component in the list, extract all the
            // SOURCESET and BUILD dependencies and emit them as
            // Makefile dependencies of this component.  The
            // getCompleteDependencies method resolves any
            // option="..." options in the input set of dependencies.
            //
            // Because this resolves the _complete_ set of
            // dependencies, it includes dependencies which make could
            // work out for itself.  This redundancy does no harm.
            //
            // The method getCompleteDependencies additionally checks
            // for dependency cycles, for all except LINK dependencies.

            Set alldeps = c.getCompleteDependencies(Dependency.SOURCESET);
            if (alldeps == null) {
                System.err.println
                        ("Circularity detected in sourceset dependencies of "
                         + c.getName());
                if (testMode)
                    makefile.println("Component " + c
                                       + ": circular sourceset dependencies");
                else
                    globalStatus++;
                continue;
            }

            Set builddeps = c.getCompleteDependencies(Dependency.BUILD);
            if (builddeps == null) {
                System.err.println
                        ("Circularity detected in build dependencies of "
                         + c.getName());
                if (testMode)
                    makefile.println("Component " + c
                                       + ": circular build dependencies");
                else
                    globalStatus++;
                continue;
            }
            alldeps.addAll(builddeps);

            // What type of component is this?
            String componentType = null;
            if (Dependency.getConfigureDependencies().contains(c))
                componentType = "confdep";
            if (c.getBuildsupport() != Component.BUILDSUPPORT_NO) {
                if (componentType != null) {
                    System.err.println("Warning: Component " + c
                                       + " is a buildsupport component, but is also " +
                                       "marked as a configure dependency -- latter ignored");
                }
                componentType = "buildsupport";
            }

            // Comments before the target
            if (componentType == "confdep")
                makefile.println("# Component " + c
                 + " is a configure dependency, so is configurable in place.");
            if (componentType == "buildsupport")
                makefile.println("# Component " + c
                                   + " is a buildsupport component");
            Set confdeps = c.getCompleteDependencies(Dependency.CONFIGURE);
            if (confdeps != null && confdeps.size() > 0) {
                makefile.println("# Component " + c
                                 + " has configure dependencies on "
                                 + showSet(confdeps) + ",");
                makefile.println("# which must be built before this component is configured.");
            }
            if (c.getStatus() == Component.STATUS_OBSOLETE) {
                makefile.println("# Component " + c
                                   + " is OBSOLETE -- do not build");
                is_obsolete = true;
            }

            // Emit the target
            makefile.print(manifestString + c.getName() + ':');


            // Emit the list of dependencies
            Component lastComponent = null;
            for (Iterator cpts=alldeps.iterator(); cpts.hasNext(); ) {
                Component cpt = ((Dependency)cpts.next()).component();
                // Don't emit duplicate dependencies;
                // and don't emit a dependency of a component on itself
                // (which is possible but harmless, and confuses make).
                if (cpt != lastComponent && cpt != c) {
                    makefile.print(newlineString + manifestString + cpt);
                    lastComponent = cpt;
                }

                if ((cpt.getStatus() == Component.STATUS_OBSOLETE)
                        && ! is_obsolete) {
                    System.err.println("Component " + c.getName()
                                       + " depends on obsolete component "
                                       + cpt.getName());
                }
            }
            makefile.println();

            // Emit the rule
            makefile.println(cdString + c.componentPath() + " \\");
            if (componentType == "buildsupport") {
                makefile.println(makeBuildsupportString + " \\");
                allbuildsupport.add(c);
            } else if (componentType == "confdep") {
                makefile.println(runConfigureString + " \\");
            }
            makefile.println(makeString);
            makefile.println();
        }

        // Generate a list of all the supported components
        makefile.println("# List of all supported components");
        makefile.print("SUPPORTED_COMPONENTS =");
        for (Iterator ci = Component.allComponents(); ci.hasNext(); ) {
            Component c = (Component) ci.next();

            if (c.getSupported() == Component.SUPPORTED_YES) {
                if (c.getStatus() == Component.STATUS_OBSOLETE) {
                    System.err.println("Component " + c.getName()
                                       + " is both supported and obsolete!");
                } else {
                    makefile.println(" \\");
                    makefile.print("\t$(MANIFESTS)/" + c.getName());
                }
            }
        }
        makefile.println();

        // Add all of the buildsupport tools to either autoBuildsupport or
        // nonautoBuildsupport, depending on whether they are or are not
        // (respectively) to be build automatically.
        StringBuffer autoBuildsupport = new StringBuffer();
        StringBuffer nonautoBuildsupport = new StringBuffer();
        for (Iterator i=allbuildsupport.iterator(); i.hasNext(); ) {
            Component c = (Component)i.next();
            (c.getBuildsupport() == Component.BUILDSUPPORT_AUTO
             ? autoBuildsupport
             : nonautoBuildsupport)
                    .append(newlineString)
                    .append(manifestString)
                    .append(c.getName());
        }
        makefile.println();
        makefile.println("# Buildsupport tools -- building and cleaning");
        makefile.println("BUILDSUPPORT_MANIFESTS ="
                           + autoBuildsupport.toString());
        makefile.println("# Buildsupport tools which are conditionally built");
        makefile.println("EXTRA_BUILDSUPPORT_MANIFESTS ="
                           + nonautoBuildsupport.toString());
        makefile.println();
        makefile.println("# Make all the buildsupport tools which are");
        makefile.println("# unconditionally build");
        makefile.println("buildsupport: $(BUILDSUPPORT_MANIFESTS)");
        makefile.println();
        makefile.println("# Clean out the buildsupport tools,");
        makefile.println("# including any conditionally built ones");
        makefile.println("clean-buildsupport:");
        makefile.println("\trm -f $(BUILDSUPPORT_MANIFESTS) $(EXTRA_BUILDSUPPORT_MANIFESTS)");

        // Emit the list of configure dependencies, if any
        Set confdeps = Dependency.getConfigureDependencies();
        makefile.println();
        if (confdeps.isEmpty()) {
            makefile.println("# No configure dependencies");
            makefile.println("configure-deps:");
            makefile.println("\techo \"There are no configure dependencies\"");
        } else {
            makefile.println("# Configuration dependencies");
            makefile.println("# Run 'make configure-deps' to make these components before ./configure");
            makefile.print("configure-deps:");
            for (Iterator cdi=confdeps.iterator(); cdi.hasNext(); ) {
                Component c = (Component)cdi.next();
                makefile.print(newlineString + manifestString + c);
            }
            makefile.println();
        }
    }

    /**
     * Creates a list of all the dependencies in an order which
     * respects the dependencies.  That is, each (Component) element
     * in the list comes after each of the components upon which it
     * depends.
     *
     * @param dependencies a Set of Dependency objects
     * @return a List of Components, in (one possible) build order
     */
    private static List makeBuildSequence() {
        List buildSeq = new java.util.ArrayList();
        Set seenCpts = new java.util.HashSet();

        for (Iterator cpts=Component.allComponents(); cpts.hasNext(); ) {
            Component cpt = (Component)cpts.next();
            pseudoMake(cpt, buildSeq, seenCpts);
        }
        return buildSeq;
    }

    /**
     * Does a `make' on the given component, adding it to the list of
     * `make' components.  This imitates the action of make in
     * following through the chain of dependencies, but the only
     * actual action is to add the component to the build-sequence
     * list.
     *
     * @param cpt the Component to make
     * @param buildSeq the build sequence as a list, edited in place
     * (not very Functional!)
     * @param seenCpts the set of components already traversed by
     * this method
     */
    private static void pseudoMake(Component cpt,
                                   List buildSeq,
                                   Set seenCpts) {
        if (seenCpts.contains(cpt))
            return;
        seenCpts.add(cpt);

        if (cpt.getStatus() == Component.STATUS_OBSOLETE
            || cpt.getBuildsupport() != Component.BUILDSUPPORT_NO)
            return;

        Set deps = cpt.getDeps(Dependency.SOURCESET);
        deps.addAll(cpt.getDeps(Dependency.BUILD));
        for (Iterator i = deps.iterator(); i.hasNext(); ) {
            Component depcpt = ((Dependency)i.next()).component();
            pseudoMake(depcpt, buildSeq, seenCpts);
        }
        buildSeq.add(cpt);
    }

    /**
     * Make an XML file containing the flattened dependencies of each
     * component.  This is very similar in its logic to
     * {@link makeMakefile} -- see that routine for discussion.
     */
    private static void makeFlatdeps(PrintStream flatdeps) {
        String[] header = {
            "<?xml version=\"1.0\"?>",
            "<!-- Other derived information about components.",
            "",
            "     This file is not an instance of a DTD, and its structure",
            "     is as yet provisional: use with caution.",
            "",
            "     The dependencies elements below contain the full",
            "     dependencies of the given components, flattened.",
            "-->",
            "<extrainfo>",
            "",
        };
        for (int i=0; i<header.length; i++)
            flatdeps.println(header[i]);

        for (Iterator ci = Component.allComponents(); ci.hasNext(); ) {
            Component c = (Component)ci.next();

            Set ssdeps = new TreeSet(Dependency.simpleComparator());
            Set s = c.getCompleteDependencies(Dependency.SOURCESET);
            if (s == null) {
                System.err.println
                        ("Circularity detected in sourceset dependencies of "
                         + c.getName());
                globalStatus++;
                continue;
            }
            ssdeps.addAll(s);

            s = c.getCompleteDependencies(Dependency.BUILD);
            if (s == null) {
                System.err.println
                        ("Circularity detected in build dependencies of "
                         + c.getName());
                globalStatus++;
                continue;
            }

            Set confdeps = new TreeSet(Dependency.simpleComparator());
            confdeps.addAll(c.getCompleteDependencies(Dependency.CONFIGURE));

            ssdeps.addAll(confdeps);

            Set builddeps = new TreeSet(Dependency.simpleComparator());
            builddeps.addAll(s);
            builddeps.addAll(ssdeps);
            builddeps.addAll(confdeps);

            if (builddeps.isEmpty()) {
                // implies ssdeps and confdeps are empty, too
                flatdeps.println("<!-- no dependencies for " + c + " -->");
            } else {
                flatdeps.println("<component id='" + c + "'");
                int bs = c.getBuildsupport();
                switch (bs) {
                  case Component.BUILDSUPPORT_AUTO:
                    flatdeps.println("\tbuildsupport='yes'"); break;
                  case Component.BUILDSUPPORT_NOAUTO:
                    flatdeps.println("\tbuildsupport='noauto'"); break;
                  case Component.BUILDSUPPORT_NO:
                    flatdeps.println("\tbuildsupport='no'"); break;
                  default:
                    // Component.extractBuildsupport should have guaranteed
                    // that this is impossible
                    assert(false);
                    break;
                }
                flatdeps.println("\tstatus='"
                                 + (c.getStatus() == Component.STATUS_OBSOLETE
                                    ? "obsolete"
                                    : "current")
                                 + "'");
                flatdeps.println("\t>");
                flatdeps.println("<dependencies>");
                if (! confdeps.isEmpty())
                    for (Iterator i=confdeps.iterator(); i.hasNext(); ) {
                        flatdeps.println
                                ("<configure>" + i.next() + "</configure>");
                    }
                if (! ssdeps.isEmpty())
                    for (Iterator i=ssdeps.iterator(); i.hasNext(); ) {
                        flatdeps.println
                                ("<sourceset>" + i.next() + "</sourceset>");
                    }
                for (Iterator i=builddeps.iterator(); i.hasNext(); ) {
                    flatdeps.println("<build>" + i.next() + "</build>");
                }
                flatdeps.println("</dependencies>");
                flatdeps.println("</component>");
            }
            flatdeps.println();
        }

        flatdeps.println("</extrainfo>");

        return;
    }

    /**
     * Represents a single component.
     */
    private static final class Component
            implements Comparable {

        /**
         * A map of all the direct dependencies.  The keys are dependency
         * types (SOURCESET, and so on).  Each element of this map
         * is a Set of Dependency objects.
         */
        private Map allDirectDependencies;
        /**
         * A map of all the dependencies.  The keys are dependency
         * types (SOURCESET, and so on).  Each element of this map is a
         * Set of Dependency objects.
         */
        private Map allComponentDependencies;

        /** The name of the component */
        private String name;

        /** The path to the component */
        private String path;

        /** Contents of the dependencies element */
        private NodeList depElements;
        /** Indicates whether this is a buildsupport object */
        private int buildsupportType;
        /** Indicates the current/obsolete status of this component */
        private int currencyStatus;
        /** Indicates the supported/unsupported status of this component */
        private int supportStatus;

        private Set marks = new java.util.HashSet();

        /**
         * This component is a buildsupport component, and should be
         * built automatically.  Value returned by getBuildsupport.
         */
        public static final int BUILDSUPPORT_AUTO = 1;
        /**
         * This component is a buildsupport component, but should not be
         * built automatically.  Value returned by getBuildsupport.
         */
        public static final int BUILDSUPPORT_NOAUTO = 2;
        /**
         * This component is not a buildsupport component.
         * Value returned by getBuildsupport.
         */
        public static final int BUILDSUPPORT_NO = 3;

        /**
         * This component is a current one.
         */
        public static final int STATUS_CURRENT = 1;
        /**
         * This component is an obsolete one.
         */
        public static final int STATUS_OBSOLETE = 2;

        /**
         * This component is a supported one.
         */
        private static final int SUPPORTED_YES = 1;
        /**
         * This component is unsupported.
         */
        private static final int SUPPORTED_NO = 2;

        /**
         * If true, then the resolveComponents method has been
         * called.  All component references are solved, and so
         * it becomes an error to create any new components.
         */
        private static boolean componentsResolved = false;

        private static final Set emptySet = new java.util.HashSet();

        /**
         * Map of all the components created
         */
        private static final java.util.Map componentMap
                = new java.util.TreeMap();


        private Component(Element el) {
            name = el.getAttribute("id").intern();
            buildsupportType = extractBuildsupport(el);
            path = extractComponentPath(el);
            currencyStatus = extractStatus(el);
            supportStatus = extractSupportStatus(el);
            depElements = el.getElementsByTagName("dependencies");

            allComponentDependencies = new java.util.HashMap();
            if (componentPath().indexOf("obsolete") >= 0
                && getStatus() != STATUS_OBSOLETE) {
                System.err.println("Warning: Component " + getName()
                                   + ": path " + componentPath()
                                   + " contains 'obsolete' but attribute 'status' does not");
            }
        }

        public static Component newComponent(Element el) {
            Component rval;
            if (componentsResolved) {
                System.err.println("Error: component network locked: "+
                                   "one of the query methods has been called");
                rval = null;
            } else {
                Component c = new Component(el);
                componentMap.put(c.getName(), c);
                rval = c;
            }
            return rval;
        }

        /** Name of the component */
        public String getName() {
            return name;
        }

        /**
         * Looks up a component by name.
         * @param componentName the name of a component
         * @return the Component which has this name, or null if no
         * such component exists
         */
        public static Component lookupComponent(String componentName) {
            return (Component) componentMap.get(componentName);
        }

        /**
         * Retrieves all of the components which have been allocated
         * @return an Iterator which produces each of the components
         * in turn
         */
        public static Iterator allComponents() {
            // Use a TreeSet so that the iterator produces the
            // components in a known order
            Set rset = new java.util.TreeSet();
            for (Iterator ei=componentMap.entrySet().iterator();
                 ei.hasNext(); ) {
                rset.add(((Map.Entry)ei.next()).getValue());
            }
            return rset.iterator();
        }

        public String toString() {
            return name;
        }

        /** Retrieves the effective of the buildsupport attribute.
         * @return one of the values <code>BUILDSUPPORT_AUTO</code>,
         * <code>BUILDSUPPORT_NOAUTO</code> or <code>BUILDSUPPORT_NO</code>.
         */
        public int getBuildsupport() {
            return buildsupportType;
        }

        private int extractBuildsupport(Element el) {
            String s = el.getAttribute("buildsupport");
            int ret = -1;       // invalid value
            if (s.equals("yes"))
                ret = BUILDSUPPORT_AUTO;
            else if (s.equals("noauto"))
                ret = BUILDSUPPORT_NOAUTO;
            else if (s.equals("no") || (s.length() == 0))
                ret = BUILDSUPPORT_NO;
            else {
                System.err.println("Element " + el
                                   + " has bad buildsupport attribute: " + s);
                System.exit(1);
            }

            return ret;
        }

        /**
         * Retrieves the path to this component.
         */
        public String componentPath() {
            return path;
        }

        private String extractComponentPath(Element el) {
            NodeList nl = el.getElementsByTagName("path");
            if (nl.getLength() != 1) {
                System.err.println("Component " + getName()
                                   + " does not have precisely one <path> element");
                System.exit(1);
            }
            return nl.item(0).getFirstChild().getNodeValue();
        }

        /**
         * Retrieves the status of this component.
         * @return one of the status values {@link #STATUS_CURRENT} or
         * {@link #STATUS_OBSOLETE}
         */
        public int getStatus() {
            return currencyStatus;
        }

        private int extractStatus(Element el) {
            String s = el.getAttribute("status");
            int ret;

            if (s.equals("current")
                || s.equals("")) // default is current
                ret = STATUS_CURRENT;
            else if (s.equals("obsolete"))
                ret = STATUS_OBSOLETE;
            else {
                System.err.println("Component " + getName()
                                   + " has unknown status " + s
                                   + " (defaulting to 'current')");
                ret = STATUS_CURRENT;
            }
            return ret;
        }

        /**
         * Retrieves the supported/unsupported status of this component.
         * @return one of the values {@link SUPPORTED_YES} or
         * {@link SUPPORTED_NO}
         */
        public int getSupported() {
            return supportStatus;
        }

        private int extractSupportStatus(Element el) {
            String s = el.getAttribute("support");
            int ret;

            if (s.equals("")) {
                System.err.println("Component " + getName()
                                   + " has blank support attribute "
                                   + "(defaulting to 'supported')");
                ret = SUPPORTED_YES;
            } else if (s.equals("S")) {
                ret = SUPPORTED_YES;
            } else if (s.equals("U")) {
                ret = SUPPORTED_NO;
            } else {
                System.err.println("Component " + getName()
                                   + " has unrecognised support status " + s
                                   + " (defaulting to 'supported')");
                ret = SUPPORTED_YES;
            }
            return ret;
        }

        /**
         * Gets the set of direct dependencies of this component.
         * @param type one of the type strings SOURCESET, INCLUDE, and so on.
         * @return a Set of Dependency objects
         */
        public Set getDeps(String type) {
            if (!componentsResolved)
                resolveComponents();

            Set rval = (Set)allDirectDependencies.get(type);
            if (rval == null)
                rval = emptySet;
            return rval;
        }

        /**
         * Gets the complete set of dependencies.  As for
         * <code>getDeps</code>, except that the dependencies of the
         * direct dependencies are returned, and so on recursively.
         *
         * @param type one of the type strings
         * @return a set of all the Dependency objects which this component
         *     depends on.  If it detects a circular dependency, return
         *     null, print a message on <code>stderr</code>, and increment
         *     {@link #globalStatus}.
         */
        public Set getCompleteDependencies(String type) {
            Set rval;

            if (type == null) {
                System.err.println("getCompleteDependencies: null argument");
                System.exit(1);
            }

            if (!componentsResolved)
                resolveComponents();

            if (allComponentDependencies.containsKey(type)) {
                // our work is already done
                rval = (Set)allComponentDependencies.get(type);
                if (verbose) {
                    System.err.println("Cached for " + this
                                       + ", type " + type + ": "
                                       + showSet(rval));
                }
            } else {
                rval = getCompleteDependencies(type, true, 0);
            }
            return rval;
        }

        private Set getCompleteDependencies(String type,
                                            boolean errorCircular,
                                            int recurseLevel) {
            assert type != null;

            boolean foundCircular = false;

            if (!componentsResolved)
                resolveComponents();

            if (verbose) {
                System.err.println("[" + recurseLevel + "] "
                                   + this.toString()
                                   + ".getCompleteDependencies(" + type
                                   + ", " + errorCircular
                                   + ", " + recurseLevel
                                   + "): marks "
                                   + showSet(marks));
            }

            if (marks.contains(type)) {
                if (type == Dependency.LINK) {
                    return new java.util.TreeSet();
                } else {
                    System.err.println("Detected circular dependency on " + this);
                    return null;
                }
            }
            marks.add(type);

            // collectedDeps is a Set of Dependency objects
            Set collectedDeps = new java.util.TreeSet();
            for (Iterator i=getDeps(type).iterator(); i.hasNext(); ) {
                Dependency d = (Dependency)i.next();
                Component c = d.component();
                if (verbose)
                    System.err.println("[" + recurseLevel
                                       + "] ...depends on " + c);

                collectedDeps.add(d);

                Set cdeps;      // Set of Dependency objects
                if (d.option() != null) {
                    if (verbose)
                        System.err.println("[" + recurseLevel
                                           + "] Component " + c
                                           + " has option " + d.option());
                    cdeps = c.getCompleteDependencies
                            (d.option(),
                             (d.option() == Dependency.LINK ? false : true),
                             recurseLevel+1);
                } else {
                    cdeps = c.getCompleteDependencies
                            (type,
                             (type == Dependency.LINK ? false : true),
                             recurseLevel+1);
                }

                if (cdeps == null) {
                    // We found circular dependencies.  As we work our
                    // way back up the call tree, print out the
                    // sequence of dependencies that brought us here.
                    System.err.println("...dependency of " + this);
                    foundCircular = true;
                } else {
                    collectedDeps.addAll(cdeps);
                }
            }
            if (verbose) {
                System.err.println("[" + recurseLevel
                                   + "] ...collected for " + this
                                   + ", type " + type + ": "
                                   + showSet(collectedDeps));
            }

            try {
                allComponentDependencies.put
                        (type,
                         foundCircular ? null : collectedDeps);
            } catch (NullPointerException e) {
                System.err.println("Map does not permit null values");
                System.exit(1);
            }

            marks.remove(type);

            return (foundCircular ? null : collectedDeps);
        }

        private static void resolveComponents() {
            componentsResolved = true;

            for (Iterator ci=allComponents(); ci.hasNext(); ) {
                Component c = (Component)ci.next();
                c.extractElementDependencySets();
            }
        }

        private void extractElementDependencySets() {
            // allDirectDependencies is a hash of (type, Set-of-Dependency)
            // pairs.
            // We should come this way only once.
            assert allDirectDependencies == null;
            allDirectDependencies = new java.util.HashMap();
            if (depElements.getLength() != 1) {
                System.err.println
                  ("Element does not have exactly one <dependencies> element");
                System.exit(1);
            }

            NodeList nl = depElements.item(0).getChildNodes();
            for (int i=0; i<nl.getLength(); i++) {
                Node n = nl.item(i);
                if (n.getNodeType() != Node.ELEMENT_NODE) {
                    // What's this?  Never mind -- skip it.
                    continue;
                }
                n.normalize();
                Node text = n.getFirstChild();
                if (text == null || text.getNodeType() != Node.TEXT_NODE)
                    throw new IllegalArgumentException
                            ("Element does not contain text");

                Component c = Component.lookupComponent(text.getNodeValue());
                if (c == null) {
                    System.err.println("Dependency " + text.getNodeValue()
                                       + " unknown");
                    System.exit(1);
                }
                Dependency d = Dependency.newDependency
                        (n.getNodeName(), c,
                         ((Element)n).getAttribute("option"));
                Set dep = (Set)allDirectDependencies.get(d.type());
                if (dep == null) {
                    dep = new java.util.HashSet();
                    allDirectDependencies.put(d.type(), dep);
                }
                dep.add(d);
            }
            if (verbose) {
                System.err.println("Component " + getName()
                                   + ".extractElementDependencySets ->");
                for (Iterator i=allDirectDependencies.keySet().iterator();
                     i.hasNext();) {
                    String s = (String)i.next();
                    Set ss = (Set)allDirectDependencies.get(s);
                    System.err.println("  " + s + "-->" + showSet(ss));
                }
            }
        }

        // Implementation of the Comparable interface, plus the equals
        // method that ought to be implemented when compareTo is.

        public boolean equals(Object o) {
            if (o == this)
                return true;
            else if (o instanceof Component) {
                Component c = (Component) o;
                return c.name == name
                        && c.path == path
                        && c.getBuildsupport() == getBuildsupport();
            } else {
                return false;
            }
        }
        public int compareTo(Object o)
                throws ClassCastException {
            if (equals(o))
                return 0;
            else {
                Component c = (Component)o;
                int i = name.compareTo(c.name);
                if (i != 0)
                    return i;
                i = path.compareTo(c.path);
                if (i != 0)
                    return i;
                i = getBuildsupport() - c.getBuildsupport();
                assert i != 0;  // or else equals() should have been true
                return i;
            }
        }
        public int hashCode() {
            return (name.hashCode() ^ path.hashCode()) + getBuildsupport();
        }
    }

    /**
     * Encapsulates a dependency on a component.  Expresses that there
     * is a dependency of type {@link #type} on the component
     * {@link #component}.
     */
    private static final class Dependency
            implements Comparable {
        private String mytype;
        private Component dependsOnComponent;
        private String option;

        public static final String SOURCESET = "sourceset";
        public static final String BUILD = "build";
        public static final String LINK = "link";
        public static final String USE = "use";
        public static final String TEST = "test";
        public static final String CONFIGURE = "configure";
        private static final String DUMMY = "dummy";

        /** Flag is true when we have expanded dependencies */
        private static boolean dependenciesAreExpanded = false;

        /**
         * Set of all the components which are a configure dependency
         * of some component.  This starts off as a set of component
         * names, then turns into a set of Component objects when
         * getConfigureDependencies is called.
         */
        private static Set configureDependencyObjectSet
                = new java.util.HashSet();

        private static Comparator simpleComparatorImpl = null;


        private Dependency(String type,
                           Component component,
                           String attoption) {

            mytype = type.intern();
            if (!typeOK(mytype))
                throw new IllegalArgumentException
                        ("Dependency element of unrecognised type: " + type);

            dependsOnComponent = component;

            if (mytype == CONFIGURE)
                configureDependencyObjectSet.add(dependsOnComponent);

            if (attoption.length() > 0) {
                if (verbose)
                    System.err.println("dependency on "
                                       + component.getName()
                                       + ", option <" + attoption + ">");
                option = attoption.trim().intern();
                if (! typeOK(option)) {
                    System.err.println("Option " + option + " illegal");
                    option = null;
                }
            }

            assert dependsOnComponent != null
                    && mytype != null;
        }

        public static Dependency newDependency(String type,
                                               Component component,
                                               String attoption) {
            Dependency rval;
            if (dependenciesAreExpanded) {
                System.err.println("Error: creation of Dependency objects locked");
                rval = null;
            } else {
                rval = new Dependency(type, component, attoption);
            }
            return rval;
        }

        public String type() {
            return mytype;
        }

        public Component component() {
            return dependsOnComponent;
        }

        public String option() {
            return option;
        }

        public String toString() {
            return dependsOnComponent.getName();
        }

        /** Returns true if the type is one of the legal ones */
        private boolean typeOK(String type) {
            return (type == SOURCESET
                    || type == BUILD
                    || type == LINK
                    || type == USE
                    || type == TEST
                    || type == CONFIGURE);
        }

        public boolean equals(Object o) {
            if (o == this)
                return true;
            else if (o instanceof Dependency) {
                Dependency d = (Dependency)o;
                return d.dependsOnComponent == dependsOnComponent
                        && d.mytype == mytype
                        && d.option == option;
            } else {
                return false;
            }
        }
        public int compareTo(Object o)
                throws ClassCastException {
            if (equals(o))
                return 0;
            Dependency od = (Dependency)o; // throws Exception if not possible
            int i = dependsOnComponent
                    .compareTo(od.dependsOnComponent);
            if (i != 0)
                return i;
            i = mytype.compareTo(od.mytype);
            if (i != 0)
                return i;
            if (option == null) {
                assert od.option != null; // otherwise equals() was true
                return -1;
            } else {
                if (od.option == null)
                    return +1;
                else {
                    i = option.compareTo(od.option);
                    assert i != 0;      // otherwise equals() was true
                    return i;
                }
            }
        }

        public int hashCode() {
            return dependsOnComponent.hashCode()
                    ^ mytype.hashCode()
                    ^ (option == null ? "" : option).hashCode();
        }

        /**
         * Returns a Comparator object, which compares based only on
         * names, rather than types and options, as this class as a
         * whole does.  Thus:
         *
         * <p><strong>Note</strong> This comparator imposes orderings
         * that are inconsistent with equals, since it regards
         * objects as equal when the <code>equals()</code> method on
         * <code>Dependency</code> distinguishes them.
         */
        public static Comparator simpleComparator() {
            if (simpleComparatorImpl == null) {
                simpleComparatorImpl = new Comparator() {
                        public boolean equals(Object o) {
                            return o == simpleComparatorImpl;
                        }
                        public int compare(Object o1, Object o2)
                                throws ClassCastException {
                            return ((Dependency)o1).component()
                                    .compareTo(((Dependency)o2).component());
                        }
                    };
            }
            return simpleComparatorImpl;
        }

        /**
         * Returns a set containing all those components which are the
         * target of a configure dependency, plus all those which are
         * build dependencies of those.
         * @return a Set of Component objects
         */
        public static Set getConfigureDependencies() {
            if (!dependenciesAreExpanded)
                resolveDependencies();

            return configureDependencyObjectSet;
        }

        public static void resolveDependencies() {
            // Replace configureDependencyObjectSet with a new set
            // which is a copy, with all the dependencies followed.
            // Be careful not to mutate the references we get below.
            // Use a TreeSet to ensure a repeatable order.
            Set newset = new java.util.TreeSet();
            for (Iterator cdi=configureDependencyObjectSet.iterator();
                 cdi.hasNext(); ) {
                Component c = (Component)cdi.next();
                newset.add(c);
                Set deps = c.getCompleteDependencies(CONFIGURE);
                    if (deps != null && deps.size() > 0)
                        for (Iterator i=deps.iterator(); i.hasNext(); )
                            newset.add(((Dependency)i.next()).component());
                    deps = c.getCompleteDependencies(BUILD);
                    if (deps != null && deps.size() > 0)
                        for (Iterator i=deps.iterator(); i.hasNext(); )
                            newset.add(((Dependency)i.next()).component());
            }
            configureDependencyObjectSet = newset;
            dependenciesAreExpanded = true;
        }
    }
}
