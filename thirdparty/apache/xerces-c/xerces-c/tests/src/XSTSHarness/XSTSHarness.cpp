/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Id: XSTSHarness.cpp 677563 2008-07-17 11:51:39Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "XSTSHarness.hpp"
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#if defined(XERCES_NEW_IOSTREAMS)
#include <fstream>
#else
#include <fstream.h>
#endif
#include <xercesc/util/OutOfMemoryException.hpp>

// ---------------------------------------------------------------------------
//  Local helper methods
// ---------------------------------------------------------------------------
void usage()
{
    XERCES_STD_QUALIFIER cout << "\nUsage:\n"
            "    XSTSHarness <XSTS testSet>\n\n"
            "This program runs the tests listed in the XMLSchema Test Suite:\n"
            "download the suite from http://www.w3.org/XML/2004/xml-schema-test-suite\n"
            "and uncompress it. Then run this executable against each .testSet\n"
            "file found in the suite.\n\n"
         << XERCES_STD_QUALIFIER endl;
}


// ---------------------------------------------------------------------------
//  Program entry point
// ---------------------------------------------------------------------------
int main(int argC, char* argV[])
{

    // Check command line and extract arguments.
    if (argC < 2)
    {
        usage();
        return 1;
    }

    int argInd;
    for (argInd = 1; argInd < argC; argInd++)
    {
        // Break out on first parm not starting with a dash
        if (argV[argInd][0] != '-')
            break;

        // Watch for special case help request
        if (!strcmp(argV[argInd], "-?"))
        {
            usage();
            return 2;
        }
        // TODO: add option to generate the XML summarizing the result 
        else if (!strncmp(argV[argInd], "-v=", 3)
             ||  !strncmp(argV[argInd], "-V=", 3))
        {
        }
        else
        {
            XERCES_STD_QUALIFIER cout << "Unknown option '" << argV[argInd]
                << "', ignoring it\n" << XERCES_STD_QUALIFIER endl;
        }
    }

    //
    //  There should be only one and only one parameter left, and that
    //  should be the file name.
    //
    if (argInd != argC - 1)
    {
        usage();
        return 1;
    }

    try
    {
        XMLPlatformUtils::Initialize();
    }

    catch (const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER cout << "Error during initialization! Message:\n"
            << StrX(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
        return 1;
    }

    //
    //  Create a SAX parser object. Then, according to what we were told on
    //  the command line, set it to validate or not.
    //
    SAX2XMLReader* parser = XMLReaderFactory::createXMLReader();
    parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);
    parser->setFeature(XMLUni::fgSAX2CoreValidation, false);

    const char* xmlFile = argV[argInd];
    //
    //  Create our SAX handler object and install it on the parser, as the
    //  document and error handler.
    //
    XMLCh* uniFile = XMLString::transcode(xmlFile);
    XMLCh* uri = new XMLCh[XMLString::stringLen(xmlFile) + 9];
    XMLString::fixURI(uniFile, uri);
    XSTSHarnessHandlers* handler=new XSTSHarnessHandlers(uri);
    XMLString::release(&uniFile);
    delete [] uri;
    parser->setContentHandler(handler);
    parser->setErrorHandler(handler);

    //
    //  Get the starting time and kick off the parse of the indicated
    //  file. Catch any exceptions that might propogate out of it.
    //
    bool errorOccurred=false;
    const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
    try
    {
        parser->parse(xmlFile);
    }
    catch (const OutOfMemoryException&)
    {
        XERCES_STD_QUALIFIER cout << "OutOfMemoryException" << XERCES_STD_QUALIFIER endl;
        errorOccurred = true;
    }
    catch (const XMLException& e)
    {
        XERCES_STD_QUALIFIER cout << "\nError during parsing: '" << xmlFile << "'\n"
            << "Exception message is:  \n"
            << StrX(e.getMessage()) << "\n" << XERCES_STD_QUALIFIER endl;
        errorOccurred = true;
    }
    catch (...)
    {
        XERCES_STD_QUALIFIER cout << "\nUnexpected exception during parsing: '" << xmlFile << "'\n";
        errorOccurred = true;
    }
    const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
    unsigned long duration = endMillis - startMillis;

    if (handler->getSawErrors())
        errorOccurred = true;

    XERCES_STD_QUALIFIER cout << "Total tests: " << handler->getTotalTests() << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout << "Failed tests: " << handler->getFailedTests() << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout << "Success rate: " << ((double)(handler->getTotalTests()-handler->getFailedTests()))/(double)handler->getTotalTests()*100 << "%" << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout << "Duration: ";
    if(duration > 60000)
    {
        XERCES_STD_QUALIFIER cout << duration/60000 << ":";
        duration=duration % 60000;
    }
    if(duration/1000 < 10)
        XERCES_STD_QUALIFIER cout << "0";
    XERCES_STD_QUALIFIER cout << duration/1000 << "." << duration % 1000 << XERCES_STD_QUALIFIER endl;

    //
    //  Delete the parser itself.  Must be done prior to calling Terminate, below.
    //
    delete parser;
    delete handler;

    // And call the termination method
    XMLPlatformUtils::Terminate();

    if (errorOccurred)
        return 4;
    else
        return 0;

}

