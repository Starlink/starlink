// file      : xsd/cxx/parser/expat/elements.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#include <new>     // std::bad_alloc
#include <istream>
#include <fstream>
#include <cstring> // std::strchr
#include <cassert>

#include <xsd/cxx/xml/bits/literals.hxx> // xml::bits::{xml_prefix, etc}

#include <xsd/cxx/parser/error-handler.hxx>
#include <xsd/cxx/parser/schema-exceptions.hxx>

namespace xsd
{
  namespace cxx
  {
    namespace parser
    {
      namespace expat
      {

        // document
        //

        template <typename C>
        document<C>::
        document (parser_base<C>& p,
                  const std::basic_string<C>& name,
                  bool polymorphic)
            : cxx::parser::document<C> (p, std::basic_string<C> (), name),
              polymorphic_ (polymorphic),
              router_ (*this, polymorphic),
              xml_parser_ (0)
        {
        }

        template <typename C>
        document<C>::
        document (parser_base<C>& p,
                  const C* name,
                  bool polymorphic)
            : cxx::parser::document<C> (p, std::basic_string<C> (), name),
              polymorphic_ (polymorphic),
              router_ (*this, polymorphic),
              xml_parser_ (0)
        {
        }

        template <typename C>
        document<C>::
        document (parser_base<C>& p,
                  const C* ns,
                  const C* name,
                  bool polymorphic)
            : cxx::parser::document<C> (p, ns, name),
              polymorphic_ (polymorphic),
              router_ (*this, polymorphic),
              xml_parser_ (0)
        {
        }

        template <typename C>
        document<C>::
        document (parser_base<C>& p,
                  const std::basic_string<C>& ns,
                  const std::basic_string<C>& name,
                  bool polymorphic)
            : cxx::parser::document<C> (p, ns, name),
              polymorphic_ (polymorphic),
              router_ (*this, polymorphic),
              xml_parser_ (0)
        {
        }

        template <typename C>
        document<C>::
        document (bool polymorphic)
            : polymorphic_ (polymorphic),
              router_ (*this, polymorphic),
              xml_parser_ (0)
        {
        }

        // file
        //

        template <typename C>
        void document<C>::
        parse (const std::basic_string<C>& file)
        {
          std::ifstream ifs;
          ifs.exceptions (std::ios_base::badbit | std::ios_base::failbit);
          ifs.open (file.c_str (), std::ios_base::in | std::ios_base::binary);

          parse (ifs, file);
        }

        template <typename C>
        void document<C>::
        parse (const std::basic_string<C>& file, xml::error_handler<C>& eh)
        {
          std::ifstream ifs;
          ifs.exceptions (std::ios_base::badbit | std::ios_base::failbit);
          ifs.open (file.c_str (), std::ios_base::in | std::ios_base::binary);

          parse (ifs, file, eh);
        }


        // istream
        //

        template <typename C>
        void document<C>::
        parse (std::istream& is)
        {
          error_handler<C> eh;
          parse (is, 0, 0, eh);
          eh.throw_if_failed ();
        }

        template <typename C>
        void document<C>::
        parse (std::istream& is, xml::error_handler<C>& eh)
        {
          if (!parse (is, 0, 0, eh))
            throw parsing<C> ();
        }

        template <typename C>
        void document<C>::
        parse (std::istream& is, const std::basic_string<C>& system_id)
        {
          error_handler<C> eh;
          parse (is, &system_id, 0, eh);
          eh.throw_if_failed ();
        }

        template <typename C>
        void document<C>::
        parse (std::istream& is,
               const std::basic_string<C>& system_id,
               xml::error_handler<C>& eh)
        {
          if (!parse (is, &system_id, 0, eh))
            throw parsing<C> ();
        }

        template <typename C>
        void document<C>::
        parse (std::istream& is,
               const std::basic_string<C>& system_id,
               const std::basic_string<C>& public_id)
        {
          error_handler<C> eh;
          parse (is, &system_id, &public_id, eh);
          eh.throw_if_failed ();
        }

        template <typename C>
        void document<C>::
        parse (std::istream& is,
               const std::basic_string<C>& system_id,
               const std::basic_string<C>& public_id,
               xml::error_handler<C>& eh)
        {
          if (!parse (is, &system_id, &public_id, eh))
            throw parsing<C> ();
        }

        // data
        //

        template <typename C>
        void document<C>::
        parse (const void* data, std::size_t size, bool last)
        {
          error_handler<C> eh;
          parse (data, size, last, 0, 0, eh);
          eh.throw_if_failed ();
        }

        template <typename C>
        void document<C>::
        parse (const void* data, std::size_t size, bool last,
               xml::error_handler<C>& eh)
        {
          if (!parse (data, size, last, 0, 0, eh))
            throw parsing<C> ();
        }

        template <typename C>
        void document<C>::
        parse (const void* data, std::size_t size, bool last,
               const std::basic_string<C>& system_id)
        {
          error_handler<C> eh;
          parse (data, size, last, &system_id, 0, eh);
          eh.throw_if_failed ();
        }

        template <typename C>
        void document<C>::
        parse (const void* data, std::size_t size, bool last,
               const std::basic_string<C>& system_id,
               xml::error_handler<C>& eh)
        {
          if (!parse (data, size, last, &system_id, 0, eh))
            throw parsing<C> ();
        }

        template <typename C>
        void document<C>::
        parse (const void* data, std::size_t size, bool last,
               const std::basic_string<C>& system_id,
               const std::basic_string<C>& public_id)
        {
          error_handler<C> eh;
          parse (data, size, last, &system_id, &public_id, eh);
          eh.throw_if_failed ();
        }

        template <typename C>
        void document<C>::
        parse (const void* data, std::size_t size, bool last,
               const std::basic_string<C>& system_id,
               const std::basic_string<C>& public_id,
               xml::error_handler<C>& eh)
        {
          if (!parse (data, size, last, &system_id, &public_id, eh))
            throw parsing<C> ();
        }

        // Implementation details.
        //

        namespace bits
        {
          struct stream_exception_controller
          {
            ~stream_exception_controller ()
            {
              if (is_.fail () && is_.eof ())
                is_.clear (is_.rdstate () & ~std::ios_base::failbit);

              is_.exceptions (old_state_);
            }

            stream_exception_controller (std::istream& is)
                : is_ (is), old_state_ (is_.exceptions ())
            {
              is_.exceptions (old_state_ & ~std::ios_base::failbit);
            }

          private:
            stream_exception_controller (const stream_exception_controller&);

            stream_exception_controller&
            operator= (const stream_exception_controller&);

          private:
            std::istream& is_;
            std::ios_base::iostate old_state_;
          };
        };

        template <typename C>
        bool document<C>::
        parse (std::istream& is,
               const std::basic_string<C>* system_id,
               const std::basic_string<C>* public_id,
               xml::error_handler<C>& eh)
        {
          parser_auto_ptr parser (XML_ParserCreateNS (0, XML_Char (' ')));

          if (parser == 0)
            throw std::bad_alloc ();

          parse_begin (parser);

          // Temporarily unset the exception failbit. Also clear the
          // fail bit when we reset the old state if it was caused
          // by eof.
          //
          bits::stream_exception_controller sec (is);

          char buf[16384]; // 4 x page size.

          do
          {
            is.read (buf, sizeof (buf));

            if (is.bad () || (is.fail () && !is.eof ()))
            {
              // If the stream is not using exceptions then the user
              // will have to test for stream failures before calling
              // post.
              //
              return true;
            }

            try
            {
              if (XML_Parse (
                    parser, buf, is.gcount (), is.eof ()) == XML_STATUS_ERROR)
              {
                unsigned long l (XML_GetCurrentLineNumber (xml_parser_));
                unsigned long c (XML_GetCurrentColumnNumber (xml_parser_));
                std::basic_string<C> message (
                  XML_ErrorString (XML_GetErrorCode (xml_parser_)));

                parse_end ();

                eh.handle (
                  public_id
                  ? *public_id
                  : (system_id ? *system_id : std::basic_string<C> ()),
                  l, c, xml::error_handler<C>::severity::fatal, message);

                return false;
              }
            }
            catch (const schema_exception<C>& e)
            {
              unsigned long l (XML_GetCurrentLineNumber (xml_parser_));
              unsigned long c (XML_GetCurrentColumnNumber (xml_parser_));

              parse_end ();

              eh.handle (
                public_id
                ? *public_id
                : (system_id ? *system_id : std::basic_string<C> ()),
                l, c, xml::error_handler<C>::severity::fatal, e.message ());

              return false;
            }

          } while (!is.eof ());

          parse_end ();

          return true;
        }

        template <typename C>
        bool document<C>::
        parse (const void* data,
               std::size_t size,
               bool last,
               const std::basic_string<C>* system_id,
               const std::basic_string<C>* public_id,
               xml::error_handler<C>& eh)
        {
          // First call.
          //
          if (auto_xml_parser_ == 0)
          {
            auto_xml_parser_ = XML_ParserCreateNS (0, XML_Char (' '));

            if (auto_xml_parser_ == 0)
              throw std::bad_alloc ();

            parse_begin (auto_xml_parser_);
          }

          try
          {
            if (XML_Parse (xml_parser_,
                           static_cast<const char*> (data),
                           static_cast<int> (size),
                           last) == XML_STATUS_ERROR)
            {
              unsigned long l (XML_GetCurrentLineNumber (xml_parser_));
              unsigned long c (XML_GetCurrentColumnNumber (xml_parser_));
              std::basic_string<C> message (
                XML_ErrorString (XML_GetErrorCode (xml_parser_)));

              parse_end ();

              eh.handle (
                public_id
                ? *public_id
                : (system_id ? *system_id : std::basic_string<C> ()),
                l, c, xml::error_handler<C>::severity::fatal, message);

              return false;
            }
          }
          catch (const schema_exception<C>& e)
          {
            unsigned long l (XML_GetCurrentLineNumber (xml_parser_));
            unsigned long c (XML_GetCurrentColumnNumber (xml_parser_));

            parse_end ();

            eh.handle (
              public_id
              ? *public_id
              : (system_id ? *system_id : std::basic_string<C> ()),
              l, c, xml::error_handler<C>::severity::fatal, e.message ());

            return false;
          }

          if (last)
            parse_end ();

          return true;
        }

        // XML_Parser
        //

        template <typename C>
        void document<C>::
        parse_begin (XML_Parser parser)
        {
          xml_parser_ = parser;
          set ();
        }

        template <typename C>
        void document<C>::
        parse_end ()
        {
          clear ();
          xml_parser_ = 0;
          auto_xml_parser_ = 0;
        }

        //
        //
        template <typename C>
        void document<C>::
        set ()
        {
          assert (xml_parser_ != 0);

          XML_SetUserData(xml_parser_, &router_);

          XML_SetStartElementHandler (
            xml_parser_, event_router<C>::start_element);
          XML_SetEndElementHandler (
            xml_parser_, event_router<C>::end_element);
          XML_SetCharacterDataHandler (
            xml_parser_, event_router<C>::characters);

          if (polymorphic_)
          {
            XML_SetNamespaceDeclHandler (
              xml_parser_,
              event_router<C>::start_namespace_decl,
              event_router<C>::end_namespace_decl);
          }
        }

        template <typename C>
        void document<C>::
        clear ()
        {
          assert (xml_parser_ != 0);

          XML_SetUserData (xml_parser_, 0);
          XML_SetStartElementHandler (xml_parser_, 0);
          XML_SetEndElementHandler (xml_parser_, 0);
          XML_SetCharacterDataHandler (xml_parser_, 0);

          if (polymorphic_)
            XML_SetNamespaceDeclHandler (xml_parser_, 0, 0);
        }

        // event_router
        //

        template <typename C>
        event_router<C>::
        event_router (cxx::parser::document<C>& consumer, bool polymorphic)
            : consumer_ (consumer), polymorphic_ (polymorphic)
        {
        }

        // Expat thunks.
        //
        template <typename C>
        void XMLCALL event_router<C>::
        start_element (
          void* data, const XML_Char* ns_name, const XML_Char** atts)
        {
          event_router& r (*reinterpret_cast<event_router*> (data));
          r.start_element_ (ns_name, atts);
        }

        template <typename C>
        void XMLCALL event_router<C>::
        end_element (void* data, const XML_Char* ns_name)
        {
          event_router& r (*reinterpret_cast<event_router*> (data));
          r.end_element_ (ns_name);
        }

        template <typename C>
        void XMLCALL event_router<C>::
        characters (void* data, const XML_Char* s, int n)
        {
          event_router& r (*reinterpret_cast<event_router*> (data));
          r.characters_ (s, static_cast<std::size_t> (n));
        }

        template <typename C>
        void XMLCALL event_router<C>::
        start_namespace_decl (void* data,
                              const XML_Char* prefix,
                              const XML_Char* ns)
        {
          event_router& r (*reinterpret_cast<event_router*> (data));
          r.start_namespace_decl_ (prefix, ns);
        }

        template <typename C>
        void XMLCALL event_router<C>::
        end_namespace_decl (void* data, const XML_Char* prefix)
        {
          event_router& r (*reinterpret_cast<event_router*> (data));
          r.end_namespace_decl_ (prefix);
        }

        namespace bits
        {
          inline void
          split_name (const XML_Char* s,
                      const char*& ns, std::size_t& ns_s,
                      const char*& name, std::size_t& name_s)
          {
            const char* p (std::strchr (s, ' '));

            if (p)
            {
              ns = s;
              ns_s = p - s;
              name = p + 1;
            }
            else
            {
              ns = s;
              ns_s = 0;
              name = s;
            }

            name_s = std::char_traits<char>::length (name);
          }
        }

        template <typename C>
        void event_router<C>::
        start_element_ (const XML_Char* ns_name, const XML_Char** atts)
        {
          typedef std::basic_string<C> string;

          const char* ns_p;
          const char* name_p;
          size_t ns_s, name_s;

          bits::split_name (ns_name, ns_p, ns_s, name_p, name_s);

          {
            const ro_string<C> ns (ns_p, ns_s), name (name_p, name_s);

            if (!polymorphic_)
              consumer_.start_element (ns, name, 0);
            else
            {
              // Search for the xsi:type attribute.
              //
              const XML_Char** p (atts);
              for (; *p != 0; p += 2)
              {
                bits::split_name (*p, ns_p, ns_s, name_p, name_s);
                const ro_string<C> ns (ns_p, ns_s), name (name_p, name_s);

                if (name == xml::bits::type<C> () &&
                    ns == xml::bits::xsi_namespace<C> ())
                  break;
              }

              if (*p == 0)
                consumer_.start_element (ns, name, 0);
              else
              {
                // @@ Need proper QName validation.
                //
                // Get the qualified type name and try to resolve it.
                //
                ro_string<C> qn (*(p + 1));

                string tp, tn, tns;
                typename ro_string<C>::size_type pos (qn.find (C (':')));

                if (pos != ro_string<C>::npos)
                {
                  tp.assign (qn.data (), pos);
                  tn.assign (qn.data () + pos + 1);

                  if (tp.empty ())
                    throw dynamic_type<C> (qn);
                }
                else
                  tn = qn;

                if (tn.empty ())
                  throw dynamic_type<C> (qn);

                // Search our namespace declaration stack. Sun CC 5.7
                  // blows if we use const_reverse_iterator.
                //
                for (typename ns_decls::reverse_iterator
                       it (ns_decls_.rbegin ()), e (ns_decls_.rend ());
                     it != e; ++it)
                {
                  if (it->prefix == tp)
                  {
                    tns = it->ns;
                    break;
                  }
                }

                if (!tp.empty () && tns.empty ())
                {
                  // The 'xml' prefix requires special handling.
                  //
                  if (tp == xml::bits::xml_prefix<C> ())
                    tns = xml::bits::xml_namespace<C> ();
                  else
                    throw dynamic_type<C> (qn);
                }

                // Construct the compound name.
                //
                if (!tns.empty ())
                {
                  tn += C (' ');
                  tn += tns;
                }

                ro_string<C> ro_tn (tn);

                consumer_.start_element (ns, name, &ro_tn);
              }
            }
          }

          for (; *atts != 0; atts += 2)
          {
            bits::split_name (*atts, ns_p, ns_s, name_p, name_s);

            const ro_string<C> ns (ns_p, ns_s), name (name_p, name_s);
            const ro_string<C> value (*(atts + 1));

            consumer_.attribute (ns, name, value);
          }
        }

        template <typename C>
        void event_router<C>::
        end_element_ (const XML_Char* ns_name)
        {
          const char* ns_p;
          const char* name_p;
          size_t ns_s, name_s;

          bits::split_name (ns_name, ns_p, ns_s, name_p, name_s);

          const ro_string<C> ns (ns_p, ns_s), name (name_p, name_s);

          consumer_.end_element (ns, name);
        }

        template <typename C>
        void event_router<C>::
        characters_ (const XML_Char* s, std::size_t n)
        {
          if (n != 0)
          {
            const ro_string<C> str (s, n);
            consumer_.characters (str);
          }
        }

        template <typename C>
        void event_router<C>::
        start_namespace_decl_ (const XML_Char* p, const XML_Char* ns)
        {
          // prefix is 0 for default namespace
          // namespace is 0 when unsetting default namespace
          //
          if (polymorphic_)
          {
            ns_decls_.push_back (ns_decl ((p ? p : ""), (ns ? ns : "")));
          }
        }

        template <typename C>
        void event_router<C>::
        end_namespace_decl_ (const XML_Char* p)
        {
          // prefix is 0 for default namespace
          //
          if (polymorphic_)
          {
            // Here we assume the prefixes are removed in the reverse
            // order of them being added. This appears to how every
            // sensible implementation works.
            //
            assert (p
                    ? ns_decls_.back ().prefix == p
                    : ns_decls_.back ().prefix.empty ());

            ns_decls_.pop_back ();
          }
        }
      }
    }
  }
}
