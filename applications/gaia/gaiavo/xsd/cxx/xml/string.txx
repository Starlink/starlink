// file      : xsd/cxx/xml/string.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_XML_STRING_TXX
#define XSD_CXX_XML_STRING_TXX

#ifndef XSD_USE_LCP
namespace xsd
{
  namespace cxx
  {
    namespace xml
    {
      namespace bits
      {
        template <typename C>
        const unsigned char char_transcoder<C>::first_byte_mask_[5] =
        {
          0x00, 0x00, 0xC0, 0xE0, 0xF0
        };

        template <typename C>
        std::basic_string<C> char_transcoder<C>::
        to (const XMLCh* s, std::size_t len)
        {
          const XMLCh* end (s + len);

          // Find what the resulting buffer size will be.
          //
          std::size_t rl (0);
          unsigned int u (0); // Four byte UCS-4 char.

          bool valid (true);
          const XMLCh* p (s);

          for (; p < end; ++p)
          {
            XMLCh x (*p);

            if (x < 0xD800 || x > 0xDBFF)
              u = x;
            else
            {
              // Make sure we have one more char and it has a valid
              // value for the second char in a surrogate pair.
              //
              if (++p == end || !((*p >= 0xDC00) && (*p <= 0xDFFF)))
              {
                valid = false;
                break;
              }

              u = ((x - 0xD800) << 10) + (*p - 0xDC00) + 0x10000;
            }

            if (u < 0x80)
              rl++;
            else if (u < 0x800)
              rl += 2;
            else if (u < 0x10000)
              rl += 3;
            else if (u < 0x110000)
              rl += 4;
            else
            {
              valid = false;
              break;
            }
          }

          if (!valid)
            throw invalid_utf16_string ();

          std::basic_string<C> r;
          r.reserve (rl + 1);
          r.resize (rl);
          C* rs (const_cast<C*> (r.c_str ()));

          std::size_t i (0);
          unsigned int count (0);

          p = s;

          // Tight first loop for the common case.
          //
          for (; p < end && *p < 0x80; ++p)
            rs[i++] = C (*p);

          for (; p < end; ++p)
          {
            XMLCh x (*p);

            if ((x >= 0xD800) && (x <= 0xDBFF))
            {
              u = ((x - 0xD800) << 10) + (*++p - 0xDC00) + 0x10000;
            }
            else
              u = x;

            if (u < 0x80)
              count = 1;
            else if (u < 0x800)
              count = 2;
            else if (u < 0x10000)
              count = 3;
            else if (u < 0x110000)
              count = 4;

            switch(count)
            {
            case 4:
              {
                rs[i + 3] = C ((u | 0x80UL) & 0xBFUL);
                u >>= 6;
              }
            case 3:
              {
                rs[i + 2] = C ((u | 0x80UL) & 0xBFUL);
                u >>= 6;
              }
            case 2:
              {
                rs[i + 1] = C ((u | 0x80UL) & 0xBFUL);
                u >>= 6;
              }
            case 1:
              {
                rs[i] = C (u | first_byte_mask_[count]);
              }
            }

            i += count;
          }

          return r;
        }

        template <typename C>
        XMLCh* char_transcoder<C>::
        from (const C* s, std::size_t len)
        {
          bool valid (true);
          const C* end (s + len);

          // Find what the resulting buffer size will be.
          //
          std::size_t rl (0);
          unsigned int count (0);

          for (const C* p (s); p < end; ++p)
          {
            unsigned char c (*p);

            if (c < 0x80)
            {
              // Fast path.
              //
              rl += 1;
              continue;
            }
            else if ((c >> 5) == 0x06)
              count = 2;
            else if ((c >> 4) == 0x0E)
              count = 3;
            else if ((c >> 3) == 0x1E)
              count = 4;
            else
            {
              valid = false;
              break;
            }

            p += count - 1; // One will be added in the for loop

            if (p + 1 > end)
            {
              valid = false;
              break;
            }

            // BMP is represented by up to 3 code points in UTF-8.
            //
            rl += count > 3 ? 2 : 1;
          }

          if (!valid)
            throw invalid_utf8_string ();

          auto_array<XMLCh> r (new XMLCh[rl + 1]);
          XMLCh* ir (r.get ());

          unsigned int u (0); // Four byte UCS-4 char.

          for (const C* p (s); p < end; ++p)
          {
            unsigned char c (*p);

            if (c < 0x80)
            {
              // Fast path.
              //
              *ir++ = static_cast<XMLCh> (c);
              continue;
            }
            else if ((c >> 5) == 0x06)
            {
              // UTF-8:   110yyyyy 10zzzzzz
              // Unicode: 00000yyy yyzzzzzz
              //
              u = (c & 0x1F) << 6;

              c = *++p;
              if ((c >> 6) != 2)
              {
                valid = false;
                break;
              }
              u |= c & 0x3F;
            }
            else if ((c >> 4) == 0x0E)
            {
              // UTF-8:   1110xxxx 10yyyyyy 10zzzzzz
              // Unicode: xxxxyyyy yyzzzzzz
              //
              u = (c & 0x0F) << 6;

              c = *++p;
              if ((c >> 6) != 2)
              {
                valid = false;
                break;
              }
              u = (u | (c & 0x3F)) << 6;

              c = *++p;
              if ((c >> 6) != 2)
              {
                valid = false;
                break;
              }
              u |= c & 0x3F;
            }
            else if ((c >> 3) == 0x1E)
            {
              // UTF-8:   000wwwxx xxxxyyyy yyzzzzzz
              // Unicode: 11110www 10xxxxxx 10yyyyyy 10zzzzzz
              //
              u = (c & 0x07) << 6;

              c = *++p;
              if ((c >> 6) != 2)
              {
                valid = false;
                break;
              }
              u = (u | (c & 0x3F)) << 6;

              c = *++p;
              if ((c >> 6) != 2)
              {
                valid = false;
                break;
              }
              u = (u | (c & 0x3F)) << 6;

              c = *++p;
              if ((c >> 6) != 2)
              {
                valid = false;
                break;
              }
              u |= c & 0x3F;
            }

            if (u & 0xFFFF0000)
            {
              // Surrogate pair.
              //
              *ir++ = static_cast<XMLCh> (((u - 0x10000) >> 10) + 0xD800);
              *ir++ = static_cast<XMLCh> ((u & 0x3FF) + 0xDC00);
            }
            else
              *ir++ = static_cast<XMLCh> (u);
          }

          if (!valid)
            throw invalid_utf8_string ();

          *ir = XMLCh (0);

          return r.release ();
        }
      }
    }
  }
}

#endif // XSD_USE_LCP
#endif // XSD_CXX_XML_STRING_TXX


#if defined(XSD_USE_WCHAR) || !defined(XSD_USE_CHAR)

#ifndef XSD_CXX_XML_STRING_TXX_WCHAR
#define XSD_CXX_XML_STRING_TXX_WCHAR

namespace xsd
{
  namespace cxx
  {
    namespace xml
    {
      namespace bits
      {
        // wchar_transcoder (specialization for 2-byte wchar_t)
        //
        template <typename W>
        std::basic_string<W> wchar_transcoder<W, 2>::
        to (const XMLCh* s, std::size_t length)
        {
          std::basic_string<W> r;
          r.reserve (length + 1);
          r.resize (length);
          W* rs (const_cast<W*> (r.c_str ()));

          for (std::size_t i (0); i < length; ++s, ++i)
          {
            rs[i] = *s;
          }

          return r;
        }

        template <typename W>
        XMLCh* wchar_transcoder<W, 2>::
        from (const W* s, std::size_t length)
        {
          auto_array<XMLCh> r (new XMLCh[length + 1]);
          XMLCh* ir (r.get ());

          for (std::size_t i (0); i < length; ++ir, ++i)
          {
            *ir = static_cast<XMLCh> (s[i]);
          }

          *ir = XMLCh (0);

          return r.release ();
        }


        // wchar_transcoder (specialization for 4-byte wchar_t)
        //
        template <typename W>
        std::basic_string<W> wchar_transcoder<W, 4>::
        to (const XMLCh* s, std::size_t length)
        {
          const XMLCh* end (s + length);

          // Find what the resulting buffer size will be.
          //
          std::size_t rl (0);

          for (const XMLCh* p (s); p < end; ++p)
          {
            rl++;

            if ((*p >= 0xD800) && (*p <= 0xDBFF))
            {
              // Make sure we have one more char and it has a valid
              // value for the second char in a surrogate pair.
              //
              if (++p == end || !((*p >= 0xDC00) && (*p <= 0xDFFF)))
                throw invalid_utf16_string ();
            }
          }

          std::basic_string<W> r;
          r.reserve (rl + 1);
          r.resize (rl);
          W* rs (const_cast<W*> (r.c_str ()));

          std::size_t i (0);

          for (const XMLCh* p (s); p < end; ++p)
          {
            XMLCh x (*p);

            if (x < 0xD800 || x > 0xDBFF)
              rs[i++] = W (x);
            else
              rs[i++] = ((x - 0xD800) << 10) + (*++p - 0xDC00) + 0x10000;
          }

          return r;
        }

        template <typename W>
        XMLCh* wchar_transcoder<W, 4>::
        from (const W* s, std::size_t length)
        {
          // Find what the resulting buffer size will be.
          //
          std::size_t rl (0);

          for (const W* p (s); p < s + length; ++p)
          {
            rl += (*p & 0xFFFF0000) ? 2 : 1;
          }

          auto_array<XMLCh> r (new XMLCh[rl + 1]);
          XMLCh* ir (r.get ());

          for (const W* p (s); p < s + length; ++p)
          {
            W w (*p);

            if (w & 0xFFFF0000)
            {
              // Surrogate pair.
              //
              *ir++ = static_cast<XMLCh> (((w - 0x10000) >> 10) + 0xD800);
              *ir++ = static_cast<XMLCh> ((w & 0x3FF) + 0xDC00);
            }
            else
              *ir++ = static_cast<XMLCh> (w);
          }

          *ir = XMLCh (0);

          return r.release ();
        }
      }
    }
  }
}

#endif // XSD_CXX_XML_STRING_TXX_WCHAR
#endif // XSD_USE_WCHAR
