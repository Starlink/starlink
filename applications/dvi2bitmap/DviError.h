#ifndef DVIERROR_HEADER_READ
#define DVIERROR_HEADER_READ 1

#include <string>

class DviError {
 public:
    DviError(const string s) : problem_(s) { }
    DviError(const char *fmt, ...);
    virtual ~DviError() { }
    void print() const;
    string problem() const { return problem_; }
 protected:
    DviError() { };
    string problem_;
};
class DviBug : public DviError {
 public:
    DviBug(const string s) : DviError(s) { }
    DviBug(const char *fmt, ...);
    void print() const;
};

#endif /* DVIERROR_HEADER_READ */
