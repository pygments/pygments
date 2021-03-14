using std::numerical;
string contains(const char str);
string contains(const char str) {}
string* contains(const char str);
string* contains(const char str) {}
string * contains(const char str);
string * contains(const char str) {}
string    *    contains(const char str);
string    *    contains(const char str) {}
string   *contains(const char str);
string   *contains(const char str) {}
string   **contains(const char str);
string   **contains(const char str) {}
string** contains(const char str);
string** contains(const char str) {}
string  ** contains(const char str);
string  ** contains(const char str) {}
string  &  contains(const char str);
string  &  contains(const char str) {}
string&   contains(const char str);
string&   contains(const char str) {}
string   &contains(const char str);
string   &contains(const char str) {}
string   &&contains(const char str);
string   &&contains(const char str) {}
string   &&   contains(const char str);
string   &&   contains(const char str) {}
string&&   contains(const char str);
string&&   contains(const char str) {}
const string contains(const char str);
const string contains(const char str) {}
explicit const string contains(const char str);
explicit const string contains(const char str) {}
explicit const string contains(const char str) noexcept;
explicit const string contains(const char str) noexcept {}
explicit const string contains(const char str) noexcept const;
explicit const string contains(const char str) noexcept const {}


explicit const string contains(const char&str);
explicit const string contains(const char&str) {}
explicit const string contains(const char&   str);
explicit const string contains(const char&   str) {}
explicit const string contains(const char   str);
explicit const string contains(const char   str) {}
explicit const string contains(const char&&   str);
explicit const string contains(const char&&   str) {}
explicit const string contains(const char&&   str);
explicit const string contains(const char&&   str) {}
explicit const string contains(const char &&str);
explicit const string contains(const char &&str) {}
explicit const string contains(const char *****  str);
explicit const string contains(const char *****  str) {}
explicit const string contains(const char *****str);
explicit const string contains(const char *****str) {}
explicit const string contains(const char***** str);
explicit const string contains(const char***** str) {}
explicit const string contains(const char *str);
explicit const string contains(const char *str) {}
explicit const string contains(const char* str);
explicit const string contains(const char* str) {}
explicit const string contains(const char * str);
explicit const string contains(const char * str) {}

// Names with namespaces

string Type::contains(char c) const noexcept;
string Type::contains(char c) const noexcept {}
std::string contains(char c) const noexcept;
std::string contains(char c) const noexcept {}
std::string contains(std::vector<char> chars) const noexcept;
std::string contains(std::vector<char> chars) const noexcept {}
std::string std::vector::contains(std::vector<char> chars) const;
std::string std::vector::contains(std::vector<char> chars) const  {}
const inline explicit std::string std::vector::contains(std::vector<char> chars) const  {}
const inline explicit std::string std::vector::contains(std::vector<char> chars) const;

// inside classes

class raz {
    const virtual std::string contains(const std::string str);
}

// Make sure these are not functions:
else if(flag && func_call()) {}
new T();
const operator int() const {} // so int is lexed as type and not function.name