// $Id: cons-lists.cpp,v 1.85 2020-10-07 15:57:36-07 - - $

#include <cassert>
#include <cctype>
#include <iostream>
#include <string>
#include <vector>
using namespace std;


//
// NAME
//    cons-lists - parse subset of scheme lists
//
// SYNOPSIS
//    cons-lists [list_expr...]
//
// DESCRIPTION
//
//    It is intended only to expore Scheme standard lists.  This is
//    a small subset of scheme syntax, supporting only proper lists,
//    and the parsing and printing of them.
//
//    Proper OO style scatters multiple functions across class
//    definitions.  Function style scatters multiple objects across
//    individual functions.  This uses more functional than OO style.
//
//    No loops were used in the coding of this program (except in
//    the test harness in the main function.  This is likely somewhat
//    inefficient, because C++ does not guarantee that tail recursion
//    is implemented.
//
//    Multiple extra objects are created, causing some memory leak.
//    No attempt is made to delete extraneous objects, so memory
//    leak does happen.
//
//

// discriminated union to hold data
using ref = const struct object*;
enum type_t {NIL, SYM, CELL};
struct object {
   const type_t type = NIL;
   union {
      const char sym = 0;
      const struct {ref car = 0; ref cdr = 0;} cell;
   };
   object (type_t type_): type(type_) {}
   object (type_t type_, char sym_): type(type_), sym(sym_) {}
   object (type_t type_, const ref car_, const ref cdr_):
      type(type_), cell({car_,cdr_}) {
   }
};

// list data structure construction functions
const ref nil = new object (NIL);
ref sym (const char chr_) {
   return new object (SYM, chr_);
}
ref cons (const ref car_, const ref cdr_) {
   return new object (CELL, car_, cdr_);
}

// print an object
void print (const ref item, bool is_cdr = false) {
   switch (item->type) {
      case NIL:  cout << (is_cdr ? ")" : "()");
                 break;
      case SYM:  cout << (is_cdr ? " " : "") << item->sym;
                 break;
      case CELL: cout << (is_cdr ? " " : "(");
                 print (item->cell.car);
                 print (item->cell.cdr, true);
                 break;
      default:   assert (false && "item->type is corrupted");
   }
}

// buffer to hold input string and scan tokens
struct buffer {
   string const* const str;
   size_t const pos;
   bool eof() const {return pos == str->size();}
   char first() const {return str->at (pos);}
   buffer rest() const {return {str, pos + 1};}
   operator string() const {return str->substr(pos);}
   pair<int,buffer> scan() const;
};
pair<int,buffer> buffer::scan() const {
   if (eof()) return {EOF, *this};
   if (isspace (first())) return rest().scan();
   return {first(), rest()};
}

// mutually recursive sym and list parsers
struct parse_state {const ref obj; const buffer buf;};
parse_state parse_buf (const buffer&);

parse_state parse_buf_cdr (const buffer& buf) {
   const auto [chr, rest] = buf.scan();
   if (chr == EOF) return {nil, rest};
   if (chr == ')') return {nil, rest};
   const auto [car, restcar] = parse_buf (buf);
   const auto [cdr, restcdr] = parse_buf_cdr (restcar);
   return {cons (car, cdr), restcdr};
}

parse_state parse_buf (const buffer& buf) {
   const auto [chr, rest] = buf.scan();
   if (chr == EOF) return {nil, rest};
   if (chr == '(') return parse_buf_cdr (rest);
   return {sym (chr), rest};
}

// top level parser to control parse
parse_state parse (const string& str) {
   cout << "string: \"" << str << "\"" << endl;
   const auto [obj, rest] = parse_buf ({&str, 0});
   return {obj, rest};
}

void report (const parse_state& state) {
   cout << "object: \"";
   print (state.obj);
   cout << "\"" << endl;
   const auto [chr, rest] = state.buf.scan();
   if (chr != EOF) {
      cout << "*ERROR: \"" << char (chr) << string (rest)
           << "\"" << endl;
   }
   cout << endl;
}

// explicitly construct arbitrary example
ref example_1() {
   return cons (sym ('a'), cons (sym ('b'), cons (sym ('c'), nil)));
}
ref example_2() {
   return cons (cons (sym ('a'), cons (sym ('b'), nil)),
                cons (cons (sym ('c'), cons (sym ('d'), nil)),
                      nil));
}
ref example_3() {
   // '((a (b)) (c d) () e)
   const ref p1 = cons (sym ('a'),
                        cons (cons (sym ('b'),
                                    nil),
                              nil));
   const ref p2 = cons (sym ('c'),
                        cons (sym ('d'),
                              nil));
   const ref p3 = nil;
   const ref p4 = sym ('e');
   const ref list = cons (p1, cons (p2, cons (p3, cons (p4, nil))));
   return list;
}
const vector<pair<string,ref>> builtin_examples {
   {"example_1", example_1()},
   {"example_2", example_2()},
   {"example_3", example_3()},
};

const vector<string> scan_tests {"a", "((a (b)) (c d) () e)"};
// main function is test harness
int main (int argc, char** argv) {
   vector<string> args (&argv[1], &argv[argc]);
   for (const auto& example: builtin_examples) {
      cout << example.first << ":" << endl;
      report ({example.second, {new string(),0}});
   }
   for (const auto& test: scan_tests) report (parse (test));
   for (const auto& arg: args) report (parse (arg));
   string line;
   while (getline (cin, line)) report (parse (line));
   return 0;
}

