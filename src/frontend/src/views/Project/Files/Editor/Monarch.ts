export {Monarch};

namespace Monarch {
  export function getRacketTokenizer() {
    return {
      // Set defaultToken to invalid to see what you do not tokenize yet
      defaultToken: "invalid",

      keywords: [
        "define", "cond", "else", "if", "struct", "provide",
        "require", "+", "-", "*", "/", "quotient", "remainder",
        "=>", "<=", "<", ">", "=", "and", "or", "boolean?",
        "not", "equal?", "eqv?", "eq?",
        "true", "false", "symbol=?", "boolean=?",
        "false?", "nand", "nor", "implies", "xor", "number?",
        "real?", "rational?", "integer?", "zero?", "positive?", "negative?",
        "even?", "odd?", "quotient/remainder",
        "modulo", "add1", "sub1", "abs", "max", "min", "gcd",
        "lcm", "round", "floor", "ceiling", "sqrt", "expt", "exp", "log",
        "sin", "cos", "tan", "asin", "acos", "atan",
        "random", "random-seed", "number->string", "string->number",
        "pi", "sqr", "sgn", "sinh", "cosh", "tanh",
        "symbol?", "symbol->string", "string->symbol",
        "string", "make-string", "string?", "string-length",
        "string-ref", "substring", "string-append", "string->list",
        "list->string", "build-string", "string=?", "string<?",
        "string<=?", "string>?", "string>=?", "string-ci=?",
        "string-ci<?", "string-ci<=?", "string-ci>?", "string-ci>=?",
        "string-upcase", "string-downcase", "string-titlecase",
        "string-foldcase", "string-append*", "string-join",
        "string-replace", "string-split", "string-trim",
        "non-empty-string?", "string-contains?", "string-prefix?",
        "string-suffix?", "char?", "char->integer", "integer->char",
        "char=?", "char<?", "char<=?", "char>?", "char>=?",
        "char-ci=?", "char-ci<?", "char-ci<=?", "char-ci>?",
        "char-ci>=?", "char-alphabetic?", "char-lower-case?",
        "char-upper-case?", "char-title-case?", "char-numeric?",
        "char-symbolic?", "char-punctuation?", "char-whitespace?",
        "char-blank?", "char-graphic?", "char-iso-control?",
        "char-upcase", "char-downcase", "char-titlecase",
        "char-foldcase", "pair?", "null?", "cons",
        "null", "list?", "car", "cdr", "list*", "build-list",
        "length", "list-ref", "list-tail", "append", "reverse",
        "map", "andmap", "ormap", "for-each", "foldl", "foldr",
        "filter", "remove", "remove*", "sort", "member", "memf",
        "findf", "assoc", "assf", "empty", "cons?", "empty?",
        "first", "second", "rest", "third", "fourth", "fifth",
        "sixth", "seventh", "eighth", "ninth", "tenth", "last",
        "last-pair", "make-list", "index-of", "take", "drop",
        "split-at", "takef", "dropf", "append*", "flatten",
        "check-duplicates", "remove-duplicates", "filter-map",
        "count", "partition", "range", "append-map", "filter-not",
        "shuffle", "argmin", "argmax", "group-by", "void",
        "begin", "let", "lambda", "local"
      ],

      // C style strings
      escapes: /\\(?:[abfnrtv\\""]|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

      // The main tokenizer for our languages
      tokenizer: {
        root: [
          // whitespace
          { include: "@whitespace" },

          // lang
          [/\#lang.*$/, "keyword"],

          // bRackets
          [/[{}()\[\]]/, "@brackets"],

          // booleans
          [/\#(true|false|[tfTF])/, "string"],

          // numbers
          [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
          [/0[xX][0-9a-fA-F]+/, "number.hex"],
          [/\d+/, "keyword"],

          // symbols
          [/'((\\.)|[^()\[\]{}\s])+/, "keyword"],

          // strings
          [/"([^"\\]|\\.)*$/, "string.invalid" ],  // non-teminated string
          [/"/,  { token: "string.quote", bracket: "@open", next: "@string" } ],

          // characters
          [/\#\\u[0-9a-fA-F]+/, "string"],
          [/\#\\[^\s]/, "string"],
          [/\#/, "string.invalid"],

          // identifiers and keywords
          [/[^()\[\]{}',"`;#|\\$][\w$]*/, { cases: {
                                       "@keywords": "keyword",
                                       "@default": "identifier" } }],
        ],

        comment: [
          [/[^\#\|]+/, "comment" ],
          [/\#\|/,    "comment", "@push" ],    // nested comment
          [/\|\#/,    "comment", "@pop"  ],
          [/\|\#/,   "comment" ]
        ],

        string: [
          [/[^\\"]+/,  "string"],
          [/@escapes/, "string.escape"],
          [/\\./,      "string.escape.invalid"],
          [/"/,        { token: "string.quote", bracket: "@close", next: "@pop" } ]
        ],

        whitespace: [
          [/[ \t\r\n]+/, "white"],
          [/\#\|/, "comment", "@comment"],
          [/;.*$/,    "comment"]
        ],
      },
    };

  }

}
