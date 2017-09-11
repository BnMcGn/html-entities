# About html-entities

Html-entities lets you encode and decode entities in HTML.

### Encoding

    (html-entities:encode-entities "This string contains <>&") => "This string contains &lt;&gt;&amp;"

By default, it will encode all strange characters.  For example,

    (html-entities:encode-entities "ø ¬ Û") => "&oslash; &not; &Ucirc;"

You can make it a different set of characters by providing the `encode-entities` function with a regex.  Only matching characters will be encoded:

    ;; only encode <, >, and &
    (html-entities:encode-entities "ø ¬ Û <> &" :regex "[<>&]") => "ø ¬ Û &lt;&gt; &amp;"

You can also control the behavior with the variables `*encode-using-named-entities*` and `*encode-in-hexadecimal*`.

  * `*encode-using-named-entities*` -- default is t. When true, use names for entities if possible.
  * `*encode-in-hexadecimal*` -- default is t. When true, use hexadecimal rather than decimal to encode entities that have no name.

### Decoding

    (html-entities:decode-entities "&oslash; &not; &Ucirc;") => "ø ¬ Û"

The `decode-entities` function has no special options, it simply decodes everything it comes across.

### SGML mode

The special variable `*enable-sgml*` (nil by default) makes the encoding functions use the SGML mappings for encoding and decoding rather than the HTML ones.  SGML entities are almost a perfect superset of HTML entities, with the exception of &apos;, which maps to U+0027 (the normal single-quote character) in HTML but U+02BC (MODIFIER_LETTER_APOSTROPHE) in SGML.  You probably don't need this, unless you get errors when decoding someone else's text because the SGML entities aren't recognized.
 
### Other Notes

This library requires cl-ppcre.  It has only been tested with SBCL, but it doesn't have anything crazy that should cause trouble with other Common Lisp implementations.  Unicode might be a problem, I'm not sure how that's handled in other lisps.

Html-entities was originally written by Aaron Sokoloski (asokoloski@gmail.com). The old repository is [here](https://code.google.com/archive/p/html-entities/)
