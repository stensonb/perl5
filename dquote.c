/*    dquote.c
 *
 * This file contains functions that are related to
 * parsing double-quotish expressions.
 *
*/

#include "EXTERN.h"
#define PERL_IN_DQUOTE_C
#include "perl.h"
#include "dquote_inline.h"

/* XXX Add documentation after final interface and behavior is decided */
/* May want to show context for error, so would pass S_grok_bslash_c(pTHX_ const char* current, const char* start, const bool output_warning)
    U8 source = *current;
*/

bool
Perl_grok_bslash_c(pTHX_ const char   source,
                         U8 *         result,
                         const char** message,
                         U32 *        packed_warn)
{
    const bool output_warning = packed_warn == NULL;

    PERL_ARGS_ASSERT_GROK_BSLASH_C;

    *message = NULL;

    if (! isPRINT_A(source)) {
        *message = "Character following \"\\c\" must be printable ASCII";
        return FALSE;
    }

    if (source == '{') {
        const char control = toCTRL('{');
        if (isPRINT_A(control)) {
            /* diag_listed_as: Use "%s" instead of "%s" */
            *message = Perl_form(aTHX_ "Use \"%c\" instead of \"\\c{\"", control);
        }
        else {
            *message = "Sequence \"\\c{\" invalid";
        }
        return FALSE;
    }

    *result = toCTRL(source);
    if (isPRINT_A(*result) && ckWARN(WARN_SYNTAX)) {
        U8 clearer[3];
        U8 i = 0;
        char format[] = "\"\\c%c\" is more clearly written simply as \"%s\"";

        if (! isWORDCHAR(*result)) {
            clearer[i++] = '\\';
        }
        clearer[i++] = *result;
        clearer[i++] = '\0';

        if (output_warning) {
            Perl_warner(aTHX_ packWARN(WARN_SYNTAX), format, source, clearer);
        }
        else {
            *message = Perl_form(aTHX_ format, source, clearer);
            *packed_warn = packWARN(WARN_SYNTAX);
        }
    }

    return TRUE;
}

bool
Perl_grok_bslash_o(pTHX_ char **s, const char * const send, UV *uv,
                      const char** message,
                      const bool strict,
                      const bool silence_non_portable,
                      U32 *      packed_warn,
                      const bool UTF)
{

/*  Documentation to be supplied when interface nailed down finally
 *  This returns FALSE if there is an error which the caller need not recover
 *  from; otherwise TRUE.  In either case the caller should look at *len [???].
 *  It guarantees that the returned codepoint, *uv, when expressed as
 *  utf8 bytes, would fit within the skipped "\o{...}" bytes.
 *  On input:
 *	s   is the address of a pointer to a string.  **s is 'o', and the
 *	    previous character was a backslash.  At exit, *s will be advanced
 *	    to the byte just after those absorbed by this function.  Hence the
 *	    caller can continue parsing from there.  In the case of an error,
 *	    this routine has generally positioned *s to point just to the right
 *	    of the first bad spot, so that a message that has a "<--" to mark
 *	    the spot will be correctly positioned.
 *	send - 1  gives a limit in *s that this function is not permitted to
 *	    look beyond.  That is, the function may look at bytes only in the
 *	    range *s..send-1
 *	uv  points to a UV that will hold the output value, valid only if the
 *	    return from the function is TRUE
 *      message is a pointer that will be set to an internal buffer giving an
 *	    error message upon failure (the return is FALSE).  Untouched if
 *	    function succeeds
 *	output_warning says whether to output any warning messages, or suppress
 *	    them
 *	strict is true if this should fail instead of warn if there are
 *	    non-octal digits within the braces
 *      silence_non_portable is true if to suppress warnings about the code
 *          point returned being too large to fit on all platforms.
 *	UTF is true iff the string *s is encoded in UTF-8.
 */
    const bool output_warning = packed_warn == NULL;
    char* e;
    STRLEN numbers_len;
    NV overflowed = 0.0;
    I32 flags = PERL_SCAN_ALLOW_UNDERSCORES
              | PERL_SCAN_DISALLOW_PREFIX
              | PERL_SCAN_SILENT_ILLDIGIT
              | PERL_SCAN_GREATER_THAN_UV_MAX;

    PERL_ARGS_ASSERT_GROK_BSLASH_O;

    assert(*(*s - 1) == '\\');
    assert(* *s       == 'o');

    *message = NULL;
    if (! output_warning) {
        flags |= PERL_SCAN_SILENT_NON_PORTABLE;
    }

    (*s)++;

    if (send <= *s || **s != '{') {
	*message = "Missing braces on \\o{}";
	return FALSE;
    }

    e = (char *) memchr(*s, '}', send - *s);
    if (!e) {
        (*s)++;  /* Move past the '{' */
        while (isOCTAL(**s)) { /* Position beyond the legal digits */
            (*s)++;
        }
        *message = "Missing right brace on \\o{";
	return FALSE;
    }

    (*s)++;    /* Point to expected first digit (could be first byte of utf8
                  sequence if not a digit) */
    numbers_len = e - *s;
    if (numbers_len == 0) {
        (*s)++;    /* Move past the } */
	*message = "Empty \\o{}";
	return FALSE;
    }

    *uv = grok_oct(*s, &numbers_len, &flags, &overflowed);
    if (overflowed != 0.0) {
        *s = e;
        *message = Perl_form(aTHX_ "Use of code point %a is not allowed; the"
                                   " permissible max is %a (0%" UVof ")",
                                   overflowed, (NV) MAX_LEGAL_CP, MAX_LEGAL_CP);
        return FALSE;
    }

    /* Note that if has non-octal, will ignore everything starting with that up
     * to the '}' */
    if (numbers_len != (STRLEN) (e - *s)) {
        if (strict) {
            *s += numbers_len;
            *s += (UTF) ? UTF8_SAFE_SKIP(*s, send) : 1;
            *message = "Non-octal character";
            return FALSE;
        }

                /* XXX diag_listed_as: Non-octal character '%c'.  Resolved as "%s" */
        if (ckWARN(WARN_DIGIT)) {
            char non_octal = *(*s + numbers_len);
            if (isPRINT_A(non_octal)) {
                char format[] = "Non-octal character '%c'.  Resolved as \"\\o{%.*s}\"";

                if (output_warning) {
                    Perl_warner(aTHX_ packWARN(WARN_DIGIT), format, non_octal,
                                    (int) numbers_len, *s);
                }
                else {
                    *message = Perl_form(aTHX_ format, non_octal, (int) numbers_len, *s);
                    *packed_warn = packWARN(WARN_DIGIT);
                }
            }
            else {
                char format[] = "Non-octal character.  Resolved as \"\\o{%.*s}\"";

                if (output_warning) {
                    Perl_warner(aTHX_ packWARN(WARN_DIGIT), format, 
                                    (int) numbers_len, *s);
                }
                else {
                    *message = Perl_form(aTHX_ format, (int) numbers_len, *s);
                    *packed_warn = packWARN(WARN_DIGIT);
                }
            }
        }
    }

    /* Return past the '}' */
    *s = e + 1;

    if (   ! silence_non_portable
        && (flags & PERL_SCAN_SILENT_NON_PORTABLE)
        && ckWARN(WARN_PORTABLE))
    {
        /*XXX EXTCONST */
	*message = Perl_form(aTHX_ "Octal number > 037777777777 non-portable");
        *packed_warn = packWARN(WARN_PORTABLE);
    }

    return TRUE;
}

bool
Perl_grok_bslash_x(pTHX_ char **s, const char * const send, UV *uv,
                      const char** message,
                      const bool strict,
                      const bool silence_non_portable,
                      U32 *      packed_warn,
                      const bool UTF)
{

/*  Documentation to be supplied when interface nailed down finally
 *  This returns FALSE if there is an error which the caller need not recover
 *  from; otherwise TRUE.
 *  It guarantees that the returned codepoint, *uv, when expressed as
 *  utf8 bytes, would fit within the skipped "\x{...}" bytes.
 *
 *  On input:
 *	s   is the address of a pointer to a string.  **s is 'x', and the
 *	    previous character was a backslash.  At exit, *s will be advanced
 *	    to the byte just after those absorbed by this function.  Hence the
 *	    caller can continue parsing from there.  In the case of an error,
 *	    this routine has generally positioned *s to point just to the right
 *	    of the first bad spot, so that a message that has a "<--" to mark
 *	    the spot will be correctly positioned.
 *	send - 1  gives a limit in *s that this function is not permitted to
 *	    look beyond.  That is, the function may look at bytes only in the
 *	    range *s..send-1
 *	uv  points to a UV that will hold the output value, valid only if the
 *	    return from the function is TRUE
 *      message is a pointer that will be set to an internal buffer giving an
 *	    error message upon failure (the return is FALSE).  Untouched if
 *	    function succeeds
 *	*fatal says whether to output any warning messages, or suppress
 *	    them
 *	strict is true if anything out of the ordinary should cause this to
 *	    fail instead of warn or be silent.  For example, it requires
 *	    exactly 2 digits following the \x (when there are no braces).
 *	    3 digits could be a mistake, so is forbidden in this mode.
 *      silence_non_portable is true if to suppress warnings about the code
 *          point returned being too large to fit on all platforms.
 *	UTF is true iff the string *s is encoded in UTF-8.
 */
    const bool output_warning = packed_warn == NULL;
    char* e;
    STRLEN numbers_len;
    NV overflowed = 0.0;
    I32 flags = PERL_SCAN_DISALLOW_PREFIX
              | PERL_SCAN_NOTIFY_ILLDIGIT
              | PERL_SCAN_SILENT_ILLDIGIT
              | PERL_SCAN_GREATER_THAN_UV_MAX;

    PERL_ARGS_ASSERT_GROK_BSLASH_X;

    assert(*(*s - 1) == '\\');
    assert(* *s      == 'x');

    *message = NULL;
    if (! output_warning) {
        flags |= PERL_SCAN_SILENT_NON_PORTABLE;
    }

    (*s)++;

    if (send <= *s) {
        if (strict) {
            *message = "Empty \\x";
            return FALSE;
        }

        /* Sadly, to preserve backcompat, an empty \x at the end of string is
         * interpreted as a NUL */
        *uv = 0;
        return TRUE;
    }

    if (**s != '{') {
        STRLEN len = (strict) ? 3 : 2;

	*uv = grok_hex(*s, &len, &flags, NULL);
	*s += len;

        if (len != 2 && (strict || (flags & PERL_SCAN_NOTIFY_ILLDIGIT))) {
            if (len < 2) {
                if (strict) {
                    *s += (UTF) ? UTF8_SAFE_SKIP(*s, send) : 1;
                    *message = "Non-hex character";
                    return FALSE;
                }
                
                if (ckWARN(WARN_DIGIT)) {
                    char format[] = "Non-hex character terminates \\x early.  Resolved as \"\\x%02X\"";

                    if (output_warning) {
                        Perl_warner(aTHX_ packWARN(WARN_DIGIT), format, (unsigned) *uv);
                    }
                    else {
                        *message = Perl_form(aTHX_ format, (unsigned) *uv);
                        *packed_warn = packWARN(WARN_DIGIT);
                    }
                }
            }
            else if (strict) {
                *message = "Use \\x{...} for more than two hex characters";
                return FALSE;
            }
        }
	return TRUE;
    }

    e = (char *) memchr(*s, '}', send - *s);
    if (!e) {
        (*s)++;  /* Move past the '{' */
        while (isXDIGIT(**s)) { /* Position beyond the legal digits */
            (*s)++;
        }
        /* XXX The corresponding message above for \o is just '\\o{'; other
         * messages for other constructs include the '}', so are inconsistent.
         */
	*message = "Missing right brace on \\x{}";
	return FALSE;
    }

    (*s)++;    /* Point to expected first digit (could be first byte of utf8
                  sequence if not a digit) */
    numbers_len = e - *s;
    if (numbers_len == 0) {
        if (strict) {
            (*s)++;    /* Move past the } */
            *message = "Empty \\x{}";
            return FALSE;
        }
        *s = e + 1;
        *uv = 0;
        return TRUE;
    }

    flags |= PERL_SCAN_ALLOW_UNDERSCORES;

    *uv = grok_hex(*s, &numbers_len, &flags, &overflowed);
    if (overflowed != 0.0) {
        *s = e;
        *message = Perl_form(aTHX_ "Use of code point %a is not allowed; the"
                                   " permissible max is %a (0x%" UVXf ")",
                                   overflowed, (NV) MAX_LEGAL_CP, MAX_LEGAL_CP);
        return FALSE;
    }

    if (numbers_len != (STRLEN) (e - *s)) {
        if (strict) {
            *s += numbers_len;
            *s += (UTF) ? UTF8_SAFE_SKIP(*s, send) : 1;
            *message = "Non-hex character";
            return FALSE;
        }

        if (ckWARN(WARN_DIGIT)) {
            char format[] = "Non-hex character.  Resolved as \"\\x{%.*s}\"";

            if (output_warning) {
                Perl_warner(aTHX_ packWARN(WARN_DIGIT), format, 
                                  (int) numbers_len, *s);
            }
            else {
                *message = Perl_form(aTHX_ format, (int) numbers_len, *s);
                *packed_warn = packWARN(WARN_DIGIT);
            }
        }
    }

    /* Return past the '}' */
    *s = e + 1;

    if (   ! silence_non_portable
        && (flags & PERL_SCAN_SILENT_NON_PORTABLE)
        && ckWARN(WARN_PORTABLE))
    {
        /*XXX EXTCONST */
	*message = Perl_form(aTHX_ "Hexadecimal number > 0xffffffff non-portable");
        *packed_warn = packWARN(WARN_PORTABLE);
    }

    return TRUE;
}

/*
 * ex: set ts=8 sts=4 sw=4 et:
 */
