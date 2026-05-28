/* This is from https://github.com/vlm/asn1c.git, (C) Lev Walkin under the
   BSD 2-clause licence.  Picked because it contains many (F)lex edge cases */

%{

#include <string.h>
#include <errno.h>
#include <assert.h>

#include "asn1parser.h"
#include "asn1p_y.h"

int asn1p_lex(void);
void asn1p_lexer_hack_push_opaque_state(void);		/* Used in .y */
void asn1p_lexer_hack_enable_with_syntax(void);		/* Used in .y */
void asn1p_lexer_hack_push_encoding_control(void);	/* Used in .y */

#define	YY_FATAL_ERROR(msg)	do {			\
		fprintf(stderr,				\
			"lexer error at line %d, "	\
			"text \"%s\"\n",		\
			yylineno, yytext);		\
		exit(1);				\
	} while(0)

int asn1p_lexer_pedantic_1990 = 0;
int asn1p_lexer_types_year = 0;
int asn1p_lexer_constructs_year = 0;
int asn1p_lexer_extended_values = 0;

int asn1p_as_pointer;

static asn1c_integer_t _lex_atoi(const char *ptr);
static double          _lex_atod(const char *ptr);

/*
 * Check that the type is defined in the year of the standard choosen.
 */
#define	TYPE_LIFETIME(fyr, lyr)				\
	(!asn1p_lexer_types_year			\
	|| (fyr && fyr <= asn1p_lexer_types_year)	\
	|| (lyr && lyr  > asn1p_lexer_types_year))

/*
 * Check the the construction (or concept, i.e. CLASS) is defined in
 * a given year.
 */
#define	CONSTRUCT_LIFETIME(fyr, lyr)			\
	(!asn1p_lexer_constructs_year			\
	|| (fyr && fyr <= asn1p_lexer_constructs_year)	\
	|| (lyr && lyr  > asn1p_lexer_constructs_year))

/*
 * Append quoted string.
 */
#define	QAPPEND(text, tlen)	do {				\
		char *prev_text = asn1p_lval.tv_opaque.buf;	\
		int prev_len = asn1p_lval.tv_opaque.len;	\
		char *p;					\
								\
		p = malloc((tlen) + prev_len + 1);		\
		if(p == NULL) return -1;			\
								\
		if(prev_text) memcpy(p, prev_text, prev_len);	\
		memcpy(p + prev_len, text, tlen);		\
		p[prev_len + (tlen)] = '\0';			\
								\
		free(asn1p_lval.tv_opaque.buf);			\
		asn1p_lval.tv_opaque.buf = p;			\
		asn1p_lval.tv_opaque.len = (tlen) + prev_len;	\
	} while(0)

%}

%option	never-interactive
%option	noinput 
%option	noyywrap stack
/* Performance penalty is OK */
%option yylineno	
/* Controlled from within application */
%option debug		

%pointer

%x dash_comment
%x idash_comment
%x cpp_comment
%x quoted
%x opaque
%x encoding_control
%x with_syntax
%x extended_values

/* Newline */
NL	[\r\v\f\n]
/* White-space */
WSP	[\t\r\v\f\n ]

%%

<INITIAL>"\xef\xbb\xbf"		return UTF8_BOM;

-{3,}/[\r\n]	/* Immediately terminated long comment */
-{3,}/[^-\r\n]	yy_push_state(idash_comment);	/* Incorrect, but acceptable */
<idash_comment>{
	-{3,}	yy_pop_state(); /* Acceptable end of comment */
}

--<[ \t]*ASN1C.RepresentAsPointer[ \t]*>--	asn1p_as_pointer = 1;

<extended_values>{
    "#BIT STRING"    {
        yy_pop_state();
        return TOK_ExtValue_BIT_STRING;
    }
}

<INITIAL,with_syntax>--		yy_push_state(dash_comment);
<dash_comment,idash_comment>{

	{NL}	yy_pop_state();

	--	yy_pop_state();	/* End of comment */
	-	/* Eat single dash */
	[^\r\v\f\n-]+	/* Eat */
}

<INITIAL,cpp_comment,with_syntax>"/*"		yy_push_state(cpp_comment);
<cpp_comment>{
	[^*/<]	/* Eat */
	"*/"	yy_pop_state();
	.	/* Eat */
}


	/*
	 * This is state is being set from corresponding .y module when
	 * higher-level data is necessary to make proper parsing of the
	 * underlying data. Thus, we enter the <opaque> state and save
	 * everything for later processing.
	 */
<opaque>{

	"{"	{
			yy_push_state(opaque);
			asn1p_lval.tv_opaque.buf = strdup(yytext);
			asn1p_lval.tv_opaque.len = yyleng;
			return TOK_opaque;
		}

	"}"	{
			yy_pop_state();
			asn1p_lval.tv_opaque.buf = strdup(yytext);
			asn1p_lval.tv_opaque.len = yyleng;
			return TOK_opaque;
		}

	[^{}:=]+	{
			asn1p_lval.tv_opaque.buf = strdup(yytext);
			asn1p_lval.tv_opaque.len = yyleng;
			return TOK_opaque;
		}

	"::="	{
			fprintf(stderr,
				"ASN.1 Parser synchronization failure: "
				"\"%s\" at line %d must not appear "
				"inside value definition\n",
				yytext, yylineno);
			return -1;
		}

	[:=]	{
			asn1p_lval.tv_opaque.buf = strdup(yytext);
			asn1p_lval.tv_opaque.len = yyleng;
			return TOK_opaque;
		}

	}

\"[^\"]*		{
			asn1p_lval.tv_opaque.buf = 0;
			asn1p_lval.tv_opaque.len = 0;
			QAPPEND(yytext+1, yyleng-1);
			yy_push_state(quoted);
		}
<quoted>{

	\"\"	{ QAPPEND(yytext, yyleng-1); }	/* Add a single quote */
	[^\"]+	{ QAPPEND(yytext, yyleng); }

	\"	{
			yy_pop_state();
			/* Do not append last quote:
			// QAPPEND(yytext, yyleng); */

			if(asn1p_lexer_pedantic_1990
			&& strchr(yytext, '\n')) {
				fprintf(stderr, "%s: "
				"Newlines are prohibited by ASN.1:1990\n",
				asn1p_lval.tv_opaque.buf);
				return -1;
			}

			return TOK_cstring;
		}

	}

<encoding_control>{
	ENCODING-CONTROL	{
			const char *s = "ENCODING-CONTROL";
			const char *p = s + sizeof("ENCODING-CONTROL") - 2;
			for(; p >= s; p--) unput(*p);
			yy_pop_state();
		}
	END	unput('D'); unput('N'); unput('E'); yy_pop_state();
	[^{} \t\r\v\f\n]+
	[[:alnum:]]+
	.	/* Eat everything else */
	"\n"
	}

'[0-9A-F \t\r\v\f\n]+'H {
		/* " \t\r\n" weren't allowed in ASN.1:1990. */
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_hstring;
	}

'[01 \t\r\v\f\n]+'B	{
		/* " \t\r\n" weren't allowed in ASN.1:1990. */
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_bstring;
	}


-[1-9][0-9]*	{
		asn1p_lval.a_int = _lex_atoi(yytext);
		if(errno == ERANGE)
			return -1;
		return TOK_number_negative;
	}

[1-9][0-9]*	{
		asn1p_lval.a_int = _lex_atoi(yytext);
		if(errno == ERANGE)
			return -1;
		return TOK_number;
	}

"0"	{
		asn1p_lval.a_int = _lex_atoi(yytext);
		if(errno == ERANGE)
			return -1;
		return TOK_number;
	}

[-+]?[0-9]+[.]?([eE][-+]?)?[0-9]+ {
		asn1p_lval.a_dbl = _lex_atod(yytext);
		if(errno == ERANGE)
			return -1;
		return TOK_realnumber;
	}

ABSENT			return TOK_ABSENT;
ALL			return TOK_ALL;
ANY			{
				/* Appeared in 1990, removed in 1997 */
				if(TYPE_LIFETIME(1990, 1997))
					return TOK_ANY;	
				fprintf(stderr, "Keyword \"%s\" at line %d "
					"is obsolete\n", yytext, yylineno);
				REJECT;
			}
APPLICATION		return TOK_APPLICATION;
AUTOMATIC		return TOK_AUTOMATIC;
BEGIN			{
        if(asn1p_lexer_extended_values) {
            yy_push_state(extended_values);
        }
        return TOK_BEGIN;
    }
BIT			return TOK_BIT;
BMPString		{
				if(TYPE_LIFETIME(1994, 0))
					return TOK_BMPString;
				REJECT;
			}
BOOLEAN			return TOK_BOOLEAN;
BY			return TOK_BY;
CHARACTER		return TOK_CHARACTER;
CHOICE			return TOK_CHOICE;
CLASS			return TOK_CLASS;
COMPONENT		return TOK_COMPONENT;
COMPONENTS		return TOK_COMPONENTS;
CONSTRAINED		return TOK_CONSTRAINED;
CONTAINING		return TOK_CONTAINING;
DEFAULT			return TOK_DEFAULT;
DEFINED			{
				/* Appeared in 1990, removed in 1997 */
				if(TYPE_LIFETIME(1990, 1997))
					return TOK_DEFINED;
				fprintf(stderr, "Keyword \"%s\" at line %d "
					"is obsolete\n", yytext, yylineno);
				/* Deprecated since */
				REJECT;
			}
DEFINITIONS		return TOK_DEFINITIONS;
EMBEDDED		return TOK_EMBEDDED;
ENCODED			return TOK_ENCODED;
ENCODING-CONTROL	return TOK_ENCODING_CONTROL;
END			{
		    if(YYSTATE == extended_values) {
                yy_pop_state();
            }
            return TOK_END;
        }
ENUMERATED		return TOK_ENUMERATED;
EXCEPT			return TOK_EXCEPT;
EXPLICIT		return TOK_EXPLICIT;
EXPORTS			return TOK_EXPORTS;
EXTENSIBILITY		return TOK_EXTENSIBILITY;
EXTERNAL		return TOK_EXTERNAL;
FALSE			return TOK_FALSE;
FROM			return TOK_FROM;
GeneralizedTime		return TOK_GeneralizedTime;
GeneralString		return TOK_GeneralString;
GraphicString		return TOK_GraphicString;
IA5String		return TOK_IA5String;
IDENTIFIER		return TOK_IDENTIFIER;
IMPLICIT		return TOK_IMPLICIT;
IMPLIED			return TOK_IMPLIED;
IMPORTS			return TOK_IMPORTS;
INCLUDES		return TOK_INCLUDES;
INSTANCE		return TOK_INSTANCE;
INSTRUCTIONS		return TOK_INSTRUCTIONS;
INTEGER			return TOK_INTEGER;
INTERSECTION		return TOK_INTERSECTION;
ISO646String		return TOK_ISO646String;
MAX			return TOK_MAX;
MIN			return TOK_MIN;
MINUS-INFINITY		return TOK_MINUS_INFINITY;
NULL			return TOK_NULL;
NumericString		return TOK_NumericString;
OBJECT			return TOK_OBJECT;
ObjectDescriptor	return TOK_ObjectDescriptor;
OCTET			return TOK_OCTET;
OF			return TOK_OF;
OPTIONAL		return TOK_OPTIONAL;
PATTERN			return TOK_PATTERN;
PDV			return TOK_PDV;
PLUS-INFINITY		return TOK_PLUS_INFINITY;
PRESENT			return TOK_PRESENT;
PrintableString		return TOK_PrintableString;
PRIVATE			return TOK_PRIVATE;
REAL			return TOK_REAL;
RELATIVE-OID		return TOK_RELATIVE_OID;
SEQUENCE		return TOK_SEQUENCE;
SET			return TOK_SET;
SIZE			return TOK_SIZE;
STRING			return TOK_STRING;
SYNTAX			return TOK_SYNTAX;
T61String		return TOK_T61String;
TAGS			return TOK_TAGS;
TeletexString		return TOK_TeletexString;
TRUE			return TOK_TRUE;
UNION			return TOK_UNION;
UNIQUE			return TOK_UNIQUE;
UNIVERSAL		return TOK_UNIVERSAL;
UniversalString		{
				if(TYPE_LIFETIME(1994, 0))
					return TOK_UniversalString;
				REJECT;
			}
UTCTime			return TOK_UTCTime;
UTF8String		{
				if(TYPE_LIFETIME(1994, 0))
					return TOK_UTF8String;
				REJECT;
			}
VideotexString		return TOK_VideotexString;
VisibleString		return TOK_VisibleString;
WITH			return TOK_WITH;


<INITIAL,with_syntax>&[A-Z][A-Za-z0-9]*([-][A-Za-z0-9]+)*	{
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_typefieldreference;
	}

<INITIAL,with_syntax>&[a-z][a-zA-Z0-9]*([-][a-zA-Z0-9]+)*	{
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_valuefieldreference;
	}


[a-z][a-zA-Z0-9]*([-][a-zA-Z0-9]+)*	{
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_identifier;
	}

	/*
	 * objectclassreference
	 */
<INITIAL,extended_values>[A-Z][A-Z0-9]*([-][A-Z0-9]+)*	{
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_capitalreference;
	}

	/*
	 * typereference, modulereference
	 * NOTE: TOK_objectclassreference must be combined
	 * with this token to produce true typereference.
	 */
[A-Z][A-Za-z0-9]*([-][A-Za-z0-9]+)*	{
		asn1p_lval.tv_str = strdup(yytext);
		return TOK_typereference;
	}

<INITIAL,extended_values>"::="		return TOK_PPEQ;

"..."		return TOK_ThreeDots;
".."		return TOK_TwoDots;

<with_syntax>{

	[A-Z][A-Za-z0-9]*([-][A-Za-z0-9]+)*	{
				asn1p_lval.tv_str = strdup(yytext);
				return TOK_Literal;
			}

	","		{
				asn1p_lval.tv_str = strdup(yytext);
				return TOK_Literal;
			}

	"{"		{
				yy_push_state(with_syntax);
				asn1p_lval.tv_str = strdup(yytext);
				return TOK_Literal;
			}

	"["		return '[';
	"]"		return ']';

	{WSP}+		{
			asn1p_lval.tv_opaque.buf = strdup(yytext);
			asn1p_lval.tv_opaque.len = yyleng;
			return TOK_whitespace;
			}

	"}"		{
				yy_pop_state();
				if(YYSTATE == with_syntax) {
					asn1p_lval.tv_str = strdup(yytext);
					return TOK_Literal;
				} else {
					return '}';
				}
			}

}


<INITIAL,extended_values>{WSP}+	/* Ignore whitespace */


[{][\t\r\v\f\n ]*[0-7][,][\t\r\v\f\n ]*[0-9]+[\t\r\v\f\n ]*[}]	{
		asn1c_integer_t v1 = -1, v2 = -1;
		char *p;
		for(p = yytext; *p; p++)
			if(*p >= '0' && *p <= '9')
			{ v1 = _lex_atoi(p); break; }
		while(*p >= '0' && *p <= '9') p++;	/* Skip digits */
		for(; *p; p++) if(*p >= '0' && *p <= '9')
			{ v2 = _lex_atoi(p); break; }
		if(v1 < 0 || v1 > 7) {
			fprintf(stderr, "%s at line %d: X.680:2003, #37.14 "
				"mandates 0..7 range for Tuple's TableColumn\n",
				yytext, yylineno);
			return -1;
		}
		if(v2 < 0 || v2 > 15) {
			fprintf(stderr, "%s at line %d: X.680:2003, #37.14 "
				"mandates 0..15 range for Tuple's TableRow\n",
				yytext, yylineno);
			return -1;
		}
		asn1p_lval.a_int = (v1 << 4) + v2;
		return TOK_tuple;
	}

[{][\t\r\v\f\n ]*[0-9]+[,][\t\r\v\f\n ]*[0-9]+[,][\t\r\v\f\n ]*[0-9]+[,][\t\r\v\f\n ]*[0-9]+[\t\r\v\f\n ]*[}]	{
		asn1c_integer_t v1 = -1, v2 = -1, v3 = -1, v4 = -1;
		char *p;
		for(p = yytext; *p; p++)
			if(*p >= '0' && *p <= '9')
			{ v1 = _lex_atoi(p); break; }
		while(*p >= '0' && *p <= '9') p++;	/* Skip digits */
		for(; *p; p++) if(*p >= '0' && *p <= '9')
			{ v2 = _lex_atoi(p); break; }
		while(*p >= '0' && *p <= '9') p++;
		for(; *p; p++) if(*p >= '0' && *p <= '9')
			{ v3 = _lex_atoi(p); break; }
		while(*p >= '0' && *p <= '9') p++;
		for(; *p; p++) if(*p >= '0' && *p <= '9')
			{ v4 = _lex_atoi(p); break; }
		if(v1 < 0 || v1 > 127) {
			fprintf(stderr, "%s at line %d: X.680:2003, #37.12 "
				"mandates 0..127 range for Quadruple's Group\n",
				yytext, yylineno);
			return -1;
		}
		if(v2 < 0 || v2 > 255) {
			fprintf(stderr, "%s at line %d: X.680:2003, #37.12 "
				"mandates 0..255 range for Quadruple's Plane\n",
				yytext, yylineno);
			return -1;
		}
		if(v3 < 0 || v3 > 255) {
			fprintf(stderr, "%s at line %d: X.680:2003, #37.12 "
				"mandates 0..255 range for Quadruple's Row\n",
				yytext, yylineno);
			return -1;
		}
		if(v4 < 0 || v4 > 255) {
			fprintf(stderr, "%s at line %d: X.680:2003, #37.12 "
				"mandates 0..255 range for Quadruple's Cell\n",
				yytext, yylineno);
			return -1;
		}
		asn1p_lval.a_int = (v1 << 24) | (v2 << 16) | (v3 << 8) | v4;
		return TOK_quadruple;
	}


"[["        return TOK_VBracketLeft;
"]]"        return TOK_VBracketRight;

[(){},;:|!.&@\[\]^]	return yytext[0];

[^A-Za-z0-9:=,{}<.@()[]'\"|&^*;!-] {
		if(TYPE_LIFETIME(1994, 0))
			fprintf(stderr, "ERROR: ");
		fprintf(stderr,
		"Symbol '%c' at line %d is prohibited "
		"by ASN.1:1994 and ASN.1:1997\n",
			yytext[0], yylineno);
		if(TYPE_LIFETIME(1994, 0))
			return -1;
	}

<*>.	{
		fprintf(stderr,
			"Unexpected token at line %d: \"%s\"\n",
			yylineno, yytext);
		while(YYSTATE != INITIAL)
			yy_pop_state();
		if(0) {
			yy_top_state();	/* Just to use this function. */
			yy_fatal_error("Parse error");
		}
		return -1;
}

<*><<EOF>>      {
		while(YYSTATE != INITIAL)
			yy_pop_state();
		yyterminate();
	}


%%

/*
 * Very dirty but wonderful hack allowing to rule states from within .y file.
 */
void asn1p_lexer_hack_push_opaque_state() { yy_push_state(opaque); }

/*
 * Another hack which disables recognizing some tokens when inside WITH SYNTAX.
 */
void asn1p_lexer_hack_enable_with_syntax() { yy_push_state(with_syntax); }

/* Yet another */
void asn1p_lexer_hack_push_encoding_control() {
	yy_push_state(encoding_control);
}

static asn1c_integer_t
_lex_atoi(const char *ptr) {
	asn1c_integer_t value;
	if(asn1p_atoi(ptr, &value)) {
		fprintf(stderr,
			"Value \"%s\" at line %d is too large "
			"for this compiler! Please contact the asn1c author.\n",
			ptr, yylineno);
		errno = ERANGE;
	}
	return value;
}

static double
_lex_atod(const char *ptr) {
	double value;
	errno = 0;
	value = strtod(ptr, 0);
	if(errno) {
		fprintf(stderr,
			"Value \"%s\" at line %d is outside of `double` range "
			"in this compiler! Please contact the asn1c author.\n",
			ptr, yylineno);
		errno = ERANGE;
	}
	return value;
}

