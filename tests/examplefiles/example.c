#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "codegen.h"
#include "symboltable.h"
#include "stringbuffer.h"

extern void yyerror(char* msg);

static stringBuffer* staticVariableBuffer;
static stringBuffer* classInitBuffer;
static stringBuffer* currentMethodBuffer;
static stringBuffer* finishedMethodsBuffer;
static stringBuffer* mainBuffer;

static int currentMethodBufferIndex;
static int currentMethodStackSize;
static int currentMethodStackSizeMax;
static int currentMethodNumberOfLocals;

static int classInitBufferIndex;
static int classInitStackSize;
static int classInitStackSizeMax;

static int labelCounter = 0;
static int global       = 1;

char tempString[MAX_LENGTH_OF_COMMAND];

extern char* className;        /* from minako-syntax.y */

/* forward declarations */
static void increaseStackby(int stackdiff);
char convertType(int type);

void codegenInit() {
	staticVariableBuffer  = newStringBuffer();
	classInitBuffer       = newStringBuffer();
	currentMethodBuffer   = 0;
	finishedMethodsBuffer = newStringBuffer();
	mainBuffer            = newStringBuffer();

	stringBufferAppend(mainBuffer, "; ------- Header --------------------------------------------"); 
	sprintf(tempString, ".class  public synchronized %s", className);
	stringBufferAppend(mainBuffer, tempString);
	stringBufferAppend(mainBuffer, ".super  java/lang/Object");
	stringBufferAppend(mainBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(mainBuffer, "");
	
	stringBufferAppend(finishedMethodsBuffer, "; ------- Constructor ---------------------------------------");
	stringBufferAppend(finishedMethodsBuffer, ".method public <init>()V");
	stringBufferAppend(finishedMethodsBuffer, "\t.limit stack 1");
	stringBufferAppend(finishedMethodsBuffer, "\t.limit locals 1");
	stringBufferAppend(finishedMethodsBuffer, "\taload_0");
	stringBufferAppend(finishedMethodsBuffer, "\tinvokenonvirtual java/lang/Object/<init>()V");
	stringBufferAppend(finishedMethodsBuffer, "\treturn");
	stringBufferAppend(finishedMethodsBuffer, ".end method");
	stringBufferAppend(finishedMethodsBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(finishedMethodsBuffer, "");

	stringBufferAppend(staticVariableBuffer, "; ------- Class Variables -----------------------------------");

	stringBufferAppend(classInitBuffer, "; ------- Class Initializer ---------------------------------");
	stringBufferAppend(classInitBuffer, ".method static <clinit>()V");
	classInitBufferIndex = classInitBuffer->numberOfNextElement;
	stringBufferAppend(classInitBuffer, "\t.limit locals 0");

}

void codegenAppendCommand(char* cmd, int stackdiff) {
	char tempString[MAX_LENGTH_OF_COMMAND];
	sprintf(tempString, "\t%s", cmd);
	if (global) stringBufferAppend(classInitBuffer, tempString);
	else stringBufferAppend(currentMethodBuffer, tempString);
	increaseStackby(stackdiff);
}

void codegenInsertCommand(int address, char* cmd, int stackdiff) {
	char tempString[MAX_LENGTH_OF_COMMAND];
	sprintf(tempString, "\t%s", cmd);
	if (global) stringBufferInsert(classInitBuffer, address, tempString);
	else stringBufferInsert(currentMethodBuffer, address, tempString);
	increaseStackby(stackdiff);
}

void codegenAppendLabel(int label) {
	char tempString[MAX_LENGTH_OF_COMMAND];
	sprintf(tempString, "Label%d:", label);
	if (global) stringBufferAppend(classInitBuffer, tempString);
	else stringBufferAppend(currentMethodBuffer, tempString);
}

void codegenAddVariable(char* name, int type) {
	/*fprintf(stderr, "add variable %s(%d) global=%d ", name, convertType(type), global);*/
	if (global) {
		if (type == TYPE_INT) sprintf(tempString, ".field static %s %c", name, 'I');
		else if (type == TYPE_FLOAT) sprintf(tempString, ".field static %s %c", name, 'F');
		else if (type == TYPE_BOOLEAN) sprintf(tempString, ".field static %s %c", name, 'Z');
		else yyerror("compiler-intern error in codegenAddGlobalVariable().\n");
		stringBufferAppend(staticVariableBuffer, tempString);
	}
	else {
		currentMethodNumberOfLocals++;
	}
}

int codegenGetNextLabel() {
	return labelCounter++;
}

int codegenGetCurrentAddress() {
	if (global) return classInitBuffer->numberOfNextElement;
	else return currentMethodBuffer->numberOfNextElement;
}

void codegenEnterFunction(symtabEntry* entry) {
	currentMethodBuffer = newStringBuffer();
	currentMethodStackSize = 0;
	currentMethodStackSizeMax = 0;
	labelCounter = 1;
	global = 0;
	
	if (strcmp(entry->name, "main") == 0) {
		if (entry->idtype != TYPE_VOID) yyerror("main has to be void.\n");
		currentMethodNumberOfLocals = 1;
		symtabInsert(strdup("#main-param#"), TYPE_VOID, CLASS_FUNC);
		stringBufferAppend(currentMethodBuffer, "; ------- Methode ---- void main() --------------------------");
		stringBufferAppend(currentMethodBuffer, ".method public static main([Ljava/lang/String;)V");
	}
	else {
		int i;
		currentMethodNumberOfLocals = entry->paramIndex;
		stringBufferAppend(currentMethodBuffer, "; ------- Methode -------------------------------------------");
		sprintf(tempString, ".method public static %s(", entry->name);
		for (i=entry->paramIndex-1; i>=0; i--) {
			int type = entry->params[i]->idtype;
			tempString[strlen(tempString)+1] = 0;
			tempString[strlen(tempString)] = convertType(type);
		}
		tempString[strlen(tempString)+2] = 0;
		tempString[strlen(tempString)+1] = convertType(entry->idtype);
		tempString[strlen(tempString)]   = ')';
		stringBufferAppend(currentMethodBuffer, tempString);
	}
	currentMethodBufferIndex = currentMethodBuffer->numberOfNextElement;
}

void codegenLeaveFunction() {
	global = 1;
	sprintf(tempString, "\t.limit locals %d", currentMethodNumberOfLocals);
	stringBufferInsert(currentMethodBuffer, currentMethodBufferIndex, tempString);
	sprintf(tempString, "\t.limit stack %d", currentMethodStackSizeMax);
	stringBufferInsert(currentMethodBuffer, currentMethodBufferIndex, tempString);
	stringBufferAppend(currentMethodBuffer, "\treturn");
	stringBufferAppend(currentMethodBuffer, ".end method");
	stringBufferAppend(currentMethodBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(currentMethodBuffer, "");

	stringBufferConcatenate(finishedMethodsBuffer, currentMethodBuffer);
}



void codegenFinishCode() {
	stringBufferAppend(staticVariableBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(staticVariableBuffer, "");

	sprintf(tempString, "\t.limit stack %d", classInitStackSizeMax);
	stringBufferInsert(classInitBuffer, classInitBufferIndex, tempString);
	stringBufferAppend(classInitBuffer, "\treturn");
	stringBufferAppend(classInitBuffer, ".end method");
	stringBufferAppend(classInitBuffer, "; -----------------------------------------------------------");
	
	stringBufferConcatenate(mainBuffer, staticVariableBuffer);
	stringBufferConcatenate(mainBuffer, finishedMethodsBuffer);
	stringBufferConcatenate(mainBuffer, classInitBuffer);

	stringBufferPrint(mainBuffer);
}

static void increaseStackby(int stackdiff) {
	if (global) {
		classInitStackSize += stackdiff;
		if (classInitStackSize > classInitStackSizeMax) classInitStackSizeMax = classInitStackSize;
	}
	else {
		currentMethodStackSize += stackdiff;
		if (currentMethodStackSize > currentMethodStackSizeMax) currentMethodStackSizeMax = currentMethodStackSize;
	}
}

char convertType(int type) {
	switch(type) {
		case TYPE_VOID:    return 'V';
		case TYPE_INT:     return 'I';
		case TYPE_FLOAT:   return 'F';
		case TYPE_BOOLEAN: return 'Z';
		default: yyerror("compiler-intern error in convertType().\n");
	}
	return 0; /* to avoid compiler-warning */
}


//#include <stdlib.h>
//#include <stdio.h>

int main() {
	int a = 12, b = 44;
	while (a != b) {
		if (a > b)
			a -= b;
		else
			b -= a;
	}
	printf("%d\n%d", a, 0X0);\
}


/**********************************************************************

  array.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Fri Aug  6 09:46:12 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#include "ruby.h"
#include "util.h"
#include "st.h"
#include "node.h"

VALUE rb_cArray, rb_cValues;

static ID id_cmp;

#define ARY_DEFAULT_SIZE 16


void
rb_mem_clear(mem, size)
    register VALUE *mem;
    register long size;
{
    while (size--) {
	*mem++ = Qnil;
    }
}

static inline void
memfill(mem, size, val)
    register VALUE *mem;
    register long size;
    register VALUE val;
{
    while (size--) {
	*mem++ = val;
    }
}

#define ARY_TMPLOCK  FL_USER1

static inline void
rb_ary_modify_check(ary)
    VALUE ary;
{
    if (OBJ_FROZEN(ary)) rb_error_frozen("array");
    if (FL_TEST(ary, ARY_TMPLOCK))
	rb_raise(rb_eRuntimeError, "can't modify array during iteration");
    if (!OBJ_TAINTED(ary) && rb_safe_level() >= 4)
	rb_raise(rb_eSecurityError, "Insecure: can't modify array");
}

static void
rb_ary_modify(ary)
    VALUE ary;
{
    VALUE *ptr;

    rb_ary_modify_check(ary);
    if (FL_TEST(ary, ELTS_SHARED)) {
	ptr = ALLOC_N(VALUE, RARRAY(ary)->len);
	FL_UNSET(ary, ELTS_SHARED);
	RARRAY(ary)->aux.capa = RARRAY(ary)->len;
	MEMCPY(ptr, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);
	RARRAY(ary)->ptr = ptr;
    }
}

VALUE
rb_ary_freeze(ary)
    VALUE ary;
{
    return rb_obj_freeze(ary);
}

/*
 *  call-seq:
 *     array.frozen?  -> true or false
 *
 *  Return <code>true</code> if this array is frozen (or temporarily frozen
 *  while being sorted).
 */

static VALUE
rb_ary_frozen_p(ary)
    VALUE ary;
{
    if (OBJ_FROZEN(ary)) return Qtrue;
    if (FL_TEST(ary, ARY_TMPLOCK)) return Qtrue;
    return Qfalse;
}

static VALUE ary_alloc(VALUE);
static VALUE
ary_alloc(klass)
    VALUE klass;
{
    NEWOBJ(ary, struct RArray);
    OBJSETUP(ary, klass, T_ARRAY);

    ary->len = 0;
    ary->ptr = 0;
    ary->aux.capa = 0;

    return (VALUE)ary;
}

static VALUE
ary_new(klass, len)
    VALUE klass;
    long len;
{
    VALUE ary;

    if (len < 0) {
	rb_raise(rb_eArgError, "negative array size (or size too big)");
    }
    if (len > 0 && len * sizeof(VALUE) <= len) {
	rb_raise(rb_eArgError, "array size too big");
    }
    if (len == 0) len++;
    
    ary = ary_alloc(klass);
    RARRAY(ary)->ptr = ALLOC_N(VALUE, len);
    RARRAY(ary)->aux.capa = len;

    return ary;
}

VALUE
rb_ary_new2(len)
    long len;
{
    return ary_new(rb_cArray, len);
}


VALUE
rb_ary_new()
{
    return rb_ary_new2(ARY_DEFAULT_SIZE);
}

#ifdef HAVE_STDARG_PROTOTYPES
#include <stdarg.h>
#define va_init_list(a,b) va_start(a,b)
#else
#include <varargs.h>
#define va_init_list(a,b) va_start(a)
#endif

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_ary_new3(long n, ...)
#else
rb_ary_new3(n, va_alist)
    long n;
    va_dcl
#endif
{
    va_list ar;
    VALUE ary;
    long i;

    ary = rb_ary_new2(n);

    va_init_list(ar, n);
    for (i=0; i<n; i++) {
	RARRAY(ary)->ptr[i] = va_arg(ar, VALUE);
    }
    va_end(ar);

    RARRAY(ary)->len = n;
    return ary;
}

VALUE
rb_ary_new4(n, elts)
    long n;
    const VALUE *elts;
{
    VALUE ary;

    ary = rb_ary_new2(n);
    if (n > 0 && elts) {
	MEMCPY(RARRAY(ary)->ptr, elts, VALUE, n);
    }
    RARRAY(ary)->len = n;

    return ary;
}

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_values_new(long n, ...)
#else
rb_values_new(n, va_alist)
    long n;
    va_dcl
#endif
{
    va_list ar;
    VALUE val;
    long i;

    val = ary_new(rb_cValues, n);
    va_init_list(ar, n);
    for (i=0; i<n; i++) {
	RARRAY(val)->ptr[i] = va_arg(ar, VALUE);
    }
    va_end(ar);
    RARRAY(val)->len = n;

    return val;
}

VALUE
rb_values_new2(n, elts)
    long n;
    const VALUE *elts;
{
    VALUE val;

    val = ary_new(rb_cValues, n);
    if (n > 0 && elts) {
	RARRAY(val)->len = n;
	MEMCPY(RARRAY(val)->ptr, elts, VALUE, n);
    }

    return val;
}

static VALUE
ary_make_shared(ary)
    VALUE ary;
{
    if (!FL_TEST(ary, ELTS_SHARED)) {
	NEWOBJ(shared, struct RArray);
	OBJSETUP(shared, rb_cArray, T_ARRAY);

	shared->len = RARRAY(ary)->len;
	shared->ptr = RARRAY(ary)->ptr;
	shared->aux.capa = RARRAY(ary)->aux.capa;
	RARRAY(ary)->aux.shared = (VALUE)shared;
	FL_SET(ary, ELTS_SHARED);
	OBJ_FREEZE(shared);
	return (VALUE)shared;
    }
    else {
	return RARRAY(ary)->aux.shared;
    }
}

static VALUE
ary_shared_array(klass, ary)
    VALUE klass, ary;
{
    VALUE val = ary_alloc(klass);

    ary_make_shared(ary);
    RARRAY(val)->ptr = RARRAY(ary)->ptr;
    RARRAY(val)->len = RARRAY(ary)->len;
    RARRAY(val)->aux.shared = RARRAY(ary)->aux.shared;
    FL_SET(val, ELTS_SHARED);
    return val;
}

VALUE
rb_values_from_ary(ary)
    VALUE ary;
{
    return ary_shared_array(rb_cValues, ary);
}

VALUE
rb_ary_from_values(val)
    VALUE val;
{
    return ary_shared_array(rb_cArray, val);
}

VALUE
rb_assoc_new(car, cdr)
    VALUE car, cdr;
{
    return rb_values_new(2, car, cdr);
}

static VALUE
to_ary(ary)
    VALUE ary;
{
    return rb_convert_type(ary, T_ARRAY, "Array", "to_ary");
}

static VALUE
to_a(ary)
    VALUE ary;
{
    return rb_convert_type(ary, T_ARRAY, "Array", "to_a");
}

VALUE
rb_check_array_type(ary)
    VALUE ary;
{
    return rb_check_convert_type(ary, T_ARRAY, "Array", "to_ary");
}

static VALUE rb_ary_replace _((VALUE, VALUE));

/*
 *  call-seq:
 *     Array.new(size=0, obj=nil)
 *     Array.new(array)
 *     Array.new(size) {|index| block }
 *
 *  Returns a new array. In the first form, the new array is
 *  empty. In the second it is created with _size_ copies of _obj_
 *  (that is, _size_ references to the same
 *  _obj_). The third form creates a copy of the array
 *  passed as a parameter (the array is generated by calling
 *  to_ary  on the parameter). In the last form, an array
 *  of the given size is created. Each element in this array is
 *  calculated by passing the element's index to the given block and
 *  storing the return value.
 *
 *     Array.new
 *     Array.new(2)
 *     Array.new(5, "A")
 * 
 *     # only one copy of the object is created
 *     a = Array.new(2, Hash.new)
 *     a[0]['cat'] = 'feline'
 *     a
 *     a[1]['cat'] = 'Felix'
 *     a
 * 
 *     # here multiple copies are created
 *     a = Array.new(2) { Hash.new }
 *     a[0]['cat'] = 'feline'
 *     a
 * 
 *     squares = Array.new(5) {|i| i*i}
 *     squares
 * 
 *     copy = Array.new(squares)
 */

static VALUE
rb_ary_initialize(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long len;
    VALUE size, val;

    if (rb_scan_args(argc, argv, "02", &size, &val) == 0) {
	RARRAY(ary)->len = 0;
	if (rb_block_given_p()) {
	    rb_warning("given block not used");
	}
	return ary;
    }

    if (argc == 1 && !FIXNUM_P(size)) {
	val = rb_check_array_type(size);
	if (!NIL_P(val)) {
	    rb_ary_replace(ary, val);
	    return ary;
	}
    }

    len = NUM2LONG(size);
    if (len < 0) {
	rb_raise(rb_eArgError, "negative array size");
    }
    if (len > 0 && len * (long)sizeof(VALUE) <= len) {
	rb_raise(rb_eArgError, "array size too big");
    }
    rb_ary_modify(ary);
    if (len > RARRAY(ary)->aux.capa) {
	REALLOC_N(RARRAY(ary)->ptr, VALUE, len);
	RARRAY(ary)->aux.capa = len;
    }
    if (rb_block_given_p()) {
	long i;

	if (argc == 2) {
	    rb_warn("block supersedes default value argument");
	}
	for (i=0; i<len; i++) {
	    rb_ary_store(ary, i, rb_yield(LONG2NUM(i)));
	    RARRAY(ary)->len = i + 1;
	}
    }
    else {
	memfill(RARRAY(ary)->ptr, len, val);
	RARRAY(ary)->len = len;
    }

    return ary;
}


/* 
* Returns a new array populated with the given objects. 
*
*   Array.[]( 1, 'a', /^A/ )
*   Array[ 1, 'a', /^A/ ]
*   [ 1, 'a', /^A/ ]
*/

static VALUE
rb_ary_s_create(argc, argv, klass)
    int argc;
    VALUE *argv;
    VALUE klass;
{
    VALUE ary = ary_alloc(klass);

    if (argc > 0) {
	RARRAY(ary)->ptr = ALLOC_N(VALUE, argc);
	MEMCPY(RARRAY(ary)->ptr, argv, VALUE, argc);
    }
    RARRAY(ary)->len = RARRAY(ary)->aux.capa = argc;

    return ary;
}

void
rb_ary_store(ary, idx, val)
    VALUE ary;
    long idx;
    VALUE val;
{
    if (idx < 0) {
	idx += RARRAY(ary)->len;
	if (idx < 0) {
	    rb_raise(rb_eIndexError, "index %ld out of array",
		    idx - RARRAY(ary)->len);
	}
    }

    rb_ary_modify(ary);
    if (idx >= RARRAY(ary)->aux.capa) {
	long new_capa = RARRAY(ary)->aux.capa / 2;

	if (new_capa < ARY_DEFAULT_SIZE) {
	    new_capa = ARY_DEFAULT_SIZE;
	}
	new_capa += idx;
	if (new_capa * (long)sizeof(VALUE) <= new_capa) {
	    rb_raise(rb_eArgError, "index too big");
	}
	REALLOC_N(RARRAY(ary)->ptr, VALUE, new_capa);
	RARRAY(ary)->aux.capa = new_capa;
    }
    if (idx > RARRAY(ary)->len) {
	rb_mem_clear(RARRAY(ary)->ptr + RARRAY(ary)->len,
		     idx-RARRAY(ary)->len + 1);
    }

    if (idx >= RARRAY(ary)->len) {
	RARRAY(ary)->len = idx + 1;
    }
    RARRAY(ary)->ptr[idx] = val;
}

static VALUE
ary_shared_first(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE nv, result;
    long n;

    rb_scan_args(argc, argv, "1", &nv);
    n = NUM2LONG(nv);
    if (n > RARRAY(ary)->len) {
	n = RARRAY(ary)->len;
    }
    else if (n < 0) {
	rb_raise(rb_eArgError, "negative array size");
    }
    result = ary_shared_array(rb_cArray, ary);
    RARRAY(result)->len = n;
    return result;
}

static VALUE
ary_shared_last(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE result = ary_shared_first(argc, argv, ary);

    RARRAY(result)->ptr += RARRAY(ary)->len - RARRAY(result)->len;
    return result;
}

/*
 *  call-seq:
 *     array << obj            -> array
 *  
 *  Append---Pushes the given object on to the end of this array. This
 *  expression returns the array itself, so several appends
 *  may be chained together.
 *
 *     [ 1, 2 ] << "c" << "d" << [ 3, 4 ]
 *             #=>  [ 1, 2, "c", "d", [ 3, 4 ] ]
 *
 */

VALUE
rb_ary_push(ary, item)
    VALUE ary;
    VALUE item;
{
    rb_ary_store(ary, RARRAY(ary)->len, item);
    return ary;
}

/* 
 *  call-seq:
 *     array.push(obj, ... )   -> array
 *  
 *  Append---Pushes the given object(s) on to the end of this array. This
 *  expression returns the array itself, so several appends
 *  may be chained together.
 *
 *     a = [ "a", "b", "c" ]
 *     a.push("d", "e", "f")  
 *             #=> ["a", "b", "c", "d", "e", "f"]
 */

static VALUE
rb_ary_push_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    while (argc--) {
	rb_ary_push(ary, *argv++);
    }
    return ary;
}

VALUE
rb_ary_pop(ary)
    VALUE ary;
{
    rb_ary_modify_check(ary);
    if (RARRAY(ary)->len == 0) return Qnil;
    if (!FL_TEST(ary, ELTS_SHARED) &&
	    RARRAY(ary)->len * 2 < RARRAY(ary)->aux.capa &&
	    RARRAY(ary)->aux.capa > ARY_DEFAULT_SIZE) {
	RARRAY(ary)->aux.capa = RARRAY(ary)->len * 2;
	REALLOC_N(RARRAY(ary)->ptr, VALUE, RARRAY(ary)->aux.capa);
    }
    return RARRAY(ary)->ptr[--RARRAY(ary)->len];
}

/*
 *  call-seq:
 *     array.pop  -> obj or nil
 *  
 *  Removes the last element from <i>self</i> and returns it, or
 *  <code>nil</code> if the array is empty.
 *     
 *     a = [ "a", "b", "c", "d" ]
 *     a.pop     #=> "d"
 *     a.pop(2)  #=> ["b", "c"]
 *     a         #=> ["a"]
 */

static VALUE
rb_ary_pop_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE result;

    if (argc == 0) {
	return rb_ary_pop(ary);
    }

    rb_ary_modify_check(ary);

    result = ary_shared_last(argc, argv, ary);
    RARRAY(ary)->len -= RARRAY(result)->len;
    return result;
}

VALUE
rb_ary_shift(ary)
    VALUE ary;
{
    VALUE top;

    rb_ary_modify_check(ary);
    if (RARRAY(ary)->len == 0) return Qnil;
    top = RARRAY(ary)->ptr[0];
    ary_make_shared(ary);
    RARRAY(ary)->ptr++;		/* shift ptr */
    RARRAY(ary)->len--;

    return top;
}

/*
 *  call-seq:
 *     array.shift   ->   obj or nil
 *  
 *  Returns the first element of <i>self</i> and removes it (shifting all
 *  other elements down by one). Returns <code>nil</code> if the array
 *  is empty.
 *     
 *     args = [ "-m", "-q", "filename" ]
 *     args.shift     #=> "-m"
 *     args           #=> ["-q", "filename"]
 *
 *     args = [ "-m", "-q", "filename" ]
 *     args.shift(2)  #=> ["-m", "-q"]
 *     args           #=> ["filename"]
 */

static VALUE
rb_ary_shift_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE result;
    long n;

    if (argc == 0) {
	return rb_ary_shift(ary);
    }

    rb_ary_modify_check(ary);

    result = ary_shared_first(argc, argv, ary);
    n = RARRAY(result)->len;
    RARRAY(ary)->ptr += n;
    RARRAY(ary)->len -= n;

    return result;
}

VALUE
rb_ary_unshift(ary, item)
    VALUE ary, item;
{
    rb_ary_modify(ary);
    if (RARRAY(ary)->len == RARRAY(ary)->aux.capa) {
	long capa_inc = RARRAY(ary)->aux.capa / 2;
	if (capa_inc < ARY_DEFAULT_SIZE) {
	    capa_inc = ARY_DEFAULT_SIZE;
	}
	RARRAY(ary)->aux.capa += capa_inc;
	REALLOC_N(RARRAY(ary)->ptr, VALUE, RARRAY(ary)->aux.capa);
    }

    /* sliding items */
    MEMMOVE(RARRAY(ary)->ptr + 1, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);

    RARRAY(ary)->len++;
    RARRAY(ary)->ptr[0] = item;

    return ary;
}

/*
 *  call-seq:
 *     array.unshift(obj, ...)  -> array
 *  
 *  Prepends objects to the front of <i>array</i>.
 *  other elements up one.
 *     
 *     a = [ "b", "c", "d" ]
 *     a.unshift("a")   #=> ["a", "b", "c", "d"]
 *     a.unshift(1, 2)  #=> [ 1, 2, "a", "b", "c", "d"]
 */

static VALUE
rb_ary_unshift_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long len = RARRAY(ary)->len;

    if (argc == 0) return ary;

    /* make rooms by setting the last item */
    rb_ary_store(ary, len + argc - 1, Qnil);

    /* sliding items */
    MEMMOVE(RARRAY(ary)->ptr + argc, RARRAY(ary)->ptr, VALUE, len);
    MEMCPY(RARRAY(ary)->ptr, argv, VALUE, argc);
    
    return ary;
}

/* faster version - use this if you don't need to treat negative offset */
static inline VALUE
rb_ary_elt(ary, offset)
    VALUE ary;
    long offset;
{
    if (RARRAY(ary)->len == 0) return Qnil;
    if (offset < 0 || RARRAY(ary)->len <= offset) {
	return Qnil;
    }
    return RARRAY(ary)->ptr[offset];
}

VALUE
rb_ary_entry(ary, offset)
    VALUE ary;
    long offset;
{
    if (offset < 0) {
	offset += RARRAY(ary)->len;
    }
    return rb_ary_elt(ary, offset);
}

static VALUE
rb_ary_subseq(ary, beg, len)
    VALUE ary;
    long beg, len;
{
    VALUE klass, ary2, shared;
    VALUE *ptr;

    if (beg > RARRAY(ary)->len) return Qnil;
    if (beg < 0 || len < 0) return Qnil;

    if (beg + len > RARRAY(ary)->len) {
	len = RARRAY(ary)->len - beg;
	if (len < 0)
	    len = 0;
    }
    klass = rb_obj_class(ary);
    if (len == 0) return ary_new(klass, 0);

    shared = ary_make_shared(ary);
    ptr = RARRAY(ary)->ptr;
    ary2 = ary_alloc(klass);
    RARRAY(ary2)->ptr = ptr + beg;
    RARRAY(ary2)->len = len;
    RARRAY(ary2)->aux.shared = shared;
    FL_SET(ary2, ELTS_SHARED);

    return ary2;
}

/* 
 *  call-seq:
 *     array[index]                -> obj      or nil
 *     array[start, length]        -> an_array or nil
 *     array[range]                -> an_array or nil
 *     array.slice(index)          -> obj      or nil
 *     array.slice(start, length)  -> an_array or nil
 *     array.slice(range)          -> an_array or nil
 *
 *  Element Reference---Returns the element at _index_,
 *  or returns a subarray starting at _start_ and
 *  continuing for _length_ elements, or returns a subarray
 *  specified by _range_.
 *  Negative indices count backward from the end of the
 *  array (-1 is the last element). Returns nil if the index
 *  (or starting index) are out of range.
 *
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a[2] +  a[0] + a[1]    #=> "cab"
 *     a[6]                   #=> nil
 *     a[1, 2]                #=> [ "b", "c" ]
 *     a[1..3]                #=> [ "b", "c", "d" ]
 *     a[4..7]                #=> [ "e" ]
 *     a[6..10]               #=> nil
 *     a[-3, 3]               #=> [ "c", "d", "e" ]
 *     # special cases
 *     a[5]                   #=> nil
 *     a[5, 1]                #=> []
 *     a[5..10]               #=> []
 *
 */

VALUE
rb_ary_aref(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE arg;
    long beg, len;

    if (argc == 2) {
	beg = NUM2LONG(argv[0]);
	len = NUM2LONG(argv[1]);
	if (beg < 0) {
	    beg += RARRAY(ary)->len;
	}
	return rb_ary_subseq(ary, beg, len);
    }
    if (argc != 1) {
	rb_scan_args(argc, argv, "11", 0, 0);
    }
    arg = argv[0];
    /* special case - speeding up */
    if (FIXNUM_P(arg)) {
	return rb_ary_entry(ary, FIX2LONG(arg));
    }
    /* check if idx is Range */
    switch (rb_range_beg_len(arg, &beg, &len, RARRAY(ary)->len, 0)) {
      case Qfalse:
	break;
      case Qnil:
	return Qnil;
      default:
	return rb_ary_subseq(ary, beg, len);
    }
    return rb_ary_entry(ary, NUM2LONG(arg));
}

/* 
 *  call-seq:
 *     array.at(index)   ->   obj  or nil
 *
 *  Returns the element at _index_. A
 *  negative index counts from the end of _self_.  Returns +nil+
 *  if the index is out of range. See also <code>Array#[]</code>.
 *  (<code>Array#at</code> is slightly faster than <code>Array#[]</code>,
 *  as it does not accept ranges and so on.)
 *
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a.at(0)     #=> "a"
 *     a.at(-1)    #=> "e"
 */

static VALUE
rb_ary_at(ary, pos)
    VALUE ary, pos;
{
    return rb_ary_entry(ary, NUM2LONG(pos));
}

/*
 *  call-seq:
 *     array.first     ->   obj or nil
 *     array.first(n)  ->   an_array
 *  
 *  Returns the first element of the array. If the array is empty,
 *  returns <code>nil</code>.
 *     
 *     a = [ "q", "r", "s", "t" ]
 *     a.first     #=> "q"
 *     a.first(2)  #=> ["q", "r"]
 */

static VALUE
rb_ary_first(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    if (argc == 0) {
	if (RARRAY(ary)->len == 0) return Qnil;
	return RARRAY(ary)->ptr[0];
    }
    else {
	return ary_shared_first(argc, argv, ary);
    }
}

/*
 *  call-seq:
 *     array.last     ->  obj or nil
 *     array.last(n)  ->  an_array
 *  
 *  Returns the last element(s) of <i>self</i>. If the array is empty,
 *  the first form returns <code>nil</code>.
 *     
 *     a = [ "w", "x", "y", "z" ]
 *     a.last     #=> "z"
 *     a.last(2)  #=> ["y", "z"]
 */

static VALUE
rb_ary_last(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    if (argc == 0) {
	if (RARRAY(ary)->len == 0) return Qnil;
	return RARRAY(ary)->ptr[RARRAY(ary)->len-1];
    }
    else {
	return ary_shared_last(argc, argv, ary);
    }
}

/*
 *  call-seq:
 *     array.fetch(index)                    -> obj
 *     array.fetch(index, default )          -> obj
 *     array.fetch(index) {|index| block }   -> obj
 *  
 *  Tries to return the element at position <i>index</i>. If the index
 *  lies outside the array, the first form throws an
 *  <code>IndexError</code> exception, the second form returns
 *  <i>default</i>, and the third form returns the value of invoking
 *  the block, passing in the index. Negative values of <i>index</i>
 *  count from the end of the array.
 *     
 *     a = [ 11, 22, 33, 44 ]
 *     a.fetch(1)               #=> 22
 *     a.fetch(-1)              #=> 44
 *     a.fetch(4, 'cat')        #=> "cat"
 *     a.fetch(4) { |i| i*i }   #=> 16
 */

static VALUE
rb_ary_fetch(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE pos, ifnone;
    long block_given;
    long idx;

    rb_scan_args(argc, argv, "11", &pos, &ifnone);
    block_given = rb_block_given_p();
    if (block_given && argc == 2) {
	rb_warn("block supersedes default value argument");
    }
    idx = NUM2LONG(pos);

    if (idx < 0) {
	idx +=  RARRAY(ary)->len;
    }
    if (idx < 0 || RARRAY(ary)->len <= idx) {
	if (block_given) return rb_yield(pos);
	if (argc == 1) {
	    rb_raise(rb_eIndexError, "index %ld out of array", idx);
	}
	return ifnone;
    }
    return RARRAY(ary)->ptr[idx];
}

/*
 *  call-seq:
 *     array.index(obj)           ->  int or nil
 *     array.index {|item| block} ->  int or nil
 *  
 *  Returns the index of the first object in <i>self</i> such that is
 *  <code>==</code> to <i>obj</i>. If a block is given instead of an
 *  argument, returns first object for which <em>block</em> is true.
 *  Returns <code>nil</code> if no match is found.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.index("b")        #=> 1
 *     a.index("z")        #=> nil
 *     a.index{|x|x=="b"}  #=> 1
 */

static VALUE
rb_ary_index(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE val;
    long i;

    if (rb_scan_args(argc, argv, "01", &val) == 0) {
	for (i=0; i<RARRAY(ary)->len; i++) {
	    if (RTEST(rb_yield(RARRAY(ary)->ptr[i]))) {
		return LONG2NUM(i);
	    }
	}
    }
    else {
	for (i=0; i<RARRAY(ary)->len; i++) {
	    if (rb_equal(RARRAY(ary)->ptr[i], val))
		return LONG2NUM(i);
	}
    }
    return Qnil;
}

/*
 *  call-seq:
 *     array.rindex(obj)    ->  int or nil
 *  
 *  Returns the index of the last object in <i>array</i>
 *  <code>==</code> to <i>obj</i>. If a block is given instead of an
 *  argument, returns first object for which <em>block</em> is
 *  true. Returns <code>nil</code> if no match is found.
 *     
 *     a = [ "a", "b", "b", "b", "c" ]
 *     a.rindex("b")        #=> 3
 *     a.rindex("z")        #=> nil
 *     a.rindex{|x|x=="b"}  #=> 3
 */

static VALUE
rb_ary_rindex(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE val;
    long i = RARRAY(ary)->len;

    if (rb_scan_args(argc, argv, "01", &val) == 0) {
	while (i--) {
	    if (RTEST(rb_yield(RARRAY(ary)->ptr[i])))
		return LONG2NUM(i);
	    if (i > RARRAY(ary)->len) {
		i = RARRAY(ary)->len;
	    }
	}
    }
    else {
	while (i--) {
	    if (rb_equal(RARRAY(ary)->ptr[i], val))
		return LONG2NUM(i);
	    if (i > RARRAY(ary)->len) {
		i = RARRAY(ary)->len;
	    }
	}
    }
    return Qnil;
}

VALUE
rb_ary_to_ary(obj)
    VALUE obj;
{
    if (TYPE(obj) == T_ARRAY) {
	return obj;
    }
    if (rb_respond_to(obj, rb_intern("to_ary"))) {
	return to_ary(obj);
    }
    return rb_ary_new3(1, obj);
}

static void
rb_ary_splice(ary, beg, len, rpl)
    VALUE ary;
    long beg, len;
    VALUE rpl;
{
    long rlen;

    if (len < 0) rb_raise(rb_eIndexError, "negative length (%ld)", len);
    if (beg < 0) {
	beg += RARRAY(ary)->len;
	if (beg < 0) {
	    beg -= RARRAY(ary)->len;
	    rb_raise(rb_eIndexError, "index %ld out of array", beg);
	}
    }
    if (beg + len > RARRAY(ary)->len) {
	len = RARRAY(ary)->len - beg;
    }

    if (rpl == Qundef) {
	rlen = 0;
    }
    else {
	rpl = rb_ary_to_ary(rpl);
	rlen = RARRAY(rpl)->len;
    }
    rb_ary_modify(ary);

    if (beg >= RARRAY(ary)->len) {
	len = beg + rlen;
	if (len >= RARRAY(ary)->aux.capa) {
	    REALLOC_N(RARRAY(ary)->ptr, VALUE, len);
	    RARRAY(ary)->aux.capa = len;
	}
	rb_mem_clear(RARRAY(ary)->ptr + RARRAY(ary)->len, beg - RARRAY(ary)->len);
	if (rlen > 0) {
	    MEMCPY(RARRAY(ary)->ptr + beg, RARRAY(rpl)->ptr, VALUE, rlen);
	}
	RARRAY(ary)->len = len;
    }
    else {
	long alen;

	if (beg + len > RARRAY(ary)->len) {
	    len = RARRAY(ary)->len - beg;
	}

	alen = RARRAY(ary)->len + rlen - len;
	if (alen >= RARRAY(ary)->aux.capa) {
	    REALLOC_N(RARRAY(ary)->ptr, VALUE, alen);
	    RARRAY(ary)->aux.capa = alen;
	}

	if (len != rlen) {
	    MEMMOVE(RARRAY(ary)->ptr + beg + rlen, RARRAY(ary)->ptr + beg + len,
		    VALUE, RARRAY(ary)->len - (beg + len));
	    RARRAY(ary)->len = alen;
	}
	if (rlen > 0) {
	    MEMMOVE(RARRAY(ary)->ptr + beg, RARRAY(rpl)->ptr, VALUE, rlen);
	}
    }
}

/* 
 *  call-seq:
 *     array[index]         = obj                     ->  obj
 *     array[start, length] = obj or an_array or nil  ->  obj or an_array or nil
 *     array[range]         = obj or an_array or nil  ->  obj or an_array or nil
 *
 *  Element Assignment---Sets the element at _index_,
 *  or replaces a subarray starting at _start_ and
 *  continuing for _length_ elements, or replaces a subarray
 *  specified by _range_.  If indices are greater than
 *  the current capacity of the array, the array grows
 *  automatically. A negative indices will count backward
 *  from the end of the array. Inserts elements if _length_ is
 *  zero. An +IndexError+ is raised if a negative index points
 *  past the beginning of the array. See also
 *  <code>Array#push</code>, and <code>Array#unshift</code>.
 * 
 *     a = Array.new
 *     a[4] = "4";                 #=> [nil, nil, nil, nil, "4"]
 *     a[0, 3] = [ 'a', 'b', 'c' ] #=> ["a", "b", "c", nil, "4"]
 *     a[1..2] = [ 1, 2 ]          #=> ["a", 1, 2, nil, "4"]
 *     a[0, 2] = "?"               #=> ["?", 2, nil, "4"]
 *     a[0..2] = "A"               #=> ["A", "4"]
 *     a[-1]   = "Z"               #=> ["A", "Z"]
 *     a[1..-1] = nil              #=> ["A", nil]
 *     a[1..-1] = []              #=> ["A"]
 */

static VALUE
rb_ary_aset(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long offset, beg, len;

    if (argc == 3) {
	rb_ary_splice(ary, NUM2LONG(argv[0]), NUM2LONG(argv[1]), argv[2]);
	return argv[2];
    }
    if (argc != 2) {
	rb_raise(rb_eArgError, "wrong number of arguments (%d for 2)", argc);
    }
    if (FIXNUM_P(argv[0])) {
	offset = FIX2LONG(argv[0]);
	goto fixnum;
    }
    if (rb_range_beg_len(argv[0], &beg, &len, RARRAY(ary)->len, 1)) {
	/* check if idx is Range */
	rb_ary_splice(ary, beg, len, argv[1]);
	return argv[1];
    }

    offset = NUM2LONG(argv[0]);
fixnum:
    rb_ary_store(ary, offset, argv[1]);
    return argv[1];
}

/*
 *  call-seq:
 *     array.insert(index, obj...)  -> array
 *  
 *  Inserts the given values before the element with the given index
 *  (which may be negative).
 *     
 *     a = %w{ a b c d }
 *     a.insert(2, 99)         #=> ["a", "b", 99, "c", "d"]
 *     a.insert(-2, 1, 2, 3)   #=> ["a", "b", 99, "c", 1, 2, 3, "d"]
 */

static VALUE
rb_ary_insert(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long pos;

    if (argc < 1) {
	rb_raise(rb_eArgError, "wrong number of arguments (at least 1)");
    }
    pos = NUM2LONG(argv[0]);
    if (pos == -1) {
	pos = RARRAY(ary)->len;
    }
    else if (pos < 0) {
	pos++;
    }

    if (argc == 1) return ary;
    rb_ary_splice(ary, pos, 0, rb_ary_new4(argc - 1, argv + 1));
    return ary;
}

/*
 *  call-seq:
 *     array.each {|item| block }   ->   array
 *  
 *  Calls <i>block</i> once for each element in <i>self</i>, passing that
 *  element as a parameter.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.each {|x| print x, " -- " }
 *     
 *  produces:
 *     
 *     a -- b -- c --
 */

VALUE
rb_ary_each(ary)
    VALUE ary;
{
    long i;

    for (i=0; i<RARRAY(ary)->len; i++) {
	rb_yield(RARRAY(ary)->ptr[i]);
    }
    return ary;
}

/*
 *  call-seq:
 *     array.each_index {|index| block }  ->  array
 *  
 *  Same as <code>Array#each</code>, but passes the index of the element
 *  instead of the element itself.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.each_index {|x| print x, " -- " }
 *     
 *  produces:
 *     
 *     0 -- 1 -- 2 --
 */

static VALUE
rb_ary_each_index(ary)
    VALUE ary;
{
    long i;

    for (i=0; i<RARRAY(ary)->len; i++) {
	rb_yield(LONG2NUM(i));
    }
    return ary;
}

/*
 *  call-seq:
 *     array.reverse_each {|item| block } 
 *  
 *  Same as <code>Array#each</code>, but traverses <i>self</i> in reverse
 *  order.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.reverse_each {|x| print x, " " }
 *     
 *  produces:
 *     
 *     c b a
 */

static VALUE
rb_ary_reverse_each(ary)
    VALUE ary;
{
    long len = RARRAY(ary)->len;

    while (len--) {
	rb_yield(RARRAY(ary)->ptr[len]);
	if (RARRAY(ary)->len < len) {
	    len = RARRAY(ary)->len;
	}
    }
    return ary;
}

/*
 *  call-seq:
 *     array.length -> int
 *  
 *  Returns the number of elements in <i>self</i>. May be zero.
 *     
 *     [ 1, 2, 3, 4, 5 ].length   #=> 5
 */

static VALUE
rb_ary_length(ary)
    VALUE ary;
{
    return LONG2NUM(RARRAY(ary)->len);
}

/*
 *  call-seq:
 *     array.empty?   -> true or false
 *  
 *  Returns <code>true</code> if <i>self</i> array contains no elements.
 *     
 *     [].empty?   #=> true
 */

static VALUE
rb_ary_empty_p(ary)
    VALUE ary;
{
    if (RARRAY(ary)->len == 0)
	return Qtrue;
    return Qfalse;
}

VALUE
rb_ary_dup(ary)
    VALUE ary;
{
    VALUE dup = rb_ary_new2(RARRAY(ary)->len);

    DUPSETUP(dup, ary);
    MEMCPY(RARRAY(dup)->ptr, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);
    RARRAY(dup)->len = RARRAY(ary)->len;
    return dup;
}

extern VALUE rb_output_fs;

static VALUE
recursive_join(ary, arg, recur)
    VALUE ary;
    VALUE *arg;
    int recur;
{
    if (recur) {
	return rb_str_new2("[...]");
    }
    return rb_ary_join(arg[0], arg[1]);
}

VALUE
rb_ary_join(ary, sep)
    VALUE ary, sep;
{
    long len = 1, i;
    int taint = Qfalse;
    VALUE result, tmp;

    if (RARRAY(ary)->len == 0) return rb_str_new(0, 0);
    if (OBJ_TAINTED(ary) || OBJ_TAINTED(sep)) taint = Qtrue;

    for (i=0; i<RARRAY(ary)->len; i++) {
	tmp = rb_check_string_type(RARRAY(ary)->ptr[i]);
	len += NIL_P(tmp) ? 10 : RSTRING(tmp)->len;
    }
    if (!NIL_P(sep)) {
	StringValue(sep);
	len += RSTRING(sep)->len * (RARRAY(ary)->len - 1);
    }
    result = rb_str_buf_new(len);
    for (i=0; i<RARRAY(ary)->len; i++) {
	tmp = RARRAY(ary)->ptr[i];
	switch (TYPE(tmp)) {
	  case T_STRING:
	    break;
	  case T_ARRAY:
	    {
		VALUE args[2];

		args[0] = tmp;
		args[1] = sep;
		tmp = rb_exec_recursive(recursive_join, ary, (VALUE)args);
	    }
	    break;
	  default:
	    tmp = rb_obj_as_string(tmp);
	}
	if (i > 0 && !NIL_P(sep))
	    rb_str_buf_append(result, sep);
	rb_str_buf_append(result, tmp);
	if (OBJ_TAINTED(tmp)) taint = Qtrue;
    }

    if (taint) OBJ_TAINT(result);
    return result;
}

/*
 *  call-seq:
 *     array.join(sep=$,)    -> str
 *  
 *  Returns a string created by converting each element of the array to
 *  a string, separated by <i>sep</i>.
 *     
 *     [ "a", "b", "c" ].join        #=> "abc"
 *     [ "a", "b", "c" ].join("-")   #=> "a-b-c"
 */

static VALUE
rb_ary_join_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE sep;

    rb_scan_args(argc, argv, "01", &sep);
    if (NIL_P(sep)) sep = rb_output_fs;
    
    return rb_ary_join(ary, sep);
}

/*
 *  call-seq:
 *     array.to_s -> string
 *  
 *  Returns _self_<code>.join</code>.
 *     
 *     [ "a", "e", "i", "o" ].to_s   #=> "aeio"
 *
 */

VALUE
rb_ary_to_s(ary)
    VALUE ary;
{
    if (RARRAY(ary)->len == 0) return rb_str_new(0, 0);
    
    return rb_ary_join(ary, rb_output_fs);
}

static VALUE
inspect_ary(ary, dummy, recur)
    VALUE ary;
    VALUE dummy;
    int recur;
{
    int tainted = OBJ_TAINTED(ary);
    long i;
    VALUE s, str;

    if (recur) return rb_tainted_str_new2("[...]");
    str = rb_str_buf_new2("[");
    for (i=0; i<RARRAY(ary)->len; i++) {
	s = rb_inspect(RARRAY(ary)->ptr[i]);
	if (OBJ_TAINTED(s)) tainted = Qtrue;
	if (i > 0) rb_str_buf_cat2(str, ", ");
	rb_str_buf_append(str, s);
    }
    rb_str_buf_cat2(str, "]");
    if (tainted) OBJ_TAINT(str);
    return str;
}

/*
 *  call-seq:
 *     array.inspect  -> string
 *
 *  Create a printable version of <i>array</i>.
 */

static VALUE
rb_ary_inspect(ary)
    VALUE ary;
{
    if (RARRAY(ary)->len == 0) return rb_str_new2("[]");
    return rb_exec_recursive(inspect_ary, ary, 0);
}

/*
 *  call-seq:
 *     array.to_a     -> array
 *  
 *  Returns _self_. If called on a subclass of Array, converts
 *  the receiver to an Array object.
 */

static VALUE
rb_ary_to_a(ary)
    VALUE ary;
{
    if (rb_obj_class(ary) != rb_cArray) {
	VALUE dup = rb_ary_new2(RARRAY(ary)->len);
	rb_ary_replace(dup, ary);
	return dup;
    }
    return ary;
}

/*
 *  call-seq:
 *     array.to_ary -> array
 *  
 *  Returns _self_.
 */

static VALUE
rb_ary_to_ary_m(ary)
    VALUE ary;
{
    return ary;
}

VALUE
rb_ary_reverse(ary)
    VALUE ary;
{
    VALUE *p1, *p2;
    VALUE tmp;

    rb_ary_modify(ary);
    if (RARRAY(ary)->len > 1) {
	p1 = RARRAY(ary)->ptr;
	p2 = p1 + RARRAY(ary)->len - 1;	/* points last item */

	while (p1 < p2) {
	    tmp = *p1;
	    *p1++ = *p2;
	    *p2-- = tmp;
	}
    }
    return ary;
}

/*
 *  call-seq:
 *     array.reverse!   -> array 
 *  
 *  Reverses _self_ in place.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.reverse!       #=> ["c", "b", "a"]
 *     a                #=> ["c", "b", "a"]
 */

static VALUE
rb_ary_reverse_bang(ary)
    VALUE ary;
{
    return rb_ary_reverse(ary);
}

/*
 *  call-seq:
 *     array.reverse -> an_array
 *  
 *  Returns a new array containing <i>self</i>'s elements in reverse order.
 *     
 *     [ "a", "b", "c" ].reverse   #=> ["c", "b", "a"]
 *     [ 1 ].reverse               #=> [1]
 */

static VALUE
rb_ary_reverse_m(ary)
    VALUE ary;
{
    return rb_ary_reverse(rb_ary_dup(ary));
}

struct ary_sort_data {
    VALUE ary;
    VALUE *ptr;
    long len;
};

static void
ary_sort_check(data)
    struct ary_sort_data *data;
{
    if (RARRAY(data->ary)->ptr != data->ptr || RARRAY(data->ary)->len != data->len) {
	rb_raise(rb_eRuntimeError, "array modified during sort");
    }
}

static int
sort_1(a, b, data)
    VALUE *a, *b;
    struct ary_sort_data *data;
{
    VALUE retval = rb_yield_values(2, *a, *b);
    int n;

    n = rb_cmpint(retval, *a, *b);
    ary_sort_check(data);
    return n;
}

static int
sort_2(ap, bp, data)
    VALUE *ap, *bp;
    struct ary_sort_data *data;
{
    VALUE retval;
    VALUE a = *ap, b = *bp;
    int n;

    if (FIXNUM_P(a) && FIXNUM_P(b)) {
	if ((long)a > (long)b) return 1;
	if ((long)a < (long)b) return -1;
	return 0;
    }
    if (TYPE(a) == T_STRING && TYPE(b) == T_STRING) {
	return rb_str_cmp(a, b);
    }

    retval = rb_funcall(a, id_cmp, 1, b);
    n = rb_cmpint(retval, a, b);
    ary_sort_check(data);

    return n;
}

static VALUE
sort_internal(ary)
    VALUE ary;
{
    struct ary_sort_data data;

    data.ary = ary;
    data.ptr = RARRAY(ary)->ptr; data.len = RARRAY(ary)->len;
    qsort(RARRAY(ary)->ptr, RARRAY(ary)->len, sizeof(VALUE),
	  rb_block_given_p()?sort_1:sort_2, &data);
    return ary;
}

static VALUE
sort_unlock(ary)
    VALUE ary;
{
    FL_UNSET(ary, ARY_TMPLOCK);
    return ary;
}

/*
 *  call-seq:
 *     array.sort!                   -> array
 *     array.sort! {| a,b | block }  -> array 
 *  
 *  Sorts _self_. Comparisons for
 *  the sort will be done using the <code><=></code> operator or using
 *  an optional code block. The block implements a comparison between
 *  <i>a</i> and <i>b</i>, returning -1, 0, or +1. See also
 *  <code>Enumerable#sort_by</code>.
 *     
 *     a = [ "d", "a", "e", "c", "b" ]
 *     a.sort                    #=> ["a", "b", "c", "d", "e"]
 *     a.sort {|x,y| y <=> x }   #=> ["e", "d", "c", "b", "a"]
 */

VALUE
rb_ary_sort_bang(ary)
    VALUE ary;
{
    rb_ary_modify(ary);
    if (RARRAY(ary)->len > 1) {
	FL_SET(ary, ARY_TMPLOCK);	/* prohibit modification during sort */
	rb_ensure(sort_internal, ary, sort_unlock, ary);
    }
    return ary;
}

/*
 *  call-seq:
 *     array.sort                   -> an_array 
 *     array.sort {| a,b | block }  -> an_array 
 *  
 *  Returns a new array created by sorting <i>self</i>. Comparisons for
 *  the sort will be done using the <code><=></code> operator or using
 *  an optional code block. The block implements a comparison between
 *  <i>a</i> and <i>b</i>, returning -1, 0, or +1. See also
 *  <code>Enumerable#sort_by</code>.
 *     
 *     a = [ "d", "a", "e", "c", "b" ]
 *     a.sort                    #=> ["a", "b", "c", "d", "e"]
 *     a.sort {|x,y| y <=> x }   #=> ["e", "d", "c", "b", "a"]
 */

VALUE
rb_ary_sort(ary)
    VALUE ary;
{
    ary = rb_ary_dup(ary);
    rb_ary_sort_bang(ary);
    return ary;
}

/*
 *  call-seq:
 *     array.collect {|item| block }  -> an_array
 *     array.map     {|item| block }  -> an_array
 *  
 *  Invokes <i>block</i> once for each element of <i>self</i>. Creates a 
 *  new array containing the values returned by the block.
 *  See also <code>Enumerable#collect</code>.
 *     
 *     a = [ "a", "b", "c", "d" ]
 *     a.collect {|x| x + "!" }   #=> ["a!", "b!", "c!", "d!"]
 *     a                          #=> ["a", "b", "c", "d"]
 */

static VALUE
rb_ary_collect(ary)
    VALUE ary;
{
    long i;
    VALUE collect;

    if (!rb_block_given_p()) {
	return rb_ary_new4(RARRAY(ary)->len, RARRAY(ary)->ptr);
    }

    collect = rb_ary_new2(RARRAY(ary)->len);
    for (i = 0; i < RARRAY(ary)->len; i++) {
	rb_ary_push(collect, rb_yield(RARRAY(ary)->ptr[i]));
    }
    return collect;
}

/* 
 *  call-seq:
 *     array.collect! {|item| block }   ->   array
 *     array.map!     {|item| block }   ->   array
 *
 *  Invokes the block once for each element of _self_, replacing the
 *  element with the value returned by _block_.
 *  See also <code>Enumerable#collect</code>.
 *   
 *     a = [ "a", "b", "c", "d" ]
 *     a.collect! {|x| x + "!" }
 *     a             #=>  [ "a!", "b!", "c!", "d!" ]
 */

static VALUE
rb_ary_collect_bang(ary)
    VALUE ary;
{
    long i;

    rb_ary_modify(ary);
    for (i = 0; i < RARRAY(ary)->len; i++) {
	rb_ary_store(ary, i, rb_yield(RARRAY(ary)->ptr[i]));
    }
    return ary;
}

VALUE
rb_get_values_at(obj, olen, argc, argv, func)
    VALUE obj;
    long olen;
    int argc;
    VALUE *argv;
    VALUE (*func) _((VALUE,long));
{
    VALUE result = rb_ary_new2(argc);
    long beg, len, i, j;

    for (i=0; i<argc; i++) {
	if (FIXNUM_P(argv[i])) {
	    rb_ary_push(result, (*func)(obj, FIX2LONG(argv[i])));
	    continue;
	}
	/* check if idx is Range */
	switch (rb_range_beg_len(argv[i], &beg, &len, olen, 0)) {
	  case Qfalse:
	    break;
	  case Qnil:
	    continue;
	  default:
	    for (j=0; j<len; j++) {
		rb_ary_push(result, (*func)(obj, j+beg));
	    }
	    continue;
	}
	rb_ary_push(result, (*func)(obj, NUM2LONG(argv[i])));
    }
    return result;
}

/* 
 *  call-seq:
 *     array.values_at(selector,... )  -> an_array
 *
 *  Returns an array containing the elements in
 *  _self_ corresponding to the given selector(s). The selectors
 *  may be either integer indices or ranges. 
 *  See also <code>Array#select</code>.
 * 
 *     a = %w{ a b c d e f }
 *     a.values_at(1, 3, 5)
 *     a.values_at(1, 3, 5, 7)
 *     a.values_at(-1, -3, -5, -7)
 *     a.values_at(1..3, 2...5)
 */

static VALUE
rb_ary_values_at(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    return rb_get_values_at(ary, RARRAY(ary)->len, argc, argv, rb_ary_entry);
}

/*
 *  call-seq:
 *     array.select {|item| block } -> an_array
 *  
 *  Invokes the block passing in successive elements from <i>array</i>,
 *  returning an array containing those elements for which the block
 *  returns a true value (equivalent to <code>Enumerable#select</code>).
 *     
 *     a = %w{ a b c d e f }
 *     a.select {|v| v =~ /[aeiou]/}   #=> ["a", "e"]
 */

static VALUE
rb_ary_select(ary)
    VALUE ary;
{
    VALUE result;
    long i;

    result = rb_ary_new2(RARRAY(ary)->len);
    for (i = 0; i < RARRAY(ary)->len; i++) {
	if (RTEST(rb_yield(RARRAY(ary)->ptr[i]))) {
	    rb_ary_push(result, rb_ary_elt(ary, i));
	}
    }
    return result;
}

/*
 *  call-seq:
 *     array.delete(obj)            -> obj or nil 
 *     array.delete(obj) { block }  -> obj or nil
 *  
 *  Deletes items from <i>self</i> that are equal to <i>obj</i>. If
 *  the item is not found, returns <code>nil</code>. If the optional
 *  code block is given, returns the result of <i>block</i> if the item
 *  is not found.
 *     
 *     a = [ "a", "b", "b", "b", "c" ]
 *     a.delete("b")                   #=> "b"
 *     a                               #=> ["a", "c"]
 *     a.delete("z")                   #=> nil
 *     a.delete("z") { "not found" }   #=> "not found"
 */

VALUE
rb_ary_delete(ary, item)
    VALUE ary;
    VALUE item;
{
    long i1, i2;

    for (i1 = i2 = 0; i1 < RARRAY(ary)->len; i1++) {
	VALUE e = RARRAY(ary)->ptr[i1];

	if (rb_equal(e, item)) continue;
	if (i1 != i2) {
	    rb_ary_store(ary, i2, e);
	}
	i2++;
    }
    if (RARRAY(ary)->len == i2) {
	if (rb_block_given_p()) {
	    return rb_yield(item);
	}
	return Qnil;
    }

    rb_ary_modify(ary);
    if (RARRAY(ary)->len > i2) {
	RARRAY(ary)->len = i2;
	if (i2 * 2 < RARRAY(ary)->aux.capa &&
	    RARRAY(ary)->aux.capa > ARY_DEFAULT_SIZE) {
	    REALLOC_N(RARRAY(ary)->ptr, VALUE, i2 * 2);
	    RARRAY(ary)->aux.capa = i2 * 2;
	}
    }

    return item;
}

VALUE
rb_ary_delete_at(ary, pos)
    VALUE ary;
    long pos;
{
    long i, len = RARRAY(ary)->len;
    VALUE del;

    if (pos >= len) return Qnil;
    if (pos < 0) {
	pos += len;
	if (pos < 0) return Qnil;
    }

    rb_ary_modify(ary);
    del = RARRAY(ary)->ptr[pos];
    for (i = pos + 1; i < len; i++, pos++) {
	RARRAY(ary)->ptr[pos] = RARRAY(ary)->ptr[i];
    }
    RARRAY(ary)->len = pos;

    return del;
}

/*
 *  call-seq:
 *     array.delete_at(index)  -> obj or nil
 *  
 *  Deletes the element at the specified index, returning that element,
 *  or <code>nil</code> if the index is out of range. See also
 *  <code>Array#slice!</code>.
 *     
 *     a = %w( ant bat cat dog )
 *     a.delete_at(2)    #=> "cat"
 *     a                 #=> ["ant", "bat", "dog"]
 *     a.delete_at(99)   #=> nil
 */

static VALUE
rb_ary_delete_at_m(ary, pos)
    VALUE ary, pos;
{
    return rb_ary_delete_at(ary, NUM2LONG(pos));
}

/*
 *  call-seq:
 *     array.slice!(index)         -> obj or nil
 *     array.slice!(start, length) -> sub_array or nil
 *     array.slice!(range)         -> sub_array or nil 
 *  
 *  Deletes the element(s) given by an index (optionally with a length)
 *  or by a range. Returns the deleted object, subarray, or
 *  <code>nil</code> if the index is out of range. Equivalent to:
 *     
 *     def slice!(*args)
 *       result = self[*args]
 *       self[*args] = nil
 *       result
 *     end
 *     
 *     a = [ "a", "b", "c" ]
 *     a.slice!(1)     #=> "b"
 *     a               #=> ["a", "c"]
 *     a.slice!(-1)    #=> "c"
 *     a               #=> ["a"]
 *     a.slice!(100)   #=> nil
 *     a               #=> ["a"]
 */

static VALUE
rb_ary_slice_bang(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE arg1, arg2;
    long pos, len;

    if (rb_scan_args(argc, argv, "11", &arg1, &arg2) == 2) {
	pos = NUM2LONG(arg1);
	len = NUM2LONG(arg2);
      delete_pos_len:
	if (pos < 0) {
	    pos = RARRAY(ary)->len + pos;
	}
	arg2 = rb_ary_subseq(ary, pos, len);
	rb_ary_splice(ary, pos, len, Qundef);	/* Qnil/rb_ary_new2(0) */
	return arg2;
    }

    if (!FIXNUM_P(arg1) && rb_range_beg_len(arg1, &pos, &len, RARRAY(ary)->len, 1)) {
	goto delete_pos_len;
    }

    return rb_ary_delete_at(ary, NUM2LONG(arg1));
}

/*
 *  call-seq:
 *     array.reject! {|item| block }  -> array or nil
 *  
 *  Equivalent to <code>Array#delete_if</code>, deleting elements from
 *  _self_ for which the block evaluates to true, but returns
 *  <code>nil</code> if no changes were made. Also see
 *  <code>Enumerable#reject</code>.
 */

static VALUE
rb_ary_reject_bang(ary)
    VALUE ary;
{
    long i1, i2;

    rb_ary_modify(ary);
    for (i1 = i2 = 0; i1 < RARRAY(ary)->len; i1++) {
	VALUE v = RARRAY(ary)->ptr[i1];
	if (RTEST(rb_yield(v))) continue;
	if (i1 != i2) {
	    rb_ary_store(ary, i2, v);
	}
	i2++;
    }
    if (RARRAY(ary)->len == i2) return Qnil;
    if (i2 < RARRAY(ary)->len)
	RARRAY(ary)->len = i2;

    return ary;
}

/*
 *  call-seq:
 *     array.reject {|item| block }  -> an_array
 *  
 *  Returns a new array containing the items in _self_
 *  for which the block is not true.
 */

static VALUE
rb_ary_reject(ary)
    VALUE ary;
{
    ary = rb_ary_dup(ary);
    rb_ary_reject_bang(ary);
    return ary;
}

/*
 *  call-seq:
 *     array.delete_if {|item| block }  -> array
 *  
 *  Deletes every element of <i>self</i> for which <i>block</i> evaluates
 *  to <code>true</code>.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.delete_if {|x| x >= "b" }   #=> ["a"]
 */

static VALUE
rb_ary_delete_if(ary)
    VALUE ary;
{
    rb_ary_reject_bang(ary);
    return ary;
}

/*
 *  call-seq:
 *     array.zip(arg, ...)                   -> an_array
 *     array.zip(arg, ...) {| arr | block }  -> nil
 *  
 *  Converts any arguments to arrays, then merges elements of
 *  <i>self</i> with corresponding elements from each argument. This
 *  generates a sequence of <code>self.size</code> <em>n</em>-element
 *  arrays, where <em>n</em> is one more that the count of arguments. If
 *  the size of any argument is less than <code>enumObj.size</code>,
 *  <code>nil</code> values are supplied. If a block given, it is
 *  invoked for each output array, otherwise an array of arrays is
 *  returned.
 *     
 *     a = [ 4, 5, 6 ]
 *     b = [ 7, 8, 9 ]
 *     
 *     [1,2,3].zip(a, b)      #=> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
 *     [1,2].zip(a,b)         #=> [[1, 4, 7], [2, 5, 8]]
 *     a.zip([1,2],[8])       #=> [[4,1,8], [5,2,nil], [6,nil,nil]]
 */

static VALUE
rb_ary_zip(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    int i, j;
    long len;
    VALUE result;

    for (i=0; i<argc; i++) {
	argv[i] = to_a(argv[i]);
    }
    if (rb_block_given_p()) {
	for (i=0; i<RARRAY(ary)->len; i++) {
	    VALUE tmp = rb_ary_new2(argc+1);

	    rb_ary_push(tmp, rb_ary_elt(ary, i));
	    for (j=0; j<argc; j++) {
		rb_ary_push(tmp, rb_ary_elt(argv[j], i));
	    }
	    rb_yield(tmp);
	}
	return Qnil;
    }
    len = RARRAY(ary)->len;
    result = rb_ary_new2(len);
    for (i=0; i<len; i++) {
	VALUE tmp = rb_ary_new2(argc+1);

	rb_ary_push(tmp, rb_ary_elt(ary, i));
	for (j=0; j<argc; j++) {
	    rb_ary_push(tmp, rb_ary_elt(argv[j], i));
	}
	rb_ary_push(result, tmp);
    }
    return result;
}

/*
 *  call-seq:
 *     array.transpose -> an_array
 *  
 *  Assumes that <i>self</i> is an array of arrays and transposes the
 *  rows and columns.
 *     
 *     a = [[1,2], [3,4], [5,6]]
 *     a.transpose   #=> [[1, 3, 5], [2, 4, 6]]
 */

static VALUE
rb_ary_transpose(ary)
    VALUE ary;
{
    long elen = -1, alen, i, j;
    VALUE tmp, result = 0;

    alen = RARRAY(ary)->len;
    if (alen == 0) return rb_ary_dup(ary);
    for (i=0; i<alen; i++) {
	tmp = to_ary(rb_ary_elt(ary, i));
	if (elen < 0) {		/* first element */
	    elen = RARRAY(tmp)->len;
	    result = rb_ary_new2(elen);
	    for (j=0; j<elen; j++) {
		rb_ary_store(result, j, rb_ary_new2(alen));
	    }
	}
	else if (elen != RARRAY(tmp)->len) {
	    rb_raise(rb_eIndexError, "element size differs (%d should be %d)",
		     RARRAY(tmp)->len, elen);
	}
	for (j=0; j<elen; j++) {
	    rb_ary_store(rb_ary_elt(result, j), i, rb_ary_elt(tmp, j));
	}
    }
    return result;
}

/*
 *  call-seq:
 *     array.replace(other_array)  -> array
 *  
 *  Replaces the contents of <i>self</i> with the contents of
 *  <i>other_array</i>, truncating or expanding if necessary.
 *     
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a.replace([ "x", "y", "z" ])   #=> ["x", "y", "z"]
 *     a                              #=> ["x", "y", "z"]
 */

static VALUE
rb_ary_replace(copy, orig)
    VALUE copy, orig;
{
    VALUE shared;

    rb_ary_modify(copy);
    orig = to_ary(orig);
    if (copy == orig) return copy;
    shared = ary_make_shared(orig);
    if (RARRAY(copy)->ptr && !FL_TEST(copy, ELTS_SHARED))
	free(RARRAY(copy)->ptr);
    RARRAY(copy)->ptr = RARRAY(orig)->ptr;
    RARRAY(copy)->len = RARRAY(orig)->len;
    RARRAY(copy)->aux.shared = shared;
    FL_SET(copy, ELTS_SHARED);

    return copy;
}

/* 
 *  call-seq:
 *     array.clear    ->  array
 *
 *  Removes all elements from _self_.
 *
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a.clear    #=> [ ]
 */

VALUE
rb_ary_clear(ary)
    VALUE ary;
{
    rb_ary_modify(ary);
    RARRAY(ary)->len = 0;
    if (ARY_DEFAULT_SIZE * 2 < RARRAY(ary)->aux.capa) {
	REALLOC_N(RARRAY(ary)->ptr, VALUE, ARY_DEFAULT_SIZE * 2);
	RARRAY(ary)->aux.capa = ARY_DEFAULT_SIZE * 2;
    }
    return ary;
}

/*
 *  call-seq:
 *     array.fill(obj)                                -> array
 *     array.fill(obj, start [, length])              -> array
 *     array.fill(obj, range )                        -> array
 *     array.fill {|index| block }                    -> array
 *     array.fill(start [, length] ) {|index| block } -> array
 *     array.fill(range) {|index| block }             -> array
 *  
 *  The first three forms set the selected elements of <i>self</i> (which
 *  may be the entire array) to <i>obj</i>. A <i>start</i> of
 *  <code>nil</code> is equivalent to zero. A <i>length</i> of
 *  <code>nil</code> is equivalent to <i>self.length</i>. The last three
 *  forms fill the array with the value of the block. The block is
 *  passed the absolute index of each element to be filled.
 *     
 *     a = [ "a", "b", "c", "d" ]
 *     a.fill("x")              #=> ["x", "x", "x", "x"]
 *     a.fill("z", 2, 2)        #=> ["x", "x", "z", "z"]
 *     a.fill("y", 0..1)        #=> ["y", "y", "z", "z"]
 *     a.fill {|i| i*i}         #=> [0, 1, 4, 9]
 *     a.fill(-2) {|i| i*i*i}   #=> [0, 1, 8, 27]
 */

static VALUE
rb_ary_fill(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE item, arg1, arg2;
    long beg, end, len;
    VALUE *p, *pend;
    int block_p = Qfalse;

    if (rb_block_given_p()) {
	block_p = Qtrue;
	rb_scan_args(argc, argv, "02", &arg1, &arg2);
	argc += 1;		/* hackish */
    }
    else {
	rb_scan_args(argc, argv, "12", &item, &arg1, &arg2);
    }
    switch (argc) {
      case 1:
	beg = 0;
	len = RARRAY(ary)->len;
	break;
      case 2:
	if (rb_range_beg_len(arg1, &beg, &len, RARRAY(ary)->len, 1)) {
	    break;
	}
	/* fall through */
      case 3:
	beg = NIL_P(arg1) ? 0 : NUM2LONG(arg1);
	if (beg < 0) {
	    beg = RARRAY(ary)->len + beg;
	    if (beg < 0) beg = 0;
	}
	len = NIL_P(arg2) ? RARRAY(ary)->len - beg : NUM2LONG(arg2);
	break;
    }
    rb_ary_modify(ary);
    end = beg + len;
    if (end > RARRAY(ary)->len) {
	if (end >= RARRAY(ary)->aux.capa) {
	    REALLOC_N(RARRAY(ary)->ptr, VALUE, end);
	    RARRAY(ary)->aux.capa = end;
	}
	if (beg > RARRAY(ary)->len) {
	    rb_mem_clear(RARRAY(ary)->ptr + RARRAY(ary)->len, end - RARRAY(ary)->len);
	}
	RARRAY(ary)->len = end;
    }

    if (block_p) {
	VALUE v;
	long i;

	for (i=beg; i<end; i++) {
	    v = rb_yield(LONG2NUM(i));
	    if (i>=RARRAY(ary)->len) break;
	    RARRAY(ary)->ptr[i] = v;
	}
    }
    else {
	p = RARRAY(ary)->ptr + beg;
	pend = p + len;
	while (p < pend) {
	    *p++ = item;
	}
    }
    return ary;
}

/* 
 *  call-seq:
 *     array + other_array   -> an_array
 *
 *  Concatenation---Returns a new array built by concatenating the
 *  two arrays together to produce a third array.
 * 
 *     [ 1, 2, 3 ] + [ 4, 5 ]    #=> [ 1, 2, 3, 4, 5 ]
 */

VALUE
rb_ary_plus(x, y)
    VALUE x, y;
{
    VALUE z;
    long len;

    y = to_ary(y);
    len = RARRAY(x)->len + RARRAY(y)->len;
    z = rb_ary_new2(len);
    MEMCPY(RARRAY(z)->ptr, RARRAY(x)->ptr, VALUE, RARRAY(x)->len);
    MEMCPY(RARRAY(z)->ptr + RARRAY(x)->len, RARRAY(y)->ptr, VALUE, RARRAY(y)->len);
    RARRAY(z)->len = len;
    return z;
}

/* 
 *  call-seq:
 *     array.concat(other_array)   ->  array
 *
 *  Appends the elements in other_array to _self_.
 *  
 *     [ "a", "b" ].concat( ["c", "d"] ) #=> [ "a", "b", "c", "d" ]
 */


VALUE
rb_ary_concat(x, y)
    VALUE x, y;
{
    y = to_ary(y);
    if (RARRAY(y)->len > 0) {
	rb_ary_splice(x, RARRAY(x)->len, 0, y);
    }
    return x;
}


/* 
 *  call-seq:
 *     array * int     ->    an_array
 *     array * str     ->    a_string
 *
 *  Repetition---With a String argument, equivalent to
 *  self.join(str). Otherwise, returns a new array
 *  built by concatenating the _int_ copies of _self_.
 *
 *
 *     [ 1, 2, 3 ] * 3    #=> [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]
 *     [ 1, 2, 3 ] * ","  #=> "1,2,3"
 *
 */

static VALUE
rb_ary_times(ary, times)
    VALUE ary, times;
{
    VALUE ary2, tmp;
    long i, len;

    tmp = rb_check_string_type(times);
    if (!NIL_P(tmp)) {
	return rb_ary_join(ary, tmp);
    }

    len = NUM2LONG(times);
    if (len == 0) return ary_new(rb_obj_class(ary), 0);
    if (len < 0) {
	rb_raise(rb_eArgError, "negative argument");
    }
    if (LONG_MAX/len < RARRAY(ary)->len) {
	rb_raise(rb_eArgError, "argument too big");
    }
    len *= RARRAY(ary)->len;

    ary2 = ary_new(rb_obj_class(ary), len);
    RARRAY(ary2)->len = len;

    for (i=0; i<len; i+=RARRAY(ary)->len) {
	MEMCPY(RARRAY(ary2)->ptr+i, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);
    }
    OBJ_INFECT(ary2, ary);

    return ary2;
}

/* 
 *  call-seq:
 *     array.assoc(obj)   ->  an_array  or  nil
 *
 *  Searches through an array whose elements are also arrays
 *  comparing _obj_ with the first element of each contained array
 *  using obj.==.
 *  Returns the first contained array that matches (that
 *  is, the first associated array),
 *  or +nil+ if no match is found.
 *  See also <code>Array#rassoc</code>.
 *
 *     s1 = [ "colors", "red", "blue", "green" ]
 *     s2 = [ "letters", "a", "b", "c" ]
 *     s3 = "foo"
 *     a  = [ s1, s2, s3 ]
 *     a.assoc("letters")  #=> [ "letters", "a", "b", "c" ]
 *     a.assoc("foo")      #=> nil
 */

VALUE
rb_ary_assoc(ary, key)
    VALUE ary, key;
{
    long i;
    VALUE v;

    for (i = 0; i < RARRAY(ary)->len; ++i) {
	v = RARRAY(ary)->ptr[i];
	if (TYPE(v) == T_ARRAY &&
	    RARRAY(v)->len > 0 &&
	    rb_equal(RARRAY(v)->ptr[0], key))
	    return v;
    }
    return Qnil;
}

/*
 *  call-seq:
 *     array.rassoc(key) -> an_array or nil
 *  
 *  Searches through the array whose elements are also arrays. Compares
 *  <em>key</em> with the second element of each contained array using
 *  <code>==</code>. Returns the first contained array that matches. See
 *  also <code>Array#assoc</code>.
 *     
 *     a = [ [ 1, "one"], [2, "two"], [3, "three"], ["ii", "two"] ]
 *     a.rassoc("two")    #=> [2, "two"]
 *     a.rassoc("four")   #=> nil
 */

VALUE
rb_ary_rassoc(ary, value)
    VALUE ary, value;
{
    long i;
    VALUE v;

    for (i = 0; i < RARRAY(ary)->len; ++i) {
	v = RARRAY(ary)->ptr[i];
	if (TYPE(v) == T_ARRAY &&
	    RARRAY(v)->len > 1 &&
	    rb_equal(RARRAY(v)->ptr[1], value))
	    return v;
    }
    return Qnil;
}

/* 
 *  call-seq:
 *     array == other_array   ->   bool
 *
 *  Equality---Two arrays are equal if they contain the same number
 *  of elements and if each element is equal to (according to
 *  Object.==) the corresponding element in the other array.
 *
 *     [ "a", "c" ]    == [ "a", "c", 7 ]     #=> false
 *     [ "a", "c", 7 ] == [ "a", "c", 7 ]     #=> true
 *     [ "a", "c", 7 ] == [ "a", "d", "f" ]   #=> false
 *
 */

static VALUE
rb_ary_equal(ary1, ary2)
    VALUE ary1, ary2;
{
    long i;

    if (ary1 == ary2) return Qtrue;
    if (TYPE(ary2) != T_ARRAY) {
	if (!rb_respond_to(ary2, rb_intern("to_ary"))) {
	    return Qfalse;
	}
	return rb_equal(ary2, ary1);
    }
    if (RARRAY(ary1)->len != RARRAY(ary2)->len) return Qfalse;
    for (i=0; i<RARRAY(ary1)->len; i++) {
	if (!rb_equal(rb_ary_elt(ary1, i), rb_ary_elt(ary2, i)))
	    return Qfalse;
    }
    return Qtrue;
}

/*
 *  call-seq:
 *     array.eql?(other)  -> true or false
 *
 *  Returns <code>true</code> if _array_ and _other_ are the same object,
 *  or are both arrays with the same content.
 */

static VALUE
rb_ary_eql(ary1, ary2)
    VALUE ary1, ary2;
{
    long i;

    if (ary1 == ary2) return Qtrue;
    if (TYPE(ary2) != T_ARRAY) return Qfalse;
    if (RARRAY(ary1)->len != RARRAY(ary2)->len) return Qfalse;
    for (i=0; i<RARRAY(ary1)->len; i++) {
	if (!rb_eql(rb_ary_elt(ary1, i), rb_ary_elt(ary2, i)))
	    return Qfalse;
    }
    return Qtrue;
}

static VALUE
recursive_hash(ary, dummy, recur)
    VALUE ary, dummy;
    int recur;
{
    long i, h;
    VALUE n;

    if (recur) {
	return LONG2FIX(0);
    }
    h = RARRAY(ary)->len;
    for (i=0; i<RARRAY(ary)->len; i++) {
	h = (h << 1) | (h<0 ? 1 : 0);
	n = rb_hash(RARRAY(ary)->ptr[i]);
	h ^= NUM2LONG(n);
    }
    return LONG2FIX(h);
}

/*
 *  call-seq:
 *     array.hash   -> fixnum
 *
 *  Compute a hash-code for this array. Two arrays with the same content
 *  will have the same hash code (and will compare using <code>eql?</code>).
 */

static VALUE
rb_ary_hash(ary)
    VALUE ary;
{
    return rb_exec_recursive(recursive_hash, ary, 0);
}

/*
 *  call-seq:
 *     array.include?(obj)   -> true or false
 *  
 *  Returns <code>true</code> if the given object is present in
 *  <i>self</i> (that is, if any object <code>==</code> <i>anObject</i>),
 *  <code>false</code> otherwise.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.include?("b")   #=> true
 *     a.include?("z")   #=> false
 */

VALUE
rb_ary_includes(ary, item)
    VALUE ary;
    VALUE item;
{
    long i;
    
    for (i=0; i<RARRAY(ary)->len; i++) {
	if (rb_equal(RARRAY(ary)->ptr[i], item)) {
	    return Qtrue;
	}
    }
    return Qfalse;
}


/* 
 *  call-seq:
 *     array <=> other_array   ->  -1, 0, +1
 *
 *  Comparison---Returns an integer (-1, 0,
 *  or +1) if this array is less than, equal to, or greater than
 *  other_array.  Each object in each array is compared
 *  (using <=>). If any value isn't
 *  equal, then that inequality is the return value. If all the
 *  values found are equal, then the return is based on a
 *  comparison of the array lengths.  Thus, two arrays are
 *  ``equal'' according to <code>Array#<=></code> if and only if they have
 *  the same length and the value of each element is equal to the
 *  value of the corresponding element in the other array.
 *  
 *     [ "a", "a", "c" ]    <=> [ "a", "b", "c" ]   #=> -1
 *     [ 1, 2, 3, 4, 5, 6 ] <=> [ 1, 2 ]            #=> +1
 *
 */

VALUE
rb_ary_cmp(ary1, ary2)
    VALUE ary1, ary2;
{
    long i, len;

    ary2 = to_ary(ary2);
    len = RARRAY(ary1)->len;
    if (len > RARRAY(ary2)->len) {
	len = RARRAY(ary2)->len;
    }
    for (i=0; i<len; i++) {
	VALUE v = rb_funcall(rb_ary_elt(ary1, i), id_cmp, 1, rb_ary_elt(ary2, i));
	if (v != INT2FIX(0)) {
	    return v;
	}
    }
    len = RARRAY(ary1)->len - RARRAY(ary2)->len;
    if (len == 0) return INT2FIX(0);
    if (len > 0) return INT2FIX(1);
    return INT2FIX(-1);
}

static VALUE
ary_make_hash(ary1, ary2)
    VALUE ary1, ary2;
{
    VALUE hash = rb_hash_new();
    long i;

    for (i=0; i<RARRAY(ary1)->len; i++) {
	rb_hash_aset(hash, RARRAY(ary1)->ptr[i], Qtrue);
    }
    if (ary2) {
	for (i=0; i<RARRAY(ary2)->len; i++) {
	    rb_hash_aset(hash, RARRAY(ary2)->ptr[i], Qtrue);
	}
    }
    return hash;
}

/* 
 *  call-seq:
 *     array - other_array    -> an_array
 *
 *  Array Difference---Returns a new array that is a copy of
 *  the original array, removing any items that also appear in
 *  other_array. (If you need set-like behavior, see the
 *  library class Set.)
 *
 *     [ 1, 1, 2, 2, 3, 3, 4, 5 ] - [ 1, 2, 4 ]  #=>  [ 3, 3, 5 ]
 */

static VALUE
rb_ary_diff(ary1, ary2)
    VALUE ary1, ary2;
{
    VALUE ary3, hash;
    long i;

    hash = ary_make_hash(to_ary(ary2), 0);
    ary3 = rb_ary_new();

    for (i=0; i<RARRAY(ary1)->len; i++) {
	if (st_lookup(RHASH(hash)->tbl, RARRAY(ary1)->ptr[i], 0)) continue;
	rb_ary_push(ary3, rb_ary_elt(ary1, i));
    }
    return ary3;
}

/* 
 *  call-seq:
 *     array & other_array
 *
 *  Set Intersection---Returns a new array
 *  containing elements common to the two arrays, with no duplicates.
 *
 *     [ 1, 1, 3, 5 ] & [ 1, 2, 3 ]   #=> [ 1, 3 ]
 */


static VALUE
rb_ary_and(ary1, ary2)
    VALUE ary1, ary2;
{
    VALUE hash, ary3, v, vv;
    long i;

    ary2 = to_ary(ary2);
    ary3 = rb_ary_new2(RARRAY(ary1)->len < RARRAY(ary2)->len ?
	    RARRAY(ary1)->len : RARRAY(ary2)->len);
    hash = ary_make_hash(ary2, 0);

    for (i=0; i<RARRAY(ary1)->len; i++) {
	v = vv = rb_ary_elt(ary1, i);
	if (st_delete(RHASH(hash)->tbl, (st_data_t*)&vv, 0)) {
	    rb_ary_push(ary3, v);
	}
    }

    return ary3;
}

/* 
 *  call-seq:
 *     array | other_array     ->  an_array
 *
 *  Set Union---Returns a new array by joining this array with
 *  other_array, removing duplicates.
 *
 *     [ "a", "b", "c" ] | [ "c", "d", "a" ]
 *            #=> [ "a", "b", "c", "d" ]
 */

static VALUE
rb_ary_or(ary1, ary2)
    VALUE ary1, ary2;
{
    VALUE hash, ary3;
    VALUE v, vv;
    long i;

    ary2 = to_ary(ary2);
    ary3 = rb_ary_new2(RARRAY(ary1)->len+RARRAY(ary2)->len);
    hash = ary_make_hash(ary1, ary2);

    for (i=0; i<RARRAY(ary1)->len; i++) {
	v = vv = rb_ary_elt(ary1, i);
	if (st_delete(RHASH(hash)->tbl, (st_data_t*)&vv, 0)) {
	    rb_ary_push(ary3, v);
	}
    }
    for (i=0; i<RARRAY(ary2)->len; i++) {
	v = vv = rb_ary_elt(ary2, i);
	if (st_delete(RHASH(hash)->tbl, (st_data_t*)&vv, 0)) {
	    rb_ary_push(ary3, v);
	}
    }
    return ary3;
}

/*
 *  call-seq:
 *     array.uniq! -> array or nil
 *  
 *  Removes duplicate elements from _self_.
 *  Returns <code>nil</code> if no changes are made (that is, no
 *  duplicates are found).
 *     
 *     a = [ "a", "a", "b", "b", "c" ]
 *     a.uniq!   #=> ["a", "b", "c"]
 *     b = [ "a", "b", "c" ]
 *     b.uniq!   #=> nil
 */

static VALUE
rb_ary_uniq_bang(ary)
    VALUE ary;
{
    VALUE hash, v, vv;
    long i, j;

    hash = ary_make_hash(ary, 0);

    if (RARRAY(ary)->len == RHASH(hash)->tbl->num_entries) {
	return Qnil;
    }
    for (i=j=0; i<RARRAY(ary)->len; i++) {
	v = vv = rb_ary_elt(ary, i);
	if (st_delete(RHASH(hash)->tbl, (st_data_t*)&vv, 0)) {
	    rb_ary_store(ary, j++, v);
	}
    }
    RARRAY(ary)->len = j;

    return ary;
}

/*
 *  call-seq:
 *     array.uniq   -> an_array
 *  
 *  Returns a new array by removing duplicate values in <i>self</i>.
 *     
 *     a = [ "a", "a", "b", "b", "c" ]
 *     a.uniq   #=> ["a", "b", "c"]
 */

static VALUE
rb_ary_uniq(ary)
    VALUE ary;
{
    ary = rb_ary_dup(ary);
    rb_ary_uniq_bang(ary);
    return ary;
}

/* 
 *  call-seq:
 *     array.compact!    ->   array  or  nil
 *
 *  Removes +nil+ elements from array.
 *  Returns +nil+ if no changes were made.
 *
 *     [ "a", nil, "b", nil, "c" ].compact! #=> [ "a", "b", "c" ]
 *     [ "a", "b", "c" ].compact!           #=> nil
 */

static VALUE
rb_ary_compact_bang(ary)
    VALUE ary;
{
    VALUE *p, *t, *end;

    rb_ary_modify(ary);
    p = t = RARRAY(ary)->ptr;
    end = p + RARRAY(ary)->len;
    
    while (t < end) {
	if (NIL_P(*t)) t++;
	else *p++ = *t++;
    }
    if (RARRAY(ary)->len == (p - RARRAY(ary)->ptr)) {
	return Qnil;
    }
    RARRAY(ary)->len = RARRAY(ary)->aux.capa = (p - RARRAY(ary)->ptr);
    REALLOC_N(RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);

    return ary;
}

/*
 *  call-seq:
 *     array.compact     ->  an_array
 *
 *  Returns a copy of _self_ with all +nil+ elements removed.
 *
 *     [ "a", nil, "b", nil, "c", nil ].compact
 *                       #=> [ "a", "b", "c" ]
 */

static VALUE
rb_ary_compact(ary)
    VALUE ary;
{
    ary = rb_ary_dup(ary);
    rb_ary_compact_bang(ary);
    return ary;
}

/*
 *  call-seq:
 *     array.nitems -> int
 *  
 *  Returns the number of non-<code>nil</code> elements in _self_.
 *  May be zero.
 *     
 *     [ 1, nil, 3, nil, 5 ].nitems   #=> 3
 */

static VALUE
rb_ary_nitems(ary)
    VALUE ary;
{
    long n = 0;
    VALUE *p, *pend;

    p = RARRAY(ary)->ptr;
    pend = p + RARRAY(ary)->len;

    while (p < pend) {
	if (!NIL_P(*p)) n++;
	p++;
    }
    return LONG2NUM(n);
}

static long
flatten(ary, idx, ary2, memo)
    VALUE ary;
    long idx;
    VALUE ary2, memo;
{
    VALUE id;
    long i = idx;
    long n, lim = idx + RARRAY(ary2)->len;

    id = rb_obj_id(ary2);
    if (rb_ary_includes(memo, id)) {
	rb_raise(rb_eArgError, "tried to flatten recursive array");
    }
    rb_ary_push(memo, id);
    rb_ary_splice(ary, idx, 1, ary2);
    while (i < lim) {
	VALUE tmp;

	tmp = rb_check_array_type(rb_ary_elt(ary, i));
	if (!NIL_P(tmp)) {
	    n = flatten(ary, i, tmp, memo);
	    i += n; lim += n;
	}
	i++;
    }
    rb_ary_pop(memo);

    return lim - idx - 1;	/* returns number of increased items */
}

/*
 *  call-seq:
 *     array.flatten! -> array or nil
 *  
 *  Flattens _self_ in place.
 *  Returns <code>nil</code> if no modifications were made (i.e.,
 *  <i>array</i> contains no subarrays.)
 *     
 *     a = [ 1, 2, [3, [4, 5] ] ]
 *     a.flatten!   #=> [1, 2, 3, 4, 5]
 *     a.flatten!   #=> nil
 *     a            #=> [1, 2, 3, 4, 5]
 */

static VALUE
rb_ary_flatten_bang(ary)
    VALUE ary;
{
    long i = 0;
    int mod = 0;
    VALUE memo = Qnil;

    while (i<RARRAY(ary)->len) {
	VALUE ary2 = RARRAY(ary)->ptr[i];
	VALUE tmp;

	tmp = rb_check_array_type(ary2);
	if (!NIL_P(tmp)) {
	    if (NIL_P(memo)) {
		memo = rb_ary_new();
	    }
	    i += flatten(ary, i, tmp, memo);
	    mod = 1;
	}
	i++;
    }
    if (mod == 0) return Qnil;
    return ary;
}

/*
 *  call-seq:
 *     array.flatten -> an_array
 *  
 *  Returns a new array that is a one-dimensional flattening of this
 *  array (recursively). That is, for every element that is an array,
 *  extract its elements into the new array.
 *     
 *     s = [ 1, 2, 3 ]           #=> [1, 2, 3]
 *     t = [ 4, 5, 6, [7, 8] ]   #=> [4, 5, 6, [7, 8]]
 *     a = [ s, t, 9, 10 ]       #=> [[1, 2, 3], [4, 5, 6, [7, 8]], 9, 10]
 *     a.flatten                 #=> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10
 */

static VALUE
rb_ary_flatten(ary)
    VALUE ary;
{
    ary = rb_ary_dup(ary);
    rb_ary_flatten_bang(ary);
    return ary;
}


/* Arrays are ordered, integer-indexed collections of any object. 
 * Array indexing starts at 0, as in C or Java.  A negative index is 
 * assumed to be relative to the end of the array---that is, an index of -1 
 * indicates the last element of the array, -2 is the next to last 
 * element in the array, and so on. 
 */

void
Init_Array()
{
    rb_cArray  = rb_define_class("Array", rb_cObject);
    rb_include_module(rb_cArray, rb_mEnumerable);

    rb_define_alloc_func(rb_cArray, ary_alloc);
    rb_define_singleton_method(rb_cArray, "[]", rb_ary_s_create, -1);
    rb_define_method(rb_cArray, "initialize", rb_ary_initialize, -1);
    rb_define_method(rb_cArray, "initialize_copy", rb_ary_replace, 1);

    rb_define_method(rb_cArray, "to_s", rb_ary_to_s, 0);
    rb_define_method(rb_cArray, "inspect", rb_ary_inspect, 0);
    rb_define_method(rb_cArray, "to_a", rb_ary_to_a, 0);
    rb_define_method(rb_cArray, "to_ary", rb_ary_to_ary_m, 0);
    rb_define_method(rb_cArray, "frozen?",  rb_ary_frozen_p, 0);

    rb_define_method(rb_cArray, "==", rb_ary_equal, 1);
    rb_define_method(rb_cArray, "eql?", rb_ary_eql, 1);
    rb_define_method(rb_cArray, "hash", rb_ary_hash, 0);

    rb_define_method(rb_cArray, "[]", rb_ary_aref, -1);
    rb_define_method(rb_cArray, "[]=", rb_ary_aset, -1);
    rb_define_method(rb_cArray, "at", rb_ary_at, 1);
    rb_define_method(rb_cArray, "fetch", rb_ary_fetch, -1);
    rb_define_method(rb_cArray, "first", rb_ary_first, -1);
    rb_define_method(rb_cArray, "last", rb_ary_last, -1);
    rb_define_method(rb_cArray, "concat", rb_ary_concat, 1);
    rb_define_method(rb_cArray, "<<", rb_ary_push, 1);
    rb_define_method(rb_cArray, "push", rb_ary_push_m, -1);
    rb_define_method(rb_cArray, "pop", rb_ary_pop_m, -1);
    rb_define_method(rb_cArray, "shift", rb_ary_shift_m, -1);
    rb_define_method(rb_cArray, "unshift", rb_ary_unshift_m, -1);
    rb_define_method(rb_cArray, "insert", rb_ary_insert, -1);
    rb_define_method(rb_cArray, "each", rb_ary_each, 0);
    rb_define_method(rb_cArray, "each_index", rb_ary_each_index, 0);
    rb_define_method(rb_cArray, "reverse_each", rb_ary_reverse_each, 0);
    rb_define_method(rb_cArray, "length", rb_ary_length, 0);
    rb_define_alias(rb_cArray,  "size", "length");
    rb_define_method(rb_cArray, "empty?", rb_ary_empty_p, 0);
    rb_define_method(rb_cArray, "index", rb_ary_index, -1);
    rb_define_method(rb_cArray, "rindex", rb_ary_rindex, -1);
    rb_define_method(rb_cArray, "join", rb_ary_join_m, -1);
    rb_define_method(rb_cArray, "reverse", rb_ary_reverse_m, 0);
    rb_define_method(rb_cArray, "reverse!", rb_ary_reverse_bang, 0);
    rb_define_method(rb_cArray, "sort", rb_ary_sort, 0);
    rb_define_method(rb_cArray, "sort!", rb_ary_sort_bang, 0);
    rb_define_method(rb_cArray, "collect", rb_ary_collect, 0);
    rb_define_method(rb_cArray, "collect!", rb_ary_collect_bang, 0);
    rb_define_method(rb_cArray, "map", rb_ary_collect, 0);
    rb_define_method(rb_cArray, "map!", rb_ary_collect_bang, 0);
    rb_define_method(rb_cArray, "select", rb_ary_select, 0);
    rb_define_method(rb_cArray, "values_at", rb_ary_values_at, -1);
    rb_define_method(rb_cArray, "delete", rb_ary_delete, 1);
    rb_define_method(rb_cArray, "delete_at", rb_ary_delete_at_m, 1);
    rb_define_method(rb_cArray, "delete_if", rb_ary_delete_if, 0);
    rb_define_method(rb_cArray, "reject", rb_ary_reject, 0);
    rb_define_method(rb_cArray, "reject!", rb_ary_reject_bang, 0);
    rb_define_method(rb_cArray, "zip", rb_ary_zip, -1);
    rb_define_method(rb_cArray, "transpose", rb_ary_transpose, 0);
    rb_define_method(rb_cArray, "replace", rb_ary_replace, 1);
    rb_define_method(rb_cArray, "clear", rb_ary_clear, 0);
    rb_define_method(rb_cArray, "fill", rb_ary_fill, -1);
    rb_define_method(rb_cArray, "include?", rb_ary_includes, 1);
    rb_define_method(rb_cArray, "<=>", rb_ary_cmp, 1);

    rb_define_method(rb_cArray, "slice", rb_ary_aref, -1);
    rb_define_method(rb_cArray, "slice!", rb_ary_slice_bang, -1);

    rb_define_method(rb_cArray, "assoc", rb_ary_assoc, 1);
    rb_define_method(rb_cArray, "rassoc", rb_ary_rassoc, 1);

    rb_define_method(rb_cArray, "+", rb_ary_plus, 1);
    rb_define_method(rb_cArray, "*", rb_ary_times, 1);

    rb_define_method(rb_cArray, "-", rb_ary_diff, 1);
    rb_define_method(rb_cArray, "&", rb_ary_and, 1);
    rb_define_method(rb_cArray, "|", rb_ary_or, 1);

    rb_define_method(rb_cArray, "uniq", rb_ary_uniq, 0);
    rb_define_method(rb_cArray, "uniq!", rb_ary_uniq_bang, 0);
    rb_define_method(rb_cArray, "compact", rb_ary_compact, 0);
    rb_define_method(rb_cArray, "compact!", rb_ary_compact_bang, 0);
    rb_define_method(rb_cArray, "flatten", rb_ary_flatten, 0);
    rb_define_method(rb_cArray, "flatten!", rb_ary_flatten_bang, 0);
    rb_define_method(rb_cArray, "nitems", rb_ary_nitems, 0);

    id_cmp = rb_intern("<=>");

    rb_cValues  = rb_define_class("Values", rb_cArray);
}
/**********************************************************************
  ascii.c -  Oniguruma (regular expression library)
**********************************************************************/
/*-
 * Copyright (c) 2002-2004  K.Kosako  <sndgk393 AT ybb DOT ne DOT jp>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "regenc.h"

static int
ascii_is_code_ctype(OnigCodePoint code, unsigned int ctype)
{
  if (code < 128)
    return ONIGENC_IS_ASCII_CODE_CTYPE(code, ctype);
  else
    return FALSE;
}

OnigEncodingType OnigEncodingASCII = {
  onigenc_single_byte_mbc_enc_len,
  "US-ASCII",  /* name */
  1,           /* max byte length */
  1,           /* min byte length */
  ONIGENC_AMBIGUOUS_MATCH_ASCII_CASE,
  {
      (OnigCodePoint )'\\'                       /* esc */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* anychar '.'  */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* anytime '*'  */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* zero or one time '?' */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* one or more time '+' */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* anychar anytime */
  },
  onigenc_is_mbc_newline_0x0a,
  onigenc_single_byte_mbc_to_code,
  onigenc_single_byte_code_to_mbclen,
  onigenc_single_byte_code_to_mbc,
  onigenc_ascii_mbc_to_normalize,
  onigenc_ascii_is_mbc_ambiguous,
  onigenc_ascii_get_all_pair_ambig_codes,
  onigenc_nothing_get_all_comp_ambig_codes,
  ascii_is_code_ctype,
  onigenc_not_support_get_ctype_code_range,
  onigenc_single_byte_left_adjust_char_head,
  onigenc_always_true_is_allowed_reverse_match
};
/**********************************************************************

  bignum.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Fri Jun 10 00:48:55 JST 1994

  Copyright (C) 1993-2003 Yukihiro Matsumoto

**********************************************************************/

#include "ruby.h"

#include <math.h>
#include <ctype.h>
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

VALUE rb_cBignum;

#if defined __MINGW32__
#define USHORT _USHORT
#endif

#define BDIGITS(x) ((BDIGIT*)RBIGNUM(x)->digits)
#define BITSPERDIG (SIZEOF_BDIGITS*CHAR_BIT)
#define BIGRAD ((BDIGIT_DBL)1 << BITSPERDIG)
#define DIGSPERLONG ((unsigned int)(SIZEOF_LONG/SIZEOF_BDIGITS))
#if HAVE_LONG_LONG
# define DIGSPERLL ((unsigned int)(SIZEOF_LONG_LONG/SIZEOF_BDIGITS))
#endif
#define BIGUP(x) ((BDIGIT_DBL)(x) << BITSPERDIG)
#define BIGDN(x) RSHIFT(x,BITSPERDIG)
#define BIGLO(x) ((BDIGIT)((x) & (BIGRAD-1)))
#define BDIGMAX ((BDIGIT)-1)

#define BIGZEROP(x) (RBIGNUM(x)->len == 0 || (RBIGNUM(x)->len == 1 && BDIGITS(x)[0] == 0))

static VALUE
bignew_1(klass, len, sign)
    VALUE klass;
    long len;
    char sign;
{
    NEWOBJ(big, struct RBignum);
    OBJSETUP(big, klass, T_BIGNUM);
    big->sign = sign;
    big->len = len;
    big->digits = ALLOC_N(BDIGIT, len);

    return (VALUE)big;
}

#define bignew(len,sign) bignew_1(rb_cBignum,len,sign)

VALUE
rb_big_clone(x)
    VALUE x;
{
    VALUE z = bignew_1(CLASS_OF(x), RBIGNUM(x)->len, RBIGNUM(x)->sign);

    MEMCPY(BDIGITS(z), BDIGITS(x), BDIGIT, RBIGNUM(x)->len);
    return z;
}

static void
get2comp(x, carry)		/* get 2's complement */
    VALUE x;
    int carry;
{
    long i = RBIGNUM(x)->len;
    BDIGIT *ds = BDIGITS(x);
    BDIGIT_DBL num;

    while (i--) ds[i] = ~ds[i];
    i = 0; num = 1;
    do {
	num += ds[i];
	ds[i++] = BIGLO(num);
	num = BIGDN(num);
    } while (i < RBIGNUM(x)->len);
    if (!carry) return;
    if ((ds[RBIGNUM(x)->len-1] & (1<<(BITSPERDIG-1))) == 0) {
	REALLOC_N(RBIGNUM(x)->digits, BDIGIT, ++RBIGNUM(x)->len);
	ds = BDIGITS(x);
	ds[RBIGNUM(x)->len-1] = ~0;
    }
}

void
rb_big_2comp(x)			/* get 2's complement */
    VALUE x;
{
    get2comp(x, Qtrue);
}

static VALUE
bignorm(x)
    VALUE x;
{
    if (!FIXNUM_P(x)) {
	long len = RBIGNUM(x)->len;
	BDIGIT *ds = BDIGITS(x);

	while (len-- && !ds[len]) ;
	RBIGNUM(x)->len = ++len;

	if (len*SIZEOF_BDIGITS <= sizeof(VALUE)) {
	    long num = 0;
	    while (len--) {
		num = BIGUP(num) + ds[len];
	    }
	    if (num >= 0) {
		if (RBIGNUM(x)->sign) {
		    if (POSFIXABLE(num)) return LONG2FIX(num);
		}
		else if (NEGFIXABLE(-(long)num)) return LONG2FIX(-(long)num);
	    }
	}
    }
    return x;
}

VALUE
rb_big_norm(x)
    VALUE x;
{
    return bignorm(x);
}

VALUE
rb_uint2big(n)
    unsigned long n;
{
    BDIGIT_DBL num = n;
    long i = 0;
    BDIGIT *digits;
    VALUE big;

    big = bignew(DIGSPERLONG, 1);
    digits = BDIGITS(big);
    while (i < DIGSPERLONG) {
	digits[i++] = BIGLO(num);
	num = BIGDN(num);
    }

    i = DIGSPERLONG;
    while (--i && !digits[i]) ;
    RBIGNUM(big)->len = i+1;
    return big;
}

VALUE
rb_int2big(n)
    long n;
{
    long neg = 0;
    VALUE big;

    if (n < 0) {
	n = -n;
	neg = 1;
    }
    big = rb_uint2big(n);
    if (neg) {
	RBIGNUM(big)->sign = 0;
    }
    return big;
}

VALUE
rb_uint2inum(n)
    unsigned long n;
{
    if (POSFIXABLE(n)) return LONG2FIX(n);
    return rb_uint2big(n);
}

VALUE
rb_int2inum(n)
    long n;
{
    if (FIXABLE(n)) return LONG2FIX(n);
    return rb_int2big(n);
}

#ifdef HAVE_LONG_LONG

void
rb_quad_pack(buf, val)
    char *buf;
    VALUE val;
{
    LONG_LONG q;

    val = rb_to_int(val);
    if (FIXNUM_P(val)) {
	q = FIX2LONG(val);
    }
    else {
	long len = RBIGNUM(val)->len;
	BDIGIT *ds;

	if (len > SIZEOF_LONG_LONG/SIZEOF_BDIGITS) {
	    len = SIZEOF_LONG/SIZEOF_BDIGITS;
	}
	ds = BDIGITS(val);
	q = 0;
	while (len--) {
	    q = BIGUP(q);
	    q += ds[len];
	}
	if (!RBIGNUM(val)->sign) q = -q;
    }
    memcpy(buf, (char*)&q, SIZEOF_LONG_LONG);
}

VALUE
rb_quad_unpack(buf, sign)
    const char *buf;
    int sign;
{
    unsigned LONG_LONG q;
    long neg = 0;
    long i;
    BDIGIT *digits;
    VALUE big;

    memcpy(&q, buf, SIZEOF_LONG_LONG);
    if (sign) {
	if (FIXABLE((LONG_LONG)q)) return LONG2FIX((LONG_LONG)q);
	if ((LONG_LONG)q < 0) {
	    q = -(LONG_LONG)q;
	    neg = 1;
	}
    }
    else {
	if (POSFIXABLE(q)) return LONG2FIX(q);
    }

    i = 0;
    big = bignew(DIGSPERLL, 1);
    digits = BDIGITS(big);
    while (i < DIGSPERLL) {
	digits[i++] = BIGLO(q);
	q = BIGDN(q);
    }

    i = DIGSPERLL;
    while (i-- && !digits[i]) ;
    RBIGNUM(big)->len = i+1;

    if (neg) {
	RBIGNUM(big)->sign = 0;
    }
    return bignorm(big);
}

#else

#define QUAD_SIZE 8

void
rb_quad_pack(buf, val)
    char *buf;
    VALUE val;
{
    long len;

    memset(buf, 0, QUAD_SIZE);
    val = rb_to_int(val);
    if (FIXNUM_P(val)) {
	val = rb_int2big(FIX2LONG(val));
    }
    len = RBIGNUM(val)->len * SIZEOF_BDIGITS;
    if (len > QUAD_SIZE) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `quad int'");
    }
    memcpy(buf, (char*)BDIGITS(val), len);
    if (!RBIGNUM(val)->sign) {
	len = QUAD_SIZE;
	while (len--) {
	    *buf = ~*buf;
	    buf++;
	}
    }
}

#define BNEG(b) (RSHIFT(((BDIGIT*)b)[QUAD_SIZE/SIZEOF_BDIGITS-1],BITSPERDIG-1) != 0)

VALUE
rb_quad_unpack(buf, sign)
    const char *buf;
    int sign;
{
    VALUE big = bignew(QUAD_SIZE/SIZEOF_BDIGITS, 1);

    memcpy((char*)BDIGITS(big), buf, QUAD_SIZE);
    if (sign && BNEG(buf)) {
	long len = QUAD_SIZE;
	char *tmp = (char*)BDIGITS(big);

	RBIGNUM(big)->sign = 0;
	while (len--) {
	    *tmp = ~*tmp;
	    tmp++;
	}
    }

    return bignorm(big);
}

#endif

VALUE
rb_cstr_to_inum(str, base, badcheck)
    const char *str;
    int base;
    int badcheck;
{
    const char *s = str;
    char *end;
    char sign = 1, nondigit = 0;
    int c;
    BDIGIT_DBL num;
    long len, blen = 1;
    long i;
    VALUE z;
    BDIGIT *zds;

    if (!str) {
	if (badcheck) goto bad;
	return INT2FIX(0);
    }
    if (badcheck) {
	while (ISSPACE(*str)) str++;
    }
    else {
	while (ISSPACE(*str) || *str == '_') str++;
    }

    if (str[0] == '+') {
	str++;
    }
    else if (str[0] == '-') {
	str++;
	sign = 0;
    }
    if (str[0] == '+' || str[0] == '-') {
	if (badcheck) goto bad;
	return INT2FIX(0);
    }
    if (base <= 0) {
	if (str[0] == '0') {
	    switch (str[1]) {
	      case 'x': case 'X':
		base = 16;
		break;
	      case 'b': case 'B':
		base = 2;
		break;
	      case 'o': case 'O':
		base = 8;
		break;
	      case 'd': case 'D':
		base = 10;
		break;
	      default:
		base = 8;
	    }
	}
	else if (base < -1) {
	    base = -base;
	}
	else {
	    base = 10;
	}
    }
    switch (base) {
      case 2:
	len = 1;
	if (str[0] == '0' && (str[1] == 'b'||str[1] == 'B')) {
	    str += 2;
	}
	break;
      case 3:
	len = 2;
	break;
      case 8:
	if (str[0] == '0' && (str[1] == 'o'||str[1] == 'O')) {
	    str += 2;
	}
      case 4: case 5: case 6: case 7:
	len = 3;
	break;
      case 10:
	if (str[0] == '0' && (str[1] == 'd'||str[1] == 'D')) {
	    str += 2;
	}
      case 9: case 11: case 12: case 13: case 14: case 15:
	len = 4;
	break;
      case 16:
	len = 4;
	if (str[0] == '0' && (str[1] == 'x'||str[1] == 'X')) {
	    str += 2;
	}
	break;
      default:
	if (base < 2 || 36 < base) {
	    rb_raise(rb_eArgError, "illegal radix %d", base);
	}
	if (base <= 32) {
	    len = 5;
	}
	else {
	    len = 6;
	}
	break;
    }
    if (*str == '0') {		/* squeeze preceeding 0s */
	while (*++str == '0');
	--str;
    }
    len *= strlen(str)*sizeof(char);

    if (len <= (sizeof(VALUE)*CHAR_BIT)) {
	unsigned long val = strtoul((char*)str, &end, base);

	if (*end == '_') goto bigparse;
	if (badcheck) {
	    if (end == str) goto bad; /* no number */
	    while (*end && ISSPACE(*end)) end++;
	    if (*end) goto bad;	      /* trailing garbage */
	}

	if (POSFIXABLE(val)) {
	    if (sign) return LONG2FIX(val);
	    else {
		long result = -(long)val;
		return LONG2FIX(result);
	    }
	}
	else {
	    VALUE big = rb_uint2big(val);
	    RBIGNUM(big)->sign = sign;
	    return bignorm(big);
	}
    }
  bigparse:
    len = (len/BITSPERDIG)+1;
    if (badcheck && *str == '_') goto bad;

    z = bignew(len, sign);
    zds = BDIGITS(z);
    for (i=len;i--;) zds[i]=0;
    while (c = *str++) {
	if (c == '_') {
	    if (badcheck) {
		if (nondigit) goto bad;
		nondigit = c;
	    }
	    continue;
	}
	else if (!ISASCII(c)) {
	    break;
	}
	else if (isdigit(c)) {
	    c -= '0';
	}
	else if (islower(c)) {
	    c -= 'a' - 10;
	}
	else if (isupper(c)) {
	    c -= 'A' - 10;
	}
	else {
	    break;
	}
	if (c >= base) break;
	nondigit = 0;
	i = 0;
	num = c;
	for (;;) {
	    while (i<blen) {
		num += (BDIGIT_DBL)zds[i]*base;
		zds[i++] = BIGLO(num);
		num = BIGDN(num);
	    }
	    if (num) {
		blen++;
		continue;
	    }
	    break;
	}
    }
    if (badcheck) {
	str--;
	if (s+1 < str && str[-1] == '_') goto bad;
	while (*str && ISSPACE(*str)) str++;
	if (*str) {
	  bad:
	    rb_invalid_str(s, "Integer");
	}
    }

    return bignorm(z);
}

VALUE
rb_str_to_inum(str, base, badcheck)
    VALUE str;
    int base;
    int badcheck;
{
    char *s;
    long len;

    StringValue(str);
    if (badcheck) {
	s = StringValueCStr(str);
    }
    else {
	s = RSTRING(str)->ptr;
    }
    if (s) {
	len = RSTRING(str)->len;
	if (s[len]) {		/* no sentinel somehow */
	    char *p = ALLOCA_N(char, len+1);

	    MEMCPY(p, s, char, len);
	    p[len] = '\0';
	    s = p;
	}
    }
    return rb_cstr_to_inum(s, base, badcheck); 
}

#if HAVE_LONG_LONG

VALUE
rb_ull2big(n)
    unsigned LONG_LONG n;
{
    BDIGIT_DBL num = n;
    long i = 0;
    BDIGIT *digits;
    VALUE big;

    big = bignew(DIGSPERLL, 1);
    digits = BDIGITS(big);
    while (i < DIGSPERLL) {
	digits[i++] = BIGLO(num);
	num = BIGDN(num);
    }

    i = DIGSPERLL;
    while (i-- && !digits[i]) ;
    RBIGNUM(big)->len = i+1;
    return big;
}

VALUE
rb_ll2big(n)
    LONG_LONG n;
{
    long neg = 0;
    VALUE big;

    if (n < 0) {
	n = -n;
	neg = 1;
    }
    big = rb_ull2big(n);
    if (neg) {
	RBIGNUM(big)->sign = 0;
    }
    return big;
}

VALUE
rb_ull2inum(n)
    unsigned LONG_LONG n;
{
    if (POSFIXABLE(n)) return LONG2FIX(n);
    return rb_ull2big(n);
}

VALUE
rb_ll2inum(n)
    LONG_LONG n;
{
    if (FIXABLE(n)) return LONG2FIX(n);
    return rb_ll2big(n);
}

#endif  /* HAVE_LONG_LONG */
 
VALUE
rb_cstr2inum(str, base)
    const char *str;
    int base;
{
    return rb_cstr_to_inum(str, base, base==0);
}

VALUE
rb_str2inum(str, base)
    VALUE str;
    int base;
{
    return rb_str_to_inum(str, base, base==0);
}

const char ruby_digitmap[] = "0123456789abcdefghijklmnopqrstuvwxyz";
VALUE
rb_big2str(x, base)
    VALUE x;
    int base;
{
    volatile VALUE t;
    BDIGIT *ds;
    long i, j, hbase;
    VALUE ss;
    char *s, c;

    if (FIXNUM_P(x)) {
	return rb_fix2str(x, base);
    }
    i = RBIGNUM(x)->len;
    if (BIGZEROP(x)) {
	return rb_str_new2("0");
    }
    j = SIZEOF_BDIGITS*CHAR_BIT*i;
    switch (base) {
      case 2: break;
      case 3:
	j = j * 647L / 1024;
	break;
      case 4: case 5: case 6: case 7:
	j /= 2;
	break;
      case 8: case 9:
	j /= 3;
	break;
      case 10: case 11: case 12: case 13: case 14: case 15:
	j = j * 241L / 800;
	break;
      case 16: case 17: case 18: case 19: case 20: case 21:
      case 22: case 23: case 24: case 25: case 26: case 27:
      case 28: case 29: case 30: case 31:
	j /= 4;
	break;
      case 32: case 33: case 34: case 35: case 36:
	j /= 5;
	break;
      default:
	rb_raise(rb_eArgError, "illegal radix %d", base);
	break;
    }
    j += 2;

    hbase = base * base;
#if SIZEOF_BDIGITS > 2
    hbase *= hbase;
#endif

    t = rb_big_clone(x);
    ds = BDIGITS(t);
    ss = rb_str_new(0, j);
    s = RSTRING(ss)->ptr;

    s[0] = RBIGNUM(x)->sign ? '+' : '-';
    while (i && j) {
	long k = i;
	BDIGIT_DBL num = 0;

	while (k--) {
	    num = BIGUP(num) + ds[k];
	    ds[k] = (BDIGIT)(num / hbase);
	    num %= hbase;
	}
	if (ds[i-1] == 0) i--;
	k = SIZEOF_BDIGITS;
	while (k--) {
	    c = (char)(num % base);
	    s[--j] = ruby_digitmap[(int)c];
	    num /= base;
	    if (i == 0 && num == 0) break;
	}
    }
    while (s[j] == '0') j++;
    RSTRING(ss)->len -= RBIGNUM(x)->sign?j:j-1;
    memmove(RBIGNUM(x)->sign?s:s+1, s+j, RSTRING(ss)->len);
    s[RSTRING(ss)->len] = '\0';

    return ss;
}

/*
 *  call-seq:
 *     big.to_s(base=10)   =>  string
 *  
 *  Returns a string containing the representation of <i>big</i> radix
 *  <i>base</i> (2 through 36).
 *     
 *     12345654321.to_s         #=> "12345654321"
 *     12345654321.to_s(2)      #=> "1011011111110110111011110000110001"
 *     12345654321.to_s(8)      #=> "133766736061"
 *     12345654321.to_s(16)     #=> "2dfdbbc31"
 *     78546939656932.to_s(36)  #=> "rubyrules"
 */

static VALUE
rb_big_to_s(argc, argv, x)
    int argc;
    VALUE *argv;
    VALUE x;
{
    VALUE b;
    int base;

    rb_scan_args(argc, argv, "01", &b);
    if (argc == 0) base = 10;
    else base = NUM2INT(b);
    return rb_big2str(x, base);
}

static unsigned long
big2ulong(x, type, check)
    VALUE x;
    char *type;
    int check;
{
    long len = RBIGNUM(x)->len;
    BDIGIT_DBL num;
    BDIGIT *ds;

    if (len > SIZEOF_LONG/SIZEOF_BDIGITS) {
	if (check)
	    rb_raise(rb_eRangeError, "bignum too big to convert into `%s'", type);
	len = SIZEOF_LONG/SIZEOF_BDIGITS;
    }
    ds = BDIGITS(x);
    num = 0;
    while (len--) {
	num = BIGUP(num);
	num += ds[len];
    }
    return num;
}

unsigned long
rb_big2ulong_pack(x)
    VALUE x;
{
    unsigned long num = big2ulong(x, "unsigned long", Qfalse);
    if (!RBIGNUM(x)->sign) {
	return -num;
    }
    return num;
}

unsigned long
rb_big2ulong(x)
    VALUE x;
{
    unsigned long num = big2ulong(x, "unsigned long", Qtrue);

    if (!RBIGNUM(x)->sign) {
	if ((long)num < 0) {
	    rb_raise(rb_eRangeError, "bignum out of range of unsigned long");
	}
	return -num;
    }
    return num;
}

long
rb_big2long(x)
    VALUE x;
{
    unsigned long num = big2ulong(x, "long", Qtrue);

    if ((long)num < 0 && (RBIGNUM(x)->sign || (long)num != LONG_MIN)) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `long'");
    }
    if (!RBIGNUM(x)->sign) return -(long)num;
    return num;
}

#if HAVE_LONG_LONG

static unsigned LONG_LONG
big2ull(x, type)
    VALUE x;
    char *type;
{
    long len = RBIGNUM(x)->len;
    BDIGIT_DBL num;
    BDIGIT *ds;

    if (len > SIZEOF_LONG_LONG/SIZEOF_BDIGITS)
	rb_raise(rb_eRangeError, "bignum too big to convert into `%s'", type);
    ds = BDIGITS(x);
    num = 0;
    while (len--) {
	num = BIGUP(num);
	num += ds[len];
    }
    return num;
}

unsigned LONG_LONG
rb_big2ull(x)
    VALUE x;
{
    unsigned LONG_LONG num = big2ull(x, "unsigned long long");

    if (!RBIGNUM(x)->sign) return -num;
    return num;
}

LONG_LONG
rb_big2ll(x)
    VALUE x;
{
    unsigned LONG_LONG num = big2ull(x, "long long");

    if ((LONG_LONG)num < 0 && (RBIGNUM(x)->sign
			       || (LONG_LONG)num != LLONG_MIN)) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `long long'");
    }
    if (!RBIGNUM(x)->sign) return -(LONG_LONG)num;
    return num;
}

#endif  /* HAVE_LONG_LONG */

static VALUE
dbl2big(d)
    double d;
{
    long i = 0;
    BDIGIT c;
    BDIGIT *digits;
    VALUE z;
    double u = (d < 0)?-d:d;

    if (isinf(d)) {
	rb_raise(rb_eFloatDomainError, d < 0 ? "-Infinity" : "Infinity");
    }
    if (isnan(d)) {
	rb_raise(rb_eFloatDomainError, "NaN");
    }

    while (!POSFIXABLE(u) || 0 != (long)u) {
	u /= (double)(BIGRAD);
	i++;
    }
    z = bignew(i, d>=0);
    digits = BDIGITS(z);
    while (i--) {
	u *= BIGRAD;
	c = (BDIGIT)u;
	u -= c;
	digits[i] = c;
    }

    return z;
}

VALUE
rb_dbl2big(d)
    double d;
{
    return bignorm(dbl2big(d));
}

double
rb_big2dbl(x)
    VALUE x;
{
    double d = 0.0;
    long i = RBIGNUM(x)->len;
    BDIGIT *ds = BDIGITS(x);

    while (i--) {
	d = ds[i] + BIGRAD*d;
    }
    if (isinf(d)) {
	rb_warn("Bignum out of Float range");
	d = HUGE_VAL;
    }
    if (!RBIGNUM(x)->sign) d = -d;
    return d;
}

/*
 *  call-seq:
 *     big.to_f -> float
 *  
 *  Converts <i>big</i> to a <code>Float</code>. If <i>big</i> doesn't
 *  fit in a <code>Float</code>, the result is infinity.
 *     
 */

static VALUE
rb_big_to_f(x)
    VALUE x;
{
    return rb_float_new(rb_big2dbl(x));
}

/*
 *  call-seq:
 *     big <=> numeric   => -1, 0, +1
 *  
 *  Comparison---Returns -1, 0, or +1 depending on whether <i>big</i> is
 *  less than, equal to, or greater than <i>numeric</i>. This is the
 *  basis for the tests in <code>Comparable</code>.
 *     
 */

static VALUE
rb_big_cmp(x, y)
    VALUE x, y;
{
    long xlen = RBIGNUM(x)->len;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      case T_FLOAT:
	return rb_dbl_cmp(rb_big2dbl(x), RFLOAT(y)->value);

      default:
	return rb_num_coerce_cmp(x, y);
    }

    if (RBIGNUM(x)->sign > RBIGNUM(y)->sign) return INT2FIX(1);
    if (RBIGNUM(x)->sign < RBIGNUM(y)->sign) return INT2FIX(-1);
    if (xlen < RBIGNUM(y)->len)
	return (RBIGNUM(x)->sign) ? INT2FIX(-1) : INT2FIX(1);
    if (xlen > RBIGNUM(y)->len)
	return (RBIGNUM(x)->sign) ? INT2FIX(1) : INT2FIX(-1);

    while(xlen-- && (BDIGITS(x)[xlen]==BDIGITS(y)[xlen]));
    if (-1 == xlen) return INT2FIX(0);
    return (BDIGITS(x)[xlen] > BDIGITS(y)[xlen]) ?
	(RBIGNUM(x)->sign ? INT2FIX(1) : INT2FIX(-1)) :
	    (RBIGNUM(x)->sign ? INT2FIX(-1) : INT2FIX(1));
}

/*
 *  call-seq:
 *     big == obj  => true or false
 *  
 *  Returns <code>true</code> only if <i>obj</i> has the same value
 *  as <i>big</i>. Contrast this with <code>Bignum#eql?</code>, which
 *  requires <i>obj</i> to be a <code>Bignum</code>.
 *     
 *     68719476736 == 68719476736.0   #=> true
 */

static VALUE
rb_big_eq(x, y)
    VALUE x, y;
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;
      case T_BIGNUM:
	break;
      case T_FLOAT:
        {
	    volatile double a, b;

	    a = RFLOAT(y)->value;
	    b = rb_big2dbl(x);
	    if (isnan(a) || isnan(b)) return Qfalse;
	    return (a == b)?Qtrue:Qfalse;
	}
      default:
	return rb_equal(y, x);
    }
    if (RBIGNUM(x)->sign != RBIGNUM(y)->sign) return Qfalse;
    if (RBIGNUM(x)->len != RBIGNUM(y)->len) return Qfalse;
    if (MEMCMP(BDIGITS(x),BDIGITS(y),BDIGIT,RBIGNUM(y)->len) != 0) return Qfalse;
    return Qtrue;
}

/*
 *  call-seq:
 *     big.eql?(obj)   => true or false
 *  
 *  Returns <code>true</code> only if <i>obj</i> is a
 *  <code>Bignum</code> with the same value as <i>big</i>. Contrast this
 *  with <code>Bignum#==</code>, which performs type conversions.
 *     
 *     68719476736.eql?(68719476736.0)   #=> false
 */

static VALUE
rb_big_eql(x, y)
    VALUE x, y;
{
    if (TYPE(y) != T_BIGNUM) return Qfalse;
    if (RBIGNUM(x)->sign != RBIGNUM(y)->sign) return Qfalse;
    if (RBIGNUM(x)->len != RBIGNUM(y)->len) return Qfalse;
    if (MEMCMP(BDIGITS(x),BDIGITS(y),BDIGIT,RBIGNUM(y)->len) != 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    -big   =>  other_big
 *
 * Unary minus (returns a new Bignum whose value is 0-big)
 */

static VALUE
rb_big_uminus(x)
    VALUE x;
{
    VALUE z = rb_big_clone(x);

    RBIGNUM(z)->sign = !RBIGNUM(x)->sign;

    return bignorm(z);
}

/*
 * call-seq:
 *     ~big  =>  integer
 *
 * Inverts the bits in big. As Bignums are conceptually infinite
 * length, the result acts as if it had an infinite number of one
 * bits to the left. In hex representations, this is displayed
 * as two periods to the left of the digits.
 *  
 *   sprintf("%X", ~0x1122334455)    #=> "..FEEDDCCBBAA"
 */

static VALUE
rb_big_neg(x)
    VALUE x;
{
    VALUE z = rb_big_clone(x);
    long i = RBIGNUM(x)->len;
    BDIGIT *ds = BDIGITS(z);

    if (!RBIGNUM(x)->sign) get2comp(z, Qtrue);
    while (i--) ds[i] = ~ds[i];
    if (RBIGNUM(x)->sign) get2comp(z, Qfalse);
    RBIGNUM(z)->sign = !RBIGNUM(z)->sign;

    return bignorm(z);
}

static VALUE
bigsub(x, y)
    VALUE x, y;
{
    VALUE z = 0;
    BDIGIT *zds;
    BDIGIT_DBL_SIGNED num;
    long i = RBIGNUM(x)->len;
    
    /* if x is larger than y, swap */
    if (RBIGNUM(x)->len < RBIGNUM(y)->len) {
	z = x; x = y; y = z;	/* swap x y */
    }
    else if (RBIGNUM(x)->len == RBIGNUM(y)->len) {
	while (i > 0) {
	    i--;
	    if (BDIGITS(x)[i] > BDIGITS(y)[i]) {
		break;
	    }
	    if (BDIGITS(x)[i] < BDIGITS(y)[i]) {
		z = x; x = y; y = z;	/* swap x y */
		break;
	    }
	}
    }

    z = bignew(RBIGNUM(x)->len, (z == 0)?1:0);
    zds = BDIGITS(z);

    for (i = 0, num = 0; i < RBIGNUM(y)->len; i++) { 
	num += (BDIGIT_DBL_SIGNED)BDIGITS(x)[i] - BDIGITS(y)[i];
	zds[i] = BIGLO(num);
	num = BIGDN(num);
    } 
    while (num && i < RBIGNUM(x)->len) {
	num += BDIGITS(x)[i];
	zds[i++] = BIGLO(num);
	num = BIGDN(num);
    }
    while (i < RBIGNUM(x)->len) {
	zds[i] = BDIGITS(x)[i];
	i++;
    }
    
    return z;
}

static VALUE
bigadd(x, y, sign)
    VALUE x, y;
    char sign;
{
    VALUE z;
    BDIGIT_DBL num;
    long i, len;

    sign = (sign == RBIGNUM(y)->sign);
    if (RBIGNUM(x)->sign != sign) {
	if (sign) return bigsub(y, x);
	return bigsub(x, y);
    }

    if (RBIGNUM(x)->len > RBIGNUM(y)->len) {
	len = RBIGNUM(x)->len + 1;
        z = x; x = y; y = z;
    }
    else {
	len = RBIGNUM(y)->len + 1;
    }
    z = bignew(len, sign);

    len = RBIGNUM(x)->len;
    for (i = 0, num = 0; i < len; i++) {
	num += (BDIGIT_DBL)BDIGITS(x)[i] + BDIGITS(y)[i];
	BDIGITS(z)[i] = BIGLO(num);
	num = BIGDN(num);
    }
    len = RBIGNUM(y)->len;
    while (num && i < len) {
	num += BDIGITS(y)[i];
	BDIGITS(z)[i++] = BIGLO(num);
	num = BIGDN(num);
    }
    while (i < len) {
	BDIGITS(z)[i] = BDIGITS(y)[i];
	i++;
    }
    BDIGITS(z)[i] = (BDIGIT)num;

    return z;
}

/*
 *  call-seq:
 *     big + other  => Numeric
 *
 *  Adds big and other, returning the result.
 */

VALUE
rb_big_plus(x, y)
    VALUE x, y;
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	/* fall through */
      case T_BIGNUM:
	return bignorm(bigadd(x, y, 1));

      case T_FLOAT:
	return rb_float_new(rb_big2dbl(x) + RFLOAT(y)->value);

      default:
	return rb_num_coerce_bin(x, y);
    }
}

/*
 *  call-seq:
 *     big - other  => Numeric
 *
 *  Subtracts other from big, returning the result.
 */

VALUE
rb_big_minus(x, y)
    VALUE x, y;
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	/* fall through */
      case T_BIGNUM:
	return bignorm(bigadd(x, y, 0));

      case T_FLOAT:
	return rb_float_new(rb_big2dbl(x) - RFLOAT(y)->value);

      default:
	return rb_num_coerce_bin(x, y);
    }
}

/*
 *  call-seq:
 *     big * other  => Numeric
 *
 *  Multiplies big and other, returning the result.
 */

VALUE
rb_big_mul(x, y)
    VALUE x, y;
{
    long i, j;
    BDIGIT_DBL n = 0;
    VALUE z;
    BDIGIT *zds;

    if (FIXNUM_P(x)) x = rb_int2big(FIX2LONG(x));
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      case T_FLOAT:
	return rb_float_new(rb_big2dbl(x) * RFLOAT(y)->value);

      default:
	return rb_num_coerce_bin(x, y);
    }

    j = RBIGNUM(x)->len + RBIGNUM(y)->len + 1;
    z = bignew(j, RBIGNUM(x)->sign==RBIGNUM(y)->sign);
    zds = BDIGITS(z);
    while (j--) zds[j] = 0;
    for (i = 0; i < RBIGNUM(x)->len; i++) {
	BDIGIT_DBL dd = BDIGITS(x)[i]; 
	if (dd == 0) continue;
	n = 0;
	for (j = 0; j < RBIGNUM(y)->len; j++) {
	    BDIGIT_DBL ee = n + (BDIGIT_DBL)dd * BDIGITS(y)[j];
	    n = zds[i + j] + ee;
	    if (ee) zds[i + j] = BIGLO(n);
	    n = BIGDN(n);
	}
	if (n) {
	    zds[i + j] = n;
	}
    }

    return bignorm(z);
}

static void
bigdivrem(x, y, divp, modp)
    VALUE x, y;
    VALUE *divp, *modp;
{
    long nx = RBIGNUM(x)->len, ny = RBIGNUM(y)->len;
    long i, j;
    VALUE yy, z;
    BDIGIT *xds, *yds, *zds, *tds;
    BDIGIT_DBL t2;
    BDIGIT_DBL_SIGNED num;
    BDIGIT dd, q;

    if (BIGZEROP(y)) rb_num_zerodiv();
    yds = BDIGITS(y);
    if (nx < ny || (nx == ny && BDIGITS(x)[nx - 1] < BDIGITS(y)[ny - 1])) {
	if (divp) *divp = rb_int2big(0);
	if (modp) *modp = x;
	return;
    }
    xds = BDIGITS(x);
    if (ny == 1) {
	dd = yds[0];
	z = rb_big_clone(x);
	zds = BDIGITS(z);
	t2 = 0; i = nx;
	while (i--) {
	    t2 = BIGUP(t2) + zds[i];
	    zds[i] = (BDIGIT)(t2 / dd);
	    t2 %= dd;
	}
	RBIGNUM(z)->sign = RBIGNUM(x)->sign==RBIGNUM(y)->sign;
	if (modp) {
	    *modp = rb_uint2big((unsigned long)t2);
	    RBIGNUM(*modp)->sign = RBIGNUM(x)->sign;
	}
	if (divp) *divp = z;
	return;
    }
    z = bignew(nx==ny?nx+2:nx+1, RBIGNUM(x)->sign==RBIGNUM(y)->sign);
    zds = BDIGITS(z);
    if (nx==ny) zds[nx+1] = 0;
    while (!yds[ny-1]) ny--;

    dd = 0;
    q = yds[ny-1];
    while ((q & (1<<(BITSPERDIG-1))) == 0) {
	q <<= 1;
	dd++;
    }
    if (dd) {
	yy = rb_big_clone(y);
	tds = BDIGITS(yy);
	j = 0;
	t2 = 0;
	while (j<ny) {
	    t2 += (BDIGIT_DBL)yds[j]<<dd;
	    tds[j++] = BIGLO(t2);
	    t2 = BIGDN(t2);
	}
	yds = tds;
	j = 0;
	t2 = 0;
	while (j<nx) {
	    t2 += (BDIGIT_DBL)xds[j]<<dd;
	    zds[j++] = BIGLO(t2);
	    t2 = BIGDN(t2);
	}
	zds[j] = (BDIGIT)t2;
    }
    else {
	zds[nx] = 0;
	j = nx;
	while (j--) zds[j] = xds[j];
    }

    j = nx==ny?nx+1:nx;
    do {
	if (zds[j] ==  yds[ny-1]) q = BIGRAD-1;
	else q = (BDIGIT)((BIGUP(zds[j]) + zds[j-1])/yds[ny-1]);
	if (q) {
	    i = 0; num = 0; t2 = 0;
	    do {			/* multiply and subtract */
		BDIGIT_DBL ee;
		t2 += (BDIGIT_DBL)yds[i] * q;
		ee = num - BIGLO(t2);
		num = (BDIGIT_DBL)zds[j - ny + i] + ee;
		if (ee) zds[j - ny + i] = BIGLO(num);
		num = BIGDN(num);
		t2 = BIGDN(t2);
	    } while (++i < ny);
	    num += zds[j - ny + i] - t2;/* borrow from high digit; don't update */
	    while (num) {		/* "add back" required */
		i = 0; num = 0; q--;
		do {
		    BDIGIT_DBL ee = num + yds[i];
		    num = (BDIGIT_DBL)zds[j - ny + i] + ee;
		    if (ee) zds[j - ny + i] = BIGLO(num);
		    num = BIGDN(num);
		} while (++i < ny);
		num--;
	    }
	}
	zds[j] = q;
    } while (--j >= ny);
    if (divp) {			/* move quotient down in z */
	*divp = rb_big_clone(z);
	zds = BDIGITS(*divp);
	j = (nx==ny ? nx+2 : nx+1) - ny;
	for (i = 0;i < j;i++) zds[i] = zds[i+ny];
	RBIGNUM(*divp)->len = i;
    }
    if (modp) {			/* normalize remainder */
	*modp = rb_big_clone(z);
	zds = BDIGITS(*modp);
	while (--ny && !zds[ny]); ++ny;
	if (dd) {
	    t2 = 0; i = ny;
	    while(i--) {
		t2 = (t2 | zds[i]) >> dd;
		q = zds[i];
		zds[i] = BIGLO(t2);
		t2 = BIGUP(q);
	    }
	}
	RBIGNUM(*modp)->len = ny;
	RBIGNUM(*modp)->sign = RBIGNUM(x)->sign;
    }
}

static void
bigdivmod(x, y, divp, modp)
    VALUE x, y;
    VALUE *divp, *modp;
{
    VALUE mod;

    bigdivrem(x, y, divp, &mod);
    if (RBIGNUM(x)->sign != RBIGNUM(y)->sign && !BIGZEROP(mod)) {
	if (divp) *divp = bigadd(*divp, rb_int2big(1), 0);
	if (modp) *modp = bigadd(mod, y, 1);
    }
    else {
	if (divp) *divp = *divp;
	if (modp) *modp = mod;
    }
}

/*
 *  call-seq:
 *     big / other     => Numeric
 *     big.div(other)  => Numeric
 *
 *  Divides big by other, returning the result.
 */

static VALUE
rb_big_div(x, y)
    VALUE x, y;
{
    VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      case T_FLOAT:
	return rb_float_new(rb_big2dbl(x) / RFLOAT(y)->value);

      default:
	return rb_num_coerce_bin(x, y);
    }
    bigdivmod(x, y, &z, 0);

    return bignorm(z);
}

/*
 *  call-seq:
 *     big % other         => Numeric
 *     big.modulo(other)   => Numeric
 *
 *  Returns big modulo other. See Numeric.divmod for more
 *  information.
 */

static VALUE
rb_big_modulo(x, y)
    VALUE x, y;
{
    VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	return rb_num_coerce_bin(x, y);
    }
    bigdivmod(x, y, 0, &z);

    return bignorm(z);
}

/*
 *  call-seq:
 *     big.remainder(numeric)    => number
 *  
 *  Returns the remainder after dividing <i>big</i> by <i>numeric</i>.
 *     
 *     -1234567890987654321.remainder(13731)      #=> -6966
 *     -1234567890987654321.remainder(13731.24)   #=> -9906.22531493148
 */
static VALUE
rb_big_remainder(x, y)
    VALUE x, y;
{
    VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	return rb_num_coerce_bin(x, y);
    }
    bigdivrem(x, y, 0, &z);

    return bignorm(z);
}

/*
 *  call-seq:
 *     big.divmod(numeric)   => array
 *  
 *  See <code>Numeric#divmod</code>.
 *     
 */
VALUE
rb_big_divmod(x, y)
    VALUE x, y;
{
    VALUE div, mod;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	return rb_num_coerce_bin(x, y);
    }
    bigdivmod(x, y, &div, &mod);

    return rb_assoc_new(bignorm(div), bignorm(mod));
}

/*
 *  call-seq:
 *     big.quo(numeric) -> float
 *  
 *  Returns the floating point result of dividing <i>big</i> by
 *  <i>numeric</i>.
 *     
 *     -1234567890987654321.quo(13731)      #=> -89910996357705.5
 *     -1234567890987654321.quo(13731.24)   #=> -89909424858035.7
 *     
 */

static VALUE
rb_big_quo(x, y)
    VALUE x, y;
{
    double dx = rb_big2dbl(x);
    double dy;

    switch (TYPE(y)) {
      case T_FIXNUM:
	dy = (double)FIX2LONG(y);
	break;

      case T_BIGNUM:
	dy = rb_big2dbl(y);
	break;

      case T_FLOAT:
	dy = RFLOAT(y)->value;
	break;

      default:
	return rb_num_coerce_bin(x, y);
    }
    return rb_float_new(dx / dy);
}

/*
 *  call-seq:
 *     big ** exponent   #=> numeric
 *
 *  Raises _big_ to the _exponent_ power (which may be an integer, float,
 *  or anything that will coerce to a number). The result may be
 *  a Fixnum, Bignum, or Float
 *
 *    123456789 ** 2      #=> 15241578750190521
 *    123456789 ** 1.2    #=> 5126464716.09932
 *    123456789 ** -2     #=> 6.5610001194102e-17
 */

VALUE
rb_big_pow(x, y)
    VALUE x, y;
{
    double d;
    long yy;
    
    if (y == INT2FIX(0)) return INT2FIX(1);
    switch (TYPE(y)) {
      case T_FLOAT:
	d = RFLOAT(y)->value;
	break;

      case T_BIGNUM:
	rb_warn("in a**b, b may be too big");
	d = rb_big2dbl(y);
	break;

      case T_FIXNUM:
	yy = FIX2LONG(y);
	if (yy > 0) {
	    VALUE z = x;

	    for (;;) {
		yy -= 1;
		if (yy == 0) break;
		while (yy % 2 == 0) {
		    yy /= 2;
		    x = rb_big_mul(x, x);
		}
		z = rb_big_mul(z, x);
	    }
	    return bignorm(z);
	}
	d = (double)yy;
	break;

      default:
	return rb_num_coerce_bin(x, y);
    }
    return rb_float_new(pow(rb_big2dbl(x), d));
}

/*
 * call-seq:
 *     big & numeric   =>  integer
 *
 * Performs bitwise +and+ between _big_ and _numeric_.
 */

VALUE
rb_big_and(xx, yy)
    VALUE xx, yy;
{
    volatile VALUE x, y, z;
    BDIGIT *ds1, *ds2, *zds;
    long i, l1, l2;
    char sign;

    x = xx;
    y = rb_to_int(yy);
    if (FIXNUM_P(y)) {
	y = rb_int2big(FIX2LONG(y));
    }
    if (!RBIGNUM(y)->sign) {
	y = rb_big_clone(y);
	get2comp(y, Qtrue);
    }
    if (!RBIGNUM(x)->sign) {
	x = rb_big_clone(x);
	get2comp(x, Qtrue);
    }
    if (RBIGNUM(x)->len > RBIGNUM(y)->len) {
	l1 = RBIGNUM(y)->len;
	l2 = RBIGNUM(x)->len;
	ds1 = BDIGITS(y);
	ds2 = BDIGITS(x);
	sign = RBIGNUM(y)->sign;
    }
    else {
	l1 = RBIGNUM(x)->len;
	l2 = RBIGNUM(y)->len;
	ds1 = BDIGITS(x);
	ds2 = BDIGITS(y);
	sign = RBIGNUM(x)->sign;
    }
    z = bignew(l2, RBIGNUM(x)->sign || RBIGNUM(y)->sign);
    zds = BDIGITS(z);

    for (i=0; i<l1; i++) {
	zds[i] = ds1[i] & ds2[i];
    }
    for (; i<l2; i++) {
	zds[i] = sign?0:ds2[i];
    }
    if (!RBIGNUM(z)->sign) get2comp(z, Qfalse);
    return bignorm(z);
}

/*
 * call-seq:
 *     big | numeric   =>  integer
 *
 * Performs bitwise +or+ between _big_ and _numeric_.
 */

VALUE
rb_big_or(xx, yy)
    VALUE xx, yy;
{
    volatile VALUE x, y, z;
    BDIGIT *ds1, *ds2, *zds;
    long i, l1, l2;
    char sign;

    x = xx;
    y = rb_to_int(yy);
    if (FIXNUM_P(y)) {
	y = rb_int2big(FIX2LONG(y));
    }

    if (!RBIGNUM(y)->sign) {
	y = rb_big_clone(y);
	get2comp(y, Qtrue);
    }
    if (!RBIGNUM(x)->sign) {
	x = rb_big_clone(x);
	get2comp(x, Qtrue);
    }
    if (RBIGNUM(x)->len > RBIGNUM(y)->len) {
	l1 = RBIGNUM(y)->len;
	l2 = RBIGNUM(x)->len;
	ds1 = BDIGITS(y);
	ds2 = BDIGITS(x);
	sign = RBIGNUM(y)->sign;
    }
    else {
	l1 = RBIGNUM(x)->len;
	l2 = RBIGNUM(y)->len;
	ds1 = BDIGITS(x);
	ds2 = BDIGITS(y);
	sign = RBIGNUM(x)->sign;
    }
    z = bignew(l2, RBIGNUM(x)->sign && RBIGNUM(y)->sign);
    zds = BDIGITS(z);

    for (i=0; i<l1; i++) {
	zds[i] = ds1[i] | ds2[i];
    }
    for (; i<l2; i++) {
	zds[i] = sign?ds2[i]:(BIGRAD-1);
    }
    if (!RBIGNUM(z)->sign) get2comp(z, Qfalse);

    return bignorm(z);
}

/*
 * call-seq:
 *     big ^ numeric   =>  integer
 *
 * Performs bitwise +exclusive or+ between _big_ and _numeric_.
 */

VALUE
rb_big_xor(xx, yy)
    VALUE xx, yy;
{
    volatile VALUE x, y;
    VALUE z;
    BDIGIT *ds1, *ds2, *zds;
    long i, l1, l2;
    char sign;

    x = xx;
    y = rb_to_int(yy);
    if (FIXNUM_P(y)) {
	y = rb_int2big(FIX2LONG(y));
    }

    if (!RBIGNUM(y)->sign) {
	y = rb_big_clone(y);
	get2comp(y, Qtrue);
    }
    if (!RBIGNUM(x)->sign) {
	x = rb_big_clone(x);
	get2comp(x, Qtrue);
    }
    if (RBIGNUM(x)->len > RBIGNUM(y)->len) {
	l1 = RBIGNUM(y)->len;
	l2 = RBIGNUM(x)->len;
	ds1 = BDIGITS(y);
	ds2 = BDIGITS(x);
	sign = RBIGNUM(y)->sign;
    }
    else {
	l1 = RBIGNUM(x)->len;
	l2 = RBIGNUM(y)->len;
	ds1 = BDIGITS(x);
	ds2 = BDIGITS(y);
	sign = RBIGNUM(x)->sign;
    }
    RBIGNUM(x)->sign = RBIGNUM(x)->sign?1:0;
    RBIGNUM(y)->sign = RBIGNUM(y)->sign?1:0;
    z = bignew(l2, !(RBIGNUM(x)->sign ^ RBIGNUM(y)->sign));
    zds = BDIGITS(z);

    for (i=0; i<l1; i++) {
	zds[i] = ds1[i] ^ ds2[i];
    }
    for (; i<l2; i++) {
	zds[i] = sign?ds2[i]:~ds2[i];
    }
    if (!RBIGNUM(z)->sign) get2comp(z, Qfalse);

    return bignorm(z);
}

static VALUE rb_big_rshift _((VALUE,VALUE));

/*
 * call-seq:
 *     big << numeric   =>  integer
 *
 * Shifts big left _numeric_ positions (right if _numeric_ is negative).
 */

VALUE
rb_big_lshift(x, y)
    VALUE x, y;
{
    BDIGIT *xds, *zds;
    int shift = NUM2INT(y);
    int s1 = shift/BITSPERDIG;
    int s2 = shift%BITSPERDIG;
    VALUE z;
    BDIGIT_DBL num = 0;
    long len, i;

    if (shift < 0) return rb_big_rshift(x, INT2FIX(-shift));
    len = RBIGNUM(x)->len;
    z = bignew(len+s1+1, RBIGNUM(x)->sign);
    zds = BDIGITS(z);
    for (i=0; i<s1; i++) {
	*zds++ = 0;
    }
    xds = BDIGITS(x);
    for (i=0; i<len; i++) {
	num = num | (BDIGIT_DBL)*xds++<<s2;
	*zds++ = BIGLO(num);
	num = BIGDN(num);
    }
    *zds = BIGLO(num);
    return bignorm(z);
}

/*
 * call-seq:
 *     big >> numeric   =>  integer
 *
 * Shifts big right _numeric_ positions (left if _numeric_ is negative).
 */

static VALUE
rb_big_rshift(x, y)
    VALUE x, y;
{
    BDIGIT *xds, *zds;
    int shift = NUM2INT(y);
    long s1 = shift/BITSPERDIG;
    long s2 = shift%BITSPERDIG;
    VALUE z;
    BDIGIT_DBL num = 0;
    long i, j;

    if (shift < 0) return rb_big_lshift(x, INT2FIX(-shift));

    if (s1 > RBIGNUM(x)->len) {
	if (RBIGNUM(x)->sign)
	    return INT2FIX(0);
	else
	    return INT2FIX(-1);
    }
    if (!RBIGNUM(x)->sign) {
	x = rb_big_clone(x);
	get2comp(x, Qtrue);
    }
    xds = BDIGITS(x);
    i = RBIGNUM(x)->len; j = i - s1;
    z = bignew(j, RBIGNUM(x)->sign);
    if (!RBIGNUM(x)->sign) {
	num = ((BDIGIT_DBL)~0) << BITSPERDIG;
    }
    zds = BDIGITS(z);
    while (i--, j--) {
	num = (num | xds[i]) >> s2;
	zds[j] = BIGLO(num);
	num = BIGUP(xds[i]);
    }
    if (!RBIGNUM(x)->sign) {
	get2comp(z, Qfalse);
    }
    return bignorm(z);
}

/*
 *  call-seq:
 *     big[n] -> 0, 1
 *  
 *  Bit Reference---Returns the <em>n</em>th bit in the (assumed) binary
 *  representation of <i>big</i>, where <i>big</i>[0] is the least
 *  significant bit.
 *     
 *     a = 9**15
 *     50.downto(0) do |n|
 *       print a[n]
 *     end
 *     
 *  <em>produces:</em>
 *     
 *     000101110110100000111000011110010100111100010111001
 *     
 */

static VALUE
rb_big_aref(x, y)
    VALUE x, y;
{
    BDIGIT *xds;
    int shift;
    long s1, s2;

    if (TYPE(y) == T_BIGNUM) {
	if (!RBIGNUM(y)->sign || RBIGNUM(x)->sign)
	    return INT2FIX(0);
	return INT2FIX(1);
    }
    shift = NUM2INT(y);
    if (shift < 0) return INT2FIX(0);
    s1 = shift/BITSPERDIG;
    s2 = shift%BITSPERDIG;

    if (!RBIGNUM(x)->sign) {
	if (s1 >= RBIGNUM(x)->len) return INT2FIX(1);
	x = rb_big_clone(x);
	get2comp(x, Qtrue);
    }
    else {
	if (s1 >= RBIGNUM(x)->len) return INT2FIX(0);
    }
    xds = BDIGITS(x);
    if (xds[s1] & (1<<s2))
	return INT2FIX(1);
    return INT2FIX(0);
}

/*
 * call-seq:
 *   big.hash   => fixnum
 *
 * Compute a hash based on the value of _big_.
 */

static VALUE
rb_big_hash(x)
    VALUE x;
{
    long i, len, key;
    BDIGIT *digits;

    key = 0; digits = BDIGITS(x); len = RBIGNUM(x)->len;
    for (i=0; i<len; i++) {
	key ^= *digits++;
    }
    return LONG2FIX(key);
}

/*
 * MISSING: documentation
 */

static VALUE
rb_big_coerce(x, y)
    VALUE x, y;
{
    if (FIXNUM_P(y)) {
	return rb_assoc_new(rb_int2big(FIX2LONG(y)), x);
    }
    else {
	rb_raise(rb_eTypeError, "can't coerce %s to Bignum",
		 rb_obj_classname(y));
    }
    /* not reached */
    return Qnil;
}

/*
 *  call-seq:
 *     big.abs -> aBignum
 *  
 *  Returns the absolute value of <i>big</i>.
 *     
 *     -1234567890987654321.abs   #=> 1234567890987654321
 */

static VALUE
rb_big_abs(x)
    VALUE x;
{
    if (!RBIGNUM(x)->sign) {
	x = rb_big_clone(x);
	RBIGNUM(x)->sign = 1;
    }
    return x;
}

VALUE
rb_big_rand(max, rand_buf)
    VALUE max;
    double *rand_buf;
{
    VALUE v;
    long len = RBIGNUM(max)->len;

    if (BIGZEROP(max)) {
	return rb_float_new(rand_buf[0]);
    }
    v = bignew(len,1);
    len--;
    BDIGITS(v)[len] = BDIGITS(max)[len] * rand_buf[len];    
    while (len--) {
	BDIGITS(v)[len] = ((BDIGIT)~0) * rand_buf[len];
    }

    return v;
}

/*
 *  call-seq:
 *     big.size -> integer
 *  
 *  Returns the number of bytes in the machine representation of
 *  <i>big</i>.
 *     
 *     (256**10 - 1).size   #=> 12
 *     (256**20 - 1).size   #=> 20
 *     (256**40 - 1).size   #=> 40
 */

static VALUE
rb_big_size(big)
    VALUE big;
{
    return LONG2FIX(RBIGNUM(big)->len*SIZEOF_BDIGITS);
}

/*
 *  Bignum objects hold integers outside the range of
 *  Fixnum. Bignum objects are created
 *  automatically when integer calculations would otherwise overflow a
 *  Fixnum. When a calculation involving
 *  Bignum objects returns a result that will fit in a
 *  Fixnum, the result is automatically converted.
 *     
 *  For the purposes of the bitwise operations and <code>[]</code>, a
 *  Bignum is treated as if it were an infinite-length
 *  bitstring with 2's complement representation.
 *     
 *  While Fixnum values are immediate, Bignum
 *  objects are not---assignment and parameter passing work with
 *  references to objects, not the objects themselves.
 *     
 */

void
Init_Bignum()
{
    rb_cBignum = rb_define_class("Bignum", rb_cInteger);

    rb_define_method(rb_cBignum, "to_s", rb_big_to_s, -1);
    rb_define_method(rb_cBignum, "coerce", rb_big_coerce, 1);
    rb_define_method(rb_cBignum, "-@", rb_big_uminus, 0);
    rb_define_method(rb_cBignum, "+", rb_big_plus, 1);
    rb_define_method(rb_cBignum, "-", rb_big_minus, 1);
    rb_define_method(rb_cBignum, "*", rb_big_mul, 1);
    rb_define_method(rb_cBignum, "/", rb_big_div, 1);
    rb_define_method(rb_cBignum, "%", rb_big_modulo, 1);
    rb_define_method(rb_cBignum, "div", rb_big_div, 1);
    rb_define_method(rb_cBignum, "divmod", rb_big_divmod, 1);
    rb_define_method(rb_cBignum, "modulo", rb_big_modulo, 1);
    rb_define_method(rb_cBignum, "remainder", rb_big_remainder, 1);
    rb_define_method(rb_cBignum, "quo", rb_big_quo, 1);
    rb_define_method(rb_cBignum, "**", rb_big_pow, 1);
    rb_define_method(rb_cBignum, "&", rb_big_and, 1);
    rb_define_method(rb_cBignum, "|", rb_big_or, 1);
    rb_define_method(rb_cBignum, "^", rb_big_xor, 1);
    rb_define_method(rb_cBignum, "~", rb_big_neg, 0);
    rb_define_method(rb_cBignum, "<<", rb_big_lshift, 1);
    rb_define_method(rb_cBignum, ">>", rb_big_rshift, 1);
    rb_define_method(rb_cBignum, "[]", rb_big_aref, 1);

    rb_define_method(rb_cBignum, "<=>", rb_big_cmp, 1);
    rb_define_method(rb_cBignum, "==", rb_big_eq, 1);
    rb_define_method(rb_cBignum, "eql?", rb_big_eql, 1);
    rb_define_method(rb_cBignum, "hash", rb_big_hash, 0);
    rb_define_method(rb_cBignum, "to_f", rb_big_to_f, 0);
    rb_define_method(rb_cBignum, "abs", rb_big_abs, 0);
    rb_define_method(rb_cBignum, "size", rb_big_size, 0);
}
/**********************************************************************

  class.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Tue Aug 10 15:05:44 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto

**********************************************************************/

#include "ruby.h"
#include "rubysig.h"
#include "node.h"
#include "st.h"
#include <ctype.h>

extern st_table *rb_class_tbl;

VALUE
rb_class_boot(super)
    VALUE super;
{
    NEWOBJ(klass, struct RClass);
    OBJSETUP(klass, rb_cClass, T_CLASS);

    klass->super = super;
    klass->iv_tbl = 0;
    klass->m_tbl = 0;		/* safe GC */
    klass->m_tbl = st_init_numtable();

    OBJ_INFECT(klass, super);
    return (VALUE)klass;
}

void
rb_check_inheritable(super)
    VALUE super;
{
    if (TYPE(super) != T_CLASS) {
	rb_raise(rb_eTypeError, "superclass must be a Class (%s given)",
		 rb_obj_classname(super));
    }
    if (RBASIC(super)->flags & FL_SINGLETON) {
	rb_raise(rb_eTypeError, "can't make subclass of singleton class");
    }
}

VALUE
rb_class_new(super)
    VALUE super;
{
    Check_Type(super, T_CLASS);
    rb_check_inheritable(super);
    if (super == rb_cClass) {
	rb_raise(rb_eTypeError, "can't make subclass of Class");
    }
    return rb_class_boot(super);
}

static int
clone_method(mid, body, tbl)
    ID mid;
    NODE *body;
    st_table *tbl;
{
    st_insert(tbl, mid, (st_data_t)NEW_METHOD(body->nd_body, body->nd_noex));
    return ST_CONTINUE;
}

/* :nodoc: */
VALUE
rb_mod_init_copy(clone, orig)
    VALUE clone, orig;
{
    rb_obj_init_copy(clone, orig);
    if (!FL_TEST(CLASS_OF(clone), FL_SINGLETON)) {
	RBASIC(clone)->klass = rb_singleton_class_clone(orig);
    }
    RCLASS(clone)->super = RCLASS(orig)->super;
    if (RCLASS(orig)->iv_tbl) {
	ID id;

	RCLASS(clone)->iv_tbl = st_copy(RCLASS(orig)->iv_tbl);
	id = rb_intern("__classpath__");
	st_delete(RCLASS(clone)->iv_tbl, (st_data_t*)&id, 0);
	id = rb_intern("__classid__");
	st_delete(RCLASS(clone)->iv_tbl, (st_data_t*)&id, 0);
    }
    if (RCLASS(orig)->m_tbl) {
	RCLASS(clone)->m_tbl = st_init_numtable();
	st_foreach(RCLASS(orig)->m_tbl, clone_method,
	  (st_data_t)RCLASS(clone)->m_tbl);
    }

    return clone;
}

/* :nodoc: */
VALUE
rb_class_init_copy(clone, orig)
    VALUE clone, orig;
{
    if (RCLASS(clone)->super != 0) {
	rb_raise(rb_eTypeError, "already initialized class");
    }
    return rb_mod_init_copy(clone, orig);
}

VALUE
rb_singleton_class_clone(obj)
    VALUE obj;
{
    VALUE klass = RBASIC(obj)->klass;

    if (!FL_TEST(klass, FL_SINGLETON))
	return klass;
    else {
	/* copy singleton(unnamed) class */
	NEWOBJ(clone, struct RClass);
	OBJSETUP(clone, 0, RBASIC(klass)->flags);

	if (BUILTIN_TYPE(obj) == T_CLASS) {
	    RBASIC(clone)->klass = (VALUE)clone;
	}
	else {
	    RBASIC(clone)->klass = rb_singleton_class_clone(klass);
	}

	clone->super = RCLASS(klass)->super;
	clone->iv_tbl = 0;
	clone->m_tbl = 0;
	if (RCLASS(klass)->iv_tbl) {
	    clone->iv_tbl = st_copy(RCLASS(klass)->iv_tbl);
	}
	clone->m_tbl = st_init_numtable();
	st_foreach(RCLASS(klass)->m_tbl, clone_method,
	  (st_data_t)clone->m_tbl);
	rb_singleton_class_attached(RBASIC(clone)->klass, (VALUE)clone);
	FL_SET(clone, FL_SINGLETON);
	return (VALUE)clone;
    }
}

void
rb_singleton_class_attached(klass, obj)
    VALUE klass, obj;
{
    if (FL_TEST(klass, FL_SINGLETON)) {
	if (!RCLASS(klass)->iv_tbl) {
	    RCLASS(klass)->iv_tbl = st_init_numtable();
	}
	st_insert(RCLASS(klass)->iv_tbl, rb_intern("__attached__"), obj);
    }
}

VALUE
rb_make_metaclass(obj, super)
    VALUE obj, super;
{
    if (BUILTIN_TYPE(obj) == T_CLASS && FL_TEST(obj, FL_SINGLETON)) {
	return RBASIC(obj)->klass = rb_cClass;
    }
    else {
	VALUE metasuper;
	VALUE klass = rb_class_boot(super);

	FL_SET(klass, FL_SINGLETON);
	RBASIC(obj)->klass = klass;
	rb_singleton_class_attached(klass, obj);

	metasuper = RBASIC(rb_class_real(super))->klass;
	/* metaclass of a superclass may be NULL at boot time */
	if (metasuper) {
	    RBASIC(klass)->klass = metasuper;
	}
	return klass;
    }
}

VALUE
rb_define_class_id(id, super)
    ID id;
    VALUE super;
{
    VALUE klass;

    if (!super) super = rb_cObject;
    klass = rb_class_new(super);
    rb_make_metaclass(klass, RBASIC(super)->klass);

    return klass;
}

VALUE
rb_class_inherited(super, klass)
    VALUE super, klass;
{
    if (!super) super = rb_cObject;
    return rb_funcall(super, rb_intern("inherited"), 1, klass);
}

VALUE
rb_define_class(name, super)
    const char *name;
    VALUE super;
{
    VALUE klass;
    ID id;

    id = rb_intern(name);
    if (rb_const_defined(rb_cObject, id)) {
	klass = rb_const_get(rb_cObject, id);
	if (TYPE(klass) != T_CLASS) {
	    rb_raise(rb_eTypeError, "%s is not a class", name);
	}
	if (rb_class_real(RCLASS(klass)->super) != super) {
	    rb_name_error(id, "%s is already defined", name);
	}
	return klass;
    }
    if (!super) {
	rb_warn("no super class for `%s', Object assumed", name);
    }
    klass = rb_define_class_id(id, super);
    st_add_direct(rb_class_tbl, id, klass);
    rb_name_class(klass, id);
    rb_const_set(rb_cObject, id, klass);
    rb_class_inherited(super, klass);

    return klass;
}

VALUE
rb_define_class_under(outer, name, super)
    VALUE outer;
    const char *name;
    VALUE super;
{
    VALUE klass;
    ID id;

    id = rb_intern(name);
    if (rb_const_defined_at(outer, id)) {
	klass = rb_const_get_at(outer, id);
	if (TYPE(klass) != T_CLASS) {
	    rb_raise(rb_eTypeError, "%s is not a class", name);
	}
	if (rb_class_real(RCLASS(klass)->super) != super) {
	    rb_name_error(id, "%s is already defined", name);
	}
	return klass;
    }
    if (!super) {
	rb_warn("no super class for `%s::%s', Object assumed",
		rb_class2name(outer), name);
    }
    klass = rb_define_class_id(id, super);
    rb_set_class_path(klass, outer, name);
    rb_const_set(outer, id, klass);
    rb_class_inherited(super, klass);

    return klass;
}

VALUE
rb_module_new()
{
    NEWOBJ(mdl, struct RClass);
    OBJSETUP(mdl, rb_cModule, T_MODULE);

    mdl->super = 0;
    mdl->iv_tbl = 0;
    mdl->m_tbl = 0;
    mdl->m_tbl = st_init_numtable();

    return (VALUE)mdl;
}

VALUE
rb_define_module_id(id)
    ID id;
{
    VALUE mdl;

    mdl = rb_module_new();
    rb_name_class(mdl, id);

    return mdl;
}

VALUE
rb_define_module(name)
    const char *name;
{
    VALUE module;
    ID id;

    id = rb_intern(name);
    if (rb_const_defined(rb_cObject, id)) {
	module = rb_const_get(rb_cObject, id);
	if (TYPE(module) == T_MODULE)
	    return module;
	rb_raise(rb_eTypeError, "%s is not a module", rb_obj_classname(module));
    }
    module = rb_define_module_id(id);
    st_add_direct(rb_class_tbl, id, module);
    rb_const_set(rb_cObject, id, module);

    return module;
}

VALUE
rb_define_module_under(outer, name)
    VALUE outer;
    const char *name;
{
    VALUE module;
    ID id;

    id = rb_intern(name);
    if (rb_const_defined_at(outer, id)) {
	module = rb_const_get_at(outer, id);
	if (TYPE(module) == T_MODULE)
	    return module;
	rb_raise(rb_eTypeError, "%s::%s is not a module",
		 rb_class2name(outer), rb_obj_classname(module));
    }
    module = rb_define_module_id(id);
    rb_const_set(outer, id, module);
    rb_set_class_path(module, outer, name);

    return module;
}

static VALUE
include_class_new(module, super)
    VALUE module, super;
{
    NEWOBJ(klass, struct RClass);
    OBJSETUP(klass, rb_cClass, T_ICLASS);

    if (BUILTIN_TYPE(module) == T_ICLASS) {
	module = RBASIC(module)->klass;
    }
    if (!RCLASS(module)->iv_tbl) {
	RCLASS(module)->iv_tbl = st_init_numtable();
    }
    klass->iv_tbl = RCLASS(module)->iv_tbl;
    klass->m_tbl = RCLASS(module)->m_tbl;
    klass->super = super;
    if (TYPE(module) == T_ICLASS) {
	RBASIC(klass)->klass = RBASIC(module)->klass;
    }
    else {
	RBASIC(klass)->klass = module;
    }
    OBJ_INFECT(klass, module);
    OBJ_INFECT(klass, super);

    return (VALUE)klass;
}

void
rb_include_module(klass, module)
    VALUE klass, module;
{
    VALUE p, c;
    int changed = 0;

    rb_frozen_class_p(klass);
    if (!OBJ_TAINTED(klass)) {
	rb_secure(4);
    }
    
    if (NIL_P(module)) return;
    if (klass == module) return;

    if (TYPE(module) != T_MODULE) {
	Check_Type(module, T_MODULE);
    }

    OBJ_INFECT(klass, module);
    c = klass;
    while (module) {
	int superclass_seen = Qfalse;

	if (RCLASS(klass)->m_tbl == RCLASS(module)->m_tbl)
	    rb_raise(rb_eArgError, "cyclic include detected");
	/* ignore if the module included already in superclasses */
	for (p = RCLASS(klass)->super; p; p = RCLASS(p)->super) {
	    switch (BUILTIN_TYPE(p)) {
	      case T_ICLASS:
		if (RCLASS(p)->m_tbl == RCLASS(module)->m_tbl) {
		    if (!superclass_seen) {
			c = p;	/* move insertion point */
		    }
		    goto skip;
		}
		break;
	      case T_CLASS:
		superclass_seen = Qtrue;
		break;
	    }
	}
	c = RCLASS(c)->super = include_class_new(module, RCLASS(c)->super);
	changed = 1;
      skip:
	module = RCLASS(module)->super;
    }
    if (changed) rb_clear_cache();
}

/*
 *  call-seq:
 *     mod.included_modules -> array
 *  
 *  Returns the list of modules included in <i>mod</i>.
 *     
 *     module Mixin
 *     end
 *     
 *     module Outer
 *       include Mixin
 *     end
 *     
 *     Mixin.included_modules   #=> []
 *     Outer.included_modules   #=> [Mixin]
 */

VALUE
rb_mod_included_modules(mod)
    VALUE mod;
{
    VALUE ary = rb_ary_new();
    VALUE p;

    for (p = RCLASS(mod)->super; p; p = RCLASS(p)->super) {
	if (BUILTIN_TYPE(p) == T_ICLASS) {
	    rb_ary_push(ary, RBASIC(p)->klass);
	}
    }
    return ary;
}

/*
 *  call-seq:
 *     mod.include?(module)    => true or false
 *  
 *  Returns <code>true</code> if <i>module</i> is included in
 *  <i>mod</i> or one of <i>mod</i>'s ancestors.
 *     
 *     module A
 *     end
 *     class B
 *       include A
 *     end
 *     class C < B
 *     end
 *     B.include?(A)   #=> true
 *     C.include?(A)   #=> true
 *     A.include?(A)   #=> false
 */

VALUE
rb_mod_include_p(mod, mod2)
    VALUE mod;
    VALUE mod2;
{
    VALUE p;

    Check_Type(mod2, T_MODULE);
    for (p = RCLASS(mod)->super; p; p = RCLASS(p)->super) {
	if (BUILTIN_TYPE(p) == T_ICLASS) {
	    if (RBASIC(p)->klass == mod2) return Qtrue;
	}
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     mod.ancestors -> array
 *  
 *  Returns a list of modules included in <i>mod</i> (including
 *  <i>mod</i> itself).
 *     
 *     module Mod
 *       include Math
 *       include Comparable
 *     end
 *     
 *     Mod.ancestors    #=> [Mod, Comparable, Math]
 *     Math.ancestors   #=> [Math]
 */

VALUE
rb_mod_ancestors(mod)
    VALUE mod;
{
    VALUE p, ary = rb_ary_new();

    for (p = mod; p; p = RCLASS(p)->super) {
	if (FL_TEST(p, FL_SINGLETON))
	    continue;
	if (BUILTIN_TYPE(p) == T_ICLASS) {
	    rb_ary_push(ary, RBASIC(p)->klass);
	}
	else {
	    rb_ary_push(ary, p);
	}
    }
    return ary;
}

#define VISI(x) ((x)&NOEX_MASK)
#define VISI_CHECK(x,f) (VISI(x) == (f))

static int
ins_methods_push(name, type, ary, visi)
    ID name;
    long type;
    VALUE ary;
    long visi;
{
    if (type == -1) return ST_CONTINUE;
    switch (visi) {
      case NOEX_PRIVATE:
      case NOEX_PROTECTED:
      case NOEX_PUBLIC:
	visi = (type == visi);
	break;
      default:
	visi = (type != NOEX_PRIVATE);
	break;
    }
    if (visi) {
	rb_ary_push(ary, rb_str_new2(rb_id2name(name)));
    }
    return ST_CONTINUE;
}

static int
ins_methods_i(name, type, ary)
    ID name;
    long type;
    VALUE ary;
{
    return ins_methods_push(name, type, ary, -1); /* everything but private */
}

static int
ins_methods_prot_i(name, type, ary)
    ID name;
    long type;
    VALUE ary;
{
    return ins_methods_push(name, type, ary, NOEX_PROTECTED);
}

static int
ins_methods_priv_i(name, type, ary)
    ID name;
    long type;
    VALUE ary;
{
    return ins_methods_push(name, type, ary, NOEX_PRIVATE);
}

static int
ins_methods_pub_i(name, type, ary)
    ID name;
    long type;
    VALUE ary;
{
    return ins_methods_push(name, type, ary, NOEX_PUBLIC);
}

static int
method_entry(key, body, list)
    ID key;
    NODE *body;
    st_table *list;
{
    long type;

    if (key == ID_ALLOCATOR) return ST_CONTINUE;
    if (!st_lookup(list, key, 0)) {
	if (!body->nd_body) type = -1; /* none */
	else type = VISI(body->nd_noex);
	st_add_direct(list, key, type);
    }
    return ST_CONTINUE;
}

static VALUE
class_instance_method_list(argc, argv, mod, func)
    int argc;
    VALUE *argv;
    VALUE mod;
    int (*func) _((ID, long, VALUE));
{
    VALUE ary;
    int recur;
    st_table *list;

    if (argc == 0) {
	recur = Qtrue;
    }
    else {
	VALUE r;
	rb_scan_args(argc, argv, "01", &r);
	recur = RTEST(r);
    }

    list = st_init_numtable();
    for (; mod; mod = RCLASS(mod)->super) {
	st_foreach(RCLASS(mod)->m_tbl, method_entry, (st_data_t)list);
	if (BUILTIN_TYPE(mod) == T_ICLASS) continue;
	if (FL_TEST(mod, FL_SINGLETON)) continue;
	if (!recur) break;
    }
    ary = rb_ary_new();
    st_foreach(list, func, ary);
    st_free_table(list);

    return ary;
}

/*
 *  call-seq:
 *     mod.instance_methods(include_super=true)   => array
 *  
 *  Returns an array containing the names of public instance methods in
 *  the receiver. For a module, these are the public methods; for a
 *  class, they are the instance (not singleton) methods. With no
 *  argument, or with an argument that is <code>false</code>, the
 *  instance methods in <i>mod</i> are returned, otherwise the methods
 *  in <i>mod</i> and <i>mod</i>'s superclasses are returned.
 *     
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       def method2()  end
 *     end
 *     class C < B
 *       def method3()  end
 *     end
 *     
 *     A.instance_methods                #=> ["method1"]
 *     B.instance_methods(false)         #=> ["method2"]
 *     C.instance_methods(false)         #=> ["method3"]
 *     C.instance_methods(true).length   #=> 43
 */

VALUE
rb_class_instance_methods(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    return class_instance_method_list(argc, argv, mod, ins_methods_i);
}

/*
 *  call-seq:
 *     mod.protected_instance_methods(include_super=true)   => array
 *  
 *  Returns a list of the protected instance methods defined in
 *  <i>mod</i>. If the optional parameter is not <code>false</code>, the
 *  methods of any ancestors are included.
 */

VALUE
rb_class_protected_instance_methods(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    return class_instance_method_list(argc, argv, mod, ins_methods_prot_i);
}

/*
 *  call-seq:
 *     mod.private_instance_methods(include_super=true)    => array
 *  
 *  Returns a list of the private instance methods defined in
 *  <i>mod</i>. If the optional parameter is not <code>false</code>, the
 *  methods of any ancestors are included.
 *     
 *     module Mod
 *       def method1()  end
 *       private :method1
 *       def method2()  end
 *     end
 *     Mod.instance_methods           #=> ["method2"]
 *     Mod.private_instance_methods   #=> ["method1"]
 */

VALUE
rb_class_private_instance_methods(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    return class_instance_method_list(argc, argv, mod, ins_methods_priv_i);
}

/*
 *  call-seq:
 *     mod.public_instance_methods(include_super=true)   => array
 *  
 *  Returns a list of the public instance methods defined in <i>mod</i>.
 *  If the optional parameter is not <code>false</code>, the methods of
 *  any ancestors are included.
 */

VALUE
rb_class_public_instance_methods(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    return class_instance_method_list(argc, argv, mod, ins_methods_pub_i);
}

/*
 *  call-seq:
 *     obj.singleton_methods(all=true)    => array
 *  
 *  Returns an array of the names of singleton methods for <i>obj</i>.
 *  If the optional <i>all</i> parameter is true, the list will include
 *  methods in modules included in <i>obj</i>.
 *     
 *     module Other
 *       def three() end
 *     end
 *     
 *     class Single
 *       def Single.four() end
 *     end
 *     
 *     a = Single.new
 *     
 *     def a.one()
 *     end
 *     
 *     class << a
 *       include Other
 *       def two()
 *       end
 *     end
 *     
 *     Single.singleton_methods    #=> ["four"]
 *     a.singleton_methods(false)  #=> ["two", "one"]
 *     a.singleton_methods         #=> ["two", "one", "three"]
 */

VALUE
rb_obj_singleton_methods(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE recur, ary, klass;
    st_table *list;

    rb_scan_args(argc, argv, "01", &recur);
    if (argc == 0) {
	recur = Qtrue;
    }
    klass = CLASS_OF(obj);
    list = st_init_numtable();
    if (klass && FL_TEST(klass, FL_SINGLETON)) {
	st_foreach(RCLASS(klass)->m_tbl, method_entry, (st_data_t)list);
	klass = RCLASS(klass)->super;
    }
    if (RTEST(recur)) {
	while (klass && (FL_TEST(klass, FL_SINGLETON) || TYPE(klass) == T_ICLASS)) {
	    st_foreach(RCLASS(klass)->m_tbl, method_entry, (st_data_t)list);
	    klass = RCLASS(klass)->super;
	}
    }
    ary = rb_ary_new();
    st_foreach(list, ins_methods_i, ary);
    st_free_table(list);

    return ary;
}

void
rb_define_method_id(klass, name, func, argc)
    VALUE klass;
    ID name;
    VALUE (*func)();
    int argc;
{
    rb_add_method(klass, name, NEW_CFUNC(func,argc), NOEX_PUBLIC);
}

void
rb_define_method(klass, name, func, argc)
    VALUE klass;
    const char *name;
    VALUE (*func)();
    int argc;
{
    rb_add_method(klass, rb_intern(name), NEW_CFUNC(func, argc), NOEX_PUBLIC);
}

void
rb_define_protected_method(klass, name, func, argc)
    VALUE klass;
    const char *name;
    VALUE (*func)();
    int argc;
{
    rb_add_method(klass, rb_intern(name), NEW_CFUNC(func, argc), NOEX_PROTECTED);
}

void
rb_define_private_method(klass, name, func, argc)
    VALUE klass;
    const char *name;
    VALUE (*func)();
    int argc;
{
    rb_add_method(klass, rb_intern(name), NEW_CFUNC(func, argc), NOEX_PRIVATE);
}

void
rb_undef_method(klass, name)
    VALUE klass;
    const char *name;
{
    rb_add_method(klass, rb_intern(name), 0, NOEX_UNDEF);
}

#define SPECIAL_SINGLETON(x,c) do {\
    if (obj == (x)) {\
	return c;\
    }\
} while (0)

VALUE
rb_singleton_class(obj)
    VALUE obj;
{
    VALUE klass;

    if (FIXNUM_P(obj) || SYMBOL_P(obj)) {
	rb_raise(rb_eTypeError, "can't define singleton");
    }
    if (rb_special_const_p(obj)) {
	SPECIAL_SINGLETON(Qnil, rb_cNilClass);
	SPECIAL_SINGLETON(Qfalse, rb_cFalseClass);
	SPECIAL_SINGLETON(Qtrue, rb_cTrueClass);
	rb_bug("unknown immediate %ld", obj);
    }

    DEFER_INTS;
    if (FL_TEST(RBASIC(obj)->klass, FL_SINGLETON) &&
	rb_iv_get(RBASIC(obj)->klass, "__attached__") == obj) {
	klass = RBASIC(obj)->klass;
    }
    else {
	klass = rb_make_metaclass(obj, RBASIC(obj)->klass);
    }
    if (OBJ_TAINTED(obj)) {
	OBJ_TAINT(klass);
    }
    else {
	FL_UNSET(klass, FL_TAINT);
    }
    if (OBJ_FROZEN(obj)) OBJ_FREEZE(klass);
    ALLOW_INTS;

    return klass;
}

void
rb_define_singleton_method(obj, name, func, argc)
    VALUE obj;
    const char *name;
    VALUE (*func)();
    int argc;
{
    rb_define_method(rb_singleton_class(obj), name, func, argc);
}

void
rb_define_module_function(module, name, func, argc)
    VALUE module;
    const char *name;
    VALUE (*func)();
    int argc;
{
    rb_define_private_method(module, name, func, argc);
    rb_define_singleton_method(module, name, func, argc);
}

void
rb_define_global_function(name, func, argc)
    const char *name;
    VALUE (*func)();
    int argc;
{
    rb_define_module_function(rb_mKernel, name, func, argc);
}

void
rb_define_alias(klass, name1, name2)
    VALUE klass;
    const char *name1, *name2;
{
    rb_alias(klass, rb_intern(name1), rb_intern(name2));
}

void
rb_define_attr(klass, name, read, write)
    VALUE klass;
    const char *name;
    int read, write;
{
    rb_attr(klass, rb_intern(name), read, write, Qfalse);
}

#ifdef HAVE_STDARG_PROTOTYPES
#include <stdarg.h>
#define va_init_list(a,b) va_start(a,b)
#else
#include <varargs.h>
#define va_init_list(a,b) va_start(a)
#endif

int
#ifdef HAVE_STDARG_PROTOTYPES
rb_scan_args(int argc, const VALUE *argv, const char *fmt, ...)
#else
rb_scan_args(argc, argv, fmt, va_alist)
    int argc;
    const VALUE *argv;
    const char *fmt;
    va_dcl
#endif
{
    int n, i = 0;
    const char *p = fmt;
    VALUE *var;
    va_list vargs;

    va_init_list(vargs, fmt);

    if (*p == '*') goto rest_arg;

    if (ISDIGIT(*p)) {
	n = *p - '0';
	if (n > argc)
	    rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)", argc, n);
	for (i=0; i<n; i++) {
	    var = va_arg(vargs, VALUE*);
	    if (var) *var = argv[i];
	}
	p++;
    }
    else {
	goto error;
    }

    if (ISDIGIT(*p)) {
	n = i + *p - '0';
	for (; i<n; i++) {
	    var = va_arg(vargs, VALUE*);
	    if (argc > i) {
		if (var) *var = argv[i];
	    }
	    else {
		if (var) *var = Qnil;
	    }
	}
	p++;
    }

    if(*p == '*') {
      rest_arg:
	var = va_arg(vargs, VALUE*);
	if (argc > i) {
	    if (var) *var = rb_ary_new4(argc-i, argv+i);
	    i = argc;
	}
	else {
	    if (var) *var = rb_ary_new();
	}
	p++;
    }

    if (*p == '&') {
	var = va_arg(vargs, VALUE*);
	if (rb_block_given_p()) {
	    *var = rb_block_proc();
	}
	else {
	    *var = Qnil;
	}
	p++;
    }
    va_end(vargs);

    if (*p != '\0') {
	goto error;
    }

    if (argc > i) {
	rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)", argc, i);
    }

    return argc;

  error:
    rb_fatal("bad scan arg format: %s", fmt);
    return 0;
}
/**********************************************************************

  compar.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Thu Aug 26 14:39:48 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto

**********************************************************************/

#include "ruby.h"

VALUE rb_mComparable;

static ID cmp;

int
rb_cmpint(val, a, b)
    VALUE val, a, b;
{
    if (NIL_P(val)) {
	rb_cmperr(a, b);
    }
    if (FIXNUM_P(val)) return FIX2INT(val);
    if (TYPE(val) == T_BIGNUM) {
	if (RBIGNUM(val)->sign) return 1;
	return -1;
    }
    if (RTEST(rb_funcall(val, '>', 1, INT2FIX(0)))) return 1;
    if (RTEST(rb_funcall(val, '<', 1, INT2FIX(0)))) return -1;
    return 0;
}

void
rb_cmperr(x, y)
    VALUE x, y;
{
    const char *classname;

    if (SPECIAL_CONST_P(y)) {
	y = rb_inspect(y);
	classname = StringValuePtr(y);
    }
    else {
	classname = rb_obj_classname(y);
    }
    rb_raise(rb_eArgError, "comparison of %s with %s failed",
	     rb_obj_classname(x), classname);
}

static VALUE
cmp_eq(a)
    VALUE *a;
{
    VALUE c = rb_funcall(a[0], cmp, 1, a[1]);

    if (NIL_P(c)) return Qnil;
    if (rb_cmpint(c, a[0], a[1]) == 0) return Qtrue;
    return Qfalse;
}

static VALUE
cmp_failed()
{
    return Qnil;
}

/*
 *  call-seq:
 *     obj == other    => true or false
 *  
 *  Compares two objects based on the receiver's <code><=></code>
 *  method, returning true if it returns 0. Also returns true if
 *  _obj_ and _other_ are the same object.
 */

static VALUE
cmp_equal(x, y)
    VALUE x, y;
{
    VALUE a[2];

    if (x == y) return Qtrue;

    a[0] = x; a[1] = y;
    return rb_rescue(cmp_eq, (VALUE)a, cmp_failed, 0);
}

/*
 *  call-seq:
 *     obj > other    => true or false
 *  
 *  Compares two objects based on the receiver's <code><=></code>
 *  method, returning true if it returns 1.
 */

static VALUE
cmp_gt(x, y)
    VALUE x, y;
{
    VALUE c = rb_funcall(x, cmp, 1, y);

    if (rb_cmpint(c, x, y) > 0) return Qtrue;
    return Qfalse;
}

/*
 *  call-seq:
 *     obj >= other    => true or false
 *  
 *  Compares two objects based on the receiver's <code><=></code>
 *  method, returning true if it returns 0 or 1.
 */

static VALUE
cmp_ge(x, y)
    VALUE x, y;
{
    VALUE c = rb_funcall(x, cmp, 1, y);

    if (rb_cmpint(c, x, y) >= 0) return Qtrue;
    return Qfalse;
}

/*
 *  call-seq:
 *     obj < other    => true or false
 *  
 *  Compares two objects based on the receiver's <code><=></code>
 *  method, returning true if it returns -1.
 */

static VALUE
cmp_lt(x, y)
    VALUE x, y;
{
    VALUE c = rb_funcall(x, cmp, 1, y);

    if (rb_cmpint(c, x, y) < 0) return Qtrue;
    return Qfalse;
}


/*
 *  call-seq:
 *     obj <= other    => true or false
 *  
 *  Compares two objects based on the receiver's <code><=></code>
 *  method, returning true if it returns -1 or 0.
 */

static VALUE
cmp_le(x, y)
    VALUE x, y;
{
    VALUE c = rb_funcall(x, cmp, 1, y);

    if (rb_cmpint(c, x, y) <= 0) return Qtrue;
    return Qfalse;
}

/*
 *  call-seq:
 *     obj.between?(min, max)    => true or false
 *  
 *  Returns <code>false</code> if <i>obj</i> <code><=></code>
 *  <i>min</i> is less than zero or if <i>anObject</i> <code><=></code>
 *  <i>max</i> is greater than zero, <code>true</code> otherwise.
 *     
 *     3.between?(1, 5)               #=> true
 *     6.between?(1, 5)               #=> false
 *     'cat'.between?('ant', 'dog')   #=> true
 *     'gnu'.between?('ant', 'dog')   #=> false
 *     
 */

static VALUE
cmp_between(x, min, max)
    VALUE x, min, max;
{
    if (RTEST(cmp_lt(x, min))) return Qfalse;
    if (RTEST(cmp_gt(x, max))) return Qfalse;
    return Qtrue;
}

/*
 *  The <code>Comparable</code> mixin is used by classes whose objects
 *  may be ordered. The class must define the <code><=></code> operator,
 *  which compares the receiver against another object, returning -1, 0,
 *  or +1 depending on whether the receiver is less than, equal to, or
 *  greater than the other object. <code>Comparable</code> uses
 *  <code><=></code> to implement the conventional comparison operators
 *  (<code><</code>, <code><=</code>, <code>==</code>, <code>>=</code>,
 *  and <code>></code>) and the method <code>between?</code>.
 *     
 *     class SizeMatters
 *       include Comparable
 *       attr :str
 *       def <=>(anOther)
 *         str.size <=> anOther.str.size
 *       end
 *       def initialize(str)
 *         @str = str
 *       end
 *       def inspect
 *         @str
 *       end
 *     end
 *     
 *     s1 = SizeMatters.new("Z")
 *     s2 = SizeMatters.new("YY")
 *     s3 = SizeMatters.new("XXX")
 *     s4 = SizeMatters.new("WWWW")
 *     s5 = SizeMatters.new("VVVVV")
 *     
 *     s1 < s2                       #=> true
 *     s4.between?(s1, s3)           #=> false
 *     s4.between?(s3, s5)           #=> true
 *     [ s3, s2, s5, s4, s1 ].sort   #=> [Z, YY, XXX, WWWW, VVVVV]
 *     
 */

void
Init_Comparable()
{
    rb_mComparable = rb_define_module("Comparable");
    rb_define_method(rb_mComparable, "==", cmp_equal, 1);
    rb_define_method(rb_mComparable, ">", cmp_gt, 1);
    rb_define_method(rb_mComparable, ">=", cmp_ge, 1);
    rb_define_method(rb_mComparable, "<", cmp_lt, 1);
    rb_define_method(rb_mComparable, "<=", cmp_le, 1);
    rb_define_method(rb_mComparable, "between?", cmp_between, 2);

    cmp = rb_intern("<=>");
}
/**********************************************************************

  dir.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Wed Jan  5 09:51:01 JST 1994

  Copyright (C) 1993-2003 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#include "ruby.h"

#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined HAVE_DIRENT_H && !defined _WIN32
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#elif defined HAVE_DIRECT_H && !defined _WIN32
# include <direct.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# if !defined __NeXT__
#  define NAMLEN(dirent) (dirent)->d_namlen
# else
#  /* On some versions of NextStep, d_namlen is always zero, so avoid it. */
#  define NAMLEN(dirent) strlen((dirent)->d_name)
# endif
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
# ifdef _WIN32
#  include "win32/dir.h"
# endif
#endif

#include <errno.h>

#ifndef HAVE_STDLIB_H
char *getenv();
#endif

#ifndef HAVE_STRING_H
char *strchr _((char*,char));
#endif

#include <ctype.h>

#include "util.h"

#if !defined HAVE_LSTAT && !defined lstat
#define lstat stat
#endif

#define FNM_NOESCAPE	0x01
#define FNM_PATHNAME	0x02
#define FNM_DOTMATCH	0x04
#define FNM_CASEFOLD	0x08

#define FNM_NOMATCH	1
#define FNM_ERROR	2

#define downcase(c) (nocase && ISUPPER(c) ? tolower(c) : (c))
#define compare(c1, c2) (((unsigned char)(c1)) - ((unsigned char)(c2)))

/* caution: in case *p == '\0'
   Next(p) == p + 1 in single byte environment
   Next(p) == p     in multi byte environment
*/
#if defined(CharNext)
# define Next(p) CharNext(p)
#elif defined(DJGPP)
# define Next(p) ((p) + mblen(p, RUBY_MBCHAR_MAXSIZE))
#elif defined(__EMX__)
# define Next(p) ((p) + emx_mblen(p))
static inline int
emx_mblen(p)
    const char *p;
{
    int n = mblen(p, RUBY_MBCHAR_MAXSIZE);
    return (n < 0) ? 1 : n;
}
#endif

#ifndef Next /* single byte environment */
# define Next(p) ((p) + 1)
# define Inc(p) (++(p))
# define Compare(p1, p2) (compare(downcase(*(p1)), downcase(*(p2))))
#else /* multi byte environment */
# define Inc(p) ((p) = Next(p))
# define Compare(p1, p2) (CompareImpl(p1, p2, nocase))
static int
CompareImpl(p1, p2, nocase)
    const char *p1;
    const char *p2;
    int nocase;
{
    const int len1 = Next(p1) - p1;
    const int len2 = Next(p2) - p2;
#ifdef _WIN32
    char buf1[10], buf2[10]; /* large enough? */
#endif

    if (len1 < 0 || len2 < 0) {
	rb_fatal("CompareImpl: negative len");
    }

    if (len1 == 0) return  len2;
    if (len2 == 0) return -len1;

#ifdef _WIN32
    if (nocase) {
	if (len1 > 1) {
	    if (len1 >= sizeof(buf1)) {
		rb_fatal("CompareImpl: too large len");
	    }
	    memcpy(buf1, p1, len1);
	    buf1[len1] = '\0';
	    CharLower(buf1);
	    p1 = buf1; /* trick */
	}
	if (len2 > 1) {
	    if (len2 >= sizeof(buf2)) {
		rb_fatal("CompareImpl: too large len");
	    }
	    memcpy(buf2, p2, len2);
	    buf2[len2] = '\0';
	    CharLower(buf2);
	    p2 = buf2; /* trick */
	}
    }
#endif
    if (len1 == 1)
	if (len2 == 1)
	    return compare(downcase(*p1), downcase(*p2));
	else {
	    const int ret = compare(downcase(*p1), *p2);
	    return ret ? ret : -1;
	}
    else
	if (len2 == 1) {
	    const int ret = compare(*p1, downcase(*p2));
	    return ret ? ret : 1;
	}
	else {
	    const int ret = memcmp(p1, p2, len1 < len2 ? len1 : len2);
	    return ret ? ret : len1 - len2;
	}
}
#endif /* environment */

static char *
bracket(p, s, flags)
    const char *p; /* pattern (next to '[') */
    const char *s; /* string */
    int flags;
{
    const int nocase = flags & FNM_CASEFOLD;
    const int escape = !(flags & FNM_NOESCAPE);

    int ok = 0, not = 0;

    if (*p == '!' || *p == '^') {
	not = 1;
	p++;
    }

    while (*p != ']') {
	const char *t1 = p;
	if (escape && *t1 == '\\')
	    t1++;
	if (!*t1)
	    return NULL;
	p = Next(t1);
	if (p[0] == '-' && p[1] != ']') {
	    const char *t2 = p + 1;
	    if (escape && *t2 == '\\')
		t2++;
	    if (!*t2)
		return NULL;
	    p = Next(t2);
	    if (!ok && Compare(t1, s) <= 0 && Compare(s, t2) <= 0)
		ok = 1;
	}
	else
	    if (!ok && Compare(t1, s) == 0)
		ok = 1;
    }

    return ok == not ? NULL : (char *)p + 1;
}

/* If FNM_PATHNAME is set, only path element will be matched. (upto '/' or '\0')
   Otherwise, entire string will be matched.
   End marker itself won't be compared.
   And if function succeeds, *pcur reaches end marker.
*/
#define UNESCAPE(p) (escape && *(p) == '\\' ? (p) + 1 : (p))
#define ISEND(p) (!*(p) || (pathname && *(p) == '/'))
#define RETURN(val) return *pcur = p, *scur = s, (val);

static int
fnmatch_helper(pcur, scur, flags)
    const char **pcur; /* pattern */
    const char **scur; /* string */
    int flags;
{
    const int period = !(flags & FNM_DOTMATCH);
    const int pathname = flags & FNM_PATHNAME;
    const int escape = !(flags & FNM_NOESCAPE);
    const int nocase = flags & FNM_CASEFOLD;

    const char *ptmp = 0;
    const char *stmp = 0;

    const char *p = *pcur;
    const char *s = *scur;

    if (period && *s == '.' && *UNESCAPE(p) != '.') /* leading period */
	RETURN(FNM_NOMATCH);

    while (1) {
	switch (*p) {
	  case '*':
	    do { p++; } while (*p == '*');
	    if (ISEND(UNESCAPE(p))) {
		p = UNESCAPE(p);
		RETURN(0);
	    }
	    if (ISEND(s))
		RETURN(FNM_NOMATCH);
	    ptmp = p;
	    stmp = s;
	    continue;

	  case '?':
	    if (ISEND(s))
		RETURN(FNM_NOMATCH);
	    p++;
	    Inc(s);
	    continue;

	  case '[': {
	    const char *t;
	    if (ISEND(s))
		RETURN(FNM_NOMATCH);
	    if (t = bracket(p + 1, s, flags)) {
		p = t;
		Inc(s);
		continue;
	    }
	    goto failed;
	  }
	}

	/* ordinary */
	p = UNESCAPE(p);
	if (ISEND(s))
	    RETURN(ISEND(p) ? 0 : FNM_NOMATCH);
	if (ISEND(p))
	    goto failed;
	if (Compare(p, s) != 0)
	    goto failed;
	Inc(p);
	Inc(s);
	continue;

      failed: /* try next '*' position */
	if (ptmp && stmp) {
	    p = ptmp;
	    Inc(stmp); /* !ISEND(*stmp) */
	    s = stmp;
	    continue;
	}
	RETURN(FNM_NOMATCH);
    }
}

static int
fnmatch(p, s, flags)
    const char *p; /* pattern */
    const char *s; /* string */
    int flags;
{
    const int period = !(flags & FNM_DOTMATCH);
    const int pathname = flags & FNM_PATHNAME;

    const char *ptmp = 0;
    const char *stmp = 0;

    if (pathname) {
	while (1) {
	    if (p[0] == '*' && p[1] == '*' && p[2] == '/') {
		do { p += 3; } while (p[0] == '*' && p[1] == '*' && p[2] == '/');
		ptmp = p;
		stmp = s;
	    }
	    if (fnmatch_helper(&p, &s, flags) == 0) {
		while (*s && *s != '/') Inc(s);
		if (*p && *s) {
		    p++;
		    s++;
		    continue;
		}
		if (!*p && !*s)
		    return 0;
	    }
	    /* failed : try next recursion */
	    if (ptmp && stmp && !(period && *stmp == '.')) {
		while (*stmp && *stmp != '/') Inc(stmp);
		if (*stmp) {
		    p = ptmp;
		    stmp++;
		    s = stmp;
		    continue;
		}
	    }
	    return FNM_NOMATCH;
	}
    }
    else
	return fnmatch_helper(&p, &s, flags);
}

VALUE rb_cDir;

struct dir_data {
    DIR *dir;
    char *path;
};

static void
free_dir(dir)
    struct dir_data *dir;
{
    if (dir) {
	if (dir->dir) closedir(dir->dir);
	if (dir->path) free(dir->path);
    }
    free(dir);
}

static VALUE dir_close _((VALUE));

static VALUE dir_s_alloc _((VALUE));
static VALUE
dir_s_alloc(klass)
    VALUE klass;
{
    struct dir_data *dirp;
    VALUE obj = Data_Make_Struct(klass, struct dir_data, 0, free_dir, dirp);

    dirp->dir = NULL;
    dirp->path = NULL;

    return obj;
}

/*
 *  call-seq:
 *     Dir.new( string ) -> aDir
 *
 *  Returns a new directory object for the named directory.
 */
static VALUE
dir_initialize(dir, dirname)
    VALUE dir, dirname;
{
    struct dir_data *dp;

    FilePathValue(dirname);
    Data_Get_Struct(dir, struct dir_data, dp);
    if (dp->dir) closedir(dp->dir);
    if (dp->path) free(dp->path);
    dp->dir = NULL;
    dp->path = NULL;
    dp->dir = opendir(RSTRING(dirname)->ptr);
    if (dp->dir == NULL) {
	if (errno == EMFILE || errno == ENFILE) {
	    rb_gc();
	    dp->dir = opendir(RSTRING(dirname)->ptr);
	}
	if (dp->dir == NULL) {
	    rb_sys_fail(RSTRING(dirname)->ptr);
	}
    }
    dp->path = strdup(RSTRING(dirname)->ptr);

    return dir;
}

/*
 *  call-seq:
 *     Dir.open( string ) => aDir
 *     Dir.open( string ) {| aDir | block } => anObject
 *
 *  With no block, <code>open</code> is a synonym for
 *  <code>Dir::new</code>. If a block is present, it is passed
 *  <i>aDir</i> as a parameter. The directory is closed at the end of
 *  the block, and <code>Dir::open</code> returns the value of the
 *  block.
 */
static VALUE
dir_s_open(klass, dirname)
    VALUE klass, dirname;
{
    struct dir_data *dp;
    VALUE dir = Data_Make_Struct(klass, struct dir_data, 0, free_dir, dp);

    dir_initialize(dir, dirname);
    if (rb_block_given_p()) {
	return rb_ensure(rb_yield, dir, dir_close, dir);
    }

    return dir;
}

static void
dir_closed()
{
    rb_raise(rb_eIOError, "closed directory");
}

#define GetDIR(obj, dirp) do {\
    Data_Get_Struct(obj, struct dir_data, dirp);\
    if (dirp->dir == NULL) dir_closed();\
} while (0)

/*
 *  call-seq:
 *     dir.inspect => string
 *
 *  Return a string describing this Dir object.
 */
static VALUE
dir_inspect(dir)
    VALUE dir;
{
    struct dir_data *dirp;

    GetDIR(dir, dirp);
    if (dirp->path) {
	char *c = rb_obj_classname(dir);
	int len = strlen(c) + strlen(dirp->path) + 4;
	VALUE s = rb_str_new(0, len);
	snprintf(RSTRING(s)->ptr, len+1, "#<%s:%s>", c, dirp->path);
	return s;
    }
    return rb_funcall(dir, rb_intern("to_s"), 0, 0);
}

/*
 *  call-seq:
 *     dir.path => string or nil
 *
 *  Returns the path parameter passed to <em>dir</em>'s constructor.
 *
 *     d = Dir.new("..")
 *     d.path   #=> ".."
 */
static VALUE
dir_path(dir)
    VALUE dir;
{
    struct dir_data *dirp;

    GetDIR(dir, dirp);
    if (!dirp->path) return Qnil;
    return rb_str_new2(dirp->path);
}

/*
 *  call-seq:
 *     dir.read => string or nil
 *
 *  Reads the next entry from <em>dir</em> and returns it as a string.
 *  Returns <code>nil</code> at the end of the stream.
 *
 *     d = Dir.new("testdir")
 *     d.read   #=> "."
 *     d.read   #=> ".."
 *     d.read   #=> "config.h"
 */
static VALUE
dir_read(dir)
    VALUE dir;
{
    struct dir_data *dirp;
    struct dirent *dp;

    GetDIR(dir, dirp);
    errno = 0;
    dp = readdir(dirp->dir);
    if (dp) {
	return rb_tainted_str_new(dp->d_name, NAMLEN(dp));
    }
    else if (errno == 0) {	/* end of stream */
	return Qnil;
    }
    else {
	rb_sys_fail(0);
    }
    return Qnil;		/* not reached */
}

/*
 *  call-seq:
 *     dir.each { |filename| block }  => dir
 *
 *  Calls the block once for each entry in this directory, passing the
 *  filename of each entry as a parameter to the block.
 *
 *     d = Dir.new("testdir")
 *     d.each  {|x| puts "Got #{x}" }
 *
 *  <em>produces:</em>
 *
 *     Got .
 *     Got ..
 *     Got config.h
 *     Got main.rb
 */
static VALUE
dir_each(dir)
    VALUE dir;
{
    struct dir_data *dirp;
    struct dirent *dp;

    GetDIR(dir, dirp);
    for (dp = readdir(dirp->dir); dp != NULL; dp = readdir(dirp->dir)) {
	rb_yield(rb_tainted_str_new(dp->d_name, NAMLEN(dp)));
	if (dirp->dir == NULL) dir_closed();
    }
    return dir;
}

/*
 *  call-seq:
 *     dir.pos => integer
 *     dir.tell => integer
 *
 *  Returns the current position in <em>dir</em>. See also
 *  <code>Dir#seek</code>.
 *
 *     d = Dir.new("testdir")
 *     d.tell   #=> 0
 *     d.read   #=> "."
 *     d.tell   #=> 12
 */
static VALUE
dir_tell(dir)
    VALUE dir;
{
#ifdef HAVE_TELLDIR
    struct dir_data *dirp;
    long pos;

    GetDIR(dir, dirp);
    pos = telldir(dirp->dir);
    return rb_int2inum(pos);
#else
    rb_notimplement();
#endif
}

/*
 *  call-seq:
 *     dir.seek( integer ) => dir
 *
 *  Seeks to a particular location in <em>dir</em>. <i>integer</i>
 *  must be a value returned by <code>Dir#tell</code>.
 *
 *     d = Dir.new("testdir")   #=> #<Dir:0x401b3c40>
 *     d.read                   #=> "."
 *     i = d.tell               #=> 12
 *     d.read                   #=> ".."
 *     d.seek(i)                #=> #<Dir:0x401b3c40>
 *     d.read                   #=> ".."
 */
static VALUE
dir_seek(dir, pos)
    VALUE dir, pos;
{
    struct dir_data *dirp;
    off_t p = NUM2OFFT(pos);

    GetDIR(dir, dirp);
#ifdef HAVE_SEEKDIR
    seekdir(dirp->dir, p);
    return dir;
#else
    rb_notimplement();
#endif
}

/*
 *  call-seq:
 *     dir.pos( integer ) => integer
 *
 *  Synonym for <code>Dir#seek</code>, but returns the position
 *  parameter.
 *
 *     d = Dir.new("testdir")   #=> #<Dir:0x401b3c40>
 *     d.read                   #=> "."
 *     i = d.pos                #=> 12
 *     d.read                   #=> ".."
 *     d.pos = i                #=> 12
 *     d.read                   #=> ".."
 */
static VALUE
dir_set_pos(dir, pos)
    VALUE dir, pos;
{
    dir_seek(dir, pos);
    return pos;
}

/*
 *  call-seq:
 *     dir.rewind => dir
 *
 *  Repositions <em>dir</em> to the first entry.
 *
 *     d = Dir.new("testdir")
 *     d.read     #=> "."
 *     d.rewind   #=> #<Dir:0x401b3fb0>
 *     d.read     #=> "."
 */
static VALUE
dir_rewind(dir)
    VALUE dir;
{
    struct dir_data *dirp;

    GetDIR(dir, dirp);
    rewinddir(dirp->dir);
    return dir;
}

/*
 *  call-seq:
 *     dir.close => nil
 *
 *  Closes the directory stream. Any further attempts to access
 *  <em>dir</em> will raise an <code>IOError</code>.
 *
 *     d = Dir.new("testdir")
 *     d.close   #=> nil
 */
static VALUE
dir_close(dir)
    VALUE dir;
{
    struct dir_data *dirp;

    GetDIR(dir, dirp);
    closedir(dirp->dir);
    dirp->dir = NULL;

    return Qnil;
}

static void
dir_chdir(path)
    VALUE path;
{
    if (chdir(RSTRING(path)->ptr) < 0)
	rb_sys_fail(RSTRING(path)->ptr);
}

static int chdir_blocking = 0;
static VALUE chdir_thread = Qnil;

struct chdir_data {
    VALUE old_path, new_path;
    int done;
};

static VALUE
chdir_yield(args)
    struct chdir_data *args;
{
    dir_chdir(args->new_path);
    args->done = Qtrue;
    chdir_blocking++;
    if (chdir_thread == Qnil)
	chdir_thread = rb_thread_current();
    return rb_yield(args->new_path);
}

static VALUE
chdir_restore(args)
    struct chdir_data *args;
{
    if (args->done) {
	chdir_blocking--;
	if (chdir_blocking == 0)
	    chdir_thread = Qnil;
	dir_chdir(args->old_path);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     Dir.chdir( [ string] ) => 0
 *     Dir.chdir( [ string] ) {| path | block }  => anObject
 *
 *  Changes the current working directory of the process to the given
 *  string. When called without an argument, changes the directory to
 *  the value of the environment variable <code>HOME</code>, or
 *  <code>LOGDIR</code>. <code>SystemCallError</code> (probably
 *  <code>Errno::ENOENT</code>) if the target directory does not exist.
 *
 *  If a block is given, it is passed the name of the new current
 *  directory, and the block is executed with that as the current
 *  directory. The original working directory is restored when the block
 *  exits. The return value of <code>chdir</code> is the value of the
 *  block. <code>chdir</code> blocks can be nested, but in a
 *  multi-threaded program an error will be raised if a thread attempts
 *  to open a <code>chdir</code> block while another thread has one
 *  open.
 *
 *     Dir.chdir("/var/spool/mail")
 *     puts Dir.pwd
 *     Dir.chdir("/tmp") do
 *       puts Dir.pwd
 *       Dir.chdir("/usr") do
 *         puts Dir.pwd
 *       end
 *       puts Dir.pwd
 *     end
 *     puts Dir.pwd
 *
 *  <em>produces:</em>
 *
 *     /var/spool/mail
 *     /tmp
 *     /usr
 *     /tmp
 *     /var/spool/mail
 */
static VALUE
dir_s_chdir(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE path = Qnil;

    rb_secure(2);
    if (rb_scan_args(argc, argv, "01", &path) == 1) {
	FilePathValue(path);
    }
    else {
	const char *dist = getenv("HOME");
	if (!dist) {
	    dist = getenv("LOGDIR");
	    if (!dist) rb_raise(rb_eArgError, "HOME/LOGDIR not set");
	}
	path = rb_str_new2(dist);
    }

    if (chdir_blocking > 0) {
	if (!rb_block_given_p() || rb_thread_current() != chdir_thread)
	    rb_warn("conflicting chdir during another chdir block");
    }

    if (rb_block_given_p()) {
	struct chdir_data args;
	char *cwd = my_getcwd();

	args.old_path = rb_tainted_str_new2(cwd); free(cwd);
	args.new_path = path;
	args.done = Qfalse;
	return rb_ensure(chdir_yield, (VALUE)&args, chdir_restore, (VALUE)&args);
    }
    dir_chdir(path);

    return INT2FIX(0);
}

/*
 *  call-seq:
 *     Dir.getwd => string
 *     Dir.pwd => string
 *
 *  Returns the path to the current working directory of this process as
 *  a string.
 *
 *     Dir.chdir("/tmp")   #=> 0
 *     Dir.getwd           #=> "/tmp"
 */
static VALUE
dir_s_getwd(dir)
    VALUE dir;
{
    char *path;
    VALUE cwd;

    rb_secure(4);
    path = my_getcwd();
    cwd = rb_tainted_str_new2(path);

    free(path);
    return cwd;
}

static void check_dirname _((volatile VALUE *));
static void
check_dirname(dir)
    volatile VALUE *dir;
{
    char *path, *pend;

    rb_secure(2);
    FilePathValue(*dir);
    path = RSTRING(*dir)->ptr;
    if (path && *(pend = rb_path_end(rb_path_skip_prefix(path)))) {
	*dir = rb_str_new(path, pend - path);
    }
}

/*
 *  call-seq:
 *     Dir.chroot( string ) => 0
 *
 *  Changes this process's idea of the file system root. Only a
 *  privileged process may make this call. Not available on all
 *  platforms. On Unix systems, see <code>chroot(2)</code> for more
 *  information.
 */
static VALUE
dir_s_chroot(dir, path)
    VALUE dir, path;
{
#if defined(HAVE_CHROOT) && !defined(__CHECKER__)
    check_dirname(&path);

    if (chroot(RSTRING(path)->ptr) == -1)
	rb_sys_fail(RSTRING(path)->ptr);

    return INT2FIX(0);
#else
    rb_notimplement();
    return Qnil;		/* not reached */
#endif
}

/*
 *  call-seq:
 *     Dir.mkdir( string [, integer] ) => 0
 *
 *  Makes a new directory named by <i>string</i>, with permissions
 *  specified by the optional parameter <i>anInteger</i>. The
 *  permissions may be modified by the value of
 *  <code>File::umask</code>, and are ignored on NT. Raises a
 *  <code>SystemCallError</code> if the directory cannot be created. See
 *  also the discussion of permissions in the class documentation for
 *  <code>File</code>.
 *
 */
static VALUE
dir_s_mkdir(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE path, vmode;
    int mode;

    if (rb_scan_args(argc, argv, "11", &path, &vmode) == 2) {
	mode = NUM2INT(vmode);
    }
    else {
	mode = 0777;
    }

    check_dirname(&path);
    if (mkdir(RSTRING(path)->ptr, mode) == -1)
	rb_sys_fail(RSTRING(path)->ptr);

    return INT2FIX(0);
}

/*
 *  call-seq:
 *     Dir.delete( string ) => 0
 *     Dir.rmdir( string ) => 0
 *     Dir.unlink( string ) => 0
 *
 *  Deletes the named directory. Raises a subclass of
 *  <code>SystemCallError</code> if the directory isn't empty.
 */
static VALUE
dir_s_rmdir(obj, dir)
    VALUE obj, dir;
{
    check_dirname(&dir);
    if (rmdir(RSTRING(dir)->ptr) < 0)
	rb_sys_fail(RSTRING(dir)->ptr);

    return INT2FIX(0);
}

/* System call with warning */
static int
do_stat(path, pst)
    const char *path;
    struct stat *pst;
{
    int ret = stat(path, pst);
    if (ret < 0 && errno != ENOENT)
	rb_sys_warning(path);

    return ret;
}

static int
do_lstat(path, pst)
    const char *path;
    struct stat *pst;
{
    int ret = lstat(path, pst);
    if (ret < 0 && errno != ENOENT)
	rb_sys_warning(path);

    return ret;
}

static DIR *
do_opendir(path)
    const char *path;
{
    DIR *dirp = opendir(path);
    if (dirp == NULL && errno != ENOENT && errno != ENOTDIR)
	rb_sys_warning(path);

    return dirp;
}

/* Return nonzero if S has any special globbing chars in it.  */
static int
has_magic(s, flags)
    const char *s;
    int flags;
{
    const int escape = !(flags & FNM_NOESCAPE);

    register const char *p = s;
    register char c;

    while (c = *p++) {
	switch (c) {
	  case '*':
	  case '?':
	  case '[':
	    return 1;

	  case '\\':
	    if (escape && !(c = *p++))
		return 0;
	    continue;
	}

	p = Next(p-1);
    }

    return 0;
}

/* Find separator in globbing pattern. */
static char *
find_dirsep(s, flags)
    const char *s;
    int flags;
{
    const int escape = !(flags & FNM_NOESCAPE);

    register const char *p = s;
    register char c;
    int open = 0;

    while (c = *p++) {
	switch (c) {
	  case '[':
	    open = 1;
	    continue;
	  case ']':
	    open = 0;
	    continue;

	  case '/':
	    if (!open)
		return (char *)p-1;
	    continue;

	  case '\\':
	    if (escape && !(c = *p++))
		return (char *)p-1;
	    continue;
	}

	p = Next(p-1);
    }

    return (char *)p-1;
}

/* Remove escaping baskclashes */
static void
remove_backslashes(p)
    char *p;
{
    char *t = p;
    char *s = p;

    while (*p) {
	if (*p == '\\') {
	    if (t != s)
		memmove(t, s, p - s);
	    t += p - s;
	    s = ++p;
	    if (!*p) break;
	}
	Inc(p);
    }

    while (*p++);

    if (t != s)
	memmove(t, s, p - s); /* move '\0' too */
}

/* Globing pattern */
enum glob_pattern_type { PLAIN, MAGICAL, RECURSIVE, MATCH_ALL, MATCH_DIR };

struct glob_pattern {
    char *str;
    enum glob_pattern_type type;
    struct glob_pattern *next;
};

static struct glob_pattern *
glob_make_pattern(p, flags)
    const char *p;
    int flags;
{
    struct glob_pattern *list, *tmp, **tail = &list;
    int dirsep = 0; /* pattern is terminated with '/' */

    while (*p) {
	tmp = ALLOC(struct glob_pattern);
	if (p[0] == '*' && p[1] == '*' && p[2] == '/') {
	    /* fold continuous RECURSIVEs (needed in glob_helper) */
	    do { p += 3; } while (p[0] == '*' && p[1] == '*' && p[2] == '/');
	    tmp->type = RECURSIVE;
	    tmp->str = 0;
	    dirsep = 1;
	}
	else {
	    const char *m = find_dirsep(p, flags);
	    char *buf = ALLOC_N(char, m-p+1);
	    memcpy(buf, p, m-p);
	    buf[m-p] = '\0';
	    tmp->type = has_magic(buf, flags) ? MAGICAL : PLAIN;
	    tmp->str = buf;
	    if (*m) {
		dirsep = 1;
		p = m + 1;
	    }
	    else {
		dirsep = 0;
		p = m;
	    }
	}
	*tail = tmp;
	tail = &tmp->next;
    }

    tmp = ALLOC(struct glob_pattern);
    tmp->type = dirsep ? MATCH_DIR : MATCH_ALL;
    tmp->str = 0;
    *tail = tmp;
    tmp->next = 0;

    return list;
}

static void
glob_free_pattern(list)
    struct glob_pattern *list;
{
    while (list) {
	struct glob_pattern *tmp = list;
	list = list->next;
	if (tmp->str)
	    free(tmp->str);
	free(tmp);
    }
}

static VALUE
join_path(path, dirsep, name)
    VALUE path;
    int dirsep;
    const char *name;
{
    long len = RSTRING(path)->len;
    VALUE buf = rb_str_new(0, RSTRING(path)->len+strlen(name)+(dirsep?1:0));

    memcpy(RSTRING(buf)->ptr, RSTRING(path)->ptr, len);
    if (dirsep) {
	strcpy(RSTRING(buf)->ptr+len, "/");
	len++;
    }
    strcpy(RSTRING(buf)->ptr+len, name);
    return buf;
}

enum answer { YES, NO, UNKNOWN };

#ifndef S_ISDIR
#   define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#endif

#ifndef S_ISLNK
#  ifndef S_IFLNK
#    define S_ISLNK(m) (0)
#  else
#    define S_ISLNK(m) ((m & S_IFMT) == S_IFLNK)
#  endif
#endif

struct glob_args {
    void (*func) _((VALUE, VALUE));
    VALUE c;
    VALUE v;
};

static VALUE glob_func_caller _((VALUE));

static VALUE
glob_func_caller(val)
    VALUE val;
{
    struct glob_args *args = (struct glob_args *)val;
    VALUE path = args->c;

    OBJ_TAINT(path);
    (*args->func)(path, args->v);
    return Qnil;
}

static int
glob_call_func(func, path, arg)
    void (*func) _((VALUE, VALUE));
    VALUE path;
    VALUE arg;
{
    int status;
    struct glob_args args;

    args.func = func;
    args.c = path;
    args.v = arg;

    rb_protect(glob_func_caller, (VALUE)&args, &status);
    return status;
}

static int
glob_helper(path, dirsep, exist, isdir, beg, end, flags, func, arg)
    VALUE path;
    int dirsep; /* '/' should be placed before appending child entry's name to 'path'. */
    enum answer exist; /* Does 'path' indicate an existing entry? */
    enum answer isdir; /* Does 'path' indicate a directory or a symlink to a directory? */
    struct glob_pattern **beg;
    struct glob_pattern **end;
    int flags;
    void (*func) _((VALUE, VALUE));
    VALUE arg;
{
    struct stat st;
    int status = 0;
    struct glob_pattern **cur, **new_beg, **new_end;
    int plain = 0, magical = 0, recursive = 0, match_all = 0, match_dir = 0;
    int escape = !(flags & FNM_NOESCAPE);

    for (cur = beg; cur < end; ++cur) {
	struct glob_pattern *p = *cur;
	if (p->type == RECURSIVE) {
	    recursive = 1;
	    p = p->next;
	}
	switch (p->type) {
	case PLAIN:
	    plain = 1;
	    break;
	case MAGICAL:
	    magical = 1;
	    break;
	case MATCH_ALL:
	    match_all = 1;
	    break;
	case MATCH_DIR:
	    match_dir = 1;
	    break;
	}
    }

    if (RSTRING(path)->len > 0) {
	if (match_all && exist == UNKNOWN) {
	    if (do_lstat(RSTRING(path)->ptr, &st) == 0) {
		exist = YES;
		isdir = S_ISDIR(st.st_mode) ? YES : S_ISLNK(st.st_mode) ? UNKNOWN : NO;
	    }
	    else {
		exist = NO;
		isdir = NO;
	    }
	}

	if (match_dir && isdir == UNKNOWN) {
	    if (do_stat(RSTRING(path)->ptr, &st) == 0) {
		exist = YES;
		isdir = S_ISDIR(st.st_mode) ? YES : NO;
	    }
	    else {
		exist = NO;
		isdir = NO;
	    }
	}

	if (match_all && exist == YES) {
	    status = glob_call_func(func, path, arg);
	    if (status) return status;
	}

	if (match_dir && isdir == YES) {
	    status = glob_call_func(func, join_path(path, dirsep, ""), arg);
	    if (status) return status;
	}
    }

    if (exist == NO || isdir == NO) return 0;

    if (magical || recursive) {
	struct dirent *dp;
	DIR *dirp = do_opendir(RSTRING(path)->len > 0 ? RSTRING(path)->ptr : ".");
	if (dirp == NULL) return 0;

	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	    VALUE buf = join_path(path, dirsep, dp->d_name);

	    enum answer new_isdir = UNKNOWN;
	    if (recursive && strcmp(dp->d_name, ".") != 0 && strcmp(dp->d_name, "..") != 0
		&& fnmatch("*", dp->d_name, flags) == 0) {
#ifndef _WIN32
		if (do_lstat(RSTRING(buf)->ptr, &st) == 0)
		    new_isdir = S_ISDIR(st.st_mode) ? YES : S_ISLNK(st.st_mode) ? UNKNOWN : NO;
		else
		    new_isdir = NO;
#else
		new_isdir = dp->d_isdir ? (!dp->d_isrep ? YES : UNKNOWN) : NO;
#endif
	    }

	    new_beg = new_end = ALLOC_N(struct glob_pattern *, (end - beg) * 2);

	    for (cur = beg; cur < end; ++cur) {
		struct glob_pattern *p = *cur;
		if (p->type == RECURSIVE) {
		    if (new_isdir == YES) /* not symlink but real directory */
			*new_end++ = p; /* append recursive pattern */
		    p = p->next; /* 0 times recursion */
		}
		if (p->type == PLAIN || p->type == MAGICAL) {
		    if (fnmatch(p->str, dp->d_name, flags) == 0)
			*new_end++ = p->next;
		}
	    }

	    status = glob_helper(buf, 1, YES, new_isdir, new_beg, new_end, flags, func, arg);
	    free(new_beg);
	    if (status) break;
	}

	closedir(dirp);
    }
    else if (plain) {
	struct glob_pattern **copy_beg, **copy_end, **cur2;

	copy_beg = copy_end = ALLOC_N(struct glob_pattern *, end - beg);
	for (cur = beg; cur < end; ++cur)
	    *copy_end++ = (*cur)->type == PLAIN ? *cur : 0;

	for (cur = copy_beg; cur < copy_end; ++cur) {
	    if (*cur) {
		VALUE buf;
		char *name;
		name = ALLOC_N(char, strlen((*cur)->str) + 1);
		strcpy(name, (*cur)->str);
		if (escape) remove_backslashes(name);

		new_beg = new_end = ALLOC_N(struct glob_pattern *, end - beg);
		*new_end++ = (*cur)->next;
		for (cur2 = cur + 1; cur2 < copy_end; ++cur2) {
		    if (*cur2 && fnmatch((*cur2)->str, name, flags) == 0) {
			*new_end++ = (*cur2)->next;
			*cur2 = 0;
		    }
		}

		buf = join_path(path, dirsep, name);
		free(name);
		status = glob_helper(buf, 1, UNKNOWN, UNKNOWN, new_beg, new_end, flags, func, arg);
		free(new_beg);
		if (status) break;
	    }
	}

	free(copy_beg);
    }

    return status;
}

static int
rb_glob2(path, offset, flags, func, arg)
    VALUE path;
    long offset;
    int flags;
    void (*func) _((VALUE, VALUE));
    VALUE arg;
{
    struct glob_pattern *list;
    const char *root, *start;
    VALUE buf;
    int n;
    int status;

    if (flags & FNM_CASEFOLD) {
	rb_warn("Dir.glob() ignores File::FNM_CASEFOLD");
    }

    start = root = StringValuePtr(path) + offset;
#if defined DOSISH
    flags |= FNM_CASEFOLD;
    root = rb_path_skip_prefix(root);
#else
    flags &= ~FNM_CASEFOLD;
#endif

    if (root && *root == '/') root++;

    n = root - start;
    buf = rb_str_new(start, n);

    list = glob_make_pattern(root, flags);
    status = glob_helper(buf, 0, UNKNOWN, UNKNOWN, &list, &list + 1, flags, func, arg);
    glob_free_pattern(list);

    return status;
}

struct rb_glob_args {
    void (*func) _((const char*, VALUE));
    VALUE arg;
};

static VALUE
rb_glob_caller(path, a)
    VALUE path, a;
{
    struct rb_glob_args *args = (struct rb_glob_args *)a;
    (*args->func)(RSTRING(path)->ptr, args->arg);
    return Qnil;
}

void
rb_glob(path, func, arg)
    const char *path;
    void (*func) _((const char*, VALUE));
    VALUE arg;
{
    struct rb_glob_args args;
    int status;

    args.func = func;
    args.arg = arg;
    status = rb_glob2(rb_str_new2(path), 0, 0, rb_glob_caller, &args);

    if (status) rb_jump_tag(status);
}

static void
push_pattern(path, ary)
    VALUE path, ary;
{
    rb_ary_push(ary, path);
}

static int
push_glob(VALUE ary, VALUE s, long offset, int flags);

static int
push_glob(ary, str, offset, flags)
    VALUE ary;
    VALUE str;
    long offset;
    int flags;
{
    const int escape = !(flags & FNM_NOESCAPE);

    const char *p = RSTRING(str)->ptr + offset;
    const char *s = p;
    const char *lbrace = 0, *rbrace = 0;
    int nest = 0, status = 0;

    while (*p) {
	if (*p == '{' && nest++ == 0) {
	    lbrace = p;
	}
	if (*p == '}' && --nest <= 0) {
	    rbrace = p;
	    break;
	}
	if (*p == '\\' && escape) {
	    if (!*++p) break;
	}
	Inc(p);
    }

    if (lbrace && rbrace) {
	VALUE buffer = rb_str_new(0, strlen(s));
	char *buf;
	long shift;

	buf = RSTRING(buffer)->ptr;
	memcpy(buf, s, lbrace-s);
	shift = (lbrace-s);
	p = lbrace;
	while (p < rbrace) {
	    const char *t = ++p;
	    nest = 0;
	    while (p < rbrace && !(*p == ',' && nest == 0)) {
		if (*p == '{') nest++;
		if (*p == '}') nest--;
		if (*p == '\\' && escape) {
		    if (++p == rbrace) break;
		}
		Inc(p);
	    }
	    memcpy(buf+shift, t, p-t);
	    strcpy(buf+shift+(p-t), rbrace+1);
	    status = push_glob(ary, buffer, offset, flags);
	    if (status) break;
	}
    }
    else if (!lbrace && !rbrace) {
	status = rb_glob2(str, offset, flags, push_pattern, ary);
    }

    return status;
}

static VALUE
rb_push_glob(str, flags) /* '\0' is delimiter */
    VALUE str;
    int flags;
{
    long offset = 0;
    VALUE ary;

    FilePathValue(str);

    ary = rb_ary_new();

    while (offset < RSTRING(str)->len) {
	int status = push_glob(ary, str, offset, flags);
	char *p, *pend;
	if (status) rb_jump_tag(status);
	p = RSTRING(str)->ptr + offset;
	p += strlen(p) + 1;
	pend = RSTRING(str)->ptr + RSTRING(str)->len;
	while (p < pend && !*p)
	    p++;
	offset = p - RSTRING(str)->ptr;
    }

    if (rb_block_given_p()) {
	rb_ary_each(ary);
	return Qnil;
    }
    return ary;
}

/*
 *  call-seq:
 *     Dir[ string ] => array
 *
 *  Equivalent to calling
 *  <em>dir</em>.<code>glob(</code><i>string,</i><code>0)</code>.
 *
 */
static VALUE
dir_s_aref(obj, str)
    VALUE obj, str;
{
    return rb_push_glob(str, 0);
}

/*
 *  call-seq:
 *     Dir.glob( string, [flags] ) => array
 *     Dir.glob( string, [flags] ) {| filename | block }  => nil
 *
 *  Returns the filenames found by expanding the pattern given in
 *  <i>string</i>, either as an <i>array</i> or as parameters to the
 *  block. Note that this pattern is not a regexp (it's closer to a
 *  shell glob). See <code>File::fnmatch</code> for the meaning of
 *  the <i>flags</i> parameter. Note that case sensitivity 
 *  depends on your system (so <code>File::FNM_CASEFOLD</code> is ignored)
 *
 *  <code>*</code>::        Matches any file. Can be restricted by
 *                          other values in the glob. <code>*</code>
 *                          will match all files; <code>c*</code> will
 *                          match all files beginning with
 *                          <code>c</code>; <code>*c</code> will match
 *                          all files ending with <code>c</code>; and
 *                          <code>*c*</code> will match all files that
 *                          have <code>c</code> in them (including at
 *                          the beginning or end). Equivalent to
 *                          <code>/ .* /x</code> in regexp.
 *  <code>**</code>::       Matches directories recursively.
 *  <code>?</code>::        Matches any one character. Equivalent to
 *                          <code>/.{1}/</code> in regexp.
 *  <code>[set]</code>::    Matches any one character in +set+.
 *                          Behaves exactly like character sets in
 *                          Regexp, including set negation
 *                          (<code>[^a-z]</code>).
 *  <code>{p,q}</code>::    Matches either literal <code>p</code> or
 *                          literal <code>q</code>. Matching literals
 *                          may be more than one character in length.
 *                          More than two literals may be specified.
 *                          Equivalent to pattern alternation in
 *                          regexp.
 *  <code>\</code>::        Escapes the next metacharacter.
 *
 *     Dir["config.?"]                     #=> ["config.h"]
 *     Dir.glob("config.?")                #=> ["config.h"]
 *     Dir.glob("*.[a-z][a-z]")            #=> ["main.rb"]
 *     Dir.glob("*.[^r]*")                 #=> ["config.h"]
 *     Dir.glob("*.{rb,h}")                #=> ["main.rb", "config.h"]
 *     Dir.glob("*")                       #=> ["config.h", "main.rb"]
 *     Dir.glob("*", File::FNM_DOTMATCH)   #=> [".", "..", "config.h", "main.rb"]
 *
 *     rbfiles = File.join("**", "*.rb")
 *     Dir.glob(rbfiles)                   #=> ["main.rb",
 *                                              "lib/song.rb",
 *                                              "lib/song/karaoke.rb"]
 *     libdirs = File.join("**", "lib")
 *     Dir.glob(libdirs)                   #=> ["lib"]
 *
 *     librbfiles = File.join("**", "lib", "**", "*.rb")
 *     Dir.glob(librbfiles)                #=> ["lib/song.rb",
 *                                              "lib/song/karaoke.rb"]
 *
 *     librbfiles = File.join("**", "lib", "*.rb")
 *     Dir.glob(librbfiles)                #=> ["lib/song.rb"]
 */
static VALUE
dir_s_glob(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE str, rflags;
    int flags;

    if (rb_scan_args(argc, argv, "11", &str, &rflags) == 2)
	flags = NUM2INT(rflags);
    else
	flags = 0;

    return rb_push_glob(str, flags);
}

static VALUE
dir_open_dir(path)
    VALUE path;
{
    struct dir_data *dp;
    VALUE dir = rb_funcall(rb_cDir, rb_intern("open"), 1, path);

    if (TYPE(dir) != T_DATA ||
	RDATA(dir)->dfree != (RUBY_DATA_FUNC)free_dir) {
	rb_raise(rb_eTypeError, "wrong argument type %s (expected Dir)",
		 rb_obj_classname(dir));
    }
    return dir;
}


/*
 *  call-seq:
 *     Dir.foreach( dirname ) {| filename | block }  => nil
 *
 *  Calls the block once for each entry in the named directory, passing
 *  the filename of each entry as a parameter to the block.
 *
 *     Dir.foreach("testdir") {|x| puts "Got #{x}" }
 *
 *  <em>produces:</em>
 *
 *     Got .
 *     Got ..
 *     Got config.h
 *     Got main.rb
 *
 */
static VALUE
dir_foreach(io, dirname)
    VALUE io, dirname;
{
    VALUE dir;

    dir = dir_open_dir(dirname);
    rb_ensure(dir_each, dir, dir_close, dir);
    return Qnil;
}

/*
 *  call-seq:
 *     Dir.entries( dirname ) => array
 *
 *  Returns an array containing all of the filenames in the given
 *  directory. Will raise a <code>SystemCallError</code> if the named
 *  directory doesn't exist.
 *
 *     Dir.entries("testdir")   #=> [".", "..", "config.h", "main.rb"]
 *
 */
static VALUE
dir_entries(io, dirname)
    VALUE io, dirname;
{
    VALUE dir;

    dir = dir_open_dir(dirname);
    return rb_ensure(rb_Array, dir, dir_close, dir);
}

/*
 *  call-seq:
 *     File.fnmatch( pattern, path, [flags] ) => (true or false)
 *     File.fnmatch?( pattern, path, [flags] ) => (true or false)
 *
 *  Returns true if <i>path</i> matches against <i>pattern</i> The
 *  pattern is not a regular expression; instead it follows rules
 *  similar to shell filename globbing. It may contain the following
 *  metacharacters:
 *
 *  <code>*</code>::        Matches any file. Can be restricted by
 *                          other values in the glob. <code>*</code>
 *                          will match all files; <code>c*</code> will
 *                          match all files beginning with
 *                          <code>c</code>; <code>*c</code> will match
 *                          all files ending with <code>c</code>; and
 *                          <code>*c*</code> will match all files that
 *                          have <code>c</code> in them (including at
 *                          the beginning or end). Equivalent to
 *                          <code>/ .* /x</code> in regexp.
 *  <code>**</code>::       Matches directories recursively or files
 *                          expansively.
 *  <code>?</code>::        Matches any one character. Equivalent to
 *                          <code>/.{1}/</code> in regexp.
 *  <code>[set]</code>::    Matches any one character in +set+.
 *                          Behaves exactly like character sets in
 *                          Regexp, including set negation
 *                          (<code>[^a-z]</code>).
 *  <code>\</code>::        Escapes the next metacharacter.
 *
 *  <i>flags</i> is a bitwise OR of the <code>FNM_xxx</code>
 *  parameters. The same glob pattern and flags are used by
 *  <code>Dir::glob</code>.
 *
 *     File.fnmatch('cat',       'cat')        #=> true  : match entire string
 *     File.fnmatch('cat',       'category')   #=> false : only match partial string
 *     File.fnmatch('c{at,ub}s', 'cats')       #=> false : { } isn't supported
 *
 *     File.fnmatch('c?t',     'cat')          #=> true  : '?' match only 1 character
 *     File.fnmatch('c??t',    'cat')          #=> false : ditto
 *     File.fnmatch('c*',      'cats')         #=> true  : '*' match 0 or more characters
 *     File.fnmatch('c*t',     'c/a/b/t')      #=> true  : ditto
 *     File.fnmatch('ca[a-z]', 'cat')          #=> true  : inclusive bracket expression
 *     File.fnmatch('ca[^t]',  'cat')          #=> false : exclusive bracket expression ('^' or '!')
 *
 *     File.fnmatch('cat', 'CAT')                     #=> false : case sensitive
 *     File.fnmatch('cat', 'CAT', File::FNM_CASEFOLD) #=> true  : case insensitive
 *
 *     File.fnmatch('?',   '/', File::FNM_PATHNAME)  #=> false : wildcard doesn't match '/' on FNM_PATHNAME
 *     File.fnmatch('*',   '/', File::FNM_PATHNAME)  #=> false : ditto
 *     File.fnmatch('[/]', '/', File::FNM_PATHNAME)  #=> false : ditto
 *
 *     File.fnmatch('\?',   '?')                       #=> true  : escaped wildcard becomes ordinary
 *     File.fnmatch('\a',   'a')                       #=> true  : escaped ordinary remains ordinary
 *     File.fnmatch('\a',   '\a', File::FNM_NOESCAPE)  #=> true  : FNM_NOESACPE makes '\' ordinary
 *     File.fnmatch('[\?]', '?')                       #=> true  : can escape inside bracket expression
 *
 *     File.fnmatch('*',   '.profile')                      #=> false : wildcard doesn't match leading
 *     File.fnmatch('*',   '.profile', File::FNM_DOTMATCH)  #=> true    period by default.
 *     File.fnmatch('.*',  '.profile')                      #=> true
 *
 *     rbfiles = File.join("**", "*.rb")
 *     File.fnmatch(rbfiles, 'main.rb')                    #=> false
 *     File.fnmatch(rbfiles, './main.rb')                  #=> false
 *     File.fnmatch(rbfiles, 'lib/song.rb')                #=> true
 *     File.fnmatch('**.rb', 'main.rb')                    #=> true
 *     File.fnmatch('**.rb', './main.rb')                  #=> false
 *     File.fnmatch('**.rb', 'lib/song.rb')                #=> true
 *     File.fnmatch('*',           'dave/.profile')                      #=> true
 *
 *     File.fnmatch('* IGNORE /*', 'dave/.profile', File::FNM_PATHNAME)  #=> false
 *     File.fnmatch('* IGNORE /*', 'dave/.profile', File::FNM_PATHNAME | File::FNM_DOTMATCH) #=> true
 *
 *     File.fnmatch('** IGNORE /foo', 'a/b/c/foo', File::FNM_PATHNAME)     #=> true
 *     File.fnmatch('** IGNORE /foo', '/a/b/c/foo', File::FNM_PATHNAME)    #=> true
 *     File.fnmatch('** IGNORE /foo', 'c:/a/b/c/foo', File::FNM_PATHNAME)  #=> true
 *     File.fnmatch('** IGNORE /foo', 'a/.b/c/foo', File::FNM_PATHNAME)    #=> false
 *     File.fnmatch('** IGNORE /foo', 'a/.b/c/foo', File::FNM_PATHNAME | File::FNM_DOTMATCH) #=> true
 */
static VALUE
file_s_fnmatch(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE pattern, path;
    VALUE rflags;
    int flags;

    if (rb_scan_args(argc, argv, "21", &pattern, &path, &rflags) == 3)
	flags = NUM2INT(rflags);
    else
	flags = 0;

    StringValue(pattern);
    StringValue(path);

    if (fnmatch(RSTRING(pattern)->ptr, RSTRING(path)->ptr, flags) == 0)
	return Qtrue;

    return Qfalse;
}

/*
 *  Objects of class <code>Dir</code> are directory streams representing
 *  directories in the underlying file system. They provide a variety of
 *  ways to list directories and their contents. See also
 *  <code>File</code>.
 *
 *  The directory used in these examples contains the two regular files
 *  (<code>config.h</code> and <code>main.rb</code>), the parent
 *  directory (<code>..</code>), and the directory itself
 *  (<code>.</code>).
 */
void
Init_Dir()
{
    rb_cDir = rb_define_class("Dir", rb_cObject);

    rb_include_module(rb_cDir, rb_mEnumerable);

    rb_define_alloc_func(rb_cDir, dir_s_alloc);
    rb_define_singleton_method(rb_cDir, "open", dir_s_open, 1);
    rb_define_singleton_method(rb_cDir, "foreach", dir_foreach, 1);
    rb_define_singleton_method(rb_cDir, "entries", dir_entries, 1);

    rb_define_method(rb_cDir,"initialize", dir_initialize, 1);
    rb_define_method(rb_cDir,"path", dir_path, 0);
    rb_define_method(rb_cDir,"inspect", dir_inspect, 0);
    rb_define_method(rb_cDir,"read", dir_read, 0);
    rb_define_method(rb_cDir,"each", dir_each, 0);
    rb_define_method(rb_cDir,"rewind", dir_rewind, 0);
    rb_define_method(rb_cDir,"tell", dir_tell, 0);
    rb_define_method(rb_cDir,"seek", dir_seek, 1);
    rb_define_method(rb_cDir,"pos", dir_tell, 0);
    rb_define_method(rb_cDir,"pos=", dir_set_pos, 1);
    rb_define_method(rb_cDir,"close", dir_close, 0);

    rb_define_singleton_method(rb_cDir,"chdir", dir_s_chdir, -1);
    rb_define_singleton_method(rb_cDir,"getwd", dir_s_getwd, 0);
    rb_define_singleton_method(rb_cDir,"pwd", dir_s_getwd, 0);
    rb_define_singleton_method(rb_cDir,"chroot", dir_s_chroot, 1);
    rb_define_singleton_method(rb_cDir,"mkdir", dir_s_mkdir, -1);
    rb_define_singleton_method(rb_cDir,"rmdir", dir_s_rmdir, 1);
    rb_define_singleton_method(rb_cDir,"delete", dir_s_rmdir, 1);
    rb_define_singleton_method(rb_cDir,"unlink", dir_s_rmdir, 1);

    rb_define_singleton_method(rb_cDir,"glob", dir_s_glob, -1);
    rb_define_singleton_method(rb_cDir,"[]", dir_s_aref, 1);

    rb_define_singleton_method(rb_cFile,"fnmatch", file_s_fnmatch, -1);
    rb_define_singleton_method(rb_cFile,"fnmatch?", file_s_fnmatch, -1);

    rb_file_const("FNM_NOESCAPE", INT2FIX(FNM_NOESCAPE));
    rb_file_const("FNM_PATHNAME", INT2FIX(FNM_PATHNAME));
    rb_file_const("FNM_DOTMATCH", INT2FIX(FNM_DOTMATCH));
    rb_file_const("FNM_CASEFOLD", INT2FIX(FNM_CASEFOLD));
}
/**********************************************************************

  dln.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Tue Jan 18 17:05:06 JST 1994

  Copyright (C) 1993-2003 Yukihiro Matsumoto

**********************************************************************/

#include "ruby.h"
#include "dln.h"

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef __CHECKER__
#undef HAVE_DLOPEN
#undef USE_DLN_A_OUT
#undef USE_DLN_DLOPEN
#endif

#ifdef USE_DLN_A_OUT
char *dln_argv0;
#endif

#ifdef _AIX
#pragma alloca
#endif

#if defined(HAVE_ALLOCA_H)
#include <alloca.h>
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#else
# include <strings.h>
#endif

#ifndef xmalloc
void *xmalloc();
void *xcalloc();
void *xrealloc();
#endif

#include <stdio.h>
#if defined(_WIN32) || defined(__VMS)
#include "missing/file.h"
#endif
#include <sys/types.h>
#include <sys/stat.h>

#ifndef S_ISDIR
#   define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#endif

#ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
#ifndef MAXPATHLEN
# define MAXPATHLEN 1024
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef _WIN32
char *getenv();
#endif

#if defined(__VMS)
#pragma builtins
#include <dlfcn.h>
#endif

#ifdef __MACOS__
# include <TextUtils.h>
# include <CodeFragments.h>
# include <Aliases.h>
# include "macruby_private.h"
#endif

#ifdef __BEOS__
# include <image.h>
#endif

int eaccess();

#ifndef NO_DLN_LOAD

#if defined(HAVE_DLOPEN) && !defined(USE_DLN_A_OUT) && !defined(_AIX) && !defined(__APPLE__) && !defined(_UNICOSMP)
/* dynamic load with dlopen() */
# define USE_DLN_DLOPEN
#endif

#ifndef FUNCNAME_PATTERN
# if defined(__hp9000s300) ||  (defined(__NetBSD__) && !defined(__ELF__)) || defined(__BORLANDC__) || (defined(__FreeBSD__) && !defined(__ELF__)) || (defined(__OpenBSD__) && !defined(__ELF__)) || defined(NeXT) || defined(__WATCOMC__) || defined(__APPLE__)
#  define FUNCNAME_PATTERN "_Init_%s"
# else
#  define FUNCNAME_PATTERN "Init_%s"
# endif
#endif

static int
init_funcname_len(buf, file)
    char **buf;
    const char *file;
{
    char *p;
    const char *slash;
    int len;

    /* Load the file as an object one */
    for (slash = file-1; *file; file++) /* Find position of last '/' */
#ifdef __MACOS__
	if (*file == ':') slash = file;
#else
	if (*file == '/') slash = file;
#endif

    len = strlen(FUNCNAME_PATTERN) + strlen(slash + 1);
    *buf = xmalloc(len);
    snprintf(*buf, len, FUNCNAME_PATTERN, slash + 1);
    for (p = *buf; *p; p++) {         /* Delete suffix if it exists */
	if (*p == '.') {
	    *p = '\0'; break;
	}
    }
    return p - *buf;
}

#define init_funcname(buf, file) do {\
    int len = init_funcname_len(buf, file);\
    char *tmp = ALLOCA_N(char, len+1);\
    if (!tmp) {\
	free(*buf);\
	rb_memerror();\
    }\
    strcpy(tmp, *buf);\
    free(*buf);\
    *buf = tmp;\
} while (0)

#ifdef USE_DLN_A_OUT

#ifndef LIBC_NAME
# define LIBC_NAME "libc.a"
#endif

#ifndef DLN_DEFAULT_LIB_PATH
#  define DLN_DEFAULT_LIB_PATH "/lib:/usr/lib:/usr/local/lib:."
#endif

#include <errno.h>

static int dln_errno;

#define DLN_ENOEXEC	ENOEXEC	/* Exec format error */
#define DLN_ECONFL	1201	/* Symbol name conflict */
#define DLN_ENOINIT	1202	/* No initializer given */
#define DLN_EUNDEF	1203	/* Undefine symbol remains */
#define DLN_ENOTLIB	1204	/* Not a library file */
#define DLN_EBADLIB	1205	/* Malformed library file */
#define DLN_EINIT	1206	/* Not initialized */

static int dln_init_p = 0;

#include <ar.h>
#include <a.out.h>
#ifndef N_COMM
# define N_COMM 0x12
#endif
#ifndef N_MAGIC
# define N_MAGIC(x) (x).a_magic
#endif

#define INVALID_OBJECT(h) (N_MAGIC(h) != OMAGIC)

#include "util.h"
#include "st.h"

static st_table *sym_tbl;
static st_table *undef_tbl;

static int load_lib();

static int
load_header(fd, hdrp, disp)
    int fd;
    struct exec *hdrp;
    long disp;
{
    int size;

    lseek(fd, disp, 0);
    size = read(fd, hdrp, sizeof(struct exec));
    if (size == -1) {
	dln_errno = errno;
	return -1;
    }
    if (size != sizeof(struct exec) || N_BADMAG(*hdrp)) {
	dln_errno = DLN_ENOEXEC;
	return -1;
    }
    return 0;
}

#if defined(sequent)
#define RELOC_SYMBOL(r)			((r)->r_symbolnum)
#define RELOC_MEMORY_SUB_P(r)		((r)->r_bsr)
#define RELOC_PCREL_P(r)		((r)->r_pcrel || (r)->r_bsr)
#define RELOC_TARGET_SIZE(r)		((r)->r_length)
#endif

/* Default macros */
#ifndef RELOC_ADDRESS
#define RELOC_ADDRESS(r)		((r)->r_address)
#define RELOC_EXTERN_P(r)		((r)->r_extern)
#define RELOC_SYMBOL(r)			((r)->r_symbolnum)
#define RELOC_MEMORY_SUB_P(r)		0
#define RELOC_PCREL_P(r)		((r)->r_pcrel)
#define RELOC_TARGET_SIZE(r)		((r)->r_length)
#endif

#if defined(sun) && defined(sparc)
/* Sparc (Sun 4) macros */
#  undef relocation_info
#  define relocation_info reloc_info_sparc
#  define R_RIGHTSHIFT(r)	(reloc_r_rightshift[(r)->r_type])
#  define R_BITSIZE(r) 		(reloc_r_bitsize[(r)->r_type])
#  define R_LENGTH(r)		(reloc_r_length[(r)->r_type])
static int reloc_r_rightshift[] = {
  0, 0, 0, 0, 0, 0, 2, 2, 10, 0, 0, 0, 0, 0, 0,
};
static int reloc_r_bitsize[] = {
  8, 16, 32, 8, 16, 32, 30, 22, 22, 22, 13, 10, 32, 32, 16,
};
static int reloc_r_length[] = {
  0, 1, 2, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
};
#  define R_PCREL(r) \
    ((r)->r_type >= RELOC_DISP8 && (r)->r_type <= RELOC_WDISP22)
#  define R_SYMBOL(r) ((r)->r_index)
#endif

#if defined(sequent)
#define R_SYMBOL(r)		((r)->r_symbolnum)
#define R_MEMORY_SUB(r)		((r)->r_bsr)
#define R_PCREL(r)		((r)->r_pcrel || (r)->r_bsr)
#define R_LENGTH(r)		((r)->r_length)
#endif

#ifndef R_SYMBOL
#  define R_SYMBOL(r) 		((r)->r_symbolnum)
#  define R_MEMORY_SUB(r)	0
#  define R_PCREL(r)  		((r)->r_pcrel)
#  define R_LENGTH(r) 		((r)->r_length)
#endif

static struct relocation_info *
load_reloc(fd, hdrp, disp)
     int fd;
     struct exec *hdrp;
     long disp;
{
    struct relocation_info *reloc;
    int size;

    lseek(fd, disp + N_TXTOFF(*hdrp) + hdrp->a_text + hdrp->a_data, 0);
    size = hdrp->a_trsize + hdrp->a_drsize;
    reloc = (struct relocation_info*)xmalloc(size);
    if (reloc == NULL) {
	dln_errno = errno;
	return NULL;
    }

    if (read(fd, reloc, size) !=  size) {
	dln_errno = errno;
	free(reloc);
	return NULL;
    }

    return reloc;
}

static struct nlist *
load_sym(fd, hdrp, disp)
    int fd;
    struct exec *hdrp;
    long disp;
{
    struct nlist * buffer;
    struct nlist * sym;
    struct nlist * end;
    long displ;
    int size;

    lseek(fd, N_SYMOFF(*hdrp) + hdrp->a_syms + disp, 0);
    if (read(fd, &size, sizeof(int)) != sizeof(int)) {
	goto err_noexec;
    }

    buffer = (struct nlist*)xmalloc(hdrp->a_syms + size);
    if (buffer == NULL) {
	dln_errno = errno;
	return NULL;
    }

    lseek(fd, disp + N_SYMOFF(*hdrp), 0);
    if (read(fd, buffer, hdrp->a_syms + size) != hdrp->a_syms + size) {
	free(buffer);
	goto err_noexec;
    }

    sym = buffer;
    end = sym + hdrp->a_syms / sizeof(struct nlist);
    displ = (long)buffer + (long)(hdrp->a_syms);

    while (sym < end) {
	sym->n_un.n_name = (char*)sym->n_un.n_strx + displ;
	sym++;
    }
    return buffer;

  err_noexec:
    dln_errno = DLN_ENOEXEC;
    return NULL;
}

static st_table *
sym_hash(hdrp, syms)
    struct exec *hdrp;
    struct nlist *syms;
{
    st_table *tbl;
    struct nlist *sym = syms;
    struct nlist *end = syms + (hdrp->a_syms / sizeof(struct nlist));

    tbl = st_init_strtable();
    if (tbl == NULL) {
	dln_errno = errno;
	return NULL;
    }

    while (sym < end) {
	st_insert(tbl, sym->n_un.n_name, sym);
	sym++;
    }
    return tbl;
}

static int
dln_init(prog)
    const char *prog;
{
    char *file;
    int fd;
    struct exec hdr;
    struct nlist *syms;

    if (dln_init_p == 1) return 0;

    file = dln_find_exe(prog, NULL);
    if (file == NULL || (fd = open(file, O_RDONLY)) < 0) {
	dln_errno = errno;
	return -1;
    }

    if (load_header(fd, &hdr, 0) == -1) return -1;
    syms = load_sym(fd, &hdr, 0);
    if (syms == NULL) {
	close(fd);
	return -1;
    }
    sym_tbl = sym_hash(&hdr, syms);
    if (sym_tbl == NULL) {	/* file may be start with #! */
	char c = '\0';
	char buf[MAXPATHLEN];
	char *p;

	free(syms);
	lseek(fd, 0L, 0);
	if (read(fd, &c, 1) == -1) {
	    dln_errno = errno;
	    return -1;
	}
	if (c != '#') goto err_noexec;
	if (read(fd, &c, 1) == -1) {
	    dln_errno = errno;
	    return -1;
	}
	if (c != '!') goto err_noexec;

	p = buf;
	/* skip forwarding spaces */
	while (read(fd, &c, 1) == 1) {
	    if (c == '\n') goto err_noexec;
	    if (c != '\t' && c != ' ') {
		*p++ = c;
		break;
	    }
	}
	/* read in command name */
	while (read(fd, p, 1) == 1) {
	    if (*p == '\n' || *p == '\t' || *p == ' ') break;
	    p++;
	    if (p-buf >= MAXPATHLEN) {
		dln_errno = ENAMETOOLONG;
		return -1;
	    }
	}
	*p = '\0';

	return dln_init(buf);
    }
    dln_init_p = 1;
    undef_tbl = st_init_strtable();
    close(fd);
    return 0;

  err_noexec:
    close(fd);
    dln_errno = DLN_ENOEXEC;
    return -1;
}

static long
load_text_data(fd, hdrp, bss, disp)
    int fd;
    struct exec *hdrp;
    int bss;
    long disp;
{
    int size;
    unsigned char* addr;

    lseek(fd, disp + N_TXTOFF(*hdrp), 0);
    size = hdrp->a_text + hdrp->a_data;

    if (bss == -1) size += hdrp->a_bss;
    else if (bss > 1) size += bss;

    addr = (unsigned char*)xmalloc(size);
    if (addr == NULL) {
	dln_errno = errno;
	return 0;
    }

    if (read(fd, addr, size) !=  size) {
	dln_errno = errno;
	free(addr);
	return 0;
    }

    if (bss == -1) {
	memset(addr +  hdrp->a_text + hdrp->a_data, 0, hdrp->a_bss);
    }
    else if (bss > 0) {
	memset(addr +  hdrp->a_text + hdrp->a_data, 0, bss);
    }

    return (long)addr;
}

static int
undef_print(key, value)
    char *key, *value;
{
    fprintf(stderr, "  %s\n", key);
    return ST_CONTINUE;
}

static void
dln_print_undef()
{
    fprintf(stderr, " Undefined symbols:\n");
    st_foreach(undef_tbl, undef_print, NULL);
}

static void
dln_undefined()
{
    if (undef_tbl->num_entries > 0) {
	fprintf(stderr, "dln: Calling undefined function\n");
	dln_print_undef();
	rb_exit(1);
    }
}

struct undef {
    char *name;
    struct relocation_info reloc;
    long base;
    char *addr;
    union {
	char c;
	short s;
	long l;
    } u;
};

static st_table *reloc_tbl = NULL;
static void
link_undef(name, base, reloc)
    const char *name;
    long base;
    struct relocation_info *reloc;
{
    static int u_no = 0;
    struct undef *obj;
    char *addr = (char*)(reloc->r_address + base);

    obj = (struct undef*)xmalloc(sizeof(struct undef));
    obj->name = strdup(name);
    obj->reloc = *reloc;
    obj->base = base;
    switch (R_LENGTH(reloc)) {
      case 0:		/* byte */
	obj->u.c = *addr;
	break;
      case 1:		/* word */
	obj->u.s = *(short*)addr;
	break;
      case 2:		/* long */
	obj->u.l = *(long*)addr;
	break;
    }
    if (reloc_tbl == NULL) {
	reloc_tbl = st_init_numtable();
    }
    st_insert(reloc_tbl, u_no++, obj);
}

struct reloc_arg {
    const char *name;
    long value;
};

static int
reloc_undef(no, undef, arg)
    int no;
    struct undef *undef;
    struct reloc_arg *arg;
{
    int datum;
    char *address;
#if defined(sun) && defined(sparc)
    unsigned int mask = 0;
#endif

    if (strcmp(arg->name, undef->name) != 0) return ST_CONTINUE;
    address = (char*)(undef->base + undef->reloc.r_address);
    datum = arg->value;

    if (R_PCREL(&(undef->reloc))) datum -= undef->base;
#if defined(sun) && defined(sparc)
    datum += undef->reloc.r_addend;
    datum >>= R_RIGHTSHIFT(&(undef->reloc));
    mask = (1 << R_BITSIZE(&(undef->reloc))) - 1;
    mask |= mask -1;
    datum &= mask;
    switch (R_LENGTH(&(undef->reloc))) {
      case 0:
	*address = undef->u.c;
	*address &= ~mask;
	*address |= datum;
	break;
      case 1:
	*(short *)address = undef->u.s;
	*(short *)address &= ~mask;
	*(short *)address |= datum;
	break;
      case 2:
	*(long *)address = undef->u.l;
	*(long *)address &= ~mask;
	*(long *)address |= datum;
	break;
    }
#else
    switch (R_LENGTH(&(undef->reloc))) {
      case 0:		/* byte */
	if (R_MEMORY_SUB(&(undef->reloc)))
	    *address = datum - *address;
	else *address = undef->u.c + datum;
	break;
      case 1:		/* word */
	if (R_MEMORY_SUB(&(undef->reloc)))
	    *(short*)address = datum - *(short*)address;
	else *(short*)address = undef->u.s + datum;
	break;
      case 2:		/* long */
	if (R_MEMORY_SUB(&(undef->reloc)))
	    *(long*)address = datum - *(long*)address;
	else *(long*)address = undef->u.l + datum;
	break;
    }
#endif
    free(undef->name);
    free(undef);
    return ST_DELETE;
}

static void
unlink_undef(name, value)
    const char *name;
    long value;
{
    struct reloc_arg arg;

    arg.name = name;
    arg.value = value;
    st_foreach(reloc_tbl, reloc_undef, &arg);
}

#ifdef N_INDR
struct indr_data {
    char *name0, *name1;
};

static int
reloc_repl(no, undef, data)
    int no;
    struct undef *undef;
    struct indr_data *data;
{
    if (strcmp(data->name0, undef->name) == 0) {
	free(undef->name);
	undef->name = strdup(data->name1);
    }
    return ST_CONTINUE;
}
#endif

static int
load_1(fd, disp, need_init)
    int fd;
    long disp;
    const char *need_init;
{
    static char *libc = LIBC_NAME;
    struct exec hdr;
    struct relocation_info *reloc = NULL;
    long block = 0;
    long new_common = 0; /* Length of new common */
    struct nlist *syms = NULL;
    struct nlist *sym;
    struct nlist *end;
    int init_p = 0;

    if (load_header(fd, &hdr, disp) == -1) return -1;
    if (INVALID_OBJECT(hdr)) {
	dln_errno = DLN_ENOEXEC;
	return -1;
    }
    reloc = load_reloc(fd, &hdr, disp);
    if (reloc == NULL) return -1;

    syms = load_sym(fd, &hdr, disp);
    if (syms == NULL) {
	free(reloc);
	return -1;
    }

    sym = syms;
    end = syms + (hdr.a_syms / sizeof(struct nlist));
    while (sym < end) {
	struct nlist *old_sym;
	int value = sym->n_value;

#ifdef N_INDR
	if (sym->n_type == (N_INDR | N_EXT)) {
	    char *key = sym->n_un.n_name;

	    if (st_lookup(sym_tbl, sym[1].n_un.n_name, &old_sym)) {
		if (st_delete(undef_tbl, (st_data_t*)&key, NULL)) {
		    unlink_undef(key, old_sym->n_value);
		    free(key);
		}
	    }
	    else {
		struct indr_data data;

		data.name0 = sym->n_un.n_name;
		data.name1 = sym[1].n_un.n_name;
		st_foreach(reloc_tbl, reloc_repl, &data);

		st_insert(undef_tbl, strdup(sym[1].n_un.n_name), NULL);
		if (st_delete(undef_tbl, (st_data_t*)&key, NULL)) {
		    free(key);
		}
	    }
	    sym += 2;
	    continue;
	}
#endif
	if (sym->n_type == (N_UNDF | N_EXT)) {
	    if (st_lookup(sym_tbl, sym->n_un.n_name, &old_sym) == 0) {
		old_sym = NULL;
	    }

	    if (value) {
		if (old_sym) {
		    sym->n_type = N_EXT | N_COMM;
		    sym->n_value = old_sym->n_value;
		}
		else {
		    int rnd =
			value >= sizeof(double) ? sizeof(double) - 1
			    : value >= sizeof(long) ? sizeof(long) - 1
				: sizeof(short) - 1;

		    sym->n_type = N_COMM;
		    new_common += rnd;
		    new_common &= ~(long)rnd;
		    sym->n_value = new_common;
		    new_common += value;
		}
	    }
	    else {
		if (old_sym) {
		    sym->n_type = N_EXT | N_COMM;
		    sym->n_value = old_sym->n_value;
		}
		else {
		    sym->n_value = (long)dln_undefined;
		    st_insert(undef_tbl, strdup(sym->n_un.n_name), NULL);
		}
	    }
	}
	sym++;
    }

    block = load_text_data(fd, &hdr, hdr.a_bss + new_common, disp);
    if (block == 0) goto err_exit;

    sym = syms;
    while (sym < end) {
	struct nlist *new_sym;
	char *key;

	switch (sym->n_type) {
	  case N_COMM:
	    sym->n_value += hdr.a_text + hdr.a_data;
	  case N_TEXT|N_EXT:
	  case N_DATA|N_EXT:

	    sym->n_value += block;

	    if (st_lookup(sym_tbl, sym->n_un.n_name, &new_sym) != 0
		&& new_sym->n_value != (long)dln_undefined) {
		dln_errno = DLN_ECONFL;
		goto err_exit;
	    }

	    key = sym->n_un.n_name;
	    if (st_delete(undef_tbl, (st_data_t*)&key, NULL) != 0) {
		unlink_undef(key, sym->n_value);
		free(key);
	    }

	    new_sym = (struct nlist*)xmalloc(sizeof(struct nlist));
	    *new_sym = *sym;
	    new_sym->n_un.n_name = strdup(sym->n_un.n_name);
	    st_insert(sym_tbl, new_sym->n_un.n_name, new_sym);
	    break;

	  case N_TEXT:
	  case N_DATA:
	    sym->n_value += block;
	    break;
	}
	sym++;
    }

    /*
     * First comes the text-relocation
     */
    {
	struct relocation_info * rel = reloc;
	struct relocation_info * rel_beg = reloc +
	    (hdr.a_trsize/sizeof(struct relocation_info));
	struct relocation_info * rel_end = reloc +
	    (hdr.a_trsize+hdr.a_drsize)/sizeof(struct relocation_info);

	while (rel < rel_end) {
	    char *address = (char*)(rel->r_address + block);
	    long datum = 0;
#if defined(sun) && defined(sparc)
	    unsigned int mask = 0;
#endif

	    if(rel >= rel_beg)
		address += hdr.a_text;

	    if (rel->r_extern) { /* Look it up in symbol-table */
		sym = &(syms[R_SYMBOL(rel)]);
		switch (sym->n_type) {
		  case N_EXT|N_UNDF:
		    link_undef(sym->n_un.n_name, block, rel);
		  case N_EXT|N_COMM:
		  case N_COMM:
		    datum = sym->n_value;
		    break;
		  default:
		    goto err_exit;
		}
	    } /* end.. look it up */
	    else { /* is static */
		switch (R_SYMBOL(rel)) { 
		  case N_TEXT:
		  case N_DATA:
		    datum = block;
		    break;
		  case N_BSS:
		    datum = block +  new_common;
		    break;
		  case N_ABS:
		    break;
		}
	    } /* end .. is static */
	    if (R_PCREL(rel)) datum -= block;

#if defined(sun) && defined(sparc)
	    datum += rel->r_addend;
	    datum >>= R_RIGHTSHIFT(rel);
	    mask = (1 << R_BITSIZE(rel)) - 1;
	    mask |= mask -1;
	    datum &= mask;

	    switch (R_LENGTH(rel)) {
	      case 0:
		*address &= ~mask;
		*address |= datum;
		break;
	      case 1:
		*(short *)address &= ~mask;
		*(short *)address |= datum;
		break;
	      case 2:
		*(long *)address &= ~mask;
		*(long *)address |= datum;
		break;
	    }
#else
	    switch (R_LENGTH(rel)) {
	      case 0:		/* byte */
		if (datum < -128 || datum > 127) goto err_exit;
		*address += datum;
		break;
	      case 1:		/* word */
		*(short *)address += datum;
		break;
	      case 2:		/* long */
		*(long *)address += datum;
		break;
	    }
#endif
	    rel++;
	}
    }

    if (need_init) {
	int len;
	char **libs_to_be_linked = 0;
	char *buf;

	if (undef_tbl->num_entries > 0) {
	    if (load_lib(libc) == -1) goto err_exit;
	}

	init_funcname(&buf, need_init);
	len = strlen(buf);

	for (sym = syms; sym<end; sym++) {
	    char *name = sym->n_un.n_name;
	    if (name[0] == '_' && sym->n_value >= block) {
		if (strcmp(name+1, "dln_libs_to_be_linked") == 0) {
		    libs_to_be_linked = (char**)sym->n_value;
		}
		else if (strcmp(name+1, buf) == 0) {
		    init_p = 1;
		    ((int (*)())sym->n_value)();
		}
	    }
	}
	if (libs_to_be_linked && undef_tbl->num_entries > 0) {
	    while (*libs_to_be_linked) {
		load_lib(*libs_to_be_linked);
		libs_to_be_linked++;
	    }
	}
    }
    free(reloc);
    free(syms);
    if (need_init) {
	if (init_p == 0) {
	    dln_errno = DLN_ENOINIT;
	    return -1;
	}
	if (undef_tbl->num_entries > 0) {
	    if (load_lib(libc) == -1) goto err_exit;
	    if (undef_tbl->num_entries > 0) {
		dln_errno = DLN_EUNDEF;
		return -1;
	    }
	}
    }
    return 0;

  err_exit:
    if (syms) free(syms);
    if (reloc) free(reloc);
    if (block) free((char*)block);
    return -1;
}

static int target_offset;
static int
search_undef(key, value, lib_tbl)
    const char *key;
    int value;
    st_table *lib_tbl;
{
    long offset;

    if (st_lookup(lib_tbl, key, &offset) == 0) return ST_CONTINUE;
    target_offset = offset;
    return ST_STOP;
}

struct symdef {
    int rb_str_index;
    int lib_offset;
};

char *dln_librrb_ary_path = DLN_DEFAULT_LIB_PATH;

static int
load_lib(lib)
    const char *lib;
{
    char *path, *file;
    char armagic[SARMAG];
    int fd, size;
    struct ar_hdr ahdr;
    st_table *lib_tbl = NULL;
    int *data, nsym;
    struct symdef *base;
    char *name_base;

    if (dln_init_p == 0) {
	dln_errno = DLN_ENOINIT;
	return -1;
    }

    if (undef_tbl->num_entries == 0) return 0;
    dln_errno = DLN_EBADLIB;

    if (lib[0] == '-' && lib[1] == 'l') {
	char *p = alloca(strlen(lib) + 4);
	sprintf(p, "lib%s.a", lib+2);
	lib = p;
    }

    /* library search path: */
    /* look for environment variable DLN_LIBRARY_PATH first. */
    /* then variable dln_librrb_ary_path. */
    /* if path is still NULL, use "." for path. */
    path = getenv("DLN_LIBRARY_PATH");
    if (path == NULL) path = dln_librrb_ary_path;

    file = dln_find_file(lib, path);
    fd = open(file, O_RDONLY);
    if (fd == -1) goto syserr;
    size = read(fd, armagic, SARMAG);
    if (size == -1) goto syserr;

    if (size != SARMAG) {
	dln_errno = DLN_ENOTLIB;
	goto badlib;
    }
    size = read(fd, &ahdr, sizeof(ahdr));
    if (size == -1) goto syserr;
    if (size != sizeof(ahdr) || sscanf(ahdr.ar_size, "%d", &size) != 1) {
	goto badlib;
    }

    if (strncmp(ahdr.ar_name, "__.SYMDEF", 9) == 0) {
	/* make hash table from __.SYMDEF */

	lib_tbl = st_init_strtable();
	data = (int*)xmalloc(size);
	if (data == NULL) goto syserr;
	size = read(fd, data, size);
	nsym = *data / sizeof(struct symdef);
	base = (struct symdef*)(data + 1);
	name_base = (char*)(base + nsym) + sizeof(int);
	while (nsym > 0) {
	    char *name = name_base + base->rb_str_index;

	    st_insert(lib_tbl, name, base->lib_offset + sizeof(ahdr));
	    nsym--;
	    base++;
	}
	for (;;) {
	    target_offset = -1;
	    st_foreach(undef_tbl, search_undef, lib_tbl);
	    if (target_offset == -1) break;
	    if (load_1(fd, target_offset, 0) == -1) {
		st_free_table(lib_tbl);
		free(data);
		goto badlib;
	    }
	    if (undef_tbl->num_entries == 0) break;
	}
	free(data);
	st_free_table(lib_tbl);
    }
    else {
	/* linear library, need to scan (FUTURE) */

	for (;;) {
	    int offset = SARMAG;
	    int found = 0;
	    struct exec hdr;
	    struct nlist *syms, *sym, *end;

	    while (undef_tbl->num_entries > 0) {
		found = 0;
		lseek(fd, offset, 0);
		size = read(fd, &ahdr, sizeof(ahdr));
		if (size == -1) goto syserr;
		if (size == 0) break;
		if (size != sizeof(ahdr)
		    || sscanf(ahdr.ar_size, "%d", &size) != 1) {
		    goto badlib;
		}
		offset += sizeof(ahdr);
		if (load_header(fd, &hdr, offset) == -1)
		    goto badlib;
		syms = load_sym(fd, &hdr, offset);
		if (syms == NULL) goto badlib;
		sym = syms;
		end = syms + (hdr.a_syms / sizeof(struct nlist));
		while (sym < end) {
		    if (sym->n_type == N_EXT|N_TEXT
			&& st_lookup(undef_tbl, sym->n_un.n_name, NULL)) {
			break;
		    }
		    sym++;
		}
		if (sym < end) {
		    found++;
		    free(syms);
		    if (load_1(fd, offset, 0) == -1) {
			goto badlib;
		    }
		}
		offset += size;
		if (offset & 1) offset++;
	    }
	    if (found) break;
	}
    }
    close(fd);
    return 0;

  syserr:
    dln_errno = errno;
  badlib:
    if (fd >= 0) close(fd);
    return -1;
}

static int
load(file)
    const char *file;
{
    int fd;
    int result;

    if (dln_init_p == 0) {
	if (dln_init(dln_argv0) == -1) return -1;
    }
    result = strlen(file);
    if (file[result-1] == 'a') {
	return load_lib(file);
    }

    fd = open(file, O_RDONLY);
    if (fd == -1) {
	dln_errno = errno;
	return -1;
    }
    result = load_1(fd, 0, file);
    close(fd);

    return result;
}

void*
dln_sym(name)
    const char *name;
{
    struct nlist *sym;

    if (st_lookup(sym_tbl, name, &sym))
	return (void*)sym->n_value;
    return NULL;
}

#endif /* USE_DLN_A_OUT */

#ifdef USE_DLN_DLOPEN
# include <dlfcn.h>
#endif

#ifdef __hpux
#include <errno.h>
#include "dl.h"
#endif

#if defined(_AIX)
#include <ctype.h>	/* for isdigit()	*/
#include <errno.h>	/* for global errno	*/
#include <sys/ldr.h>
#endif

#ifdef NeXT
#if NS_TARGET_MAJOR < 4
#include <mach-o/rld.h>
#else
#include <mach-o/dyld.h>
#ifndef NSLINKMODULE_OPTION_BINDNOW
#define NSLINKMODULE_OPTION_BINDNOW 1
#endif
#endif
#else
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
#endif

#if defined _WIN32 && !defined __CYGWIN__
#include <windows.h>
#endif

#ifdef _WIN32_WCE
#undef FormatMessage
#define FormatMessage FormatMessageA
#undef LoadLibrary
#define LoadLibrary LoadLibraryA
#undef GetProcAddress
#define GetProcAddress GetProcAddressA
#endif

static const char *
dln_strerror()
{
#ifdef USE_DLN_A_OUT
    char *strerror();

    switch (dln_errno) {
      case DLN_ECONFL:
	return "Symbol name conflict";
      case DLN_ENOINIT:
	return "No initializer given";
      case DLN_EUNDEF:
	return "Unresolved symbols";
      case DLN_ENOTLIB:
	return "Not a library file";
      case DLN_EBADLIB:
	return "Malformed library file";
      case DLN_EINIT:
	return "Not initialized";
      default:
	return strerror(dln_errno);
    }
#endif

#ifdef USE_DLN_DLOPEN
    return (char*)dlerror();
#endif

#if defined _WIN32 && !defined __CYGWIN__
    static char message[1024];
    int error = GetLastError();
    char *p = message;
    p += sprintf(message, "%d: ", error);
    FormatMessage(
	FORMAT_MESSAGE_FROM_SYSTEM	 | FORMAT_MESSAGE_IGNORE_INSERTS,
	NULL,
	error,
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
	p,
	sizeof message - strlen(message),
	NULL);

    for (p = message; *p; p++) {
	if (*p == '\n' || *p == '\r')
	    *p = ' ';
    }
    return message;
#endif
}


#if defined(_AIX) && ! defined(_IA64)
static void
aix_loaderror(const char *pathname)
{
    char *message[8], errbuf[1024];
    int i,j;

    struct errtab { 
	int errnum;
	char *errstr;
    } load_errtab[] = {
	{L_ERROR_TOOMANY,	"too many errors, rest skipped."},
	{L_ERROR_NOLIB,		"can't load library:"},
	{L_ERROR_UNDEF,		"can't find symbol in library:"},
	{L_ERROR_RLDBAD,
	     "RLD index out of range or bad relocation type:"},
	{L_ERROR_FORMAT,	"not a valid, executable xcoff file:"},
	{L_ERROR_MEMBER,
	     "file not an archive or does not contain requested member:"},
	{L_ERROR_TYPE,		"symbol table mismatch:"},
	{L_ERROR_ALIGN,		"text alignment in file is wrong."},
	{L_ERROR_SYSTEM,	"System error:"},
	{L_ERROR_ERRNO,		NULL}
    };

#define LOAD_ERRTAB_LEN	(sizeof(load_errtab)/sizeof(load_errtab[0]))
#define ERRBUF_APPEND(s) strncat(errbuf, s, sizeof(errbuf)-strlen(errbuf)-1)

    snprintf(errbuf, 1024, "load failed - %s ", pathname);

    if (!loadquery(1, &message[0], sizeof(message))) 
	ERRBUF_APPEND(strerror(errno));
    for(i = 0; message[i] && *message[i]; i++) {
	int nerr = atoi(message[i]);
	for (j=0; j<LOAD_ERRTAB_LEN; j++) {
           if (nerr == load_errtab[i].errnum && load_errtab[i].errstr)
		ERRBUF_APPEND(load_errtab[i].errstr);
	}
	while (isdigit(*message[i])) message[i]++; 
	ERRBUF_APPEND(message[i]);
	ERRBUF_APPEND("\n");
    }
    errbuf[strlen(errbuf)-1] = '\0';	/* trim off last newline */
    rb_loaderror(errbuf);
    return;
}
#endif

#endif /* NO_DLN_LOAD */

void*
dln_load(file)
    const char *file;
{
#ifdef NO_DLN_LOAD
    rb_raise(rb_eLoadError, "this executable file can't load extension libraries");
#else

#if !defined(_AIX) && !defined(NeXT)
    const char *error = 0;
#define DLN_ERROR() (error = dln_strerror(), strcpy(ALLOCA_N(char, strlen(error) + 1), error))
#endif

#if defined _WIN32 && !defined __CYGWIN__
    HINSTANCE handle;
    char winfile[MAXPATHLEN];
    void (*init_fct)();
    char *buf;

    if (strlen(file) >= MAXPATHLEN) rb_loaderror("filename too long");

    /* Load the file as an object one */
    init_funcname(&buf, file);

    strcpy(winfile, file);

    /* Load file */
    if ((handle = LoadLibrary(winfile)) == NULL) {
	error = dln_strerror();
	goto failed;
    }

    if ((init_fct = (void(*)())GetProcAddress(handle, buf)) == NULL) {
	rb_loaderror("%s - %s\n%s", dln_strerror(), buf, file);
    }

    /* Call the init code */
    (*init_fct)();
    return handle;
#else
#ifdef USE_DLN_A_OUT
    if (load(file) == -1) {
	error = dln_strerror();
	goto failed;
    }
    return 0;
#else

    char *buf;
    /* Load the file as an object one */
    init_funcname(&buf, file);

#ifdef USE_DLN_DLOPEN
#define DLN_DEFINED
    {
	void *handle;
	void (*init_fct)();

#ifndef RTLD_LAZY
# define RTLD_LAZY 1
#endif
#ifdef __INTERIX
# undef RTLD_GLOBAL
#endif
#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL 0
#endif

	/* Load file */
	if ((handle = (void*)dlopen(file, RTLD_LAZY|RTLD_GLOBAL)) == NULL) {
	    error = dln_strerror();
	    goto failed;
	}

	init_fct = (void(*)())dlsym(handle, buf);
	if (init_fct == NULL) {
	    error = DLN_ERROR();
	    dlclose(handle);
	    goto failed;
	}
	/* Call the init code */
	(*init_fct)();

	return handle;
    }
#endif /* USE_DLN_DLOPEN */

#ifdef __hpux
#define DLN_DEFINED
    {
	shl_t lib = NULL;
	int flags;
	void (*init_fct)();

	flags = BIND_DEFERRED;
	lib = shl_load(file, flags, 0);
	if (lib == NULL) {
	    extern int errno;
	    rb_loaderror("%s - %s", strerror(errno), file);
	}
	shl_findsym(&lib, buf, TYPE_PROCEDURE, (void*)&init_fct);
	if (init_fct == NULL) {
	    shl_findsym(&lib, buf, TYPE_UNDEFINED, (void*)&init_fct);
	    if (init_fct == NULL) {
		errno = ENOSYM;
		rb_loaderror("%s - %s", strerror(ENOSYM), file);
	    }
	}
	(*init_fct)();
	return (void*)lib;
    }
#endif /* hpux */

#if defined(_AIX) && ! defined(_IA64)
#define DLN_DEFINED
    {
	void (*init_fct)();

	init_fct = (void(*)())load((char*)file, 1, 0);
	if (init_fct == NULL) {
	    aix_loaderror(file);
	}
	if (loadbind(0, (void*)dln_load, (void*)init_fct) == -1) {
	    aix_loaderror(file);
	}
	(*init_fct)();
	return (void*)init_fct;
    }
#endif /* _AIX */

#if defined(NeXT) || defined(__APPLE__)
#define DLN_DEFINED
/*----------------------------------------------------
   By SHIROYAMA Takayuki Psi@fortune.nest.or.jp
 
   Special Thanks...
    Yu tomoak-i@is.aist-nara.ac.jp,
    Mi hisho@tasihara.nest.or.jp,
    sunshine@sunshineco.com,
    and... Miss ARAI Akino(^^;)
 ----------------------------------------------------*/
#if defined(NeXT) && (NS_TARGET_MAJOR < 4)/* NeXTSTEP rld functions */

    {
        NXStream* s;
	unsigned long init_address;
	char *object_files[2] = {NULL, NULL};

	void (*init_fct)();
	
	object_files[0] = (char*)file;
	
	s = NXOpenFile(2,NX_WRITEONLY);

	/* Load object file, if return value ==0 ,  load failed*/
	if(rld_load(s, NULL, object_files, NULL) == 0) {
	    NXFlush(s);
	    NXClose(s);
	    rb_loaderror("Failed to load %.200s", file);
	}

	/* lookup the initial function */
	if(rld_lookup(s, buf, &init_address) == 0) {
	    NXFlush(s);
	    NXClose(s);
	    rb_loaderror("Failed to lookup Init function %.200s", file);
	}

	NXFlush(s);
	NXClose(s);

	/* Cannot call *init_address directory, so copy this value to
	   funtion pointer */
	init_fct = (void(*)())init_address;
	(*init_fct)();
	return (void*)init_address;
    }
#else/* OPENSTEP dyld functions */
    {
	int dyld_result;
	NSObjectFileImage obj_file; /* handle, but not use it */
	/* "file" is module file name .
	   "buf" is pointer to initial function name with "_" . */

	void (*init_fct)();


	dyld_result = NSCreateObjectFileImageFromFile(file, &obj_file);

	if (dyld_result != NSObjectFileImageSuccess) {
	    rb_loaderror("Failed to load %.200s", file);
	}

	NSLinkModule(obj_file, file, NSLINKMODULE_OPTION_BINDNOW);

	/* lookup the initial function */
	if(!NSIsSymbolNameDefined(buf)) {
	    rb_loaderror("Failed to lookup Init function %.200s",file);
	}	
	init_fct = NSAddressOfSymbol(NSLookupAndBindSymbol(buf));
	(*init_fct)();

	return (void*)init_fct;
    }
#endif /* rld or dyld */
#endif

#ifdef __BEOS__
# define DLN_DEFINED
    {
      status_t err_stat;  /* BeOS error status code */
      image_id img_id;    /* extention module unique id */
      void (*init_fct)(); /* initialize function for extention module */

      /* load extention module */
      img_id = load_add_on(file);
      if (img_id <= 0) {
	rb_loaderror("Failed to load %.200s", file);
      }
      
      /* find symbol for module initialize function. */
      /* The Be Book KernelKit Images section described to use
	 B_SYMBOL_TYPE_TEXT for symbol of function, not
	 B_SYMBOL_TYPE_CODE. Why ? */
      /* strcat(init_fct_symname, "__Fv"); */  /* parameter nothing. */
      /* "__Fv" dont need! The Be Book Bug ? */
      err_stat = get_image_symbol(img_id, buf,
				  B_SYMBOL_TYPE_TEXT, (void **)&init_fct);

      if (err_stat != B_NO_ERROR) {
	char real_name[MAXPATHLEN];

	strcpy(real_name, buf);
	strcat(real_name, "__Fv");
        err_stat = get_image_symbol(img_id, real_name,
				    B_SYMBOL_TYPE_TEXT, (void **)&init_fct);
      }

      if ((B_BAD_IMAGE_ID == err_stat) || (B_BAD_INDEX == err_stat)) {
	unload_add_on(img_id);
	rb_loaderror("Failed to lookup Init function %.200s", file);
      }
      else if (B_NO_ERROR != err_stat) {
	char errmsg[] = "Internal of BeOS version. %.200s (symbol_name = %s)";
	unload_add_on(img_id);
	rb_loaderror(errmsg, strerror(err_stat), buf);
      }

      /* call module initialize function. */
      (*init_fct)();
      return (void*)img_id;
    }
#endif /* __BEOS__*/

#ifdef __MACOS__
# define DLN_DEFINED
    {
      OSErr err;
      FSSpec libspec;
      CFragConnectionID connID;
      Ptr mainAddr;
      char errMessage[1024];
      Boolean isfolder, didsomething;
      Str63 fragname;
      Ptr symAddr;
      CFragSymbolClass class;
      void (*init_fct)();
      char fullpath[MAXPATHLEN];

      strcpy(fullpath, file);

      /* resolve any aliases to find the real file */
      c2pstr(fullpath);
      (void)FSMakeFSSpec(0, 0, fullpath, &libspec);
      err = ResolveAliasFile(&libspec, 1, &isfolder, &didsomething);
      if (err) {
	  rb_loaderror("Unresolved Alias - %s", file);
      }

      /* Load the fragment (or return the connID if it is already loaded */
      fragname[0] = 0;
      err = GetDiskFragment(&libspec, 0, 0, fragname, 
			    kLoadCFrag, &connID, &mainAddr,
			    errMessage);
      if (err) {
	  p2cstr(errMessage);
	  rb_loaderror("%s - %s",errMessage , file);
      }

      /* Locate the address of the correct init function */
      c2pstr(buf);
      err = FindSymbol(connID, buf, &symAddr, &class);
      if (err) {
	  rb_loaderror("Unresolved symbols - %s" , file);
      }
      init_fct = (void (*)())symAddr;
      (*init_fct)();
      return (void*)init_fct;
    }
#endif /* __MACOS__ */

#if defined(__VMS)
#define DLN_DEFINED
    {
	void *handle, (*init_fct)();
	char *fname, *p1, *p2;

	fname = (char *)__alloca(strlen(file)+1);
	strcpy(fname,file);
	if (p1 = strrchr(fname,'/'))
	    fname = p1 + 1;
	if (p2 = strrchr(fname,'.'))
	    *p2 = '\0';

	if ((handle = (void*)dlopen(fname, 0)) == NULL) {
	    error = dln_strerror();
	    goto failed;
	}

	if ((init_fct = (void (*)())dlsym(handle, buf)) == NULL) {
	    error = DLN_ERROR();
	    dlclose(handle);
	    goto failed;
	}
	/* Call the init code */
	(*init_fct)();
	return handle;
    }
#endif /* __VMS */

#ifndef DLN_DEFINED
    rb_notimplement();
#endif

#endif /* USE_DLN_A_OUT */
#endif
#if !defined(_AIX) && !defined(NeXT)
  failed:
    rb_loaderror("%s - %s", error, file);
#endif

#endif /* NO_DLN_LOAD */
    return 0;			/* dummy return */
}

static char *dln_find_1();

char *
dln_find_exe(fname, path)
    const char *fname;
    const char *path;
{
    if (!path) {
	path = getenv(PATH_ENV);
    }

    if (!path) {
#if defined(MSDOS) || defined(_WIN32) || defined(__human68k__) || defined(__MACOS__)
	path = "/usr/local/bin;/usr/ucb;/usr/bin;/bin;.";
#else
	path = "/usr/local/bin:/usr/ucb:/usr/bin:/bin:.";
#endif
    }
    return dln_find_1(fname, path, 1);
}

char *
dln_find_file(fname, path)
    const char *fname;
    const char *path;
{
#ifndef __MACOS__
    if (!path) path = ".";
    return dln_find_1(fname, path, 0);
#else
    if (!path) path = ".";
    return _macruby_path_conv_posix_to_macos(dln_find_1(fname, path, 0));
#endif
}

#if defined(__CYGWIN32__)
const char *
conv_to_posix_path(win32, posix, len)
    char *win32;
    char *posix;
    int len;
{
    char *first = win32;
    char *p = win32;
    char *dst = posix;

    for (p = win32; *p; p++)
	if (*p == ';') {
	    *p = 0;
	    cygwin32_conv_to_posix_path(first, posix);
	    posix += strlen(posix);
	    *posix++ = ':';
	    first = p + 1;
	    *p = ';';
	}
    if (len < strlen(first))
	fprintf(stderr, "PATH length too long: %s\n", first);
    else
	cygwin32_conv_to_posix_path(first, posix);
    return dst;
}
#endif

static char fbuf[MAXPATHLEN];

static char *
dln_find_1(fname, path, exe_flag)
    char *fname;
    char *path;
    int exe_flag;		/* non 0 if looking for executable. */
{
    register char *dp;
    register char *ep;
    register char *bp;
    struct stat st;
#ifdef __MACOS__
    const char* mac_fullpath;
#endif

    if (!fname) return fname;
    if (fname[0] == '/') return fname;
    if (strncmp("./", fname, 2) == 0 || strncmp("../", fname, 3) == 0)
      return fname;
    if (exe_flag && strchr(fname, '/')) return fname;
#ifdef DOSISH
    if (fname[0] == '\\') return fname;
# ifdef DOSISH_DRIVE_LETTER
    if (strlen(fname) > 2 && fname[1] == ':') return fname;
# endif
    if (strncmp(".\\", fname, 2) == 0 || strncmp("..\\", fname, 3) == 0)
      return fname;
    if (exe_flag && strchr(fname, '\\')) return fname;
#endif

    for (dp = path;; dp = ++ep) {
	register int l;
	int i;
	int fspace;

	/* extract a component */
	ep = strchr(dp, PATH_SEP[0]);
	if (ep == NULL)
	    ep = dp+strlen(dp);

	/* find the length of that component */
	l = ep - dp;
	bp = fbuf;
	fspace = sizeof fbuf - 2;
	if (l > 0) {
	    /*
	    **	If the length of the component is zero length,
	    **	start from the current directory.  If the
	    **	component begins with "~", start from the
	    **	user's $HOME environment variable.  Otherwise
	    **	take the path literally.
	    */

	    if (*dp == '~' && (l == 1 ||
#if defined(DOSISH)
			       dp[1] == '\\' || 
#endif
			       dp[1] == '/')) {
		char *home;

		home = getenv("HOME");
		if (home != NULL) {
		    i = strlen(home);
		    if ((fspace -= i) < 0)
			goto toolong;
		    memcpy(bp, home, i);
		    bp += i;
		}
		dp++;
		l--;
	    }
	    if (l > 0) {
		if ((fspace -= l) < 0)
		    goto toolong;
		memcpy(bp, dp, l);
		bp += l;
	    }

	    /* add a "/" between directory and filename */
	    if (ep[-1] != '/')
		*bp++ = '/';
	}

	/* now append the file name */
	i = strlen(fname);
	if ((fspace -= i) < 0) {
	  toolong:
	    fprintf(stderr, "openpath: pathname too long (ignored)\n");
	    *bp = '\0';
	    fprintf(stderr, "\tDirectory \"%s\"\n", fbuf);
	    fprintf(stderr, "\tFile \"%s\"\n", fname);
	    goto next;
	}
	memcpy(bp, fname, i + 1);

#ifndef __MACOS__
	if (stat(fbuf, &st) == 0) {
	    if (exe_flag == 0) return fbuf;
	    /* looking for executable */
	    if (!S_ISDIR(st.st_mode) && eaccess(fbuf, X_OK) == 0)
		return fbuf;
	}
#else
	if (mac_fullpath = _macruby_exist_file_in_libdir_as_posix_name(fbuf)) {
	    if (exe_flag == 0) return mac_fullpath;
	    /* looking for executable */
	    if (stat(mac_fullpath, &st) == 0) {
		if (!S_ISDIR(st.st_mode) && eaccess(mac_fullpath, X_OK) == 0)
		    return mac_fullpath;
	    }
	}
#endif
#if defined(DOSISH)
	if (exe_flag) {
	    static const char *extension[] = {
#if defined(MSDOS)
		".com", ".exe", ".bat",
#if defined(DJGPP)
		".btm", ".sh", ".ksh", ".pl", ".sed",
#endif
#elif defined(__EMX__) || defined(_WIN32)
		".exe", ".com", ".cmd", ".bat",
/* end of __EMX__ or _WIN32 */
#else
		".r", ".R", ".x", ".X", ".bat", ".BAT",
/* __human68k__ */
#endif
		(char *) NULL
	    };
	    int j;

	    for (j = 0; extension[j]; j++) {
		if (fspace < strlen(extension[j])) {
		    fprintf(stderr, "openpath: pathname too long (ignored)\n");
		    fprintf(stderr, "\tDirectory \"%.*s\"\n", (int) (bp - fbuf), fbuf);
		    fprintf(stderr, "\tFile \"%s%s\"\n", fname, extension[j]);
		    continue;
		}
		strcpy(bp + i, extension[j]);
#ifndef __MACOS__
		if (stat(fbuf, &st) == 0)
		    return fbuf;
#else
		if (mac_fullpath = _macruby_exist_file_in_libdir_as_posix_name(fbuf))
		    return mac_fullpath;

#endif
	    }
	}
#endif /* MSDOS or _WIN32 or __human68k__ or __EMX__ */

      next:
	/* if not, and no other alternatives, life is bleak */
	if (*ep == '\0') {
	    return NULL;
	}

	/* otherwise try the next component in the search path */
    }
}
#define NO_DLN_LOAD 1
#include "dln.c"
void
Init_ext()
{
}
/**********************************************************************

  enum.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Fri Oct  1 15:15:19 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto

**********************************************************************/

#include "ruby.h"
#include "node.h"
#include "util.h"

VALUE rb_mEnumerable;
static ID id_each, id_eqq, id_cmp;

VALUE
rb_each(obj)
    VALUE obj;
{
    return rb_funcall(obj, id_each, 0, 0);
}

static VALUE
grep_i(i, arg)
    VALUE i, *arg;
{
    if (RTEST(rb_funcall(arg[0], id_eqq, 1, i))) {
	rb_ary_push(arg[1], i);
    }
    return Qnil;
}

static VALUE
grep_iter_i(i, arg)
    VALUE i, *arg;
{
    if (RTEST(rb_funcall(arg[0], id_eqq, 1, i))) {
	rb_ary_push(arg[1], rb_yield(i));
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.grep(pattern)                   => array
 *     enum.grep(pattern) {| obj | block }  => array
 *  
 *  Returns an array of every element in <i>enum</i> for which
 *  <code>Pattern === element</code>. If the optional <em>block</em> is
 *  supplied, each matching element is passed to it, and the block's
 *  result is stored in the output array.
 *     
 *     (1..100).grep 38..44   #=> [38, 39, 40, 41, 42, 43, 44]
 *     c = IO.constants
 *     c.grep(/SEEK/)         #=> ["SEEK_END", "SEEK_SET", "SEEK_CUR"]
 *     res = c.grep(/SEEK/) {|v| IO.const_get(v) }
 *     res                    #=> [2, 0, 1]
 *     
 */

static VALUE
enum_grep(obj, pat)
    VALUE obj, pat;
{
    VALUE ary = rb_ary_new();
    VALUE arg[2];

    arg[0] = pat;
    arg[1] = ary;

    rb_iterate(rb_each, obj, rb_block_given_p() ? grep_iter_i : grep_i, (VALUE)arg);
    
    return ary;
}

static VALUE
find_i(i, memo)
    VALUE i;
    NODE *memo;
{
    if (RTEST(rb_yield(i))) {
	memo->u2.value = Qtrue;
	memo->u1.value = i;
	rb_iter_break();
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.detect(ifnone = nil) {| obj | block }  => obj or nil
 *     enum.find(ifnone = nil)   {| obj | block }  => obj or nil
 *  
 *  Passes each entry in <i>enum</i> to <em>block</em>. Returns the
 *  first for which <em>block</em> is not <code>false</code>.  If no
 *  object matches, calls <i>ifnone</i> and returns its result when it
 *  is specified, or returns <code>nil</code>
 *     
 *     (1..10).detect  {|i| i % 5 == 0 and i % 7 == 0 }   #=> nil
 *     (1..100).detect {|i| i % 5 == 0 and i % 7 == 0 }   #=> 35
 *     
 */

static VALUE
enum_find(argc, argv, obj)
    int argc;
    VALUE* argv;
    VALUE obj;
{
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, Qfalse, 0);
    VALUE if_none;

    rb_scan_args(argc, argv, "01", &if_none);
    rb_iterate(rb_each, obj, find_i, (VALUE)memo);
    if (memo->u2.value) {
	return memo->u1.value;
    }
    if (!NIL_P(if_none)) {
	return rb_funcall(if_none, rb_intern("call"), 0, 0);
    }
    return Qnil;
}

static VALUE
find_all_i(i, ary)
    VALUE i, ary;
{
    if (RTEST(rb_yield(i))) {
	rb_ary_push(ary, i);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.find_all {| obj | block }  => array
 *     enum.select   {| obj | block }  => array
 *  
 *  Returns an array containing all elements of <i>enum</i> for which
 *  <em>block</em> is not <code>false</code> (see also
 *  <code>Enumerable#reject</code>).
 *     
 *     (1..10).find_all {|i|  i % 3 == 0 }   #=> [3, 6, 9]
 *     
 */

static VALUE
enum_find_all(obj)
    VALUE obj;
{
    VALUE ary = rb_ary_new();
    
    rb_iterate(rb_each, obj, find_all_i, ary);

    return ary;
}

static VALUE
reject_i(i, ary)
    VALUE i, ary;
{
    if (!RTEST(rb_yield(i))) {
	rb_ary_push(ary, i);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.reject {| obj | block }  => array
 *  
 *  Returns an array for all elements of <i>enum</i> for which
 *  <em>block</em> is false (see also <code>Enumerable#find_all</code>).
 *     
 *     (1..10).reject {|i|  i % 3 == 0 }   #=> [1, 2, 4, 5, 7, 8, 10]
 *     
 */

static VALUE
enum_reject(obj)
    VALUE obj;
{
    VALUE ary = rb_ary_new();
    
    rb_iterate(rb_each, obj, reject_i, ary);

    return ary;
}

static VALUE
collect_i(i, ary)
    VALUE i, ary;
{
    rb_ary_push(ary, rb_yield(i));

    return Qnil;
}

static VALUE
collect_all(i, ary)
    VALUE i, ary;
{
    rb_ary_push(ary, i);

    return Qnil;
}

/*
 *  call-seq:
 *     enum.collect {| obj | block }  => array
 *     enum.map     {| obj | block }  => array
 *  
 *  Returns a new array with the results of running <em>block</em> once
 *  for every element in <i>enum</i>.
 *     
 *     (1..4).collect {|i| i*i }   #=> [1, 4, 9, 16]
 *     (1..4).collect { "cat"  }   #=> ["cat", "cat", "cat", "cat"]
 *     
 */

static VALUE
enum_collect(obj)
    VALUE obj;
{
    VALUE ary = rb_ary_new();

    rb_iterate(rb_each, obj, rb_block_given_p() ? collect_i : collect_all, ary);

    return ary;
}

/*
 *  call-seq:
 *     enum.to_a      =>    array
 *     enum.entries   =>    array
 *  
 *  Returns an array containing the items in <i>enum</i>.
 *     
 *     (1..7).to_a                       #=> [1, 2, 3, 4, 5, 6, 7]
 *     { 'a'=>1, 'b'=>2, 'c'=>3 }.to_a   #=> [["a", 1], ["b", 2], ["c", 3]]
 */
static VALUE
enum_to_a(obj)
    VALUE obj;
{
    VALUE ary = rb_ary_new();

    rb_iterate(rb_each, obj, collect_all, ary);

    return ary;
}

static VALUE
inject_i(i, memo)
    VALUE i;
    NODE *memo;
{
    if (memo->u2.value) {
        memo->u2.value = Qfalse;
        memo->u1.value = i;
    }
    else {
        memo->u1.value = rb_yield_values(2, memo->u1.value, i);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.inject(initial) {| memo, obj | block }  => obj
 *     enum.inject          {| memo, obj | block }  => obj
 *  
 *  Combines the elements of <i>enum</i> by applying the block to an
 *  accumulator value (<i>memo</i>) and each element in turn. At each
 *  step, <i>memo</i> is set to the value returned by the block. The
 *  first form lets you supply an initial value for <i>memo</i>. The
 *  second form uses the first element of the collection as a the
 *  initial value (and skips that element while iterating).
 *     
 *     # Sum some numbers
 *     (5..10).inject {|sum, n| sum + n }              #=> 45
 *     # Multiply some numbers
 *     (5..10).inject(1) {|product, n| product * n }   #=> 151200
 *     
 *     # find the longest word
 *     longest = %w{ cat sheep bear }.inject do |memo,word|
 *        memo.length > word.length ? memo : word
 *     end
 *     longest                                         #=> "sheep"
 *     
 *     # find the length of the longest word
 *     longest = %w{ cat sheep bear }.inject(0) do |memo,word|
 *        memo >= word.length ? memo : word.length
 *     end
 *     longest                                         #=> 5
 *     
 */

static VALUE
enum_inject(argc, argv, obj)
    int argc;
    VALUE *argv, obj;
{
    NODE *memo;
    VALUE n;

    if (rb_scan_args(argc, argv, "01", &n) == 1) {
        memo = rb_node_newnode(NODE_MEMO, n, Qfalse, 0);
    }
    else {
        memo = rb_node_newnode(NODE_MEMO, Qnil, Qtrue, 0);
    }
    rb_iterate(rb_each, obj, inject_i, (VALUE)memo);
    n = memo->u1.value;
    return n;
}

static VALUE
partition_i(i, ary)
    VALUE i, *ary;
{
    if (RTEST(rb_yield(i))) {
	rb_ary_push(ary[0], i);
    }
    else {
	rb_ary_push(ary[1], i);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.partition {| obj | block }  => [ true_array, false_array ]
 *  
 *  Returns two arrays, the first containing the elements of
 *  <i>enum</i> for which the block evaluates to true, the second
 *  containing the rest.
 *     
 *     (1..6).partition {|i| (i&1).zero?}   #=> [[2, 4, 6], [1, 3, 5]]
 *     
 */

static VALUE
enum_partition(obj)
    VALUE obj;
{
    VALUE ary[2];

    ary[0] = rb_ary_new();
    ary[1] = rb_ary_new();
    rb_iterate(rb_each, obj, partition_i, (VALUE)ary);

    return rb_assoc_new(ary[0], ary[1]);
}

/*
 *  call-seq:
 *     enum.sort                     => array
 *     enum.sort {| a, b | block }   => array
 *  
 *  Returns an array containing the items in <i>enum</i> sorted,
 *  either according to their own <code><=></code> method, or by using
 *  the results of the supplied block. The block should return -1, 0, or
 *  +1 depending on the comparison between <i>a</i> and <i>b</i>. As of
 *  Ruby 1.8, the method <code>Enumerable#sort_by</code> implements a
 *  built-in Schwartzian Transform, useful when key computation or
 *  comparison is expensive..
 *     
 *     %w(rhea kea flea).sort         #=> ["flea", "kea", "rhea"]
 *     (1..10).sort {|a,b| b <=> a}   #=> [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
 */

static VALUE
enum_sort(obj)
    VALUE obj;
{
    return rb_ary_sort(enum_to_a(obj));
}

static VALUE
sort_by_i(i, ary)
    VALUE i, ary;
{
    VALUE v;
    NODE *memo;

    v = rb_yield(i);
    if (RBASIC(ary)->klass) {
	rb_raise(rb_eRuntimeError, "sort_by reentered");
    }
    memo = rb_node_newnode(NODE_MEMO, v, i, 0);
    rb_ary_push(ary, (VALUE)memo);
    return Qnil;
}

static int
sort_by_cmp(aa, bb)
    NODE **aa, **bb;
{
    VALUE a = aa[0]->u1.value;
    VALUE b = bb[0]->u1.value;

    return rb_cmpint(rb_funcall(a, id_cmp, 1, b), a, b);
}

/*
 *  call-seq:
 *     enum.sort_by {| obj | block }    => array
 *  
 *  Sorts <i>enum</i> using a set of keys generated by mapping the
 *  values in <i>enum</i> through the given block.
 *     
 *     %w{ apple pear fig }.sort_by {|word| word.length}
                    #=> ["fig", "pear", "apple"]
 *     
 *  The current implementation of <code>sort_by</code> generates an
 *  array of tuples containing the original collection element and the
 *  mapped value. This makes <code>sort_by</code> fairly expensive when
 *  the keysets are simple
 *     
 *     require 'benchmark'
 *     include Benchmark
 *     
 *     a = (1..100000).map {rand(100000)}
 *     
 *     bm(10) do |b|
 *       b.report("Sort")    { a.sort }
 *       b.report("Sort by") { a.sort_by {|a| a} }
 *     end
 *     
 *  <em>produces:</em>
 *     
 *     user     system      total        real
 *     Sort        0.180000   0.000000   0.180000 (  0.175469)
 *     Sort by     1.980000   0.040000   2.020000 (  2.013586)
 *     
 *  However, consider the case where comparing the keys is a non-trivial
 *  operation. The following code sorts some files on modification time
 *  using the basic <code>sort</code> method.
 *     
 *     files = Dir["*"]
 *     sorted = files.sort {|a,b| File.new(a).mtime <=> File.new(b).mtime}
 *     sorted   #=> ["mon", "tues", "wed", "thurs"]
 *     
 *  This sort is inefficient: it generates two new <code>File</code>
 *  objects during every comparison. A slightly better technique is to
 *  use the <code>Kernel#test</code> method to generate the modification
 *  times directly.
 *     
 *     files = Dir["*"]
 *     sorted = files.sort { |a,b|
 *       test(?M, a) <=> test(?M, b)
 *     }
 *     sorted   #=> ["mon", "tues", "wed", "thurs"]
 *     
 *  This still generates many unnecessary <code>Time</code> objects. A
 *  more efficient technique is to cache the sort keys (modification
 *  times in this case) before the sort. Perl users often call this
 *  approach a Schwartzian Transform, after Randal Schwartz. We
 *  construct a temporary array, where each element is an array
 *  containing our sort key along with the filename. We sort this array,
 *  and then extract the filename from the result.
 *     
 *     sorted = Dir["*"].collect { |f|
 *        [test(?M, f), f]
 *     }.sort.collect { |f| f[1] }
 *     sorted   #=> ["mon", "tues", "wed", "thurs"]
 *     
 *  This is exactly what <code>sort_by</code> does internally.
 *     
 *     sorted = Dir["*"].sort_by {|f| test(?M, f)}
 *     sorted   #=> ["mon", "tues", "wed", "thurs"]
 */

static VALUE
enum_sort_by(obj)
    VALUE obj;
{
    VALUE ary;
    long i;

    if (TYPE(obj) == T_ARRAY) {
	ary  = rb_ary_new2(RARRAY(obj)->len);
    }
    else {
	ary = rb_ary_new();
    }
    RBASIC(ary)->klass = 0;
    rb_iterate(rb_each, obj, sort_by_i, ary);
    if (RARRAY(ary)->len > 1) {
	qsort(RARRAY(ary)->ptr, RARRAY(ary)->len, sizeof(VALUE), sort_by_cmp, 0);
    }
    if (RBASIC(ary)->klass) {
	rb_raise(rb_eRuntimeError, "sort_by reentered");
    }
    for (i=0; i<RARRAY(ary)->len; i++) {
	RARRAY(ary)->ptr[i] = RNODE(RARRAY(ary)->ptr[i])->u2.value;
    }
    RBASIC(ary)->klass = rb_cArray;
    return ary;
}

static VALUE
all_iter_i(i, memo)
    VALUE i;
    NODE *memo;
{
    if (!RTEST(rb_yield(i))) {
	memo->u1.value = Qfalse;
	rb_iter_break();
    }
    return Qnil;
}

static VALUE
all_i(i, memo)
    VALUE i;
    NODE *memo;
{
    if (!RTEST(i)) {
	memo->u1.value = Qfalse;
	rb_iter_break();
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.all? [{|obj| block } ]   => true or false
 *  
 *  Passes each element of the collection to the given block. The method
 *  returns <code>true</code> if the block never returns
 *  <code>false</code> or <code>nil</code>. If the block is not given,
 *  Ruby adds an implicit block of <code>{|obj| obj}</code> (that is
 *  <code>all?</code> will return <code>true</code> only if none of the
 *  collection members are <code>false</code> or <code>nil</code>.)
 *     
 *     %w{ ant bear cat}.all? {|word| word.length >= 3}   #=> true
 *     %w{ ant bear cat}.all? {|word| word.length >= 4}   #=> false
 *     [ nil, true, 99 ].all?                             #=> false
 *     
 */

static VALUE
enum_all(obj)
    VALUE obj;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, 0, 0);

    memo->u1.value = Qtrue;
    rb_iterate(rb_each, obj, rb_block_given_p() ? all_iter_i : all_i, (VALUE)memo);
    result = memo->u1.value;
    return result;
}

static VALUE
any_iter_i(i, memo)
    VALUE i;
    NODE *memo;
{
    if (RTEST(rb_yield(i))) {
	memo->u1.value = Qtrue;
	rb_iter_break();
    }
    return Qnil;
}

static VALUE
any_i(i, memo)
    VALUE i;
    NODE *memo;
{
    if (RTEST(i)) {
	memo->u1.value = Qtrue;
	rb_iter_break();
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.any? [{|obj| block } ]   => true or false
 *  
 *  Passes each element of the collection to the given block. The method
 *  returns <code>true</code> if the block ever returns a value other
 *  that <code>false</code> or <code>nil</code>. If the block is not
 *  given, Ruby adds an implicit block of <code>{|obj| obj}</code> (that
 *  is <code>any?</code> will return <code>true</code> if at least one
 *  of the collection members is not <code>false</code> or
 *  <code>nil</code>.
 *     
 *     %w{ ant bear cat}.any? {|word| word.length >= 3}   #=> true
 *     %w{ ant bear cat}.any? {|word| word.length >= 4}   #=> true
 *     [ nil, true, 99 ].any?                             #=> true
 *     
 */

static VALUE
enum_any(obj)
    VALUE obj;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, 0, 0);

    memo->u1.value = Qfalse;
    rb_iterate(rb_each, obj, rb_block_given_p() ? any_iter_i : any_i, (VALUE)memo);
    result = memo->u1.value;
    return result;
}

static VALUE
min_i(i, memo)
    VALUE i;
    NODE *memo;
{
    VALUE cmp;

    if (NIL_P(memo->u1.value)) {
	memo->u1.value = i;
    }
    else {
	cmp = rb_funcall(i, id_cmp, 1, memo->u1.value);
	if (rb_cmpint(cmp, i, memo->u1.value) < 0) {
	    memo->u1.value = i;
	}
    }
    return Qnil;
}

static VALUE
min_ii(i, memo)
    VALUE i;
    NODE *memo;
{
    VALUE cmp;

    if (NIL_P(memo->u1.value)) {
	memo->u1.value = i;
    }
    else {
	cmp = rb_yield_values(2, i, memo->u1.value);
	if (rb_cmpint(cmp, i, memo->u1.value) < 0) {
	    memo->u1.value = i;
	}
    }
    return Qnil;
}


/*
 *  call-seq:
 *     enum.min                    => obj
 *     enum.min {| a,b | block }   => obj
 *  
 *  Returns the object in <i>enum</i> with the minimum value. The
 *  first form assumes all objects implement <code>Comparable</code>;
 *  the second uses the block to return <em>a <=> b</em>.
 *     
 *     a = %w(albatross dog horse)
 *     a.min                                  #=> "albatross"
 *     a.min {|a,b| a.length <=> b.length }   #=> "dog"
 */

static VALUE
enum_min(obj)
    VALUE obj;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, 0, 0);

    rb_iterate(rb_each, obj, rb_block_given_p() ? min_ii : min_i, (VALUE)memo);
    result = memo->u1.value;
    return result;
}

static VALUE
max_i(i, memo)
    VALUE i;
    NODE *memo;
{
    VALUE cmp;

    if (NIL_P(memo->u1.value)) {
	memo->u1.value = i;
    }
    else {
	cmp = rb_funcall(i, id_cmp, 1, memo->u1.value);
	if (rb_cmpint(cmp, i, memo->u1.value) > 0) {
	    memo->u1.value = i;
	}
    }
    return Qnil;
}

static VALUE
max_ii(i, memo)
    VALUE i;
    NODE *memo;
{
    VALUE cmp;

    if (NIL_P(memo->u1.value)) {
	memo->u1.value = i;
    }
    else {
	cmp = rb_yield_values(2, i, memo->u1.value);
	if (rb_cmpint(cmp, i, memo->u1.value) > 0) {
	    memo->u1.value = i;
	}
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.max                   => obj
 *     enum.max {|a,b| block }    => obj
 *  
 *  Returns the object in _enum_ with the maximum value. The
 *  first form assumes all objects implement <code>Comparable</code>;
 *  the second uses the block to return <em>a <=> b</em>.
 *     
 *     a = %w(albatross dog horse)
 *     a.max                                  #=> "horse"
 *     a.max {|a,b| a.length <=> b.length }   #=> "albatross"
 */  

static VALUE
enum_max(obj)
    VALUE obj;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, 0, 0);

    rb_iterate(rb_each, obj, rb_block_given_p() ? max_ii : max_i, (VALUE)memo);
    result = memo->u1.value;
    return result;
}

static VALUE
min_by_i(i, memo)
    VALUE i;
    NODE *memo;
{
    VALUE v;

    v = rb_yield(i);
    if (NIL_P(memo->u1.value)) {
	memo->u1.value = v;
	memo->u2.value = i;
    }
    else if (rb_cmpint(rb_funcall(v, id_cmp, 1, memo->u1.value), v, memo->u1.value) < 0) {
	memo->u1.value = v;
	memo->u2.value = i;
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.min_by {| obj| block }   => obj
 *  
 *  Returns the object in <i>enum</i> that gives the minimum
 *  value from the given block.
 *     
 *     a = %w(albatross dog horse)
 *     a.min_by {|x| x.length }   #=> "dog"
 */

static VALUE
enum_min_by(obj)
    VALUE obj;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, 0, 0);

    rb_iterate(rb_each, obj, min_by_i, (VALUE)memo);
    result = memo->u2.value;
    return result;
}

static VALUE
max_by_i(i, memo)
    VALUE i;
    NODE *memo;
{
    VALUE v;

    v = rb_yield(i);
    if (NIL_P(memo->u1.value)) {
	memo->u1.value = v;
	memo->u2.value = i;
    }
    else if (rb_cmpint(rb_funcall(v, id_cmp, 1, memo->u1.value), v, memo->u1.value) > 0) {
	memo->u1.value = v;
	memo->u2.value = i;
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.max_by {| obj| block }   => obj
 *  
 *  Returns the object in <i>enum</i> that gives the maximum
 *  value from the given block.
 *     
 *     a = %w(albatross dog horse)
 *     a.max_by {|x| x.length }   #=> "albatross"
 */

static VALUE
enum_max_by(obj)
    VALUE obj;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, Qnil, 0, 0);

    rb_iterate(rb_each, obj, max_by_i, (VALUE)memo);
    result = memo->u2.value;
    return result;
}

static VALUE
member_i(item, memo)
    VALUE item;
    NODE *memo;
{
    if (rb_equal(item, memo->u1.value)) {
	memo->u2.value = Qtrue;
	rb_iter_break();
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.include?(obj)     => true or false
 *     enum.member?(obj)      => true or false
 *  
 *  Returns <code>true</code> if any member of <i>enum</i> equals
 *  <i>obj</i>. Equality is tested using <code>==</code>.
 *     
 *     IO.constants.include? "SEEK_SET"          #=> true
 *     IO.constants.include? "SEEK_NO_FURTHER"   #=> false
 *     
 */

static VALUE
enum_member(obj, val)
    VALUE obj, val;
{
    VALUE result;
    NODE *memo = rb_node_newnode(NODE_MEMO, val, Qfalse, 0);

    rb_iterate(rb_each, obj, member_i, (VALUE)memo);
    result = memo->u2.value;
    return result;
}

static VALUE
each_with_index_i(val, memo)
    VALUE val;
    NODE *memo;
{
    rb_yield_values(2, val, INT2FIX(memo->u3.cnt));
    memo->u3.cnt++;
    return Qnil;
}

/*
 *  call-seq:
 *     enum.each_with_index {|obj, i| block }  -> enum
 *  
 *  Calls <em>block</em> with two arguments, the item and its index, for
 *  each item in <i>enum</i>.
 *     
 *     hash = Hash.new
 *     %w(cat dog wombat).each_with_index {|item, index|
 *       hash[item] = index
 *     }
 *     hash   #=> {"cat"=>0, "wombat"=>2, "dog"=>1}
 *     
 */

static VALUE
enum_each_with_index(obj)
    VALUE obj;
{
    NODE *memo = rb_node_newnode(NODE_MEMO, 0, 0, 0);

    rb_iterate(rb_each, obj, each_with_index_i, (VALUE)memo);
    return obj;
}

static VALUE
zip_i(val, memo)
    VALUE val;
    NODE *memo;
{
    VALUE result = memo->u1.value;
    VALUE args = memo->u2.value;
    int idx = memo->u3.cnt++;
    VALUE tmp;
    int i;

    tmp = rb_ary_new2(RARRAY(args)->len + 1);
    rb_ary_store(tmp, 0, val);
    for (i=0; i<RARRAY(args)->len; i++) {
	rb_ary_push(tmp, rb_ary_entry(RARRAY(args)->ptr[i], idx));
    }
    if (rb_block_given_p()) {
	rb_yield(tmp);
    }
    else {
	rb_ary_push(result, tmp);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     enum.zip(arg, ...)                   => array
 *     enum.zip(arg, ...) {|arr| block }    => nil
 *  
 *  Converts any arguments to arrays, then merges elements of
 *  <i>enum</i> with corresponding elements from each argument. This
 *  generates a sequence of <code>enum#size</code> <em>n</em>-element
 *  arrays, where <em>n</em> is one more that the count of arguments. If
 *  the size of any argument is less than <code>enum#size</code>,
 *  <code>nil</code> values are supplied. If a block given, it is
 *  invoked for each output array, otherwise an array of arrays is
 *  returned.
 *     
 *     a = [ 4, 5, 6 ]
 *     b = [ 7, 8, 9 ]
 *     
 *     (1..3).zip(a, b)      #=> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
 *     "cat\ndog".zip([1])   #=> [["cat\n", 1], ["dog", nil]]
 *     (1..3).zip            #=> [[1], [2], [3]]
 *     
 */

static VALUE
enum_zip(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    int i;
    VALUE result;
    NODE *memo;

    for (i=0; i<argc; i++) {
	argv[i] = rb_convert_type(argv[i], T_ARRAY, "Array", "to_a");
    }
    result = rb_block_given_p() ? Qnil : rb_ary_new();
    memo = rb_node_newnode(NODE_MEMO, result, rb_ary_new4(argc, argv), 0);
    rb_iterate(rb_each, obj, zip_i, (VALUE)memo);

    return result;
}

/*
 *  The <code>Enumerable</code> mixin provides collection classes with
 *  several traversal and searching methods, and with the ability to
 *  sort. The class must provide a method <code>each</code>, which
 *  yields successive members of the collection. If
 *  <code>Enumerable#max</code>, <code>#min</code>, or
 *  <code>#sort</code> is used, the objects in the collection must also
 *  implement a meaningful <code><=></code> operator, as these methods
 *  rely on an ordering between members of the collection.
 */

void
Init_Enumerable()
{
    rb_mEnumerable = rb_define_module("Enumerable");

    rb_define_method(rb_mEnumerable,"to_a", enum_to_a, 0);
    rb_define_method(rb_mEnumerable,"entries", enum_to_a, 0);

    rb_define_method(rb_mEnumerable,"sort", enum_sort, 0);
    rb_define_method(rb_mEnumerable,"sort_by", enum_sort_by, 0);
    rb_define_method(rb_mEnumerable,"grep", enum_grep, 1);
    rb_define_method(rb_mEnumerable,"find", enum_find, -1);
    rb_define_method(rb_mEnumerable,"detect", enum_find, -1);
    rb_define_method(rb_mEnumerable,"find_all", enum_find_all, 0);
    rb_define_method(rb_mEnumerable,"select", enum_find_all, 0);
    rb_define_method(rb_mEnumerable,"reject", enum_reject, 0);
    rb_define_method(rb_mEnumerable,"collect", enum_collect, 0);
    rb_define_method(rb_mEnumerable,"map", enum_collect, 0);
    rb_define_method(rb_mEnumerable,"inject", enum_inject, -1);
    rb_define_method(rb_mEnumerable,"partition", enum_partition, 0);
    rb_define_method(rb_mEnumerable,"all?", enum_all, 0);
    rb_define_method(rb_mEnumerable,"any?", enum_any, 0);
    rb_define_method(rb_mEnumerable,"min", enum_min, 0);
    rb_define_method(rb_mEnumerable,"max", enum_max, 0);
    rb_define_method(rb_mEnumerable,"min_by", enum_min_by, 0);
    rb_define_method(rb_mEnumerable,"max_by", enum_max_by, 0);
    rb_define_method(rb_mEnumerable,"member?", enum_member, 1);
    rb_define_method(rb_mEnumerable,"include?", enum_member, 1);
    rb_define_method(rb_mEnumerable,"each_with_index", enum_each_with_index, 0);
    rb_define_method(rb_mEnumerable, "zip", enum_zip, -1);

    id_eqq  = rb_intern("===");
    id_each = rb_intern("each");
    id_cmp  = rb_intern("<=>");
}

/**********************************************************************

  error.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Mon Aug  9 16:11:34 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto

**********************************************************************/

#include "ruby.h"
#include "env.h"
#include "st.h"

#include <stdio.h>
#ifdef HAVE_STDARG_PROTOTYPES
#include <stdarg.h>
#define va_init_list(a,b) va_start(a,b)
#else
#include <varargs.h>
#define va_init_list(a,b) va_start(a)
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

extern const char ruby_version[], ruby_release_date[], ruby_platform[];

int ruby_nerrs;

static int
err_position(buf, len)
    char *buf;
    long len;
{
    ruby_set_current_source();
    if (!ruby_sourcefile) {
	return 0;
    }
    else if (ruby_sourceline == 0) {
	return snprintf(buf, len, "%s: ", ruby_sourcefile);
    }
    else {
	return snprintf(buf, len, "%s:%d: ", ruby_sourcefile, ruby_sourceline);
    }
}

static void
err_snprintf(buf, len, fmt, args)
    char *buf;
    long len;
    const char *fmt;
    va_list args;
{
    long n;

    n = err_position(buf, len);
    if (len > n) {
	vsnprintf((char*)buf+n, len-n, fmt, args);
    }
}

static void err_append _((const char*));
static void
err_print(fmt, args)
    const char *fmt;
    va_list args;
{
    char buf[BUFSIZ];

    err_snprintf(buf, BUFSIZ, fmt, args);
    err_append(buf);
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_compile_error(const char *fmt, ...)
#else
rb_compile_error(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    va_list args;

    va_init_list(args, fmt);
    err_print(fmt, args);
    va_end(args);
    ruby_nerrs++;
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_compile_error_append(const char *fmt, ...)
#else
rb_compile_error_append(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    va_list args;
    char buf[BUFSIZ];

    va_init_list(args, fmt);
    vsnprintf(buf, BUFSIZ, fmt, args);
    va_end(args);
    err_append(buf);
}

static void
warn_print(fmt, args)
    const char *fmt;
    va_list args;
{
    char buf[BUFSIZ];
    int len;

    err_snprintf(buf, BUFSIZ, fmt, args);
    len = strlen(buf);
    buf[len++] = '\n';
    rb_write_error2(buf, len);
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_warn(const char *fmt, ...)
#else
rb_warn(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    char buf[BUFSIZ];
    va_list args;

    if (NIL_P(ruby_verbose)) return;

    snprintf(buf, BUFSIZ, "warning: %s", fmt);

    va_init_list(args, fmt);
    warn_print(buf, args);
    va_end(args);
}

/* rb_warning() reports only in verbose mode */
void
#ifdef HAVE_STDARG_PROTOTYPES
rb_warning(const char *fmt, ...)
#else
rb_warning(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    char buf[BUFSIZ];
    va_list args;

    if (!RTEST(ruby_verbose)) return;

    snprintf(buf, BUFSIZ, "warning: %s", fmt);

    va_init_list(args, fmt);
    warn_print(buf, args);
    va_end(args);
}

/*
 * call-seq:
 *    warn(msg)   => nil
 *
 * Display the given message (followed by a newline) on STDERR unless
 * warnings are disabled (for example with the <code>-W0</code> flag).
 */

static VALUE
rb_warn_m(self, mesg)
    VALUE self, mesg;
{
    if (!NIL_P(ruby_verbose)) {
	rb_io_write(rb_stderr, mesg);
	rb_io_write(rb_stderr, rb_default_rs);
    }
    return Qnil;
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_bug(const char *fmt, ...)
#else
rb_bug(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    char buf[BUFSIZ];
    va_list args;
    FILE *out = stderr;
    int len = err_position(buf, BUFSIZ);

    if (fwrite(buf, 1, len, out) == len ||
	fwrite(buf, 1, len, (out = stdout)) == len) {
	fputs("[BUG] ", out);
	va_init_list(args, fmt);
	vfprintf(out, fmt, args);
	va_end(args);
	fprintf(out, "\nruby %s (%s) [%s]\n\n",
		ruby_version, ruby_release_date, ruby_platform);
    }
    abort();
}

static struct types {
    int type;
    const char *name;
} builtin_types[] = {
    {T_NIL,	"nil"},
    {T_OBJECT,	"Object"},
    {T_CLASS,	"Class"},
    {T_ICLASS,	"iClass"},	/* internal use: mixed-in module holder */
    {T_MODULE,	"Module"},
    {T_FLOAT,	"Float"},
    {T_STRING,	"String"},
    {T_REGEXP,	"Regexp"},
    {T_ARRAY,	"Array"},
    {T_FIXNUM,	"Fixnum"},
    {T_HASH,	"Hash"},
    {T_STRUCT,	"Struct"},
    {T_BIGNUM,	"Bignum"},
    {T_FILE,	"File"},
    {T_TRUE,	"true"},
    {T_FALSE,	"false"},
    {T_SYMBOL,	"Symbol"},	/* :symbol */
    {T_DATA,	"Data"},	/* internal use: wrapped C pointers */
    {T_MATCH,	"MatchData"},	/* data of $~ */
    {T_VARMAP,	"Varmap"},	/* internal use: dynamic variables */
    {T_SCOPE,	"Scope"},	/* internal use: variable scope */
    {T_NODE,	"Node"},	/* internal use: syntax tree node */
    {T_UNDEF,	"undef"},	/* internal use: #undef; should not happen */
    {-1,	0}
};

void
rb_check_type(x, t)
    VALUE x;
    int t;
{
    struct types *type = builtin_types;

    if (x == Qundef) {
	rb_bug("undef leaked to the Ruby space");
    }

    if (TYPE(x) != t) {
	while (type->type >= 0) {
	    if (type->type == t) {
		char *etype;

		if (NIL_P(x)) {
		    etype = "nil";
		}
		else if (FIXNUM_P(x)) {
		    etype = "Fixnum";
		}
		else if (SYMBOL_P(x)) {
		    etype = "Symbol";
		}
		else if (rb_special_const_p(x)) {
		    etype = RSTRING(rb_obj_as_string(x))->ptr;
		}
		else {
		    etype = rb_obj_classname(x);
		}
		rb_raise(rb_eTypeError, "wrong argument type %s (expected %s)",
			 etype, type->name);
	    }
	    type++;
	}
	rb_bug("unknown type 0x%x (0x%x given)", t, TYPE(x));
    }
}

/* exception classes */
#include <errno.h>

VALUE rb_eException;
VALUE rb_eSystemExit;
VALUE rb_eInterrupt;
VALUE rb_eSignal;
VALUE rb_eFatal;
VALUE rb_eStandardError;
VALUE rb_eRuntimeError;
VALUE rb_eTypeError;
VALUE rb_eArgError;
VALUE rb_eIndexError;
VALUE rb_eKeyError;
VALUE rb_eRangeError;
VALUE rb_eNameError;
VALUE rb_eNoMethodError;
VALUE rb_eSecurityError;
VALUE rb_eNotImpError;
VALUE rb_eNoMemError;
static VALUE rb_cNameErrorMesg;

VALUE rb_eScriptError;
VALUE rb_eSyntaxError;
VALUE rb_eLoadError;

VALUE rb_eSystemCallError;
VALUE rb_mErrno;
static VALUE eNOERROR;

VALUE
rb_exc_new(etype, ptr, len)
    VALUE etype;
    const char *ptr;
    long len;
{
    return rb_funcall(etype, rb_intern("new"), 1, rb_str_new(ptr, len));
}

VALUE
rb_exc_new2(etype, s)
    VALUE etype;
    const char *s;
{
    return rb_exc_new(etype, s, strlen(s));
}

VALUE
rb_exc_new3(etype, str)
    VALUE etype, str;
{
    StringValue(str);
    return rb_funcall(etype, rb_intern("new"), 1, str);
}

/*
 * call-seq:
 *    Exception.new(msg = nil)   =>  exception
 *
 *  Construct a new Exception object, optionally passing in 
 *  a message.
 */

static VALUE
exc_initialize(argc, argv, exc)
    int argc;
    VALUE *argv;
    VALUE exc;
{
    VALUE arg;

    rb_scan_args(argc, argv, "01", &arg);
    rb_iv_set(exc, "mesg", arg);
    rb_iv_set(exc, "bt", Qnil);

    return exc;
}

/*
 *  Document-method: exception
 *
 *  call-seq:
 *     exc.exception(string) -> an_exception or exc
 *  
 *  With no argument, or if the argument is the same as the receiver,
 *  return the receiver. Otherwise, create a new
 *  exception object of the same class as the receiver, but with a
 *  message equal to <code>string.to_str</code>.
 *     
 */

static VALUE
exc_exception(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    VALUE exc;

    if (argc == 0) return self;
    if (argc == 1 && self == argv[0]) return self;
    exc = rb_obj_clone(self);
    exc_initialize(argc, argv, exc);

    return exc;
}

/*
 * call-seq:
 *   exception.to_s   =>  string
 *
 * Returns exception's message (or the name of the exception if
 * no message is set).
 */

static VALUE
exc_to_s(exc)
    VALUE exc;
{
    VALUE mesg = rb_attr_get(exc, rb_intern("mesg"));

    if (NIL_P(mesg)) return rb_class_name(CLASS_OF(exc));
    if (OBJ_TAINTED(exc)) OBJ_TAINT(mesg);
    return mesg;
}

/*
 * call-seq:
 *   exception.message   =>  string
 *
 * Returns the result of invoking <code>exception.to_s</code>.
 * Normally this returns the exception's message or name. By
 * supplying a to_str method, exceptions are agreeing to
 * be used where Strings are expected.
 */

static VALUE
exc_message(exc)
    VALUE exc;
{
    return rb_funcall(exc, rb_intern("to_s"), 0, 0);
}

/*
 * call-seq:
 *   exception.inspect   => string
 *
 * Return this exception's class name an message
 */

static VALUE
exc_inspect(exc)
    VALUE exc;
{
    VALUE str, klass;

    klass = CLASS_OF(exc);
    exc = rb_obj_as_string(exc);
    if (RSTRING(exc)->len == 0) {
	return rb_str_dup(rb_class_name(klass));
    }

    str = rb_str_buf_new2("#<");
    klass = rb_class_name(klass);
    rb_str_buf_append(str, klass);
    rb_str_buf_cat(str, ": ", 2);
    rb_str_buf_append(str, exc);
    rb_str_buf_cat(str, ">", 1);

    return str;
}

/*
 *  call-seq:
 *     exception.backtrace    => array
 *  
 *  Returns any backtrace associated with the exception. The backtrace
 *  is an array of strings, each containing either ``filename:lineNo: in
 *  `method''' or ``filename:lineNo.''
 *     
 *     def a
 *       raise "boom"
 *     end
 *     
 *     def b
 *       a()
 *     end
 *     
 *     begin
 *       b()
 *     rescue => detail
 *       print detail.backtrace.join("\n")
 *     end
 *     
 *  <em>produces:</em>
 *     
 *     prog.rb:2:in `a'
 *     prog.rb:6:in `b'
 *     prog.rb:10
*/

static VALUE
exc_backtrace(exc)
    VALUE exc;
{
    ID bt = rb_intern("bt");

    if (!rb_ivar_defined(exc, bt)) return Qnil;
    return rb_ivar_get(exc, bt);
}

static VALUE
check_backtrace(bt)
    VALUE bt;
{
    long i;
    static char *err = "backtrace must be Array of String";

    if (!NIL_P(bt)) {
	int t = TYPE(bt);

	if (t == T_STRING) return rb_ary_new3(1, bt);
	if (t != T_ARRAY) {
	    rb_raise(rb_eTypeError, err);
	}
	for (i=0;i<RARRAY(bt)->len;i++) {
	    if (TYPE(RARRAY(bt)->ptr[i]) != T_STRING) {
		rb_raise(rb_eTypeError, err);
	    }
	}
    }
    return bt;
}

/*
 *  call-seq:
 *     exc.set_backtrace(array)   =>  array
 *  
 *  Sets the backtrace information associated with <i>exc</i>. The
 *  argument must be an array of <code>String</code> objects in the
 *  format described in <code>Exception#backtrace</code>.
 *     
 */

static VALUE
exc_set_backtrace(exc, bt)
    VALUE exc;
    VALUE bt;
{
    return rb_iv_set(exc, "bt", check_backtrace(bt));
}

/*
 *  call-seq:
 *     exc == obj   => true or false
 *  
 *  Equality---If <i>obj</i> is not an <code>Exception</code>, returns
 *  <code>false</code>. Otherwise, returns <code>true</code> if <i>exc</i> and 
 *  <i>obj</i> share same class, messages, and backtrace.
 */

static VALUE
exc_equal(exc, obj)
    VALUE exc;
    VALUE obj;
{
    ID id_mesg = rb_intern("mesg");

    if (exc == obj) return Qtrue;
    if (rb_obj_class(exc) != rb_obj_class(obj))
	return Qfalse;
    if (!rb_equal(rb_attr_get(exc, id_mesg), rb_attr_get(obj, id_mesg)))
	return Qfalse;
    if (!rb_equal(exc_backtrace(exc), exc_backtrace(obj)))
	return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *   SystemExit.new(status=0)   => system_exit
 *
 * Create a new +SystemExit+ exception with the given status.
 */

static VALUE
exit_initialize(argc, argv, exc)
    int argc;
    VALUE *argv;
    VALUE exc;
{
    VALUE status = INT2FIX(EXIT_SUCCESS);
    if (argc > 0 && FIXNUM_P(argv[0])) {
	status = *argv++;
	--argc;
    }
    exc_initialize(argc, argv, exc);
    rb_iv_set(exc, "status", status);
    return exc;
}


/*
 * call-seq:
 *   system_exit.status   => fixnum
 *
 * Return the status value associated with this system exit.
 */

static VALUE
exit_status(exc)
    VALUE exc;
{
    return rb_attr_get(exc, rb_intern("status"));
}


/*
 * call-seq:
 *   system_exit.success?  => true or false
 *
 * Returns +true+ if exiting successful, +false+ if not.
 */

static VALUE
exit_success_p(exc)
    VALUE exc;
{
    VALUE status = rb_attr_get(exc, rb_intern("status"));
    if (NIL_P(status)) return Qtrue;
    if (status == INT2FIX(EXIT_SUCCESS)) return Qtrue;
    return Qfalse;
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_name_error(ID id, const char *fmt, ...)
#else
rb_name_error(id, fmt, va_alist)
    ID id;
    const char *fmt;
    va_dcl
#endif
{
    VALUE exc, argv[2];
    va_list args;
    char buf[BUFSIZ];

    va_init_list(args, fmt);
    vsnprintf(buf, BUFSIZ, fmt, args);
    va_end(args);

    argv[0] = rb_str_new2(buf);
    argv[1] = ID2SYM(id);
    exc = rb_class_new_instance(2, argv, rb_eNameError);
    rb_exc_raise(exc);
}

/*
 * call-seq:
 *   NameError.new(msg [, name])  => name_error
 *
 * Construct a new NameError exception. If given the <i>name</i>
 * parameter may subsequently be examined using the <code>NameError.name</code>
 * method.
 */

static VALUE
name_err_initialize(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    VALUE name;

    name = (argc > 1) ? argv[--argc] : Qnil;
    exc_initialize(argc, argv, self);
    rb_iv_set(self, "name", name);
    return self;
}

/*
 *  call-seq:
 *    name_error.name    =>  string or nil
 *
 *  Return the name associated with this NameError exception.
 */

static VALUE
name_err_name(self)
    VALUE self;
{
    return rb_attr_get(self, rb_intern("name"));
}

/*
 * call-seq:
 *  name_error.to_s   => string
 *
 * Produce a nicely-formated string representing the +NameError+.
 */

static VALUE
name_err_to_s(exc)
    VALUE exc;
{
    VALUE mesg = rb_attr_get(exc, rb_intern("mesg"));
    VALUE str = mesg;

    if (NIL_P(mesg)) return rb_class_name(CLASS_OF(exc));
    StringValue(str);
    if (str != mesg) {
	rb_iv_set(exc, "mesg", mesg = str);
    }
    if (OBJ_TAINTED(exc)) OBJ_TAINT(mesg);
    return mesg;
}

/*
 * call-seq:
 *   NoMethodError.new(msg, name [, args])  => no_method_error
 *
 * Construct a NoMethodError exception for a method of the given name
 * called with the given arguments. The name may be accessed using
 * the <code>#name</code> method on the resulting object, and the
 * arguments using the <code>#args</code> method.
 */

static VALUE
nometh_err_initialize(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    VALUE args = (argc > 2) ? argv[--argc] : Qnil;
    name_err_initialize(argc, argv, self);
    rb_iv_set(self, "args", args);
    return self;
}

/* :nodoc: */
static void
name_err_mesg_mark(ptr)
    VALUE *ptr;
{
    rb_gc_mark_locations(ptr, ptr+3);
}

/* :nodoc: */
static VALUE
name_err_mesg_new(obj, mesg, recv, method)
    VALUE obj, mesg, recv, method;
{
    VALUE *ptr = ALLOC_N(VALUE, 3);

    ptr[0] = mesg;
    ptr[1] = recv;
    ptr[2] = method;
    return Data_Wrap_Struct(rb_cNameErrorMesg, name_err_mesg_mark, -1, ptr);
}

/* :nodoc: */
static VALUE
name_err_mesg_equal(obj1, obj2)
    VALUE obj1, obj2;
{
    VALUE *ptr1, *ptr2;
    int i;

    if (obj1 == obj2) return Qtrue;
    if (rb_obj_class(obj2) != rb_cNameErrorMesg)
	return Qfalse;

    Data_Get_Struct(obj1, VALUE, ptr1);
    Data_Get_Struct(obj2, VALUE, ptr2);
    for (i=0; i<3; i++) {
	if (!rb_equal(ptr1[i], ptr2[i]))
	    return Qfalse;
    }
    return Qtrue;
}

/* :nodoc: */
static VALUE
name_err_mesg_to_str(obj)
    VALUE obj;
{
    VALUE *ptr, mesg;
    Data_Get_Struct(obj, VALUE, ptr);

    mesg = ptr[0];
    if (NIL_P(mesg)) return Qnil;
    else {
	char *desc = 0;
	VALUE d = 0, args[3];

	obj = ptr[1];
	switch (TYPE(obj)) {
	  case T_NIL:
	    desc = "nil";
	    break;
	  case T_TRUE:
	    desc = "true";
	    break;
	  case T_FALSE:
	    desc = "false";
	    break;
	  default:
	    d = rb_protect(rb_inspect, obj, 0);
	    if (NIL_P(d) || RSTRING(d)->len > 65) {
		d = rb_any_to_s(obj);
	    }
	    desc = RSTRING(d)->ptr;
	    break;
	}
	if (desc && desc[0] != '#') {
	    d = rb_str_new2(desc);
	    rb_str_cat2(d, ":");
	    rb_str_cat2(d, rb_obj_classname(obj));
	}
	args[0] = mesg;
	args[1] = ptr[2];
	args[2] = d;
	mesg = rb_f_sprintf(3, args);
    }
    if (OBJ_TAINTED(obj)) OBJ_TAINT(mesg);
    return mesg;
}

/* :nodoc: */
static VALUE
name_err_mesg_load(klass, str)
    VALUE klass, str;
{
    return str;
}

/*
 * call-seq:
 *   no_method_error.args  => obj
 *
 * Return the arguments passed in as the third parameter to
 * the constructor.
 */

static VALUE
nometh_err_args(self)
    VALUE self;
{
    return rb_attr_get(self, rb_intern("args"));
}

void
rb_invalid_str(str, type)
    const char *str, *type;
{
    VALUE s = rb_str_inspect(rb_str_new2(str));

    rb_raise(rb_eArgError, "invalid value for %s: %s", type, RSTRING(s)->ptr);
}

/* 
 *  Document-module: Errno
 *
 *  Ruby exception objects are subclasses of <code>Exception</code>.
 *  However, operating systems typically report errors using plain
 *  integers. Module <code>Errno</code> is created dynamically to map
 *  these operating system errors to Ruby classes, with each error
 *  number generating its own subclass of <code>SystemCallError</code>.
 *  As the subclass is created in module <code>Errno</code>, its name
 *  will start <code>Errno::</code>.
 *     
 *  The names of the <code>Errno::</code> classes depend on
 *  the environment in which Ruby runs. On a typical Unix or Windows
 *  platform, there are <code>Errno</code> classes such as
 *  <code>Errno::EACCES</code>, <code>Errno::EAGAIN</code>,
 *  <code>Errno::EINTR</code>, and so on.
 *     
 *  The integer operating system error number corresponding to a
 *  particular error is available as the class constant
 *  <code>Errno::</code><em>error</em><code>::Errno</code>.
 *     
 *     Errno::EACCES::Errno   #=> 13
 *     Errno::EAGAIN::Errno   #=> 11
 *     Errno::EINTR::Errno    #=> 4
 *     
 *  The full list of operating system errors on your particular platform
 *  are available as the constants of <code>Errno</code>.
 *
 *     Errno.constants   #=> E2BIG, EACCES, EADDRINUSE, EADDRNOTAVAIL, ...
 */

static st_table *syserr_tbl;

static VALUE
set_syserr(n, name)
    int n;
    const char *name;
{
    VALUE error;

    if (!st_lookup(syserr_tbl, n, &error)) {
	error = rb_define_class_under(rb_mErrno, name, rb_eSystemCallError);
	rb_define_const(error, "Errno", INT2NUM(n));
	st_add_direct(syserr_tbl, n, error);
    }
    else {
	rb_define_const(rb_mErrno, name, error);
    }
    return error;
}

static VALUE
get_syserr(n)
    int n;
{
    VALUE error;

    if (!st_lookup(syserr_tbl, n, &error)) {
	char name[8];	/* some Windows' errno have 5 digits. */

	snprintf(name, sizeof(name), "E%03d", n);
	error = set_syserr(n, name);
    }
    return error;
}

/*
 * call-seq:
 *   SystemCallError.new(msg, errno)  => system_call_error_subclass
 *
 * If _errno_ corresponds to a known system error code, constructs
 * the appropriate <code>Errno</code> class for that error, otherwise
 * constructs a generic <code>SystemCallError</code> object. The
 * error number is subsequently available via the <code>errno</code>
 * method.
 */

static VALUE
syserr_initialize(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
#if !defined(_WIN32) && !defined(__VMS)
    char *strerror();
#endif
    char *err;
    VALUE mesg, error;
    VALUE klass = rb_obj_class(self);

    if (klass == rb_eSystemCallError) {
	rb_scan_args(argc, argv, "11", &mesg, &error);
	if (argc == 1 && FIXNUM_P(mesg)) {
	    error = mesg; mesg = Qnil;
	}
	if (!NIL_P(error) && st_lookup(syserr_tbl, NUM2LONG(error), &klass)) {
	    /* change class */
	    if (TYPE(self) != T_OBJECT) { /* insurance to avoid type crash */
		rb_raise(rb_eTypeError, "invalid instance type");
	    }
	    RBASIC(self)->klass = klass;
	}
    }
    else {
	rb_scan_args(argc, argv, "01", &mesg);
	error = rb_const_get(klass, rb_intern("Errno"));
    }
    if (!NIL_P(error)) err = strerror(NUM2LONG(error));
    else err = "unknown error";
    if (!NIL_P(mesg)) {
	VALUE str = mesg;
	StringValue(str);
	mesg = rb_str_new(0, strlen(err)+RSTRING(str)->len+3);
	sprintf(RSTRING(mesg)->ptr, "%s - %.*s", err,
		(int)RSTRING(str)->len, RSTRING(str)->ptr);
	rb_str_resize(mesg, strlen(RSTRING(mesg)->ptr));
    }
    else {
	mesg = rb_str_new2(err);
    }
    exc_initialize(1, &mesg, self);
    rb_iv_set(self, "errno", error);
    return self;
}

/*
 * call-seq:
 *   system_call_error.errno   => fixnum
 *
 * Return this SystemCallError's error number.
 */

static VALUE
syserr_errno(self)
    VALUE self;
{
    return rb_attr_get(self, rb_intern("errno"));
}

/*
 * call-seq:
 *   system_call_error === other  => true or false
 *
 * Return +true+ if the receiver is a generic +SystemCallError+, or
 * if the error numbers _self_ and _other_ are the same.
 */

static VALUE
syserr_eqq(self, exc)
    VALUE self, exc;
{
    VALUE num, e;

    if (!rb_obj_is_kind_of(exc, rb_eSystemCallError)) return Qfalse;
    if (self == rb_eSystemCallError) return Qtrue;

    num = rb_attr_get(exc, rb_intern("errno"));
    if (NIL_P(num)) {
	VALUE klass = CLASS_OF(exc);

	while (TYPE(klass) == T_ICLASS || FL_TEST(klass, FL_SINGLETON)) {
	    klass = (VALUE)RCLASS(klass)->super;
	}
	num = rb_const_get(klass, rb_intern("Errno"));
    }
    e = rb_const_get(self, rb_intern("Errno"));
    if (FIXNUM_P(num) ? num == e : rb_equal(num, e))
	return Qtrue;
    return Qfalse;
}

/*
 * call-seq:
 *   Errno.const_missing   => SystemCallError
 *
 * Returns default SystemCallError class.
 */
static VALUE
errno_missing(self, id)
    VALUE self, id;
{
    return eNOERROR;
}

/*
 *  Descendents of class <code>Exception</code> are used to communicate
 *  between <code>raise</code> methods and <code>rescue</code>
 *  statements in <code>begin/end</code> blocks. <code>Exception</code>
 *  objects carry information about the exception---its type (the
 *  exception's class name), an optional descriptive string, and
 *  optional traceback information. Programs may subclass 
 *  <code>Exception</code> to add additional information.
 */

void
Init_Exception()
{
    rb_eException   = rb_define_class("Exception", rb_cObject);
    rb_define_singleton_method(rb_eException, "exception", rb_class_new_instance, -1);
    rb_define_method(rb_eException, "exception", exc_exception, -1);
    rb_define_method(rb_eException, "initialize", exc_initialize, -1);
    rb_define_method(rb_eException, "==", exc_equal, 1);
    rb_define_method(rb_eException, "to_s", exc_to_s, 0);
    rb_define_method(rb_eException, "message", exc_message, 0);
    rb_define_method(rb_eException, "inspect", exc_inspect, 0);
    rb_define_method(rb_eException, "backtrace", exc_backtrace, 0);
    rb_define_method(rb_eException, "set_backtrace", exc_set_backtrace, 1);

    rb_eSystemExit  = rb_define_class("SystemExit", rb_eException);
    rb_define_method(rb_eSystemExit, "initialize", exit_initialize, -1);
    rb_define_method(rb_eSystemExit, "status", exit_status, 0);
    rb_define_method(rb_eSystemExit, "success?", exit_success_p, 0);

    rb_eFatal  	    = rb_define_class("fatal", rb_eException);
    rb_eSignal      = rb_define_class("SignalException", rb_eException);
    rb_eInterrupt   = rb_define_class("Interrupt", rb_eSignal);

    rb_eStandardError = rb_define_class("StandardError", rb_eException);
    rb_eTypeError     = rb_define_class("TypeError", rb_eStandardError);
    rb_eArgError      = rb_define_class("ArgumentError", rb_eStandardError);
    rb_eIndexError    = rb_define_class("IndexError", rb_eStandardError);
    rb_eKeyError      = rb_define_class("KeyError", rb_eIndexError);
    rb_eRangeError    = rb_define_class("RangeError", rb_eStandardError);
    rb_eNameError     = rb_define_class("NameError", rb_eStandardError);
    rb_define_method(rb_eNameError, "initialize", name_err_initialize, -1);
    rb_define_method(rb_eNameError, "name", name_err_name, 0);
    rb_define_method(rb_eNameError, "to_s", name_err_to_s, 0);
    rb_cNameErrorMesg = rb_define_class_under(rb_eNameError, "message", rb_cData);
    rb_define_singleton_method(rb_cNameErrorMesg, "!", name_err_mesg_new, 3);
    rb_define_method(rb_cNameErrorMesg, "==", name_err_mesg_equal, 1);
    rb_define_method(rb_cNameErrorMesg, "to_str", name_err_mesg_to_str, 0);
    rb_define_method(rb_cNameErrorMesg, "_dump", name_err_mesg_to_str, 1);
    rb_define_singleton_method(rb_cNameErrorMesg, "_load", name_err_mesg_load, 1);
    rb_eNoMethodError = rb_define_class("NoMethodError", rb_eNameError);
    rb_define_method(rb_eNoMethodError, "initialize", nometh_err_initialize, -1);
    rb_define_method(rb_eNoMethodError, "args", nometh_err_args, 0);

    rb_eScriptError = rb_define_class("ScriptError", rb_eException);
    rb_eSyntaxError = rb_define_class("SyntaxError", rb_eScriptError);
    rb_eLoadError   = rb_define_class("LoadError", rb_eScriptError);
    rb_eNotImpError = rb_define_class("NotImplementedError", rb_eScriptError);

    rb_eRuntimeError = rb_define_class("RuntimeError", rb_eStandardError);
    rb_eSecurityError = rb_define_class("SecurityError", rb_eStandardError);
    rb_eNoMemError = rb_define_class("NoMemoryError", rb_eException);

    syserr_tbl = st_init_numtable();
    rb_eSystemCallError = rb_define_class("SystemCallError", rb_eStandardError);
    rb_define_method(rb_eSystemCallError, "initialize", syserr_initialize, -1);
    rb_define_method(rb_eSystemCallError, "errno", syserr_errno, 0);
    rb_define_singleton_method(rb_eSystemCallError, "===", syserr_eqq, 1);

    rb_mErrno = rb_define_module("Errno");
    rb_define_singleton_method(rb_mErrno, "const_missing", errno_missing, 1);

    rb_define_global_function("warn", rb_warn_m, 1);
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_raise(VALUE exc, const char *fmt, ...)
#else
rb_raise(exc, fmt, va_alist)
    VALUE exc;
    const char *fmt;
    va_dcl
#endif
{
    va_list args;
    char buf[BUFSIZ];

    va_init_list(args,fmt);
    vsnprintf(buf, BUFSIZ, fmt, args);
    va_end(args);
    rb_exc_raise(rb_exc_new2(exc, buf));
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_loaderror(const char *fmt, ...)
#else
rb_loaderror(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    va_list args;
    char buf[BUFSIZ];

    va_init_list(args, fmt);
    vsnprintf(buf, BUFSIZ, fmt, args);
    va_end(args);
    rb_exc_raise(rb_exc_new2(rb_eLoadError, buf));
}

void
rb_notimplement()
{
    rb_raise(rb_eNotImpError,
	     "The %s() function is unimplemented on this machine",
	     rb_id2name(ruby_frame->callee));
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_fatal(const char *fmt, ...)
#else
rb_fatal(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    va_list args;
    char buf[BUFSIZ];

    va_init_list(args, fmt);
    vsnprintf(buf, BUFSIZ, fmt, args);
    va_end(args);

    ruby_in_eval = 0;
    rb_exc_fatal(rb_exc_new2(rb_eFatal, buf));
}

void
rb_sys_fail(mesg)
    const char *mesg;
{
    extern int errno;
    int n = errno;
    VALUE arg;

    errno = 0;
    if (n == 0) {
	rb_bug("rb_sys_fail(%s) - errno == 0", mesg ? mesg : "");
    }

    arg = mesg ? rb_str_new2(mesg) : Qnil;
    rb_exc_raise(rb_class_new_instance(1, &arg, get_syserr(n)));
}

void
#ifdef HAVE_STDARG_PROTOTYPES
rb_sys_warning(const char *fmt, ...)
#else
rb_sys_warning(fmt, va_alist)
     const char *fmt;
     va_dcl
#endif
{
     char buf[BUFSIZ];
     va_list args;
     int errno_save;
     
     errno_save = errno;

     if (!RTEST(ruby_verbose)) return;

     snprintf(buf, BUFSIZ, "warning: %s", fmt);
     snprintf(buf+strlen(buf), BUFSIZ-strlen(buf), ": %s", strerror(errno_save));
     
     va_init_list(args, fmt);
     warn_print(buf, args);
     va_end(args);
     errno = errno_save;
}

void
rb_load_fail(path)
    const char *path;
{
    rb_loaderror("%s -- %s", strerror(errno), path);
}

void
rb_error_frozen(what)
    const char *what;
{
    rb_raise(rb_eRuntimeError, "can't modify frozen %s", what);
}

void
rb_check_frozen(obj)
    VALUE obj;
{
    if (OBJ_FROZEN(obj)) rb_error_frozen(rb_obj_classname(obj));
}

void
Init_syserr()
{
#ifdef EPERM
    set_syserr(EPERM, "EPERM");
#endif
#ifdef ENOENT
    set_syserr(ENOENT, "ENOENT");
#endif
#ifdef ESRCH
    set_syserr(ESRCH, "ESRCH");
#endif
#ifdef EINTR
    set_syserr(EINTR, "EINTR");
#endif
#ifdef EIO
    set_syserr(EIO, "EIO");
#endif
#ifdef ENXIO
    set_syserr(ENXIO, "ENXIO");
#endif
#ifdef E2BIG
    set_syserr(E2BIG, "E2BIG");
#endif
#ifdef ENOEXEC
    set_syserr(ENOEXEC, "ENOEXEC");
#endif
#ifdef EBADF
    set_syserr(EBADF, "EBADF");
#endif
#ifdef ECHILD
    set_syserr(ECHILD, "ECHILD");
#endif
#ifdef EAGAIN
    set_syserr(EAGAIN, "EAGAIN");
#endif
#ifdef ENOMEM
    set_syserr(ENOMEM, "ENOMEM");
#endif
#ifdef EACCES
    set_syserr(EACCES, "EACCES");
#endif
#ifdef EFAULT
    set_syserr(EFAULT, "EFAULT");
#endif
#ifdef ENOTBLK
    set_syserr(ENOTBLK, "ENOTBLK");
#endif
#ifdef EBUSY
    set_syserr(EBUSY, "EBUSY");
#endif
#ifdef EEXIST
    set_syserr(EEXIST, "EEXIST");
#endif
#ifdef EXDEV
    set_syserr(EXDEV, "EXDEV");
#endif
#ifdef ENODEV
    set_syserr(ENODEV, "ENODEV");
#endif
#ifdef ENOTDIR
    set_syserr(ENOTDIR, "ENOTDIR");
#endif
#ifdef EISDIR
    set_syserr(EISDIR, "EISDIR");
#endif
#ifdef EINVAL
    set_syserr(EINVAL, "EINVAL");
#endif
#ifdef ENFILE
    set_syserr(ENFILE, "ENFILE");
#endif
#ifdef EMFILE
    set_syserr(EMFILE, "EMFILE");
#endif
#ifdef ENOTTY
    set_syserr(ENOTTY, "ENOTTY");
#endif
#ifdef ETXTBSY
    set_syserr(ETXTBSY, "ETXTBSY");
#endif
#ifdef EFBIG
    set_syserr(EFBIG, "EFBIG");
#endif
#ifdef ENOSPC
    set_syserr(ENOSPC, "ENOSPC");
#endif
#ifdef ESPIPE
    set_syserr(ESPIPE, "ESPIPE");
#endif
#ifdef EROFS
    set_syserr(EROFS, "EROFS");
#endif
#ifdef EMLINK
    set_syserr(EMLINK, "EMLINK");
#endif
#ifdef EPIPE
    set_syserr(EPIPE, "EPIPE");
#endif
#ifdef EDOM
    set_syserr(EDOM, "EDOM");
#endif
#ifdef ERANGE
    set_syserr(ERANGE, "ERANGE");
#endif
#ifdef EDEADLK
    set_syserr(EDEADLK, "EDEADLK");
#endif
#ifdef ENAMETOOLONG
    set_syserr(ENAMETOOLONG, "ENAMETOOLONG");
#endif
#ifdef ENOLCK
    set_syserr(ENOLCK, "ENOLCK");
#endif
#ifdef ENOSYS
    set_syserr(ENOSYS, "ENOSYS");
#endif
#ifdef ENOTEMPTY
    set_syserr(ENOTEMPTY, "ENOTEMPTY");
#endif
#ifdef ELOOP
    set_syserr(ELOOP, "ELOOP");
#endif
#ifdef EWOULDBLOCK
    set_syserr(EWOULDBLOCK, "EWOULDBLOCK");
#endif
#ifdef ENOMSG
    set_syserr(ENOMSG, "ENOMSG");
#endif
#ifdef EIDRM
    set_syserr(EIDRM, "EIDRM");
#endif
#ifdef ECHRNG
    set_syserr(ECHRNG, "ECHRNG");
#endif
#ifdef EL2NSYNC
    set_syserr(EL2NSYNC, "EL2NSYNC");
#endif
#ifdef EL3HLT
    set_syserr(EL3HLT, "EL3HLT");
#endif
#ifdef EL3RST
    set_syserr(EL3RST, "EL3RST");
#endif
#ifdef ELNRNG
    set_syserr(ELNRNG, "ELNRNG");
#endif
#ifdef EUNATCH
    set_syserr(EUNATCH, "EUNATCH");
#endif
#ifdef ENOCSI
    set_syserr(ENOCSI, "ENOCSI");
#endif
#ifdef EL2HLT
    set_syserr(EL2HLT, "EL2HLT");
#endif
#ifdef EBADE
    set_syserr(EBADE, "EBADE");
#endif
#ifdef EBADR
    set_syserr(EBADR, "EBADR");
#endif
#ifdef EXFULL
    set_syserr(EXFULL, "EXFULL");
#endif
#ifdef ENOANO
    set_syserr(ENOANO, "ENOANO");
#endif
#ifdef EBADRQC
    set_syserr(EBADRQC, "EBADRQC");
#endif
#ifdef EBADSLT
    set_syserr(EBADSLT, "EBADSLT");
#endif
#ifdef EDEADLOCK
    set_syserr(EDEADLOCK, "EDEADLOCK");
#endif
#ifdef EBFONT
    set_syserr(EBFONT, "EBFONT");
#endif
#ifdef ENOSTR
    set_syserr(ENOSTR, "ENOSTR");
#endif
#ifdef ENODATA
    set_syserr(ENODATA, "ENODATA");
#endif
#ifdef ETIME
    set_syserr(ETIME, "ETIME");
#endif
#ifdef ENOSR
    set_syserr(ENOSR, "ENOSR");
#endif
#ifdef ENONET
    set_syserr(ENONET, "ENONET");
#endif
#ifdef ENOPKG
    set_syserr(ENOPKG, "ENOPKG");
#endif
#ifdef EREMOTE
    set_syserr(EREMOTE, "EREMOTE");
#endif
#ifdef ENOLINK
    set_syserr(ENOLINK, "ENOLINK");
#endif
#ifdef EADV
    set_syserr(EADV, "EADV");
#endif
#ifdef ESRMNT
    set_syserr(ESRMNT, "ESRMNT");
#endif
#ifdef ECOMM
    set_syserr(ECOMM, "ECOMM");
#endif
#ifdef EPROTO
    set_syserr(EPROTO, "EPROTO");
#endif
#ifdef EMULTIHOP
    set_syserr(EMULTIHOP, "EMULTIHOP");
#endif
#ifdef EDOTDOT
    set_syserr(EDOTDOT, "EDOTDOT");
#endif
#ifdef EBADMSG
    set_syserr(EBADMSG, "EBADMSG");
#endif
#ifdef EOVERFLOW
    set_syserr(EOVERFLOW, "EOVERFLOW");
#endif
#ifdef ENOTUNIQ
    set_syserr(ENOTUNIQ, "ENOTUNIQ");
#endif
#ifdef EBADFD
    set_syserr(EBADFD, "EBADFD");
#endif
#ifdef EREMCHG
    set_syserr(EREMCHG, "EREMCHG");
#endif
#ifdef ELIBACC
    set_syserr(ELIBACC, "ELIBACC");
#endif
#ifdef ELIBBAD
    set_syserr(ELIBBAD, "ELIBBAD");
#endif
#ifdef ELIBSCN
    set_syserr(ELIBSCN, "ELIBSCN");
#endif
#ifdef ELIBMAX
    set_syserr(ELIBMAX, "ELIBMAX");
#endif
#ifdef ELIBEXEC
    set_syserr(ELIBEXEC, "ELIBEXEC");
#endif
#ifdef EILSEQ
    set_syserr(EILSEQ, "EILSEQ");
#endif
#ifdef ERESTART
    set_syserr(ERESTART, "ERESTART");
#endif
#ifdef ESTRPIPE
    set_syserr(ESTRPIPE, "ESTRPIPE");
#endif
#ifdef EUSERS
    set_syserr(EUSERS, "EUSERS");
#endif
#ifdef ENOTSOCK
    set_syserr(ENOTSOCK, "ENOTSOCK");
#endif
#ifdef EDESTADDRREQ
    set_syserr(EDESTADDRREQ, "EDESTADDRREQ");
#endif
#ifdef EMSGSIZE
    set_syserr(EMSGSIZE, "EMSGSIZE");
#endif
#ifdef EPROTOTYPE
    set_syserr(EPROTOTYPE, "EPROTOTYPE");
#endif
#ifdef ENOPROTOOPT
    set_syserr(ENOPROTOOPT, "ENOPROTOOPT");
#endif
#ifdef EPROTONOSUPPORT
    set_syserr(EPROTONOSUPPORT, "EPROTONOSUPPORT");
#endif
#ifdef ESOCKTNOSUPPORT
    set_syserr(ESOCKTNOSUPPORT, "ESOCKTNOSUPPORT");
#endif
#ifdef EOPNOTSUPP
    set_syserr(EOPNOTSUPP, "EOPNOTSUPP");
#endif
#ifdef EPFNOSUPPORT
    set_syserr(EPFNOSUPPORT, "EPFNOSUPPORT");
#endif
#ifdef EAFNOSUPPORT
    set_syserr(EAFNOSUPPORT, "EAFNOSUPPORT");
#endif
#ifdef EADDRINUSE
    set_syserr(EADDRINUSE, "EADDRINUSE");
#endif
#ifdef EADDRNOTAVAIL
    set_syserr(EADDRNOTAVAIL, "EADDRNOTAVAIL");
#endif
#ifdef ENETDOWN
    set_syserr(ENETDOWN, "ENETDOWN");
#endif
#ifdef ENETUNREACH
    set_syserr(ENETUNREACH, "ENETUNREACH");
#endif
#ifdef ENETRESET
    set_syserr(ENETRESET, "ENETRESET");
#endif
#ifdef ECONNABORTED
    set_syserr(ECONNABORTED, "ECONNABORTED");
#endif
#ifdef ECONNRESET
    set_syserr(ECONNRESET, "ECONNRESET");
#endif
#ifdef ENOBUFS
    set_syserr(ENOBUFS, "ENOBUFS");
#endif
#ifdef EISCONN
    set_syserr(EISCONN, "EISCONN");
#endif
#ifdef ENOTCONN
    set_syserr(ENOTCONN, "ENOTCONN");
#endif
#ifdef ESHUTDOWN
    set_syserr(ESHUTDOWN, "ESHUTDOWN");
#endif
#ifdef ETOOMANYREFS
    set_syserr(ETOOMANYREFS, "ETOOMANYREFS");
#endif
#ifdef ETIMEDOUT
    set_syserr(ETIMEDOUT, "ETIMEDOUT");
#endif
#ifdef ECONNREFUSED
    set_syserr(ECONNREFUSED, "ECONNREFUSED");
#endif
#ifdef EHOSTDOWN
    set_syserr(EHOSTDOWN, "EHOSTDOWN");
#endif
#ifdef EHOSTUNREACH
    set_syserr(EHOSTUNREACH, "EHOSTUNREACH");
#endif
#ifdef EALREADY
    set_syserr(EALREADY, "EALREADY");
#endif
#ifdef EINPROGRESS
    set_syserr(EINPROGRESS, "EINPROGRESS");
#endif
#ifdef ESTALE
    set_syserr(ESTALE, "ESTALE");
#endif
#ifdef EUCLEAN
    set_syserr(EUCLEAN, "EUCLEAN");
#endif
#ifdef ENOTNAM
    set_syserr(ENOTNAM, "ENOTNAM");
#endif
#ifdef ENAVAIL
    set_syserr(ENAVAIL, "ENAVAIL");
#endif
#ifdef EISNAM
    set_syserr(EISNAM, "EISNAM");
#endif
#ifdef EREMOTEIO
    set_syserr(EREMOTEIO, "EREMOTEIO");
#endif
#ifdef EDQUOT
    set_syserr(EDQUOT, "EDQUOT");
#endif
    eNOERROR = set_syserr(0, "NOERROR");
}

static void
err_append(s)
    const char *s;
{
    extern VALUE ruby_errinfo;

    if (ruby_in_eval) {
	if (NIL_P(ruby_errinfo)) {
	    ruby_errinfo = rb_exc_new2(rb_eSyntaxError, s);
	}
	else {
	    VALUE str = rb_obj_as_string(ruby_errinfo);

	    rb_str_cat2(str, "\n");
	    rb_str_cat2(str, s);
	    ruby_errinfo = rb_exc_new3(rb_eSyntaxError, str);
	}
    }
    else {
	rb_write_error(s);
	rb_write_error("\n");
    }
}
/**********************************************************************
  euc_jp.c -  Oniguruma (regular expression library)
**********************************************************************/
/*-
 * Copyright (c) 2002-2005  K.Kosako  <sndgk393 AT ybb DOT ne DOT jp>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "regenc.h"

#define eucjp_islead(c)    ((UChar )((c) - 0xa1) > 0xfe - 0xa1)

static int EncLen_EUCJP[] = {
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1
};

static int
eucjp_mbc_enc_len(const UChar* p)
{
  return EncLen_EUCJP[*p];
}

static OnigCodePoint
eucjp_mbc_to_code(const UChar* p, const UChar* end)
{
  int c, i, len;
  OnigCodePoint n;

  len = enc_len(ONIG_ENCODING_EUC_JP, p);
  n = (OnigCodePoint )*p++;
  if (len == 1) return n;

  for (i = 1; i < len; i++) {
    if (p >= end) break;
    c = *p++;
    n <<= 8;  n += c;
  }
  return n;
}

static int
eucjp_code_to_mbclen(OnigCodePoint code)
{
  if (ONIGENC_IS_CODE_ASCII(code)) return 1;
  else if ((code & 0xff0000) != 0) return 3;
  else if ((code &   0xff00) != 0) return 2;
  else return 0;
}

#if 0
static int
eucjp_code_to_mbc_first(OnigCodePoint code)
{
  int first;

  if ((code & 0xff0000) != 0) {
    first = (code >> 16) & 0xff;
  }
  else if ((code & 0xff00) != 0) {
    first = (code >> 8) & 0xff;
  }
  else {
    return (int )code;
  }
  return first;
}
#endif

static int
eucjp_code_to_mbc(OnigCodePoint code, UChar *buf)
{
  UChar *p = buf;

  if ((code & 0xff0000) != 0) *p++ = (UChar )(((code >> 16) & 0xff));
  if ((code &   0xff00) != 0) *p++ = (UChar )(((code >>  8) & 0xff));
  *p++ = (UChar )(code & 0xff);

#if 1
  if (enc_len(ONIG_ENCODING_EUC_JP, buf) != (p - buf))
    return ONIGENCERR_INVALID_WIDE_CHAR_VALUE;
#endif  
  return p - buf;
}

static int
eucjp_mbc_to_normalize(OnigAmbigType flag,
		       const UChar** pp, const UChar* end, UChar* lower)
{
  int len;
  const UChar* p = *pp;

  if (ONIGENC_IS_MBC_ASCII(p)) {
    if ((flag & ONIGENC_AMBIGUOUS_MATCH_ASCII_CASE) != 0) {
      *lower = ONIGENC_ASCII_CODE_TO_LOWER_CASE(*p);
    }
    else {
      *lower = *p;
    }

    (*pp)++;
    return 1;
  }
  else {
    len = enc_len(ONIG_ENCODING_EUC_JP, p);
    if (lower != p) {
      int i;
      for (i = 0; i < len; i++) {
	*lower++ = *p++;
      }
    }
    (*pp) += len;
    return len; /* return byte length of converted char to lower */
  }
}

static int
eucjp_is_mbc_ambiguous(OnigAmbigType flag, const UChar** pp, const UChar* end)
{
  return onigenc_mbn_is_mbc_ambiguous(ONIG_ENCODING_EUC_JP, flag, pp, end);
}

static int
eucjp_is_code_ctype(OnigCodePoint code, unsigned int ctype)
{
  if ((ctype & ONIGENC_CTYPE_WORD) != 0) {
    if (code < 128)
      return ONIGENC_IS_ASCII_CODE_CTYPE(code, ctype);
    else
      return (eucjp_code_to_mbclen(code) > 1 ? TRUE : FALSE);

    ctype &= ~ONIGENC_CTYPE_WORD;
    if (ctype == 0) return FALSE;
  }

  if (code < 128)
    return ONIGENC_IS_ASCII_CODE_CTYPE(code, ctype);
  else
    return FALSE;
}

static UChar*
eucjp_left_adjust_char_head(const UChar* start, const UChar* s)
{
  /* In this encoding
     mb-trail bytes doesn't mix with single bytes.
  */
  const UChar *p;
  int len;

  if (s <= start) return (UChar* )s;
  p = s;

  while (!eucjp_islead(*p) && p > start) p--;
  len = enc_len(ONIG_ENCODING_EUC_JP, p);
  if (p + len > s) return (UChar* )p;
  p += len;
  return (UChar* )(p + ((s - p) & ~1));
}

static int
eucjp_is_allowed_reverse_match(const UChar* s, const UChar* end)
{
  const UChar c = *s;
  if (c <= 0x7e || c == 0x8e || c == 0x8f)
    return TRUE;
  else
    return FALSE;
}

OnigEncodingType OnigEncodingEUC_JP = {
  eucjp_mbc_enc_len,
  "EUC-JP",   /* name */
  3,          /* max enc length */
  1,          /* min enc length */
  ONIGENC_AMBIGUOUS_MATCH_ASCII_CASE,
  {
      (OnigCodePoint )'\\'                       /* esc */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* anychar '.'  */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* anytime '*'  */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* zero or one time '?' */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* one or more time '+' */
    , (OnigCodePoint )ONIG_INEFFECTIVE_META_CHAR /* anychar anytime */
  },
  onigenc_is_mbc_newline_0x0a,
  eucjp_mbc_to_code,
  eucjp_code_to_mbclen,
  eucjp_code_to_mbc,
  eucjp_mbc_to_normalize,
  eucjp_is_mbc_ambiguous,
  onigenc_ascii_get_all_pair_ambig_codes,
  onigenc_nothing_get_all_comp_ambig_codes,
  eucjp_is_code_ctype,
  onigenc_not_support_get_ctype_code_range,
  eucjp_left_adjust_char_head,
  eucjp_is_allowed_reverse_match
};
/**********************************************************************

  eval.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Thu Jun 10 14:22:17 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#include "ruby.h"
#include "node.h"
#include "env.h"
#include "util.h"
#include "rubysig.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

#include <stdio.h>
#if defined(HAVE_GETCONTEXT) && defined(HAVE_SETCONTEXT)
#include <ucontext.h>
#define USE_CONTEXT
#else
#include <setjmp.h>
#endif

#include "st.h"
#include "dln.h"

#ifdef __APPLE__
#include <crt_externs.h>
#endif

/* Make alloca work the best possible way.  */
#ifdef __GNUC__
# ifndef atarist
#  ifndef alloca
#   define alloca __builtin_alloca
#  endif
# endif /* atarist */
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
void *alloca ();
#   endif
#  endif /* AIX */
# endif /* HAVE_ALLOCA_H */
#endif /* __GNUC__ */

#ifdef HAVE_STDARG_PROTOTYPES
#include <stdarg.h>
#define va_init_list(a,b) va_start(a,b)
#else
#include <varargs.h>
#define va_init_list(a,b) va_start(a)
#endif

#ifndef HAVE_STRING_H
char *strrchr _((const char*,const char));
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __BEOS__
#include <net/socket.h>
#endif

#ifdef __MACOS__
#include "macruby_private.h"
#endif

#ifdef USE_CONTEXT
typedef struct {
    ucontext_t context;
    volatile int status;
} rb_jmpbuf_t[1];

#undef longjmp
#undef setjmp
NORETURN(static void rb_jump_context(rb_jmpbuf_t, int));
static inline void
rb_jump_context(env, val)
    rb_jmpbuf_t env;
    int val;
{
    env->status = val;
    setcontext(&env->context);
    abort();			/* ensure noreturn */
}
#define longjmp(env, val) rb_jump_context(env, val)
#define setjmp(j) ((j)->status = 0, getcontext(&(j)->context), (j)->status)
#else
typedef jmp_buf rb_jmpbuf_t;
#ifndef setjmp
#ifdef HAVE__SETJMP
#define setjmp(env) _setjmp(env)
#define longjmp(env,val) _longjmp(env,val)
#endif
#endif
#endif

#include <sys/types.h>
#include <signal.h>
#include <errno.h>

#if defined(__VMS)
#pragma nostandard
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <sys/stat.h>

VALUE rb_cProc;
static VALUE rb_cBinding;
static VALUE proc_invoke _((VALUE,VALUE,VALUE,VALUE));
static VALUE rb_f_binding _((VALUE));
static void rb_f_END _((void));
static VALUE rb_f_block_given_p _((void));
static VALUE block_pass _((VALUE,NODE*));
static VALUE rb_cMethod;
static VALUE method_call _((int, VALUE*, VALUE));
static VALUE rb_cUnboundMethod;
static VALUE umethod_bind _((VALUE, VALUE));
static VALUE rb_mod_define_method _((int, VALUE*, VALUE));
NORETURN(static void rb_raise_jump _((VALUE)));
static VALUE rb_make_exception _((int argc, VALUE *argv));

static int scope_vmode;
#define SCOPE_PUBLIC    0
#define SCOPE_PRIVATE   1
#define SCOPE_PROTECTED 2
#define SCOPE_MODFUNC   5
#define SCOPE_MASK      7
#define SCOPE_SET(f)  (scope_vmode=(f))
#define SCOPE_TEST(f) (scope_vmode&(f))

NODE* ruby_current_node;
int ruby_safe_level = 0;
/* safe-level:
   0 - strings from streams/environment/ARGV are tainted (default)
   1 - no dangerous operation by tainted value
   2 - process/file operations prohibited
   3 - all generated objects are tainted
   4 - no global (non-tainted) variable modification/no direct output
*/

static VALUE safe_getter _((void));
static void safe_setter _((VALUE val));

void
rb_secure(level)
    int level;
{
    if (level <= ruby_safe_level) {
	if (ruby_frame->callee) {
	    rb_raise(rb_eSecurityError, "Insecure operation `%s' at level %d",
		     rb_id2name(ruby_frame->callee), ruby_safe_level);
	}
	else {
	    rb_raise(rb_eSecurityError, "Insecure operation at level %d", ruby_safe_level);
	}
    }
}

void
rb_secure_update(obj)
    VALUE obj;
{
    if (!OBJ_TAINTED(obj)) rb_secure(4);
}

void
rb_check_safe_obj(x)
    VALUE x;
{
    if (ruby_safe_level > 0 && OBJ_TAINTED(x)){
	if (ruby_frame->callee) {
	    rb_raise(rb_eSecurityError, "Insecure operation - %s",
		     rb_id2name(ruby_frame->callee));
	}
	else {
	    rb_raise(rb_eSecurityError, "Insecure operation: -r");
	}
    }
    rb_secure(4);
}

void
rb_check_safe_str(x)
    VALUE x;
{
    rb_check_safe_obj(x);
    if (TYPE(x)!= T_STRING) {
	rb_raise(rb_eTypeError, "wrong argument type %s (expected String)",
		 rb_obj_classname(x));
    }
}

NORETURN(static void print_undef _((VALUE, ID)));
static void
print_undef(klass, id)
    VALUE klass;
    ID id;
{
    rb_name_error(id, "undefined method `%s' for %s `%s'",
		  rb_id2name(id),
		  (TYPE(klass) == T_MODULE) ? "module" : "class",
		  rb_class2name(klass));
}

static ID removed, singleton_removed, undefined, singleton_undefined;

#define CACHE_SIZE 0x800
#define CACHE_MASK 0x7ff
#define EXPR1(c,m) ((((c)>>3)^(m))&CACHE_MASK)

struct cache_entry {		/* method hash table. */
    ID mid;			/* method's id */
    ID mid0;			/* method's original id */
    VALUE klass;		/* receiver's class */
    VALUE origin;		/* where method defined  */
    NODE *method;
    int noex;
};

static struct cache_entry cache[CACHE_SIZE];
static int ruby_running = 0;

void
rb_clear_cache()
{
   struct cache_entry *ent, *end;

    if (!ruby_running) return;
    ent = cache; end = ent + CACHE_SIZE;
    while (ent < end) {
	ent->mid = 0;
	ent++;
    }
}

static void
rb_clear_cache_for_undef(klass, id)
    VALUE klass;
    ID id;
{
    struct cache_entry *ent, *end;

    if (!ruby_running) return;
    ent = cache; end = ent + CACHE_SIZE;
    while (ent < end) {
	if (ent->origin == klass && ent->mid == id) {
	    ent->mid = 0;
	}
	ent++;
    }
}

static void
rb_clear_cache_by_id(id)
    ID id;
{
    struct cache_entry *ent, *end;

    if (!ruby_running) return;
    ent = cache; end = ent + CACHE_SIZE;
    while (ent < end) {
	if (ent->mid == id) {
	    ent->mid = 0;
	}
	ent++;
    }
}

void
rb_clear_cache_by_class(klass)
    VALUE klass;
{
    struct cache_entry *ent, *end;

    if (!ruby_running) return;
    ent = cache; end = ent + CACHE_SIZE;
    while (ent < end) {
	if (ent->klass == klass || ent->origin == klass) {
	    ent->mid = 0;
	}
	ent++;
    }
}

static ID init, eqq, each, aref, aset, match, missing;
static ID added, singleton_added;
static ID __id__, __send__, respond_to;

void
rb_add_method(klass, mid, node, noex)
    VALUE klass;
    ID mid;
    NODE *node;
    int noex;
{
    NODE *body;

    if (NIL_P(klass)) klass = rb_cObject;
    if (ruby_safe_level >= 4 && (klass == rb_cObject || !OBJ_TAINTED(klass))) {
	rb_raise(rb_eSecurityError, "Insecure: can't define method");
    }
    if (!FL_TEST(klass, FL_SINGLETON) &&
	node && nd_type(node) != NODE_ZSUPER &&
	(mid == rb_intern("initialize" )|| mid == rb_intern("initialize_copy"))) {
	noex = NOEX_PRIVATE | noex;
    }
    else if (FL_TEST(klass, FL_SINGLETON) && node && nd_type(node) == NODE_CFUNC &&
	     mid == rb_intern("allocate")) {
	rb_warn("defining %s.allocate is deprecated; use rb_define_alloc_func()",
		rb_class2name(rb_iv_get(klass, "__attached__")));
	mid = ID_ALLOCATOR;
    }
    if (OBJ_FROZEN(klass)) rb_error_frozen("class/module");
    rb_clear_cache_by_id(mid);
    body = NEW_METHOD(node, noex);
    st_insert(RCLASS(klass)->m_tbl, mid, (st_data_t)body);
    if (node && mid != ID_ALLOCATOR && ruby_running) {
	if (FL_TEST(klass, FL_SINGLETON)) {
	    rb_funcall(rb_iv_get(klass, "__attached__"), singleton_added, 1, ID2SYM(mid));
	}
	else {
	    rb_funcall(klass, added, 1, ID2SYM(mid));
	}
    }
}

void
rb_define_alloc_func(klass, func)
    VALUE klass;
    VALUE (*func) _((VALUE));
{
    Check_Type(klass, T_CLASS);
    rb_add_method(CLASS_OF(klass), ID_ALLOCATOR, NEW_CFUNC(func, 0), NOEX_PRIVATE);
}

void
rb_undef_alloc_func(klass)
    VALUE klass;
{
    Check_Type(klass, T_CLASS);
    rb_add_method(CLASS_OF(klass), ID_ALLOCATOR, 0, NOEX_UNDEF);
}

static NODE*
search_method(klass, id, origin)
    VALUE klass, *origin;
    ID id;
{
    NODE *body;

    if (!klass) return 0;
    while (!st_lookup(RCLASS(klass)->m_tbl, id, (st_data_t *)&body)) {
	klass = RCLASS(klass)->super;
	if (!klass) return 0;
    }

    if (origin) *origin = klass;
    return body;
}

static NODE*
rb_get_method_body(klassp, idp, noexp)
    VALUE *klassp;
    ID *idp;
    int *noexp;
{
    ID id = *idp;
    VALUE klass = *klassp;
    VALUE origin;
    NODE * volatile body;
    struct cache_entry *ent;

    if ((body = search_method(klass, id, &origin)) == 0 || !body->nd_body) {
	/* store empty info in cache */
	ent = cache + EXPR1(klass, id);
	ent->klass  = klass;
	ent->origin = klass;
	ent->mid = ent->mid0 = id;
	ent->noex   = 0;
	ent->method = 0;

	return 0;
    }

    if (ruby_running) {
	/* store in cache */
	ent = cache + EXPR1(klass, id);
	ent->klass  = klass;
	ent->noex   = body->nd_noex;
	if (noexp) *noexp = body->nd_noex;
	body = body->nd_body;
	if (nd_type(body) == NODE_FBODY) {
	    ent->mid = id;
	    *klassp = body->nd_orig;
	    ent->origin = body->nd_orig;
	    *idp = ent->mid0 = body->nd_mid;
	    body = ent->method = body->nd_head;
	}
	else {
	    *klassp = origin;
	    ent->origin = origin;
	    ent->mid = ent->mid0 = id;
	    ent->method = body;
	}
    }
    else {
	if (noexp) *noexp = body->nd_noex;
	body = body->nd_body;
	if (nd_type(body) == NODE_FBODY) {
	    *klassp = body->nd_orig;
	    *idp = body->nd_mid;
	    body = body->nd_head;
	}
	else {
	    *klassp = origin;
	}
    }

    return body;
}

NODE*
rb_method_node(klass, id)
    VALUE klass;
    ID id;
{
    int noex;
    struct cache_entry *ent;

    ent = cache + EXPR1(klass, id);
    if (ent->mid == id && ent->klass == klass && ent->method){
	return ent->method;
    }

    return rb_get_method_body(&klass, &id, &noex);
}

static void
remove_method(klass, mid)
    VALUE klass;
    ID mid;
{
    NODE *body;

    if (klass == rb_cObject) {
	rb_secure(4);
    }
    if (ruby_safe_level >= 4 && !OBJ_TAINTED(klass)) {
	rb_raise(rb_eSecurityError, "Insecure: can't remove method");
    }
    if (OBJ_FROZEN(klass)) rb_error_frozen("class/module");
    if (mid == __id__ || mid == __send__ || mid == init) {
	rb_warn("removing `%s' may cause serious problem", rb_id2name(mid));
    }
    if (!st_delete(RCLASS(klass)->m_tbl, &mid, (st_data_t *)&body) ||
	!body->nd_body) {
	rb_name_error(mid, "method `%s' not defined in %s",
		      rb_id2name(mid), rb_class2name(klass));
    }
    rb_clear_cache_for_undef(klass, mid);
    if (FL_TEST(klass, FL_SINGLETON)) {
	rb_funcall(rb_iv_get(klass, "__attached__"), singleton_removed, 1, ID2SYM(mid));
    }
    else {
	rb_funcall(klass, removed, 1, ID2SYM(mid));
    }
}

void
rb_remove_method(klass, name)
    VALUE klass;
    const char *name;
{
    remove_method(klass, rb_intern(name));
}

/*
 *  call-seq:
 *     remove_method(symbol)   => self
 *  
 *  Removes the method identified by _symbol_ from the current
 *  class. For an example, see <code>Module.undef_method</code>.
 */

static VALUE
rb_mod_remove_method(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    int i;

    for (i=0; i<argc; i++) {
	remove_method(mod, rb_to_id(argv[i]));
    }
    return mod;
}

#undef rb_disable_super
#undef rb_enable_super

void
rb_disable_super(klass, name)
    VALUE klass;
    const char *name;
{
    /* obsolete - no use */
}

void
rb_enable_super(klass, name)
    VALUE klass;
    const char *name;
{
    rb_warning("rb_enable_super() is obsolete");
}

static void
rb_export_method(klass, name, noex)
    VALUE klass;
    ID name;
    ID noex;
{
    NODE *body;
    VALUE origin;

    if (klass == rb_cObject) {
	rb_secure(4);
    }
    body = search_method(klass, name, &origin);
    if (!body && TYPE(klass) == T_MODULE) {
	body = search_method(rb_cObject, name, &origin);
    }
    if (!body || !body->nd_body) {
	print_undef(klass, name);
    }
    if (body->nd_noex != noex) {
	if (klass == origin) {
	    body->nd_noex = noex;
	}
	else {
	    rb_add_method(klass, name, NEW_ZSUPER(), noex);
	}
    }
}

int
rb_method_boundp(klass, id, ex)
    VALUE klass;
    ID id;
    int ex;
{
    struct cache_entry *ent;
    int noex;

    /* is it in the method cache? */
    ent = cache + EXPR1(klass, id);
    if (ent->mid == id && ent->klass == klass) {
	if (ex && (ent->noex & NOEX_PRIVATE))
	    return Qfalse;
	if (!ent->method) return Qfalse;
	return Qtrue;
    }
    if (rb_get_method_body(&klass, &id, &noex)) {
	if (ex && (noex & NOEX_PRIVATE))
	    return Qfalse;
	return Qtrue;
    }
    return Qfalse;
}

void
rb_attr(klass, id, read, write, ex)
    VALUE klass;
    ID id;
    int read, write, ex;
{
    const char *name;
    char *buf;
    ID attriv;
    int noex;

    if (!ex) noex = NOEX_PUBLIC;
    else {
	if (SCOPE_TEST(SCOPE_PRIVATE)) {
	    noex = NOEX_PRIVATE;
	    rb_warning((scope_vmode == SCOPE_MODFUNC) ?
		       "attribute accessor as module_function" :
		       "private attribute?");
	}
	else if (SCOPE_TEST(SCOPE_PROTECTED)) {
	    noex = NOEX_PROTECTED;
	}
	else {
	    noex = NOEX_PUBLIC;
	}
    }

    if (!rb_is_local_id(id) && !rb_is_const_id(id)) {
	rb_name_error(id, "invalid attribute name `%s'", rb_id2name(id));
    }
    name = rb_id2name(id);
    if (!name) {
	rb_raise(rb_eArgError, "argument needs to be symbol or string");
    }
    buf = ALLOCA_N(char,strlen(name)+2);
    sprintf(buf, "@%s", name);
    attriv = rb_intern(buf);
    if (read) {
	rb_add_method(klass, id, NEW_IVAR(attriv), noex);
    }
    if (write) {
	rb_add_method(klass, rb_id_attrset(id), NEW_ATTRSET(attriv), noex);
    }
}

VALUE ruby_errinfo = Qnil;
extern int ruby_nerrs;

static VALUE rb_eLocalJumpError;
static VALUE rb_eSysStackError;

extern VALUE ruby_top_self;

struct FRAME *ruby_frame;
struct SCOPE *ruby_scope;
static struct FRAME *top_frame;
static struct SCOPE *top_scope;

static unsigned long frame_unique = 0;

#define PUSH_FRAME() do {		\
    struct FRAME _frame;		\
    _frame.prev = ruby_frame;		\
    _frame.tmp  = 0;			\
    _frame.node = ruby_current_node;	\
    _frame.iter = ruby_iter->iter;	\
    _frame.argc = 0;			\
    _frame.flags = 0;			\
    _frame.uniq = frame_unique++;	\
    ruby_frame = &_frame

#define POP_FRAME()  			\
    ruby_current_node = _frame.node;	\
    ruby_frame = _frame.prev;		\
} while (0)

struct BLOCK {
    NODE *var;
    NODE *body;
    VALUE self;
    struct FRAME frame;
    struct SCOPE *scope;
    VALUE klass;
    NODE *cref;
    int iter;
    int vmode;
    int flags;
    int uniq;
    struct RVarmap *dyna_vars;
    VALUE orig_thread;
    VALUE wrapper;
    VALUE block_obj;
    struct BLOCK *outer;
    struct BLOCK *prev;
};

#define BLOCK_D_SCOPE 1
#define BLOCK_LAMBDA  2
#define BLOCK_FROM_METHOD  4

static struct BLOCK *ruby_block;
static unsigned long block_unique = 0;

#define PUSH_BLOCK(v,b) do {		\
    struct BLOCK _block;		\
    _block.var = (v);			\
    _block.body = (b);			\
    _block.self = self;			\
    _block.frame = *ruby_frame;		\
    _block.klass = ruby_class;		\
    _block.cref = ruby_cref;		\
    _block.frame.node = ruby_current_node;\
    _block.scope = ruby_scope;		\
    _block.prev = ruby_block;		\
    _block.outer = ruby_block;		\
    _block.iter = ruby_iter->iter;	\
    _block.vmode = scope_vmode;		\
    _block.flags = BLOCK_D_SCOPE;	\
    _block.dyna_vars = ruby_dyna_vars;	\
    _block.wrapper = ruby_wrapper;	\
    _block.block_obj = 0;		\
    _block.uniq = (b)?block_unique++:0; \
    if (b) {				\
	prot_tag->blkid = _block.uniq;  \
    }                                   \
    ruby_block = &_block

#define POP_BLOCK() \
   ruby_block = _block.prev; \
} while (0)

struct RVarmap *ruby_dyna_vars;
#define PUSH_VARS() do { \
    struct RVarmap * volatile _old; \
    _old = ruby_dyna_vars; \
    ruby_dyna_vars = 0

#define POP_VARS() \
    if (_old && (ruby_scope->flags & SCOPE_DONT_RECYCLE)) {\
	if (RBASIC(_old)->flags) /* unless it's already recycled */ \
	    FL_SET(_old, DVAR_DONT_RECYCLE); \
    }\
    ruby_dyna_vars = _old; \
} while (0)

#define DVAR_DONT_RECYCLE FL_USER2

static struct RVarmap*
new_dvar(id, value, prev)
    ID id;
    VALUE value;
    struct RVarmap *prev;
{
    NEWOBJ(vars, struct RVarmap);
    OBJSETUP(vars, 0, T_VARMAP);
    vars->id = id;
    vars->val = value;
    vars->next = prev;

    return vars;
}

VALUE
rb_dvar_defined(id)
    ID id;
{
    struct RVarmap *vars = ruby_dyna_vars;

    while (vars) {
	if (vars->id == id) return Qtrue;
	vars = vars->next;
    }
    return Qfalse;
}

VALUE
rb_dvar_curr(id)
    ID id;
{
    struct RVarmap *vars = ruby_dyna_vars;

    while (vars) {
	if (vars->id == 0) break;
	if (vars->id == id) return Qtrue;
	vars = vars->next;
    }
    return Qfalse;
}

VALUE
rb_dvar_ref(id)
    ID id;
{
    struct RVarmap *vars = ruby_dyna_vars;

    while (vars) {
	if (vars->id == id) {
	    return vars->val;
	}
	vars = vars->next;
    }
    return Qnil;
}

void
rb_dvar_push(id, value)
    ID id;
    VALUE value;
{
    ruby_dyna_vars = new_dvar(id, value, ruby_dyna_vars);
}

static void
dvar_asgn_internal(id, value, curr)
    ID id;
    VALUE value;
    int curr;
{
    int n = 0;
    struct RVarmap *vars = ruby_dyna_vars;

    while (vars) {
	if (curr && vars->id == 0) {
	    /* first null is a dvar header */
	    n++;
	    if (n == 2) break;
	}
	if (vars->id == id) {
	    vars->val = value;
	    return;
	}
	vars = vars->next;
    }
    if (!ruby_dyna_vars) {
	ruby_dyna_vars = new_dvar(id, value, 0);
    }
    else {
	vars = new_dvar(id, value, ruby_dyna_vars->next);
	ruby_dyna_vars->next = vars;
    }
}

static inline void
dvar_asgn(id, value)
    ID id;
    VALUE value;
{
    dvar_asgn_internal(id, value, 0);
}

static inline void
dvar_asgn_curr(id, value)
    ID id;
    VALUE value;
{
    dvar_asgn_internal(id, value, 1);
}

VALUE *
rb_svar(cnt)
    int cnt;
{
    struct RVarmap *vars = ruby_dyna_vars;
    ID id;

    if (!ruby_scope->local_tbl) return NULL;
    if (cnt >= ruby_scope->local_tbl[0]) return NULL;
    id = ruby_scope->local_tbl[cnt+1];
    while (vars) {
	if (vars->id == id) return &vars->val;
	vars = vars->next;
    }
    if (ruby_scope->local_vars == 0) return NULL;
    return &ruby_scope->local_vars[cnt];
}

struct iter {
    int iter;
    struct iter *prev;
};
static struct iter *ruby_iter;

#define ITER_NOT 0
#define ITER_PRE 1
#define ITER_CUR 2

#define PUSH_ITER(i) do {		\
    struct iter _iter;			\
    _iter.prev = ruby_iter;		\
    _iter.iter = (i);			\
    ruby_iter = &_iter

#define POP_ITER()			\
    ruby_iter = _iter.prev;		\
} while (0)

struct tag {
    rb_jmpbuf_t buf;
    struct FRAME *frame;
    struct iter *iter;
    VALUE tag;
    VALUE retval;
    struct SCOPE *scope;
    VALUE dst;
    struct tag *prev;
    int blkid;
};
static struct tag *prot_tag;

#define PUSH_TAG(ptag) do {		\
    struct tag _tag;			\
    _tag.retval = Qnil;			\
    _tag.frame = ruby_frame;		\
    _tag.iter = ruby_iter;		\
    _tag.prev = prot_tag;		\
    _tag.scope = ruby_scope;		\
    _tag.tag = ptag;			\
    _tag.dst = 0;			\
    _tag.blkid = 0;			\
    prot_tag = &_tag

#define PROT_NONE   Qfalse	/* 0 */
#define PROT_THREAD Qtrue	/* 2 */
#define PROT_FUNC   INT2FIX(0)	/* 1 */
#define PROT_LOOP   INT2FIX(1)	/* 3 */
#define PROT_LAMBDA INT2FIX(2)	/* 5 */
#define PROT_YIELD  INT2FIX(3)	/* 7 */
#define PROT_TOP    INT2FIX(4)	/* 9 */

#define EXEC_TAG()    (FLUSH_REGISTER_WINDOWS, setjmp(prot_tag->buf))

#define JUMP_TAG(st) do {		\
    ruby_frame = prot_tag->frame;	\
    ruby_iter = prot_tag->iter;		\
    longjmp(prot_tag->buf,(st));	\
} while (0)

#define POP_TAG()			\
    prot_tag = _tag.prev;		\
} while (0)

#define TAG_DST() (_tag.dst == (VALUE)ruby_frame->uniq)

#define TAG_RETURN	0x1
#define TAG_BREAK	0x2
#define TAG_NEXT	0x3
#define TAG_RETRY	0x4
#define TAG_REDO	0x5
#define TAG_RAISE	0x6
#define TAG_THROW	0x7
#define TAG_FATAL	0x8
#define TAG_CONTCALL	0x9
#define TAG_THREAD	0xa
#define TAG_MASK	0xf

VALUE ruby_class;
static VALUE ruby_wrapper;	/* security wrapper */

#define PUSH_CLASS(c) do {		\
    VALUE _class = ruby_class;		\
    ruby_class = (c)

#define POP_CLASS() ruby_class = _class; \
} while (0)

static NODE *ruby_cref = 0;
static NODE *top_cref;
#define PUSH_CREF(c) ruby_cref = NEW_NODE(NODE_CREF,(c),0,ruby_cref)
#define POP_CREF() ruby_cref = ruby_cref->nd_next

#define PUSH_SCOPE() do {		\
    volatile int _vmode = scope_vmode;	\
    struct SCOPE * volatile _old;	\
    NEWOBJ(_scope, struct SCOPE);	\
    OBJSETUP(_scope, 0, T_SCOPE);	\
    _scope->local_tbl = 0;		\
    _scope->local_vars = 0;		\
    _scope->flags = 0;			\
    _old = ruby_scope;			\
    ruby_scope = _scope;		\
    scope_vmode = SCOPE_PUBLIC

typedef struct thread * rb_thread_t;
static rb_thread_t curr_thread = 0;
static rb_thread_t main_thread;
static void scope_dup _((struct SCOPE *));

#define POP_SCOPE() 			\
    if (ruby_scope->flags & SCOPE_DONT_RECYCLE) {\
	if (_old) scope_dup(_old);	\
    }					\
    if (!(ruby_scope->flags & SCOPE_MALLOC)) {\
	ruby_scope->local_vars = 0;	\
	ruby_scope->local_tbl  = 0;	\
	if (!(ruby_scope->flags & SCOPE_DONT_RECYCLE) && \
	    ruby_scope != top_scope) {	\
	    rb_gc_force_recycle((VALUE)ruby_scope);\
	}				\
    }					\
    ruby_scope->flags |= SCOPE_NOSTACK;	\
    ruby_scope = _old;			\
    scope_vmode = _vmode;		\
} while (0)

struct ruby_env {
    struct ruby_env *prev;
    struct FRAME *frame;
    struct SCOPE *scope;
    struct BLOCK *block;
    struct iter *iter;
    struct tag *tag;
    NODE *cref;
};

static void push_thread_anchor _((struct ruby_env *));
static void pop_thread_anchor _((struct ruby_env *));

#define PUSH_THREAD_TAG() PUSH_TAG(PROT_THREAD); \
    do {					 \
	struct ruby_env _interp;		 \
	push_thread_anchor(&_interp);
#define POP_THREAD_TAG()			 \
	pop_thread_anchor(&_interp);		 \
    } while (0);				 \
    POP_TAG()

static VALUE rb_eval _((VALUE,NODE*));
static VALUE eval _((VALUE,VALUE,VALUE,char*,int));
static NODE *compile _((VALUE, char*, int));

static VALUE rb_yield_0 _((VALUE, VALUE, VALUE, int, int));

#define YIELD_LAMBDA_CALL 1
#define YIELD_PROC_CALL   2
#define YIELD_PUBLIC_DEF  4
#define YIELD_FUNC_AVALUE 1
#define YIELD_FUNC_SVALUE 2

static VALUE rb_call _((VALUE,VALUE,ID,int,const VALUE*,int));
static VALUE module_setup _((VALUE,NODE*));

static VALUE massign _((VALUE,NODE*,VALUE,int));
static void assign _((VALUE,NODE*,VALUE,int));

typedef struct event_hook {
    rb_event_hook_func_t func;
    rb_event_t events;
    struct event_hook *next;
} rb_event_hook_t;

static rb_event_hook_t *event_hooks;

#define EXEC_EVENT_HOOK(event, node, self, id, klass) \
    do { \
	rb_event_hook_t *hook; \
	\
	for (hook = event_hooks; hook; hook = hook->next) { \
	    if (hook->events & event) \
		(*hook->func)(event, node, self, id, klass); \
	} \
    } while (0)

static VALUE trace_func = 0;
static int tracing = 0;
static void call_trace_func _((rb_event_t,NODE*,VALUE,ID,VALUE));

#if 0
#define SET_CURRENT_SOURCE() (ruby_sourcefile = ruby_current_node->nd_file, \
			      ruby_sourceline = nd_line(ruby_current_node))
#else
#define SET_CURRENT_SOURCE() ((void)0)
#endif

void
ruby_set_current_source()
{
    if (ruby_current_node) {
	ruby_sourcefile = ruby_current_node->nd_file;
	ruby_sourceline = nd_line(ruby_current_node);
    }
}

static void
#ifdef HAVE_STDARG_PROTOTYPES
warn_printf(const char *fmt, ...)
#else
warn_printf(fmt, va_alist)
    const char *fmt;
    va_dcl
#endif
{
    char buf[BUFSIZ];
    va_list args;

    va_init_list(args, fmt);
    vsnprintf(buf, BUFSIZ, fmt, args);
    va_end(args);
    rb_write_error(buf);
}

#define warn_print(x) rb_write_error(x)
#define warn_print2(x,l) rb_write_error2(x,l)

static void
error_pos()
{
    ruby_set_current_source();
    if (ruby_sourcefile) {
	if (ruby_frame->callee) {
	    warn_printf("%s:%d:in `%s'", ruby_sourcefile, ruby_sourceline,
			rb_id2name(ruby_frame->callee));
	}
	else if (ruby_sourceline == 0) {
	    warn_printf("%s", ruby_sourcefile);
	}
	else {
	    warn_printf("%s:%d", ruby_sourcefile, ruby_sourceline);
	}
    }
}

static VALUE
get_backtrace(info)
    VALUE info;
{
    if (NIL_P(info)) return Qnil;
    info = rb_funcall(info, rb_intern("backtrace"), 0);
    if (NIL_P(info)) return Qnil;
    return rb_check_array_type(info);
}

static void
set_backtrace(info, bt)
    VALUE info, bt;
{
    rb_funcall(info, rb_intern("set_backtrace"), 1, bt);
}

static void
error_print()
{
    VALUE errat = Qnil;		/* OK */
    volatile VALUE eclass, e;
    char *einfo;
    long elen;

    if (NIL_P(ruby_errinfo)) return;

    PUSH_TAG(PROT_NONE);
    if (EXEC_TAG() == 0) {
	errat = get_backtrace(ruby_errinfo);
    }
    else {
	errat = Qnil;
    }
    if (EXEC_TAG()) goto error;
    if (NIL_P(errat)){
	ruby_set_current_source();
	if (ruby_sourcefile)
	    warn_printf("%s:%d", ruby_sourcefile, ruby_sourceline);
	else
	    warn_printf("%d", ruby_sourceline);
    }
    else if (RARRAY(errat)->len == 0) {
	error_pos();
    }
    else {
	VALUE mesg = RARRAY(errat)->ptr[0];

	if (NIL_P(mesg)) error_pos();
	else {
	    warn_print2(RSTRING(mesg)->ptr, RSTRING(mesg)->len);
	}
    }

    eclass = CLASS_OF(ruby_errinfo);
    if (EXEC_TAG() == 0) {
	e = rb_funcall(ruby_errinfo, rb_intern("message"), 0, 0);
	StringValue(e);
	einfo = RSTRING(e)->ptr;
	elen = RSTRING(e)->len;
    }
    else {
	einfo = "";
	elen = 0;
    }
    if (EXEC_TAG()) goto error;
    if (eclass == rb_eRuntimeError && elen == 0) {
	warn_print(": unhandled exception\n");
    }
    else {
	VALUE epath;

	epath = rb_class_name(eclass);
	if (elen == 0) {
	    warn_print(": ");
	    warn_print2(RSTRING(epath)->ptr, RSTRING(epath)->len);
	    warn_print("\n");
	}
	else {
	    char *tail  = 0;
	    long len = elen;

	    if (RSTRING(epath)->ptr[0] == '#') epath = 0;
	    if (tail = memchr(einfo, '\n', elen)) {
		len = tail - einfo;
		tail++;		/* skip newline */
	    }
	    warn_print(": ");
	    warn_print2(einfo, len);
	    if (epath) {
		warn_print(" (");
		warn_print2(RSTRING(epath)->ptr, RSTRING(epath)->len);
		warn_print(")\n");
	    }
	    if (tail) {
		warn_print2(tail, elen-len-1);
	    }
	}
    }

    if (!NIL_P(errat)) {
	long i;
	struct RArray *ep = RARRAY(errat);

#define TRACE_MAX (TRACE_HEAD+TRACE_TAIL+5)
#define TRACE_HEAD 8
#define TRACE_TAIL 5

	ep = RARRAY(errat);
	for (i=1; i<ep->len; i++) {
	    if (TYPE(ep->ptr[i]) == T_STRING) {
		warn_printf("\tfrom %s\n", RSTRING(ep->ptr[i])->ptr);
	    }
	    if (i == TRACE_HEAD && ep->len > TRACE_MAX) {
		warn_printf("\t ... %ld levels...\n",
			ep->len - TRACE_HEAD - TRACE_TAIL);
		i = ep->len - TRACE_TAIL;
	    }
	}
    }
  error:
    POP_TAG();
}

#if defined(__APPLE__)
#define environ (*_NSGetEnviron())
#elif !defined(_WIN32) && !defined(__MACOS__) || defined(_WIN32_WCE)
extern char **environ;
#endif
char **rb_origenviron;

void rb_call_inits _((void));
void Init_stack _((VALUE*));
void Init_heap _((void));
void Init_ext _((void));

#ifdef HAVE_NATIVETHREAD
static rb_nativethread_t ruby_thid;
int
is_ruby_native_thread()
{
    return NATIVETHREAD_EQUAL(ruby_thid, NATIVETHREAD_CURRENT());
}

# ifdef HAVE_NATIVETHREAD_KILL
void
ruby_native_thread_kill(sig)
    int sig;
{
    NATIVETHREAD_KILL(ruby_thid, sig);
}
# endif
#endif

NORETURN(static void rb_thread_start_1 _((void)));

void
ruby_init()
{
    static int initialized = 0;
    static struct FRAME frame;
    static struct iter iter;
    int state;

    if (initialized)
	return;
    initialized = 1;
#ifdef HAVE_NATIVETHREAD
    ruby_thid = NATIVETHREAD_CURRENT();
#endif

    ruby_frame = top_frame = &frame;
    ruby_iter = &iter;

#ifdef __MACOS__
    rb_origenviron = 0;
#else
    rb_origenviron = environ;
#endif

    Init_stack((void*)&state);
    Init_heap();
    PUSH_SCOPE();
    top_scope = ruby_scope;
    /* default visibility is private at toplevel */
    SCOPE_SET(SCOPE_PRIVATE);

    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	rb_call_inits();
	ruby_class = rb_cObject;
	ruby_frame->self = ruby_top_self;
	top_cref = rb_node_newnode(NODE_CREF,rb_cObject,0,0);
	ruby_cref = top_cref;
	rb_define_global_const("TOPLEVEL_BINDING", rb_f_binding(ruby_top_self));
#ifdef __MACOS__
	_macruby_init();
#endif
	ruby_prog_init();
	ALLOW_INTS;
    }
    POP_TAG();
    if (state) {
	error_print();
	exit(EXIT_FAILURE);
    }
    POP_SCOPE();
    ruby_scope = top_scope;
    top_scope->flags &= ~SCOPE_NOSTACK;
    ruby_running = 1;
}

static VALUE
eval_node(self, node)
    VALUE self;
    NODE *node;
{
    if (!node) return Qnil;
    if (nd_type(node) == NODE_PRELUDE) {
	rb_eval(self, node->nd_head);
	node = node->nd_body;
    }
    if (!node) return Qnil;
    return rb_eval(self, node);
}

int ruby_in_eval;

static void rb_thread_cleanup _((void));
static void rb_thread_wait_other_threads _((void));

static int thread_set_raised();
static int thread_reset_raised();

static VALUE exception_error;
static VALUE sysstack_error;

static int
error_handle(ex)
    int ex;
{
    int status = EXIT_FAILURE;

    if (thread_set_raised()) return EXIT_FAILURE;
    switch (ex & TAG_MASK) {
      case 0:
	status = EXIT_SUCCESS;
	break;

      case TAG_RETURN:
	error_pos();
	warn_print(": unexpected return\n");
	break;
      case TAG_NEXT:
	error_pos();
	warn_print(": unexpected next\n");
	break;
      case TAG_BREAK:
	error_pos();
	warn_print(": unexpected break\n");
	break;
      case TAG_REDO:
	error_pos();
	warn_print(": unexpected redo\n");
	break;
      case TAG_RETRY:
	error_pos();
	warn_print(": retry outside of rescue clause\n");
	break;
      case TAG_THROW:
	if (prot_tag && prot_tag->frame && prot_tag->frame->node) {
	    NODE *tag = prot_tag->frame->node;
	    warn_printf("%s:%d: uncaught throw\n",
		    tag->nd_file, nd_line(tag));
	}
	else {
	    error_pos();
	    warn_printf(": unexpected throw\n");
	}
	break;
      case TAG_RAISE:
      case TAG_FATAL:
	if (rb_obj_is_kind_of(ruby_errinfo, rb_eSystemExit)) {
	    VALUE st = rb_iv_get(ruby_errinfo, "status");
	    status = NUM2INT(st);
	}
	else {
	    error_print();
	}
	break;
      default:
	rb_bug("Unknown longjmp status %d", ex);
	break;
    }
    thread_reset_raised();
    return status;
}

void
ruby_options(argc, argv)
    int argc;
    char **argv;
{
    int state;

#ifdef _WIN32
    argc = rb_w32_cmdvector(GetCommandLine(), &argv);
#endif

    Init_stack((void*)&state);
    PUSH_THREAD_TAG();
    if ((state = EXEC_TAG()) == 0) {
	ruby_process_options(argc, argv);
    }
    else {
	if (state == TAG_THREAD) {
	    rb_thread_start_1();
	}
	trace_func = 0;
	tracing = 0;
	exit(error_handle(state));
    }
    POP_THREAD_TAG();

#ifdef _WIN32_WCE
    wce_FreeCommandLine();
#endif
}

void rb_exec_end_proc _((void));

static void
ruby_finalize_0()
{
    PUSH_TAG(PROT_NONE);
    if (EXEC_TAG() == 0) {
	rb_trap_exit();
    }
    POP_TAG();
    rb_exec_end_proc();
}

static void
ruby_finalize_1()
{
    signal(SIGINT, SIG_DFL);
    ruby_errinfo = 0;
    rb_gc_call_finalizer_at_exit();
    trace_func = 0;
    tracing = 0;
}

void
ruby_finalize()
{
    ruby_finalize_0();
    ruby_finalize_1();
}

int
ruby_cleanup(ex)
    int ex;
{
    int state;
    volatile VALUE err = ruby_errinfo;

    ruby_safe_level = 0;
    Init_stack((void*)&state);
    PUSH_THREAD_TAG();
    PUSH_ITER(ITER_NOT);
    if ((state = EXEC_TAG()) == 0) {
	ruby_finalize_0();
	if (ruby_errinfo) err = ruby_errinfo;
	rb_thread_cleanup();
	rb_thread_wait_other_threads();
    }
    else if (state == TAG_THREAD) {
	rb_thread_start_1();
    }
    else if (ex == 0) {
	ex = state;
    }
    POP_ITER();
    ruby_errinfo = err;
    ex = error_handle(ex);
    ruby_finalize_1();
    POP_THREAD_TAG();

    if (err && rb_obj_is_kind_of(err, rb_eSystemExit)) {
	VALUE st = rb_iv_get(err, "status");
	return NUM2INT(st);
    }
    return ex;
}

extern NODE *ruby_eval_tree;

static void cont_call _((VALUE));

static int
ruby_exec_internal()
{
    int state;

    PUSH_THREAD_TAG();
    PUSH_ITER(ITER_NOT);
    /* default visibility is private at toplevel */
    SCOPE_SET(SCOPE_PRIVATE);
    if ((state = EXEC_TAG()) == 0) {
	eval_node(ruby_top_self, ruby_eval_tree);
    }
#if 0
    else if (state == TAG_CONTCALL) {
	cont_call(prot_tag->retval);
    }
#endif
    else if (state == TAG_THREAD) {
	rb_thread_start_1();
    }
    POP_ITER();
    POP_THREAD_TAG();
    return state;
}

int
ruby_exec()
{
    volatile NODE *tmp;

    Init_stack((void*)&tmp);
    return ruby_exec_internal();
}

void
ruby_stop(ex)
    int ex;
{
    exit(ruby_cleanup(ex));
}

void
ruby_run()
{
    int state;
    static int ex;

    if (ruby_nerrs > 0) exit(EXIT_FAILURE);
    state = ruby_exec();
    if (state && !ex) ex = state;
    ruby_stop(ex);
}

static void
compile_error(at)
    const char *at;
{
    VALUE str;

    ruby_nerrs = 0;
    str = rb_str_buf_new2("compile error");
    if (at) {
	rb_str_buf_cat2(str, " in ");
	rb_str_buf_cat2(str, at);
    }
    rb_str_buf_cat(str, "\n", 1);
    if (!NIL_P(ruby_errinfo)) {
	rb_str_append(str, rb_obj_as_string(ruby_errinfo));
    }
    rb_exc_raise(rb_exc_new3(rb_eSyntaxError, str));
}

VALUE
rb_eval_string(str)
    const char *str;
{
    VALUE v;
    NODE *oldsrc = ruby_current_node;

    ruby_current_node = 0;
    ruby_sourcefile = rb_source_filename("(eval)");
    v = eval(ruby_top_self, rb_str_new2(str), Qnil, 0, 0);
    ruby_current_node = oldsrc;

    return v;
}

VALUE
rb_eval_string_protect(str, state)
    const char *str;
    int *state;
{
    return rb_protect((VALUE (*)_((VALUE)))rb_eval_string, (VALUE)str, state);
}

VALUE
rb_eval_string_wrap(str, state)
    const char *str;
    int *state;
{
    int status;
    VALUE self = ruby_top_self;
    VALUE wrapper = ruby_wrapper;
    VALUE val;

    PUSH_CLASS(ruby_wrapper = rb_module_new());
    ruby_top_self = rb_obj_clone(ruby_top_self);
    rb_extend_object(ruby_top_self, ruby_wrapper);
    PUSH_FRAME();
    ruby_frame->callee = 0;
    ruby_frame->this_func = 0;
    ruby_frame->this_class = 0;
    ruby_frame->self = self;
    PUSH_CREF(ruby_wrapper);
    PUSH_SCOPE();

    val = rb_eval_string_protect(str, &status);
    ruby_top_self = self;

    POP_SCOPE();
    POP_FRAME();
    POP_CLASS();
    ruby_wrapper = wrapper;
    if (state) {
	*state = status;
    }
    else if (status) {
	JUMP_TAG(status);
    }
    return val;
}

NORETURN(static void localjump_error(const char*, VALUE, int));
static void
localjump_error(mesg, value, reason)
    const char *mesg;
    VALUE value;
    int reason;
{
    VALUE exc = rb_exc_new2(rb_eLocalJumpError, mesg);
    ID id;

    rb_iv_set(exc, "@exit_value", value);
    switch (reason) {
      case TAG_BREAK:
	id = rb_intern("break"); break;
      case TAG_REDO:
	id = rb_intern("redo"); break;
      case TAG_RETRY:
	id = rb_intern("retry"); break;
      case TAG_NEXT:
	id = rb_intern("next"); break;
      case TAG_RETURN:
	id = rb_intern("return"); break;
      default:
	id = rb_intern("noreason"); break;
    }
    rb_iv_set(exc, "@reason", ID2SYM(id));
    rb_exc_raise(exc);
}

/*
 * call_seq:
 *   local_jump_error.exit_value  => obj
 *
 * Returns the exit value associated with this +LocalJumpError+.
 */
static VALUE
localjump_xvalue(exc)
    VALUE exc;
{
    return rb_iv_get(exc, "@exit_value");
}

/*
 * call-seq:
 *    local_jump_error.reason   => symbol
 *
 * The reason this block was terminated:
 * :break, :redo, :retry, :next, :return, or :noreason.
 */

static VALUE
localjump_reason(exc)
    VALUE exc;
{
    return rb_iv_get(exc, "@reason");
}

NORETURN(static void jump_tag_but_local_jump _((int,VALUE)));
static void
jump_tag_but_local_jump(state, val)
    int state;
    VALUE val;
{

    if (val == Qundef) val = prot_tag->retval;
    switch (state) {
      case 0:
	break;
      case TAG_RETURN:
	localjump_error("unexpected return", val, state);
	break;
      case TAG_BREAK:
	localjump_error("unexpected break", val, state);
	break;
      case TAG_NEXT:
	localjump_error("unexpected next", val, state);
	break;
      case TAG_REDO:
	localjump_error("unexpected redo", Qnil, state);
	break;
      case TAG_RETRY:
	localjump_error("retry outside of rescue clause", Qnil, state);
	break;
      default:
	break;
    }
    JUMP_TAG(state);
}

VALUE
rb_eval_cmd(cmd, arg, level)
    VALUE cmd, arg;
    int level;
{
    int state;
    VALUE val = Qnil;		/* OK */
    struct SCOPE *saved_scope;
    volatile int safe = ruby_safe_level;

    if (OBJ_TAINTED(cmd)) {
	level = 4;
    }
    if (TYPE(cmd) != T_STRING) {
	PUSH_ITER(ITER_NOT);
	PUSH_TAG(PROT_NONE);
	ruby_safe_level = level;
	if ((state = EXEC_TAG()) == 0) {
	    val = rb_funcall2(cmd, rb_intern("call"), RARRAY(arg)->len, RARRAY(arg)->ptr);
	}
	ruby_safe_level = safe;
	POP_TAG();
	POP_ITER();
	if (state) JUMP_TAG(state);
	return val;
    }

    saved_scope = ruby_scope;
    ruby_scope = top_scope;
    PUSH_FRAME();
    ruby_frame->callee = 0;
    ruby_frame->this_func = 0;
    ruby_frame->this_class = 0;
    ruby_frame->self = ruby_top_self;
    PUSH_CREF(ruby_wrapper ? ruby_wrapper : rb_cObject);

    ruby_safe_level = level;

    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	val = eval(ruby_top_self, cmd, Qnil, 0, 0);
    }
    if (ruby_scope->flags & SCOPE_DONT_RECYCLE)
	scope_dup(saved_scope);
    ruby_scope = saved_scope;
    ruby_safe_level = safe;
    POP_TAG();
    POP_FRAME();

    jump_tag_but_local_jump(state, val);
    return val;
}

#define ruby_cbase (ruby_cref->nd_clss)

static VALUE
ev_const_defined(cref, id, self)
    NODE *cref;
    ID id;
    VALUE self;
{
    NODE *cbase = cref;
    VALUE result;

    while (cbase && cbase->nd_next) {
	struct RClass *klass = RCLASS(cbase->nd_clss);

	if (NIL_P(klass)) return rb_const_defined(CLASS_OF(self), id);
	if (klass->iv_tbl && st_lookup(klass->iv_tbl, id, &result)) {
	    if (result == Qundef && NIL_P(rb_autoload_p((VALUE)klass, id))) {
		return Qfalse;
	    }
	    return Qtrue;
	}
	cbase = cbase->nd_next;
    }
    return rb_const_defined(cref->nd_clss, id);
}

static VALUE
ev_const_get(cref, id, self)
    NODE *cref;
    ID id;
    VALUE self;
{
    NODE *cbase = cref;
    VALUE result;

    while (cbase && cbase->nd_next) {
	VALUE klass = cbase->nd_clss;

	if (NIL_P(klass)) return rb_const_get(CLASS_OF(self), id);
	while (RCLASS(klass)->iv_tbl && st_lookup(RCLASS(klass)->iv_tbl, id, &result)) {
	    if (result == Qundef) {
		rb_autoload_load(klass, id);
		continue;
	    }
	    return result;
	}
	cbase = cbase->nd_next;
    }
    return rb_const_get(cref->nd_clss, id);
}

static VALUE
cvar_cbase()
{
    NODE *cref = ruby_cref;

    while (cref && cref->nd_next && (NIL_P(cref->nd_clss) || FL_TEST(cref->nd_clss, FL_SINGLETON))) {
	cref = cref->nd_next;
	if (!cref->nd_next) {
	    rb_warn("class variable access from toplevel singleton method");
	}
    }
    if (NIL_P(cref->nd_clss)) {
	rb_raise(rb_eTypeError, "no class variables available");
    }
    return cref->nd_clss;
}

/*
 *  call-seq:
 *     Module.nesting    => array
 *  
 *  Returns the list of +Modules+ nested at the point of call.
 *     
 *     module M1
 *       module M2
 *         $a = Module.nesting
 *       end
 *     end
 *     $a           #=> [M1::M2, M1]
 *     $a[0].name   #=> "M1::M2"
 */

static VALUE
rb_mod_nesting()
{
    NODE *cbase = ruby_cref;
    VALUE ary = rb_ary_new();

    while (cbase && cbase->nd_next) {
	if (!NIL_P(cbase->nd_clss)) rb_ary_push(ary, cbase->nd_clss);
	cbase = cbase->nd_next;
    }
    if (ruby_wrapper && RARRAY(ary)->len == 0) {
	rb_ary_push(ary, ruby_wrapper);
    }
    return ary;
}

/*
 *  call-seq:
 *     Module.constants   => array
 *  
 *  Returns an array of the names of all constants defined in the
 *  system. This list includes the names of all modules and classes.
 *     
 *     p Module.constants.sort[1..5]
 *     
 *  <em>produces:</em>
 *     
 *     ["ARGV", "ArgumentError", "Array", "Bignum", "Binding"]
 */

static VALUE
rb_mod_s_constants()
{
    NODE *cbase = ruby_cref;
    void *data = 0;

    while (cbase) {
	if (!NIL_P(cbase->nd_clss)) {
	    data = rb_mod_const_at(cbase->nd_clss, data);
	}
	cbase = cbase->nd_next;
    }

    if (!NIL_P(ruby_cbase)) {
	data = rb_mod_const_of(ruby_cbase, data);
    }
    return rb_const_list(data);
}

void
rb_frozen_class_p(klass)
    VALUE klass;
{
    char *desc = "something(?!)";

    if (OBJ_FROZEN(klass)) {
	if (FL_TEST(klass, FL_SINGLETON))
	    desc = "object";
	else {
	    switch (TYPE(klass)) {
	      case T_MODULE:
	      case T_ICLASS:
		desc = "module"; break;
	      case T_CLASS:
		desc = "class"; break;
	    }
	}
	rb_error_frozen(desc);
    }
}

void
rb_undef(klass, id)
    VALUE klass;
    ID id;
{
    VALUE origin;
    NODE *body;

    if (ruby_cbase == rb_cObject && klass == rb_cObject) {
	rb_secure(4);
    }
    if (ruby_safe_level >= 4 && !OBJ_TAINTED(klass)) {
	rb_raise(rb_eSecurityError, "Insecure: can't undef `%s'", rb_id2name(id));
    }
    rb_frozen_class_p(klass);
    if (id == __id__ || id == __send__ || id == init) {
	rb_warn("undefining `%s' may cause serious problem", rb_id2name(id));
    }
    body = search_method(klass, id, &origin);
    if (!body || !body->nd_body) {
	char *s0 = " class";
	VALUE c = klass;

	if (FL_TEST(c, FL_SINGLETON)) {
	    VALUE obj = rb_iv_get(klass, "__attached__");

	    switch (TYPE(obj)) {
	      case T_MODULE:
	      case T_CLASS:
		c = obj;
		s0 = "";
	    }
	}
	else if (TYPE(c) == T_MODULE) {
	    s0 = " module";
	}
	rb_name_error(id, "undefined method `%s' for%s `%s'",
		      rb_id2name(id),s0,rb_class2name(c));
    }
    rb_add_method(klass, id, 0, NOEX_PUBLIC);
    if (FL_TEST(klass, FL_SINGLETON)) {
	rb_funcall(rb_iv_get(klass, "__attached__"),
		   singleton_undefined, 1, ID2SYM(id));
    }
    else {
	rb_funcall(klass, undefined, 1, ID2SYM(id));
    }
}

/*
 *  call-seq:
 *     undef_method(symbol)    => self
 *  
 *  Prevents the current class from responding to calls to the named
 *  method. Contrast this with <code>remove_method</code>, which deletes
 *  the method from the particular class; Ruby will still search
 *  superclasses and mixed-in modules for a possible receiver.
 *     
 *     class Parent
 *       def hello
 *         puts "In parent"
 *       end
 *     end
 *     class Child < Parent
 *       def hello
 *         puts "In child"
 *       end
 *     end
 *     
 *     
 *     c = Child.new
 *     c.hello
 *     
 *     
 *     class Child
 *       remove_method :hello  # remove from child, still in parent
 *     end
 *     c.hello
 *     
 *     
 *     class Child
 *       undef_method :hello   # prevent any calls to 'hello'
 *     end
 *     c.hello
 *     
 *  <em>produces:</em>
 *     
 *     In child
 *     In parent
 *     prog.rb:23: undefined method `hello' for #<Child:0x401b3bb4> (NoMethodError)
 */

static VALUE
rb_mod_undef_method(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    int i;

    for (i=0; i<argc; i++) {
	rb_undef(mod, rb_to_id(argv[i]));
    }
    return mod;
}

void
rb_alias(klass, name, def)
    VALUE klass;
    ID name, def;
{
    VALUE origin;
    NODE *orig, *body, *node;
    VALUE singleton = 0;

    rb_frozen_class_p(klass);
    if (name == def) return;
    if (klass == rb_cObject) {
	rb_secure(4);
    }
    orig = search_method(klass, def, &origin);
    if (!orig || !orig->nd_body) {
	if (TYPE(klass) == T_MODULE) {
	    orig = search_method(rb_cObject, def, &origin);
	}
    }
    if (!orig || !orig->nd_body) {
	print_undef(klass, def);
    }
    if (FL_TEST(klass, FL_SINGLETON)) {
	singleton = rb_iv_get(klass, "__attached__");
    }
    body = orig->nd_body;
    orig->nd_cnt++;
    if (nd_type(body) == NODE_FBODY) { /* was alias */
	def = body->nd_mid;
	origin = body->nd_orig;
	body = body->nd_head;
    }

    rb_clear_cache_by_id(name);
    if (RTEST(ruby_verbose) && st_lookup(RCLASS(klass)->m_tbl, name, (st_data_t *)&node)) {
	if (node->nd_cnt == 0 && node->nd_body) {
	    rb_warning("discarding old %s", rb_id2name(name));
	}
    }
    st_insert(RCLASS(klass)->m_tbl, name,
      (st_data_t)NEW_METHOD(NEW_FBODY(body, def, origin), orig->nd_noex));
    if (singleton) {
	rb_funcall(singleton, singleton_added, 1, ID2SYM(name));
    }
    else {
	rb_funcall(klass, added, 1, ID2SYM(name));
    }
}

/*
 *  call-seq:
 *     alias_method(new_name, old_name)   => self
 *  
 *  Makes <i>new_name</i> a new copy of the method <i>old_name</i>. This can
 *  be used to retain access to methods that are overridden.
 *     
 *     module Mod
 *       alias_method :orig_exit, :exit
 *       def exit(code=0)
 *         puts "Exiting with code #{code}"
 *         orig_exit(code)
 *       end
 *     end
 *     include Mod
 *     exit(99)
 *     
 *  <em>produces:</em>
 *     
 *     Exiting with code 99
 */

static VALUE
rb_mod_alias_method(mod, newname, oldname)
    VALUE mod, newname, oldname;
{
    rb_alias(mod, rb_to_id(newname), rb_to_id(oldname));
    return mod;
}

static NODE*
copy_node_scope(node, rval)
    NODE *node;
    NODE *rval;
{
    NODE *copy = NEW_NODE(NODE_SCOPE,0,rval,node->nd_next);

    if (node->nd_tbl) {
	copy->nd_tbl = ALLOC_N(ID, node->nd_tbl[0]+1);
	MEMCPY(copy->nd_tbl, node->nd_tbl, ID, node->nd_tbl[0]+1);
    }
    else {
	copy->nd_tbl = 0;
    }
    return copy;
}

#ifdef C_ALLOCA
# define TMP_PROTECT NODE * volatile tmp__protect_tmp=0
# define TMP_ALLOC(n)							\
    (tmp__protect_tmp = rb_node_newnode(NODE_ALLOCA,			\
			     ALLOC_N(VALUE,n),tmp__protect_tmp,n),	\
     (void*)tmp__protect_tmp->nd_head)
#else
# define TMP_PROTECT typedef int foobazzz
# define TMP_ALLOC(n) ALLOCA_N(VALUE,n)
#endif

#define SETUP_ARGS0(anode,alen) do {\
    NODE *n = anode;\
    if (!n) {\
	argc = 0;\
	argv = 0;\
    }\
    else if (nd_type(n) == NODE_ARRAY) {\
	argc=alen;\
	if (argc > 0) {\
	    int i;\
	    n = anode;\
	    argv = TMP_ALLOC(argc);\
	    for (i=0;i<argc;i++) {\
		argv[i] = rb_eval(self,n->nd_head);\
		n=n->nd_next;\
	    }\
	}\
	else {\
	    argc = 0;\
	    argv = 0;\
	}\
    }\
    else {\
	VALUE args = rb_eval(self,n);\
	if (TYPE(args) != T_ARRAY)\
	    args = rb_ary_to_ary(args);\
	argc = RARRAY(args)->len;\
	argv = ALLOCA_N(VALUE, argc);\
	MEMCPY(argv, RARRAY(args)->ptr, VALUE, argc);\
    }\
} while (0)

#define SETUP_ARGS(anode) SETUP_ARGS0(anode, anode->nd_alen)

#define BEGIN_CALLARGS do {\
    struct BLOCK *tmp_block = ruby_block;\
    int tmp_iter = ruby_iter->iter;\
    if (tmp_iter == ITER_PRE) {\
	ruby_block = ruby_block->outer;\
	tmp_iter = ITER_NOT;\
    }\
    PUSH_ITER(tmp_iter)

#define END_CALLARGS \
    ruby_block = tmp_block;\
    POP_ITER();\
} while (0)

#define MATCH_DATA *rb_svar(node->nd_cnt)

static const char* is_defined _((VALUE, NODE*, char*, int));

static char*
arg_defined(self, node, buf, type)
    VALUE self;
    NODE *node;
    char *buf;
    char *type;
{
    int argc;
    int i;

    if (!node) return type;	/* no args */
    if (nd_type(node) == NODE_ARRAY) {
	argc=node->nd_alen;
	if (argc > 0) {
	    for (i=0;i<argc;i++) {
		if (!is_defined(self, node->nd_head, buf, 0))
		    return 0;
		node = node->nd_next;
	    }
	}
    }
    else if (!is_defined(self, node, buf, 0)) {
	return 0;
    }
    return type;
}

static const char*
is_defined(self, node, buf, noeval)
    VALUE self;
    NODE *node;			/* OK */
    char *buf;
    int noeval;
{
    VALUE val;			/* OK */
    int state;
    static const char *ex = "expression";

    if (!node) return ex;
    switch (nd_type(node)) {
      case NODE_SUPER:
      case NODE_ZSUPER:
	if (ruby_frame->this_func == 0) return 0;
	else if (ruby_frame->this_class == 0) return 0;
	val = ruby_frame->this_class;
	if (rb_method_boundp(RCLASS(val)->super, ruby_frame->this_func, 0)) {
	    if (nd_type(node) == NODE_SUPER) {
		return arg_defined(self, node->nd_args, buf, "super");
	    }
	    return "super";
	}
	break;

      case NODE_VCALL:
      case NODE_FCALL:
	val = self;
	goto check_bound;

      case NODE_ATTRASGN:
	val = self;
	if (node->nd_recv == (NODE *)1) goto check_bound;
      case NODE_CALL:
	if (!is_defined(self, node->nd_recv, buf, Qtrue)) return 0;
	if (noeval) return ex;
	val = rb_eval(self, node->nd_recv);
      check_bound:
	{
	    int call = nd_type(node)==NODE_CALL;

	    val = CLASS_OF(val);
	    if (call) {
		int noex;
		ID id = node->nd_mid;

		if (!rb_get_method_body(&val, &id, &noex))
		    break;
		if ((noex & NOEX_PRIVATE))
		    break;
		if ((noex & NOEX_PROTECTED) &&
		    !rb_obj_is_kind_of(self, rb_class_real(val)))
		    break;
	    }
	    else if (!rb_method_boundp(val, node->nd_mid, call))
		break;
	    return arg_defined(self, node->nd_args, buf,
			       nd_type(node) == NODE_ATTRASGN ?
			       "assignment" : "method");
	}
	break;

      case NODE_MATCH2:
      case NODE_MATCH3:
	return "method";

      case NODE_YIELD:
	if (rb_block_given_p()) {
	    return "yield";
	}
	break;

      case NODE_SELF:
	return "self";

      case NODE_NIL:
	return "nil";

      case NODE_TRUE:
	return "true";

      case NODE_FALSE:
	return "false";

      case NODE_ATTRSET:
      case NODE_OP_ASGN1:
      case NODE_OP_ASGN2:
      case NODE_MASGN:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_DASGN_CURR:
      case NODE_GASGN:
      case NODE_IASGN:
      case NODE_CDECL:
      case NODE_CVDECL:
      case NODE_CVASGN:
	return "assignment";

      case NODE_LVAR:
	return "local-variable";
      case NODE_DVAR:
	return "local-variable(in-block)";

      case NODE_GVAR:
	if (rb_gvar_defined(node->nd_entry)) {
	    return "global-variable";
	}
	break;

      case NODE_IVAR:
	if (rb_ivar_defined(self, node->nd_vid)) {
	    return "instance-variable";
	}
	break;

      case NODE_CONST:
	if (ev_const_defined(ruby_cref, node->nd_vid, self)) {
	    return "constant";
	}
	break;

      case NODE_CVAR:
	if (rb_cvar_defined(cvar_cbase(), node->nd_vid)) {
	    return "class variable";
	}
	break;

      case NODE_COLON2:
	if (!is_defined(self, node->nd_recv, buf, Qtrue)) return 0;
	if (noeval) return ex;
	val = rb_eval(self, node->nd_recv);
	switch (TYPE(val)) {
	  case T_CLASS:
	  case T_MODULE:
	    if (rb_const_defined_from(val, node->nd_mid))
		return "constant";
	    break;
	  default:
	    if (rb_method_boundp(CLASS_OF(val), node->nd_mid, 1)) {
		return "method";
	    }
	}
	break;

      case NODE_COLON3:
	if (rb_const_defined_from(rb_cObject, node->nd_mid)) {
	    return "constant";
	}
	break;

      case NODE_NTH_REF:
	if (RTEST(rb_reg_nth_defined(node->nd_nth, MATCH_DATA))) {
	    if (!buf) return ex;
	    sprintf(buf, "$%d", (int)node->nd_nth);
	    return buf;
	}
	break;

      case NODE_BACK_REF:
	if (RTEST(rb_reg_nth_defined(0, MATCH_DATA))) {
	    if (!buf) return ex;
	    sprintf(buf, "$%c", (char)node->nd_nth);
	    return buf;
	}
	break;

      default:
	PUSH_TAG(PROT_NONE);
	if ((state = EXEC_TAG()) == 0) {
	    rb_eval(self, node);
	}
	POP_TAG();
	if (!state) {
	    return ex;
	}
	ruby_errinfo = Qnil;
	break;
    }
    return 0;
}

static int handle_rescue _((VALUE,NODE*));

static void blk_free();

static VALUE
rb_obj_is_proc(proc)
    VALUE proc;
{
    if (TYPE(proc) == T_DATA && RDATA(proc)->dfree == (RUBY_DATA_FUNC)blk_free) {
	return Qtrue;
    }
    return Qfalse;
}

void
rb_add_event_hook(func, events)
    rb_event_hook_func_t func;
    rb_event_t events;
{
    rb_event_hook_t *hook;

    hook = ALLOC(rb_event_hook_t);
    hook->func = func;
    hook->events = events;
    hook->next = event_hooks;
    event_hooks = hook;
}

int
rb_remove_event_hook(func)
    rb_event_hook_func_t func;
{
    rb_event_hook_t *prev, *hook;

    prev = NULL;
    hook = event_hooks;
    while (hook) {
	if (hook->func == func) {
	    if (prev) {
		prev->next = hook->next;
	    }
	    else {
		event_hooks = hook->next;
	    }
	    xfree(hook);
	    return 0;
	}
	prev = hook;
	hook = hook->next;
    }
    return -1;
}

/*
 *  call-seq:
 *     set_trace_func(proc)    => proc
 *     set_trace_func(nil)     => nil
 *  
 *  Establishes _proc_ as the handler for tracing, or disables
 *  tracing if the parameter is +nil+. _proc_ takes up
 *  to six parameters: an event name, a filename, a line number, an
 *  object id, a binding, and the name of a class. _proc_ is
 *  invoked whenever an event occurs. Events are: <code>c-call</code>
 *  (call a C-language routine), <code>c-return</code> (return from a
 *  C-language routine), <code>call</code> (call a Ruby method),
 *  <code>class</code> (start a class or module definition),
 *  <code>end</code> (finish a class or module definition),
 *  <code>line</code> (execute code on a new line), <code>raise</code>
 *  (raise an exception), and <code>return</code> (return from a Ruby
 *  method). Tracing is disabled within the context of _proc_.
 *
 *      class Test
 *	def test
 *	  a = 1
 *	  b = 2
 *	end
 *      end
 *
 *      set_trace_func proc { |event, file, line, id, binding, classname|
 *	   printf "%8s %s:%-2d %10s %8s\n", event, file, line, id, classname
 *      }
 *      t = Test.new
 *      t.test
 *
 *	  line prog.rb:11               false
 *      c-call prog.rb:11        new    Class
 *      c-call prog.rb:11 initialize   Object
 *    c-return prog.rb:11 initialize   Object
 *    c-return prog.rb:11        new    Class
 *	  line prog.rb:12               false
 *  	  call prog.rb:2        test     Test
 *	  line prog.rb:3        test     Test
 *	  line prog.rb:4        test     Test
 *      return prog.rb:4        test     Test
 */


static VALUE
set_trace_func(obj, trace)
    VALUE obj, trace;
{
    rb_event_hook_t *hook;

    if (NIL_P(trace)) {
	trace_func = 0;
	rb_remove_event_hook(call_trace_func);
	return Qnil;
    }
    if (!rb_obj_is_proc(trace)) {
	rb_raise(rb_eTypeError, "trace_func needs to be Proc");
    }
    trace_func = trace;
    for (hook = event_hooks; hook; hook = hook->next) {
	if (hook->func == call_trace_func)
	    return trace;
    }
    rb_add_event_hook(call_trace_func, RUBY_EVENT_ALL);
    return trace;
}

static char *
get_event_name(rb_event_t event)
{
    switch (event) {
    case RUBY_EVENT_LINE:
	return "line";
    case RUBY_EVENT_CLASS:
	return "class";
    case RUBY_EVENT_END:
	return "end";
    case RUBY_EVENT_CALL:
	return "call";
    case RUBY_EVENT_RETURN:
	return "return";
    case RUBY_EVENT_C_CALL:
	return "c-call";
    case RUBY_EVENT_C_RETURN:
	return "c-return";
    case RUBY_EVENT_RAISE:
	return "raise";
    default:
	return "unknown";
    }
}

static void
call_trace_func(event, node, self, id, klass)
    rb_event_t event;
    NODE *node;
    VALUE self;
    ID id;
    VALUE klass;		/* OK */
{
    int state, raised;
    struct FRAME *prev;
    NODE *node_save;
    VALUE srcfile;
    char *event_name;

    if (!trace_func) return;
    if (tracing) return;
    if (id == ID_ALLOCATOR) return;
    if (!node && ruby_sourceline == 0) return;

    if (!(node_save = ruby_current_node)) {
	node_save = NEW_BEGIN(0);
    }
    tracing = 1;
    prev = ruby_frame;
    PUSH_FRAME();
    *ruby_frame = *prev;
    ruby_frame->prev = prev;
    ruby_frame->iter = 0;	/* blocks not available anyway */

    if (node) {
	ruby_current_node = node;
	ruby_frame->node = node;
	ruby_sourcefile = node->nd_file;
	ruby_sourceline = nd_line(node);
    }
    if (klass) {
	if (TYPE(klass) == T_ICLASS) {
	    klass = RBASIC(klass)->klass;
	}
	else if (FL_TEST(klass, FL_SINGLETON)) {
	    klass = self;
	}
    }
    PUSH_TAG(PROT_NONE);
    raised = thread_reset_raised();
    if ((state = EXEC_TAG()) == 0) {
	srcfile = rb_str_new2(ruby_sourcefile?ruby_sourcefile:"(ruby)");
	event_name = get_event_name(event);
	proc_invoke(trace_func, rb_ary_new3(6, rb_str_new2(event_name),
					    srcfile,
					    INT2FIX(ruby_sourceline),
					    id?ID2SYM(id):Qnil,
					    self ? rb_f_binding(self) : Qnil,
					    klass?klass:Qnil),
		    Qundef, 0);
    }
    if (raised) thread_set_raised();
    POP_TAG();
    POP_FRAME();

    tracing = 0;
    ruby_current_node = node_save;
    SET_CURRENT_SOURCE();
    if (state) JUMP_TAG(state);
}

static VALUE
avalue_to_svalue(v)
    VALUE v;
{
    VALUE tmp, top;

    tmp = rb_check_array_type(v);
    if (NIL_P(tmp)) {
	return v;
    }
    if (RARRAY(tmp)->len == 0) {
	return Qundef;
    }
    if (RARRAY(tmp)->len == 1) {
	top = rb_check_array_type(RARRAY(tmp)->ptr[0]);
	if (NIL_P(top)) {
	    return RARRAY(tmp)->ptr[0];
	}
	if (RARRAY(top)->len > 1) {
	    return v;
	}
	return top;
    }
    return tmp;
}

static VALUE
svalue_to_avalue(v)
    VALUE v;
{
    VALUE tmp, top;

    if (v == Qundef) return rb_ary_new2(0);
    tmp = rb_check_array_type(v);
    if (NIL_P(tmp)) {
	return rb_ary_new3(1, v);
    }
    if (RARRAY(tmp)->len == 1) {
	top = rb_check_array_type(RARRAY(tmp)->ptr[0]);
	if (!NIL_P(top) && RARRAY(top)->len > 1) {
	    return tmp;
	}
	return rb_ary_new3(1, v);
    }
    return tmp;
}

static VALUE
svalue_to_mrhs(v, lhs)
    VALUE v;
    NODE *lhs;
{
    VALUE tmp;

    if (v == Qundef) return rb_values_new2(0, 0);
    tmp = rb_check_array_type(v);
    if (NIL_P(tmp)) {
	return rb_values_new(1, v);
    }
    /* no lhs means splat lhs only */
    if (!lhs) {
	return rb_values_new(1, v);
    }
    return tmp;
}

static VALUE
avalue_splat(v)
    VALUE v;
{
    if (RARRAY(v)->len == 0) {
	return Qundef;
    }
    if (RARRAY(v)->len == 1) {
	return RARRAY(v)->ptr[0];
    }
    return v;
}

static VALUE
splat_value(v)
    VALUE v;
{
    VALUE val;

    if (NIL_P(v)) val = rb_ary_new3(1, Qnil);
    else val = rb_Array(v);
    return rb_values_from_ary(val);
}

static VALUE
class_prefix(self, cpath)
    VALUE self;
    NODE *cpath;
{
    if (!cpath) {
	rb_bug("class path missing");
    }
    if (cpath->nd_head) {
	VALUE c = rb_eval(self, cpath->nd_head);
	switch (TYPE(c)) {
	  case T_CLASS:
	  case T_MODULE:
	    break;
	  default:
	    rb_raise(rb_eTypeError, "%s is not a class/module",
		     RSTRING(rb_obj_as_string(c))->ptr);
	}
	return c;
    }
    else if (nd_type(cpath) == NODE_COLON2) {
	return ruby_cbase;
    }
    else if (ruby_wrapper) {
	return ruby_wrapper;
    }
    else {
	return rb_cObject;
    }
}

#define return_value(v) do {\
  if ((prot_tag->retval = (v)) == Qundef) {\
    prot_tag->retval = Qnil;\
  }\
} while (0)

NORETURN(static void return_jump _((VALUE)));
NORETURN(static void break_jump _((VALUE)));

static VALUE
rb_eval(self, n)
    VALUE self;
    NODE *n;
{
    NODE * volatile contnode = 0;
    NODE * volatile node = n;
    int state;
    volatile VALUE result = Qnil;

#define RETURN(v) do { \
    result = (v); \
    goto finish; \
} while (0)

  again:
    if (!node) RETURN(Qnil);

    ruby_current_node = node;
    if (node->flags & NODE_NEWLINE) {
	EXEC_EVENT_HOOK(RUBY_EVENT_LINE, node, self, 
			ruby_frame->this_func,
			ruby_frame->this_class);
    }
    switch (nd_type(node)) {
      case NODE_BLOCK:
	if (contnode) {
	    result = rb_eval(self, node);
	    break;
	}
	contnode = node->nd_next;
	node = node->nd_head;
	goto again;

      case NODE_POSTEXE:
	rb_f_END();
	nd_set_type(node, NODE_NIL); /* exec just once */
	result = Qnil;
	break;

	/* begin .. end without clauses */
      case NODE_BEGIN:
	node = node->nd_body;
	goto again;

	/* nodes for speed-up(default match) */
      case NODE_MATCH:
	result = rb_reg_match2(node->nd_lit);
	break;

	/* nodes for speed-up(literal match) */
      case NODE_MATCH2:
	{
	    VALUE l = rb_eval(self,node->nd_recv);
	    VALUE r = rb_eval(self,node->nd_value);
	    result = rb_reg_match(l, r);
	}
	break;

	/* nodes for speed-up(literal match) */
      case NODE_MATCH3:
	{
	    VALUE r = rb_eval(self,node->nd_recv);
	    VALUE l = rb_eval(self,node->nd_value);
	    if (TYPE(l) == T_STRING) {
		result = rb_reg_match(r, l);
	    }
	    else {
		result = rb_funcall(l, match, 1, r);
	    }
	}
	break;

	/* node for speed-up(top-level loop for -n/-p) */
      case NODE_OPT_N:
	PUSH_TAG(PROT_LOOP);
	switch (state = EXEC_TAG()) {
	  case 0:
	  opt_n_next:
	    while (!NIL_P(rb_gets())) {
	      opt_n_redo:
		rb_eval(self, node->nd_body);
	    }
	    break;

	  case TAG_REDO:
	    state = 0;
	    goto opt_n_redo;
	  case TAG_NEXT:
	    state = 0;
	    goto opt_n_next;
	  case TAG_BREAK:
	    state = 0;
	  default:
	    break;
	}
	POP_TAG();
	if (state) JUMP_TAG(state);
	RETURN(Qnil);

      case NODE_SELF:
	RETURN(self);

      case NODE_NIL:
	RETURN(Qnil);

      case NODE_TRUE:
	RETURN(Qtrue);

      case NODE_FALSE:
	RETURN(Qfalse);

      case NODE_ERRINFO:
	RETURN(ruby_errinfo);

      case NODE_IF:
	EXEC_EVENT_HOOK(RUBY_EVENT_LINE, node, self,
			ruby_frame->this_func,
			ruby_frame->this_class);
	if (RTEST(rb_eval(self, node->nd_cond))) {
	    node = node->nd_body;
	}
	else {
	    node = node->nd_else;
	}
	goto again;

      case NODE_WHEN:
	while (node) {
	    NODE *tag;

	    if (nd_type(node) != NODE_WHEN) goto again;
	    tag = node->nd_head;
	    while (tag) {
		EXEC_EVENT_HOOK(RUBY_EVENT_LINE, tag, self,
				ruby_frame->this_func,
				ruby_frame->this_class);
		if (tag->nd_head && nd_type(tag->nd_head) == NODE_WHEN) {
		    VALUE v = rb_eval(self, tag->nd_head->nd_head);
		    long i;

		    if (TYPE(v) != T_ARRAY) v = rb_ary_to_ary(v);
		    for (i=0; i<RARRAY(v)->len; i++) {
			if (RTEST(RARRAY(v)->ptr[i])) {
			    node = node->nd_body;
			    goto again;
			}
		    }
		    tag = tag->nd_next;
		    continue;
		}
		if (RTEST(rb_eval(self, tag->nd_head))) {
		    node = node->nd_body;
		    goto again;
		}
		tag = tag->nd_next;
	    }
	    node = node->nd_next;
	}
	RETURN(Qnil);

      case NODE_CASE:
	{
	    VALUE val;

	    val = rb_eval(self, node->nd_head);
	    node = node->nd_body;
	    while (node) {
		NODE *tag;

		if (nd_type(node) != NODE_WHEN) {
		    goto again;
		}
		tag = node->nd_head;
		while (tag) {
		    EXEC_EVENT_HOOK(RUBY_EVENT_LINE, tag, self,
				    ruby_frame->this_func,
				    ruby_frame->this_class);
		    if (tag->nd_head && nd_type(tag->nd_head) == NODE_WHEN) {
			VALUE v = rb_eval(self, tag->nd_head->nd_head);
			long i;

			if (TYPE(v) != T_ARRAY) v = rb_ary_to_ary(v);
			for (i=0; i<RARRAY(v)->len; i++) {
			    if (RTEST(rb_funcall2(RARRAY(v)->ptr[i], eqq, 1, &val))){
				node = node->nd_body;
				goto again;
			    }
			}
			tag = tag->nd_next;
			continue;
		    }
		    if (RTEST(rb_funcall2(rb_eval(self, tag->nd_head), eqq, 1, &val))) {
			node = node->nd_body;
			goto again;
		    }
		    tag = tag->nd_next;
		}
		node = node->nd_next;
	    }
	}
	RETURN(Qnil);

      case NODE_WHILE:
	PUSH_TAG(PROT_LOOP);
	result = Qnil;
	switch (state = EXEC_TAG()) {
	  case 0:
	    if (node->nd_state && !RTEST(rb_eval(self, node->nd_cond)))
		goto while_out;
	    do {
	      while_redo:
		rb_eval(self, node->nd_body);
	      while_next:
		;
	    } while (RTEST(rb_eval(self, node->nd_cond)));
	    break;

	  case TAG_REDO:
	    state = 0;
	    goto while_redo;
	  case TAG_NEXT:
	    state = 0;
	    goto while_next;
	  case TAG_BREAK:
	    if (TAG_DST()) {
		state = 0;
		result = prot_tag->retval;
	    }
	    /* fall through */
	  default:
	    break;
	}
      while_out:
	POP_TAG();
	if (state) JUMP_TAG(state);
	RETURN(result);

      case NODE_UNTIL:
	PUSH_TAG(PROT_LOOP);
	result = Qnil;
	switch (state = EXEC_TAG()) {
	  case 0:
	    if (node->nd_state && RTEST(rb_eval(self, node->nd_cond)))
		goto until_out;
	    do {
	      until_redo:
		rb_eval(self, node->nd_body);
	      until_next:
		;
	    } while (!RTEST(rb_eval(self, node->nd_cond)));
	    break;

	  case TAG_REDO:
	    state = 0;
	    goto until_redo;
	  case TAG_NEXT:
	    state = 0;
	    goto until_next;
	  case TAG_BREAK:
	    if (TAG_DST()) {
		state = 0;
		result = prot_tag->retval;
	    }
	    /* fall through */
	  default:
	    break;
	}
      until_out:
	POP_TAG();
	if (state) JUMP_TAG(state);
	RETURN(result);

      case NODE_BLOCK_PASS:
	result = block_pass(self, node);
	break;

      case NODE_ITER:
      case NODE_FOR:
      case NODE_LAMBDA:
	{
	    PUSH_TAG(PROT_LOOP);
	    PUSH_BLOCK(node->nd_var, node->nd_body);

	    state = EXEC_TAG();
	    if (state == 0) {
	      iter_retry:
		PUSH_ITER(ITER_PRE);
		if (nd_type(node) == NODE_ITER) {
		    result = rb_eval(self, node->nd_iter);
		}
		else if (nd_type(node) == NODE_LAMBDA) {
		    ruby_iter->iter = ruby_frame->iter = ITER_CUR;
		    result = rb_block_proc();
		}
		else {
		    VALUE recv;

		    _block.flags &= ~BLOCK_D_SCOPE;
		    BEGIN_CALLARGS;
		    recv = rb_eval(self, node->nd_iter);
		    END_CALLARGS;
		    ruby_current_node = node;
		    SET_CURRENT_SOURCE();
		    result = rb_call(CLASS_OF(recv),recv,each,0,0,0);
		}
		POP_ITER();
	    }
	    else if (state == TAG_BREAK && TAG_DST()) {
		result = prot_tag->retval;
		state = 0;
	    }
	    else if (state == TAG_RETRY && ruby_block == &_block) {
		state = 0;
		goto iter_retry;
	    }
	    POP_BLOCK();
	    POP_TAG();
	    switch (state) {
	      case 0:
		break;
	      default:
		JUMP_TAG(state);
	    }
	}
	break;

      case NODE_BREAK:
	break_jump(rb_eval(self, node->nd_stts));
	break;

      case NODE_NEXT:
	CHECK_INTS;
	return_value(rb_eval(self, node->nd_stts));
	JUMP_TAG(TAG_NEXT);
	break;

      case NODE_REDO:
	CHECK_INTS;
	JUMP_TAG(TAG_REDO);
	break;

      case NODE_RETRY:
	CHECK_INTS;
	JUMP_TAG(TAG_RETRY);
	break;

      case NODE_SPLAT:
	result = splat_value(rb_eval(self, node->nd_head));
	break;

      case NODE_TO_ARY:
	result = rb_ary_to_ary(rb_eval(self, node->nd_head));
	break;

      case NODE_SVALUE:
	result = avalue_splat(rb_eval(self, node->nd_head));
	if (result == Qundef) result = Qnil;
	break;

      case NODE_YIELD:
	if (node->nd_head) {
	    result = rb_eval(self, node->nd_head);
	    ruby_current_node = node;
	}
	else {
	    result = Qundef;	/* no arg */
	}
	SET_CURRENT_SOURCE();
	result = rb_yield_0(result, 0, 0, 0, node->nd_state);
	break;

      case NODE_RESCUE:
	{
	    volatile VALUE e_info = ruby_errinfo;
	    volatile int rescuing = 0;

	    PUSH_TAG(PROT_NONE);
	    if ((state = EXEC_TAG()) == 0) {
	      retry_entry:
		result = rb_eval(self, node->nd_head);
	    }
	    else if (rescuing) {
		if (rescuing < 0) {
		    /* in rescue argument, just reraise */
		}
		else if (state == TAG_RETRY) {
		    rescuing = state = 0;
		    ruby_errinfo = e_info;
		    goto retry_entry;
		}
		else if (state != TAG_RAISE) {
		    result = prot_tag->retval;
		}
	    }
	    else if (state == TAG_RAISE) {
		NODE *resq = node->nd_resq;

		rescuing = -1;
		while (resq) {
		    ruby_current_node = resq;
		    if (handle_rescue(self, resq)) {
			state = 0;
			rescuing = 1;
			result = rb_eval(self, resq->nd_body);
			break;
		    }
		    resq = resq->nd_head; /* next rescue */
		}
	    }
	    else {
		result = prot_tag->retval;
	    }
	    POP_TAG();
	    if (state != TAG_RAISE) ruby_errinfo = e_info;
	    if (state) {
		if (state == TAG_NEXT) prot_tag->retval = result;
		JUMP_TAG(state);
	    }
	    /* no exception raised */
	    if (!rescuing && (node = node->nd_else)) { /* else clause given */
		goto again;
	    }
	}
	break;

      case NODE_ENSURE:
	PUSH_TAG(PROT_NONE);
	if ((state = EXEC_TAG()) == 0) {
	    result = rb_eval(self, node->nd_head);
	}
	POP_TAG();
	if (node->nd_ensr) {
	    VALUE retval = prot_tag->retval; /* save retval */
	    VALUE errinfo = ruby_errinfo;

	    rb_eval(self, node->nd_ensr);
	    return_value(retval);
	    ruby_errinfo = errinfo;
	}
	if (state) JUMP_TAG(state);
	break;

      case NODE_AND:
	result = rb_eval(self, node->nd_1st);
	if (!RTEST(result)) break;
	node = node->nd_2nd;
	goto again;

      case NODE_OR:
	result = rb_eval(self, node->nd_1st);
	if (RTEST(result)) break;
	node = node->nd_2nd;
	goto again;

      case NODE_NOT:
	if (RTEST(rb_eval(self, node->nd_body))) result = Qfalse;
	else result = Qtrue;
	break;

      case NODE_DOT2:
      case NODE_DOT3:
	result = rb_range_new(rb_eval(self, node->nd_beg),
			      rb_eval(self, node->nd_end),
			      nd_type(node) == NODE_DOT3);
	break;

      case NODE_FLIP2:		/* like AWK */
	{
	    VALUE *flip = rb_svar(node->nd_cnt);
	    if (!flip) rb_bug("unexpected local variable");
	    if (!RTEST(*flip)) {
		if (RTEST(rb_eval(self, node->nd_beg))) {
		    *flip = RTEST(rb_eval(self, node->nd_end))?Qfalse:Qtrue;
		    result = Qtrue;
		}
		else {
		    result = Qfalse;
		}
	    }
	    else {
		if (RTEST(rb_eval(self, node->nd_end))) {
		    *flip = Qfalse;
		}
		result = Qtrue;
	    }
	}
	break;

      case NODE_FLIP3:		/* like SED */
	{
	    VALUE *flip = rb_svar(node->nd_cnt);
	    if (!flip) rb_bug("unexpected local variable");
	    if (!RTEST(*flip)) {
		result = RTEST(rb_eval(self, node->nd_beg)) ? Qtrue : Qfalse;
		*flip = result;
	    }
	    else {
		if (RTEST(rb_eval(self, node->nd_end))) {
		    *flip = Qfalse;
		}
		result = Qtrue;
	    }
	}
	break;

      case NODE_RETURN:
	return_jump(rb_eval(self, node->nd_stts));
	break;

      case NODE_ARGSCAT:
	{
	    VALUE args = rb_eval(self, node->nd_head);
	    result = rb_ary_concat(args, splat_value(rb_eval(self, node->nd_body)));
	}
	break;

      case NODE_ARGSPUSH:
	{
	    VALUE args = rb_ary_dup(rb_eval(self, node->nd_head));
	    result = rb_ary_push(args, rb_eval(self, node->nd_body));
	}
	break;

      case NODE_ATTRASGN:
	{
	    VALUE recv;
	    int argc; VALUE *argv; /* used in SETUP_ARGS */
	    int scope;
	    TMP_PROTECT;

	    BEGIN_CALLARGS;
	    if (node->nd_recv == (NODE *)1) {
		recv = self;
		scope = 1;
	    }
	    else {
		recv = rb_eval(self, node->nd_recv);
		scope = 0;
	    }
	    SETUP_ARGS(node->nd_args);
	    END_CALLARGS;

	    ruby_current_node = node;
	    SET_CURRENT_SOURCE();
	    rb_call(CLASS_OF(recv),recv,node->nd_mid,argc,argv,scope);
	    result = argv[argc-1];
	}
	break;

      case NODE_CALL:
	{
	    VALUE recv;
	    int argc; VALUE *argv; /* used in SETUP_ARGS */
	    TMP_PROTECT;

	    BEGIN_CALLARGS;
	    recv = rb_eval(self, node->nd_recv);
	    SETUP_ARGS(node->nd_args);
	    END_CALLARGS;

	    ruby_current_node = node;
	    SET_CURRENT_SOURCE();
	    result = rb_call(CLASS_OF(recv),recv,node->nd_mid,argc,argv,0);
	}
	break;

      case NODE_FCALL:
	{
	    int argc; VALUE *argv; /* used in SETUP_ARGS */
	    TMP_PROTECT;

	    BEGIN_CALLARGS;
	    SETUP_ARGS(node->nd_args);
	    END_CALLARGS;

	    ruby_current_node = node;
	    SET_CURRENT_SOURCE();
	    result = rb_call(CLASS_OF(self),self,node->nd_mid,argc,argv,1);
	}
	break;

      case NODE_VCALL:
	SET_CURRENT_SOURCE();
	result = rb_call(CLASS_OF(self),self,node->nd_mid,0,0,2);
	break;

      case NODE_SUPER:
      case NODE_ZSUPER:
	{
	    int argc; VALUE *argv; /* used in SETUP_ARGS */
	    TMP_PROTECT;

	    if (ruby_frame->this_class == 0) {
		if (ruby_frame->this_func) {
		    rb_name_error(ruby_frame->callee,
				  "superclass method `%s' disabled",
				  rb_id2name(ruby_frame->this_func));
		}
		else {
		    rb_raise(rb_eNoMethodError, "super called outside of method");
		}
	    }
	    if (nd_type(node) == NODE_ZSUPER) {
		argc = ruby_frame->argc;
		if (argc && ruby_frame->prev &&
                    (ruby_frame->prev->flags & FRAME_DMETH)) {
                    if (TYPE(RBASIC(ruby_scope)->klass) != T_ARRAY ||
                        RARRAY(RBASIC(ruby_scope)->klass)->len != argc) {
                        rb_raise(rb_eRuntimeError, 
                                 "super: specify arguments explicitly");
                    }
                    argv = RARRAY(RBASIC(ruby_scope)->klass)->ptr;
                }
                else {
                    argv = ruby_scope->local_vars + 2;
                }
	    }
	    else {
		BEGIN_CALLARGS;
		SETUP_ARGS(node->nd_args);
		END_CALLARGS;
		ruby_current_node = node;
	    }

	    SET_CURRENT_SOURCE();
	    result = rb_call_super(argc, argv);
	}
	break;

      case NODE_SCOPE:
	{
	    struct FRAME frame;
	    NODE *saved_cref = 0;

	    frame = *ruby_frame;
	    frame.tmp = ruby_frame;
	    ruby_frame = &frame;

	    PUSH_SCOPE();
	    PUSH_TAG(PROT_NONE);
	    if (node->nd_rval) {
		saved_cref = ruby_cref;
		ruby_cref = (NODE*)node->nd_rval;
	    }
	    if (node->nd_tbl) {
		VALUE *vars = ALLOCA_N(VALUE, node->nd_tbl[0]+1);
		*vars++ = (VALUE)node;
		ruby_scope->local_vars = vars;
		rb_mem_clear(ruby_scope->local_vars, node->nd_tbl[0]);
		ruby_scope->local_tbl = node->nd_tbl;
	    }
	    else {
		ruby_scope->local_vars = 0;
		ruby_scope->local_tbl  = 0;
	    }
	    if ((state = EXEC_TAG()) == 0) {
		result = rb_eval(self, node->nd_next);
	    }
	    POP_TAG();
	    POP_SCOPE();
	    ruby_frame = frame.tmp;
	    if (saved_cref)
		ruby_cref = saved_cref;
	    if (state) JUMP_TAG(state);
	}
	break;

      case NODE_OP_ASGN1:
	{
	    int argc; VALUE *argv; /* used in SETUP_ARGS */
	    VALUE recv, val;
	    NODE *rval;
	    TMP_PROTECT;

	    recv = rb_eval(self, node->nd_recv);
	    rval = node->nd_args->nd_head;
	    SETUP_ARGS0(node->nd_args->nd_next, node->nd_args->nd_alen - 1);
	    val = rb_funcall2(recv, aref, argc-1, argv);
	    switch (node->nd_mid) {
	    case 0: /* OR */
		if (RTEST(val)) RETURN(val);
		val = rb_eval(self, rval);
		break;
	    case 1: /* AND */
		if (!RTEST(val)) RETURN(val);
		val = rb_eval(self, rval);
		break;
	    default:
		val = rb_funcall(val, node->nd_mid, 1, rb_eval(self, rval));
	    }
	    argv[argc-1] = val;
	    rb_funcall2(recv, aset, argc, argv);
	    result = val;
	}
	break;

      case NODE_OP_ASGN2:
	{
	    ID id = node->nd_next->nd_vid;
	    VALUE recv, val;

	    recv = rb_eval(self, node->nd_recv);
	    val = rb_funcall(recv, id, 0);
	    switch (node->nd_next->nd_mid) {
	    case 0: /* OR */
		if (RTEST(val)) RETURN(val);
		val = rb_eval(self, node->nd_value);
		break;
	    case 1: /* AND */
		if (!RTEST(val)) RETURN(val);
		val = rb_eval(self, node->nd_value);
		break;
	    default:
		val = rb_funcall(val, node->nd_next->nd_mid, 1,
				 rb_eval(self, node->nd_value));
	    }

	    rb_funcall2(recv, node->nd_next->nd_aid, 1, &val);
	    result = val;
	}
	break;

      case NODE_OP_ASGN_AND:
	result = rb_eval(self, node->nd_head);
	if (!RTEST(result)) break;
	node = node->nd_value;
	goto again;

      case NODE_OP_ASGN_OR:
	if ((node->nd_aid && !is_defined(self, node->nd_head, 0, 0)) ||
	    !RTEST(result = rb_eval(self, node->nd_head))) {
	    node = node->nd_value;
	    goto again;
	}
	break;

      case NODE_MASGN:
	result = massign(self, node, rb_eval(self, node->nd_value), 0);
	break;

      case NODE_LASGN:
	if (ruby_scope->local_vars == 0)
	    rb_bug("unexpected local variable assignment");
	result = rb_eval(self, node->nd_value);
	ruby_scope->local_vars[node->nd_cnt] = result;
	break;

      case NODE_DASGN:
	result = rb_eval(self, node->nd_value);
	dvar_asgn(node->nd_vid, result);
	break;

      case NODE_DASGN_CURR:
	result = rb_eval(self, node->nd_value);
	dvar_asgn_curr(node->nd_vid, result);
	break;

      case NODE_GASGN:
	result = rb_eval(self, node->nd_value);
	rb_gvar_set(node->nd_entry, result);
	break;

      case NODE_IASGN:
	result = rb_eval(self, node->nd_value);
	rb_ivar_set(self, node->nd_vid, result);
	break;

      case NODE_CDECL:
	result = rb_eval(self, node->nd_value);
	if (node->nd_vid == 0) {
	    rb_const_set(class_prefix(self, node->nd_else), node->nd_else->nd_mid, result);
	}
	else {
	    if (NIL_P(ruby_cbase)) {
		rb_raise(rb_eTypeError, "no class/module to define constant");
	    }
	    rb_const_set(ruby_cbase, node->nd_vid, result);
	}
	break;

      case NODE_CVDECL:
	if (NIL_P(ruby_cbase)) {
	    rb_raise(rb_eTypeError, "no class/module to define class variable");
	}
	result = rb_eval(self, node->nd_value);
	rb_cvar_set(cvar_cbase(), node->nd_vid, result, Qtrue);
	break;

      case NODE_CVASGN:
	result = rb_eval(self, node->nd_value);
	rb_cvar_set(cvar_cbase(), node->nd_vid, result, Qfalse);
	break;

      case NODE_LVAR:
	if (ruby_scope->local_vars == 0) {
	    rb_bug("unexpected local variable");
	}
	result = ruby_scope->local_vars[node->nd_cnt];
	break;

      case NODE_DVAR:
	result = rb_dvar_ref(node->nd_vid);
	break;

      case NODE_GVAR:
	result = rb_gvar_get(node->nd_entry);
	break;

      case NODE_IVAR:
	result = rb_ivar_get(self, node->nd_vid);
	break;

      case NODE_CONST:
	result = ev_const_get(ruby_cref, node->nd_vid, self);
	break;

      case NODE_CVAR:
	result = rb_cvar_get(cvar_cbase(), node->nd_vid);
	break;

      case NODE_BLOCK_ARG:
	if (ruby_scope->local_vars == 0)
	    rb_bug("unexpected block argument");
	if (rb_block_given_p()) {
	    result = rb_block_proc();
	    ruby_scope->local_vars[node->nd_cnt] = result;
	}
	else {
	    result = Qnil;
	}
	break;

      case NODE_COLON2:
	{
	    VALUE klass;

	    klass = rb_eval(self, node->nd_head);
	    if (rb_is_const_id(node->nd_mid)) {
		switch (TYPE(klass)) {
		  case T_CLASS:
		  case T_MODULE:
		    result = rb_const_get_from(klass, node->nd_mid);
		    break;
		  default:
		    rb_raise(rb_eTypeError, "%s is not a class/module",
			     RSTRING(rb_obj_as_string(klass))->ptr);
		    break;
		}
	    }
	    else {
		result = rb_funcall(klass, node->nd_mid, 0, 0);
	    }
	}
	break;

      case NODE_COLON3:
	result = rb_const_get_from(rb_cObject, node->nd_mid);
	break;

      case NODE_NTH_REF:
	result = rb_reg_nth_match(node->nd_nth, MATCH_DATA);
	break;

      case NODE_BACK_REF:
	switch (node->nd_nth) {
	  case '&':
	    result = rb_reg_last_match(MATCH_DATA);
	    break;
	  case '`':
	    result = rb_reg_match_pre(MATCH_DATA);
	    break;
	  case '\'':
	    result = rb_reg_match_post(MATCH_DATA);
	    break;
	  case '+':
	    result = rb_reg_match_last(MATCH_DATA);
	    break;
	  default:
	    rb_bug("unexpected back-ref");
	}
	break;

      case NODE_HASH:
	{
	    NODE *list;
	    VALUE hash = rb_hash_new();
	    VALUE key, val;

	    list = node->nd_head;
	    while (list) {
		key = rb_eval(self, list->nd_head);
		list = list->nd_next;
		if (list == 0)
		    rb_bug("odd number list for Hash");
		val = rb_eval(self, list->nd_head);
		list = list->nd_next;
		rb_hash_aset(hash, key, val);
	    }
	    result = hash;
	}
	break;

      case NODE_ZARRAY:		/* zero length list */
	result = rb_ary_new();
	break;

      case NODE_ARRAY:
	{
	    VALUE ary;
	    long i;

	    i = node->nd_alen;
	    ary = rb_ary_new2(i);
	    for (i=0;node;node=node->nd_next) {
		RARRAY(ary)->ptr[i++] = rb_eval(self, node->nd_head);
		RARRAY(ary)->len = i;
	    }

	    result = ary;
	}
	break;

      case NODE_VALUES:
	{
	    VALUE val;
	    long i;

	    i = node->nd_alen;
	    val = rb_values_new2(i, 0);
	    for (i=0;node;node=node->nd_next) {
		RARRAY(val)->ptr[i++] = rb_eval(self, node->nd_head);
		RARRAY(val)->len = i;
	    }

	    result = val;
	}
	break;

      case NODE_STR:
	result = rb_str_new3(node->nd_lit);
	break;

      case NODE_EVSTR:
	result = rb_obj_as_string(rb_eval(self, node->nd_body));
	break;

      case NODE_DSTR:
      case NODE_DXSTR:
      case NODE_DREGX:
      case NODE_DREGX_ONCE:
      case NODE_DSYM:
	{
	    VALUE str, str2;
	    NODE *list = node->nd_next;

	    str = rb_str_new3(node->nd_lit);
	    while (list) {
		if (list->nd_head) {
		    switch (nd_type(list->nd_head)) {
		      case NODE_STR:
			str2 = list->nd_head->nd_lit;
			break;
		      default:
			str2 = rb_eval(self, list->nd_head);
			break;
		    }
		    rb_str_append(str, str2);
		    OBJ_INFECT(str, str2);
		}
		list = list->nd_next;
	    }
	    switch (nd_type(node)) {
	      case NODE_DREGX:
		result = rb_reg_new(RSTRING(str)->ptr, RSTRING(str)->len,
				    node->nd_cflag);
		break;
	      case NODE_DREGX_ONCE:	/* regexp expand once */
		result = rb_reg_new(RSTRING(str)->ptr, RSTRING(str)->len,
				    node->nd_cflag);
		nd_set_type(node, NODE_LIT);
		node->nd_lit = result;
		break;
	      case NODE_LIT:
		/* other thread may replace NODE_DREGX_ONCE to NODE_LIT */
		goto again;
	      case NODE_DXSTR:
		result = rb_funcall(self, '`', 1, str);
		break;
	      case NODE_DSYM:
		result = rb_str_intern(str);
		break;
	      default:
		result = str;
		break;
	    }
	}
	break;

      case NODE_XSTR:
	result = rb_funcall(self, '`', 1, rb_str_new3(node->nd_lit));
	break;

      case NODE_LIT:
	result = node->nd_lit;
	break;

      case NODE_DEFN:
	if (node->nd_defn) {
	    NODE *body,  *defn;
	    VALUE origin;
	    int noex;

	    if (NIL_P(ruby_class)) {
		rb_raise(rb_eTypeError, "no class/module to add method");
	    }
	    if (ruby_class == rb_cObject && node->nd_mid == init) {
		rb_warn("redefining Object#initialize may cause infinite loop");
	    }
	    if (node->nd_mid == __id__ || node->nd_mid == __send__) {
		rb_warn("redefining `%s' may cause serious problem",
			rb_id2name(node->nd_mid));
	    }
	    rb_frozen_class_p(ruby_class);
	    body = search_method(ruby_class, node->nd_mid, &origin);
	    if (body){
		if (RTEST(ruby_verbose) && ruby_class == origin && body->nd_cnt == 0 && body->nd_body) {
		    rb_warning("method redefined; discarding old %s", rb_id2name(node->nd_mid));
		}
	    }

	    if (SCOPE_TEST(SCOPE_PRIVATE) || node->nd_mid == init) {
		noex = NOEX_PRIVATE;
	    }
	    else if (SCOPE_TEST(SCOPE_PROTECTED)) {
		noex = NOEX_PROTECTED;
	    }
	    else {
		noex = NOEX_PUBLIC;
	    }
	    if (body && origin == ruby_class && body->nd_body == 0) {
		noex |= NOEX_NOSUPER;
	    }

	    defn = copy_node_scope(node->nd_defn, ruby_cref);
	    rb_add_method(ruby_class, node->nd_mid, defn, noex);
	    if (scope_vmode == SCOPE_MODFUNC) {
		rb_add_method(rb_singleton_class(ruby_class),
			      node->nd_mid, defn, NOEX_PUBLIC);
	    }
	    result = Qnil;
	}
	break;

      case NODE_DEFS:
	if (node->nd_defn) {
	    VALUE recv = rb_eval(self, node->nd_recv);
	    VALUE klass;
	    NODE *body = 0, *defn;

	    if (ruby_safe_level >= 4 && !OBJ_TAINTED(recv)) {
		rb_raise(rb_eSecurityError, "Insecure: can't define singleton method");
	    }
	    if (FIXNUM_P(recv) || SYMBOL_P(recv)) {
		rb_raise(rb_eTypeError,
			 "can't define singleton method \"%s\" for %s",
			 rb_id2name(node->nd_mid),
			 rb_obj_classname(recv));
	    }

	    if (OBJ_FROZEN(recv)) rb_error_frozen("object");
	    klass = rb_singleton_class(recv);
	    if (st_lookup(RCLASS(klass)->m_tbl, node->nd_mid, (st_data_t *)&body)) {
		if (ruby_safe_level >= 4) {
		    rb_raise(rb_eSecurityError, "redefining method prohibited");
		}
		if (RTEST(ruby_verbose)) {
		    rb_warning("redefine %s", rb_id2name(node->nd_mid));
		}
	    }
	    defn = copy_node_scope(node->nd_defn, ruby_cref);
	    rb_add_method(klass, node->nd_mid, defn,
			  NOEX_PUBLIC|(body?body->nd_noex&NOEX_UNDEF:0));
	    result = Qnil;
	}
	break;

      case NODE_UNDEF:
	if (NIL_P(ruby_class)) {
	    rb_raise(rb_eTypeError, "no class to undef method");
	}
	rb_undef(ruby_class, rb_to_id(rb_eval(self, node->u2.node)));
	result = Qnil;
	break;

      case NODE_ALIAS:
	if (NIL_P(ruby_class)) {
	    rb_raise(rb_eTypeError, "no class to make alias");
	}
	rb_alias(ruby_class, rb_to_id(rb_eval(self, node->u1.node)),
		             rb_to_id(rb_eval(self, node->u2.node)));
	result = Qnil;
	break;

      case NODE_VALIAS:
	rb_alias_variable(node->u1.id, node->u2.id);
	result = Qnil;
	break;

      case NODE_CLASS:
	{
	    VALUE super, klass, tmp, cbase;
	    ID cname;
	    int gen = Qfalse;

	    cbase = class_prefix(self, node->nd_cpath);
	    cname = node->nd_cpath->nd_mid;

	    if (NIL_P(ruby_cbase)) {
		rb_raise(rb_eTypeError, "no outer class/module");
	    }
	    if (node->nd_super) {
	       super = rb_eval(self, node->nd_super);
	       rb_check_inheritable(super);
	    }
	    else {
		super = 0;
	    }

	    if (rb_const_defined_at(cbase, cname)) {
		klass = rb_const_get_at(cbase, cname);
		if (TYPE(klass) != T_CLASS) {
		    rb_raise(rb_eTypeError, "%s is not a class",
			     rb_id2name(cname));
		}
		if (super) {
		    tmp = rb_class_real(RCLASS(klass)->super);
		    if (tmp != super) {
			rb_raise(rb_eTypeError, "superclass mismatch for class %s",
				 rb_id2name(cname));
		    }
		    super = 0;
		}
		if (ruby_safe_level >= 4) {
		    rb_raise(rb_eSecurityError, "extending class prohibited");
		}
	    }
	    else {
		if (!super) super = rb_cObject;
		klass = rb_define_class_id(cname, super);
		rb_set_class_path(klass, cbase, rb_id2name(cname));
		rb_const_set(cbase, cname, klass);
		gen = Qtrue;
	    }
	    if (ruby_wrapper) {
		rb_extend_object(klass, ruby_wrapper);
		rb_include_module(klass, ruby_wrapper);
	    }
	    if (super && gen) {
		rb_class_inherited(super, klass);
	    }
	    result = module_setup(klass, node);
	}
	break;

      case NODE_MODULE:
	{
	    VALUE module, cbase;
	    ID cname;

	    if (NIL_P(ruby_cbase)) {
		rb_raise(rb_eTypeError, "no outer class/module");
	    }
	    cbase = class_prefix(self, node->nd_cpath);
	    cname = node->nd_cpath->nd_mid;
	    if (rb_const_defined_at(cbase, cname)) {
		module = rb_const_get_at(cbase, cname);
		if (TYPE(module) != T_MODULE) {
		    rb_raise(rb_eTypeError, "%s is not a module",
			     rb_id2name(cname));
		}
		if (ruby_safe_level >= 4) {
		    rb_raise(rb_eSecurityError, "extending module prohibited");
		}
	    }
	    else {
		module = rb_define_module_id(cname);
		rb_set_class_path(module, cbase, rb_id2name(cname));
		rb_const_set(cbase, cname, module);
	    }
	    if (ruby_wrapper) {
		rb_extend_object(module, ruby_wrapper);
		rb_include_module(module, ruby_wrapper);
	    }

	    result = module_setup(module, node);
	}
	break;

      case NODE_SCLASS:
	{
	    VALUE klass;

	    result = rb_eval(self, node->nd_recv);
	    if (FIXNUM_P(result) || SYMBOL_P(result)) {
		rb_raise(rb_eTypeError, "no singleton class for %s",
			 rb_obj_classname(result));
	    }
	    if (ruby_safe_level >= 4 && !OBJ_TAINTED(result))
		rb_raise(rb_eSecurityError, "Insecure: can't extend object");
	    klass = rb_singleton_class(result);

	    if (ruby_wrapper) {
		rb_extend_object(klass, ruby_wrapper);
		rb_include_module(klass, ruby_wrapper);
	    }

	    result = module_setup(klass, node);
	}
	break;

      case NODE_DEFINED:
	{
	    char buf[20];
	    const char *desc = is_defined(self, node->nd_head, buf, 0);

	    if (desc) result = rb_str_new2(desc);
	    else result = Qnil;
	}
	break;

      default:
	rb_bug("unknown node type %d", nd_type(node));
    }
  finish:
    CHECK_INTS;
    if (contnode) {
	node = contnode;
	contnode = 0;
	goto again;
    }
    return result;
}

static VALUE
module_setup(module, n)
    VALUE module;
    NODE *n;
{
    NODE * volatile node = n->nd_body;
    int state;
    struct FRAME frame;
    VALUE result = Qnil;	/* OK */
    TMP_PROTECT;

    frame = *ruby_frame;
    frame.tmp = ruby_frame;
    ruby_frame = &frame;

    PUSH_CLASS(module);
    PUSH_SCOPE();
    PUSH_VARS();

    if (node->nd_tbl) {
	VALUE *vars = TMP_ALLOC(node->nd_tbl[0]+1);
	*vars++ = (VALUE)node;
	ruby_scope->local_vars = vars;
	rb_mem_clear(ruby_scope->local_vars, node->nd_tbl[0]);
	ruby_scope->local_tbl = node->nd_tbl;
    }
    else {
	ruby_scope->local_vars = 0;
	ruby_scope->local_tbl  = 0;
    }

    PUSH_CREF(module);
    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	EXEC_EVENT_HOOK(RUBY_EVENT_CLASS, n, ruby_cbase,
			ruby_frame->this_func, ruby_frame->this_class);
	result = rb_eval(ruby_cbase, node->nd_next);
    }
    POP_TAG();
    POP_CREF();
    POP_VARS();
    POP_SCOPE();
    POP_CLASS();

    ruby_frame = frame.tmp;
    EXEC_EVENT_HOOK(RUBY_EVENT_END, n, 0, ruby_frame->this_func,
		    ruby_frame->this_class);
    if (state) JUMP_TAG(state);

    return result;
}

static NODE *basic_respond_to = 0;

int
rb_respond_to(obj, id)
    VALUE obj;
    ID id;
{
    VALUE klass = CLASS_OF(obj);
    if (rb_method_node(klass, respond_to) == basic_respond_to &&
	rb_method_boundp(klass, id, 0)) {
	return Qtrue;
    }
    else{
	return rb_funcall(obj, respond_to, 1, ID2SYM(id));
    }
    return Qfalse;
}


/*
 *  call-seq:
 *     obj.respond_to?(symbol, include_private=false) => true or false
 *  
 *  Returns +true+> if _obj_ responds to the given
 *  method. Private methods are included in the search only if the
 *  optional second parameter evaluates to +true+.
 */

static VALUE
rb_obj_respond_to(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE mid, priv;
    ID id;

    rb_scan_args(argc, argv, "11", &mid, &priv);
    id = rb_to_id(mid);
    if (rb_method_boundp(CLASS_OF(obj), id, !RTEST(priv))) {
	return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     mod.method_defined?(symbol)    => true or false
 *  
 *  Returns +true+ if the named method is defined by
 *  _mod_ (or its included modules and, if _mod_ is a class,
 *  its ancestors). Public and protected methods are matched.
 *     
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       def method2()  end
 *     end
 *     class C < B
 *       include A
 *       def method3()  end
 *     end
 *     
 *     A.method_defined? :method1    #=> true
 *     C.method_defined? "method1"   #=> true
 *     C.method_defined? "method2"   #=> true
 *     C.method_defined? "method3"   #=> true
 *     C.method_defined? "method4"   #=> false
 */

static VALUE
rb_mod_method_defined(mod, mid)
    VALUE mod, mid;
{
    return rb_method_boundp(mod, rb_to_id(mid), 1);
}

#define VISI_CHECK(x,f) (((x)&NOEX_MASK) == (f))

/*
 *  call-seq:
 *     mod.public_method_defined?(symbol)   => true or false
 *  
 *  Returns +true+ if the named public method is defined by
 *  _mod_ (or its included modules and, if _mod_ is a class,
 *  its ancestors).
 *     
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       protected
 *       def method2()  end
 *     end
 *     class C < B
 *       include A
 *       def method3()  end
 *     end
 *     
 *     A.method_defined? :method1           #=> true
 *     C.public_method_defined? "method1"   #=> true
 *     C.public_method_defined? "method2"   #=> false
 *     C.method_defined? "method2"          #=> true
 */

static VALUE
rb_mod_public_method_defined(mod, mid)
    VALUE mod, mid;
{
    ID id = rb_to_id(mid);
    int noex;

    if (rb_get_method_body(&mod, &id, &noex)) {
	if (VISI_CHECK(noex, NOEX_PUBLIC))
	    return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     mod.private_method_defined?(symbol)    => true or false
 *  
 *  Returns +true+ if the named private method is defined by
 *  _ mod_ (or its included modules and, if _mod_ is a class,
 *  its ancestors).
 *     
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       private
 *       def method2()  end
 *     end
 *     class C < B
 *       include A
 *       def method3()  end
 *     end
 *     
 *     A.method_defined? :method1            #=> true
 *     C.private_method_defined? "method1"   #=> false
 *     C.private_method_defined? "method2"   #=> true
 *     C.method_defined? "method2"           #=> false
 */

static VALUE
rb_mod_private_method_defined(mod, mid)
    VALUE mod, mid;
{
    ID id = rb_to_id(mid);
    int noex;

    if (rb_get_method_body(&mod, &id, &noex)) {
	if (VISI_CHECK(noex, NOEX_PRIVATE))
	    return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     mod.protected_method_defined?(symbol)   => true or false
 *  
 *  Returns +true+ if the named protected method is defined
 *  by _mod_ (or its included modules and, if _mod_ is a
 *  class, its ancestors).
 *     
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       protected
 *       def method2()  end
 *     end
 *     class C < B
 *       include A
 *       def method3()  end
 *     end
 *     
 *     A.method_defined? :method1              #=> true
 *     C.protected_method_defined? "method1"   #=> false
 *     C.protected_method_defined? "method2"   #=> true
 *     C.method_defined? "method2"             #=> true
 */

static VALUE
rb_mod_protected_method_defined(mod, mid)
    VALUE mod, mid;
{
    ID id = rb_to_id(mid);
    int noex;

    if (rb_get_method_body(&mod, &id, &noex)) {
	if (VISI_CHECK(noex, NOEX_PROTECTED))
	    return Qtrue;
    }
    return Qfalse;
}

NORETURN(static VALUE terminate_process _((int, const char *, long)));
static VALUE
terminate_process(status, mesg, mlen)
    int status;
    const char *mesg;
    long mlen;
{
    VALUE args[2];
    args[0] = INT2NUM(status);
    args[1] = rb_str_new(mesg, mlen);

    rb_exc_raise(rb_class_new_instance(2, args, rb_eSystemExit));
}

void
rb_exit(status)
    int status;
{
    if (prot_tag) {
	terminate_process(status, "exit", 4);
    }
    ruby_finalize();
    exit(status);
}


/*
 *  call-seq:
 *     exit(integer=0)
 *     Kernel::exit(integer=0)
 *     Process::exit(integer=0)
 *  
 *  Initiates the termination of the Ruby script by raising the
 *  <code>SystemExit</code> exception. This exception may be caught. The
 *  optional parameter is used to return a status code to the invoking
 *  environment.
 *     
 *     begin
 *       exit
 *       puts "never get here"
 *     rescue SystemExit
 *       puts "rescued a SystemExit exception"
 *     end
 *     puts "after begin block"
 *     
 *  <em>produces:</em>
 *     
 *     rescued a SystemExit exception
 *     after begin block
 *     
 *  Just prior to termination, Ruby executes any <code>at_exit</code> functions
 *  (see Kernel::at_exit) and runs any object finalizers (see
 *  ObjectSpace::define_finalizer).
 *     
 *     at_exit { puts "at_exit function" }
 *     ObjectSpace.define_finalizer("string",  proc { puts "in finalizer" })
 *     exit
 *     
 *  <em>produces:</em>
 *     
 *     at_exit function
 *     in finalizer
 */

VALUE
rb_f_exit(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE status;
    int istatus;

    rb_secure(4);
    if (rb_scan_args(argc, argv, "01", &status) == 1) {
	switch (status) {
	  case Qtrue:
	    istatus = EXIT_SUCCESS;
	    break;
	  case Qfalse:
	    istatus = EXIT_FAILURE;
	    break;
	  default:
	    istatus = NUM2INT(status);
	    break;
	}
    }
    else {
	istatus = EXIT_SUCCESS;
    }
    rb_exit(istatus);
    return Qnil;		/* not reached */
}


/*
 *  call-seq:
 *     abort
 *     Kernel::abort
 *     Process::abort
 *  
 *  Terminate execution immediately, effectively by calling
 *  <code>Kernel.exit(1)</code>. If _msg_ is given, it is written
 *  to STDERR prior to terminating.
 */

VALUE
rb_f_abort(argc, argv)
    int argc;
    VALUE *argv;
{
    rb_secure(4);
    if (argc == 0) {
	if (!NIL_P(ruby_errinfo)) {
	    error_print();
	}
	rb_exit(EXIT_FAILURE);
    }
    else {
	VALUE mesg;

	rb_scan_args(argc, argv, "1", &mesg);
	StringValue(argv[0]);
	rb_io_puts(argc, argv, rb_stderr);
	terminate_process(EXIT_FAILURE, RSTRING(argv[0])->ptr, RSTRING(argv[0])->len);
    }
    return Qnil;		/* not reached */
}

void
rb_iter_break()
{
    break_jump(Qnil);
}

NORETURN(static void rb_longjmp _((int, VALUE)));
static VALUE make_backtrace _((void));

static void
rb_longjmp(tag, mesg)
    int tag;
    VALUE mesg;
{
    VALUE at;

    if (thread_set_raised()) {
	ruby_errinfo = exception_error;
	JUMP_TAG(TAG_FATAL);
    }
    if (NIL_P(mesg)) mesg = ruby_errinfo;
    if (NIL_P(mesg)) {
	mesg = rb_exc_new(rb_eRuntimeError, 0, 0);
    }

    ruby_set_current_source();
    if (ruby_sourcefile && !NIL_P(mesg)) {
	at = get_backtrace(mesg);
	if (NIL_P(at)) {
	    at = make_backtrace();
	    set_backtrace(mesg, at);
	}
    }
    if (!NIL_P(mesg)) {
	ruby_errinfo = mesg;
    }

    if (RTEST(ruby_debug) && !NIL_P(ruby_errinfo)
	&& !rb_obj_is_kind_of(ruby_errinfo, rb_eSystemExit)) {
	VALUE e = ruby_errinfo;
	int status;

	PUSH_TAG(PROT_NONE);
	if ((status = EXEC_TAG()) == 0) {
	    e = rb_obj_as_string(e);
	    warn_printf("Exception `%s' at %s:%d - %s\n",
			rb_obj_classname(ruby_errinfo),
			ruby_sourcefile, ruby_sourceline,
			RSTRING(e)->ptr);
	}
	POP_TAG();
	if (status == TAG_FATAL && ruby_errinfo == exception_error) {
	    ruby_errinfo = mesg;
	}
	else if (status) {
	    thread_reset_raised();
	    JUMP_TAG(status);
	}
    }

    rb_trap_restore_mask();
    if (tag != TAG_FATAL) {
	EXEC_EVENT_HOOK(RUBY_EVENT_RAISE, ruby_current_node,
			ruby_frame->self,
			ruby_frame->this_func,
			ruby_frame->this_class);
    }
    if (!prot_tag) {
	error_print();
    }
    thread_reset_raised();
    JUMP_TAG(tag);
}

void
rb_exc_raise(mesg)
    VALUE mesg;
{
    rb_longjmp(TAG_RAISE, mesg);
}

void
rb_exc_fatal(mesg)
    VALUE mesg;
{
    rb_longjmp(TAG_FATAL, mesg);
}

void
rb_interrupt()
{
    rb_raise(rb_eInterrupt, "");
}

/*
 *  call-seq:
 *     raise
 *     raise(string)
 *     raise(exception [, string [, array]])
 *     fail
 *     fail(string)
 *     fail(exception [, string [, array]])
 *  
 *  With no arguments, raises the exception in <code>$!</code> or raises
 *  a <code>RuntimeError</code> if <code>$!</code> is +nil+.
 *  With a single +String+ argument, raises a
 *  +RuntimeError+ with the string as a message. Otherwise,
 *  the first parameter should be the name of an +Exception+
 *  class (or an object that returns an +Exception+ object when sent
 *  an +exception+ message). The optional second parameter sets the
 *  message associated with the exception, and the third parameter is an
 *  array of callback information. Exceptions are caught by the
 *  +rescue+ clause of <code>begin...end</code> blocks.
 *     
 *     raise "Failed to create socket"
 *     raise ArgumentError, "No parameters", caller
 */

static VALUE
rb_f_raise(argc, argv)
    int argc;
    VALUE *argv;
{
    rb_raise_jump(rb_make_exception(argc, argv));
    return Qnil;		/* not reached */
}

static VALUE
rb_make_exception(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE mesg;
    ID exception;
    int n;

    mesg = Qnil;
    switch (argc) {
      case 0:
	mesg = Qnil;
	break;
      case 1:
	if (NIL_P(argv[0])) break;
	if (TYPE(argv[0]) == T_STRING) {
	    mesg = rb_exc_new3(rb_eRuntimeError, argv[0]);
	    break;
	}
	n = 0;
	goto exception_call;

      case 2:
      case 3:
	n = 1;
      exception_call:
	exception = rb_intern("exception");
	if (!rb_respond_to(argv[0], exception)) {
	    rb_raise(rb_eTypeError, "exception class/object expected");
	}
	mesg = rb_funcall(argv[0], exception, n, argv[1]);
	break;
      default:
	rb_raise(rb_eArgError, "wrong number of arguments");
	break;
    }
    if (argc > 0) {
	if (!rb_obj_is_kind_of(mesg, rb_eException))
	    rb_raise(rb_eTypeError, "exception object expected");
	if (argc>2)
	    set_backtrace(mesg, argv[2]);
    }

    return mesg;
}

static void
rb_raise_jump(mesg)
    VALUE mesg;
{
    if (ruby_frame != top_frame) {
	PUSH_FRAME();		/* fake frame */
	*ruby_frame = *_frame.prev->prev;
	rb_longjmp(TAG_RAISE, mesg);
	POP_FRAME();
    }
    rb_longjmp(TAG_RAISE, mesg);
}

void
rb_jump_tag(tag)
    int tag;
{
    JUMP_TAG(tag);
}

int
rb_block_given_p()
{
    if (ruby_frame->iter == ITER_CUR && ruby_block)
	return Qtrue;
    return Qfalse;
}

int
rb_iterator_p()
{
    return rb_block_given_p();
}

/*
 *  call-seq:
 *     block_given?   => true or false
 *     iterator?      => true or false
 *  
 *  Returns <code>true</code> if <code>yield</code> would execute a
 *  block in the current context. The <code>iterator?</code> form
 *  is mildly deprecated.
 *     
 *     def try
 *       if block_given?
 *         yield
 *       else
 *         "no block"
 *       end
 *     end
 *     try                  #=> "no block"
 *     try { "hello" }      #=> "hello"
 *     try do "hello" end   #=> "hello"
 */


static VALUE
rb_f_block_given_p()
{
    if (ruby_frame->prev && ruby_frame->prev->iter == ITER_CUR && ruby_block)
	return Qtrue;
    return Qfalse;
}

static VALUE rb_eThreadError;

NORETURN(static void proc_jump_error(int, VALUE));
static void
proc_jump_error(state, result)
    int state;
    VALUE result;
{
    char mesg[32];
    char *statement;

    switch (state) {
      case TAG_BREAK:
	statement = "break"; break;
      case TAG_RETURN:
	statement = "return"; break;
      case TAG_RETRY:
	statement = "retry"; break;
      default:
	statement = "local-jump"; break; /* should not happen */
    }
    snprintf(mesg, sizeof mesg, "%s from proc-closure", statement);
    localjump_error(mesg, result, state);
}

NORETURN(static void return_jump(VALUE));
static void
return_jump(retval)
    VALUE retval;
{
    struct tag *tt = prot_tag;
    int yield = Qfalse;

    if (retval == Qundef) retval = Qnil;
    while (tt) {
	if (tt->tag == PROT_YIELD) {
	    yield = Qtrue;
	    tt = tt->prev;
	}
	if ((tt->tag == PROT_FUNC && tt->frame->uniq == ruby_frame->uniq) ||
	    (tt->tag == PROT_LAMBDA && !yield))
	{ 
	    tt->dst = (VALUE)tt->frame->uniq;
	    tt->retval = retval;
	    JUMP_TAG(TAG_RETURN);
	}
	if (tt->tag == PROT_THREAD) {
	    rb_raise(rb_eThreadError, "return can't jump across threads");
	}
	tt = tt->prev;
    }
    localjump_error("unexpected return", retval, TAG_RETURN);
}

static void
break_jump(retval)
    VALUE retval;
{
    struct tag *tt = prot_tag;

    if (retval == Qundef) retval = Qnil;
    while (tt) {
	switch (tt->tag) {
	  case PROT_THREAD:
	  case PROT_YIELD:
	  case PROT_LOOP:
	  case PROT_LAMBDA:
	    tt->dst = (VALUE)tt->frame->uniq;
	    tt->retval = retval;
	    JUMP_TAG(TAG_BREAK);
	    break;
	  default:
	    break;
	}
	tt = tt->prev;
    }
    localjump_error("unexpected break", retval, TAG_BREAK);
}

static VALUE bmcall _((VALUE, VALUE));
static int method_arity _((VALUE));

static VALUE
rb_yield_0(val, self, klass, flags, avalue)
    VALUE val, self, klass;	/* OK */
    int flags, avalue;
{
    NODE *node, *var;
    volatile VALUE result = Qnil;
    volatile VALUE old_cref;
    volatile VALUE old_wrapper;
    struct BLOCK * volatile block;
    struct SCOPE * volatile old_scope;
    int old_vmode;
    struct FRAME frame;
    NODE *cnode = ruby_current_node;
    int lambda = flags & YIELD_LAMBDA_CALL;
    int state;

    if (!rb_block_given_p()) {
	localjump_error("no block given", Qnil, 0);
    }

    PUSH_VARS();
    block = ruby_block;
    frame = block->frame;
    frame.prev = ruby_frame;
    ruby_frame = &(frame);
    old_cref = (VALUE)ruby_cref;
    ruby_cref = block->cref;
    old_wrapper = ruby_wrapper;
    ruby_wrapper = block->wrapper;
    old_scope = ruby_scope;
    ruby_scope = block->scope;
    old_vmode = scope_vmode;
    scope_vmode = (flags & YIELD_PUBLIC_DEF) ? SCOPE_PUBLIC : block->vmode;
    ruby_block = block->prev;
    if (block->flags & BLOCK_D_SCOPE) {
	/* put place holder for dynamic (in-block) local variables */
	ruby_dyna_vars = new_dvar(0, 0, block->dyna_vars);
    }
    else {
	/* FOR does not introduce new scope */
	ruby_dyna_vars = block->dyna_vars;
    }
    PUSH_CLASS(klass ? klass : block->klass);
    if (!klass) {
	self = block->self;
    }
    node = block->body;
    var = block->var;

    if (var) {
	PUSH_TAG(PROT_NONE);
	if ((state = EXEC_TAG()) == 0) {
	    NODE *bvar = NULL;
	  block_var:
	    if (var == (NODE*)1) { /* no parameter || */
		if (lambda && RARRAY(val)->len != 0) {
		    rb_raise(rb_eArgError, "wrong number of arguments (%ld for 0)",
			     RARRAY(val)->len);
		}
	    }
	    else if (var == (NODE*)2) {
		if (TYPE(val) == T_ARRAY && RARRAY(val)->len != 0) {
		    rb_raise(rb_eArgError, "wrong number of arguments (%ld for 0)",
			     RARRAY(val)->len);
		}
	    }
	    else if (!bvar && nd_type(var) == NODE_BLOCK_PASS) {
		bvar = var->nd_body;
		var = var->nd_args;
		goto block_var;
	    }
	    else if (nd_type(var) == NODE_MASGN) {
		if (!avalue) {
		    val = svalue_to_mrhs(val, var->nd_head);
		}
		massign(self, var, val, lambda);
	    }
	    else {
		int len = 0;
		if (avalue) {
		    len = RARRAY(val)->len;
		    if (len == 0) {
			goto zero_arg;
		    }
		    if (len == 1) {
			val = RARRAY(val)->ptr[0];
		    }
		    else {
			goto multi_values;
		    }
		}
		else if (val == Qundef) {
		  zero_arg:
		    val = Qnil;
		  multi_values:
		    {
			ruby_current_node = var;
			rb_warn("multiple values for a block parameter (%d for 1)\n\tfrom %s:%d",
				len, cnode->nd_file, nd_line(cnode));
			ruby_current_node = cnode;
		    }
		}
		assign(self, var, val, lambda);
	    }
	    if (bvar) {
		VALUE blk;
		if (flags & YIELD_PROC_CALL)
		    blk = block->block_obj;
		else
		    blk = rb_block_proc();
		assign(self, bvar, blk, 0);
	    }
	}
	POP_TAG();
	if (state) goto pop_state;
    }
    else if (lambda && RARRAY(val)->len != 0 &&
	     (!node || nd_type(node) != NODE_IFUNC ||
	      node->nd_cfnc != bmcall)) {
	rb_raise(rb_eArgError, "wrong number of arguments (%ld for 0)",
		 RARRAY(val)->len);
    }
    if (!node) {
	state = 0;
	goto pop_state;
    }
    ruby_current_node = node;

    PUSH_ITER(block->iter);
    PUSH_TAG(lambda ? PROT_NONE : PROT_YIELD);
    if ((state = EXEC_TAG()) == 0) {
      redo:
	if (nd_type(node) == NODE_CFUNC || nd_type(node) == NODE_IFUNC) {
	    if (node->nd_state == YIELD_FUNC_AVALUE) {
		if (!avalue) {
		    val = svalue_to_avalue(val);
		}
	    }
	    else {
		if (avalue) {
		    val = avalue_to_svalue(val);
		}
		if (val == Qundef && node->nd_state != YIELD_FUNC_SVALUE)
		    val = Qnil;
	    }
	    if ((block->flags&BLOCK_FROM_METHOD) && RTEST(block->block_obj)) {
		struct BLOCK *data, _block;
		Data_Get_Struct(block->block_obj, struct BLOCK, data);
		_block = *data;
		_block.outer = ruby_block;
		_block.uniq = block_unique++;
		ruby_block = &_block;
		PUSH_ITER(ITER_PRE);
		ruby_frame->iter = ITER_CUR;
		result = (*node->nd_cfnc)(val, node->nd_tval, self);
		POP_ITER();
	    }
	    else {
		result = (*node->nd_cfnc)(val, node->nd_tval, self);
	    }
	}
	else {
	    result = rb_eval(self, node);
	}
    }
    else {
	switch (state) {
	  case TAG_REDO:
	    state = 0;
	    CHECK_INTS;
	    goto redo;
	  case TAG_NEXT:
	    state = 0;
	    result = prot_tag->retval;
	    break;
	  case TAG_BREAK:
	    if (TAG_DST()) {
		result = prot_tag->retval;
	    }
	    else {
		lambda = Qtrue;	/* just pass TAG_BREAK */
	    }
	    break;
	  default:
	    break;
	}
    }
    POP_TAG();
    POP_ITER();
  pop_state:
    POP_CLASS();
    if (ruby_dyna_vars && (block->flags & BLOCK_D_SCOPE) &&
	!FL_TEST(ruby_dyna_vars, DVAR_DONT_RECYCLE)) {
	struct RVarmap *vars = ruby_dyna_vars;

	if (ruby_dyna_vars->id == 0) {
	    vars = ruby_dyna_vars->next;
	    rb_gc_force_recycle((VALUE)ruby_dyna_vars);
	    while (vars && vars->id != 0 && vars != block->dyna_vars) {
		struct RVarmap *tmp = vars->next;
		rb_gc_force_recycle((VALUE)vars);
		vars = tmp;
	    }
	}
    }
    POP_VARS();
    ruby_block = block;
    ruby_frame = ruby_frame->prev;
    ruby_cref = (NODE*)old_cref;
    ruby_wrapper = old_wrapper;
    if (ruby_scope->flags & SCOPE_DONT_RECYCLE)
	scope_dup(old_scope);
    ruby_scope = old_scope;
    scope_vmode = old_vmode;
    switch (state) {
      case 0:
	break;
      case TAG_BREAK:
	if (!lambda) {
	    struct tag *tt = prot_tag;

	    while (tt) {
		if (tt->tag == PROT_LOOP && tt->blkid == ruby_block->uniq) {
		    tt->dst = (VALUE)tt->frame->uniq;
		    tt->retval = result;
		    JUMP_TAG(TAG_BREAK);
		}
		tt = tt->prev;
	    }
	    proc_jump_error(TAG_BREAK, result);
	}
	/* fall through */
      default:
	JUMP_TAG(state);
	break;
    }
    ruby_current_node = cnode;
    return result;
}

VALUE
rb_yield(val)
    VALUE val;
{
    return rb_yield_0(val, 0, 0, 0, Qfalse);
}

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_yield_values(int n, ...)
#else
rb_yield_values(n, va_alist)
    int n;
    va_dcl
#endif
{
    int i;
    va_list args;
    VALUE val;

    if (n == 0) {
	return rb_yield_0(Qundef, 0, 0, 0, Qfalse);
    }
    val = rb_values_new2(n, 0);
    va_init_list(args, n);
    for (i=0; i<n; i++) {
	RARRAY(val)->ptr[i] = va_arg(args, VALUE);
    }
    RARRAY(val)->len = n;
    va_end(args);
    return rb_yield_0(val, 0, 0, 0, Qtrue);
}

VALUE
rb_yield_splat(values)
    VALUE values;
{
    int avalue = Qfalse;

    if (TYPE(values) == T_ARRAY) {
	if (RARRAY(values)->len == 0) {
	    values = Qundef;
	}
	else {
	    avalue = Qtrue;
	}
    }
    return rb_yield_0(values, 0, 0, 0, avalue);
}

/*
 *  call-seq:
 *     loop {|| block }
 *  
 *  Repeatedly executes the block.
 *     
 *     loop do
 *       print "Input: "
 *       line = gets
 *       break if !line or line =~ /^qQ/
 *       # ...
 *     end
 */

static VALUE
rb_f_loop()
{
    for (;;) {
	rb_yield_0(Qundef, 0, 0, 0, Qfalse);
	CHECK_INTS;
    }
    return Qnil;		/* dummy */
}

static VALUE
massign(self, node, val, pcall)
    VALUE self;
    NODE *node;
    VALUE val;
    int pcall;
{
    NODE *list;
    long i = 0, len;

    len = RARRAY(val)->len;
    list = node->nd_head;
    for (; list && i<len; i++) {
	assign(self, list->nd_head, RARRAY(val)->ptr[i], pcall);
	list = list->nd_next;
    }
    if (pcall && list) goto arg_error;
    if (node->nd_args) {
	if ((long)(node->nd_args) == -1) {
	    /* no check for mere `*' */
	}
	else if (!list && i<len) {
	    assign(self, node->nd_args, rb_ary_new4(len-i, RARRAY(val)->ptr+i), pcall);
	}
	else {
	    assign(self, node->nd_args, rb_ary_new2(0), pcall);
	}
    }
    else if (pcall && i < len) {
	goto arg_error;
    }

    while (list) {
	i++;
	assign(self, list->nd_head, Qnil, pcall);
	list = list->nd_next;
    }
    return val;

  arg_error:
    while (list) {
	i++;
	list = list->nd_next;
    }
    rb_raise(rb_eArgError, "wrong number of arguments (%ld for %ld)", len, i);
}

static void
assign(self, lhs, val, pcall)
    VALUE self;
    NODE *lhs;
    VALUE val;
    int pcall;
{
    ruby_current_node = lhs;
    if (val == Qundef) {
	rb_warning("assigning void value");
	val = Qnil;
    }
    switch (nd_type(lhs)) {
      case NODE_GASGN:
	rb_gvar_set(lhs->nd_entry, val);
	break;

      case NODE_IASGN:
	rb_ivar_set(self, lhs->nd_vid, val);
	break;

      case NODE_LASGN:
	if (ruby_scope->local_vars == 0)
	    rb_bug("unexpected local variable assignment");
	ruby_scope->local_vars[lhs->nd_cnt] = val;
	break;

      case NODE_DASGN:
	dvar_asgn(lhs->nd_vid, val);
	break;

      case NODE_DASGN_CURR:
	dvar_asgn_curr(lhs->nd_vid, val);
	break;

      case NODE_CDECL:
	if (lhs->nd_vid == 0) {
	    rb_const_set(class_prefix(self, lhs->nd_else), lhs->nd_else->nd_mid, val);
	}
	else {
	    rb_const_set(ruby_cbase, lhs->nd_vid, val);
	}
	break;

      case NODE_CVDECL:
	if (RTEST(ruby_verbose) && FL_TEST(ruby_cbase, FL_SINGLETON)) {
	    rb_warn("declaring singleton class variable");
	}
	rb_cvar_set(cvar_cbase(), lhs->nd_vid, val, Qtrue);
	break;

      case NODE_CVASGN:
	rb_cvar_set(cvar_cbase(), lhs->nd_vid, val, Qfalse);
	break;

      case NODE_MASGN:
	massign(self, lhs, svalue_to_mrhs(val, lhs->nd_head), pcall);
	break;

      case NODE_CALL:
      case NODE_ATTRASGN:
	{
	    VALUE recv;
	    int scope;
	    if (lhs->nd_recv == (NODE *)1) {
		recv = self;
		scope = 1;
	    }
	    else {
		recv = rb_eval(self, lhs->nd_recv);
		scope = 0;
	    }
	    if (!lhs->nd_args) {
		/* attr set */
		ruby_current_node = lhs;
		SET_CURRENT_SOURCE();
		rb_call(CLASS_OF(recv), recv, lhs->nd_mid, 1, &val, scope);
	    }
	    else {
		/* array set */
		VALUE args;

		args = rb_eval(self, lhs->nd_args);
		rb_ary_push(args, val);
		ruby_current_node = lhs;
		SET_CURRENT_SOURCE();
		rb_call(CLASS_OF(recv), recv, lhs->nd_mid,
			RARRAY(args)->len, RARRAY(args)->ptr, scope);
	    }
	}
	break;

      default:
	rb_bug("bug in variable assignment");
	break;
    }
}

VALUE
rb_iterate(it_proc, data1, bl_proc, data2)
    VALUE (*it_proc) _((VALUE)), (*bl_proc)(ANYARGS);
    VALUE data1, data2;
{
    int state;
    volatile VALUE retval = Qnil;
    NODE *node = NEW_IFUNC(bl_proc, data2);
    VALUE self = ruby_top_self;

    PUSH_ITER(ITER_PRE);
    PUSH_TAG(PROT_LOOP);
    PUSH_BLOCK(0, node);
    state = EXEC_TAG();
    if (state == 0) {
  iter_retry:
	retval = (*it_proc)(data1);
    }
    else if (state == TAG_BREAK && TAG_DST()) {
	retval = prot_tag->retval;
	state = 0;
    }
    else if (state == TAG_RETRY) {
	state = 0;
	goto iter_retry;
    }
    POP_BLOCK();
    POP_TAG();
    POP_ITER();

    switch (state) {
      case 0:
	break;
      default:
	JUMP_TAG(state);
    }
    return retval;
}

static int
handle_rescue(self, node)
    VALUE self;
    NODE *node;
{
    int argc; VALUE *argv; /* used in SETUP_ARGS */
    TMP_PROTECT;

    if (!node->nd_args) {
	return rb_obj_is_kind_of(ruby_errinfo, rb_eStandardError);
    }

    BEGIN_CALLARGS;
    SETUP_ARGS(node->nd_args);
    END_CALLARGS;

    while (argc--) {
	if (!rb_obj_is_kind_of(argv[0], rb_cModule)) {
	    rb_raise(rb_eTypeError, "class or module required for rescue clause");
	}
	if (RTEST(rb_funcall(*argv, eqq, 1, ruby_errinfo))) return 1;
	argv++;
    }
    return 0;
}

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_rescue2(VALUE (*b_proc)(ANYARGS), VALUE data1, VALUE (*r_proc)(ANYARGS), VALUE data2, ...)
#else
rb_rescue2(b_proc, data1, r_proc, data2, va_alist)
    VALUE (*b_proc)(ANYARGS), (*r_proc)(ANYARGS);
    VALUE data1, data2;
    va_dcl
#endif
{
    int state;
    volatile VALUE result;
    volatile VALUE e_info = ruby_errinfo;
    va_list args;

    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
      retry_entry:
	result = (*b_proc)(data1);
    }
    else if (state == TAG_RAISE) {
	int handle = Qfalse;
	VALUE eclass;

	va_init_list(args, data2);
	while (eclass = va_arg(args, VALUE)) {
	    if (rb_obj_is_kind_of(ruby_errinfo, eclass)) {
		handle = Qtrue;
		break;
	    }
	}
	va_end(args);

	if (handle) {
	    if (r_proc) {
		PUSH_TAG(PROT_NONE);
		if ((state = EXEC_TAG()) == 0) {
		    result = (*r_proc)(data2, ruby_errinfo);
		}
		POP_TAG();
		if (state == TAG_RETRY) {
		    state = 0;
		    ruby_errinfo = Qnil;
		    goto retry_entry;
		}
	    }
	    else {
		result = Qnil;
		state = 0;
	    }
	    if (state == 0) {
		ruby_errinfo = e_info;
	    }
	}
    }
    POP_TAG();
    if (state) JUMP_TAG(state);

    return result;
}

VALUE
rb_rescue(b_proc, data1, r_proc, data2)
    VALUE (*b_proc)(), (*r_proc)();
    VALUE data1, data2;
{
    return rb_rescue2(b_proc, data1, r_proc, data2, rb_eStandardError, (VALUE)0);
}

static VALUE cont_protect;

VALUE
rb_protect(proc, data, state)
    VALUE (*proc) _((VALUE));
    VALUE data;
    int *state;
{
    VALUE result = Qnil;	/* OK */
    int status;

    PUSH_THREAD_TAG();
    cont_protect = (VALUE)rb_node_newnode(NODE_MEMO, cont_protect, 0, 0);
    if ((status = EXEC_TAG()) == 0) {
	result = (*proc)(data);
    }
    else if (status == TAG_THREAD) {
	rb_thread_start_1();
    }
    cont_protect = ((NODE *)cont_protect)->u1.value;
    POP_THREAD_TAG();
    if (state) {
	*state = status;
    }
    if (status != 0) {
	return Qnil;
    }

    return result;
}

VALUE
rb_ensure(b_proc, data1, e_proc, data2)
    VALUE (*b_proc)();
    VALUE data1;
    VALUE (*e_proc)();
    VALUE data2;
{
    int state;
    volatile VALUE result = Qnil;
    VALUE retval;

    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	result = (*b_proc)(data1);
    }
    POP_TAG();
    retval = prot_tag ? prot_tag->retval : Qnil;	/* save retval */
    (*e_proc)(data2);
    if (prot_tag) return_value(retval);
    if (state) JUMP_TAG(state);
    return result;
}

VALUE
rb_with_disable_interrupt(proc, data)
    VALUE (*proc)();
    VALUE data;
{
    VALUE result = Qnil;	/* OK */
    int status;

    DEFER_INTS;
    {
	int thr_critical = rb_thread_critical;

	rb_thread_critical = Qtrue;
	PUSH_TAG(PROT_NONE);
	if ((status = EXEC_TAG()) == 0) {
	    result = (*proc)(data);
	}
	POP_TAG();
	rb_thread_critical = thr_critical;
    }
    ENABLE_INTS;
    if (status) JUMP_TAG(status);

    return result;
}

static inline void
stack_check()
{
    static int overflowing = 0;

    if (!overflowing && ruby_stack_check()) {
	int state;
	overflowing = 1;
	PUSH_TAG(PROT_NONE);
	if ((state = EXEC_TAG()) == 0) {
	    rb_exc_raise(sysstack_error);
	}
	POP_TAG();
	overflowing = 0;
	JUMP_TAG(state);
    }
}

static int last_call_status;

#define CSTAT_PRIV  1
#define CSTAT_PROT  2
#define CSTAT_VCALL 4
#define CSTAT_SUPER 8

/*
 *  call-seq:
 *     obj.method_missing(symbol [, *args] )   => result
 *  
 *  Invoked by Ruby when <i>obj</i> is sent a message it cannot handle.
 *  <i>symbol</i> is the symbol for the method called, and <i>args</i>
 *  are any arguments that were passed to it. By default, the interpreter
 *  raises an error when this method is called. However, it is possible
 *  to override the method to provide more dynamic behavior.
 *  The example below creates
 *  a class <code>Roman</code>, which responds to methods with names
 *  consisting of roman numerals, returning the corresponding integer
 *  values.
 *     
 *     class Roman
 *       def romanToInt(str)
 *         # ...
 *       end
 *       def method_missing(methId)
 *         str = methId.id2name
 *         romanToInt(str)
 *       end
 *     end
 *     
 *     r = Roman.new
 *     r.iv      #=> 4
 *     r.xxiii   #=> 23
 *     r.mm      #=> 2000
 */

static VALUE
rb_method_missing(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    ID id;
    VALUE exc = rb_eNoMethodError;
    char *format = 0;
    NODE *cnode = ruby_current_node;

    if (argc == 0 || !SYMBOL_P(argv[0])) {
	rb_raise(rb_eArgError, "no id given");
    }

    stack_check();

    id = SYM2ID(argv[0]);

    if (last_call_status & CSTAT_PRIV) {
	format = "private method `%s' called for %s";
    }
    else if (last_call_status & CSTAT_PROT) {
	format = "protected method `%s' called for %s";
    }
    else if (last_call_status & CSTAT_VCALL) {
	format = "undefined local variable or method `%s' for %s";
	exc = rb_eNameError;
    }
    else if (last_call_status & CSTAT_SUPER) {
	format = "super: no superclass method `%s'";
    }
    if (!format) {
	format = "undefined method `%s' for %s";
    }

    ruby_current_node = cnode;
    {
	int n = 0;
	VALUE args[3];

	args[n++] = rb_funcall(rb_const_get(exc, rb_intern("message")), '!',
			       3, rb_str_new2(format), obj, argv[0]);
	args[n++] = argv[0];
	if (exc == rb_eNoMethodError) {
	    args[n++] = rb_ary_new4(argc-1, argv+1);
	}
	exc = rb_class_new_instance(n, args, exc);
	ruby_frame = ruby_frame->prev; /* pop frame for "method_missing" */
	rb_exc_raise(exc);
    }

    return Qnil;		/* not reached */
}

static VALUE
method_missing(obj, id, argc, argv, call_status)
    VALUE obj;
    ID    id;
    int   argc;
    const VALUE *argv;
    int   call_status;
{
    VALUE *nargv;

    last_call_status = call_status;

    if (id == missing) {
	PUSH_FRAME();
	rb_method_missing(argc, argv, obj);
	POP_FRAME();
    }
    else if (id == ID_ALLOCATOR) {
	rb_raise(rb_eTypeError, "allocator undefined for %s", rb_class2name(obj));
    }

    nargv = ALLOCA_N(VALUE, argc+1);
    nargv[0] = ID2SYM(id);
    MEMCPY(nargv+1, argv, VALUE, argc);

    return rb_funcall2(obj, missing, argc+1, nargv);
}

static inline VALUE
call_cfunc(func, recv, len, argc, argv)
    VALUE (*func)();
    VALUE recv;
    int len, argc;
    VALUE *argv;
{
    if (len >= 0 && argc != len) {
	rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)",
		 argc, len);
    }

    switch (len) {
      case -2:
	return (*func)(recv, rb_ary_new4(argc, argv));
	break;
      case -1:
	return (*func)(argc, argv, recv);
	break;
      case 0:
	return (*func)(recv);
	break;
      case 1:
	return (*func)(recv, argv[0]);
	break;
      case 2:
	return (*func)(recv, argv[0], argv[1]);
	break;
      case 3:
	return (*func)(recv, argv[0], argv[1], argv[2]);
	break;
      case 4:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3]);
	break;
      case 5:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4]);
	break;
      case 6:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5]);
	break;
      case 7:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6]);
	break;
      case 8:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7]);
	break;
      case 9:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8]);
	break;
      case 10:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8], argv[9]);
	break;
      case 11:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
	break;
      case 12:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8], argv[9],
		       argv[10], argv[11]);
	break;
      case 13:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8], argv[9], argv[10],
		       argv[11], argv[12]);
	break;
      case 14:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8], argv[9], argv[10],
		       argv[11], argv[12], argv[13]);
	break;
      case 15:
	return (*func)(recv, argv[0], argv[1], argv[2], argv[3], argv[4],
		       argv[5], argv[6], argv[7], argv[8], argv[9], argv[10],
		       argv[11], argv[12], argv[13], argv[14]);
	break;
      default:
	rb_raise(rb_eArgError, "too many arguments (%d)", len);
	break;
    }
    return Qnil;		/* not reached */
}

static VALUE
rb_call0(klass, recv, id, oid, argc, argv, body, nosuper)
    VALUE klass, recv;
    ID    id;
    ID    oid;
    int argc;			/* OK */
    VALUE *argv;		/* OK */
    NODE *body;			/* OK */
    int nosuper;
{
    NODE *b2;		/* OK */
    volatile VALUE result = Qnil;
    int itr;
    static int tick;
    volatile VALUE args;
    TMP_PROTECT;

    switch (ruby_iter->iter) {
      case ITER_PRE:
	itr = ITER_CUR;
	break;
      case ITER_CUR:
      default:
	itr = ITER_NOT;
	break;
    }

    if ((++tick & 0xff) == 0) {
	CHECK_INTS;		/* better than nothing */
	stack_check();
	rb_gc_finalize_deferred();
    }
    if (argc < 0) {
	argc = -argc-1;
	args = rb_ary_concat(rb_ary_new4(argc, argv), splat_value(argv[argc]));
	argc = RARRAY(args)->len;
	argv = RARRAY(args)->ptr;
    }
    PUSH_ITER(itr);
    PUSH_FRAME();
    ruby_frame->callee = id;
    ruby_frame->this_func = oid;
    ruby_frame->this_class = nosuper?0:klass;
    ruby_frame->self = recv;
    ruby_frame->argc = argc;

    switch (nd_type(body)) {
      case NODE_CFUNC:
	{
	    int len = body->nd_argc;

	    if (len < -2) {
		rb_bug("bad argc (%d) specified for `%s(%s)'",
		       len, rb_class2name(klass), rb_id2name(id));
	    }
	    if (event_hooks) {
		int state;

		EXEC_EVENT_HOOK(RUBY_EVENT_C_CALL, ruby_current_node,
				recv, id, klass);
		PUSH_TAG(PROT_FUNC);
		if ((state = EXEC_TAG()) == 0) {
		    result = call_cfunc(body->nd_cfnc, recv, len, argc, argv);
		}
		POP_TAG();
		ruby_current_node = ruby_frame->node;
		EXEC_EVENT_HOOK(RUBY_EVENT_C_RETURN, ruby_current_node,
				recv, id, klass);
		if (state) JUMP_TAG(state);
	    }
	    else {
		result = call_cfunc(body->nd_cfnc, recv, len, argc, argv);
	    }
	}
	break;

	/* for attr get/set */
      case NODE_IVAR:
	if (argc != 0) {
	    rb_raise(rb_eArgError, "wrong number of arguments (%d for 0)", argc);
	}
	result = rb_attr_get(recv, body->nd_vid);
	break;

      case NODE_ATTRSET:
	if (argc != 1)
	    rb_raise(rb_eArgError, "wrong number of arguments (%d for 1)", argc);
	result = rb_ivar_set(recv, body->nd_vid, argv[0]);
	break;

      case NODE_ZSUPER:		/* visibility override */
	result = rb_call_super(argc, argv);
	break;

      case NODE_BMETHOD:
	ruby_frame->flags |= FRAME_DMETH;
	result = proc_invoke(body->nd_cval, rb_ary_new4(argc, argv), recv, klass);
	break;

      case NODE_SCOPE:
	{
	    int state;
	    VALUE *local_vars;	/* OK */
	    NODE *saved_cref = 0;

	    PUSH_SCOPE();

	    if (body->nd_rval) {
		saved_cref = ruby_cref;
		ruby_cref = (NODE*)body->nd_rval;
	    }
	    PUSH_CLASS(ruby_cbase);
	    if (body->nd_tbl) {
		local_vars = TMP_ALLOC(body->nd_tbl[0]+1);
		*local_vars++ = (VALUE)body;
		rb_mem_clear(local_vars, body->nd_tbl[0]);
		ruby_scope->local_tbl = body->nd_tbl;
		ruby_scope->local_vars = local_vars;
	    }
	    else {
		local_vars = ruby_scope->local_vars = 0;
		ruby_scope->local_tbl  = 0;
	    }
	    b2 = body = body->nd_next;

	    PUSH_VARS();
	    PUSH_TAG(PROT_FUNC);

	    if ((state = EXEC_TAG()) == 0) {
		NODE *node = 0;
		int i;

		if (nd_type(body) == NODE_ARGS) {
		    node = body;
		    body = 0;
		}
		else if (nd_type(body) == NODE_BLOCK) {
		    node = body->nd_head;
		    body = body->nd_next;
		}
		if (node) {
		    if (nd_type(node) != NODE_ARGS) {
			rb_bug("no argument-node");
		    }

		    i = node->nd_cnt;
		    if (i > argc) {
			rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)", argc, i);
		    }
		    if ((long)node->nd_rest == -1) {
			int opt = i;
			NODE *optnode = node->nd_opt;

			while (optnode) {
			    opt++;
			    optnode = optnode->nd_next;
			}
			if (opt < argc) {
			    rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)",
				     argc, opt);
			}
			ruby_frame->argc = opt;
		    }

		    if (local_vars) {
			if (i > 0) {
			    /* +2 for $_ and $~ */
			    MEMCPY(local_vars+2, argv, VALUE, i);
			}
			argv += i; argc -= i;
			if (node->nd_opt) {
			    NODE *opt = node->nd_opt;

			    while (opt && argc) {
				assign(recv, opt->nd_head, *argv, 1);
				argv++; argc--;
				opt = opt->nd_next;
			    }
			    if (opt) {
				rb_eval(recv, opt);
			    }
			}
			if ((long)node->nd_rest >= 0) {
			    VALUE v;

			    if (argc > 0)
				v = rb_ary_new4(argc,argv);
			    else
				v = rb_ary_new2(0);
			    ruby_scope->local_vars[node->nd_rest] = v;
			}
		    }
		}
		if ((long)node->nd_rest >= 0) {
		    ruby_frame->argc = -(ruby_frame->argc - argc)-1;
		}

		if (event_hooks) {
		    EXEC_EVENT_HOOK(RUBY_EVENT_CALL, b2, recv, id, klass);
		}
		result = rb_eval(recv, body);
	    }
	    else if (state == TAG_RETURN && TAG_DST()) {
		result = prot_tag->retval;
		state = 0;
	    }
	    POP_TAG();
	    POP_VARS();
	    POP_CLASS();
	    POP_SCOPE();
	    ruby_cref = saved_cref;
	    if (event_hooks) {
		EXEC_EVENT_HOOK(RUBY_EVENT_RETURN, body, recv, id, klass);
	    }
	    switch (state) {
	      case 0:
		break;

	      case TAG_BREAK:
	      case TAG_RETURN:
		JUMP_TAG(state);
		break;

	      case TAG_RETRY:
		if (rb_block_given_p()) JUMP_TAG(state);
		/* fall through */
	      default:
		jump_tag_but_local_jump(state, result);
		break;
	    }
	}
	break;

      default:
	rb_bug("unknown node type %d", nd_type(body));
	break;
    }
    POP_FRAME();
    POP_ITER();
    return result;
}

static VALUE
rb_call(klass, recv, mid, argc, argv, scope)
    VALUE klass, recv;
    ID    mid;
    int argc;			/* OK */
    const VALUE *argv;		/* OK */
    int scope;
{
    NODE  *body;		/* OK */
    int    noex;
    ID     id = mid;
    struct cache_entry *ent;

    if (!klass) {
	rb_raise(rb_eNotImpError, "method `%s' called on terminated object (0x%lx)",
		 rb_id2name(mid), recv);
    }
    /* is it in the method cache? */
    ent = cache + EXPR1(klass, mid);
    if (ent->mid == mid && ent->klass == klass) {
	if (!ent->method)
	    return method_missing(recv, mid, argc, argv, scope==2?CSTAT_VCALL:0);
	klass = ent->origin;
	id    = ent->mid0;
	noex  = ent->noex;
	body  = ent->method;
    }
    else if ((body = rb_get_method_body(&klass, &id, &noex)) == 0) {
	if (scope == 3) {
	    return method_missing(recv, mid, argc, argv, CSTAT_SUPER);
	}
	return method_missing(recv, mid, argc, argv, scope==2?CSTAT_VCALL:0);
    }

    if (mid != missing) {
	/* receiver specified form for private method */
	if ((noex & NOEX_PRIVATE) && scope == 0)
	    return method_missing(recv, mid, argc, argv, CSTAT_PRIV);

	/* self must be kind of a specified form for protected method */
	if ((noex & NOEX_PROTECTED)) {
	    VALUE defined_class = klass;

	    if (TYPE(defined_class) == T_ICLASS) {
		defined_class = RBASIC(defined_class)->klass;
	    }
	    if (!rb_obj_is_kind_of(ruby_frame->self, rb_class_real(defined_class)))
		return method_missing(recv, mid, argc, argv, CSTAT_PROT);
	}
    }

    return rb_call0(klass, recv, mid, id, argc, argv, body, noex & NOEX_NOSUPER);
}

VALUE
rb_apply(recv, mid, args)
    VALUE recv;
    ID mid;
    VALUE args;
{
    int argc;
    VALUE *argv;

    argc = RARRAY(args)->len; /* Assigns LONG, but argc is INT */
    argv = ALLOCA_N(VALUE, argc);
    MEMCPY(argv, RARRAY(args)->ptr, VALUE, argc);
    return rb_call(CLASS_OF(recv), recv, mid, argc, argv, 1);
}

/*
 *  call-seq:
 *     obj.send(symbol [, args...])        => obj
 *     obj.__send__(symbol [, args...])    => obj
 *  
 *  Invokes the method identified by _symbol_, passing it any
 *  arguments specified. You can use <code>__send__</code> if the name
 *  +send+ clashes with an existing method in _obj_.
 *     
 *     class Klass
 *       def hello(*args)
 *         "Hello " + args.join(' ')
 *       end
 *     end
 *     k = Klass.new
 *     k.send :hello, "gentle", "readers"   #=> "Hello gentle readers"
 */

static VALUE
rb_f_send(argc, argv, recv)
    int argc;
    VALUE *argv;
    VALUE recv;
{
    VALUE vid;

    if (argc == 0) rb_raise(rb_eArgError, "no method name given");

    vid = *argv++; argc--;
    PUSH_ITER(rb_block_given_p()?ITER_PRE:ITER_NOT);
    vid = rb_call(CLASS_OF(recv), recv, rb_to_id(vid), argc, argv, 1);
    POP_ITER();

    return vid;
}

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_funcall(VALUE recv, ID mid, int n, ...)
#else
rb_funcall(recv, mid, n, va_alist)
    VALUE recv;
    ID mid;
    int n;
    va_dcl
#endif
{
    VALUE *argv;
    va_list ar;
    va_init_list(ar, n);

    if (n > 0) {
	long i;

	argv = ALLOCA_N(VALUE, n);

	for (i=0;i<n;i++) {
	    argv[i] = va_arg(ar, VALUE);
	}
	va_end(ar);
    }
    else {
	argv = 0;
    }

    return rb_call(CLASS_OF(recv), recv, mid, n, argv, 1);
}

VALUE
rb_funcall2(recv, mid, argc, argv)
    VALUE recv;
    ID mid;
    int argc;
    const VALUE *argv;
{
    return rb_call(CLASS_OF(recv), recv, mid, argc, argv, 1);
}

VALUE
rb_funcall3(recv, mid, argc, argv)
    VALUE recv;
    ID mid;
    int argc;
    const VALUE *argv;
{
    return rb_call(CLASS_OF(recv), recv, mid, argc, argv, 0);
}

VALUE
rb_call_super(argc, argv)
    int argc;
    const VALUE *argv;
{
    VALUE result, self, klass, k;

    if (ruby_frame->this_class == 0) {
	rb_name_error(ruby_frame->callee, "calling `super' from `%s' is prohibited",
		      rb_id2name(ruby_frame->this_func));
    }

    self = ruby_frame->self;
    klass = ruby_frame->this_class;

    PUSH_ITER(ruby_iter->iter ? ITER_PRE : ITER_NOT);
    result = rb_call(RCLASS(klass)->super, self, ruby_frame->this_func, argc, argv, 3);
    POP_ITER();

    return result;
}

static VALUE
backtrace(lev)
    int lev;
{
    struct FRAME *frame = ruby_frame;
    char buf[BUFSIZ];
    volatile VALUE ary;
    NODE *n;

    ary = rb_ary_new();
    if (frame->this_func == ID_ALLOCATOR) {
	frame = frame->prev;
    }
    if (lev < 0) {
	ruby_set_current_source();
	if (frame->this_func) {
	    snprintf(buf, BUFSIZ, "%s:%d:in `%s'",
		     ruby_sourcefile, ruby_sourceline,
		     rb_id2name(frame->this_func));
	}
	else if (ruby_sourceline == 0) {
	    snprintf(buf, BUFSIZ, "%s", ruby_sourcefile);
	}
	else {
	    snprintf(buf, BUFSIZ, "%s:%d", ruby_sourcefile, ruby_sourceline);
	}
	rb_ary_push(ary, rb_str_new2(buf));
	if (lev < -1) return ary;
    }
    else {
	while (lev-- > 0) {
	    frame = frame->prev;
	    if (!frame) {
		ary = Qnil;
		break;
	    }
	}
    }
    while (frame && (n = frame->node)) {
	if (frame->prev && frame->prev->this_func) {
	    snprintf(buf, BUFSIZ, "%s:%d:in `%s'",
		     n->nd_file, nd_line(n),
		     rb_id2name(frame->prev->this_func));
	}
	else {
	    snprintf(buf, BUFSIZ, "%s:%d", n->nd_file, nd_line(n));
	}
	rb_ary_push(ary, rb_str_new2(buf));
	frame = frame->prev;
    }

    return ary;
}

/*
 *  call-seq:
 *     caller(start=1)    => array
 *  
 *  Returns the current execution stack---an array containing strings in
 *  the form ``<em>file:line</em>'' or ``<em>file:line: in
 *  `method'</em>''. The optional _start_ parameter
 *  determines the number of initial stack entries to omit from the
 *  result.
 *     
 *     def a(skip)
 *       caller(skip)
 *     end
 *     def b(skip)
 *       a(skip)
 *     end
 *     def c(skip)
 *       b(skip)
 *     end
 *     c(0)   #=> ["prog:2:in `a'", "prog:5:in `b'", "prog:8:in `c'", "prog:10"]
 *     c(1)   #=> ["prog:5:in `b'", "prog:8:in `c'", "prog:11"]
 *     c(2)   #=> ["prog:8:in `c'", "prog:12"]
 *     c(3)   #=> ["prog:13"]
 */

static VALUE
rb_f_caller(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE level;
    int lev;

    rb_scan_args(argc, argv, "01", &level);

    if (NIL_P(level)) lev = 1;
    else lev = NUM2INT(level);
    if (lev < 0) rb_raise(rb_eArgError, "negative level (%d)", lev);

    return backtrace(lev);
}

void
rb_backtrace()
{
    long i;
    VALUE ary;

    ary = backtrace(-1);
    for (i=0; i<RARRAY(ary)->len; i++) {
	printf("\tfrom %s\n", RSTRING(RARRAY(ary)->ptr[i])->ptr);
    }
}

static VALUE
make_backtrace()
{
    return backtrace(-1);
}

ID
rb_frame_this_func()
{
    return ruby_frame->this_func;
}

static NODE*
compile(src, file, line)
    VALUE src;
    char *file;
    int line;
{
    NODE *node;
    int critical;

    ruby_nerrs = 0;
    StringValue(src);
    critical = rb_thread_critical;
    rb_thread_critical = Qtrue;
    node = rb_compile_string(file, src, line);
    rb_thread_critical = critical;

    if (ruby_nerrs == 0) return node;
    return 0;
}

static VALUE
eval(self, src, scope, file, line)
    VALUE self, src, scope;
    char *file;
    int line;
{
    struct BLOCK *data = NULL;
    volatile VALUE result = Qnil;
    struct SCOPE * volatile old_scope;
    struct BLOCK * volatile old_block;
    struct RVarmap * volatile old_dyna_vars;
    VALUE volatile old_cref;
    int volatile old_vmode;
    volatile VALUE old_wrapper;
    struct FRAME frame;
    NODE *nodesave = ruby_current_node;
    volatile int iter = ruby_frame->iter;
    volatile int safe = ruby_safe_level;
    int state;

    if (!NIL_P(scope)) {
	if (!rb_obj_is_proc(scope)) {
	    rb_raise(rb_eTypeError, "wrong argument type %s (expected Proc/Binding)",
		     rb_obj_classname(scope));
	}

	Data_Get_Struct(scope, struct BLOCK, data);
	/* PUSH BLOCK from data */
	frame = data->frame;
	frame.tmp = ruby_frame;	/* gc protection */
	ruby_frame = &(frame);
	old_scope = ruby_scope;
	ruby_scope = data->scope;
	old_block = ruby_block;
	ruby_block = data->prev;
	old_dyna_vars = ruby_dyna_vars;
	ruby_dyna_vars = data->dyna_vars;
	old_vmode = scope_vmode;
	scope_vmode = data->vmode;
	old_cref = (VALUE)ruby_cref;
	ruby_cref = data->cref;
	old_wrapper = ruby_wrapper;
	ruby_wrapper = data->wrapper;
	if ((file == 0 || (line == 1 && strcmp(file, "(eval)") == 0)) && data->frame.node) {
	    file = data->frame.node->nd_file;
	    if (!file) file = "__builtin__";
	    line = nd_line(data->frame.node);
	}

	self = data->self;
	ruby_frame->iter = data->iter;
    }
    else {
	if (ruby_frame->prev) {
	    ruby_frame->iter = ruby_frame->prev->iter;
	}
    }
    if (file == 0) {
	ruby_set_current_source();
	file = ruby_sourcefile;
	line = ruby_sourceline;
    }
    PUSH_CLASS(ruby_cbase);
    ruby_in_eval++;
    if (TYPE(ruby_class) == T_ICLASS) {
	ruby_class = RBASIC(ruby_class)->klass;
    }
    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	NODE *node;

	ruby_safe_level = 0;
	result = ruby_errinfo;
	ruby_errinfo = Qnil;
	node = compile(src, file, line);
	ruby_safe_level = safe;
	if (ruby_nerrs > 0) {
	    compile_error(0);
	}
	if (!NIL_P(result)) ruby_errinfo = result;
	result = eval_node(self, node);
    }
    POP_TAG();
    POP_CLASS();
    ruby_in_eval--;
    ruby_safe_level = safe;
    if (!NIL_P(scope)) {
	int dont_recycle = ruby_scope->flags & SCOPE_DONT_RECYCLE;

	ruby_wrapper = old_wrapper;
	ruby_cref  = (NODE*)old_cref;
	ruby_frame = frame.tmp;
	ruby_scope = old_scope;
	ruby_block = old_block;
	ruby_dyna_vars = old_dyna_vars;
	data->vmode = scope_vmode; /* write back visibility mode */
	scope_vmode = old_vmode;
	if (dont_recycle) {
	    struct tag *tag;
	    struct RVarmap *vars;

	    scope_dup(ruby_scope);
	    for (tag=prot_tag; tag; tag=tag->prev) {
		scope_dup(tag->scope);
	    }
	    for (vars = ruby_dyna_vars; vars; vars = vars->next) {
		FL_SET(vars, DVAR_DONT_RECYCLE);
	    }
	}
    }
    else {
	ruby_frame->iter = iter;
    }
    ruby_current_node = nodesave;
    ruby_set_current_source();
    if (state) {
	if (state == TAG_RAISE) {
	    if (strcmp(file, "(eval)") == 0) {
		VALUE mesg, errat;

		errat = get_backtrace(ruby_errinfo);
		mesg  = rb_attr_get(ruby_errinfo, rb_intern("mesg"));
		if (!NIL_P(errat) && TYPE(errat) == T_ARRAY) {
		    if (!NIL_P(mesg) && TYPE(mesg) == T_STRING) {
			rb_str_update(mesg, 0, 0, rb_str_new2(": "));
			rb_str_update(mesg, 0, 0, RARRAY(errat)->ptr[0]);
		    }
		    RARRAY(errat)->ptr[0] = RARRAY(backtrace(-2))->ptr[0];
		}
	    }
	    rb_exc_raise(ruby_errinfo);
	}
	JUMP_TAG(state);
    }

    return result;
}

/*
 *  call-seq:
 *     eval(string [, binding [, filename [,lineno]]])  => obj
 *  
 *  Evaluates the Ruby expression(s) in <em>string</em>. If
 *  <em>binding</em> is given, the evaluation is performed in its
 *  context. The binding may be a <code>Binding</code> object or a
 *  <code>Proc</code> object. If the optional <em>filename</em> and
 *  <em>lineno</em> parameters are present, they will be used when
 *  reporting syntax errors.
 *     
 *     def getBinding(str)
 *       return binding
 *     end
 *     str = "hello"
 *     eval "str + ' Fred'"                      #=> "hello Fred"
 *     eval "str + ' Fred'", getBinding("bye")   #=> "bye Fred"
 */

static VALUE
rb_f_eval(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    VALUE src, scope, vfile, vline;
    char *file = "(eval)";
    int line = 1;

    rb_scan_args(argc, argv, "13", &src, &scope, &vfile, &vline);
    if (ruby_safe_level >= 4) {
	StringValue(src);
	if (!NIL_P(scope) && !OBJ_TAINTED(scope)) {
	    rb_raise(rb_eSecurityError, "Insecure: can't modify trusted binding");
	}
    }
    else {
	SafeStringValue(src);
    }
    if (argc >= 3) {
	StringValue(vfile);
    }
    if (argc >= 4) {
	line = NUM2INT(vline);
    }

    if (!NIL_P(vfile)) file = RSTRING(vfile)->ptr;
    if (NIL_P(scope) && ruby_frame->prev) {
	struct FRAME *prev;
	VALUE val;

	prev = ruby_frame;
	PUSH_FRAME();
	*ruby_frame = *prev->prev;
	ruby_frame->prev = prev;
	val = eval(self, src, scope, file, line);
	POP_FRAME();

	return val;
    }
    return eval(self, src, scope, file, line);
}

/* function to call func under the specified class/module context */
static VALUE
exec_under(func, under, cbase, args)
    VALUE (*func)();
    VALUE under, cbase;
    void *args;
{
    VALUE val = Qnil;		/* OK */
    int state;
    int mode;

    PUSH_CLASS(under);
    PUSH_FRAME();
    ruby_frame->self = _frame.prev->self;
    ruby_frame->callee = _frame.prev->callee;
    ruby_frame->this_func = _frame.prev->this_func;
    ruby_frame->this_class = _frame.prev->this_class;
    ruby_frame->argc = _frame.prev->argc;
    if (cbase) {
	PUSH_CREF(cbase);
    }

    mode = scope_vmode;
    SCOPE_SET(SCOPE_PUBLIC);
    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	val = (*func)(args);
    }
    POP_TAG();
    if (cbase) POP_CREF();
    SCOPE_SET(mode);
    POP_FRAME();
    POP_CLASS();
    if (state) JUMP_TAG(state);

    return val;
}

static VALUE
eval_under_i(args)
    VALUE *args;
{
    return eval(args[0], args[1], Qnil, (char*)args[2], (int)args[3]);
}

/* string eval under the class/module context */
static VALUE
eval_under(under, self, src, file, line)
    VALUE under, self, src;
    const char *file;
    int line;
{
    VALUE args[4];

    if (ruby_safe_level >= 4) {
	StringValue(src);
    }
    else {
	SafeStringValue(src);
    }
    args[0] = self;
    args[1] = src;
    args[2] = (VALUE)file;
    args[3] = (VALUE)line;
    return exec_under(eval_under_i, under, under, args);
}

static VALUE
yield_under_i(self)
    VALUE self;
{
    return rb_yield_0(self, self, ruby_class, YIELD_PUBLIC_DEF, Qfalse);
}

/* block eval under the class/module context */
static VALUE
yield_under(under, self)
    VALUE under, self;
{
    return exec_under(yield_under_i, under, 0, self);
}

static VALUE
specific_eval(argc, argv, klass, self)
    int argc;
    VALUE *argv;
    VALUE klass, self;
{
    if (rb_block_given_p()) {
	if (argc > 0) {
	    rb_raise(rb_eArgError, "wrong number of arguments (%d for 0)", argc);
	}
	return yield_under(klass, self);
    }
    else {
	char *file = "(eval)";
	int   line = 1;

	if (argc == 0) {
	    rb_raise(rb_eArgError, "block not supplied");
	}
	else {
	    if (ruby_safe_level >= 4) {
		StringValue(argv[0]);
	    }
	    else {
		SafeStringValue(argv[0]);
	    }
	    if (argc > 3) {
		rb_raise(rb_eArgError, "wrong number of arguments: %s(src) or %s{..}",
			 rb_id2name(ruby_frame->callee),
			 rb_id2name(ruby_frame->callee));
	    }
	    if (argc > 2) line = NUM2INT(argv[2]);
	    if (argc > 1) {
		file = StringValuePtr(argv[1]);
	    }
	}
	return eval_under(klass, self, argv[0], file, line);
    }
}

/*
 *  call-seq:
 *     obj.instance_eval(string [, filename [, lineno]] )   => obj
 *     obj.instance_eval {| | block }                       => obj
 *  
 *  Evaluates a string containing Ruby source code, or the given block,
 *  within the context of the receiver (_obj_). In order to set the
 *  context, the variable +self+ is set to _obj_ while
 *  the code is executing, giving the code access to _obj_'s
 *  instance variables. In the version of <code>instance_eval</code>
 *  that takes a +String+, the optional second and third
 *  parameters supply a filename and starting line number that are used
 *  when reporting compilation errors.
 *     
 *     class Klass
 *       def initialize
 *         @secret = 99
 *       end
 *     end
 *     k = Klass.new
 *     k.instance_eval { @secret }   #=> 99
 */

VALUE
rb_obj_instance_eval(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    VALUE klass;

    if (FIXNUM_P(self) || SYMBOL_P(self)) {
	klass = Qnil;
    }
    else {
	klass = rb_singleton_class(self);
    }
    return specific_eval(argc, argv, klass, self);
}

/*
 *  call-seq:
 *     mod.class_eval(string [, filename [, lineno]])  => obj
 *     mod.module_eval {|| block }                     => obj
 *  
 *  Evaluates the string or block in the context of _mod_. This can
 *  be used to add methods to a class. <code>module_eval</code> returns
 *  the result of evaluating its argument. The optional _filename_
 *  and _lineno_ parameters set the text for error messages.
 *     
 *     class Thing
 *     end
 *     a = %q{def hello() "Hello there!" end}
 *     Thing.module_eval(a)
 *     puts Thing.new.hello()
 *     Thing.module_eval("invalid code", "dummy", 123)
 *     
 *  <em>produces:</em>
 *     
 *     Hello there!
 *     dummy:123:in `module_eval': undefined local variable
 *         or method `code' for Thing:Class
 */

VALUE
rb_mod_module_eval(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    return specific_eval(argc, argv, mod, mod);
}

VALUE rb_load_path;

NORETURN(static void load_failed _((VALUE)));

void
rb_load(fname, wrap)
    VALUE fname;
    int wrap;
{
    VALUE tmp;
    int state;
    volatile int prohibit_int = rb_prohibit_interrupt;
    volatile ID callee, this_func;
    volatile VALUE wrapper = ruby_wrapper;
    volatile VALUE self = ruby_top_self;
    NODE * volatile last_node;
    NODE *saved_cref = ruby_cref;
    TMP_PROTECT;

    if (!wrap) rb_secure(4);
    FilePathValue(fname);
    fname = rb_str_new4(fname);
    tmp = rb_find_file(fname);
    if (!tmp) {
	load_failed(fname);
    }
    fname = tmp;

    ruby_errinfo = Qnil;	/* ensure */
    PUSH_VARS();
    PUSH_CLASS(ruby_wrapper);
    ruby_cref = top_cref;
    if (!wrap) {
	rb_secure(4);		/* should alter global state */
	ruby_class = rb_cObject;
	ruby_wrapper = 0;
    }
    else {
	/* load in anonymous module as toplevel */
	ruby_class = ruby_wrapper = rb_module_new();
	self = rb_obj_clone(ruby_top_self);
	rb_extend_object(self, ruby_wrapper);
	PUSH_CREF(ruby_wrapper);
    }
    PUSH_ITER(ITER_NOT);
    PUSH_FRAME();
    ruby_frame->callee = 0;
    ruby_frame->this_func = 0;
    ruby_frame->this_class = 0;
    ruby_frame->self = self;
    PUSH_SCOPE();
    /* default visibility is private at loading toplevel */
    SCOPE_SET(SCOPE_PRIVATE);
    PUSH_TAG(PROT_NONE);
    state = EXEC_TAG();
    callee = ruby_frame->callee;
    this_func = ruby_frame->this_func;
    last_node = ruby_current_node;
    if (!ruby_current_node && ruby_sourcefile) {
	last_node = NEW_BEGIN(0);
    }
    ruby_current_node = 0;
    if (state == 0) {
	NODE * volatile node;
	volatile int critical;

	DEFER_INTS;
	ruby_in_eval++;
	critical = rb_thread_critical;
	rb_thread_critical = Qtrue;
	rb_load_file(RSTRING(fname)->ptr);
	ruby_in_eval--;
	node = ruby_eval_tree;
	rb_thread_critical = critical;
	ALLOW_INTS;
	if (ruby_nerrs == 0) {
	    eval_node(self, node);
	}
    }
    ruby_frame->callee = callee;
    ruby_frame->this_func = this_func;
    ruby_current_node = last_node;
    ruby_sourcefile = 0;
    ruby_set_current_source();
    if (ruby_scope->flags == SCOPE_ALLOCA && ruby_class == rb_cObject) {
	if (ruby_scope->local_tbl) /* toplevel was empty */
	    free(ruby_scope->local_tbl);
    }
    POP_TAG();
    rb_prohibit_interrupt = prohibit_int;
    ruby_cref = saved_cref;
    POP_SCOPE();
    POP_FRAME();
    POP_ITER();
    POP_CLASS();
    POP_VARS();
    ruby_wrapper = wrapper;
    if (ruby_nerrs > 0) {
	ruby_nerrs = 0;
	rb_exc_raise(ruby_errinfo);
    }
    if (state) jump_tag_but_local_jump(state, Qundef);
    if (!NIL_P(ruby_errinfo))	/* exception during load */
	rb_exc_raise(ruby_errinfo);
}

void
rb_load_protect(fname, wrap, state)
    VALUE fname;
    int wrap;
    int *state;
{
    int status;

    PUSH_THREAD_TAG();
    if ((status = EXEC_TAG()) == 0) {
	rb_load(fname, wrap);
    }
    else if (status == TAG_THREAD) {
	rb_thread_start_1();
    }
    POP_THREAD_TAG();
    if (state) *state = status;
}

/*
 *  call-seq:
 *     load(filename, wrap=false)   => true
 *  
 *  Loads and executes the Ruby
 *  program in the file _filename_. If the filename does not
 *  resolve to an absolute path, the file is searched for in the library
 *  directories listed in <code>$:</code>. If the optional _wrap_
 *  parameter is +true+, the loaded script will be executed
 *  under an anonymous module, protecting the calling program's global
 *  namespace. In no circumstance will any local variables in the loaded
 *  file be propagated to the loading environment.
 */


static VALUE
rb_f_load(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE fname, wrap;

    rb_scan_args(argc, argv, "11", &fname, &wrap);
    rb_load(fname, RTEST(wrap));
    return Qtrue;
}

VALUE ruby_dln_librefs;
static VALUE rb_features;
static st_table *loading_tbl;

#define IS_SOEXT(e) (strcmp(e, ".so") == 0 || strcmp(e, ".o") == 0)
#ifdef DLEXT2
#define IS_DLEXT(e) (strcmp(e, DLEXT) == 0 || strcmp(e, DLEXT2) == 0)
#else
#define IS_DLEXT(e) (strcmp(e, DLEXT) == 0)
#endif

static char *
rb_feature_p(feature, ext, rb)
    const char *feature, *ext;
    int rb;
{
    VALUE v;
    char *f, *e;
    long i, len, elen;

    if (ext) {
	len = ext - feature;
	elen = strlen(ext);
    }
    else {
	len = strlen(feature);
	elen = 0;
    }
    for (i = 0; i < RARRAY(rb_features)->len; ++i) {
	v = RARRAY(rb_features)->ptr[i];
	f = StringValuePtr(v);
	if (strncmp(f, feature, len) != 0) continue;
	if (!*(e = f + len)) {
	    if (ext) continue;
	    return e;
	}
	if (*e != '.') continue;
	if ((!rb || !ext) && (IS_SOEXT(e) || IS_DLEXT(e))) {
	    return e;
	}
	if ((rb || !ext) && (strcmp(e, ".rb") == 0)) {
	    return e;
	}
    }
    return 0;
}

static const char *const loadable_ext[] = {
    ".rb", DLEXT,
#ifdef DLEXT2
    DLEXT2,
#endif
    0
};

static int search_required _((VALUE, VALUE *));

int
rb_provided(feature)
    const char *feature;
{
    int i;
    char *buf;
    VALUE fname;

    if (rb_feature_p(feature, 0, Qfalse))
	return Qtrue;
    if (loading_tbl) {
	if (st_lookup(loading_tbl, (st_data_t)feature, 0)) return Qtrue;
	buf = ALLOCA_N(char, strlen(feature)+8);
	strcpy(buf, feature);
	for (i=0; loadable_ext[i]; i++) {
	    strcpy(buf+strlen(feature), loadable_ext[i]);
	    if (st_lookup(loading_tbl, (st_data_t)buf, 0)) return Qtrue;
	}
    }
    if (search_required(rb_str_new2(feature), &fname)) {
	feature = RSTRING(fname)->ptr;
	if (rb_feature_p(feature, 0, Qfalse))
	    return Qtrue;
	if (loading_tbl && st_lookup(loading_tbl, (st_data_t)feature, 0))
	    return Qtrue;
    }
    return Qfalse;
}

static void
rb_provide_feature(feature)
    VALUE feature;
{
    rb_ary_push(rb_features, feature);
}

void
rb_provide(feature)
    const char *feature;
{
    rb_provide_feature(rb_str_new2(feature));
}

static int
load_wait(ftptr)
    char *ftptr;
{
    st_data_t th;

    if (!loading_tbl) return Qfalse;
    if (!st_lookup(loading_tbl, (st_data_t)ftptr, &th)) return Qfalse;
    if ((rb_thread_t)th == curr_thread) return Qtrue;
    do {
	CHECK_INTS;
	rb_thread_schedule();
    } while (st_lookup(loading_tbl, (st_data_t)ftptr, &th));
    return Qtrue;
}

/*
 *  call-seq:
 *     require(string)    => true or false
 *  
 *  Ruby tries to load the library named _string_, returning
 *  +true+ if successful. If the filename does not resolve to
 *  an absolute path, it will be searched for in the directories listed
 *  in <code>$:</code>. If the file has the extension ``.rb'', it is
 *  loaded as a source file; if the extension is ``.so'', ``.o'', or
 *  ``.dll'', or whatever the default shared library extension is on
 *  the current platform, Ruby loads the shared library as a Ruby
 *  extension. Otherwise, Ruby tries adding ``.rb'', ``.so'', and so on
 *  to the name. The name of the loaded feature is added to the array in
 *  <code>$"</code>. A feature will not be loaded if it's name already
 *  appears in <code>$"</code>. However, the file name is not converted
 *  to an absolute path, so that ``<code>require 'a';require
 *  './a'</code>'' will load <code>a.rb</code> twice.
 *     
 *     require "my-library.rb"
 *     require "db-driver"
 */

VALUE
rb_f_require(obj, fname)
    VALUE obj, fname;
{
    return rb_require_safe(fname, ruby_safe_level);
}

static int
search_required(fname, path)
    VALUE fname, *path;
{
    VALUE tmp;
    char *ext, *ftptr;
    int type;

    *path = 0;
    ext = strrchr(ftptr = RSTRING(fname)->ptr, '.');
    if (ext && !strchr(ext, '/')) {
	if (strcmp(".rb", ext) == 0) {
	    if (rb_feature_p(ftptr, ext, Qtrue)) return 'r';
	    if (tmp = rb_find_file(fname)) {
		tmp = rb_file_expand_path(tmp, Qnil);
		ext = strrchr(ftptr = RSTRING(tmp)->ptr, '.');
		if (!rb_feature_p(ftptr, ext, Qtrue))
		    *path = tmp;
		return 'r';
	    }
	    return 0;
	}
	else if (IS_SOEXT(ext)) {
	    if (rb_feature_p(ftptr, ext, Qfalse)) return 's';
	    tmp = rb_str_new(RSTRING(fname)->ptr, ext-RSTRING(fname)->ptr);
#ifdef DLEXT2
	    OBJ_FREEZE(tmp);
	    if (rb_find_file_ext(&tmp, loadable_ext+1)) {
		tmp = rb_file_expand_path(tmp, Qnil);
		ext = strrchr(ftptr = RSTRING(tmp)->ptr, '.');
		if (!rb_feature_p(ftptr, ext, Qfalse))
		    *path = tmp;
		return 's';
	    }
#else
	    rb_str_cat2(tmp, DLEXT);
	    OBJ_FREEZE(tmp);
	    if (tmp = rb_find_file(tmp)) {
		tmp = rb_file_expand_path(tmp, Qnil);
		ext = strrchr(ftptr = RSTRING(tmp)->ptr, '.');
		if (!rb_feature_p(ftptr, ext, Qfalse))
		    *path = tmp;
		return 's';
	    }
#endif
	}
	else if (IS_DLEXT(ext)) {
	    if (rb_feature_p(ftptr, ext, Qfalse)) return 's';
	    if (tmp = rb_find_file(fname)) {
		tmp = rb_file_expand_path(tmp, Qnil);
		ext = strrchr(ftptr = RSTRING(tmp)->ptr, '.');
		if (!rb_feature_p(ftptr, ext, Qfalse))
		    *path = tmp;
		return 's';
	    }
	}
    }
    else if (ext = rb_feature_p(ftptr, 0, Qfalse)) {
	return (*ext && (IS_SOEXT(ext) || IS_DLEXT(ext))) ? 's' : 'r';
    }
    tmp = fname;
    type = rb_find_file_ext(&tmp, loadable_ext);
    tmp = rb_file_expand_path(tmp, Qnil);
    switch (type) {
      case 0:
	ftptr = RSTRING(tmp)->ptr;
	if ((ext = rb_feature_p(ftptr, 0, Qfalse))) {
	    type = strcmp(".rb", ext);
	    break;
	}
	return 0;

      default:
	ext = strrchr(ftptr = RSTRING(tmp)->ptr, '.');
	if (rb_feature_p(ftptr, ext, !--type)) break;
	*path = tmp;
    }
    return type ? 's' : 'r';
}

static void
load_failed(fname)
    VALUE fname;
{
    rb_raise(rb_eLoadError, "no such file to load -- %s", RSTRING(fname)->ptr);
}

VALUE
rb_require_safe(fname, safe)
    VALUE fname;
    int safe;
{
    VALUE result = Qnil;
    volatile VALUE errinfo = ruby_errinfo;
    int state;
    struct {
	NODE *node;
	ID this_func, callee;
	int vmode, safe;
    } volatile saved;
    char *volatile ftptr = 0;

    saved.vmode = scope_vmode;
    saved.node = ruby_current_node;
    saved.callee = ruby_frame->callee;
    saved.this_func = ruby_frame->this_func;
    saved.safe = ruby_safe_level;
    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	VALUE path;
	long handle;
	int found;

	ruby_safe_level = safe;
	FilePathValue(fname);
	*(volatile VALUE *)&fname = rb_str_new4(fname);
	found = search_required(fname, &path);
	if (found) {
	    if (!path || load_wait(RSTRING(path)->ptr)) {
		result = Qfalse;
	    }
	    else {
		ruby_safe_level = 0;
		switch (found) {
		  case 'r':
		    /* loading ruby library should be serialized. */
		    if (!loading_tbl) {
			loading_tbl = st_init_strtable();
		    }
		    /* partial state */
		    ftptr = ruby_strdup(RSTRING(path)->ptr);
		    st_insert(loading_tbl, (st_data_t)ftptr, (st_data_t)curr_thread);
		    rb_load(path, 0);
		    break;

		  case 's':
		    ruby_current_node = 0;
		    ruby_sourcefile = rb_source_filename(RSTRING(path)->ptr);
		    ruby_sourceline = 0;
		    ruby_frame->callee = 0;
		    ruby_frame->this_func = 0;
		    SCOPE_SET(SCOPE_PUBLIC);
		    handle = (long)dln_load(RSTRING(path)->ptr);
		    rb_ary_push(ruby_dln_librefs, LONG2NUM(handle));
		    break;
		}
		rb_provide_feature(path);
		result = Qtrue;
	    }
	}
    }
    POP_TAG();
    ruby_current_node = saved.node;
    ruby_set_current_source();
    ruby_frame->this_func = saved.this_func;
    ruby_frame->callee = saved.callee;
    SCOPE_SET(saved.vmode);
    ruby_safe_level = saved.safe;
    if (ftptr) {
	if (st_delete(loading_tbl, (st_data_t *)&ftptr, 0)) { /* loading done */
	    free(ftptr);
	}
    }
    if (state) JUMP_TAG(state);
    if (NIL_P(result)) {
	load_failed(fname);
    }
    ruby_errinfo = errinfo;

    return result;
}

VALUE
rb_require(fname)
    const char *fname;
{
    VALUE fn = rb_str_new2(fname);
    OBJ_FREEZE(fn);
    return rb_require_safe(fn, ruby_safe_level);
}

static void
secure_visibility(self)
    VALUE self;
{
    if (ruby_safe_level >= 4 && !OBJ_TAINTED(self)) {
	rb_raise(rb_eSecurityError, "Insecure: can't change method visibility");
    }
}

static void
set_method_visibility(self, argc, argv, ex)
    VALUE self;
    int argc;
    VALUE *argv;
    ID ex;
{
    int i;

    secure_visibility(self);
    for (i=0; i<argc; i++) {
	rb_export_method(self, rb_to_id(argv[i]), ex);
    }
    rb_clear_cache_by_class(self);
}

/*
 *  call-seq:
 *     public                 => self
 *     public(symbol, ...)    => self
 *  
 *  With no arguments, sets the default visibility for subsequently
 *  defined methods to public. With arguments, sets the named methods to
 *  have public visibility.
 */

static VALUE
rb_mod_public(argc, argv, module)
    int argc;
    VALUE *argv;
    VALUE module;
{
    secure_visibility(module);
    if (argc == 0) {
	SCOPE_SET(SCOPE_PUBLIC);
    }
    else {
	set_method_visibility(module, argc, argv, NOEX_PUBLIC);
    }
    return module;
}

/*
 *  call-seq:
 *     protected                => self
 *     protected(symbol, ...)   => self
 *  
 *  With no arguments, sets the default visibility for subsequently
 *  defined methods to protected. With arguments, sets the named methods
 *  to have protected visibility.
 */

static VALUE
rb_mod_protected(argc, argv, module)
    int argc;
    VALUE *argv;
    VALUE module;
{
    secure_visibility(module);
    if (argc == 0) {
	SCOPE_SET(SCOPE_PROTECTED);
    }
    else {
	set_method_visibility(module, argc, argv, NOEX_PROTECTED);
    }
    return module;
}

/*
 *  call-seq:
 *     private                 => self
 *     private(symbol, ...)    => self
 *  
 *  With no arguments, sets the default visibility for subsequently
 *  defined methods to private. With arguments, sets the named methods
 *  to have private visibility.
 *     
 *     module Mod
 *       def a()  end
 *       def b()  end
 *       private
 *       def c()  end
 *       private :a
 *     end
 *     Mod.private_instance_methods   #=> ["a", "c"]
 */

static VALUE
rb_mod_private(argc, argv, module)
    int argc;
    VALUE *argv;
    VALUE module;
{
    secure_visibility(module);
    if (argc == 0) {
	SCOPE_SET(SCOPE_PRIVATE);
    }
    else {
	set_method_visibility(module, argc, argv, NOEX_PRIVATE);
    }
    return module;
}

/*
 *  call-seq:
 *     mod.public_class_method(symbol, ...)    => mod
 *  
 *  Makes a list of existing class methods public.
 */

static VALUE
rb_mod_public_method(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    set_method_visibility(CLASS_OF(obj), argc, argv, NOEX_PUBLIC);
    return obj;
}

/*
 *  call-seq:
 *     mod.private_class_method(symbol, ...)   => mod
 *  
 *  Makes existing class methods private. Often used to hide the default
 *  constructor <code>new</code>.
 *     
 *     class SimpleSingleton  # Not thread safe
 *       private_class_method :new
 *       def SimpleSingleton.create(*args, &block)
 *         @me = new(*args, &block) if ! @me
 *         @me
 *       end
 *     end
 */

static VALUE
rb_mod_private_method(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    set_method_visibility(CLASS_OF(obj), argc, argv, NOEX_PRIVATE);
    return obj;
}

/*
 *  call-seq:
 *     public
 *     public(symbol, ...)
 *  
 *  With no arguments, sets the default visibility for subsequently
 *  defined methods to public. With arguments, sets the named methods to
 *  have public visibility.
 */

static VALUE
top_public(argc, argv)
    int argc;
    VALUE *argv;
{
    return rb_mod_public(argc, argv, rb_cObject);
}

static VALUE
top_private(argc, argv)
    int argc;
    VALUE *argv;
{
    return rb_mod_private(argc, argv, rb_cObject);
}

/*
 *  call-seq:
 *     module_function(symbol, ...)    => self
 *  
 *  Creates module functions for the named methods. These functions may
 *  be called with the module as a receiver, and also become available
 *  as instance methods to classes that mix in the module. Module
 *  functions are copies of the original, and so may be changed
 *  independently. The instance-method versions are made private. If
 *  used with no arguments, subsequently defined methods become module
 *  functions.
 *     
 *     module Mod
 *       def one
 *         "This is one"
 *       end
 *       module_function :one
 *     end
 *     class Cls
 *       include Mod
 *       def callOne
 *         one
 *       end
 *     end
 *     Mod.one     #=> "This is one"
 *     c = Cls.new
 *     c.callOne   #=> "This is one"
 *     module Mod
 *       def one
 *         "This is the new one"
 *       end
 *     end
 *     Mod.one     #=> "This is one"
 *     c.callOne   #=> "This is the new one"
 */

static VALUE
rb_mod_modfunc(argc, argv, module)
    int argc;
    VALUE *argv;
    VALUE module;
{
    int i;
    ID id;
    NODE *body;

    if (TYPE(module) != T_MODULE) {
	rb_raise(rb_eTypeError, "module_function must be called for modules");
    }

    secure_visibility(module);
    if (argc == 0) {
	SCOPE_SET(SCOPE_MODFUNC);
	return module;
    }

    set_method_visibility(module, argc, argv, NOEX_PRIVATE);
    for (i=0; i<argc; i++) {
	VALUE m = module;

	id = rb_to_id(argv[i]);
	for (;;) {
	    body = search_method(m, id, &m);
	    if (body == 0) {
		body = search_method(rb_cObject, id, &m);
	    }
	    if (body == 0 || body->nd_body == 0) {
		rb_bug("undefined method `%s'; can't happen", rb_id2name(id));
	    }
	    if (nd_type(body->nd_body) != NODE_ZSUPER) {
		break;		/* normal case: need not to follow 'super' link */
	    }
	    m = RCLASS(m)->super;
	    if (!m) break;
	}
	rb_add_method(rb_singleton_class(module), id, body->nd_body, NOEX_PUBLIC);
    }
    return module;
}

/*
 *  call-seq:
 *     append_features(mod)   => mod
 *  
 *  When this module is included in another, Ruby calls
 *  <code>append_features</code> in this module, passing it the
 *  receiving module in _mod_. Ruby's default implementation is
 *  to add the constants, methods, and module variables of this module
 *  to _mod_ if this module has not already been added to
 *  _mod_ or one of its ancestors. See also <code>Module#include</code>.
 */

static VALUE
rb_mod_append_features(module, include)
    VALUE module, include;
{
    switch (TYPE(include)) {
      case T_CLASS:
      case T_MODULE:
	break;
      default:
	Check_Type(include, T_CLASS);
	break;
    }
    rb_include_module(include, module);

    return module;
}

/*
 *  call-seq:
 *     include(module, ...)    => self
 *  
 *  Invokes <code>Module.append_features</code> on each parameter in turn.
 */

static VALUE
rb_mod_include(argc, argv, module)
    int argc;
    VALUE *argv;
    VALUE module;
{
    int i;

    for (i=0; i<argc; i++) Check_Type(argv[i], T_MODULE);
    while (argc--) {
	rb_funcall(argv[argc], rb_intern("append_features"), 1, module);
	rb_funcall(argv[argc], rb_intern("included"), 1, module);
    }
    return module;
}

void
rb_obj_call_init(obj, argc, argv)
    VALUE obj;
    int argc;
    VALUE *argv;
{
    PUSH_ITER(rb_block_given_p()?ITER_PRE:ITER_NOT);
    rb_funcall2(obj, init, argc, argv);
    POP_ITER();
}

void
rb_extend_object(obj, module)
    VALUE obj, module;
{
    rb_include_module(rb_singleton_class(obj), module);
}

/*
 *  call-seq:
 *     extend_object(obj)    => obj
 *  
 *  Extends the specified object by adding this module's constants and
 *  methods (which are added as singleton methods). This is the callback
 *  method used by <code>Object#extend</code>.
 *     
 *     module Picky
 *       def Picky.extend_object(o)
 *         if String === o
 *           puts "Can't add Picky to a String"
 *         else
 *           puts "Picky added to #{o.class}"
 *           super
 *         end
 *       end
 *     end
 *     (s = Array.new).extend Picky  # Call Object.extend
 *     (s = "quick brown fox").extend Picky
 *     
 *  <em>produces:</em>
 *     
 *     Picky added to Array
 *     Can't add Picky to a String
 */

static VALUE
rb_mod_extend_object(mod, obj)
    VALUE mod, obj;
{
    rb_extend_object(obj, mod);
    return obj;
}

/*
 *  call-seq:
 *     obj.extend(module, ...)    => obj
 *  
 *  Adds to _obj_ the instance methods from each module given as a
 *  parameter.
 *     
 *     module Mod
 *       def hello
 *         "Hello from Mod.\n"
 *       end
 *     end
 *     
 *     class Klass
 *       def hello
 *         "Hello from Klass.\n"
 *       end
 *     end
 *     
 *     k = Klass.new
 *     k.hello         #=> "Hello from Klass.\n"
 *     k.extend(Mod)   #=> #<Klass:0x401b3bc8>
 *     k.hello         #=> "Hello from Mod.\n"
 */

static VALUE
rb_obj_extend(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    int i;

    if (argc == 0) {
	rb_raise(rb_eArgError, "wrong number of arguments (0 for 1)");
    }
    for (i=0; i<argc; i++) Check_Type(argv[i], T_MODULE);
    while (argc--) {
	rb_funcall(argv[argc], rb_intern("extend_object"), 1, obj);
	rb_funcall(argv[argc], rb_intern("extended"), 1, obj);
    }
    return obj;
}

/*
 *  call-seq:
 *     include(module, ...)   => self
 *  
 *  Invokes <code>Module.append_features</code>
 *  on each parameter in turn. Effectively adds the methods and constants
 *  in each module to the receiver.
 */

static VALUE
top_include(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    rb_secure(4);
    if (ruby_wrapper) {
	rb_warning("main#include in the wrapped load is effective only in wrapper module");
	return rb_mod_include(argc, argv, ruby_wrapper);
    }
    return rb_mod_include(argc, argv, rb_cObject);
}

VALUE rb_f_trace_var();
VALUE rb_f_untrace_var();

static void
errinfo_setter(val, id, var)
    VALUE val;
    ID id;
    VALUE *var;
{
    if (!NIL_P(val) && !rb_obj_is_kind_of(val, rb_eException)) {
	rb_raise(rb_eTypeError, "assigning non-exception to $!");
    }
    *var = val;
}

static VALUE
errat_getter(id)
    ID id;
{
    return get_backtrace(ruby_errinfo);
}

static void
errat_setter(val, id, var)
    VALUE val;
    ID id;
    VALUE *var;
{
    if (NIL_P(ruby_errinfo)) {
	rb_raise(rb_eArgError, "$! not set");
    }
    set_backtrace(ruby_errinfo, val);
}

/*
 *  call-seq:
 *     local_variables    => array
 *  
 *  Returns the names of the current local variables.
 *     
 *     fred = 1
 *     for i in 1..10
 *        # ...
 *     end
 *     local_variables   #=> ["fred", "i"]
 */

static VALUE
rb_f_local_variables()
{
    ID *tbl;
    int n, i;
    VALUE ary = rb_ary_new();
    struct RVarmap *vars;

    tbl = ruby_scope->local_tbl;
    if (tbl) {
	n = *tbl++;
	for (i=2; i<n; i++) {	/* skip first 2 ($_ and $~) */
	    if (!rb_is_local_id(tbl[i])) continue; /* skip flip states */
	    rb_ary_push(ary, rb_str_new2(rb_id2name(tbl[i])));
	}
    }

    vars = ruby_dyna_vars;
    while (vars) {
	if (vars->id && rb_is_local_id(vars->id)) { /* skip $_, $~ and flip states */
	    rb_ary_push(ary, rb_str_new2(rb_id2name(vars->id)));
	}
	vars = vars->next;
    }

    return ary;
}

static VALUE rb_f_catch _((VALUE,VALUE));
NORETURN(static VALUE rb_f_throw _((int,VALUE*)));

struct end_proc_data {
    void (*func)();
    VALUE data;
    int safe;
    struct end_proc_data *next;
};

static struct end_proc_data *end_procs, *ephemeral_end_procs, *tmp_end_procs;

void
rb_set_end_proc(func, data)
    void (*func) _((VALUE));
    VALUE data;
{
    struct end_proc_data *link = ALLOC(struct end_proc_data);
    struct end_proc_data **list;

    if (ruby_wrapper) list = &ephemeral_end_procs;
    else              list = &end_procs;
    link->next = *list;
    link->func = func;
    link->data = data;
    link->safe = ruby_safe_level;
    *list = link;
}

void
rb_mark_end_proc()
{
    struct end_proc_data *link;

    link = end_procs;
    while (link) {
	rb_gc_mark(link->data);
	link = link->next;
    }
    link = ephemeral_end_procs;
    while (link) {
	rb_gc_mark(link->data);
	link = link->next;
    }
    link = tmp_end_procs;
    while (link) {
	rb_gc_mark(link->data);
	link = link->next;
    }
}

static void call_end_proc _((VALUE data));

static void
call_end_proc(data)
    VALUE data;
{
    PUSH_ITER(ITER_NOT);
    PUSH_FRAME();
    ruby_frame->self = ruby_frame->prev->self;
    ruby_frame->node = 0;
    ruby_frame->callee = 0;
    ruby_frame->this_func = 0;
    ruby_frame->this_class = 0;
    proc_invoke(data, rb_ary_new2(0), Qundef, 0);
    POP_FRAME();
    POP_ITER();
}

static void
rb_f_END()
{
    PUSH_FRAME();
    ruby_frame->argc = 0;
    ruby_frame->iter = ITER_CUR;
    rb_set_end_proc(call_end_proc, rb_block_proc());
    POP_FRAME();
}

/*
 *  call-seq:
 *     at_exit { block } -> proc
 *  
 *  Converts _block_ to a +Proc+ object (and therefore
 *  binds it at the point of call) and registers it for execution when
 *  the program exits. If multiple handlers are registered, they are
 *  executed in reverse order of registration.
 *     
 *     def do_at_exit(str1)
 *       at_exit { print str1 }
 *     end
 *     at_exit { puts "cruel world" }
 *     do_at_exit("goodbye ")
 *     exit
 *     
 *  <em>produces:</em>
 *     
 *     goodbye cruel world
 */

static VALUE
rb_f_at_exit()
{
    VALUE proc;

    if (!rb_block_given_p()) {
	rb_raise(rb_eArgError, "called without a block");
    }
    proc = rb_block_proc();
    rb_set_end_proc(call_end_proc, proc);
    return proc;
}

void
rb_exec_end_proc()
{
    struct end_proc_data *link, *tmp;
    int status;
    volatile int safe = ruby_safe_level;

    while (ephemeral_end_procs) {
	tmp_end_procs = link = ephemeral_end_procs;
	ephemeral_end_procs = 0;
	while (link) {
	    PUSH_TAG(PROT_NONE);
	    if ((status = EXEC_TAG()) == 0) {
		ruby_safe_level = link->safe;
		(*link->func)(link->data);
	    }
	    POP_TAG();
	    if (status) {
		error_handle(status);
	    }
	    tmp = link;
	    tmp_end_procs = link = link->next;
	    free(tmp);
	}
    }
    while (end_procs) {
	tmp_end_procs = link = end_procs;
	end_procs = 0;
	while (link) {
	    PUSH_TAG(PROT_NONE);
	    if ((status = EXEC_TAG()) == 0) {
		ruby_safe_level = link->safe;
		(*link->func)(link->data);
	    }
	    POP_TAG();
	    if (status) {
		error_handle(status);
	    }
	    tmp = link;
	    tmp_end_procs = link = link->next;
	    free(tmp);
	}
    }
    ruby_safe_level = safe;
}

void
Init_eval()
{
    init = rb_intern("initialize");
    eqq = rb_intern("===");
    each = rb_intern("each");

    aref = rb_intern("[]");
    aset = rb_intern("[]=");
    match = rb_intern("=~");
    missing = rb_intern("method_missing");
    added = rb_intern("method_added");
    singleton_added = rb_intern("singleton_method_added");
    removed = rb_intern("method_removed");
    singleton_removed = rb_intern("singleton_method_removed");
    undefined = rb_intern("method_undefined");
    singleton_undefined = rb_intern("singleton_method_undefined");

    __id__ = rb_intern("__id__");
    __send__ = rb_intern("__send__");

    rb_global_variable((VALUE*)&top_scope);
    rb_global_variable((VALUE*)&ruby_eval_tree);
    rb_global_variable((VALUE*)&ruby_dyna_vars);

    rb_define_virtual_variable("$@", errat_getter, errat_setter);
    rb_define_hooked_variable("$!", &ruby_errinfo, 0, errinfo_setter);

    rb_define_global_function("eval", rb_f_eval, -1);
    rb_define_global_function("iterator?", rb_f_block_given_p, 0);
    rb_define_global_function("block_given?", rb_f_block_given_p, 0);
    rb_define_global_function("method_missing", rb_method_missing, -1);
    rb_define_global_function("loop", rb_f_loop, 0);

    rb_define_method(rb_mKernel, "respond_to?", rb_obj_respond_to, -1);
    respond_to   = rb_intern("respond_to?");
    basic_respond_to = rb_method_node(rb_cObject, respond_to);
    rb_global_variable((VALUE*)&basic_respond_to);
    
    rb_define_global_function("raise", rb_f_raise, -1);
    rb_define_global_function("fail", rb_f_raise, -1);

    rb_define_global_function("caller", rb_f_caller, -1);

    rb_define_global_function("exit", rb_f_exit, -1);
    rb_define_global_function("abort", rb_f_abort, -1);

    rb_define_global_function("at_exit", rb_f_at_exit, 0);

    rb_define_global_function("catch", rb_f_catch, 1);
    rb_define_global_function("throw", rb_f_throw, -1);
    rb_define_global_function("global_variables", rb_f_global_variables, 0); /* in variable.c */
    rb_define_global_function("local_variables", rb_f_local_variables, 0);

    rb_define_method(rb_mKernel, "send", rb_f_send, -1);
    rb_define_method(rb_mKernel, "__send__", rb_f_send, -1);
    rb_define_method(rb_mKernel, "instance_eval", rb_obj_instance_eval, -1);

    rb_define_private_method(rb_cModule, "append_features", rb_mod_append_features, 1);
    rb_define_private_method(rb_cModule, "extend_object", rb_mod_extend_object, 1);
    rb_define_private_method(rb_cModule, "include", rb_mod_include, -1);
    rb_define_private_method(rb_cModule, "public", rb_mod_public, -1);
    rb_define_private_method(rb_cModule, "protected", rb_mod_protected, -1);
    rb_define_private_method(rb_cModule, "private", rb_mod_private, -1);
    rb_define_private_method(rb_cModule, "module_function", rb_mod_modfunc, -1);
    rb_define_method(rb_cModule, "method_defined?", rb_mod_method_defined, 1);
    rb_define_method(rb_cModule, "public_method_defined?", rb_mod_public_method_defined, 1);
    rb_define_method(rb_cModule, "private_method_defined?", rb_mod_private_method_defined, 1);
    rb_define_method(rb_cModule, "protected_method_defined?", rb_mod_protected_method_defined, 1);
    rb_define_method(rb_cModule, "public_class_method", rb_mod_public_method, -1);
    rb_define_method(rb_cModule, "private_class_method", rb_mod_private_method, -1);
    rb_define_method(rb_cModule, "module_eval", rb_mod_module_eval, -1);
    rb_define_method(rb_cModule, "class_eval", rb_mod_module_eval, -1);

    rb_undef_method(rb_cClass, "module_function");

    rb_define_private_method(rb_cModule, "remove_method", rb_mod_remove_method, -1);
    rb_define_private_method(rb_cModule, "undef_method", rb_mod_undef_method, -1);
    rb_define_private_method(rb_cModule, "alias_method", rb_mod_alias_method, 2);
    rb_define_private_method(rb_cModule, "define_method", rb_mod_define_method, -1);

    rb_define_singleton_method(rb_cModule, "nesting", rb_mod_nesting, 0);
    rb_define_singleton_method(rb_cModule, "constants", rb_mod_s_constants, 0);

    rb_define_singleton_method(ruby_top_self, "include", top_include, -1);
    rb_define_singleton_method(ruby_top_self, "public", top_public, -1);
    rb_define_singleton_method(ruby_top_self, "private", top_private, -1);

    rb_define_method(rb_mKernel, "extend", rb_obj_extend, -1);

    rb_define_global_function("trace_var", rb_f_trace_var, -1); /* in variable.c */
    rb_define_global_function("untrace_var", rb_f_untrace_var, -1); /* in variable.c */

    rb_define_global_function("set_trace_func", set_trace_func, 1);
    rb_global_variable(&trace_func);

    rb_define_virtual_variable("$SAFE", safe_getter, safe_setter);
}

/*
 *  call-seq:
 *     mod.autoload(name, filename)   => nil
 *  
 *  Registers _filename_ to be loaded (using <code>Kernel::require</code>)
 *  the first time that _module_ (which may be a <code>String</code> or
 *  a symbol) is accessed in the namespace of _mod_.
 *     
 *     module A
 *     end
 *     A.autoload(:B, "b")
 *     A::B.doit            # autoloads "b"
 */

static VALUE
rb_mod_autoload(mod, sym, file)
    VALUE mod;
    VALUE sym;
    VALUE file;
{
    ID id = rb_to_id(sym);

    Check_SafeStr(file);
    rb_autoload(mod, id, RSTRING(file)->ptr);
    return Qnil;
}

/*
 * MISSING: documentation
 */

static VALUE
rb_mod_autoload_p(mod, sym)
    VALUE mod, sym;
{
    return rb_autoload_p(mod, rb_to_id(sym));
}

/*
 *  call-seq:
 *     autoload(module, filename)   => nil
 *  
 *  Registers _filename_ to be loaded (using <code>Kernel::require</code>)
 *  the first time that _module_ (which may be a <code>String</code> or
 *  a symbol) is accessed.
 *     
 *     autoload(:MyModule, "/usr/local/lib/modules/my_module.rb")
 */

static VALUE
rb_f_autoload(obj, sym, file)
    VALUE obj;
    VALUE sym;
    VALUE file;
{
    return rb_mod_autoload(ruby_cbase, sym, file);
}


/*
 * MISSING: documentation
 */

static VALUE
rb_f_autoload_p(obj, sym)
    VALUE obj;
    VALUE sym;
{
    /* use ruby_cbase as same as rb_f_autoload. */
    return rb_mod_autoload_p(ruby_cbase, sym);
}

void
Init_load()
{
    rb_load_path = rb_ary_new();
    rb_define_readonly_variable("$:", &rb_load_path);
    rb_define_readonly_variable("$-I", &rb_load_path);
    rb_define_readonly_variable("$LOAD_PATH", &rb_load_path);

    rb_features = rb_ary_new();
    rb_define_readonly_variable("$\"", &rb_features);
    rb_define_readonly_variable("$LOADED_FEATURES", &rb_features);

    rb_define_global_function("load", rb_f_load, -1);
    rb_define_global_function("require", rb_f_require, 1);
    rb_define_method(rb_cModule, "autoload",  rb_mod_autoload,   2);
    rb_define_method(rb_cModule, "autoload?", rb_mod_autoload_p, 1);
    rb_define_global_function("autoload",  rb_f_autoload,   2);
    rb_define_global_function("autoload?", rb_f_autoload_p, 1);
    rb_global_variable(&ruby_wrapper);

    ruby_dln_librefs = rb_ary_new();
    rb_global_variable(&ruby_dln_librefs);
}

static void
scope_dup(scope)
    struct SCOPE *scope;
{
    volatile ID *tbl;
    VALUE *vars;

    scope->flags |= SCOPE_DONT_RECYCLE;
    if (scope->flags & SCOPE_MALLOC) return;

    if (scope->local_tbl) {
	tbl = scope->local_tbl;
	vars = ALLOC_N(VALUE, tbl[0]+1);
	*vars++ = scope->local_vars[-1];
	MEMCPY(vars, scope->local_vars, VALUE, tbl[0]);
	scope->local_vars = vars;
	scope->flags |= SCOPE_MALLOC;
    }
}

static void
blk_mark(data)
    struct BLOCK *data;
{
    while (data) {
	rb_gc_mark_frame(&data->frame);
	rb_gc_mark((VALUE)data->scope);
	rb_gc_mark((VALUE)data->var);
	rb_gc_mark((VALUE)data->body);
	rb_gc_mark((VALUE)data->self);
	rb_gc_mark((VALUE)data->dyna_vars);
	rb_gc_mark((VALUE)data->cref);
	rb_gc_mark(data->wrapper);
	rb_gc_mark(data->block_obj);
	data = data->prev;
    }
}

static void
frame_free(frame)
    struct FRAME *frame;
{
    struct FRAME *tmp;

    frame = frame->prev;
    while (frame) {
	tmp = frame;
	frame = frame->prev;
	free(tmp);
    }
}

static void
blk_free(data)
    struct BLOCK *data;
{
    void *tmp;

    while (data) {
	frame_free(&data->frame);
	tmp = data;
	data = data->prev;
	free(tmp);
    }
}

static void
frame_dup(frame)
    struct FRAME *frame;
{
    struct FRAME *tmp;

    for (;;) {
	frame->tmp = 0;		/* should not preserve tmp */
	if (!frame->prev) break;
	tmp = ALLOC(struct FRAME);
	*tmp = *frame->prev;
	frame->prev = tmp;
	frame = tmp;
    }
}

static void
blk_copy_prev(block)
    struct BLOCK *block;
{
    struct BLOCK *tmp;
    struct RVarmap* vars;

    while (block->prev) {
	tmp = ALLOC_N(struct BLOCK, 1);
	MEMCPY(tmp, block->prev, struct BLOCK, 1);
	scope_dup(tmp->scope);
	frame_dup(&tmp->frame);

	for (vars = tmp->dyna_vars; vars; vars = vars->next) {
	    if (FL_TEST(vars, DVAR_DONT_RECYCLE)) break;
	    FL_SET(vars, DVAR_DONT_RECYCLE);
	}

	block->prev = tmp;
	block = tmp;
    }
}


static void
blk_dup(dup, orig)
    struct BLOCK *dup, *orig;
{
    MEMCPY(dup, orig, struct BLOCK, 1);
    frame_dup(&dup->frame);

    if (dup->iter) {
	blk_copy_prev(dup);
    }
    else {
	dup->prev = 0;
    }
}

/*
 * MISSING: documentation
 */

static VALUE
proc_clone(self)
    VALUE self;
{
    struct BLOCK *orig, *data;
    VALUE bind;

    Data_Get_Struct(self, struct BLOCK, orig);
    bind = Data_Make_Struct(rb_obj_class(self),struct BLOCK,blk_mark,blk_free,data);
    CLONESETUP(bind, self);
    blk_dup(data, orig);

    return bind;
}

/*
 * MISSING: documentation
 */

static VALUE
proc_dup(self)
    VALUE self;
{
    struct BLOCK *orig, *data;
    VALUE bind;

    Data_Get_Struct(self, struct BLOCK, orig);
    bind = Data_Make_Struct(rb_obj_class(self),struct BLOCK,blk_mark,blk_free,data);
    blk_dup(data, orig);

    return bind;
}

/*
 *  call-seq:
 *     binding -> a_binding
 *  
 *  Returns a +Binding+ object, describing the variable and
 *  method bindings at the point of call. This object can be used when
 *  calling +eval+ to execute the evaluated command in this
 *  environment. Also see the description of class +Binding+.
 *     
 *     def getBinding(param)
 *       return binding
 *     end
 *     b = getBinding("hello")
 *     eval("param", b)   #=> "hello"
 */

static VALUE
rb_f_binding(self)
    VALUE self;
{
    struct BLOCK *data, *p;
    struct RVarmap *vars;
    VALUE bind;

    PUSH_BLOCK(0,0);
    bind = Data_Make_Struct(rb_cBinding,struct BLOCK,blk_mark,blk_free,data);
    *data = *ruby_block;

    data->orig_thread = rb_thread_current();
    data->wrapper = ruby_wrapper;
    data->iter = rb_f_block_given_p();
    frame_dup(&data->frame);
    if (ruby_frame->prev) {
	data->frame.callee = ruby_frame->prev->callee;
	data->frame.this_func = ruby_frame->prev->this_func;
	data->frame.this_class = ruby_frame->prev->this_class;
    }

    if (data->iter) {
	blk_copy_prev(data);
    }
    else {
	data->prev = 0;
    }

    for (p = data; p; p = p->prev) {
	for (vars = p->dyna_vars; vars; vars = vars->next) {
	    if (FL_TEST(vars, DVAR_DONT_RECYCLE)) break;
	    FL_SET(vars, DVAR_DONT_RECYCLE);
	}
    }
    scope_dup(data->scope);
    POP_BLOCK();

    return bind;
}

/*
 *  call-seq:
 *     binding.eval(string [, filename [,lineno]])  => obj
 *
 *  Evaluates the Ruby expression(s) in <em>string</em>, in the
 *  <em>binding</em>'s context.  If the optional <em>filename</em> and
 *  <em>lineno</em> parameters are present, they will be used when
 *  reporting syntax errors.
 *
 *     def getBinding(param)
 *       return binding
 *     end
 *     b = getBinding("hello")
 *     b.eval("param")   #=> "hello"
 */

static VALUE
bind_eval(argc, argv, bind)
    int argc;
    VALUE *argv;
    VALUE bind;
{
    struct BLOCK *data;
    VALUE args[4];

    rb_scan_args(argc, argv, "12", &args[0], &args[2], &args[3]);
    args[1] = bind;
    Data_Get_Struct(bind, struct BLOCK, data);

    return rb_f_eval(argc+1, args, data->self);
}

#define PROC_TSHIFT (FL_USHIFT+1)
#define PROC_TMASK  (FL_USER1|FL_USER2|FL_USER3)
#define PROC_TMAX   (PROC_TMASK >> PROC_TSHIFT)
#define PROC_NOSAFE FL_USER4

#define SAFE_LEVEL_MAX PROC_TMASK

#define proc_safe_level_p(data) (!(RBASIC(data)->flags & PROC_NOSAFE))

static void
proc_save_safe_level(data)
    VALUE data;
{
    int safe = ruby_safe_level;
    if (safe > PROC_TMAX) safe = PROC_TMAX;
    FL_SET(data, (safe << PROC_TSHIFT) & PROC_TMASK);
}

static int
proc_get_safe_level(data)
    VALUE data;
{
    return (RBASIC(data)->flags & PROC_TMASK) >> PROC_TSHIFT;
}

static void
proc_set_safe_level(data)
    VALUE data;
{
    if (!proc_safe_level_p(data)) return;
    ruby_safe_level = proc_get_safe_level(data);
}

static VALUE
proc_alloc(klass, proc)
    VALUE klass;
    int proc;
{
    volatile VALUE block;
    struct BLOCK *data, *p;
    struct RVarmap *vars;

    if (!rb_block_given_p() && !rb_f_block_given_p()) {
	rb_raise(rb_eArgError, "tried to create Proc object without a block");
    }
    if (proc && !rb_block_given_p()) {
	rb_warn("tried to create Proc object without a block");
    }

    if (!proc && ruby_block->block_obj) {
	VALUE obj = ruby_block->block_obj;
	if (CLASS_OF(obj) != klass) {
	    obj = proc_clone(obj);
	    RBASIC(obj)->klass = klass;
	}
	return obj;
    }
    block = Data_Make_Struct(klass, struct BLOCK, blk_mark, blk_free, data);
    *data = *ruby_block;

    data->orig_thread = rb_thread_current();
    data->wrapper = ruby_wrapper;
    data->iter = data->prev?Qtrue:Qfalse;
    data->block_obj = block;
    frame_dup(&data->frame);
    if (data->iter) {
	blk_copy_prev(data);
    }
    else {
	data->prev = 0;
    }

    for (p = data; p; p = p->prev) {
	for (vars = p->dyna_vars; vars; vars = vars->next) {
	    if (FL_TEST(vars, DVAR_DONT_RECYCLE)) break;
	    FL_SET(vars, DVAR_DONT_RECYCLE);
	}
    }
    scope_dup(data->scope);
    proc_save_safe_level(block);
    if (proc) {
	data->flags |= BLOCK_LAMBDA;
    }
    else {
	ruby_block->block_obj = block;
    }

    return block;
}

/*
 *  call-seq:
 *     Proc.new {|...| block } => a_proc
 *     Proc.new                => a_proc
 *  
 *  Creates a new <code>Proc</code> object, bound to the current
 *  context. <code>Proc::new</code> may be called without a block only
 *  within a method with an attached block, in which case that block is
 *  converted to the <code>Proc</code> object.
 *     
 *     def proc_from
 *       Proc.new
 *     end
 *     proc = proc_from { "hello" }
 *     proc.call   #=> "hello"
 */

static VALUE
proc_s_new(argc, argv, klass)
    int argc;
    VALUE *argv;
    VALUE klass;
{
    VALUE block = proc_alloc(klass, Qfalse);

    rb_obj_call_init(block, argc, argv);
    return block;
}

/*
 * call-seq:
 *   proc   { |...| block }  => a_proc
 *
 * Equivalent to <code>Proc.new</code>.
 */

VALUE
rb_block_proc()
{
    return proc_alloc(rb_cProc, Qfalse);
}

VALUE
rb_f_lambda()
{
    rb_warn("rb_f_lambda() is deprecated; use rb_block_proc() instead");
    return proc_alloc(rb_cProc, Qtrue);
}

/*
 * call-seq:
 *   lambda { |...| block }  => a_proc
 *
 * Equivalent to <code>Proc.new</code>, except the resulting Proc objects
 * check the number of parameters passed when called.
 */

static VALUE
proc_lambda()
{
    return proc_alloc(rb_cProc, Qtrue);
}

static int
block_orphan(data)
    struct BLOCK *data;
{
    if (data->scope->flags & SCOPE_NOSTACK) {
	return 1;
    }
    if (data->orig_thread != rb_thread_current()) {
	return 1;
    }
    return 0;
}

static VALUE
proc_invoke(proc, args, self, klass)
    VALUE proc, args;		/* OK */
    VALUE self, klass;
{
    struct BLOCK * volatile old_block;
    struct BLOCK _block;
    struct BLOCK *data;
    volatile VALUE result = Qundef;
    int state;
    volatile int safe = ruby_safe_level;
    volatile VALUE old_wrapper = ruby_wrapper;
    volatile int pcall, avalue = Qtrue;
    VALUE bvar = Qnil, tmp = args;

    Data_Get_Struct(proc, struct BLOCK, data);
    pcall = (data->flags & BLOCK_LAMBDA) ? YIELD_LAMBDA_CALL : 0;
    if (!pcall && RARRAY(args)->len == 1) {
	avalue = Qfalse;
	args = RARRAY(args)->ptr[0];
    }
    if (rb_block_given_p() && ruby_frame->callee) {
	if (klass != ruby_frame->this_class)
	    klass = rb_obj_class(proc);
	bvar = rb_block_proc();
    }

    PUSH_VARS();
    ruby_wrapper = data->wrapper;
    ruby_dyna_vars = data->dyna_vars;
    /* PUSH BLOCK from data */
    old_block = ruby_block;
    _block = *data;
    _block.block_obj = bvar;
    if (self != Qundef) _block.frame.self = self;
    if (klass) _block.frame.this_class = klass;
    _block.frame.argc = RARRAY(tmp)->len;
    if (_block.frame.argc && (ruby_frame->flags & FRAME_DMETH)) {
        NEWOBJ(scope, struct SCOPE);
        OBJSETUP(scope, tmp, T_SCOPE);
        scope->local_tbl = _block.scope->local_tbl;
        scope->local_vars = _block.scope->local_vars;
        _block.scope = scope;
    }
    ruby_block = &_block;

    PUSH_ITER(ITER_CUR);
    ruby_frame->iter = ITER_CUR;
    PUSH_TAG((pcall&YIELD_LAMBDA_CALL) ? PROT_LAMBDA : PROT_NONE);
    state = EXEC_TAG();
    if (state == 0) {
	proc_set_safe_level(proc);
	result = rb_yield_0(args, self, (self!=Qundef)?CLASS_OF(self):0,
			    pcall | YIELD_PROC_CALL, avalue);
    }
    else if (TAG_DST()) {
	result = prot_tag->retval;
    }
    POP_TAG();
    POP_ITER();
    ruby_block = old_block;
    ruby_wrapper = old_wrapper;
    POP_VARS();
    if (proc_safe_level_p(proc)) ruby_safe_level = safe;

    switch (state) {
      case 0:
	break;
      case TAG_RETRY:
	proc_jump_error(TAG_RETRY, Qnil); /* xxx */
	JUMP_TAG(state);
	break;
      case TAG_BREAK:
	if (!pcall && result != Qundef) {
	    proc_jump_error(state, result);
	}
      case TAG_RETURN:
	if (result != Qundef) {
	    if (pcall) break;
	    return_jump(result);
	}
      default:
	JUMP_TAG(state);
    }
    return result;
}

/* CHECKME: are the argument checking semantics correct? */

/*
 *  call-seq:
 *     prc.call(params,...)   => obj
 *     prc[params,...]        => obj
 *  
 *  Invokes the block, setting the block's parameters to the values in
 *  <i>params</i> using something close to method calling semantics.
 *  Generates a warning if multiple values are passed to a proc that
 *  expects just one (previously this silently converted the parameters
 *  to an array).
 *
 *  For procs created using <code>Kernel.proc</code>, generates an
 *  error if the wrong number of parameters
 *  are passed to a proc with multiple parameters. For procs created using
 *  <code>Proc.new</code>, extra parameters are silently discarded.
 *
 *  Returns the value of the last expression evaluated in the block. See
 *  also <code>Proc#yield</code>.
 *     
 *     a_proc = Proc.new {|a, *b| b.collect {|i| i*a }}
 *     a_proc.call(9, 1, 2, 3)   #=> [9, 18, 27]
 *     a_proc[9, 1, 2, 3]        #=> [9, 18, 27]
 *     a_proc = Proc.new {|a,b| a}
 *     a_proc.call(1,2,3)
 *     
 *  <em>produces:</em>
 *     
 *     prog.rb:5: wrong number of arguments (3 for 2) (ArgumentError)
 *     	from prog.rb:4:in `call'
 *     	from prog.rb:5
 */

static VALUE
proc_call(proc, args)
    VALUE proc, args;		/* OK */
{
    return proc_invoke(proc, args, Qundef, 0);
}

int
rb_proc_arity(proc)
    VALUE proc;
{
    struct BLOCK *data;
    NODE *var, *list;
    int n;

    Data_Get_Struct(proc, struct BLOCK, data);
    var = data->var;
    if (var == 0) {
	if (data->body && nd_type(data->body) == NODE_IFUNC &&
	    data->body->nd_cfnc == bmcall) {
	    return method_arity(data->body->nd_tval);
	}
	return 0;
    }
    if (var == (NODE*)1) return 0;
    if (var == (NODE*)2) return 0;
    if (nd_type(var) == NODE_BLOCK_ARG) {
	var = var->nd_args;
	if (var == (NODE*)1) return 0;
	if (var == (NODE*)2) return 0;
    }
    switch (nd_type(var)) {
      default:
	return 1;
      case NODE_MASGN:
	list = var->nd_head;
	n = 0;
	while (list) {
	    n++;
	    list = list->nd_next;
	}
	if (var->nd_args) return -n-1;
	return n;
    }
}

/*
 *  call-seq:
 *     prc.arity -> fixnum
 *  
 *  Returns the number of arguments that would not be ignored. If the block
 *  is declared to take no arguments, returns 0. If the block is known
 *  to take exactly n arguments, returns n. If the block has optional
 *  arguments, return -n-1, where n is the number of mandatory
 *  arguments. A <code>proc</code> with no argument declarations
 *  is the same a block declaring <code>||</code> as its arguments.
 *     
 *     Proc.new {}.arity          #=>  0
 *     Proc.new {||}.arity        #=>  0
 *     Proc.new {|a|}.arity       #=>  1
 *     Proc.new {|a,b|}.arity     #=>  2
 *     Proc.new {|a,b,c|}.arity   #=>  3
 *     Proc.new {|*a|}.arity      #=> -1
 *     Proc.new {|a,*b|}.arity    #=> -2
 */

static VALUE
proc_arity(proc)
    VALUE proc;
{
    int arity = rb_proc_arity(proc);
    return INT2FIX(arity);
}

/*
 * call-seq:
 *   prc == other_proc   =>  true or false
 *
 * Return <code>true</code> if <i>prc</i> is the same object as
 * <i>other_proc</i>, or if they are both procs with the same body.
 */

static VALUE
proc_eq(self, other)
    VALUE self, other;
{
    struct BLOCK *data, *data2;

    if (self == other) return Qtrue;
    if (TYPE(other) != T_DATA) return Qfalse;
    if (RDATA(other)->dmark != (RUBY_DATA_FUNC)blk_mark) return Qfalse;
    if (CLASS_OF(self) != CLASS_OF(other)) return Qfalse;
    Data_Get_Struct(self, struct BLOCK, data);
    Data_Get_Struct(other, struct BLOCK, data2);
    if (data->body != data2->body) return Qfalse;
    if (data->var != data2->var) return Qfalse;
    if (data->scope != data2->scope) return Qfalse;
    if (data->dyna_vars != data2->dyna_vars) return Qfalse;
    if (data->flags != data2->flags) return Qfalse;

    return Qtrue;
}

/*
 * call-seq:
 *   prc.hash   =>  integer
 *
 * Return hash value corresponding to proc body.
 */

static VALUE
proc_hash(self)
    VALUE self;
{
    struct BLOCK *data;
    long hash;

    Data_Get_Struct(self, struct BLOCK, data);
    hash = (long)data->body;
    hash ^= (long)data->var;
    hash ^= data->frame.uniq << 16;
    hash ^= data->flags;

    return INT2FIX(hash);
}

/*
 * call-seq:
 *   prc.to_s   => string
 *
 * Shows the unique identifier for this proc, along with
 * an indication of where the proc was defined.
 */

static VALUE
proc_to_s(self)
    VALUE self;
{
    struct BLOCK *data;
    NODE *node;
    char *cname = rb_obj_classname(self);
    const int w = (SIZEOF_LONG * CHAR_BIT) / 4;
    long len = strlen(cname)+6+w; /* 6:tags 16:addr */
    VALUE str;

    Data_Get_Struct(self, struct BLOCK, data);
    if ((node = data->frame.node) || (node = data->body)) {
	len += strlen(node->nd_file) + 2 + (SIZEOF_LONG*CHAR_BIT-NODE_LSHIFT)/3;
	str = rb_str_new(0, len);
	sprintf(RSTRING(str)->ptr, "#<%s:0x%.*lx@%s:%d>", cname, w, (VALUE)data->body,
		node->nd_file, nd_line(node));
    }
    else {
	str = rb_str_new(0, len);
	sprintf(RSTRING(str)->ptr, "#<%s:0x%.*lx>", cname, w, (VALUE)data->body);
    }
    RSTRING(str)->len = strlen(RSTRING(str)->ptr);
    if (OBJ_TAINTED(self)) OBJ_TAINT(str);

    return str;
}

/*
 *  call-seq:
 *     prc.to_proc -> prc
 *  
 *  Part of the protocol for converting objects to <code>Proc</code>
 *  objects. Instances of class <code>Proc</code> simply return
 *  themselves.
 */

static VALUE
proc_to_self(self)
    VALUE self;
{
    return self;
}

/*
 *  call-seq:
 *     prc.binding    => binding
 *  
 *  Returns the binding associated with <i>prc</i>. Note that
 *  <code>Kernel#eval</code> accepts either a <code>Proc</code> or a
 *  <code>Binding</code> object as its second parameter.
 *     
 *     def fred(param)
 *       proc {}
 *     end
 *     
 *     b = fred(99)
 *     eval("param", b.binding)   #=> 99
 *     eval("param", b)           #=> 99
 */

static VALUE
proc_binding(proc)
    VALUE proc;
{
    struct BLOCK *orig, *data;
    VALUE bind;

    Data_Get_Struct(proc, struct BLOCK, orig);
    bind = Data_Make_Struct(rb_cBinding,struct BLOCK,blk_mark,blk_free,data);
    MEMCPY(data, orig, struct BLOCK, 1);
    frame_dup(&data->frame);

    if (data->iter) {
	blk_copy_prev(data);
    }
    else {
	data->prev = 0;
    }

    return bind;
}

static VALUE
rb_block_pass(func, arg, proc)
    VALUE (*func) _((VALUE));
    VALUE arg;
    VALUE proc;
{
    VALUE b;
    struct BLOCK * volatile old_block;
    struct BLOCK _block;
    struct BLOCK *data;
    volatile VALUE result = Qnil;
    int state;
    volatile int orphan;
    volatile int safe = ruby_safe_level;

    if (NIL_P(proc)) {
	PUSH_ITER(ITER_NOT);
	result = (*func)(arg);
	POP_ITER();
	return result;
    }
    if (!rb_obj_is_proc(proc)) {
	b = rb_check_convert_type(proc, T_DATA, "Proc", "to_proc");
	if (!rb_obj_is_proc(b)) {
	    rb_raise(rb_eTypeError, "wrong argument type %s (expected Proc)",
		     rb_obj_classname(proc));
	}
	proc = b;
    }

    if (ruby_safe_level >= 1 && OBJ_TAINTED(proc)) {
	if (ruby_safe_level > proc_get_safe_level(proc)) {
	    rb_raise(rb_eSecurityError, "Insecure: tainted block value");
	}
    }

    if (ruby_block && ruby_block->block_obj == proc) {
	PUSH_ITER(ITER_PRE);
	result = (*func)(arg);
	POP_ITER();
	return result;
    }

    Data_Get_Struct(proc, struct BLOCK, data);
    orphan = block_orphan(data);

    /* PUSH BLOCK from data */
    _block = *data;
    _block.outer = ruby_block;
    if (orphan) _block.uniq = block_unique++;
    ruby_block = &_block;
    PUSH_ITER(ITER_PRE);
    if (ruby_frame->iter == ITER_NOT)
	ruby_frame->iter = ITER_PRE;

    PUSH_TAG(PROT_LOOP);
    state = EXEC_TAG();
    if (state == 0) {
      retry:
	proc_set_safe_level(proc);
	if (safe > ruby_safe_level)
	    ruby_safe_level = safe;
	result = (*func)(arg);
    }
    else if (state == TAG_BREAK && TAG_DST()) {
	result = prot_tag->retval;
	state = 0;
    }
    else if (state == TAG_RETRY) {
	state = 0;
	goto retry;
    }
    POP_TAG();
    POP_ITER();
    ruby_block = _block.outer;
    if (proc_safe_level_p(proc)) ruby_safe_level = safe;

    switch (state) {/* escape from orphan block */
      case 0:
	break;
      case TAG_RETURN:
	if (orphan) {
	    proc_jump_error(state, prot_tag->retval);
	}
      default:
	JUMP_TAG(state);
    }

    return result;
}

struct block_arg {
    VALUE self;
    NODE *iter;
};

static VALUE
call_block(arg)
    struct block_arg *arg;
{
    return rb_eval(arg->self, arg->iter);
}

static VALUE
block_pass(self, node)
    VALUE self;
    NODE *node;
{
    struct block_arg arg;
    arg.self = self;
    arg.iter = node->nd_iter;
    return rb_block_pass((VALUE (*)_((VALUE)))call_block,
			 (VALUE)&arg, rb_eval(self, node->nd_body));
}

struct METHOD {
    VALUE klass, rklass;
    VALUE recv;
    ID id, oid;
    NODE *body;
};

static void
bm_mark(data)
    struct METHOD *data;
{
    rb_gc_mark(data->rklass);
    rb_gc_mark(data->klass);
    rb_gc_mark(data->recv);
    rb_gc_mark((VALUE)data->body);
}

static VALUE
mnew(klass, obj, id, mklass)
    VALUE klass, obj, mklass;
    ID id;
{
    VALUE method;
    NODE *body;
    int noex;
    struct METHOD *data;
    VALUE rklass = klass;
    ID oid = id;

  again:
    if ((body = rb_get_method_body(&klass, &id, &noex)) == 0) {
	print_undef(rklass, oid);
    }

    if (nd_type(body) == NODE_ZSUPER) {
	klass = RCLASS(klass)->super;
	goto again;
    }

    while (rklass != klass &&
	   (FL_TEST(rklass, FL_SINGLETON) || TYPE(rklass) == T_ICLASS)) {
	rklass = RCLASS(rklass)->super;
    }
    if (TYPE(klass) == T_ICLASS) klass = RBASIC(klass)->klass;
    method = Data_Make_Struct(mklass, struct METHOD, bm_mark, -1, data);
    data->klass = klass;
    data->recv = obj;
    data->id = id;
    data->body = body;
    data->rklass = rklass;
    data->oid = oid;
    OBJ_INFECT(method, klass);

    return method;
}


/**********************************************************************
 *
 * Document-class : Method
 *
 *  Method objects are created by <code>Object#method</code>, and are
 *  associated with a particular object (not just with a class). They
 *  may be used to invoke the method within the object, and as a block
 *  associated with an iterator. They may also be unbound from one
 *  object (creating an <code>UnboundMethod</code>) and bound to
 *  another.
 *     
 *     class Thing
 *       def square(n)
 *         n*n
 *       end
 *     end
 *     thing = Thing.new
 *     meth  = thing.method(:square)
 *     
 *     meth.call(9)                 #=> 81
 *     [ 1, 2, 3 ].collect(&meth)   #=> [1, 4, 9]
 *     
 */

/*
 * call-seq:
 *   meth == other_meth  => true or false
 *
 * Two method objects are equal if that are bound to the same
 * object and contain the same body.
 */


static VALUE
method_eq(method, other)
    VALUE method, other;
{
    struct METHOD *m1, *m2;

    if (TYPE(other) != T_DATA || RDATA(other)->dmark != (RUBY_DATA_FUNC)bm_mark)
	return Qfalse;
    if (CLASS_OF(method) != CLASS_OF(other))
	return Qfalse;

    Data_Get_Struct(method, struct METHOD, m1);
    Data_Get_Struct(other, struct METHOD, m2);

    if (m1->klass != m2->klass || m1->rklass != m2->rklass ||
	m1->recv != m2->recv || m1->body != m2->body)
	return Qfalse;

    return Qtrue;
}

/*
 * call-seq:
 *    meth.hash   => integer
 *
 * Return a hash value corresponding to the method object.
 */

static VALUE
method_hash(method)
    VALUE method;
{
    struct METHOD *m;
    long hash;

    Data_Get_Struct(method, struct METHOD, m);
    hash = (long)m->klass;
    hash ^= (long)m->rklass;
    hash ^= (long)m->recv;
    hash ^= (long)m->body;

    return INT2FIX(hash);
}

/*
 *  call-seq:
 *     meth.unbind    => unbound_method
 *  
 *  Dissociates <i>meth</i> from it's current receiver. The resulting
 *  <code>UnboundMethod</code> can subsequently be bound to a new object
 *  of the same class (see <code>UnboundMethod</code>).
 */

static VALUE
method_unbind(obj)
    VALUE obj;
{
    VALUE method;
    struct METHOD *orig, *data;

    Data_Get_Struct(obj, struct METHOD, orig);
    method = Data_Make_Struct(rb_cUnboundMethod, struct METHOD, bm_mark, free, data);
    data->klass = orig->klass;
    data->recv = Qundef;
    data->id = orig->id;
    data->body = orig->body;
    data->rklass = orig->rklass;
    data->oid = orig->oid;
    OBJ_INFECT(method, obj);

    return method;
}

/*
 *  call-seq:
 *     obj.method(sym)    => method
 *  
 *  Looks up the named method as a receiver in <i>obj</i>, returning a
 *  <code>Method</code> object (or raising <code>NameError</code>). The
 *  <code>Method</code> object acts as a closure in <i>obj</i>'s object
 *  instance, so instance variables and the value of <code>self</code>
 *  remain available.
 *     
 *     class Demo
 *       def initialize(n)
 *         @iv = n
 *       end
 *       def hello()
 *         "Hello, @iv = #{@iv}"
 *       end
 *     end
 *     
 *     k = Demo.new(99)
 *     m = k.method(:hello)
 *     m.call   #=> "Hello, @iv = 99"
 *     
 *     l = Demo.new('Fred')
 *     m = l.method("hello")
 *     m.call   #=> "Hello, @iv = Fred"
 */

static VALUE
rb_obj_method(obj, vid)
    VALUE obj;
    VALUE vid;
{
    return mnew(CLASS_OF(obj), obj, rb_to_id(vid), rb_cMethod);
}

/*
 *  call-seq:
 *     mod.instance_method(symbol)   => unbound_method
 *  
 *  Returns an +UnboundMethod+ representing the given
 *  instance method in _mod_.
 *     
 *     class Interpreter
 *       def do_a() print "there, "; end
 *       def do_d() print "Hello ";  end
 *       def do_e() print "!\n";     end
 *       def do_v() print "Dave";    end
 *       Dispatcher = {
 *        ?a => instance_method(:do_a),
 *        ?d => instance_method(:do_d),
 *        ?e => instance_method(:do_e),
 *        ?v => instance_method(:do_v)
 *       }
 *       def interpret(string)
 *         string.each_byte {|b| Dispatcher[b].bind(self).call }
 *       end
 *     end
 *     
 *     
 *     interpreter = Interpreter.new
 *     interpreter.interpret('dave')
 *     
 *  <em>produces:</em>
 *     
 *     Hello there, Dave!
 */

static VALUE
rb_mod_method(mod, vid)
    VALUE mod;
    VALUE vid;
{
    return mnew(mod, Qundef, rb_to_id(vid), rb_cUnboundMethod);
}

/*
 * MISSING: documentation
 */

static VALUE
method_clone(self)
    VALUE self;
{
    VALUE clone;
    struct METHOD *orig, *data;

    Data_Get_Struct(self, struct METHOD, orig);
    clone = Data_Make_Struct(CLASS_OF(self),struct METHOD, bm_mark, free, data);
    CLONESETUP(clone, self);
    *data = *orig;

    return clone;
}

/*
 *  call-seq:
 *     meth.call(args, ...)    => obj
 *     meth[args, ...]         => obj
 *  
 *  Invokes the <i>meth</i> with the specified arguments, returning the
 *  method's return value.
 *     
 *     m = 12.method("+")
 *     m.call(3)    #=> 15
 *     m.call(20)   #=> 32
 */

static VALUE
method_call(argc, argv, method)
    int argc;
    VALUE *argv;
    VALUE method;
{
    VALUE result = Qnil;	/* OK */
    struct METHOD *data;
    int state;
    volatile int safe = -1;

    Data_Get_Struct(method, struct METHOD, data);
    if (data->recv == Qundef) {
	rb_raise(rb_eTypeError, "can't call unbound method; bind first");
    }
    PUSH_ITER(rb_block_given_p()?ITER_PRE:ITER_NOT);
    PUSH_TAG(PROT_NONE);
    if (OBJ_TAINTED(method)) {
	safe = ruby_safe_level;
	if (ruby_safe_level < 4) ruby_safe_level = 4;
    }
    if ((state = EXEC_TAG()) == 0) {
	result = rb_call0(data->klass,data->recv,data->id,data->oid,argc,argv,data->body,0);
    }
    POP_TAG();
    POP_ITER();
    if (safe >= 0) ruby_safe_level = safe;
    if (state) JUMP_TAG(state);
    return result;
}

/**********************************************************************
 *
 * Document-class: UnboundMethod
 *
 *  Ruby supports two forms of objectified methods. Class
 *  <code>Method</code> is used to represent methods that are associated
 *  with a particular object: these method objects are bound to that
 *  object. Bound method objects for an object can be created using
 *  <code>Object#method</code>.
 *     
 *  Ruby also supports unbound methods; methods objects that are not
 *  associated with a particular object. These can be created either by
 *  calling <code>Module#instance_method</code> or by calling
 *  <code>unbind</code> on a bound method object. The result of both of
 *  these is an <code>UnboundMethod</code> object.
 *     
 *  Unbound methods can only be called after they are bound to an
 *  object. That object must be be a kind_of? the method's original
 *  class.
 *     
 *     class Square
 *       def area
 *         @side * @side
 *       end
 *       def initialize(side)
 *         @side = side
 *       end
 *     end
 *     
 *     area_un = Square.instance_method(:area)
 *     
 *     s = Square.new(12)
 *     area = area_un.bind(s)
 *     area.call   #=> 144
 *     
 *  Unbound methods are a reference to the method at the time it was
 *  objectified: subsequent changes to the underlying class will not
 *  affect the unbound method.
 *     
 *     class Test
 *       def test
 *         :original
 *       end
 *     end
 *     um = Test.instance_method(:test)
 *     class Test
 *       def test
 *         :modified
 *       end
 *     end
 *     t = Test.new
 *     t.test            #=> :modified
 *     um.bind(t).call   #=> :original
 *     
 */

/*
 *  call-seq:
 *     umeth.bind(obj) -> method
 *  
 *  Bind <i>umeth</i> to <i>obj</i>. If <code>Klass</code> was the class
 *  from which <i>umeth</i> was obtained,
 *  <code>obj.kind_of?(Klass)</code> must be true.
 *     
 *     class A
 *       def test
 *         puts "In test, class = #{self.class}"
 *       end
 *     end
 *     class B < A
 *     end
 *     class C < B
 *     end
 *     
 *     
 *     um = B.instance_method(:test)
 *     bm = um.bind(C.new)
 *     bm.call
 *     bm = um.bind(B.new)
 *     bm.call
 *     bm = um.bind(A.new)
 *     bm.call
 *     
 *  <em>produces:</em>
 *     
 *     In test, class = C
 *     In test, class = B
 *     prog.rb:16:in `bind': bind argument must be an instance of B (TypeError)
 *     	from prog.rb:16
 */

static VALUE
umethod_bind(method, recv)
    VALUE method, recv;
{
    struct METHOD *data, *bound;

    Data_Get_Struct(method, struct METHOD, data);
    if (data->rklass != CLASS_OF(recv)) {
	if (FL_TEST(data->rklass, FL_SINGLETON)) {
	    rb_raise(rb_eTypeError, "singleton method called for a different object");
	}
	if(!rb_obj_is_kind_of(recv, data->rklass)) {
	    rb_raise(rb_eTypeError, "bind argument must be an instance of %s",
		     rb_class2name(data->rklass));
	}
    }

    method = Data_Make_Struct(rb_cMethod,struct METHOD,bm_mark,free,bound);
    *bound = *data;
    bound->recv = recv;
    bound->rklass = CLASS_OF(recv);

    return method;
}

int
rb_node_arity(body)
    NODE *body;
{
    int n;

    switch (nd_type(body)) {
      case NODE_CFUNC:
	if (body->nd_argc < 0) return -1;
	return body->nd_argc;
      case NODE_ZSUPER:
	return -1;
      case NODE_ATTRSET:
	return 1;
      case NODE_IVAR:
	return 0;
      case NODE_BMETHOD:
	return rb_proc_arity(body->nd_cval);
      case NODE_SCOPE:
	body = body->nd_next;	/* skip NODE_SCOPE */
	if (nd_type(body) == NODE_BLOCK)
	    body = body->nd_head;
	if (!body) return 0;
	n = body->nd_cnt;
	if (body->nd_opt || body->nd_rest != -1)
	    n = -n-1;
	return n;
      default:
	rb_raise(rb_eArgError, "invalid node 0x%x", nd_type(body));
    }
}

/*
 *  call-seq:
 *     meth.arity    => fixnum
 *  
 *  Returns an indication of the number of arguments accepted by a
 *  method. Returns a nonnegative integer for methods that take a fixed
 *  number of arguments. For Ruby methods that take a variable number of
 *  arguments, returns -n-1, where n is the number of required
 *  arguments. For methods written in C, returns -1 if the call takes a
 *  variable number of arguments.
 *     
 *     class C
 *       def one;    end
 *       def two(a); end
 *       def three(*a);  end
 *       def four(a, b); end
 *       def five(a, b, *c);    end
 *       def six(a, b, *c, &d); end
 *     end
 *     c = C.new
 *     c.method(:one).arity     #=> 0
 *     c.method(:two).arity     #=> 1
 *     c.method(:three).arity   #=> -1
 *     c.method(:four).arity    #=> 2
 *     c.method(:five).arity    #=> -3
 *     c.method(:six).arity     #=> -3
 *     
 *     "cat".method(:size).arity      #=> 0
 *     "cat".method(:replace).arity   #=> 1
 *     "cat".method(:squeeze).arity   #=> -1
 *     "cat".method(:count).arity     #=> -1
 */

static VALUE
method_arity_m(method)
    VALUE method;
{
    int n = method_arity(method);
    return INT2FIX(n);
}

static int
method_arity(method)
    VALUE method;
{
    struct METHOD *data;

    Data_Get_Struct(method, struct METHOD, data);
    return rb_node_arity(data->body);
}

int
rb_mod_method_arity(mod, id)
    VALUE mod;
    ID id;
{
    NODE *node = rb_method_node(mod, id);
    return rb_node_arity(node);
}

int
rb_obj_method_arity(obj, id)
    VALUE obj;
    ID id;
{
    return rb_mod_method_arity(CLASS_OF(obj), id);
}

/*
 *  call-seq:
 *   meth.to_s      =>  string
 *   meth.inspect   =>  string
 *
 *  Show the name of the underlying method.
 *
 *    "cat".method(:count).inspect   #=> "#<Method: String#count>"
 */

static VALUE
method_inspect(method)
    VALUE method;
{
    struct METHOD *data;
    VALUE str;
    const char *s;
    char *sharp = "#";

    Data_Get_Struct(method, struct METHOD, data);
    str = rb_str_buf_new2("#<");
    s = rb_obj_classname(method);
    rb_str_buf_cat2(str, s);
    rb_str_buf_cat2(str, ": ");

    if (FL_TEST(data->klass, FL_SINGLETON)) {
	VALUE v = rb_iv_get(data->klass, "__attached__");

	if (data->recv == Qundef) {
	    rb_str_buf_append(str, rb_inspect(data->klass));
	}
	else if (data->recv == v) {
	    rb_str_buf_append(str, rb_inspect(v));
	    sharp = ".";
	}
	else {
	    rb_str_buf_append(str, rb_inspect(data->recv));
	    rb_str_buf_cat2(str, "(");
	    rb_str_buf_append(str, rb_inspect(v));
	    rb_str_buf_cat2(str, ")");
	    sharp = ".";
	}
    }
    else {
	rb_str_buf_cat2(str, rb_class2name(data->rklass));
	if (data->rklass != data->klass) {
	    rb_str_buf_cat2(str, "(");
	    rb_str_buf_cat2(str, rb_class2name(data->klass));
	    rb_str_buf_cat2(str, ")");
	}
    }
    rb_str_buf_cat2(str, sharp);
    rb_str_buf_cat2(str, rb_id2name(data->oid));
    rb_str_buf_cat2(str, ">");

    return str;
}

static VALUE
mproc(method)
    VALUE method;
{
    VALUE proc;

    /* emulate ruby's method call */
    PUSH_ITER(ITER_CUR);
    PUSH_FRAME();
    proc = rb_block_proc();
    POP_FRAME();
    POP_ITER();

    return proc;
}

static VALUE
bmcall(args, method)
    VALUE args, method;
{
    volatile VALUE a;

    a = svalue_to_avalue(args);
    return method_call(RARRAY(a)->len, RARRAY(a)->ptr, method);
}

VALUE
rb_proc_new(func, val)
    VALUE (*func)(ANYARGS);	/* VALUE yieldarg[, VALUE procarg] */
    VALUE val;
{
    struct BLOCK *data;
    VALUE proc = rb_iterate((VALUE(*)_((VALUE)))mproc, 0, func, val);

    Data_Get_Struct(proc, struct BLOCK, data);
    data->body->nd_state = YIELD_FUNC_AVALUE;
    return proc;
}

/*
 *  call-seq:
 *     meth.to_proc    => prc
 *  
 *  Returns a <code>Proc</code> object corresponding to this method.
 */

static VALUE
method_proc(method)
    VALUE method;
{
    VALUE proc;
    struct METHOD *mdata;
    struct BLOCK *bdata;

    Data_Get_Struct(method, struct METHOD, mdata);
    if (nd_type(mdata->body) == NODE_BMETHOD) {
	return mdata->body->nd_cval;
    }
    proc = rb_iterate((VALUE(*)_((VALUE)))mproc, 0, bmcall, method);
    Data_Get_Struct(proc, struct BLOCK, bdata);
    bdata->body->nd_file = mdata->body->nd_file;
    nd_set_line(bdata->body, nd_line(mdata->body));
    bdata->body->nd_state = YIELD_FUNC_SVALUE;
    bdata->flags |= BLOCK_FROM_METHOD;

    return proc;
}

static VALUE
rb_obj_is_method(m)
    VALUE m;
{
    if (TYPE(m) == T_DATA && RDATA(m)->dmark == (RUBY_DATA_FUNC)bm_mark) {
	return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     define_method(symbol, method)     => new_method
 *     define_method(symbol) { block }   => proc
 *  
 *  Defines an instance method in the receiver. The _method_
 *  parameter can be a +Proc+ or +Method+ object.
 *  If a block is specified, it is used as the method body. This block
 *  is evaluated using <code>instance_eval</code>, a point that is
 *  tricky to demonstrate because <code>define_method</code> is private.
 *  (This is why we resort to the +send+ hack in this example.)
 *     
 *     class A
 *       def fred
 *         puts "In Fred"
 *       end
 *       def create_method(name, &block)
 *         self.class.send(:define_method, name, &block)
 *       end
 *       define_method(:wilma) { puts "Charge it!" }
 *     end
 *     class B < A
 *       define_method(:barney, instance_method(:fred))
 *     end
 *     a = B.new
 *     a.barney
 *     a.wilma
 *     a.create_method(:betty) { p self }
 *     a.betty
 *     
 *  <em>produces:</em>
 *     
 *     In Fred
 *     Charge it!
 *     #<B:0x401b39e8>
 */

static VALUE
rb_mod_define_method(argc, argv, mod)
    int argc;
    VALUE *argv;
    VALUE mod;
{
    ID id;
    VALUE body;
    NODE *node;
    int noex;

    if (argc == 1) {
	id = rb_to_id(argv[0]);
	body = proc_lambda();
    }
    else if (argc == 2) {
	id = rb_to_id(argv[0]);
	body = argv[1];
	if (!rb_obj_is_method(body) && !rb_obj_is_proc(body)) {
	    rb_raise(rb_eTypeError, "wrong argument type %s (expected Proc/Method)",
		     rb_obj_classname(body));
	}
    }
    else {
	rb_raise(rb_eArgError, "wrong number of arguments (%d for 1)", argc);
    }
    if (RDATA(body)->dmark == (RUBY_DATA_FUNC)bm_mark) {
	struct METHOD *method = (struct METHOD *)DATA_PTR(body);
	VALUE rklass = method->rklass;
	if (rklass != mod) {
	    if (FL_TEST(rklass, FL_SINGLETON)) {
		rb_raise(rb_eTypeError, "can't bind singleton method to a different class");
	    }
	    if (!RTEST(rb_class_inherited_p(mod, rklass))) {
		rb_raise(rb_eTypeError, "bind argument must be a subclass of %s",
			 rb_class2name(rklass));
	    }
	}
	node = method->body;
    }
    else if (RDATA(body)->dmark == (RUBY_DATA_FUNC)blk_mark) {
	struct BLOCK *block;

	body = proc_clone(body);
	RBASIC(body)->flags |= PROC_NOSAFE;
	Data_Get_Struct(body, struct BLOCK, block);
	block->frame.callee = id;
	block->frame.this_func = id;
	block->frame.this_class = mod;
	node = NEW_BMETHOD(body);
    }
    else {
	/* type error */
	rb_raise(rb_eTypeError, "wrong argument type (expected Proc/Method)");
    }

    if (SCOPE_TEST(SCOPE_PRIVATE)) {
	noex = NOEX_PRIVATE;
    }
    else if (SCOPE_TEST(SCOPE_PROTECTED)) {
	noex = NOEX_PROTECTED;
    }
    else {
	noex = NOEX_PUBLIC;
    }
    rb_add_method(mod, id, node, noex);
    return body;
}

/*
 *  <code>Proc</code> objects are blocks of code that have been bound to
 *  a set of local variables. Once bound, the code may be called in
 *  different contexts and still access those variables.
 *     
 *     def gen_times(factor)
 *       return Proc.new {|n| n*factor }
 *     end
 *     
 *     times3 = gen_times(3)
 *     times5 = gen_times(5)
 *     
 *     times3.call(12)               #=> 36
 *     times5.call(5)                #=> 25
 *     times3.call(times5.call(4))   #=> 60
 *     
 */

void
Init_Proc()
{
    rb_eLocalJumpError = rb_define_class("LocalJumpError", rb_eStandardError);
    rb_define_method(rb_eLocalJumpError, "exit_value", localjump_xvalue, 0);
    rb_define_method(rb_eLocalJumpError, "reason", localjump_reason, 0);

    exception_error = rb_exc_new2(rb_eFatal, "exception reentered");
    rb_global_variable(&exception_error);

    rb_eSysStackError = rb_define_class("SystemStackError", rb_eException);
    sysstack_error = rb_exc_new2(rb_eSysStackError, "stack level too deep");
    OBJ_TAINT(sysstack_error);
    rb_global_variable(&sysstack_error);

    rb_cProc = rb_define_class("Proc", rb_cObject);
    rb_undef_alloc_func(rb_cProc);
    rb_define_singleton_method(rb_cProc, "new", proc_s_new, -1);

    rb_define_method(rb_cProc, "clone", proc_clone, 0);
    rb_define_method(rb_cProc, "dup", proc_dup, 0);
    rb_define_method(rb_cProc, "call", proc_call, -2);
    rb_define_method(rb_cProc, "arity", proc_arity, 0);
    rb_define_method(rb_cProc, "[]", proc_call, -2);
    rb_define_method(rb_cProc, "==", proc_eq, 1);
    rb_define_method(rb_cProc, "eql?", proc_eq, 1);
    rb_define_method(rb_cProc, "hash", proc_hash, 0);
    rb_define_method(rb_cProc, "to_s", proc_to_s, 0);
    rb_define_method(rb_cProc, "to_proc", proc_to_self, 0);
    rb_define_method(rb_cProc, "binding", proc_binding, 0);

    rb_define_global_function("proc", rb_block_proc, 0);
    rb_define_global_function("lambda", proc_lambda, 0);

    rb_cMethod = rb_define_class("Method", rb_cObject);
    rb_undef_alloc_func(rb_cMethod);
    rb_undef_method(CLASS_OF(rb_cMethod), "new");
    rb_define_method(rb_cMethod, "==", method_eq, 1);
    rb_define_method(rb_cMethod, "eql?", method_eq, 1);
    rb_define_method(rb_cMethod, "hash", method_hash, 0);
    rb_define_method(rb_cMethod, "clone", method_clone, 0);
    rb_define_method(rb_cMethod, "call", method_call, -1);
    rb_define_method(rb_cMethod, "[]", method_call, -1);
    rb_define_method(rb_cMethod, "arity", method_arity_m, 0);
    rb_define_method(rb_cMethod, "inspect", method_inspect, 0);
    rb_define_method(rb_cMethod, "to_s", method_inspect, 0);
    rb_define_method(rb_cMethod, "to_proc", method_proc, 0);
    rb_define_method(rb_cMethod, "unbind", method_unbind, 0);
    rb_define_method(rb_mKernel, "method", rb_obj_method, 1);

    rb_cUnboundMethod = rb_define_class("UnboundMethod", rb_cObject);
    rb_undef_alloc_func(rb_cUnboundMethod);
    rb_undef_method(CLASS_OF(rb_cUnboundMethod), "new");
    rb_define_method(rb_cUnboundMethod, "==", method_eq, 1);
    rb_define_method(rb_cUnboundMethod, "eql?", method_eq, 1);
    rb_define_method(rb_cUnboundMethod, "hash", method_hash, 0);
    rb_define_method(rb_cUnboundMethod, "clone", method_clone, 0);
    rb_define_method(rb_cUnboundMethod, "arity", method_arity_m, 0);
    rb_define_method(rb_cUnboundMethod, "inspect", method_inspect, 0);
    rb_define_method(rb_cUnboundMethod, "to_s", method_inspect, 0);
    rb_define_method(rb_cUnboundMethod, "bind", umethod_bind, 1);
    rb_define_method(rb_cModule, "instance_method", rb_mod_method, 1);
}

/*
 *  Objects of class <code>Binding</code> encapsulate the execution
 *  context at some particular place in the code and retain this context
 *  for future use. The variables, methods, value of <code>self</code>,
 *  and possibly an iterator block that can be accessed in this context
 *  are all retained. Binding objects can be created using
 *  <code>Kernel#binding</code>, and are made available to the callback
 *  of <code>Kernel#set_trace_func</code>.
 *     
 *  These binding objects can be passed as the second argument of the
 *  <code>Kernel#eval</code> method, establishing an environment for the
 *  evaluation.
 *     
 *     class Demo
 *       def initialize(n)
 *         @secret = n
 *       end
 *       def getBinding
 *         return binding()
 *       end
 *     end
 *     
 *     k1 = Demo.new(99)
 *     b1 = k1.getBinding
 *     k2 = Demo.new(-3)
 *     b2 = k2.getBinding
 *     
 *     eval("@secret", b1)   #=> 99
 *     eval("@secret", b2)   #=> -3
 *     eval("@secret")       #=> nil
 *     
 *  Binding objects have no class-specific methods.
 *     
 */

void
Init_Binding()
{
    rb_cBinding = rb_define_class("Binding", rb_cObject);
    rb_undef_alloc_func(rb_cBinding);
    rb_undef_method(CLASS_OF(rb_cBinding), "new");
    rb_define_method(rb_cBinding, "clone", proc_clone, 0);
    rb_define_method(rb_cBinding, "eval", bind_eval, -1);
    rb_define_global_function("binding", rb_f_binding, 0);
}

#ifdef __ia64__
#if defined(__FreeBSD__)
/*
 * FreeBSD/ia64 currently does not have a way for a process to get the
 * base address for the RSE backing store, so hardcode it.
 */
#define __libc_ia64_register_backing_store_base (4ULL<<61)
#else
#ifdef HAVE_UNWIND_H
#include <unwind.h>
#else
#pragma weak __libc_ia64_register_backing_store_base
extern unsigned long __libc_ia64_register_backing_store_base;
#endif
#endif
#endif

/* Windows SEH refers data on the stack. */
#undef SAVE_WIN32_EXCEPTION_LIST
#if defined _WIN32 || defined __CYGWIN__
#if defined __CYGWIN__
typedef unsigned long DWORD;
#endif

static inline DWORD
win32_get_exception_list()
{
    DWORD p;
# if defined _MSC_VER
#   ifdef _M_IX86
#   define SAVE_WIN32_EXCEPTION_LIST
#   if _MSC_VER >= 1310
      /* warning: unsafe assignment to fs:0 ... this is ok */
#     pragma warning(disable: 4733)
#   endif
    __asm mov eax, fs:[0];
    __asm mov p, eax;
#   endif
# elif defined __GNUC__
#   ifdef __i386__
#   define SAVE_WIN32_EXCEPTION_LIST
    __asm__("movl %%fs:0,%0" : "=r"(p));
#   endif
# elif defined __BORLANDC__
#   define SAVE_WIN32_EXCEPTION_LIST
    __emit__(0x64, 0xA1, 0, 0, 0, 0); /* mov eax, fs:[0] */
    p = _EAX;
# endif
    return p;
}

static inline void
win32_set_exception_list(p)
    DWORD p;
{
# if defined _MSC_VER
#   ifdef _M_IX86
    __asm mov eax, p;
    __asm mov fs:[0], eax;
#   endif
# elif defined __GNUC__
#   ifdef __i386__
    __asm__("movl %0,%%fs:0" :: "r"(p));
#   endif
# elif defined __BORLANDC__
    _EAX = p;
    __emit__(0x64, 0xA3, 0, 0, 0, 0); /* mov fs:[0], eax */
# endif
}

#if !defined SAVE_WIN32_EXCEPTION_LIST && !defined _WIN32_WCE
# error unsupported platform
#endif
#endif

int rb_thread_pending = 0;

VALUE rb_cThread;

extern VALUE rb_last_status;

enum thread_status {
    THREAD_TO_KILL,
    THREAD_RUNNABLE,
    THREAD_STOPPED,
    THREAD_KILLED,
};

#define WAIT_FD		(1<<0)
#define WAIT_SELECT	(1<<1)
#define WAIT_TIME	(1<<2)
#define WAIT_JOIN	(1<<3)
#define WAIT_PID	(1<<4)

/* +infty, for this purpose */
#define DELAY_INFTY 1E30

#if !defined HAVE_PAUSE
# if defined _WIN32 && !defined __CYGWIN__
#  define pause() Sleep(INFINITE)
# else
#  define pause() sleep(0x7fffffff)
# endif
#endif

/* typedef struct thread * rb_thread_t; */

struct thread {
    struct thread *next, *prev;
    rb_jmpbuf_t context;
#ifdef SAVE_WIN32_EXCEPTION_LIST
    DWORD win32_exception_list;
#endif

    VALUE result;

    long   stk_len;
    long   stk_max;
    VALUE *stk_ptr;
    VALUE *stk_pos;
#ifdef __ia64__
    VALUE *bstr_ptr;
    long   bstr_len;
#endif

    struct FRAME *frame;
    struct SCOPE *scope;
    struct RVarmap *dyna_vars;
    struct BLOCK *block;
    struct iter *iter;
    struct tag *tag;
    VALUE klass;
    VALUE wrapper;
    NODE *cref;
    struct ruby_env *anchor;

    int flags;		/* misc. states (vmode/rb_trap_immediate/raised) */

    NODE *node;

    int tracing;
    VALUE errinfo;
    VALUE last_status;
    VALUE last_line;
    VALUE last_match;

    int safe;

    enum thread_status status;
    int wait_for;
    int fd;
    fd_set readfds;
    fd_set writefds;
    fd_set exceptfds;
    int select_value;
    double delay;
    rb_thread_t join;

    int abort;
    int priority;
    VALUE thgroup;

    st_table *locals;

    VALUE thread;
};

#define THREAD_RAISED 0x200	 /* temporary flag */
#define THREAD_TERMINATING 0x400 /* persistent flag */
#define THREAD_FLAGS_MASK  0x400 /* mask for persistent flags */

#define FOREACH_THREAD_FROM(f,x) x = f; do { x = x->next;
#define END_FOREACH_FROM(f,x) } while (x != f)

#define FOREACH_THREAD(x) FOREACH_THREAD_FROM(curr_thread,x)
#define END_FOREACH(x)    END_FOREACH_FROM(curr_thread,x)

struct thread_status_t {
    NODE *node;

    int tracing;
    VALUE errinfo;
    VALUE last_status;
    VALUE last_line;
    VALUE last_match;

    int safe;

    enum thread_status status;
    int wait_for;
    int fd;
    fd_set readfds;
    fd_set writefds;
    fd_set exceptfds;
    int select_value;
    double delay;
    rb_thread_t join;
};

#define THREAD_COPY_STATUS(src, dst) (void)(	\
    (dst)->node = (src)->node,			\
						\
    (dst)->tracing = (src)->tracing,		\
    (dst)->errinfo = (src)->errinfo,		\
    (dst)->last_status = (src)->last_status,	\
    (dst)->last_line = (src)->last_line,	\
    (dst)->last_match = (src)->last_match,	\
						\
    (dst)->safe = (src)->safe,			\
						\
    (dst)->status = (src)->status,		\
    (dst)->wait_for = (src)->wait_for,		\
    (dst)->fd = (src)->fd,			\
    (dst)->readfds = (src)->readfds,		\
    (dst)->writefds = (src)->writefds,		\
    (dst)->exceptfds = (src)->exceptfds,	\
    (dst)->select_value = (src)->select_value,	\
    (dst)->delay = (src)->delay,		\
    (dst)->join = (src)->join,			\
    0)

static int
thread_set_raised()
{
    if (curr_thread->flags & THREAD_RAISED) return 1;
    curr_thread->flags |= THREAD_RAISED;
    return 0;
}

static int
thread_reset_raised()
{
    if (!(curr_thread->flags & THREAD_RAISED)) return 0;
    curr_thread->flags &= ~THREAD_RAISED;
    return 1;
}

static void rb_thread_ready _((rb_thread_t));

static VALUE run_trap_eval _((VALUE));
static VALUE
run_trap_eval(arg)
    VALUE arg;
{
    VALUE *p = (VALUE *)arg;
    return rb_eval_cmd(p[0], p[1], (int)p[2]);
}

static VALUE
rb_trap_eval(cmd, sig, safe)
    VALUE cmd;
    int sig, safe;
{
    int state;
    VALUE val = Qnil;		/* OK */
    volatile struct thread_status_t save;
    VALUE arg[3];

    arg[0] = cmd;
    arg[1] = rb_ary_new3(1, INT2FIX(sig));
    arg[2] = (VALUE)safe;
    THREAD_COPY_STATUS(curr_thread, &save);
    rb_thread_ready(curr_thread);
    PUSH_ITER(ITER_NOT);
    val = rb_protect(run_trap_eval, (VALUE)&arg, &state);
    POP_ITER();
    THREAD_COPY_STATUS(&save, curr_thread);

    if (state) {
	rb_trap_immediate = 0;
	JUMP_TAG(state);
    }

    if (curr_thread->status == THREAD_STOPPED) {
	rb_thread_schedule();
    }
    errno = EINTR;

    return val;
}

static const char *
thread_status_name(status)
    enum thread_status status;
{
    switch (status) {
      case THREAD_RUNNABLE:
	return "run";
      case THREAD_STOPPED:
	return "sleep";
      case THREAD_TO_KILL:
	return "aborting";
      case THREAD_KILLED:
	return "dead";
      default:
	return "unknown";
    }
}

/* $SAFE accessor */
void
rb_set_safe_level(level)
    int level;
{
    if (level > ruby_safe_level) {
	if (level > SAFE_LEVEL_MAX) level = SAFE_LEVEL_MAX;
	ruby_safe_level = level;
	curr_thread->safe = level;
    }
}

static VALUE
safe_getter()
{
    return INT2NUM(ruby_safe_level);
}

static void
safe_setter(val)
    VALUE val;
{
    int level = NUM2INT(val);

    if (level < ruby_safe_level) {
	rb_raise(rb_eSecurityError, "tried to downgrade safe level from %d to %d",
		 ruby_safe_level, level);
    }
    if (level > SAFE_LEVEL_MAX) level = SAFE_LEVEL_MAX;
    ruby_safe_level = level;
    curr_thread->safe = level;
}

/* Return the current time as a floating-point number */
static double
timeofday()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + (double)tv.tv_usec * 1e-6;
}

#define STACK(addr) (th->stk_pos<(VALUE*)(addr) && (VALUE*)(addr)<th->stk_pos+th->stk_len)
#define ADJ(addr) (void*)(STACK(addr)?(((VALUE*)(addr)-th->stk_pos)+th->stk_ptr):(VALUE*)(addr))

static void
thread_mark(th)
    rb_thread_t th;
{
    struct FRAME *frame;
    struct BLOCK *block;

    rb_gc_mark(th->result);
    rb_gc_mark(th->thread);
    if (th->join) rb_gc_mark(th->join->thread);

    rb_gc_mark(th->klass);
    rb_gc_mark(th->wrapper);
    rb_gc_mark((VALUE)th->cref);

    rb_gc_mark((VALUE)th->scope);
    rb_gc_mark((VALUE)th->dyna_vars);
    rb_gc_mark(th->errinfo);
    rb_gc_mark(th->last_line);
    rb_gc_mark(th->last_match);
    rb_mark_tbl(th->locals);
    rb_gc_mark(th->thgroup);

    /* mark data in copied stack */
    if (th == curr_thread) return;
    if (th->status == THREAD_KILLED) return;
    if (th->stk_len == 0) return;  /* stack not active, no need to mark. */
    if (th->stk_ptr) {
	rb_gc_mark_locations(th->stk_ptr, th->stk_ptr+th->stk_len);
#if defined(THINK_C) || defined(__human68k__)
	rb_gc_mark_locations(th->stk_ptr+2, th->stk_ptr+th->stk_len+2);
#endif
#ifdef __ia64__
	if (th->bstr_ptr) {
	    rb_gc_mark_locations(th->bstr_ptr, th->bstr_ptr+th->bstr_len);
	}
#endif
    }
    frame = th->frame;
    while (frame && frame != top_frame) {
	frame = ADJ(frame);
	rb_gc_mark_frame(frame);
	if (frame->tmp) {
	    struct FRAME *tmp = frame->tmp;

	    while (tmp && tmp != top_frame) {
		tmp = ADJ(tmp);
		rb_gc_mark_frame(tmp);
		tmp = tmp->prev;
	    }
	}
	frame = frame->prev;
    }
    block = th->block;
    while (block) {
	block = ADJ(block);
	rb_gc_mark_frame(&block->frame);
	block = block->prev;
    }
}

static struct {
    rb_thread_t thread;
    VALUE proc, arg;
} new_thread;

void
rb_gc_mark_threads()
{
    rb_thread_t th;

    /* static global mark */
    rb_gc_mark((VALUE)ruby_cref);

    if (!curr_thread) return;
    FOREACH_THREAD(th) {
	rb_gc_mark(th->thread);
    } END_FOREACH(th);
    if (new_thread.thread) {
	rb_gc_mark(new_thread.thread->thread);
	rb_gc_mark(new_thread.proc);
	rb_gc_mark(new_thread.arg);
    }
}

static void
thread_free(th)
    rb_thread_t th;
{
    if (th->stk_ptr) free(th->stk_ptr);
    th->stk_ptr = 0;
#ifdef __ia64__
    if (th->bstr_ptr) free(th->bstr_ptr);
    th->bstr_ptr = 0;
#endif
    if (th->locals) st_free_table(th->locals);
    if (th->status != THREAD_KILLED) {
	if (th->prev) th->prev->next = th->next;
	if (th->next) th->next->prev = th->prev;
    }
    if (th != main_thread) free(th);
}

static rb_thread_t
rb_thread_check(data)
    VALUE data;
{
    if (TYPE(data) != T_DATA || RDATA(data)->dmark != (RUBY_DATA_FUNC)thread_mark) {
	rb_raise(rb_eTypeError, "wrong argument type %s (expected Thread)",
		 rb_obj_classname(data));
    }
    return (rb_thread_t)RDATA(data)->data;
}

static VALUE rb_thread_raise _((int, VALUE*, rb_thread_t));

static VALUE th_raise_exception;
static NODE *th_raise_node;
static VALUE th_cmd;
static int   th_sig, th_safe;
static char *th_signm;

#define RESTORE_NORMAL		1
#define RESTORE_FATAL		2
#define RESTORE_INTERRUPT	3
#define RESTORE_TRAP		4
#define RESTORE_RAISE		5
#define RESTORE_SIGNAL		6
#define RESTORE_EXIT		7

extern VALUE *rb_gc_stack_start;

static void
rb_thread_save_context(th)
    rb_thread_t th;
{
    VALUE *pos;
    int len;
    static VALUE tval;

    len = ruby_stack_length(&pos);
    th->stk_len = 0;
    th->stk_pos = pos;
    if (len > th->stk_max) {
	REALLOC_N(th->stk_ptr, VALUE, len);
	th->stk_max = len;
    }
    th->stk_len = len;
    FLUSH_REGISTER_WINDOWS;
    MEMCPY(th->stk_ptr, th->stk_pos, VALUE, th->stk_len);
#ifdef __ia64__
    {
	VALUE *top, *bot;
#ifdef HAVE_UNWIND_H
	_Unwind_Context *unwctx = _UNW_createContextForSelf();

	_UNW_currentContext(unwctx);
	bot = (VALUE*)(long)_UNW_getAR(unwctx, _UNW_AR_BSP);
	top = (VALUE*)(long)_UNW_getAR(unwctx, _UNW_AR_BSPSTORE);
	_UNW_destroyContext(unwctx);
#else
	ucontext_t ctx;

	getcontext(&ctx);
	bot = (VALUE*)__libc_ia64_register_backing_store_base;
	top = (VALUE*)ctx.uc_mcontext.IA64_BSPSTORE;
#endif
	th->bstr_len = top - bot;
	REALLOC_N(th->bstr_ptr, VALUE, th->bstr_len);
	MEMCPY(th->bstr_ptr, bot, VALUE, th->bstr_len);
    }
#endif
#ifdef SAVE_WIN32_EXCEPTION_LIST
    th->win32_exception_list = win32_get_exception_list();
#endif

    th->frame = ruby_frame;
    th->scope = ruby_scope;
    th->klass = ruby_class;
    th->wrapper = ruby_wrapper;
    th->cref = ruby_cref;
    th->dyna_vars = ruby_dyna_vars;
    th->block = ruby_block;
    th->flags &= THREAD_FLAGS_MASK;
    th->flags |= (rb_trap_immediate<<8) | scope_vmode;
    th->iter = ruby_iter;
    th->tag = prot_tag;
    th->tracing = tracing;
    th->errinfo = ruby_errinfo;
    th->last_status = rb_last_status;
    tval = rb_lastline_get();
    rb_lastline_set(th->last_line);
    th->last_line = tval;
    tval = rb_backref_get();
    rb_backref_set(th->last_match);
    th->last_match = tval;
    th->safe = ruby_safe_level;

    th->node = ruby_current_node;
}

static int
rb_thread_switch(n)
    int n;
{
    rb_trap_immediate = (curr_thread->flags&(1<<8))?1:0;
    switch (n) {
      case 0:
	return 0;
      case RESTORE_FATAL:
	JUMP_TAG(TAG_FATAL);
	break;
      case RESTORE_INTERRUPT:
	rb_interrupt();
	break;
      case RESTORE_TRAP:
	rb_trap_eval(th_cmd, th_sig, th_safe);
	break;
      case RESTORE_RAISE:
	ruby_frame->callee = 0;
	ruby_frame->this_func = 0;
	ruby_current_node = th_raise_node;
	rb_raise_jump(th_raise_exception);
	break;
      case RESTORE_SIGNAL:
	rb_raise(rb_eSignal, "SIG%s", th_signm);
	break;
      case RESTORE_EXIT:
	ruby_errinfo = th_raise_exception;
	ruby_current_node = th_raise_node;
	error_print();
	terminate_process(EXIT_FAILURE, 0, 0);
	break;
      case RESTORE_NORMAL:
      default:
	break;
    }
    return 1;
}

#define THREAD_SAVE_CONTEXT(th) \
    (rb_thread_save_context(th),\
     rb_thread_switch((FLUSH_REGISTER_WINDOWS, setjmp((th)->context))))

NORETURN(static void rb_thread_restore_context _((rb_thread_t,int)));
NOINLINE(static void stack_extend _((rb_thread_t, int)));

static void
stack_extend(th, exit)
    rb_thread_t th;
    int exit;
{
    VALUE space[1024];

    memset(space, 0, 1);	/* prevent array from optimization */
    rb_thread_restore_context(th, exit);
}

static void
rb_thread_restore_context(th, exit)
    rb_thread_t th;
    int exit;
{
    VALUE v;
    static rb_thread_t tmp;
    static int ex;
    static VALUE tval;

    if (!th->stk_ptr) rb_bug("unsaved context");

#if STACK_GROW_DIRECTION < 0
    if (&v > th->stk_pos) stack_extend(th, exit);
#elif STACK_GROW_DIRECTION > 0
    if (&v < th->stk_pos + th->stk_len) stack_extend(th, exit);
#else
    if (&v < rb_gc_stack_start) {
	/* Stack grows downward */
	if (&v > th->stk_pos) stack_extend(th, exit);
    }
    else {
	/* Stack grows upward */
	if (&v < th->stk_pos + th->stk_len) stack_extend(th, exit);
    }
#endif

    rb_trap_immediate = 0;	/* inhibit interrupts from here */
    ruby_frame = th->frame;
    ruby_scope = th->scope;
    ruby_class = th->klass;
    ruby_wrapper = th->wrapper;
    ruby_cref = th->cref;
    ruby_dyna_vars = th->dyna_vars;
    ruby_block = th->block;
    scope_vmode = th->flags&SCOPE_MASK;
    ruby_iter = th->iter;
    prot_tag = th->tag;
    tracing = th->tracing;
    ruby_errinfo = th->errinfo;
    rb_last_status = th->last_status;
    ruby_safe_level = th->safe;

    ruby_current_node = th->node;

#ifdef SAVE_WIN32_EXCEPTION_LIST
    win32_set_exception_list(th->win32_exception_list);
#endif
    tmp = th;
    ex = exit;
    FLUSH_REGISTER_WINDOWS;
    MEMCPY(tmp->stk_pos, tmp->stk_ptr, VALUE, tmp->stk_len);
#ifdef __ia64__
    {
	VALUE *base;
#ifdef HAVE_UNWIND_H
	_Unwind_Context *unwctx = _UNW_createContextForSelf();

	_UNW_currentContext(unwctx);
	base = (VALUE*)(long)_UNW_getAR(unwctx, _UNW_AR_BSP);
	_UNW_destroyContext(unwctx);
#else
	base = (VALUE*)__libc_ia64_register_backing_store_base;
#endif
	MEMCPY(base, tmp->bstr_ptr, VALUE, tmp->bstr_len);
    }
#endif

    tval = rb_lastline_get();
    rb_lastline_set(tmp->last_line);
    tmp->last_line = tval;
    tval = rb_backref_get();
    rb_backref_set(tmp->last_match);
    tmp->last_match = tval;

    longjmp(tmp->context, ex);
}

static void
rb_thread_ready(th)
    rb_thread_t th;
{
    th->wait_for = 0;
    if (th->status != THREAD_TO_KILL) {
	th->status = THREAD_RUNNABLE;
    }
}

static void
rb_thread_die(th)
    rb_thread_t th;
{
    th->thgroup = 0;
    th->status = THREAD_KILLED;
    if (th->stk_ptr) free(th->stk_ptr);
    th->stk_ptr = 0;
}

static void
rb_thread_remove(th)
    rb_thread_t th;
{
    if (th->status == THREAD_KILLED) return;

    rb_thread_ready(th);
    rb_thread_die(th);
    th->prev->next = th->next;
    th->next->prev = th->prev;
}

static int
rb_thread_dead(th)
    rb_thread_t th;
{
    return th->status == THREAD_KILLED;
}

void
rb_thread_fd_close(fd)
    int fd;
{
    rb_thread_t th;

    FOREACH_THREAD(th) {
	if (((th->wait_for & WAIT_FD) && fd == th->fd) ||
	    ((th->wait_for & WAIT_SELECT) && (fd < th->fd) &&
	     (FD_ISSET(fd, &th->readfds) ||
	      FD_ISSET(fd, &th->writefds) ||
	      FD_ISSET(fd, &th->exceptfds)))) {
	    VALUE exc = rb_exc_new2(rb_eIOError, "stream closed");
	    rb_thread_raise(1, &exc, th);
	}
    }
    END_FOREACH(th);
}

NORETURN(static void rb_thread_main_jump _((VALUE, int)));
static void
rb_thread_main_jump(err, tag)
    VALUE err;
    int tag;
{
    curr_thread = main_thread;
    th_raise_exception = err;
    th_raise_node = ruby_current_node;
    rb_thread_restore_context(main_thread, tag);
}

NORETURN(static void rb_thread_deadlock _((void)));
static void
rb_thread_deadlock()
{
    char msg[21+SIZEOF_LONG*2];
    VALUE e;

    sprintf(msg, "Thread(0x%lx): deadlock", curr_thread->thread);
    e = rb_exc_new2(rb_eFatal, msg);
    if (curr_thread == main_thread) {
	rb_exc_raise(e);
    }
    rb_thread_main_jump(e, RESTORE_RAISE);
}

static void
copy_fds(dst, src, max)
    fd_set *dst, *src;
    int max;
{
    int n = 0;
    int i;

    for (i=0; i<=max; i++) {
	if (FD_ISSET(i, src)) {
	    n = i;
	    FD_SET(i, dst);
	}
    }
}

static int
match_fds(dst, src, max)
    fd_set *dst, *src;
    int max;
{
    int i;

    for (i=0; i<=max; i++) {
	if (FD_ISSET(i, src) && FD_ISSET(i, dst)) {
	    return Qtrue;
	}
    }
    return Qfalse;
}

static int
intersect_fds(src, dst, max)
    fd_set *src, *dst;
    int max;
{
    int i, n = 0;

    for (i=0; i<=max; i++) {
	if (FD_ISSET(i, dst)) {
	    if (FD_ISSET(i, src)) {
		/* Wake up only one thread per fd. */
		FD_CLR(i, src);
		n++;
	    }
	    else {
		FD_CLR(i, dst);
	    }
	}
    }
    return n;
}

static int
find_bad_fds(dst, src, max)
    fd_set *dst, *src;
    int max;
{
    int i, test = Qfalse;

    for (i=0; i<=max; i++) {
	if (FD_ISSET(i, src) && !FD_ISSET(i, dst)) {
	    FD_CLR(i, src);
	    test = Qtrue;
	}
    }
    return test;
}

void
rb_thread_schedule()
{
    rb_thread_t next;		/* OK */
    rb_thread_t th;
    rb_thread_t curr;
    int found = 0;

    fd_set readfds;
    fd_set writefds;
    fd_set exceptfds;
    struct timeval delay_tv, *delay_ptr;
    double delay, now;	/* OK */
    int n, max;
    int need_select = 0;
    int select_timeout = 0;

#ifdef HAVE_NATIVETHREAD
    if (!is_ruby_native_thread()) {
	rb_bug("cross-thread violation on rb_thread_schedule()");
    }
#endif
    rb_thread_pending = 0;
    if (curr_thread == curr_thread->next
	&& curr_thread->status == THREAD_RUNNABLE)
	return;

    next = 0;
    curr = curr_thread;		/* starting thread */

    while (curr->status == THREAD_KILLED) {
	curr = curr->prev;
    }

  again:
    max = -1;
    FD_ZERO(&readfds);
    FD_ZERO(&writefds);
    FD_ZERO(&exceptfds);
    delay = DELAY_INFTY;
    now = -1.0;

    FOREACH_THREAD_FROM(curr, th) {
	if (!found && th->status <= THREAD_RUNNABLE) {
	    found = 1;
	}
	if (th->status != THREAD_STOPPED) continue;
	if (th->wait_for & WAIT_JOIN) {
	    if (rb_thread_dead(th->join)) {
		th->status = THREAD_RUNNABLE;
		found = 1;
	    }
	}
	if (th->wait_for & WAIT_FD) {
	    FD_SET(th->fd, &readfds);
	    if (max < th->fd) max = th->fd;
	    need_select = 1;
	}
	if (th->wait_for & WAIT_SELECT) {
	    copy_fds(&readfds, &th->readfds, th->fd);
	    copy_fds(&writefds, &th->writefds, th->fd);
	    copy_fds(&exceptfds, &th->exceptfds, th->fd);
	    if (max < th->fd) max = th->fd;
	    need_select = 1;
	    if (th->wait_for & WAIT_TIME) {
		select_timeout = 1;
	    }
	    th->select_value = 0;
	}
	if (th->wait_for & WAIT_TIME) {
	    double th_delay;

	    if (now < 0.0) now = timeofday();
	    th_delay = th->delay - now;
	    if (th_delay <= 0.0) {
		th->status = THREAD_RUNNABLE;
		found = 1;
	    }
	    else if (th_delay < delay) {
		delay = th_delay;
		need_select = 1;
	    }
	    else if (th->delay == DELAY_INFTY) {
		need_select = 1;
	    }
	}
    }
    END_FOREACH_FROM(curr, th);

    /* Do the select if needed */
    if (need_select) {
	/* Convert delay to a timeval */
	/* If a thread is runnable, just poll */
	if (found) {
	    delay_tv.tv_sec = 0;
	    delay_tv.tv_usec = 0;
	    delay_ptr = &delay_tv;
	}
	else if (delay == DELAY_INFTY) {
	    delay_ptr = 0;
	}
	else {
	    delay_tv.tv_sec = delay;
	    delay_tv.tv_usec = (delay - (double)delay_tv.tv_sec)*1e6;
	    delay_ptr = &delay_tv;
	}

	n = select(max+1, &readfds, &writefds, &exceptfds, delay_ptr);
	if (n < 0) {
	    int e = errno;

	    if (rb_trap_pending) rb_trap_exec();
	    if (e == EINTR) goto again;
#ifdef ERESTART
	    if (e == ERESTART) goto again;
#endif
	    FOREACH_THREAD_FROM(curr, th) {
		if (th->wait_for & WAIT_SELECT) {
		    int v = 0;

		    v |= find_bad_fds(&readfds, &th->readfds, th->fd);
		    v |= find_bad_fds(&writefds, &th->writefds, th->fd);
		    v |= find_bad_fds(&exceptfds, &th->exceptfds, th->fd);
		    if (v) {
			th->select_value = n;
			n = max;
		    }
		}
	    }
	    END_FOREACH_FROM(curr, th);
	}
 	if (select_timeout && n == 0) {
 	    if (now < 0.0) now = timeofday();
 	    FOREACH_THREAD_FROM(curr, th) {
 		if (((th->wait_for&(WAIT_SELECT|WAIT_TIME)) == (WAIT_SELECT|WAIT_TIME)) &&
		    th->delay <= now) {
 		    th->status = THREAD_RUNNABLE;
 		    th->wait_for = 0;
 		    th->select_value = 0;
 		    found = 1;
		    intersect_fds(&readfds, &th->readfds, max);
		    intersect_fds(&writefds, &th->writefds, max);
		    intersect_fds(&exceptfds, &th->exceptfds, max);
		}
	    }
	    END_FOREACH_FROM(curr, th);
	}
	if (n > 0) {
	    now = -1.0;
	    /* Some descriptors are ready.
	       Make the corresponding threads runnable. */
	    FOREACH_THREAD_FROM(curr, th) {
		if ((th->wait_for&WAIT_FD) && FD_ISSET(th->fd, &readfds)) {
		    /* Wake up only one thread per fd. */
		    FD_CLR(th->fd, &readfds);
		    th->status = THREAD_RUNNABLE;
		    th->fd = 0;
		    th->wait_for = 0;
		    found = 1;
		}
		if ((th->wait_for&WAIT_SELECT) &&
		    (match_fds(&readfds, &th->readfds, max) ||
		     match_fds(&writefds, &th->writefds, max) ||
		     match_fds(&exceptfds, &th->exceptfds, max))) {
		    /* Wake up only one thread per fd. */
		    th->status = THREAD_RUNNABLE;
		    th->wait_for = 0;
		    n = intersect_fds(&readfds, &th->readfds, max) +
			intersect_fds(&writefds, &th->writefds, max) +
			intersect_fds(&exceptfds, &th->exceptfds, max);
		    th->select_value = n;
		    found = 1;
		}
	    }
	    END_FOREACH_FROM(curr, th);
	}
	/* The delays for some of the threads should have expired.
	   Go through the loop once more, to check the delays. */
	if (!found && delay != DELAY_INFTY)
	    goto again;
    }

    FOREACH_THREAD_FROM(curr, th) {
	if (th->status == THREAD_TO_KILL) {
	    next = th;
	    break;
	}
	if (th->status == THREAD_RUNNABLE && th->stk_ptr) {
	    if (!next || next->priority < th->priority)
	       next = th;
	}
    }
    END_FOREACH_FROM(curr, th);

    if (!next) {
	/* raise fatal error to main thread */
	curr_thread->node = ruby_current_node;
	if (curr->next == curr) {
	    TRAP_BEG;
	    pause();
	    TRAP_END;
	}
	FOREACH_THREAD_FROM(curr, th) {
	    warn_printf("deadlock 0x%lx: %s:",
			th->thread, thread_status_name(th->status));
	    if (th->wait_for & WAIT_FD) warn_printf("F(%d)", th->fd);
	    if (th->wait_for & WAIT_SELECT) warn_printf("S");
	    if (th->wait_for & WAIT_TIME) warn_printf("T(%f)", th->delay);
	    if (th->wait_for & WAIT_JOIN)
		warn_printf("J(0x%lx)", th->join ? th->join->thread : 0);
	    if (th->wait_for & WAIT_PID) warn_printf("P");
	    if (!th->wait_for) warn_printf("-");
	    warn_printf(" %s - %s:%d\n",
			th==main_thread ? "(main)" : "",
			th->node->nd_file, nd_line(th->node));
	}
	END_FOREACH_FROM(curr, th);
	next = main_thread;
	rb_thread_ready(next);
	next->status = THREAD_TO_KILL;
	if (!rb_thread_dead(curr_thread)) {
	    rb_thread_save_context(curr_thread);
	}
	rb_thread_deadlock();
    }
    next->wait_for = 0;
    if (next->status == THREAD_RUNNABLE && next == curr_thread) {
	return;
    }

    /* context switch */
    if (curr == curr_thread) {
	if (THREAD_SAVE_CONTEXT(curr)) {
	    return;
	}
    }

    curr_thread = next;
    if (next->status == THREAD_TO_KILL) {
	if (!(next->flags & THREAD_TERMINATING)) {
	    next->flags |= THREAD_TERMINATING;
	    /* terminate; execute ensure-clause if any */
	    rb_thread_restore_context(next, RESTORE_FATAL);
	}
    }
    rb_thread_restore_context(next, RESTORE_NORMAL);
}

void
rb_thread_wait_fd(fd)
    int fd;
{
    if (rb_thread_critical) return;
    if (curr_thread == curr_thread->next) return;
    if (curr_thread->status == THREAD_TO_KILL) return;

    curr_thread->status = THREAD_STOPPED;
    curr_thread->fd = fd;
    curr_thread->wait_for = WAIT_FD;
    rb_thread_schedule();
}

int
rb_thread_fd_writable(fd)
    int fd;
{
    if (rb_thread_critical) return Qtrue;
    if (curr_thread == curr_thread->next) return Qtrue;
    if (curr_thread->status == THREAD_TO_KILL) return Qtrue;

    curr_thread->status = THREAD_STOPPED;
    FD_ZERO(&curr_thread->readfds);
    FD_ZERO(&curr_thread->writefds);
    FD_SET(fd, &curr_thread->writefds);
    FD_ZERO(&curr_thread->exceptfds);
    curr_thread->fd = fd+1;
    curr_thread->wait_for = WAIT_SELECT;
    rb_thread_schedule();
    return Qfalse;
}

void
rb_thread_wait_for(time)
    struct timeval time;
{
    double date;

    if (rb_thread_critical ||
	curr_thread == curr_thread->next ||
	curr_thread->status == THREAD_TO_KILL) {
	int n;
	int thr_critical = rb_thread_critical;
#ifndef linux
	double d, limit;
	limit = timeofday()+(double)time.tv_sec+(double)time.tv_usec*1e-6;
#endif
	for (;;) {
	    rb_thread_critical = Qtrue;
	    TRAP_BEG;
	    n = select(0, 0, 0, 0, &time);
	    rb_thread_critical = thr_critical;
	    TRAP_END;
	    if (n == 0) return;
	    if (n < 0) {
		switch (errno) {
		  case EINTR:
#ifdef ERESTART
		  case ERESTART:
#endif
		    return;
		  default:
		    rb_sys_fail("sleep");
		}
	    }
#ifndef linux
	    d = limit - timeofday();

	    time.tv_sec = (int)d;
	    time.tv_usec = (int)((d - (int)d)*1e6);
	    if (time.tv_usec < 0) {
		time.tv_usec += (long)1e6;
		time.tv_sec -= 1;
	    }
	    if (time.tv_sec < 0) return;
#endif
	}
    }

    date = timeofday() + (double)time.tv_sec + (double)time.tv_usec*1e-6;
    curr_thread->status = THREAD_STOPPED;
    curr_thread->delay = date;
    curr_thread->wait_for = WAIT_TIME;
    rb_thread_schedule();
}

void rb_thread_sleep_forever _((void));

int
rb_thread_alone()
{
    return curr_thread == curr_thread->next;
}

int
rb_thread_select(max, read, write, except, timeout)
    int max;
    fd_set *read, *write, *except;
    struct timeval *timeout;
{
    double limit;
    int n;

    if (!read && !write && !except) {
	if (!timeout) {
	    rb_thread_sleep_forever();
	    return 0;
	}
	rb_thread_wait_for(*timeout);
	return 0;
    }

    if (timeout) {
	limit = timeofday()+
	    (double)timeout->tv_sec+(double)timeout->tv_usec*1e-6;
    }

    if (rb_thread_critical ||
	curr_thread == curr_thread->next ||
	curr_thread->status == THREAD_TO_KILL) {
#ifndef linux
	struct timeval tv, *tvp = timeout;

	if (timeout) {
	    tv = *timeout;
	    tvp = &tv;
	}
#else
	struct timeval *const tvp = timeout;
#endif
	for (;;) {
	    TRAP_BEG;
	    n = select(max, read, write, except, tvp);
	    TRAP_END;
	    if (n < 0) {
		switch (errno) {
		  case EINTR:
#ifdef ERESTART
		  case ERESTART:
#endif
#ifndef linux
		    if (timeout) {
			double d = limit - timeofday();

			tv.tv_sec = (unsigned int)d;
			tv.tv_usec = (long)((d-(double)tv.tv_sec)*1e6);
			if (tv.tv_sec < 0)  tv.tv_sec = 0;
			if (tv.tv_usec < 0) tv.tv_usec = 0;
		    }
#endif
		    continue;
		  default:
		    break;
		}
	    }
	    return n;
	}
    }

    curr_thread->status = THREAD_STOPPED;
    if (read) curr_thread->readfds = *read;
    else FD_ZERO(&curr_thread->readfds);
    if (write) curr_thread->writefds = *write;
    else FD_ZERO(&curr_thread->writefds);
    if (except) curr_thread->exceptfds = *except;
    else FD_ZERO(&curr_thread->exceptfds);
    curr_thread->fd = max;
    curr_thread->wait_for = WAIT_SELECT;
    if (timeout) {
	curr_thread->delay = timeofday() +
	    (double)timeout->tv_sec + (double)timeout->tv_usec*1e-6;
	curr_thread->wait_for |= WAIT_TIME;
    }
    rb_thread_schedule();
    if (read) *read = curr_thread->readfds;
    if (write) *write = curr_thread->writefds;
    if (except) *except = curr_thread->exceptfds;
    return curr_thread->select_value;
}

static int rb_thread_join _((rb_thread_t, double));

static int
rb_thread_join(th, limit)
    rb_thread_t th;
    double limit;
{
    enum thread_status last_status = THREAD_RUNNABLE;

    if (rb_thread_critical) rb_thread_deadlock();
    if (!rb_thread_dead(th)) {
	if (th == curr_thread)
	    rb_raise(rb_eThreadError, "thread 0x%lx tried to join itself",
		     th->thread);
	if ((th->wait_for & WAIT_JOIN) && th->join == curr_thread)
	    rb_raise(rb_eThreadError, "Thread#join: deadlock 0x%lx - mutual join(0x%lx)",
		     curr_thread->thread, th->thread);
	if (curr_thread->status == THREAD_TO_KILL)
	    last_status = THREAD_TO_KILL;
	if (limit == 0) return Qfalse;
	curr_thread->status = THREAD_STOPPED;
	curr_thread->join = th;
	curr_thread->wait_for = WAIT_JOIN;
	curr_thread->delay = timeofday() + limit;
	if (limit < DELAY_INFTY) curr_thread->wait_for |= WAIT_TIME;
	rb_thread_schedule();
	curr_thread->status = last_status;
	if (!rb_thread_dead(th)) return Qfalse;
    }

    if (!NIL_P(th->errinfo) && (th->flags & THREAD_RAISED)) {
	VALUE oldbt = get_backtrace(th->errinfo);
	VALUE errat = make_backtrace();
	VALUE errinfo = rb_obj_dup(th->errinfo);

	if (TYPE(oldbt) == T_ARRAY && RARRAY(oldbt)->len > 0) {
	    rb_ary_unshift(errat, rb_ary_entry(oldbt, 0));
	}
	set_backtrace(errinfo, errat);
	rb_exc_raise(errinfo);
    }

    return Qtrue;
}


/*
 *  call-seq:
 *     thr.join          => thr
 *     thr.join(limit)   => thr
 *  
 *  The calling thread will suspend execution and run <i>thr</i>. Does not
 *  return until <i>thr</i> exits or until <i>limit</i> seconds have passed. If
 *  the time limit expires, <code>nil</code> will be returned, otherwise
 *  <i>thr</i> is returned.
 *     
 *  Any threads not joined will be killed when the main program exits.  If
 *  <i>thr</i> had previously raised an exception and the
 *  <code>abort_on_exception</code> and <code>$DEBUG</code> flags are not set
 *  (so the exception has not yet been processed) it will be processed at this
 *  time.
 *     
 *     a = Thread.new { print "a"; sleep(10); print "b"; print "c" }
 *     x = Thread.new { print "x"; Thread.pass; print "y"; print "z" }
 *     x.join # Let x thread finish, a will be killed on exit.
 *     
 *  <em>produces:</em>
 *     
 *     axyz
 *     
 *  The following example illustrates the <i>limit</i> parameter.
 *     
 *     y = Thread.new { 4.times { sleep 0.1; puts 'tick... ' }}
 *     puts "Waiting" until y.join(0.15)
 *     
 *  <em>produces:</em>
 *     
 *     tick...
 *     Waiting
 *     tick...
 *     Waitingtick...
 *     
 *     
 *     tick...
 */

static VALUE
rb_thread_join_m(argc, argv, thread)
    int argc;
    VALUE *argv;
    VALUE thread;
{
    VALUE limit;
    double delay = DELAY_INFTY;
    rb_thread_t th = rb_thread_check(thread);

    rb_scan_args(argc, argv, "01", &limit);
    if (!NIL_P(limit)) delay = rb_num2dbl(limit);
    if (!rb_thread_join(th, delay))
	return Qnil;
    return thread;
}


/*
 *  call-seq:
 *     Thread.current   => thread
 *  
 *  Returns the currently executing thread.
 *     
 *     Thread.current   #=> #<Thread:0x401bdf4c run>
 */

VALUE
rb_thread_current()
{
    return curr_thread->thread;
}


/*
 *  call-seq:
 *     Thread.main   => thread
 *  
 *  Returns the main thread for the process.
 *     
 *     Thread.main   #=> #<Thread:0x401bdf4c run>
 */

VALUE
rb_thread_main()
{
    return main_thread->thread;
}


/*
 *  call-seq:
 *     Thread.list   => array
 *  
 *  Returns an array of <code>Thread</code> objects for all threads that are
 *  either runnable or stopped.
 *     
 *     Thread.new { sleep(200) }
 *     Thread.new { 1000000.times {|i| i*i } }
 *     Thread.new { Thread.stop }
 *     Thread.list.each {|t| p t}
 *     
 *  <em>produces:</em>
 *     
 *     #<Thread:0x401b3e84 sleep>
 *     #<Thread:0x401b3f38 run>
 *     #<Thread:0x401b3fb0 sleep>
 *     #<Thread:0x401bdf4c run>
 */

VALUE
rb_thread_list()
{
    rb_thread_t th;
    VALUE ary = rb_ary_new();

    FOREACH_THREAD(th) {
	switch (th->status) {
	  case THREAD_RUNNABLE:
	  case THREAD_STOPPED:
	  case THREAD_TO_KILL:
	    rb_ary_push(ary, th->thread);
	  default:
	    break;
	}
    }
    END_FOREACH(th);

    return ary;
}


/*
 *  call-seq:
 *     thr.wakeup   => thr
 *  
 *  Marks <i>thr</i> as eligible for scheduling (it may still remain blocked on
 *  I/O, however). Does not invoke the scheduler (see <code>Thread#run</code>).
 *     
 *     c = Thread.new { Thread.stop; puts "hey!" }
 *     c.wakeup
 *     
 *  <em>produces:</em>
 *     
 *     hey!
 */

VALUE
rb_thread_wakeup(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    if (th->status == THREAD_KILLED)
	rb_raise(rb_eThreadError, "killed thread");
    rb_thread_ready(th);

    return thread;
}


/*
 *  call-seq:
 *     thr.run   => thr
 *  
 *  Wakes up <i>thr</i>, making it eligible for scheduling. If not in a critical
 *  section, then invokes the scheduler.
 *     
 *     a = Thread.new { puts "a"; Thread.stop; puts "c" }
 *     Thread.pass
 *     puts "Got here"
 *     a.run
 *     a.join
 *     
 *  <em>produces:</em>
 *     
 *     a
 *     Got here
 *     c
 */

VALUE
rb_thread_run(thread)
    VALUE thread;
{
    rb_thread_wakeup(thread);
    if (!rb_thread_critical) rb_thread_schedule();

    return thread;
}


/*
 *  call-seq:
 *     thr.exit        => thr or nil
 *     thr.kill        => thr or nil
 *     thr.terminate   => thr or nil
 *  
 *  Terminates <i>thr</i> and schedules another thread to be run. If this thread
 *  is already marked to be killed, <code>exit</code> returns the
 *  <code>Thread</code>. If this is the main thread, or the last thread, exits
 *  the process.
 */

VALUE
rb_thread_kill(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    if (th != curr_thread && th->safe < 4) {
	rb_secure(4);
    }
    if (th->status == THREAD_TO_KILL || th->status == THREAD_KILLED)
	return thread;
    if (th == th->next || th == main_thread) rb_exit(EXIT_SUCCESS);

    rb_thread_ready(th);
    th->status = THREAD_TO_KILL;
    if (!rb_thread_critical) rb_thread_schedule();
    return thread;
}


/*
 *  call-seq:
 *     Thread.kill(thread)   => thread
 *  
 *  Causes the given <em>thread</em> to exit (see <code>Thread::exit</code>).
 *     
 *     count = 0
 *     a = Thread.new { loop { count += 1 } }
 *     sleep(0.1)       #=> 0
 *     Thread.kill(a)   #=> #<Thread:0x401b3d30 dead>
 *     count            #=> 93947
 *     a.alive?         #=> false
 */

static VALUE
rb_thread_s_kill(obj, th)
    VALUE obj, th;
{
    return rb_thread_kill(th);
}


/*
 *  call-seq:
 *     Thread.exit   => thread
 *  
 *  Terminates the currently running thread and schedules another thread to be
 *  run. If this thread is already marked to be killed, <code>exit</code>
 *  returns the <code>Thread</code>. If this is the main thread, or the last
 *  thread, exit the process.
 */

static VALUE
rb_thread_exit()
{
    return rb_thread_kill(curr_thread->thread);
}


/*
 *  call-seq:
 *     Thread.pass   => nil
 *  
 *  Invokes the thread scheduler to pass execution to another thread.
 *     
 *     a = Thread.new { print "a"; Thread.pass;
 *                      print "b"; Thread.pass;
 *                      print "c" }
 *     b = Thread.new { print "x"; Thread.pass;
 *                      print "y"; Thread.pass;
 *                      print "z" }
 *     a.join
 *     b.join
 *     
 *  <em>produces:</em>
 *     
 *     axbycz
 */

static VALUE
rb_thread_pass()
{
    rb_thread_schedule();
    return Qnil;
}


/*
 *  call-seq:
 *     Thread.stop   => nil
 *  
 *  Stops execution of the current thread, putting it into a ``sleep'' state,
 *  and schedules execution of another thread. Resets the ``critical'' condition
 *  to <code>false</code>.
 *     
 *     a = Thread.new { print "a"; Thread.stop; print "c" }
 *     Thread.pass
 *     print "b"
 *     a.run
 *     a.join
 *     
 *  <em>produces:</em>
 *     
 *     abc
 */

VALUE
rb_thread_stop()
{
    enum thread_status last_status = THREAD_RUNNABLE;

    rb_thread_critical = 0;
    if (curr_thread == curr_thread->next) {
	rb_raise(rb_eThreadError, "stopping only thread\n\tnote: use sleep to stop forever");
    }
    if (curr_thread->status == THREAD_TO_KILL)
	last_status = THREAD_TO_KILL;
    curr_thread->status = THREAD_STOPPED;
    rb_thread_schedule();
    curr_thread->status = last_status;

    return Qnil;
}

struct timeval rb_time_timeval();

void
rb_thread_polling()
{
    if (curr_thread != curr_thread->next) {
	curr_thread->status = THREAD_STOPPED;
	curr_thread->delay = timeofday() + (double)0.06;
	curr_thread->wait_for = WAIT_TIME;
	rb_thread_schedule();
    }
}

void
rb_thread_sleep(sec)
    int sec;
{
    if (curr_thread == curr_thread->next) {
	TRAP_BEG;
	sleep(sec);
	TRAP_END;
	return;
    }
    rb_thread_wait_for(rb_time_timeval(INT2FIX(sec)));
}

void
rb_thread_sleep_forever()
{
    int thr_critical = rb_thread_critical;
    if (curr_thread == curr_thread->next ||
	curr_thread->status == THREAD_TO_KILL) {
	rb_thread_critical = Qtrue;
	TRAP_BEG;
	pause();
	rb_thread_critical = thr_critical;
	TRAP_END;
	return;
    }

    curr_thread->delay = DELAY_INFTY;
    curr_thread->wait_for = WAIT_TIME;
    curr_thread->status = THREAD_STOPPED;
    rb_thread_schedule();
}


/*
 *  call-seq:
 *     thr.priority   => integer
 *  
 *  Returns the priority of <i>thr</i>. Default is zero; higher-priority threads
 *  will run before lower-priority threads.
 *     
 *     Thread.current.priority   #=> 0
 */

static VALUE
rb_thread_priority(thread)
    VALUE thread;
{
    return INT2NUM(rb_thread_check(thread)->priority);
}


/*
 *  call-seq:
 *     thr.priority= integer   => thr
 *  
 *  Sets the priority of <i>thr</i> to <i>integer</i>. Higher-priority threads
 *  will run before lower-priority threads.
 *     
 *     count1 = count2 = 0
 *     a = Thread.new do
 *           loop { count1 += 1 }
 *         end
 *     a.priority = -1
 *     
 *     b = Thread.new do
 *           loop { count2 += 1 }
 *         end
 *     b.priority = -2
 *     sleep 1   #=> 1
 *     Thread.critical = 1
 *     count1    #=> 622504
 *     count2    #=> 5832
 */

static VALUE
rb_thread_priority_set(thread, prio)
    VALUE thread, prio;
{
    rb_thread_t th;

    rb_secure(4);
    th = rb_thread_check(thread);

    th->priority = NUM2INT(prio);
    rb_thread_schedule();
    return prio;
}


/*
 *  call-seq:
 *     thr.safe_level   => integer
 *  
 *  Returns the safe level in effect for <i>thr</i>. Setting thread-local safe
 *  levels can help when implementing sandboxes which run insecure code.
 *     
 *     thr = Thread.new { $SAFE = 3; sleep }
 *     Thread.current.safe_level   #=> 0
 *     thr.safe_level              #=> 3
 */

static VALUE
rb_thread_safe_level(thread)
    VALUE thread;
{
    rb_thread_t th;

    th = rb_thread_check(thread);
    if (th == curr_thread) {
	return INT2NUM(ruby_safe_level);
    }
    return INT2NUM(th->safe);
}

static int ruby_thread_abort;
static VALUE thgroup_default;


/*
 *  call-seq:
 *     Thread.abort_on_exception   => true or false
 *  
 *  Returns the status of the global ``abort on exception'' condition.  The
 *  default is <code>false</code>. When set to <code>true</code>, or if the
 *  global <code>$DEBUG</code> flag is <code>true</code> (perhaps because the
 *  command line option <code>-d</code> was specified) all threads will abort
 *  (the process will <code>exit(0)</code>) if an exception is raised in any
 *  thread. See also <code>Thread::abort_on_exception=</code>.
 */

static VALUE
rb_thread_s_abort_exc()
{
    return ruby_thread_abort?Qtrue:Qfalse;
}


/*
 *  call-seq:
 *     Thread.abort_on_exception= boolean   => true or false
 *  
 *  When set to <code>true</code>, all threads will abort if an exception is
 *  raised. Returns the new state.
 *     
 *     Thread.abort_on_exception = true
 *     t1 = Thread.new do
 *       puts  "In new thread"
 *       raise "Exception from thread"
 *     end
 *     sleep(1)
 *     puts "not reached"
 *     
 *  <em>produces:</em>
 *     
 *     In new thread
 *     prog.rb:4: Exception from thread (RuntimeError)
 *     	from prog.rb:2:in `initialize'
 *     	from prog.rb:2:in `new'
 *     	from prog.rb:2
 */

static VALUE
rb_thread_s_abort_exc_set(self, val)
    VALUE self, val;
{
    rb_secure(4);
    ruby_thread_abort = RTEST(val);
    return val;
}


/*
 *  call-seq:
 *     thr.abort_on_exception   => true or false
 *  
 *  Returns the status of the thread-local ``abort on exception'' condition for
 *  <i>thr</i>. The default is <code>false</code>. See also
 *  <code>Thread::abort_on_exception=</code>.
 */

static VALUE
rb_thread_abort_exc(thread)
    VALUE thread;
{
    return rb_thread_check(thread)->abort?Qtrue:Qfalse;
}


/*
 *  call-seq:
 *     thr.abort_on_exception= boolean   => true or false
 *  
 *  When set to <code>true</code>, causes all threads (including the main
 *  program) to abort if an exception is raised in <i>thr</i>. The process will
 *  effectively <code>exit(0)</code>.
 */

static VALUE
rb_thread_abort_exc_set(thread, val)
    VALUE thread, val;
{
    rb_secure(4);
    rb_thread_check(thread)->abort = RTEST(val);
    return val;
}


/*
 *  call-seq:
 *     thr.group   => thgrp or nil
 *  
 *  Returns the <code>ThreadGroup</code> which contains <i>thr</i>, or nil if
 *  the thread is not a member of any group.
 *     
 *     Thread.main.group   #=> #<ThreadGroup:0x4029d914>
 */

VALUE
rb_thread_group(thread)
    VALUE thread;
{
    VALUE group = rb_thread_check(thread)->thgroup;
    if (!group) {
	group = Qnil;
    }
    return group;
}

#ifdef __ia64__
# define IA64_INIT(x) x
#else
# define IA64_INIT(x)
#endif

#define THREAD_ALLOC(th) do {\
    th = ALLOC(struct thread);\
\
    th->next = 0;\
    th->prev = 0;\
\
    th->status = THREAD_RUNNABLE;\
    th->result = 0;\
    th->flags = 0;\
\
    th->stk_ptr = 0;\
    th->stk_len = 0;\
    th->stk_max = 0;\
    th->wait_for = 0;\
    IA64_INIT(th->bstr_ptr = 0);\
    IA64_INIT(th->bstr_len = 0);\
    FD_ZERO(&th->readfds);\
    FD_ZERO(&th->writefds);\
    FD_ZERO(&th->exceptfds);\
    th->delay = 0.0;\
    th->join = 0;\
\
    th->frame = 0;\
    th->scope = 0;\
    th->klass = 0;\
    th->wrapper = 0;\
    th->cref = ruby_cref;\
    th->dyna_vars = ruby_dyna_vars;\
    th->block = 0;\
    th->iter = 0;\
    th->tag = 0;\
    th->tracing = 0;\
    th->errinfo = Qnil;\
    th->last_status = 0;\
    th->last_line = 0;\
    th->last_match = Qnil;\
    th->abort = 0;\
    th->priority = 0;\
    th->thgroup = thgroup_default;\
    th->locals = 0;\
    th->thread = 0;\
    th->anchor = 0;\
} while (0)

static rb_thread_t
rb_thread_alloc(klass)
    VALUE klass;
{
    rb_thread_t th;
    struct RVarmap *vars;

    THREAD_ALLOC(th);
    th->thread = Data_Wrap_Struct(klass, thread_mark, thread_free, th);

    for (vars = th->dyna_vars; vars; vars = vars->next) {
	if (FL_TEST(vars, DVAR_DONT_RECYCLE)) break;
	FL_SET(vars, DVAR_DONT_RECYCLE);
    }
    return th;
}

static int thread_init = 0;

#if defined(_THREAD_SAFE)
static void
catch_timer(sig)
    int sig;
{
#if !defined(POSIX_SIGNAL) && !defined(BSD_SIGNAL)
    signal(sig, catch_timer);
#endif
    /* cause EINTR */
}

static pthread_t time_thread;

static void*
thread_timer(dummy)
    void *dummy;
{
    for (;;) {
#ifdef HAVE_NANOSLEEP
	struct timespec req, rem;

	req.tv_sec = 0;
	req.tv_nsec = 10000000;
	nanosleep(&req, &rem);
#else
	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 10000;
	select(0, NULL, NULL, NULL, &tv);
#endif
	if (!rb_thread_critical) {
	    rb_thread_pending = 1;
	    if (rb_trap_immediate) {
		pthread_kill(ruby_thid, SIGVTALRM);
	    }
	}
    }
}

void
rb_thread_start_timer()
{
}

void
rb_thread_stop_timer()
{
}
#elif defined(HAVE_SETITIMER)
static void
catch_timer(sig)
    int sig;
{
#if !defined(POSIX_SIGNAL) && !defined(BSD_SIGNAL)
    signal(sig, catch_timer);
#endif
    if (!rb_thread_critical) {
	rb_thread_pending = 1;
    }
    /* cause EINTR */
}

void
rb_thread_start_timer()
{
    struct itimerval tval;

    if (!thread_init) return;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = 10000;
    tval.it_value = tval.it_interval;
    setitimer(ITIMER_VIRTUAL, &tval, NULL);
}

void
rb_thread_stop_timer()
{
    struct itimerval tval;

    if (!thread_init) return;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = 0;
    tval.it_value = tval.it_interval;
    setitimer(ITIMER_VIRTUAL, &tval, NULL);
}
#else  /* !(_THREAD_SAFE || HAVE_SETITIMER) */
int rb_thread_tick = THREAD_TICK;
#endif

NORETURN(static void rb_thread_terminated _((rb_thread_t, int, enum thread_status)));
static VALUE rb_thread_yield _((VALUE, rb_thread_t));

static void
push_thread_anchor(ip)
    struct ruby_env *ip;
{
    ip->tag = prot_tag;
    ip->frame = ruby_frame;
    ip->block = ruby_block;
    ip->scope = ruby_scope;
    ip->iter = ruby_iter;
    ip->cref = ruby_cref;
    ip->prev = curr_thread->anchor;
    curr_thread->anchor = ip;
}

static void
pop_thread_anchor(ip)
    struct ruby_env *ip;
{
    curr_thread->anchor = ip->prev;
}

static void
thread_insert(th)
    rb_thread_t th;
{
    if (!th->next) {
	/* merge in thread list */
	th->prev = curr_thread;
	curr_thread->next->prev = th;
	th->next = curr_thread->next;
	curr_thread->next = th;
	th->priority = curr_thread->priority;
	th->thgroup = curr_thread->thgroup;
    }
}

static VALUE
rb_thread_start_0(fn, arg, th)
    VALUE (*fn)();
    void *arg;
    rb_thread_t th;
{
    volatile rb_thread_t th_save = th;
    volatile VALUE thread = th->thread;
    struct BLOCK *volatile saved_block = 0;
    enum thread_status status;
    int state;

    if (OBJ_FROZEN(curr_thread->thgroup)) {
	rb_raise(rb_eThreadError,
		 "can't start a new thread (frozen ThreadGroup)");
    }

    if (!thread_init) {
	thread_init = 1;
#if defined(HAVE_SETITIMER) || defined(_THREAD_SAFE)
#if defined(POSIX_SIGNAL)
	posix_signal(SIGVTALRM, catch_timer);
#else
	signal(SIGVTALRM, catch_timer);
#endif

#ifdef _THREAD_SAFE
	pthread_create(&time_thread, 0, thread_timer, 0);
#else
	rb_thread_start_timer();
#endif
#endif
    }

    if (THREAD_SAVE_CONTEXT(curr_thread)) {
	return thread;
    }

    if (fn == rb_thread_yield && curr_thread->anchor) {
	struct ruby_env *ip = curr_thread->anchor;
	new_thread.thread = th;
	new_thread.proc = rb_block_proc();
	new_thread.arg = (VALUE)arg;
	th->anchor = ip;
	thread_insert(th);
	curr_thread = th;
	longjmp((prot_tag = ip->tag)->buf, TAG_THREAD);
    }

    if (ruby_block) {		/* should nail down higher blocks */
	struct BLOCK dummy;

	dummy.prev = ruby_block;
	blk_copy_prev(&dummy);
	saved_block = ruby_block = dummy.prev;
    }
    scope_dup(ruby_scope);

    thread_insert(th);

    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	if (THREAD_SAVE_CONTEXT(th) == 0) {
	    curr_thread = th;
	    th->result = (*fn)(arg, th);
	}
	th = th_save;
    }
    else if (TAG_DST()) {
	th = th_save;
	th->result = prot_tag->retval;
    }
    POP_TAG();
    status = th->status;

    if (th == main_thread) ruby_stop(state);
    rb_thread_remove(th);

    if (saved_block) {
	blk_free(saved_block);
    }

    rb_thread_terminated(th, state, status);
    return 0;			/* not reached */
}

static void
rb_thread_terminated(th, state, status)
    rb_thread_t th;
    int state;
    enum thread_status status;
{
    if (state && status != THREAD_TO_KILL && !NIL_P(ruby_errinfo)) {
	th->flags |= THREAD_RAISED;
	if (state == TAG_FATAL) {
	    /* fatal error within this thread, need to stop whole script */
	    main_thread->errinfo = ruby_errinfo;
	    rb_thread_cleanup();
	}
	else if (rb_obj_is_kind_of(ruby_errinfo, rb_eSystemExit)) {
	    if (th->safe >= 4) {
		char buf[32];

		sprintf(buf, "Insecure exit at level %d", th->safe);
		th->errinfo = rb_exc_new2(rb_eSecurityError, buf);
	    }
	    else {
		/* delegate exception to main_thread */
		rb_thread_main_jump(ruby_errinfo, RESTORE_RAISE);
	    }
	}
	else if (th->safe < 4 && (ruby_thread_abort || th->abort || RTEST(ruby_debug))) {
	    /* exit on main_thread */
	    rb_thread_main_jump(ruby_errinfo, RESTORE_EXIT);
	}
	else {
	    th->errinfo = ruby_errinfo;
	}
    }
    rb_thread_schedule();
    ruby_stop(0);		/* last thread termination */
}

static VALUE
rb_thread_yield_0(arg)
    VALUE arg;
{
    return rb_thread_yield(arg, curr_thread);
}

static void
rb_thread_start_1()
{
    rb_thread_t th = new_thread.thread;
    volatile rb_thread_t th_save = th;
    VALUE proc = new_thread.proc;
    VALUE arg = new_thread.arg;
    struct ruby_env *ip = th->anchor;
    enum thread_status status;
    int state;

    ruby_frame = ip->frame;
    ruby_block = ip->block;
    ruby_scope = ip->scope;
    ruby_iter = ip->iter;
    ruby_cref = ip->cref;
    ruby_dyna_vars = ((struct BLOCK *)DATA_PTR(proc))->dyna_vars;
    PUSH_FRAME();
    *ruby_frame = *ip->frame;
    ruby_frame->prev = ip->frame;
    ruby_frame->iter = ITER_CUR;
    PUSH_TAG(PROT_NONE);
    if ((state = EXEC_TAG()) == 0) {
	if (THREAD_SAVE_CONTEXT(th) == 0) {
	    new_thread.thread = 0;
	    th->result = rb_block_pass(rb_thread_yield_0, arg, proc);
	}
	th = th_save;
    }
    else if (TAG_DST()) {
	th = th_save;
	th->result = prot_tag->retval;
    }
    POP_TAG();
    POP_FRAME();
    status = th->status;

    if (th == main_thread) ruby_stop(state);
    rb_thread_remove(th);
    rb_thread_terminated(th, state, status);
}

VALUE
rb_thread_create(fn, arg)
    VALUE (*fn)();
    void *arg;
{
    Init_stack((VALUE*)&arg);
    return rb_thread_start_0(fn, arg, rb_thread_alloc(rb_cThread));
}

static VALUE
rb_thread_yield(arg, th)
    VALUE arg;
    rb_thread_t th;
{
    const ID *tbl;

    scope_dup(ruby_block->scope);

    tbl = ruby_scope->local_tbl;
    if (tbl) {
	int n = *tbl++;
	for (tbl += 2, n -= 2; n > 0; --n) { /* skip first 2 ($_ and $~) */
	    ID id = *tbl++;
	    if (id != 0 && !rb_is_local_id(id))  /* push flip states */
		rb_dvar_push(id, Qfalse);
	}
    }
    rb_dvar_push('_', Qnil);
    rb_dvar_push('~', Qnil);
    ruby_block->dyna_vars = ruby_dyna_vars;

    return rb_yield_0(arg, 0, 0, YIELD_LAMBDA_CALL, Qtrue);
}

/*
 *  call-seq:
 *     Thread.new([arg]*) {|args| block }   => thread
 *  
 *  Creates and runs a new thread to execute the instructions given in
 *  <i>block</i>. Any arguments passed to <code>Thread::new</code> are passed
 *  into the block.
 *     
 *     x = Thread.new { sleep 0.1; print "x"; print "y"; print "z" }
 *     a = Thread.new { print "a"; print "b"; sleep 0.2; print "c" }
 *     x.join # Let the threads finish before
 *     a.join # main thread exits...
 *     
 *  <em>produces:</em>
 *     
 *     abxyzc
 */

static VALUE
rb_thread_s_new(argc, argv, klass)
    int argc;
    VALUE *argv;
    VALUE klass;
{
    rb_thread_t th = rb_thread_alloc(klass);
    volatile VALUE *pos;

    pos = th->stk_pos;
    rb_obj_call_init(th->thread, argc, argv);
    if (th->stk_pos == 0) {
	rb_raise(rb_eThreadError, "uninitialized thread - check `%s#initialize'",
		 rb_class2name(klass));
    }

    return th->thread;
}


/*
 *  call-seq:
 *     Thread.new([arg]*) {|args| block }   => thread
 *  
 *  Creates and runs a new thread to execute the instructions given in
 *  <i>block</i>. Any arguments passed to <code>Thread::new</code> are passed
 *  into the block.
 *     
 *     x = Thread.new { sleep 0.1; print "x"; print "y"; print "z" }
 *     a = Thread.new { print "a"; print "b"; sleep 0.2; print "c" }
 *     x.join # Let the threads finish before
 *     a.join # main thread exits...
 *     
 *  <em>produces:</em>
 *     
 *     abxyzc
 */

static VALUE
rb_thread_initialize(thread, args)
    VALUE thread, args;
{
    rb_thread_t th;

    if (!rb_block_given_p()) {
	rb_raise(rb_eThreadError, "must be called with a block");
    }
    th = rb_thread_check(thread);
    if (th->stk_max) {
	NODE *node = th->node;
	if (!node) {
	    rb_raise(rb_eThreadError, "already initialized thread");
	}
	rb_raise(rb_eThreadError, "already initialized thread - %s:%d",
		 node->nd_file, nd_line(node));
    }
    return rb_thread_start_0(rb_thread_yield, args, th);
}


/*
 *  call-seq:
 *     Thread.start([args]*) {|args| block }   => thread
 *     Thread.fork([args]*) {|args| block }    => thread
 *  
 *  Basically the same as <code>Thread::new</code>. However, if class
 *  <code>Thread</code> is subclassed, then calling <code>start</code> in that
 *  subclass will not invoke the subclass's <code>initialize</code> method.
 */

static VALUE
rb_thread_start(klass, args)
    VALUE klass, args;
{
    if (!rb_block_given_p()) {
	rb_raise(rb_eThreadError, "must be called with a block");
    }
    return rb_thread_start_0(rb_thread_yield, args, rb_thread_alloc(klass));
}


/*
 *  call-seq:
 *     thr.value   => obj
 *  
 *  Waits for <i>thr</i> to complete (via <code>Thread#join</code>) and returns
 *  its value.
 *     
 *     a = Thread.new { 2 + 2 }
 *     a.value   #=> 4
 */

static VALUE
rb_thread_value(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    while (!rb_thread_join(th, DELAY_INFTY));

    return th->result;
}


/*
 *  call-seq:
 *     thr.status   => string, false or nil
 *  
 *  Returns the status of <i>thr</i>: ``<code>sleep</code>'' if <i>thr</i> is
 *  sleeping or waiting on I/O, ``<code>run</code>'' if <i>thr</i> is executing,
 *  ``<code>aborting</code>'' if <i>thr</i> is aborting, <code>false</code> if
 *  <i>thr</i> terminated normally, and <code>nil</code> if <i>thr</i>
 *  terminated with an exception.
 *     
 *     a = Thread.new { raise("die now") }
 *     b = Thread.new { Thread.stop }
 *     c = Thread.new { Thread.exit }
 *     d = Thread.new { sleep }
 *     Thread.critical = true
 *     d.kill                  #=> #<Thread:0x401b3678 aborting>
 *     a.status                #=> nil
 *     b.status                #=> "sleep"
 *     c.status                #=> false
 *     d.status                #=> "aborting"
 *     Thread.current.status   #=> "run"
 */

static VALUE
rb_thread_status(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    if (rb_thread_dead(th)) {
	if (!NIL_P(th->errinfo) && (th->flags & THREAD_RAISED))
	    return Qnil;
	return Qfalse;
    }

    return rb_str_new2(thread_status_name(th->status));
}


/*
 *  call-seq:
 *     thr.alive?   => true or false
 *  
 *  Returns <code>true</code> if <i>thr</i> is running or sleeping.
 *     
 *     thr = Thread.new { }
 *     thr.join                #=> #<Thread:0x401b3fb0 dead>
 *     Thread.current.alive?   #=> true
 *     thr.alive?              #=> false
 */

static VALUE
rb_thread_alive_p(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    if (rb_thread_dead(th)) return Qfalse;
    return Qtrue;
}


/*
 *  call-seq:
 *     thr.stop?   => true or false
 *  
 *  Returns <code>true</code> if <i>thr</i> is dead or sleeping.
 *     
 *     a = Thread.new { Thread.stop }
 *     b = Thread.current
 *     a.stop?   #=> true
 *     b.stop?   #=> false
 */

static VALUE
rb_thread_stop_p(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    if (rb_thread_dead(th)) return Qtrue;
    if (th->status == THREAD_STOPPED) return Qtrue;
    return Qfalse;
}

static void
rb_thread_wait_other_threads()
{
    rb_thread_t th;
    int found;

    /* wait other threads to terminate */
    while (curr_thread != curr_thread->next) {
	found = 0;
	FOREACH_THREAD(th) {
	    if (th != curr_thread && th->status != THREAD_STOPPED) {
		found = 1;
		break;
	    }
	}
	END_FOREACH(th);
	if (!found) return;
	rb_thread_schedule();
    }
}

static void
rb_thread_cleanup()
{
    rb_thread_t curr, th;

    curr = curr_thread;
    while (curr->status == THREAD_KILLED) {
	curr = curr->prev;
    }

    FOREACH_THREAD_FROM(curr, th) {
	if (th->status != THREAD_KILLED) {
	    rb_thread_ready(th);
	    if (th != main_thread) {
		th->thgroup = 0;
		th->priority = 0;
		th->status = THREAD_TO_KILL;
		RDATA(th->thread)->dfree = NULL;
	    }
	}
    }
    END_FOREACH_FROM(curr, th);
}

int rb_thread_critical;


/*
 *  call-seq:
 *     Thread.critical   => true or false
 *  
 *  Returns the status of the global ``thread critical'' condition.
 */

static VALUE
rb_thread_critical_get()
{
    return rb_thread_critical?Qtrue:Qfalse;
}


/*
 *  call-seq:
 *     Thread.critical= boolean   => true or false
 *  
 *  Sets the status of the global ``thread critical'' condition and returns
 *  it. When set to <code>true</code>, prohibits scheduling of any existing
 *  thread. Does not block new threads from being created and run. Certain
 *  thread operations (such as stopping or killing a thread, sleeping in the
 *  current thread, and raising an exception) may cause a thread to be scheduled
 *  even when in a critical section.  <code>Thread::critical</code> is not
 *  intended for daily use: it is primarily there to support folks writing
 *  threading libraries.
 */

static VALUE
rb_thread_critical_set(obj, val)
    VALUE obj, val;
{
    rb_thread_critical = RTEST(val);
    return val;
}

void
rb_thread_interrupt()
{
    rb_thread_critical = 0;
    rb_thread_ready(main_thread);
    if (curr_thread == main_thread) {
	rb_interrupt();
    }
    if (!rb_thread_dead(curr_thread)) {
	if (THREAD_SAVE_CONTEXT(curr_thread)) {
	    return;
	}
    }
    curr_thread = main_thread;
    rb_thread_restore_context(curr_thread, RESTORE_INTERRUPT);
}

void
rb_thread_signal_raise(sig)
    char *sig;
{
    if (sig == 0) return;	/* should not happen */
    rb_thread_critical = 0;
    if (curr_thread == main_thread) {
	rb_thread_ready(curr_thread);
	rb_raise(rb_eSignal, "SIG%s", sig);
    }
    rb_thread_ready(main_thread);
    if (!rb_thread_dead(curr_thread)) {
	if (THREAD_SAVE_CONTEXT(curr_thread)) {
	    return;
	}
    }
    th_signm = sig;
    curr_thread = main_thread;
    rb_thread_restore_context(curr_thread, RESTORE_SIGNAL);
}

void
rb_thread_trap_eval(cmd, sig, safe)
    VALUE cmd;
    int sig, safe;
{
    rb_thread_critical = 0;
    if (curr_thread == main_thread) {
	rb_trap_eval(cmd, sig, safe);
	return;
    }
    if (!rb_thread_dead(curr_thread)) {
	if (THREAD_SAVE_CONTEXT(curr_thread)) {
	    return;
	}
    }
    th_cmd = cmd;
    th_sig = sig;
    th_safe = safe;
    curr_thread = main_thread;
    rb_thread_restore_context(curr_thread, RESTORE_TRAP);
}

static VALUE
rb_thread_raise(argc, argv, th)
    int argc;
    VALUE *argv;
    rb_thread_t th;
{
    volatile rb_thread_t th_save = th;
    VALUE exc;

    if (!th->next) {
	rb_raise(rb_eArgError, "unstarted thread");
    }
    if (rb_thread_dead(th)) return Qnil;
    exc = rb_make_exception(argc, argv);
    if (curr_thread == th) {
	rb_raise_jump(exc);
    }

    if (!rb_thread_dead(curr_thread)) {
	if (THREAD_SAVE_CONTEXT(curr_thread)) {
	    return th_save->thread;
	}
    }

    rb_thread_ready(th);
    curr_thread = th;

    th_raise_exception = exc;
    th_raise_node = ruby_current_node;
    rb_thread_restore_context(curr_thread, RESTORE_RAISE);
    return Qnil;		/* not reached */
}


/*
 *  call-seq:
 *     thr.raise(exception)
 *  
 *  Raises an exception (see <code>Kernel::raise</code>) from <i>thr</i>. The
 *  caller does not have to be <i>thr</i>.
 *     
 *     Thread.abort_on_exception = true
 *     a = Thread.new { sleep(200) }
 *     a.raise("Gotcha")
 *     
 *  <em>produces:</em>
 *     
 *     prog.rb:3: Gotcha (RuntimeError)
 *     	from prog.rb:2:in `initialize'
 *     	from prog.rb:2:in `new'
 *     	from prog.rb:2
 */

static VALUE
rb_thread_raise_m(argc, argv, thread)
    int argc;
    VALUE *argv;
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);

    if (ruby_safe_level > th->safe) {
	rb_secure(4);
    }
    rb_thread_raise(argc, argv, th);
    return Qnil;		/* not reached */
}

VALUE
rb_thread_local_aref(thread, id)
    VALUE thread;
    ID id;
{
    rb_thread_t th;
    VALUE val;

    th = rb_thread_check(thread);
    if (ruby_safe_level >= 4 && th != curr_thread) {
	rb_raise(rb_eSecurityError, "Insecure: thread locals");
    }
    if (!th->locals) return Qnil;
    if (st_lookup(th->locals, id, &val)) {
	return val;
    }
    return Qnil;
}


/*
 *  call-seq:
 *      thr[sym]   => obj or nil
 *  
 *  Attribute Reference---Returns the value of a thread-local variable, using
 *  either a symbol or a string name. If the specified variable does not exist,
 *  returns <code>nil</code>.
 *     
 *     a = Thread.new { Thread.current["name"] = "A"; Thread.stop }
 *     b = Thread.new { Thread.current[:name]  = "B"; Thread.stop }
 *     c = Thread.new { Thread.current["name"] = "C"; Thread.stop }
 *     Thread.list.each {|x| puts "#{x.inspect}: #{x[:name]}" }
 *     
 *  <em>produces:</em>
 *     
 *     #<Thread:0x401b3b3c sleep>: C
 *     #<Thread:0x401b3bc8 sleep>: B
 *     #<Thread:0x401b3c68 sleep>: A
 *     #<Thread:0x401bdf4c run>:
 */

static VALUE
rb_thread_aref(thread, id)
    VALUE thread, id;
{
    return rb_thread_local_aref(thread, rb_to_id(id));
}

VALUE
rb_thread_local_aset(thread, id, val)
    VALUE thread;
    ID id;
    VALUE val;
{
    rb_thread_t th = rb_thread_check(thread);

    if (ruby_safe_level >= 4 && th != curr_thread) {
	rb_raise(rb_eSecurityError, "Insecure: can't modify thread locals");
    }
    if (OBJ_FROZEN(thread)) rb_error_frozen("thread locals");

    if (!th->locals) {
	th->locals = st_init_numtable();
    }
    if (NIL_P(val)) {
	st_delete(th->locals, (st_data_t*)&id, 0);
	return Qnil;
    }
    st_insert(th->locals, id, val);

    return val;
}


/*
 *  call-seq:
 *      thr[sym] = obj   => obj
 *  
 *  Attribute Assignment---Sets or creates the value of a thread-local variable,
 *  using either a symbol or a string. See also <code>Thread#[]</code>.
 */

static VALUE
rb_thread_aset(thread, id, val)
    VALUE thread, id, val;
{
    return rb_thread_local_aset(thread, rb_to_id(id), val);
}


/*
 *  call-seq:
 *     thr.key?(sym)   => true or false
 *  
 *  Returns <code>true</code> if the given string (or symbol) exists as a
 *  thread-local variable.
 *     
 *     me = Thread.current
 *     me[:oliver] = "a"
 *     me.key?(:oliver)    #=> true
 *     me.key?(:stanley)   #=> false
 */

static VALUE
rb_thread_key_p(thread, id)
    VALUE thread, id;
{
    rb_thread_t th = rb_thread_check(thread);

    if (!th->locals) return Qfalse;
    if (st_lookup(th->locals, rb_to_id(id), 0))
	return Qtrue;
    return Qfalse;
}

static int
thread_keys_i(key, value, ary)
    ID key;
    VALUE value, ary;
{
    rb_ary_push(ary, ID2SYM(key));
    return ST_CONTINUE;
}


/*
 *  call-seq:
 *     thr.keys   => array
 *  
 *  Returns an an array of the names of the thread-local variables (as Symbols).
 *     
 *     thr = Thread.new do
 *       Thread.current[:cat] = 'meow'
 *       Thread.current["dog"] = 'woof'
 *     end
 *     thr.join   #=> #<Thread:0x401b3f10 dead>
 *     thr.keys   #=> [:dog, :cat]
 */

static VALUE
rb_thread_keys(thread)
    VALUE thread;
{
    rb_thread_t th = rb_thread_check(thread);
    VALUE ary = rb_ary_new();

    if (th->locals) {
	st_foreach(th->locals, thread_keys_i, ary);
    }
    return ary;
}

/*
 * call-seq:
 *   thr.inspect   => string
 *
 * Dump the name, id, and status of _thr_ to a string.
 */

static VALUE
rb_thread_inspect(thread)
    VALUE thread;
{
    char *cname = rb_obj_classname(thread);
    rb_thread_t th = rb_thread_check(thread);
    const char *status = thread_status_name(th->status);
    VALUE str;

    str = rb_str_new(0, strlen(cname)+7+16+9+1); /* 7:tags 16:addr 9:status 1:nul */
    sprintf(RSTRING(str)->ptr, "#<%s:0x%lx %s>", cname, thread, status);
    RSTRING(str)->len = strlen(RSTRING(str)->ptr);
    OBJ_INFECT(str, thread);

    return str;
}

void
rb_thread_atfork()
{
    rb_thread_t th;

    if (rb_thread_alone()) return;
    FOREACH_THREAD(th) {
	if (th != curr_thread) {
	    rb_thread_die(th);
	}
    }
    END_FOREACH(th);
    main_thread = curr_thread;
    curr_thread->next = curr_thread;
    curr_thread->prev = curr_thread;
}


/*
 *  Document-class: Continuation
 *
 *  Continuation objects are generated by
 *  <code>Kernel#callcc</code>. They hold a return address and execution
 *  context, allowing a nonlocal return to the end of the
 *  <code>callcc</code> block from anywhere within a program.
 *  Continuations are somewhat analogous to a structured version of C's
 *  <code>setjmp/longjmp</code> (although they contain more state, so
 *  you might consider them closer to threads).
 *     
 *  For instance:
 *     
 *     arr = [ "Freddie", "Herbie", "Ron", "Max", "Ringo" ]
 *     callcc{|$cc|}
 *     puts(message = arr.shift)
 *     $cc.call unless message =~ /Max/
 *     
 *  <em>produces:</em>
 *     
 *     Freddie
 *     Herbie
 *     Ron
 *     Max
 *     
 *  This (somewhat contrived) example allows the inner loop to abandon
 *  processing early:
 *     
 *     callcc {|cont|
 *       for i in 0..4
 *         print "\n#{i}: "
 *         for j in i*5...(i+1)*5
 *           cont.call() if j == 17
 *           printf "%3d", j
 *         end
 *       end
 *     }
 *     print "\n"
 *     
 *  <em>produces:</em>
 *     
 *     0:   0  1  2  3  4
 *     1:   5  6  7  8  9
 *     2:  10 11 12 13 14
 *     3:  15 16
 */

static VALUE rb_cCont;

/*
 *  call-seq:
 *     callcc {|cont| block }   =>  obj
 *  
 *  Generates a <code>Continuation</code> object, which it passes to the
 *  associated block. Performing a <em>cont</em><code>.call</code> will
 *  cause the <code>callcc</code> to return (as will falling through the
 *  end of the block). The value returned by the <code>callcc</code> is
 *  the value of the block, or the value passed to
 *  <em>cont</em><code>.call</code>. See class <code>Continuation</code>
 *  for more details. Also see <code>Kernel::throw</code> for
 *  an alternative mechanism for unwinding a call stack.
 */

static VALUE
rb_callcc(self)
    VALUE self;
{
    volatile VALUE cont;
    rb_thread_t th;
    volatile rb_thread_t th_save;
    struct tag *tag;
    struct RVarmap *vars;

    THREAD_ALLOC(th);
    cont = Data_Wrap_Struct(rb_cCont, thread_mark, thread_free, th);

    scope_dup(ruby_scope);
    for (tag=prot_tag; tag; tag=tag->prev) {
	scope_dup(tag->scope);
    }
    th->thread = curr_thread->thread;
    th->thgroup = cont_protect;

    for (vars = ruby_dyna_vars; vars; vars = vars->next) {
	if (FL_TEST(vars, DVAR_DONT_RECYCLE)) break;
	FL_SET(vars, DVAR_DONT_RECYCLE);
    }
    th_save = th;
    if (THREAD_SAVE_CONTEXT(th)) {
	return th_save->result;
    }
    else {
	return rb_yield(cont);
    }
}

/*
 *  call-seq:
 *     cont.call(args, ...)
 *     cont[args, ...]
 *  
 *  Invokes the continuation. The program continues from the end of the
 *  <code>callcc</code> block. If no arguments are given, the original
 *  <code>callcc</code> returns <code>nil</code>. If one argument is
 *  given, <code>callcc</code> returns it. Otherwise, an array
 *  containing <i>args</i> is returned.
 *     
 *     callcc {|cont|  cont.call }           #=> nil
 *     callcc {|cont|  cont.call 1 }         #=> 1
 *     callcc {|cont|  cont.call 1, 2, 3 }   #=> [1, 2, 3]
 */

static VALUE
rb_cont_call(argc, argv, cont)
    int argc;
    VALUE *argv;
    VALUE cont;
{
    rb_thread_t th = rb_thread_check(cont);

    if (th->thread != curr_thread->thread) {
	rb_raise(rb_eRuntimeError, "continuation called across threads");
    }
    if (th->thgroup != cont_protect) {
	rb_raise(rb_eRuntimeError, "continuation called across trap");
    }
    switch (argc) {
      case 0:
	th->result = Qnil;
	break;
      case 1:
	th->result = argv[0];
	break;
      default:
	th->result = rb_ary_new4(argc, argv);
	break;
    }

    rb_thread_restore_context(th, RESTORE_NORMAL);
    return Qnil;
}

struct thgroup {
    int enclosed;
    VALUE group;
};


/*
 * Document-class: ThreadGroup
 *
 *  <code>ThreadGroup</code> provides a means of keeping track of a number of
 *  threads as a group. A <code>Thread</code> can belong to only one
 *  <code>ThreadGroup</code> at a time; adding a thread to a new group will
 *  remove it from any previous group.
 *     
 *  Newly created threads belong to the same group as the thread from which they
 *  were created.
 */

static VALUE thgroup_s_alloc _((VALUE));
static VALUE
thgroup_s_alloc(klass)
    VALUE klass;
{
    VALUE group;
    struct thgroup *data;

    group = Data_Make_Struct(klass, struct thgroup, 0, free, data);
    data->enclosed = 0;
    data->group = group;

    return group;
}


/*
 *  call-seq:
 *     thgrp.list   => array
 *  
 *  Returns an array of all existing <code>Thread</code> objects that belong to
 *  this group.
 *     
 *     ThreadGroup::Default.list   #=> [#<Thread:0x401bdf4c run>]
 */

static VALUE
thgroup_list(group)
    VALUE group;
{
    struct thgroup *data;
    rb_thread_t th;
    VALUE ary;

    Data_Get_Struct(group, struct thgroup, data);
    ary = rb_ary_new();

    FOREACH_THREAD(th) {
	if (th->thgroup == data->group) {
	    rb_ary_push(ary, th->thread);
	}
    }
    END_FOREACH(th);

    return ary;
}


/*
 *  call-seq:
 *     thgrp.enclose   => thgrp
 *  
 *  Prevents threads from being added to or removed from the receiving
 *  <code>ThreadGroup</code>. New threads can still be started in an enclosed
 *  <code>ThreadGroup</code>.
 *     
 *     ThreadGroup::Default.enclose        #=> #<ThreadGroup:0x4029d914>
 *     thr = Thread::new { Thread.stop }   #=> #<Thread:0x402a7210 sleep>
 *     tg = ThreadGroup::new               #=> #<ThreadGroup:0x402752d4>
 *     tg.add thr
 *
 *  <em>produces:</em>
 *
 *     ThreadError: can't move from the enclosed thread group
 */

VALUE
thgroup_enclose(group)
    VALUE group;
{
    struct thgroup *data;

    Data_Get_Struct(group, struct thgroup, data);
    data->enclosed = 1;

    return group;
}


/*
 *  call-seq:
 *     thgrp.enclosed?   => true or false
 *  
 *  Returns <code>true</code> if <em>thgrp</em> is enclosed. See also
 *  ThreadGroup#enclose.
 */

static VALUE
thgroup_enclosed_p(group)
    VALUE group;
{
    struct thgroup *data;

    Data_Get_Struct(group, struct thgroup, data);
    if (data->enclosed) return Qtrue;
    return Qfalse;
}


/*
 *  call-seq:
 *     thgrp.add(thread)   => thgrp
 *  
 *  Adds the given <em>thread</em> to this group, removing it from any other
 *  group to which it may have previously belonged.
 *     
 *     puts "Initial group is #{ThreadGroup::Default.list}"
 *     tg = ThreadGroup.new
 *     t1 = Thread.new { sleep }
 *     t2 = Thread.new { sleep }
 *     puts "t1 is #{t1}"
 *     puts "t2 is #{t2}"
 *     tg.add(t1)
 *     puts "Initial group now #{ThreadGroup::Default.list}"
 *     puts "tg group now #{tg.list}"
 *     
 *  <em>produces:</em>
 *     
 *     Initial group is #<Thread:0x401bdf4c>
 *     t1 is #<Thread:0x401b3c90>
 *     t2 is #<Thread:0x401b3c18>
 *     Initial group now #<Thread:0x401b3c18>#<Thread:0x401bdf4c>
 *     tg group now #<Thread:0x401b3c90>
 */

static VALUE
thgroup_add(group, thread)
    VALUE group, thread;
{
    rb_thread_t th;
    struct thgroup *data;

    rb_secure(4);
    th = rb_thread_check(thread);
    if (!th->next || !th->prev) {
	rb_raise(rb_eTypeError, "wrong argument type %s (expected Thread)",
		 rb_obj_classname(thread));
    }

    if (OBJ_FROZEN(group)) {
      rb_raise(rb_eThreadError, "can't move to the frozen thread group");
    }
    Data_Get_Struct(group, struct thgroup, data);
    if (data->enclosed) {
	rb_raise(rb_eThreadError, "can't move to the enclosed thread group");
    }

    if (!th->thgroup) {
	return Qnil;
    }
    if (OBJ_FROZEN(th->thgroup)) {
	rb_raise(rb_eThreadError, "can't move from the frozen thread group");
    }
    Data_Get_Struct(th->thgroup, struct thgroup, data);
    if (data->enclosed) {
	rb_raise(rb_eThreadError, "can't move from the enclosed thread group");
    }

    th->thgroup = group;
    return group;
}

/* variables for recursive traversals */
static ID recursive_key;
static VALUE recursive_tbl;


/*
 *  +Thread+ encapsulates the behavior of a thread of
 *  execution, including the main thread of the Ruby script.
 *     
 *  In the descriptions of the methods in this class, the parameter _sym_
 *  refers to a symbol, which is either a quoted string or a
 *  +Symbol+ (such as <code>:name</code>).
 */

void
Init_Thread()
{
    VALUE cThGroup;

    rb_eThreadError = rb_define_class("ThreadError", rb_eStandardError);
    rb_cThread = rb_define_class("Thread", rb_cObject);
    rb_undef_alloc_func(rb_cThread);

    rb_define_singleton_method(rb_cThread, "new", rb_thread_s_new, -1);
    rb_define_method(rb_cThread, "initialize", rb_thread_initialize, -2);
    rb_define_singleton_method(rb_cThread, "start", rb_thread_start, -2);
    rb_define_singleton_method(rb_cThread, "fork", rb_thread_start, -2);

    rb_define_singleton_method(rb_cThread, "stop", rb_thread_stop, 0);
    rb_define_singleton_method(rb_cThread, "kill", rb_thread_s_kill, 1);
    rb_define_singleton_method(rb_cThread, "exit", rb_thread_exit, 0);
    rb_define_singleton_method(rb_cThread, "pass", rb_thread_pass, 0);
    rb_define_singleton_method(rb_cThread, "current", rb_thread_current, 0);
    rb_define_singleton_method(rb_cThread, "main", rb_thread_main, 0);
    rb_define_singleton_method(rb_cThread, "list", rb_thread_list, 0);

    rb_define_singleton_method(rb_cThread, "critical", rb_thread_critical_get, 0);
    rb_define_singleton_method(rb_cThread, "critical=", rb_thread_critical_set, 1);

    rb_define_singleton_method(rb_cThread, "abort_on_exception", rb_thread_s_abort_exc, 0);
    rb_define_singleton_method(rb_cThread, "abort_on_exception=", rb_thread_s_abort_exc_set, 1);

    rb_define_method(rb_cThread, "run", rb_thread_run, 0);
    rb_define_method(rb_cThread, "wakeup", rb_thread_wakeup, 0);
    rb_define_method(rb_cThread, "kill", rb_thread_kill, 0);
    rb_define_method(rb_cThread, "terminate", rb_thread_kill, 0);
    rb_define_method(rb_cThread, "exit", rb_thread_kill, 0);
    rb_define_method(rb_cThread, "value", rb_thread_value, 0);
    rb_define_method(rb_cThread, "status", rb_thread_status, 0);
    rb_define_method(rb_cThread, "join", rb_thread_join_m, -1);
    rb_define_method(rb_cThread, "alive?", rb_thread_alive_p, 0);
    rb_define_method(rb_cThread, "stop?", rb_thread_stop_p, 0);
    rb_define_method(rb_cThread, "raise", rb_thread_raise_m, -1);

    rb_define_method(rb_cThread, "abort_on_exception", rb_thread_abort_exc, 0);
    rb_define_method(rb_cThread, "abort_on_exception=", rb_thread_abort_exc_set, 1);

    rb_define_method(rb_cThread, "priority", rb_thread_priority, 0);
    rb_define_method(rb_cThread, "priority=", rb_thread_priority_set, 1);
    rb_define_method(rb_cThread, "safe_level", rb_thread_safe_level, 0);
    rb_define_method(rb_cThread, "group", rb_thread_group, 0);

    rb_define_method(rb_cThread, "[]", rb_thread_aref, 1);
    rb_define_method(rb_cThread, "[]=", rb_thread_aset, 2);
    rb_define_method(rb_cThread, "key?", rb_thread_key_p, 1);
    rb_define_method(rb_cThread, "keys", rb_thread_keys, 0);

    rb_define_method(rb_cThread, "inspect", rb_thread_inspect, 0);

    rb_cCont = rb_define_class("Continuation", rb_cObject);
    rb_undef_alloc_func(rb_cCont);
    rb_undef_method(CLASS_OF(rb_cCont), "new");
    rb_define_method(rb_cCont, "call", rb_cont_call, -1);
    rb_define_method(rb_cCont, "[]", rb_cont_call, -1);
    rb_define_global_function("callcc", rb_callcc, 0);
    rb_global_variable(&cont_protect);

    cThGroup = rb_define_class("ThreadGroup", rb_cObject);
    rb_define_alloc_func(cThGroup, thgroup_s_alloc);
    rb_define_method(cThGroup, "list", thgroup_list, 0);
    rb_define_method(cThGroup, "enclose", thgroup_enclose, 0);
    rb_define_method(cThGroup, "enclosed?", thgroup_enclosed_p, 0);
    rb_define_method(cThGroup, "add", thgroup_add, 1);
    thgroup_default = rb_obj_alloc(cThGroup);
    rb_define_const(cThGroup, "Default", thgroup_default);
    rb_global_variable(&thgroup_default);

    /* allocate main thread */
    main_thread = rb_thread_alloc(rb_cThread);
    curr_thread = main_thread->prev = main_thread->next = main_thread;
    recursive_key = rb_intern("__recursive_key__");
}

/*
 *  call-seq:
 *     catch(symbol) {| | block }  > obj
 *  
 *  +catch+ executes its block. If a +throw+ is
 *  executed, Ruby searches up its stack for a +catch+ block
 *  with a tag corresponding to the +throw+'s
 *  _symbol_. If found, that block is terminated, and
 *  +catch+ returns the value given to +throw+. If
 *  +throw+ is not called, the block terminates normally, and
 *  the value of +catch+ is the value of the last expression
 *  evaluated. +catch+ expressions may be nested, and the
 *  +throw+ call need not be in lexical scope.
 *     
 *     def routine(n)
 *       puts n
 *       throw :done if n <= 0
 *       routine(n-1)
 *     end
 *     
 *     
 *     catch(:done) { routine(3) }
 *     
 *  <em>produces:</em>
 *     
 *     3
 *     2
 *     1
 *     0
 */

static VALUE
rb_f_catch(dmy, tag)
    VALUE dmy, tag;
{
    int state;
    VALUE val = Qnil;		/* OK */

    tag = ID2SYM(rb_to_id(tag));
    PUSH_TAG(tag);
    if ((state = EXEC_TAG()) == 0) {
	val = rb_yield_0(tag, 0, 0, 0, Qfalse);
    }
    else if (state == TAG_THROW && tag == prot_tag->dst) {
	val = prot_tag->retval;
	state = 0;
    }
    POP_TAG();
    if (state) JUMP_TAG(state);

    return val;
}

static VALUE
catch_i(tag)
    VALUE tag;
{
    return rb_funcall(Qnil, rb_intern("catch"), 1, tag);
}

VALUE
rb_catch(tag, func, data)
    const char *tag;
    VALUE (*func)();
    VALUE data;
{
    return rb_iterate((VALUE(*)_((VALUE)))catch_i, ID2SYM(rb_intern(tag)), func, data);
}

/*
 *  call-seq:
 *     throw(symbol [, obj])
 *  
 *  Transfers control to the end of the active +catch+ block
 *  waiting for _symbol_. Raises +NameError+ if there
 *  is no +catch+ block for the symbol. The optional second
 *  parameter supplies a return value for the +catch+ block,
 *  which otherwise defaults to +nil+. For examples, see
 *  <code>Kernel::catch</code>.
 */

static VALUE
rb_f_throw(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE tag, value;
    struct tag *tt = prot_tag;

    rb_scan_args(argc, argv, "11", &tag, &value);
    tag = ID2SYM(rb_to_id(tag));

    while (tt) {
	if (tt->tag == tag) {
	    tt->dst = tag;
	    tt->retval = value;
	    break;
	}
	if (tt->tag == PROT_THREAD) {
	    rb_raise(rb_eThreadError, "uncaught throw `%s' in thread 0x%lx",
		     rb_id2name(SYM2ID(tag)),
		     curr_thread);
	}
	tt = tt->prev;
    }
    if (!tt) {
	rb_name_error(SYM2ID(tag), "uncaught throw `%s'", rb_id2name(SYM2ID(tag)));
    }
    rb_trap_restore_mask();
    JUMP_TAG(TAG_THROW);
#ifndef __GNUC__
    return Qnil; 		/* not reached */
#endif
}

void
rb_throw(tag, val)
    const char *tag;
    VALUE val;
{
    VALUE argv[2];

    argv[0] = ID2SYM(rb_intern(tag));
    argv[1] = val;
    rb_f_throw(2, argv);
}

static VALUE
recursive_check(obj)
    VALUE obj;
{
    VALUE hash = rb_thread_local_aref(rb_thread_current(), recursive_key);

    if (NIL_P(hash) || TYPE(hash) != T_HASH) {
	return Qfalse;
    }
    else {
	VALUE list = rb_hash_aref(hash, ID2SYM(ruby_frame->this_func));

	if (NIL_P(list) || TYPE(list) != T_ARRAY) return Qfalse;
	return rb_ary_includes(list, rb_obj_id(obj));
    }
}

static void
recursive_push(obj)
    VALUE obj;
{
    VALUE hash = rb_thread_local_aref(rb_thread_current(), recursive_key);
    VALUE list, sym;

    sym = ID2SYM(ruby_frame->this_func);
    if (NIL_P(hash) || TYPE(hash) != T_HASH) {
	hash = rb_hash_new();
	rb_thread_local_aset(rb_thread_current(), recursive_key, hash);
	list = Qnil;
    }
    else {
	list = rb_hash_aref(hash, sym);
    }
    if (NIL_P(list) || TYPE(list) != T_ARRAY) {
	list = rb_ary_new();
	rb_hash_aset(hash, sym, list);
    }
    rb_ary_push(list, rb_obj_id(obj));
}

static void
recursive_pop()
{
    VALUE hash = rb_thread_local_aref(rb_thread_current(), recursive_key);
    VALUE list, sym;

    sym = ID2SYM(ruby_frame->this_func);
    if (NIL_P(hash) || TYPE(hash) != T_HASH) {
	VALUE symname = rb_inspect(sym);
	VALUE thrname = rb_inspect(rb_thread_current());
	rb_raise(rb_eTypeError, "invalid inspect_tbl hash for %s in %s",
		 StringValuePtr(symname), StringValuePtr(thrname));
    }
    list = rb_hash_aref(hash, sym);
    if (NIL_P(list) || TYPE(list) != T_ARRAY) {
	VALUE symname = rb_inspect(sym);
	VALUE thrname = rb_inspect(rb_thread_current());
	rb_raise(rb_eTypeError, "invalid inspect_tbl list for %s in %s",
		 StringValuePtr(symname), StringValuePtr(thrname));
    }
    rb_ary_pop(list);
}

VALUE
rb_exec_recursive(func, obj, arg)
    VALUE (*func)(ANYARGS);	/* VALUE obj, VALUE arg, int flag */
    VALUE obj, arg;
{
    if (recursive_check(obj)) {
	return (*func)(obj, arg, Qtrue);
    }
    else {
	VALUE result;
	int state;

	recursive_push(obj);
	PUSH_TAG(PROT_NONE);
	if ((state = EXEC_TAG()) == 0) {
	    result = (*func)(obj, arg, Qfalse);
	}
	POP_TAG();
	recursive_pop();
	if (state) JUMP_TAG(state);
	return result;
    }
}
/**********************************************************************

  file.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Mon Nov 15 12:24:34 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#ifdef _WIN32
#include "missing/file.h"
#endif

#include "ruby.h"
#include "rubyio.h"
#include "rubysig.h"
#include "util.h"
#include "dln.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_FILE_H
# include <sys/file.h>
#else
int flock _((int, int));
#endif

#ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
#ifndef MAXPATHLEN
# define MAXPATHLEN 1024
#endif

#include <time.h>

VALUE rb_time_new _((time_t, time_t));

#ifdef HAVE_UTIME_H
#include <utime.h>
#elif defined HAVE_SYS_UTIME_H
#include <sys/utime.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#ifndef HAVE_STRING_H
char *strrchr _((const char*,const char));
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_SYS_MKDEV_H
#include <sys/mkdev.h>
#endif

#if !defined HAVE_LSTAT && !defined lstat
#define lstat stat
#endif

VALUE rb_cFile;
VALUE rb_mFileTest;
static VALUE rb_cStat;

VALUE
rb_get_path(obj)
    VALUE obj;
{
    VALUE tmp;
    static ID to_path;

    rb_check_safe_obj(obj);
    tmp = rb_check_string_type(obj);
    if (!NIL_P(tmp)) goto exit;

    if (!to_path) {
	to_path = rb_intern("to_path");
    }
    if (rb_respond_to(obj, to_path)) {
	obj = rb_funcall(obj, to_path, 0, 0);
    }
    tmp = rb_str_to_str(obj);
  exit:
    if (obj != tmp) {
	rb_check_safe_obj(tmp);
    }
    return tmp;
}

static long
apply2files(func, vargs, arg)
    void (*func)();
    VALUE vargs;
    void *arg;
{
    long i;
    VALUE path;
    struct RArray *args = RARRAY(vargs);

    rb_secure(4);
    for (i=0; i<args->len; i++) {
	path = rb_get_path(args->ptr[i]);
	(*func)(StringValueCStr(path), arg);
    }

    return args->len;
}

/*
 *  call-seq:
 *     file.path -> filename
 *  
 *  Returns the pathname used to create <i>file</i> as a string. Does
 *  not normalize the name.
 *     
 *     File.new("testfile").path               #=> "testfile"
 *     File.new("/tmp/../tmp/xxx", "w").path   #=> "/tmp/../tmp/xxx"
 *     
 */

static VALUE
rb_file_path(obj)
    VALUE obj;
{
    OpenFile *fptr;

    fptr = RFILE(rb_io_taint_check(obj))->fptr;
    rb_io_check_initialized(fptr);
    if (!fptr->path) return Qnil;
    return rb_tainted_str_new2(fptr->path);
}

static VALUE
stat_new_0(klass, st)
    VALUE klass;
    struct stat *st;
{
    struct stat *nst = 0;

    if (st) {
	nst = ALLOC(struct stat);
	*nst = *st;
    }
    return Data_Wrap_Struct(klass, NULL, free, nst);
}

static VALUE
stat_new(st)
    struct stat *st;
{
    return stat_new_0(rb_cStat, st);
}

static struct stat*
get_stat(self)
    VALUE self;
{
    struct stat* st;
    Data_Get_Struct(self, struct stat, st);
    if (!st) rb_raise(rb_eTypeError, "uninitialized File::Stat");
    return st;
}

/*
 *  call-seq:
 *     stat <=> other_stat    => -1, 0, 1
 *  
 *  Compares <code>File::Stat</code> objects by comparing their
 *  respective modification times.
 *     
 *     f1 = File.new("f1", "w")
 *     sleep 1
 *     f2 = File.new("f2", "w")
 *     f1.stat <=> f2.stat   #=> -1
 */

static VALUE
rb_stat_cmp(self, other)
    VALUE self, other;
{
    if (rb_obj_is_kind_of(other, rb_obj_class(self))) {
	time_t t1 = get_stat(self)->st_mtime;
	time_t t2 = get_stat(other)->st_mtime;
	if (t1 == t2)
	    return INT2FIX(0);
	else if (t1 < t2)
	    return INT2FIX(-1);
	else
	    return INT2FIX(1);
    }
    return Qnil;
}

/*
 *  call-seq:
 *     stat.dev    => fixnum
 *  
 *  Returns an integer representing the device on which <i>stat</i>
 *  resides.
 *     
 *     File.stat("testfile").dev   #=> 774
 */

static VALUE
rb_stat_dev(self)
    VALUE self;
{
    return INT2NUM(get_stat(self)->st_dev);
}

/*
 *  call-seq:
 *     stat.dev_major   => fixnum
 *  
 *  Returns the major part of <code>File_Stat#dev</code> or
 *  <code>nil</code>.
 *     
 *     File.stat("/dev/fd1").dev_major   #=> 2
 *     File.stat("/dev/tty").dev_major   #=> 5
 */

static VALUE
rb_stat_dev_major(self)
    VALUE self;
{
#if defined(major)
    long dev = get_stat(self)->st_dev;
    return ULONG2NUM(major(dev));
#else
    return Qnil;
#endif
}

/*
 *  call-seq:
 *     stat.dev_minor   => fixnum
 *  
 *  Returns the minor part of <code>File_Stat#dev</code> or
 *  <code>nil</code>.
 *     
 *     File.stat("/dev/fd1").dev_minor   #=> 1
 *     File.stat("/dev/tty").dev_minor   #=> 0
 */

static VALUE
rb_stat_dev_minor(self)
    VALUE self;
{
#if defined(minor)
    long dev = get_stat(self)->st_dev;
    return ULONG2NUM(minor(dev));
#else
    return Qnil;
#endif
}


/*
 *  call-seq:
 *     stat.ino   => fixnum
 *  
 *  Returns the inode number for <i>stat</i>.
 *     
 *     File.stat("testfile").ino   #=> 1083669
 *     
 */

static VALUE
rb_stat_ino(self)
    VALUE self;
{
#ifdef HUGE_ST_INO
    return ULL2NUM(get_stat(self)->st_ino);
#else
    return ULONG2NUM(get_stat(self)->st_ino);
#endif
}

/*
 *  call-seq:
 *     stat.mode   => fixnum
 *  
 *  Returns an integer representing the permission bits of
 *  <i>stat</i>. The meaning of the bits is platform dependent; on
 *  Unix systems, see <code>stat(2)</code>.
 *     
 *     File.chmod(0644, "testfile")   #=> 1
 *     s = File.stat("testfile")
 *     sprintf("%o", s.mode)          #=> "100644"
 */

static VALUE
rb_stat_mode(self)
    VALUE self;
{
#ifdef __BORLANDC__
    return UINT2NUM((unsigned short)(get_stat(self)->st_mode));
#else
    return UINT2NUM(get_stat(self)->st_mode);
#endif
}

/*
 *  call-seq:
 *     stat.nlink   => fixnum
 *  
 *  Returns the number of hard links to <i>stat</i>.
 *     
 *     File.stat("testfile").nlink             #=> 1
 *     File.link("testfile", "testfile.bak")   #=> 0
 *     File.stat("testfile").nlink             #=> 2
 *     
 */

static VALUE
rb_stat_nlink(self)
    VALUE self;
{
    return UINT2NUM(get_stat(self)->st_nlink);
}


/*
 *  call-seq:
 *     stat.uid    => fixnum
 *  
 *  Returns the numeric user id of the owner of <i>stat</i>.
 *     
 *     File.stat("testfile").uid   #=> 501
 *     
 */

static VALUE
rb_stat_uid(self)
    VALUE self;
{
    return UINT2NUM(get_stat(self)->st_uid);
}

/*
 *  call-seq:
 *     stat.gid   => fixnum
 *  
 *  Returns the numeric group id of the owner of <i>stat</i>.
 *     
 *     File.stat("testfile").gid   #=> 500
 *     
 */

static VALUE
rb_stat_gid(self)
    VALUE self;
{
    return UINT2NUM(get_stat(self)->st_gid);
}


/*
 *  call-seq:
 *     stat.rdev   =>  fixnum or nil
 *  
 *  Returns an integer representing the device type on which
 *  <i>stat</i> resides. Returns <code>nil</code> if the operating
 *  system doesn't support this feature.
 *     
 *     File.stat("/dev/fd1").rdev   #=> 513
 *     File.stat("/dev/tty").rdev   #=> 1280
 */

static VALUE
rb_stat_rdev(self)
    VALUE self;
{
#ifdef HAVE_ST_RDEV
    return ULONG2NUM(get_stat(self)->st_rdev);
#else
    return Qnil;
#endif
}

/*
 *  call-seq:
 *     stat.rdev_major   => fixnum
 *  
 *  Returns the major part of <code>File_Stat#rdev</code> or
 *  <code>nil</code>.
 *     
 *     File.stat("/dev/fd1").rdev_major   #=> 2
 *     File.stat("/dev/tty").rdev_major   #=> 5
 */

static VALUE
rb_stat_rdev_major(self)
    VALUE self;
{
#if defined(HAVE_ST_RDEV) && defined(major)
    long rdev = get_stat(self)->st_rdev;
    return ULONG2NUM(major(rdev));
#else
    return Qnil;
#endif
}

/*
 *  call-seq:
 *     stat.rdev_minor   => fixnum
 *  
 *  Returns the minor part of <code>File_Stat#rdev</code> or
 *  <code>nil</code>.
 *     
 *     File.stat("/dev/fd1").rdev_minor   #=> 1
 *     File.stat("/dev/tty").rdev_minor   #=> 0
 */

static VALUE
rb_stat_rdev_minor(self)
    VALUE self;
{
#if defined(HAVE_ST_RDEV) && defined(minor)
    long rdev = get_stat(self)->st_rdev;
    return ULONG2NUM(minor(rdev));
#else
    return Qnil;
#endif
}

/*
 *  call-seq:
 *     stat.size    => fixnum
 *  
 *  Returns the size of <i>stat</i> in bytes.
 *     
 *     File.stat("testfile").size   #=> 66
 */

static VALUE
rb_stat_size(self)
    VALUE self;
{
    return OFFT2NUM(get_stat(self)->st_size);
}

/*
 *  call-seq:
 *     stat.blksize   => integer or nil
 *  
 *  Returns the native file system's block size. Will return <code>nil</code>
 *  on platforms that don't support this information.
 *     
 *     File.stat("testfile").blksize   #=> 4096
 *     
 */

static VALUE
rb_stat_blksize(self)
    VALUE self;
{
#ifdef HAVE_ST_BLKSIZE
    return ULONG2NUM(get_stat(self)->st_blksize);
#else
    return Qnil;
#endif
}

/*
 *  call-seq:
 *     stat.blocks    => integer or nil
 *  
 *  Returns the number of native file system blocks allocated for this
 *  file, or <code>nil</code> if the operating system doesn't 
 *  support this feature.
 *     
 *     File.stat("testfile").blocks   #=> 2
 */

static VALUE
rb_stat_blocks(self)
    VALUE self;
{
#ifdef HAVE_ST_BLOCKS
    return ULONG2NUM(get_stat(self)->st_blocks);
#else
    return Qnil;
#endif
}


/*
 *  call-seq:
 *     stat.atime   => time
 *  
 *  Returns the last access time for this file as an object of class
 *  <code>Time</code>.
 *     
 *     File.stat("testfile").atime   #=> Wed Dec 31 18:00:00 CST 1969
 *     
 */

static VALUE
rb_stat_atime(self)
    VALUE self;
{
    return rb_time_new(get_stat(self)->st_atime, 0);
}

/*
 *  call-seq:
 *     stat.mtime -> aTime
 *  
 *  Returns the modification time of <i>stat</i>.
 *     
 *     File.stat("testfile").mtime   #=> Wed Apr 09 08:53:14 CDT 2003
 *     
 */

static VALUE
rb_stat_mtime(self)
    VALUE self;
{
    return rb_time_new(get_stat(self)->st_mtime, 0);
}

/*
 *  call-seq:
 *     stat.ctime -> aTime
 *  
 *  Returns the change time for <i>stat</i> (that is, the time
 *  directory information about the file was changed, not the file
 *  itself).
 *     
 *     File.stat("testfile").ctime   #=> Wed Apr 09 08:53:14 CDT 2003
 *     
 */

static VALUE
rb_stat_ctime(self)
    VALUE self;
{
    return rb_time_new(get_stat(self)->st_ctime, 0);
}

/*
 * call-seq:
 *   stat.inspect  =>  string
 *
 * Produce a nicely formatted description of <i>stat</i>.
 *
 *   File.stat("/etc/passwd").inspect
 *      #=> "#<File::Stat dev=0xe000005, ino=1078078, mode=0100644, 
 *           nlink=1, uid=0, gid=0, rdev=0x0, size=1374, blksize=4096, 
 *           blocks=8, atime=Wed Dec 10 10:16:12 CST 2003, 
 *           mtime=Fri Sep 12 15:41:41 CDT 2003, 
 *           ctime=Mon Oct 27 11:20:27 CST 2003>"
 */

static VALUE
rb_stat_inspect(self)
    VALUE self;
{
    VALUE str;
    int i;
    static struct {
        char *name;
        VALUE (*func)();
    } member[] = {
        {"dev",     rb_stat_dev},
        {"ino",     rb_stat_ino},
        {"mode",    rb_stat_mode},
        {"nlink",   rb_stat_nlink},
        {"uid",     rb_stat_uid},
        {"gid",     rb_stat_gid},
        {"rdev",    rb_stat_rdev},
        {"size",    rb_stat_size},
        {"blksize", rb_stat_blksize},
        {"blocks",  rb_stat_blocks},
        {"atime",   rb_stat_atime},
        {"mtime",   rb_stat_mtime},
        {"ctime",   rb_stat_ctime},
    };

    str = rb_str_buf_new2("#<");
    rb_str_buf_cat2(str, rb_obj_classname(self));
    rb_str_buf_cat2(str, " ");

    for (i = 0; i < sizeof(member)/sizeof(member[0]); i++) {
	VALUE v;

	if (i > 0) {
	    rb_str_buf_cat2(str, ", ");
	}
	rb_str_buf_cat2(str, member[i].name);
	rb_str_buf_cat2(str, "=");
	v = (*member[i].func)(self);
	if (i == 2) {		/* mode */
	    char buf[32];

	    sprintf(buf, "0%lo", NUM2ULONG(v));
	    rb_str_buf_cat2(str, buf);
	}
	else if (i == 0 || i == 6) { /* dev/rdev */
	    char buf[32];

	    sprintf(buf, "0x%lx", NUM2ULONG(v));
	    rb_str_buf_cat2(str, buf);
	}
	else {
	    rb_str_append(str, rb_inspect(v));
	}
    }
    rb_str_buf_cat2(str, ">");
    OBJ_INFECT(str, self);

    return str;
}

static int
rb_stat(file, st)
    VALUE file;
    struct stat *st;
{
    VALUE tmp;

    rb_secure(2);
    tmp = rb_check_convert_type(file, T_FILE, "IO", "to_io");
    if (!NIL_P(tmp)) {
	OpenFile *fptr;

	GetOpenFile(tmp, fptr);
	return fstat(fptr->fd, st);
    }
    FilePathValue(file);
    return stat(StringValueCStr(file), st);
}

/*
 *  call-seq:
 *     File.stat(file_name)   =>  stat
 *  
 *  Returns a <code>File::Stat</code> object for the named file (see
 *  <code>File::Stat</code>).
 *     
 *     File.stat("testfile").mtime   #=> Tue Apr 08 12:58:04 CDT 2003
 *     
 */

static VALUE
rb_file_s_stat(klass, fname)
    VALUE klass, fname;
{
    struct stat st;

    rb_secure(4);
    FilePathValue(fname);
    if (rb_stat(fname, &st) < 0) {
	rb_sys_fail(StringValueCStr(fname));
    }
    return stat_new(&st);
}

/*
 *  call-seq:
 *     ios.stat    => stat
 *  
 *  Returns status information for <em>ios</em> as an object of type
 *  <code>File::Stat</code>.
 *     
 *     f = File.new("testfile")
 *     s = f.stat
 *     "%o" % s.mode   #=> "100644"
 *     s.blksize       #=> 4096
 *     s.atime         #=> Wed Apr 09 08:53:54 CDT 2003
 *     
 */

static VALUE
rb_io_stat(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fptr->fd, &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return stat_new(&st);
}

/*
 *  call-seq:
 *     File.lstat(file_name)   => stat
 *  
 *  Same as <code>File::stat</code>, but does not follow the last symbolic
 *  link. Instead, reports on the link itself.
 *     
 *     File.symlink("testfile", "link2test")   #=> 0
 *     File.stat("testfile").size              #=> 66
 *     File.lstat("link2test").size            #=> 8
 *     File.stat("link2test").size             #=> 66
 *     
 */

static VALUE
rb_file_s_lstat(klass, fname)
    VALUE klass, fname;
{
#ifdef HAVE_LSTAT
    struct stat st;

    rb_secure(2);
    FilePathValue(fname);
    if (lstat(StringValueCStr(fname), &st) == -1) {
	rb_sys_fail(RSTRING(fname)->ptr);
    }
    return stat_new(&st);
#else
    return rb_file_s_stat(klass, fname);
#endif
}


/*
 *  call-seq:
 *     file.lstat   =>  stat
 *  
 *  Same as <code>IO#stat</code>, but does not follow the last symbolic
 *  link. Instead, reports on the link itself.
 *     
 *     File.symlink("testfile", "link2test")   #=> 0
 *     File.stat("testfile").size              #=> 66
 *     f = File.new("link2test")
 *     f.lstat.size                            #=> 8
 *     f.stat.size                             #=> 66
 */

static VALUE
rb_file_lstat(obj)
    VALUE obj;
{
#ifdef HAVE_LSTAT
    OpenFile *fptr;
    struct stat st;

    rb_secure(2);
    GetOpenFile(obj, fptr);
    if (!fptr->path) return Qnil;
    if (lstat(fptr->path, &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return stat_new(&st);
#else
    return rb_io_stat(obj);
#endif
}

static int
group_member(gid)
    GETGROUPS_T gid;
{
#ifndef _WIN32
    if (getgid() ==  gid)
	return Qtrue;

# ifdef HAVE_GETGROUPS
#  ifndef NGROUPS
#   ifdef NGROUPS_MAX
#    define NGROUPS NGROUPS_MAX
#   else
#    define NGROUPS 32
#   endif
#  endif
    {
	GETGROUPS_T gary[NGROUPS];
	int anum;

	anum = getgroups(NGROUPS, gary);
	while (--anum >= 0)
	    if (gary[anum] == gid)
		return Qtrue;
    }
# endif
#endif
    return Qfalse;
}

#ifndef S_IXUGO
#  define S_IXUGO		(S_IXUSR | S_IXGRP | S_IXOTH)
#endif

int
eaccess(path, mode)
     const char *path;
     int mode;
{
#if defined(S_IXGRP) && !defined(_WIN32) && !defined(__CYGWIN__)
    struct stat st;
    int euid;

    if (stat(path, &st) < 0) return -1;

    euid = geteuid();

    if (euid == 0) {
	/* Root can read or write any file. */
	if (!(mode & X_OK))
	    return 0;

	/* Root can execute any file that has any one of the execute
	   bits set. */
	if (st.st_mode & S_IXUGO)
	    return 0;

	return -1;
    }

    if (st.st_uid == euid)        /* owner */
	mode <<= 6;
    else if (getegid() == st.st_gid || group_member(st.st_gid))
	mode <<= 3;

    if ((st.st_mode & mode) == mode) return 0;

    return -1;
#else
# if _MSC_VER >= 1400
    mode &= 6;
# endif
    return access(path, mode);
#endif
}


/*
 * Document-class: FileTest
 *
 *  <code>FileTest</code> implements file test operations similar to
 *  those used in <code>File::Stat</code>. It exists as a standalone
 *  module, and its methods are also insinuated into the <code>File</code>
 *  class. (Note that this is not done by inclusion: the interpreter cheats).
 *     
 */


/*
 * call-seq:
 *   File.directory?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a directory,
 * <code>false</code> otherwise.
 *
 *    File.directory?(".")
 */

static VALUE
test_d(obj, fname)
    VALUE obj, fname;
{
#ifndef S_ISDIR
#   define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#endif

    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (S_ISDIR(st.st_mode)) return Qtrue;
    return Qfalse;
}

/*
 * call-seq:
 *   File.pipe?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a pipe.
 */

static VALUE
test_p(obj, fname)
    VALUE obj, fname;
{
#ifdef S_IFIFO
#  ifndef S_ISFIFO
#    define S_ISFIFO(m) ((m & S_IFMT) == S_IFIFO)
#  endif

    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (S_ISFIFO(st.st_mode)) return Qtrue;

#endif
    return Qfalse;
}

/*
 * call-seq:
 *   File.symlink?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a symbolic link.
 */

static VALUE
test_l(obj, fname)
    VALUE obj, fname;
{
#ifndef S_ISLNK
#  ifdef _S_ISLNK
#    define S_ISLNK(m) _S_ISLNK(m)
#  else
#    ifdef _S_IFLNK
#      define S_ISLNK(m) ((m & S_IFMT) == _S_IFLNK)
#    else
#      ifdef S_IFLNK
#	 define S_ISLNK(m) ((m & S_IFMT) == S_IFLNK)
#      endif
#    endif
#  endif
#endif

#ifdef S_ISLNK
    struct stat st;

    rb_secure(2);
    FilePathValue(fname);
    if (lstat(StringValueCStr(fname), &st) < 0) return Qfalse;
    if (S_ISLNK(st.st_mode)) return Qtrue;
#endif

    return Qfalse;
}

/*
 * call-seq:
 *   File.socket?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a socket.
 */

static VALUE
test_S(obj, fname)
    VALUE obj, fname;
{
#ifndef S_ISSOCK
#  ifdef _S_ISSOCK
#    define S_ISSOCK(m) _S_ISSOCK(m)
#  else
#    ifdef _S_IFSOCK
#      define S_ISSOCK(m) ((m & S_IFMT) == _S_IFSOCK)
#    else
#      ifdef S_IFSOCK
#	 define S_ISSOCK(m) ((m & S_IFMT) == S_IFSOCK)
#      endif
#    endif
#  endif
#endif

#ifdef S_ISSOCK
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (S_ISSOCK(st.st_mode)) return Qtrue;

#endif
    return Qfalse;
}

/*
 * call-seq:
 *   File.blockdev?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a block device.
 */

static VALUE
test_b(obj, fname)
    VALUE obj, fname;
{
#ifndef S_ISBLK
#   ifdef S_IFBLK
#	define S_ISBLK(m) ((m & S_IFMT) == S_IFBLK)
#   else
#	define S_ISBLK(m) (0)  /* anytime false */
#   endif
#endif

#ifdef S_ISBLK
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (S_ISBLK(st.st_mode)) return Qtrue;

#endif
    return Qfalse;
}

/*
 * call-seq:
 *   File.chardev?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a character device.
 */
static VALUE
test_c(obj, fname)
    VALUE obj, fname;
{
#ifndef S_ISCHR
#   define S_ISCHR(m) ((m & S_IFMT) == S_IFCHR)
#endif

    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (S_ISCHR(st.st_mode)) return Qtrue;

    return Qfalse;
}


/*
 * call-seq:
 *    File.exist?(file_name)    =>  true or false
 *    File.exists?(file_name)   =>  true or false    (obsolete)
 *
 * Return <code>true</code> if the named file exists.
 */

static VALUE
test_e(obj, fname)
    VALUE obj, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    File.readable?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file is readable by the effective
 * user id of this process.
 */

static VALUE
test_r(obj, fname)
    VALUE obj, fname;
{
    rb_secure(2);
    FilePathValue(fname);
    if (eaccess(StringValueCStr(fname), R_OK) < 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    File.readable_real?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file is readable by the real
 * user id of this process.
 */

static VALUE
test_R(obj, fname)
    VALUE obj, fname;
{
    rb_secure(2);
    FilePathValue(fname);
    if (access(StringValueCStr(fname), R_OK) < 0) return Qfalse;
    return Qtrue;
}

#ifndef S_IRUGO
#  define S_IRUGO		(S_IRUSR | S_IRGRP | S_IROTH)
#endif

#ifndef S_IWUGO
#  define S_IWUGO		(S_IWUSR | S_IWGRP | S_IWOTH)
#endif

/*
 * call-seq:
 *    File.world_readable?(file_name)   => fixnum or nil
 *
 * If <i>file_name</i> is readable by others, returns an integer
 * representing the file permission bits of <i>file_name</i>. Returns
 * <code>nil</code> otherwise. The meaning of the bits is platform
 * dependent; on Unix systems, see <code>stat(2)</code>.
 *     
 *    File.world_readable?("/etc/passwd")	    # => 420
 *    m = File.world_readable?("/etc/passwd")
 *    sprintf("%o", m)				    # => "644"
 */

static VALUE
test_wr(obj, fname)
    VALUE obj, fname;
{
#ifdef S_IROTH
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qnil;
    if ((st.st_mode & (S_IROTH)) == S_IROTH) {
	return UINT2NUM(st.st_mode & (S_IRUGO|S_IWUGO|S_IXUGO));
    }
#endif
    return Qnil;
}

/*
 * call-seq:
 *    File.writable?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file is writable by the effective
 * user id of this process.
 */

static VALUE
test_w(obj, fname)
    VALUE obj, fname;
{
    rb_secure(2);
    FilePathValue(fname);
    if (eaccess(StringValueCStr(fname), W_OK) < 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    File.writable_real?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file is writable by the real
 * user id of this process.
 */

static VALUE
test_W(obj, fname)
    VALUE obj, fname;
{
    rb_secure(2);
    FilePathValue(fname);
    if (access(StringValueCStr(fname), W_OK) < 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    File.world_writable?(file_name)   => fixnum or nil
 *
 * If <i>file_name</i> is writable by others, returns an integer
 * representing the file permission bits of <i>file_name</i>. Returns
 * <code>nil</code> otherwise. The meaning of the bits is platform
 * dependent; on Unix systems, see <code>stat(2)</code>.
 *     
 *    File.world_writable?("/tmp")		    #=> 511
 *    m = File.world_writable?("/tmp")
 *    sprintf("%o", m)				    #=> "777"
 */

static VALUE
test_ww(obj, fname)
    VALUE obj, fname;
{
#ifdef S_IWOTH
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if ((st.st_mode & (S_IWOTH)) == S_IWOTH) {
	return UINT2NUM(st.st_mode & (S_IRUGO|S_IWUGO|S_IXUGO));
    }
#endif
    return Qnil;
}

/*
 * call-seq:
 *    File.executable?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file is executable by the effective
 * user id of this process.
 */

static VALUE
test_x(obj, fname)
    VALUE obj, fname;
{
    rb_secure(2);
    FilePathValue(fname);
    if (eaccess(StringValueCStr(fname), X_OK) < 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    File.executable_real?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file is executable by the real
 * user id of this process.
 */

static VALUE
test_X(obj, fname)
    VALUE obj, fname;
{
    rb_secure(2);
    FilePathValue(fname);
    if (access(StringValueCStr(fname), X_OK) < 0) return Qfalse;
    return Qtrue;
}

#ifndef S_ISREG
#   define S_ISREG(m) ((m & S_IFMT) == S_IFREG)
#endif

/*
 * call-seq:
 *    File.file?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file exists and is a
 * regular file.
 */

static VALUE
test_f(obj, fname)
    VALUE obj, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (S_ISREG(st.st_mode)) return Qtrue;
    return Qfalse;
}

/*
 * call-seq:
 *    File.zero?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file exists and has
 * a zero size.
 */

static VALUE
test_z(obj, fname)
    VALUE obj, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (st.st_size == 0) return Qtrue;
    return Qfalse;
}

/*
 * call-seq:
 *    File.file?(file_name)   => integer  or  nil
 *
 * Returns <code>nil</code> if <code>file_name</code> doesn't
 * exist or has zero size, the size of the file otherwise.
 */

static VALUE
test_s(obj, fname)
    VALUE obj, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qnil;
    if (st.st_size == 0) return Qnil;
    return OFFT2NUM(st.st_size);
}

/*
 * call-seq:
 *    File.owned?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file exists and the
 * effective used id of the calling process is the owner of
 * the file.
 */

static VALUE
test_owned(obj, fname)
    VALUE obj, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (st.st_uid == geteuid()) return Qtrue;
    return Qfalse;
}

static VALUE
test_rowned(obj, fname)
    VALUE obj, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (st.st_uid == getuid()) return Qtrue;
    return Qfalse;
}

/*
 * call-seq:
 *    File.grpowned?(file_name)   => true or false
 *
 * Returns <code>true</code> if the named file exists and the
 * effective group id of the calling process is the owner of
 * the file. Returns <code>false</code> on Windows.
 */

static VALUE
test_grpowned(obj, fname)
    VALUE obj, fname;
{
#ifndef _WIN32
    struct stat st;

    if (rb_stat(fname, &st) < 0) return Qfalse;
    if (st.st_gid == getegid()) return Qtrue;
#endif
    return Qfalse;
}

#if defined(S_ISUID) || defined(S_ISGID) || defined(S_ISVTX)
static VALUE
check3rdbyte(fname, mode)
    VALUE fname;
    int mode;
{
    struct stat st;

    rb_secure(2);
    FilePathValue(fname);
    if (stat(StringValueCStr(fname), &st) < 0) return Qfalse;
    if (st.st_mode & mode) return Qtrue;
    return Qfalse;
}
#endif

/*
 * call-seq:
 *   File.setuid?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a has the setuid bit set.
 */

static VALUE
test_suid(obj, fname)
    VALUE obj, fname;
{
#ifdef S_ISUID
    return check3rdbyte(fname, S_ISUID);
#else
    return Qfalse;
#endif
}

/*
 * call-seq:
 *   File.setgid?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a has the setgid bit set.
 */

static VALUE
test_sgid(obj, fname)
    VALUE obj, fname;
{
#ifdef S_ISGID
    return check3rdbyte(fname, S_ISGID);
#else
    return Qfalse;
#endif
}

/*
 * call-seq:
 *   File.sticky?(file_name)   =>  true or false
 *
 * Returns <code>true</code> if the named file is a has the sticky bit set.
 */

static VALUE
test_sticky(obj, fname)
    VALUE obj, fname;
{
#ifdef S_ISVTX
    return check3rdbyte(fname, S_ISVTX);
#else
    return Qnil;
#endif
}

/*
 * call-seq:
 *    File.size(file_name)   => integer
 *
 * Returns the size of <code>file_name</code>.
 */

static VALUE
rb_file_s_size(klass, fname)
    VALUE klass, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0)
	rb_sys_fail(StringValueCStr(fname));
    return OFFT2NUM(st.st_size);
}

static VALUE
rb_file_ftype(st)
    struct stat *st;
{
    char *t;

    if (S_ISREG(st->st_mode)) {
	t = "file";
    }
    else if (S_ISDIR(st->st_mode)) {
	t = "directory";
    }
    else if (S_ISCHR(st->st_mode)) {
	t = "characterSpecial";
    }
#ifdef S_ISBLK
    else if (S_ISBLK(st->st_mode)) {
	t = "blockSpecial";
    }
#endif
#ifdef S_ISFIFO
    else if (S_ISFIFO(st->st_mode)) {
	t = "fifo";
    }
#endif
#ifdef S_ISLNK
    else if (S_ISLNK(st->st_mode)) {
	t = "link";
    }
#endif
#ifdef S_ISSOCK
    else if (S_ISSOCK(st->st_mode)) {
	t = "socket";
    }
#endif
    else {
	t = "unknown";
    }

    return rb_str_new2(t);
}

/*
 *  call-seq:
 *     File.ftype(file_name)   => string
 *  
 *  Identifies the type of the named file; the return string is one of
 *  ``<code>file</code>'', ``<code>directory</code>'',
 *  ``<code>characterSpecial</code>'', ``<code>blockSpecial</code>'',
 *  ``<code>fifo</code>'', ``<code>link</code>'',
 *  ``<code>socket</code>'', or ``<code>unknown</code>''.
 *     
 *     File.ftype("testfile")            #=> "file"
 *     File.ftype("/dev/tty")            #=> "characterSpecial"
 *     File.ftype("/tmp/.X11-unix/X0")   #=> "socket"
 */

static VALUE
rb_file_s_ftype(klass, fname)
    VALUE klass, fname;
{
    struct stat st;

    rb_secure(2);
    FilePathValue(fname);
    if (lstat(StringValueCStr(fname), &st) == -1) {
	rb_sys_fail(RSTRING(fname)->ptr);
    }

    return rb_file_ftype(&st);
}

/*
 *  call-seq:
 *     File.atime(file_name)  =>  time
 *  
 *  Returns the last access time for the named file as a Time object).
 *     
 *     File.atime("testfile")   #=> Wed Apr 09 08:51:48 CDT 2003
 *     
 */

static VALUE
rb_file_s_atime(klass, fname)
    VALUE klass, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0)
	rb_sys_fail(StringValueCStr(fname));
    return rb_time_new(st.st_atime, 0);
}

/*
 *  call-seq:
 *     file.atime    => time
 *  
 *  Returns the last access time (a <code>Time</code> object)
 *   for <i>file</i>, or epoch if <i>file</i> has not been accessed.
 *     
 *     File.new("testfile").atime   #=> Wed Dec 31 18:00:00 CST 1969
 *     
 */

static VALUE
rb_file_atime(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fptr->fd, &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return rb_time_new(st.st_atime, 0);
}

/*
 *  call-seq:
 *     File.mtime(file_name)  =>  time
 *  
 *  Returns the modification time for the named file as a Time object.
 *     
 *     File.mtime("testfile")   #=> Tue Apr 08 12:58:04 CDT 2003
 *     
 */

static VALUE
rb_file_s_mtime(klass, fname)
    VALUE klass, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0)
	rb_sys_fail(RSTRING(fname)->ptr);
    return rb_time_new(st.st_mtime, 0);
}

/*
 *  call-seq:
 *     file.mtime -> time
 *  
 *  Returns the modification time for <i>file</i>.
 *     
 *     File.new("testfile").mtime   #=> Wed Apr 09 08:53:14 CDT 2003
 *     
 */

static VALUE
rb_file_mtime(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fptr->fd, &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return rb_time_new(st.st_mtime, 0);
}

/*
 *  call-seq:
 *     File.ctime(file_name)  => time
 *  
 *  Returns the change time for the named file (the time at which
 *  directory information about the file was changed, not the file
 *  itself).
 *     
 *     File.ctime("testfile")   #=> Wed Apr 09 08:53:13 CDT 2003
 *     
 */

static VALUE
rb_file_s_ctime(klass, fname)
    VALUE klass, fname;
{
    struct stat st;

    if (rb_stat(fname, &st) < 0)
	rb_sys_fail(RSTRING(fname)->ptr);
    return rb_time_new(st.st_ctime, 0);
}

/*
 *  call-seq:
 *     file.ctime -> time
 *  
 *  Returns the change time for <i>file</i> (that is, the time directory
 *  information about the file was changed, not the file itself).
 *     
 *     File.new("testfile").ctime   #=> Wed Apr 09 08:53:14 CDT 2003
 *     
 */

static VALUE
rb_file_ctime(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fptr->fd, &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return rb_time_new(st.st_ctime, 0);
}

static void
chmod_internal(path, mode)
    const char *path;
    int mode;
{
    if (chmod(path, mode) < 0)
	rb_sys_fail(path);
}

/*
 *  call-seq:
 *     File.chmod(mode_int, file_name, ... ) -> integer
 *  
 *  Changes permission bits on the named file(s) to the bit pattern
 *  represented by <i>mode_int</i>. Actual effects are operating system
 *  dependent (see the beginning of this section). On Unix systems, see
 *  <code>chmod(2)</code> for details. Returns the number of files
 *  processed.
 *     
 *     File.chmod(0644, "testfile", "out")   #=> 2
 */

static VALUE
rb_file_s_chmod(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE vmode;
    VALUE rest;
    int mode;
    long n;

    rb_secure(2);
    rb_scan_args(argc, argv, "1*", &vmode, &rest);
    mode = NUM2INT(vmode);

    n = apply2files(chmod_internal, rest, (void *)(long)mode);
    return LONG2FIX(n);
}

/*
 *  call-seq:
 *     file.chmod(mode_int)   => 0
 *  
 *  Changes permission bits on <i>file</i> to the bit pattern
 *  represented by <i>mode_int</i>. Actual effects are platform
 *  dependent; on Unix systems, see <code>chmod(2)</code> for details.
 *  Follows symbolic links. Also see <code>File#lchmod</code>.
 *     
 *     f = File.new("out", "w");
 *     f.chmod(0644)   #=> 0
 */

static VALUE
rb_file_chmod(obj, vmode)
    VALUE obj, vmode;
{
    OpenFile *fptr;
    int mode;

    rb_secure(2);
    mode = NUM2INT(vmode);

    GetOpenFile(obj, fptr);
#ifdef HAVE_FCHMOD
    if (fchmod(fptr->fd, mode) == -1)
	rb_sys_fail(fptr->path);
#else
    if (!fptr->path) return Qnil;
    if (chmod(fptr->path, mode) == -1)
	rb_sys_fail(fptr->path);
#endif

    return INT2FIX(0);
}

#if defined(HAVE_LCHMOD)
static void
lchmod_internal(path, mode)
    const char *path;
    int mode;
{
    if (lchmod(path, mode) < 0)
	rb_sys_fail(path);
}

/*
 *  call-seq:
 *     File.lchmod(mode_int, file_name, ...)  => integer
 *  
 *  Equivalent to <code>File::chmod</code>, but does not follow symbolic
 *  links (so it will change the permissions associated with the link,
 *  not the file referenced by the link). Often not available.
 *     
 */

static VALUE
rb_file_s_lchmod(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE vmode;
    VALUE rest;
    long mode, n;

    rb_secure(2);
    rb_scan_args(argc, argv, "1*", &vmode, &rest);
    mode = NUM2INT(vmode);

    n = apply2files(lchmod_internal, rest, (void *)(long)mode);
    return LONG2FIX(n);
}
#else
static VALUE
rb_file_s_lchmod(argc, argv)
    int argc;
    VALUE *argv;
{
    rb_notimplement();
    return Qnil;		/* not reached */
}
#endif

struct chown_args {
    int owner, group;
};

static void
chown_internal(path, args)
    const char *path;
    struct chown_args *args;
{
    if (chown(path, args->owner, args->group) < 0)
	rb_sys_fail(path);
}

/*
 *  call-seq:
 *     File.chown(owner_int, group_int, file_name,... ) -> integer
 *  
 *  Changes the owner and group of the named file(s) to the given
 *  numeric owner and group id's. Only a process with superuser
 *  privileges may change the owner of a file. The current owner of a
 *  file may change the file's group to any group to which the owner
 *  belongs. A <code>nil</code> or -1 owner or group id is ignored.
 *  Returns the number of files processed.
 *     
 *     File.chown(nil, 100, "testfile")
 *     
 */

static VALUE
rb_file_s_chown(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE o, g, rest;
    struct chown_args arg;
    long n;

    rb_secure(2);
    rb_scan_args(argc, argv, "2*", &o, &g, &rest);
    if (NIL_P(o)) {
	arg.owner = -1;
    }
    else {
	arg.owner = NUM2INT(o);
    }
    if (NIL_P(g)) {
	arg.group = -1;
    }
    else {
	arg.group = NUM2INT(g);
    }

    n = apply2files(chown_internal, rest, &arg);
    return LONG2FIX(n);
}

/*
 *  call-seq:
 *     file.chown(owner_int, group_int )   => 0
 *  
 *  Changes the owner and group of <i>file</i> to the given numeric
 *  owner and group id's. Only a process with superuser privileges may
 *  change the owner of a file. The current owner of a file may change
 *  the file's group to any group to which the owner belongs. A
 *  <code>nil</code> or -1 owner or group id is ignored. Follows
 *  symbolic links. See also <code>File#lchown</code>.
 *     
 *     File.new("testfile").chown(502, 1000)
 *     
 */

static VALUE
rb_file_chown(obj, owner, group)
    VALUE obj, owner, group;
{
    OpenFile *fptr;
    int o, g;

    rb_secure(2);
    o = NUM2INT(owner);
    g = NUM2INT(group);
    GetOpenFile(obj, fptr);
#if defined(DJGPP) || defined(__CYGWIN32__) || defined(_WIN32) || defined(__EMX__)
    if (!fptr->path) return Qnil;
    if (chown(fptr->path, o, g) == -1)
	rb_sys_fail(fptr->path);
#else
    if (fchown(fptr->fd, o, g) == -1)
	rb_sys_fail(fptr->path);
#endif

    return INT2FIX(0);
}

#if defined(HAVE_LCHOWN) && !defined(__CHECKER__)
static void
lchown_internal(path, args)
    const char *path;
    struct chown_args *args;
{
    if (lchown(path, args->owner, args->group) < 0)
	rb_sys_fail(path);
}


/*
 *  call-seq:
 *     file.lchown(owner_int, group_int, file_name,..) => integer
 *  
 *  Equivalent to <code>File::chown</code>, but does not follow symbolic
 *  links (so it will change the owner associated with the link, not the
 *  file referenced by the link). Often not available. Returns number
 *  of files in the argument list.
 *     
 */

static VALUE
rb_file_s_lchown(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE o, g, rest;
    struct chown_args arg;
    long n;

    rb_secure(2);
    rb_scan_args(argc, argv, "2*", &o, &g, &rest);
    if (NIL_P(o)) {
	arg.owner = -1;
    }
    else {
	arg.owner = NUM2INT(o);
    }
    if (NIL_P(g)) {
	arg.group = -1;
    }
    else {
	arg.group = NUM2INT(g);
    }

    n = apply2files(lchown_internal, rest, &arg);
    return LONG2FIX(n);
}
#else
static VALUE
rb_file_s_lchown(argc, argv)
    int argc;
    VALUE *argv;
{
    rb_notimplement();
}
#endif

struct timeval rb_time_timeval();

#if defined(HAVE_UTIMES) && !defined(__CHECKER__)

static void
utime_internal(path, tvp)
    char *path;
    struct timeval tvp[];
{
    if (utimes(path, tvp) < 0)
	rb_sys_fail(path);
}

/*
 * call-seq:
 *  File.utime(atime, mtime, file_name,...)   =>  integer
 *
 * Sets the access and modification times of each
 * named file to the first two arguments. Returns
 * the number of file names in the argument list.
 */

static VALUE
rb_file_s_utime(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE atime, mtime, rest;
    struct timeval tvp[2];
    long n;

    rb_scan_args(argc, argv, "2*", &atime, &mtime, &rest);

    tvp[0] = rb_time_timeval(atime);
    tvp[1] = rb_time_timeval(mtime);

    n = apply2files(utime_internal, rest, tvp);
    return LONG2FIX(n);
}

#else

#if !defined HAVE_UTIME_H && !defined HAVE_SYS_UTIME_H
struct utimbuf {
    long actime;
    long modtime;
};
#endif

static void
utime_internal(path, utp)
    const char *path;
    struct utimbuf *utp;
{
    if (utime(path, utp) < 0)
	rb_sys_fail(path);
}

static VALUE
rb_file_s_utime(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE atime, mtime, rest;
    long n;
    struct timeval tv;
    struct utimbuf utbuf;

    rb_scan_args(argc, argv, "2*", &atime, &mtime, &rest);

    tv = rb_time_timeval(atime);
    utbuf.actime = tv.tv_sec;
    tv = rb_time_timeval(mtime);
    utbuf.modtime = tv.tv_sec;

    n = apply2files(utime_internal, rest, &utbuf);
    return LONG2FIX(n);
}

#endif

NORETURN(static void sys_fail2 _((VALUE,VALUE)));
static void
sys_fail2(s1, s2)
    VALUE s1, s2;
{
    char *buf;
    int len;

    len = RSTRING(s1)->len + RSTRING(s2)->len + 5;
    buf = ALLOCA_N(char, len);
    snprintf(buf, len, "%s or %s", RSTRING(s1)->ptr, RSTRING(s2)->ptr);
    rb_sys_fail(buf);
}

/*
 *  call-seq:
 *     File.link(old_name, new_name)    => 0
 *  
 *  Creates a new name for an existing file using a hard link. Will not
 *  overwrite <i>new_name</i> if it already exists (raising a subclass
 *  of <code>SystemCallError</code>). Not available on all platforms.
 *     
 *     File.link("testfile", ".testfile")   #=> 0
 *     IO.readlines(".testfile")[0]         #=> "This is line one\n"
 */

static VALUE
rb_file_s_link(klass, from, to)
    VALUE klass, from, to;
{
#ifdef HAVE_LINK
    rb_secure(2);
    FilePathValue(from);
    FilePathValue(to);

    if (link(StringValueCStr(from), StringValueCStr(to)) < 0) {
	sys_fail2(from, to);
    }
    return INT2FIX(0);
#else
    rb_notimplement();
    return Qnil;		/* not reached */
#endif
}

/*
 *  call-seq:
 *     File.symlink(old_name, new_name)   => 0
 *  
 *  Creates a symbolic link called <i>new_name</i> for the existing file
 *  <i>old_name</i>. Raises a <code>NotImplemented</code> exception on
 *  platforms that do not support symbolic links.
 *     
 *     File.symlink("testfile", "link2test")   #=> 0
 *     
 */

static VALUE
rb_file_s_symlink(klass, from, to)
    VALUE klass, from, to;
{
#ifdef HAVE_SYMLINK
    rb_secure(2);
    FilePathValue(from);
    FilePathValue(to);

    if (symlink(StringValueCStr(from), StringValueCStr(to)) < 0) {
	sys_fail2(from, to);
    }
    return INT2FIX(0);
#else
    rb_notimplement();
    return Qnil;		/* not reached */
#endif
}

/*
 *  call-seq:
 *     File.readlink(link_name) -> file_name
 *  
 *  Returns the name of the file referenced by the given link.
 *  Not available on all platforms.
 *     
 *     File.symlink("testfile", "link2test")   #=> 0
 *     File.readlink("link2test")              #=> "testfile"
 */

static VALUE
rb_file_s_readlink(klass, path)
    VALUE klass, path;
{
#ifdef HAVE_READLINK
    char *buf;
    int size = 100;
    int rv;
    VALUE v;

    rb_secure(2);
    FilePathValue(path);
    buf = xmalloc(size);
    while ((rv = readlink(StringValueCStr(path), buf, size)) == size) {
	size *= 2;
	buf = xrealloc(buf, size);
    }
    if (rv < 0) {
	free(buf);
	rb_sys_fail(RSTRING(path)->ptr);
    }
    v = rb_tainted_str_new(buf, rv);
    free(buf);

    return v;
#else
    rb_notimplement();
    return Qnil;		/* not reached */
#endif
}

static void
unlink_internal(path)
    const char *path;
{
    if (unlink(path) < 0)
	rb_sys_fail(path);
}

/*
 *  call-seq:
 *     File.delete(file_name, ...)  => integer
 *     File.unlink(file_name, ...)  => integer
 *  
 *  Deletes the named files, returning the number of names
 *  passed as arguments. Raises an exception on any error.
 *  See also <code>Dir::rmdir</code>.
 */

static VALUE
rb_file_s_unlink(klass, args)
    VALUE klass, args;
{
    long n;

    rb_secure(2);
    n = apply2files(unlink_internal, args, 0);
    return LONG2FIX(n);
}

/*
 *  call-seq:
 *     File.rename(old_name, new_name)   => 0
 *  
 *  Renames the given file to the new name. Raises a
 *  <code>SystemCallError</code> if the file cannot be renamed.
 *     
 *     File.rename("afile", "afile.bak")   #=> 0
 */

static VALUE
rb_file_s_rename(klass, from, to)
    VALUE klass, from, to;
{
    const char *src, *dst;

    rb_secure(2);
    FilePathValue(from);
    FilePathValue(to);
    src = StringValueCStr(from);
    dst = StringValueCStr(to);
    if (rename(src, dst) < 0) {
#if defined __CYGWIN__
	extern unsigned long __attribute__((stdcall)) GetLastError();
	errno = GetLastError(); /* This is a Cygwin bug */
#elif defined DOSISH && !defined _WIN32
	if (errno == EEXIST
#if defined (__EMX__)
	    || errno == EACCES
#endif
	    ) {
	    if (chmod(dst, 0666) == 0 &&
		unlink(dst) == 0 &&
		rename(src, dst) == 0)
		return INT2FIX(0);
	}
#endif
	sys_fail2(from, to);
    }

    return INT2FIX(0);
}

/*
 *  call-seq:
 *     File.umask()          => integer
 *     File.umask(integer)   => integer
 *  
 *  Returns the current umask value for this process. If the optional
 *  argument is given, set the umask to that value and return the
 *  previous value. Umask values are <em>subtracted</em> from the
 *  default permissions, so a umask of <code>0222</code> would make a
 *  file read-only for everyone.
 *     
 *     File.umask(0006)   #=> 18
 *     File.umask         #=> 6
 */

static VALUE
rb_file_s_umask(argc, argv)
    int argc;
    VALUE *argv;
{
    int omask = 0;

    rb_secure(2);
    if (argc == 0) {
	omask = umask(0);
	umask(omask);
    }
    else if (argc == 1) {
	omask = umask(NUM2INT(argv[0]));
    }
    else {
	rb_raise(rb_eArgError, "wrong number of arguments");
    }
    return INT2FIX(omask);
}

#if defined DOSISH
#define DOSISH_UNC
#define isdirsep(x) ((x) == '/' || (x) == '\\')
#else
#define isdirsep(x) ((x) == '/')
#endif
#ifndef CharNext		/* defined as CharNext[AW] on Windows. */
# if defined(DJGPP)
#   define CharNext(p) ((p) + mblen(p, RUBY_MBCHAR_MAXSIZE))
# else
#   define CharNext(p) ((p) + 1)
# endif
#endif

#ifdef __CYGWIN__
#undef DOSISH
#define DOSISH_UNC
#define DOSISH_DRIVE_LETTER
#endif

#ifdef DOSISH_DRIVE_LETTER
static inline int
has_drive_letter(buf)
    const char *buf;
{
    if (ISALPHA(buf[0]) && buf[1] == ':') {
	return 1;
    }
    else {
	return 0;
    }
}

static char*
getcwdofdrv(drv)
    int drv;
{
    char drive[4];
    char *drvcwd, *oldcwd;

    drive[0] = drv;
    drive[1] = ':';
    drive[2] = '\0';

    /* the only way that I know to get the current directory
       of a particular drive is to change chdir() to that drive,
       so save the old cwd before chdir()
    */
    oldcwd = my_getcwd();
    if (chdir(drive) == 0) {
	drvcwd = my_getcwd();
	chdir(oldcwd);
	free(oldcwd);
    }
    else {
	/* perhaps the drive is not exist. we return only drive letter */
	drvcwd = strdup(drive);
    }
    return drvcwd;
}
#endif

static inline char *
skiproot(path)
    const char *path;
{
#ifdef DOSISH_DRIVE_LETTER
    if (has_drive_letter(path)) path += 2;
#endif
    while (isdirsep(*path)) path++;
    return (char *)path;
}

#define nextdirsep rb_path_next
char *
rb_path_next(s)
    const char *s;
{
    while (*s && !isdirsep(*s)) {
	s = CharNext(s);
    }
    return (char *)s;
}

#define skipprefix rb_path_skip_prefix
char *
rb_path_skip_prefix(path)
    const char *path;
{
#if defined(DOSISH_UNC) || defined(DOSISH_DRIVE_LETTER) 
#ifdef DOSISH_UNC
    if (isdirsep(path[0]) && isdirsep(path[1])) {
	if (*(path = nextdirsep(path + 2)))
	    path = nextdirsep(path + 1);
	return (char *)path;
    }
#endif
#ifdef DOSISH_DRIVE_LETTER
    if (has_drive_letter(path))
	return (char *)(path + 2);
#endif
#endif
    return (char *)path;
}

#define strrdirsep rb_path_last_separator
char *
rb_path_last_separator(path)
    const char *path;
{
    char *last = NULL;
    while (*path) {
	if (isdirsep(*path)) {
	    const char *tmp = path++;
	    while (isdirsep(*path)) path++;
	    if (!*path) break;
	    last = (char *)tmp;
	}
	else {
	    path = CharNext(path);
	}
    }
    return last;
}

#define chompdirsep rb_path_end
char *
rb_path_end(path)
    const char *path;
{
    while (*path) {
	if (isdirsep(*path)) {
	    const char *last = path++;
	    while (isdirsep(*path)) path++;
	    if (!*path) return (char *)last;
	}
	else {
	    path = CharNext(path);
	}
    }
    return (char *)path;
}

#define BUFCHECK(cond) do {\
    long bdiff = p - buf;\
    while (cond) {\
	buflen *= 2;\
    }\
    rb_str_resize(result, buflen);\
    buf = RSTRING(result)->ptr;\
    p = buf + bdiff;\
    pend = buf + buflen;\
} while (0)

#define BUFINIT() (\
    p = buf = RSTRING(result)->ptr,\
    buflen = RSTRING(result)->len,\
    pend = p + buflen)

#if !defined(TOLOWER)
#define TOLOWER(c) (ISUPPER(c) ? tolower(c) : (c))
#endif

static int is_absolute_path _((const char*));

static VALUE
file_expand_path(fname, dname, result)
    VALUE fname, dname, result;
{
    char *s, *buf, *b, *p, *pend, *root;
    long buflen, dirlen;
    int tainted;

    s = StringValuePtr(fname);
    BUFINIT();
    tainted = OBJ_TAINTED(fname);

    if (s[0] == '~') {
	if (isdirsep(s[1]) || s[1] == '\0') {
	    char *dir = getenv("HOME");

	    if (!dir) {
		rb_raise(rb_eArgError, "couldn't find HOME environment -- expanding `%s'", s);
	    }
	    dirlen = strlen(dir);
	    BUFCHECK(dirlen > buflen);
	    strcpy(buf, dir);
#if defined DOSISH || defined __CYGWIN__
	    for (p = buf; *p; p = CharNext(p)) {
		if (*p == '\\') {
		    *p = '/';
		}
	    }
#else
	    p = buf + strlen(dir);
#endif
	    s++;
	    tainted = 1;
	}
	else {
#ifdef HAVE_PWD_H
	    struct passwd *pwPtr;
	    s++;
#endif
	    s = nextdirsep(b = s);
	    BUFCHECK(bdiff + (s-b) >= buflen);
	    memcpy(p, b, s-b);
	    p += s-b;
	    *p = '\0';
#ifdef HAVE_PWD_H
	    pwPtr = getpwnam(buf);
	    if (!pwPtr) {
		endpwent();
		rb_raise(rb_eArgError, "user %s doesn't exist", buf);
	    }
	    dirlen = strlen(pwPtr->pw_dir);
	    BUFCHECK(dirlen > buflen);
	    strcpy(buf, pwPtr->pw_dir);
	    p = buf + strlen(pwPtr->pw_dir);
	    endpwent();
#endif
	}
    }
#ifdef DOSISH_DRIVE_LETTER
    /* skip drive letter */
    else if (has_drive_letter(s)) {
	if (isdirsep(s[2])) {
	    /* specified drive letter, and full path */
	    /* skip drive letter */
	    BUFCHECK(bdiff + 2 >= buflen);
	    memcpy(p, s, 2);
	    p += 2;
	    s += 2;
	}
	else {
	    /* specified drive, but not full path */
	    int same = 0;
	    if (!NIL_P(dname)) {
		file_expand_path(dname, Qnil, result);
		BUFINIT();
		if (has_drive_letter(p) && TOLOWER(p[0]) == TOLOWER(s[0])) {
		    /* ok, same drive */
		    same = 1;
		}
	    }
	    if (!same) {
		char *dir = getcwdofdrv(*s);

		tainted = 1;
		dirlen = strlen(dir);
		BUFCHECK(dirlen > buflen);
		strcpy(buf, dir);
		free(dir);
	    }
	    p = chompdirsep(skiproot(buf));
	    s += 2;
	}
    }
#endif
    else if (!is_absolute_path(s)) {
	if (!NIL_P(dname)) {
	    file_expand_path(dname, Qnil, result);
	    BUFINIT();
	}
	else {
	    char *dir = my_getcwd();

	    tainted = 1;
	    dirlen = strlen(dir);
	    BUFCHECK(dirlen > buflen);
	    strcpy(buf, dir);
	    free(dir);
	}
#if defined DOSISH || defined __CYGWIN__
	if (isdirsep(*s)) {
	    /* specified full path, but not drive letter nor UNC */
	    /* we need to get the drive letter or UNC share name */
	    p = skipprefix(buf);
	}
	else
#endif
	    p = chompdirsep(skiproot(buf));
    }
    else {
	b = s;
	do s++; while (isdirsep(*s));
	p = buf + (s - b);
	BUFCHECK(bdiff >= buflen);
	memset(buf, '/', p - buf);
    }
    if (p > buf && p[-1] == '/')
	--p;
    else
	*p = '/';

    p[1] = 0;
    root = skipprefix(buf);

    b = s;
    while (*s) {
	switch (*s) {
	  case '.':
	    if (b == s++) {	/* beginning of path element */
		switch (*s) {
		  case '\0':
		    b = s;
		    break;
		  case '.':
		    if (*(s+1) == '\0' || isdirsep(*(s+1))) {
			/* We must go back to the parent */
			*p = '\0';
			if (!(b = strrdirsep(root))) {
			    *p = '/';
			}
			else {
			    p = b;
			}
			b = ++s;
		    }
		    break;
		  case '/':
#if defined DOSISH || defined __CYGWIN__
		  case '\\':
#endif
		    b = ++s;
		    break;
		  default:
		    /* ordinary path element, beginning don't move */
		    break;
		}
	    }
	    break;
	  case '/':
#if defined DOSISH || defined __CYGWIN__
	  case '\\':
#endif
	    if (s > b) {
		long rootdiff = root - buf;
		BUFCHECK(bdiff + (s-b+1) >= buflen);
		root = buf + rootdiff;
		memcpy(++p, b, s-b);
		p += s-b;
		*p = '/';
	    }
	    b = ++s;
	    break;
	  default:
	    s = CharNext(s);
	    break;
	}
    }

    if (s > b) {
	BUFCHECK(bdiff + (s-b) >= buflen);
	memcpy(++p, b, s-b);
	p += s-b;
    }
    if (p == skiproot(buf) - 1) p++;

    if (tainted) OBJ_TAINT(result);
    RSTRING(result)->len = p - buf;
    *p = '\0';
    return result;
}

VALUE
rb_file_expand_path(fname, dname)
    VALUE fname, dname;
{
    return file_expand_path(fname, dname, rb_str_new(0, MAXPATHLEN + 2));
}

/*
 *  call-seq:
 *     File.expand_path(file_name [, dir_string] ) -> abs_file_name
 *  
 *  Converts a pathname to an absolute pathname. Relative paths are
 *  referenced from the current working directory of the process unless
 *  <i>dir_string</i> is given, in which case it will be used as the
 *  starting point. The given pathname may start with a
 *  ``<code>~</code>'', which expands to the process owner's home
 *  directory (the environment variable <code>HOME</code> must be set
 *  correctly). ``<code>~</code><i>user</i>'' expands to the named
 *  user's home directory.
 *     
 *     File.expand_path("~oracle/bin")           #=> "/home/oracle/bin"
 *     File.expand_path("../../bin", "/tmp/x")   #=> "/bin"
 */

VALUE
rb_file_s_expand_path(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE fname, dname;

    if (argc == 1) {
	return rb_file_expand_path(argv[0], Qnil);
    }
    rb_scan_args(argc, argv, "11", &fname, &dname);

    return rb_file_expand_path(fname, dname);
}

static int
rmext(p, e)
    const char *p, *e;
{
    int l1, l2;

    if (!e) return 0;

    l1 = chompdirsep(p) - p;
    l2 = strlen(e);
    if (l2 == 2 && e[1] == '*') {
	e = strrchr(p, *e);
	if (!e) return 0;
	return e - p;
    }
    if (l1 < l2) return l1;

    if (strncmp(p+l1-l2, e, l2) == 0) {
	return l1-l2;
    }
    return 0;
}

/*
 *  call-seq:
 *     File.basename(file_name [, suffix] ) -> base_name
 *  
 *  Returns the last component of the filename given in <i>file_name</i>,
 *  which must be formed using forward slashes (``<code>/</code>'')
 *  regardless of the separator used on the local file system. If
 *  <i>suffix</i> is given and present at the end of <i>file_name</i>,
 *  it is removed.
 *     
 *     File.basename("/home/gumby/work/ruby.rb")          #=> "ruby.rb"
 *     File.basename("/home/gumby/work/ruby.rb", ".rb")   #=> "ruby"
 */

static VALUE
rb_file_s_basename(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE fname, fext, basename;
    char *name, *p;
    int f;

    if (rb_scan_args(argc, argv, "11", &fname, &fext) == 2) {
	StringValue(fext);
    }
    StringValue(fname);
    if (RSTRING(fname)->len == 0 || !*(name = RSTRING(fname)->ptr))
	return fname;
    if (!*(name = skiproot(name))) {
	p = name - 1;
	f = 1;
#ifdef DOSISH_DRIVE_LETTER
	if (*p == ':') {
	    p++;
	    f = 0;
	}
#endif
    }
    else if (!(p = strrdirsep(name))) {
	if (NIL_P(fext) || !(f = rmext(name, StringValueCStr(fext)))) {
	    f = chompdirsep(name) - name;
	    if (f == RSTRING(fname)->len) return fname;
	}
	p = name;
    }
    else {
	while (isdirsep(*p)) p++; /* skip last / */
	if (NIL_P(fext) || !(f = rmext(p, StringValueCStr(fext)))) {
	    f = chompdirsep(p) - p;
	}
    }
    basename = rb_str_new(p, f);
    OBJ_INFECT(basename, fname);
    return basename;
}

/*
 *  call-seq:
 *     File.dirname(file_name ) -> dir_name
 *  
 *  Returns all components of the filename given in <i>file_name</i>
 *  except the last one. The filename must be formed using forward
 *  slashes (``<code>/</code>'') regardless of the separator used on the
 *  local file system.
 *     
 *     File.dirname("/home/gumby/work/ruby.rb")   #=> "/home/gumby/work"
 */

static VALUE
rb_file_s_dirname(klass, fname)
    VALUE klass, fname;
{
    char *name, *root, *p;
    VALUE dirname;

    name = StringValueCStr(fname);
    root = skiproot(name);
#ifdef DOSISH_UNC
    if (root > name + 2 && isdirsep(*name))
	name = root - 2;
#else
    if (root > name + 1)
	name = root - 1;
#endif
    p = strrdirsep(root);
    if (!p) {
	p = root;
    }
    if (p == name)
	return rb_str_new2(".");
    dirname = rb_str_new(name, p - name);
#ifdef DOSISH_DRIVE_LETTER
    if (root == name + 2 && name[1] == ':')
	rb_str_cat(dirname, ".", 1);
#endif
    OBJ_INFECT(dirname, fname);
    return dirname;
}

/*
 *  call-seq:
 *     File.extname(path) -> string
 *  
 *  Returns the extension (the portion of file name in <i>path</i>
 *  after the period).
 *     
 *     File.extname("test.rb")         #=> ".rb"
 *     File.extname("a/b/d/test.rb")   #=> ".rb"
 *     File.extname("test")            #=> ""
 *     File.extname(".profile")        #=> ""
 *     
 */

static VALUE
rb_file_s_extname(klass, fname)
    VALUE klass, fname;
{
    char *name, *p, *e;
    VALUE extname;

    name = StringValueCStr(fname);
    p = strrdirsep(name);	/* get the last path component */
    if (!p)
 	p = name;
    else
 	p++;
 
     e = strrchr(p, '.');	/* get the last dot of the last component */
     if (!e || e == p)		/* no dot, or the only dot is first? */
	 return rb_str_new2("");
     extname = rb_str_new(e, chompdirsep(e) - e);	/* keep the dot, too! */
     OBJ_INFECT(extname, fname);
     return extname;
}

/*
 *  call-seq:
 *     File.path(path) -> string
 *  
 *  Returns the string representation of the path
 * 
 *     File.path("/dev/null")          #=> "/dev/null"
 *     File.path(Pathname.new("/tmp")) #=> "/tmp"
 *     
 */

static VALUE
rb_file_s_path(klass, fname)
    VALUE klass, fname;
{
    return rb_get_path(fname);
}

/*
 *  call-seq:
 *     File.split(file_name)   => array
 *  
 *  Splits the given string into a directory and a file component and
 *  returns them in a two-element array. See also
 *  <code>File::dirname</code> and <code>File::basename</code>.
 *     
 *     File.split("/home/gumby/.profile")   #=> ["/home/gumby", ".profile"]
 */

static VALUE
rb_file_s_split(klass, path)
    VALUE klass, path;
{
    StringValue(path);		/* get rid of converting twice */
    return rb_assoc_new(rb_file_s_dirname(Qnil, path), rb_file_s_basename(1,&path));
}

static VALUE separator;

static VALUE rb_file_join _((VALUE ary, VALUE sep));

static VALUE
file_inspect_join(ary, arg, recur)
    VALUE ary;
    VALUE *arg;
{
    if (recur) return rb_str_new2("[...]");
    return rb_file_join(arg[0], arg[1]);
}

static VALUE
rb_file_join(ary, sep)
    VALUE ary, sep;
{
    long len, i;
    int taint = 0;
    VALUE result, tmp;
    char *name;

    if (RARRAY(ary)->len == 0) return rb_str_new(0, 0);
    if (OBJ_TAINTED(ary)) taint = 1;
    if (OBJ_TAINTED(sep)) taint = 1;

    len = 1;
    for (i=0; i<RARRAY(ary)->len; i++) {
	if (TYPE(RARRAY(ary)->ptr[i]) == T_STRING) {
	    len += RSTRING(RARRAY(ary)->ptr[i])->len;
	}
	else {
	    len += 10;
	}
    }
    if (!NIL_P(sep) && TYPE(sep) == T_STRING) {
	len += RSTRING(sep)->len * RARRAY(ary)->len - 1;
    }
    result = rb_str_buf_new(len);
    for (i=0; i<RARRAY(ary)->len; i++) {
	tmp = RARRAY(ary)->ptr[i];
	switch (TYPE(tmp)) {
	  case T_STRING:
	    break;
	  case T_ARRAY:
	    {
		VALUE args[2];

		args[0] = tmp;
		args[1] = sep;
		tmp = rb_exec_recursive(file_inspect_join, ary, (VALUE)args);
	    }
	    break;
	  default:
	    tmp = rb_obj_as_string(tmp);
	}
	name = StringValueCStr(result);
	if (i > 0 && !NIL_P(sep) && !*chompdirsep(name))
	    rb_str_buf_append(result, sep);
	rb_str_buf_append(result, tmp);
	if (OBJ_TAINTED(tmp)) taint = 1;
    }

    if (taint) OBJ_TAINT(result);
    return result;
}

/*
 *  call-seq:
 *     File.join(string, ...) -> path
 *  
 *  Returns a new string formed by joining the strings using
 *  <code>File::SEPARATOR</code>.
 *     
 *     File.join("usr", "mail", "gumby")   #=> "usr/mail/gumby"
 *     
 */

static VALUE
rb_file_s_join(klass, args)
    VALUE klass, args;
{
    return rb_file_join(args, separator);
}

/*
 *  call-seq:
 *     File.truncate(file_name, integer)  => 0
 *  
 *  Truncates the file <i>file_name</i> to be at most <i>integer</i>
 *  bytes long. Not available on all platforms.
 *     
 *     f = File.new("out", "w")
 *     f.write("1234567890")     #=> 10
 *     f.close                   #=> nil
 *     File.truncate("out", 5)   #=> 0
 *     File.size("out")          #=> 5
 *     
 */

static VALUE
rb_file_s_truncate(klass, path, len)
    VALUE klass, path, len;
{
    off_t pos;

    rb_secure(2);
    pos = NUM2OFFT(len);
    FilePathValue(path);
#ifdef HAVE_TRUNCATE
    if (truncate(StringValueCStr(path), pos) < 0)
	rb_sys_fail(RSTRING(path)->ptr);
#else
# ifdef HAVE_CHSIZE
    {
	int tmpfd;

#  ifdef _WIN32
	if ((tmpfd = open(StringValueCStr(path), O_RDWR)) < 0) {
	    rb_sys_fail(RSTRING(path)->ptr);
	}
#  else
	if ((tmpfd = open(StringValueCStr(path), 0)) < 0) {
	    rb_sys_fail(RSTRING(path)->ptr);
	}
#  endif
	if (chsize(tmpfd, pos) < 0) {
	    close(tmpfd);
	    rb_sys_fail(RSTRING(path)->ptr);
	}
	close(tmpfd);
    }
# else
    rb_notimplement();
# endif
#endif
    return INT2FIX(0);
}

/*
 *  call-seq:
 *     file.truncate(integer)    => 0
 *  
 *  Truncates <i>file</i> to at most <i>integer</i> bytes. The file
 *  must be opened for writing. Not available on all platforms.
 *     
 *     f = File.new("out", "w")
 *     f.syswrite("1234567890")   #=> 10
 *     f.truncate(5)              #=> 0
 *     f.close()                  #=> nil
 *     File.size("out")           #=> 5
 */

static VALUE
rb_file_truncate(obj, len)
    VALUE obj, len;
{
    OpenFile *fptr;
    off_t pos;

    rb_secure(2);
    pos = NUM2OFFT(len);
    GetOpenFile(obj, fptr);
    if (!(fptr->mode & FMODE_WRITABLE)) {
	rb_raise(rb_eIOError, "not opened for writing");
    }
    rb_io_flush(obj);
#ifdef HAVE_TRUNCATE
    if (ftruncate(fptr->fd, pos) < 0)
	rb_sys_fail(fptr->path);
#else
# ifdef HAVE_CHSIZE
    if (chsize(fptr->fd, pos) < 0)
	rb_sys_fail(fptr->path);
# else
    rb_notimplement();
# endif
#endif
    return INT2FIX(0);
}

# ifndef LOCK_SH
#  define LOCK_SH 1
# endif
# ifndef LOCK_EX
#  define LOCK_EX 2
# endif
# ifndef LOCK_NB
#  define LOCK_NB 4
# endif
# ifndef LOCK_UN
#  define LOCK_UN 8
# endif

#if 1
static int
rb_thread_flock(fd, op, fptr)
    int fd, op;
    OpenFile *fptr;
{
    if (rb_thread_alone() || (op & LOCK_NB)) {
	return flock(fd, op);
    }
    op |= LOCK_NB;
    while (flock(fd, op) < 0) {
	switch (errno) {
          case EAGAIN:
          case EACCES:
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
	  case EWOULDBLOCK:
#endif
	    rb_thread_polling();	/* busy wait */
	    rb_io_check_closed(fptr);
            continue;
	  default:
	    return -1;
	}
    }
    return 0;
}
#define flock(fd, op) rb_thread_flock(fd, op, fptr)
#endif

/*
 *  call-seq:
 *     file.flock (locking_constant ) =>  0 or false
 *  
 *  Locks or unlocks a file according to <i>locking_constant</i> (a
 *  logical <em>or</em> of the values in the table below).
 *  Returns <code>false</code> if <code>File::LOCK_NB</code> is
 *  specified and the operation would otherwise have blocked. Not
 *  available on all platforms.
 *     
 *  Locking constants (in class File):
 *
 *     LOCK_EX   | Exclusive lock. Only one process may hold an
 *               | exclusive lock for a given file at a time.
 *     ----------+------------------------------------------------
 *     LOCK_NB   | Don't block when locking. May be combined
 *               | with other lock options using logical or.
 *     ----------+------------------------------------------------
 *     LOCK_SH   | Shared lock. Multiple processes may each hold a
 *               | shared lock for a given file at the same time.
 *     ----------+------------------------------------------------
 *     LOCK_UN   | Unlock.
 *
 *  Example:
 *
 *     File.new("testfile").flock(File::LOCK_UN)   #=> 0
 *     
 */

static VALUE
rb_file_flock(obj, operation)
    VALUE obj;
    VALUE operation;
{
#ifndef __CHECKER__
    OpenFile *fptr;
    int op;

    rb_secure(2);
    op = NUM2INT(operation);
    GetOpenFile(obj, fptr);

    if (fptr->mode & FMODE_WRITABLE) {
        rb_io_flush(obj);
    }
  retry:
    if (flock(fptr->fd, op) < 0) {
        switch (errno) {
          case EAGAIN:
          case EACCES:
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
          case EWOULDBLOCK:
#endif
              return Qfalse;
	  case EINTR:
#if defined(ERESTART)
	  case ERESTART:
#endif
	    goto retry;
        }
	rb_sys_fail(fptr->path);
    }
#endif
    return INT2FIX(0);
}
#undef flock

static void
test_check(n, argc, argv)
    int n, argc;
    VALUE *argv;
{
    int i;

    rb_secure(2);
    n+=1;
    if (n != argc) rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)", argc, n);
    for (i=1; i<n; i++) {
	switch (TYPE(argv[i])) {
	  case T_STRING:
	  default:
	    FilePathValue(argv[i]);
    	    break;
	  case T_FILE:
	    break;
	}
    }
}

#define CHECK(n) test_check((n), argc, argv)

/*
 *  call-seq:
 *     test(int_cmd, file1 [, file2] ) => obj
 *  
 *  Uses the integer <i>aCmd</i> to perform various tests on
 *  <i>file1</i> (first table below) or on <i>file1</i> and
 *  <i>file2</i> (second table).
 *     
 *  File tests on a single file:
 *
 *    Test   Returns   Meaning
 *     ?A  | Time    | Last access time for file1
 *     ?b  | boolean | True if file1 is a block device
 *     ?c  | boolean | True if file1 is a character device
 *     ?C  | Time    | Last change time for file1
 *     ?d  | boolean | True if file1 exists and is a directory
 *     ?e  | boolean | True if file1 exists
 *     ?f  | boolean | True if file1 exists and is a regular file
 *     ?g  | boolean | True if files has the \CF{setgid} bit
 *         |         | set (false under NT)
 *     ?G  | boolean | True if file1 exists and has a group
 *         |         | ownership equal to the caller's group
 *     ?k  | boolean | True if file1 exists and has the sticky bit set
 *     ?l  | boolean | True if files exists and is a symbolic link
 *     ?M  | Time    | Last modification time for file1
 *     ?o  | boolean | True if files exists and is owned by 
 *         |         | the caller's effective uid
 *     ?O  | boolean | True if file1 exists and is owned by
 *         |         | the caller's real uid
 *     ?p  | boolean | True if file1 exists and is a fifo
 *     ?r  | boolean | True if file1 is readable by the effective
 *         |         | uid/gid of the caller
 *     ?R  | boolean | True if file is readable by the real
 *         |         | uid/gid of the caller
 *     ?s  | int/nil | If files has nonzero size, return the size,
 *         |         | otherwise return nil
 *     ?S  | boolean | True if file1 exists and is a socket
 *     ?u  | boolean | True if file1 has the setuid bit set
 *     ?w  | boolean | True if file1 exists and is writable by
 *         |         | the effective uid/gid
 *     ?W  | boolean | True if file1 exists and is writable by
 *         |         | the real uid/gid
 *     ?x  | boolean | True if file1 exists and is executable by
 *         |         | the effective uid/gid
 *     ?X  | boolean | True if file1 exists and is executable by
 *         |         | the real uid/gid
 *     ?z  | boolean | True if file1 exists and has a zero length
 *
 * Tests that take two files:
 *
 *     ?-  | boolean | True if file1 is a hard link to file2
 *     ?=  | boolean | True if the modification times of file1
 *         |         | and file2 are equal
 *     ?<  | boolean | True if the modification time of file1
 *         |         | is prior to that of file2
 *     ?>  | boolean | True if the modification time of file1
 *         |         | is after that of file2
 */

static VALUE
rb_f_test(argc, argv)
    int argc;
    VALUE *argv;
{
    int cmd;

    if (argc == 0) rb_raise(rb_eArgError, "wrong number of arguments");
#if 0 /* 1.7 behavior? */
    if (argc == 1) {
	return RTEST(argv[0]) ? Qtrue : Qfalse;
    }
#endif
    cmd = NUM2CHR(argv[0]);
    if (cmd == 0) return Qfalse;
    if (strchr("bcdefgGkloOprRsSuwWxXz", cmd)) {
	CHECK(1);
	switch (cmd) {
	  case 'b':
	    return test_b(0, argv[1]);

	  case 'c':
	    return test_c(0, argv[1]);

	  case 'd':
	    return test_d(0, argv[1]);

	  case 'a':
	  case 'e':
	    return test_e(0, argv[1]);

	  case 'f':
	    return test_f(0, argv[1]);

	  case 'g':
	    return test_sgid(0, argv[1]);

	  case 'G':
	    return test_grpowned(0, argv[1]);

	  case 'k':
	    return test_sticky(0, argv[1]);

	  case 'l':
	    return test_l(0, argv[1]);

	  case 'o':
	    return test_owned(0, argv[1]);

	  case 'O':
	    return test_rowned(0, argv[1]);

	  case 'p':
	    return test_p(0, argv[1]);

	  case 'r':
	    return test_r(0, argv[1]);

	  case 'R':
	    return test_R(0, argv[1]);

	  case 's':
	    return test_s(0, argv[1]);

	  case 'S':
	    return test_S(0, argv[1]);

	  case 'u':
	    return test_suid(0, argv[1]);

	  case 'w':
	    return test_w(0, argv[1]);

	  case 'W':
	    return test_W(0, argv[1]);

	  case 'x':
	    return test_x(0, argv[1]);

	  case 'X':
	    return test_X(0, argv[1]);

	  case 'z':
	    return test_z(0, argv[1]);
	}
    }

    if (strchr("MAC", cmd)) {
	struct stat st;

	CHECK(1);
	if (rb_stat(argv[1], &st) == -1) {
	    rb_sys_fail(RSTRING(argv[1])->ptr);
	}

	switch (cmd) {
	  case 'A':
	    return rb_time_new(st.st_atime, 0);
	  case 'M':
	    return rb_time_new(st.st_mtime, 0);
	  case 'C':
	    return rb_time_new(st.st_ctime, 0);
	}
    }

    if (strchr("-=<>", cmd)) {
	struct stat st1, st2;

	CHECK(2);
	if (rb_stat(argv[1], &st1) < 0) return Qfalse;
	if (rb_stat(argv[2], &st2) < 0) return Qfalse;

	switch (cmd) {
	  case '-':
	    if (st1.st_dev == st2.st_dev && st1.st_ino == st2.st_ino)
		return Qtrue;
            return Qfalse;

	  case '=':
	    if (st1.st_mtime == st2.st_mtime) return Qtrue;
	    return Qfalse;

	  case '>':
	    if (st1.st_mtime > st2.st_mtime) return Qtrue;
	    return Qfalse;

	  case '<':
	    if (st1.st_mtime < st2.st_mtime) return Qtrue;
	    return Qfalse;
        }
    }
    /* unknown command */
    rb_raise(rb_eArgError, "unknown command ?%c", cmd);
    return Qnil;		/* not reached */
}



/*
 *  Document-class: File::Stat
 *
 *  Objects of class <code>File::Stat</code> encapsulate common status
 *  information for <code>File</code> objects. The information is
 *  recorded at the moment the <code>File::Stat</code> object is
 *  created; changes made to the file after that point will not be
 *  reflected. <code>File::Stat</code> objects are returned by
 *  <code>IO#stat</code>, <code>File::stat</code>,
 *  <code>File#lstat</code>, and <code>File::lstat</code>. Many of these
 *  methods return platform-specific values, and not all values are
 *  meaningful on all systems. See also <code>Kernel#test</code>.
 */

static VALUE rb_stat_s_alloc _((VALUE));
static VALUE
rb_stat_s_alloc(klass)
    VALUE klass;
{
    return stat_new_0(klass, 0);
}

/*
 * call-seq:
 *
 *   File::Stat.new(file_name)  => stat
 *
 * Create a File::Stat object for the given file name (raising an
 * exception if the file doesn't exist).
 */

static VALUE
rb_stat_init(obj, fname)
    VALUE obj, fname;
{
    struct stat st, *nst;

    rb_secure(2);
    FilePathValue(fname);
    if (stat(StringValueCStr(fname), &st) == -1) {
	rb_sys_fail(RSTRING(fname)->ptr);
    }
    if (DATA_PTR(obj)) {
	free(DATA_PTR(obj));
	DATA_PTR(obj) = NULL;
    }
    nst = ALLOC(struct stat);
    *nst = st;
    DATA_PTR(obj) = nst;

    return Qnil;
}

/* :nodoc: */
static VALUE
rb_stat_init_copy(copy, orig)
    VALUE copy, orig;
{
    struct stat *nst;

    if (copy == orig) return orig;
    rb_check_frozen(copy);
    /* need better argument type check */
    if (!rb_obj_is_instance_of(orig, rb_obj_class(copy))) {
	rb_raise(rb_eTypeError, "wrong argument class");
    }
    if (DATA_PTR(copy)) {
	free(DATA_PTR(copy));
	DATA_PTR(copy) = 0;
    }
    if (DATA_PTR(orig)) {
	nst = ALLOC(struct stat);
	*nst = *(struct stat*)DATA_PTR(orig);
	DATA_PTR(copy) = nst;
    }

    return copy;
}

/*
 *  call-seq:
 *     stat.ftype   => string
 *  
 *  Identifies the type of <i>stat</i>. The return string is one of:
 *  ``<code>file</code>'', ``<code>directory</code>'',
 *  ``<code>characterSpecial</code>'', ``<code>blockSpecial</code>'',
 *  ``<code>fifo</code>'', ``<code>link</code>'',
 *  ``<code>socket</code>'', or ``<code>unknown</code>''.
 *     
 *     File.stat("/dev/tty").ftype   #=> "characterSpecial"
 *     
 */

static VALUE
rb_stat_ftype(obj)
    VALUE obj;
{
    return rb_file_ftype(get_stat(obj));
}

/*
 *  call-seq:
 *     stat.directory?   => true or false
 *  
 *  Returns <code>true</code> if <i>stat</i> is a directory,
 *  <code>false</code> otherwise.
 *     
 *     File.stat("testfile").directory?   #=> false
 *     File.stat(".").directory?          #=> true
 */

static VALUE
rb_stat_d(obj)
    VALUE obj;
{
    if (S_ISDIR(get_stat(obj)->st_mode)) return Qtrue;
    return Qfalse;
}

/*
 *  call-seq:
 *     stat.pipe?    => true or false
 *  
 *  Returns <code>true</code> if the operating system supports pipes and
 *  <i>stat</i> is a pipe; <code>false</code> otherwise.
 */

static VALUE
rb_stat_p(obj)
    VALUE obj;
{
#ifdef S_IFIFO
    if (S_ISFIFO(get_stat(obj)->st_mode)) return Qtrue;

#endif
    return Qfalse;
}

/*
 *  call-seq:
 *     stat.symlink?    => true or false
 *  
 *  Returns <code>true</code> if <i>stat</i> is a symbolic link,
 *  <code>false</code> if it isn't or if the operating system doesn't
 *  support this feature. As <code>File::stat</code> automatically
 *  follows symbolic links, <code>symlink?</code> will always be
 *  <code>false</code> for an object returned by
 *  <code>File::stat</code>.
 *     
 *     File.symlink("testfile", "alink")   #=> 0
 *     File.stat("alink").symlink?         #=> false
 *     File.lstat("alink").symlink?        #=> true
 *     
 */

static VALUE
rb_stat_l(obj)
    VALUE obj;
{
#ifdef S_ISLNK
    if (S_ISLNK(get_stat(obj)->st_mode)) return Qtrue;
#endif
    return Qfalse;
}

/*
 *  call-seq:
 *     stat.socket?    => true or false
 *  
 *  Returns <code>true</code> if <i>stat</i> is a socket,
 *  <code>false</code> if it isn't or if the operating system doesn't
 *  support this feature.
 *     
 *     File.stat("testfile").socket?   #=> false
 *     
 */

static VALUE
rb_stat_S(obj)
    VALUE obj;
{
#ifdef S_ISSOCK
    if (S_ISSOCK(get_stat(obj)->st_mode)) return Qtrue;

#endif
    return Qfalse;
}

