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

