/*    perl.h
 *
 *    Copyright (c) 1987-1997, Larry Wall
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */
#ifndef H_PERL
#define H_PERL 1

#ifdef PERL_FOR_X2P
/*
 * This file is being used for x2p stuff. 
 * Above symbol is defined via -D in 'x2p/Makefile.SH'
 * Decouple x2p stuff from some of perls more extreme eccentricities. 
 */
#undef MULTIPLICITY
#undef USE_STDIO
#define USE_STDIO
#endif /* PERL_FOR_X2P */

#define VOIDUSED 1
#include "config.h"

/* See L<perlguts/"The Perl API"> for detailed notes on
 * PERL_IMPLICIT_CONTEXT and PERL_IMPLICIT_SYS */

/* XXXXXX testing threads via implicit pointer */
#ifdef USE_THREADS
#  ifndef PERL_IMPLICIT_CONTEXT
#    define PERL_IMPLICIT_CONTEXT
#  endif
#  ifndef PERL_IMPLICIT_SYS
/*#    define PERL_IMPLICIT_SYS*/		/* XXX not done yet */
#  endif
#endif

/* XXXXXX testing multiplicity via implicit pointer */
#if defined(MULTIPLICITY)
#  ifndef PERL_IMPLICIT_CONTEXT
#    define PERL_IMPLICIT_CONTEXT
#  endif
#  ifndef PERL_IMPLICIT_SYS
/*#    define PERL_IMPLICIT_SYS*/		/* XXX not done yet */
#  endif
#endif

#ifdef PERL_CAPI
#  undef PERL_OBJECT
#  ifndef PERL_IMPLICIT_CONTEXT
#    define PERL_IMPLICIT_CONTEXT
#  endif
#  ifndef PERL_IMPLICIT_SYS
#    define PERL_IMPLICIT_SYS
#  endif
#endif

#ifdef PERL_OBJECT
#  ifndef PERL_IMPLICIT_CONTEXT
#    define PERL_IMPLICIT_CONTEXT
#  endif
#  ifndef PERL_IMPLICIT_SYS
#    define PERL_IMPLICIT_SYS
#  endif
#endif

#ifdef PERL_OBJECT

/* PERL_OBJECT explained  - DickH and DougL @ ActiveState.com

Defining PERL_OBJECT turns on creation of a C++ object that
contains all writable core perl global variables and functions.
Stated another way, all necessary global variables and functions
are members of a big C++ object. This object's class is CPerlObj.
This allows a Perl Host to have multiple, independent perl
interpreters in the same process space. This is very important on
Win32 systems as the overhead of process creation is quite high --
this could be even higher than the script compile and execute time
for small scripts.

The perl executable implementation on Win32 is composed of perl.exe
(the Perl Host) and perlX.dll. (the Perl Core). This allows the
same Perl Core to easily be embedded in other applications that use
the perl interpreter.

+-----------+
| Perl Host |
+-----------+
      ^
	  |
	  v
+-----------+   +-----------+
| Perl Core |<->| Extension |
+-----------+   +-----------+ ...

Defining PERL_OBJECT has the following effects:

PERL CORE
1. CPerlObj is defined (this is the PERL_OBJECT)
2. all static functions that needed to access either global
variables or functions needed are made member functions
3. all writable static variables are made member variables
4. all global variables and functions are defined as:
	#define var CPerlObj::PL_var
	#define func CPerlObj::Perl_func
	* these are in embed.h
This necessitated renaming some local variables and functions that
had the same name as a global variable or function. This was
probably a _good_ thing anyway.


EXTENSIONS
1. Access to global variables and perl functions is through a
pointer to the PERL_OBJECT. This pointer type is CPerlObj*. This is
made transparent to extension developers by the following macros:
	#define var pPerl->PL_var
	#define func pPerl->Perl_func
	* these are done in objXSUB.h
This requires that the extension be compiled as C++, which means
that the code must be ANSI C and not K&R C. For K&R extensions,
please see the C API notes located in Win32/GenCAPI.pl. This script
creates a perlCAPI.lib that provides a K & R compatible C interface
to the PERL_OBJECT.
2. Local variables and functions cannot have the same name as perl's
variables or functions since the macros will redefine these. Look for
this if you get some strange error message and it does not look like
the code that you had written. This often happens with variables that
are local to a function.

PERL HOST
1. The perl host is linked with perlX.lib to get perl_alloc. This
function will return a pointer to CPerlObj (the PERL_OBJECT). It
takes pointers to the various PerlXXX_YYY interfaces (see iperlsys.h
for more information on this).
2. The perl host calls the same functions as normally would be
called in setting up and running a perl script, except that the
functions are now member functions of the PERL_OBJECT.

*/


class CPerlObj;

#define STATIC
#define CPERLscope(x)		CPerlObj::x
#define CALL_FPTR(fptr)		(this->*fptr)

#define pTHXo			CPerlObj *pPerl
#define pTHXo_			pTHXo,
#define _pTHXo			,pTHXo
#define aTHXo			this
#define aTHXo_			this,
#define _aTHXo			,this
#define PERL_OBJECT_THIS	aTHXo
#define PERL_OBJECT_THIS_	aTHXo_
#define _PERL_OBJECT_THIS	_aTHXo
#define dTHXoa(a)		pTHXo = (CPerlObj *)a
#define dTHXo			dTHXoa(PERL_GET_INTERP)

#define pTHXx		void
#define pTHXx_
#define _pTHXx
#define aTHXx
#define aTHXx_
#define _aTHXx

#else /* !PERL_OBJECT */

#ifdef PERL_IMPLICIT_CONTEXT
#  ifdef USE_THREADS
struct perl_thread;
#    define pTHX	register struct perl_thread *thr
#    define aTHX	thr
#    define dTHXa(a)	pTHX = (struct perl_thread *)a
#    define dTHX	dTHXa(THR)
#    define dTHR	dNOOP
#  else
#    ifndef MULTIPLICITY
#      define MULTIPLICITY
#    endif
#    define pTHX	register PerlInterpreter *my_perl
#    define aTHX	my_perl
#    define dTHXa(a)	pTHX = (PerlInterpreter *)a
#    define dTHX	dTHXa(PERL_GET_INTERP)
#  endif
#  define pTHX_		pTHX,
#  define _pTHX		,pTHX
#  define aTHX_		aTHX,
#  define _aTHX		,aTHX
#endif

#define STATIC static
#define CPERLscope(x) x
#define CPERLarg void
#define CPERLarg_
#define _CPERLarg
#define PERL_OBJECT_THIS
#define _PERL_OBJECT_THIS
#define PERL_OBJECT_THIS_
#define CALL_FPTR(fptr) (*fptr)

#endif /* PERL_OBJECT */

#define CALLRUNOPS  CALL_FPTR(PL_runops)
#define CALLREGCOMP CALL_FPTR(PL_regcompp)
#define CALLREGEXEC CALL_FPTR(PL_regexecp)
#define CALLREG_INTUIT_START CALL_FPTR(PL_regint_start)
#define CALLREG_INTUIT_STRING CALL_FPTR(PL_regint_string)
#define CALLREGFREE CALL_FPTR(PL_regfree)
#define CALLPROTECT CALL_FPTR(PL_protect)

#define NOOP (void)0
#define dNOOP extern int Perl___notused

#ifndef pTHX
#  define pTHX		void
#  define pTHX_
#  define _pTHX
#  define aTHX
#  define aTHX_
#  define _aTHX
#  define dTHXa(a)	dNOOP
#  define dTHX		dNOOP
#endif

#ifndef pTHXo
#  define pTHXo		pTHX
#  define pTHXo_	pTHX_
#  define _pTHXo	_pTHX
#  define aTHXo		aTHX
#  define aTHXo_	aTHX_
#  define _aTHXo	_aTHX
#  define dTHXo		dTHX
#endif

#ifndef pTHXx
#  define pTHXx		register PerlInterpreter *my_perl
#  define pTHXx_	pTHXx,
#  define _pTHXx	,pTHXx
#  define aTHXx		my_perl
#  define aTHXx_	aTHXx,
#  define _aTHXx	,aTHXx
#  define dTHXx		dTHX
#endif

#undef START_EXTERN_C
#undef END_EXTERN_C
#undef EXTERN_C
#ifdef __cplusplus
#  define START_EXTERN_C extern "C" {
#  define END_EXTERN_C }
#  define EXTERN_C extern "C"
#else
#  define START_EXTERN_C 
#  define END_EXTERN_C 
#  define EXTERN_C extern
#endif

#ifdef OP_IN_REGISTER
#  ifdef __GNUC__
#    define stringify_immed(s) #s
#    define stringify(s) stringify_immed(s)
register struct op *Perl_op asm(stringify(OP_IN_REGISTER));
#  endif
#endif

/*
 * STMT_START { statements; } STMT_END;
 * can be used as a single statement, as in
 * if (x) STMT_START { ... } STMT_END; else ...
 *
 * Trying to select a version that gives no warnings...
 */
#if !(defined(STMT_START) && defined(STMT_END))
# if defined(__GNUC__) && !defined(__STRICT_ANSI__) && !defined(__cplusplus)
#   define STMT_START	(void)(	/* gcc supports ``({ STATEMENTS; })'' */
#   define STMT_END	)
# else
   /* Now which other defined()s do we need here ??? */
#  if (VOIDFLAGS) && (defined(sun) || defined(__sun__))
#   define STMT_START	if (1)
#   define STMT_END	else (void)0
#  else
#   define STMT_START	do
#   define STMT_END	while (0)
#  endif
# endif
#endif

#define WITH_THX(s) STMT_START { dTHX; s; } STMT_END
#define WITH_THR(s) STMT_START { dTHR; s; } STMT_END

/*
 * SOFT_CAST can be used for args to prototyped functions to retain some
 * type checking; it only casts if the compiler does not know prototypes.
 */
#if defined(CAN_PROTOTYPE) && defined(DEBUGGING_COMPILE)
#define SOFT_CAST(type)	
#else
#define SOFT_CAST(type)	(type)
#endif

#ifndef BYTEORDER  /* Should never happen -- byteorder is in config.h */
#   define BYTEORDER 0x1234
#endif

/* Overall memory policy? */
#ifndef CONSERVATIVE
#   define LIBERAL 1
#endif

#if 'A' == 65 && 'I' == 73 && 'J' == 74 && 'Z' == 90
#define ASCIIish
#else
#undef  ASCIIish
#endif

/*
 * The following contortions are brought to you on behalf of all the
 * standards, semi-standards, de facto standards, not-so-de-facto standards
 * of the world, as well as all the other botches anyone ever thought of.
 * The basic theory is that if we work hard enough here, the rest of the
 * code can be a lot prettier.  Well, so much for theory.  Sorry, Henry...
 */

/* define this once if either system, instead of cluttering up the src */
#if defined(MSDOS) || defined(atarist) || defined(WIN32)
#define DOSISH 1
#endif

#if defined(__STDC__) || defined(vax11c) || defined(_AIX) || defined(__stdc__) || defined(__cplusplus) || defined( EPOC)
# define STANDARD_C 1
#endif

#if defined(__cplusplus) || defined(WIN32) || defined(__sgi) || defined(OS2) || defined(__DGUX) || defined( EPOC) || defined(__QNX__)
# define DONT_DECLARE_STD 1
#endif

#if defined(HASVOLATILE) || defined(STANDARD_C)
#   ifdef __cplusplus
#	define VOL		// to temporarily suppress warnings
#   else
#	define VOL volatile
#   endif
#else
#   define VOL
#endif

#define TAINT		(PL_tainted = TRUE)
#define TAINT_NOT	(PL_tainted = FALSE)
#define TAINT_IF(c)	if (c) { PL_tainted = TRUE; }
#define TAINT_ENV()	if (PL_tainting) { taint_env(); }
#define TAINT_PROPER(s)	if (PL_tainting) { taint_proper(Nullch, s); }

/* XXX All process group stuff is handled in pp_sys.c.  Should these 
   defines move there?  If so, I could simplify this a lot. --AD  9/96.
*/
/* Process group stuff changed from traditional BSD to POSIX.
   perlfunc.pod documents the traditional BSD-style syntax, so we'll
   try to preserve that, if possible.
*/
#ifdef HAS_SETPGID
#  define BSD_SETPGRP(pid, pgrp)	setpgid((pid), (pgrp))
#else
#  if defined(HAS_SETPGRP) && defined(USE_BSD_SETPGRP)
#    define BSD_SETPGRP(pid, pgrp)	setpgrp((pid), (pgrp))
#  else
#    ifdef HAS_SETPGRP2  /* DG/UX */
#      define BSD_SETPGRP(pid, pgrp)	setpgrp2((pid), (pgrp))
#    endif
#  endif
#endif
#if defined(BSD_SETPGRP) && !defined(HAS_SETPGRP)
#  define HAS_SETPGRP  /* Well, effectively it does . . . */
#endif

/* getpgid isn't POSIX, but at least Solaris and Linux have it, and it makes
    our life easier :-) so we'll try it.
*/
#ifdef HAS_GETPGID
#  define BSD_GETPGRP(pid)		getpgid((pid))
#else
#  if defined(HAS_GETPGRP) && defined(USE_BSD_GETPGRP)
#    define BSD_GETPGRP(pid)		getpgrp((pid))
#  else
#    ifdef HAS_GETPGRP2  /* DG/UX */
#      define BSD_GETPGRP(pid)		getpgrp2((pid))
#    endif
#  endif
#endif
#if defined(BSD_GETPGRP) && !defined(HAS_GETPGRP)
#  define HAS_GETPGRP  /* Well, effectively it does . . . */
#endif

/* These are not exact synonyms, since setpgrp() and getpgrp() may 
   have different behaviors, but perl.h used to define USE_BSDPGRP
   (prior to 5.003_05) so some extension might depend on it.
*/
#if defined(USE_BSD_SETPGRP) || defined(USE_BSD_GETPGRP)
#  ifndef USE_BSDPGRP
#    define USE_BSDPGRP
#  endif
#endif

/* HP-UX 10.X CMA (Common Multithreaded Architecure) insists that
   pthread.h must be included before all other header files.
*/
#if defined(USE_THREADS) && defined(PTHREAD_H_FIRST)
#  include <pthread.h>
#endif

#ifndef _TYPES_		/* If types.h defines this it's easy. */
#   ifndef major		/* Does everyone's types.h define this? */
#	include <sys/types.h>
#   endif
#endif

#ifdef __cplusplus
#  ifndef I_STDARG
#    define I_STDARG 1
#  endif
#endif

#ifdef I_STDARG
#  include <stdarg.h>
#else
#  ifdef I_VARARGS
#    include <varargs.h>
#  endif
#endif

#ifdef USE_NEXT_CTYPE

#if NX_CURRENT_COMPILER_RELEASE >= 500
#  include <bsd/ctypes.h>
#else
#  if NX_CURRENT_COMPILER_RELEASE >= 400
#    include <objc/NXCType.h>
#  else /*  NX_CURRENT_COMPILER_RELEASE < 400 */
#    include <appkit/NXCType.h>
#  endif /*  NX_CURRENT_COMPILER_RELEASE >= 400 */
#endif /*  NX_CURRENT_COMPILER_RELEASE >= 500 */

#else /* !USE_NEXT_CTYPE */
#include <ctype.h>
#endif /* USE_NEXT_CTYPE */

#ifdef METHOD 	/* Defined by OSF/1 v3.0 by ctype.h */
#undef METHOD
#endif

#ifdef I_LOCALE
#   include <locale.h>
#endif

#if !defined(NO_LOCALE) && defined(HAS_SETLOCALE)
#   define USE_LOCALE
#   if !defined(NO_LOCALE_COLLATE) && defined(LC_COLLATE) \
       && defined(HAS_STRXFRM)
#	define USE_LOCALE_COLLATE
#   endif
#   if !defined(NO_LOCALE_CTYPE) && defined(LC_CTYPE)
#	define USE_LOCALE_CTYPE
#   endif
#   if !defined(NO_LOCALE_NUMERIC) && defined(LC_NUMERIC)
#	define USE_LOCALE_NUMERIC
#   endif
#endif /* !NO_LOCALE && HAS_SETLOCALE */

#include <setjmp.h>

#ifdef I_SYS_PARAM
#   ifdef PARAM_NEEDS_TYPES
#	include <sys/types.h>
#   endif
#   include <sys/param.h>
#endif


/* Use all the "standard" definitions? */
#if defined(STANDARD_C) && defined(I_STDLIB)
#   include <stdlib.h>
#endif

#if !defined(PERL_FOR_X2P) && !defined(PERL_OBJECT)
#  include "embed.h"
#endif

#define MEM_SIZE Size_t

#if defined(STANDARD_C) && defined(I_STDDEF)
#   include <stddef.h>
#   define STRUCT_OFFSET(s,m)  offsetof(s,m)
#else
#   define STRUCT_OFFSET(s,m)  (Size_t)(&(((s *)0)->m))
#endif

#if defined(I_STRING) || defined(__cplusplus)
#   include <string.h>
#else
#   include <strings.h>
#endif

/* This comes after <stdlib.h> so we don't try to change the standard
 * library prototypes; we'll use our own in proto.h instead. */

#ifdef MYMALLOC
#  ifdef PERL_POLLUTE_MALLOC
#    define Perl_malloc		malloc
#    define Perl_calloc		calloc
#    define Perl_realloc	realloc
#    define Perl_mfree		free
#  else
#    define EMBEDMYMALLOC	/* for compatibility */
#  endif
Malloc_t Perl_malloc (MEM_SIZE nbytes);
Malloc_t Perl_calloc (MEM_SIZE elements, MEM_SIZE size);
Malloc_t Perl_realloc (Malloc_t where, MEM_SIZE nbytes);
/* 'mfree' rather than 'free', since there is already a 'perl_free'
 * that causes clashes with case-insensitive linkers */
Free_t   Perl_mfree (Malloc_t where);

#  define safemalloc  Perl_malloc
#  define safecalloc  Perl_calloc
#  define saferealloc Perl_realloc
#  define safefree    Perl_mfree
#else  /* MYMALLOC */
#  define safemalloc  safesysmalloc
#  define safecalloc  safesyscalloc
#  define saferealloc safesysrealloc
#  define safefree    safesysfree
#endif /* MYMALLOC */

#if !defined(HAS_STRCHR) && defined(HAS_INDEX) && !defined(strchr)
#define strchr index
#define strrchr rindex
#endif

#ifdef I_MEMORY
#  include <memory.h>
#endif

#ifdef HAS_MEMCPY
#  if !defined(STANDARD_C) && !defined(I_STRING) && !defined(I_MEMORY)
#    ifndef memcpy
        extern char * memcpy (char*, char*, int);
#    endif
#  endif
#else
#   ifndef memcpy
#	ifdef HAS_BCOPY
#	    define memcpy(d,s,l) bcopy(s,d,l)
#	else
#	    define memcpy(d,s,l) my_bcopy(s,d,l)
#	endif
#   endif
#endif /* HAS_MEMCPY */

#ifdef HAS_MEMSET
#  if !defined(STANDARD_C) && !defined(I_STRING) && !defined(I_MEMORY)
#    ifndef memset
	extern char *memset (char*, int, int);
#    endif
#  endif
#else
#  define memset(d,c,l) my_memset(d,c,l)
#endif /* HAS_MEMSET */

#if !defined(HAS_MEMMOVE) && !defined(memmove)
#   if defined(HAS_BCOPY) && defined(HAS_SAFE_BCOPY)
#	define memmove(d,s,l) bcopy(s,d,l)
#   else
#	if defined(HAS_MEMCPY) && defined(HAS_SAFE_MEMCPY)
#	    define memmove(d,s,l) memcpy(d,s,l)
#	else
#	    define memmove(d,s,l) my_bcopy(s,d,l)
#	endif
#   endif
#endif

#if defined(mips) && defined(ultrix) && !defined(__STDC__)
#   undef HAS_MEMCMP
#endif

#if defined(HAS_MEMCMP) && defined(HAS_SANE_MEMCMP)
#  if !defined(STANDARD_C) && !defined(I_STRING) && !defined(I_MEMORY)
#    ifndef memcmp
	extern int memcmp (char*, char*, int);
#    endif
#  endif
#  ifdef BUGGY_MSC
  #  pragma function(memcmp)
#  endif
#else
#   ifndef memcmp
#	define memcmp 	my_memcmp
#   endif
#endif /* HAS_MEMCMP && HAS_SANE_MEMCMP */

#ifndef memzero
#   ifdef HAS_MEMSET
#	define memzero(d,l) memset(d,0,l)
#   else
#	ifdef HAS_BZERO
#	    define memzero(d,l) bzero(d,l)
#	else
#	    define memzero(d,l) my_bzero(d,l)
#	endif
#   endif
#endif

#ifndef memchr
#   ifndef HAS_MEMCHR
#       define memchr(s,c,n) ninstr((char*)(s), ((char*)(s)) + n, &(c), &(c) + 1)
#   endif
#endif

#ifndef HAS_BCMP
#   ifndef bcmp
#	define bcmp(s1,s2,l) memcmp(s1,s2,l)
#   endif
#endif /* !HAS_BCMP */

#ifdef I_NETINET_IN
#   include <netinet/in.h>
#endif

#ifdef I_ARPA_INET
#   include <arpa/inet.h>
#endif

#if defined(SF_APPEND) && defined(USE_SFIO) && defined(I_SFIO)
/* <sfio.h> defines SF_APPEND and <sys/stat.h> might define SF_APPEND
 * (the neo-BSD seem to do this).  */
#   undef SF_APPEND
#endif

#ifdef I_SYS_STAT
#   include <sys/stat.h>
#endif

/* The stat macros for Amdahl UTS, Unisoft System V/88 (and derivatives
   like UTekV) are broken, sometimes giving false positives.  Undefine
   them here and let the code below set them to proper values.

   The ghs macro stands for GreenHills Software C-1.8.5 which
   is the C compiler for sysV88 and the various derivatives.
   This header file bug is corrected in gcc-2.5.8 and later versions.
   --Kaveh Ghazi (ghazi@noc.rutgers.edu) 10/3/94.  */

#if defined(uts) || (defined(m88k) && defined(ghs))
#   undef S_ISDIR
#   undef S_ISCHR
#   undef S_ISBLK
#   undef S_ISREG
#   undef S_ISFIFO
#   undef S_ISLNK
#endif

#ifdef I_TIME
#   include <time.h>
#endif

#ifdef I_SYS_TIME
#   ifdef I_SYS_TIME_KERNEL
#	define KERNEL
#   endif
#   include <sys/time.h>
#   ifdef I_SYS_TIME_KERNEL
#	undef KERNEL
#   endif
#endif

#if defined(HAS_TIMES) && defined(I_SYS_TIMES)
#    include <sys/times.h>
#endif

#if defined(HAS_STRERROR) && (!defined(HAS_MKDIR) || !defined(HAS_RMDIR))
#   undef HAS_STRERROR
#endif

#include <errno.h>
#ifdef HAS_SOCKET
#   ifdef I_NET_ERRNO
#     include <net/errno.h>
#   endif
#endif

#ifdef VMS
#   define SETERRNO(errcode,vmserrcode) \
	STMT_START {			\
	    set_errno(errcode);		\
	    set_vaxc_errno(vmserrcode);	\
	} STMT_END
#else
#   define SETERRNO(errcode,vmserrcode) (errno = (errcode))
#endif

#ifdef USE_THREADS
#  define ERRSV (thr->errsv)
#  define ERRHV (thr->errhv)
#  define DEFSV THREADSV(0)
#  define SAVE_DEFSV save_threadsv(0)
#else
#  define ERRSV GvSV(PL_errgv)
#  define ERRHV GvHV(PL_errgv)
#  define DEFSV GvSV(PL_defgv)
#  define SAVE_DEFSV SAVESPTR(GvSV(PL_defgv))
#endif /* USE_THREADS */

#ifndef errno
	extern int errno;     /* ANSI allows errno to be an lvalue expr.
			       * For example in multithreaded environments
			       * something like this might happen:
			       * extern int *_errno(void);
			       * #define errno (*_errno()) */
#endif

#ifdef HAS_STRERROR
#       ifdef VMS
	char *strerror (int,...);
#       else
#ifndef DONT_DECLARE_STD
	char *strerror (int);
#endif
#       endif
#       ifndef Strerror
#           define Strerror strerror
#       endif
#else
#    ifdef HAS_SYS_ERRLIST
	extern int sys_nerr;
	extern char *sys_errlist[];
#       ifndef Strerror
#           define Strerror(e) \
		((e) < 0 || (e) >= sys_nerr ? "(unknown)" : sys_errlist[e])
#       endif
#   endif
#endif

#ifdef I_SYS_IOCTL
#   ifndef _IOCTL_
#	include <sys/ioctl.h>
#   endif
#endif

#if defined(mc300) || defined(mc500) || defined(mc700) || defined(mc6000)
#   ifdef HAS_SOCKETPAIR
#	undef HAS_SOCKETPAIR
#   endif
#   ifdef I_NDBM
#	undef I_NDBM
#   endif
#endif

#if INTSIZE == 2
#   define htoni htons
#   define ntohi ntohs
#else
#   define htoni htonl
#   define ntohi ntohl
#endif

/* Configure already sets Direntry_t */
#if defined(I_DIRENT)
#   include <dirent.h>
    /* NeXT needs dirent + sys/dir.h */
#   if  defined(I_SYS_DIR) && (defined(NeXT) || defined(__NeXT__))
#	include <sys/dir.h>
#   endif
#else
#   ifdef I_SYS_NDIR
#	include <sys/ndir.h>
#   else
#	ifdef I_SYS_DIR
#	    ifdef hp9000s500
#		include <ndir.h>	/* may be wrong in the future */
#	    else
#		include <sys/dir.h>
#	    endif
#	endif
#   endif
#endif

#ifdef FPUTS_BOTCH
/* work around botch in SunOS 4.0.1 and 4.0.2 */
#   ifndef fputs
#	define fputs(sv,fp) fprintf(fp,"%s",sv)
#   endif
#endif

/*
 * The following gobbledygook brought to you on behalf of __STDC__.
 * (I could just use #ifndef __STDC__, but this is more bulletproof
 * in the face of half-implementations.)
 */

#ifndef S_IFMT
#   ifdef _S_IFMT
#	define S_IFMT _S_IFMT
#   else
#	define S_IFMT 0170000
#   endif
#endif

#ifndef S_ISDIR
#   define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#endif

#ifndef S_ISCHR
#   define S_ISCHR(m) ((m & S_IFMT) == S_IFCHR)
#endif

#ifndef S_ISBLK
#   ifdef S_IFBLK
#	define S_ISBLK(m) ((m & S_IFMT) == S_IFBLK)
#   else
#	define S_ISBLK(m) (0)
#   endif
#endif

#ifndef S_ISREG
#   define S_ISREG(m) ((m & S_IFMT) == S_IFREG)
#endif

#ifndef S_ISFIFO
#   ifdef S_IFIFO
#	define S_ISFIFO(m) ((m & S_IFMT) == S_IFIFO)
#   else
#	define S_ISFIFO(m) (0)
#   endif
#endif

#ifndef S_ISLNK
#   ifdef _S_ISLNK
#	define S_ISLNK(m) _S_ISLNK(m)
#   else
#	ifdef _S_IFLNK
#	    define S_ISLNK(m) ((m & S_IFMT) == _S_IFLNK)
#	else
#	    ifdef S_IFLNK
#		define S_ISLNK(m) ((m & S_IFMT) == S_IFLNK)
#	    else
#		define S_ISLNK(m) (0)
#	    endif
#	endif
#   endif
#endif

#ifndef S_ISSOCK
#   ifdef _S_ISSOCK
#	define S_ISSOCK(m) _S_ISSOCK(m)
#   else
#	ifdef _S_IFSOCK
#	    define S_ISSOCK(m) ((m & S_IFMT) == _S_IFSOCK)
#	else
#	    ifdef S_IFSOCK
#		define S_ISSOCK(m) ((m & S_IFMT) == S_IFSOCK)
#	    else
#		define S_ISSOCK(m) (0)
#	    endif
#	endif
#   endif
#endif

#ifndef S_IRUSR
#   ifdef S_IREAD
#	define S_IRUSR S_IREAD
#	define S_IWUSR S_IWRITE
#	define S_IXUSR S_IEXEC
#   else
#	define S_IRUSR 0400
#	define S_IWUSR 0200
#	define S_IXUSR 0100
#   endif
#   define S_IRGRP (S_IRUSR>>3)
#   define S_IWGRP (S_IWUSR>>3)
#   define S_IXGRP (S_IXUSR>>3)
#   define S_IROTH (S_IRUSR>>6)
#   define S_IWOTH (S_IWUSR>>6)
#   define S_IXOTH (S_IXUSR>>6)
#endif

#ifndef S_ISUID
#   define S_ISUID 04000
#endif

#ifndef S_ISGID
#   define S_ISGID 02000
#endif

#ifdef ff_next
#   undef ff_next
#endif

#if defined(cray) || defined(gould) || defined(i860) || defined(pyr)
#   define SLOPPYDIVIDE
#endif

#ifdef UV
#undef UV
#endif

#ifdef I_INTTYPES
#include <inttypes.h>
#endif

/*  XXX QUAD stuff is not currently supported on most systems.
    Specifically, perl internals don't support long long.  Among
    the many problems is that some compilers support long long,
    but the underlying library functions (such as sprintf) don't.
    Some things do work (such as quad pack/unpack on convex);
    also some systems use long long for the fpos_t typedef.  That
    seems to work too.

    The IV type is supposed to be long enough to hold any integral
    value or a pointer.
    --Andy Dougherty	August 1996
*/

/*  Much more 64-bit probing added.  Now we should get Quad_t
    in most systems: int64_t, long long, long, int, will do.

    Beware of LP32 systems (ILP32, ILP32LL64).  Such systems have been
    used to sizeof(long) == sizeof(foo*).  This is a bad assumption
    because then IV/UV have been 32 bits, too.  Which, in turn means
    that even if the system has quads (e.g. long long), IV cannot be a
    quad.  Introducing a 64-bit IV (because of long long existing)
    will introduce binary incompatibility.

    Summary: a long long system needs to add -DUSE_LONG_LONG to $ccflags
    to get quads -- and if its pointers are still 32 bits, this will break
    binary compatibility.  Casting an IV (a long long) to a pointer will
    truncate half of the IV away.

    --jhi		September 1998 */

#if INTSIZE == 4 && LONGSIZE == 4 && PTRSIZE == 4
#   define PERL_ILP32
#   if defined(HAS_LONG_LONG) && LONGLONGSIZE == 8
#       define PERL_ILP32LL64
#   endif
#endif

#if LONGSIZE == 8 && PTRSIZE == 8
#   define PERL_LP64
#   if INTSIZE == 8
#        define PERL_ILP64
#   endif
#endif

#ifndef Quad_t
#    if LONGSIZE == 8
#       define Quad_t  long
#       define Uquad_t unsigned long
#       define PERL_QUAD_IS_LONG
#    endif
#endif

#ifndef Quad_t
#    if INTSIZE == 8
#       define Quad_t  int
#       define Uquad_t unsigned int
#       define PERL_QUAD_IS_INT
#    endif
#endif

#ifndef Quad_t
#    ifdef USE_LONG_LONG /* See above note about LP32. --jhi */
#       if defined(HAS_LONG_LONG) && LONGLONGSIZE == 8
#	    define Quad_t  long long
#           define Uquad_t unsigned long long
#           define PERL_QUAD_IS_LONG_LONG
#       endif
#    endif
#endif

#ifndef Quad_t
#    ifdef HAS_INT64_T
#        define Quad_t  int64_t
#        define Uquad_t uint64_t
#        define PERL_QUAD_IS_INT64_T
#    endif
#endif

#ifdef Quad_t
#   define HAS_QUAD
#   ifndef Uquad_t
    /* Note that if your Quad_t is a typedef (not a #define) you *MUST*
     * have defined by now Uquad_t yourself because 'unsigned type'
     * is illegal. */
#       define Uquad_t unsigned Quad_t
#   endif
#endif

#if defined(USE_64_BITS) && defined(HAS_QUAD)
#  ifdef PERL_QUAD_IS_LONG			/* LP64 */
   typedef          long               IV;
   typedef	    unsigned long      UV;
#  else
#      ifdef PERL_QUAD_IS_INT			/* ILP64 */
   typedef          int                IV;
   typedef	    unsigned int       UV;
#      else
#          ifdef PERL_QUAD_IS_LONG_LONG		/* LL64 */
   typedef          long long          IV;
   typedef	    unsigned long long UV;
#          else
#              ifdef PERL_QUAD_IS_INT64_T	/* C9X */
   typedef          int64_t            IV;
   typedef	    uint64_t           UV;
#              endif
#          endif
#      endif
#  endif     
#  if defined(PERL_QUAD_IS_INT64_T) && defined(INT64_MAX)
#    define IV_MAX INT64_MAX
#    define IV_MIN INT64_MIN
#    define UV_MAX UINT64_MAX
#    define UV_MIN UINT64_MIN
#  else
#    define IV_MAX PERL_QUAD_MAX
#    define IV_MIN PERL_QUAD_MIN
#    define UV_MAX PERL_UQUAD_MAX
#    define UV_MIN PERL_UQUAD_MIN
#  endif
#  define IV_SIZEOF 8
#  define UV_SIZEOF 8
#  define IV_IS_QUAD
#  define UV_IS_QUAD
#else
   typedef          long               IV;
   typedef	    unsigned long      UV;
#  if defined(INT32_MAX) && LONGSIZE == 4
#    define IV_MAX INT32_MAX
#    define IV_MIN INT32_MIN
#    define UV_MAX UINT32_MAX
#    define UV_MIN UINT32_MIN
#  else
#    define IV_MAX PERL_LONG_MAX
#    define IV_MIN PERL_LONG_MIN
#    define UV_MAX PERL_ULONG_MAX
#    define UV_MIN PERL_ULONG_MIN
#  endif
#  define UV_SIZEOF LONGSIZE
#  define IV_SIZEOF LONGSIZE
#  if LONGSIZE == 8
#    define IV_IS_QUAD
#    define UV_IS_QUAD
#  else
#    undef IV_IS_QUAD
#    undef UV_IS_QUAD
#  endif
#  define UV_SIZEOF LONGSIZE
#  define IV_SIZEOF LONGSIZE
#endif

#ifdef USE_LONG_DOUBLE
#  if defined(HAS_LONG_DOUBLE) && (LONG_DOUBLESIZE > DOUBLESIZE)
#    define LDoub_t long double
#  endif
#endif

#ifdef USE_LONG_DOUBLE
#   define HAS_LDOUB
    typedef LDoub_t NV;
#   define Perl_modf modfl
#   define Perl_frexp frexpl
#   define Perl_cos cosl
#   define Perl_sin sinl
#   define Perl_sqrt sqrtl
#   define Perl_exp expl
#   define Perl_log logl
#   define Perl_atan2 atan2l
#   define Perl_pow powl
#   define Perl_floor floorl
#   define Perl_fmod fmodl
#else
    typedef double NV;
#   define Perl_modf modf
#   define Perl_frexp frexp
#   define Perl_cos cos
#   define Perl_sin sin
#   define Perl_sqrt sqrt
#   define Perl_exp exp
#   define Perl_log log
#   define Perl_atan2 atan2
#   define Perl_pow pow
#   define Perl_floor floor
#   define Perl_fmod fmod
#endif

#if defined(USE_LONG_DOUBLE) && defined(HAS_LONG_DOUBLE) && defined(HAS_ATOLF)
#   define Perl_atof atolf
#else
#   define Perl_atof atof
#endif

/* Previously these definitions used hardcoded figures. 
 * It is hoped these formula are more portable, although
 * no data one way or another is presently known to me.
 * The "PERL_" names are used because these calculated constants
 * do not meet the ANSI requirements for LONG_MAX, etc., which
 * need to be constants acceptable to #if - kja
 *    define PERL_LONG_MAX        2147483647L
 *    define PERL_LONG_MIN        (-LONG_MAX - 1)
 *    define PERL ULONG_MAX       4294967295L
 */

#ifdef I_LIMITS  /* Needed for cast_xxx() functions below. */
#  include <limits.h>
#else
#ifdef I_VALUES
#  include <values.h>
#endif
#endif

/*
 * Try to figure out max and min values for the integral types.  THE CORRECT
 * SOLUTION TO THIS MESS: ADAPT enquire.c FROM GCC INTO CONFIGURE.  The
 * following hacks are used if neither limits.h or values.h provide them:
 * U<TYPE>_MAX: for types >= int: ~(unsigned TYPE)0
 *              for types <  int:  (unsigned TYPE)~(unsigned)0
 *	The argument to ~ must be unsigned so that later signed->unsigned
 *	conversion can't modify the value's bit pattern (e.g. -0 -> +0),
 *	and it must not be smaller than int because ~ does integral promotion.
 * <type>_MAX: (<type>) (U<type>_MAX >> 1)
 * <type>_MIN: -<type>_MAX - <is_twos_complement_architecture: (3 & -1) == 3>.
 *	The latter is a hack which happens to work on some machines but
 *	does *not* catch any random system, or things like integer types
 *	with NaN if that is possible.
 *
 * All of the types are explicitly cast to prevent accidental loss of
 * numeric range, and in the hope that they will be less likely to confuse
 * over-eager optimizers.
 *
 */

#define PERL_UCHAR_MIN ((unsigned char)0)

#ifdef UCHAR_MAX
#  define PERL_UCHAR_MAX ((unsigned char)UCHAR_MAX)
#else
#  ifdef MAXUCHAR
#    define PERL_UCHAR_MAX ((unsigned char)MAXUCHAR)
#  else
#    define PERL_UCHAR_MAX       ((unsigned char)~(unsigned)0)
#  endif
#endif
 
/*
 * CHAR_MIN and CHAR_MAX are not included here, as the (char) type may be
 * ambiguous. It may be equivalent to (signed char) or (unsigned char)
 * depending on local options. Until Configure detects this (or at least
 * detects whether the "signed" keyword is available) the CHAR ranges
 * will not be included. UCHAR functions normally.
 *                                                           - kja
 */

#define PERL_USHORT_MIN ((unsigned short)0)

#ifdef USHORT_MAX
#  define PERL_USHORT_MAX ((unsigned short)USHORT_MAX)
#else
#  ifdef MAXUSHORT
#    define PERL_USHORT_MAX ((unsigned short)MAXUSHORT)
#  else
#    ifdef USHRT_MAX
#      define PERL_USHORT_MAX ((unsigned short)USHRT_MAX)
#    else
#      define PERL_USHORT_MAX       ((unsigned short)~(unsigned)0)
#    endif
#  endif
#endif

#ifdef SHORT_MAX
#  define PERL_SHORT_MAX ((short)SHORT_MAX)
#else
#  ifdef MAXSHORT    /* Often used in <values.h> */
#    define PERL_SHORT_MAX ((short)MAXSHORT)
#  else
#    ifdef SHRT_MAX
#      define PERL_SHORT_MAX ((short)SHRT_MAX)
#    else
#      define PERL_SHORT_MAX      ((short) (PERL_USHORT_MAX >> 1))
#    endif
#  endif
#endif

#ifdef SHORT_MIN
#  define PERL_SHORT_MIN ((short)SHORT_MIN)
#else
#  ifdef MINSHORT
#    define PERL_SHORT_MIN ((short)MINSHORT)
#  else
#    ifdef SHRT_MIN
#      define PERL_SHORT_MIN ((short)SHRT_MIN)
#    else
#      define PERL_SHORT_MIN        (-PERL_SHORT_MAX - ((3 & -1) == 3))
#    endif
#  endif
#endif

#ifdef UINT_MAX
#  define PERL_UINT_MAX ((unsigned int)UINT_MAX)
#else
#  ifdef MAXUINT
#    define PERL_UINT_MAX ((unsigned int)MAXUINT)
#  else
#    define PERL_UINT_MAX       (~(unsigned int)0)
#  endif
#endif

#define PERL_UINT_MIN ((unsigned int)0)

#ifdef INT_MAX
#  define PERL_INT_MAX ((int)INT_MAX)
#else
#  ifdef MAXINT    /* Often used in <values.h> */
#    define PERL_INT_MAX ((int)MAXINT)
#  else
#    define PERL_INT_MAX        ((int)(PERL_UINT_MAX >> 1))
#  endif
#endif

#ifdef INT_MIN
#  define PERL_INT_MIN ((int)INT_MIN)
#else
#  ifdef MININT
#    define PERL_INT_MIN ((int)MININT)
#  else
#    define PERL_INT_MIN        (-PERL_INT_MAX - ((3 & -1) == 3))
#  endif
#endif

#ifdef ULONG_MAX
#  define PERL_ULONG_MAX ((unsigned long)ULONG_MAX)
#else
#  ifdef MAXULONG
#    define PERL_ULONG_MAX ((unsigned long)MAXULONG)
#  else
#    define PERL_ULONG_MAX       (~(unsigned long)0)
#  endif
#endif

#define PERL_ULONG_MIN ((unsigned long)0L)

#ifdef LONG_MAX
#  define PERL_LONG_MAX ((long)LONG_MAX)
#else
#  ifdef MAXLONG    /* Often used in <values.h> */
#    define PERL_LONG_MAX ((long)MAXLONG)
#  else
#    define PERL_LONG_MAX        ((long) (PERL_ULONG_MAX >> 1))
#  endif
#endif

#ifdef LONG_MIN
#  define PERL_LONG_MIN ((long)LONG_MIN)
#else
#  ifdef MINLONG
#    define PERL_LONG_MIN ((long)MINLONG)
#  else
#    define PERL_LONG_MIN        (-PERL_LONG_MAX - ((3 & -1) == 3))
#  endif
#endif

#ifdef HAS_QUAD

#  ifdef UQUAD_MAX
#    define PERL_UQUAD_MAX ((UV)UQUAD_MAX)
#  else
#    define PERL_UQUAD_MAX	(~(UV)0)
#  endif

#  define PERL_UQUAD_MIN ((UV)0)

#  ifdef QUAD_MAX
#    define PERL_QUAD_MAX ((IV)QUAD_MAX)
#  else
#    define PERL_QUAD_MAX 	((IV) (PERL_UQUAD_MAX >> 1))
#  endif

#  ifdef QUAD_MIN
#    define PERL_QUAD_MIN ((IV)QUAD_MIN)
#  else
#    define PERL_QUAD_MIN 	(-PERL_QUAD_MAX - ((3 & -1) == 3))
#  endif

#endif

typedef MEM_SIZE STRLEN;

typedef struct op OP;
typedef struct cop COP;
typedef struct unop UNOP;
typedef struct binop BINOP;
typedef struct listop LISTOP;
typedef struct logop LOGOP;
typedef struct pmop PMOP;
typedef struct svop SVOP;
typedef struct gvop GVOP;
typedef struct pvop PVOP;
typedef struct loop LOOP;

typedef struct Outrec Outrec;
typedef struct interpreter PerlInterpreter;
#ifndef __BORLANDC__
typedef struct ff FF;		/* XXX not defined anywhere, should go? */
#endif
typedef struct sv SV;
typedef struct av AV;
typedef struct hv HV;
typedef struct cv CV;
typedef struct regexp REGEXP;
typedef struct gp GP;
typedef struct gv GV;
typedef struct io IO;
typedef struct context PERL_CONTEXT;
typedef struct block BLOCK;

typedef struct magic MAGIC;
typedef struct xrv XRV;
typedef struct xpv XPV;
typedef struct xpviv XPVIV;
typedef struct xpvuv XPVUV;
typedef struct xpvnv XPVNV;
typedef struct xpvmg XPVMG;
typedef struct xpvlv XPVLV;
typedef struct xpvav XPVAV;
typedef struct xpvhv XPVHV;
typedef struct xpvgv XPVGV;
typedef struct xpvcv XPVCV;
typedef struct xpvbm XPVBM;
typedef struct xpvfm XPVFM;
typedef struct xpvio XPVIO;
typedef struct mgvtbl MGVTBL;
typedef union any ANY;

#include "handy.h"

/* Some day when we have more 64-bit experience under our belts we may
 * be able to merge some of the USE_64_BIT_{FILES,OFFSETS,STDIO,DBM}. At
 * the moment (Oct 1998), though, keep them separate. --jhi
 */
#ifdef USE_64_BITS
#   ifdef USE_64_BIT_FILES
#       ifndef USE_64_BIT_OFFSETS
#          define USE_64_BIT_OFFSETS
#       endif
#       ifndef USE_64_BIT_STDIO
#           define USE_64_BIT_STDIO
#       endif
#       ifndef USE_64_BIT_DBM
#           define USE_64_BIT_DBM
#       endif
#   endif
/* Mention LSEEKSIZE here to get it included in %Config. */
#   ifdef USE_64_BIT_OFFSETS
#       ifdef HAS_FSTAT64
#           define fstat fstat64
#       endif
#       ifdef HAS_FTRUNCATE64
#           define ftruncate ftruncate64
#       endif
#       ifdef HAS_LSEEK64
#           define lseek lseek64
#           ifdef HAS_OFF64_T
#               undef Off_t
#               define Off_t off64_t
#           endif
#       endif
#       ifdef HAS_LSTAT64
#           define lstat lstat64
#       endif
	/* Some systems have open64() in libc but use that only
	 * for true LP64 mode, in mixed mode (ILP32LL64, for example)
	 * they use the vanilla open().  Such systems should undefine
	 * d_open64 in their hints files. --jhi */
#       if defined(HAS_OPEN64)
#           define open open64
#       endif
#       ifdef HAS_OPENDIR64
#           define opendir opendir64
#       endif
#       ifdef HAS_READDIR64
#           define readdir readdir64
#	    ifdef HAS_STRUCT_DIRENT64
#               define dirent dirent64
#           endif
#       endif
#       ifdef HAS_SEEKDIR64
#           define seekdir seekdir64
#       endif
#       ifdef HAS_STAT64
#           define stat stat64 /* Affects also struct stat, hopefully okay. */
#       endif
#       ifdef HAS_TELLDIR64
#           define telldir telldir64
#       endif
#       ifdef HAS_TRUNCATE64
#           define truncate truncate64
#       endif
        /* flock is not #defined here to be flock64 because it seems
	   that a system may have struct flock64 but still use flock()
	   and not flock64().  The actual flocking code in pp_sys.c
	   must be changed.  Also lockf and lockf64 must be dealt
	   with in pp_sys.c. --jhi */
#   endif
#   ifdef USE_64_BIT_STDIO
#       ifdef HAS_FGETPOS64
#           define fgetpos fgetpos64
#       endif
#       ifdef HAS_FOPEN64
#           define fopen fopen64
#       endif
#       ifdef HAS_FREOPEN64
#           define freopen freopen64
#       endif
#       ifdef HAS_FSEEK64
#           define fseek fseek64
#       endif
#       ifdef HAS_FSEEKO64
#           define fseeko fseeko64
#       endif
#       ifdef HAS_FSETPOS64
#           define fsetpos fsetpos64
#       endif
#       ifdef HAS_FTELL64
#           define ftell ftell64
#       endif
#       ifdef HAS_FTELLO64
#           define ftello ftello64
#       endif
#       ifdef HAS_TMPFILE64
#           define tmpfile tmpfile64
#       endif
#   endif
#   ifdef USE_64_BIT_DBM
#       ifdef HAS_DBMINIT64
#           define dbminit dbminit64
#       endif
#       ifdef HAS_DBMCLOSE64
#           define dbmclose dbmclose64
#       endif
#       ifdef HAS_FETCH64
#           define fetch fetch64
#       endif
#       ifdef HAS_DELETE64
#           define delete delete64
#       endif
#       ifdef HAS_STORE64
#           define store store64
#       endif
#       ifdef HAS_FIRSTKEY64
#           define firstkey firstkey64
#       endif
#       ifdef HAS_NEXTKEY64
#           define nextkey nextkey64
#       endif
#   endif
#endif

#if defined(OS2)
#  include "iperlsys.h"
#endif

#if defined(__OPEN_VM)
# include "vmesa/vmesaish.h"
#endif

#ifdef DOSISH
# if defined(OS2)
#   include "os2ish.h"
# else
#   include "dosish.h"
# endif
#else
# if defined(VMS)
#   include "vmsish.h"
# else
#   if defined(PLAN9)
#     include "./plan9/plan9ish.h"
#   else
#     if defined(MPE)
#       include "mpeix/mpeixish.h"
#     else
#       if defined(__VOS__)
#         include "vosish.h"
#       else
#         if defined(EPOC)
#           include "epocish.h"
#         else
#           include "unixish.h"
#         endif
#       endif
#     endif
#   endif
# endif
#endif         

#ifndef MAXPATHLEN
#  ifdef PATH_MAX
#    ifdef _POSIX_PATH_MAX
#       if PATH_MAX > _POSIX_PATH_MAX
/* MAXPATHLEN is supposed to include the final null character,
 * as opposed to PATH_MAX and _POSIX_PATH_MAX. */
#         define MAXPATHLEN (PATH_MAX+1)
#       else
#         define MAXPATHLEN (_POSIX_PATH_MAX+1)
#       endif
#    else
#      define MAXPATHLEN (PATH_MAX+1)
#    endif
#  else
#    ifdef _POSIX_PATH_MAX
#       define MAXPATHLEN (_POSIX_PATH_MAX+1)
#    else
#       define MAXPATHLEN 1024	/* Err on the large side. */
#    endif
#  endif
#endif

/* 
 * USE_THREADS needs to be after unixish.h as <pthread.h> includes
 * <sys/signal.h> which defines NSIG - which will stop inclusion of <signal.h>
 * this results in many functions being undeclared which bothers C++
 * May make sense to have threads after "*ish.h" anyway
 */

#ifdef USE_THREADS
   /* pending resolution of licensing issues, we avoid the erstwhile
    * atomic.h everywhere */
#  define EMULATE_ATOMIC_REFCOUNTS

#  ifdef FAKE_THREADS
#    include "fakethr.h"
#  else
#    ifdef WIN32
#      include <win32thread.h>
#    else
#      ifdef OS2
#        include "os2thread.h"
#      else
#        ifdef I_MACH_CTHREADS
#          include <mach/cthreads.h>
#          if (defined(NeXT) || defined(__NeXT__)) && defined(PERL_POLLUTE_MALLOC)
#            define MUTEX_INIT_CALLS_MALLOC
#          endif
typedef cthread_t	perl_os_thread;
typedef mutex_t		perl_mutex;
typedef condition_t	perl_cond;
typedef void *		perl_key;
#        else /* Posix threads */
#          include <pthread.h>
typedef pthread_t	perl_os_thread;
typedef pthread_mutex_t	perl_mutex;
typedef pthread_cond_t	perl_cond;
typedef pthread_key_t	perl_key;
#        endif /* I_MACH_CTHREADS */
#      endif /* OS2 */
#    endif /* WIN32 */
#  endif /* FAKE_THREADS */
#endif /* USE_THREADS */

#ifdef WIN32
#include "win32.h"
#endif

#ifdef VMS
#   define STATUS_NATIVE	PL_statusvalue_vms
#   define STATUS_NATIVE_EXPORT \
	((I32)PL_statusvalue_vms == -1 ? 44 : PL_statusvalue_vms)
#   define STATUS_NATIVE_SET(n)						\
	STMT_START {							\
	    PL_statusvalue_vms = (n);					\
	    if ((I32)PL_statusvalue_vms == -1)				\
		PL_statusvalue = -1;					\
	    else if (PL_statusvalue_vms & STS$M_SUCCESS)		\
		PL_statusvalue = 0;					\
	    else if ((PL_statusvalue_vms & STS$M_SEVERITY) == 0)	\
		PL_statusvalue = 1 << 8;				\
	    else							\
		PL_statusvalue = (PL_statusvalue_vms & STS$M_SEVERITY) << 8;	\
	} STMT_END
#   define STATUS_POSIX	PL_statusvalue
#   ifdef VMSISH_STATUS
#	define STATUS_CURRENT	(VMSISH_STATUS ? STATUS_NATIVE : STATUS_POSIX)
#   else
#	define STATUS_CURRENT	STATUS_POSIX
#   endif
#   define STATUS_POSIX_SET(n)				\
	STMT_START {					\
	    PL_statusvalue = (n);				\
	    if (PL_statusvalue != -1) {			\
		PL_statusvalue &= 0xFFFF;			\
		PL_statusvalue_vms = PL_statusvalue ? 44 : 1;	\
	    }						\
	    else PL_statusvalue_vms = -1;			\
	} STMT_END
#   define STATUS_ALL_SUCCESS	(PL_statusvalue = 0, PL_statusvalue_vms = 1)
#   define STATUS_ALL_FAILURE	(PL_statusvalue = 1, PL_statusvalue_vms = 44)
#else
#   define STATUS_NATIVE	STATUS_POSIX
#   define STATUS_NATIVE_EXPORT	STATUS_POSIX
#   define STATUS_NATIVE_SET	STATUS_POSIX_SET
#   define STATUS_POSIX		PL_statusvalue
#   define STATUS_POSIX_SET(n)		\
	STMT_START {			\
	    PL_statusvalue = (n);		\
	    if (PL_statusvalue != -1)	\
		PL_statusvalue &= 0xFFFF;	\
	} STMT_END
#   define STATUS_CURRENT STATUS_POSIX
#   define STATUS_ALL_SUCCESS	(PL_statusvalue = 0)
#   define STATUS_ALL_FAILURE	(PL_statusvalue = 1)
#endif

#ifndef MEMBER_TO_FPTR
#define MEMBER_TO_FPTR(name)		name
#endif

/* This defines a way to flush all output buffers.  This may be a
 * performance issue, so we allow people to disable it.
 * XXX the default needs a Configure test, as it may not work everywhere.
 */
#ifndef PERL_FLUSHALL_FOR_CHILD
# if defined(FFLUSH_NULL) || defined(USE_SFIO)
#  define PERL_FLUSHALL_FOR_CHILD	PerlIO_flush((PerlIO*)NULL)
# else
#  ifdef FFLUSH_ALL
#   define PERL_FLUSHALL_FOR_CHILD	my_fflush_all()
#  else
#   define PERL_FLUSHALL_FOR_CHILD	NOOP
#  endif
# endif
#endif

#ifndef PERL_SET_INTERP
#  define PERL_SET_INTERP(i)		(PL_curinterp = (PerlInterpreter*)(i))
#endif

#ifndef PERL_GET_INTERP
#  define PERL_GET_INTERP		(PL_curinterp)
#endif

/* Some unistd.h's give a prototype for pause() even though
   HAS_PAUSE ends up undefined.  This causes the #define
   below to be rejected by the compmiler.  Sigh.
*/
#ifdef HAS_PAUSE
#define Pause	pause
#else
#define Pause() sleep((32767<<16)+32767)
#endif

#ifndef IOCPARM_LEN
#   ifdef IOCPARM_MASK
	/* on BSDish systes we're safe */
#	define IOCPARM_LEN(x)  (((x) >> 16) & IOCPARM_MASK)
#   else
	/* otherwise guess at what's safe */
#	define IOCPARM_LEN(x)	256
#   endif
#endif

#if defined(CYGWIN)
/* USEMYBINMODE
 *   This symbol, if defined, indicates that the program should
 *   use the routine my_binmode(FILE *fp, char iotype) to insure
 *   that a file is in "binary" mode -- that is, that no translation
 *   of bytes occurs on read or write operations.
 */
#  define USEMYBINMODE / **/
#  define my_binmode(fp, iotype) \
            (PerlLIO_setmode(PerlIO_fileno(fp), O_BINARY) != -1 ? TRUE : FALSE)
#endif

#ifdef UNION_ANY_DEFINITION
UNION_ANY_DEFINITION;
#else
union any {
    void*	any_ptr;
    I32		any_i32;
    IV		any_iv;
    long	any_long;
    void	(*any_dptr) (pTHXo_ void*);
};
#endif

#ifdef USE_THREADS
#define ARGSproto struct perl_thread *thr
#else
#define ARGSproto
#endif /* USE_THREADS */

typedef I32 (*filter_t) (pTHXo_ int, SV *, int);

#define FILTER_READ(idx, sv, len)  filter_read(idx, sv, len)
#define FILTER_DATA(idx)	   (AvARRAY(PL_rsfp_filters)[idx])
#define FILTER_ISREADER(idx)	   (idx >= AvFILLp(PL_rsfp_filters))

#if !defined(OS2)
#  include "iperlsys.h"
#endif
#include "regexp.h"
#include "sv.h"
#include "util.h"
#include "form.h"
#include "gv.h"
#include "cv.h"
#include "opnames.h"
#include "op.h"
#include "cop.h"
#include "av.h"
#include "hv.h"
#include "mg.h"
#include "scope.h"
#include "warning.h"
#include "utf8.h"

/* Current curly descriptor */
typedef struct curcur CURCUR;
struct curcur {
    int		parenfloor;	/* how far back to strip paren data */
    int		cur;		/* how many instances of scan we've matched */
    int		min;		/* the minimal number of scans to match */
    int		max;		/* the maximal number of scans to match */
    int		minmod;		/* whether to work our way up or down */
    regnode *	scan;		/* the thing to match */
    regnode *	next;		/* what has to match after it */
    char *	lastloc;	/* where we started matching this scan */
    CURCUR *	oldcc;		/* current curly before we started this one */
};

typedef struct _sublex_info SUBLEXINFO;
struct _sublex_info {
    I32 super_state;	/* lexer state to save */
    I32 sub_inwhat;	/* "lex_inwhat" to use */
    OP *sub_op;		/* "lex_op" to use */
    char *super_bufptr;	/* PL_bufptr that was */
    char *super_bufend;	/* PL_bufend that was */
};

typedef struct magic_state MGS;	/* struct magic_state defined in mg.c */

struct scan_data_t;		/* Used in S_* functions in regcomp.c */

typedef I32 CHECKPOINT;

#if defined(iAPX286) || defined(M_I286) || defined(I80286)
#   define I286
#endif

#if defined(htonl) && !defined(HAS_HTONL)
#define HAS_HTONL
#endif
#if defined(htons) && !defined(HAS_HTONS)
#define HAS_HTONS
#endif
#if defined(ntohl) && !defined(HAS_NTOHL)
#define HAS_NTOHL
#endif
#if defined(ntohs) && !defined(HAS_NTOHS)
#define HAS_NTOHS
#endif
#ifndef HAS_HTONL
#if (BYTEORDER & 0xffff) != 0x4321
#define HAS_HTONS
#define HAS_HTONL
#define HAS_NTOHS
#define HAS_NTOHL
#define MYSWAP
#define htons my_swap
#define htonl my_htonl
#define ntohs my_swap
#define ntohl my_ntohl
#endif
#else
#if (BYTEORDER & 0xffff) == 0x4321
#undef HAS_HTONS
#undef HAS_HTONL
#undef HAS_NTOHS
#undef HAS_NTOHL
#endif
#endif

/*
 * Little-endian byte order functions - 'v' for 'VAX', or 'reVerse'.
 * -DWS
 */
#if BYTEORDER != 0x1234
# define HAS_VTOHL
# define HAS_VTOHS
# define HAS_HTOVL
# define HAS_HTOVS
# if BYTEORDER == 0x4321 || BYTEORDER == 0x87654321
#  define vtohl(x)	((((x)&0xFF)<<24)	\
			+(((x)>>24)&0xFF)	\
			+(((x)&0x0000FF00)<<8)	\
			+(((x)&0x00FF0000)>>8)	)
#  define vtohs(x)	((((x)&0xFF)<<8) + (((x)>>8)&0xFF))
#  define htovl(x)	vtohl(x)
#  define htovs(x)	vtohs(x)
# endif
	/* otherwise default to functions in util.c */
#endif

#ifdef CASTNEGFLOAT
#define U_S(what) ((U16)(what))
#define U_I(what) ((unsigned int)(what))
#define U_L(what) ((U32)(what))
#else
#define U_S(what) ((U16)cast_ulong((NV)(what)))
#define U_I(what) ((unsigned int)cast_ulong((NV)(what)))
#define U_L(what) (cast_ulong((NV)(what)))
#endif

#ifdef CASTI32
#define I_32(what) ((I32)(what))
#define I_V(what) ((IV)(what))
#define U_V(what) ((UV)(what))
#else
#define I_32(what) (cast_i32((NV)(what)))
#define I_V(what) (cast_iv((NV)(what)))
#define U_V(what) (cast_uv((NV)(what)))
#endif

/* Used with UV/IV arguments: */
					/* XXXX: need to speed it up */
#define CLUMP_2UV(iv)	((iv) < 0 ? 0 : (UV)(iv))
#define CLUMP_2IV(uv)	((uv) > (UV)IV_MAX ? IV_MAX : (IV)(uv))

struct Outrec {
    I32		o_lines;
    char	*o_str;
    U32		o_len;
};

#ifndef MAXSYSFD
#   define MAXSYSFD 2
#endif

#ifndef TMPPATH
#  define TMPPATH "/tmp/perl-eXXXXXX"
#endif

#ifndef __cplusplus
Uid_t getuid (void);
Uid_t geteuid (void);
Gid_t getgid (void);
Gid_t getegid (void);
#endif

#ifndef Perl_debug_log
#define Perl_debug_log	PerlIO_stderr()
#endif

#ifdef DEBUGGING
#undef  YYDEBUG
#define YYDEBUG 1
#define DEB(a)     			a
#define DEBUG(a)   if (PL_debug)		a
#define DEBUG_p(a) if (PL_debug & 1)	a
#define DEBUG_s(a) if (PL_debug & 2)	a
#define DEBUG_l(a) if (PL_debug & 4)	a
#define DEBUG_t(a) if (PL_debug & 8)	a
#define DEBUG_o(a) if (PL_debug & 16)	a
#define DEBUG_c(a) if (PL_debug & 32)	a
#define DEBUG_P(a) if (PL_debug & 64)	a
#  if defined(PERL_OBJECT)
#    define DEBUG_m(a) if (PL_debug & 128)	a
#  else
#    define DEBUG_m(a) if (PERL_GET_INTERP && PL_debug & 128)	a
#  endif
#define DEBUG_f(a) if (PL_debug & 256)	a
#define DEBUG_r(a) if (PL_debug & 512)	a
#define DEBUG_x(a) if (PL_debug & 1024)	a
#define DEBUG_u(a) if (PL_debug & 2048)	a
#define DEBUG_L(a) if (PL_debug & 4096)	a
#define DEBUG_H(a) if (PL_debug & 8192)	a
#define DEBUG_X(a) if (PL_debug & 16384)	a
#define DEBUG_D(a) if (PL_debug & 32768)	a
#  ifdef USE_THREADS
#    define DEBUG_S(a) if (PL_debug & (1<<16))	a
#  else
#    define DEBUG_S(a)
#  endif
#else
#define DEB(a)
#define DEBUG(a)
#define DEBUG_p(a)
#define DEBUG_s(a)
#define DEBUG_l(a)
#define DEBUG_t(a)
#define DEBUG_o(a)
#define DEBUG_c(a)
#define DEBUG_P(a)
#define DEBUG_m(a)
#define DEBUG_f(a)
#define DEBUG_r(a)
#define DEBUG_x(a)
#define DEBUG_u(a)
#define DEBUG_S(a)
#define DEBUG_H(a)
#define DEBUG_X(a)
#define DEBUG_D(a)
#define DEBUG_S(a)
#endif
#define YYMAXDEPTH 300

#ifndef assert  /* <assert.h> might have been included somehow */
#define assert(what)	DEB( {						\
	if (!(what)) {							\
	    Perl_croak(aTHX_ "Assertion failed: file \"%s\", line %d",	\
		__FILE__, __LINE__);					\
	    PerlProc_exit(1);						\
	}})
#endif

struct ufuncs {
    I32 (*uf_val)(IV, SV*);
    I32 (*uf_set)(IV, SV*);
    IV uf_index;
};

/* Fix these up for __STDC__ */
#ifndef DONT_DECLARE_STD
char *mktemp (char*);
double atof (const char*);
#endif

#ifndef STANDARD_C
/* All of these are in stdlib.h or time.h for ANSI C */
Time_t time();
struct tm *gmtime(), *localtime();
#if defined(OEMVS) || defined(__OPEN_VM)
char *(strchr)(), *(strrchr)();
char *(strcpy)(), *(strcat)();
#else
char *strchr(), *strrchr();
char *strcpy(), *strcat();
#endif
#endif /* ! STANDARD_C */


#ifdef I_MATH
#    include <math.h>
#else
START_EXTERN_C
	    double exp (double);
	    double log (double);
	    double log10 (double);
	    double sqrt (double);
	    double frexp (double,int*);
	    double ldexp (double,int);
	    double modf (double,double*);
	    double sin (double);
	    double cos (double);
	    double atan2 (double,double);
	    double pow (double,double);
END_EXTERN_C
#endif

#ifndef __cplusplus
#  if defined(NeXT) || defined(__NeXT__) /* or whatever catches all NeXTs */
char *crypt ();       /* Maybe more hosts will need the unprototyped version */
#  else
#    if !defined(WIN32) || !defined(HAVE_DES_FCRYPT)
char *crypt (const char*, const char*);
#    endif /* !WIN32 && !HAVE_CRYPT_SOURCE */
#  endif /* !NeXT && !__NeXT__ */
#  ifndef DONT_DECLARE_STD
#    ifndef getenv
char *getenv (const char*);
#    endif /* !getenv */
#ifndef EPOC
Off_t lseek (int,Off_t,int);
#endif
#  endif /* !DONT_DECLARE_STD */
char *getlogin (void);
#endif /* !__cplusplus */

#ifdef UNLINK_ALL_VERSIONS /* Currently only makes sense for VMS */
#define UNLINK unlnk
I32 unlnk (char*);
#else
#define UNLINK PerlLIO_unlink
#endif

#ifndef HAS_SETREUID
#  ifdef HAS_SETRESUID
#    define setreuid(r,e) setresuid(r,e,(Uid_t)-1)
#    define HAS_SETREUID
#  endif
#endif
#ifndef HAS_SETREGID
#  ifdef HAS_SETRESGID
#    define setregid(r,e) setresgid(r,e,(Gid_t)-1)
#    define HAS_SETREGID
#  endif
#endif

typedef Signal_t (*Sighandler_t) (int);

#ifdef HAS_SIGACTION
typedef struct sigaction Sigsave_t;
#else
typedef Sighandler_t Sigsave_t;
#endif

#define SCAN_DEF 0
#define SCAN_TR 1
#define SCAN_REPL 2

#ifdef DEBUGGING
# ifndef register
#  define register
# endif
# define PAD_SV(po) pad_sv(po)
# define RUNOPS_DEFAULT Perl_runops_debug
#else
# define PAD_SV(po) PL_curpad[po]
# define RUNOPS_DEFAULT Perl_runops_standard
#endif

#ifdef MYMALLOC
#  ifdef MUTEX_INIT_CALLS_MALLOC
#    define MALLOC_INIT					\
	STMT_START {					\
		PL_malloc_mutex = NULL;			\
		MUTEX_INIT(&PL_malloc_mutex);		\
	} STMT_END
#    define MALLOC_TERM					\
	STMT_START {					\
		perl_mutex tmp = PL_malloc_mutex;	\
		PL_malloc_mutex = NULL;			\
		MUTEX_DESTROY(&tmp);			\
	} STMT_END
#  else
#    define MALLOC_INIT MUTEX_INIT(&PL_malloc_mutex)
#    define MALLOC_TERM MUTEX_DESTROY(&PL_malloc_mutex)
#  endif
#else
#  define MALLOC_INIT
#  define MALLOC_TERM
#endif


typedef int (CPERLscope(*runops_proc_t)) (pTHX);
typedef OP* (CPERLscope(*PPADDR_t)[]) (pTHX);

/* _ (for $_) must be first in the following list (DEFSV requires it) */
#define THREADSV_NAMES "_123456789&`'+/.,\\\";^-%=|~:\001\005!@"

/* NeXT has problems with crt0.o globals */
#if defined(__DYNAMIC__) && \
    (defined(NeXT) || defined(__NeXT__) || defined(__APPLE__))
#  if defined(NeXT) || defined(__NeXT)
#    include <mach-o/dyld.h>
#    define environ (*environ_pointer)
EXT char *** environ_pointer;
#  else
#    if defined(__APPLE__)
#      include <crt_externs.h>	/* for the env array */
#      define environ (*_NSGetEnviron())
#    endif
#  endif
#else
   /* VMS and some other platforms don't use the environ array */
#  if !defined(VMS)
#    if !defined(DONT_DECLARE_STD) || \
        (defined(__svr4__) && defined(__GNUC__) && defined(sun)) || \
        defined(__sgi) || \
        defined(__DGUX) || defined(EPOC)
extern char **	environ;	/* environment variables supplied via exec */
#    endif
#  endif
#endif

START_EXTERN_C

/* handy constants */
EXTCONST char PL_warn_uninit[]
  INIT("Use of uninitialized value");
EXTCONST char PL_warn_nosemi[]
  INIT("Semicolon seems to be missing");
EXTCONST char PL_warn_reserved[]
  INIT("Unquoted string \"%s\" may clash with future reserved word");
EXTCONST char PL_warn_nl[]
  INIT("Unsuccessful %s on filename containing newline");
EXTCONST char PL_no_wrongref[]
  INIT("Can't use %s ref as %s ref");
EXTCONST char PL_no_symref[]
  INIT("Can't use string (\"%.32s\") as %s ref while \"strict refs\" in use");
EXTCONST char PL_no_usym[]
  INIT("Can't use an undefined value as %s reference");
EXTCONST char PL_no_aelem[]
  INIT("Modification of non-creatable array value attempted, subscript %d");
EXTCONST char PL_no_helem[]
  INIT("Modification of non-creatable hash value attempted, subscript \"%s\"");
EXTCONST char PL_no_modify[]
  INIT("Modification of a read-only value attempted");
EXTCONST char PL_no_mem[]
  INIT("Out of memory!\n");
EXTCONST char PL_no_security[]
  INIT("Insecure dependency in %s%s");
EXTCONST char PL_no_sock_func[]
  INIT("Unsupported socket function \"%s\" called");
EXTCONST char PL_no_dir_func[]
  INIT("Unsupported directory function \"%s\" called");
EXTCONST char PL_no_func[]
  INIT("The %s function is unimplemented");
EXTCONST char PL_no_myglob[]
  INIT("\"my\" variable %s can't be in a package");

EXTCONST char PL_uuemap[65]
  INIT("`!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");


#ifdef DOINIT
EXT char *PL_sig_name[] = { SIG_NAME };
EXT int   PL_sig_num[]  = { SIG_NUM };
EXT SV	* PL_psig_ptr[sizeof(PL_sig_num)/sizeof(*PL_sig_num)];
EXT SV  * PL_psig_name[sizeof(PL_sig_num)/sizeof(*PL_sig_num)];
#else
EXT char *PL_sig_name[];
EXT int   PL_sig_num[];
EXT SV  * PL_psig_ptr[];
EXT SV  * PL_psig_name[];
#endif

/* fast case folding tables */

#ifdef DOINIT
#ifdef EBCDIC
EXT unsigned char PL_fold[] = { /* fast EBCDIC case folding table */
    0,      1,      2,      3,      4,      5,      6,      7,
    8,      9,      10,     11,     12,     13,     14,     15,
    16,     17,     18,     19,     20,     21,     22,     23,
    24,     25,     26,     27,     28,     29,     30,     31,
    32,     33,     34,     35,     36,     37,     38,     39,
    40,     41,     42,     43,     44,     45,     46,     47,
    48,     49,     50,     51,     52,     53,     54,     55,
    56,     57,     58,     59,     60,     61,     62,     63,
    64,     65,     66,     67,     68,     69,     70,     71,
    72,     73,     74,     75,     76,     77,     78,     79,
    80,     81,     82,     83,     84,     85,     86,     87,
    88,     89,     90,     91,     92,     93,     94,     95,
    96,     97,     98,     99,     100,    101,    102,    103,
    104,    105,    106,    107,    108,    109,    110,    111,
    112,    113,    114,    115,    116,    117,    118,    119,
    120,    121,    122,    123,    124,    125,    126,    127,
    128,    'A',    'B',    'C',    'D',    'E',    'F',    'G',
    'H',    'I',    138,    139,    140,    141,    142,    143,
    144,    'J',    'K',    'L',    'M',    'N',    'O',    'P',
    'Q',    'R',    154,    155,    156,    157,    158,    159,
    160,    161,    'S',    'T',    'U',    'V',    'W',    'X',
    'Y',    'Z',    170,    171,    172,    173,    174,    175,
    176,    177,    178,    179,    180,    181,    182,    183,
    184,    185,    186,    187,    188,    189,    190,    191,
    192,    'a',    'b',    'c',    'd',    'e',    'f',    'g',
    'h',    'i',    202,    203,    204,    205,    206,    207,
    208,    'j',    'k',    'l',    'm',    'n',    'o',    'p',
    'q',    'r',    218,    219,    220,    221,    222,    223,
    224,    225,    's',    't',    'u',    'v',    'w',    'x',
    'y',    'z',    234,    235,    236,    237,    238,    239,
    240,    241,    242,    243,    244,    245,    246,    247,
    248,    249,    250,    251,    252,    253,    254,    255
};
#else   /* ascii rather than ebcdic */
EXTCONST  unsigned char PL_fold[] = {
	0,	1,	2,	3,	4,	5,	6,	7,
	8,	9,	10,	11,	12,	13,	14,	15,
	16,	17,	18,	19,	20,	21,	22,	23,
	24,	25,	26,	27,	28,	29,	30,	31,
	32,	33,	34,	35,	36,	37,	38,	39,
	40,	41,	42,	43,	44,	45,	46,	47,
	48,	49,	50,	51,	52,	53,	54,	55,
	56,	57,	58,	59,	60,	61,	62,	63,
	64,	'a',	'b',	'c',	'd',	'e',	'f',	'g',
	'h',	'i',	'j',	'k',	'l',	'm',	'n',	'o',
	'p',	'q',	'r',	's',	't',	'u',	'v',	'w',
	'x',	'y',	'z',	91,	92,	93,	94,	95,
	96,	'A',	'B',	'C',	'D',	'E',	'F',	'G',
	'H',	'I',	'J',	'K',	'L',	'M',	'N',	'O',
	'P',	'Q',	'R',	'S',	'T',	'U',	'V',	'W',
	'X',	'Y',	'Z',	123,	124,	125,	126,	127,
	128,	129,	130,	131,	132,	133,	134,	135,
	136,	137,	138,	139,	140,	141,	142,	143,
	144,	145,	146,	147,	148,	149,	150,	151,
	152,	153,	154,	155,	156,	157,	158,	159,
	160,	161,	162,	163,	164,	165,	166,	167,
	168,	169,	170,	171,	172,	173,	174,	175,
	176,	177,	178,	179,	180,	181,	182,	183,
	184,	185,	186,	187,	188,	189,	190,	191,
	192,	193,	194,	195,	196,	197,	198,	199,
	200,	201,	202,	203,	204,	205,	206,	207,
	208,	209,	210,	211,	212,	213,	214,	215,
	216,	217,	218,	219,	220,	221,	222,	223,	
	224,	225,	226,	227,	228,	229,	230,	231,
	232,	233,	234,	235,	236,	237,	238,	239,
	240,	241,	242,	243,	244,	245,	246,	247,
	248,	249,	250,	251,	252,	253,	254,	255
};
#endif  /* !EBCDIC */
#else
EXTCONST unsigned char PL_fold[];
#endif

#ifdef DOINIT
EXT unsigned char PL_fold_locale[] = {
	0,	1,	2,	3,	4,	5,	6,	7,
	8,	9,	10,	11,	12,	13,	14,	15,
	16,	17,	18,	19,	20,	21,	22,	23,
	24,	25,	26,	27,	28,	29,	30,	31,
	32,	33,	34,	35,	36,	37,	38,	39,
	40,	41,	42,	43,	44,	45,	46,	47,
	48,	49,	50,	51,	52,	53,	54,	55,
	56,	57,	58,	59,	60,	61,	62,	63,
	64,	'a',	'b',	'c',	'd',	'e',	'f',	'g',
	'h',	'i',	'j',	'k',	'l',	'm',	'n',	'o',
	'p',	'q',	'r',	's',	't',	'u',	'v',	'w',
	'x',	'y',	'z',	91,	92,	93,	94,	95,
	96,	'A',	'B',	'C',	'D',	'E',	'F',	'G',
	'H',	'I',	'J',	'K',	'L',	'M',	'N',	'O',
	'P',	'Q',	'R',	'S',	'T',	'U',	'V',	'W',
	'X',	'Y',	'Z',	123,	124,	125,	126,	127,
	128,	129,	130,	131,	132,	133,	134,	135,
	136,	137,	138,	139,	140,	141,	142,	143,
	144,	145,	146,	147,	148,	149,	150,	151,
	152,	153,	154,	155,	156,	157,	158,	159,
	160,	161,	162,	163,	164,	165,	166,	167,
	168,	169,	170,	171,	172,	173,	174,	175,
	176,	177,	178,	179,	180,	181,	182,	183,
	184,	185,	186,	187,	188,	189,	190,	191,
	192,	193,	194,	195,	196,	197,	198,	199,
	200,	201,	202,	203,	204,	205,	206,	207,
	208,	209,	210,	211,	212,	213,	214,	215,
	216,	217,	218,	219,	220,	221,	222,	223,	
	224,	225,	226,	227,	228,	229,	230,	231,
	232,	233,	234,	235,	236,	237,	238,	239,
	240,	241,	242,	243,	244,	245,	246,	247,
	248,	249,	250,	251,	252,	253,	254,	255
};
#else
EXT unsigned char PL_fold_locale[];
#endif

#ifdef DOINIT
#ifdef EBCDIC
EXT unsigned char PL_freq[] = {/* EBCDIC frequencies for mixed English/C */
    1,      2,      84,     151,    154,    155,    156,    157,
    165,    246,    250,    3,      158,    7,      18,     29,
    40,     51,     62,     73,     85,     96,     107,    118,
    129,    140,    147,    148,    149,    150,    152,    153,
    255,      6,      8,      9,     10,     11,     12,     13,
     14,     15,     24,     25,     26,     27,     28,    226,
     29,     30,     31,     32,     33,     43,     44,     45,
     46,     47,     48,     49,     50,     76,     77,     78,
     79,     80,     81,     82,     83,     84,     85,     86,
     87,     94,     95,    234,    181,    233,    187,    190,
    180,     96,     97,     98,     99,    100,    101,    102,
    104,    112,    182,    174,    236,    232,    229,    103,
    228,    226,    114,    115,    116,    117,    118,    119,
    120,    121,    122,    235,    176,    230,    194,    162,
    130,    131,    132,    133,    134,    135,    136,    137,
    138,    139,    201,    205,    163,    217,    220,    224,
    5,      248,    227,    244,    242,    255,    241,    231,
    240,    253,    16,     197,    19,     20,     21,     187,
    23,     169,    210,    245,    237,    249,    247,    239,
    168,    252,    34,     196,    36,     37,     38,     39,
    41,     42,     251,    254,    238,    223,    221,    213,
    225,    177,    52,     53,     54,     55,     56,     57,
    58,     59,     60,     61,     63,     64,     65,     66,
    67,     68,     69,     70,     71,     72,     74,     75,
    205,    208,    186,    202,    200,    218,    198,    179,
    178,    214,    88,     89,     90,     91,     92,     93,
    217,    166,    170,    207,    199,    209,    206,    204,
    160,    212,    105,    106,    108,    109,    110,    111,
    203,    113,    216,    215,    192,    175,    193,    243,
    172,    161,    123,    124,    125,    126,    127,    128,
    222,    219,    211,    195,    188,    193,    185,    184,
    191,    183,    141,    142,    143,    144,    145,    146
};
#else  /* ascii rather than ebcdic */
EXTCONST unsigned char PL_freq[] = {	/* letter frequencies for mixed English/C */
	1,	2,	84,	151,	154,	155,	156,	157,
	165,	246,	250,	3,	158,	7,	18,	29,
	40,	51,	62,	73,	85,	96,	107,	118,
	129,	140,	147,	148,	149,	150,	152,	153,
	255,	182,	224,	205,	174,	176,	180,	217,
	233,	232,	236,	187,	235,	228,	234,	226,
	222,	219,	211,	195,	188,	193,	185,	184,
	191,	183,	201,	229,	181,	220,	194,	162,
	163,	208,	186,	202,	200,	218,	198,	179,
	178,	214,	166,	170,	207,	199,	209,	206,
	204,	160,	212,	216,	215,	192,	175,	173,
	243,	172,	161,	190,	203,	189,	164,	230,
	167,	248,	227,	244,	242,	255,	241,	231,
	240,	253,	169,	210,	245,	237,	249,	247,
	239,	168,	252,	251,	254,	238,	223,	221,
	213,	225,	177,	197,	171,	196,	159,	4,
	5,	6,	8,	9,	10,	11,	12,	13,
	14,	15,	16,	17,	19,	20,	21,	22,
	23,	24,	25,	26,	27,	28,	30,	31,
	32,	33,	34,	35,	36,	37,	38,	39,
	41,	42,	43,	44,	45,	46,	47,	48,
	49,	50,	52,	53,	54,	55,	56,	57,
	58,	59,	60,	61,	63,	64,	65,	66,
	67,	68,	69,	70,	71,	72,	74,	75,
	76,	77,	78,	79,	80,	81,	82,	83,
	86,	87,	88,	89,	90,	91,	92,	93,
	94,	95,	97,	98,	99,	100,	101,	102,
	103,	104,	105,	106,	108,	109,	110,	111,
	112,	113,	114,	115,	116,	117,	119,	120,
	121,	122,	123,	124,	125,	126,	127,	128,
	130,	131,	132,	133,	134,	135,	136,	137,
	138,	139,	141,	142,	143,	144,	145,	146
};
#endif
#else
EXTCONST unsigned char PL_freq[];
#endif

#ifdef DEBUGGING
#ifdef DOINIT
EXTCONST char* PL_block_type[] = {
	"NULL",
	"SUB",
	"EVAL",
	"LOOP",
	"SUBST",
	"BLOCK",
};
#else
EXTCONST char* PL_block_type[];
#endif
#endif

END_EXTERN_C

/*****************************************************************************/
/* This lexer/parser stuff is currently global since yacc is hard to reenter */
/*****************************************************************************/
/* XXX This needs to be revisited, since BEGIN makes yacc re-enter... */

#include "perly.h"

#define LEX_NOTPARSING		11	/* borrowed from toke.c */

typedef enum {
    XOPERATOR,
    XTERM,
    XREF,
    XSTATE,
    XBLOCK,
    XTERMBLOCK
} expectation;

enum {		/* pass one of these to get_vtbl */
    want_vtbl_sv,
    want_vtbl_env,
    want_vtbl_envelem,
    want_vtbl_sig,
    want_vtbl_sigelem,
    want_vtbl_pack,
    want_vtbl_packelem,
    want_vtbl_dbline,
    want_vtbl_isa,
    want_vtbl_isaelem,
    want_vtbl_arylen,
    want_vtbl_glob,
    want_vtbl_mglob,
    want_vtbl_nkeys,
    want_vtbl_taint,
    want_vtbl_substr,
    want_vtbl_vec,
    want_vtbl_pos,
    want_vtbl_bm,
    want_vtbl_fm,
    want_vtbl_uvar,
    want_vtbl_defelem,
    want_vtbl_regexp,
    want_vtbl_collxfrm,
    want_vtbl_amagic,
    want_vtbl_amagicelem,
#ifdef USE_THREADS
    want_vtbl_mutex,
#endif
    want_vtbl_regdata,
    want_vtbl_regdatum,
    want_vtbl_backref
};

				/* Note: the lowest 8 bits are reserved for
				   stuffing into op->op_private */
#define HINT_PRIVATE_MASK	0x000000ff
#define HINT_INTEGER		0x00000001
#define HINT_STRICT_REFS	0x00000002
/* #define HINT_notused4	0x00000004 */
#define HINT_UTF8		0x00000008
/* #define HINT_notused10	0x00000010 */
				/* Note: 20,40,80 used for NATIVE_HINTS */

#define HINT_BLOCK_SCOPE	0x00000100
#define HINT_STRICT_SUBS	0x00000200
#define HINT_STRICT_VARS	0x00000400
#define HINT_LOCALE		0x00000800

#define HINT_NEW_INTEGER	0x00001000
#define HINT_NEW_FLOAT		0x00002000
#define HINT_NEW_BINARY		0x00004000
#define HINT_NEW_STRING		0x00008000
#define HINT_NEW_RE		0x00010000
#define HINT_LOCALIZE_HH	0x00020000 /* %^H needs to be copied */

#define HINT_RE_TAINT		0x00100000
#define HINT_RE_EVAL		0x00200000

#define HINT_FILETEST_ACCESS	0x00400000

/* Various states of an input record separator SV (rs, nrs) */
#define RsSNARF(sv)   (! SvOK(sv))
#define RsSIMPLE(sv)  (SvOK(sv) && (! SvPOK(sv) || SvCUR(sv)))
#define RsPARA(sv)    (SvPOK(sv) && ! SvCUR(sv))
#define RsRECORD(sv)  (SvROK(sv) && (SvIV(SvRV(sv)) > 0))

/* Enable variables which are pointers to functions */
typedef regexp*(CPERLscope(*regcomp_t)) (pTHX_ char* exp, char* xend, PMOP* pm);
typedef I32 (CPERLscope(*regexec_t)) (pTHX_ regexp* prog, char* stringarg,
				      char* strend, char* strbeg, I32 minend,
				      SV* screamer, void* data, U32 flags);
typedef char* (CPERLscope(*re_intuit_start_t)) (pTHX_ regexp *prog, SV *sv,
						char *strpos, char *strend,
						U32 flags,
						struct re_scream_pos_data_s *d);
typedef SV*	(CPERLscope(*re_intuit_string_t)) (pTHX_ regexp *prog);
typedef void	(CPERLscope(*regfree_t)) (pTHX_ struct regexp* r);

#ifdef USE_PURE_BISON
int Perl_yylex(pTHX_ YYSTYPE *lvalp, int *lcharp);
#endif

typedef void (*DESTRUCTORFUNC_t) (pTHXo_ void*);
typedef void (*SVFUNC_t) (pTHXo_ SV*);
typedef I32  (*SVCOMPARE_t) (pTHXo_ SV*, SV*);
typedef void (*XSINIT_t) (pTHXo);
typedef void (*ATEXIT_t) (pTHXo_ void*);
typedef void (*XSUBADDR_t) (pTHXo_ CV *);

/* Set up PERLVAR macros for populating structs */
#define PERLVAR(var,type) type var;
#define PERLVARA(var,n,type) type var[n];
#define PERLVARI(var,type,init) type var;
#define PERLVARIC(var,type,init) type var;

/* Interpreter exitlist entry */
typedef struct exitlistentry {
    void (*fn) (pTHXo_ void*);
    void *ptr;
} PerlExitListEntry;

#ifdef PERL_OBJECT
#undef perl_alloc
#define perl_alloc Perl_alloc
CPerlObj* Perl_alloc (IPerlMem*, IPerlEnv*, IPerlStdIO*, IPerlLIO*, IPerlDir*, IPerlSock*, IPerlProc*);

#undef EXT
#define EXT
#undef EXTCONST
#define EXTCONST
#undef INIT
#define INIT(x)

class CPerlObj {
public:
	CPerlObj(IPerlMem*, IPerlEnv*, IPerlStdIO*, IPerlLIO*, IPerlDir*, IPerlSock*, IPerlProc*);
	void Init(void);
	void* operator new(size_t nSize, IPerlMem *pvtbl);
	static void operator delete(void* pPerl, IPerlMem *pvtbl);
#endif /* PERL_OBJECT */

#ifdef PERL_GLOBAL_STRUCT
struct perl_vars {
#include "perlvars.h"
};

#ifdef PERL_CORE
EXT struct perl_vars PL_Vars;
EXT struct perl_vars *PL_VarsPtr INIT(&PL_Vars);
#else /* PERL_CORE */
#if !defined(__GNUC__) || !(defined(WIN32) || defined(CYGWIN))
EXT
#endif /* WIN32 */
struct perl_vars *PL_VarsPtr;
#define PL_Vars (*((PL_VarsPtr) ? PL_VarsPtr : (PL_VarsPtr =  Perl_GetVars())))
#endif /* PERL_CORE */
#endif /* PERL_GLOBAL_STRUCT */

#ifdef MULTIPLICITY
/* If we have multiple interpreters define a struct 
   holding variables which must be per-interpreter
   If we don't have threads anything that would have 
   be per-thread is per-interpreter.
*/

struct interpreter {
#include "thrdvar.h"
#include "intrpvar.h"
};

#else
struct interpreter {
    char broiled;
};
#endif

#ifdef USE_THREADS
/* If we have threads define a struct with all the variables
 * that have to be per-thread
 */


struct perl_thread {
#include "thrdvar.h"
};

typedef struct perl_thread *Thread;

#else
typedef void *Thread;
#endif

/* Done with PERLVAR macros for now ... */
#undef PERLVAR
#undef PERLVARA
#undef PERLVARI
#undef PERLVARIC

#include "thread.h"
#include "pp.h"

#ifndef PERL_CALLCONV
#  define PERL_CALLCONV
#endif 

#ifdef PERL_OBJECT
#  define VIRTUAL virtual PERL_CALLCONV
#else
#  define VIRTUAL PERL_CALLCONV
/*START_EXTERN_C*/
#endif

#ifndef NEXT30_NO_ATTRIBUTE
#  ifndef HASATTRIBUTE       /* disable GNU-cc attribute checking? */
#    ifdef  __attribute__      /* Avoid possible redefinition errors */
#      undef  __attribute__
#    endif
#    define __attribute__(attr)
#  endif
#endif

#ifdef PERL_OBJECT
#define PERL_DECL_PROT
#define perl_alloc Perl_alloc
#endif

#include "proto.h"

#undef PERL_CKDEF
#undef PERL_PPDEF
#define PERL_CKDEF(s)	OP *s (pTHX_ OP *o);
#define PERL_PPDEF(s)	OP *s (pTHX);
#ifdef PERL_OBJECT
public:
#endif

#include "pp_proto.h"

#ifdef PERL_OBJECT
VIRTUAL int CPerlObj::do_aspawn (void *vreally, void **vmark, void **vsp);
#undef PERL_DECL_PROT
#else
/*END_EXTERN_C*/
#endif

#ifndef PERL_OBJECT
/* this has structure inits, so it cannot be included before here */
#  include "opcode.h"
#endif

/* The following must follow proto.h as #defines mess up syntax */

#if !defined(PERL_FOR_X2P)
#  include "embedvar.h"
#endif

/* Now include all the 'global' variables 
 * If we don't have threads or multiple interpreters
 * these include variables that would have been their struct-s 
 */
                         
#define PERLVAR(var,type) EXT type PL_##var;
#define PERLVARA(var,n,type) EXT type PL_##var[n];
#define PERLVARI(var,type,init) EXT type  PL_##var INIT(init);
#define PERLVARIC(var,type,init) EXTCONST type PL_##var INIT(init);

#ifndef MULTIPLICITY

#  include "intrpvar.h"
#  ifndef USE_THREADS
#    include "thrdvar.h"
#  endif

#endif

#ifdef PERL_OBJECT
/*
 * The following is a buffer where new variables must
 * be defined to maintain binary compatibility with PERL_OBJECT
 * for 5.005
 */
PERLVARA(object_compatibility,30,	char)
};


#  include "embed.h"
#  if defined(WIN32) && !defined(WIN32IO_IS_STDIO)
#    define errno	CPerlObj::ErrorNo()
#  endif

#  ifdef DOINIT
#    include "INTERN.h"
#  else
#    include "EXTERN.h"
#  endif

/* this has structure inits, so it cannot be included before here */
#  include "opcode.h"

#endif  /* PERL_OBJECT */

#ifndef PERL_GLOBAL_STRUCT
START_EXTERN_C

#  include "perlvars.h"

END_EXTERN_C
#endif

#undef PERLVAR
#undef PERLVARA
#undef PERLVARI
#undef PERLVARIC

START_EXTERN_C

#ifdef DOINIT

EXT MGVTBL PL_vtbl_sv =	{Perl_magic_get,
				Perl_magic_set,
					Perl_magic_len,
						0,	0};
EXT MGVTBL PL_vtbl_env =	{0,	Perl_magic_set_all_env,
				0,	Perl_magic_clear_all_env,
							0};
EXT MGVTBL PL_vtbl_envelem =	{0,	Perl_magic_setenv,
					0,	Perl_magic_clearenv,
							0};
EXT MGVTBL PL_vtbl_sig =	{0,	0,		 0, 0, 0};
EXT MGVTBL PL_vtbl_sigelem =	{Perl_magic_getsig,
					Perl_magic_setsig,
					0,	Perl_magic_clearsig,
							0};
EXT MGVTBL PL_vtbl_pack =	{0,	0,	Perl_magic_sizepack,	Perl_magic_wipepack,
							0};
EXT MGVTBL PL_vtbl_packelem =	{Perl_magic_getpack,
				Perl_magic_setpack,
					0,	Perl_magic_clearpack,
							0};
EXT MGVTBL PL_vtbl_dbline =	{0,	Perl_magic_setdbline,
					0,	0,	0};
EXT MGVTBL PL_vtbl_isa =	{0,	Perl_magic_setisa,
					0,	Perl_magic_setisa,
							0};
EXT MGVTBL PL_vtbl_isaelem =	{0,	Perl_magic_setisa,
					0,	0,	0};
EXT MGVTBL PL_vtbl_arylen =	{Perl_magic_getarylen,
				Perl_magic_setarylen,
					0,	0,	0};
EXT MGVTBL PL_vtbl_glob =	{Perl_magic_getglob,
				Perl_magic_setglob,
					0,	0,	0};
EXT MGVTBL PL_vtbl_mglob =	{0,	Perl_magic_setmglob,
					0,	0,	0};
EXT MGVTBL PL_vtbl_nkeys =	{Perl_magic_getnkeys,
				Perl_magic_setnkeys,
					0,	0,	0};
EXT MGVTBL PL_vtbl_taint =	{Perl_magic_gettaint,Perl_magic_settaint,
					0,	0,	0};
EXT MGVTBL PL_vtbl_substr =	{Perl_magic_getsubstr, Perl_magic_setsubstr,
					0,	0,	0};
EXT MGVTBL PL_vtbl_vec =	{Perl_magic_getvec,
				Perl_magic_setvec,
					0,	0,	0};
EXT MGVTBL PL_vtbl_pos =	{Perl_magic_getpos,
				Perl_magic_setpos,
					0,	0,	0};
EXT MGVTBL PL_vtbl_bm =	{0,	Perl_magic_setbm,
					0,	0,	0};
EXT MGVTBL PL_vtbl_fm =	{0,	Perl_magic_setfm,
					0,	0,	0};
EXT MGVTBL PL_vtbl_uvar =	{Perl_magic_getuvar,
				Perl_magic_setuvar,
					0,	0,	0};
#ifdef USE_THREADS
EXT MGVTBL PL_vtbl_mutex =	{0,	0,	0,	0,	Perl_magic_mutexfree};
#endif /* USE_THREADS */
EXT MGVTBL PL_vtbl_defelem = {Perl_magic_getdefelem,Perl_magic_setdefelem,
					0,	0,	0};

EXT MGVTBL PL_vtbl_regexp = {0,0,0,0, Perl_magic_freeregexp};
EXT MGVTBL PL_vtbl_regdata = {0, 0, Perl_magic_regdata_cnt, 0, 0};
EXT MGVTBL PL_vtbl_regdatum = {Perl_magic_regdatum_get, 0, 0, 0, 0};

#ifdef USE_LOCALE_COLLATE
EXT MGVTBL PL_vtbl_collxfrm = {0,
				Perl_magic_setcollxfrm,
					0,	0,	0};
#endif

EXT MGVTBL PL_vtbl_amagic =       {0,     Perl_magic_setamagic,
                                        0,      0,      Perl_magic_setamagic};
EXT MGVTBL PL_vtbl_amagicelem =   {0,     Perl_magic_setamagic,
                                        0,      0,      Perl_magic_setamagic};

EXT MGVTBL PL_vtbl_backref = 	  {0,	0,
					0,	0,	Perl_magic_killbackrefs};

#else /* !DOINIT */

EXT MGVTBL PL_vtbl_sv;
EXT MGVTBL PL_vtbl_env;
EXT MGVTBL PL_vtbl_envelem;
EXT MGVTBL PL_vtbl_sig;
EXT MGVTBL PL_vtbl_sigelem;
EXT MGVTBL PL_vtbl_pack;
EXT MGVTBL PL_vtbl_packelem;
EXT MGVTBL PL_vtbl_dbline;
EXT MGVTBL PL_vtbl_isa;
EXT MGVTBL PL_vtbl_isaelem;
EXT MGVTBL PL_vtbl_arylen;
EXT MGVTBL PL_vtbl_glob;
EXT MGVTBL PL_vtbl_mglob;
EXT MGVTBL PL_vtbl_nkeys;
EXT MGVTBL PL_vtbl_taint;
EXT MGVTBL PL_vtbl_substr;
EXT MGVTBL PL_vtbl_vec;
EXT MGVTBL PL_vtbl_pos;
EXT MGVTBL PL_vtbl_bm;
EXT MGVTBL PL_vtbl_fm;
EXT MGVTBL PL_vtbl_uvar;

#ifdef USE_THREADS
EXT MGVTBL PL_vtbl_mutex;
#endif /* USE_THREADS */

EXT MGVTBL PL_vtbl_defelem;
EXT MGVTBL PL_vtbl_regexp;
EXT MGVTBL PL_vtbl_regdata;
EXT MGVTBL PL_vtbl_regdatum;

#ifdef USE_LOCALE_COLLATE
EXT MGVTBL PL_vtbl_collxfrm;
#endif

EXT MGVTBL PL_vtbl_amagic;
EXT MGVTBL PL_vtbl_amagicelem;

EXT MGVTBL PL_vtbl_backref;

#endif /* !DOINIT */

enum {
  fallback_amg,        abs_amg,
  bool__amg,   nomethod_amg,
  string_amg,  numer_amg,
  add_amg,     add_ass_amg,
  subtr_amg,   subtr_ass_amg,
  mult_amg,    mult_ass_amg,
  div_amg,     div_ass_amg,
  modulo_amg,  modulo_ass_amg,
  pow_amg,     pow_ass_amg,
  lshift_amg,  lshift_ass_amg,
  rshift_amg,  rshift_ass_amg,
  band_amg,    band_ass_amg,
  bor_amg,     bor_ass_amg,
  bxor_amg,    bxor_ass_amg,
  lt_amg,      le_amg,
  gt_amg,      ge_amg,
  eq_amg,      ne_amg,
  ncmp_amg,    scmp_amg,
  slt_amg,     sle_amg,
  sgt_amg,     sge_amg,
  seq_amg,     sne_amg,
  not_amg,     compl_amg,
  inc_amg,     dec_amg,
  atan2_amg,   cos_amg,
  sin_amg,     exp_amg,
  log_amg,     sqrt_amg,
  repeat_amg,   repeat_ass_amg,
  concat_amg,  concat_ass_amg,
  copy_amg,    neg_amg,
  to_sv_amg,   to_av_amg,
  to_hv_amg,   to_gv_amg,
  to_cv_amg,   iter_amg,    
  max_amg_code
  /* Do not leave a trailing comma here.  C9X allows it, C89 doesn't. */
};

#define NofAMmeth max_amg_code

#ifdef DOINIT
EXTCONST char * PL_AMG_names[NofAMmeth] = {
  "fallback",	"abs",			/* "fallback" should be the first. */
  "bool",	"nomethod",
  "\"\"",	"0+",
  "+",		"+=",
  "-",		"-=",
  "*",		"*=",
  "/",		"/=",
  "%",		"%=",
  "**",		"**=",
  "<<",		"<<=",
  ">>",		">>=",
  "&",		"&=",
  "|",		"|=",
  "^",		"^=",
  "<",		"<=",
  ">",		">=",
  "==",		"!=",
  "<=>",	"cmp",
  "lt",		"le",
  "gt",		"ge",
  "eq",		"ne",
  "!",		"~",
  "++",		"--",
  "atan2",	"cos",
  "sin",	"exp",
  "log",	"sqrt",
  "x",		"x=",
  ".",		".=",
  "=",		"neg",
  "${}",	"@{}",
  "%{}",	"*{}",
  "&{}",	"<>",
};
#else
EXTCONST char * PL_AMG_names[NofAMmeth];
#endif /* def INITAMAGIC */

END_EXTERN_C

struct am_table {
  long was_ok_sub;
  long was_ok_am;
  U32 flags;
  CV* table[NofAMmeth];
  long fallback;
};
struct am_table_short {
  long was_ok_sub;
  long was_ok_am;
  U32 flags;
};
typedef struct am_table AMT;
typedef struct am_table_short AMTS;

#define AMGfallNEVER	1
#define AMGfallNO	2
#define AMGfallYES	3

#define AMTf_AMAGIC		1
#define AMT_AMAGIC(amt)		((amt)->flags & AMTf_AMAGIC)
#define AMT_AMAGIC_on(amt)	((amt)->flags |= AMTf_AMAGIC)
#define AMT_AMAGIC_off(amt)	((amt)->flags &= ~AMTf_AMAGIC)


/*
 * some compilers like to redefine cos et alia as faster
 * (and less accurate?) versions called F_cos et cetera (Quidquid
 * latine dictum sit, altum viditur.)  This trick collides with
 * the Perl overloading (amg).  The following #defines fool both.
 */

#ifdef _FASTMATH
#   ifdef atan2
#       define F_atan2_amg  atan2_amg
#   endif
#   ifdef cos
#       define F_cos_amg    cos_amg
#   endif
#   ifdef exp
#       define F_exp_amg    exp_amg
#   endif
#   ifdef log
#       define F_log_amg    log_amg
#   endif
#   ifdef pow
#       define F_pow_amg    pow_amg
#   endif
#   ifdef sin
#       define F_sin_amg    sin_amg
#   endif
#   ifdef sqrt
#       define F_sqrt_amg   sqrt_amg
#   endif
#endif /* _FASTMATH */

#define PERLDB_ALL	0x3f		/* No _NONAME, _GOTO */
#define PERLDBf_SUB	0x01		/* Debug sub enter/exit. */
#define PERLDBf_LINE	0x02		/* Keep line #. */
#define PERLDBf_NOOPT	0x04		/* Switch off optimizations. */
#define PERLDBf_INTER	0x08		/* Preserve more data for
					   later inspections.  */
#define PERLDBf_SUBLINE	0x10		/* Keep subr source lines. */
#define PERLDBf_SINGLE	0x20		/* Start with single-step on. */
#define PERLDBf_NONAME	0x40		/* For _SUB: no name of the subr. */
#define PERLDBf_GOTO	0x80		/* Report goto: call DB::goto. */

#define PERLDB_SUB	(PL_perldb && (PL_perldb & PERLDBf_SUB))
#define PERLDB_LINE	(PL_perldb && (PL_perldb & PERLDBf_LINE))
#define PERLDB_NOOPT	(PL_perldb && (PL_perldb & PERLDBf_NOOPT))
#define PERLDB_INTER	(PL_perldb && (PL_perldb & PERLDBf_INTER))
#define PERLDB_SUBLINE	(PL_perldb && (PL_perldb & PERLDBf_SUBLINE))
#define PERLDB_SINGLE	(PL_perldb && (PL_perldb & PERLDBf_SINGLE))
#define PERLDB_SUB_NN	(PL_perldb && (PL_perldb & (PERLDBf_NONAME)))
#define PERLDB_GOTO	(PL_perldb && (PL_perldb & PERLDBf_GOTO))


#ifdef USE_LOCALE_NUMERIC

#define SET_NUMERIC_STANDARD() \
    STMT_START {				\
	if (! PL_numeric_standard)		\
	    set_numeric_standard();		\
    } STMT_END

#define SET_NUMERIC_LOCAL() \
    STMT_START {				\
	if (! PL_numeric_local)			\
	    set_numeric_local();		\
    } STMT_END

#define IS_NUMERIC_RADIX(c)	\
	((PL_hints & HINT_LOCALE) && \
	  PL_numeric_radix && (c) == PL_numeric_radix)

#define RESTORE_NUMERIC_LOCAL()		if ((PL_hints & HINT_LOCALE) && PL_numeric_standard) SET_NUMERIC_LOCAL()
#define RESTORE_NUMERIC_STANDARD()	if ((PL_hints & HINT_LOCALE) && PL_numeric_local) SET_NUMERIC_STANDARD()
#define Atof				my_atof

#else /* !USE_LOCALE_NUMERIC */

#define SET_NUMERIC_STANDARD()  	/**/
#define SET_NUMERIC_LOCAL()     	/**/
#define IS_NUMERIC_RADIX(c)		(0)
#define RESTORE_NUMERIC_LOCAL()		/**/
#define RESTORE_NUMERIC_STANDARD()	/**/
#define Atof				Perl_atof

#endif /* !USE_LOCALE_NUMERIC */

#if defined(USE_LONG_LONG) && defined(HAS_LONG_LONG) && defined(HAS_ATOLL)
#define Atol atoll 
#else
#define Atol atol
#endif

#if defined(USE_LONG_LONG) && defined(HAS_LONG_LONG) && defined(HAS_STRTOULL)
#define Strtoul strtoull
#else
#define Strtoul strtoul
#endif

#if !defined(PERLIO_IS_STDIO) && defined(HASATTRIBUTE)
/* 
 * Now we have __attribute__ out of the way 
 * Remap printf 
 */
#define printf PerlIO_stdoutf
#endif

#ifndef PERL_SCRIPT_MODE
#define PERL_SCRIPT_MODE "r"
#endif

/*
 * nice_chunk and nice_chunk size need to be set
 * and queried under the protection of sv_mutex
 */
#define offer_nice_chunk(chunk, chunk_size) do {	\
	LOCK_SV_MUTEX;					\
	if (!PL_nice_chunk) {				\
	    PL_nice_chunk = (char*)(chunk);		\
	    PL_nice_chunk_size = (chunk_size);		\
	}						\
	else {						\
	    Safefree(chunk);				\
	}						\
	UNLOCK_SV_MUTEX;				\
    } while (0)

#ifdef HAS_SEM
#   include <sys/ipc.h>
#   include <sys/sem.h>
#   ifndef HAS_UNION_SEMUN	/* Provide the union semun. */
    union semun {
	int val;
	struct semid_ds *buf;
	unsigned short *array;
    };
#   endif
#   ifdef USE_SEMCTL_SEMUN
#       define Semctl(id, num, cmd, semun) semctl(id, num, cmd, semun)
#   else
#       ifdef USE_SEMCTL_SEMID_DS
#           define Semctl(id, num, cmd, semun) semctl(id, num, cmd, semun.buf)
#       endif
#   endif
#endif

/* Mention

   INSTALL_USR_BIN_PERL

   I_SYS_MMAN
   HAS_MMAP
   HAS_MUNMAP
   HAS_MPROTECT
   HAS_MSYNC
   HAS_MADVISE
   Mmap_t

   here so that Configure picks them up. */

#ifdef IAMSUID

#ifdef I_SYS_STATVFS
#   include <sys/statvfs.h>     /* for f?statvfs() */
#endif
#ifdef I_SYS_MOUNT
#   include <sys/mount.h>       /* for *BSD f?statfs() */
#endif
#ifdef I_MNTENT
#   include <mntent.h>          /* for getmntent() */
#endif

#endif /* IAMSUID */

/* and finally... */
#define PERL_PATCHLEVEL_H_IMPLICIT
#include "patchlevel.h"
#undef PERL_PATCHLEVEL_H_IMPLICIT

#endif /* Include guard */
