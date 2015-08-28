use super::grammar;

#[test]
fn simple() {
    let specs = vec!["struct foo { int bar; unsigned int blat; hyper foo; unsigned hyper hyperfoo; };",
                     "const blop = 123;",
                     "typedef opaque Ioaddr<>;"
                     ];

    for sp in specs {
        let s = grammar::specification(sp);
        println!("spec sp \"{}\" => {:?}", sp, s);
        assert!(s.is_ok())
    }
}

#[test]
fn rfc4506() {
    let s = grammar::specification(r#"

         const MAXUSERNAME = 32;     /* max length of a user name */
         const MAXFILELEN = 65535;   /* max length of a file      */
         const MAXNAMELEN = 255;     /* max length of a file name */

         /*
          * Types of files:
          */
         enum filekind {
            TEXT = 0,       /* ascii data */
            DATA = 1,       /* raw data   */
            EXEC = 2        /* executable */
         };

         /*
          * File information, per kind of file:
          */
         union filetype switch (filekind kind) {
         case TEXT:
            void;                           /* no extra information */
         case DATA:
            string creator<MAXNAMELEN>;     /* data creator         */
         case EXEC:
            string interpretor<MAXNAMELEN>; /* program interpretor  */
         };

         /*
          * A complete file:
          */
         struct file {
            string filename<MAXNAMELEN>; /* name of file    */
            filetype type;               /* info about file */
            string owner<MAXUSERNAME>;   /* owner of file   */
            opaque data<MAXFILELEN>;     /* file data       */
         };
"#);
    println!("spec {:?}", s);
    assert!(s.is_ok())
}
    
#[test]
fn large() {
    let s = grammar::specification(r#"
/* -*- c -*- */
%/*
% * Copyright 2011-2014  Exablox Corporation
% */
%/**
% * @file stormsg.x
% *
% * WARNING: THIS IS A COMPAT WITH "DMSG" SMASHFS FOR THE TIME BEING
% *          TO CONVERT THE CODE AND MAINTAIN API COMPAT FOR NOW
% *
% * Message definitions for distributed object store
% */

const STORMSG_MAJOR = 0;
const STORMSG_MINOR = 951;
#ifdef RPC_HDR
%
%#define DMSG_MAJOR	STORMSG_MAJOR
%#define DMSG_MINOR	STORMSG_MINOR
%
#endif

/* Enumerate the message operations */
enum Stormsg_e
{
	STORMSG_DETACHED	= 0,

	/*
	 * Requests: "(T)ransmit", AKA T-Messages
	 */
	STORMSG_THELLO		= 1,

	STORMSG_TGETOBJ		= 10,
	STORMSG_TPUTOBJ		= 11,
	STORMSG_TDELOBJ		= 12,
	STORMSG_TMARKOBJ	= 13,

	/* DEPRECATED */
	DMSG_THELLO		= 20,
	DMSG_TPING		= 21,
	DMSG_TBDBSTATS		= 22,
	DMSG_TBDBFLUSH		= 23,

	/* can extend without renumbering */

	/*
	 * Responses: (R)eply, AKA R-Messages
	 */
	STORMSG_RHELLO		= 101,

	STORMSG_RGETOBJ		= 110,
	STORMSG_RPUTOBJ		= 111,
	STORMSG_RDELOBJ		= 112,
	STORMSG_RMARKOBJ	= 113,

	/* DEPRECATED */
	DMSG_RHELLO		= 120,
	DMSG_RPING		= 121,
	DMSG_RBDBSTATS		= 122,
	DMSG_RBDBFLUSH		= 123,
	DMSG_RERROR		= 124

	/* can extend without renumbering... */

};
#ifdef RPC_HDR
%
%#define STORMSG_MAX	201
%
#endif

struct Wave
{
	u64	_wave;
};
#ifdef RPC_HDR
%
%#define WAVE_NULL	((Wave) { ._wave = 0 })
%#define WAVE_MAX	((Wave) { ._wave = ~0ull })
%
#endif

enum Objclass_e
{
	OBJCLASS_E_DATA	= 0,	/* has no references to other objects */
	OBJCLASS_E_META	= 1	/* has references to other objects */
};

struct Objclass
{
	Objclass_e	_class;
};
#ifdef RPC_HDR
%
%#define OBJCLASS_DATA  ((Objclass) { ._class = OBJCLASS_E_DATA })
%#define OBJCLASS_META  ((Objclass) { ._class = OBJCLASS_E_META })
%
#endif

%
%/* Transmitted messages that will get a response from the other end
% * of the communcation channel
% */

%
%/* Hello Request and Response payload - establish a control channel with other cluster nodes
% * responses: RHello or Detach
% */

%
struct THello
{
	uint	major;		/* protocol version - see STORMSG_MAJOR */
	uint	minor;		/* protocol version - see STORMSG_MINOR */
	Smash	bdb;		/* Physical storage id in HASH namespace */
};

struct Virtinfo
{
	Smash	svid;		/* Seed hash for the virtual id(s) */
	int	nvid;		/* Number of virtual id(s) to generate */
};

enum RHello_e
{
	RHELLO_SUCCESS = 0,
	RHELLO_ERROR = -1
};

union RHello switch(RHello_e status)
{
case RHELLO_SUCCESS:
	Virtinfo	vinfo;

case RHELLO_ERROR:
	Err	e;
};

%
%/* Retrieve the object contents identified
% * responses: RGetobj or Detach
% */
struct TGetobj
{
	Smash		bdb;
	Objclass	objclass;
	Smash		objhash;
};

enum RGetobj_e
{
	RGETOBJ_SUCCESS = 0,
	RGETOBJ_ERROR = -1
};

union RGetobj switch(RGetobj_e status)
{
case RGETOBJ_SUCCESS:
	Iobufaddr	baddr;

case RGETOBJ_ERROR:
	Err		e;
};

%
%/* Push Object
% * responses: RPutobj or Detach
% */
struct TPutobj
{
	Smash		bdb;
	Objclass	objclass;	/* object class	*/
	Smash		objhash;	/* hash of data being put */
	Wave		mark;		/* initial mark for object */
	Wave		sgmark;		/* initial subgraph mark for object - WAVE_NULL if unknown */
	Iobufaddr	baddr;		/* object content */
	bool		placement;	/* object is being placed - not user created */
};

enum RPutobj_e
{
	RPUTOBJ_SUCCESS = 0,
	RPUTOBJ_ERROR = -1
};

union RPutobj switch(RPutobj_e status)
{
case RPUTOBJ_SUCCESS:
	void;

case RPUTOBJ_ERROR:
	Err	e;
};

%
%/* Remove an object
% * responses: RDelobj or Detach
% */
struct TDelobj
{
	Smash		bdb;
	Objclass	objclass;
	Smash		objhash;
};

enum RDelobj_e
{
	RDELOBJ_SUCCESS = 0,
	RDELOBJ_ERROR = -1
};

union RDelobj switch(RDelobj_e status)
{
case RDELOBJ_SUCCESS:
	void;

case RDELOBJ_ERROR:
	Err	e;
};

%
%/*
% * A marking operation
% *
% * Marking operations can be grouped and applied atomically to a given object by a bdb. The
% * application of the mark by the bdb is required to maintain invariants on the object locally:
% *  - mark always monotonically increases
% *  - mark is always >= current wave
% *
% * Because the bdb and the caller can have a different value for "current wave" (one or the other -
% * or both - could be out of date), then its not necessarily an error to try and set a mark with too
% * small a value, and its always correct to mark an object with a "too-high" mark, so the bdb can
% * adjust the mark in order to maintain its invariants.
% */
enum Markop_e
{
	MARKOP_NOP		= 0,	/* Do nothing - placeholder in ops array */

	/* Get operations make no changes and can only fail if the object doesn't exist */
	MARKOP_GET_MARK		= 1,	/* get object's mark */
	MARKOP_GET_SUBGRAPH	= 2,	/* get object's subgraph */

	/* Set operations, subject to invariants */
	MARKOP_SET_MARK		= 3,	/* set object's mark directly, returns resulting wave */
	MARKOP_SET_MARK_GEOM	= 4,	/* set object's mark with geometric doubling (wave ignored), returns resulting wave */
	MARKOP_SET_MARK_OFFSET	= 5,	/* set object's mark with offset from current, returns resulting wave */

	/* Subgraph marks are for the marker's use, and so have no bdb invariants */
	MARKOP_SET_SUBGRAPH	= 6,	/* set object's subgraph mark (unconditional), returns resulting wave */

	MARKOP_SET_MARK_REBASE	= 7,	/* rebase object's mark to be wave (if wave > mark),
					   updating curwave by the same amount. sgmark unchanged */
	MARKOP_GET_PLACEMENT	= 8
};

struct Markop
{
	Markop_e	op;	/* operation to be performed */
	Wave		wave;	/* operation parameter, if needed */
};

enum Markres_e
{
	MARKRES_OK	= 1,	/* general success */
	MARKRES_WAVE	= 2,	/* result is a comparison */
	MARKRES_CHECK	= 3,	/* result indicates object placement/replica issue */
	MARKRES_ERR	= -1	/* result is an error */
};

struct Markchk
{
	bool	copies;		/* true if object has enough copies */
	bool	placement;	/* true if object is correctly placed */
};

/* Operation result */
union Markres switch(Markres_e res)
{
case MARKRES_OK:
	void;			/* Happy families are all alike; */
case MARKRES_WAVE:
	Wave	wave;
case MARKRES_CHECK:
	Markchk	check;
case MARKRES_ERR:
	Err	err;		/* every unhappy family is unhappy in its own way. */
};

%
%/*
% * Test or set marks on an object. All mark operations are applied atomically; if any fail, then no
% * changes are made. Tests are performed in the order they appear within the sequence of operations,
% * and return a result as if the change had been made.
% *
% * responses: RMarkobj or Detach
% */
struct TMarkobj
{
	Smash		bdb;		/* target bdb */
	Objclass	objclass;	/* object class */
	Smash		objhash;	/* object being marked */
	Wave		curwave;	/* current wave at mark time */
	Markop		markops<>;	/* mark operations */
};

enum RMarkobj_e
{
	RMARKOBJ_SUCCESS = 0,
	RMARKOBJ_ERROR = -1
};

union RMarkobj switch(RMarkobj_e status)
{
case RMARKOBJ_SUCCESS:
	Markres		markres<>;	/* a mark result per operation (see markops) */

case RMARKOBJ_ERROR:
	Err		e;		/* failed for this bdb */
};


%
%/* DEPRECATED DMSG PROTOS - BEING CONVERTED TO NEW PROTO */
%

%
%/* Hello Request and Response payload - establish a control channel with other cluster nodes
% * responses: RHello or Detach
% */

%
struct TRhello
{
	int		major;		/* protocol version - see DMSG_MAJOR */
	int		minor;		/* protocol version - see DMSG_MINOR */
	Smash		nodeid;		/* physical node id in HASH namespace */
	Smash		ringid;		/* my ring id */
	u32		type;		/* type of connection (see DCTYPE_ enum) */
	Virtinfo	vinfo<>;	/* Virtual id information */
};

%
%/* Ping Request
% * responses: RPing or RDetached
% */

%
%/* Get the stats for the specific bdb
% * responses: RBdbstats or Detach
% */
struct TBdbstats
{
	Smash	bdb;
};

enum Bdbblkstats
{
	/*
	 * Bstor statistic types
	 */
	BS_NEWDBLK	= 0,
	BS_NEWDBYTES,
	BS_NEWMBLK,
	BS_NEWMBYTES,
	BS_DUPDBLK,
	BS_DUPDBYTES,
	BS_DUPMBLK,
	BS_DUPMBYTES,
	BS_REBALBLK,
	BS_REBALBYTES,
	BS_UPDATE,

	BS_MNEW,
	BS_MREPLACE,
	BS_MNOOP,
	BS_NSTAT
};

const	BDB_NAMELEN = 256;	/* sized for path element max len (NAME_MAX + 1) */

struct Bdbstats
{
	u64	nblk;
	u64	nused;
	u64	nrsvd;
	u64	bdbblkstats[BS_NSTAT];
	char	bdbname[BDB_NAMELEN];
};

enum Getstats_e
{
	GETSTATS_SUCCESS = 0,
	GETSTATS_ERROR = -1
};

union Getstats  switch(Getstats_e status)
{
case GETSTATS_SUCCESS:
	Bdbstats	stats;

case GETSTATS_ERROR:
	Err		e;
};

struct RBdbstats
{
	Getstats	resp;
};

%
%/* Flush the Bdb incore state: two different types: full and mark
% * full: flush all outstanding data and its corresponding index and make a journal checkpoint as result
% * mark: only intended for the marker since it ensures all the pending mark(s) are flushed into hdb
% * responses: RBdbflush or Detach
% */
enum Bdbflushop
{
	BDBFLUSH_FULL = 1,
	BDBFLUSH_MARK = 2
};

struct TBdbflush
{
	Smash		bdb;	/* target bdb */
	Bdbflushop	flushop;
};

enum Bdbflush_e
{
	BDBFLUSH_SUCCESS = 0,
	BDBFLUSH_ERROR = -1
};

union Bdbflush switch(Bdbflush_e status)
{
case BDBFLUSH_SUCCESS:
	void;

case BDBFLUSH_ERROR:
	Err		e;
};

struct RBdbflush
{
	Bdbflush	resp;
};

%
%/*
% * Special detached structure used when the connection is down
% */
struct Detached
{
	int		err;
};

%
%/*
% * Header structure marshalled across
% */
/* Enumerate the message types */
enum Stormhdr_e
{
	STORMSG_ISREQUEST	= 1,
	STORMSG_ISREPLY		= 2
};

struct Stormhdr
{
	u32		msgid;
	Stormhdr_e	type;
	Tinop		tinop;
};

%
%/*
% * Data structure for the messages
% */
union Stormdata switch(Stormsg_e op)
{
case STORMSG_DETACHED:
	Detached	detached;

case STORMSG_THELLO:
	THello		_thello;

case STORMSG_TGETOBJ:
	TGetobj		tgetobj;

case STORMSG_TPUTOBJ:
	TPutobj		tputobj;

case STORMSG_TDELOBJ:
	TDelobj		tdelobj;

case STORMSG_TMARKOBJ:
	TMarkobj	tmarkobj;

case STORMSG_RHELLO:
	RHello		_rhello;

case STORMSG_RGETOBJ:
	RGetobj		rgetobj;

case STORMSG_RPUTOBJ:
	RPutobj		rputobj;

case STORMSG_RDELOBJ:
	RDelobj		rdelobj;

case STORMSG_RMARKOBJ:
	RMarkobj	rmarkobj;

/* DEPREATED AND NOT CONVERTED YET */
case DMSG_THELLO:
	TRhello		thello;

case DMSG_TPING:
	void;

case DMSG_TBDBSTATS:
	TBdbstats	tbdbstats;

case DMSG_TBDBFLUSH:
	TBdbflush	tbdbflush;

case DMSG_RHELLO:
	TRhello		rhello;

case DMSG_RPING:
	void;

case DMSG_RBDBSTATS:
	RBdbstats	rbdbstats;

case DMSG_RBDBFLUSH:
	RBdbflush	rbdbflush;

case DMSG_RERROR:
	Err		rerror;
};

"#);

    println!("spec {:?}", s);
    assert!(s.is_ok())
}

#[test]
fn typedef_void() {
    let s = grammar::specification(r#"
typedef void;           /* syntactically defined, semantically meaningless  */
"#);

        println!("spec {:?}", s);
        assert!(s.is_err())
}

#[test]
fn kwishnames() {
    let specs = vec!["const in = 1;",
                     "const intt = 2;",
                     "const intint = 3;",
                     "struct unsignedint { int/**/foo;\n\tint\nbar; void;\n};",
                     ];

    for sp in specs {
        let s = grammar::specification(sp);
        println!("spec sp \"{}\" => {:?}", sp, s);
        assert!(s.is_ok())
    }
}

#[test]
fn kwnames() {
    let specs = vec!["const int = 1;",
                     "struct void { int i; };",
                     "struct foo { int int; };",
                     "typedef int int;",
                     ];

    for sp in specs {
        let s = grammar::specification(sp);
        println!("spec {:?}", s);
        assert!(s.is_err())
    }
}
