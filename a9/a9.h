void *r__t__envr__m__cpsr__t__, *r__t__kr__t__, *r__t__ar__m__kr__t__, *r__t__vr__t__, *r__t__cexprr__t__, *r__t__closr__t__;

void (*r__t__pcr__t__)();

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _catch_expr,
    _pitch_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _catch;
    struct { void *_kexp; void *_vexp; } _pitch;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_catch(void *body);
void *exprr_pitch(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _extendr__m__env_envr,
    _emptyr__m__env_envr
  } tag;
  union {
    struct { void *_ar__ex__; void *_envr__ex__; } _extendr__m__env;
    struct { char dummy; } _emptyr__m__env;
  } u;
};

void *envrr_extendr__m__env(void *ar__ex__, void *envr__ex__);
void *envrr_emptyr__m__env();

struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _maker__m__multr__m__outerr__m__k_kt,
    _maker__m__multr__m__innerr__m__k_kt,
    _maker__m__subr1r__m__k_kt,
    _maker__m__zeror__m__k_kt,
    _maker__m__ifr__m__k_kt,
    _maker__m__pitchr__m__outerr__m__k_kt,
    _maker__m__pitchr__m__innerr__m__k_kt,
    _maker__m__letr__m__k_kt,
    _maker__m__appr__m__outerr__m__k_kt,
    _maker__m__appr__m__innerr__m__k_kt,
    _emptyr__m__k_kt
  } tag;
  union {
    struct { void *_vr__ex__; void *_env; void *_k; } _maker__m__multr__m__outerr__m__k;
    struct { void *_vr__ex__; void *_k; } _maker__m__multr__m__innerr__m__k;
    struct { void *_kr__ex__; } _maker__m__subr1r__m__k;
    struct { void *_kr__ex__; } _maker__m__zeror__m__k;
    struct { void *_conseqr__ex__; void *_altr__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__ifr__m__k;
    struct { void *_vr__ex__; void *_envr__ex__; } _maker__m__pitchr__m__outerr__m__k;
    struct { void *_vr__ex__; } _maker__m__pitchr__m__innerr__m__k;
    struct { void *_vr__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__letr__m__k;
    struct { void *_vr__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__appr__m__outerr__m__k;
    struct { void *_vr__ex__; void *_kr__ex__; } _maker__m__appr__m__innerr__m__k;
    struct { void *_jumpout; } _emptyr__m__k;
  } u;
};

void *ktr_maker__m__multr__m__outerr__m__k(void *vr__ex__, void *env, void *k);
void *ktr_maker__m__multr__m__innerr__m__k(void *vr__ex__, void *k);
void *ktr_maker__m__subr1r__m__k(void *kr__ex__);
void *ktr_maker__m__zeror__m__k(void *kr__ex__);
void *ktr_maker__m__ifr__m__k(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_maker__m__pitchr__m__outerr__m__k(void *vr__ex__, void *envr__ex__);
void *ktr_maker__m__pitchr__m__innerr__m__k(void *vr__ex__);
void *ktr_maker__m__letr__m__k(void *vr__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_maker__m__appr__m__outerr__m__k(void *vr__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_maker__m__appr__m__innerr__m__k(void *vr__ex__, void *kr__ex__);
void *ktr_emptyr__m__k(void *jumpout);

struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _maker__m__closure_clos
  } tag;
  union {
    struct { void *_x; void *_env; } _maker__m__closure;
  } u;
};

void *closr_maker__m__closure(void *x, void *env);

void applyr__m__env();
void applyr__m__closure();
void applyr__m__k();
void valuer__m__ofr__m__cps();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

