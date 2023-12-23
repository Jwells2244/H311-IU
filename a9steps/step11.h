void *r__t__nr__t__, *r__t__kr__t__, *r__t__apr__m__kr__t__, *r__t__vr__t__;

void (*r__t__pcr__t__)();

void fib();
void applyr__m__k();
struct cont;
typedef struct cont cont;
struct cont {
  enum {
    _maker__m__subr1r__m__k_cont,
    _maker__m__subr2r__m__k_cont,
    _emptyr__m__k_cont
  } tag;
  union {
    struct { void *_nr__ex__; void *_kr__ex__; } _maker__m__subr1r__m__k;
    struct { void *_vr__ex__; void *_kr__ex__; } _maker__m__subr2r__m__k;
    struct { void *_jumpout; } _emptyr__m__k;
  } u;
};

void *contr_maker__m__subr1r__m__k(void *nr__ex__, void *kr__ex__);
void *contr_maker__m__subr2r__m__k(void *vr__ex__, void *kr__ex__);
void *contr_emptyr__m__k(void *jumpout);

int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

