#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "step11.h"

void *contr_maker__m__subr1r__m__k(void *nr__ex__, void *kr__ex__) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__subr1r__m__k_cont;
  _data->u._maker__m__subr1r__m__k._nr__ex__ = nr__ex__;
  _data->u._maker__m__subr1r__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *contr_maker__m__subr2r__m__k(void *vr__ex__, void *kr__ex__) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__subr2r__m__k_cont;
  _data->u._maker__m__subr2r__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__subr2r__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *contr_emptyr__m__k(void *jumpout) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_cont;
  _data->u._emptyr__m__k._jumpout = jumpout;
  return (void *)_data;
}

int main()
{
r__t__nr__t__ = (void *)(void *)8;
r__t__pcr__t__ = &fib;
mount_tram();
printf("Value of (fib 5) is %d\n", (int)r__t__vr__t__);}

void applyr__m__k()
{
cont* _c = (cont*)r__t__apr__m__kr__t__;
switch (_c->tag) {
case _emptyr__m__k_cont: {
void *jumpout = _c->u._emptyr__m__k._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
case _maker__m__subr1r__m__k_cont: {
void *nr__ex__ = _c->u._maker__m__subr1r__m__k._nr__ex__;
void *kr__ex__ = _c->u._maker__m__subr1r__m__k._kr__ex__;
r__t__kr__t__ = (void *)contr_maker__m__subr2r__m__k(r__t__vr__t__,kr__ex__);
r__t__nr__t__ = (void *)(void *)((int)(void *)((int)nr__ex__ - 1) - 1);
r__t__pcr__t__ = &fib;
break; }
case _maker__m__subr2r__m__k_cont: {
void *vr__ex__ = _c->u._maker__m__subr2r__m__k._vr__ex__;
void *kr__ex__ = _c->u._maker__m__subr2r__m__k._kr__ex__;
r__t__apr__m__kr__t__ = (void *)kr__ex__;
r__t__vr__t__ = (void *)(void *)((int)vr__ex__ + (int)r__t__vr__t__);
r__t__pcr__t__ = &applyr__m__k;
break; }
}
}

void fib()
{
if((r__t__nr__t__ <= (void *)1)) {
  r__t__apr__m__kr__t__ = (void *)r__t__kr__t__;
r__t__vr__t__ = (void *)(void *)1;
r__t__pcr__t__ = &applyr__m__k;

} else {
  r__t__kr__t__ = (void *)contr_maker__m__subr1r__m__k(r__t__nr__t__,r__t__kr__t__);
r__t__nr__t__ = (void *)(void *)((int)r__t__nr__t__ - 1);
r__t__pcr__t__ = &fib;

}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
r__t__kr__t__= (void *)contr_emptyr__m__k(dismount);
for(;;) {
r__t__pcr__t__();
}
}
return 0;
}
