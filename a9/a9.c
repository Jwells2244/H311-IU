#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"
#include <time.h>

void *closr_maker__m__closure(void *x, void *env) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__closure_clos;
  _data->u._maker__m__closure._x = x;
  _data->u._maker__m__closure._env = env;
  return (void *)_data;
}

void *ktr_maker__m__multr__m__outerr__m__k(void *vr__ex__, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__multr__m__outerr__m__k_kt;
  _data->u._maker__m__multr__m__outerr__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__multr__m__outerr__m__k._env = env;
  _data->u._maker__m__multr__m__outerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_maker__m__multr__m__innerr__m__k(void *vr__ex__, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__multr__m__innerr__m__k_kt;
  _data->u._maker__m__multr__m__innerr__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__multr__m__innerr__m__k._k = k;
  return (void *)_data;
}

void *ktr_maker__m__subr1r__m__k(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__subr1r__m__k_kt;
  _data->u._maker__m__subr1r__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__zeror__m__k(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__zeror__m__k_kt;
  _data->u._maker__m__zeror__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__ifr__m__k(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__ifr__m__k_kt;
  _data->u._maker__m__ifr__m__k._conseqr__ex__ = conseqr__ex__;
  _data->u._maker__m__ifr__m__k._altr__ex__ = altr__ex__;
  _data->u._maker__m__ifr__m__k._envr__ex__ = envr__ex__;
  _data->u._maker__m__ifr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__pitchr__m__outerr__m__k(void *vr__ex__, void *envr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__pitchr__m__outerr__m__k_kt;
  _data->u._maker__m__pitchr__m__outerr__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__pitchr__m__outerr__m__k._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__pitchr__m__innerr__m__k(void *vr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__pitchr__m__innerr__m__k_kt;
  _data->u._maker__m__pitchr__m__innerr__m__k._vr__ex__ = vr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__letr__m__k(void *vr__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__letr__m__k_kt;
  _data->u._maker__m__letr__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__letr__m__k._envr__ex__ = envr__ex__;
  _data->u._maker__m__letr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__appr__m__outerr__m__k(void *vr__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__appr__m__outerr__m__k_kt;
  _data->u._maker__m__appr__m__outerr__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__appr__m__outerr__m__k._envr__ex__ = envr__ex__;
  _data->u._maker__m__appr__m__outerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__appr__m__innerr__m__k(void *vr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__appr__m__innerr__m__k_kt;
  _data->u._maker__m__appr__m__innerr__m__k._vr__ex__ = vr__ex__;
  _data->u._maker__m__appr__m__innerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_emptyr__m__k(void *jumpout) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._jumpout = jumpout;
  return (void *)_data;
}

void *envrr_extendr__m__env(void *ar__ex__, void *envr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._ar__ex__ = ar__ex__;
  _data->u._extendr__m__env._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *envrr_emptyr__m__env() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envr;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_catch(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _catch_expr;
  _data->u._catch._body = body;
  return (void *)_data;
}

void *exprr_pitch(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _pitch_expr;
  _data->u._pitch._kexp = kexp;
  _data->u._pitch._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
r__t__cexprr__t__ = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_catch(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_pitch(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
r__t__envr__m__cpsr__t__ = (void *)envrr_emptyr__m__env();
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
mount_tram();
printf("Value of expression is %d\n", (int)r__t__vr__t__);}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)r__t__cexprr__t__;
switch (_c->tag) {
case _const_expr: {
void *cexpr = _c->u._const._cexp;
r__t__ar__m__kr__t__ = (void *)r__t__kr__t__;
r__t__vr__t__ = (void *)cexpr;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _mult_expr: {
void *xr1 = _c->u._mult._nexpr1;
void *xr2 = _c->u._mult._nexpr2;
r__t__kr__t__ = (void *)ktr_maker__m__multr__m__outerr__m__k(xr2,r__t__envr__m__cpsr__t__,r__t__kr__t__);
r__t__cexprr__t__ = (void *)xr1;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *x = _c->u._subr1._nexp;
r__t__kr__t__ = (void *)ktr_maker__m__subr1r__m__k(r__t__kr__t__);
r__t__cexprr__t__ = (void *)x;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *x = _c->u._zero._nexp;
r__t__kr__t__ = (void *)ktr_maker__m__zeror__m__k(r__t__kr__t__);
r__t__cexprr__t__ = (void *)x;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
r__t__kr__t__ = (void *)ktr_maker__m__ifr__m__k(conseq,alt,r__t__envr__m__cpsr__t__,r__t__kr__t__);
r__t__cexprr__t__ = (void *)test;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _catch_expr: {
void *body = _c->u._catch._body;
r__t__envr__m__cpsr__t__ = (void *)envrr_extendr__m__env(r__t__kr__t__,r__t__envr__m__cpsr__t__);
r__t__cexprr__t__ = (void *)body;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _pitch_expr: {
void *kr__m__exp = _c->u._pitch._kexp;
void *vr__m__exp = _c->u._pitch._vexp;
r__t__kr__t__ = (void *)ktr_maker__m__pitchr__m__outerr__m__k(vr__m__exp,r__t__envr__m__cpsr__t__);
r__t__cexprr__t__ = (void *)kr__m__exp;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *e = _c->u._let._exp;
void *body = _c->u._let._body;
r__t__kr__t__ = (void *)ktr_maker__m__letr__m__k(body,r__t__envr__m__cpsr__t__,r__t__kr__t__);
r__t__cexprr__t__ = (void *)e;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *v = _c->u._var._n;
r__t__vr__t__ = (void *)v;
r__t__pcr__t__ = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
r__t__vr__t__ = (void *)closr_maker__m__closure(body,r__t__envr__m__cpsr__t__);
r__t__ar__m__kr__t__ = (void *)r__t__kr__t__;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
r__t__kr__t__ = (void *)ktr_maker__m__appr__m__outerr__m__k(rand,r__t__envr__m__cpsr__t__,r__t__kr__t__);
r__t__cexprr__t__ = (void *)rator;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__k()
{
kt* _c = (kt*)r__t__ar__m__kr__t__;
switch (_c->tag) {
case _emptyr__m__k_kt: {
void *jumpout = _c->u._emptyr__m__k._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
case _maker__m__multr__m__outerr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__multr__m__outerr__m__k._vr__ex__;
void *env = _c->u._maker__m__multr__m__outerr__m__k._env;
void *k = _c->u._maker__m__multr__m__outerr__m__k._k;
r__t__kr__t__ = (void *)ktr_maker__m__multr__m__innerr__m__k(r__t__vr__t__,k);
r__t__cexprr__t__ = (void *)vr__ex__;
r__t__envr__m__cpsr__t__ = (void *)env;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__multr__m__innerr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__multr__m__innerr__m__k._vr__ex__;
void *k = _c->u._maker__m__multr__m__innerr__m__k._k;
r__t__vr__t__ = (void *)(void *)((int)vr__ex__ * (int)r__t__vr__t__);
r__t__ar__m__kr__t__ = (void *)k;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__subr1r__m__k_kt: {
void *kr__ex__ = _c->u._maker__m__subr1r__m__k._kr__ex__;
r__t__vr__t__ = (void *)(void *)((int)r__t__vr__t__ - 1);
r__t__ar__m__kr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__zeror__m__k_kt: {
void *kr__ex__ = _c->u._maker__m__zeror__m__k._kr__ex__;
r__t__vr__t__ = (void *)(r__t__vr__t__ == 0);
r__t__ar__m__kr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__ifr__m__k_kt: {
void *conseqr__ex__ = _c->u._maker__m__ifr__m__k._conseqr__ex__;
void *altr__ex__ = _c->u._maker__m__ifr__m__k._altr__ex__;
void *envr__ex__ = _c->u._maker__m__ifr__m__k._envr__ex__;
void *kr__ex__ = _c->u._maker__m__ifr__m__k._kr__ex__;
if(r__t__vr__t__) {
  r__t__cexprr__t__ = (void *)conseqr__ex__;
r__t__envr__m__cpsr__t__ = (void *)envr__ex__;
r__t__kr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;

} else {
  r__t__cexprr__t__ = (void *)altr__ex__;
r__t__envr__m__cpsr__t__ = (void *)envr__ex__;
r__t__kr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;

}
break; }
case _maker__m__pitchr__m__outerr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__pitchr__m__outerr__m__k._vr__ex__;
void *envr__ex__ = _c->u._maker__m__pitchr__m__outerr__m__k._envr__ex__;
r__t__cexprr__t__ = (void *)vr__ex__;
r__t__envr__m__cpsr__t__ = (void *)envr__ex__;
r__t__kr__t__ = (void *)ktr_maker__m__pitchr__m__innerr__m__k(r__t__vr__t__);
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__pitchr__m__innerr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__pitchr__m__innerr__m__k._vr__ex__;
r__t__ar__m__kr__t__ = (void *)vr__ex__;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__letr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__letr__m__k._vr__ex__;
void *envr__ex__ = _c->u._maker__m__letr__m__k._envr__ex__;
void *kr__ex__ = _c->u._maker__m__letr__m__k._kr__ex__;
r__t__envr__m__cpsr__t__ = (void *)envrr_extendr__m__env(r__t__vr__t__,envr__ex__);
r__t__cexprr__t__ = (void *)vr__ex__;
r__t__kr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__appr__m__outerr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__appr__m__outerr__m__k._vr__ex__;
void *envr__ex__ = _c->u._maker__m__appr__m__outerr__m__k._envr__ex__;
void *kr__ex__ = _c->u._maker__m__appr__m__outerr__m__k._kr__ex__;
r__t__kr__t__ = (void *)ktr_maker__m__appr__m__innerr__m__k(r__t__vr__t__,kr__ex__);
r__t__cexprr__t__ = (void *)vr__ex__;
r__t__envr__m__cpsr__t__ = (void *)envr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__appr__m__innerr__m__k_kt: {
void *vr__ex__ = _c->u._maker__m__appr__m__innerr__m__k._vr__ex__;
void *kr__ex__ = _c->u._maker__m__appr__m__innerr__m__k._kr__ex__;
r__t__closr__t__ = (void *)vr__ex__;
r__t__kr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &applyr__m__closure;
break; }
}
}

void applyr__m__closure()
{
clos* _c = (clos*)r__t__closr__t__;
switch (_c->tag) {
case _maker__m__closure_clos: {
void *x = _c->u._maker__m__closure._x;
void *env = _c->u._maker__m__closure._env;
r__t__envr__m__cpsr__t__ = (void *)envrr_extendr__m__env(r__t__vr__t__,env);
r__t__cexprr__t__ = (void *)x;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)r__t__envr__m__cpsr__t__;
switch (_c->tag) {
case _extendr__m__env_envr: {
void *ar__ex__ = _c->u._extendr__m__env._ar__ex__;
void *envr__ex__ = _c->u._extendr__m__env._envr__ex__;
if((r__t__vr__t__ == 0)) {
  r__t__ar__m__kr__t__ = (void *)r__t__kr__t__;
r__t__vr__t__ = (void *)ar__ex__;
r__t__pcr__t__ = &applyr__m__k;

} else {
  r__t__envr__m__cpsr__t__ = (void *)envr__ex__;
r__t__vr__t__ = (void *)(void *)((int)r__t__vr__t__ - 1);
r__t__pcr__t__ = &applyr__m__env;

}
break; }
case _emptyr__m__env_envr: {
fprintf(stderr, "*v*");
 exit(1);
break; }
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
r__t__kr__t__= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
r__t__pcr__t__();
}
}
return 0;
}
