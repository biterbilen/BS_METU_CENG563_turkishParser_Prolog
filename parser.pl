s(s(Np,Vp)) --> np(Np,agr(nom,NUM,PER)), vp(Vp,agr(_,NUM,PER)).
s(s(Np,Vp)) --> np(Np,agr(pnom,NUM,PER)), vp(Vp,agr(_,NUM,PER)).
s(s(Np,Vp)) --> np(Np,agr(kinom,NUM,PER)), vp(Vp,agr(_,NUM,PER)).

s(s(Vp,Np)) --> vp(Vp,agr(_,NUM,PER)), np(Np,agr(nom,NUM,PER)).
s(s(Vp,Np)) --> vp(Vp,agr(_,NUM,PER)), np(Np,agr(pnom,NUM,PER)).
s(s(Vp,Np)) --> vp(Vp,agr(_,NUM,PER)), np(Np,agr(ki,NUM,PER)).

% arabayi ali gordu
s(s(Np1, Np2, Vp)) --> np(Np1,agr(acc,_,_)), np(Np2,agr(nom,NUM,PER)), vp(Vp,agr(acc,NUM,PER)).

% arabayi ben gordum
s(s(Np1, Np2, Vp)) --> np(Np1,agr(acc,_,_)),np(Np2,agr(pnom,NUM,PER)), vp(Vp,agr(acc,NUM,PER)).

% seni ben gordum
s(s(Np1, Np2, Vp)) --> np(Np1,agr(pacc,_,_)),np(Np2,agr(pnom,NUM,PER)), vp(Vp,agr(acc,NUM,PER)).

% gordu ali arabayi
s(s(Vp, Np1, Np2)) --> vp(Vp,agr(acc,NUM,PER)), np(Np1,agr(nom,NUM,PER)),np(Np2,agr(acc,_,_)).


% gordum ben arabayi
s(s(Vp, Np1, Np2)) --> vp(Vp,agr(acc,NUM,PER)), np(Np1,agr(pnom,NUM,PER)),np(Np2,agr(acc,_,_)).

% gordu ali seni
s(s(Vp, Np1, Np2)) --> vp(Vp,agr(acc,NUM,PER)), np(Np1,agr(nom,NUM,PER)),np(Np2,agr(pacc,_,_)).

% gordum ben seni
s(s(Vp, Np1, Np2)) --> vp(Vp,agr(acc,NUM,PER)), np(Np1,agr(pnom,NUM,PER)),np(Np2,agr(pacc,_,_)).

s(s(Vp)) --> vp(Vp,_).

% np(r(bir),n(doktor)) hatasini duzeltmek icin
% eski hali : np(np(a(X), n(N)),A) --> a(X, A),  n(N,A).
np(np(a(X), n(N)),agr(GEN, NUM, PER) ) --> a(X, agr(nongen, NUM, PER)),  n(N,agr(GEN, NUM, PER)).
np(np(a(X), n(N)),agr(kiacc, NUM, PER)) --> a(X, agr(ki, _, _)),  n(N,agr(acc, NUM, PER)).
np(np(a(X), n(N)),agr(kinom, NUM, PER)) --> a(X, agr(ki, _, _)),  n(N,agr(nom, NUM, PER)).

% gordugum adam
np(np(X, n(N)),agr(GEN, NUM, PER)) --> rp(X, agr(gen, _, _)),  n(N,agr(GEN, NUM, PER)).
np(n(N),A) --> n(N,A).


% gordum
vp(v(V),A) --> v(V,A).

vp(vp(N,V), agr(NOM, NUM, PER)) --> np(N, agr(NOM, _, _)), vp(V, agr(NOM, NUM, PER)).
vp(vp(N,V), agr(acc, NUM, PER)) --> np(N, agr(pacc, _, _)), vp(V, agr(acc, NUM, PER)).
% evdeki arabayI gOrdUm
vp(vp(N,V), agr(acc, NUM, PER)) --> np(N, agr(kiacc, _, _)), vp(V, agr(acc, NUM, PER)).

% bana kitabi verdi
vp(vp(N1, N2,V), agr(acc, NUM, PER)) --> np(N1, agr(pdat, _, _)), np(N2, agr(acc, _, _)), vp(V, agr(acd, NUM, PER)).

% gordum arabayi
vp(vp(v(V), N), agr(acc, NUM, PER)) --> v(V, agr(acc, NUM, PER)), np(N, agr(acc, _, _)).
% gordum evdeki arabayI
vp(vp(v(V), N), agr(acc, NUM, PER)) --> v(V, agr(acc, NUM, PER)), np(N, agr(kiacc, _, _)).
% gordum seni 
vp(vp(v(V), N), agr(acc, NUM, PER)) --> v(V, agr(acc, NUM, PER)), np(N, agr(pacc, _, _)).

% gordugum
rp(r(X),agr(gen,NUM,PER)) -->  a(X,agr(gen,NUM,PER)).
% benim gordugum
rp(rp(p(N), r(X)),agr(gen,NUM,PER)) --> n(N, agr(pgen,NUM,PER)),  a(X,agr(gen,NUM,PER)).
% adamin gordugu
rp(rp(n(N), r(X)),agr(gen,NUM,PER)) --> n(N, agr(gen,NUM,PER)),  a(X,agr(gen,NUM,PER)).

% uyuyan adamin gordugu 
%rp(np(a(X),R),agr(GEN,NUM,PER)) --> a(X, agr(GEN,_,_)), rp(R,agr(GEN,NUM,PER)).
%rp(rp(R, a(X)),agr(GEN,NUM,PER)) --> rp(R,agr(GEN,NUM,PER)), a(X, agr(gen,_,_)).
rp(rp(r(X), n(N), r(Y)),agr(GEN,NUM,PER)) --> a(X, agr(gen,_,_)), n(N, agr(GEN, NUM, PER)), a(Y, agr(GEN, NUM, PER)).

n(X,A) --> [X], {noun(X,A)}.

v(X,A) --> [X], {verb(X,A)}.

a(X, A) --> [X], {adj(X, A)}.

%r(X, A) --> [X], {rel(X, A)}.


noun(X,agr(GEN,NUM,PER)) :- lex(X,noun,GEN,NUM,PER).


verb(X,agr(GEN,NUM,PER)) :- lex(X,verb,GEN,NUM,PER).

adj(X,agr(GEN,NUM,PER)) :- lex(X,adj,GEN,NUM,PER).

%rel(X,agr(GEN,NUM,PER)) :- lex(X,rel,GEN,NUM,PER).


lex(her,adj, nongen,sing, _).
lex(bir,adj, nongen,sing, _).
lex(iki,adj, nongen,sing, _).
lex(gUzel,adj, nongen,_, _).
lex(gOrdUGUm,adj,gen,_,p1).
lex(gOrdUGU,adj,gen,_,p3).
lex(gOren,adj,gen,_,p3).
lex(uyuyan, adj, gen, _, p3).

lex(evdeki, adj, ki, _, _).


lex(adam,noun,nom,sing,p3).
lex(doktor,noun,nom,sing,p3).
lex(doktoru,noun,acc,sing,p3).
lex(adamI,noun,acc,sing,p3).
lex(adamIn,noun,gen,sing,p3).
lex(adamlar,noun,nom,plu,p3).
lex(oGrenciler,noun,nom,plu,p3).
lex(oGrenciyi,noun,acc,sing,p3).
lex(adamlarI,noun,acc,plu,p3).
lex(kitabI,noun,acc,sing,p3).
lex(araba, noun, nom, sing, p3).
lex(arabayI, noun, acc, sing, p3).

lex(ben,noun,pnom,sing,p1).
lex(bana,noun,pdat,sing,p1).
lex(beni,noun,pacc,sing,_).
lex(seni,noun,pacc,sing,_).
lex(benim,noun,pgen,_,p1).

lex(uyudu,verb,no,sing,p3).
lex(uyudum,verb,no,sing,p1).
lex(gOrdUm,verb,nom,sing,p1).
lex(gOrdUm,verb,acc,sing,p1).
lex(gOrdU,verb,nom,sing,p3).
lex(gOrdU,verb,acc,sing,p3).
lex(verdi,verb,nom, sing, p3).
lex(verdi,verb,acc, sing, p3).
lex(verdi,verb,dat, sing, p3).
lex(verdi,verb,acd, sing, p3).
