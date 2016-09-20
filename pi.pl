% Data type definitions

piterm(par(P, Q)) :- piterm(P), piterm(Q).
piterm(new(C, P)) :- channel(C), piterm(P).
piterm(srv(B, C, P)) :- channel(B), channel(C), piterm(P).
piterm(rcv(B, C, P)) :- channel(B), channel(C), piterm(P).
piterm(send(M, C, P)) :- channel(M), channel(C), piterm(P).
piterm(end).

channel(chan) :- atom(chan).

%% pstep(+P:piterm, -Q:piterm)
%
% Reduction semantics for piterms, i.e. an evaluation function
%
% @param P a piterm
% @param Q the result - a single evaluation step from P

pstep(par(send(M, C, P), rcv(B, C, Q)), par(P, QP)) :- subst(Q, M, B, QP).
pstep(par(P, Q), par(PP, Q)) :- pstep(P, PP).
pstep(new(X, P), new(X, PP)) :- pstep(P, PP).
pstep(P, Q) :- struct(P, SP), pstep(SP, Q).

%% struct(+P:piterm, +Q:piterm)
%
% Structural congruence
%
% @param P a piterm
% @param Q a piterm
% @result true if P is structurally congruent to Q, false otherwise

struct(par(P, end), P).
struct(par(P, par(Q, R)), par(par(P, Q), R)).
struct(par(P, Q), par(Q, P)).

struct(new(_, end), end).
struct(new(X, new(Y, P)), new(Y, new(X, P))).

% TODO struct(par(new(X, P), Q), new(X, par(P, Q))) :- X not free in Q.

%% subst(+P:piterm, +N:channel, +B:channel, -PP:piterm)
%
% Pi term substitution
%
% @param P a piterm within which to perform substitution
% @param N the name being substituted into the term P
% @param B the name being replaced within the term P
% @param PP the result of the computation - the term P where occurences of the
%           name B are replaced by the name N.

subst(par(P, Q), N, B, par(PP, QP)) :- subst(P, N, B, PP), subst(Q, N, B, QP).

subst(new(B, P), _, B, new(B, P)).
subst(new(C, P), N, B, new(C, PP)) :- subst(P, N, B, PP).

subst(srv(B, C, P), N, B, srv(B, CP, P)) :- csubst(C, N, B, CP).
subst(srv(B1, C, P), N, B2, srv(B1, CP, PP)) :-
    csubst(C, N, B2, CP), subst(P, N, B2, PP).

subst(rcv(B, C, P), N, B, rcv(B, CP, P)) :- csubst(C, N, B, CP).
subst(rcv(B1, C, P), N, B2, rcv(B1, CP, PP)) :-
    csubst(C, N, B2, CP), subst(P, N, B2, PP).

subst(send(M, C, P), N, B, send(MP, CP, PP)) :-
    csubst(M, N, B, MP), csubst(C, N, B, CP), subst(P, N, B, PP).

subst(end, _, _, end).

%% csubst(+Ch:channel, +To:channel, +From:channel, -ChP:channel)
%
% Channel substitution
%
% @param C a name that is potentially to be renamed
% @param T a name that may replace C
% @param F if F is C then T replaces C
% @param CP the result of the computation - C if C is not F, T if C is F

csubst(B, M, B, M).
csubst(C, _, _, C).

tm(X) :- X = par(send(m, x, end), rcv(b, x, send(b, b, end))).
