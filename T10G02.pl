% Author:
% Date: 16/04/2014


get_name(2,final).
get_name(4,semi_final).
get_name(8,quarter_final).
get_name(16,round_four).
get_name(32,round_five).
get_name(64,round_six).
get_name(128,round_seven).
get_name(256,round_eight).
get_name(512,round_nine).
get_name(1024,round_ten).

create(N, [N], N).
create(X, [X|S], N):-   X1 is X+1, X<N ,
                        create(X1, S , N).

pick(H, [H|T], T).
pick(C, [H|T], [H|R]):-     pick(C, T, R).

schedule_helper([],[],[],_).
schedule_helper([P1|P ], HalfList, [R | R1], N ):- pick(C, HalfList, Rest),
                                              get_name(N, Round ),
                                              R = game(P1, C , Round ),
                                              schedule_helper(P , Rest, R1, N).

schedule_round(N, R):-  N1 is N / 2 ,
                        create(1, List,N1 ),
                        X is N1 + 1 ,
                        create(X, L2,N ),
                        schedule_helper(List, L2, R, N).
schedule_rounds(1,[]).
schedule_rounds(N,[H |T]):- N > 1 ,
                     schedule_round(N , H),
                     N1 is N / 2 ,
                     schedule_rounds(N1 , T).

ismember2(X):- member(game(1,_,_),X),member(game(2,_,_),X).

generate(L,Z):-member(X,L),member(Y,L),(Z = [X,Y],X\=Y,X@<Y,\+ismember2(Z)).
generate(L,R):-member(X,L),member(Y,L),member(Z,L),(R = [X,Y,Z],X\=Y,X\=Z,Y\=Z,X@<Y,Y@<Z,\+ismember2([X,Y])).
generate(L,Z):-member(X,L),Z = [X].


flat([],[]).
flat([H|T],Z):- is_list(H),flat(H,H2),flat(T,Z1),append(H2,Z1,Z).
flat([H|T],Z):- \+(is_list(H)),flat(T,Z1),append([H],Z1,Z).

my_append([[]],[X],[X]).
my_append(X,Y,Z):- X\=[[]],append(X,Y,Z).

real_generate(L,Acc,Z):-flat(Acc,Acc2),length(Acc2,L2),length(L,L3),L2=L3,Z=Acc.
real_generate(L,Acc,Z):-flat(Acc,Acc2),length(Acc2,L2),length(L,L3),L2<L3,generate(L,X),
            intersect(X,Acc2,[]),my_append(Acc,[X],Acc1),real_generate(L,Acc1,Z).

intersect([X|Y],M,[X|Z]) :- member(X,M), intersect(Y,M,Z).
intersect([X|Y],M,Z) :- \+ member(X,M), intersect(Y,M,Z).
intersect([],_,[]).

tournament(2,1,[[game(1,2,final)]]).
tournament(X,D,Z):-   X>1,D>0,
                      schedule_round(X,Y),X1 is X/2,
                      real_generate(Y,[[]],Z1),length(Z1,L1),
                      D1 is D -L1,
                      tournament(X1,D1,Z2),
                      append(Z1,Z2,Z).
% pick2([H |T ] , [H | R] , L , Length) :-  Length > 0 ,
%                                         Length1 is Length - 1 ,
%                                         pick2(T ,R ,L , Length1 ).
% pick2([H |T ] ,[H ],T ,Length) :- Length > 0 .
% pick2([H | T ] , R , [H | L ] , Length ) :- Length > 0 ,
%                                              pick2(T ,R, L ,Length).
% callPick2([],[] , Days,Days) :- Days >= 0 .
% callPick2(List , [H | T] ,Days,DLeft ) :- Days >0 ,
%                                     pick2(List , H , Rest , 3 ),
%                                     \+ (member(game(1 , _ , _ ) , H ) , member(game(2,_,_), H )),
%                                     Days2 is Days - 1 ,
%                                     callPick2(Rest , T , Days2 , DLeft).
% 
% tournament(2, 1,[[game(1,2,final)]]).
% 
% tournament(N ,Days ,L) :- N > 1 , Days > 0 ,
%                                schedule_round(N , Schedule) ,
%                                 callPick2(Schedule , H ,Days,DLeft),
%                                 N1 is N / 2 ,
%                                 tournament(N1 , DLeft,R) ,
%                                 append(H,R,L).
% 
% 