card(clubs,2).
card(clubs,3).
card(clubs,4).
card(clubs,5).
card(clubs,6).
card(clubs,7).
card(clubs,8).
card(clubs,9).
card(clubs,10).
card(clubs,jack).
card(clubs,queen).
card(clubs,king).
card(clubs,ace).

card(spades,2).
card(spades,3).
card(spades,4).
card(spades,5).
card(spades,6).
card(spades,7).
card(spades,8).
card(spades,9).
card(spades,10).
card(spades,jack).
card(spades,queen).
card(spades,king).
card(spades,ace).

card(hearts,2).
card(hearts,3).
card(hearts,4).
card(hearts,5).
card(hearts,6).
card(hearts,7).
card(hearts,8).
card(hearts,9).
card(hearts,10).
card(hearts,jack).
card(hearts,queen).
card(hearts,king).
card(hearts,ace).

card(diamonds,2).
card(diamonds,3).
card(diamonds,4).
card(diamonds,5).
card(diamonds,6).
card(diamonds,7).
card(diamonds,8).
card(diamonds,9).
card(diamonds,10).
card(diamonds,jack).
card(diamonds,queen).
card(diamonds,king).
card(diamonds,ace).

successor(3,2).
successor(4,3).
successor(5,4).
successor(6,5).
successor(7,6).
successor(8,7).
successor(9,8).
successor(10,9).
successor(jack,10).
successor(queen,jack).
successor(king,queen).
successor(ace,king).

successor(pair,no_pair).
successor(two_pair,pair).
successor(three_of_a_kind,two_pair).
successor(straight,three_of_a_kind).
successor(flush,straight).
successor(full_house,flush).
successor(four_of_a_kind,full_house).
successor(straight_flush,four_of_a_kind).

higher_value(X,Y):- successor(X,P),(Y=P ; higher_value(P,Y)).

beats((_,V),(_,V),tie).
beats((S,V1),(_,V2),(S,V1)) :- higher_value(V1,V2).
beats((_,V1),(S,V2),(S,V2)) :- higher_value(V2,V1).

beats(X,X,tie).
beats(X,Y,X) :- higher_value(X,Y).
beats(X,Y,Y) :- higher_value(Y,X).

appendl([],Y,Y).
appendl([H|X],Y,[H|Z]):- appendl(X,Y,Z).

partitionl([], Y, [], []).  
partitionl([H|T],Pivot,[H|Lower], Higher) :- beats(Pivot,H,Z), (Z = Pivot ; Z = tie), partitionl(T, Pivot, Lower, Higher).
partitionl([H|T],Pivot, Lower, [H|Higher]) :- beats(Pivot,H,H), partitionl(T, Pivot, Lower, Higher). 

qsortl([H|T], Sorted) :- partitionl(T,H,Lower,Higher), qsortl(Lower,SortedLower), qsortl(Higher,SortedHigher), appendl(SortedLower,[H|SortedHigher],Sorted).
qsortl([],[]).

% hand_value(L,Y) L is the 5 cards and Y is the hand value of the person.

hand_value([(S,A),(S,B),(S,C),(S,D),(S,E)], straight_flush):- successor(E,D), successor(D,C), successor(C,B), successor(B,A).
hand_value([(_,C),(_,A),(_,A),(_,A),(_,E)], four_of_a_kind):- C = A ; E = A.
hand_value([(_,A),(_,B),(_,C),(_,D),(_,E)], full_house):- A = B, D = E, (C = D ; C = B).
hand_value([(S,A),(S,B),(S,C),(S,D),(S,D)], flush):- A\=B, B\=C, C\=D, D\=E.
hand_value([(S1,A),(S2,B),(S3,C),(S4,D),(S5,E)], straight) :- S1\=S2, S2\=S3, S3\=S4, S4\=S5, successor(E,D), successor(D,C), successor(C,B), successor(B,A).
hand_value([(_,A),(_,B),(_,C),(_,D),(_,E)], three_of_a_kind) :- (A = B, B = C); (B = C, C = D); (C = D, D = E).
hand_value([(_,A),(_,A),(_,B),(_,B),(_,_)], two_pair) :- A\=B.
hand_value([(_,_),(_,A),(_,A),(_,B),(_,B)], two_pair) :- A\=B.
hand_value([(_,A),(_,A),(_,_),(_,B),(_,B)], two_pair) :- A\=B.
hand_value([(_,A),(_,B),(_,C),(_,D),(_,E)], pair):- A=B; B=C; C=D; D=E.
hand_value([(_,A),(_,B),(_,C),(_,D),(_,E)], no_pair).

% hand_value(L,Y) L is the 3 face up cards and Y is the hand value of the person.

hand_value([(S,A),(S,B),(S,C)], straight_flush):- successor(E,D), successor(C,B), successor(B,A).
hand_value([(S,A),(S,B),(S,C)], flush):- A\=B, B\=C.
hand_value([(S1,A),(S2,B),(S3,C)], straight) :- S1\=S2, S2\=S3, successor(D,C), successor(C,B), successor(B,A).
hand_value([(_,A),(_,B),(_,C)], three_of_a_kind) :- (A = B, B = C).
hand_value([(_,A),(_,A),(_,_)], two_pair).
hand_value([(_,A),(_,B),(_,C)], pair):- A=B; B=C.
hand_value([(_,A),(_,B),(_,C)], no_pair).

% Tiebreak cases

tiebreak(straight_flush, H1, H2, Winner)  :- higher_last_card(H1, H2, Winner).
tiebreak(four_of_a_kind, H1, H2, Winner)  :- higher_middle_card(H1, H2, Winner).
tiebreak(full_house, H1, H2, Winner)      :- higher_middle_card(H1, H2, Winner).
tiebreak(flush, H1, H2, Winner)           :- tiebreak(high_card, H1, H2, Winner).
tiebreak(straight, H1, H2, Winner)        :- higher_last_card(H1, H2, Winner).
tiebreak(three_of_a_kind, H1, H2, Winner) :- higher_middle_card(H1, H2, Winner).

tiebreak(two_pair, H1, H2, Winner) :-
  isolate_pairs(H1, (_,HighCard1), (_,LowCard1), Last1),
  isolate_pairs(H2, (_,HighCard2), (_,LowCard2), Last2),
  (beats_with_hand(H1, HighCard1, H2, HighCard2, Winner),
   Winner \= tie;
   beats_with_hand(H1, LowCard1, H2, LowCard2, Winner),
   Winner \= tie;
   beats_with_hand(H1, Last1, H2, Last2, Winner)).
     
tiebreak(pair, H1, H2, Winner) :-
  isolate_pair(H1, (_,PairCard1), Rst1),
  isolate_pair(H2, (_,PairCard2), Rst2),
  (beats_with_hand(H1, PairCard1, H2, PairCard2, Winner), Winner \= tie ;
   tiebreak(high_card, Rst1, Rst2, Winner)).

tiebreak(high_card, H1, H2, X) :- 
  reverse(H1, RevH1),
  reverse(H2, RevH2),
  highest_card_chain(RevH1, RevH2, X).

beats_with_hand(H1, C1, H2, C2, X) :-
  beats(C1, C2, C1), X = left ;
  beats(C1, C2, C2), X = right ;
  X = tie.

% Really ugly.  How to better do this?
isolate_pairs(Hand, High_Pair, Low_Pair, Last) :-
  [(S1,V1),(S2,V2),(S3,V3),(S4,V4),(S5,V5)] = Hand,
  (V5 = V4, High_Pair = [(S4,V4),(S5,V5)],
    (V3 = V2, Low_Pair = [(S3,V3),(S2,V2)], Last = (S1,V1) ;
     V1 = V2, Low_Pair = [(S1,V1),(S2,V2)], Last = (S3,V3))) ;
  (Low_Pair = [(S1,V1),(S2,V2)], 
   High_Pair = [(S3,V3),(S4,V4)],
   Last = (S5,V5)).

isolate_pair(Hand, Pair, Rst) :-
  [(S1,V1),(S2,V2),(S3,V3),(S4,V4),(S5,V5)] = Hand,
  (V1 = V2, Pair = [(S1,V1),(S2,V2)], Rst = [(S3,V3),(S4,V4),(S5,V5)] ;
   V2 = V3, Pair = [(S3,V3),(S2,V2)], Rst = [(S1,V1),(S4,V4),(S5,V5)] ;
   V4 = V3, Pair = [(S3,V3),(S4,V4)], Rst = [(S1,V1),(S2,V2),(S5,V5)] ;
   V4 = V5, Pair = [(S5,V5),(S4,V4)], Rst = [(S1,V1),(S2,V2),(S3,V3)]).

highest_card_chain([H1|T1], [H2|T2], X) :-
  beats(H1,H2,Verdict),
  (Verdict = H1, X = left ;
   Verdict = H2, X = right ;
   Verdict = tie, highest_card_chain(T1,T2,X)).

higher_last_card(H1,H2,Winner) :-
  H1 = [_,_,_,_,(_,V1)],
  H2 = [_,_,_,_,(_,V2)],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left ;
   Higher = V2, Winner = right).

higher_middle_card(H1, H2, Winner) :-
  H1 = [_,_,(_,V1),_,_],
  H2 = [_,_,(_,V2),_,_],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left;
   Higher = V2, Winner = right).

% calculateprobability: calcualtion of the probability of the cards.
calculateprobability(straight_flush) :- print("0.0000138517."),nl.
calculateprobability(pair) :- print("0.422569."),nl.
calculateprobability(two_pair) :- print("0.047539."),nl.
calculateprobability(three_of_a_kind) :- print("0.0211285."),nl.
calculateprobability(no_pair) :- print("0.501177."),nl.
calculateprobability(straight) :- print("0.00392465."),nl.
calculateprobability(full_house) :- print("0.00144058."),nl.
calculateprobability(four_of_a_kind) :- print("0.000240096."),nl.
calculateprobability(flush) :- print("0.0019654."),nl.

calculateprobability(straight_flush,X) :- X= 0.0000138517.
calculateprobability(pair,X) :- X= 0.422569.
calculateprobability(two_pair,X) :- X = 0.047539.
calculateprobability(three_of_a_kind,X) :- X =  0.0211285.
calculateprobability(no_pair,X) :- X = 0.501177.
calculateprobability(straight,X) :- X=  0.00392465.
calculateprobability(full_house,X) :- X= 0.00144058.
calculateprobability(four_of_a_kind,X) :- X= 0.000240096.
calculateprobability(flush,X) :- X= 0.0019654.

random_permutation(L,L3) :- 
    add_random_keys(L,L1), 
    keysort(L1,L2), 
    strip_keys(L2,L3). 
 add_random_keys([],[]). 
 add_random_keys([X|L],[R-X|L1]) :- 
    random(R), 
    add_random_keys(L,L1). 
 strip_keys([],[]). 
 strip_keys([_-X|L],[X|L1]) :- strip_keys(L,L1). 
take(List, N, Prefix) :-
    length(List, Len),
    (   Len =< N
    ->  Prefix = List
    ;   length(Prefix, N),
        append(Prefix, _, List)
    ).

combination(0, _, []) :- 
    !.
combination(N, L, [V|R]) :-
    N > 0,
    NN is N - 1,
    unknown(V, L, Rem),
    combination(NN, Rem, R).

unknown(X,[X|L],L).
unknown(X,[_|L],R) :- 
    unknown(X,L,R).
split([H,M,N,O,P|T], H, T).

split([H,M,N,O,P|T], X, [H,M,N,O,P|Y]) :-
    split(T, X, Y).
printcards([A,B,C|R]):- 
print(A), 
nl,
print(B), 
nl,
print(C).

printcards([A,B,C|R],M):- 
print(A), 
nl,
print(B), 
nl,
print(C).
.
pokertwoplayer(Hand):- 
	  random_permutation([(clubs,2),(clubs,3),(clubs,4),(clubs,5),(clubs,6),(clubs,7),(clubs,8),(clubs,9),(clubs,10),(clubs,jack),(clubs,queen),(clubs,king),(clubs,ace),(spades,2),(spades,3),(spades,4),
(spades,5),(spades,6),(spades,7),(spades,8),(spades,9),(spades,10),(spades,jack),
(spades,queen),(spades,king),(spades,ace),(hearts,2),(hearts,3),(hearts,4),(hearts,5),
(hearts,6),(hearts,7),(hearts,8),(hearts,9),(hearts,10),(hearts,jack),(hearts,queen),(hearts,king),(hearts,ace),
(diamonds,2),(diamonds,3),(diamonds,4),(diamonds,5),(diamonds,6),(diamonds,7),
(diamonds,8),(diamonds,9),(diamonds,10),(diamonds,jack),(diamonds,queen),(diamonds,king),
(diamonds,ace)],L3),
		take(L3,5,L1),
		split(L3,_,R),
		take(R,5,L2),
		print("Randomly shuffling of the cards is done"),
		nl,
		print("Hand One:"),
		nl,
		printcards(L1),
		nl,
		print("Hand Two:"),
		nl,
		printcards(L2),
		nl,
		take(L1,3,M),
		take(L2,3,N),
		print("Considering only the three cards: "),
		nl,
		print("The probability for the hand 1 is :"),
		findall(X,(combination(2,R,Ans),append(M,Ans,Z),qsortl(Z, Sorted_Hand),hand_value(Sorted_Hand, Hvalue),calculateprobability(Hvalue,X)),List),
		list_sum(List,Sum),
		length(List,X),
		Prob1 is Sum/X,
		print(Prob1),
		nl,
		print("The probability for the Hand 2 is :"),
		findall(X1,(combination(2,R,Ans1),append(N,Ans1,Z1),qsortl(Z1, Sorted_Hand_1),hand_value(Sorted_Hand_1, Hvalue_1),calculateprobability(Hvalue_1,X1)),List1),
		list_sum(List1,Sum1),
		length(List1,X1),
		Prob2 is Sum1/X,
		print(Prob2),
		better_poker_hand(L1,L2,Hand).		
list_sum([],0).

list_sum([Head|Tail], TotalSum):-
list_sum(Tail, Sum1),
TotalSum is Head+Sum1.

better_poker_hand(Hand1,Hand2,Hand):- 
      qsortl(Hand1, Sorted_Hand1),
      qsortl(Hand2, Sorted_Hand2), 
      hand_value(Sorted_Hand1, Hvalue1), 
      hand_value(Sorted_Hand2,Hvalue2),
	  nl,
	  nl,
	  print("With all five cards in consideration"),
	  nl,
	  print("Hand 1 : "),
	  print(Sorted_Hand1),
	  nl,
	  print("Hand 2 : "),
	  print(Sorted_Hand2),
	  nl,
	  print("For Hand One the probability with 5 cards is : "),
	  calculateprobability(Hvalue1),
	  print("For Hand Two the probability with 5 cards is : "),
	  calculateprobability(Hvalue2),
      beats(Hvalue1,Hvalue2,Hwinner),
			(Hwinner = Hvalue1, Hand = Sorted_Hand1; Hwinner = Hvalue2, Hand = Sorted_Hand2; Hwinner = tie,
         tiebreak(Hvalue1, Sorted_Hand1,Sorted_Hand2, SortedWinner),
				 (SortedWinner = left, Hand = Hand1; SortedWinner = right, Hand = Hand2)
      ),
	  print("The winner after considering all five cards is: ").