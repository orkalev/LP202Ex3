user:file_search_path(sat, '/home/os202/Desktop/LP/ass3/satsolver').
:- use_module(sat(satsolver)).

%---Task 1--
%sudoku_propagate(Instance, List).
%sudoku_propagate(+,-)
%
%Input : Sudoku insance represented using thew term Instance = sudoku(Dimension, Hints)
%Output : Unifies List with a list oa as many assignment constaints possible.
%
%Exmpele: 
%?- Instance = sudoku(2, [cell(1,1) = 1, cell(2,3) = 2,
%						cell(3,2) = 4, cell(3,4) = 3] ),
%						sudoku_propagate(Instance, List).
%
%List = [cell(1,4)=4, cell(1,3)=3, cell(1,2)=2, cell(2,2)=3,
%cell(2,1)=4, cell(2,4)=1, cell(3,1)=2, cell(3,3)=1,
%cell(4,1)=3, cell(4,2)=1, cell(4,3)=4, cell(4,4)=2 ]
%
%?- Instance = sudoku(3, [ cell(1,1) = 1, cell(4,1) = 5, cell(6,1) = 3,
%						cell(8,1) = 9, cell(1,2) = 3, cell(6,2) = 8,
%						cell(1,3) = 9, cell(4,3) = 4, cell(5,3) = 6,
%						cell(6,3) = 7, cell(3,4) = 9, cell(7,4) = 6,
%						cell(8,4) = 8, cell(3,5) = 2, cell(5,5) = 1,
%						cell(7,5) = 5, cell(9,5) = 3, cell(1,6) = 6,
%						cell(7,6) = 4, cell(3,7) = 3, cell(9,7) = 5,
%						cell(1,8) = 5, cell(3,8) = 7, cell(4,8) = 9,
%						cell(6,9) = 6, cell(7,9) = 8]),
%
%sudoku_propagate(Instance, List).
%List = [cell(5,1)=2, cell(7,1)=7, cell(4,2)=1,
%		cell(5,2)=9, cell(7,2)=2, cell(8,5)=7]
	
%The main pradicat get all the hints the Dimension and return the list with the solution
sudoku_propagate(sudoku(Dim,Hints),List) :-
    SizeOfMatrix is Dim*Dim,																%Get the Size of the Row,Col and Box
    findall(
    	cell(I,J) = _V,
    	(between(1,SizeOfMatrix,I),between(1,SizeOfMatrix,J)),
    	Matrix),			 																%build the matix board
   	findall(
   		cell(I,J) = Struct1,
   		(between(1,SizeOfMatrix,I),between(1,SizeOfMatrix,J),buildPropStruct(Dim,SizeOfMatrix,Struct1,I,J))	
   		,PropStruct),																		%bulild the propagate struct the tamplete is:
   																							% "cell(I,J)=[cell(I1,J1) = V,...."
   	insertHintsToMatrix(Hints,Matrix), 														%Update the matrix (the borad) with the hints.
   	insertHintsToPropStruct(Hints,PropStruct),												%Update the propagate struct with the hints
   	removeHintsFromPropStruct(Hints,PropStruct,PropStruct1),								%The pradicat will remove all the hints from the propagate truct
   	solve(Matrix,PropStruct1,Dim,SizeOfMatrix,List,_ListExp),								%The solve pradicat is the backtracknight recursion pradicat
    true.

%The pradicat will insert all the hints that we get to the propagate struct
insertHintsToPropStruct([],_).						%Base case, the hints list is empty

insertHintsToPropStruct([Cell|Hints],PropStruct):-
	link(Cell,PropStruct),
	insertHintsToPropStruct(Hints,PropStruct).

%The pradicat will insert all the hints that we get to the materix
insertHintsToMatrix([],_).							%Base case, the hints list is empty

insertHintsToMatrix([cell(I,J) = V|Hints],Matrix) :- 
    select(cell(I,J) = _,Matrix,cell(I,J) = V,Matrix),
    insertHintsToMatrix(Hints,Matrix).

%The pradicat will build the propagate struct, all the element in the struct will look like,
%[call(I,J)=[cell(I1,J1) = V1,....,],
%.
%.
%.
%[cell(In,Jn)=[cell(Ik,Jk) = Vk,.....,]]]
%The idea is that for every cell(I,J) will have all the cell in the matrix that is relevent to him (row,col,box) 

buildPropStruct(Dim,N,StructEnd,Row,Col):-
	findall(
		cell(Row,J) = _,
		between(1,N,J),
		StructRow),							%The cell that in row 'Row'
	findall(
		cell(I,Col) = _,
		between(1,N,I),
		StructCol),							%The cell that in col 'Col'
	append(StructRow,StructCol,Struct),					
	Row1 is Row-1,
	Col1 is Col-1,
	Row2 is Row1 mod Dim,
	Col2 is Col1 mod Dim,
	RowStart is Row-Row2,
	ColStart is Col-Col2,
	RowEnd is RowStart+Dim,
	ColEnd is ColStart+Dim,
	RowEnd1 is RowEnd-1,
	ColEnd1 is ColEnd-1,				%In this point RowStart and ColStart equale to the cell that in the laft-high corner of the relevent box,
										%The RowEnd1 and ColEnd1 equale to the cell that in the right-low corner og hte relvent box.
	findall(
		cell(RowBox,ColBox) = _,
		(between(RowStart,RowEnd1,RowBox),between(ColStart,ColEnd1,ColBox)),
		StructBox),						%The cell in the relevent box
	append(Struct,StructBox,StructEnd).	


%The pradicat will get 'Cell' and update all the propagate struct with thw value of the cell
link(_,[]) :- true,!.				%Base case

link(Cell,[cell(_,_) = List|T]):-
	linkForCell(Cell,List),			%Update one cell hints in the propagate struct
	link(Cell,T),!.

%The pradicat will update the cells that related to the cell that we get
linkForCell(_,[]) :- true,!.
linkForCell(cell(I,J) = V,[cell(I,J) = V|T]) :-
	linkForCell(cell(I,J) = V,T).

linkForCell(cell(I,J) = V,[cell(I1,J1) = _|T]) :-
	I == I1, J \= J1,
	linkForCell(cell(I,J) = V,T).

linkForCell(cell(I,J) = V,[cell(I1,J1) = _|T]) :-
	I \= I1, J == J1,
	linkForCell(cell(I,J) = V,T).

linkForCell(cell(I,J) = V,[cell(I1,J1) = _|T]) :-
	I \= I1, J \= J1,
	linkForCell(cell(I,J) = V,T).

%The pradicat that solve the matrix by using backtracknight recursion.
solve(Matrix,PropStruct,Dim,SizeOfMatrix,[cell(I,J) = V|ListTail],[Ex|Exs]) :-
	cellToAdd(PropStruct,SizeOfMatrix,cell(I,J) = V,ListNums,ListProcForNum),   			%Check if there is a cell to add (there is enough hints to know what is the value of the cell )  			
	nonvar(ListProcForNum),
	propagate_list(ListProcForNum,ListProcForNum2),
	sort(ListProcForNum2,ListProcForNum3),
	Ex = [ListProcForNum3 -> [cell(I,J) = V]],								%Add the cell and the propagate to a list
	numlist(1,SizeOfMatrix,FullList),										
	subtract(FullList,ListNums,Num),
	length(Num,1),!,
	Num = [V|_],															%V is the value of the cell
	link(cell(I,J) = V,PropStruct),		
	solve(Matrix,PropStruct,Dim,SizeOfMatrix,ListTail,Exs).
solve(_,_,_,_,[],[]).	

cellToAdd([],_,_,_,_) :- true,!.
cellToAdd([cell(I,J) = L|T],Size,cell(I1,J1) = _,List,ListProcForNum):-
	%writeln(Size),fail,
	findall(V,(member(cell(_,_) = V,L),nonvar(V)),List1),
	sort(List1,List2),
	Size1 is Size-1,
	(length(List2,Size1)-> I1 = I,J1 = J,List2 = List,L = ListProcForNum,!);
	cellToAdd(T,Size,cell(I1,J1) = _,List,ListProcForNum).

removeHintsFromPropStruct([],PropStructF,PropStructF).
removeHintsFromPropStruct([cell(I,J) = _|T],PropStruct,PropStructF) :-
	%writeln(PropStruct),writeln(''),writeln(cell(I,J)),writeln(''),
	select(cell(I,J) = _,PropStruct,PropStruct1),
	removeHintsFromPropStruct(T,PropStruct1,PropStructF).
 %
 %
 %----------------------------------------- Task 2 ---------------------------------------
 %
 %

%sudoku_propagate_explain(Instance,Explain).
%
%sudoku_propagate(+,-).
%
%Input: Instancee of sudoku Hints and Dimension
%Output: List of Explain od how we solve the Sudoku
%
%Idea: I use the sudoku_propagate pradicat and update the pradicat to save the answer

sudoku_propagate_explain(sudoku(Dim,Hints),ListExp) :-
    SizeOfMatrix is Dim*Dim,																%Get the Size of the Row,Col and Box
    findall(
    	cell(I,J) = _V,
    	(between(1,SizeOfMatrix,I),between(1,SizeOfMatrix,J)),
    	Matrix),			 																%build the matix board
   	findall(
   		cell(I,J) = Struct1,
   		(between(1,SizeOfMatrix,I),between(1,SizeOfMatrix,J),buildPropStruct(Dim,SizeOfMatrix,Struct1,I,J))	
   		,PropStruct),																		%bulild the propagate struct the tamplete is:
   																							% "cell(I,J)=[cell(I1,J1) = V,...."
   	insertHintsToMatrix(Hints,Matrix), 														%Update the matrix (the borad) with the hints.
   	insertHintsToPropStruct(Hints,PropStruct),												%Update the propagate struct with the hints
   	removeHintsFromPropStruct(Hints,PropStruct,PropStruct1),								%The pradicat will remove all the hints from the propagate truct
   	solve(Matrix,PropStruct1,Dim,SizeOfMatrix,_List,ListExp),								%The solve pradicat is the backtracknight recursion pradicat
    true.

propagate_list([cell(I,J) = V|T],[cell(I,J) = V|Res]):-
	nonvar(V),
	propagate_list(T,Res).
propagate_list([cell(_I,_J) = V|T],Res):-
	var(V),
	propagate_list(T,Res).
propagate_list([],[]).

 %
 %
 %----------------------------------------- Task 3 ---------------------------------------
 %
 %

 %verify_killer(Insert,Solution,Verified).
 %verify_killer(+,+,-).
 %
 %input: Insert = killer(Hints)
 %		 Solution = list of the solution
 %		 
 %Output: Verified = killer of the solution is Sudoku-killer and list of problem id not killer.

%The main pradicat.
 verify_killer(killer(Hints),Solution,Verified):-
 	checkHintsInSolution(Hints,Solution,OK),
 	(OK==0 -> Verified = worngHints;
 	findall(
 		cell(I,J) = V,
 		(between(1,9,I),between(1,9,J)),		
 		KillerMatrix),							%First build 9*9 matrix for the sudoku table
 	insertHintsToMatrix(Solution,KillerMatrix),	%used the pradicat of task 1 to insert all the solution that we get to the matrix
 	%Build a propagate struct the for every cell(I,J) = V -> [List] , The list is all the cells that is relevnt to him,
 	%like, in is row,col,box,knight move,king move. in that way cell(I,J) = V -> [List].
 	%We know all the time the value of the current cell, and can build the Verified if needed.
  	findall(
 		cell(I,J) = V -> PropStructCell,
 		(member(cell(I,J) = V,KillerMatrix),buildPropoationStruct(KillerMatrix,PropStructCell,cell(I,J) = V)),
 		PropStructAll),															%the same idea of the last propagate struct but only hold the cell that near the current cell by one. 	
 	findall(
 		cell(I,J) = V -> PropStructCell2,
 		(member(cell(I,J) = V,KillerMatrix),buildPropoationStructForAbs(KillerMatrix,PropStructCell2,cell(I,J) = V)),
 		PropStructAbs),
 	checkForWrongAll(PropStructAll,Ans),								%Check if there is a some cells that not valid for the first propagate struct
 	length(Ans,LengthAnsAll),				
 	(LengthAnsAll > 0 -> Verified = Ans,!;								%Check if we get some cells, if and length is 0, there is not problem at the first struct
 		checkForWrongAbs(PropStructAbs,Verified))).						%If not problem detected at the first propagate struct then we check the secend one,
 																		%we return in the Verified the answer from the secend check
 checkHintsInSolution([],_,1).
 checkHintsInSolution([cell(I,J) = V|RestHints],Solution,OK):-
 		(member(cell(I,J)=V,Solution)->checkHintsInSolution(RestHints,Solution,OK);
 			OK = 0).

 	
 	%build all the condition for the first propagate struct														
 buildPropoationStruct(KillerMatrix,PropStruct,cell(I,J) = _V):-
 		findall(
 			cell(I1,J1) = V1,
 			(member(cell(I1,J1) = V1,KillerMatrix),I == I1,J \= J1),		
 			PropStructRow),
 		findall(
 			cell(I1,J1) = V2,
 			(member(cell(I1,J1) = V2,KillerMatrix),J == J1,I \= I1),		
 			PropStructCol),
 		I1 is I-1,
		J1 is J-1,
		I2 is I1 mod 3,		
		J2 is J1 mod 3,		
		RowStart is I-I2,
		ColStart is J-J2,
		RowEnd is RowStart+3,		
		ColEnd is ColStart+3,		
		RowEnd1 is RowEnd-1,
		ColEnd1 is ColEnd-1,
		findall(
			cell(I3,J3) = V3,
 			(member(cell(I3,J3) = V3,KillerMatrix),J \= J3,I \= I3,between(RowStart,RowEnd1,I3),between(ColStart,ColEnd1,J3)),
 			PropStructBox),													%The box
		findall(
			cell(I4,J4) = V4,
			(member(cell(I4,J4) = V4,KillerMatrix),knight_move1(I,J,I4,J4),\+member(cell(I4,J4) = _,PropStructBox)),
			PropStructKnight1),    											%4 move of knight      
		findall(
			cell(I4,J4) = V4,
			(member(cell(I4,J4) = V4,KillerMatrix),knight_move2(I,J,I4,J4),\+member(cell(I4,J4) = _,PropStructBox)),
			PropStructKnight2),												%4 move of knight
		findall(
			cell(I4,J4) = V4,
			(member(cell(I4,J4) = V4,KillerMatrix),king_move(I,J,I4,J4),\+member(cell(I4,J4) = _,PropStructBox)),
			PropStructKing),												% the move of the king 									
 		flatten([PropStructRow,PropStructCol,PropStructBox,PropStructKnight1,PropStructKnight2,PropStructKing],PropStruct).

 	%The pradicat that build the secend propagate struct
 	buildPropoationStructForAbs(KillerMatrix,PropStruct,cell(I,J) = _V):-
 			findall(
 				cell(I1,J1) = V1,
 				(member(cell(I1,J1) = V1,KillerMatrix),nearCol(I,J,I1,J1)),
 				PropStructNearCol),
 			findall(
 				cell(I1,J1) = V1,
 				(member(cell(I1,J1) = V1,KillerMatrix),nearRow(I,J,I1,J1)),
 				PropStructNearRow),
 			flatten([PropStructNearCol,PropStructNearRow],PropStruct).

 		%The pradicat that check of the first vaild of the first struct  	
 	checkForWrongAll([],[]).
 	checkForWrongAll([cell(I,J) = V ->PropStructAll|Rest],VerifiedAll) :-
 			findall(
 				cell(I1,J1) = V1,
 				(member(cell(I1,J1) = V1,PropStructAll),V1 == V),
 				VerifiedAll2),
 			(length(VerifiedAll2,0) -> checkForWrongAll(Rest,VerifiedAll);
			VerifiedAll2 = [Tmp|_],VerifiedAll = [cell(I,J) = V,Tmp],!).

 	%The pradicat that check of the secend  vaild of the struct	
 	checkForWrongAbs([],VerifiedAbs) :-VerifiedAbs = killer.
 	checkForWrongAbs([cell(I,J) = V ->PropStructAbs|Rest],VerifiedAbs) :-
 			findall(
 				cell(I1,J1) = V1,
 				(member(cell(I1,J1) = V1,PropStructAbs),T is abs(V-V1),T == 1),
 				VerifiedAbs2),
 			(length(VerifiedAbs2,0) -> checkForWrongAbs(Rest,VerifiedAbs);
			VerifiedAbs2 = [Tmp|_],VerifiedAbs = [cell(I,J) = V,Tmp],!).
			
 	%The condition of the knight	
 	knight_move1(I,J,I1,J1) :-
			RowAbs is abs(I - I1),
			ColAbs is abs(J - J1),
			RowAbs == 2,
			ColAbs == 1.

	%The condition of the knight
	knight_move2(I,J,I1,J1) :-
			RowAbs is abs(I - I1),
			ColAbs is abs(J - J1),
			RowAbs == 1,
			ColAbs == 2.

	%The condition of the king 
	king_move(I,J,I1,J1) :-
			RowAbs is abs(I - I1),
			ColAbs is abs(J - J1),
			RowAbs == 1,
			ColAbs == 1.

	nearCol(I,J,I1,J1) :-
			Col is abs(J-J1),
			Col == 1,
			I == I1.

	nearRow(I,J,I1,J1) :-
			Row is abs(I-I1),
			Row == 1,
			J == J1.


%
%
%----------------------------------------- Task 4 ---------------------------------------
%
%

%encode_killer(Instance,Map,CNF).
%
%encode_killer(+,-,-).
%
%Input: Instance =killer(Hints)
%
%Output: Map - unifies Map with a representation of the instance as a list of 81 terms of the form cell(I,J) = Value,
%where I and J are Prolog intergers representing the index of the cell, and Value represents an unlnown onteger 
%value between 1 and 9 encoded in CNF,
%A call to the predicate instantiates CNF with a CNF formula such that the satisfying assignments for CNF 
%correspond to solutions
%of the Killer sudoku instance Instance and bind the variables in MAp to represent the corresponding integer values.

encode_killer(killer(Hints),Map,CNF) :-
	build_map(Map),	                  %Create the Map, every value in the struct will be represent by 9 bits in unary_direct mode
	insert_Hints_For_CNF(Hints,Map),
	encode_unary_direct_exactly_one(Map,Map,CNF_unary_direct),
	encode_row_cnf(1,Map,[],CNF_row),
	append(CNF_unary_direct,CNF_row,CNFT),
	encode_col_cnf(1,Map,[],CNF_col),
	append(CNFT,CNF_col,CNF1),
	encode_box_cnf([(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)],Map,[],CNF_Box),
	append(CNF1,CNF_Box,CNF2),
	findall(
		(I,J),
		member(cell(I,J)=_,Map),
		AllCells),
	encode_knight_move_cnf(AllCells,Map,[],CNF_knife),
	append(CNF2,CNF_knife,CNF3),
	encode_king_move_cnf(AllCells,Map,[],CNF_king),
	append(CNF3,CNF_king,CNF4),
	encode_near_by_abs_diff_cnf(AllCells,Map,[],CNF_ABS),
	append(CNF4,CNF_ABS,CNF).
	
%build the Map with unary direct representation
build_map(Map) :-
	findall(
		cell(I,J) = [_X1,_X2,_X3,_X4,_X5,_X6,_X7,_X8,_X9],		
		%cell(I,J) = [X1,X2,X3,X4],						
		(between(1,9,I),between(1,9,J)),				
		Map).

%insert the hints to the map	
insert_Hints_For_CNF([],_).
insert_Hints_For_CNF([cell(I,J)=V|Rest],Map):-
	insert_H_For_CNF(cell(I,J) = V,Map),
	insert_Hints_For_CNF(Rest,Map).

%The pradicat get a cell(I,J) = V and return the cell(I,J) = V in unary direct representation
insert_H_For_CNF(cell(I,J) = 1,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [1,-1,-1,-1,-1,-1,-1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 2,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,1,-1,-1,-1,-1,-1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 3,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,1,-1,-1,-1,-1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 4,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,-1,1,-1,-1,-1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 5,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,-1,-1,1,-1,-1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 6,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,-1,-1,-1,1,-1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 7,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,-1,-1,-1,-1,1,-1,-1],Map).

insert_H_For_CNF(cell(I,J) = 8,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,-1,-1,-1,-1,-1,1,-1],Map).

insert_H_For_CNF(cell(I,J) = 9,Map) :-
	select(cell(I,J) = _,Map,cell(I,J) = [-1,-1,-1,-1,-1,-1,-1,-1,1],Map).



%encode the CNF for the 5 rule of the assignment
encode_near_by_abs_diff_cnf([],_,CNF,CNF).
encode_near_by_abs_diff_cnf([(I,J)|RestCells],Map,InCNF,BackCNF) :-
	where_the_near_cells(CellsMember,Map,(I,J)),
	build_value_unary_numbers_for_cells(CellsMember,Map,CellsMemberValueUnary),
	member(cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9],Map),
	diff_by_one([X1,X2,X3,X4,X5,X6,X7,X8,X9],CellsMemberValueUnary,AbsCNF),
	append(InCNF,AbsCNF,OutCNF),
	encode_near_by_abs_diff_cnf(RestCells,Map,OutCNF,BackCNF).

%encode the CNF for the 4 rule of the assignment, the king move
encode_king_move_cnf([],_,CNF,CNF).
encode_king_move_cnf([(I,J)|RestCells],Map,InCNF,BackCNF) :-
	where_the_king_move(KingMember,Map,(I,J)),
	build_value_unary_numbers_for_cells(KingMember,Map,KingValueUnary),
	member(cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9],Map),
	diff([X1,X2,X3,X4,X5,X6,X7,X8,X9],KingValueUnary,KingCNF),
	append(InCNF,KingCNF,OutCNF),
	encode_king_move_cnf(RestCells,Map,OutCNF,BackCNF).

%encode the CNF for the 3 rule of the assignment, the knight move
encode_knight_move_cnf([],_,CNF,CNF).
encode_knight_move_cnf([(I,J)|RestCells],Map,InCNF,BackCNF) :-
	where_the_knight_move(KnightMember,Map,(I,J)),
	build_value_unary_numbers_for_cells(KnightMember,Map,KnightValueUnary),
	member(cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9],Map), 			
	diff([X1,X2,X3,X4,X5,X6,X7,X8,X9],KnightValueUnary,KnightCNF),
	append(InCNF,KnightCNF,OutCNF),
	encode_knight_move_cnf(RestCells,Map,OutCNF,BackCNF).

%encode the CNF for the BOX that every cell is diffrent 
encode_box_cnf([],_,CNF,CNF).
encode_box_cnf([(I,J)|RestCells],Map,InCNF,BackCNF) :-
	who_in_the_box(BoxMember,Map,(I,J)),
	build_value_unary_numbers_for_cells(BoxMember,Map,BoxsValueUnary),
	transpose(BoxsValueUnary,BeforeCNF),
	exactly_one(BeforeCNF,BoxCNF),
	append(InCNF,BoxCNF,OutCNF),
	encode_box_cnf(RestCells,Map,OutCNF,BackCNF).

%encode the CNF for the ROW that every cell is diffrent
encode_row_cnf(I,_,CNF,CNF) :- I > 9.
encode_row_cnf(I,Map,InCNF,BackCNF) :-
	I < 10,
	I1 is I+1,
	who_in_the_row(RowMember,Map,I),
	build_value_unary_numbers_for_cells(RowMember,Map,RowsValueUnary),
	transpose(RowsValueUnary,BeforeCNF),
	exactly_one(BeforeCNF,RowCNF),
	append(InCNF,RowCNF,OutCNF),
	encode_row_cnf(I1,Map,OutCNF,BackCNF).

%encode the col that every cell is diffrent
encode_col_cnf(J,_,CNF,CNF) :- J > 9.
encode_col_cnf(J,Map,InCNF,BackCNF) :-
	J < 10,
	J1 is J+1,
	who_in_the_col(ColMember,Map,J),
	build_value_unary_numbers_for_cells(ColMember,Map,ColsValueUnary),
	transpose(ColsValueUnary,BeforeCNF),
	exactly_one(BeforeCNF,ColCNF),
	append(InCNF,ColCNF,OutCNF),
	encode_col_cnf(J1,Map,OutCNF,BackCNF).

%reutrn the cell that in the col
where_the_near_cells(CellsMember,Map,(I,J)) :-
		findall(
 				cell(I1,J1),
 				(member(cell(I1,J1) = _,Map),nearCol(I,J,I1,J1)),
 				MemberNearCol),
 		findall(
 				cell(I1,J1),
 				(member(cell(I1,J1) = _,Map),nearRow(I,J,I1,J1)),
 				MemberNearRow),
 		flatten([MemberNearCol,MemberNearRow],CellsMember).
 
%create a list of (cell(I,J) that represent all the cells in the roe of I)
where_the_king_move(KingMember,Map,(I,J)) :- 
 	findall(
 		cell(I1,J1),
 		(member(cell(I1,J1) = _,Map),king_move(I,J,I1,J1)),
 		KingMember).

%create a list of (cell(I,J) that represent all the cells in the roe of I)
where_the_knight_move(KnifeMember,Map,(I,J)) :- 
 	findall(
 		cell(I1,J1),
 		(member(cell(I1,J1) = _,Map),knight_move1(I,J,I1,J1)),
 		KnifeMember1),
 	findall(
 		cell(I2,J2),
 		(member(cell(I2,J2) = _,Map),knight_move2(I,J,I2,J2)),
 		KnifeMember2),
		flatten([KnifeMember1,KnifeMember2],KnifeMember).

%return the cell in the box, there is a 9 boxs, in 9*9 board  	
who_in_the_box(BoxMember,Map,(I,J)) :-
	Iend is I+2,
	Jend is J+2,
 	findall(
 		cell(I1,J1),
 		(member(cell(I1,J1) = _,Map),between(I,Iend,I1),between(J,Jend,J1)),
 		BoxMember).

%create a list of (cell(I,J) that represent all the cells in the roe of I)
who_in_the_col(ColMember,Map,J) :- 
 	findall(
 		cell(I1,J1),
 		(member(cell(I1,J1) = _,Map),J == J1,between(1,9,J1)),
 		ColMember).

%create a list of (cell(I,J) that represent all the cells in the roe of I)
who_in_the_row(RowMember,Map,I) :- 
 	findall(
 		cell(I1,J1),
 		(member(cell(I1,J1) = _,Map),I == I1,between(1,9,J1)),
 		RowMember).

build_value_unary_numbers_for_cells([],_,[]).
build_value_unary_numbers_for_cells([cell(I,J)|RestRowMember],Map,[[X1,X2,X3,X4,X5,X6,X7,X8,X9]|RestCellVal]) :-
	member(cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9],Map),
	build_value_unary_numbers_for_cells(RestRowMember,Map,RestCellVal).

%Use the transpose from the next url: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([H|Hs], Ts) :-
    transpose(H, [H|Hs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%The pradict will encode the values for every call, that only one bit at value will be 1 and the rest will be 0.
encode_unary_direct_exactly_one([],_,[]).
encode_unary_direct_exactly_one([cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9]|RestMap],Map,
											[[X1,X2,X3,X4,X5,X6,X7,X8,X9],[-X1,-X2],[-X1,-X3],[-X1,-X4],[-X1,-X5],[-X1,-X6],[-X1,-X7],[-X1,-X8],[-X1,-X9],
											[-X2,-X3],[-X2,-X4],[-X2,-X5],[-X2,-X6],[-X2,-X7],[-X2,-X8],[-X2,-X9],[-X3,-X4],[-X3,-X5],[-X3,-X6],[-X3,-X7],[-X4,-X8],[-X3,-X9],
											[-X4,-X5],[-X4,-X6],[-X4,-X7],[-X4,-X8],[-X4,-X9],[-X5,-X6],[-X5,-X7],[-X5,-X8],[-X5,-X9],[-X6,-X7],[-X6,-X8],[-X6,-X9],[-X7,-X8],
											[-X7,-X9],[-X8,-X9]|RestCNF]) :-
	member(cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9],Map),
	encode_unary_direct_exactly_one(RestMap,Map,RestCNF).

%The pradicat will implemnt the exetly one we leran in class.
exactly_one([],[]).
exactly_one([[X1,X2,X3,X4,X5,X6,X7,X8,X9]|RestMap],
											[[X1,X2,X3,X4,X5,X6,X7,X8,X9],[-X1,-X2],[-X1,-X3],[-X1,-X4],[-X1,-X5],[-X1,-X6],[-X1,-X7],[-X1,-X8],[-X1,-X9],
											[-X2,-X3],[-X2,-X4],[-X2,-X5],[-X2,-X6],[-X2,-X7],[-X2,-X8],[-X2,-X9],[-X3,-X4],[-X3,-X5],[-X3,-X6],[-X3,-X7],[-X3,-X8],[-X3,-X9],
											[-X4,-X5],[-X4,-X6],[-X4,-X7],[-X4,-X8],[-X4,-X9],[-X5,-X6],[-X5,-X7],[-X5,-X8],[-X5,-X9],[-X6,-X7],[-X6,-X8],[-X6,-X9],[-X7,-X8],
											[-X7,-X9],[-X8,-X9]|RestCNF]) :-
exactly_one(RestMap,RestCNF).
%The pradicat will get two number represented in unary direct and return CNF that thay are diffrent
diff(_,[],[]).
diff([X1,X2,X3,X4,X5,X6,X7,X8,X9],[[Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9]|Rest],[[-X1,-Y1],[-X2,-Y2],[-X3,-Y3],[-X4,-Y4],[-X5,-Y5],[-X6,-Y6],[-X7,-Y7],[-X8,-Y8],[-X9,-Y9]|RestCNF]):-
	diff([X1,X2,X3,X4,X5,X6,X7,X8,X9],Rest,RestCNF).
%The pradicat will get a two number represent in unary direct and return a CNF for the 2 numbers that the abs(X-Y)>1
diff_by_one(_,[],[]).
diff_by_one([X1,X2,X3,X4,X5,X6,X7,X8,X9],[[Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9]|Rest],[[-X1,-Y2],[-X2,-Y1],[-X2,-Y3],[-X3,-Y2],[-X3,-Y4],[-X4,-Y3],[-X4,-Y5],[-X5,-Y4],[-X5,-Y6],[-X6,-Y5],[-X6,-Y7],[-X7,-Y6],[-X7,-Y8],[-X8,-Y7],[-X8,-Y9],[-X9,-Y8]|RestCNF]):-
	diff_by_one([X1,X2,X3,X4,X5,X6,X7,X8,X9],Rest,RestCNF).

%
%
%----------------------------------------- Task 5 ---------------------------------------
%
%


%decode_killer(Map,Solution).
%
%encode_killer(+,-).
%
%Input: Map 
%
%Output: Solution

decode_killer(Map,Solution):-
	build_solution_matrix(Solution),
	decode(Map,Solution).


build_solution_matrix(Solution):-
 	findall(
 		cell(I,J) = _V,
 		(between(1,9,I),between(1,9,J)),		
 		Solution).

decode([],[]).
decode([cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9]|RestMap],[cell(I,J) = V|RestSol]):-
	decodeCell([X1,X2,X3,X4,X5,X6,X7,X8,X9],V),
	decode(RestMap,RestSol).

decodeCell([1,-1,-1,-1,-1,-1,-1,-1,-1],1).
decodeCell([-1,1,-1,-1,-1,-1,-1,-1,-1],2).
decodeCell([-1,-1,1,-1,-1,-1,-1,-1,-1],3).
decodeCell([-1,-1,-1,1,-1,-1,-1,-1,-1],4).
decodeCell([-1,-1,-1,-1,1,-1,-1,-1,-1],5).
decodeCell([-1,-1,-1,-1,-1,1,-1,-1,-1],6).
decodeCell([-1,-1,-1,-1,-1,-1,1,-1,-1],7).
decodeCell([-1,-1,-1,-1,-1,-1,-1,1,-1],8).
decodeCell([-1,-1,-1,-1,-1,-1,-1,-1,1],9).

%
%
%----------------------------------------- Task 6 ---------------------------------------
%
%

%legal_killer(Instance,Islegal).
%
%legal_killer(Instance,Islegal).
%
%Input: Instance = killer(Hints).
%
%Output: IsLegal - return legal if there is one solution to killer Sudoku, else return problem

%The pradicat is use the satMulti to know if there is more then 1 solution, if there is more then 1 solution it's call the 
%the find multi pradicat to find where there is more then one solution and return it.
legal_killer(Instance, IsLegal) :-
	encode_killer(Instance,Map,Cnf),
	satMulti(Cnf,2,SolCount,_Time),
	(SolCount == 1 -> IsLegal = legal;
		find_multi(Map,IsLegal)).

find_multi([cell(I,J) = [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5],[X6,Y6],[X7,Y7],[X8,Y8],[X9,Y9]]|Rest],IsLegal) :-
	decodeCell1(cell(I,J) = [X1,X2,X3,X4,X5,X6,X7,X8,X9],cell(I,J) = X),
	decodeCell1(cell(I,J) = [Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9],cell(I,J) = Y),
	(X \= Y -> IsLegal = [cell(I,J) = X,cell(I,J) = Y];
		find_multi(Rest,IsLegal)).



decodeCell1(cell(I,J) = [1,-1,-1,-1,-1,-1,-1,-1,-1],cell(I,J) = 1).
decodeCell1(cell(I,J) = [-1,1,-1,-1,-1,-1,-1,-1,-1],cell(I,J) = 2).
decodeCell1(cell(I,J) = [-1,-1,1,-1,-1,-1,-1,-1,-1],cell(I,J) = 3).
decodeCell1(cell(I,J) = [-1,-1,-1,1,-1,-1,-1,-1,-1],cell(I,J) = 4).
decodeCell1(cell(I,J) = [-1,-1,-1,-1,1,-1,-1,-1,-1],cell(I,J) = 5).
decodeCell1(cell(I,J) = [-1,-1,-1,-1,-1,1,-1,-1,-1],cell(I,J) = 6).
decodeCell1(cell(I,J) = [-1,-1,-1,-1,-1,-1,1,-1,-1],cell(I,J) = 7).
decodeCell1(cell(I,J) = [-1,-1,-1,-1,-1,-1,-1,1,-1],cell(I,J) = 8).
decodeCell1(cell(I,J) = [-1,-1,-1,-1,-1,-1,-1,-1,1],cell(I,J) = 9).

%
%
%----------------------------------------- Task 7 ---------------------------------------
%
%

%generrate_killer(K,Hints).
%
%generrate_killer(+,-).
%
%Input: K - intger that we unifies to 'Hints' K hints such that killer(Hints) is legal .
%

%The pradicat will get the K intget number build that K's Hints by using the createHints pradicat and send it to
%killer_legal to answer, when legal_killer is return legal, the pradicat is reutrn the Hints and countinue with backtracknight
generate_killer(K,Hints):-
	createHints(K,[],Hints,[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]),
	legal_killer(killer(Hints),legal),writeln('Bla').

createHints(0,Hints,Hints,_,_,_).
createHints(K,Acc,Hints,List1,List2,List3):-
	K > 0,
	K1 is K-1,
	append(Acc,[cell(I,J) = V],Next),
	random_select(I,List1,Rest1),random_select(J,List2,Rest2),random_select(V,List3,Rest3),
	%random_between(1,9,I),random_between(1,9,J),random_between(1,9,V),
	\+member(cell(I,J) = _,Acc),
	createHints(K1,Next,Hints,Rest1,Rest2,Rest3).


