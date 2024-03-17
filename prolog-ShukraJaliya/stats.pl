


%calulating sum of list values
sum_list([], 0).

sum_list([Head | Tail], Sum) :-
    sum_list(Tail, SumOfTail),
    Sum is Head + SumOfTail.

%calculating length of list
list_length([], 0).  % The length of an empty list 

list_length([_ | Tail], Length) :-
    list_length(Tail, LengthOfTail),
    Length is LengthOfTail+ 1.

%caculating mean
mean([], 0). %if least is empty

mean(List, Mean) :- 
    sum_list(List, Sum),
    list_length(List, Length),   
    (Length > 0 -> Mean is Sum / Length ; Mean is 0). % if length is greater than 0 (;) else mean is 0

%calculating the differenc (x- xmean)^2
squared_diffs([], _, []). % base case 
squared_diffs([X], Mean, [D]) :- % single element
    D is (X - Mean)^2.
    
squared_diffs([H|T], Mean, [D|Ds]) :- 
    D is (H - Mean)^2,  
    squared_diffs(T, Mean, Ds).

%calulating the th difference (x-xmean)
diffs([], _, []).
diffs([X], Mean, [D]) :- % single element
    D is (X - Mean).
diffs([Head | Tail], Mean, [D|Ds]):- %convarinace without suaring)
    D is Head - Mean,
    diffs(Tail, Mean, Ds).

%Calulating Standard Deviation

stddev([], 0). %base case for zero elements
stddev([_], 0). %base case for 1 ekement

stddev(List, Stddev) :-
   list_length(List, N),
   ( N > 1 ->
    mean(List, Mean),
    squared_diffs(List, Mean, SquaredDiffs),
    sum_list(SquaredDiffs, Sum),
    (Sum = 0 -> Stddev is 0;
    Stddev is sqrt(Sum / N )
    )
   ; Stddev = 0
   ).


% Base case: If both lists are empty, the sum is 0.
product([], [], 0).
product([X|Xs], [Y|Ys], Sum) :-
    P is X * Y,
    product(Xs, Ys, Rest),
    Sum is Rest +P.

%calculating alpha
regressiona([], [], 0). %base case
regressiona([_], [_], 0).
regressiona(Xvalue, Yvalue, Slope):-
    mean(Xvalue, Xmean),
    mean(Yvalue, Ymean),
    diffs(Xvalue, Xmean, XDiffs),
    diffs(Yvalue, Ymean, YDiffs),
    product(YDiffs, XDiffs, Nume), % Use products/3 for covariance
    squared_diffs(Xvalue, Xmean, XsquaredDiffs),
    sum_list(XsquaredDiffs, Sum),
    Slope is Nume /Sum.


%b = y̅ -mx̅,
regressionb([], [], 0).
regressionb([_], [_], 0).
regressionb(Xvalue, Yvalue, Intercept):-
    list_length(Xvalue, N),
    mean(Xvalue, Xmean),
    mean(Yvalue, Ymean),
    regressiona(Xvalue, Yvalue, Slope),
    Intercept is( Ymean - (Slope * Xmean)).

%calculating correation
correlation([], [], 0). %base case
correlation(Xvalue, Yvalue, R) :-
    mean(Xvalue, Xmean),
    mean(Yvalue, Ymean),
    diffs(Xvalue, Xmean, XDiffs), % Calculate differences from X mean
    diffs(Yvalue, Ymean, YDiffs), % Calculate differences from Y mean
    squared_diffs(Xvalue, Xmean, XsquaredDiffs), % Calculate squared differences for X
    squared_diffs(Yvalue, Ymean, YsquaredDiffs), % Calculate squared differences for Y
    product(XDiffs, YDiffs, Numerator), % Calculate the product of differences
    sum_list(XsquaredDiffs, SumXsquaredDiffs), % Sum of squared differences for X
    sum_list(YsquaredDiffs, SumYsquaredDiffs), % Sum of squared differences for Y
    Denominator is sqrt(SumXsquaredDiffs * SumYsquaredDiffs), % Calculate denominator
    R is Numerator / Denominator . % Compute correlation coefficient


 /* Load Data from CSV */

load_data_column(File, Header, Index, Data) :-
    setup_call_cleanup(
        open(File, read, Stream),
        (
            read_header(Stream, Header),
            parse_columns(Stream, Index, [], Data) % Start with an empty list
        ),
        close(Stream)
    ).

read_header(Stream, true) :-%read a file
    read_line_to_codes(Stream, _), !.
read_header(_, false).

parse_columns(Stream, Index, ColumnData, UpdatedColumn) :-
    (at_end_of_stream(Stream) ->
        UpdatedColumn = ColumnData % Base case for end of stream
    ;
        read_line_to_string(Stream, Line),
        split_csv_row(Line, Row),
        extract_column(Row, Index, Value),
        append(ColumnData, [Value], UpdatedColumnData),
        parse_columns(Stream, Index, UpdatedColumnData, UpdatedColumn)
    ).

read_csv_line(Stream, Line) :- %gets stream and bresk it into lnes
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes).

split_csv_row(Line, Row) :- %splits the line into rows suing a comma
    atomic_list_concat(Row, ',', Line).

extract_column(Row, Index, Value) :-% extrats he rows using indexes which will be the coloms
    nth0(Index, Row, Value).

getLines(L):- %getting the lines sat, gpa ect
    setup_call_cleanup(
    open(File, read, In),
    readData(In, L),
    close(In)
    ).

readData(In, L):- %reading the lines sat, gpa ect
  read_term(In, H, []),
  (   H == end_of_file
  ->  L = []
  ;   L = [H|T],
      readData(In,T)
  ).
   
/*
    Test File for Prolog Assignment
    
*/
