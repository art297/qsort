/*==================================================================+
 | Name:    Qsort.                                                   |
 |===================================================================|
 | Developer: Paul M. Dorfman.                                       |
 |===================================================================|
 | Function: Sort array in place via the QuickSort algorithm.        |
 |===================================================================|
 | General:                                                          |
 |-------------------------------------------------------------------|
 | Qsort is a macro call routine designed to be invoked from within  |
 | a DATA step to sort a single-subscripted array or a parallel list |
 | of N arrays using one array as a key. The array(s) can be arrays  |
 | SAS variables or a temporary array; numeric or character. Arrays  |
 | in the list do not have to be the same type and/or item length.   |
 |                                                                   |
 | The kernel of the routine is a DATA step implementation of the    |
 | QuickSort algorithm (C.A.R. Hoare, 1960). Final ordering is done  |
 | by a single sweep of the modified insertion sort after quicksort  |
 | has reduced all array subpartitions to no more than &M items.     |
 |-------------------------------------------------------------------|
 | Performance:                                                      |
 |-------------------------------------------------------------------|
 | Run-time increases as N*log2(N), where N is the number of array   |
 | elements to be sorted. As a benchmark, a random numeric temporary |
 | array is sorted into ascending order in lt 1 CPU sec under OS/390 |
 | IBM model 9672 R36 running SAS version 8.0.                       |
 |===================================================================|
 | Usage examples:                                                   |
 |===================================================================|
 | 1. Sorting entire array A ascending (semicolon is optional):      |
 |                                                                   |
 |    %Qsort (Arr=A, Seq=a);                                         |
 |                                                                   |
 |    or simply, using Seq=a default,                                |
 |                                                                   |
 |    %Qsort (Arr=A);                                                |
 |-------------------------------------------------------------------|
 | 2. Sorting entire array B descending:                             |
 |                                                                   |
 |    %Qsort (Arr=B, Seq=d);                                         |
 |-------------------------------------------------------------------|
 | 3. Sorting elements -12345 through 12345 of array Z ascending,    |
 |    leaving the rest of the items intact;                          |
 |                                                                   |
 |    %Qsort (Arr=Z, first=-12345, last=12345);                      |
 |-------------------------------------------------------------------|
 | 4. Sorting first 100 elements of array C descending, the rest -   |
 |    ascending:                                                     |
 |                                                                   |
 |    %Qsort (Arr=C, Seq=d, last= 100);                              |
 |    %Qsort (Arr=C, Seq=a, first=101);                              |
 |-------------------------------------------------------------------|
 | 5. Sorting first half of array D ascending, second half -         |
 |    descending:                                                    |
 |                                                                   |
 |    half = int((lbound(D)+hbound(D))*.5);                          |
 |    %Qsort (Arr=D, Seq=D, last=half   );                           |
 |    %Qsort (Arr=D, Seq=A, first=half+1);                           |
 |                                                                   |
 |    or without HALF and omitting Seq=A by default:                 |
 |                                                                   |
 |    %Qsort (Arr=D, Seq=D, last= (lbound(D)+hbound(D))*.5  );       |
 |    %Qsort (Arr=D,        first=(lbound(D)+hbound(D))*.5+1);       |
 |-------------------------------------------------------------------|
 | 6. Doing the same as in 5 without the auxiliary variable HALF     |
 |    and omitting Seq=A by default:                                 |
 |                                                                   |
 |    %Qsort (Arr=D, Seq=D, last= (lbound(D)+hbound(D))*.5  );       |
 |    %Qsort (Arr=D,        first=(lbound(D)+hbound(D))*.5+1);       |
 |-------------------------------------------------------------------|
 | 7. Sorting parallel arrays A B C using array B as the key array:  |
 |                                                                   |
 |    %Qsort (Arr=A B C, By=C) ;                                     |
 |                                                                   |
 |    Illustration. The following step uses array C as a key:        |
 |    ------------                                                   |
 |    data _null_;                                                   |
 |       array a(10)    ( 9   8   7   6   5   4   3   2   1   0 );   |
 |       array b(10) $  ('a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j');   |
 |       array c(10)    ( 0   1   2   3   4   5   6   7   8   9 );   |
 |      %Qsort (Arr=A B C, By=C, Seq=D);                             |
 |       put A(*) / B(*) / C(*);                                     |
 |    run;                                                           |
 |                                                                   |
 |    It prints the following in the log:                            |
 |                                                                   |
 |    0 1 2 3 4 5 6 7 8 9                                            |
 |    j i h g f e d c b a                                            |
 |    9 8 7 6 5 4 3 2 1 0                                            |
 |                                                                   |
 |    Note: QuickSort is an unstable algorithm, i.e. if the key array|
 |    ----------------------------------------                       |
 |    has ties (duplicate keys), subordinate array elements DO NOT   |
 |                               ---------------------------------   |
 |    retain their original relative order. This behavior is similar |
 |    ------------------------------------                           |
 |    to that of Proc Sort with NOEQUALS option.                     |
 |    The parallel array feature is most useful when it is necessary |
 |    to perform and indirect sort, i.e. with a single parallel array|
 |    containing pointers to long 'records'. For instance, instead of|
 |    giving Qsort the labor of ordering 20 parallel arrays, it is   |
 |    more efficient to store the pointers of the arrays in a single |
 |    parallel array, and then finish the entire thing using a sweep |
 |    of indirect sorting.                                           |
 |                                                                   |
 |    Note: When using LB= and HB= options with parallel arrays, ALL |
 |    ----------------------------------------                       |
 |    lower array bounds must be GE LB=value. Likewise, the upper    |
 |    bounds must all be LE than HB= value. As long as the domain of |
 |    ALL indices lies within an LB= and HB, the indexing of arrays  |
 |    on the list can be arbitrary (in particular, negative).        |
 |===================================================================|
 | Arguments:                                                        |
 |-------------------------------------------------------------------|
 | Parameter  Usage     Description                                  |
 |-------------------------------------------------------------------|
 | Arr=       Required  The name of the array (or list of any number |
 |                      of parallel arrays) to be sorted. By default,|
 |                      the first array in the list becomes the key  |
 |                      array, and the rest of the arrays are permut-|
 |                      ed accordingly.                              |
 |-------------------------------------------------------------------|
 | By=        Optional  The name of the key array in the list. If the|
 |                      list consists of a single array, it is the   |
 |                      key array itself - along with the rule above.|
 |                      See usage for more details.                  |
 |-------------------------------------------------------------------|
 | Seq=       Optional  Sorting sequence. Ascending is default. To   |
 |                      specify it explicitly, set Seq=A (any case). |
 |                      Anything else will result in the array sorted|
 |                      decsending.                                  |
 |-------------------------------------------------------------------|
 | Nodupkey=  Optional  set Nodupkey=y if you want to keep only the  |
 |                      first values within a by group               |
 |-------------------------------------------------------------------|
 | LB=        Optional  The indices of the first and last array      |
 | HB=        Numeric   elements to be included into sorting. You    |
 |                      can specify any valid numeric SAS expression,|
 |                      hardcoded numeric value, or a macrovariable  |
 |                      reference resolving to any of the above. The |
 |                      values of these parms default to the lower   |
 |                      and upper bounds.                            |
 |                      Use LB= and HB= parameters if you need only  |
 |                      part of the array to be ordered, or if you   |
 |                      want different parts ordered differently,    |
 |                      which can be  achieved by issuing two or more|
 |                      consecutive calls to Qsort with LB= and HB=  |
 |                      specified accordingly (see Usage above).     |
 |-------------------------------------------------------------------|
 | M=         Optional  Tuning parm: The largest subpartition size   |
 |            Numeric   Quicksort attempts to partition further. Any |
 |                      subpartition LE &M is passed to the straight |
 |                      insertion sort. &M=1 corresponds to 'pure'   |
 |                      Quicksort working until all subpartitions    |
 |                      have been reduced to just 1 element. &M=9 is |
 |                      optimal. Variations from 5 to 15 affect the  |
 |                      sorting speed very slightly. Best advice as  |
 |                      to M= : Leave it alone at 9.                 |
 +===================================================================*/
%Macro Qsort (
  Arr = /* Parallel array name list */
  ,By = %QScan(&Arr,1,%Str( )) /* Key array name */
  ,Seq = A /* Seq=D for descending */
  ,LB = Lbound(&By) /* Lower bound to sort */
  ,HB = Hbound(&By) /* Upper bound to sort */
  ,M = 9 /* Tuning range: (1:15) */
 );
  %Local _ H I J L N P Q S T W ;

  %Macro Swap (I,J) ;
    %Local W ;
     Do ;
       %Do W = 1 %To &N ;
         &T&W      = &&A&W(&I) ;
         &&A&W(&I) = &&A&W(&J) ;
         &&A&W(&J) =  &T&W     ;
       %End ;
    End ;
  %Mend Swap ;

  %If %Upcase(&Seq) = %Upcase(A) %Then %Let Q = G ;
  %Else                                %Let Q = L ;

  %Do %Until (&&A&N EQ ) ;
    %Let N  = %Eval(&N + 1) ;
    %Local A&N ;
    %Let A&N = %Scan(&Arr,&N,%Str( )) ;
  %End ;
  %Let N = %Eval(&N - 1) ;

  %Let _ = %Substr(%Sysfunc(Ranuni(0)),3,
  %Eval(7 - %Length(&N) + 5*(%Substr(&Sysver,1,1) GT 6))) ;

  %Let H = H&_ ; %Let I = I&_ ; %Let J = J&_ ; %Let L = L&_ ;
  %Let P = P&_ ; %Let S = S&_ ; %Let T = T&_ ; %Let Z = Z&_ ;

  Array &Z (0:1, 0:50) _Temporary_ ;

  &L = &LB ; &H = &HB ;

  If &H - &L GT &M Then Do &S=1 By 0 While (&S) ;
    &J = (&H - &L)/3 ; &I = &L + &J ; &J = &I + &J ;
    If &By(&L) &Q.T &By(&I) Then %Swap(&L,&I) ;
    If &By(&I) &Q.T &By(&J) Then %Swap(&I,&J) ;
    If &By(&J) &Q.T &By(&H) Then %Swap(&J,&H) ;
    If &By(&L) &Q.T &By(&I) Then %Swap(&L,&I) ;
    If &By(&I) &Q.T &By(&J) Then %Swap(&I,&J) ;
    If &By(&L) &Q.T &By(&I) Then %Swap(&L,&I) ;

    %If &M LE 3 %Then %Do ;
      If &H - &L LE 3 Then Do ;
        &L = &Z(0,&S) ; &H = &Z(1,&S) ; &S +- 1 ;
        Continue ;
      End ;
    %End ;

    %Swap(&L,&I) ; &P = &By(&L) ; &I = &L ;

    Do &J=&H + 1 By 0 ;
      Do &I=&I + 1 By  + 1 Until(&By(&I) &Q.E &P) ; End ;
      Do &J=&J - 1 By  - 1 Until(&P &Q.E &By(&J)) ; End ;
      If &I GE &J Then Leave ;
      %Swap(&I,&J) ;
    End ;

    %Swap(&L,&J) ;

    If      &H - &J GE &J - &L GT &M Then Do &S = &S + 1 ;
      &Z(0,&S) = &J + 1 ; &Z(1,&S) = &H ; &H = &J - 1 ;
    End ;
    Else If &J - &L GE &H - &J GT &M Then Do &S = &S + 1 ;
      &Z(0,&S) = &L ; &Z(1,&S) = &J - 1 ; &L = &J + 1 ;
    End ;
    Else If &J - &L GT &M GE &H - &J Then &H = &J - 1 ;
    Else If &H - &J GT &M GE &J - &L Then &L = &J + 1 ;
    Else Do ;
      &L = &Z(0,&S) ; &H = &Z(1,&S) ; &S +- 1 ;
    End ;
  End ;

  %If &M = 1 %Then %Goto Exit ;

  Do &J = &LB + 1 To &HB ;
    If &By(&J - 1) &Q.T &By(&J) Then Do ;
      &P = &By(&J) ;
      %Do W = 1 %To &N ;
        %If &&A&W Ne &By %Then &T&W = &&A&W(&J)  ;  ;
      %End ;
      Do &I = &J - 1 To &LB By  - 1 ;
        If &P &Q.E &By(&I) Then Leave ;
        %Do W = 1 %To &N ;
          &&A&W(&I + 1) = &&A&W(&I) ;
        %End ;
      End ;
      &By(&I + 1) = &P ;
      %Do W = 1 %To &N ;
        %If &&A&W Ne &By %Then &&A&W(&I + 1) = &T&W ; ;
      %End ;
    End ;
  End ;

  %Exit: Drop &H &I &J &L &P &S T&_: ;

%Mend Qsort ;
