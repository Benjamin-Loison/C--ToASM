open CAST
open Genlab

(*could use -loc_of_expr- and e_of_expr instead of doing it by myself*)

let loc = ("", 0, 0, 0, 0);;
let exprSkip = ESEQ [];;
let locExprSkip = loc, exprSkip;;
let codeSkip = CEXPR locExprSkip;;
let locCodeSkip = loc, codeSkip;;
let loc0 = loc, CST 0;;


let variables = ref [];;

(*let registers = [| "-8(%rbp)"; "-16(%rbp)"; "-24(%rbp)"; "-32(%rbp)" |];;*)(* [| "eax"; "ebx"; "ecx"; "edx" |];; *)

let subLog s i j l =
    (*print_string (l ^ " 0");*)
    let res = String.sub s i j in
    (*print_string (l ^ " 1");*)
    res;;

let registersCounter = ref 0;;

let print_variables () =
    let rec aux vars = match vars with
    | (varName, value, d)::q -> print_string (varName ^ " "); print_int value; print_string " "; print_int d; print_newline (); aux q;
    | _ -> ()
    in aux !variables;;

let addRegisterUse () =
    incr registersCounter;
    !registersCounter - 1;; (* failwith if out of bounds ? *)

let getRegisterIndex varName depth = (* make sure to check depth by depth and not take the first one *)
    (*print_string "Variables:\n"; print_variables (); print_string ("Searching: " ^ varName ^ " "); print_int depth; print_newline ();*)
    let rec aux vars dept = match vars with
    | (vN, v, d)::q when vN = varName && dept = d -> v
    | (vN, v, d)::q -> (aux q dept)
    | _ when dept = 0 -> (-1)
    | _ -> aux !variables (dept - 1)
    in aux !variables (depth(* + 1*));;

let multp i =
    string_of_int ((-i) * 8);;

let getRegisterName varName depth funName =
    if varName = "argv" then varName else
    (
    let i = getRegisterIndex (funName ^ "-" ^ varName) (depth + 1) in
    if i = (-1) then
        (
            (*print_string "NOT FOUND !";*)
            varName ^ "(%rip)"
        )
    else
        (*registers.(i)*)
        ((multp (i + 1)) ^ "(%rbp)"));;

let default = ".bss\n.text\n.align 8\n";; (* all declarations in one statement is the best ? #Doubt *)
let asmContent = ref "";;
let finalAsmContent = ref "";;
let fullMain = ref false;;

let indexe str subStr =
    let nStr = (String.length str) and nSubStr = (String.length subStr) and i = ref 0 in
    let upperBound = (nStr - nSubStr) in
    while ((0 <= !i) && (!i <= upperBound)) do
        let subStrTmp = (subLog str !i nSubStr "A") in
        (if subStrTmp = subStr then (i := -(!i)) else (incr i))
    done;
    (if !i <= upperBound then (-(!i) + nSubStr) else (-1));;

let realIndex str subStr =
    (indexe str subStr) - (String.length subStr);;

let replace s s0 s1 =
    let n = ref (String.length s) and n0 = String.length s0 and ss = ref s and i = ref 0 in
    while !i <= (!n - n0) do
        if (subLog (!ss) !i n0 "B") = s0 then
        (
            ss := (subLog (!ss) 0 !i "C") ^ s1 ^ (subLog (!ss) (!i + n0) (!n - n0 - !i) "D");
            n := String.length !ss;
        );
        incr i;
    done;
    !ss;;

let fullMainFunc () =
    let i = (indexe !asmContent "\nmain:\n") + 31 and n = (String.length !asmContent) and argcReg = (multp (!registersCounter + 1)) ^ "(%rbp)" and argvNb = (multp (!registersCounter + 2)) in let argvReg = (multp (!registersCounter + 2)) ^ "(%rbp)" in
    asmContent := (subLog !asmContent 0 i "E") ^
    ((*"  subq $" ^ (multp (-(!registersCounter + 2))) ^ ", %rsp\n"*)"  movq %rdi, " ^ argcReg ^ "\n  movq %rsi, " ^ argvReg ^ "\n") ^
    (subLog !asmContent i (n - i) "F");
    asmContent := (replace !asmContent "argc(%rip)" argcReg);
    asmContent := (replace !asmContent "argv" argvReg);
    argvNb;; (* should make a string excluder *)

let dontPrint = ref false;;
(*let fill = ref false;;*)
(*let filler = ref "";;*)
(*let enableFiller () =
    filler := "";
    fill := true;;*)
let fillers = ref [];;
let pushFiller filler =
    fillers := filler::!fillers;;
let popFiller () =
    match !fillers with
    | t::q -> fillers := q(*; !t*)
    | _ -> print_string "fillers is empty !"(*; ""*);;

let printAux out s newLine =
    if not !dontPrint then
        (let newAsmContent = ref (if !fillers <> [] then "" else !asmContent) in
        if !newAsmContent <> "" then
            newAsmContent := !newAsmContent;
        if newLine then newAsmContent := !newAsmContent ^ "\n";
        newAsmContent := !newAsmContent ^ s;
        if !fillers <> [] then (let filler = List.hd !fillers in filler := !filler ^ !newAsmContent)
        else asmContent := !newAsmContent);;

let print out s =
    (*Printf.fprintf out "%s\n" s;;*)
    (*print_string (s ^ "\n");*)
    (*print_string ("\nBEGIN\n" ^ !asmContent ^ "\nEND\n");*)
    printAux out s true;;

let addContentAt str i =
    (*print_string ("str: " ^ str ^ " |");
    print_string ("i: " ^ (string_of_int i) ^ " |");
    print_string ("asm: " ^ !asmContent ^ " |");
    if !asmContent = "" then asmContent := str else*) (* SURE OF THIS NEW LINE ? - doesn't seem correct should understand why !asmContent = "" *)
    asmContent := (subLog !asmContent 0 i "F") ^ str ^ (subLog !asmContent i ((String.length !asmContent) - i) "G");;

(* https://stackoverflow.com/a/11203255 *)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if subLog s1 i len "H" = s2 then raise Exit
    done;
    false
  with Exit -> true;;

(* contains function remasterised *)
let index s1 s2 =
    let len = String.length s2 and found = ref false and i = ref 0 in
    while (!i <= (String.length s1) - len) && (not !found) do
    (
        if (subLog s1 !i len "I") = s2 then
        (
            found := true;
        );
        incr i;
    )
    done;
    if !found then (!i - 1) else (-1);;

let join lines delim =
    let rec aux lines = match lines with
    | line::q -> line ^ delim ^ (aux q)
    | _ -> ""
    in aux lines;;

let job line needle =
    (*print_string line;*)
    let i0 = index line needle in
    (*print_int i0;*)
    let i1 = i0 + (String.length needle) + 1 and i2 = String.rindex line '_' in
    (*print_string "not stonks\n";
    print_string (line ^ "!\n");
    print_int i1; print_string " "; print_int i2;*)
    let var = subLog line i1 (i2 - i1) "J" in
    (*print_string "stonks";*)
    (if i0 = 2 then "" else ((subLog line 0 i0 "K") ^ var ^ (subLog line (i2 + 1) (String.length line - i2 - 1) "L")))::["  " ^ (if needle = "M_POST_INC" then "add" else "sub") ^ "q $1, " ^ var];;

let afterMain = ref false;;

let work line =
    if contains line "main:" then afterMain := true;
    let lineLen = String.length line in
    if ((lineLen > 0 && line.[0] = '%') || (lineLen > 3 && (line.[2] = '$' || line.[2] = '-' || line.[2] = '%' || (line.[2] = '.' && not !afterMain)))) then [""] (* warning this line absorbs a lot of stuff *)
    else
    if contains line "M_POST_INC" then
    (
        job line "M_POST_INC"
    )
    else
    (
        if contains line "M_POST_DEC" then
        (
            job line "M_POST_DEC"
        )
        else [line]
    );;

let rec print_strings l = match l with
| t::q -> print_string t; print_newline (); print_strings q
| [] -> ();;

let write out =
    (*asmContent := *)(*optimize !asmContent;*) (* in order to solve ex3 problem of godbolt or use Neven bazuka https://discord.com/channels/@me/774296443254341654/783796120685510677 *)
    (*let offset = ref "" in*)
    (*print_string ("ios 0a\n");*)
    let offset = (if (!fullMain) then (string_of_int (-(int_of_string (fullMainFunc ())))) else (multp (-(!registersCounter * 2)))) in
    (*print_string ("ios 0b\n");*)
    (*print_string ("!regCount: " ^ (string_of_int !registersCounter ^ "\n"));*)
    (*(multp (-(!registersCounter * 2used to be a * :') (if !fullMain then 2 else 0))))*)
    (*(string_of_int (-(!offset)))*)
    let ss = " $" ^ offset ^ ", %rsp" and i = (realIndex !asmContent "\nmain:\n") + 38 in (* could just add 8 if isn't equal to 0 mod 16 instead of doubling *)
    addContentAt ("  subq" ^ ss ^ "\n") i;
    (*print out ("  addq" ^ ss);*)
    (*print out "  popq %rbp";
    print out "  ret";*)
    
    (*addContentAt ("  addq" ^ ss ^ "\n") ((String.length !asmContent) - 17);*)
    
    (*print out ("  addq" ^ ss);*)

    (*asmContent := join ["abc"; "def"] "\n";*)
    let lines = ref (String.split_on_char '\n' !asmContent) in

    let linesLen = List.length !lines in
    let lastLine = List.nth !lines (linesLen - 2) in
    (*print_string ("|" ^ lastLine ^ "|");*)
    if lastLine <> (*"  cltq"*)"  call exit@PLT" then
        addContentAt ("  addq" ^ ss ^ "\n") ((String.length !asmContent) - 17);
    lines := String.split_on_char '\n' !asmContent;

    let rec aux linesT = match linesT with
    | line::q -> (work line)@(aux q)
    | _ -> []
    in
        let treatedLines = ref (aux !lines) in
        (*print_strings treatedLines;*)
        asmContent := join !treatedLines "\n";
    
    asmContent := replace !asmContent "\n  \n" "\n";
    asmContent := replace !asmContent "\n\n" "\n";
    asmContent := replace !asmContent "\n\n" "\n";
    asmContent := replace !asmContent "  %rax\n" "";
    asmContent := replace !asmContent "  movq %rax, %rax\n" "";

    (*afterMain := false;*)
    (*treatedLines := aux !treatedLines;*)
    (*lines := String.split_on_char '\n' !asmContent;
    treatedLines := aux !lines;

    asmContent := join !treatedLines "\n";*)

    (*asmContent := replace !asmContent "\n%rbx.L" "\n.L";*)

    Printf.fprintf out "%s\n" (*!finalAsmContent*)(default ^ !asmContent);;

let sectionCounter = ref 0;;

let getSectionName i =
    ".L" ^ (string_of_int i);;

let addSection () =
    incr sectionCounter; ((!sectionCounter) - 1);;

let stringCounter = ref 0;;
let addString () = incr stringCounter; !stringCounter - 1;;

let addHeader s =
	asmContent := s ^ "\n" ^ !asmContent;;

let globalVar = ref false;;

let addVar varName out depth funName =
    (*print out ("addVar " ^ varName);*)
    if depth = 0 then
    (
        if not !globalVar then
        (
            globalVar := true;
            print out ".data\n";
        );
        print out (".globl " ^ varName); (* .global also works *)
        print out (varName ^ ":");
        print out "  .zero 8";
    )
    else
    (
        (*print_string "Old content:\n";
        print_variables ();*)
        (*print_string ("varName: " ^ varName ^ " !\n");*)
        if varName = "argc" || varName = "argv" then
        (
            fullMain := true;
            variables := (varName, (if varName = "argc" then (-1) else (-2)), depth)::(!variables);
        )
        else
            variables := (funName ^ "-" ^ varName, addRegisterUse (), depth)::(!variables);
        (*print_string "New content:\n";
        print_variables ();*)
    );;

let needVar = ref false;;

let isMem s = if s = "" then false else (let c = s.[0] in c = '-' || c = 'a');; (* the check for equality to empty string is weird but avoid crash should look into it if have bugs *)
(*let callDisp = ref true;;*)

let args = ["di"; "si"; "dx"];;
let takeArgs n =
    let rec aux l n = match l with
    | t::q when n > 0 -> t::(aux q (n - 1))
    | _ -> []
    in aux args n;;

let needSetVar = ref false;;

(* it would be interesting to make well formated asm code at the end *)

let functionsNoConv = ["malloc"; "fopen"; "atoi"; "atol"; "strtol"; "labs"; "fflush"; "fclose"; "fputc"; "exit"];;

let needConv fctName =
    let rec aux l = match l with
    | t::q when t = fctName -> false
    | t::q -> aux q
    | _ -> true
    in aux functionsNoConv;;

let rec exprToStr expr out callDisp depth funName = match expr with
| VAR varName ->
    (*"%" ^*) let res = getRegisterName varName depth funName in
        (*print_string ("!" ^ res ^ "!");*)
        res
| CST x -> ("$" ^ (string_of_int x))
| SET_VAR (varName, locExpr) ->
        ((*print_string "sv 0";*)
        
        match locExpr with
        | (loc, expr) -> (*print out_variables (); print out_string "a\n";*)
                let src = exprToStr expr out true depth funName and dst = getRegisterName varName depth funName in
                (*print_string "sv 1";*)
                if src.[0] = '$' then
                (
                    (*print_string "sv 2";*)
                    print out ("  movq " ^ src ^ ", " ^ dst);
                    if !needVar then dst else ""
                )
                else
                (
                    (*print_string "sv 3";*)
                    print out ("  movq " ^ src ^ ", %rax\n  movq %rax, " ^ dst);
                    if !needSetVar then (needSetVar := false; src) else "")) (* this "empty" return seems to make a empty line in the result (could just delete empty lines at the héhé *)
                (* on godbolt, ex3, x = y gives two lines, might need to implement this - yes let's do it *)
(*| CEXPR locExpr -> (match locExpr with
        | (loc, expr) -> exprToStr expr) *)
| SET_ARRAY (arrayName, locExpr0, locExpr1) -> (*"  movq " ^*) 
        let _, expr0 = locExpr0 and _, expr1 = locExpr1 in
        let expr0Str = exprToStr expr0 out false depth funName in
        let expr1Str = exprToStr expr1 out false depth funName in
        (*expr0Str ^ " " ^ expr1Str*)
        (*print_string ("a0 " ^ expr0Str ^ " !\n");*)
        let res = ref "" in
        if contains expr0Str "(%rbp)" then
        (
            (*let res0 = ref ("movq " ^ (getRegisterName arrayName depth) ^ ", %rax\n") in
            res0 := !res0 ^ ("  movq " ^ expr0Str ^ ", %rdx\n");
            res0 := !res0 ^ ("  imulq $8, %rdx\n");
            res0 := !res0 ^ ("  addq %rdx, %rax\n");
            res0 := !res0 ^ ("  movq " ^ expr1Str ^ ", (A%rax)\n");
            !res0*)
            res := "  movq " ^ (getRegisterName arrayName depth funName) ^ ", %rax\n" ^
                   "  movq " ^ expr0Str ^ ", %rdx\n" ^
                   "  imulq $8, %rdx\n" ^
                   "  addq %rdx, %rax\n" ^
                   "  movq " ^ expr1Str ^ ", (%rax)\n";
            print out !res;
            expr1Str
        )
        else
        (
            (*print_string ("a0 " ^ expr0Str ^ " !\n");*)
            let expr0Nb = int_of_string (subLog expr0Str 1 ((String.length expr0Str) - 1) "M") in (* same possible troubles than S_INDEX *)
            (*print_string "a1\n";*)
            let common = "  movq " ^ (getRegisterName arrayName depth funName) ^ ", %rax\n" ^
            "  addq $" ^ (string_of_int (8 * expr0Nb)) ^ ", %rax\n" ^
            "  movq " in
            res := (if contains expr1Str "\n" then expr1Str ^ "\n  " ^ common ^ ", (%rax)" else common ^ (*"|" ^*) expr1Str ^ ", (%rax)"); (*^ "|"*)
            (*"movq " ^ (getRegisterName arrayName depth) ^ ", %rax\n" ^
            "  addq $" ^ (string_of_int (8 * expr0Nb)) ^ ", %rax\n" ^
            "  movq |" ^ expr1Str ^ "HERE"*) (*^ ", (B%rax)\n"*)
            (*"  movq " ^ expr1Str ^ ", %rcx\n"*)
            print out !res;
            expr1Str
        )

| CALL (fctName, locExprList) ->
      (*callDisp := true;*)
      (*print_string (fctName ^ "\n");*)
      let rec aux locExprL regs = match locExprL, regs with
      | ((loc, t)::q), (reg::regQ) -> print out ("  " ^
            (match t with
             | STRING s -> (*print_string (fctName ^ "(" ^ s ^ ")\n");*) "leaq"
             | _ -> "movq")
            ^ " " ^ (exprToStr t out true depth funName) ^ ", %r" ^ reg); aux q regQ
      | _ -> ()
      in aux (List.rev locExprList) (List.rev (takeArgs (List.length locExprList)))(*["di"; "dx"; "si"]*)(*["di"; "si"; "dx"]*)(*["dx"; "si"; "di"]*);
      (*callDisp := false;*)
      (*print out ("  movq $0, %rax");*)
      (*print out ("  subq $" ^ (multp (-(!registersCounter + 1))) ^ ", %rsp");*)
      (*print_string (fctName ^ " write\n");*)
      print out ("  call " ^ fctName ^ "@PLT"); (* doesn't used to need it but seems useful now... *)
      if (needConv fctName) then
        (print out "  cltq");
      (*print out ("  addq $" ^ (multp (-(!registersCounter + 1))) ^ ", %rsp");*)
      (*print out ("  movq $0, %rax");*)
      (* print out ("!" ^ !asmContent ^ "!"); *)
      if callDisp then "%rax" else ""
| STRING (str) ->
        let strReg = ".LC" ^ (string_of_int (addString ())) in
        (
	        addHeader (strReg ^ ":\n  .string \"" ^ ((*check*)replace str "\n" "\\n") ^ "\"");
            (*"leaq " ^ str ^ "(%rip), "*) (*STR*)
            strReg ^ "(%rip)"
        )
| OP1 (monOp, locExpr) ->
    (
        let _, expr = locExpr in
        let exprStr = exprToStr expr out false depth funName in
        match monOp with
        | M_MINUS ->
                (
                    match expr with
                    | CST x -> ((*print out "-";*) "$-" ^ (replace exprStr "$" ""))(* ^ exprStr*)
                    | _ -> (print out ("  negq " ^ exprStr); exprStr)
                )
        | M_NOT -> print out ("  notq " ^ exprStr); exprStr
        | M_POST_INC -> (*print out "hey"; *)"M_POST_INC_" ^ exprStr ^ "_"
        | M_POST_DEC -> "M_POST_DEC_" ^ exprStr ^ "_"
        | M_PRE_INC -> print out ("  addq $1, " ^ exprStr); exprStr
        | M_PRE_DEC -> print out ("  subq $1, " ^ exprStr); exprStr
    )
| OP2 (binOp, locExpr0, locExpr1) ->
    (
        let ope0 ope expr0 expr1 =
            dontPrint := true;
                needVar := true; (* should use optionnal argument instead *)
                let dst = exprToStr expr0 out false depth funName and src = exprToStr expr1 out false depth funName in
                (*let src = exprToStr expr1 out false depth funName and dst = exprToStr expr0 out false depth funName in*)
                needVar := false;
                dontPrint := false;
                if true (*isMem dst && isMem src*) then
                (
                    exprToStr expr0 out false depth funName;
                    print out ("  movq " ^ dst ^ ", %rbx");
                    (*print out ("  movq " ^ src ^ ", %rbx");*)
                    (*printAux out "" false;*)
                    exprToStr expr1 out false depth funName;
                    (*print out ("  movq " ^ dst ^ ", %rcx");*)
                    (*print out ("  " ^ ope ^ " %rbx, " ^ dst);*)
                    print out ("  " ^ ope ^ " " ^ src ^ ", %rbx"); (* ordre should have i = 3 ?! *)
                    "%rbx"
                )
                else
                (
                    print out (ope ^ " " ^ dst ^ ", " ^ src ^ "\n");
                    dst
                ) in
		let ope1 ope expr0 expr1 =
            let exp0 = exprToStr expr0 out false depth funName and exp1 = exprToStr expr1 out false depth funName in
            print out ("  movq " ^ exp0 ^ ", %rax\n" ^
                       "  cqto\n  " ^
                       (if contains exp1 "%" then "idivq " ^ exp1 else "  movq " ^ exp1 ^ ", %rax\n  idivq %rax") ^ "\n"); "%r" ^ ope
		in
        let (errLoc0, expr0) = locExpr0 and (errLoc1, expr1) = locExpr1 in
        match binOp with
        | S_MUL -> print out ("  movq " ^ (exprToStr expr0 out false depth funName) ^ ", %rax\n" ^
                              "  imulq " ^ (exprToStr expr1 out false depth funName) ^ ", %rax\n"); "%rax"
        | S_DIV -> ope1 "ax" expr0 expr1
        | S_MOD -> ope1 "dx" expr0 expr1
        | S_ADD ->
                (*dontPrint := true;
                needVar := true; (* should use optionnal argument instead *)
                let dst = exprToStr expr0 out and src = exprToStr expr1 out in
                needVar := false;
                dontPrint := false;
                if isMem dst && isMem src then
                (
                    exprToStr expr0 out;
                    print out ("  movq " ^ src ^ ", %rbx");
                    (*printAux out "" false;*)
                    exprToStr expr1 out;
                    (*print out ("  movq " ^ dst ^ ", %rcx");*)
                    print out ("  addq %rbx, " ^ dst);
                    "%rbx"
                )
                else
                (
                    print out ("addq " ^ dst ^ ", " ^ src ^ "\n");
                    dst
                )*)
                ope0 "addq" expr1 expr0
        | S_SUB -> ope0 "subq" expr0 expr1
        | S_INDEX ->
                let expr1Str = exprToStr expr1 out false depth funName in
                (*print_string ("b0 " ^ expr1Str ^ "!\n");*)
                (*let expr1NbStr = (String.sub expr1Str 1 ((String.length expr1Str) - 1)) (* might have troubles if index is a variable *) in*)
                let reg = ref "" in
                (*let expr1Nb = *)if (contains expr1Str "(%rbp)"(* || contains expr1Str "argv"*)) then (print out ("  movq " ^ expr1Str ^ ", %rdx"); print out ("  imulq $8, %rdx"); reg := "%rdx") else (let expr1NbStr = (subLog expr1Str 1 ((String.length expr1Str) - 1) "N") in reg := "$" ^ (string_of_int (8 * (int_of_string expr1NbStr))));(* in*)
                (*print_string "b1\n";*)
                (*"(" ^ (exprToStr expr0 out false depth) ^ "), " ^ (exprToStr expr0 out false depth)*)
                print out ("  movq " ^ (exprToStr expr0 out false depth funName) ^ ", %rax");
                print out ("  addq " ^ !reg ^ ", %rax");(* ^ (exprToStr expr0 out false depth);*)
                "(%rax)"
    )
| CMP (cmpOp, locExpr0, locExpr1) ->
    (
        (*print_string "cmp 0\n";*)
        let (_, expr0) = locExpr0 and (_, expr1) = locExpr1 in
        (* may have troubles if use rbx and rbx here too (two "too many memory references" of two (not always different) kinds) - if always different type maybe there is enough rXx *)
        needSetVar := true;
        let firstReg = ref (exprToStr expr0 out false depth funName) and secondReg = ref (exprToStr expr1 out false depth funName) in
        (*print_string "cmp 1\n";*)
        let isMemH = (isMem !firstReg && isMem !secondReg) and b = ref "" in
        (*print_string "cmp 2\n";*)
        (*print_string ("firstReg: " ^ !firstReg ^ "\n");
        print_string ("secondReg: " ^ !secondReg ^ "\n");*)
        if isMemH then
        (
            b := "movq " ^ !firstReg ^ ", %rbx\n  ";
            firstReg := "%rbx";
        );
        
            (*printf("%s %s\n", firstReg, secondReg);*)
            (*print_string (!firstReg ^ " " ^ secondReg ^ "\n");*)
           
            (*(getSectionName (addSection ())) ^ ":\n  " ^*) 
            (*print_string (!firstReg ^ "\n");*)
            let s = ref ("  " ^
            (if isMemH then
            (
                !b
            )
            else
            (
                if isMem !secondReg then
                (
                    (*print_string ("firstReg: " ^ !firstReg ^ "\n");
                    print_string ("secondReg: " ^ !secondReg ^ "\n");*)
                    let tmp = !firstReg in
                    firstReg := !secondReg;
                    secondReg := tmp;
                    (*print_string "done\n";*)
                );
                ""
            )
            )) in
            s := !s ^ ("cmpq " ^ !secondReg);
            (*print_string "cmp 1\n";*)
            (*print_string (s ^ "\n");*)
            let res = (!s
        ^ ", " ^ (if !firstReg = "" then "%rax" else !firstReg) ^ "\n  " ^
        match cmpOp with
        | C_LT -> "jl "
        | C_LE -> "jle "
        | C_EQ -> "je "
            ) in (*print out res; ""*)res
    )
| EIF (locExpr0, locExpr1, locExpr2) ->
        let _, expr0 = locExpr0 and _, expr1 = locExpr1 and _, expr2 = locExpr2 and
        sectionName = getSectionName (addSection ()) and nextSectionName = getSectionName (addSection ()) and nextNextSectionName = getSectionName (addSection ()) in

        let filler1 = ref "" in
        pushFiller filler1;
        let reg1 = exprToStr expr1 out false depth funName in
        popFiller ();
        (*Cprint.print_ast Format.std_formatter [CFUN (loc, "debug", [], (loc, CEXPR locExpr1))];*)
        (*print_string ("|f1: " ^ !filler1 ^ "|\n");
        print_string ("|reg1: " ^ reg1 ^ "|\n");*)
        print out !filler1;

        let filler2 = ref "" in
        pushFiller filler2;
        let reg2 = exprToStr expr2 out false depth funName in
        popFiller ();
        print out !filler2;
        (*print_string ("|f2: " ^ !filler2 ^ "|\n");
        print_string ("|reg2: " ^ reg2 ^ "|\n");*)

        (*(exprToStr expr0 out) ^*) (*used to be addHeader*)let t = (sectionName ^ ":\n" ^ (if contains reg1 "\n" then (reg1 ^ nextNextSectionName(*"NOPE"*)) else "  movq " ^ reg1 ^ ", %rbx") ^ "\n  jmp " ^ nextNextSectionName ^ "\n" ^ nextSectionName ^ ":\n  movq " ^ reg2 ^ ", %rbx"(* ^ "\n  jmp " ^ nextNextSectionName(* ^ "!" ^ (exprToStr expr0 out) ^ "!"*)*)) in
        let reg = (exprToStr expr0 out false depth funName) in
        if reg.[0] = '$' then
        (
            (*print out ("  cmpq $0, " ^ reg);*)
            (*print out ("  jne " ^ sectionName);
            print out ("  jmp " ^ nextSectionName);*)
            print out ("  jmp " ^ (if reg = "$0" then nextSectionName else sectionName))
        )
        else
        (
            print out (reg ^ sectionName);
            (*print out ("  movq " ^ (exprToStr expr0 out false depth) ^ ", %rcx");*)
            (*print out ("  cmp " ^ "$0, " ^ "%rcx"); *)
            (*print out ("  jne " ^ sectionName ^ "\n");*)
            print out ("  jmp " ^ nextSectionName ^ "");
        );
        print out t;
        print out (nextNextSectionName ^ ":"(* ^ "\n"*));
        "%rbx"
| ESEQ (locExprList) ->
        if (List.length locExprList) = 0 then "nop" else "ESEQ";;
(*| _ -> "WIP";;*)

let rec compileDecl out decl depth varIndexFun funName =
      match decl with
      | CDECL (loc, varName) -> (*print out_string "69 ";*) (*addVar varName out depth;*)(* print out_variables (); print out_string "56\n";*)
      (*| CDECL (loc, str) -> print out "chacal" *)
              if funName <> "main" && varIndexFun != (-1) then
              (
                  let reg = ref (getRegisterName varName depth funName) in
                  if contains !reg "(%rip)" then (addVar varName out depth funName;
                  reg := getRegisterName varName depth funName);
                  print out ("  movq %r" ^ (List.nth args varIndexFun) ^ ", " ^ !reg)
              )
              else
              (
                  addVar varName out depth funName
              );
      | CFUN (loc, funNameFun, varDecL, locCodeL) ->
              if funNameFun = "main" then
                  (print out "\n.text\n.globl main");(* else ();*)
          print out (funNameFun ^ ":");
          print out "  pushq %rbp";
          print out "  movq %rsp, %rbp";
          let i = ref 0 in
            let rec aux varDecList =
              (*let i = ref 0 in*)
              match varDecList with
              | t::q ->
                   (compileDecl out t (depth + 1) !i funNameFun);
                   incr i;
                   aux q; 
              | _ -> ()
          in aux varDecL;
          let rec aux locCodeList d =
              match locCodeList with
                | t::q ->
                    (
                    match t with
                    | (loc, code) -> (
                        match code with
                         | CBLOCK (varDecList, locCodeList) -> compileAux out varDecList (d + 1) funNameFun; aux locCodeList d
                         | CEXPR locExpr -> (match locExpr with
                                             (*| (loc, OP1 (op, ex)) -> exprToStr (OP1 (op, ex)) out false d; ()*)
                                             | (loc, expr) -> print out ("  " ^ (exprToStr expr out false d funNameFun)))
                         | CIF (locExpr, locCode0, locCode1) ->
                            (
                            let _, expr = locExpr and
                            sectionName = getSectionName (addSection ()) and nextSectionName = getSectionName (addSection ()) and nextNextSectionName = getSectionName (addSection ()) in
                            
                            (*enableFiller ();*)
                            let filler0 = ref "" in
                            pushFiller filler0; (* could also let pushFiller create the ref *)
                            aux [locCode0] (d + 1);
                            popFiller ();
                            (*fill := false;*)
                            
                            let code0 = !filler0 in
                            (*enableFiller ();*)
                            let filler1 = ref "" in
                            pushFiller filler1;
                            aux [locCode1] (d + 1);
                            popFiller ();
                            (*fill := false;*)
                            
                            let t = (sectionName ^ ":\n  " ^ code0 ^ "\n  jmp " ^ nextNextSectionName ^ "\n" ^ nextSectionName ^ ":\n  " ^ !filler1 ^ "\n"(* ^ "\n  jmp " ^ nextNextSectionName*)) in (*and reg = exprToStr expr out false d funNameFun in*)
                            (*enableFiller ();*)
                            let filler2 = ref "" in
                            pushFiller filler2;
                            let reg = exprToStr expr out false d funNameFun in
                            (*fill := false;*)
                            popFiller ();
                            (*print_string ("!filler2: " ^ !filler2 ^ "|\n");*)
                            print out !filler2;


                            (match expr with
                            | CMP (_, _, _) ->
                            (
                                (*print_string ("reg: " ^ reg ^ "|\n");*)
                                print out (reg ^ sectionName);
                                print out ("  jmp " ^ nextSectionName);
                            )
                            | _ ->
                            (
                                print out ("  movq " ^ (if reg = "" then "%rax" else reg) ^ ", %rcx");
                                print out ("  cmpq " ^ "$0, " ^ "%rcx");
                                print out ("  jne " ^ sectionName ^ "\n");
                                print out ("  jmp " ^ nextSectionName ^ "\n");
                            ));
                            print out t;
                            print out (nextNextSectionName ^ ":\n")
                            )
                         | CWHILE (locExpr, locCode) ->
                            let testSection = getSectionName (addSection ()) and nextSection = getSectionName (addSection ()) in
                            (*print out (testSection ^ ":");*)
                            let (_, expr) = locExpr in
                            (*let filling = ref "" in*)
                            (*enableFiller ();*) (*filling*)
                            let filler = ref "" in
                            pushFiller filler;
                            let reg = (exprToStr expr out false d funNameFun) in
                            (*fill := false;*)
                            popFiller ();
                            let fillerCopy = !filler in
                            if contains reg "\n" then
                            (
                                print out ("  jmp " ^ testSection);
                                print out (nextSection ^ ":");
                                aux [locCode] d;
                                print out (testSection ^ ":");
                                (*print_string ("fillerCopy: " ^ fillerCopy ^ "@");*)
                                print out fillerCopy;
                                print out (reg ^ nextSection);
                            )
                            else
                            (
                                print out (testSection ^ ":");
                                print out fillerCopy;
                                print out ("  cmpq $0, " ^ reg(* ^ "!"*));
                                print out ("  je " ^ nextSection);
                                aux [locCode] d;
                                print out ("  jmp " ^ testSection);
                                print out (nextSection ^ ":");
                            );
                            
                            (*print_string "while dbg 0\n";*)
                            (*let (_, expr) = locExpr (*and (_, code) = locCode *)and sectionName = (getSectionName (addSection ())) and nextSectionName = getSectionName (addSection ()) and nextNextSectionName = getSectionName (addSection ()) in
                            (*write out; exit 69;*)
                            (*print_string "while dbg 1\n";*)

							let i = realIndex !asmContent "\nmain:\n" and sectionName = (getSectionName (addSection ())) in (* (moved) just in case you know *)

                            enableFiller ();
                            aux [locCode] d;
                            (*print out "test";*)
                            fill := false;

                            (*print_string ("^" ^ !filler ^ "^");*)

                            let filler0 = !filler in

                            enableFiller ();
                            exprToStr expr out false d;
                            fill := false;

                            print_string ("|" ^ filler0 ^ "|\n");
                            (*filler := "bruh";*)

                            (*print_string "while dbg 2\n";*)
                            addContentAt ("\n" ^ sectionName ^ ":I" ^ filler0 ^ "C\n" ^ (*!filler*)"" ^ "B\n" ^ (*sectionName*)nextNextSectionName ^ ":\n" ^ !filler ^ "jmpA " ^ nextSectionName) i;
                            (*print_string "while dbg 3\n";*)
                            (*print out ((exprToStr expr out) ^ sectionName);*)
                            print out ("  jmp " ^ nextNextSectionName(*nextSectionName*)); (* not right section name - seems patched *)
                            print out (nextSectionName ^ ":");*)

                            (*print out ("  jmp " ^ nextSectionName);*)
                            (*write out; exit 42;*)
                         | CRETURN locExpr ->
                             (
                                 if funNameFun = "main" then (match locExpr with | Some (loc, expr) -> exprToStr (CALL ("exit", [loc, expr])) out false d funNameFun; () | _ -> ()) else
                             ( match locExpr with
                             | Some locExprContent -> ( match locExprContent with
                                                      | (loc, expr) -> let reg = (exprToStr expr out false d funNameFun) in if reg = "%rax" then () else (print out ("  movq " ^ (if reg = "" then "%rax" else reg) ^ ", %rax"));) (* could check if exprToStr is different from eax - done - don't use movl just use movq for 64 bits *)
                             | _ -> () );
                             print out "  popq %rbp";
							 print out "  ret";
                             )
                             )
                    ; aux q d)
                | _ -> ()
          in aux [locCodeL] depth


and compileAux out decl_list depth funName =
    match decl_list with
    | t::q -> compileDecl out t depth (-1) funName; compileAux out q depth funName
    | _ -> ();;

let compile out decl_list =
    (*let decl_list_bis =

[(CFUN (loc, "main", [
    CDECL (loc, "argc");
    CDECL (loc, "argv")],
    
    (loc, (CBLOCK ([
        CDECL (loc, "c");
        CDECL (loc, "f")], [
        
        loc, CEXPR (
            loc, SET_VAR ("f", (
                loc, CALL ("fopen", [
                    loc, OP2 (S_INDEX, (loc, VAR "argv"), (loc, CST 1)); loc, STRING "r"]))));
        
        loc, CWHILE ((
            loc, EIF ((loc,
                CMP (C_EQ, (*(loc, SET_VAR ("c", (loc, CALL ("fgetc", [loc, VAR "f"]))))*)loc0, (*(loc, OP1 (M_MINUS, (loc, CST 1)))*)loc0)),
                (loc, CST 0), (loc, CST 1))), ((*
                loc, CEXPR (loc, CALL ("fputc", [loc, VAR "c"; loc, VAR "stdout"]))*)locCodeSkip));

        loc, CEXPR (loc, CALL ("fclose", [loc, (VAR "f")]));
        loc, CEXPR (loc, CALL ("fflush", [loc, (VAR "stdout")]));
        (*loc, CRETURN (Some (loc, (CST 0)))*)
        loc, CEXPR (loc, CALL ("exit", [loc, (CST 0)]))])))))] in*)
    compileAux out (*decl_list_bis*)decl_list 0 "";
	write out;;
