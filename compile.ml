open CAST
open Genlab

(* based on https://github.com/ocaml/ocaml/blob/trunk/stdlib/string.ml *)
(* code based to work with OCaml 4.04.0 but also works with 4.02.0 with this add-on *)
external unsafe_get : string -> int -> char = "%string_unsafe_get"

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if unsafe_get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r;;

(* end of the 4.02.0 add-on *)

(* we stock variables by: varName (main-i), value ("-8(%rbp)"), depth (2)*)
let variables = ref [];;

(* this function encapsulate String.sub likewise if there is a out of bounds we can print before and after the dangerous calls *)
let subLog s i j l =
  let res = String.sub s i j in
  res;;

let registersCounter = ref 0;;

let print_variables () =
  let rec aux vars = match vars with
    | (varName, value, d)::q -> print_string (varName ^ " "); print_int value; print_string " "; print_int d; print_newline (); aux q;
    | _ -> ()
  in aux !variables;;

let addRegisterUse () =
  incr registersCounter;
  !registersCounter - 1;;

(* get the assembly value (-8%(rbp)) by its name by searching firstly the more encapsulated code *)
let getRegisterIndex varName depth =
  let rec aux vars dept = match vars with
    | (vN, v, d)::q when vN = varName && dept = d -> v
    | (vN, v, d)::q -> (aux q dept)
    | _ when dept = 0 -> (-1)
    | _ -> aux !variables (dept - 1)
  in aux !variables depth;;

(* to get -8*i(%rbp) with just i *)
let multp i =
  string_of_int ((-i) * 8);;

(* argc, argv and strings are treated differently *)
let getRegisterName varName depth funName =
  if varName = "argv" then varName else
    (
      let i = getRegisterIndex (funName ^ "-" ^ varName) (depth) in
      if i = (-1) then
        (
          varName ^ "(%rip)"
        )
      else
        ((multp (i + 1)) ^ "(%rbp)"));;

(* these headers are required most of time*)
let default = ".bss\n.text\n.align 8\n";;
(* these variables are buffers which are used for treatment and at the end for writing *)
let asmContent = ref "";;
let finalAsmContent = ref "";;

let fullMain = ref false;;

(* this function returns the index of the first char where str[i:i+len(subStr)+1] = subStr *)
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

(* this functions returns a string where all occurrences of s0 in s is replaced by s1 *)
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

(* as specified in getRegisterName this function modify the code to adjust argc and argv which can be determined after we now the count of all variables *)
let fullMainFunc () =
  let i = (indexe !asmContent "\nmain:\n") + 31 and n = (String.length !asmContent) and argcReg = (multp (!registersCounter + 1)) ^ "(%rbp)" and argvReg = (multp (!registersCounter + 2)) ^ "(%rbp)" in
  asmContent := (subLog !asmContent 0 i "E") ^
                ("  movq %rdi, " ^ argcReg ^ "\n  movq %rsi, " ^ argvReg ^ "\n") ^
                (subLog !asmContent i (n - i) "F");
  asmContent := (replace !asmContent "argc(%rip)" argcReg);
  asmContent := (replace !asmContent "argv" argvReg);;

let dontPrint = ref false;;

(* fillers are used to switch function which are void (and "write" to the file) but instead of writing to the file we just fill a reference string variable *)
let fillers = ref [];;

let print_fillers () =
  let rec aux l = match l with
    | t::q -> print_string ("|" ^ !t ^ "|\n")
    | _ -> ()
  in aux !fillers;;

(* we use a stack for fillers in order to fill the right filler *)
let pushFiller filler =
  fillers := filler::!fillers;;

let popFiller () =
  match !fillers with
  | t::q -> fillers := q
  | _ -> print_string "fillers is empty !";;

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
  printAux out s true;;

(* this function introduces a bit of asm code within the already generated one *)
let addContentAt str i =
  asmContent := (subLog !asmContent 0 i "F") ^ str ^ (subLog !asmContent i ((String.length !asmContent) - i) "G");;

(* this function returns true if s2 is in s1, false otherwise *)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if subLog s1 i len "H" = s2 then raise Exit
    done;
    false
  with Exit -> true;;

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

(* this function assembles lines into a string with the "joiner" string delim *)
let join lines delim =
  let rec aux lines = match lines with
    | line::q -> line ^ delim ^ (aux q)
    | _ -> ""
  in aux lines;;

(* this code change the assembly code to inflate the post incrementation instruction *)
let job line needle =
  let i0 = index line needle in
  let i1 = i0 + (String.length needle) + 1 and i2 = String.rindex line '_' in
  let var = subLog line i1 (i2 - i1) "J" in
  ("  movq " ^ var ^ ", %r9")::("  " ^ (if needle = "M_POST_INC" then "add" else "sub") ^ "q $1, " ^ var)::[(subLog line 0 i0 "K") ^ "%r9" ^ (subLog line (i2 + 1) (String.length line - i2 - 1) "L")];;

let afterMain = ref false;;

(* this function removes unused assembly variables and inflate post incrementation instructions *)
let work line =
  if contains line "main:" then afterMain := true;
  let lineLen = String.length line in
  if ((lineLen > 0 && line.[0] = '%') || (lineLen > 3 && (line.[2] = '$' || line.[2] = '-' || line.[2] = '%' || (line.[2] = '.' && not !afterMain)))) then [""]
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

(* this function modify main to put final values for argc and argv and beautify the assembly code and write it to file *)
let write out =
  if (!fullMain) then (fullMainFunc ());

  asmContent := replace !asmContent "REG_COUNT" ("$" ^ (multp (- !registersCounter * 2)));

  let lines = ref (split_on_char '\n' !asmContent) in

  lines := split_on_char '\n' !asmContent;

  let rec aux linesT = match linesT with
    | line::q -> (work line)@(aux q)
    | _ -> []
  in
  let treatedLines = aux !lines in
  asmContent := join treatedLines "\n";

  asmContent := replace !asmContent "\n  \n" "\n";
  asmContent := replace !asmContent "\n\n" "\n";
  asmContent := replace !asmContent "\n\n" "\n";
  asmContent := replace !asmContent "  %rax\n" "";
  asmContent := replace !asmContent "  movq %rax, %rax\n" "";
  asmContent := replace !asmContent "  %r9" "";

  Printf.fprintf out "%s\n" (default ^ !asmContent);;

let sectionCounter = ref 0;;

(* gcc convention likewise there is no problem *)
let getSectionName i =
  ".L" ^ (string_of_int i);;

let addSection () =
  incr sectionCounter; ((!sectionCounter) - 1);;

let stringCounter = ref 0;;
let addString () = incr stringCounter; !stringCounter - 1;;

(* this function makes possible to add content at top of assembly code in order to have clean headers and not headers within assembly code *)
let addHeader s =
  asmContent := s ^ "\n" ^ !asmContent;;

let globalVar = ref false;;

(* this function manages variables differently if they are global or are argv or argv *)
let addVar varName out depth funName =
  if depth = 0 then
    (
      if not !globalVar then
        (
          globalVar := true;
          print out ".data\n";
        );
      print out (".globl " ^ varName);
      print out (varName ^ ":");
      print out "  .zero 8";
    )
  else
    (
      if varName = "argc" || varName = "argv" then
        (
          fullMain := true;
          variables := (varName, (if varName = "argc" then (-1) else (-2)), depth)::(!variables);
        )
      else
        variables := (funName ^ "-" ^ varName, addRegisterUse (), depth)::(!variables);
    );;

let needVar = ref false;;

(* this function returns whether or not a variable can be used in an instruction like: movq variable0 variable1 *)
let isMem s = if s = "" then false else (let c = s.[0] in c = '-' || c = 'a');;

(* arguments used before using heap to send arguments to a function *)
let args = ["%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"];;
let takeArgs n =
  let rec aux l n = match l with
    | t::q when n > 0 -> t::(aux q (n - 1))
    | _ -> []
  in aux args n;;

let needSetVar = ref false;;

(* these functions doesn't need to have their result converted *)
let functionsNoConv = ref ["malloc"; "fopen"; "atol"; "strtol"; "labs"; "fflush"; "fclose"; "fputc"; "printf"; "exit"];;
let addFunctionNoConv s =
  functionsNoConv := s::!functionsNoConv;;

let needConv fctName =
  let rec aux l = match l with
    | t::q when t = fctName -> false
    | t::q -> aux q
    | _ -> true
  in aux !functionsNoConv;;

(* these debugs parameters make assembly more verbose (may imply that the assembly doesn't compile) *)
let debugCIF = false and debugJMP = false and debugEIF = false and debugMOV = false;;

(* this function turns loc_expr expr into string, out parameter is required to write into the file, depth is how much encapsulated we are in the code, funName is the function where expr is used *)
let rec exprToStr expr out callDisp depth funName = match expr with
  | VAR varName ->
    let res = getRegisterName varName depth funName in
    res
  | CST x -> ("$" ^ (string_of_int x))
  | SET_VAR (varName, locExpr) ->
    (
      match locExpr with
      | (loc, expr) ->
        let src = exprToStr expr out true depth funName and dst = getRegisterName varName depth funName in
        if src.[0] = '$' then (* here we reduce the assembly number of lines by using a single line of assembly to move a constant to the variable *)
          (
            print out ("  movq " ^ src ^ ", " ^ dst ^ (if debugMOV then "# mov A" else ""));
            if !needVar then dst else ""
          )
        else (* while here in order not to have the two variables within head while using a movq statement, we use a temporary variable %rax *)
          (
            print out ("  movq " ^ src ^ ", %rax\n  movq %rax, " ^ dst ^ (if debugMOV then "# mov B" else ""));
            if !needSetVar then (needSetVar := false; src) else ""))
  | SET_ARRAY (arrayName, locExpr0, locExpr1) ->
    let _, expr0 = locExpr0 and _, expr1 = locExpr1 in
    let expr0Str = exprToStr expr0 out false depth funName in
    let expr1Str = exprToStr expr1 out false depth funName in
    let res = ref "" in
    if contains expr0Str "(%rbp)" then (* idivq doesn't work with constant so we use a temporary variable *)
      (
        res := "  movq " ^ (getRegisterName arrayName depth funName) ^ ", %rax" ^ (if debugMOV then "# mov C" else "") ^ "\n" ^
               "  movq " ^ expr0Str ^ ", %rdx" ^ (if debugMOV then "# mov D" else "") ^ "\n" ^
               "  imulq $8, %rdx\n" ^
               "  addq %rdx, %rax\n" ^
               "  movq " ^ expr1Str ^ ", (%rax)" ^ (if debugMOV then "# mov E" else "") ^ "\n";
        print out !res;
        expr1Str
      )
    else
      (
        let expr0Nb = int_of_string (subLog expr0Str 1 ((String.length expr0Str) - 1) "M") in
        let common = "  movq " ^ (getRegisterName arrayName depth funName) ^ ", %rax" ^ (if debugMOV then "# mov F" else "") ^ "\n" ^
                     "  addq $" ^ (string_of_int (8 * expr0Nb)) ^ ", %rax\n" ^
                     "  movq " in
        res := (if contains expr1Str "\n" then expr1Str ^ "\n  " ^ common ^ ", (%rax) # RAX 0" else common ^ (if contains expr1Str "%rbp" then (expr1Str ^ ", %r8\n  movq %r8") else expr1Str) ^ ", (%rax) # RAX 1");
        print out !res;
        expr1Str
      )

  | CALL (fctName, locExprList) ->
    let rec aux locExprL regs = match locExprL, regs with (* here we treat string variables differently *)
      | ((loc, t)::q), (reg::regQ) -> print out ("  " ^
                                                 (match t with
                                                  | STRING s -> "leaq"
                                                  | _ -> "movq")
                                                 ^ " " ^ (exprToStr t out true depth funName) ^ ", " ^ reg); aux q regQ
      | _ -> ()
    in aux (List.rev locExprList) (List.rev (takeArgs (List.length locExprList)));
    print out ("  movq $0, %rax" ^ (if debugMOV then "# mov G" else ""));
    print out ("  call " ^ fctName ^ "@PLT"); (* @PLT was required on my computer *)
    if (needConv fctName) then (* we convert return value if required *)
      (print out "  cltq");
    if callDisp then "%rax" else "" (* this recursive argument is used to reduce replace calls within write function in order to beautify the assembly code *)
  | STRING (str) ->
    (* this convention is used by gcc to name strings and with basic other strings it may fail *)
    let strReg = ".LC" ^ (string_of_int (addString ())) in
    (
      addHeader (strReg ^ ":\n  .string \"" ^ (replace str "\n" "\\n") ^ "\"");
      strReg ^ "(%rip)"
    )
  | OP1 (monOp, locExpr) ->
    (
      let _, expr = locExpr in
      let exprStr = exprToStr expr out false depth funName in
      match monOp with
      | M_MINUS ->
        (
          (* this kind of match makes assembly code smaller by not using another variable if it is not necessary *)
          match expr with
          | CST x -> ("$-" ^ (replace exprStr "$" ""))
          | _ -> (print out ("  negq " ^ exprStr); exprStr)
        )
      | M_NOT -> print out ("  notq " ^ exprStr); exprStr
      | M_POST_INC -> "M_POST_INC_" ^ exprStr ^ "_" (* we manage afterward the post incrementation/decrementation *)
      | M_POST_DEC -> "M_POST_DEC_" ^ exprStr ^ "_"
      | M_PRE_INC -> print out ("  addq $1, " ^ exprStr); exprStr
      | M_PRE_DEC -> print out ("  subq $1, " ^ exprStr); exprStr
    )
  | OP2 (binOp, locExpr0, locExpr1) ->
    (
      let ope0 ope expr0 expr1 =
        dontPrint := true;
        needVar := true;
        let dst = exprToStr expr0 out false depth funName and src = exprToStr expr1 out false depth funName in
        needVar := false;
        dontPrint := false;
        exprToStr expr0 out false depth funName;
        print out ("  movq " ^ dst ^ ", %rbx" ^ (if debugMOV then "# mov H" else ""));
        exprToStr expr1 out false depth funName;
        print out ("  " ^ ope ^ " " ^ src ^ ", %rbx");
        (
            exprToStr expr0 out false depth funName;
            print out ("  movq " ^ dst ^ ", %rbx" ^ (if debugMOV then "# mov H" else ""));
            exprToStr expr1 out false depth funName;
            print out ("  " ^ ope ^ " " ^ src ^ ", %rbx");
            "%rbx"
        )
        in
      let ope1 ope expr0 expr1 =
          exprToStr expr0 out false depth funName;
          let exp1 = exprToStr expr1 out false depth funName in
        print out ("  movq " ^ (exprToStr expr0 out false depth funName) ^ ", %rax" ^ (if debugMOV then "# mov I" else "") ^ "\n" ^
                   "  cqto\n" ^
                   (if contains exp1 "%" then "  idivq " ^ exp1 else "  movq " ^ exp1 ^ ", %rcx" ^ (if debugMOV then "# mov J" else "") ^ "\n  idivq %rcx") ^ "\n"); "%r" ^ ope
      in
      let (errLoc0, expr0) = locExpr0 and (errLoc1, expr1) = locExpr1 in
      match binOp with
      | S_MUL -> print out ("  movq " ^ (exprToStr expr0 out false depth funName) ^ ", %rax" ^ (if debugMOV then "# mov K" else "") ^ "\n" ^
                            "  imulq " ^ (exprToStr expr1 out false depth funName) ^ ", %rax\n"); "%rax"
      | S_DIV -> ope1 "ax" expr0 expr1
      | S_MOD -> ope1 "dx" expr0 expr1
      | S_ADD ->
        ope0 "addq" expr1 expr0
      | S_SUB -> ope0 "subq" expr0 expr1
      | S_INDEX ->
        let expr1Str = exprToStr expr1 out false depth funName in
        let reg = ref "" in
        if expr1Str = "%rbx" then (print out ("  movq " ^ expr1Str ^ ", %rdx" ^ (if debugMOV then "# mov L" else "")); print out ("  imulq $8, %rdx"); reg := "%rdx") else
        if (contains expr1Str "(%rbp)") then (print out ("  movq " ^ expr1Str ^ ", %rdx" ^ (if debugMOV then "# mov M" else "")); print out ("  imulq $8, %rdx"); reg := "%rdx") else (let expr1NbStr = (subLog expr1Str 1 ((String.length expr1Str) - 1) "N") in reg := "$" ^ (string_of_int (8 * (int_of_string expr1NbStr)))); (* here to have the offset from the array variable we extract X from -X(%rbp) for example *)
        print out ("  movq " ^ (exprToStr expr0 out false depth funName) ^ ", %rax" ^ (if debugMOV then "# mov N" else ""));
        print out ("  addq " ^ !reg ^ ", %rax");
        "(%rax)"
    )
  | CMP (cmpOp, locExpr0, locExpr1) ->
    (
      let (_, expr0) = locExpr0 and (_, expr1) = locExpr1 in
      needSetVar := true;
      let firstReg = ref (exprToStr expr0 out false depth funName) and secondReg = ref (exprToStr expr1 out false depth funName) in
      let isMemH = (isMem !firstReg && isMem !secondReg) and b = ref "" in
      let rev = ref false in (* we can't use heap variable in left or right slot but not the other so we reverse the order and reverse the comparator in order to reduce assembly code *)
      if isMemH then
        (
          b := "movq " ^ !firstReg ^ ", %rbx" ^ (if debugMOV then "# mov O" else "") ^ "\n  ";
          firstReg := "%rbx";
        );

      let s = ref ("  " ^
                   (if isMemH then
                      (
                        !b
                      )
                    else
                      (
                        if isMem !secondReg then
                          (
                            let tmp = !firstReg in
                            firstReg := !secondReg;
                            secondReg := tmp;
                            rev := true;
                          );
                        ""
                      )
                   )) in
      if !firstReg.[0] = '$' then
        ("movq " ^ !firstReg ^ ", %r9\n  "; firstReg := "%r9"); (* as stated previously need to use temporary variables because can't use heap/constant variables as we want *)
      s := !s ^ ("cmpq " ^ !secondReg);
      let res = (!s
                 ^ ", " ^ (if !firstReg = "" then "%rax" else !firstReg) ^ "\n  " ^
                 match cmpOp with
                 | C_LT -> ((if !rev then "jg" else "jl") ^ " ")
                 | C_LE -> "jle "
                 | C_EQ -> "je "
                ) in res
    )
  | EIF (locExpr0, locExpr1, locExpr2) ->
    let _, expr0 = locExpr0 and _, expr1 = locExpr1 and _, expr2 = locExpr2 and
    sectionName = ((getSectionName (addSection ())) ^ "A0") and nextSectionName = ((getSectionName (addSection ())) ^ "B0") and nextNextSectionName = ((getSectionName (addSection ())) ^ "C0") in

    let filler1 = ref "" in (* computing firsly the statements help to reduce current assembly to write *)
    pushFiller filler1;
    let reg1 = exprToStr expr1 out false depth funName in
    popFiller ();

    let filler2 = ref "" in
    pushFiller filler2;
    let reg2 = exprToStr expr2 out false depth funName in
    popFiller ();

    let reg1MultipleLines = contains reg1 "\n" in (* sometimes it isn't a single register which is return where we want to but code so we firstly put the code after the current writing statement *)
    let t = (sectionName ^ ":\n" ^ (if reg1MultipleLines then (reg1 ^ nextSectionName) else "  movq " ^ reg1 ^ ", %rbx" ^ (if debugMOV then "# mov P" else "")) ^ "\n  jmp " ^ nextNextSectionName ^ " #jmp F\n" ^ nextSectionName ^ ":\n  movq " ^ (if reg1MultipleLines then "$1" else reg2) ^ ", %rbx" ^ (if debugMOV then "# mov Q" else "")) in
    if debugEIF then
      print out "#EIF0";
    let reg = (exprToStr expr0 out false depth funName) in
    if debugEIF then
      print out "#EIF1";
    if reg.[0] = '$' then
      (
        print out ("  jmp " ^ (if reg = "$0" then nextSectionName else sectionName) ^ " #jmp G")
      )
    else
      (
        print out (reg ^ sectionName);
        print out ("  jmp " ^ nextSectionName ^ " #jmp H");
      );
    if debugEIF then
      print out "#EIF2";
    print out !filler1;
    if debugEIF then
      print out "#EIF3";
    print out !filler2;
    if debugEIF then
      print out "#EIF4";
    print out t;
    if debugEIF then
      print out "#EIF5";
    print out (nextNextSectionName ^ ":");
    if debugEIF then
      print out "#EIF6";
    "%rbx"
  | ESEQ (locExprList) ->
    if (List.length locExprList) = 0 then "nop" else "ESEQ";; (* likewise don't have empty lines *)

let rec compileDecl out decl depth varIndexFun funName =
  match decl with
  | CDECL (loc, varName) -> (* here we also manage variable declarations in a function with many arguments *)
    if funName <> "main" && varIndexFun != (-1) then
      (
        let reg = ref (getRegisterName varName depth funName) in
        if contains !reg "(%rip)" then (addVar varName out depth funName;
                                        reg := getRegisterName varName depth funName);
        let from = ref (if varIndexFun > 5 then ((string_of_int ((varIndexFun - 4) * 8)) ^ "(%rbp)") else (List.nth args varIndexFun)) in
        if contains !from "(%rbp)" then
          (print out ("  movq " ^ !from ^ ", %rdx"); from := "%rdx");
        print out ("  movq " ^ !from ^ ", " ^ !reg ^ (if debugMOV then "# mov Z" else ""))
      )
    else
      (
        addVar varName out depth funName
      );
  | CFUN (loc, funNameFun, varDecL, locCodeL) ->
    if funNameFun = "main" then
      (print out "\n.text\n.globl main");
    addFunctionNoConv funNameFun;
    print out (funNameFun ^ ":");
    print out "  pushq %rbp";
    print out ("  movq %rsp, %rbp" ^ (if debugMOV then "# mov R" else ""));
    print out ("  subq REG_COUNT, %rsp");
    let i = ref 0 in
    let rec aux varDecList =
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
              | CBLOCK (varDecList, locCodeList) -> compileAux out varDecList (d + 1) funNameFun; aux locCodeList (d + 1)
              | CEXPR locExpr -> (match locExpr with
                  | (loc, expr) -> print out ("  " ^ (exprToStr expr out false d funNameFun)))
              | CIF (locExpr, locCode0, locCode1) -> (* quite similar to EIF *)

                (
                  let _, expr = locExpr and
                  sectionName = ((getSectionName (addSection ())) ^ "A") and nextSectionName = ((getSectionName (addSection ())) ^ "B") and nextNextSectionName = ((getSectionName (addSection ())) ^ "C") in

                  let filler0 = ref "" in
                  pushFiller filler0;
                  aux [locCode0] (d + 1);
                  popFiller ();

                  let filler1 = ref "" in
                  pushFiller filler1;
                  aux [locCode1] (d + 1);
                  popFiller ();

                  let t = (sectionName ^ ":\n  " ^ !filler0 ^ "\n  jmp " ^ nextNextSectionName ^ " #jmp A\n" ^ nextSectionName ^ ":\n  " ^ !filler1 ^ "\n") in
                  let filler2 = ref "" in
                  pushFiller filler2;
                  let reg = exprToStr expr out false d funNameFun in
                  popFiller ();
                  if debugCIF then 
                    print out "#CIFTestBegin";
                  print out !filler2;
                  if debugCIF then
                    print out "#CIFTestMiddle";

                  (match expr with
                   | CMP (_, _, _) ->
                     (
                       print out (reg ^ sectionName);
                       print out ("  jmp " ^ nextSectionName ^ " #jmp B");
                     )
                   | _ ->
                     (
                       print out ("  movq " ^ (if reg = "" then "%rax" else reg) ^ ", %rcx" ^ (if debugMOV then "# mov T" else ""));
                       print out ("  cmpq " ^ "$0, " ^ "%rcx");
                       print out ("  jne " ^ sectionName ^ "\n");
                       print out ("  jmp " ^ nextSectionName ^ " #jmp C\n");
                     ));
                  if debugCIF then
                    print out "#CIFTestEnd";
                  if debugCIF then
                    print out "#CIFCodeBegin";
                  print out t;
                  if debugCIF then
                    print out "#CIFCodeEnd";
                  print out (nextNextSectionName ^ ":\n")
                )
              | CWHILE (locExpr, locCode) -> (* the order is: .LTEST: TEST CODE .LTHEN: THEN CODE .LELSE: ELSE CODE .LENDIF: *)
                let testSection = getSectionName (addSection ()) and nextSection = getSectionName (addSection ()) in
                let (_, expr) = locExpr in
                let filler = ref "" in
                pushFiller filler;
                let reg = (exprToStr expr out false d funNameFun) in
                popFiller ();
                let fillerCopy = !filler in
                if contains reg "\n" then
                  (
                    print out ("  jmp " ^ testSection ^ " # jmp D");
                    print out (nextSection ^ ":");
                    aux [locCode] d;
                    print out (testSection ^ ":");
                    print out fillerCopy;
                    print out (reg ^ nextSection);
                  )
                else
                  (
                    print out (testSection ^ ":");
                    print out fillerCopy;
                    print out ("  cmpq $0, " ^ reg);
                    print out ("  je " ^ nextSection ^ " #je");
                    aux [locCode] d;
                    print out ("  jmp " ^ testSection ^ " #jmp E");
                    print out (nextSection ^ ":");
                  );
              | CRETURN locExpr ->
                (
                  ( match locExpr with
                    | Some locExprContent -> ( match locExprContent with
                        | (loc, expr) -> let reg = (exprToStr expr out false d funNameFun) in if reg = "%rax" then () else (print out ("  movq " ^ (if reg = "" then "%rax" else reg) ^ ", %rax"));)
                    | _ -> () );
                  print out "  addq REG_COUNT, %rsp";
                  print out "  popq %rbp";
                  print out "  ret";
                )
            )
            ; aux q d)
      | _ -> ()
    in aux [locCodeL] depth;
    print out "  leave";
    print out "  ret";

and compileAux out decl_list depth funName =
  match decl_list with
  | t::q -> compileDecl out t depth (-1) funName; compileAux out q depth funName
  | _ -> ();;

let compile out decl_list =
  compileAux out decl_list 0 "";
  write out;;
