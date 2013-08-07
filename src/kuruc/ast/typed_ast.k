(******************************************************************************
* The Kuruc Compiler (http://www.kuru-lang.org)
* Copyright (c) 2010-2013, Gian Perrone
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
* 
* - Redistributions of source code must retain the above copyright notice, this
*   list of conditions and the following disclaimer.
*
* - Redistributions in binary form must reproduce the above copyright notice,
*   this list of conditions and the following disclaimer in the documentation
*   and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************
*  This file:
*    A typed Abstract Syntax Tree
******************************************************************************)

structure TypedAst =
struct
   structure S = Symbol

   type symbol = S.t
   type pos = Position.t 

   type ty = string

   type constructor_tag = int

   datatype ascription =
      Opaque
    | Transparent

   datatype literal =
      Int of IntInf.int
    | Real of string
    | Char of int
    | String of int vector
    | Unit

   (* A note on desugaring patterns:
    *
    * Wildcards (_) get desugared to a fresh IdentifierPattern
    * Infix patterns (h::t) get turned into constructor patterns.
    * List patterns ([1,2,3,4]) get turned into constructor patterns.
    * Nil (nil, []) gets turned into a constructor pattern.
    *)
   datatype pattern =
      LiteralPattern of literal * pos
    | TuplePattern of pattern list * ty * pos
    | ConstructorPattern of constructor_tag * pattern option * ty * pos
    | IdentifierPattern of symbol * ty * pos
    | UnitPattern of pos

   datatype expression =
      LiteralExpr of literal * pos
    | IdentifierExpr of symbol * ty * pos  (* Long and short identifiers. *)
    | TupleExpr of expression list * ty * pos
    | IfExpr of expression * expression * expression * ty * pos
    | WhileExpr of expression * expression * ty * pos
    | FnExpr of clause list * ty * pos
    | CaseExpr of expression * clause list * ty * pos
    | ApplyExpr of expression * expression * ty * pos
    | RaiseExpr of expression * pos
    | HandleExpr of expression * clause list * ty * pos
    | SeqExpr of expression * expression * ty * pos
    | LetExpr of declaration list * expression * ty * pos
    | AnnotationExpr of expression * ty * pos
    | ConstructorExpr of constructor_tag * expression option * ty * pos

   and declaration =
      ValDec of clause * pos
    | ValRecDec of clause * pos
    | FunDec of symbol * clause list * pos
    | MutualDec of declaration * declaration * pos
    | OpenDec of symbol * pos
    | BindDec of symbol * module * pos

   and definition =
      ValDef of symbol * ty * pos
    | TypDef of ty * ty * pos

   and module =
      StructureMod of symbol * module option * module * pos
    | SignatureMod of symbol * module * pos
    | StructMod of declaration list * pos
    | SigMod of definition list * pos
    | RefMod of symbol * pos
    | FunctorMod of symbol * functor_clause list * module * pos
    | DeclMod of declaration

   withtype clause = pattern * expression
        and functor_clause = module * module

   type program = module list

   fun indent 0 = ""
     | indent n = "   " ^ indent (n - 1)

   fun ppdec ind _ = indent ind ^ "(* declaration *)"
   
   fun ppdef ind _ = indent ind ^ "(* definition *)"

   fun ppmod ind (StructureMod (symbol, NONE, str, _)) =
         indent ind ^ "structure " ^ S.toString symbol ^ " =\n" ^
            ppmod ind str
     | ppmod ind (StructureMod (symbol, SOME sg, str, _)) =
         indent ind ^ "structure " ^ S.toString symbol ^ " :\n" ^
         ppmod (ind + 1) sg ^ 
         " =\n" ^
         ppmod (ind + 1) str
     | ppmod ind (SignatureMod (symbol, sg, _)) =
         indent ind ^ "signature " ^ S.toString symbol ^ " =\n" ^
            ppmod ind sg
     | ppmod ind (StructMod (decls, _)) =
         indent ind ^ "struct\n" ^
            String.concatWith "\n" (map (ppdec (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "end"
     | ppmod ind (SigMod (decls, _)) =
         indent ind ^ "sig\n" ^
            String.concatWith "\n" (map (ppdef (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "end"
     | ppmod ind (RefMod (symbol, _)) = indent ind ^ S.toString symbol 
     | ppmod ind (FunctorMod (symbol, fclause, body, _)) = indent ind ^
         "functor " ^ S.toString symbol ^ " (...) =\n" ^
            ppmod ind body
     | ppmod ind (DeclMod decl) = ppdec ind decl

   fun prettyPrint [] = ""
     | prettyPrint (h::t) = ppmod 0 h ^ "\n" ^ prettyPrint t
end
