(*******************************************************************************
*  The Kuru Programming Language Compiler Toolset (http://www.kuru-lang.org)
*  Copyright (C) 2010-2013  Gian Perrone
*
*  This program is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*******************************************************************************
*  This file:
*    A typed Abstract Syntax Tree
******************************************************************************)

structure TypedAst =
struct
   type symbol = string

   type pos = string * int * int

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

   and module =
      StructureMod of symbol * module option * declaration list * pos
    | SignatureMod of symbol * declaration list * pos
    | RefMod of symbol * pos
    | AssignMod of symbol * module * module * pos
    | AscriptionMod of module * ascription * pos
    | FunctorMod of symbol * functor_clause list * module * pos

   withtype clause = pattern * expression
        and functor_clause = module * module

   type program = module list
end
