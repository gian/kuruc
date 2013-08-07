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
*    Converts a parse tree to a typed Abstract Syntax Tree
******************************************************************************)

structure ParseTreeAstConverter =
struct
   structure S = Symbol
   structure A = TypedAst
   structure P = ParseTree
   
   fun freshTy () = "xxx"

   (** Deals with patterns. *)
   fun toPat (P.IdPat (P.Ident (s, pos), pos')) = 
         A.IdentifierPattern (
            S.fromString s pos,
            freshTy (),
            pos
         )
     | toPat _ = A.LiteralPattern (A.Unit, (0, "fake"))
   
   (** Deals with literals. *)
   and toLit (P.Int (b, s, p)) =
      let
         val i = if b then IntInf.fromString ("~" ^ s)
            else IntInf.fromString (s)
      in
         A.Int (valOf i)
      end
     | toLit (P.Real (b, s1, s2, _, _)) =
         A.Real ((if b then "~" else "") ^ s1 ^ "." ^ s2)
     | toLit (P.Char (i, _)) = A.Char i
     | toLit (P.String (l, _)) = A.String (Vector.fromList l)
     | toLit (P.UnitExp _) = A.Unit

   (** Deals with expressions. *)
   and toExp (P.UnitExp pos) = A.LiteralExpr (A.Unit, pos) 
     | toExp (P.SeqExp ([e1,e2],pos)) = 
         A.SeqExpr (toExp e1, toExp e2, freshTy (), pos)
     | toExp (P.IfExp (e1, e2, e3, pos)) =
         A.IfExpr (toExp e1, toExp e2, toExp e3, freshTy (), pos)
     | toExp (P.WhileExp (e1, e2, pos)) =
         A.WhileExpr (toExp e1, toExp e2, freshTy (), pos)
     | toExp (P.App (e1, e2, pos)) =
         A.ApplyExpr (toExp e1, toExp e2, freshTy (), pos)
     | toExp (P.Ident (symbol, pos)) =
         A.IdentifierExpr (
            S.fromString symbol pos,
            freshTy (),
            pos
         )
     | toExp (lid as (P.LongId (h, t, pos))) =
      let
         fun collapse (P.LongId (h, t, p)) =
               collapse h ^ "." ^ collapse t
           | collapse (P.Ident (s, pos)) = s
           | collapse _ = Error.bug ("ParseTreeAstConverter",
                              "Malformed long identifier", pos)
      in
         A.IdentifierExpr (
            S.fromString (collapse lid) pos,
            freshTy (),
            pos
         )
      end
     | toExp (l as (P.Int _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (l as (P.Real _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (l as (P.Char _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (l as (P.String _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (P.Var (P.Ident (symbol, pos), pos')) =
         A.IdentifierExpr (
            S.fromString symbol pos,
            freshTy (),
            pos'
         )
     | toExp _ = A.LiteralExpr (A.Unit, (0, "fake"))

   and toClause (P.ValBind (pat, exp, pos)) =
         ((toPat pat, toExp exp), pos)
     | toClause _ = 
         Error.bug("ParseTreeAstConverter", "Not implemented", (0, "fake"))

   (** Deals with top-level declarations. *)
   and toDecl [] = []
     | toDecl ((P.ValDec ([binding], pos))::t) =
         A.ValDec (toClause binding) ::
            toDecl t
     | toDecl ((P.ValDec (bindings, pos))::t) =
         A.MutualDec (
            map (fn c => A.ValDec (toClause c)) bindings,
            pos
         ) :: toDecl t
     | toDecl ((P.FunDec ([binding], pos))::t) = 
         A.FunDec 
            (S.fromString "foo" pos, 
            [(A.UnitPattern pos, A.LiteralExpr (A.Unit, pos))], 
            pos
         ) :: toDecl t
     | toDecl ((P.FunDec (bindings, pos))::t) = 
         A.MutualDec (
            map (fn f => 
               A.FunDec (
                  S.fromString "foo" pos, 
                  [(A.UnitPattern pos, A.LiteralExpr (A.Unit, pos))], 
                  pos
               )
            ) bindings,
            pos
         ) :: toDecl t
     | toDecl ((P.TypeDec (binding, pos))::t) = 
         A.ValDec ((A.UnitPattern pos, A.LiteralExpr (A.Unit, pos)), pos) ::
            toDecl t
     | toDecl ((P.StructDec (l, pos))::t) =
         toDecl l @ toDecl t
     | toDecl ((P.SigDec (l, pos))::t) =
         toDecl l @ toDecl t
     | toDecl ((P.StructBind (P.Ident (ident, pos), ascr, P.Ident (ident', pos'), pos''))::t) =
         A.StructureAssignMod (
            S.fromString ident pos,
            S.fromString ident' pos', (* TODO: Functor application? *)
            pos''
         ) :: toDecl t
     | toDecl ((P.StructBind (P.Ident (ident, pos), ascr, body, pos'))::t) =
         A.StructureMod (
            Symbol.fromString ident pos,
            NONE,
            hd (toDecl [body]),
            pos') :: toDecl t
     | toDecl ((P.SigBind (P.Ident (ident, pos), body, pos'))::t) =
         A.SignatureMod (
            Symbol.fromString ident pos,
            hd (toDecl [body]),
            pos') :: toDecl t
     | toDecl ((P.Structure (decls, pos))::t) =
         A.StructMod (
            toDecl decls, 
            pos
         ) :: toDecl t
     | toDecl ((P.Signature (defs, pos))::t) =
         A.SigMod (
            [], 
            pos
         ) :: toDecl t
     | toDecl ((P.OpenDec _)::t) = toDecl t
     | toDecl ((P.DatatypeDec _)::t) = toDecl t
     | toDecl ((P.InfixDec _)::t) = toDecl t
     | toDecl ((P.InfixrDec _)::t) = toDecl t
     | toDecl ((P.ExnDec _)::t) = toDecl t
     | toDecl ((P.Import _)::t) = toDecl t
     | toDecl (p::_) = 
         Error.bug ("ParseTreeAstConverter", P.prettyPrint p, P.getPos p)

   (** This needs to flatten a list of parse tree lists into a single AST *)
   fun convert program =
      let
         val flattened = List.concat program
      in
         toDecl flattened
      end
end

