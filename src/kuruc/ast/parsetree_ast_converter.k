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

   (** Deals with top-level declarations. *)
   fun toDecl [] = []
     | toDecl ((P.ValDec ([binding], pos))::t) =
         A.ValDec ((A.UnitPattern pos, A.LiteralExpr (A.Unit, pos)), pos) ::
            toDecl t
     | toDecl ((P.ValDec (bindings, pos))::t) =
         A.MutualDec (
            map 
               (fn x =>  A.ValDec ((A.UnitPattern pos, A.LiteralExpr (A.Unit, pos)), pos)) bindings,
            pos
         ) :: toDecl t
     | toDecl ((P.FunDec (binding, pos))::t) = 
         A.FunDec 
            (S.fromString "foo" pos, 
            [(A.UnitPattern pos, A.LiteralExpr (A.Unit, pos))], 
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

