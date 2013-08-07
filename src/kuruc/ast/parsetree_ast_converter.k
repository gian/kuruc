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

   (** Deals with top-level constructs. *)
   fun toModule [] = []
     | toModule ((P.ValDec (binding, pos))::t) = 
         A.DeclMod (A.OpenDec (Symbol.fromString "foo" pos, pos)) :: toModule t
     | toModule ((P.FunDec (binding, pos))::t) = 
         A.DeclMod (A.OpenDec (Symbol.fromString "foo" pos, pos)) :: toModule t
     | toModule ((P.TypeDec (binding, pos))::t) = 
         A.DeclMod (A.OpenDec (Symbol.fromString "foo" pos, pos)) :: toModule t
     | toModule ((P.StructDec (l, pos))::t) =
         toModule l @ toModule t
     | toModule ((P.SigDec (l, pos))::t) =
         toModule l @ toModule t
     | toModule ((P.StructBind (P.Ident (ident, pos), ascr, body, pos'))::t) =
         A.StructureMod (
            Symbol.fromString ident pos,
            NONE,
            hd (toModule [body]),
            pos') :: toModule t
     | toModule ((P.SigBind (P.Ident (ident, pos), body, pos'))::t) =
         A.SignatureMod (
            Symbol.fromString ident pos,
            hd (toModule [body]),
            pos') :: toModule t
     | toModule ((P.Structure (decls, pos))::t) =
         A.StructMod ([], pos) :: toModule t
     | toModule ((P.Signature (defs, pos))::t) =
         A.SigMod ([], pos) :: toModule t
     | toModule ((P.OpenDec _)::t) = toModule t
     | toModule ((P.DatatypeDec _)::t) = toModule t
     | toModule ((P.InfixDec _)::t) = toModule t
     | toModule ((P.InfixrDec _)::t) = toModule t
     | toModule ((P.ExnDec _)::t) = toModule t
     | toModule ((P.Import _)::t) = toModule t
     | toModule (h::t) = 
         Error.bug ("ParseTreeAstConverter", P.prettyPrint h, (0, "__fake__"))

   (** This needs to flatten a list of parse tree lists into a single AST *)
   fun convert program =
      let
         val flattened = List.concat program
      in
         toModule flattened
      end
end

