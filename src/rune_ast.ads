with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Rune_AST is

   type AST_Node is abstract tagged null record;
   type AST_Node_Access is access all AST_Node'Class;

   --  List of AST nodes for blocks, function calls, etc.
   package AST_Node_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Natural,
      Element_Type => AST_Node_Access
   );
   subtype AST_Node_Vector is AST_Node_Vectors.Vector;

   --------------------------
   --  Literal Expressions --
   --------------------------
   type Integer_Literal is new AST_Node with record
      Value : Integer;
   end record;

   type String_Literal is new AST_Node with record
      Value : Unbounded_String;
   end record;

   -----------------------
   --  Control Flow Nodes --
   -----------------------
   type Seq_Block is new AST_Node with record
      Statements : AST_Node_Vector;
   end record;

   type Fork_Join_Block is new AST_Node with record
      Branches : AST_Node_Vector;
   end record;

   -----------------------
   --  Function Nodes    --
   -----------------------
   type Parameter_Decl is new AST_Node with record
      Name : Unbounded_String;
      --  Type information could be added here later
   end record;

   package Parameter_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Natural,
      Element_Type => AST_Node_Access -- Using AST_Node_Access for Parameter_Decl
   );
   subtype Parameter_List is Parameter_Vectors.Vector;

   type Argument_Expr is new AST_Node with record
      Value : AST_Node_Access;
   end record;

   package Argument_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Natural,
      Element_Type => AST_Node_Access -- Using AST_Node_Access for Argument_Expr
   );
   subtype Argument_List is Argument_Vectors.Vector;

   type Function_Definition is new AST_Node with record
      Name        : Unbounded_String;
      Parameters  : Parameter_List;
      Function_Body : AST_Node_Access; -- Renamed from Body
   end record;

   type Function_Call is new AST_Node with record
      Name      : Unbounded_String;
      Arguments : Argument_List;
   end record;

end Rune_AST;
