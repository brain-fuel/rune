with Ada.Text_IO;
with Rune_AST;
with Rune_AST_Printer;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Main is
   --  Literals
   Int_Node_42    : constant Rune_AST.AST_Node_Access := new Rune_AST.Integer_Literal'(Value => 42);
   String_Node_Hi : constant Rune_AST.AST_Node_Access := new Rune_AST.String_Literal'(Value => To_Unbounded_String("Hello, Rune!"));

   --  Parameter for a function
   Param_X_Access : constant Rune_AST.AST_Node_Access := new Rune_AST.Parameter_Decl'(Name => To_Unbounded_String("X"));
   Params_Vector  : Rune_AST.Parameter_List;

   --  Function Body (a simple sequence block)
   Seq_Body_Statements : Rune_AST.AST_Node_Vector;
   Seq_Body_Access     : Rune_AST.AST_Node_Access := null;

   --  Function Definition
   Func_Def_Access     : Rune_AST.AST_Node_Access := null;
   Func_Def_Name       : constant Unbounded_String := To_Unbounded_String("My_Function");

   --  Arguments for a function call
   Arg_1_Access        : constant Rune_AST.AST_Node_Access := new Rune_AST.Argument_Expr'(Value => Int_Node_42);
   Args_Vector         : Rune_AST.Argument_List;

   --  Function Call
   Func_Call_Access    : Rune_AST.AST_Node_Access := null;
   Func_Call_Name      : constant Unbounded_String := To_Unbounded_String("My_Function_Call");

begin
   Ada.Text_IO.Put_Line ("Rune project initialized.");

   --  Build Seq_Body_Statements
   Rune_AST.AST_Node_Vectors.Append (Seq_Body_Statements, Int_Node_42);
   Rune_AST.AST_Node_Vectors.Append (Seq_Body_Statements, String_Node_Hi);
   Seq_Body_Access := new Rune_AST.Seq_Block'(Statements => Seq_Body_Statements);

   --  Build Params_Vector
   Rune_AST.Parameter_Vectors.Append (Params_Vector, Param_X_Access);

   --  Build Func_Def_Access
   Func_Def_Access := new Rune_AST.Function_Definition'(
      Name        => Func_Def_Name,
      Parameters  => Params_Vector,
      Function_Body => Seq_Body_Access
   );

   -- Build Args_Vector
   Rune_AST.Argument_Vectors.Append (Args_Vector, Arg_1_Access);

   -- Build Func_Call_Access
   Func_Call_Access := new Rune_AST.Function_Call'(
      Name      => Func_Call_Name,
      Arguments => Args_Vector
   );

   --  Test literals
   Rune_AST_Printer.Print (Int_Node_42);
   Rune_AST_Printer.Print (String_Node_Hi);

   --  Test Seq_Block
   Rune_AST_Printer.Print (Seq_Body_Access);

   --  Test Function Definition
   Rune_AST_Printer.Print (Func_Def_Access);

   -- Test Function Call
   Rune_AST_Printer.Print (Func_Call_Access);

end Main;
