with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Rune_AST;

package body Rune_AST_Printer is
   use type Rune_AST.AST_Node_Access; -- Make '=' visible

   procedure Print_Indent (Indent : in Natural) is
   begin
      for I in 1 .. Indent loop
         Ada.Text_IO.Put ("   "); -- 3 spaces per indent level
      end loop;
   end Print_Indent;

   procedure Print (Node : Rune_AST.AST_Node_Access; Indent : in Natural := 0) is
   begin
      if Node = null then
         Print_Indent (Indent);
         Ada.Text_IO.Put_Line ("(null node)");
         return;
      end if;

      Print_Indent (Indent);

      --  Cannot use case statement on class-wide types, use if/elsif instead
      if Node.all in Rune_AST.Integer_Literal then
         Ada.Text_IO.Put_Line ("Integer_Literal: " & Integer'Image (Rune_AST.Integer_Literal (Node.all).Value));

      elsif Node.all in Rune_AST.String_Literal then
         Ada.Text_IO.Put_Line ("String_Literal: """ & Ada.Strings.Unbounded.To_String (Rune_AST.String_Literal (Node.all).Value) & """");

      elsif Node.all in Rune_AST.Seq_Block then
         Ada.Text_IO.Put_Line ("Seq_Block:");
         for Statement of Rune_AST.Seq_Block (Node.all).Statements loop
            Print (Statement, Indent + 1);
         end loop;

      elsif Node.all in Rune_AST.Fork_Join_Block then
         Ada.Text_IO.Put_Line ("Fork_Join_Block:");
         for Branch of Rune_AST.Fork_Join_Block (Node.all).Branches loop
            Print (Branch, Indent + 1);
         end loop;

      elsif Node.all in Rune_AST.Parameter_Decl then
         Ada.Text_IO.Put_Line ("Parameter_Decl: " & Ada.Strings.Unbounded.To_String (Rune_AST.Parameter_Decl (Node.all).Name));

      elsif Node.all in Rune_AST.Function_Definition then
         Ada.Text_IO.Put_Line ("Function_Definition: " & Ada.Strings.Unbounded.To_String (Rune_AST.Function_Definition (Node.all).Name));
         Print_Indent (Indent + 1);
         Ada.Text_IO.Put_Line ("Parameters:");
         for Param of Rune_AST.Function_Definition (Node.all).Parameters loop
            Print (Param, Indent + 2);
         end loop;
         Print_Indent (Indent + 1);
         Ada.Text_IO.Put_Line ("Body:");
         Print (Rune_AST.Function_Definition (Node.all).Function_Body, Indent + 2);

      elsif Node.all in Rune_AST.Argument_Expr then
         Ada.Text_IO.Put_Line ("Argument_Expr:");
         Print (Rune_AST.Argument_Expr (Node.all).Value, Indent + 1);

      elsif Node.all in Rune_AST.Function_Call then
         Ada.Text_IO.Put_Line ("Function_Call: " & Ada.Strings.Unbounded.To_String (Rune_AST.Function_Call (Node.all).Name));
         Print_Indent (Indent + 1);
         Ada.Text_IO.Put_Line ("Arguments:");
         for Arg of Rune_AST.Function_Call (Node.all).Arguments loop
            Print (Arg, Indent + 2);
         end loop;
      else
         Ada.Text_IO.Put_Line ("Unknown AST Node Type.");
      end if;
   end Print;

end Rune_AST_Printer;
