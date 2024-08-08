// test/files/indent/indent_covergroup.ts.indent.sv
// randsequence and randcase are not supported by tree-sitter-verilog
// casex, casez not supported in implicit generate, outside of procedural block

// INFO: This one seeems to have bad syntax, because it's supported on new syntax

module asdf;

randsequence bar
    b               = c;
endsequence : bar

    casex (foo)
    1: a;
2:b;
endcase // case (foo)

    casez (foo)
    1: a;
2:b;
endcase // case (foo)

    randcase (foo)
    1: a;
2:b;
endcase // case (foo)

endmodule
